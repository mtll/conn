;;; conn-tree-sitter.el --- Treesit Things -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (compat "30.0.2.0") conn)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'conn)
(require 'conn-things)
(require 'conn-dispatch)
(require 'treesit)
(eval-when-compile
  (require 'cl-lib)
  (require 'map))

(cl-defstruct (conn-ts--thing
               (:constructor conn--make-ts-thing))
  (groups nil :type (or cons symbol) :read-only t))

(defvar conn-ts--chunk-size 10000)

(defconst conn-ts--queries (make-hash-table :test 'eq))
(defconst conn-ts--compiled-query-cache (make-hash-table :test 'eq))
(defvar conn-ts--custom-queries nil)

(defvar conn-ts-query-dir
  (expand-file-name
   "queries"
   (file-name-directory (locate-library "conn-tree-sitter"))))

(defvar conn-ts--symbol-tick 0)

(defconst conn-ts--symbol-cache
  (make-hash-table :test 'equal))

(defun conn-ts--make-symbol (&rest strings-or-symbols)
  (intern (concat
           (cl-loop for s in strings-or-symbols
                    when (stringp s) concat s
                    when (symbolp s) concat (symbol-name s))
           (number-to-string
            (cl-incf conn-ts--symbol-tick)))))

(defun conn-ts--parse-query (query)
  (cl-labels
      ((make-predicate (form capture func)
         (with-memoization (gethash form conn-ts--symbol-cache)
           `((:pred
              ,(let ((sym (conn-ts--make-symbol "conn-ts--anonymous-predicate")))
                 (fset sym func)
                 sym)
              ,capture))))
       (make-offset (capture &optional brow bcol erow ecol)
         (with-memoization (gethash (list capture brow bcol erow ecol)
                                    conn-ts--symbol-cache)
           (let ((fsym (conn-ts--make-symbol
                        (substring (symbol-name capture) 1)
                        "--offset"))
                 (capture-sym (intern (substring (symbol-name capture) 1))))
             (fset fsym
                   (pcase-lambda ((and cons `(,_fsym . ,node)))
                     (setf (car cons) capture-sym)
                     (setf (cdr cons)
                           (with-current-buffer (treesit-node-buffer node)
                             (cons (save-excursion
                                     (goto-char (treesit-node-start node))
                                     (when (and brow (/= 0 brow))
                                       (let ((col (current-column)))
                                         (forward-line brow)
                                         (move-to-column col)))
                                     (when (and bcol (/= 0 bcol))
                                       (forward-char bcol))
                                     (point))
                                   (save-excursion
                                     (goto-char (treesit-node-end node))
                                     (when (and erow (/= 0 erow))
                                       (let ((col (current-column)))
                                         (forward-line erow)
                                         (move-to-column col)))
                                     (when (and ecol (/= 0 ecol))
                                       (forward-char ecol))
                                     (point)))))))
             (put fsym :conn-ts--offset-capture t)
             (intern (concat "@" (symbol-name fsym))))))
       (subst (alist exp)
         (if (null alist)
             (list exp)
           (pcase exp
             (`(,(pred keywordp) . ,_)
              (list exp))
             ((pred consp)
              (list (mapcan (lambda (e) (subst alist e)) exp)))
             ((pred vectorp)
              `([,@(mapcan (lambda (e) (subst alist e))
                           (append exp nil))]))
             (_ (or (copy-sequence (assq exp alist))
                    (list exp))))))
       (walk (exp)
         (cl-loop
          with offset-sub = nil
          for e in exp
          nconc (pcase e
                  (`(:any-of? ,capt . ,pats)
                   `((:match? ,(regexp-opt (mapcar #'regexp-quote pats))
                              ,capt)))
                  (`(:not-any-of? ,capt . ,pats)
                   `((:not-match? ,(regexp-opt (mapcar #'regexp-quote pats))
                                  ,capt)))
                  (`(:offset! ,capt . ,offsets)
                   (push (apply #'make-offset capt offsets)
                         (alist-get capt offset-sub))
                   nil)
                  (`(:lua-match? ,capt ,pat)
                   ;; All #lua-match? directives I found work fine as
                   ;; regexps, I think
                   `((:match? ,pat ,capt)))
                  ((and form `(:not-lua-match? ,capt ,pat))
                   (make-predicate
                    form capt
                    (lambda (node)
                      (save-excursion
                        (save-match-data
                          (goto-char (treesit-node-start node))
                          (not
                           (eql (treesit-node-end node)
                                (re-search-forward
                                 pat (treesit-node-end node) nil))))))))
                  ((and form `(:not-kind-eq? ,capt ,type))
                   (make-predicate
                    form capt
                    (lambda (node)
                      (not (equal (treesit-node-type node)
                                  type)))))
                  (`(,(pred keywordp) . ,_)
                   nil)
                  ((pred vectorp)
                   `([,@(walk (append e nil))]))
                  ((guard (not (consp e)))
                   (list e))
                  (_ (list (walk e))))
          into newexp
          finally return
          (mapcan (lambda (e) (subst offset-sub e)) newexp))))
    (walk query)))

(defun conn-ts--parse-scm-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (lisp-data-mode)
    (goto-char (point-min))
    (save-excursion
      (while-let ((sp (and (re-search-forward "[#.+*?]" nil t)
                           (syntax-ppss))))
        (unless (or (nth 3 sp)
                    (nth 4 sp))
          (pcase (char-before)
            (?\#
             (delete-char -1)
             (insert ":"))
            (?.
             (unless (and (looking-at-p (rx (or (syntax word)
                                                (syntax symbol))))
                          (save-excursion
                            (backward-char 2)
                            (looking-at-p (rx (or (syntax word)
                                                  (syntax symbol))))))
               (delete-char -1)
               (insert ":anchor")))
            ((or ?+ ?* ??)
             (unless (or (looking-at-p (rx (or (syntax word)
                                               (syntax symbol))))
                         (save-excursion
                           (backward-char 2)
                           (looking-at-p (rx (or (syntax word)
                                                 (syntax symbol))))))
               (backward-char 1)
               (insert ?:)))))))
    (let ((queries nil)
          (curr nil))
      (while-let ((sexp (ignore-errors (read (current-buffer)))))
        (pcase sexp
          ('nil)
          ((pred consp)
           (when curr (push (nreverse curr) queries))
           (setf curr (list sexp)))
          (_
           (push sexp curr))))
      (when curr (push (nreverse curr) queries))
      (mapcar #'conn-ts--parse-query (nreverse queries)))))

(defun conn-ts--get-language-query (language)
  (cl-labels
      ((parse-inherits ()
         (let (inherits toplevel)
           (when (re-search-forward "; inherits:? " nil t) ;?
             (while (re-search-forward
                     (rx (or (seq "(" (group (not ")")) ")")
                             (seq (group (not ",")))))
                     (pos-eol) t)
               (if (match-beginning 1)
                   (push (intern (buffer-substring (match-beginning 1)
                                                   (match-end 1)))
                         toplevel)
                 (push (intern (buffer-substring (match-beginning 2)
                                                 (match-end 2)))
                       inherits))))
           (cons inherits toplevel)))
       (get-query (lang)
         (with-memoization (gethash lang conn-ts--queries)
           (let ((filename (expand-file-name
                            (concat (symbol-name lang) "/textobjects.scm")
                            conn-ts-query-dir)))
             (when (file-exists-p filename)
               (conn--with-work-buffer
                 (insert-file-contents-literally filename)
                 (pcase-let ((`(,inherits ,toplevel)
                              (parse-inherits)))
                   (put lang :conn-ts-inherits inherits)
                   (put lang :conn-ts-toplevel toplevel)
                   (conn-ts--parse-scm-buffer)
                   (treesit-query-expand (conn-ts--parse-scm-buffer))))))))
       (compose-query (lang)
         (concat (get-query lang)
                 (mapconcat #'compose-query
                            (get lang :conn-ts-inherits))))
       (get-extras (lang)
         (apply #'concat (alist-get lang conn-ts--custom-queries)))
       (compose-extras (lang)
         (concat (get-extras lang)
                 (mapconcat #'compose-extras
                            (get lang :conn-ts-inherits)))))
    (concat (compose-query language)
            (compose-extras language)
            (mapconcat #'get-query
                       (get language :conn-ts-toplevel))
            (mapconcat #'get-extras
                       (cons t (get language :conn-ts-toplevel))))))

(defun conn-ts--recompile-query (language &optional eager)
  (if eager
      (setf (gethash language conn-ts--compiled-query-cache)
            (treesit-query-compile language
                                   (conn-ts--get-language-query language)
                                   'eager))
    (remhash language conn-ts--compiled-query-cache)))

(defun conn-ts-add-custom-query (language query)
  (pcase query
    ((pred stringp)
     (push query (alist-get language conn-ts--custom-queries)))
    ((pred consp)
     (push (treesit-query-expand query)
           (alist-get language conn-ts--custom-queries))))
  (conn-ts--recompile-query language))

(defun conn-ts--get-query (language)
  (with-memoization (gethash language conn-ts--compiled-query-cache)
    (treesit-query-compile
     language
     (conn-ts--get-language-query language)
     'eager)))

(defun conn-ts--normalize-captures (capture-group)
  (let ((result nil))
    (dolist (capture capture-group)
      (when (treesit-node-p (cdr capture))
        (if (get (car capture) :conn-ts--offset-capture)
            (funcall (car capture) capture)
          (setf (cdr capture)
                (cons (treesit-node-start (cdr capture))
                      (treesit-node-end (cdr capture)))))
        (if-let* ((bound (alist-get (car capture) result)))
            (progn
              (cl-callf min (car bound) (cadr capture))
              (cl-callf max (cdr bound) (cddr capture)))
          (push capture result))))
    result))

(defun conn-ts-capture (start end)
  (cl-loop for parser in (append (treesit-parser-list)
                                 (treesit-local-parsers-on start end))
           for lang = (treesit-parser-language parser)
           nconc (mapcan #'conn-ts--normalize-captures
                         (treesit-query-capture
                          parser (conn-ts--get-query lang)
                          start end nil t))))

(defun conn-ts--get-thing-groups (thing)
  (pcase thing
    ((pred symbolp)
     (conn-ts--thing-groups
      (get (or (get thing :conn-command-thing) thing)
           :conn-ts-thing)))
    ((pred conn-anonymous-thing-p)
     (conn-ts--get-thing-groups
      (conn-anonymous-thing-parent thing)))
    ((pred conn-bounds-p)
     (conn-ts--get-thing-groups
      (conn-bounds-thing thing)))))

(defun conn-ts-filter-captures (things captures &optional predicate)
  (let ((groups (cl-loop for thing in (ensure-list things)
                         append (conn-ts--get-thing-groups thing)))
        (result nil))
    (pcase-dolist ((and capture `(,type . ,bound)) captures)
      (when (and (memq type groups)
                 (or (null predicate)
                     (funcall predicate bound)))
        (push capture result)))
    result))

(defun conn-ts-select-expansion (get-captures thing)
  (let ((things nil))
    (pcase-dolist (`(,type ,beg . ,end) (funcall get-captures))
      (if-let* ((b (seq-find (lambda (bounds)
                               (pcase-let (((conn-bounds `(,b . ,e))
                                            bounds))
                                 (and (eql beg b)
                                      (eql end e))))
                             things)))
          (push type (conn-anonymous-thing-property
                      (conn-bounds-thing b)
                      :types))
        (push (conn-make-bounds
               (conn-anonymous-thing
                 thing
                 :types (list type)
                 :pretty-print ( :method (self)
                                 (mapconcat
                                  (lambda (o) (format "%s" o))
                                  (conn-anonymous-thing-property self :types)
                                  " & "))
                 :bounds-op ( :method (_ _)
                              (conn-ts-select-expansion get-captures thing)))
               nil
               (cons beg end))
              things)))
    (conn-multi-thing-select things)))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-ts-thing))
                              arg
                              &key flat)
  (setq arg (prefix-numeric-value arg))
  (unless (= 0 arg)
    (let* ((seen (when (> arg 100)
                   (make-hash-table :test 'equal :size arg)))
           (captures nil)
           (at (point))
           (beg (point))
           (end nil)
           (count 0)
           (nodes nil)
           (max most-negative-fixnum)
           (min most-positive-fixnum))
      (catch 'done
        (while (if (< arg 0)
                   (> beg (point-min))
                 (< beg (point-max)))
          (setq end (if (< arg 0)
                        (max (point-min) (- beg conn-ts--chunk-size))
                      (min (point-max) (+ beg conn-ts--chunk-size)))
                captures (conn-ts-filter-captures
                          (conn-get-thing cmd)
                          (conn-ts-capture (min beg end) (max beg end)))
                beg end)
          (if flat
              (progn
                (cl-callf sort captures
                  :key #'cadr
                  :reverse (< arg 0)
                  :in-place t)
                (pcase-dolist (`(,_type . ,node) captures)
                  (when (and (if (< arg 0)
                                 (< (car node) at)
                               (> (car node) at))
                             (if seen
                                 (or (not (gethash node seen))
                                     (setf (gethash node seen) t))
                               (not (member node nodes))))
                    (cl-callf max max (cdr node))
                    (cl-callf min min (car node))
                    (push (conn-make-bounds cmd 1 node) nodes)
                    (setq at (car node))
                    (when (= (cl-incf count) (abs arg))
                      (throw 'done nil)))))
            (cl-callf sort captures
              :key (if (< arg 0)
                       (lambda (n)
                         (if (>= at (cddr n)) (cddr n) (cadr n)))
                     (lambda (n)
                       (if (<= at (cadr n)) (cadr n) (cddr n))))
              :reverse (< arg 0)
              :in-place t)
            (pcase-dolist (`(,_type . ,node) captures)
              (when (and (if (< arg 0)
                             (< (car node) at)
                           (> (cdr node) at))
                         (if seen
                             (or (not (gethash node seen))
                                 (setf (gethash node seen) t))
                           (not (member node nodes))))
                (cl-callf max max (cdr node))
                (cl-callf min min (car node))
                (push (conn-make-bounds cmd 1 node) nodes)
                (setq at (if (< arg 0) (car node) (cdr node)))
                (when (= (cl-incf count) (abs arg))
                  (throw 'done nil)))))))
      (when nodes
        (conn-make-bounds
         cmd arg
         (cons min max)
         :subregions (nreverse nodes))))))

(conn-register-thing 'conn-ts-thing)

(defvar conn-ts-multi-always-prompt-p
  (lambda ()
    (and conn-dispatch-in-progress
         (not (memq conn-dispatch-current-action
                    '(conn-dispatch-ts-goto
                      conn-dispatch-jump))))))

(defvar-local conn-ts-parent-things
  `(conn-ts-assignment-inner
    conn-ts-assignment-outer
    conn-ts-assignment-side
    conn-ts-attribute-inner
    conn-ts-attribute-outer
    conn-ts-block-inner
    conn-ts-block-outer
    conn-ts-call-inner
    conn-ts-call-outer
    conn-ts-class-inner
    conn-ts-class-outer
    conn-ts-comment-inner
    conn-ts-comment-outer
    conn-ts-conditional-inner
    conn-ts-conditional-outer
    conn-ts-frame-inner
    conn-ts-frame-outer
    conn-ts-function-inner
    conn-ts-function-outer
    conn-ts-loop-inner
    conn-ts-loop-outer
    conn-ts-number
    conn-ts-parameter-inner
    conn-ts-parameter-outer
    conn-ts-regex-inner
    conn-ts-regex-outer
    conn-ts-return-inner
    conn-ts-return-outer
    conn-ts-scopename
    conn-ts-statement))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-ts-parent-things))
                              _arg)
  (conn-ts-select-expansion
   (lambda ()
     (conn-ts-filter-captures
      conn-ts-parent-things
      (conn-ts-capture (point) (1+ (point)))
      (lambda (bd)
        (<= (car bd) (point) (cdr bd)))))
   'conn-ts-thing))

(defvar-local conn-ts-all-query-things
  conn-ts-parent-things)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-ts-all-query-things))
                              _arg)
  (conn-ts-select-expansion
   (lambda ()
     (conn-ts-filter-captures
      conn-ts-all-query-things
      (conn-ts-capture (point) (1+ (point)))
      (lambda (bd)
        (<= (car bd) (point) (cdr bd)))))
   'conn-ts-thing))

(defun conn-dispatch-ts-goto ()
  (declare (conn-dispatch-action t))
  (oclosure-lambda (conn-dispatch-goto
                    (action-description "Goto Node"))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,_transform)
                  (conn-select-target)))
      (select-window window)
      (conn-dispatch-change-group)
      (unless (= pt (point))
        (unless conn-dispatch-repeating
          (conn-push-jump-ring (point)))
        (unless (region-active-p)
          (push-mark nil t))
        (goto-char pt))
      (conn-set-last-thing-command thing arg pt))))

(conn-define-target-finder conn-ts-query-targets
    ()
    ((things :initarg :things)
     (region-predicate :initarg :region-predicate)
     (window-predicate
      :initform (lambda (win)
                  (treesit-parser-list (window-buffer win)))))
  ( :default-update-handler (state)
    (let ((region-pred (ignore-error unbound-slot
                         (oref state region-predicate)))
          (things (oref state things)))
      (cl-flet ((make-bounds (bounds things)
                  (conn-make-bounds
                   (conn-anonymous-thing
                     'conn-ts-thing
                     :types things
                     :target-finder ( :method (self _arg)
                                      (conn-ts-query-targets
                                       :things (conn-anonymous-thing-property
                                                self :types)))
                     :pretty-print ( :method (self)
                                     (mapconcat
                                      #'conn-thing-pretty-print
                                      (conn-anonymous-thing-property self :types)
                                      " & "))
                     :bounds-op ( :method (self _)
                                  (conn-ts-select-expansion
                                   (lambda ()
                                     (conn-ts-filter-captures
                                      (conn-anonymous-thing-property
                                       self :types)
                                      (conn-ts-capture (point) (1+ (point)))
                                      (lambda (bd)
                                        (<= (car bd) (point) (cdr bd)))))
                                   'conn-ts-thing)))
                   nil bounds)))
        (pcase-dolist (`(,vbeg . ,vend)
                       (conn--visible-regions (window-start)
                                              (window-end)))
          (pcase-dolist (`(,type ,beg . ,end)
                         (conn-ts-capture vbeg vend))
            (when-let* ((type-things (seq-intersection
                                      (get type :conn-ts--member-of)
                                      things))
                        (_ (and (<= (window-start) beg (window-end))
                                (or (null region-pred)
                                    (funcall region-pred beg end)))))
              (if-let* ((ov (car (conn--overlays-in-of-type
                                  beg (1+ beg) 'conn-target-overlay
                                  (selected-window)))))
                  (if-let* ((b (seq-find
                                (lambda (bound)
                                  (pcase bound
                                    ((conn-bounds `(,b . ,e))
                                     (and (eql beg b) (eql end e)))))
                                (conn-anonymous-thing-property
                                 (overlay-get ov 'thing)
                                 :bounds))))
                      (cl-callf2 seq-union
                          type-things
                          (conn-anonymous-thing-property
                           (conn-bounds-thing b)
                           :types))
                    (push (make-bounds (cons beg end) type-things)
                          (conn-anonymous-thing-property
                           (overlay-get ov 'thing)
                           :bounds)))
                (conn-make-target-overlay
                 beg 0
                 :thing (conn-anonymous-thing
                          'conn-ts-thing
                          :bounds (list (make-bounds (cons beg end) type-things))
                          :bounds-op ( :method (self _arg)
                                       (conn-with-dispatch-suspended
                                         (conn-multi-thing-select
                                          (conn-anonymous-thing-property self :bounds)
                                          (funcall conn-ts-multi-always-prompt-p))))))))))))))

(conn-define-target-finder conn-ts-all-things
    ()
    ((thing :initarg :thing)
     (window-predicate
      :initform (lambda (win)
                  (treesit-parser-list (window-buffer win)))))
  ( :default-update-handler (state)
    (let* ((thing (oref state thing))
           (query (conn-ts--thing-node-query thing)))
      (cl-flet ((make-bounds (bounds parent-thing type)
                  (conn-make-bounds
                   (conn-anonymous-thing
                     parent-thing
                     :types (list type)
                     :target-finder ( :method (self _arg)
                                      (conn-ts-all-things :thing thing))
                     :pretty-print ( :method (self)
                                     (string-join
                                      (conn-anonymous-thing-property self :types)
                                      " & "))
                     :bounds-op ( :method (_ _)
                                  (conn-ts-select-expansion
                                   (lambda ()
                                     (mapcar
                                      (pcase-lambda (`(,_ . ,node))
                                        (cons (treesit-node-type node)
                                              (cons (treesit-node-start node)
                                                    (treesit-node-end node))))
                                      (treesit-query-capture
                                       (treesit-language-at (point))
                                       query (point) (1+ (point)))))
                                   thing)))
                   nil bounds)))
        (pcase-dolist (`(,vbeg . ,vend)
                       (conn--visible-regions (window-start)
                                              (window-end)))
          (pcase-dolist ((and `(,thing . ,node)
                              (let type (treesit-node-type node))
                              (let beg (treesit-node-start node))
                              (let end (treesit-node-end node)))
                         (treesit-query-capture
                          (treesit-language-at (point))
                          query vbeg vend))
            (if-let* ((ov (car (conn--overlays-in-of-type
                                beg (1+ beg) 'conn-target-overlay
                                (selected-window)))))
                (if-let* ((b (seq-find
                              (lambda (bound)
                                (pcase bound
                                  ((conn-bounds `(,b . ,e))
                                   (and (eql beg b) (eql end e)))))
                              (conn-anonymous-thing-property
                               (overlay-get ov 'thing)
                               :bounds))))
                    (push type (conn-anonymous-thing-property
                                (conn-bounds-thing b)
                                :types))
                  (push (make-bounds (cons beg end) thing type)
                        (conn-anonymous-thing-property
                         (overlay-get ov 'thing)
                         :bounds)))
              (conn-make-target-overlay
               beg 0
               :thing (conn-anonymous-thing
                        thing
                        :bounds (list (make-bounds (cons beg end) thing type))
                        :bounds-op ( :method (self _arg)
                                     (conn-with-dispatch-suspended
                                       (conn-multi-thing-select
                                        (conn-anonymous-thing-property self :bounds)
                                        (funcall conn-ts-multi-always-prompt-p)))))))))))))

(defun conn-ts--thing-predicate (thing)
  (with-memoization (gethash (cons 'conn-all-things thing)
                             conn-ts--symbol-cache)
    (let ((fsym (conn-ts--make-symbol "conn-ts--all-" thing)))
      (fset fsym (lambda (node)
                   (treesit-node-match-p node thing)))
      fsym)))

(defun conn-ts--thing-node-query (thing)
  `(((_) @node
     (:pred? ,(conn-ts--thing-predicate thing) @node))))

(cl-defmethod conn-get-target-finder ((cmd (conn-thing conn-ts-thing))
                                      _arg)
  (conn-ts-query-targets
   :things (list (or (get cmd :conn-command-thing) cmd))))

(cl-defmethod conn-thing-pretty-print ((_cmd (conn-thing conn-ts-thing)))
  (let ((string (cl-call-next-method)))
    (if (string-prefix-p "conn-ts-" string)
        (substring string (length "conn-ts-"))
      string)))

(defun conn-ts--thing-forward (thing arg)
  (interactive "p")
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end))
     (goto-char (if (> arg 0) end beg)))
    (_ (signal 'scan-error
               (list (format-message "No more %S to move across" thing)
                     (point) (point))))))

(defun conn-ts--thing-forward-flat (thing arg)
  (interactive "p")
  (pcase (thread-first
           (conn-bounds-of thing arg :flat t)
           (conn-bounds-get :subregions)
           last car)
    ((conn-bounds `(,beg . ,_end))
     (goto-char beg))
    (_ (signal 'scan-error
               (list (format-message "No more %S to move across" thing)
                     (point) (point))))))

;;;###autoload
(defmacro conn-ts-define-thing (name group)
  (declare (indent defun)
           (autoload-macro expand))
  (let ((forward-cmd (intern (format "%s-forward" name)))
        (forward-flat-cmd (intern (format "%s-forward-flat" name)))
        (backward-cmd (intern (format "%s-backward" name)))
        (backward-flat-cmd (intern (format "%s-backward-flat" name)))
        (groups (cl-loop for g in (ensure-list group)
                         collect (intern g))))
    `(progn
       (defun ,forward-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward ',name arg))

       (defun ,backward-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward ',name (- arg)))

       (defun ,forward-flat-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward-flat ',name arg))

       (defun ,backward-flat-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward-flat ',name (- arg)))

       (conn-register-thing ',name
                            :parent 'conn-ts-thing
                            :forward-op ',forward-cmd)

       (conn-register-thing-commands
        ',name #'ignore
        ',forward-cmd
        ',backward-cmd
        ',forward-flat-cmd
        ',backward-flat-cmd)

       :autoload-end
       (put ',name :conn-ts-thing (conn--make-ts-thing :groups ',groups))
       ,@(cl-loop for g in groups
                  collect `(push ',name (get ',g :conn-ts--member-of)))
       (put ',backward-flat-cmd 'repeat-check-key 'no)
       (put ',forward-flat-cmd 'repeat-check-key 'no)
       (put ',backward-cmd 'repeat-check-key 'no)
       (put ',forward-cmd 'repeat-check-key 'no))))

(conn-ts-define-thing conn-ts-assignment-inner "assignment.inner")
(conn-ts-define-thing conn-ts-assignment-outer "assignment.outer")
(conn-ts-define-thing conn-ts-assignment-side ("assignment.lhs" "assignment.rhs"))
(conn-ts-define-thing conn-ts-assignment-lhs "assignment.lhs")
(conn-ts-define-thing conn-ts-assignment-rhs "assignment.rhs")
(conn-ts-define-thing conn-ts-attribute-inner "attribute.inner")
(conn-ts-define-thing conn-ts-attribute-outer "attribute.outer")
(conn-ts-define-thing conn-ts-block-inner "block.inner")
(conn-ts-define-thing conn-ts-block-outer "block.outer")
(conn-ts-define-thing conn-ts-call-inner "call.inner")
(conn-ts-define-thing conn-ts-call-outer "call.outer")
(conn-ts-define-thing conn-ts-class-inner "class.inner")
(conn-ts-define-thing conn-ts-class-outer "class.outer")
(conn-ts-define-thing conn-ts-comment-inner "comment.inner")
(conn-ts-define-thing conn-ts-comment-outer "comment.outer")
(conn-ts-define-thing conn-ts-conditional-inner "conditional.inner")
(conn-ts-define-thing conn-ts-conditional-outer "conditional.outer")
(conn-ts-define-thing conn-ts-frame-inner "frame.inner")
(conn-ts-define-thing conn-ts-frame-outer "frame.outer")
(conn-ts-define-thing conn-ts-function-inner "function.inner")
(conn-ts-define-thing conn-ts-function-outer "function.outer")
(conn-ts-define-thing conn-ts-loop-inner "loop.inner")
(conn-ts-define-thing conn-ts-loop-outer "loop.outer")
(conn-ts-define-thing conn-ts-number "number.inner")
(conn-ts-define-thing conn-ts-parameter-inner "parameter.inner")
(conn-ts-define-thing conn-ts-parameter-outer "parameter.outer")
(conn-ts-define-thing conn-ts-regex-inner "regex.inner")
(conn-ts-define-thing conn-ts-regex-outer "regex.outer")
(conn-ts-define-thing conn-ts-return-inner "return.inner")
(conn-ts-define-thing conn-ts-return-outer "return.outer")
(conn-ts-define-thing conn-ts-scopename "scopename.inner")
(conn-ts-define-thing conn-ts-statement "statement.outer")

(defvar-keymap conn-ts-assignment-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "m" 'conn-ts-assignment-side-forward
  "n" 'conn-ts-assignment-side-backward
  "M" 'conn-ts-assignment-side-forward-flat
  "N" 'conn-ts-assignment-side-backward-flat
  "l" 'conn-ts-assignment-inner-forward
  "j" 'conn-ts-assignment-inner-backward
  "k" 'conn-ts-assignment-inner-forward-flat
  "i" 'conn-ts-assignment-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-assignment-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "m" 'conn-ts-assignment-side-forward
  "n" 'conn-ts-assignment-side-backward
  "M" 'conn-ts-assignment-side-forward-flat
  "N" 'conn-ts-assignment-side-backward-flat
  "l" 'conn-ts-assignment-outer-forward
  "j" 'conn-ts-assignment-outer-backward
  "k" 'conn-ts-assignment-outer-forward-flat
  "i" 'conn-ts-assignment-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-attribute-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-attribute-inner-forward
  "j" 'conn-ts-attribute-inner-backward
  "k" 'conn-ts-attribute-inner-forward-flat
  "i" 'conn-ts-attribute-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-attribute-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-attribute-outer-forward
  "j" 'conn-ts-attribute-outer-backward
  "k" 'conn-ts-attribute-outer-forward-flat
  "i" 'conn-ts-attribute-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-block-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-block-inner-forward
  "j" 'conn-ts-block-inner-backward
  "k" 'conn-ts-block-inner-forward-flat
  "i" 'conn-ts-block-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-block-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-block-outer-forward
  "j" 'conn-ts-block-outer-backward
  "k" 'conn-ts-block-outer-forward-flat
  "i" 'conn-ts-block-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-call-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-call-inner-forward
  "j" 'conn-ts-call-inner-backward
  "k" 'conn-ts-call-inner-forward-flat
  "i" 'conn-ts-call-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-call-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-call-outer-forward
  "j" 'conn-ts-call-outer-backward
  "k" 'conn-ts-call-outer-forward-flat
  "i" 'conn-ts-call-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-class-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-class-inner-forward
  "j" 'conn-ts-class-inner-backward
  "k" 'conn-ts-class-inner-forward-flat
  "i" 'conn-ts-class-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-class-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-class-outer-forward
  "j" 'conn-ts-class-outer-backward
  "k" 'conn-ts-class-outer-forward-flat
  "i" 'conn-ts-class-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-comment-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-comment-inner-forward
  "j" 'conn-ts-comment-inner-backward
  "k" 'conn-ts-comment-inner-forward-flat
  "i" 'conn-ts-comment-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-comment-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-comment-outer-forward
  "j" 'conn-ts-comment-outer-backward
  "k" 'conn-ts-comment-outer-forward-flat
  "i" 'conn-ts-comment-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-conditional-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-conditional-inner-forward
  "j" 'conn-ts-conditional-inner-backward
  "k" 'conn-ts-conditional-inner-forward-flat
  "i" 'conn-ts-conditional-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-conditional-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-conditional-outer-forward
  "j" 'conn-ts-conditional-outer-backward
  "k" 'conn-ts-conditional-outer-forward-flat
  "i" 'conn-ts-conditional-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-frame-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-frame-inner-forward
  "j" 'conn-ts-frame-inner-backward
  "k" 'conn-ts-frame-inner-forward-flat
  "i" 'conn-ts-frame-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-frame-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-frame-outer-forward
  "j" 'conn-ts-frame-outer-backward
  "k" 'conn-ts-frame-outer-forward-flat
  "i" 'conn-ts-frame-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-function-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-function-inner-forward
  "j" 'conn-ts-function-inner-backward
  "k" 'conn-ts-function-inner-forward-flat
  "i" 'conn-ts-function-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-function-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-function-outer-forward
  "j" 'conn-ts-function-outer-backward
  "k" 'conn-ts-function-outer-forward-flat
  "i" 'conn-ts-function-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-loop-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-loop-inner-forward
  "j" 'conn-ts-loop-inner-backward
  "k" 'conn-ts-loop-inner-forward-flat
  "i" 'conn-ts-loop-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-loop-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-loop-outer-forward
  "j" 'conn-ts-loop-outer-backward
  "k" 'conn-ts-loop-outer-forward-flat
  "i" 'conn-ts-loop-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-number-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-number-forward
  "j" 'conn-ts-number-backward
  "k" 'conn-ts-number-forward-flat
  "i" 'conn-ts-number-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-parameter-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-parameter-inner-forward
  "j" 'conn-ts-parameter-inner-backward
  "k" 'conn-ts-parameter-inner-forward-flat
  "i" 'conn-ts-parameter-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-parameter-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-parameter-outer-forward
  "j" 'conn-ts-parameter-outer-backward
  "k" 'conn-ts-parameter-outer-forward-flat
  "i" 'conn-ts-parameter-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-regex-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-regex-inner-forward
  "j" 'conn-ts-regex-inner-backward
  "k" 'conn-ts-regex-inner-forward-flat
  "i" 'conn-ts-regex-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-regex-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-regex-outer-forward
  "j" 'conn-ts-regex-outer-backward
  "k" 'conn-ts-regex-outer-forward-flat
  "i" 'conn-ts-regex-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-return-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-return-inner-forward
  "j" 'conn-ts-return-inner-backward
  "k" 'conn-ts-return-inner-forward-flat
  "i" 'conn-ts-return-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-return-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-return-outer-forward
  "j" 'conn-ts-return-outer-backward
  "k" 'conn-ts-return-outer-forward-flat
  "i" 'conn-ts-return-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-scopename-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-ts-scopename-forward
  "j" 'conn-ts-scopename-backward
  "k" 'conn-ts-scopename-forward-flat
  "i" 'conn-ts-scopename-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-ts-things-mode-map
  "<conn-thing-map> i w l" 'conn-ts-assignment-inner-forward
  "<conn-thing-map> i w j" 'conn-ts-assignment-inner-backward
  "<conn-thing-map> i w k" 'conn-ts-assignment-inner-forward-flat
  "<conn-thing-map> i w i" 'conn-ts-assignment-inner-backward-flat
  "<conn-thing-map> w m" 'conn-ts-assignment-side-forward
  "<conn-thing-map> w n" 'conn-ts-assignment-side-backward
  "<conn-thing-map> w l" 'conn-ts-assignment-outer-forward
  "<conn-thing-map> w j" 'conn-ts-assignment-outer-backward
  "<conn-thing-map> w k" 'conn-ts-assignment-outer-forward-flat
  "<conn-thing-map> w i" 'conn-ts-assignment-outer-backward-flat
  "<conn-thing-map> i @ l" 'conn-ts-attribute-inner-forward
  "<conn-thing-map> i @ j" 'conn-ts-attribute-inner-backward
  "<conn-thing-map> i @ k" 'conn-ts-attribute-inner-forward-flat
  "<conn-thing-map> i @ i" 'conn-ts-attribute-inner-backward-flat
  "<conn-thing-map> @ l" 'conn-ts-attribute-outer-forward
  "<conn-thing-map> @ j" 'conn-ts-attribute-outer-backward
  "<conn-thing-map> @ k" 'conn-ts-attribute-outer-forward-flat
  "<conn-thing-map> @ i" 'conn-ts-attribute-outer-backward-flat
  "<conn-thing-map> i b l" 'conn-ts-block-inner-forward
  "<conn-thing-map> i b j" 'conn-ts-block-inner-backward
  "<conn-thing-map> i b k" 'conn-ts-block-inner-forward-flat
  "<conn-thing-map> i b i" 'conn-ts-block-inner-backward-flat
  "<conn-thing-map> b l" 'conn-ts-block-outer-forward
  "<conn-thing-map> b j" 'conn-ts-block-outer-backward
  "<conn-thing-map> b k" 'conn-ts-block-outer-forward-flat
  "<conn-thing-map> b i" 'conn-ts-block-outer-backward-flat
  "<conn-thing-map> i . l" 'conn-ts-call-inner-forward
  "<conn-thing-map> i . j" 'conn-ts-call-inner-backward
  "<conn-thing-map> i . k" 'conn-ts-call-inner-forward-flat
  "<conn-thing-map> i . i" 'conn-ts-call-inner-backward-flat
  "<conn-thing-map> . l" 'conn-ts-call-outer-forward
  "<conn-thing-map> . j" 'conn-ts-call-outer-backward
  "<conn-thing-map> . k" 'conn-ts-call-outer-forward-flat
  "<conn-thing-map> . i" 'conn-ts-call-outer-backward-flat
  "<conn-thing-map> i C l" 'conn-ts-class-inner-forward
  "<conn-thing-map> i C j" 'conn-ts-class-inner-backward
  "<conn-thing-map> i C k" 'conn-ts-class-inner-forward-flat
  "<conn-thing-map> i C i" 'conn-ts-class-inner-backward-flat
  "<conn-thing-map> C l" 'conn-ts-class-outer-forward
  "<conn-thing-map> C j" 'conn-ts-class-outer-backward
  "<conn-thing-map> C k" 'conn-ts-class-outer-forward-flat
  "<conn-thing-map> C i" 'conn-ts-class-outer-backward-flat
  "<conn-thing-map> i c l" 'conn-ts-comment-inner-forward
  "<conn-thing-map> i c j" 'conn-ts-comment-inner-backward
  "<conn-thing-map> i c k" 'conn-ts-comment-inner-forward-flat
  "<conn-thing-map> i c i" 'conn-ts-comment-inner-backward-flat
  "<conn-thing-map> c l" 'conn-ts-comment-outer-forward
  "<conn-thing-map> c j" 'conn-ts-comment-outer-backward
  "<conn-thing-map> c k" 'conn-ts-comment-outer-forward-flat
  "<conn-thing-map> c i" 'conn-ts-comment-outer-backward-flat
  "<conn-thing-map> i q l" 'conn-ts-conditional-inner-forward
  "<conn-thing-map> i q j" 'conn-ts-conditional-inner-backward
  "<conn-thing-map> i q k" 'conn-ts-conditional-inner-forward-flat
  "<conn-thing-map> i q i" 'conn-ts-conditional-inner-backward-flat
  "<conn-thing-map> q l" 'conn-ts-conditional-outer-forward
  "<conn-thing-map> q j" 'conn-ts-conditional-outer-backward
  "<conn-thing-map> q k" 'conn-ts-conditional-outer-forward-flat
  "<conn-thing-map> q i" 'conn-ts-conditional-outer-backward-flat
  "<conn-thing-map> i [ l" 'conn-ts-frame-inner-forward
  "<conn-thing-map> i [ j" 'conn-ts-frame-inner-backward
  "<conn-thing-map> i [ k" 'conn-ts-frame-inner-forward-flat
  "<conn-thing-map> i [ i" 'conn-ts-frame-inner-backward-flat
  "<conn-thing-map> [ l" 'conn-ts-frame-outer-forward
  "<conn-thing-map> [ j" 'conn-ts-frame-outer-backward
  "<conn-thing-map> [ k" 'conn-ts-frame-outer-forward-flat
  "<conn-thing-map> [ i" 'conn-ts-frame-outer-backward-flat
  "<conn-thing-map> i f l" 'conn-ts-function-inner-forward
  "<conn-thing-map> i f j" 'conn-ts-function-inner-backward
  "<conn-thing-map> i f k" 'conn-ts-function-inner-forward-flat
  "<conn-thing-map> i f i" 'conn-ts-function-inner-backward-flat
  "<conn-thing-map> f l" 'conn-ts-function-outer-forward
  "<conn-thing-map> f j" 'conn-ts-function-outer-backward
  "<conn-thing-map> f k" 'conn-ts-function-outer-forward-flat
  "<conn-thing-map> f i" 'conn-ts-function-outer-backward-flat
  "<conn-thing-map> i r l" 'conn-ts-loop-inner-forward
  "<conn-thing-map> i r j" 'conn-ts-loop-inner-backward
  "<conn-thing-map> i r k" 'conn-ts-loop-inner-forward-flat
  "<conn-thing-map> i r i" 'conn-ts-loop-inner-backward-flat
  "<conn-thing-map> r l" 'conn-ts-loop-outer-forward
  "<conn-thing-map> r j" 'conn-ts-loop-outer-backward
  "<conn-thing-map> r k" 'conn-ts-loop-outer-forward-flat
  "<conn-thing-map> r i" 'conn-ts-loop-outer-backward-flat
  "<conn-thing-map> n l" 'conn-ts-number-forward
  "<conn-thing-map> n j" 'conn-ts-number-backward
  "<conn-thing-map> i d l" 'conn-ts-parameter-inner-forward
  "<conn-thing-map> i d j" 'conn-ts-parameter-inner-backward
  "<conn-thing-map> i d k" 'conn-ts-parameter-inner-forward-flat
  "<conn-thing-map> i d i" 'conn-ts-parameter-inner-backward-flat
  "<conn-thing-map> d l" 'conn-ts-parameter-outer-forward
  "<conn-thing-map> d j" 'conn-ts-parameter-outer-backward
  "<conn-thing-map> d k" 'conn-ts-parameter-outer-forward-flat
  "<conn-thing-map> d i" 'conn-ts-parameter-outer-backward-flat
  "<conn-thing-map> i x l" 'conn-ts-regex-inner-forward
  "<conn-thing-map> i x j" 'conn-ts-regex-inner-backward
  "<conn-thing-map> i x k" 'conn-ts-regex-inner-forward-flat
  "<conn-thing-map> i x i" 'conn-ts-regex-inner-backward-flat
  "<conn-thing-map> x l" 'conn-ts-regex-outer-forward
  "<conn-thing-map> x j" 'conn-ts-regex-outer-backward
  "<conn-thing-map> x k" 'conn-ts-regex-outer-forward-flat
  "<conn-thing-map> x i" 'conn-ts-regex-outer-backward-flat
  "<conn-thing-map> i t l" 'conn-ts-return-inner-forward
  "<conn-thing-map> i t j" 'conn-ts-return-inner-backward
  "<conn-thing-map> i t k" 'conn-ts-return-inner-forward-flat
  "<conn-thing-map> i t i" 'conn-ts-return-inner-backward-flat
  "<conn-thing-map> t l" 'conn-ts-return-outer-forward
  "<conn-thing-map> t j" 'conn-ts-return-outer-backward
  "<conn-thing-map> t k" 'conn-ts-return-outer-forward-flat
  "<conn-thing-map> t i" 'conn-ts-return-outer-backward-flat
  "<conn-thing-map> S l" 'conn-ts-scopename-forward
  "<conn-thing-map> S j" 'conn-ts-scopename-backward)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-targets-state 'conn-ts-things-mode)
  "w" (conn-anonymous-thing
        'conn-ts-thing
        :default-action ( :method (_) (conn-dispatch-ts-goto))
        :pretty-print ( :method (_) "ts-all-things")
        :target-finder ( :method (_self _arg)
                         (conn-ts-query-targets
                          :things conn-ts-all-query-things))
        :bounds-op ( :method (_self _arg)
                     (conn-ts-select-expansion
                      (lambda ()
                        (conn-ts-filter-captures
                         conn-ts-all-query-things
                         (conn-ts-capture (point) (1+ (point)))
                         (lambda (bd)
                           (<= (car bd) (point) (cdr bd)))))
                      'conn-ts-thing)))
  "W" (conn-anonymous-thing
        'conn-ts-thing
        :default-action ( :method (_) (conn-dispatch-ts-goto))
        :pretty-print ( :method (_) "ts-all-parents")
        :target-finder ( :method (_self _arg)
                         (conn-ts-query-targets
                          :things conn-ts-parent-things
                          :region-predicate (lambda (beg end)
                                              (and (<= beg (point))
                                                   (<= (point) end)))))
        :bounds-op ( :method (_self _arg)
                     (conn-ts-select-expansion
                      (lambda ()
                        (conn-ts-filter-captures
                         conn-ts-parent-things
                         (conn-ts-capture (point) (1+ (point)))
                         (lambda (bd)
                           (<= (car bd) (point) (cdr bd)))))
                      'conn-ts-thing)))
  "n" (conn-anonymous-thing
        'sexp
        :default-action ( :method (_) (conn-dispatch-ts-goto))
        :pretty-print ( :method (_) "ts-all-sexps")
        :target-finder ( :method (_self _arg)
                         (conn-ts-all-things :thing 'sexp))
        :bounds-op ( :method (_self _arg)
                     (conn-ts-select-expansion
                      (lambda ()
                        (mapcar
                         (pcase-lambda (`(,_ . ,node))
                           (cons (treesit-node-type node)
                                 (cons (treesit-node-start node)
                                       (treesit-node-end node))))
                         (treesit-query-capture
                          (treesit-language-at (point))
                          (conn-ts--thing-node-query 'sexp)
                          (point) (1+ (point)))))
                      'sexp))))

;;;###autoload
(define-minor-mode conn-ts-things-mode
  "Minor mode for conn-ts thing bindings."
  :lighter nil)

(provide 'conn-tree-sitter)
