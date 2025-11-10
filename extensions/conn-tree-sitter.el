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
(require 'mule-util)

(cl-defstruct (conn-ts--thing
               (:constructor conn--make-ts-thing))
  (groups nil :type (or cons symbol) :read-only t))

(defvar conn-ts--chunk-size 10000)

(defconst conn-ts--queries (make-hash-table :test 'eq))
(defconst conn-ts--compiled-query-cache (make-hash-table :test 'eq))
(defvar conn-ts--custom-queries nil)
(defvar conn-ts--query-parents nil)

;; TODO: vendor the nvim-treesitter-textobjects queries
(defvar conn-ts-query-dir nil)

(defvar conn-ts--symbol-tick 0)

(defconst conn-ts--predicate-symbol-cache
  (make-hash-table :test #'equal))

(defconst conn-ts--offset-symbol-cache
  (make-hash-table :test #'equal))

(defun conn-ts--parse-query (query)
  (cl-labels
      ((get-alias (form func)
         (with-memoization (gethash form conn-ts--predicate-symbol-cache)
           (let ((sym (intern
                       (concat "conn-ts--anonymous-predicate"
                               (number-to-string
                                (cl-incf conn-ts--symbol-tick))))))
             (fset sym func)
             sym)))
       (get-offset (capture &optional brow bcol erow ecol)
         (with-memoization (gethash (list brow bcol erow ecol)
                                    conn-ts--offset-symbol-cache)
           (let ((sym (intern (concat (substring (symbol-name capture) 1)
                                      "--offset"
                                      (number-to-string
                                       (cl-incf conn-ts--symbol-tick)))))
                 (capture-sym (intern (substring (symbol-name capture) 1))))
             (fset sym (pcase-lambda ((and cons `(,_sym . ,node)))
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
             (put sym :conn-ts--offset-sym t)
             (intern (concat "@" (symbol-name sym))))))
       (sub (alist exp)
         (if (null alist)
             (list exp)
           (pcase exp
             (`(,(pred keywordp) . ,_)
              (list exp))
             ((pred consp)
              (list (mapcan (lambda (e) (sub alist e)) exp)))
             ((pred vectorp)
              `([,@(mapcan (lambda (e) (sub alist e))
                           (append exp nil))]))
             (_ (or (copy-sequence (alist-get exp alist))
                    (list exp))))))
       (walk (exp)
         (cl-loop
          with range-sub = nil
          with offset-sub = nil
          for e in exp
          nconc (pcase e
                  (`(:any-of? ,capt . ,pats)
                   `((:match? ,(regexp-opt pats) ,capt)))
                  (`(:offset! ,capt . ,offsets)
                   (push (apply #'get-offset capt offsets)
                         (alist-get capt offset-sub))
                   nil)
                  (`(:lua-match ,capt ,pat)
                   ;; All #lua-match? directives I found work fine as
                   ;; regexps, I think
                   `((:match? ,pat ,capt)))
                  ((and form `(:not-lua-match? ,capt ,pat))
                   `((:pred ,(get-alias
                              form
                              (lambda (node)
                                (save-excursion
                                  (save-match-data
                                    (goto-char (treesit-node-start node))
                                    (not
                                     (eql (treesit-node-end node)
                                          (re-search-forward
                                           pat (treesit-node-end node) nil)))))))
                            ,capt)))
                  (`(:make-range! ,capture ,beg ,end)
                   (push (intern (concat "@" capture "_start"))
                         (alist-get beg range-sub))
                   (cl-pushnew beg (alist-get beg range-sub))
                   (push (intern (concat "@" capture "_end"))
                         (alist-get end range-sub))
                   (cl-pushnew end (alist-get end range-sub))
                   nil)
                  ((and form `(:not-kind-eq? ,capt ,type))
                   `((:pred ,(get-alias
                              form
                              (lambda (node)
                                (not (equal (treesit-node-type node)
                                            type))))
                            ,capt)))
                  ((and form `(:not-any-of? ,capt . ,strs))
                   `((:pred ,(get-alias
                              form
                              (let ((re (regexp-opt strs)))
                                (lambda (node)
                                  (save-excursion
                                    (save-match-data
                                      (goto-char (treesit-node-start node))
                                      (not
                                       (eql (treesit-node-end node)
                                            (re-search-forward
                                             re (treesit-node-end node) nil))))))))
                            ,capt)))
                  (`(,(pred keywordp) . ,_)
                   nil)
                  ((pred vectorp)
                   `([,@(walk (append e nil))]))
                  ((guard (not (consp e)))
                   (list e))
                  (_ (list (walk e))))
          into newexp
          finally return
          (thread-last
            newexp
            (mapcan (lambda (e) (sub range-sub e)))
            (mapcan (lambda (e) (sub offset-sub e)))))))
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
           (push curr queries)
           (setf curr (list sexp)))
          (_
           (push sexp (cdr curr)))))
      (when curr (push curr queries))
      (let ((qs (mapcar #'conn-ts--parse-query
                        (cdr (nreverse queries)))))
        qs))))

(defun conn-ts--normalize-capture (capture)
  (when (treesit-node-p (cdr capture))
    (let ((name (car capture)))
      (if (get name :conn-ts--offset-sym)
          (funcall name capture)
        (setf (cdr capture)
              (cons (treesit-node-start (cdr capture))
                    (treesit-node-end (cdr capture))))))))

(defun conn-ts--get-language-query (language)
  (cl-labels
      ((parse-inherits (top-level)
         (let (inherits)
           (when (re-search-forward "; inherits:? " nil t) ;?
             (while (re-search-forward
                     (rx (or (seq "(" (group (not ")")) ")")
                             (seq (group (not ",")))))
                     (pos-eol) t)
               (if (match-beginning 1)
                   (when top-level
                     (push (buffer-substring (match-beginning 1)
                                             (match-end 1))
                           inherits))
                 (push (buffer-substring (match-beginning 2)
                                         (match-end 2))
                       inherits))))
           inherits))
       (get-query (language &optional top-level)
         (with-memoization (gethash language conn-ts--queries)
           (let ((filename (concat conn-ts--query-dir
                                   (symbol-name language)
                                   "/textobjects.scm")))
             (when (file-exists-p filename)
               (with-temp-buffer
                 (insert-file-contents-literally filename)
                 (conn-ts--parse-scm-buffer)
                 (let ((inherits (parse-inherits top-level)))
                   (mapc #'get-query inherits)
                   (setf (alist-get language conn-ts--query-parents)
                         (cl-loop for p in inherits
                                  append (assoc p conn-ts--query-parents)))
                   (append
                    (conn-ts--parse-scm-buffer)
                    (cl-loop for lang in inherits
                             append (gethash lang conn-ts--queries))))))))))
    (get-query language t)))

(defun conn-ts--clear-query-cache ()
  (clrhash conn-ts--queries)
  (clrhash conn-ts--compiled-query-cache))

(defun conn-ts-add-custom-query (language query)
  (conn-ts--clear-query-cache)
  (push query (alist-get language conn-ts--custom-queries)))

(defun conn-ts--get-query ()
  (unless (treesit-language-at (point))
    (error "No treesitter language at point"))
  (with-memoization (gethash (treesit-language-at (point))
                             conn-ts--compiled-query-cache)
    (treesit-query-compile
     (treesit-language-at (point))
     (conn-ts--get-language-query (treesit-language-at (point)))
     'eager)))

(defun conn-ts--query-capture (node query start end)
  (mapcar #'nreverse (treesit-query-capture node query start end nil t)))

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

(defun conn-ts--filter-captures (thing captures &optional reverse)
  (let ((groups (conn-ts--get-thing-groups thing))
        (regions nil))
    (dolist (capture captures)
      (mapc #'conn-ts--normalize-capture capture)
      (pcase-dolist (`(,group ,tbeg . ,tend) groups)
        (if-let* ((beg (alist-get tbeg capture)))
            (when-let* ((end (alist-get tend capture)))
              (push (cons (car beg) (cdr end))
                    regions))
          (when-let* ((node (alist-get group capture)))
            (push node regions)))))
    (if reverse regions (nreverse regions))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-ts-thing)) arg
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
                captures (conn-ts--filter-captures
                          cmd
                          (conn-ts--query-capture (treesit-buffer-root-node)
                                                  (conn-ts--get-query)
                                                  beg end)
                          (< arg 0))
                beg end)
          (if flat
              (progn
                (cl-callf sort captures
                  :key #'car
                  :reverse (< arg 0)
                  :in-place t)
                (dolist (node captures)
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
                         (if (>= at (cdr n)) (cdr n) (car n)))
                     (lambda (n)
                       (if (<= at (car n)) (car n) (cdr n))))
              :reverse (< arg 0)
              :in-place t)
            (dolist (node captures)
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

(defvar conn-ts-multi-always-prompt t)

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

(defvar-local conn-ts-all-things
  conn-ts-parent-things)

(defclass conn-ts-node-targets (conn-dispatch-target-window-predicate)
  ((things :initarg :things)
   (window-predicate
    :initform (lambda (win) (eq win (selected-window))))
   (region-predicate :initarg :region-predicate)))

(cl-defmethod conn-target-finder-update ((state conn-ts-node-targets))
  (cl-macrolet ((make-ts-target (thing beg end)
                  `(conn-make-target-overlay
                    ,beg 0
                    :thing (conn-anonymous-thing
                             'conn-ts-thing
                             :things (list ,thing)
                             :bounds-op ( :method (self _arg)
                                          (conn-with-dispatch-suspended
                                            (conn-multi-thing-select
                                             self
                                             conn-ts-multi-always-prompt))))
                    :properties (list 'unique-bounds (list (cons ,beg ,end))))))
    (let ((region-pred (ignore-error unbound-slot
                         (oref state region-predicate)))
          (things (oref state things))
          (truncate-string-ellipsis nil))
      (dolist (win (conn--get-target-windows))
        (with-selected-window win
          (pcase-dolist (`(,vbeg . ,vend)
                         (conn--visible-regions (window-start)
                                                (window-end)))
            (let* ((captures
                    (conn-ts--query-capture (treesit-buffer-root-node)
                                            (conn-ts--get-query)
                                            vbeg vend))
                   (all-captures nil))
              (dolist (thing things)
                (setf (alist-get thing all-captures)
                      (conn-ts--filter-captures thing captures)))
              (pcase-dolist (`(,thing . ,captures) all-captures)
                (pcase-dolist (`(,beg . ,end) captures)
                  (when (and (<= (window-start) beg (window-end))
                             (if region-pred (funcall region-pred beg end) t))
                    (if-let* ((ov (car (conn--overlays-in-of-type
                                        beg (1+ beg) 'conn-target-overlay))))
                        (progn
                          (cl-pushnew thing (conn-anonymous-thing-property
                                             (overlay-get ov 'thing)
                                             :things))
                          (when (length=
                                 (cl-pushnew (cons beg end)
                                             (overlay-get ov 'unique-bounds)
                                             :test #'equal)
                                 2)
                            (overlay-put ov 'label-suffix (truncate-string-ellipsis))))
                      (make-ts-target thing beg end)))))))))))
  (cl-call-next-method))

(cl-defmethod conn-get-target-finder ((cmd (conn-thing conn-ts-thing))
                                      _arg)
  (conn-ts-node-targets
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
     (goto-char (if (> arg 0) end beg))
     (unless (region-active-p)
       (conn--push-ephemeral-mark (if (> arg 0) beg end))))
    (_ (signal 'scan-error
               (list (format-message "No more %S to move across" thing)
                     (point) (point))))))

(defun conn-ts--thing-forward-flat (thing arg)
  (interactive "p")
  (pcase (thread-first
           (conn-bounds-of thing arg :flat t)
           (conn-bounds-get :subregions)
           last car)
    ((conn-bounds `(,beg . ,end))
     (goto-char beg)
     (conn--push-ephemeral-mark end))
    (_ (signal 'scan-error
               (list (format-message "No more %S to move across" thing)
                     (point) (point))))))

(defmacro conn-ts-define-thing (name group)
  (declare (indent defun))
  (let ((forward-cmd (intern (format "%s-forward" name)))
        (forward-flat-cmd (intern (format "%s-forward-flat" name)))
        (backward-cmd (intern (format "%s-backward" name)))
        (backward-flat-cmd (intern (format "%s-backward-flat" name)))
        (groups (cl-loop
                 for group in (ensure-list group)
                 collect (cons (intern group)
                               (cons (intern (format "%s_start" group))
                                     (intern (format "%s_end" group)))))))
    `(progn
       (put ',name :conn-ts-thing (conn--make-ts-thing :groups ',groups))

       (conn-register-thing ',name
                            :parent 'conn-ts-thing
                            :forward-op ',forward-cmd)

       (defun ,forward-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward ',name arg))
       (put ',forward-cmd 'repeat-check-key 'no)

       (defun ,backward-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward ',name (- arg)))
       (put ',backward-cmd 'repeat-check-key 'no)

       (defun ,forward-flat-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward-flat ',name arg))
       (put ',forward-flat-cmd 'repeat-check-key 'no)

       (defun ,backward-flat-cmd (&optional arg)
         (interactive "p")
         (conn-ts--thing-forward-flat ',name (- arg)))
       (put ',backward-flat-cmd 'repeat-check-key 'no)

       (conn-register-thing-commands
        ',name 'ignore
        ',forward-cmd
        ',backward-cmd
        ',forward-flat-cmd
        ',backward-flat-cmd))))

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
        :pretty-print ( :method (_) "ts-all-things")
        :target-finder ( :method (_self _arg)
                         (conn-ts-node-targets
                          :things conn-ts-all-things)))
  "W" (conn-anonymous-thing
        'conn-ts-thing
        :pretty-print ( :method (_) "ts-all-parents")
        :target-finder ( :method (_self _arg)
                         (conn-ts-node-targets
                          :things conn-ts-parent-things
                          :region-predicate (lambda (beg end)
                                              (and (<= beg (point))
                                                   (<= (point) end)))))))

;; fix ts comment thing overriding ours
(conn-register-thing
 'comment
 :bounds-op (lambda ()
              (if (conn--point-in-comment-p)
                  (cons (save-excursion
                          (while (and (conn--point-in-comment-p)
                                      (not (eobp)))
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r"))
                          (skip-chars-backward " \t\n\r")
                          (point))
                        (save-excursion
                          (while (conn--point-in-comment-p)
                            (forward-char -1)
                            (skip-chars-backward " \t\n\r"))
                          (skip-chars-forward " \t\n\r")
                          (unless (conn--point-in-comment-p)
                            (forward-char 1))
                          (point)))
                (error "Point not in comment"))))

;;;###autoload
(define-minor-mode conn-ts-things-mode
  "Minor mode for conn-ts thing bindings.")

(provide 'conn-tree-sitter)
