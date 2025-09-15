;;; conn-utils.el --- Conn Utilities -*- lexical-binding: t -*-
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

;;; Commentary:

;;; Code:

(require 'compat)
(require 'conn-vars)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(autoload 'make-vtable "vtable")

;;;; Utilities

(defconst conn--key-missing (gensym "key-missing"))

(defmacro conn-protected-let* (varlist &rest body)
  "Bind variables according to VARLIST then eval body as in `let*'.

In addition to what `let*' accepts, each element of VARLIST may also be
of the form (SYMBOL VALUEFORM . CLEANUP-FORMS), which binds SYMBOL to
VALUEFORM and if BODY exits non-locally runs CLEANUP-FORMS.

CLEANUP-FORMS are run in reverse order of their appearance in VARLIST."
  (declare (indent 1))
  (cl-with-gensyms (success)
    (named-let protect ((binding (car (last varlist)))
                        (rest (reverse (cons success (butlast varlist))))
                        (body `(prog1 ,(macroexp-progn body)
                                 (setq ,success t))))
      (pcase binding
        ('nil body)
        ((and `(,var ,val . ,cleanup) (guard cleanup))
         (protect (car rest) (cdr rest)
                  `(let* ((,var ,val))
                     (unwind-protect
                         ,body
                       (unless ,success ,@cleanup)))))
        (_ (protect (car rest) (cdr rest)
                    (macroexp-let* (list binding) body)))))))

(defmacro conn-anaphoricate (name lambda)
  (declare (indent 1))
  `(letrec ((,name ,lambda)) ,name))

(defmacro conn-with-overriding-map (keymap &rest body)
  (declare (indent 1))
  (cl-once-only (keymap)
    `(progn
       (if ,keymap (internal-push-keymap ,keymap 'overriding-terminal-local-map))
       (unwind-protect
           ,(macroexp-progn body)
         (internal-pop-keymap ,keymap 'overriding-terminal-local-map)))))

(defmacro conn--flip-last (arg1 fn &rest args)
  `(,fn ,@args ,arg1))

(defmacro conn--flip-first (fn &rest args)
  `(,fn ,@(last args) ,@(butlast args)))

(defmacro conn--compat-callf (func place &rest args)
  (declare (indent 2) (debug (cl-function place &rest form)))
  (gv-letplace (getter setter) place
    (let* ((rargs (cons getter args)))
      (funcall setter `(compat-call ,func ,@rargs)))))

(defmacro conn--compat-callf2 (func arg1 place &rest args)
  (declare (indent 3) (debug (cl-function form place &rest form)))
  (if (and (cl--safe-expr-p arg1) (cl--simple-expr-p place) (symbolp func))
      `(setf ,place (compat-call ,func ,arg1 ,place ,@args))
    (macroexp-let2 nil a1 arg1
      (gv-letplace (getter setter) place
        (let* ((rargs (cl-list* a1 getter args)))
          (funcall setter `(compat-call ,func ,@rargs)))))))

;; From repeat-mode
(defun conn--command-property (propname)
  "Return the value of the current commands PROPNAME property."
  (or (and (symbolp this-command)
           (get this-command propname))
      (and (symbolp real-this-command)
           (get real-this-command propname))))

;; We need string-pixel-width from emacs 31 since it accounts for face
;; remapping
(static-if (<= 31 emacs-major-version)
    (defalias 'conn--string-pixel-width 'string-pixel-width)
  (defun conn--string-pixel-width (string &optional buffer)
    (declare (important-return-value t))
    (if (zerop (length string))
        0
      ;; Keeping a work buffer around is more efficient than creating a
      ;; new temporary buffer.
      (with-current-buffer (get-buffer-create " *string-pixel-width*")
        ;; Setup current buffer to correctly compute pixel width.
        (when buffer
          (dolist (v '(face-remapping-alist
                       char-property-alias-alist
                       default-text-properties))
            (if (local-variable-p v buffer)
                (set (make-local-variable v)
                     (buffer-local-value v buffer)))))
        ;; Avoid deactivating the region as side effect.
        (delete-region (point-min) (point-max))
        (insert string)
        ;; If `display-line-numbers' is enabled in internal
        ;; buffers (e.g. globally), it breaks width calculation
        ;; (bug#59311).  Disable `line-prefix' and `wrap-prefix',
        ;; for the same reason.
        (add-text-properties
         (point-min) (point-max)
         '(display-line-numbers-disable t line-prefix "" wrap-prefix ""))
        (car (buffer-text-pixel-size nil nil t))))))

(defun conn--open-invisible (beg end)
  (catch 'return
    (cl-loop for pt = beg then (next-single-property-change
                                pt 'invisible nil end)
             while (and pt (< pt end))
             when (invisible-p (get-text-property pt 'invisible))
             do (throw 'return nil))
    (let (restore)
      (dolist (ov (overlays-in beg end))
        (let ((inv (overlay-get ov 'invisible)))
          (when (invisible-p inv)
            (unless (overlay-get ov 'isearch-open-invisible)
              (throw 'return (mapc #'funcall restore)))
            (push
             (if-let* ((fun (overlay-get ov 'isearch-open-invisible-temporary)))
                 (progn
                   (funcall fun ov nil)
                   (lambda () (funcall fun ov t)))
               (overlay-put ov 'invisible nil)
               (lambda () (overlay-put ov 'invisible inv)))
             restore))))
      (or restore t))))


;;;;; Buffer Properties

(defvar-local conn--buffer-properties nil)

(define-inline conn-get-buffer-property (property &optional buffer default)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (or (plist-get (buffer-local-value 'conn--buffer-properties
                                      (or ,buffer (current-buffer)))
                  ,property)
       ,default)))

(gv-define-setter conn-get-buffer-property (value property &optional buffer _default)
  `(conn-set-buffer-property ,property ,value ,buffer))

(defun conn-set-buffer-property (property value &optional buffer)
  (setf (plist-get (buffer-local-value 'conn--buffer-properties
                                       (or buffer (current-buffer)))
                   property)
        value))

(defun conn-unset-buffer-property (property &optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (cl-callf2 assq-delete-all
        property (buffer-local-value 'conn--buffer-properties buffer))))


;;;;; Rings

(cl-defstruct (conn-ring
               (:constructor conn-make-ring (capacity &key cleanup copier))
               (:copier conn--copy-ring))
  "A ring that removes elements in least recently visited order."
  (list nil :type list)
  (history nil :type list)
  (capacity 0 :type integer)
  (cleanup 'ignore :type function)
  (copier 'identity :type function))

(define-inline conn-ring--visit (ring item)
  (inline-quote
   (cl-callf thread-last (conn-ring-history ,ring)
     (cl-delete ,item ) (cons ,item))))

(defun conn-copy-ring (ring)
  (when (conn-ring-p ring)
    (let ((new-ring (conn--copy-ring ring))
          (copier (conn-ring-copier ring)))
      (setf (conn-ring-list new-ring)
            (cl-loop for elem in (conn-ring-list new-ring)
                     collect (funcall copier elem))
            (conn-ring-history new-ring)
            (cl-loop with old-list = (conn-ring-list ring)
                     with new-list = (conn-ring-list new-ring)
                     for elem in (conn-ring-history new-ring)
                     for pos = (seq-position old-list elem)
                     when pos collect (nth pos new-list)))
      new-ring)))

(defun conn-ring-insert-front (ring item)
  "Insert ITEM into front of RING."
  (cl-callf thread-last (conn-ring-list ring) (delq item) (cons item))
  (conn-ring--visit ring item)
  (when-let* ((old (drop (conn-ring-capacity ring) (conn-ring-history ring))))
    (cl-callf2 take (conn-ring-capacity ring) (conn-ring-history ring))
    (dolist (o old)
      (cl-callf2 delq o (conn-ring-list ring))
      (when-let* ((cleanup (conn-ring-cleanup ring)))
        (funcall cleanup o)))))

(defun conn-ring-insert-back (ring item)
  "Insert ITEM into back of RING."
  (conn-ring-insert-front ring item)
  (conn-ring-rotate-backward ring))

(defun conn-ring-rotate-forward (ring)
  "Rotate ring forward.

Takes (1 2 3 4) to (2 3 4 1)."
  (let ((head (car (cl-callf thread-last (conn-ring-list ring)
                     car list (nconc (cdr (conn-ring-list ring)))))))
    (conn-ring--visit ring head)
    head))

(defun conn-ring-rotate-backward (ring)
  "Rotate ring backward.

Takes (1 2 3 4) to (4 1 2 3)."
  (let ((head (car (cl-callf thread-last (conn-ring-list ring)
                     butlast (nconc (last (conn-ring-list ring)))))))
    (conn-ring--visit ring head)
    head))

(defun conn-ring-head (ring)
  "Return the front element of RING.

If ring is (1 2 3 4) 1 would be returned."
  (declare (side-effect-free t)
           (important-return-value t))
  (car (conn-ring-list ring)))

(defun conn-ring-tail (ring)
  "Return the back element of RING.

If ring is (1 2 3 4) 4 would be returned."
  (declare (side-effect-free t)
           (important-return-value t))
  (car (last (conn-ring-list ring))))

(defun conn-ring-delq (elem ring)
  (cl-callf2 delq elem (conn-ring-list ring))
  (cl-callf2 delq elem (conn-ring-history ring))
  (when-let* ((cleanup (conn-ring-cleanup ring)))
    (funcall cleanup elem)))

(defun conn-ring-remove (elem ring &optional pred)
  (cl-loop with pred = (or pred #'equal)
           for l in (conn-ring-list ring)
           for h in (conn-ring-history ring)
           if (funcall pred l elem) collect l into remove
           else collect l into list
           unless (funcall pred h elem) collect h into hist
           finally (progn
                     (setf (conn-ring-list ring) list
                           (conn-ring-history ring) hist)
                     (mapc (conn-ring-cleanup ring) remove))))


;;;;; Keymap Utils

(defmacro conn--without-conn-maps (&rest body)
  "Run BODY without any state, mode, or local maps active."
  (declare (debug (body))
           (indent 0))
  `(let ((emulation-mode-map-alists
          (seq-difference emulation-mode-map-alists
                          '(conn--minor-mode-maps
                            conn--major-mode-map
                            conn--state-map)
                          #'eq)))
     ,(macroexp-progn body)))

(defvar conn-demap-key
  `(menu-item
    "Demap key"
    nil
    :filter ,(lambda (_real-binding)
               (conn--without-conn-maps
                 (key-binding (vector last-input-event) t)))))

(defmacro conn-remap-key (from-keys &optional without-conn-maps no-accept-default)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  (let ((accept-default (if (macroexp-const-p no-accept-default)
                            (and no-accept-default t)
                          `(not ,no-accept-default)))
        (from-keys (if (stringp from-keys)
                       (key-parse from-keys)
                     from-keys)))
    `(list 'menu-item
           (format "Remap %s" ,from-keys)
           nil
           :filter (lambda (_real-binding)
                     ,(if (macroexp-const-p without-conn-maps)
                          (if without-conn-maps
                              `(conn--without-conn-maps
                                 (key-binding ,from-keys ,accept-default))
                            `(key-binding ,from-keys ,accept-default))
                        `(if ,without-conn-maps
                             (conn--without-conn-maps
                               (key-binding ,from-keys ,accept-default))
                           (key-binding ,from-keys ,accept-default)))))))

(defvar conn--remap-keymaps nil)

(eval-and-compile
  (defun conn--define-remap-keymap (description keys)
    (let ((keys (cl-loop for key in keys
                         if (stringp key) collect (key-parse key)
                         else collect key)))
      `(list 'menu-item
             ',description
             nil
             :filter (lambda (_real-binding)
                       ,(let (maps)
                          (dolist (key keys)
                            (pcase key
                              (`(:without-conn-maps ,key)
                               (if (stringp key) (setq key (key-parse key)))
                               (push `(conn--without-conn-maps
                                        (key-binding ,key t))
                                     maps))
                              (key
                               (if (stringp key) (setq key (key-parse key)))
                               (push `(key-binding ,key t) maps))))
                          `(make-composed-keymap
                            (delq nil (list ,@(nreverse maps))))))))))

(defmacro conn-define-remap-keymap (name description &rest keys)
  (declare (indent 2))
  `(progn
     (cl-pushnew ',name conn--remap-keymaps)
     (defvar ,name
       ,(conn--define-remap-keymap description keys)
       ,description)))

;; A hack but it allows the binding display in quick ref looks nice
(defmacro conn--where-is-with-remaps (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn
         (cl-loop for remap in conn--remap-keymaps
                  for val = (symbol-value remap)
                  do (setf (nth 2 val) (keymap--menu-item-binding val)))
         ,@body)
     (cl-loop for remap in conn--remap-keymaps
              do (setf (nth 2 (symbol-value remap)) nil))))

(conn-define-remap-keymap conn-search-remap
    "Conn Search Map"
  [conn-search-map]
  (:without-conn-maps "M-s"))

(conn-define-remap-keymap conn-goto-remap
    "Conn Search Map"
  [conn-goto-map]
  (:without-conn-maps "M-g"))

(conn-define-remap-keymap conn-thing-remap
    "Conn Search Map"
  [conn-thing-map])

(conn-define-remap-keymap conn-region-remap
    "Conn Search Map"
  [conn-region-map])

(conn-define-remap-keymap conn-edit-remap
    "Conn Search Map"
  [conn-edit-map])

(defvar conn-forward-word-remap (conn-remap-key conn-forward-word-keys t))
(defvar conn-forward-sexp-remap (conn-remap-key conn-forward-sexp-keys t))
(defvar conn-previous-line-remap (conn-remap-key conn-previous-line-keys t))
(defvar conn-backward-paragraph-remap (conn-remap-key conn-backward-paragraph-keys t))
(defvar conn-forward-sentence-remap (conn-remap-key conn-forward-sentence-keys t))
(defvar conn-backward-sentence-remap (conn-remap-key conn-backward-sentence-keys t))
(defvar conn-down-list-remap (conn-remap-key conn-down-list-keys t))
(defvar conn-backward-up-list-remap (conn-remap-key conn-backward-up-list-keys t))
(defvar conn-forward-list-remap (conn-remap-key conn-forward-list-keys t))
(defvar conn-backward-list-remap (conn-remap-key conn-backward-list-keys t))
(defvar conn-backward-word-remap (conn-remap-key conn-backward-word-keys t))
(defvar conn-backward-char-remap (conn-remap-key conn-backward-char-keys t))
(defvar conn-forward-paragraph-remap (conn-remap-key conn-forward-paragraph-keys t))
(defvar conn-next-line-remap (conn-remap-key conn-next-line-keys t))
(defvar conn-forward-char-remap (conn-remap-key conn-forward-char-keys t))
(defvar conn-end-of-defun-remap (conn-remap-key conn-end-of-defun-keys t))
(defvar conn-beginning-of-defun-remap (conn-remap-key conn-beginning-of-defun-keys t))
(defvar conn-backward-sexp-remap (conn-remap-key conn-backward-sexp-keys t))


;;;;; Region Utils

(defmacro conn--with-region-emphasis (regions &rest body)
  "Run BODY with the text in the complement of REGIONS shadowed."
  (declare (debug (form form body))
           (indent 1))
  (cl-with-gensyms (overlays)
    `(let (,overlays)
       (unwind-protect
           (progn
             (cl-loop for (beg end) on (nconc (list (point-min))
                                              (thread-first
                                                (conn--merge-overlapping-regions
                                                 ,regions t)
                                                flatten-tree sort)
                                              (list (point-max)))
                      by #'cddr
                      while beg
                      for ov = (make-overlay beg end)
                      do (progn
                           (overlay-put ov 'face 'shadow)
                           (push ov ,overlays)))
             ,@body)
         (mapc #'delete-overlay ,overlays)))))

;; From expand-region
(defun conn--point-in-comment-p ()
  "Check if point is within a comment."
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face)
            '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun conn--point-in-string-p ()
  "Check if point is within a string."
  (ignore-errors (nth 3 (syntax-ppss))))

(defun conn--point-in-comment-or-string-p ()
  "Check if point is within a string."
  (or (conn--point-in-string-p)
      (conn--point-in-comment-p)))

(defun conn--region-visible-p (beg end)
  "Return t if the region from BEG to END is visible."
  (cl-loop for pt = beg then (next-single-char-property-change
                              pt 'invisible nil end)
           while (and pt (< pt end))
           never (invisible-p pt)))

(defun conn--nnearest-first (list &optional buffer)
  "Move the region nearest point in LIST to the front.

LIST is either a list of markers or points, or of the form
((BEG . END) ...) where BEG and END are either markers or
points.

If BUFFER is non-nil find region nearest to point in BUFFER, else find
nearest in the current buffer.

This function destructively modifies LIST."
  (let ((min-dist most-positive-fixnum)
        min)
    (dolist (region list)
      (let ((beg (or (car-safe region) region))
            (end (or (cdr-safe region) region)))
        (when-let* (((or (not (markerp beg))
                         (eq (marker-buffer beg)
                             (or buffer (current-buffer)))))
                    (new-dist (min (abs (- (point) beg))
                                   (abs (- (point) end))))
                    ((< new-dist min-dist)))
          (setq min region
                min-dist new-dist))))
    (if min (cons min (delq min list)) list)))

(defun conn--merge-overlapping-regions (regions &optional points)
  "Merge all overlapping regions in REGIONS.

REGIONS is a list of the form ((BEG . END) ...), the returned list is of
the same form and contains disjoint (BEG . END) pairs."
  (let (merged)
    (pcase-dolist ((and region `(,beg1 . ,end1)) regions)
      (pcase (catch 'found
               (pcase-dolist ((and r `(,beg2 . ,end2)) merged)
                 (when (and (or points
                                (eq (marker-buffer beg2) (marker-buffer beg1)))
                            (not (or (< end2 beg1) (< end1 beg2))))
                   (throw 'found r))))
        ((and cons `(,beg2 . ,end2))
         (setcar cons (if (< beg1 beg2)
                          (prog1 beg1 (or points (set-marker beg2 nil)))
                        (prog1 beg2 (or points (set-marker beg1 nil)))))
         (setcdr cons (if (> end1 end2)
                          (prog1 end1 (or points (set-marker end2 nil)))
                        (prog1 end2 (or points (set-marker end1 nil))))))
        ('nil (push region merged))))
    (nreverse merged)))


;;;;; Derived Mode Utils

(static-if (< emacs-major-version 30)
    (defun conn--derived-mode-all-parents (mode)
      "Return all the parents of MODE, starting with MODE.

Although this returns a fresh list it will not beginning in Emacs 30.1,
so don't modify it."
      (let ((parents (list mode)))
        (while (and (setq mode (get mode 'derived-mode-parent))
                    (not (memq mode parents)))
          (push mode parents))
        (nreverse parents)))
  (defalias 'conn--derived-mode-all-parents 'derived-mode-all-parents))

(defun conn--derived-mode-property (property &optional buffer)
  "Check major mode in BUFFER and each `derived-mode-parent' for PROPERTY.
If BUFFER is nil check `current-buffer'."
  (cl-loop for mode in (thread-first
                         'major-mode
                         (buffer-local-value (or buffer (current-buffer)))
                         (conn--derived-mode-all-parents))
           for prop = (get mode property)
           when prop return prop))

(eval-and-compile
  (defun conn-get-mode-property--cmacro (exp mode property &optional no-inherit default)
    (if (and (macroexp-const-p no-inherit)
             (if (consp no-inherit) (cadr no-inherit) no-inherit))
        `(when-let* ((table (get ,mode :conn-properties)))
           (gethash ,property table ,default))
      exp)))

(defun conn-get-mode-property (mode property &optional no-inherit default)
  (declare (side-effect-free t)
           (important-return-value t)
           (compiler-macro conn-get-mode-property--cmacro))
  (if no-inherit
      (when-let* ((table (get mode :conn-properties)))
        (gethash property table default))
    (cl-loop for mode in (conn--derived-mode-all-parents mode)
             for prop = (if-let* ((table (get mode :conn-properties)))
                            (gethash property table conn--key-missing)
                          conn--key-missing)
             unless (eq conn--key-missing prop) return prop
             finally return default)))

(gv-define-setter conn-get-mode-property (value mode property &rest _)
  `(conn-set-mode-property ,mode ,property ,value))

(defun conn-set-mode-property (mode prop value)
  (let ((table (or (get mode :conn-properties)
                   (put mode :conn-properties (make-hash-table :test 'eq)))))
    (puthash prop value table)))

(defun conn-unset-mode-property (mode prop)
  (when-let* ((table (get mode :conn-properties)))
    (remhash prop table)))


;;;;; Misc Utils

(defun conn--create-marker (pos &optional buffer insertion-type)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    (set-marker-insertion-type marker insertion-type)
    marker))

(defun conn--isearch-matches (&optional buffer restrict)
  "Return a list of all isearch matches in buffer.

If RESTRICT either \\='before or \\='after which will then matches will
be restricted to those before or after the current match inclusive."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (pcase restrict
        ('after
         (unless isearch-forward
           (isearch-repeat 'forward))
         (goto-char isearch-other-end))
        ('before
         (when isearch-forward
           (isearch-repeat 'backward))
         (goto-char isearch-other-end))
        (_
         (goto-char (if isearch-forward (point-min) (point-max)))))
      (cl-loop with bound = (if isearch-forward (point-max) (point-min))
               with case-fold-search = isearch-case-fold-search
               while (isearch-search-string isearch-string bound t)
               when (funcall isearch-filter-predicate (match-beginning 0) (match-end 0))
               collect (cons (match-beginning 0) (match-end 0))
               when (and (= (match-beginning 0) (match-end 0))
                         (not (if isearch-forward (eobp) (bobp))))
               do (forward-char (if isearch-forward 1 -1))))))

(defun conn--string-no-upper-case-p (string)
  "Return t if STRING contains no upper case characters."
  (cl-loop for char across string always (eql char (downcase char))))

(defun conn--visible-regions (beg end &optional forward)
  (while (and (invisible-p beg)
              (/= end (setq beg (next-single-char-property-change
                                 beg 'invisible nil end)))))
  (let ((next beg)
        visible)
    (while (/= end beg)
      (while (and (/= end (setq next (next-single-char-property-change
                                      next 'invisible nil end)))
                  (not (invisible-p next))))
      (push (cons beg next) visible)
      (while (and (/= end (setq next (next-single-char-property-change
                                      next 'invisible nil end)))
                  (invisible-p next)))
      (setq beg next))
    (if forward
        (nreverse visible)
      visible)))

(defun conn--visible-matches (string &optional predicate)
  "Return all matches for STRING visible in the selected window."
  (let ((case-fold-search (conn--string-no-upper-case-p string))
        matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-regions (window-start) (window-end)))
        (goto-char beg)
        (while (search-forward string end t)
          (when (or (null predicate)
                    (save-match-data
                      (funcall predicate (match-beginning 0) (match-end 0))))
            (push (cons (match-beginning 0) (match-end 0)) matches)))))
    (nreverse matches)))

(defun conn--visible-re-matches (regexp &optional predicate)
  "Return all matches for REGEXP visible in the selected window."
  (let (matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-regions (window-start) (window-end)))
        (goto-char beg)
        (while (re-search-forward regexp end t)
          (when (or (null predicate)
                    (save-match-data
                      (funcall predicate (match-beginning 0) (match-end 0))))
            (push (cons (match-beginning 0) (match-end 0)) matches)))))
    (nreverse matches)))


;;;;; Overlay Utils

(defun conn--clear-overlays ()
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (remove-overlays nil nil 'conn-overlay t)))


;;;;; Quick Reference

(defgroup conn-quick-ref nil
  "Conn posframes."
  :prefix "conn-quick-ref-"
  :group 'conn)

(defface conn-quick-ref-heading-face
  '((t (:bold t :underline t)))
  "Face for column headings in quick ref buffer."
  :group 'conn-quick-ref)

(defface conn-quick-ref-error-face
  '((t (:inherit error)))
  "Face for key not found errors."
  :group 'conn-quick-ref)

(defface conn-quick-ref-page-header
  '((t ( :inverse-video nil :extend t
         :box nil :underline (:style line :position t)
         :inherit header-line)))
  "Face for selection in Conn posframes."
  :group 'conn-quick-ref)

(cl-defstruct (conn-reference-page
                  (:constructor conn--make-reference-page (title definition)))
  (title nil :type string :read-only t)
  (definition nil :type list :read-only t))

(defmacro conn-reference-page (title &rest definition)
  (declare (indent 1))
  (cl-labels ((process-definition (def)
                (pcase def
                  (`(,(and (or :eval :splice :keymap) type) . ,form)
                   (cons type (list '\, (cons 'lambda (cons nil form)))))
                  ((pred consp)
                   (mapcar #'process-definition def))
                  (_ def))))
    `(conn--make-reference-page
      ,title
      (list ,@(cl-loop for row in definition
                       if (listp row)
                       collect (list '\` (process-definition row))
                       else
                       collect row)))))

(defvar-keymap conn-quick-ref-map
  "C-q" 'next
  "M-q" 'previous
  "<escape>" 'close)

(defvar conn-quick-ref-display-function 'conn--quick-ref-minibuffer)

(defvar conn-quick-ref-text-scale 0.95)

(defvar conn--quick-ref-unbound
  (propertize "Ø" 'face 'conn-quick-ref-error-face))

(defun conn-quick-ref-find-remap (remap &optional keymap)
  (let (result)
    (cl-labels ((find-keys (keymap remap prefix)
                  (map-keymap
                   (lambda (key def)
                     (let ((all-keys (vconcat prefix (vector key))))
                       (pcase def
                         ((and (pred keymapp) sub-keymap)
                          (find-keys sub-keymap remap all-keys))
                         ((guard (and (equal def remap)
                                      (eq (keymap--menu-item-binding remap)
                                          (lookup-key keymap all-keys))))
                          (push all-keys result)))))
                   keymap)))
      (find-keys (pcase keymap
                   ('nil (make-composed-keymap (current-active-maps)))
                   ((pred keymapp) keymap)
                   (_ (make-composed-keymap keymap)))
                 remap []))
    (if-let* ((keys (car (sort result :key 'length))))
        (propertize (key-description keys)
                    'face 'help-key-binding)
      conn--quick-ref-unbound)))

(defun conn--format-ref-page (definition keymap-buffer)
  (cl-labels ((transpose (columns)
                (let ((curr columns)
                      rows)
                  (while (seq-find 'identity curr)
                    (cl-loop for col in curr
                             collect (or (car col) "-") into row
                             collect (cdr col) into next
                             finally do (progn
                                          (push row rows)
                                          (setq curr next))))
                  (nreverse rows)))
              (check-advertised (bind)
                (when-let* ((_(symbolp bind))
                            (adv (get bind :advertised-binding))
                            (desc (key-description adv))
                            (_(eq bind (key-binding adv))))
                  adv))
              (get-key (bind keymap)
                (if-let* ((key (if keymap
                                   (where-is-internal bind keymap t)
                                 (or (check-advertised bind)
                                     (when overriding-terminal-local-map
                                       (where-is-internal
                                        bind
                                        (list overriding-terminal-local-map)
                                        t))
                                     (where-is-internal bind nil t)))))
                    (propertize (key-description key) 'face 'help-key-binding)
                  conn--quick-ref-unbound))
              (process-bindings (description bindings keymap)
                (let (keys)
                  (while bindings
                    (pcase (pop bindings)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) bindings))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) bindings))
                      ((and str (pred stringp))
                       (push str keys))
                      (bind (push (get-key bind keymap) keys))))
                  (concat (string-join (nreverse keys) ", ")
                          ": " description)))
              (process-col (col keymap)
                (let ((result nil))
                  (while col
                    (pcase (pop col)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) col))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) col))
                      (`(:heading ,str)
                       (push (propertize str 'face 'conn-quick-ref-heading-face)
                             result))
                      ((and str (pred stringp)) (push str result))
                      (`(,desc . ,bindings)
                       (push (process-bindings desc bindings keymap)
                             result))))
                  (nreverse result)))
              (process-row (row)
                (let (keymap
                      result)
                  (while row
                    (pcase (pop row)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) row))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) row))
                      ((and (pred consp) col)
                       (push (process-col col keymap) result))))
                  (nreverse result))))
    (let ((ref-buffer (current-buffer)))
      (with-current-buffer keymap-buffer
        (conn--where-is-with-remaps
          (while definition
            (pcase (pop definition)
              ((and (pred stringp) row)
               (let ((str (substitute-command-keys row))
                     beg)
                 (with-current-buffer ref-buffer
                   (setq beg (point))
                   (insert str "\n")
                   (goto-char beg)
                   (while (search-forward "\n" nil 'move-to-end)
                     (replace-match " \n"))
                   (goto-char (point-max)))))
              (`(:eval . ,fn)
               (push (funcall fn) definition))
              (`(:splice . ,fn)
               (cl-callf2 append (funcall fn) definition))
              ((and row (pred consp))
               (let ((objs (transpose (process-row row))))
                 (with-current-buffer ref-buffer
                   (make-vtable
                    :face `( :inherit default
                             :height ,conn-quick-ref-text-scale)
                    :divider-width 2
                    :use-header-line nil
                    :objects objs)
                   (goto-char (point-max))))))))))))

(defun conn-quick-ref-insert-page (page buffer)
  (pcase-let (((cl-struct conn-reference-page
                          title
                          definition)
               page)
              (keymap-buffer (current-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (let (buffer-read-only
            header-pos)
        (delete-region (point-min) (point-max))
        (insert (substitute-command-keys
                 (concat "\\<conn-quick-ref-map> "
                         (propertize title 'face 'bold)
                         " — \\[next]: Next; \\[previous]: Previous; \\[close]: Close \n")))
        (setq header-pos (point))
        (conn--format-ref-page definition keymap-buffer)
        (indent-region header-pos (point-max) 1)
        (add-face-text-property
         (point-min) (point-max)
         `(:height ,conn-quick-ref-text-scale)
         t)
        (add-face-text-property
         (point-min) header-pos
         'conn-quick-ref-page-header t)))))

(defun conn-quick-reference (pages)
  (let ((buf (get-buffer-create " *conn-quick-ref*"))
        (display-function conn-quick-ref-display-function)
        (pages (copy-sequence pages))
        (inhibit-message t))
    (conn-quick-ref-insert-page (car pages) buf)
    (funcall display-function buf nil)
    (unwind-protect
        (catch 'break
          (conn-with-overriding-map conn-quick-ref-map
            (while t
              (let ((keys (read-key-sequence-vector nil)))
                (pcase (key-binding keys)
                  ('close
                   (throw 'break nil))
                  ('next
                   (setq pages (nconc (cdr pages) (list (car pages))))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall display-function buf nil))
                  ('previous
                   (setq pages (nconc (last pages) (butlast pages)))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall display-function buf nil))
                  ((or 'quit 'keyboard-quit)
                   (keyboard-quit))
                  (_ (setq unread-command-events
                           (mapcar (lambda (key)
                                     (cons 'no-record key))
                                   (listify-key-sequence keys)))
                     (throw 'break nil)))))))
      (funcall display-function buf t))))

(defun conn--quick-ref-minibuffer (buffer hide-p)
  (let (inhibit-message message-log-max)
    (if hide-p
        (message nil)
      (with-current-buffer buffer
        (message (buffer-string))))))

(provide 'conn-utils)
