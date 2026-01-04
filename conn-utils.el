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

(defun conn--protected-let* (varlist body)
  (cl-with-gensyms (success)
    (named-let protect ((binding (car (last varlist)))
                        (rest (reverse (cons success (butlast varlist))))
                        (body `(prog1 ,(macroexp-progn body)
                                 (setq ,success t))))
      (pcase binding
        ('nil body)
        ((and `(,var ,val . ,cleanup)
              (guard cleanup))
         (protect (car rest) (cdr rest)
                  `(let* ((,var ,val))
                     (unwind-protect
                         ,body
                       (unless ,success ,@cleanup)))))
        (_ (protect (car rest) (cdr rest)
                    (macroexp-let* (list binding) body)))))))

(defmacro conn-protected-let* (varlist &rest body)
  "Bind variables according to VARLIST then eval body as in `let*'.

In addition to what `let*' accepts, each element of VARLIST may also be
of the form (SYMBOL VALUEFORM . CLEANUP-FORMS), which binds SYMBOL to
VALUEFORM and if BODY exits non-locally runs CLEANUP-FORMS.

CLEANUP-FORMS are run in reverse order of their appearance in VARLIST."
  (declare (indent 1))
  (conn--protected-let* varlist body))

(defmacro conn--unwind-protect-all (&rest body)
  (declare (indent 0))
  (named-let wrap ((wrapped (car body))
                   (to-wrap (cdr body)))
    (if to-wrap
        (wrap `(unwind-protect ,wrapped ,(car to-wrap))
              (cdr to-wrap))
      wrapped)))

(defmacro conn-with-overriding-map (keymap &rest body)
  (declare (indent 1))
  (macroexp-let2 nil keymap keymap
    `(progn
       (if ,keymap (internal-push-keymap ,keymap 'overriding-terminal-local-map))
       (unwind-protect
           ,(macroexp-progn body)
         (internal-pop-keymap ,keymap 'overriding-terminal-local-map)))))

(defmacro conn-thread<- (&rest forms)
  (declare (indent 0))
  (cl-flet ((expand-last (&rest forms)
              `(thread-last ,@forms)))
    `(thread-first
       ,@(mapcar (lambda (form)
                   (macroexpand form `((:-> . ,#'expand-last))))
                 forms))))

(defmacro conn-threadf<- (&rest forms)
  (declare (indent 1))
  (cl-flet ((expand-last (&rest forms)
              `(thread-last ,@forms)))
    `(cl-callf thread-first
         ,@(mapcar (lambda (form)
                     (macroexpand form `((:-> . ,#'expand-last))))
                   forms))))

(defmacro conn-thread-> (&rest forms)
  (declare (indent 0))
  (cl-with-gensyms (first)
    (cl-flet ((expand-first (&rest forms)
                `(,first ,forms)))
      `(cl-macrolet ((,first (forms a1)
                       `(thread-first ,a1 ,@forms)))
         (thread-last
           ,@(mapcar (lambda (form)
                       (macroexpand form `((:<- . ,#'expand-first))))
                     forms))))))

(defmacro conn-threadf-> (&rest forms)
  (declare (indent 1))
  (cl-with-gensyms (first)
    (cl-flet ((expand-first (&rest args)
                `(,first ,args)))
      `(cl-macrolet ((,first (forms a1)
                       `(thread-first ,a1 ,@forms)))
         (cl-callf thread-last
             ,@(mapcar (lambda (form)
                         (macroexpand form `((:<- . ,#'expand-first))))
                       forms))))))

(defmacro conn--compat-callf (func place &rest args)
  (declare (indent 2)
           (debug (cl-function place &rest form)))
  (gv-letplace (getter setter) place
    (let* ((rargs (cons getter args)))
      (funcall setter `(compat-call ,func ,@rargs)))))

(defmacro conn--compat-callf2 (func arg1 place &rest args)
  (declare (indent 3)
           (debug (cl-function form place &rest form)))
  (if (and (cl--safe-expr-p arg1) (cl--simple-expr-p place) (symbolp func))
      `(setf ,place (compat-call ,func ,arg1 ,place ,@args))
    (macroexp-let2 nil a1 arg1
      (gv-letplace (getter setter) place
        (let* ((rargs (nconc (list a1 getter) args)))
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

(defalias 'conn--with-work-buffer
  (static-if (<= 31 emacs-major-version)
      'with-temp-buffer
    'with-work-buffer))

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

(defun conn--copy-mark (marker &optional buffer)
  (let ((mk (make-marker)))
    (set-marker mk (marker-position marker) buffer)
    (set-marker-insertion-type mk (marker-insertion-type marker))
    mk))

;; From quail
(defun conn-add-unread-events (key &optional reset)
  "Add KEY to `unread-command-events', but avoid recording it a second time.

See `quail-add-unread-command-events'."
  (if reset (setq unread-command-events nil))
  (if (characterp key)
      (cl-callf2 cons
          (cons 'no-record key)
          unread-command-events)
    (cl-loop for e in (append (if (vectorp key) key (vector key)) nil)
             collect (cons 'no-record e) into evs
             finally (cl-callf2 nconc evs unread-command-events))))

(defun conn-argument-format-register (name register)
  (concat
   name
   (when register
     (concat
      " "
      (propertize
       (concat "<" (char-to-string register) ">")
       'face 'conn-argument-active-face)))))

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

(gv-define-setter conn-get-buffer-property (value
                                            property
                                            &optional
                                            buffer
                                            _default)
  `(conn-set-buffer-property ,property ,value ,buffer))

(defun conn-set-buffer-property (property value &optional buffer)
  (setf (plist-get (buffer-local-value 'conn--buffer-properties
                                       (or buffer (current-buffer)))
                   property)
        value))

(defun conn-unset-buffer-property (property &optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (cl-callf2 assq-delete-all property
               (buffer-local-value 'conn--buffer-properties buffer))))

;;;;; Rings

(cl-defstruct (conn-ring
               ( :constructor conn-make-ring
                 (capacity &key cleanup copier))
               (:copier conn--copy-ring))
  "A ring that removes elements in least recently visited order."
  (list nil :type list)
  (history nil :type list)
  (capacity 0 :type integer)
  (cleanup #'ignore :type function)
  (copier #'identity :type function))

(define-inline conn-ring--visit (ring item)
  (inline-quote
   (conn-threadf->
     (conn-ring-history ,ring)
     (cl-delete ,item)
     (cons ,item))))

(defun conn-copy-ring (ring)
  (when (conn-ring-p ring)
    (let* ((new-ring (conn--copy-ring ring))
           (copier (conn-ring-copier ring))
           (new-list (cl-loop for elem in (conn-ring-list new-ring)
                              collect (funcall copier elem)))
           (new-hist (cl-loop with old-list = (conn-ring-list ring)
                              for elem in (conn-ring-history ring)
                              for pos = (seq-position old-list elem #'eq)
                              when pos collect (nth pos new-list))))
      (setf (conn-ring-list new-ring) new-list
            (conn-ring-history new-ring) new-hist)
      new-ring)))

(defun conn-ring-insert-front (ring item)
  "Insert ITEM into front of RING."
  (conn-threadf->
    (conn-ring-list ring)
    (delq item)
    (cons item))
  (conn-ring--visit ring item)
  (when-let* ((old (drop (conn-ring-capacity ring) (conn-ring-history ring))))
    (cl-callf2 take (conn-ring-capacity ring) (conn-ring-history ring))
    (let ((cleanup (conn-ring-cleanup ring)))
      (dolist (o old)
        (cl-callf2 delq o (conn-ring-list ring))
        (funcall cleanup o)))))

(defun conn-ring-insert-back (ring item)
  "Insert ITEM into back of RING."
  (conn-ring-insert-front ring item)
  (conn-ring-rotate-backward ring))

(defun conn-ring-rotate-forward (ring)
  "Rotate ring forward.

Takes (1 2 3 4) to (2 3 4 1)."
  (let ((head (car (conn-threadf->
                     (conn-ring-list ring)
                     car
                     list
                     (nconc (cdr (conn-ring-list ring)))))))
    (conn-ring--visit ring head)
    head))

(defun conn-ring-rotate-backward (ring)
  "Rotate ring backward.

Takes (1 2 3 4) to (4 1 2 3)."
  (let ((head (car (conn-threadf->
                     (conn-ring-list ring)
                     butlast
                     (nconc (last (conn-ring-list ring)))))))
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

(defun conn-ring-extract-head (ring)
  (let ((elem (conn-ring-head ring)))
    (cl-callf2 delq elem (conn-ring-list ring))
    (cl-callf2 delq elem (conn-ring-history ring))
    elem))

(defun conn-ring-delete (elem ring &optional pred)
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

;;;;;; Jump Ring

(defvar-local conn-jump-ring nil
  "Ring of previous jump positions in a buffer.")

(defun conn-push-jump-ring (location &optional back msg)
  "Push LOCATION to the jump ring.

If BACK is non-nil then push LOCATION to the back of the jump ring."
  (interactive (list (point) nil t))
  (when (not conn-jump-ring)
    (setq conn-jump-ring
          (conn-make-ring 40
                          :cleanup (lambda (mk) (set-marker mk nil))
                          :copier #'conn--copy-mark)))
  (pcase-let ((ptb (conn-ring-tail conn-jump-ring))
              (ptf (conn-ring-head conn-jump-ring)))
    (cond
     ((and ptf (= location ptf))
      (when back (conn-ring-rotate-forward conn-jump-ring)))
     ((and ptb (= location ptb))
      (unless back (conn-ring-rotate-backward conn-jump-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-jump-ring
                                 (conn--create-marker location))
        (conn-ring-insert-front conn-jump-ring
                                (conn--create-marker location))))))
  (when msg (message "Jump ring pushed")))

(defun conn--jump-post-command-hook ()
  (when-let* ((_ (marker-position conn-this-command-start))
              (pred (function-get this-command :conn-jump-command)))
    (if (eq t pred)
        (when (and (eq (marker-buffer conn-this-command-start)
                       (current-buffer))
                   (/= (point) conn-this-command-start))
          (conn-push-jump-ring conn-this-command-start))
      (ignore-errors
        (funcall pred conn-this-command-start)))))

(defun conn-set-last-thing-command (thing arg pt)
  (let ((marker (if-let* ((mk (car conn--last-thing-override))
                          (_ (markerp mk)))
                    mk
                  (make-marker))))
    (setq conn--last-thing-override
          (cons (set-marker marker pt)
                (cons thing arg)))))

(defun conn-set-jump-command (command &optional predicate)
  "Register COMMAND as a jump command.

If optional argument PREDICATE is nil then COMMAND will unconditionally
push to the jump ring.  If predicate is non-nil it should be a function
that will be called from the `post-command-hook' with the position at
which command was first called and is responsible for pushing a position
to the jump ring."
  (dolist (cmd (ensure-list command))
    (function-put cmd :conn-jump-command (or predicate t))))

(defun conn--isearch-jump-predicate ()
  (when (/= (point) isearch-opoint)
    (or mark-active
        (conn-push-jump-ring isearch-opoint))))

;;;;; Region Utils

(defmacro conn-with-region-emphasis (regions &rest body)
  "Run BODY with the text in the complement of REGIONS shadowed."
  (declare (debug (form form body))
           (indent 1))
  (cl-with-gensyms (overlays)
    `(let (,overlays)
       (unwind-protect
           (progn
             (when-let* ((r (sort (conn--merge-overlapping-regions ,regions t))))
               (setf r `(,(point-min) ,@(flatten-tree r) ,(point-max)))
               (while r
                 (pcase-let* ((`(,beg ,end) r)
                              (ov (make-overlay beg end)))
                   (overlay-put ov 'face 'shadow)
                   (push ov ,overlays))
                 (cl-callf cddr r)))
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
      (pcase region
        ((and (pred overlayp)
              (let beg (overlay-start region))
              (let end (overlay-end region))
              (let buf (overlay-buffer region)))
         (when-let* ((_ (eq buf (or buffer (current-buffer))))
                     (new-dist (min (abs (- (point) beg))
                                    (abs (- (point) end))))
                     ((< new-dist min-dist)))
           (setq min region
                 min-dist new-dist)))
        ((or `(,beg . ,end)
             (and beg end))
         (when-let* (((or (not (markerp beg))
                          (eq (marker-buffer beg)
                              (or buffer (current-buffer)))))
                     (new-dist (min (abs (- (point) beg))
                                    (abs (- (point) end))))
                     ((< new-dist min-dist)))
           (setq min region
                 min-dist new-dist)))))
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

(defun conn-read-regexp (prompt &rest regions)
  (conn-with-region-emphasis regions
    (minibuffer-with-setup-hook
        (minibuffer-lazy-highlight-setup
         :case-fold case-fold-search
         :filter (lambda (mb me)
                   (or (null regions)
                       (cl-loop for (beg . end) in regions
                                thereis (<= beg mb me end))))
         :highlight query-replace-lazy-highlight
         :regexp t
         :regexp-function (lambda (str _) str)
         :lax-whitespace nil)
      (read-regexp prompt nil 'regexp-history))))

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

(defun conn--visible-regions (beg end &optional backward)
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
    (if backward visible (nreverse visible))))

(defmacro conn-for-each-visible (beg end &rest body)
  "Run body with buffer narrowed to each visible region between BEG and END.

If END is less than BEG then the order of iteration is reversed.

\(fn BEG END [:label label] &rest BODY)"
  (declare (indent 2))
  (cl-with-gensyms (vbeg vend)
    (macroexp-let2* nil ((beg beg)
                         (end end))
      `(save-excursion
         (pcase-dolist (`(,,vbeg . ,,vend)
                        (conn--visible-regions (if (>= ,end ,beg) ,beg ,end)
                                               (if (>= ,end ,beg) ,end ,beg)
                                               (< ,end ,beg)))
           (with-restriction ,vbeg ,vend
             ,@body))))))

(defun conn--visible-matches (string &optional predicate)
  "Return all matches for STRING visible in the selected window."
  (let ((case-fold-search (conn--string-no-upper-case-p string))
        matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-regions (window-start) (window-end) t))
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
                     (conn--visible-regions (window-start) (window-end) t))
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

(defun conn--overlays-in-of-type (beg end category &optional window)
  (declare (important-return-value t))
  (cl-loop for ov in (overlays-in beg end)
           when (and (eq (overlay-get ov 'category) category)
                     (or (null window)
                         (eq (overlay-get ov 'window) window)))
           collect ov))

;;;;; Project Utils

(defvar conn-project-files-function 'project-files)

(provide 'conn-utils)
