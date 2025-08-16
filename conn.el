;;; conn.el --- A modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") (transient "0.8.7") (seq "2.23"))
;; Homepage: https://github.com/mtll/conn
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

;; A modal keybinding mode.

;;; Code:

;;;; Requires

(require 'compat)
(require 'eieio)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib)
  (require 'map))


;;;; Declerations

(defvar conn-mode)
(defvar conn-local-mode)

(defvar-local conn--hide-mark-cursor nil)

(defvar conn-lighter " Conn")

(declare-function kmacro-p "kmacro")
(declare-function kmacro-step-edit-macro "kmacro")
(declare-function project-files "project")
(declare-function conn-dispatch-kapply-prefix "conn-transients")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")
(declare-function face-remap-remove-relative "face-remap")
(declare-function mwheel-scroll "mwheel")


;;;;; Mark Variables

(defvar conn-this-command-handler nil
  "Mark handler for current command.

Commands may set this variable if they need to change their handler
dynamically.")

(defvar conn-this-command-thing nil
  "`this-command'\\='s thing.")

(defvar conn-this-command-start (make-marker)
  "Start position for current mark movement command.")


;;;; Custom Variables

(defgroup conn nil
  "A modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-marks nil
  "Conn-mode marks."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-faces nil
  "Conn-mode faces."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-states nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-key-remapping nil
  "Conn-mode key remapping."
  :prefix "conn-"
  :group 'conn)


;;;;; Key Remapping

(defcustom conn-undo-keys "C-/"
  "`undo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-undo-redo-keys "C-?"
  "`undo-redo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-yank-keys "C-y"
  "`yank' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-region-keys "C-w"
  "`kill-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-region-keys "C-S-w"
  "`delete-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sexp-keys "C-M-f"
  "`forward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sexp-keys "C-M-b"
  "`backward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-paragraph-keys "M-{"
  "`backward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-paragraph-keys "M-}"
  "`forward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-beginning-of-defun-keys "C-M-a"
  "`beginning-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-end-of-defun-keys "C-M-e"
  "`end-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-next-line-keys "C-n"
  "`next-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-previous-line-keys "C-p"
  "`previous-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-char-keys "C-f"
  "`forward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-char-keys "C-b"
  "`backward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-word-keys "M-f"
  "`forward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-word-keys "M-b"
  "`backward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sentence-keys "M-a"
  "`backward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sentence-keys "M-e"
  "`forward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-delete-char-keys "DEL"
  "`backward-delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-char-keys "C-d"
  "`delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-up-list-keys "C-M-u"
  "`backward-up-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-down-list-keys "C-M-d"
  "`down-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-list-keys "C-M-n"
  "`forward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-list-keys "C-M-p"
  "`backward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-line-keys "C-k"
  "`kill-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))


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
               (:constructor conn-make-ring (capacity &key cleanup)))
  "A ring that removes elements in least recently visited order."
  (list nil :type list)
  (history nil :type list)
  (capacity 0 :type integer)
  (cleanup nil :type (or nil function)))

(define-inline conn-ring--visit (ring item)
  (inline-quote
   (cl-callf thread-last (conn-ring-history ,ring)
     (delq ,item) (cons ,item))))

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

(defun conn-ring-delete (ring elem)
  (cl-callf2 delq elem (conn-ring-list ring))
  (cl-callf2 delq elem (conn-ring-history ring))
  (when-let* ((cleanup (conn-ring-cleanup ring)))
    (funcall cleanup elem)))


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

(defun conn-remap-key (from-keys &optional without-conn-maps no-accept-default)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  (let ((from-keys (key-parse from-keys)))
    `(menu-item
      ,(format "Remap %s" (key-description from-keys))
      ,(conn--without-conn-maps (key-binding from-keys (not no-accept-default)))
      :filter ,(lambda (_real-binding)
                 (if without-conn-maps
                     (conn--without-conn-maps
                       (key-binding from-keys (not no-accept-default)))
                   (key-binding from-keys (not no-accept-default)))))))

(defun conn-remap-keymap (from-keys &optional without-conn-maps)
  "Map to the keymap at FROM-KEYS.

If the binding at FROM-KEYS is for any reason not a keymap, say because
a minor mode has shadowed the keymap originally bound there, then map to
the original binding.  Also see `conn-remap-key'."
  (let ((from-keys (key-parse from-keys)))
    `(menu-item
      ,(format "Remap %s Keymap" (key-description from-keys))
      ,(conn--without-conn-maps (key-binding from-keys t))
      :filter ,(lambda (real-binding)
                 (let ((binding (if without-conn-maps
                                    (conn--without-conn-maps
                                      (key-binding from-keys t))
                                  (key-binding from-keys t))))
                   (if (keymapp binding) binding real-binding))))))


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
    merged))


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

(defun conn--narrow-indirect (beg end &optional record)
  "Narrow from BEG to END in an indirect buffer."
  (let* ((line-beg (line-number-at-pos beg))
         (linenum (- (line-number-at-pos end) line-beg))
         (name (format "%s@%s+%s - %s"
                       (buffer-name (current-buffer)) line-beg linenum
                       (thread-first
                         (buffer-substring-no-properties beg end)
                         (string-trim)
                         (substring 0 (min 20 (- end beg)))))))
    (clone-indirect-buffer-other-window name t)
    (conn--narrow-to-region-1 beg end record)
    (deactivate-mark)))

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

(defun conn--read-from-with-preview (prompt bounds &optional regexp-flag)
  "Read a from string with `minibuffer-lazy-highlight-setup' previews.

PROMPT is used as the minibuffer prompt when reading.

BOUNDS is a list of the form returned by `region-bounds' and defines the
limits of the highlighting.

REGEXP-FLAG means to treat the from string as a regexp for the purpose
of highlighting."
  (let ((default (conn-replace-read-default)))
    (conn--with-region-emphasis bounds
      (minibuffer-with-setup-hook
          (minibuffer-lazy-highlight-setup
           :case-fold case-fold-search
           :filter (lambda (mb me)
                     (cl-loop for (beg . end) in bounds
                              when (<= beg mb me end) return t))
           :highlight query-replace-lazy-highlight
           :regexp regexp-flag
           :regexp-function (or replace-regexp-function
                                (and replace-char-fold
                                     (not regexp-flag)
                                     #'char-fold-to-regexp))
           :transform (lambda (string)
                        (when (and case-fold-search search-upper-case)
                          (setq isearch-case-fold-search
                                (isearch-no-upper-case-p string regexp-flag)))
                        string))
        (if regexp-flag
            (read-regexp (format-prompt prompt default)
                         (when default (regexp-quote default))
                         'minibuffer-history)
          (let ((from (read-string
                       (format-prompt prompt default)
                       nil nil
                       (if default
                           (delete-dups
                            (cons default (query-replace-read-from-suggestions)))
                         (query-replace-read-from-suggestions))
                       t)))
            (or (and (length= from 0) default)
                from)))))))


;;;;; Overlay Utils

(defun conn--clear-overlays ()
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (remove-overlays nil nil 'conn-overlay t)))


;;;; Thing Types

(cl-deftype conn-thing-function () '(satisfies conn-command-thing))

(cl-deftype conn-thing () '(satisfies conn-thing-p))

(define-inline conn-command-thing (cmd)
  (declare (side-effect-free t)
           (gv-setter conn-set-command-thing))
  (inline-letevals (cmd)
    (inline-quote
     (and (symbolp ,cmd)
          (get ,cmd :conn-command-thing)))))

(defun conn-set-command-thing (cmd thing)
  (put cmd :conn-command-thing thing))

(define-inline conn-thing-p (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (thing)
    (inline-quote
     (when (symbolp ,thing)
       (or (get ,thing :conn-thing)
           (get ,thing 'forward-op)
           (intern-soft (format "forward-%s" ,thing))
           (get ,thing 'end-op)
           (get ,thing 'bounds-of-thing-at-point))))))

(cl-defun conn-register-thing (thing &key forward-op beg-op end-op bounds-op)
  (put thing :conn-thing t)
  (when forward-op
    (put thing 'forward-op forward-op))
  (when (or beg-op end-op)
    (cl-assert (and beg-op end-op)
               nil "If either beg-op or end-op is specified then both must be")
    (put thing 'beginning-op beg-op)
    (put thing 'end-op end-op))
  (when bounds-op
    (put thing 'bounds-of-thing-at-point bounds-op)))

(cl-defstruct (conn-anonymous-thing
               (:constructor nil)
               (:constructor conn--make-anonymous-thing)
               ;; This would be nice but cl-defsubst does not handle
               ;; &rest arguments properly and as a result PROPERTIES
               ;; gets evaluated twice in the expansion.
               ;; (:constructor conn-anonymous-thing (parent &rest properties))
               )
  (parent nil :read-only t)
  (properties nil :read-only t))

(defun conn-anonymous-thing (parent &rest properties)
  "Make an anonymous thing."
  (declare (important-return-value t)
           (compiler-macro
            (lambda (_exp)
              `(conn--make-anonymous-thing
                :parent ,parent
                :properties (list ,@properties)))))
  (conn--make-anonymous-thing
   :parent parent
   :properties properties))

(define-inline conn-get-thing (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (thing)
    (inline-quote
     (or (when (conn-anonymous-thing-p ,thing)
           (conn-anonymous-thing-parent ,thing))
         (conn-command-thing ,thing)
         ,thing))))

;; from cl--generic-make-defmethod-docstring/pcase--make-docstring
(defun conn--make-anonymous-thing-docstring ()
  (let* ((main (documentation (symbol-function 'conn-anonymous-thing) 'raw))
         (ud (help-split-fundoc main 'conn-anonymous-thing)))
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main ""))
      (insert "\n\n\tCurrently supported properties for anonymous things:\n\n")
      (pcase-dolist (`(,prop . ,fn) (get 'conn-anonymous-thing :known-properties))
        (if (stringp fn)
            (insert (format "%s - %s" prop fn))
          (insert (format "%s - `%s' method for this thing" prop fn)))
        (insert "\n\n"))
      (let ((combined-doc (buffer-string)))
        (if ud
            (help-add-fundoc-usage combined-doc (car ud))
          combined-doc)))))

(put 'conn-anonymous-thing 'function-documentation
     '(conn--make-anonymous-thing-docstring))

(eval-and-compile
  (defun conn--set-anonymous-thing-property (f _args property &optional description)
    `(setf (alist-get ,property (get 'conn-anonymous-thing :known-properties))
           (or ,description ',f)))
  (setf (alist-get 'conn-anonymous-thing-property defun-declarations-alist)
        (list #'conn--set-anonymous-thing-property)))

(define-inline conn-anonymous-thing-property (object property)
  (declare (side-effect-free t))
  (inline-quote
   (plist-get (conn-anonymous-thing-properties ,object) ,property)))


;;;; States

(defcustom conn-null-state-modes
  (list 'calc-mode
        'calc-trail-mode
        'calc-keypad-mode
        'image-mode
        'doc-view-mode
        'pdf-view-mode)
  "Major modes in which `conn-null-state' should be active."
  :group 'conn
  :type '(list symbol))

(defcustom conn-command-state-modes
  (list 'prog-mode
        'text-mode
        'conf-mode
        'fundamental-mode)
  "Major modes in which `conn-command-state' should be the base state."
  :group 'conn
  :type '(list symbol))

(defvar conn-setup-state-hook nil
  "Hook responsible for setting up the base state in a new buffer.

The hook is run when `conn-local-mode' is turned on. Each function is
called in turn until one returns non-nil.  A function returning non-nil
should set the base state for the current buffer by pushing it to the
stack with `conn-push-state'.  The function may setup any other
necessary state as well.")

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar-local conn-next-state nil)
(defvar-local conn-previous-state nil)

(defvar-local conn--state-stack nil
  "Previous conn states in buffer.")

(defvar conn--minor-mode-maps-sort-tick 0)

(cl-defstruct (conn-state
               (:constructor nil)
               ( :constructor conn--make-state
                 ( name docstring parents
                   &aux
                   (properties (make-hash-table :test 'eq))
                   (minor-mode-depths (make-hash-table :test 'eq))
                   (minor-mode-sort-tick conn--minor-mode-maps-sort-tick)
                   (minor-mode-maps (list :conn-minor-mode-map-alist))
                   (major-mode-maps (make-hash-table :test 'eq))))
               (:conc-name conn-state--)
               (:copier nil))
  (name nil :type symbol :read-only t)
  (docstring nil :type string)
  (parents nil :type (list-of symbol))
  (children nil :type (list-of symbol))
  (properties nil :type hash-table)
  (keymap nil :type (or nil keymap))
  (minor-mode-depths nil :type hash-table)
  (minor-mode-sort-tick nil :type (or nil integer))
  (minor-mode-maps nil :type alist)
  (major-mode-maps nil :type hash-table))

(defmacro conn--find-state (state)
  `(get ,state :conn--state))

(define-inline conn-state-minor-mode-maps-alist (state)
  "Return the minor mode maps alist for STATE."
  (declare (side-effect-free t)
           (important-return-value t)
           (gv-setter
            (lambda (value)
              `(setf (conn-state--minor-mode-maps
                      (conn--find-state ,state))
                     ,value))))
  (inline-quote
   (conn-state--minor-mode-maps
    (conn--find-state ,state))))

(define-inline conn-state-name-p (state)
  "Return non-nil if STATE is a conn-state."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (eq 'conn-state (type-of (conn--find-state ,state)))))

(define-inline conn-state-parents (state)
  "Return only the immediate parents for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (conn-state--parents (conn--find-state ,state))))

(defconst conn--state-all-parents-cache (make-hash-table :test 'eq))

(defun conn-state-all-parents (state)
  "Return all parents for STATE."
  (declare (important-return-value t))
  (with-memoization (gethash state conn--state-all-parents-cache)
    (cons state (merge-ordered-lists
                 (mapcar 'conn-state-all-parents
                         (conn-state--parents (conn--find-state state)))))))

(defun conn-state-all-keymap-parents (state)
  "Return all parents for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (cons state
        (merge-ordered-lists
         (cl-loop with no-inherit = (conn-state-get state :no-inherit-keymaps)
                  for parent in (conn-state--parents (conn--find-state state))
                  unless (memq parent no-inherit)
                  collect (conn-state-all-keymap-parents parent)))))

(defun conn-state-all-children (state)
  "Return all children for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (cl-labels ((all-children (state)
                (let ((children (conn-state--children
                                 (conn--find-state state))))
                  (append children (mapcan #'all-children children)))))
    (delete-dups (all-children state))))

(define-inline conn-substate-p (state parent)
  "Return non-nil if STATE is a substate of PARENT."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (memq ,parent (conn-state-all-parents ,state))))


;;;;; State Properties

(eval-and-compile
  (defun conn-declare-property-static (property)
    "Declare a state PROPERTY static.

Static state properties can only be changed by redefining a state and
are not inherited."
    (setf (get property :conn-static-property) t))

  (define-inline conn-property-static-p (property)
    "Return non-nil if PROPERTY is static."
    (declare (side-effect-free t)
             (important-return-value t))
    (inline-quote
     (and (get ,property :conn-static-property) t)))

  (conn-declare-property-static :no-keymap)
  (conn-declare-property-static :no-inherit-keymaps)
  (conn-declare-property-static :abstract)

  (defun conn-state-get--cmacro ( exp state property
                                  &optional
                                  no-inherit default)
    (let ((no-inherit (macroexpand-all no-inherit macroexpand-all-environment))
          (prop (macroexpand-all property macroexpand-all-environment)))
      (if (or (and (macroexp-const-p no-inherit)
                   (if (consp no-inherit) (cadr no-inherit) no-inherit))
              (and (symbolp prop)
                   (conn-property-static-p prop)))
          `(gethash ,property
                    (conn-state--properties (conn--find-state ,state))
                    ,default)
        exp))))

(defun conn-state-get (state property &optional no-inherit default)
  "Return the value of PROPERTY for STATE.

If PROPERTY is not set for STATE then check all of STATE's parents for
PROPERTY.  If no parent has that property either than nil is returned."
  (declare (compiler-macro conn-state-get--cmacro)
           (side-effect-free t)
           (important-return-value t)
           (gv-setter conn-state-set))
  (if (or no-inherit (conn-property-static-p property))
      (gethash property
               (conn-state--properties (conn--find-state state))
               default)
    (cl-loop for parent in (conn-state-all-parents state)
             for table = (conn-state--properties (conn--find-state parent))
             for prop = (gethash property table conn--key-missing)
             unless (eq prop conn--key-missing) return prop
             finally return default)))

(define-inline conn-state-set (state property value)
  "Set the value of PROPERTY in STATE to VALUE.

Returns VALUE."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (thread-last
         (conn--find-state ,state)
         conn-state--properties (puthash ,property ,value))))))

(define-inline conn-state-unset (state property)
  "Make PROPERTY unset in STATE.

If a property is unset in a state it will inherit the value of that
property from its parents."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (remhash (conn-state--properties (conn--find-state ,state))
                ,property)))))

(define-inline conn-state-has-property-p (state property)
  "Return t if PROPERTY is set for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (property)
    (inline-quote
     (thread-first
       (gethash ,property
                (conn-state--properties (conn--find-state ,state))
                conn--key-missing)
       (eq conn--key-missing) not))))


;;;;; State Keymap Impl

(defvar-local conn--state-map nil)
(defvar-local conn--major-mode-map nil)
(defvar-local conn--minor-mode-maps nil)

(defun conn--rebuild-state-keymaps (state)
  "Rebuild all composed keymaps for STATE.

Called when the inheritance hierarchy for STATE changes."
  (unless (conn-state-get state :no-keymap)
    (let ((parents (conn-state-all-keymap-parents state))
          (state-obj (conn--find-state state)))
      (when-let* ((state-map (gethash state conn--composed-state-maps)))
        (setf (cdr state-map)
              (cl-loop for pstate in parents
                       for pmap = (conn-get-state-map pstate t)
                       when pmap collect pmap)))
      (let (to-remove)
        (pcase-dolist ((and `(,mode . ,map) cons)
                       (cdr (conn-state--minor-mode-maps state-obj)))
          (setf (cdr map)
                (cl-loop for parent in parents
                         for pmap = (conn-get-minor-mode-map parent mode t)
                         when pmap collect pmap))
          (unless (cdr map)
            (push cons to-remove)))
        (cl-callf seq-difference (cdr (conn-state--minor-mode-maps state-obj))
          to-remove #'eq))
      (maphash
       (lambda (mode map)
         (setf (cdr map)
               (cl-loop for pstate in parents
                        for pmap = (conn-get-major-mode-map pstate mode t)
                        when pmap collect pmap)))
       (conn-state--major-mode-maps state-obj)))))

;;;;;; State Maps

(defconst conn--composed-state-maps (make-hash-table :test 'eq))

(defun conn--compose-state-map (state)
  "Return composed state map for STATE.

The composed keymap is of the form:

(keymap
 (keymap . bindings)  ;; state map for STATE
 (keymap . bindings)  ;; state map for STATE parent
 ...)"
  (declare (important-return-value t))
  (with-memoization (gethash state conn--composed-state-maps)
    (cl-assert (not (conn-state-get state :no-keymap))
               nil "%s :no-keymap property is non-nil" state)
    (make-composed-keymap
     (cl-loop for pstate in (conn-state-all-keymap-parents state)
              for pmap = (conn-state--keymap (conn--find-state pstate))
              when pmap collect pmap))))

(defun conn-set-state-map (state map)
  (cl-assert (keymapp map))
  (setf (conn-state--keymap (conn--find-state state)) map)
  (dolist (child (cons state (conn-state-all-children state)) map)
    (when-let* ((map (gethash child conn--composed-state-maps)))
      (cl-loop for parent in (conn-state-all-keymap-parents child)
               for pmap = (conn-get-state-map parent t)
               when pmap collect pmap into pmaps
               finally (setf (cdr map) pmaps)))))

(gv-define-simple-setter conn-get-state-map conn-set-state-map)

(defun conn-get-state-map (state &optional dont-create)
  "Return the state keymap for STATE."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (conn-state--keymap (conn--find-state state))
        (unless dont-create
          (setf (conn-get-state-map state) (make-sparse-keymap))))))

;;;;;; Major Mode Maps

(defun conn--ensure-major-mode-map (state mode)
  (declare (important-return-value t))
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--major-mode-maps-cache))
                (get-composed-map (state)
                  `(gethash mode (conn-state--major-mode-maps
                                  (conn--find-state ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (or (get-composed-map state)
        (setf (get-composed-map state)
              (make-composed-keymap (parent-maps state))))))

(defconst conn--major-mode-maps-cache (make-hash-table :test 'equal))

(defun conn-set-major-mode-map (state mode map)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--major-mode-maps-cache))
                (get-composed-map (state)
                  `(gethash mode (conn-state--major-mode-maps
                                  (conn--find-state ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (cl-assert (keymapp map))
    (cl-check-type (conn--find-state state) conn-state)
    (cl-check-type mode symbol)
    (setf (get-map state) map)
    (let* ((keymap (make-composed-keymap (parent-maps state))))
      (setf (get-composed-map state) keymap)
      (dolist (child (conn-state-all-children state) map)
        (if-let* ((map (get-composed-map child)))
            (setf (cdr map) (parent-maps child))
          (unless (conn-state-get child :no-keymap)
            (setf (get-composed-map child)
                  (make-composed-keymap (parent-maps child)))))))))

(gv-define-simple-setter conn-get-major-mode-map conn-set-major-mode-map)

(defun conn-get-major-mode-map (state mode &optional dont-create)
  "Return keymap for major MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (gethash (cons state mode) conn--major-mode-maps-cache)
        (unless dont-create
          (setf (conn-get-major-mode-map state mode)
                (make-sparse-keymap))))))

(defconst conn--composed-major-mode-maps (make-hash-table :test 'equal))

(defun conn--compose-major-mode-map (state)
  "Return composed major mode maps for STATE.

The composed map is a keymap of the form:

;; Fully composed major-mode map
(keymap
 ;; major-mode composed map
 (keymap (keymap . bindings)  ;; map in STATE for major-mode
         (keymap . bindings)  ;; map in STATE-parent for major-mode
         ...)
 ;; parent-mode composed map
 (keymap (keymap . bindings)  ;; map in STATE for parent-mode
         (keymap . bindings)  ;; map in STATE-parent for parent-mode
         ..)
 ...)"
  (declare (important-return-value t))
  (with-memoization
      (gethash (cons state (conn--derived-mode-all-parents major-mode))
               conn--composed-major-mode-maps)
    (cl-assert (not (conn-state-get state :no-keymap))
               nil "%s :no-keymap property is non-nil" state)
    (make-composed-keymap
     (cl-loop for pmode in (conn--derived-mode-all-parents major-mode)
              collect (conn--ensure-major-mode-map state pmode)))))

;;;;;; Minor Mode Maps

(define-inline conn--mode-maps-sorted-p (state)
  (declare (side-effect-free t)
           (important-return-value t)
           (gv-setter
            (lambda (value)
              (cl-labels ((set (v)
                            `(setf (conn-state--minor-mode-sort-tick
                                    (conn--find-state ,state))
                                   ,v)))
                (pcase value
                  ('nil (set nil))
                  ((pred macroexp-const-p)
                   (set 'conn--minor-mode-maps-sort-tick))
                  (_ (macroexp-let2 nil value value
                       `(progn
                          ,(set `(when ,value conn--minor-mode-maps-sort-tick))
                          ,value))))))))
  (inline-quote
   (eql conn--minor-mode-maps-sort-tick
        (conn-state--minor-mode-sort-tick
         (conn--find-state ,state)))))

(defun conn--sort-mode-maps (state)
  (cl-check-type (conn--find-state state) conn-state)
  (unless (conn-state-get state :no-keymap)
    (let* ((parents (conn-state-all-keymap-parents state))
           (tables (mapcar (lambda (s)
                             (conn-state--minor-mode-depths
                              (conn--find-state s)))
                           parents)))
      (conn--compat-callf sort (cdr (conn-state-minor-mode-maps-alist state))
        :key (lambda (cons)
               (or (catch 'break
                     (dolist (table tables)
                       (when-let* ((v (gethash (car cons) table)))
                         (throw 'break v))))
                   (get (car cons) :conn-mode-depth)
                   0))
        :in-place t)
      (setf (conn--mode-maps-sorted-p state) t))))

(defun conn-set-mode-map-depth (mode depth &optional state)
  (cl-check-type mode symbol)
  (cl-assert (<= -100 depth 100) nil "Depth must be between -100 and 100")
  (if (null state)
      (progn
        (setf (get mode :conn-mode-depth) depth)
        (cl-incf conn--minor-mode-maps-sort-tick))
    (let* ((state-obj (conn--find-state state))
           (mmode-maps (cdr (conn-state--minor-mode-maps state-obj)))
           (table (conn-state--minor-mode-depths state-obj)))
      (cl-assert (not (conn-state-get state :no-keymap))
                 nil "%s :no-keymap property is non-nil" state)
      (unless (eql depth (gethash mode table))
        (setf (gethash mode table) depth)
        (dolist (child (conn-state-all-children state))
          (setf (conn--mode-maps-sorted-p child) nil))
        (when (alist-get mode mmode-maps)
          (conn--sort-mode-maps state)))))
  nil)

;; setup special mode maps
(conn-set-mode-map-depth :override -80)
(conn-set-mode-map-depth :bind-last -90)

(defconst conn--minor-mode-maps-cache (make-hash-table :test 'equal))

(defun conn-set-minor-mode-map (state mode map)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--minor-mode-maps-cache))
                (get-composed-map (state)
                  `(alist-get mode (cdr (conn-state-minor-mode-maps-alist ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (cl-assert (keymapp map))
    (cl-check-type (conn--find-state state) conn-state)
    (cl-check-type mode symbol)
    (setf (get-map state) map)
    (let ((keymap (make-composed-keymap (parent-maps state))))
      (setf (get-composed-map state) keymap)
      (conn--sort-mode-maps state)
      (dolist (child (conn-state-all-children state) map)
        (if-let* ((map (get-composed-map child)))
            (setf (cdr map) (parent-maps child))
          (conn--ensure-minor-mode-map child mode))))))

(gv-define-simple-setter conn-get-minor-mode-map conn-set-minor-mode-map)

(defun conn-get-minor-mode-map (state mode &optional dont-create)
  "Return keymap for MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (gethash (cons state mode) conn--minor-mode-maps-cache)
        (unless dont-create
          (setf (conn-get-minor-mode-map state mode)
                (make-sparse-keymap))))))

(defun conn--ensure-minor-mode-map (state mode)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--minor-mode-maps-cache))
                (get-composed-map (state)
                  `(alist-get mode (cdr (conn-state-minor-mode-maps-alist ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (unless (or (conn-state-get state :no-keymap)
                (get-map state))
      (setf (get-composed-map state)
            (make-composed-keymap (parent-maps state)))
      (setf (conn--mode-maps-sorted-p state) nil))))


;;;;; State Input Methods

(defvar-local conn--input-method nil
  "Input method for current buffer.")
(put 'conn--input-method 'permanent-local t)

(defvar-local conn--input-method-title nil
  "Title string of the current input method shown in mode line.")
(put 'conn--input-method-title 'permanent-local t)

(defvar-local conn--prev-mode-line-mule-info nil)
(put 'conn--prev-mode-line-mule-info 'risky-local-variable t)

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property."
  (when conn-local-mode
    (let (input-method-activate-hook
          input-method-deactivate-hook)
      (pcase (conn-state-get conn-current-state :suppress-input-method)
        ((and 'nil (guard current-input-method))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title))
        ((and 'nil (guard conn--input-method))
         (activate-input-method conn--input-method))
        ((guard (and current-input-method
                     conn--input-method
                     deactivate-current-input-method-function))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title)
         (deactivate-input-method))
        ((guard (and current-input-method
                     deactivate-current-input-method-function))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title)
         (deactivate-input-method))))))
(put 'conn--activate-input-method 'permanent-local-hook t)

(defun conn--deactivate-input-method ()
  (setq conn--input-method nil
        conn--input-method-title nil))
(put 'conn--deactivate-input-method 'permanent-local-hook t)

(defun conn--isearch-input-method ()
  "Ensure input method is enabled in isearch-mode."
  (when (and conn--input-method isearch-mode)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (let ((overriding-terminal-local-map nil))
      (activate-input-method conn--input-method))
    (setq isearch-input-method-function input-method-function)
    (setq-local input-method-function nil)
    (isearch-update)
    (add-hook 'isearch-mode-end-hook
              (conn-anaphoricate hook
                (lambda ()
                  (conn--activate-input-method)
                  (add-hook 'input-method-activate-hook
                            #'conn--activate-input-method
                            nil t)
                  (add-hook 'input-method-deactivate-hook
                            #'conn--deactivate-input-method
                            nil t)
                  (remove-hook 'isearch-mode-end-hook hook))))))
(put 'conn--isearch-input-method 'permanent-local-hook t)

(defun conn--input-method-mode-line ()
  "Display the mode line information for conn--input-method.

This ensures that the mode line information for the current input method
is shown even if the input method is deactivated because a state is
suppressing it."
  (cond
   (conn-local-mode
    (setq conn--prev-mode-line-mule-info mode-line-mule-info
          mode-line-mule-info
          `(""
            (conn--input-method
             ( :propertize ("" conn--input-method-title)
               help-echo (concat
                          "Current input method: "
                          conn--input-method
                          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
               local-map ,mode-line-input-method-map
               mouse-face mode-line-highlight))
            ,( propertize "%z"
               'help-echo 'mode-line-mule-info-help-echo
               'mouse-face 'mode-line-highlight
               'local-map mode-line-coding-system-map)
            (:eval (mode-line-eol-desc)))))
   (conn--prev-mode-line-mule-info
    (setq mode-line-mule-info conn--prev-mode-line-mule-info))))

(defmacro conn-with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (input-method-p)
    `(let ((,input-method-p conn--input-method))
       (unwind-protect
           (progn
             (when ,input-method-p
               (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
               (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
               (activate-input-method conn--input-method))
             ,@body)
         (when ,input-method-p
           (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
           (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
           (conn--activate-input-method))))))


;;;;; State Macros

;; Adapted from map pattern, we can't just use the map pattern on the
;; property table because of inheritance.
(pcase-defmacro conn-state-props (&rest properties)
  "Build a `pcase' pattern matching state properties.

PROPERTIES is a list of elements to be matched in the state.

Each element of PROPERTIES is of the form (KEY PAT), which looks up the
property KEY in the state and matches to associated value against
`pcase' pattern PAT.

Each element can also be a SYMBOL, which is an abbreviation of a (KEY
PAT) tuple of the form (\\='SYMBOL SYMBOL).  When SYMBOL is a keyword,
it is an abbreviation of the form (:SYMBOL SYMBOL)."
  `(and (pred conn-state-name-p)
        ,@(mapcar
           (static-if (< emacs-major-version 30)
               (lambda (prop)
                 (cond ((consp elt)
                        (cl-with-gensyms (state)
                          `(app (lambda (,state)
                                  (conn-state-get ,state ,(car prop) ,(caddr prop)))
                                ,(cadr prop))))
                       ((keywordp prop)
                        (let ((var (intern (substring (symbol-name prop) 1))))
                          `(app (pcase--flip conn-state-get ,prop) ,var)))
                       (t `(app (pcase--flip conn-state-get ',prop) ,prop))))
             (lambda (prop)
               (cond ((consp prop)
                      `(app (conn-state-get _ ,(car prop) ,(caddr prop)) ,(cadr prop)))
                     ((keywordp prop)
                      (let ((var (intern (substring (symbol-name prop) 1))))
                        `(app (conn-state-get _ ,prop) ,var)))
                     (t `(app (conn-state-get _ ',prop) ,prop)))))
           properties)))

(pcase-defmacro conn-substate (parent)
  "Matches if EXPVAL is a substate of PARENT."
  `(and (pred conn-state-name-p)
        (pred ,(static-if (< emacs-major-version 30)
                   `(pcase--flip conn-substate-p ',parent)
                 `(conn-substate-p _ ',parent)))))

(defmacro conn-with-recursive-stack (state &rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms (buffer stack)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (conn-enter-recursive-stack ,state)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer ,buffer
           (conn-enter-state (car ,stack))
           (setq conn--state-stack ,stack))))))

(defmacro conn-without-recursive-stack (&rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (stack buffer)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (conn-exit-recursive-stack)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer ,buffer
           (conn-enter-state (car ,stack))
           (setq conn--state-stack ,stack))))))


;;;;; Cl-Generic Specializers

(cl-generic-define-generalizer conn--substate-generalizer
  90 (lambda (state &rest _)
       `(and (conn-state-name-p ,state)
             (conn-state-all-parents ,state)))
  (lambda (tag &rest _)
    (when tag
      `(,@(mapcar (lambda (state) `(conn-substate ,state)) tag)
        (conn-substate t)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-substate)))
  "Support for (conn-substate STATE) specializers.
These match if the argument is a substate of STATE."
  (list conn--substate-generalizer))

(defconst conn--thing-cmd-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--thing-command-generalizer
  70 (lambda (cmd &rest _)
       `(when-let* ((thing (conn-command-thing ,cmd)))
          (with-memoization (gethash thing conn--thing-cmd-tag-cache)
            (cons 'command thing))))
  (lambda (thing &rest _)
    (when thing
      `((conn-thing-command ,(cdr thing))
        (conn-thing ,(cdr thing))
        (conn-thing-command t)
        (conn-thing t)))))

(cl-generic-define-generalizer conn--anonymous-thing-generalizer
  70 (lambda (cmd &rest _)
       `(when (conn-anonymous-thing-p ,cmd)
          'conn-anonymous-thing))
  (lambda (thing &rest _)
    (when thing
      `((conn-thing anonymous-thing)
        (conn-thing t)))))

(cl-generic-define-generalizer conn--thing-generalizer
  70 (lambda (cmd &rest _) `(and (conn-thing-p ,cmd) ,cmd))
  (lambda (thing &rest _)
    (when thing
      `((conn-thing ,thing)
        (conn-thing t)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-thing)))
  "Support for (conn-thing THING) specializers."
  (list conn--thing-command-generalizer
        conn--anonymous-thing-generalizer
        conn--thing-generalizer))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-thing-command)))
  "Support for conn-thing specializers."
  (list conn--thing-command-generalizer))


;;;;; Enter/Exit Functions

(defvar conn-state-entry-functions nil
  "Hook run when a state is entered.

When this hook is run `conn-previous-state' will be bound to the state
that has just been exited.")

(defvar-local conn--state-defered nil
  "Code to be run when the current state is exited.")

(defvar conn-state-lighter-separator "→"
  "Separator string for state lighters in `conn-lighter'.")

(defconst conn--lighter-cache
  (make-hash-table :test 'equal :weakness 'value))

(defun conn--get-lighter ()
  (with-memoization (buffer-local-value 'conn-lighter (current-buffer))
    (with-memoization (gethash conn--state-stack conn--lighter-cache)
      (let ((lighter (conn-state-get conn-current-state :lighter)))
        (dolist (elem (cdr conn--state-stack))
          (setq lighter
                (if elem
                    (concat (conn-state-get elem :lighter)
                            conn-state-lighter-separator
                            lighter)
                  (concat "[" lighter "]"))))
        (concat " [" lighter "]")))))

(defun conn-update-lighter (&optional buffer)
  "Force the mode-line lighter to be updated in BUFFER.

If BUFFER is nil then use the current buffer."
  (setf (buffer-local-value 'conn-lighter (or buffer (current-buffer))) nil)
  (force-mode-line-update))

(defmacro conn-state-defer (&rest body)
  "Defer evaluation of BODY until the current state is exited.

Note that if a `conn-state-defer' form is evaluated multiple times in
one state then BODY will evaluated that many times when the state is
exited.  If you want to ensure that BODY will be evaluated only once
when the current state exits then use `conn-state-defer-once'.

When BODY is evaluated `conn-next-state' will be bound to the state
that is being entered after the current state has exited."
  (declare (indent 0))
  `(push (lambda () ,@body) conn--state-defered))

(defmacro conn-state-defer-once (&rest body)
  "Like `conn-state-defer' but BODY will be evaluated only once per state.

For more information see `conn-state-defer'."
  (declare (indent 0))
  (cl-with-gensyms (id)
    `(when (cl-loop for fn in conn--state-defered
                    never (eq ',id (car-safe fn)))
       (push (cons ',id (lambda () ,@body))
             conn--state-defered))))

(define-inline conn--state-call-deferred ()
  (inline-quote
   (dolist (fn (cl-shiftf conn--state-defered nil))
     (funcall (or (cdr-safe fn) fn)))))

(defun conn--setup-state-properties (state)
  (if (conn-state-get state :no-keymap)
      (setf conn--state-map nil
            conn--major-mode-map nil
            conn--minor-mode-maps nil)
    (setf conn--state-map `((conn-local-mode . ,(conn--compose-state-map state)))
          conn--major-mode-map `((conn-local-mode . ,(conn--compose-major-mode-map state)))
          conn--minor-mode-maps (conn-state-minor-mode-maps-alist state)))
  (setf conn--hide-mark-cursor (or (when-let* ((hide (conn-get-buffer-property
                                                      :hide-mark-cursor)))
                                     (if (eq hide t) t
                                       (alist-get state hide)))
                                   (when-let* ((hide (conn-get-mode-property
                                                      major-mode :hide-mark-cursor)))
                                     (if (eq hide t) t
                                       (alist-get state hide)))
                                   (conn-state-get state :hide-mark-cursor))
        cursor-type (conn-state-get state :cursor nil t)))

(cl-defgeneric conn-enter-state (state)
  "Enter STATE.

Code that should be run when a state is entered should be added as
methods to this function.  The (conn-substate STATE) method specializer is
provided so that code can be run for every state inheriting from some
state.

To execute code when a state is exiting use `conn-state-defer'."
  (:method ((_state (eql 'nil))) "Noop" nil)
  (:method ((_state (conn-substate t))) "Noop" nil)
  (:method (state) (error "Attempting to enter unknown state: %s" state)))

(cl-defmethod conn-enter-state :around ((state (conn-substate t)))
  (unless (symbol-value state)
    (when (conn-state-get state :abstract)
      (error "Attempting to enter abstract state %s" state))
    (let (conn-previous-state)
      (unwind-protect
          (progn
            (let ((conn-next-state state))
              (conn--state-call-deferred))
            (cl-shiftf conn-previous-state conn-current-state state)
            (conn--setup-state-properties state)
            (conn--activate-input-method)
            (unless (conn--mode-maps-sorted-p state)
              (conn--sort-mode-maps state))
            (cl-call-next-method)
            (conn-update-lighter)
            (set state t)
            (conn-state-defer
              (set conn-current-state nil)
              (setq cursor-type t
                    conn-current-state nil)))
        (unless (symbol-value state)
          (conn-local-mode -1)
          (message "Error entering state %s." state)))
      (run-hook-wrapped
       'conn-state-entry-functions
       (lambda (fn)
         (condition-case err
             (funcall fn)
           (error
            (remove-hook 'conn-state-entry-functions fn)
            (message "Error in conn-state-entry-functions: %s" (car err)))))))))

(defun conn-push-state (state)
  "Push STATE to the state stack."
  (unless (symbol-value state)
    (conn-enter-state state)
    (push state conn--state-stack)))

(defun conn-pop-state ()
  "Pop to the previous state in the state stack."
  (interactive)
  (if-let* ((state (cadr conn--state-stack)))
      (progn
        (conn-enter-state state)
        (pop conn--state-stack))
    (conn-push-state
     (conn-state-get conn-current-state :pop-alternate
                     t 'conn-command-state))))

(defun conn-peek-state ()
  "Peek the state stack.

Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (cadr conn--state-stack))

(defun conn-enter-recursive-stack (state)
  "Enter a recursive state stack."
  (conn-enter-state state)
  ;; Ensure the lighter gets updates even if we haven't changed state
  (conn-update-lighter)
  (push nil conn--state-stack)
  (push state conn--state-stack))

(defun conn-exit-recursive-stack ()
  "Exit the current recursive state stack.

If there is not recursive stack an error is signaled."
  (interactive)
  (if-let* ((tail (memq nil conn--state-stack)))
      (progn
        (conn-enter-state (cadr tail))
        (setq conn--state-stack (cdr tail))
        ;; Ensure the lighter gets updates
        ;; even if we haven't changed state
        (conn-update-lighter))
    (error "Not in a recursive state")))


;;;;; State Definitions

(defun conn--define-state (name docstring parents properties no-inherit-keymaps)
  (cl-labels ((setup-properties (table)
                (cl-loop with kvs = properties
                         for (k v) on kvs by #'cddr
                         do (puthash k v table))
                (cl-callf seq-union (gethash :no-inherit-keymaps table)
                  no-inherit-keymaps)))
    (if-let* ((state-obj (conn--find-state name)))
        (let ((prev-parents (conn-state--parents state-obj)))
          (remhash name conn--state-all-parents-cache)
          (setup-properties (setf (conn-state--properties state-obj)
                                  (make-hash-table :test 'eq)))
          (setf (conn-state--parents state-obj) parents)
          (dolist (former (seq-difference prev-parents parents))
            (cl-callf2 delq name (conn-state--children
                                  (conn--find-state former))))
          (let (new-mode-maps)
            (dolist (parent (seq-difference parents prev-parents))
              (pcase-dolist (`(,mode . ,_)
                             (cdr (conn-state-minor-mode-maps-alist parent)))
                (unless (gethash (cons name mode) conn--minor-mode-maps-cache)
                  (conn--ensure-minor-mode-map name mode)
                  (push mode new-mode-maps))))
            (conn--rebuild-state-keymaps name)
            (dolist (child (conn-state-all-children name))
              (remhash child conn--state-all-parents-cache)
              (dolist (mode new-mode-maps)
                (conn--ensure-minor-mode-map child mode))
              (conn--rebuild-state-keymaps child))))
      (let* ((state-obj (conn--make-state name docstring parents)))
        (setf (conn--find-state name) state-obj)
        (setup-properties (conn-state--properties state-obj))
        (dolist (parent parents)
          (cl-pushnew name (conn-state--children
                            (conn--find-state parent))))
        (unless (conn-state-get name :no-keymap)
          (dolist (parent parents)
            (pcase-dolist (`(,mode . ,_)
                           (cdr (conn-state-minor-mode-maps-alist parent)))
              (conn--ensure-minor-mode-map name mode))))))))

(defmacro conn-define-state (name parents &rest properties)
  "Define a conn state NAME.

Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.

PARENTS is a list of states from which NAME should inherit properties
and keymaps.

PROPERTIES is a property list defining the state properties for NAME.  A
state may have any number of properties and a state will inherit the
value for a property from its parents if it does not set the value
explicitly.

The following properties have a special meaning:

:LIGHTER is the mode-line lighter text for NAME.

:HIDE-MARK-CURSOR if non-nil will hide the mark cursor in NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in
NAME.

:CURSOR is the `cursor-type' in NAME.

:ABSTRACT if non-nil indicates that NAME is only for other states to
inherit from and should never be entered directly.

\(fn NAME PARENTS &optional DOCSTRING [KEY VALUE ...])"
  (declare (debug ( name form string-or-null-p
                    [&rest keywordp sexp]))
           (indent 2))
  (let ((docstring (or (and (stringp (car properties))
                            (pop properties))
                       (format "Non-nil when `%s' is active." name))))
    (cl-assert (plistp properties))
    `(progn
       (dolist (parent ',parents)
         (cl-check-type (conn--find-state parent) conn-state))
       (cl-assert
        (cl-loop for parent in ',parents
                 never (memq ',name (conn-state-all-parents parent)))
        nil "Cycle detected in %s inheritance hierarchy" ',name)
       (conn--define-state
        ',name
        ,docstring
        (list ,@(mapcar (lambda (p) `',(or (car-safe p) p)) parents))
        (list ,@(cl-loop for (key value) on properties by #'cddr
                         nconc (pcase key
                                 ((pred keywordp) (list key value))
                                 ((pred symbolp) `(',key ,value))
                                 (_ (error "State property name must be a symbol")))))
        (list ,@(cl-loop for p in parents
                         when (and (consp p)
                                   (eq (cadr p) :no-inherit-keymap))
                         collect `',(car p))))
       (defvar-local ,name nil ,docstring)
       ',name)))

(conn-define-state conn-null-state ()
  "An empty state.

For use in buffers that should not have any other state."
  :no-keymap t
  :lighter "Ø"
  :hide-mark-cursor t
  :cursor '(bar . 4))

(conn-define-state conn-movement-state ()
  "A `conn-mode' state moving in a buffer."
  :suppress-input-method t
  :abstract t)

(conn-define-state conn-menu-state ()
  "A `conn-mode' state for remapping key menus."
  :abstract t)

(conn-define-state conn-command-state (conn-menu-state conn-movement-state)
  "A `conn-mode' state for editing test."
  :pop-alternate 'conn-emacs-state
  :lighter "C"
  :suppress-input-method t
  :cursor 'box)

(conn-define-state conn-outline-state ()
  "State for editing outline sections."
  :cursor '(hbar . 10)
  :lighter "*"
  :suppress-input-method t)

(conn-define-state conn-org-state (conn-outline-state)
  "A `conn-mode' state for structural editing of `org-mode' buffers.")

(conn-define-state conn-emacs-state ()
  "A `conn-mode' state for inserting text.

By default `conn-emacs-state' does not bind anything."
  :lighter "E"
  :cursor '(bar . 4))

(conn-define-state conn-mode-line-face-state ()
  "An abstract state for adding a mode-line face to a state.

Causes the mode-line face to be remapped to the face specified by the
:mode-line-face state property when the state is current."
  :abstract t
  :no-keymap t)

(cl-defmethod conn-enter-state ((state (conn-substate conn-mode-line-face-state)))
  (when-let* ((face (conn-state-get state :mode-line-face))
              (cookie (face-remap-add-relative 'mode-line face)))
    (conn-state-defer
      (face-remap-remove-relative cookie)))
  (cl-call-next-method))

;;;;; Read Thing State

(defface conn-read-thing-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(conn-define-state conn-read-thing-common-state (conn-command-state
                                                 conn-mode-line-face-state)
  "Common elements of thing reading states."
  :suppress-input-method t
  :mode-line-face 'conn-read-thing-mode-line-face
  :abstract t)

;;;;; Emacs State

(defvar conn-emacs-state-register nil
  "If non-nil specifies a register to contain the last `conn-emacs-state' position.")

(defvar-local conn-emacs-state-ring nil
  "Ring of previous positions where `conn-emacs-state' was exited.")

(cl-defmethod conn-enter-state ((_state (conn-substate conn-emacs-state)))
  (conn-state-defer
    (unless (eql (point) (conn-ring-head conn-emacs-state-ring))
      (conn-ring-insert-front conn-emacs-state-ring (point-marker)))
    (when conn-emacs-state-register
      (when-let* ((marker (get-register conn-emacs-state-register))
                  ((markerp marker)))
        (set-marker marker (point) (current-buffer)))
      (set-register conn-emacs-state-register (point-marker)))))

(defun conn-copy-emacs-state-ring ()
  "Create a copy of `conn-emacs-state-ring'.

Used by `conn--clone-buffer-setup' to copy the ring when cloning a buffer."
  (when (conn-ring-p conn-emacs-state-ring)
    (let ((new-ring (copy-conn-ring conn-emacs-state-ring)))
      (setf (conn-ring-list new-ring)
            (cl-loop for mk in (conn-ring-list new-ring)
                     collect (copy-marker (marker-position mk)))
            (conn-ring-history new-ring)
            (cl-loop with old-list = (conn-ring-list conn-emacs-state-ring)
                     with new-list = (conn-ring-list new-ring)
                     for elem in (conn-ring-history new-ring)
                     for pos = (seq-position old-list elem)
                     when pos collect (nth pos new-list))
            conn-emacs-state-ring new-ring))))

;;;;; Autopop

(conn-define-state conn-autopop-state ()
  "Abstract state that automatically pops the state after executing a command.

The :pop-predicate state property is called at the end of
`post-command-hook' and should return non-nil if the state should be
popped and nil if the state should stay active.  The default value of
:pop-predicate is `always'.  Note that `pop-predicate' is not called and
the state stays active if the previous command was a prefix command."
  :abstract t
  :no-keymap t
  :pop-predicate #'always)

(cl-defmethod conn-enter-state ((state (conn-substate conn-autopop-state)))
  (unless conn--state-stack
    (error "An autopop state cannot be the base state"))
  (letrec ((prefix-command nil)
           (preserve-state
            (lambda ()
              (setq prefix-command t)))
           (msg-fn (conn-state-get state :message-function))
           (pop-pred
            (let ((pred (conn-state-get state :pop-predicate)))
              (cl-check-type pred function)
              (lambda ()
                (unless (or (cl-shiftf prefix-command nil)
                            (not (funcall pred)))
                  (when msg-fn
                    (remove-hook 'post-command-hook msg-fn t))
                  (remove-hook 'post-command-hook pop-pred t)
                  (remove-hook 'prefix-command-preserve-state-hook preserve-state)
                  (if (eq conn-current-state state)
                      (conn-pop-state)
                    (setq conn--state-stack (delq state conn--state-stack)))))))
           (setup
            (lambda ()
              (remove-hook 'post-command-hook setup t)
              (add-hook 'prefix-command-preserve-state-hook preserve-state)
              (when msg-fn
                (add-hook 'post-command-hook msg-fn 91 t))
              (add-hook 'post-command-hook pop-pred 90 t))))
    (add-hook 'post-command-hook setup 99 t)
    (cl-call-next-method)))

(conn-define-state conn-one-command-state (conn-command-state
                                           conn-autopop-state)
  "Execute one command in `conn-command-state'."
  :lighter "1C")


;;;;; State Setup Functions

(defun conn-setup-commit-state ()
  "Set the base state to `conn-emacs-state' in commit message buffers."
  (when (buffer-match-p "COMMIT_EDITMSG" (current-buffer))
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-commit-state -80)

(defun conn-setup-edmacro-state ()
  "Set the base state to `conn-command-state' in edit macro buffers."
  (when (buffer-match-p "\\*Edit Macro\\*" (current-buffer))
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-edmacro-state -80)

(defun conn-setup-dired-state ()
  "Set the base state to `conn-emacs-state' in dired buffers."
  (when (derived-mode-p 'dired-mode)
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-dired-state -50)

(defun conn-setup-null-state ()
  "Set the base state to `conn-null-state' in `conn-null-state-modes' buffers."
  (when (derived-mode-p conn-null-state-modes)
    (conn-push-state 'conn-null-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-null-state -90)

(defun conn-setup-command-state ()
  "Set base state to `conn-command-state' in `conn-command-state-modes' buffers."
  (when (derived-mode-p conn-command-state-modes)
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-command-state)

(defun conn-setup-minibuffer-state ()
  "Setup `minibuffer-mode' buffer state."
  (when (eq major-mode 'minibuffer-mode)
    (setf (alist-get 'conn-emacs-state
                     (conn-get-buffer-property :hide-mark-cursor))
          t)
    (conn-push-state 'conn-emacs-state)
    (add-hook 'minibuffer-setup-hook
              (conn-anaphoricate hook
                (lambda ()
                  (conn--push-ephemeral-mark)
                  (remove-hook 'minibuffer-setup-hook hook))))
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-minibuffer-state -95)


;;;; State Command Loops

(defvar conn-state-eval-last-command nil
  "Last command read by `conn-eval-with-state'.")

(defvar conn-state-eval-inhibit-message nil
  "Value for `inhibit-message' in `conn-eval-with-state' message functions.")

(defvar conn--state-eval-prefix-mag nil)
(defvar conn--state-eval-prefix-sign nil)
(defvar conn--state-eval-error-message nil)
(defvar conn--state-eval-message nil)
(defvar conn--state-eval-message-timeout nil)
(defvar conn--state-eval-exiting nil)

(defun conn-state-eval-prefix-arg ()
  "Return the value of the current prefix argument during `conn-eval-with-state'."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond (conn--state-eval-prefix-mag
         (* (if conn--state-eval-prefix-sign -1 1) conn--state-eval-prefix-mag))
        (conn--state-eval-prefix-sign -1)))

(defun conn-state-eval-consume-prefix-arg ()
  "Return the value of the current prefix argument during `conn-eval-with-state'.

Resets the current prefix argument."
  (prog1 (conn-state-eval-prefix-arg)
    (setf conn--state-eval-prefix-mag nil
          conn--state-eval-prefix-sign nil)))

(defun conn-state-eval-handle (&optional form)
  "Handle the current command.

This function should be called from any function passed as the
:command-handler argument to `conn-eval-with-state' when the function
chooses to handle a command."
  (setf conn--state-eval-error-message "")
  form)

(defun conn-state-eval-message (format-string &rest args)
  (let ((inhibit-message conn-state-eval-inhibit-message)
        (message-log-max nil))
    (setq conn--state-eval-message (apply #'format format-string args)
          conn--state-eval-message-timeout (time-add nil minibuffer-message-timeout))))

(defun conn--state-eval-prompt (prompt arguments)
  (let ((inhibit-message conn-state-eval-inhibit-message)
        (message-log-max nil))
    (message
     (substitute-command-keys
      (concat
       (propertize prompt 'face 'minibuffer-prompt)
       " (arg: "
       (propertize
        (cond (conn--state-eval-prefix-mag
               (number-to-string
                (* (if conn--state-eval-prefix-sign -1 1)
                   conn--state-eval-prefix-mag)))
              (conn--state-eval-prefix-sign "[-1]")
              (t "[1]"))
        'face 'read-multiple-choice-face)
       ", \\[reset-arg] reset"
       (string-join (cons nil (conn-display-argument arguments)) "; ")
       "): "
       (when conn--state-eval-message (format "[%s] " conn--state-eval-message))
       (propertize conn--state-eval-error-message 'face 'error))))))

;; From embark
(defun conn--all-bindings (keymap)
  (let (bindings)
    (map-keymap
     (lambda (_key def)
       (pcase (keymap--menu-item-binding def)
         ((and (pred keymapp) keymap)
          (setq bindings (nconc (conn--all-bindings keymap) bindings)))
         ((and (pred symbolp) sym)
          (push sym bindings))))
     (keymap-canonicalize keymap))
    bindings))

(defun conn--state-eval-completing-read (state args)
  (when-let* ((metadata (conn-state-get state :loop-completion-metadata))
              (table
               (cl-loop for sym in (mapcan #'conn--all-bindings
                                           (current-active-maps))
                        when (conn-argument-completion-predicate args sym)
                        collect sym)))
    (condition-case _
        (intern
         (completing-read
          "Command: "
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata ,@metadata)
              (complete-with-action action table string pred)))
          nil t))
      (quit nil))))

(cl-defun conn--eval-with-state (state arglist &key command-handler prompt prefix pre post)
  (conn-protected-let* ((arguments (list arglist)
                                   (conn-cancel-argument (car arguments)))
                        (prompt (or prompt (symbol-name state)))
                        (conn--state-eval-prefix-mag (when prefix (abs prefix)))
                        (conn--state-eval-prefix-sign (when prefix (> 0 prefix)))
                        (conn--state-eval-error-message "")
                        (conn--state-eval-message nil)
                        (conn--state-eval-message-timeout nil)
                        (conn--state-eval-exiting nil)
                        (inhibit-message t))
    (cl-labels ((updated-p (old new)
                  (or (not (eq (cl-type-of old) (cl-type-of new)))
                      (if (and (listp old) old new)
                          (or (updated-p (car old) (car new))
                              (updated-p (cdr old) (cdr new)))
                        (not (conn-argument-equal-p old new)))))
                (update-args (cmd)
                  (setf conn--state-eval-error-message "Invalid Command")
                  (let* ((handler (if command-handler
                                      (funcall command-handler cmd arguments)
                                    arguments))
                         (next (conn-update-argument (car handler) cmd)))
                    (when (updated-p (car arguments) next)
                      (setq conn--state-eval-error-message "")
                      (setq arguments (cons next handler))))))
      (conn-with-recursive-stack state
        (while (conn-argument-required-p (car arguments))
          (when (and conn--state-eval-message-timeout
                     (time-less-p conn--state-eval-message-timeout nil))
            (setq conn--state-eval-message nil
                  conn--state-eval-message-timeout nil))
          (conn--state-eval-prompt prompt (car arguments))
          (let ((cmd (key-binding (read-key-sequence nil) t)))
            (setf conn--state-eval-error-message "")
            (while (arrayp cmd) ; keyboard macro
              (setq cmd (key-binding cmd t)))
            (when pre (funcall pre cmd))
            (pcase cmd
              ('nil)
              ('digit-argument
               (let* ((char (if (integerp last-input-event)
                                last-input-event
                              (get last-input-event 'ascii-character)))
                      (digit (- (logand char ?\177) ?0)))
                 (setf conn--state-eval-prefix-mag
                       (if (integerp conn--state-eval-prefix-mag)
                           (+ (* 10 conn--state-eval-prefix-mag) digit)
                         (when (/= 0 digit) digit)))))
              ('backward-delete-arg
               (when conn--state-eval-prefix-mag
                 (cl-callf floor conn--state-eval-prefix-mag 10)))
              ('reset-arg
               (setf conn--state-eval-prefix-mag nil))
              ('negative-argument
               (cl-callf not conn--state-eval-prefix-sign))
              ('keyboard-quit
               (keyboard-quit))
              ((or 'help 'execute-extended-command)
               (when-let* ((cmd (conn--state-eval-completing-read state arguments)))
                 (update-args cmd)))
              (_ (update-args cmd)))
            (setq conn-state-eval-last-command cmd)
            (when post (funcall post cmd)))))
      (let ((inhibit-message nil))
        (message nil)
        (eval (conn-eval-argument (car arguments)) t)))))

(eval-and-compile
  (defun conn--state-eval-quote (form)
    (cl-labels ((qt (form)
                  (pcase form
                    ('nil nil)
                    (`(&! ,exp . ,tail)
                     (cons ``(list ',',exp) (qt tail)))
                    (`(& ,exp . ,tail)
                     (cons ``(list '',,exp) (qt tail)))
                    (`(&& ,exp . ,tail)
                     (cons ``(mapcar 'macroexp-quote ',,exp) (qt tail)))
                    (`(,head . ,tail)
                     (cons (if (consp head)
                               ``(list ,(list 'nconc ,@(qt head)))
                             (qt head))
                           (if (listp tail) (qt tail) (list `'',tail))))
                    (_ ``(list ',',form)))))
      `(list 'nconc ,@(qt form)))))

(defmacro conn-state-eval-quote (form)
  (conn--state-eval-quote form))

(defmacro conn-eval-with-state (state form &rest keys)
  "Eval FORM after replacing arguments with values read in STATE.

\(fn STATE ARGLIST &key COMMAND-HANDLER PROMPT PREFIX PRE POST)"
  (declare (indent 2))
  `(eval (conn--eval-with-state ,state
                                ,(conn--state-eval-quote form)
                                ,@keys)
         t))

(defun conn--fontify-state-eval ()
  (font-lock-add-keywords
   nil '(("\\_<\\(&[&!]?\\)\\_>" 1 'font-lock-keyword-face))))

;;;###autoload
(define-minor-mode conn-fontify-state-eval-mode
  "Highlight `conn-eval-with-state' symbols."
  :global t
  :lighter nil
  (if conn-fontify-state-eval-mode
      (add-hook 'emacs-lisp-mode-hook 'conn--fontify-state-eval)
    (remove-hook 'emacs-lisp-mode-hook 'conn--fontify-state-eval)))

;;;;; Loop Arguments

(oclosure-define (conn-state-eval-argument
                  (:predicate conn-state-eval-argument-p)
                  (:copier conn-set-argument (value &aux (set-flag t)))
                  (:copier conn-unset-argument (value &aux (set-flag nil))))
  (value :type t)
  (set-flag :type boolean)
  (required :type boolean)
  (name :type (or nil string function)))

(defalias 'conn-state-eval-argument-name
  'conn-state-eval-argument--name)

(defalias 'conn-state-eval-argument-value
  'conn-state-eval-argument--value)

(cl-defgeneric conn-cancel-argument (argument)
  ( :method (_arg) nil)
  ( :method ((_arg (eql nil))) nil)
  ( :method ((arg list))
    (conn-cancel-argument (car arg))
    (conn-cancel-argument (cdr arg))))

(cl-defgeneric conn-argument-required-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((_arg (eql nil))) nil)
  ( :method ((arg list))
    (or (conn-argument-required-p (car arg))
        (conn-argument-required-p (cdr arg))))
  ( :method ((arg conn-state-eval-argument))
    (and (conn-state-eval-argument--required arg)
         (not (conn-state-eval-argument--set-flag arg)))))

(cl-defgeneric conn-argument-equal-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (a b) (eql a b)))

(cl-defgeneric conn-update-argument (argument form)
  ( :method (arg _form) arg)
  ( :method ((_arg (eql nil)) _form) nil)
  ( :method ((arg list) form)
    (cons (conn-update-argument (car arg) form)
          (conn-update-argument (cdr arg) form)))
  ( :method ((arg conn-state-eval-argument) form)
    (funcall arg arg form)))

(cl-defgeneric conn-eval-argument (argument)
  (declare (important-return-value t))
  ( :method (arg) arg)
  ( :method ((_arg (eql nil))) nil)
  ( :method ((arg list))
    (cons (conn-eval-argument (car arg))
          (conn-eval-argument (cdr arg))))
  ( :method ((arg conn-state-eval-argument))
    (conn-state-eval-argument-value arg)))

(cl-defgeneric conn-display-argument (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((_arg (eql nil))) nil)
  ( :method ((arg list))
    (nconc (ensure-list (conn-display-argument (car arg)))
           (ensure-list (conn-display-argument (cdr arg)))))
  ( :method ((arg conn-state-eval-argument))
    (pcase (conn-state-eval-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and (pred functionp) fn)
       (and-let* ((str (funcall fn arg))
                  ((stringp str)))
         str)))))

(cl-defgeneric conn-argument-completion-predicate (argument symbol)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg _sym) nil)
  ( :method ((_arg (eql nil)) _sym) nil)
  ( :method ((arg list) sym)
    (or (conn-argument-completion-predicate (car arg) sym)
        (conn-argument-completion-predicate (cdr arg) sym))))


;;;; Mark Handlers

(defvar-local conn-mark-handler-overrides-alist nil
  "Buffer local overrides for command mark handlers.

Is an alist of the form ((CMD . MARK-HANDLER) ...).

For the meaning of MARK-HANDLER see `conn-command-mark-handler'.")

(define-inline conn-command-mark-handler (command)
  "Return the mark handler for COMMAND."
  (declare (important-return-value t)
           (side-effect-free t)
           (gv-setter conn-set-command-mark-handler))
  (inline-letevals (command)
    (inline-quote
     (and (symbolp ,command)
          (or (alist-get ,command conn-mark-handler-overrides-alist)
              (get ,command :conn-mark-handler))))))

(defun conn-set-command-mark-handler (command handler)
  (put command :conn-mark-handler handler))

(defun conn-symbol-handler (beg)
  "Mark handler for symbols."
  (let ((list (ignore-errors (bounds-of-thing-at-point 'list))))
    (cond ((not (derived-mode-p 'prog-mode))
           (conn-continuous-thing-handler beg))
          ((and (conn--point-in-comment-or-string-p)
                (save-excursion
                  (goto-char beg)
                  (conn--point-in-comment-or-string-p)))
           (conn-continuous-thing-handler beg))
          ((or (conn--point-in-comment-or-string-p)
               (save-excursion
                 (goto-char beg)
                 (conn--point-in-comment-or-string-p)))
           (conn-discrete-thing-handler beg))
          ((equal list (save-excursion
                         (goto-char beg)
                         (bounds-of-thing-at-point 'list)))
           (conn-continuous-thing-handler beg))
          ((conn-discrete-thing-handler beg)))))

(defun conn-continuous-thing-handler (beg)
  "Mark the things which have been moved over."
  (ignore-errors
    (cond ((= 0 (abs (prefix-numeric-value current-prefix-arg))))
          ((= (point) beg)
           (pcase (bounds-of-thing-at-point conn-this-command-thing)
             (`(,beg . ,end)
              (cond ((= (point) beg) (conn--push-ephemeral-mark end))
                    ((= (point) end) (conn--push-ephemeral-mark beg))))))
          ((let ((dir (pcase (- (point) beg)
                        (0 0)
                        ((pred (< 0)) 1)
                        ((pred (> 0)) -1))))
             (save-excursion
               (goto-char beg)
               (forward-thing conn-this-command-thing dir)
               (forward-thing conn-this-command-thing (- dir))
               (conn--push-ephemeral-mark)))))))

(defun conn-discrete-thing-handler (_beg)
  "Mark the thing at point."
  (pcase (ignore-errors (bounds-of-thing-at-point conn-this-command-thing))
    (`(,beg . ,end)
     (conn--push-ephemeral-mark (if (= (point) end) beg end)))))

(defun conn-jump-handler (beg)
  "Place a mark where point used to be."
  (unless (= beg (point))
    (conn--push-ephemeral-mark beg)))


;;;; Mark Cursor

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :set (lambda (sym val)
         (set sym val)
         (put 'conn--mark-cursor 'priority val))
  :group 'conn)

(defface conn-mark-face
  '((t (:inherit region :extend nil)))
  "Face for conn mark cursor."
  :group 'conn-faces)

(defvar-local conn--mark-cursor nil)
(put 'conn--mark-cursor 'permanent-local t)

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.

Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--ephemeral-mark nil)

(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)
(put 'conn--mark-cursor 'conn-overlay t)
(put 'conn--mark-cursor 'overlay-after-string
     (propertize " " 'face 'conn-mark-face))

(defun conn--mark-cursor-redisplay (win)
  (if (or (not conn-local-mode)
          conn--hide-mark-cursor
          (null (mark t))
          (and (window-minibuffer-p win)
               (not (eq win (active-minibuffer-window)))))
      (progn
        (when conn--mark-cursor
          (delete-overlay conn--mark-cursor))
        (setf conn--mark-cursor nil))
    (unless conn--mark-cursor
      (setf conn--mark-cursor (make-overlay (mark t) (1+ (mark t))))
      (overlay-put conn--mark-cursor 'category 'conn--mark-cursor))
    (cond ((or (use-region-p)
               (= (point-max) (mark t) (point)))
           (when (overlay-get conn--mark-cursor 'after-string)
             (overlay-put conn--mark-cursor 'after-string nil))
           (unless (eql (overlay-start conn--mark-cursor)
                        (overlay-end conn--mark-cursor))
             (move-overlay conn--mark-cursor (point-max) (point-max))))
          ((and (eql (overlay-start conn--mark-cursor) (mark t))
                (or (eql (overlay-end conn--mark-cursor) (1+ (mark t)))
                    (and (eql (overlay-start conn--mark-cursor) (point-max))
                         (overlay-get conn--mark-cursor 'after-string)))))
          ((= (mark t) (point-max))
           (move-overlay conn--mark-cursor (point-max) (point-max))
           (overlay-put conn--mark-cursor 'after-string
                        (get 'conn--mark-cursor 'overlay-after-string)))
          ((and (eql (char-after (mark t)) ?\t)
                (< 1 (save-excursion
                       (goto-char (mark t))
                       (let ((col (current-column)))
                         (- (indent-next-tab-stop col) col)))))
           (move-overlay conn--mark-cursor (mark t) (mark t))
           (overlay-put conn--mark-cursor 'after-string
                        (get 'conn--mark-cursor 'overlay-after-string)))
          (t
           (move-overlay conn--mark-cursor (mark t) (1+ (mark t)))
           (overlay-put conn--mark-cursor 'after-string nil)))))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.

For the meaning of MSG and ACTIVATE see `push-mark'."
  (if (not conn-local-mode)
      (push-mark location (not msg) activate)
    (push-mark location (not msg) activate)
    (setq conn--ephemeral-mark t)
    nil))

(defvar conn--movement-ring-rotating nil)
(defvar conn--movement-tick nil)
(defvar conn--movement-mark nil)

(defun conn--mark-pre-command-hook ()
  (unless conn--hide-mark-cursor
    (set-marker conn-this-command-start (point))
    (setq conn--movement-tick (buffer-chars-modified-tick)
          conn--movement-mark (mark t)
          conn--movement-ring-rotating nil
          conn-this-command-thing nil
          conn-this-command-handler nil)))

(defun conn--mark-post-command-hook ()
  (unless conn--hide-mark-cursor
    (cl-callf2 assq-delete-all (recursion-depth) conn--last-perform-bounds)
    (unless conn-this-command-thing
      (setq conn-this-command-thing (or (conn-command-thing this-command)
                                        (conn-command-thing real-this-command))))
    (when (and conn-local-mode
               (marker-position conn-this-command-start)
               (eq (current-buffer) (marker-buffer conn-this-command-start)))
      (when-let* (((not (region-active-p)))
                  (handler
                   (or (conn-command-mark-handler this-command)
                       (conn-command-mark-handler real-this-command))))
        (with-demoted-errors "Error in Mark Handler: %s"
          (funcall handler conn-this-command-start)))
      (unless (or conn--movement-ring-rotating
                  (null conn--movement-mark)
                  (not (eql conn--movement-tick (buffer-chars-modified-tick)))
                  (eql (mark t) conn--movement-mark))
        (with-demoted-errors "Error in Movement Ring: %s"
          (conn-push-region (point) (mark t)))))))

(defun conn--setup-mark ()
  (if conn-mode
      (progn
        (setq conn--prev-mark-even-if-inactive mark-even-if-inactive
              mark-even-if-inactive t)
        (add-hook 'pre-redisplay-functions 'conn--mark-cursor-redisplay 91)
        (add-hook 'pre-command-hook #'conn--mark-pre-command-hook)
        (add-hook 'post-command-hook #'conn--mark-post-command-hook))
    (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
    (remove-hook 'pre-redisplay-functions 'conn--mark-cursor-redisplay)
    (remove-hook 'pre-command-hook #'conn--mark-pre-command-hook)
    (remove-hook 'post-command-hook #'conn--mark-post-command-hook)))


;;;; Things

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER.

HANDLER will be run from the `post-command-hook' and should be a
function of one argument, the location of `point' before the command
ran.  HANDLER is responsible for calling `conn--push-ephemeral-mark' in
order to mark the region that should be defined by any of COMMANDS."
  (dolist (cmd commands)
    (cl-assert (not (conn-state-name-p cmd))
               nil "States and thing commands must be disjoint")
    (setf (conn-command-thing cmd) thing
          (conn-command-mark-handler cmd) handler)))

(defmacro conn-define-mark-command (name thing &optional ignore-mark-active)
  `(progn
     (defun ,name ()
       (interactive)
       (pcase (ignore-errors (bounds-of-thing-at-point ',thing))
         (`(,beg . ,end)
          (if ,(unless ignore-mark-active '(region-active-p))
              (pcase (car (read-multiple-choice
                           "Mark to?"
                           '((?e "end")
                             (?a "beginning"))))
                (?e (goto-char end))
                (?b (goto-char beg)))
            (goto-char beg)
            (conn--push-ephemeral-mark end)))))
     (conn-register-thing-commands ',thing 'ignore ',name)))


;;;; Thing Definitions

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)
(conn-define-mark-command conn-mark-comment comment)

(conn-register-thing 'dispatch)

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

(conn-register-thing 'defun :forward-op 'conn-forward-defun)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing defun)))
  (conn-dispatch-all-defuns))

(conn-register-thing
 'visual-line
 :forward-op (lambda (&optional N)
               (let ((line-move-visual t))
                 (vertical-motion 0)
                 (line-move N t))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing visual-line)))
  'conn-dispatch-visual-lines)

(conn-define-mark-command conn-mark-visual-line visual-line)

(conn-register-thing
 'region
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing
 'buffer-after-point
 :bounds-op (lambda () (cons (point) (point-max))))

(conn-register-thing
 'buffer-before-point
 :bounds-op (lambda () (cons (point-min) (point))))

(conn-define-mark-command conn-mark-after-point buffer-after-point t)
(conn-define-mark-command conn-mark-before-point buffer-before-point t)

(conn-register-thing
 'visible
 :bounds-op (lambda () (cons (window-start) (window-end))))

(conn-define-mark-command conn-mark-visible visible)

(conn-register-thing-commands
 'recursive-edit nil
 'recursive-edit 'exit-recursive-edit)

(conn-register-thing-commands 'dispatch nil 'conn-dispatch)

(conn-register-thing-commands
 'isearch nil
 'isearch-forward
 'isearch-backward
 'isearch-forward-regexp
 'isearch-backward-regexp
 'isearch-forward-symbol
 'isearch-forward-word
 'isearch-forward-symbol-at-point
 'isearch-forward-thing-at-point)

(conn-register-thing-commands
 'visible nil
 'conn-scroll-up 'conn-scroll-down
 'scroll-up-command 'scroll-down-command
 'conn-mark-visible)

(conn-register-thing-commands
 'region nil
 'conn-toggle-mark-command)

(conn-register-thing 'symbol :forward-op 'forward-symbol)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing symbol)))
  (conn-dispatch-things-read-prefix 'symbol 1))

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing 'page :forward-op 'forward-page)

(conn-register-thing-commands
 'page 'conn-discrete-thing-handler
 'forward-page 'backward-page)

(defun conn-char-mark-handler (beg)
  (when current-prefix-arg
    (conn--push-ephemeral-mark beg)))

(conn-register-thing-commands
 'char 'conn-char-mark-handler
 'forward-char 'backward-char)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing word)))
  (conn-dispatch-things-read-prefix 'word 1))

(conn-register-thing-commands
 'word 'conn-symbol-handler
 'forward-word 'backward-word
 'upcase-word 'downcase-word 'capitalize-word
 'upcase-dwim 'downcase-dwim 'capitalize-dwim)

(conn-register-thing 'sexp :forward-op 'forward-sexp)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sexp)))
  (conn-dispatch-things-read-prefix 'sexp 1))

(conn-register-thing-commands
 'sexp 'conn-continuous-thing-handler
 'forward-sexp 'backward-sexp)

(conn-register-thing 'list :forward-op 'forward-list)

(conn-register-thing-commands
 'list 'conn-continuous-thing-handler
 'forward-list 'backward-list)

(defun conn--up-list-mark-handler (beg)
  (condition-case _err
      (cond ((> (point) beg)
             (save-excursion
               (forward-thing 'list -1)
               (conn--push-ephemeral-mark (point))))
            ((< (point) beg)
             (save-excursion
               (forward-thing 'list 1)
               (conn--push-ephemeral-mark (point)))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--up-list-mark-handler
 'up-list 'backward-up-list)

(defun conn--down-list-mark-handler (_beg)
  (condition-case _err
      (cond ((= (point) (save-excursion
                          (up-list 1 t t)
                          (down-list -1 t)
                          (point)))
             (conn--push-ephemeral-mark (save-excursion
                                          (up-list -1 t t)
                                          (down-list 1 t)
                                          (point))))
            ((= (point) (save-excursion
                          (up-list -1 t t)
                          (down-list 1 t)
                          (point)))
             (conn--push-ephemeral-mark (save-excursion
                                          (up-list 1 t t)
                                          (down-list -1 t)
                                          (point)))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'down-list)

(conn-register-thing 'whitespace :forward-op 'forward-whitespace)

(conn-register-thing-commands
 'whitespace 'conn-discrete-thing-handler
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing 'sentence :forward-op 'forward-sentence)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sentence)))
  (conn-dispatch-all-things 'sentence))

(conn-register-thing-commands
 'sentence 'conn-continuous-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing 'paragraph :forward-op 'forward-paragraph)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing paragraph)))
  (conn-dispatch-all-things 'paragraph))

(conn-register-thing-commands
 'paragraph 'conn-continuous-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-continuous-thing-handler
 'end-of-defun 'beginning-of-defun
 'conn-forward-defun)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing char)))
  (conn-dispatch-read-string-with-timeout))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing char)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'buffer 'conn-discrete-thing-handler
 'end-of-buffer 'beginning-of-buffer)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line)))
  'conn-dispatch-lines)

(conn-register-thing-commands
 'line 'conn-continuous-thing-handler
 'forward-line 'conn-backward-line
 'conn-line-forward-op
 'conn-goto-line)

(conn-register-thing 'line-column :forward-op 'next-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column)))
  'conn-dispatch-columns)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing line-column)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'line-column 'conn-jump-handler
 'next-line 'previous-line
 'rectangle-next-line 'rectangle-previous-line)

(conn-register-thing-commands 'line nil 'comment-line)

(conn-register-thing
 'outer-line
 :beg-op (lambda () (move-beginning-of-line nil))
 :end-op (lambda () (move-end-of-line nil)))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing outer-line)))
  'conn-dispatch-lines)

(conn-register-thing-commands
 'outer-line 'conn-discrete-thing-handler
 'move-beginning-of-line 'move-end-of-line
 'org-beginning-of-line 'org-end-of-line)

(defun conn--bounds-of-inner-line ()
  (cons
   (save-excursion
     (back-to-indentation)
     (point))
   (save-excursion
     (conn--end-of-inner-line-1)
     (point))))

(conn-register-thing
 'inner-line
 :bounds-op 'conn--bounds-of-inner-line
 :forward-op 'conn-forward-inner-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing inner-line)))
  'conn-dispatch-inner-lines)

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line)))
  'conn-dispatch-end-of-inner-lines)

(conn-register-thing-commands
 'inner-line 'conn-continuous-thing-handler
 'back-to-indentation
 'conn-forward-inner-line
 'conn-backward-inner-line
 'conn-beginning-of-inner-line
 'conn-end-of-inner-line
 'comment-line)

(conn-register-thing-commands
 'org-link 'conn-discrete-thing-handler
 'org-next-link 'org-previous-link)

(conn-register-thing-commands
 'org-link nil
 'org-insert-link-global 'org-store-link 'org-insert-link)

(conn-register-thing-commands
 'expansion nil
 'conn-expand 'conn-contract)

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'conn-beginning-of-list
 'conn-end-of-list)


;;;; Read Things

;;;;; Thing Args

(oclosure-define (conn-thing-argument
                  (:parent conn-state-eval-argument))
  (recursive-edit :type boolean))

(defun conn-thing-argument (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (conn-handle-thing-argument cmd self)))

(defun conn-thing-argument-dwim (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p))
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (conn-handle-thing-argument cmd self)))

(cl-defgeneric conn-handle-thing-argument (cmd arg)
  ( :method (_ arg) arg))

(cl-defmethod conn-handle-thing-argument ((cmd (conn-thing t)) arg)
  (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg))))

(cl-defmethod conn-handle-thing-argument ((cmd (conn-thing recursive-edit)) arg)
  (if (conn-thing-argument--recursive-edit arg)
      (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg)))
    (cl-call-next-method)))

(cl-defmethod conn-eval-argument ((arg conn-thing-argument))
  (conn-state-eval-argument-value arg))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-thing-argument) sym)
  (or (conn-thing-p sym)
      (conn-command-thing sym)
      (cl-call-next-method)))

;;;;;; Subregions

(oclosure-define (conn-subregions-argument
                  (:parent conn-state-eval-argument)))

(defun conn-subregions-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-subregions-argument
                    (value value))
      (self cmd)
    (conn-handle-subregions-argument cmd self)))

(cl-defgeneric conn-handle-subregions-argument (cmd arg)
  ( :method (_ arg) arg))

(cl-defmethod conn-handle-subregions-argument ((_cmd (eql toggle-subregions))
                                               arg)
  (conn-set-argument arg (not (conn-state-eval-argument-value arg))))

(cl-defmethod conn-handle-subregions-argument ((_cmd (eql conn-things-in-region))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-handle-subregions-argument ((_cmd (conn-thing region))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-handle-subregions-argument ((_cmd (conn-thing recursive-edit))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-subregions-argument)
                                                  sym)
  (or (eq sym 'toggle-subregions)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-subregions-argument))
  (concat "\\[toggle-subregions] "
          (propertize "subregions"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Trim

(oclosure-define (conn-trim-argument
                  (:parent conn-state-eval-argument)))

(defun conn-trim-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-trim-argument
                    (value value))
      (self cmd)
    (if (eq cmd 'toggle-trim)
        (conn-set-argument self t)
      self)))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-trim-argument)
                                                  sym)
  (or (eq sym 'toggle-trim)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-trim-argument))
  (concat "\\[toggle-trim] "
          (propertize "trim"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;; Read Mover State

(conn-define-state conn-read-thing-state (conn-read-thing-common-state)
  "A state for reading things."
  :lighter "THG"
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-h" 'help
  "," (conn-remap-key "<conn-thing-map>")
  "r" 'recursive-edit
  "x" 'toggle-trim
  "z" 'toggle-subregions
  "y" 'conn-things-in-region)

(put 'reset-arg :advertised-binding (key-parse "M-DEL"))


;;;; Bounds of Thing

(defvar conn-bounds-trim-chars " \t\r\n")

(defvar conn--last-perform-bounds nil)

(cl-defstruct (conn-bounds
               (:constructor conn-bounds--make))
  (thing nil :type symbol)
  (arg nil :type (or nil integer))
  (properties nil :type list))

(defun conn-bounds (thing arg &rest properties)
  (declare (compiler-macro
            (lambda (_exp)
              `(conn-bounds--make
                :thing ,thing
                :arg ,arg
                :properties (list ,@properties)))))
  (conn-bounds--make
   :thing thing
   :arg arg
   :properties properties))

(define-inline conn-bounds-of (thing arg)
  (inline-quote
   (setf (alist-get (recursion-depth) conn--last-perform-bounds)
         (save-mark-and-excursion
           (conn-bounds-of-subr ,thing ,arg)))))

(define-inline conn-bounds-get (bounds prop)
  (declare (gv-setter
            (lambda (val)
              `(setf (plist-get (conn-bounds-properties ,bounds) ,prop)
                     ,val))))
  (inline-letevals (bounds)
    (inline-quote
     (pcase (plist-get (conn-bounds-properties ,bounds) ,prop)
       ((and (pred functionp) fn)
        (funcall fn ,bounds))
       (val val)))))

(pcase-defmacro conn-bounds-get (property &optional pat)
  (static-if (< emacs-major-version 30)
      `(app (pcase--flip conn-bounds-get ,property)
            ,(or pat (if (keywordp property)
                         (intern (substring (symbol-name property) 1))
                       property)))
    `(app (conn-bounds-get _ ,property)
          ,(or pat (if (keywordp property)
                       (intern (substring (symbol-name property) 1))
                     property)))))

(cl-defgeneric conn-bounds-of-subr (cmd arg)
  (declare (conn-anonymous-thing-property :bounds-op)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) arg)
    (if-let* ((bounds-op (conn-anonymous-thing-property cmd :bounds-op)))
        (funcall bounds-op arg)
      (conn-bounds-of-subr (conn-anonymous-thing-parent cmd) arg))))

(defun conn--trim-bounds (bounds)
  (pcase-let (((conn-bounds-get :outer `(,beg . ,end)) bounds))
    (setf (conn-bounds-get bounds :trimmed)
          (cons (save-excursion
                  (goto-char beg)
                  (skip-chars-forward conn-bounds-trim-chars end)
                  (point))
                (save-excursion
                  (goto-char end)
                  (skip-chars-backward conn-bounds-trim-chars beg)
                  (point))))))

(cl-defmethod conn-bounds-of-subr :extra "trim" ((_ (conn-thing t)) _arg)
  (let ((bounds (cl-call-next-method)))
    (unless (plist-member (conn-bounds-properties bounds) :trimmed)
      (setf (conn-bounds-get bounds :trimmed)
            #'conn--trim-bounds))
    bounds))

(cl-defmethod conn-bounds-of-subr ((thing (conn-thing t)) arg)
  (conn-bounds thing arg :outer (bounds-of-thing-at-point thing)))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing-command t)) arg)
  (deactivate-mark t)
  (pcase (prefix-numeric-value arg)
    (0 nil)
    ((and n (or 1 -1))
     (let ((pt (point))
           (mk (mark))
           (current-prefix-arg n)
           (conn-this-command-handler (conn-command-mark-handler cmd))
           (conn-this-command-thing (conn-command-thing cmd))
           (conn-this-command-start (point-marker))
           (this-command cmd))
       (unwind-protect
           (progn
             (ignore-errors
               (call-interactively cmd)
               (funcall conn-this-command-handler conn-this-command-start))
             (unless (and (eql pt (point))
                          (eql mk (mark)))
               (conn-bounds cmd 1 :outer (cons (region-beginning) (region-end)))))
         (set-marker conn-this-command-start nil))))
    (n
     (let (subregions)
       (catch 'break
         (dotimes (_ (abs n))
           (if-let* ((bound (conn-bounds-of-subr cmd (cl-signum arg))))
               (push bound subregions)
             (throw 'break nil))))
       (conn-bounds
        cmd arg
        :outer (cl-loop for bound in subregions
                        for (b . e) = (conn-bounds-get bound :outer)
                        minimize b into beg
                        maximize e into end
                        finally return (cons beg end))
        :subregions (nreverse subregions))))))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing region)) arg)
  (conn-bounds
   cmd arg
   :outer (cons (region-beginning) (region-end))
   :subregions (cl-loop for r in (region-bounds)
                        collect (conn-bounds cmd arg :outer r))))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing buffer)) arg)
  (conn-bounds cmd arg :outer (cons (point-min) (point-max))))

(cl-defmethod conn-bounds-of-subr ((_cmd (eql conn-bounds-of)) _arg)
  (alist-get (recursion-depth) conn--last-perform-bounds))


;;;;; Perform Bounds

(conn-define-state conn-bounds-of-recursive-edit-state (conn-command-state)
  :lighter "R")

(cl-defmethod conn-enter-state ((_ (conn-substate conn-bounds-of-recursive-edit-state)))
  (setq buffer-read-only t)
  (conn-state-defer
    (setq buffer-read-only nil))
  (cl-call-next-method))

(define-keymap
  :keymap (conn-get-state-map 'conn-bounds-of-recursive-edit-state)
  "<escape>" 'exit-recursive-edit
  "e" 'exit-recursive-edit
  "C-]" 'abort-recursive-edit
  "q" 'abort-recursive-edit)

(cl-defmethod conn-bounds-of-subr ((_cmd (conn-thing recursive-edit)) _arg)
  (let* ((eldoc-message-function 'ignore)
         (pre (lambda ()
                (message
                 (substitute-command-keys
                  (concat
                   "Recursive Edit: Press \\[exit-recursive-edit] to exit, "
                   "\\[abort-recursive-edit] to abort"))))))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook pre)
          (conn-with-recursive-stack 'conn-bounds-of-recursive-edit-state
            (funcall pre)
            (recursive-edit))
          (conn-bounds-of-subr 'region nil))
      (remove-hook 'pre-command-hook pre))))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing emacs-state)) arg)
  (setq arg (prefix-numeric-value arg))
  (when (> arg 0) (cl-decf arg))
  (when (eq cmd 'conn-next-emacs-state)
    (setq arg (- arg)))
  (let* ((ring (conn-ring-list conn-emacs-state-ring))
         (mk (nth (mod arg (length ring)) ring))
         (pt (point)))
    (conn-bounds cmd arg :outer (cons (min pt mk) (max pt mk)))))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing isearch)) arg)
  (let ((start (point))
        (name (symbol-name cmd))
        (quit (lambda ()
                (when isearch-mode-end-hook-quit
                  (abort-recursive-edit)))))
    (add-hook 'isearch-mode-end-hook quit)
    (unwind-protect
        (isearch-mode (not (string-match-p "backward" name))
                      (string-match-p "regexp" name)
                      nil t)
      (remove-hook 'isearch-mode-end-hook quit))
    (conn-bounds cmd arg :outer (cons (min start (point)) (max start (point))))))


;;;; Bounds of Things in Region

(defun conn--things-in-region-subr (thing beg end)
  (ignore-errors
    (goto-char beg)
    (forward-thing thing 1)
    (cl-loop for bounds = (cons (save-excursion
                                  (forward-thing thing -1)
                                  (point))
                                (point))
             while (and bounds (< (car bounds) end))
             collect bounds into regions
             while (and (< (point) end)
                        (ignore-errors
                          (forward-thing thing 1)
                          t))
             finally return regions)))

(cl-defgeneric conn-get-things-in-region (thing beg end)
  (declare (conn-anonymous-thing-property :things-in-region)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) beg end)
    (if-let* ((op (conn-anonymous-thing-property cmd :thing-in-region)))
        (funcall op beg end)
      (conn-get-things-in-region (conn-anonymous-thing-parent cmd) beg end))))

(cl-defmethod conn-get-things-in-region ((thing (conn-thing t))
                                         beg end)
  (conn--things-in-region-subr (conn-get-thing thing) beg end))

(defun conn-bounds-of-things-in-region (thing bounds)
  "Bounds of the THINGs contained within the region BOUNDS.

BOUNDS is of the form returned by `region-bounds', which see."
  (declare (important-return-value t))
  (save-mark-and-excursion
    (mapcan (pcase-lambda (`(,beg . ,end))
              (conn-get-things-in-region thing beg end))
            bounds)))

(conn-register-thing 'conn-things-in-region)

(cl-defmethod conn-bounds-of-subr ((cmd (eql conn-things-in-region)) arg)
  (cl-loop for region in (conn-eval-with-state 'conn-read-thing-state
                             (conn-get-things-in-region
                              (car & (conn-thing-argument-dwim))
                              (region-beginning)
                              (region-end))
                           :prompt "Things in Region")
           collect (conn-bounds cmd nil :outer region) into contents
           finally return (conn-bounds
                           cmd arg
                           :outer (cons (region-beginning)
                                        (region-end))
                           :subregions (nreverse contents))))


;;;; Kapply

(defvar kmacro-step-edit-replace)

(defvar conn-kmacro-applying-p nil
  "Non-nil during kmacro application.")

(defvar conn-kmacro-apply-error nil
  "If non-nil contains the error encountered during macro application.")

(defvar conn-kmacro-apply-start-hook nil
  "Hook run before macro application begins.")

(defvar conn-kmacro-apply-end-hook nil
  "Hook run after macro application has completed.")

;;;;; Kmacro Utils

(defun conn--kmacro-display (macro &optional trunc)
  (pcase macro
    ((or 'nil '[] "") "nil")
    (_ (let* ((m (format-kbd-macro macro))
              (l (length m))
              (z (and trunc (> l trunc))))
         (format "%s%s"
                 (if z (substring m 0 (1- trunc)) m)
                 (if z "…" ""))))))

(defun conn--kmacro-ring-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Ring: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value)
     " - "
     (propertize
      (conn--kmacro-display last-kbd-macro 35)
      'face 'transient-value))))

(defun conn--kmacro-counter-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Counter: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value))))

(defun conn--in-kbd-macro-p ()
  (or defining-kbd-macro executing-kbd-macro))


;;;;; Kapply Query

(defvar conn--kapply-automatic-flag nil)

(defun conn-kapply-kbd-macro-query (flag)
  "Query user during kbd macro execution.

With prefix argument FLAG, enter recursive edit, reading
keyboard commands even within a kbd macro.  You can give
different commands each time the macro executes.

Without prefix argument, ask whether to continue running the
macro.

Your options are: \\<query-replace-map>

\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that.
\\[automatic]   Apply keyboard macro to rest."
  (interactive "P")
  (or executing-kbd-macro
      defining-kbd-macro
      (user-error "Not defining or executing kbd macro"))
  (let ((msg (substitute-command-keys
              "Proceed with macro?\\<query-replace-map>\
 (\\[act] act, \\[skip] skip, \\[exit] exit, \\[recenter] recenter, \\[edit] edit, \\[automatic] auto)")))
    (cond
     (flag
      (let (executing-kbd-macro defining-kbd-macro)
        (recursive-edit)))
     ((not executing-kbd-macro))
     ((not conn--kapply-automatic-flag)
      (catch 'end
        (while t
          (pcase (let ((executing-kbd-macro nil)
                       (defining-kbd-macro nil))
                   (message "%s" msg)
                   (lookup-key query-replace-map (vector (read-event))))
            ('act (throw 'end nil))
            ('skip
             (setq executing-kbd-macro "")
             (throw 'end nil))
            ('exit
             (setq executing-kbd-macro t)
             (throw 'end nil))
            ('recenter
             (recenter nil))
            ('edit
             (let (executing-kbd-macro defining-kbd-macro)
               (recursive-edit)))
            ('quit
             (setq quit-flag t)
             (throw 'end nil))
            ('automatic
             (setq conn--kapply-automatic-flag t)
             (throw 'end nil))
            ('help
             (with-output-to-temp-buffer "*Help*"
               (princ
                (substitute-command-keys
                 "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that.
\\[automatic]   Apply keyboard macro to rest."))
               (with-current-buffer standard-output
                 (help-mode))))
            (_ (ding)))))))))


;;;;; Iterators

(defun conn--kapply-macro (applier iterator pipeline)
  (funcall applier
           (seq-reduce (pcase-lambda (iterator (or `(,ctor . ,args) ctor))
                         (apply ctor iterator args))
                       (delq nil pipeline)
                       iterator)))

(defun conn--kapply-infinite-iterator ()
  (declare (important-return-value t)
           (side-effect-free t))
  (lambda (_state) (cons (point) (point))))

(defun conn--kapply-highlight-iterator (beg end &optional sort-function read-patterns)
  (declare (important-return-value t)
           (side-effect-free t))
  (let ((patterns
         (when (and (boundp 'hi-lock-interactive-patterns)
                    (boundp 'hi-lock-interactive-lighters))
           (if read-patterns
               (mapcar (lambda (regexp)
                         (alist-get regexp hi-lock-interactive-lighters
                                    nil nil #'equal))
                       (completing-read-multiple
                        "Regexps for kapply: "
                        (mapcar (lambda (pattern)
                                  (thread-first
                                    (rassq pattern hi-lock-interactive-lighters)
                                    car (or (car pattern)) (cons pattern)))
                                hi-lock-interactive-patterns)
                        nil t nil nil))
             hi-lock-interactive-patterns)))
        matches)
    (save-excursion
      (with-restriction beg end
        (pcase-dolist (`(,fn (,subexp . ,_)) patterns)
          (goto-char (point-min))
          (while-let ((match (funcall fn (point-max))))
            (push (cons (conn--create-marker (match-beginning subexp) nil t)
                        (conn--create-marker (match-end subexp)))
                  matches)))))
    (unless matches
      (user-error "No highlights for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp matches)
           (pcase-dolist (`(,beg . ,end) matches)
             (set-marker beg nil)
             (set-marker end nil))))
        ((or :record :next)
         (pop matches))))))

(defun conn--kapply-thing-iterator (thing bounds)
  (declare (important-return-value t))
  (deactivate-mark t)
  (conn--kapply-region-iterator
   (let ((regions (conn-bounds-of-things-in-region thing bounds)))
     (if (= (point) (region-end))
         (nreverse regions)
       regions))))

(defun conn--kapply-region-iterator (regions &optional sort-function)
  (declare (important-return-value t))
  (unless regions
    (user-error "No regions for kapply."))
  (pcase-dolist ((and reg `(,beg . ,end))
                 (setq regions (funcall (or sort-function #'identity)
                                        regions)))
    (if (markerp beg)
        (set-marker-insertion-type beg t)
      (setcar reg (conn--create-marker beg nil t)))
    (if (markerp end)
        (set-marker-insertion-type end nil)
      (setcdr reg (conn--create-marker end (marker-buffer (car reg))))))
  (lambda (state)
    (pcase state
      (:cleanup
       (when (consp regions)
         (pcase-dolist (`(,beg . ,end) regions)
           (set-marker beg nil)
           (set-marker end nil))))
      ((or :record :next)
       (pop regions)))))

(defun conn--kapply-point-iterator (points &optional sort-function)
  (declare (important-return-value t))
  (unless points
    (user-error "No points for kapply."))
  (let ((points
         (cl-loop for pt in (funcall (or sort-function #'identity) points)
                  collect (if (markerp pt) pt (conn--create-marker pt)))))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp points)
           (dolist (pt points)
             (set-marker pt nil))))
        ((or :record :next)
         (when-let* ((pt (pop points)))
           (cons pt pt)))))))

(defun conn--kapply-match-iterator ( string regions
                                     &optional
                                     sort-function
                                     regexp-flag
                                     delimited-flag)
  (declare (important-return-value t))
  (let (matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end) regions)
        (goto-char beg)
        (cl-loop
         while (replace-search string end regexp-flag
                               delimited-flag case-fold-search)
         for (mb me . _) = (match-data t)
         do (push (cons (conn--create-marker mb nil t)
                        (conn--create-marker me))
                  matches))))
    (unless matches
      (user-error "No matches for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp matches)
           (mapc (pcase-lambda (`(,beg . ,end))
                   (set-marker beg nil)
                   (set-marker end nil))
                 matches)))
        ((or :next :record)
         (pop matches))))))


;;;;; Pipeline Functions

(defvar conn--kapply-pipeline-depths
  '((kapply-skip-empty . 95)
    (kapply-relocate . 85)
    (kapply-invisible . 75)
    (kapply-nth . 70)
    (kapply-query-record . 50)
    (kapply-query-per-iteration . 49)
    (kapply-restrictions . 10)
    (kapply-excursions . 0)
    (kapply-undo . -10)
    (kapply-region . -20)
    (kapply-state . -30)
    (kapply-wconf . -70)
    (kapply-ibuffer . -80)
    (kapply-pulse . -90)))

(defun conn--kapply-query (iterator &optional each-iteration)
  (declare (important-return-value t)
           (side-effect-free t))
  (when each-iteration
    (add-function
     :after (var iterator)
     (lambda (state)
       (when (eq state :record)
         (push 'conn-kbd-macro-query unread-command-events)))
     `((depth . ,(alist-get 'kapply-query-record
                            conn--kapply-pipeline-depths))
       (name . kapply-query-per-iteration))))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       (:record
        (let ((hl (make-overlay (point) (point))))
          (overlay-put hl 'priority 2000)
          (overlay-put hl 'face 'query-replace)
          (overlay-put hl 'conn-overlay t)
          (unwind-protect
              (cl-loop
               for val = (funcall iterator state)
               until (or (null val)
                         (progn
                           (recenter nil)
                           (move-overlay hl
                                         (region-beginning)
                                         (region-end)
                                         (current-buffer))
                           (y-or-n-p (format "Record here?"))))
               finally return val)
            (delete-overlay hl))))
       ((or :cleanup :next)
        (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-query-record conn--kapply-pipeline-depths))
     (name . kapply-query-record))))

(defun conn--kapply-skip-empty (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       (:cleanup
        (funcall iterator state))
       ((or :next :record)
        (catch 'non-empty
          (while-let ((region (funcall iterator state)))
            (pcase region
              ((and `(,beg . ,end)
                    (guard (/= beg end))
                    region)
               (throw 'non-empty region))
              (`(,beg . ,end)
               (when (markerp beg) (set-marker beg nil))
               (when (markerp end) (set-marker end nil)))))))))
   `((depth . ,(alist-get 'kapply-skip-empty
                          conn--kapply-pipeline-depths))
     (name . kapply-skip-empty))))

(defun conn--kapply-every-nth (iterator N)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       (:cleanup
        (funcall iterator state))
       ((or :next :record)
        (dotimes (_ (1- N))
          (pcase (funcall iterator state)
            (`(,beg . ,end)
             (when (markerp beg) (set-marker beg nil))
             (when (markerp end) (set-marker end nil)))))
        (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-nth conn--kapply-pipeline-depths))
     (name . kapply-nth))))

(defun conn--kapply-skip-invisible-points (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (cl-loop for ret = (funcall iterator state)
                 until (or (null ret)
                           (not (invisible-p (car ret))))
                 when (markerp ret) do (set-marker ret nil)
                 finally return ret))
       (:cleanup (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-skip-invisible-regions (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (cl-loop for ret = (funcall iterator state)
                 until (or (null ret)
                           (conn--region-visible-p (car ret) (cdr ret)))
                 do (pcase-let ((`(,beg . ,end) ret))
                      (when (markerp beg) (set-marker beg nil))
                      (when (markerp end) (set-marker end nil)))
                 finally return ret))
       (:cleanup (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-open-invisible (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (restore)
     (lambda (iterator state)
       (pcase state
         ((or :next :record)
          (cl-loop for next = (funcall iterator state)
                   for res = (or (null next)
                                 (conn--open-invisible (car next) (cdr next)))
                   until res
                   do (pcase-let ((`(,beg . ,end) next))
                        (when (markerp beg) (set-marker beg nil))
                        (when (markerp end) (set-marker end nil)))
                   finally return (prog1 next
                                    (when (consp res)
                                      (setq restore (nconc res restore))))))
         (:cleanup
          (funcall iterator state)
          (mapc #'funcall restore)))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-relocate-to-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (let ((region (funcall iterator state)))
       (pcase state
         ((or :next :record)
          (pcase region
            ((and (pred identity)
                  `(,beg . ,end))
             (when-let* ((buffer (and (markerp beg) (marker-buffer beg)))
                         ((not (eq buffer (current-buffer)))))
               (pop-to-buffer-same-window buffer)
               (deactivate-mark t)
               (unless (eq buffer (window-buffer (selected-window)))
                 (error "Could not pop to buffer %s" buffer)))
             (goto-char beg)
             (conn--push-ephemeral-mark end)))))
       region))
   `((depth . ,(alist-get 'kapply-relocate conn--kapply-pipeline-depths))
     (name . kapply-relocate))))

(defun conn--kapply-per-buffer-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (undo-handles)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(_ . ,handle) undo-handles)
            (accept-change-group handle)
            (undo-amalgamate-change-group handle)))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (unless (or (alist-get (current-buffer) undo-handles)
                        (eq buffer-undo-list t))
              (activate-change-group
               (setf (alist-get (current-buffer) undo-handles)
                     (prepare-change-group)))))))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-per-buffer-atomic-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (undo-handles)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(_ . ,handle) undo-handles)
            (if conn-kmacro-apply-error
                (cancel-change-group handle)
              (accept-change-group handle)
              (undo-amalgamate-change-group handle))))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (unless (or (alist-get (current-buffer) undo-handles)
                        (eq buffer-undo-list t))
              (activate-change-group
               (setf (alist-get (current-buffer) undo-handles)
                     (prepare-change-group)))))))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-per-iteration-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (handle)
     (lambda (iterator state)
       (pcase state
         (:record
          (prog1
              (funcall iterator state)
            (unless (eq buffer-undo-list t)
              (setq handle (prepare-change-group))
              (activate-change-group handle))))
         (:cleanup
          (when handle
            (accept-change-group handle)
            (undo-amalgamate-change-group handle))
          (funcall iterator state))
         (:next
          (when handle
            (accept-change-group handle)
            (undo-amalgamate-change-group handle))
          (prog1
              (funcall iterator state)
            (if (eq buffer-undo-list t)
                (setq handle nil)
              (undo-boundary)
              (setq handle (prepare-change-group))
              (activate-change-group handle)))))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-ibuffer-overview (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let ((msg (substitute-command-keys
               "\\<query-replace-map>Buffer is modified, save before continuing?\
 \\[act], \\[skip], \\[quit], \\[edit], \\[automatic], \\[help]"))
         buffers automatic)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (when (and (not conn-kmacro-apply-error)
                     (length> buffers 1))
            (ibuffer t "*Kapply Ibuffer*"
                     `((predicate . (memq (current-buffer) ',buffers))))))
         ((or :record :next)
          (prog1 (funcall iterator state)
            (unless (or automatic
                        (memq (current-buffer) buffers))
              (push (current-buffer) buffers)
              (when (and (buffer-modified-p)
                         buffer-file-name)
                (redisplay)
                (catch 'end
                  (while t
                    (ding t)
                    (pcase (let ((executing-kbd-macro nil)
                                 (defining-kbd-macro nil))
                             (message "%s" msg)
                             (lookup-key query-replace-map (vector (read-event))))
                      ('act (throw 'end (save-buffer '(16))))
                      ('skip (throw 'end nil))
                      ('quit (setq quit-flag t))
                      ('edit
                       (let (executing-kbd-macro defining-kbd-macro)
                         (recursive-edit))
                       (throw 'end nil))
                      ('automatic
                       (setq automatic t)
                       (throw 'end nil))
                      ('help
                       (with-output-to-temp-buffer "*Help*"
                         (princ
                          (substitute-command-keys
                           "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Save file and continue iteration.
\\[skip]	Don't save file and continue iteration.
\\[quit]	Stop the macro entirely right now.
\\[edit]	Enter recursive edit; resume executing the keyboard macro afterwards.
\\[automatic]	Continue iteration and don't ask to save again."))
                         (with-current-buffer standard-output
                           (help-mode))))))))))))))
   `((depth . ,(alist-get 'kapply-ibuffer conn--kapply-pipeline-depths))
     (name . kapply-ibuffer))))

(defun conn--kapply-save-excursion (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (saved-excursions)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(,buffer ,pt . ,saved) saved-excursions)
            (with-current-buffer buffer
              (goto-char pt)
              (set-marker pt nil)
              (save-mark-and-excursion--restore saved))))
         (:record
          (setf (alist-get (current-buffer) saved-excursions)
                (let ((pt (point-marker)))
                  (set-marker-insertion-type pt t)
                  (cons pt (save-mark-and-excursion--save))))
          (funcall iterator state))
         (:next
          (if saved-excursions
              (prog1 (funcall iterator state)
                (unless (alist-get (current-buffer) saved-excursions)
                  (setf (alist-get (current-buffer) saved-excursions)
                        (cons (point-marker) (save-mark-and-excursion--save)))))
            (setf (alist-get (current-buffer) saved-excursions)
                  (cons (point-marker) (save-mark-and-excursion--save)))
            (funcall iterator state))))))
   `((depth . ,(alist-get 'kapply-excursions conn--kapply-pipeline-depths))
     (name . kapply-excursions))))

(defun conn--kapply-save-restriction (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (kapply-saved-restrictions)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(,buffer ,beg . ,end) kapply-saved-restrictions)
            (with-current-buffer buffer
              (widen)
              (narrow-to-region (or beg (point-min))
                                (or end (point-max))))))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (pcase (alist-get (current-buffer) kapply-saved-restrictions)
              ('nil
               (let ((beg (point-min-marker))
                     (end (point-max-marker)))
                 (without-restriction
                   (setf (alist-get (current-buffer) kapply-saved-restrictions)
                         (cons (when (/= beg (point-min)) beg)
                               (when (/= end (point-max)) end))))))
              (`(nil . nil) (widen))
              (`(,beg . ,end)
               (widen)
               (narrow-to-region (or beg (point-min))
                                 (or end (point-max))))))))))
   `((depth . ,(alist-get 'kapply-restrictions conn--kapply-pipeline-depths))
     (name . kapply-restrictions))))

(defun conn--kapply-change-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (unless (eq state :cleanup)
       (delete-region (region-beginning) (region-end))))
   `((depth . ,(alist-get 'kapply-region conn--kapply-pipeline-depths))
     (name . kapply-region))))

(defun conn--kapply-at-end (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (unless (eq state :cleanup)
       (conn-exchange-mark-command)))
   `((depth . ,(alist-get 'kapply-region conn--kapply-pipeline-depths))
     (name . kapply-region))))

(defun conn--kapply-with-state (iterator conn-state)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (let (buffer-stacks)
     (lambda (state)
       (pcase state
         (:cleanup
          (pcase-dolist (`(,buf . ,stack) buffer-stacks)
            (with-current-buffer buf
              (setq conn--state-stack stack
                    conn-lighter nil)
              (conn-enter-state (car stack)))))
         ((or :record :next)
          (when conn-local-mode
            (if-let* ((stack (alist-get (current-buffer) buffer-stacks)))
                (setf conn--state-stack stack)
              (setf (alist-get (current-buffer) buffer-stacks)
                    conn--state-stack))
            (conn-enter-recursive-stack conn-state))))))
   `((depth . ,(alist-get 'kapply-state conn--kapply-pipeline-depths))
     (name . kapply-state))))

(defun conn--kapply-pulse-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (when (eq state :record)
       (pulse-momentary-highlight-region (region-beginning)
                                         (region-end)
                                         'query-replace)))
   `((depth . ,(alist-get 'kapply-pulse conn--kapply-pipeline-depths))
     (name . kapply-pulse))))

(defun conn--kapply-save-windows (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (wconf)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (set-window-configuration wconf))
         ((or :record :next)
          (unless wconf (setq wconf (current-window-configuration)))
          (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-wconf conn--kapply-pipeline-depths))
     (name . kapply-wconf))))


;;;;; Applier Definitions

(defvar conn-kapply-suppress-message nil)

(defmacro conn--define-kapply (name arglist &rest body)
  "Define a macro application function.
The iterator must be the first argument in ARGLIST.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug ( name lambda-list
                    [&optional lambda-doc]
                    def-body))
           (doc-string 3)
           (indent 2))
  (let ((iterator (car arglist))
        (docstring (if (stringp (car body)) (pop body) "")))
    (cl-with-gensyms (iterations)
      `(defun ,name ,arglist
         ,docstring
         (require 'kmacro)
         (let* ((undo-outer-limit nil)
                (undo-limit most-positive-fixnum)
                (undo-strong-limit most-positive-fixnum)
                (conn-kmacro-applying-p t)
                (conn--kapply-automatic-flag nil)
                (,iterations 0)
                (success nil)
                (,iterator (lambda (&optional state)
                             (when-let* ((ret (funcall ,iterator (or state :next))))
                               (pcase ret
                                 (`(,beg . ,end)
                                  (when (markerp beg) (set-marker beg nil))
                                  (when (markerp end) (set-marker end nil))))
                               (cl-incf ,iterations)))))
           (run-hooks 'conn-kmacro-apply-start-hook)
           (deactivate-mark)
           (unwind-protect
               (cl-letf (((symbol-function 'kmacro-loop-setup-function)))
                 (advice-add 'kmacro-loop-setup-function :before-while ,iterator)
                 ,@body
                 (setq success t)
                 (unless conn-kapply-suppress-message
                   (message "Kapply completed successfully after %s iterations" ,iterations)))
             (let ((conn-kmacro-apply-error (not success)))
               (funcall ,iterator :cleanup)
               (run-hooks 'conn-kmacro-apply-end-hook))))))))

(conn--define-kapply conn--kmacro-apply (iterator &optional count macro)
  (pcase-exhaustive macro
    ((pred kmacro-p)
     (funcall macro (or count 0)))
    ((or (pred stringp) (pred vectorp))
     (kmacro-call-macro (or count 0) nil nil macro))
    ('nil
     (when (funcall iterator :record)
       (let ((last-macro last-kbd-macro))
         (kmacro-start-macro nil)
         (unwind-protect
             (progn
               (recursive-edit)
               (when (not defining-kbd-macro)
                 (user-error "Not defining keyboard macro")))
           (when defining-kbd-macro (kmacro-end-macro nil)))
         (when (eq last-macro last-kbd-macro)
           (user-error "New keyboard macro not defined"))
         (kmacro-call-macro (or count 0)))))))

(conn--define-kapply conn--kmacro-apply-append (iterator &optional count skip-exec)
  (when (funcall iterator :record)
    (kmacro-start-macro (if skip-exec '(16) '(4)))
    (unwind-protect
        (progn
          (recursive-edit)
          (when (not defining-kbd-macro)
            (user-error "Not defining keyboard macro")))
      (when defining-kbd-macro (kmacro-end-macro nil)))
    (kmacro-call-macro (or count 0))))

(conn--define-kapply conn--kmacro-apply-step-edit (iterator &optional count)
  (when (funcall iterator :record)
    (let* ((apply nil)
           (hook (lambda () (setq apply kmacro-step-edit-replace))))
      (add-hook 'kbd-macro-termination-hook hook)
      (unwind-protect
          (kmacro-step-edit-macro)
        (remove-hook 'kbd-macro-termination-hook hook))
      (unless apply
        (user-error "Keyboard macro edit aborted")))
    (kmacro-call-macro (or count 0))))


;;;; Labels

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e"
        "r" "t" "y" "u" "i" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defcustom conn-disptach-stable-label-characters
  `(("j" "u" "m" "k" "i" "," "l" "o" "h" "y" "n" "p" ";")
    ("f" "r" "v" "d" "e" "c" "s" "w" "x" "g" "t" "b" "a"))
  "Chars to use for label overlays when recording a keyboard macro."
  :group 'conn
  :type '(list integer))

(defface conn-dispatch-label-face
  '((t (:inherit highlight :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defvar conn-window-labeling-function 'conn-header-line-label
  "Function to label windows for `conn-prompt-for-window'.

The function should accept a single argument, the list of windows to be
labeled and it should return a list of structs for `conn-label-select',
which see.")

(defvar conn-target-window-predicate)

(defvar conn-dispatch-all-frames 'visible)

(cl-defstruct (conn-dispatch-label)
  "State for a dispatch label."
  (string nil :type string)
  (narrowed-string nil :type string)
  (overlay nil :type overlay)
  (target nil :type overlay)
  (setup-function nil :type function)
  (padding-function nil :type function))

(cl-defstruct (conn-window-label
               (:constructor nil)
               ( :constructor conn-window-label
                 ( string window
                   &aux
                   (state (list (window-point window)
                                (window-vscroll window)
                                (window-hscroll window))))))
  "State for a window label."
  (string nil :type string)
  (window nil :type window)
  (state nil :type list))

(defun conn-simple-labels (count &optional face)
  "Return a list of label strings of length COUNT.

If FACE is non-nil set label string face to FACE.  Otherwise label
strings have `conn-dispatch-label-face'."
  (declare (side-effect-free t)
           (important-return-value t))
  (cl-labels
      ((rec (count labels)
         (let* ((prefixes nil))
           (while (and labels
                       (> count (+ (length labels)
                                   (* (length prefixes)
                                      (length conn-simple-label-characters)))))
             (push (pop labels) prefixes))
           (if (and (null labels) (> count 0))
               (let (new-labels)
                 (dolist (a prefixes)
                   (dolist (b conn-simple-label-characters)
                     (push (concat a b) new-labels)))
                 (rec count new-labels))
             (catch 'done
               (let ((n (length labels)))
                 (setq labels (nreverse labels))
                 (dolist (prefix (nreverse prefixes))
                   (dolist (c conn-simple-label-characters)
                     (push (concat prefix c) labels)
                     (when (= (cl-incf n) count)
                       (throw 'done nil))))))
             (dolist (label labels)
               (put-text-property 0 (length label) 'face face label))
             (nreverse labels)))))
    (rec count (mapcar #'copy-sequence
                       (take count conn-simple-label-characters)))))

(defun conn--get-target-windows ()
  (declare (important-return-value t))
  (if conn-target-window-predicate
      (conn--get-windows nil nil conn-dispatch-all-frames
                         nil conn-target-window-predicate)
    (list (selected-window))))


;;;;; Label Reading

(defmacro conn-with-dispatch-event-handler (tag keymap handler &rest body)
  (declare (indent 3))
  (cl-once-only (keymap)
    (let ((body `(let ((conn--dispatch-read-event-handlers
                        (cons ,handler conn--dispatch-read-event-handlers)))
                   ,@body)))
      `(catch ,tag
         ,(if keymap
              `(let ((conn--dispatch-event-handler-maps
                      (cons ,keymap conn--dispatch-event-handler-maps)))
                 ,body)
            body)))))

(cl-defgeneric conn-label-delete (label)
  "Delete the label LABEL.

This function is called on each label after a label has been selected
and allow labels to clean up after themselves."
  (:method (_label) "Noop" nil))

(cl-defgeneric conn-label-narrow (label prefix)
  "Narrow LABEL by PREFIX.

If LABEL contains PREFIX then the label state should be updated to
reflect that prefix has been processed and LABEL should be returned.  If
the label does not contain the prefix then the label state be updated
to reflect that the label is no longer active and nil should be
returned."
  (declare (important-return-value t)))

(cl-defgeneric conn-label-redisplay (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload."
  (declare (important-return-value t)))

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (conn-dispatch-label-target label))

(cl-defmethod conn-label-reset ((label conn-dispatch-label))
  (setf (conn-dispatch-label-narrowed-string label)
        (conn-dispatch-label-string label)))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label))
  (setf (overlay-get (conn-dispatch-label-target label) 'conn-label) nil))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label) prefix-char)
  (if (thread-first
        (conn-dispatch-label-narrowed-string label)
        (aref 0) (eql prefix-char) not)
      (setf (conn-dispatch-label-narrowed-string label) nil)
    (cl-callf substring (conn-dispatch-label-narrowed-string label) 1)
    label))

(cl-defmethod conn-label-redisplay ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label
                          overlay
                          target
                          narrowed-string
                          setup-function)
               label))
    (with-current-buffer (overlay-buffer overlay)
      (if narrowed-string
          (progn
            (overlay-put overlay 'display nil)
            (overlay-put overlay 'before-string nil)
            (overlay-put overlay 'after-string nil)
            (funcall setup-function label)
            (overlay-put target 'face 'conn-target-overlay-face))
        (move-overlay overlay (overlay-start overlay) (overlay-start overlay))
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'after-string nil)
        (overlay-put overlay 'before-string nil)
        (overlay-put target 'after-string nil)
        (overlay-put target 'face nil)))))

(cl-defmethod conn-label-payload ((label conn-window-label))
  (conn-window-label-window label))

(cl-defmethod conn-label-reset ((label conn-window-label))
  (set-window-parameter (conn-window-label-window label)
                        'conn-label-string
                        (conn-window-label-string label)))

(cl-defmethod conn-label-delete ((label conn-window-label))
  (pcase-let* (((cl-struct conn-window-label window string state) label)
               (`(,pt ,vscroll ,hscroll) state))
    (with-current-buffer (window-buffer window)
      (when (eq 'conn-mode (car-safe (car-safe header-line-format)))
        (setq-local header-line-format (cadadr header-line-format))))
    (set-window-point window pt)
    (set-window-hscroll window hscroll)
    (set-window-vscroll window vscroll)
    (set-window-parameter window 'conn-label-string string)
    (set-window-parameter window 'conn-window-labeled-p nil)))

(cl-defmethod conn-label-narrow ((label conn-window-label) prefix-char)
  (pcase-let* (((cl-struct conn-window-label window) label)
               (string (window-parameter window 'conn-label-string)))
    (unless (or (length= string 0)
                (not (eql prefix-char (aref string 0))))
      (set-window-parameter window 'conn-label-string (substring string 1))
      label)))

(defvar conn-label-select-always-prompt nil)

(defun conn-label-select (candidates char-reader &optional prompt always-prompt)
  "Select a label from CANDIDATES.

Prompts the user for prefix characters one at a time and narrows the
labels after each one.

Each of CANDIDATES should be a DFA that defines its transition functions
as methods of the `conn-label-narrow' and `conn-label-reset' generic
functions.  `conn-label-narrow' is called when user input is received
for the label to process and `conn-label-reset' is called when the user
has failed to select a label and the narrowing process must restart from
the beginning.  `conn-label-delete' allows labels to clean up after
themselves once the selection process has concluded."
  (declare (important-return-value t))
  (let* ((prompt (propertize (or prompt "Chars")
                             'face 'minibuffer-prompt))
         (prompt-suffix "")
         (prompt-flag (or conn-label-select-always-prompt
                          always-prompt))
         (current candidates))
    (while-no-input
      (mapc #'conn-label-redisplay candidates))
    (cl-loop
     (pcase current
       ('nil
        (setq current candidates
              prompt-suffix ""
              prompt-flag (or conn-label-select-always-prompt
                              always-prompt))
        (conn-state-eval-message "No matches")
        (mapc #'conn-label-reset current)
        (while-no-input
          (mapc #'conn-label-redisplay candidates)))
       (`(,it . nil)
        (unless prompt-flag
          (cl-return (conn-label-payload it)))))
     (setq prompt-flag nil)
     (let ((next nil)
           (c (funcall char-reader prompt)))
       (setq prompt-suffix (concat prompt-suffix (string c))
             current (dolist (label current next)
                       (when-let* ((l (conn-label-narrow label c)))
                         (push l next)))
             conn--state-eval-error-message nil)
       (while-no-input
         (mapc #'conn-label-redisplay candidates))))))


;;;;; Window Header-line Labels

(defface conn-window-label-face
  '((t (:inherit help-key-binding :height 2.5)))
  "Face for conn window prompt overlay."
  :group 'conn-faces)

(defun conn--centered-header-label ()
  (when (window-parameter (selected-window) 'conn-window-labeled-p)
    (let* ((window-width (window-width nil t))
           (label (window-parameter nil 'conn-label-string))
           (label-width (string-pixel-width label))
           (padding-width (floor (- window-width label-width) 2))
           (padding (propertize " " 'display `(space :width (,padding-width)))))
      (concat padding label))))

(defvar conn--window-label-pool
  (conn-simple-labels 30 'conn-window-label-face))

(defun conn--ensure-window-labels ()
  (let* ((windows (conn--get-windows nil 'nomini t))
         (window-count (length windows)))
    (when (length< conn--window-label-pool window-count)
      (setq conn--window-label-pool
            (conn-simple-labels (* 2 window-count))))
    (cl-loop with available = (copy-sequence conn--window-label-pool)
             for win in windows
             for label = (window-parameter win 'conn-label-string)
             unless (and label
                         (when (member label available)
                           (setq available (delete label available))
                           t))
             collect win into unlabeled
             finally (dolist (win unlabeled)
                       (set-window-parameter win 'conn-label-string
                                             (pop available))))))

(defun conn-header-line-label (window string)
  "Label WINDOWS using `head-line-format'."
  (let ((header-line-label
         '(conn-mode (:eval (conn--centered-header-label)))))
    (set-window-parameter window 'conn-window-labeled-p t)
    (with-selected-window window
      (unless (equal header-line-label (car header-line-format))
        (setq-local header-line-format
                    `(,header-line-label (nil ,header-line-format))))
      (prog1
          (conn-window-label (propertize string 'face 'conn-window-label-face) window)
        (goto-char (window-start))))))

;; From ace-window
(defun conn--get-windows (&optional window minibuffer all-frames dedicated predicate)
  (declare (important-return-value t))
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           unless (or ;; ignore child frames
                   (and (fboundp 'frame-parent) (frame-parent (window-frame window)))
                   ;; When `ignore-window-parameters' is nil, ignore windows whose
                   ;; `no-other-window’ or `no-delete-other-windows' parameter is non-nil.
                   (unless ignore-window-parameters
                     (window-parameter window 'no-other-window))
                   (and (null dedicated) (window-dedicated-p win))
                   (and predicate (not (funcall predicate win))))
           collect win))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (declare (important-return-value t))
  (cond
   ((null windows) nil)
   (t
    (conn--ensure-window-labels)
    (let ((labels
           (cl-loop for win in windows
                    collect (funcall conn-window-labeling-function
                                     win (window-parameter win 'conn-label-string))))
          (conn-label-select-always-prompt always-prompt))
      (unwind-protect
          (conn-with-dispatch-event-handler 'mouse-click
              nil
              (lambda (cmd)
                (when (or (and (eq cmd 'act)
                               (mouse-event-p last-input-event))
                          (eq 'dispatch-mouse-repeat
                              (event-basic-type last-input-event)))
                  (let* ((posn (event-start last-input-event))
                         (win (posn-window posn)))
                    (when (and (not (posn-area posn))
                               (funcall conn-target-window-predicate win))
                      (throw 'mouse-click win)))))
            (conn-label-select labels #'conn-dispatch-read-event))
        (mapc #'conn-label-delete labels))))))


;;;; Dispatch State

(defface conn-dispatch-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a dispatch state."
  :group 'conn-faces)

(defvar conn-dispatch-ring)

(defvar conn--dispatch-must-prompt nil)
(defvar conn--dispatch-action-always-prompt nil)
(defvar conn--dispatch-always-retarget nil)

(defvar conn--dispatch-remap-cookies nil)

(defvar conn-dispatch-repeat-count nil)
(defvar conn-dispatch-other-end nil)
(defvar conn-dispatch-no-other-end nil)

(conn-define-state conn-dispatch-mover-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(conn-define-state conn-dispatch-state (conn-dispatch-mover-state)
  "State for reading a dispatch command."
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(defvar-keymap conn-dispatch-common-map
  "C-z" 'dispatch-other-end
  "C-\\" 'toggle-input-method
  "C-M-\\" 'set-input-method)

(defvar conn-dispatch-read-event-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map)
                          (cons #x100 (max-char))
		          'dispatch-character-event)
    (cl-loop for i from ?\s below 256
             do (define-key map (vector i) 'dispatch-character-event))
    map))

(defvar conn--dispatch-event-handler-maps nil)

(define-keymap
  :keymap conn-dispatch-read-event-map
  :parent conn-dispatch-common-map
  "C-/" 'undo
  "C-'" 'recursive-edit
  "<mouse-1>" 'act
  "<mouse-3>" 'undo
  "DEL" 'backward-delete-char
  "<backspace>" 'backward-delete-char
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-f" 'retarget
  "M-f" 'always-retarget
  "C-t" 'change-target-finder
  "<escape>" 'finish
  "C-o" 'conn-goto-window
  "C-s" 'isearch-forward
  "C-M-s" 'isearch-regexp-forward
  "C-M-r" 'isearch-regexp-backward
  "C-v" 'scroll-up
  "M-v" 'scroll-down
  "C-n" 'restrict-windows)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-mover-state)
  :parent conn-dispatch-common-map
  "z" 'dispatch-other-end
  "<escape>" 'keyboard-quit
  "C-h" 'help
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "TAB" 'repeat-dispatch
  "C-n" 'restrict-windows
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "f" 'conn-dispatch-over-or-goto
  "u" 'forward-symbol
  "i" 'forward-line
  "k" 'next-line
  "n" 'end-of-defun
  "," (conn-remap-key "<conn-thing-map>"))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-mover-state :override)
  "<remap> <conn-expand>" (conn-anonymous-thing
                           'expansion
                           :bounds-op (lambda (arg)
                                        (conn--push-ephemeral-mark)
                                        (conn-bounds-of-subr 'conn-expand arg)))
  "m" 'forward-sexp
  ";" 'conn-forward-inner-line
  "<conn-thing-map> e" 'move-end-of-line
  "<conn-thing-map> a" 'move-beginning-of-line
  "O" (conn-anonymous-thing
       'word
       :description "all-words"
       :target-finder (lambda ()
                        (conn-dispatch-all-things 'word)))
  "U" (conn-anonymous-thing
       'symbol
       :description "all-symbols"
       :target-finder (lambda ()
                        (conn-dispatch-all-things 'symbol)))
  "b" 'conn-dispatch-buttons)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-state :override)
  "M-r" (conn-remap-key "<conn-region-map>")
  "," (conn-remap-key "<conn-thing-map>"))

(keymap-set (conn-get-state-map 'conn-dispatch-state)
            "'" 'conn-dispatch-kapply)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-mover-state)
  ")" (conn-anonymous-thing
       'forward-sexp
       :description "list"
       :target-finder (lambda ()
                        (conn-dispatch-things-with-re-prefix
                         'sexp (rx (syntax open-parenthesis)))))
  "." (conn-anonymous-thing
       'forward-sexp
       :description "list"
       :target-finder (lambda ()
                        (conn-dispatch-things-with-re-prefix
                         'sexp (rx (syntax open-parenthesis)))))
  "]" (conn-anonymous-thing
       'sexp
       :description "inner-list"
       :bounds-op (lambda (arg)
                    (conn-bounds-of-subr 'down-list arg))
       :target-finder (lambda ()
                        (conn-dispatch-things-with-re-prefix
                         'sexp (rx (syntax open-parenthesis))))))

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(cl-defgeneric conn-make-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)))
    (if-let* ((action (conn-anonymous-thing-property cmd :default-action)))
        (funcall action)
      (conn-make-default-action (conn-anonymous-thing-parent cmd)))))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing t)))
  (conn-make-action 'conn-dispatch-goto))

(cl-defgeneric conn-get-target-finder (cmd)
  (declare (conn-anonymous-thing-property :target-finder)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)))
    (if-let* ((tf (conn-anonymous-thing-property cmd :target-finder)))
        (funcall tf)
      (conn-get-target-finder (conn-anonymous-thing-parent cmd)))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing t)))
  (conn-dispatch-read-n-chars :string-length 2))

(defun conn--dispatch-restrict-windows (win)
  (declare (side-effect-free t))
  (eq win (selected-window)))

(defun conn--dispatch-command-affixation (command-names)
  (with-selected-window (or (minibuffer-selected-window) (selected-window))
    (cl-loop
     for command-name in command-names
     collect (let* ((fun (and (stringp command-name) (intern-soft command-name)))
                    (binding (where-is-internal fun nil t))
                    (binding (if (and binding (not (stringp binding)))
                                 (format " {%s}" (key-description binding))
                               ""))
                    (thing (format " (%s)" (or (conn-command-thing fun)
                                               "action"))))
               (put-text-property 0 (length binding)
                                  'face 'help-key-binding binding)
               (put-text-property 0 (length thing)
                                  'face 'completions-annotations thing)
               (list command-name "" (concat thing binding))))))

;;;;;; Action

(oclosure-define (conn-dispatch-action-argument
                  (:parent conn-state-eval-argument)))

(defun conn-dispatch-action-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-action-argument)
      (self type)
    (if (conn--action-type-p type)
        (progn
          (conn-cancel-action value)
          (conn-set-argument
           self (unless (cl-typep value type)
                  (conn-make-action type))))
      self)))

(cl-defmethod conn-cancel-argument ((arg conn-dispatch-action-argument))
  (conn-cancel-action (conn-state-eval-argument-value arg)))

(cl-defmethod conn-eval-argument :before ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-state-eval-argument-value arg)))
    (conn-accept-action action)))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-dispatch-action-argument)
                                                  sym)
  (or (conn--action-type-p sym)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-state-eval-argument-value arg)))
    (propertize (conn-describe-action action)
                'face 'eldoc-highlight-function-argument)))

;;;;;; Other End

(oclosure-define (conn-dispatch-other-end-argument
                  (:parent conn-state-eval-argument)))

(defun conn-dispatch-other-end-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-other-end-argument
                    (value value))
      (self cmd)
    (if (eq cmd 'dispatch-other-end)
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-dispatch-other-end-argument)
                                                  sym)
  (or (eq sym 'dispatch-other-end)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-dispatch-other-end-argument))
  (concat "\\[dispatch-other-end] "
          (propertize "other-end"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Repeat

(defvar conn-dispatch-autorepeat-actions (list 'conn-dispatch-kapply))

(oclosure-define (conn-dispatch-repeat-argument
                  (:parent conn-state-eval-argument)))

(defun conn-dispatch-repeat-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-repeat-argument
                    (value value))
      (self cmd)
    (if (or (eq cmd 'repeat-dispatch)
            (memq cmd conn-dispatch-autorepeat-actions))
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-dispatch-repeat-argument)
                                                  sym)
  (or (eq sym 'dispatch-repeat)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-dispatch-repeat-argument))
  (concat "\\[repeat-dispatch] "
          (propertize "repeat"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Restrict Windows

(oclosure-define (conn-dispatch-restrict-windows-argument
                  (:parent conn-state-eval-argument)))

(defun conn-dispatch-restrict-windows-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-restrict-windows-argument
                    (value value))
      (self cmd)
    (if (eq cmd 'restrict-windows)
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-dispatch-restrict-windows-argument)
                                                  sym)
  (or (eq sym 'restrict-windows)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-dispatch-restrict-windows-argument))
  (concat "\\[restrict-windows] "
          (propertize "this-win"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Command Handler

(cl-defgeneric conn-handle-dispatch-command (cmd arglist)
  ( :method (_ arglist) arglist))

(cl-defmethod conn-handle-dispatch-command ((_ (eql conn-dispatch-cycle-ring-next))
                                            arglist)
  (condition-case _
      (progn
        (conn-dispatch-cycle-ring-next)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-state-eval-message "%s" (conn-describe-dispatch
                                         (conn-ring-head conn-dispatch-ring))))
        (conn-state-eval-handle)
        arglist)
    (user-error
     (setq conn--state-eval-error-message "Dispatch ring empty")
     arglist)))

(cl-defmethod conn-handle-dispatch-command ((_ (eql conn-dispatch-cycle-ring-previous))
                                            arglist)
  (condition-case _
      (progn
        (conn-dispatch-cycle-ring-previous)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-state-eval-message "%s" (conn-describe-dispatch
                                         (conn-ring-head conn-dispatch-ring))))
        (conn-state-eval-handle)
        arglist)
    (user-error
     (setq conn--state-eval-error-message "Dispatch ring empty")
     arglist)))

(cl-defmethod conn-handle-dispatch-command ((_ (eql conn-dispatch-ring-describe-head))
                                            arglist)
  (conn-dispatch-ring-remove-stale)
  (if-let* ((head (conn-ring-head conn-dispatch-ring)))
      (if (bound-and-true-p conn-posframe-mode)
          (conn-posframe--dispatch-ring-display-subr)
        (conn-state-eval-message "%s" (conn-describe-dispatch head)))
    (conn-state-eval-message "Dispatch ring empty"))
  (conn-state-eval-handle)
  arglist)


;;;;; Dispatch Window Filtering

(defcustom conn-dispatch-thing-ignored-modes
  (list 'image-mode 'doc-view-mode 'pdf-view-mode)
  "List of modes to ignore when searching for dispatch candidates."
  :group 'conn
  :type '(list symbol))

(defun conn-dispatch-ignored-mode (win)
  "Return non-nil if the major mode of WIN's buffer is ignored by dispatch.

Ignored modes are those satisfying `provided-mode-derived-p' when called
with `conn-dispatch-thing-ignored-modes'."
  (not (apply #'provided-mode-derived-p
              (buffer-local-value 'major-mode (window-buffer win))
              conn-dispatch-thing-ignored-modes)))


;;;;; Dispatch Target Overlays

(defcustom conn-read-string-timeout 0.5
  "Timeout for string reading functions."
  :group 'conn
  :type 'number)

(defface conn-target-overlay-face
  '((t (:inherit isearch)))
  "Face for matches when reading strings."
  :group 'conn-faces)

(defvar conn-targets nil)

(defvar conn-target-count 0)

(defvar conn-target-sort-function 'conn-target-nearest-op)

(defvar conn-target-window-predicate 'conn-dispatch-ignored-mode
  "Predicate which windows must satisfy in order to be considered during
dispatch.

Each function should take a window and return nil if the window should
be ignored by during dispatch.")

(defvar conn-target-predicate
  (lambda (pt length window)
    (not (conn--overlays-in-of-type pt (+ pt length)
                                    'conn-target-overlay
                                    window))))

(defvar conn--dispatch-init-state nil)

(put 'conn-target-overlay 'conn-overlay t)
(put 'conn-target-overlay 'priority 2002)

(defun conn--overlays-in-of-type (beg end category &optional window)
  (declare (important-return-value t))
  (cl-loop for ov in (overlays-in beg end)
           when (and (eq (overlay-get ov 'category) category)
                     (or (null window)
                         (eq (overlay-get ov 'window) window)))
           collect ov))

(cl-defun conn-make-target-overlay ( pt length
                                     &key
                                     padding-function
                                     window
                                     thing)
  "Make a target overlay at PT of LENGTH.

Optionally the overlay may have an associated THING."
  (unless window (setq window (selected-window)))
  (when (funcall conn-target-predicate pt length window)
    (conn-protected-let*
        ((line-bounds
          (save-excursion
            (goto-char pt)
            (cons (pos-bol) (pos-eol))))
         (composition-end
          (when (get-text-property pt 'composition)
            (next-single-property-change
             pt 'composition nil (cdr line-bounds))))
         (composition-start
          (when composition-end
            (previous-single-property-change
             composition-end 'composition nil (car line-bounds))))
         (ov (if composition-start
                 (if (> length 0)
                     (make-overlay composition-start composition-end nil t)
                   (make-overlay composition-start composition-start nil t))
               (make-overlay pt (min (+ pt length) (cdr line-bounds)) nil t))
             (delete-overlay ov)))
      (overlay-put ov 'conn-overlay t)
      (overlay-put ov 'category 'conn-target-overlay)
      (overlay-put ov 'face 'conn-target-overlay-face)
      (overlay-put ov 'window window)
      (overlay-put ov 'padding-function padding-function)
      (when thing (overlay-put ov 'thing thing))
      (push ov (alist-get window conn-targets))
      ov)))

(defun conn-delete-target-overlay (ov)
  (let ((win (overlay-get ov 'window)))
    (cl-assert (memq ov (alist-get win conn-targets))
               nil "Not a target overlay")
    (conn-label-delete (overlay-get ov 'conn-label))
    (cl-callf2 delq ov (alist-get win conn-targets)))
  (delete-overlay ov))

(defun conn-make-string-target-overlays (string &optional predicate)
  (when (length> string 0)
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-matches string predicate))
          (conn-make-target-overlay beg (- end beg)))))))

(defun conn-make-re-target-overlays (regexp &optional predicate)
  (when (length> regexp 0)
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches regexp predicate))
          (conn-make-target-overlay beg (- end beg)))))))

(defun conn--read-string-with-timeout (&optional predicate)
  (unwind-protect
      (conn-with-input-method
        (let* ((prompt (propertize "String" 'face 'minibuffer-prompt))
               (string (char-to-string (read-char prompt t))))
          (while-no-input
            (conn-make-string-target-overlays string predicate))
          (while-let ((next-char (read-char (format (concat prompt "%s ") string)
                                            t conn-read-string-timeout)))
            (setq string (concat string (char-to-string next-char)))
            (conn-delete-targets)
            (while-no-input
              (conn-make-string-target-overlays string predicate)))
          (message nil)
          string))
    (conn-delete-targets)))


;;;;; Dispatch Labels

(defvar conn-dispatch-target-finder nil)

(defvar conn--dispatch-read-event-handlers nil)

(defvar conn--dispatch-read-event-message-prefixes nil)

(defvar conn-dispatch-label-function 'conn-dispatch-smart-labels)

(defvar conn-default-label-padding-function 'conn--centered-padding
  "Default function for padding dispatch labels.

Target overlays may override this default by setting the
\\='padding-function overlay property.")

(defvar conn-pixelwise-label-target-limit 500
  "Maximum number of targets in a window for pixelwise labeling.")

(defvar conn-pixelwise-labels-window-predicate
  'conn--pixelwise-labels-window-p)

(defvar conn-dispatch-pixelwise-labels-line-limit 750
  "Maximum position in a line for pixelwise labeling.")

(defvar conn-pixelwise-labels-target-predicate
  'conn--pixelwise-labels-target-p)

(defun conn--pixelwise-labels-window-p (win)
  (declare (important-return-value t))
  (and (eq (selected-frame) (window-frame win))
       (length< (alist-get win conn-targets)
                conn-pixelwise-label-target-limit)))

(defun conn--pixelwise-labels-target-p (target)
  (declare (important-return-value t))
  (cl-loop with ov-beg = (overlay-start target)
           for (beg . end) in (conn--dispatch-window-lines
                               (overlay-get target 'window))
           when (and (<= beg ov-beg)
                     (< ov-beg end))
           return (< (- ov-beg beg)
                     conn-dispatch-pixelwise-labels-line-limit)))

(put 'conn-label-overlay 'priority 3000)
(put 'conn-label-overlay 'conn-overlay t)

(defun conn--right-justify-padding (overlay width)
  (overlay-put overlay 'after-string
               (propertize " " 'display `(space :width (,width)))))

(defun conn--left-justify-padding (overlay width)
  (overlay-put overlay 'before-string
               (propertize " " 'display `(space :width (,width)))))

(defun conn--centered-padding (overlay width)
  (let* ((left (min 15 (floor width 2)))
         (right (max (- width 15) (ceiling width 2))))
    (overlay-put overlay 'before-string
                 (propertize
                  " "
                  'display `(space :width (,left))
                  'face 'conn-dispatch-label-face))
    (overlay-put overlay 'after-string
                 (propertize
                  " "
                  'display `(space :width (,right))
                  'face 'conn-dispatch-label-face))))

(defun conn--dispatch-eol (pt window)
  (declare (important-return-value t))
  (cl-loop for (beg . end) in (conn--dispatch-window-lines window)
           when (and (<= beg pt) (< pt end))
           return (1- end)))

(defun conn--dispatch-setup-label-pixelwise (label)
  (with-slots ((string narrowed-string)
               overlay target padding-function)
      label
    (let ((display-width
           (conn--string-pixel-width string
                                     (thread-first
                                       (overlay-get overlay 'window)
                                       (window-buffer))))
          (padding-width 0)
          ;; display-line-numbers, line-prefix and wrap-prefix break
          ;; width calculations, temporarily disable them.
          (old-state (buffer-local-set-state
                      display-line-numbers nil
                      line-prefix nil
                      wrap-prefix nil)))
      (unwind-protect
          (progn
            (unless (= (overlay-start overlay) (point-max))
              (let* ((win (overlay-get target 'window))
                     (beg (overlay-end target))
                     (end nil)
                     (line-end
                      (or (conn--dispatch-eol beg win)
                          (save-excursion
                            (goto-char beg)
                            (pos-eol))))
                     (pt beg))
                ;; Find the end of the label overlay.  Barring
                ;; exceptional conditions, which see the test clauses of
                ;; the following cond form, we want the label overlay to
                ;; be wider than the label string.
                (while (not end)
                  (cond
                   ;; If we are at the end of a line than end the label overlay.
                   ((= line-end pt)
                    (if (and (not (invisible-p pt))
                             (/= pt beg))
                        (setq end pt)
                      ;; If we are at the end of the line and the label
                      ;; overlay has width 0 then we need to expand the
                      ;; label overlay to include the EOL and append it
                      ;; as an after overlay.  Ensure we preserve the
                      ;; invisibility property when we do so.
                      (setq end (1+ pt))
                      (let ((str (buffer-substring pt end)))
                        (add-text-properties
                         0 (length str)
                         `(invisible ,(get-char-property pt 'invisible win))
                         str)
                        (overlay-put overlay 'after-string str))))
                   ;; If the label overlay is wider than the label
                   ;; string we are done.
                   ((pcase-let ((`(,width . ,_)
                                 (save-excursion
                                   (with-restriction beg pt
                                     (window-text-pixel-size
                                      (overlay-get overlay 'window)
                                      beg pt)))))
                      (when (or (= pt (point-max))
                                (>= width display-width))
                        (setq padding-width (max (- width display-width) 0)
                              end pt))))
                   ;; If we are abutting another target overlay then end
                   ;; the label overlay here so that we don't hide it.
                   ((dolist (ov (overlays-in pt (1+ pt)) end)
                      (when (and (eq 'conn-target-overlay
                                     (overlay-get ov 'category))
                                 (or (/= (overlay-start target)
                                         (overlay-start ov))
                                     (/= (overlay-end target)
                                         (overlay-end ov))))
                        (setq end pt))))
                   ((get-text-property pt 'composition)
                    (setq pt (next-single-property-change
                              pt 'composition nil line-end)))
                   (t (cl-incf pt))))
                (move-overlay overlay (overlay-start overlay) end)))
            (cond
             ((= (overlay-start overlay) (overlay-end overlay))
              (overlay-put overlay 'before-string string))
             ((overlay-get overlay 'after-string)
              (overlay-put overlay 'display string))
             (t
              (overlay-put overlay 'display string)
              (if padding-function
                  (funcall padding-function overlay padding-width)
                (funcall conn-default-label-padding-function overlay padding-width)))))
        (buffer-local-restore-state old-state)))))

(defun conn--dispatch-setup-label-charwise (label)
  (with-slots ((string narrowed-string)
               overlay target)
      label
    (unless (= (overlay-start overlay) (point-max))
      (let* ((win (overlay-get overlay 'window))
             (beg (overlay-start overlay))
             (end nil)
             (line-end
              (or (conn--dispatch-eol beg win)
                  (save-excursion
                    (goto-char beg)
                    (pos-eol))))
             (pt beg))
        (while (not end)
          (cond
           ((= line-end pt)
            (if (and (not (invisible-p pt))
                     (/= pt beg))
                (setq end pt)
              (setq end (1+ pt))
              (let ((str (buffer-substring pt end)))
                (add-text-properties
                 0 (length str)
                 `(invisible ,(get-char-property pt 'invisible win))
                 str)
                (overlay-put overlay 'after-string str))))
           ((or (= pt (point-max))
                (= (- pt beg) (length string)))
            (setq end pt))
           ((dolist (ov (overlays-in pt (1+ pt)) end)
              (when (and (eq 'conn-target-overlay
                             (overlay-get ov 'category))
                         (or (/= (overlay-start target)
                                 (overlay-start ov))
                             (/= (overlay-end target)
                                 (overlay-end ov))))
                (setq end pt))))
           (t (cl-incf pt))))
        (move-overlay overlay (overlay-start overlay) end)))
    (if (= (overlay-start overlay) (overlay-end overlay))
        (overlay-put overlay 'before-string string)
      (overlay-put overlay 'display string))))

(defconst conn--dispatch-window-lines-cache (make-hash-table :test 'eq))

(defun conn--dispatch-window-lines (window)
  (declare (important-return-value t))
  (with-memoization (gethash window conn--dispatch-window-lines-cache)
    (let (lines prev)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char (window-start window))
          (setq prev (pos-bol))
          (while (and (<= prev (window-end window))
                      (not (eobp)))
            (forward-line)
            (push (cons prev (point)) lines)
            (setq prev (point)))))
      lines)))

(defconst conn--pixelwise-window-cache (make-hash-table :test 'eq))

(defun conn-dispatch-pixelwise-label-p (ov)
  (declare (important-return-value t))
  (and (with-memoization
           (gethash (overlay-get ov 'window) conn--pixelwise-window-cache)
         (funcall conn-pixelwise-labels-window-predicate
                  (overlay-get ov 'window)))
       (funcall conn-pixelwise-labels-target-predicate ov)))

(defun conn-disptach-label-target (target string)
  (declare (important-return-value t))
  (let ((window (overlay-get target 'window)))
    (when (<= (window-start window)
              (overlay-start target)
              (overlay-end target)
              (window-end window))
      (conn-protected-let*
          ((beg (overlay-end target))
           (ov (make-overlay beg beg (overlay-buffer target))
               (delete-overlay ov))
           (str (propertize string 'face 'conn-dispatch-label-face)))
        (setf (overlay-get ov 'category) 'conn-label-overlay
              (overlay-get ov 'window) window
              (overlay-get target 'conn-label)
              (make-conn-dispatch-label
               :setup-function (if (conn-dispatch-pixelwise-label-p ov)
                                   'conn--dispatch-setup-label-pixelwise
                                 'conn--dispatch-setup-label-charwise)
               :padding-function (overlay-get target 'padding-function)
               :string str
               :narrowed-string str
               :overlay ov
               :target target))))))

(defun conn-dispatch-get-targets (&optional sort-function)
  (declare (important-return-value t))
  (let ((result nil))
    (setq conn-target-count 0)
    (pcase-dolist (`(,window . ,targets) conn-targets)
      (let ((filtered
             (cl-loop for tar in targets
                      when (<= (window-start window)
                               (overlay-start tar)
                               (overlay-end tar)
                               (window-end window))
                      do (cl-incf conn-target-count)
                      and collect tar)))
        (push (cons window
                    (if sort-function
                        (compat-call sort filtered
                                     :lessp sort-function
                                     :in-place t)
                      filtered))
              result)))
    result))

(defun conn-dispatch-simple-labels ()
  (declare (important-return-value t))
  (let* ((all-targets (conn-dispatch-get-targets conn-target-sort-function))
         (label-strings (conn-simple-labels conn-target-count))
         (labels nil))
    (pcase-dolist (`(,_window . ,targets) all-targets)
      (dolist (tar targets)
        (push (conn-disptach-label-target tar (pop label-strings))
              labels)))
    labels))

(defun conn--stable-label-subr (window targets characters)
  (let ((group 0)
        (chars characters)
        (labels nil)
        (window-label
         (unless (eq window (selected-window))
           (upcase (window-parameter window 'conn-label-string)))))
    (dolist (tar targets labels)
      (when (null chars)
        (cl-incf group)
        (setq chars characters))
      (push (conn-disptach-label-target
             tar (concat window-label
                         (when (> group 0)
                           (number-to-string group))
                         (pop chars)))
            labels))))

(defun conn-dispatch-stable-labels ()
  (declare (important-return-value t))
  (conn--ensure-window-labels)
  (pcase-let* ((`(,bchars ,achars) conn-disptach-stable-label-characters)
               (labels nil))
    (pcase-dolist (`(,win . ,targets)
                   (conn-dispatch-get-targets 'conn-target-nearest-op))
      (let (before after)
        (dolist (tar targets)
          (if (> (window-point win) (overlay-start tar))
              (push tar before)
            (push tar after)))
        (push (conn--stable-label-subr win (nreverse after) achars) labels)
        (push (conn--stable-label-subr win (nreverse before) bchars) labels)))
    (apply #'nconc labels)))

(defun conn-dispatch-smart-labels ()
  (declare (important-return-value t))
  (if (or executing-kbd-macro defining-kbd-macro)
      (conn-dispatch-stable-labels)
    (conn-dispatch-simple-labels)))

(defun conn--target-label-payload (overlay)
  (declare (important-return-value t))
  (list (overlay-start overlay)
        (overlay-get overlay 'window)
        (overlay-get overlay 'thing)))

(defun conn--dispatch-read-event-prefix ()
  (declare (important-return-value t))
  (when-let* ((prefix
               (cl-loop for pfx in conn--dispatch-read-event-message-prefixes
                        for str = (pcase pfx
                                    ((pred functionp) (funcall pfx))
                                    ((pred stringp) pfx))
                        if str collect str)))
    (concat " (" (string-join prefix "; ") ")")))

(defun conn-dispatch-read-event (&optional prompt
                                           inherit-input-method
                                           seconds
                                           prompt-suffix)
  (declare (important-return-value t))
  (let ((inhibit-message conn-state-eval-inhibit-message)
        (message-log-max nil)
        (prompt-suffix (concat prompt-suffix " "
                               (when conn--state-eval-error-message
                                 (propertize conn--state-eval-error-message
                                             'face 'error)))))
    (catch 'return
      (if seconds
          (while-let ((ev (conn-with-input-method
                            (read-event (concat prompt ": " prompt-suffix)
                                        inherit-input-method seconds))))
            (when (characterp ev)
              (throw 'return ev)))
        (while t
          (pcase (conn-with-overriding-map
                     (make-composed-keymap conn--dispatch-event-handler-maps
                                           conn-dispatch-read-event-map)
                   (thread-first
                     (concat prompt
                             (conn--dispatch-read-event-prefix)
                             ":" prompt-suffix)
                     (read-key-sequence-vector)
                     (key-binding t)))
            ('dispatch-character-event
             (setq conn--dispatch-must-prompt nil)
             (push `(no-record . ,last-input-event) unread-command-events)
             (throw 'return
                    (conn-with-input-method
                      (read-event
                       (concat prompt
                               (conn--dispatch-read-event-prefix)
                               ":" prompt-suffix)
                       inherit-input-method))))
            ('keyboard-quit
             (keyboard-quit))
            (cmd
             (let ((unhandled nil))
               (unwind-protect
                   (catch 'dispatch-handle
                     (cl-loop for handler in conn--dispatch-read-event-handlers
                              do (funcall handler cmd))
                     (setq unhandled t))
                 (unless unhandled
                   (setf conn-state-eval-last-command cmd)))))))))))

(cl-defgeneric conn-dispatch-select-target ()
  (declare (important-return-value t)))

(cl-defmethod conn-dispatch-select-target :around ()
  (let ((conn--dispatch-remap-cookies nil))
    (conn-with-dispatch-event-handler 'mouse-click
        nil
        (lambda (cmd)
          (when (or (and (eq cmd 'act)
                         (mouse-event-p last-input-event))
                    (eq 'dispatch-mouse-repeat
                        (event-basic-type last-input-event)))
            (let* ((posn (event-start last-input-event))
                   (win (posn-window posn))
                   (pt (posn-point posn)))
              (when (and (not (posn-area posn))
                         (funcall conn-target-window-predicate win))
                (throw 'mouse-click (list pt win nil))))))
      (conn-dispatch-select-mode 1)
      (unwind-protect
          (let ((inhibit-message t))
            (cl-call-next-method))
        (message "cookies: %s" conn--dispatch-remap-cookies)
        (conn-dispatch-select-mode -1)))))

(cl-defmethod conn-dispatch-select-target ()
  (unwind-protect
      (progn
        (when conn--dispatch-always-retarget
          (conn-dispatch-retarget conn-dispatch-target-finder))
        (conn-dispatch-update-targets conn-dispatch-target-finder)
        (thread-first
          (funcall conn-dispatch-label-function)
          (conn-label-select #'conn-dispatch-read-event
                             (concat "Label ["
                                     (number-to-string conn-target-count)
                                     "]")
                             (or conn--dispatch-must-prompt
                                 conn--dispatch-action-always-prompt
                                 (> conn-dispatch-repeat-count 0)))
          (conn--target-label-payload)))
    (conn-delete-targets)))


;;;;; Dispatch Target Finders

(defface conn-dispatch-context-separator-face
  '((t (:inherit (shadow tooltip) :extend t)))
  "Face for context region separator.")

(defun conn-target-nearest-op (a b)
  (declare (side-effect-free t))
  (< (abs (- (overlay-end a) (point)))
     (abs (- (overlay-end b) (point)))))

(defun conn-delete-targets ()
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (conn-label-delete (overlay-get target 'conn-label))
      (delete-overlay target)))
  (clrhash conn--pixelwise-window-cache)
  (clrhash conn--dispatch-window-lines-cache)
  (setq conn-targets nil
        conn-target-count 0))

(cl-defgeneric conn-dispatch-update-targets (target-finder)
  (:method (target-finder) (funcall target-finder)))

(cl-defgeneric conn-dispatch-cleanup-target-finder (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-retarget (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-retargetable-p (target-finder)
  (declare (important-return-value t))
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-has-target-p (target-finder)
  (declare (important-return-value t))
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-targets-other-end (target-finder)
  (declare (important-return-value t))
  (:method (_) nil))

(defclass conn-dispatch-target-window-predicate ()
  ((window-predicate :initform (lambda (&rest _) t)
                     :allocation :class))
  "Abstract type for target finders with a window predicate."
  :abstract t)

(cl-defmethod conn-dispatch-update-targets :before ((state conn-dispatch-target-window-predicate))
  (let ((pred (oref state window-predicate)))
    (unless (advice-function-member-p pred conn-target-window-predicate)
      (add-function :before-while conn-target-window-predicate pred)))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(cl-defmethod conn-dispatch-cleanup-target-finder ((state conn-dispatch-target-window-predicate))
  (remove-function conn-target-window-predicate
                   (oref state window-predicate))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(defclass conn-dispatch-string-targets ()
  ((string :initform nil))
  "Abstract type for target finders targeting a string."
  :abstract t)

(cl-defmethod conn-dispatch-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(cl-defmethod conn-dispatch-retargetable-p ((_ conn-dispatch-string-targets))
  t)

(cl-defmethod conn-dispatch-has-target-p ((state conn-dispatch-string-targets))
  (and (oref state string) t))

(defclass conn-dispatch-read-n-chars (conn-dispatch-string-targets)
  ((string-length :initform 1 :initarg :string-length)
   (predicate :initform nil :initarg :predicate)))

(cl-defmethod conn-dispatch-update-targets ((state conn-dispatch-read-n-chars))
  (with-slots (string string-length predicate) state
    (if string
        (conn-make-string-target-overlays string predicate)
      (conn-with-input-method
        (let* ((prompt (if (> string-length 1)
                           (propertize "Chars" 'face 'minibuffer-prompt)
                         (propertize "Char" 'face 'minibuffer-prompt))))
          (while (length< string string-length)
            (when (length> string 0)
              (while-no-input
                (conn-make-string-target-overlays string predicate)))
            (catch 'dispatch-redisplay
              (conn-with-dispatch-event-handler 'backspace
                  (define-keymap "<remap> <backward-delete-char>" 'backspace)
                  (lambda (cmd)
                    (when (eq cmd 'backspace)
                      (when (length> string 0)
                        (cl-callf substring string 0 -1))
                      (throw 'backspace nil)))
                (cl-callf thread-last
                    string
                  (conn-dispatch-read-event prompt t)
                  (char-to-string)
                  (concat string))))
            (conn-delete-targets))
          (conn-make-string-target-overlays string predicate))))))

(defclass conn-dispatch-read-with-timeout (conn-dispatch-string-targets)
  ((timeout :initform 0.5 :initarg :timeout)
   (predicate :initform nil :initarg :predicate)))

(cl-defmethod conn-dispatch-update-targets ((state conn-dispatch-read-with-timeout))
  (with-slots (string timeout predicate) state
    (if string
        (conn-make-string-target-overlays string predicate)
      (let* ((prompt (propertize "String" 'face 'minibuffer-prompt)))
        (setq string (char-to-string (conn-dispatch-read-event prompt t)))
        (while-no-input
          (conn-make-string-target-overlays string predicate))
        (while-let ((next-char (conn-dispatch-read-event
                                prompt t timeout string)))
          (conn-delete-targets)
          (setq string (concat string (char-to-string next-char)))
          (while-no-input
            (conn-make-string-target-overlays string predicate)))))))

(defun conn-dispatch-read-string-with-timeout (&optional predicate)
  (declare (important-return-value t))
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :predicate predicate))

(defclass conn-dispatch-focus-targets ()
  ((hidden :initform nil)
   (context-lines :initform 0 :initarg :context-lines)
   (separator-p :initarg :separator))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(cl-defmethod conn-dispatch-cleanup-target-finder ((state conn-dispatch-focus-targets))
  (mapc #'delete-overlay (oref state hidden)))

(cl-defmethod conn-dispatch-update-targets :after ((state conn-dispatch-focus-targets))
  (unless (oref state hidden)
    (conn-protected-let* ((hidden (list (make-overlay (point-min) (point-min)))
                                  (mapc #'delete-overlay hidden))
                          (context-lines (oref state context-lines))
                          (separator-p (if (slot-boundp state 'separator-p)
                                           (oref state separator-p)
                                         (> context-lines 0))))
      (pcase-dolist (`(,win . ,targets) conn-targets)
        (with-selected-window win
          (let ((regions (list (cons (pos-bol) (pos-bol 2)))))
            (save-excursion
              (dolist (tar targets)
                (push (or (overlay-get tar 'context)
                          (progn
                            (goto-char (overlay-start tar))
                            (cons (pos-bol (- 1 context-lines))
                                  (pos-bol (+ 2 context-lines)))))
                      regions)))
            (cl-callf conn--merge-overlapping-regions regions t)
            (conn--compat-callf sort regions :key #'car :in-place t)
            (cl-loop for beg = (point-min) then next-beg
                     for (end . next-beg) in regions
                     while end
                     do (progn
                          (thread-first
                            (push (make-overlay beg end) hidden)
                            car (overlay-put 'invisible t))
                          (when (and separator-p (/= end (point-max)))
                            (overlay-put
                             (car hidden)
                             'before-string
                             (propertize
                              (format " %s\n"
                                      (when (memq display-line-numbers
                                                  '(nil relative visual))
                                        (+ (line-number-at-pos end)
                                           context-lines)))
                              'face 'conn-dispatch-context-separator-face))))
                     finally (thread-first
                               (push (make-overlay beg (point-max)) hidden)
                               car (overlay-put 'invisible t))))
          (recenter)))
      (setf (oref state hidden) hidden)
      (sit-for 0))))

(defclass conn-dispatch-previous-emacs-state (conn-dispatch-focus-targets)
  ((context-lines :initform 1 :initarg :context-lines)))

(cl-defmethod conn-dispatch-targets-other-end ((_ conn-dispatch-previous-emacs-state))
  :no-other-end)

(cl-defmethod conn-dispatch-update-targets ((_state conn-dispatch-previous-emacs-state))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (let ((points (conn-ring-list conn-emacs-state-ring)))
          (dolist (pt points)
            (conn-make-target-overlay pt 0)))))))

(defun conn-dispatch-chars-in-thing (thing)
  (declare (important-return-value t))
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :predicate (lambda (beg _end)
                (goto-char beg)
                (ignore-errors
                  (bounds-of-thing-at-point thing)))))

(defclass conn-dispatch-headings (conn-dispatch-focus-targets
                                  conn-dispatch-target-window-predicate)
  ((window-predicate
    :initform (lambda (win)
                (let ((buf (window-buffer win)))
                  (or (buffer-local-value 'outline-minor-mode buf)
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buf)
                       'outline-mode)))))))

(cl-defmethod conn-dispatch-update-targets ((_state conn-dispatch-headings))
  (dolist (win (conn--get-target-windows))
    (with-current-buffer (window-buffer win)
      (let ((heading-regexp (concat "^\\(?:" outline-regexp "\\).*"
                                    outline-heading-end-regexp)))
        (save-excursion
          (pcase-dolist (`(,beg . ,end)
                         (conn--visible-regions (point-min) (point-max)))
            (goto-char beg)
            (while (re-search-forward heading-regexp end t)
              (conn-make-target-overlay
               (match-beginning 0) 0
               :window win))))))))

(defclass conn-dispatch-all-defuns (conn-dispatch-focus-targets
                                    conn-dispatch-target-window-predicate)
  ((window-predicate
    :initform (lambda (win) (eq win (selected-window))))))

(cl-defmethod conn-dispatch-update-targets ((_state conn-dispatch-all-defuns))
  (dolist (win (conn--get-target-windows))
    (with-current-buffer (window-buffer win)
      (if (bound-and-true-p treesit-primary-parser)
          (treesit-induce-sparse-tree
           (treesit-buffer-root-node)
           (or treesit-defun-type-regexp 'defun)
           (lambda (node)
             (save-excursion
               (goto-char (treesit-node-start node))
               (overlay-put
                (conn-make-target-overlay (point) 0 :window win)
                'context (cons (pos-bol)
                               (progn
                                 (when-let* ((name (treesit-defun-name node)))
                                   (search-forward name))
                                 (pos-bol 2)))))))
        (save-excursion
          (pcase-dolist (`(,beg . ,end)
                         (conn--visible-regions (point-min) (point-max)))
            (with-restriction beg end
              (goto-char (point-max))
              (while (beginning-of-defun)
                (conn-make-target-overlay (point) 0 :window win)))))))))

(defun conn-dispatch-all-things (thing)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (goto-char (window-end))
          (while (and (/= (point) (progn
                                    (forward-thing thing -1)
                                    (point)))
                      (<= (window-start) (point)))
            (unless (and (= (point) (point-min))
                         (not (bounds-of-thing-at-point thing)))
              (conn-make-target-overlay (point) 0))))))))

(defun conn-dispatch-all-buttons ()
  (declare (important-return-value t))
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (with-restriction (window-start) (window-end)
        (save-excursion
          (goto-char (point-min))
          (when (get-char-property (point) 'button)
            (conn-make-target-overlay (point) 0))
          (while (not (eobp))
            (goto-char (next-single-char-property-change (point) 'button))
            (when (get-char-property (point) 'button)
              (conn-make-target-overlay (point) 0))))))))

(defun conn-dispatch-re-matches (regexp)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (goto-char (window-start))
          (pcase-dolist (`(,beg . ,end) (conn--visible-re-matches regexp))
            (conn-make-target-overlay beg (- end beg))))))))

(defun conn-dispatch-things-read-prefix (thing prefix-length)
  (declare (important-return-value t))
  (conn-dispatch-read-n-chars
   :string-length prefix-length
   :predicate (lambda (beg _end)
                (save-excursion
                  (goto-char beg)
                  (pcase (ignore-errors (bounds-of-thing-at-point thing))
                    (`(,tbeg . ,_tend) (= beg tbeg)))))))

(defun conn-dispatch-things-with-prefix (thing prefix-string)
  (declare (important-return-value t))
  (lambda ()
    (conn-make-string-target-overlays
     prefix-string
     (lambda (beg _end)
       (save-excursion
         (goto-char beg)
         (pcase (ignore-errors (bounds-of-thing-at-point thing))
           (`(,tbeg . ,_tend) (= beg tbeg))))))))

(defun conn-dispatch-things-with-re-prefix (thing prefix-regex)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches
                        prefix-regex
                        (lambda (beg end)
                          (save-excursion
                            (goto-char beg)
                            (pcase (ignore-errors (bounds-of-thing-at-point thing))
                              (`(,tbeg . ,tend)
                               (and (= tbeg beg) (<= end tend))))))))
          (conn-make-target-overlay beg (- end beg)))))))

(defun conn-dispatch-things-matching-re (thing regexp)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches
                        regexp
                        (lambda (beg end)
                          (save-excursion
                            (goto-char beg)
                            (pcase (ignore-errors (bounds-of-thing-at-point thing))
                              (`(,tbeg . ,tend)
                               (and (<= tbeg beg) (<= tend end))))))))
          (conn-make-target-overlay beg (- end beg)))))))

(defun conn-dispatch-columns ()
  (let ((line-move-visual nil)
        (goal-column (or goal-column (current-column))))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (save-excursion
          (while (and (< (point) (point-max))
                      (line-move-1 1 t))
            (conn-make-target-overlay (point) 0)))
        (save-excursion
          (while (and (< (point-min) (point))
                      (line-move -1 t))
            (conn-make-target-overlay (point) 0)))))))

(defun conn-dispatch-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (when (and (bolp)
                     (<= (+ (point) (window-hscroll)) (pos-eol))
                     (goto-char (+ (point) (window-hscroll)))
                     (not (invisible-p (point))))
            (conn-make-target-overlay
             (point) 0
             :padding-function 'conn--right-justify-padding))
          (while (/= (point) (point-max))
            (forward-line)
            (when (and (bolp)
                       (<= (+ (point) (window-hscroll))
                           (pos-eol) (point-max))
                       (goto-char (+ (point) (window-hscroll)))
                       (not (invisible-p (point)))
                       (not (invisible-p (1- (point)))))
              (if (= (point) (point-max))
                  ;; hack to get the label displayed on its own line
                  (when-let* ((ov (conn-make-target-overlay (point) 0)))
                    (overlay-put ov 'after-string
                                 (propertize " " 'display '(space :width 0))))
                (conn-make-target-overlay
                 (point) 0
                 :padding-function 'conn--right-justify-padding)))))))))

(defun conn-dispatch-end-of-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (move-end-of-line nil)
          (when (and (eolp) (not (invisible-p (point))))
            (conn-make-target-overlay (point) 0))
          (while (/= (point) (point-max))
            (forward-line)
            (move-end-of-line nil)
            (when (and (eolp)
                       (not (invisible-p (point)))
                       (not (invisible-p (1- (point)))))
              (if (= (point-max) (point))
                  ;; hack to get the label displayed on its own line
                  (when-let* ((ov (conn-make-target-overlay (point) 0)))
                    (overlay-put ov 'after-string
                                 (propertize " " 'display '(space :width 0))))
                (conn-make-target-overlay (point) 0)))))))))

(cl-defmethod conn-dispatch-targets-other-end ((_ (eql conn-dispatch-end-of-lines)))
  t)

(defun conn-dispatch-inner-lines ()
  (let ((thing (conn-anonymous-thing
                'inner-line
                :bounds-op
                (lambda (arg)
                  (save-excursion
                    (goto-char (pos-bol))
                    (conn-bounds-of-subr 'conn-forward-inner-line arg))))))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (with-restriction (window-start) (window-end)
            (goto-char (point-max))
            (while (let ((pt (point)))
                     (forward-line -1)
                     (conn-beginning-of-inner-line)
                     (/= (point) pt))
              (when (not (invisible-p (point)))
                (conn-make-target-overlay
                 (point) 0
                 :thing thing)))))))))

(defun conn-dispatch-end-of-inner-lines ()
  (let ((thing (conn-anonymous-thing
                'inner-line
                :description "end-of-inner-line"
                :bounds-op
                (lambda (arg)
                  (save-excursion
                    (goto-char (pos-bol))
                    (conn-bounds-of-subr 'conn-forward-inner-line arg))))))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (with-restriction (window-start) (window-end)
            (goto-char (point-max))
            (while (let ((pt (point)))
                     (forward-line -1)
                     (conn-end-of-inner-line)
                     (/= (point) pt))
              (when (not (invisible-p (point)))
                (conn-make-target-overlay
                 (point) 0
                 :thing thing)))))))))

(cl-defmethod conn-dispatch-targets-other-end
  ((_ (eql conn-dispatch-end-of-inner-lines)))
  t)

(defun conn-dispatch-visual-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (save-excursion
        (goto-char (window-start))
        (vertical-motion 0)
        (conn-make-target-overlay
         (point) 0
         :thing 'char
         :padding-function 'conn--right-justify-padding)
        (vertical-motion 1)
        (while (<= (point) (window-end))
          (if (= (point) (point-max))
              ;; hack to get the label displayed on its own line
              (when-let* ((ov (conn-make-target-overlay
                               (point) 0
                               :thing 'char)))
                (overlay-put ov 'after-string
                             (propertize " " 'display '(space :width 0))))
            (conn-make-target-overlay
             (point) 0
             :thing 'char
             :padding-function 'conn--right-justify-padding))
          (vertical-motion 1))))))


;;;;; Dispatch Actions

(defvar conn-dispatch-looping nil)

(oclosure-define (conn-action
                  (:predicate conn-action-p)
                  (:copier conn-action--copy))
  (no-history :mutable t :type boolean)
  (description :type (or string nil))
  (window-predicate :type function)
  (target-predicate :type function)
  (always-retarget :type boolean)
  (always-prompt :type boolean))

(defvar conn--dispatch-loop-change-groups nil)

(defun conn-dispatch-loop-undo-boundary (&rest buffers)
  (when conn-dispatch-looping
    (push (let ((cg (mapcan #'prepare-change-group
                            (or buffers (list (current-buffer))))))
            (when conn--dispatch-loop-change-groups
              (dolist (b (or buffers (list (current-buffer))))
                (with-current-buffer b
                  (undo-boundary))))
            (lambda (do)
              (pcase do
                (:cancel (cancel-change-group cg))
                (:accept (accept-change-group cg)))))
          conn--dispatch-loop-change-groups)))

(defun conn--action-type-p (item)
  (declare (important-return-value t)
           (side-effect-free t))
  (when-let* ((class (and (symbolp item)
                          (cl--find-class item))))
    (and (oclosure--class-p class)
         (memq 'conn-action (oclosure--class-allparents class)))))

(cl-defgeneric conn-action-stale-p (action)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method ((_action conn-action)) nil))

(cl-defgeneric conn-action-copy (action)
  (declare (important-return-value t))
  (:method (action) (conn-action--copy action)))

(cl-defgeneric conn-action-cleanup (action)
  (:method (_action) "Noop" nil))

(cl-defgeneric conn-describe-action (action &optional short)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method ((action conn-action) &optional _)
    (conn-action--description action)))

(cl-defgeneric conn-accept-action (action)
  (:method ((action conn-action)) action))

(cl-defgeneric conn-cancel-action (action)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-make-action (type)
  (declare (important-return-value t))
  (:method (type) (error "Unknown action type %s" type))
  (:method :after (_type) (conn-state-eval-consume-prefix-arg)))

(cl-defmethod conn-make-action :around (type)
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (or (cl-call-next-method)
            (error "Failed to construct %s" type))
      (set-window-configuration wconf))))

(defun conn--action-buffer-change-group ()
  (declare (important-return-value t))
  (let ((change-group (prepare-change-group)))
    (activate-change-group change-group)
    (list change-group (point) (mark t) mark-active)))

(defun conn--action-accept-change-group (change-group)
  (pcase-let ((`(,handle ,_saved-point ,_saved-mark) change-group))
    (accept-change-group handle)))

(defun conn--action-cancel-change-group (change-group)
  (pcase-let ((`(,handle ,saved-point ,saved-mark ,saved-mark-active)
               change-group))
    (cancel-change-group handle)
    (goto-char saved-point)
    (let ((omark (marker-position (mark-marker)))
          (cur-mark-active mark-active))
      (set-marker (mark-marker) saved-mark)
      (setq mark-active saved-mark-active)
      (if saved-mark-active
          (when (or (not cur-mark-active)
                    (not (= omark saved-mark)))
            (run-hooks 'activate-mark-hook))
        (when cur-mark-active
          (run-hooks 'deactivate-mark-hook))))))

(defun conn--dispatch-fixup-whitespace ()
  (save-excursion
    (when (or (looking-at " ") (looking-back " " 1))
      (fixup-whitespace)
      (if (progn (beginning-of-line)
                 (looking-at "\n"))
          (join-line)
        (indent-for-tab-command)))
    (when (progn
            (beginning-of-line)
            (looking-at "\\s)*\n"))
      (join-line))))

(oclosure-define (conn-dispatch-goto
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-goto)))
  (oclosure-lambda (conn-dispatch-goto
                    (no-history t)
                    (description "Goto"))
      (window pt thing thing-arg)
    (select-window window)
    (unless (= pt (point))
      (let ((forward (< (point) pt)))
        (unless (region-active-p)
          (push-mark nil t))
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (when conn-dispatch-other-end
             (cl-rotatef beg end))
           (if (region-active-p)
               (goto-char (if forward end beg))
             (conn--push-ephemeral-mark end)
             (goto-char beg)))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-push-button
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-push-button)))
  (oclosure-lambda (conn-dispatch-push-button
                    (description "Push Button")
                    (no-history t))
      (window pt _thing _thing-arg)
    (select-window window)
    (if (button-at pt)
        (push-button pt)
      (when (fboundp 'widget-apply-action)
        (widget-apply-action (get-char-property pt 'button) pt)))))

(oclosure-define (conn-dispatch-copy-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-to)))
  (oclosure-lambda (conn-dispatch-copy-to
                    (str (funcall region-extract-function nil))
                    (separator (when (conn-state-eval-consume-prefix-arg)
                                 (read-string "Separator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and separator conn-dispatch-other-end)
             (insert separator))
           (insert-for-yank str)
           (when (and separator (not conn-dispatch-other-end))
             (insert separator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length separator)))
                                               (point)))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-copy-to--separator action))))
      (format "Copy To <%s>" sep)
    "Copy To"))

(oclosure-define (conn-dispatch-copy-replace-to
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-replace-to)))
  (oclosure-lambda (conn-dispatch-copy-replace-to
                    (description "Copy Region and Replace To")
                    (str (funcall region-extract-function nil))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point) (length str)) (point))))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-yank-replace-to
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-replace-to)))
  (oclosure-lambda (conn-dispatch-yank-replace-to
                    (description "Yank and Replace To")
                    (str (current-kill 0))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point) (length str)) (point))))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-yank-read-replace-to
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-read-replace-to)))
  (oclosure-lambda (conn-dispatch-yank-read-replace-to
                    (description "Yank and Replace To")
                    (str (read-from-kill-ring "Yank: "))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point) (length str)) (point))))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-to)))
  (oclosure-lambda (conn-dispatch-yank-to
                    (str (current-kill 0))
                    (separator (when (conn-state-eval-consume-prefix-arg)
                                 (read-string "Separator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and separator conn-dispatch-other-end)
             (insert separator))
           (insert-for-yank str)
           (when (and separator (not conn-dispatch-other-end))
             (insert separator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length separator)))
                                               (point)))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-yank-to) &optional short)
  (if-let* ((sep (and (not short) (conn-dispatch-yank-to--separator action))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-reading-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-reading-yank-to)))
  (oclosure-lambda (conn-dispatch-reading-yank-to
                    (str (read-from-kill-ring "Yank To: "))
                    (separator (when (conn-state-eval-consume-prefix-arg)
                                 (read-string "Separator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and separator conn-dispatch-other-end)
             (insert separator))
           (insert-for-yank str)
           (when (and separator (not conn-dispatch-other-end))
             (insert separator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length separator)))
                                               (point)))))
        (unless executing-kbd-macro
          (pulse-momentary-highlight-region (- (point)
                                               (+ (length str)
                                                  (length separator)))
                                            (point)))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-reading-yank-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-reading-yank-to--separator action))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-send
                  (:parent conn-action))
  (str :type string)
  (separator :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send)))
  (let ((sep (when (conn-state-eval-consume-prefix-arg)
               (read-string "Separator: " nil nil nil t)))
        (cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send
                      (change-group cg)
                      (str (prog1 (funcall region-extract-function t)
                             (conn--dispatch-fixup-whitespace)))
                      (separator sep)
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (goto-char (if conn-dispatch-other-end end beg))
             (when (and separator conn-dispatch-other-end)
               (insert separator))
             (insert-for-yank str)
             (when (and separator (not conn-dispatch-other-end))
               (insert separator))
             (unless executing-kbd-macro
               (pulse-momentary-highlight-region (- (point)
                                                    (+ (length str)
                                                       (length separator)))
                                                 (point)))))
          (unless executing-kbd-macro
            (pulse-momentary-highlight-region (- (point)
                                                 (+ (length str)
                                                    (length separator)))
                                              (point))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-send) &optional short)
  (if-let* ((sep (and (not short) (conn-dispatch-send--separator action))))
      (format "Send <%s>" sep)
    "Send"))

(cl-defmethod conn-accept-action ((action conn-dispatch-send))
  (conn--action-accept-change-group (conn-dispatch-send--change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send))
  (conn--action-cancel-change-group (conn-dispatch-send--change-group action)))

(oclosure-define (conn-dispatch-send-replace
                  (:parent conn-action))
  (str :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send-replace
                      (description "Send and Replace")
                      (change-group cg)
                      (str (prog1 (funcall region-extract-function t)
                             (conn--dispatch-fixup-whitespace)))
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (delete-region beg end)
             (insert-for-yank str)
             (unless executing-kbd-macro
               (pulse-momentary-highlight-region (- (point) (length str)) (point))))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send-replace))
  (conn--action-accept-change-group (conn-dispatch-send-replace--change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send-replace))
  (conn--action-cancel-change-group (conn-dispatch-send-replace--change-group action)))

(oclosure-define (conn-dispatch-downcase
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-downcase)))
  (oclosure-lambda (conn-dispatch-downcase
                    (description "Downcase")
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (downcase-region beg end))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-upcase
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-upcase)))
  (oclosure-lambda (conn-dispatch-upcase
                    (description "Upcase")
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (upcase-region beg end))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-capitalize
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-capitalize)))
  (oclosure-lambda (conn-dispatch-capitalize
                    (description "Capitalize")
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (capitalize-region beg end))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-register-load
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-load)))
  (oclosure-lambda (conn-dispatch-register-load
                    (register (register-read-with-preview "Register: ")))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (with-undo-amalgamate
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (goto-char (if conn-dispatch-other-end end beg))
             (conn-register-load register))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-register-load) &optional short)
  (if short "Register"
    (format "Register <%c>" (conn-dispatch-register-load--register action))))

(oclosure-define (conn-dispatch-register-replace
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-replace)))
  (oclosure-lambda (conn-dispatch-register-replace
                    (register (register-read-with-preview "Register: ")))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (with-undo-amalgamate
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (delete-region beg end)
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-register-replace) &optional short)
  (if short "Register Replace"
    (format "Register Replace <%c>" (conn-dispatch-register-replace--register action))))

(oclosure-define (conn-dispatch-kill
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill)))
  (oclosure-lambda (conn-dispatch-kill
                    (register (when (conn-state-eval-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (cond ((> conn-dispatch-repeat-count 0)
                  (conn-append-region beg end register t))
                 (register
                  (copy-to-register register beg end t))
                 (t
                  (kill-region beg end)))
           (conn--dispatch-fixup-whitespace)
           (message "Killed thing"))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-kill) &optional short)
  (if-let* ((register (conn-dispatch-kill--register action)))
      (if short "Kill to Reg"
        (format "Kill to Register <%c>" register))
    "Kill"))

(oclosure-define (conn-dispatch-kill-append (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill-append)))
  (oclosure-lambda (conn-dispatch-kill-append
                    (register (when (conn-state-eval-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (copy-to-register register beg end t)
               (kill-append str nil)
               (delete-region beg end))
             (conn--dispatch-fixup-whitespace)
             (message "Appended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-kill-append) &optional short)
  (if-let* ((register (conn-dispatch-kill-append--register action)))
      (if short "Kill App to Reg"
        (format "Kill Append Register <%c>" register))
    "Kill Append"))

(oclosure-define (conn-dispatch-kill-prepend
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill-prepend)))
  (oclosure-lambda (conn-dispatch-kill-prepend
                    (register (when (conn-state-eval-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (prepend-to-register register beg end t)
               (kill-append str t)
               (delete-region beg end))
             (conn--dispatch-fixup-whitespace)
             (message "Prepended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-kill-prepend) &optional short)
  (if-let* ((register (conn-dispatch-kill-prepend--register action)))
      (if short "Kill Pre to Reg"
        (format "Kill Prepend Register <%c>" register))
    "Kill Prepend"))

(oclosure-define (conn-dispatch-copy-append
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-append)))
  (oclosure-lambda (conn-dispatch-copy-append
                    (register (when (conn-state-eval-consume-prefix-arg)
                                (register-read-with-preview "Register: "))))
      (window pt thing thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (append-to-register register beg end)
               (kill-append str nil))
             (message "Copy Appended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-append) &optional short)
  (if-let* ((register (conn-dispatch-copy-append--register action)))
      (if short "Copy App to Reg"
        (format "Copy Append to Register <%c>" register))
    "Copy Append to Kill"))

(oclosure-define (conn-dispatch-copy-prepend
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-prepend)))
  (oclosure-lambda (conn-dispatch-copy-prepend
                    (register (when (conn-state-eval-consume-prefix-arg)
                                (register-read-with-preview "Register: "))))
      (window pt thing thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (prepend-to-register register beg end)
               (kill-append str t))
             (message "Copy Prepended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-prepend) &optional short)
  (if-let* ((register (conn-dispatch-copy-prepend--register action)))
      (if short "Copy Pre to Reg"
        (format "Copy Prepend to Register <%c>" register))
    "Copy Prepend to Kill"))

(oclosure-define (conn-dispatch-yank-from
                  (:parent conn-action)
                  (:copier conn-dispatch-yank-from-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-yank-from))
  (thread-first
    (conn-dispatch-yank-from--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-yank-from))
  (set-marker (conn-dispatch-yank-from--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-yank-from))
  (conn-dispatch-yank-from-copy
   action
   (copy-marker (conn-dispatch-yank-from--opoint action) t)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-from)))
  (oclosure-lambda (conn-dispatch-yank-from
                    (description "Yank From")
                    (opoint (copy-marker (point) t)))
      (window pt thing thing-arg)
    (let (str)
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (pulse-momentary-highlight-region beg end)
             (setq str (filter-buffer-substring beg end))))))
      (with-current-buffer (marker-buffer opoint)
        (conn-dispatch-loop-undo-boundary)
        (cond ((null str)
               (user-error "Cannot find thing at point"))
              ((/= (point) opoint)
               (save-excursion
                 (goto-char opoint)
                 (insert-for-yank str)))
              (t
               (goto-char opoint)
               (insert-for-yank str)))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-yank-from))
  (set-marker (conn-dispatch-yank-from--opoint action) nil))

(oclosure-define (conn-dispatch-yank-from-replace
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-from-replace)))
  (oclosure-lambda (conn-dispatch-yank-from-replace
                    (description "Yank From and Replace"))
      (window pt thing thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (pulse-momentary-highlight-region beg end)
           (copy-region-as-kill beg end)
           (conn--dispatch-fixup-whitespace))
          (_ (user-error "Cannot find thing at point")))))
    (conn-dispatch-loop-undo-boundary)
    (delete-region (region-beginning) (region-end))
    (yank)))

(oclosure-define (conn-dispatch-take-replace
                  (:parent conn-action)
                  (:copier conn-dispatch-take-replace-copy (opoint)))
  (opoint :type marker)
  (change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take-replace))
  (thread-first
    (conn-dispatch-take-replace--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take-replace))
  (thread-first
    (conn-dispatch-take-replace--opoint action)
    (copy-marker t)
    (conn--flip-last conn-dispatch-take-replace-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (delete-region (region-beginning) (region-end))
    (oclosure-lambda (conn-dispatch-take-replace
                      (description "Take From and Replace")
                      (change-group cg)
                      (opoint (copy-marker (point) t))
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg)
      (conn-dispatch-loop-undo-boundary (current-buffer) (window-buffer window))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (kill-region beg end)
             (conn--dispatch-fixup-whitespace))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer (marker-buffer opoint)
        (save-excursion
          (goto-char opoint)
          (yank))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--opoint action) nil)
  (conn--action-cancel-change-group
   (conn-dispatch-take-replace--change-group action)))

(cl-defmethod conn-accept-action ((action conn-dispatch-take-replace))
  (conn--action-accept-change-group
   (conn-dispatch-take-replace--change-group action)))

(oclosure-define (conn-dispatch-take
                  (:parent conn-action)
                  (:copier conn-dispatch-take-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take))
  (thread-first
    (conn-dispatch-take--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take))
  (thread-first
    (conn-dispatch-take--opoint action)
    (copy-marker t)
    (conn--flip-last conn-dispatch-take-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take)))
  (oclosure-lambda (conn-dispatch-take
                    (description "Take From")
                    (opoint (copy-marker (point) t))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg)
    (conn-dispatch-loop-undo-boundary (current-buffer) (window-buffer window))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds-get :outer `(,beg . ,end))
           (kill-region beg end)
           (conn--dispatch-fixup-whitespace))
          (_ (user-error "Cannot find thing at point")))))
    (with-current-buffer (marker-buffer opoint)
      (yank))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--opoint action) nil))

(oclosure-define (conn-dispatch-over
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-over)))
  (oclosure-lambda (conn-dispatch-over
                    (description "Over")
                    (no-history t)
                    (window-predicate (let ((obuf (current-buffer)))
                                        (lambda (win)
                                          (eq (window-buffer win) obuf)))))
      (window pt thing thing-arg)
    (when (and (eq (window-buffer window) (current-buffer))
               (/= pt (point)))
      (unless (region-active-p)
        (push-mark nil t))
      (pcase (cons (or (conn-bounds-get (conn-bounds-of thing thing-arg) :outer)
                       (point))
                   (progn
                     (goto-char pt)
                     (conn-bounds-get (conn-bounds-of thing thing-arg) :outer)))
        ((and `((,beg1 . ,end1) . (,beg2 . ,end2))
              (or (guard (<= beg1 end1 beg2 end2))
                  (guard (>= end1 beg1 end2 beg2))
                  (guard (and (= beg1 beg2) (= end1 end2)))))
         (if (> beg2 end1)
             (progn
               (conn--push-ephemeral-mark beg1)
               (goto-char (if conn-dispatch-other-end beg2 end2)))
           (conn--push-ephemeral-mark end1)
           (goto-char (if conn-dispatch-other-end end2 beg2))))
        ((and `(,point . (,beg . ,end))
              (guard (integerp point)))
         (cond ((<= point beg end)
                (goto-char end))
               ((<= beg point end)
                (goto-char beg)
                (conn--push-ephemeral-mark end))
               ((<= beg end point)
                (goto-char beg))))))))

(oclosure-define (conn-dispatch-jump
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-jump)))
  (oclosure-lambda (conn-dispatch-jump
                    (description "Jump")
                    (no-history t))
      (window pt _thing _thing-arg)
    (with-current-buffer (window-buffer window)
      (unless (= pt (point))
        (unless (region-active-p)
          (push-mark nil t))
        (select-window window)
        (goto-char pt)))))

(oclosure-define (conn-dispatch-kapply
                  (:parent conn-action))
  (macro :mutable t))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kapply)))
  (require 'conn-transients)
  (letrec ((action nil)
           (setup (lambda ()
                    (conn-without-recursive-stack
                      (conn-dispatch-kapply-prefix
                       (lambda (kapply-action)
                         (setf action kapply-action))))
                    (remove-hook 'post-command-hook setup))))
    (add-hook 'post-command-hook setup -99)
    (add-hook 'transient-post-exit-hook 'exit-recursive-edit)
    (unwind-protect
        (recursive-edit)
      (remove-hook 'post-command-hook setup)
      (remove-hook 'transient-post-exit-hook 'exit-recursive-edit))
    action))

(cl-defmethod conn-describe-action ((action conn-dispatch-kapply) &optional short)
  (if short "Kapply"
    (concat "Kapply"
            (when-let* ((macro (oref action macro)))
              (concat " <" (conn--kmacro-display (kmacro--keys macro)) ">")))))

(oclosure-define (conn-dispatch-transpose
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-transpose)))
  (oclosure-lambda (conn-dispatch-transpose
                    (description "Transpose")
                    (always-retarget t)
                    (window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
      (window1 pt1 thing1 window2 pt2 thing2 thing-arg)
    (conn-dispatch-loop-undo-boundary (window-buffer window1)
                                      (window-buffer window2))
    (conn--dispatch-transpose-subr
     (window-buffer window1) pt1 thing1
     (window-buffer window2) pt2 thing2
     thing-arg)))

(defun conn--dispatch-transpose-subr ( buffer1 pt1 thing1
                                       buffer2 pt2 thing2
                                       thing-arg)
  (if (eq buffer1 buffer2)
      (with-current-buffer buffer1
        (save-excursion
          (pcase-let ((`(,beg1 . ,end1)
                       (progn
                         (goto-char pt1)
                         (or (conn-bounds-get (conn-bounds-of thing1 thing-arg) :outer)
                             (user-error "Cannot find thing at point"))))
                      (`(,beg2 . ,end2)
                       (progn
                         (goto-char pt2)
                         (or (conn-bounds-get (conn-bounds-of thing2 thing-arg) :outer)
                             (user-error "Cannot find thing at point")))))
            (if (and (or (<= beg1 end1 beg2 end2)
                         (<= beg2 end2 beg1 end1))
                     (/= beg1 end1)
                     (/= beg2 end2)
                     (<= (point-min) (min beg2 end2 beg1 end1))
                     (> (point-max) (max beg2 end2 beg1 end1)))
                (transpose-regions beg1 end1 beg2 end2)
              (user-error "Invalid regions")))))
    (conn-protected-let* ((cg (nconc (prepare-change-group buffer1)
                                     (prepare-change-group buffer2))
                              (cancel-change-group cg))
                          (str1)
                          (str2))
      (activate-change-group cg)
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (pcase (conn-bounds-of thing1 thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (setq pt1 beg)
             (setq str1 (filter-buffer-substring beg end))
             (delete-region beg end))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer2
        (save-excursion
          (goto-char pt2)
          (pcase (conn-bounds-of thing2 thing-arg)
            ((conn-bounds-get :outer `(,beg . ,end))
             (setq str2 (filter-buffer-substring beg end))
             (delete-region beg end)
             (insert str1))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (insert str2)))
      (accept-change-group cg))))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-state)
  "RET" 'conn-repeat-last-dispatch
  "<return>" 'conn-repeat-last-dispatch
  "M-n" 'conn-dispatch-cycle-ring-next
  "M-p" 'conn-dispatch-cycle-ring-previous
  "M-f" 'conn-dispatch-ring-describe-head
  "d" 'conn-dispatch-copy-to
  "D" 'conn-dispatch-copy-replace-to
  "v" 'conn-dispatch-over-or-goto
  "x" 'conn-dispatch-yank-replace-to
  "X" 'conn-dispatch-yank-read-replace-to
  "C-y" 'conn-dispatch-yank-replace-to
  "M-y" 'conn-dispatch-yank-read-replace-to
  "y" 'conn-dispatch-yank-to
  "Y" 'conn-dispatch-reading-yank-to
  "F" 'conn-dispatch-yank-from-replace
  "f" 'conn-dispatch-yank-from
  "s" 'conn-dispatch-send
  "S" 'conn-dispatch-send-replace
  "T" 'conn-dispatch-take-replace
  "t" 'conn-dispatch-take
  "P" 'conn-dispatch-register-replace
  "w" 'conn-dispatch-kill
  "q" 'conn-dispatch-transpose
  "C-SPC" 'conn-dispatch-jump
  "R" 'conn-dispatch-register-load
  "<remap> <downcase-word>" 'conn-dispatch-downcase
  "<remap> <downcase-region>" 'conn-dispatch-downcase
  "<remap> <downcase-dwim>" 'conn-dispatch-downcase
  "<remap> <upcase-word>" 'conn-dispatch-upcase
  "<remap> <upcase-region>" 'conn-dispatch-upcase
  "<remap> <upcase-dwim>" 'conn-dispatch-upcase
  "<remap> <capitalize-word>" 'conn-dispatch-capitalize
  "<remap> <capitalize-region>" 'conn-dispatch-capitalize
  "<remap> <capitalize-dwim>" 'conn-dispatch-capitalize
  "<remap> <conn-kill-append-region>" 'conn-dispatch-kill-append
  "<remap> <conn-kill-prepend-region>" 'conn-dispatch-kill-prepend
  "<remap> <conn-append-region>" 'conn-dispatch-copy-append
  "<remap> <conn-prepend-region>" 'conn-dispatch-copy-prepend
  "<remap> <conn-previous-emacs-state>"
  (conn-anonymous-thing
   'char
   :default-action (lambda ()
                     (let ((jump (conn-make-action 'conn-dispatch-jump)))
                       (oclosure-lambda (conn-action
                                         (description "Previous Emacs State")
                                         (no-history t))
                           (&rest args)
                         (apply jump args)
                         (conn-push-state 'conn-emacs-state))))
   :target-finder (lambda () (conn-dispatch-previous-emacs-state))))

;;;;; Perform Dispatch Loop

(define-error 'conn-dispatch-error "Dispatch error" 'user-error)

(defun conn-dispatch-error (string)
  (setf conn--state-eval-error-message string)
  (signal 'conn-dispatch-error nil))

(define-minor-mode conn-dispatch-select-mode
  "Mode for dispatch event reading"
  :global t
  :lighter " SELECT"
  (if conn-dispatch-select-mode
      (with-memoization (alist-get (current-buffer) conn--dispatch-remap-cookies)
        (face-remap-add-relative
         'mode-line
         (conn-state-get 'conn-dispatch-state :mode-line-face)))
    (pcase-dolist (`(,buf . ,cookie) conn--dispatch-remap-cookies)
      (with-current-buffer buf
        (face-remap-remove-relative cookie)))))

(cl-defun conn-dispatch-handle-and-redisplay (&key (prompt t))
  (redisplay)
  (setq conn--dispatch-must-prompt prompt)
  (throw 'dispatch-redisplay nil))

(defun conn-dispatch-handle ()
  (throw 'dispatch-handle t))

(defmacro conn-perform-dispatch-loop (repeat &rest body)
  (declare (indent 1))
  (cl-with-gensyms (rep display-always)
    `(catch 'dispatch-select-exit
       (let* ((,rep nil)
              (,display-always nil)
              (conn-dispatch-looping t)
              (conn--dispatch-loop-change-groups nil)
              (conn--state-eval-error-message nil)
              (conn--dispatch-read-event-message-prefixes
               `(,(car conn--dispatch-read-event-message-prefixes)
                 ,(when (conn-dispatch-retargetable-p conn-dispatch-target-finder)
                    (lambda ()
                      (when-let* ((binding
                                   (and (or ,rep
                                            ,display-always
                                            conn--dispatch-always-retarget)
                                        (where-is-internal
                                         'always-retarget
                                         conn-dispatch-read-event-map
                                         t))))
                        (setf ,display-always t)
                        (concat
                         (propertize (key-description binding)
                                     'face 'help-key-binding)
                         " "
                         (propertize "always retarget"
                                     'face (when conn--dispatch-always-retarget
                                             'eldoc-highlight-function-argument))))))
                 ,@(cdr conn--dispatch-read-event-message-prefixes))))
         (unwind-protect
             (while (or (setq ,rep ,repeat)
                        (< conn-dispatch-repeat-count 1))
               (catch 'dispatch-redisplay
                 (condition-case err
                     (progn
                       ,@body
                       (cl-incf conn-dispatch-repeat-count))
                   (user-error
                    (conn-dispatch-error (error-message-string err))))))
           (dolist (undo conn--dispatch-loop-change-groups)
             (funcall undo :accept)))))))

(defmacro conn-with-dispatch-suspended (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (select-mode)
    `(pcase-let ((`(,conn-target-window-predicate
                    ,conn-target-predicate
                    ,conn-target-sort-function)
                  conn--dispatch-init-state)
                 (conn-dispatch-looping nil)
                 (conn--dispatch-loop-change-groups nil)
                 (inhibit-message nil)
                 (recenter-last-op nil)
                 (conn-dispatch-repeat-count nil)
                 (conn-dispatch-other-end nil)
                 (conn-state-eval-last-command nil)
                 (conn--state-eval-prefix-mag nil)
                 (conn--state-eval-prefix-sign nil)
                 (conn--dispatch-read-event-handlers nil)
                 (conn--dispatch-read-event-message-prefixes nil)
                 (conn--dispatch-always-retarget nil)
                 (,select-mode conn-dispatch-select-mode))
       (conn-delete-targets)
       (message nil)
       (if ,select-mode (conn-dispatch-select-mode -1))
       (unwind-protect
           ,(macroexp-progn body)
         (if ,select-mode (conn-dispatch-select-mode 1))))))

(cl-defgeneric conn-handle-dispatch-select-command (command)
  (:method (_cmd) nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql mwheel-scroll)))
  (require 'mwheel)
  (mwheel-scroll last-input-event)
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recursive-edit)))
  (conn-with-dispatch-suspended
    (recursive-edit))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((cmd (eql recenter-top-bottom)))
  (let ((this-command cmd)
        (last-command conn-state-eval-last-command))
    (recenter-top-bottom (conn-state-eval-prefix-arg)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql toggle-input-method)))
  (let ((inhibit-message nil))
    (toggle-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql set-input-method)))
  (let ((inhibit-message nil))
    (call-interactively 'set-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-forward)))
  (conn-with-dispatch-suspended
    (isearch-forward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-backward)))
  (conn-with-dispatch-suspended
    (isearch-backward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-forward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-forward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-backward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-backward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-up)))
  (let ((next-screen-context-lines (or (conn-state-eval-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up))
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-down)))
  (let ((next-screen-context-lines (or (conn-state-eval-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-down))
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(defun conn-dispatch-goto-window (window)
  (select-window window)
  (with-memoization (alist-get (current-buffer) conn--dispatch-remap-cookies)
    (face-remap-add-relative
     'mode-line
     (conn-state-get 'conn-dispatch-state :mode-line-face))))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql conn-goto-window)))
  (if-let* ((windows (delq (selected-window) (conn--get-target-windows))))
      (progn
        (conn-dispatch-goto-window (conn-prompt-for-window windows))
        (conn-dispatch-handle-and-redisplay :prompt nil))
    (conn-dispatch-handle)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql finish)))
  (throw 'dispatch-select-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql keyboard-quit)))
  (throw 'dispatch-select-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql dispatch-other-end)))
  (unless conn-dispatch-no-other-end
    (cl-callf not conn-dispatch-other-end)
    (conn-dispatch-handle-and-redisplay :prompt nil)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql retarget)))
  (conn-dispatch-retarget conn-dispatch-target-finder)
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql always-retarget)))
  (setq conn--dispatch-always-retarget (not conn--dispatch-always-retarget))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql restrict-windows)))
  (cond ((advice-function-member-p 'conn--dispatch-restrict-windows
                                   conn-target-window-predicate)
         (remove-function conn-target-window-predicate
                          'conn--dispatch-restrict-windows)
         (conn-dispatch-handle-and-redisplay))
        ((length> conn-targets 1)
         (add-function :after-while conn-target-window-predicate
                       'conn--dispatch-restrict-windows)
         (conn-dispatch-handle-and-redisplay))))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql undo)))
  (when conn--dispatch-loop-change-groups
    (funcall (pop conn--dispatch-loop-change-groups) :cancel))
  (conn-dispatch-handle-and-redisplay))

(defun conn-dispatch-change-target (thing thing-arg)
  (conn-dispatch-cleanup-target-finder conn-dispatch-target-finder)
  (setq conn-dispatch-target-finder (conn-get-target-finder thing))
  (throw 'dispatch-change-target (list thing thing-arg)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql change-target-finder)))
  (apply #'conn-dispatch-change-target
         (conn-with-dispatch-suspended
           (conn-eval-with-state 'conn-dispatch-mover-state
               (list && (conn-thing-argument-dwim t))
             :prompt "New Targets"))))


;;;;; Dispatch Ring

(cl-defstruct (conn-previous-dispatch
               (:constructor
                conn-make-dispatch
                ( action thing thing-arg keys
                  &aux
                  (restrict-windows (advice-function-member-p
                                     'conn--dispatch-restrict-windows
                                     conn-target-window-predicate))
                  (other-end (if conn-dispatch-no-other-end
                                 :no-other-end
                               conn-dispatch-other-end))
                  (always-retarget conn--dispatch-always-retarget)))
               (:copier conn--previous-dispatch-copy))
  (action nil :type conn-action)
  (thing nil :type (or symbol conn-anonymous-thing))
  (thing-arg nil :type (or nil integer))
  (keys nil :type list)
  (other-end nil :type symbol)
  (always-retarget nil :type boolean)
  (repeat nil :type boolean)
  (restrict-windows nil :type boolean))

(defvar conn-dispatch-ring-max 12)

(defvar conn-dispatch-ring
  (conn-make-ring conn-dispatch-ring-max
                  :cleanup 'conn-dispatch--cleanup))

(cl-defmethod conn-handle-dispatch-command ((_cmd (eql conn-repeat-last-dispatch))
                                            form)
  (pcase (conn-ring-head conn-dispatch-ring)
    ((and prev
          (cl-struct conn-previous-dispatch
                     thing thing-arg
                     action
                     keys
                     other-end
                     always-retarget
                     repeat
                     restrict-windows))
     (if (conn-action-stale-p action)
         (progn
           (conn-dispatch-ring-remove-stale)
           (conn-dispatch-error "Last dispatch action stale"))
       (conn-ring-delete conn-dispatch-ring prev)
       (conn-state-eval-handle)
       (cons (conn-state-eval-quote
              (conn-perform-dispatch & action & thing & thing-arg
                                     :always-retarget & always-retarget
                                     :repeat & repeat
                                     :restrict-windows & restrict-windows
                                     :other-end & other-end
                                     && keys))
             form)))
    (_ (conn-dispatch-error "Dispatch ring empty"))))

(defun conn-dispatch-copy (dispatch)
  (declare (important-return-value t))
  (let ((copy (conn--previous-dispatch-copy dispatch)))
    (setf (conn-previous-dispatch-action copy)
          (conn-action-copy (conn-previous-dispatch-action dispatch)))
    copy))

(defun conn-dispatch--cleanup (dispatch)
  (conn-action-cleanup (conn-previous-dispatch-action dispatch)))

(defun conn-describe-dispatch (dispatch)
  (declare (conn-anonymous-thing-property
            :description "dispatch description for this thing")
           (side-effect-free t))
  (format "%s @ %s <%s>"
          (conn-describe-action (conn-previous-dispatch-action dispatch))
          (pcase (conn-previous-dispatch-thing dispatch)
            ((and op (cl-type conn-anonymous-thing))
             (or (conn-anonymous-thing-property op :description)
                 (conn-anonymous-thing-parent op)))
            (op op))
          (conn-previous-dispatch-thing-arg dispatch)))

(defun conn-dispatch-push-history (dispatch)
  (conn-dispatch-ring-remove-stale)
  (unless (conn-action--no-history (conn-previous-dispatch-action dispatch))
    (conn-ring-insert-front conn-dispatch-ring dispatch)))

(defun conn-dispatch-ring-remove-stale ()
  (cl-loop for stale in (seq-filter
                         (lambda (dispatch)
                           (conn-action-stale-p
                            (conn-previous-dispatch-action dispatch)))
                         (conn-ring-list conn-dispatch-ring))
           do (conn-ring-delete conn-dispatch-ring stale)))

(defun conn-dispatch-cycle-ring-next ()
  "Cycle backwards through `conn-dispatch-ring'."
  (interactive)
  (unless (conn-ring-head conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (conn-dispatch-ring-remove-stale)
  (conn-ring-rotate-backward conn-dispatch-ring)
  (unless executing-kbd-macro
    (message (conn-describe-dispatch
              (conn-ring-head conn-dispatch-ring)))))

(defun conn-dispatch-cycle-ring-previous ()
  "Cycle backwards through `conn-dispatch-ring'."
  (interactive)
  (unless (conn-ring-head conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (conn-dispatch-ring-remove-stale)
  (conn-ring-rotate-forward conn-dispatch-ring)
  (unless executing-kbd-macro
    (message (conn-describe-dispatch
              (conn-ring-head conn-dispatch-ring)))))

(defun conn-call-previous-dispatch (dispatch &rest override-keys)
  (pcase-let (((cl-struct conn-previous-dispatch
                          thing thing-arg
                          action
                          keys
                          other-end
                          always-retarget
                          repeat
                          restrict-windows)
               dispatch))
    (apply #'conn-perform-dispatch
           `( ,action ,thing ,thing-arg
              ,@override-keys
              :always-retarget ,always-retarget
              :repeat ,repeat
              :restrict-windows ,restrict-windows
              :other-end ,other-end
              ,@keys))))


;;;;; Dispatch Commands

(cl-defgeneric conn-perform-dispatch (action
                                      thing
                                      thing-arg
                                      &key
                                      repeat
                                      restrict-windows
                                      other-end
                                      &allow-other-keys))

(cl-defmethod conn-perform-dispatch ((_action (eql nil))
                                     thing thing-arg
                                     &rest keys
                                     &key &allow-other-keys)
  (apply #'conn-perform-dispatch
         (conn-make-default-action thing)
         thing thing-arg keys))

(cl-defmethod conn-perform-dispatch :around ((action conn-action)
                                             thing
                                             thing-arg
                                             &rest keys
                                             &key
                                             restrict-windows
                                             other-end
                                             always-retarget
                                             &allow-other-keys)
  (let* ((opoint (point-marker))
         (eldoc-display-functions nil)
         (recenter-last-op nil)
         (conn-state-eval-last-command nil)
         (conn--dispatch-init-state
          (list conn-target-window-predicate
                conn-target-predicate
                conn-target-sort-function))
         (conn-target-window-predicate conn-target-window-predicate)
         (conn-target-predicate conn-target-predicate)
         (conn-target-sort-function conn-target-sort-function)
         (conn--dispatch-must-prompt nil)
         (conn--state-eval-prefix-mag nil)
         (conn--state-eval-prefix-sign nil)
         (conn--dispatch-read-event-handlers
          (cons #'conn-handle-dispatch-select-command
                conn--dispatch-read-event-handlers))
         (conn-dispatch-target-finder (conn-get-target-finder thing))
         (conn-dispatch-repeat-count 0)
         (conn--dispatch-always-retarget
          (or always-retarget
              (conn-action--always-retarget action)))
         (target-other-end (conn-dispatch-targets-other-end
                            conn-dispatch-target-finder))
         (conn-dispatch-no-other-end (or (eq other-end :no-other-end)
                                         (eq target-other-end :no-other-end)))
         (conn-dispatch-other-end
          (unless conn-dispatch-no-other-end
            (xor target-other-end (or other-end conn-dispatch-other-end))))
         (conn--dispatch-action-always-prompt (conn-action--always-prompt action))
         (conn--dispatch-read-event-message-prefixes
          `(,(propertize (conn-describe-action action t)
                         'face 'eldoc-highlight-function-argument)
            ,(when (conn-dispatch-retargetable-p conn-dispatch-target-finder)
               (lambda ()
                 (when-let* ((binding
                              (and (conn-dispatch-has-target-p conn-dispatch-target-finder)
                                   (not conn--dispatch-always-retarget)
                                   (where-is-internal 'retarget
                                                      conn-dispatch-read-event-map
                                                      t))))
                   (concat
                    (propertize (key-description binding)
                                'face 'help-key-binding)
                    " retarget"))))
            ,(unless conn-dispatch-no-other-end
               (lambda ()
                 (when-let* ((binding
                              (where-is-internal 'dispatch-other-end
                                                 conn-dispatch-read-event-map
                                                 t)))
                   (concat
                    (propertize (key-description binding)
                                'face 'help-key-binding)
                    " "
                    (propertize
                     "other end"
                     'face (when conn-dispatch-other-end
                             'eldoc-highlight-function-argument))))))
            ,(lambda ()
               (when-let* (((or (length> conn-targets 1)
                                (advice-function-member-p 'conn--dispatch-restrict-windows
                                                          conn-target-window-predicate)))
                           (binding
                            (where-is-internal 'restrict-windows
                                               conn-dispatch-read-event-map
                                               t)))
                 (concat
                  (propertize (key-description binding)
                              'face 'help-key-binding)
                  " "
                  (propertize
                   "this win"
                   'face (when (advice-function-member-p
                                'conn--dispatch-restrict-windows
                                conn-target-window-predicate)
                           'eldoc-highlight-function-argument)))))
            ,@conn--dispatch-read-event-message-prefixes)))
    (when-let* ((predicate (conn-action--window-predicate action)))
      (add-function :after-while conn-target-window-predicate predicate))
    (when-let* ((predicate (conn-action--target-predicate action)))
      (add-function :after-while conn-target-predicate predicate))
    (when restrict-windows
      (add-function :after-while conn-target-window-predicate
                    'conn--dispatch-restrict-windows))
    (unwind-protect
        (progn
          (while-let ((new-target
                       (catch 'dispatch-change-target
                         (apply #'cl-call-next-method action thing thing-arg keys)
                         nil)))
            (pcase-setq `(,thing ,thing-arg) new-target))
          (conn-dispatch-push-history
           (conn-make-dispatch action thing thing-arg keys)))
      (ignore-errors
        (conn-dispatch-cleanup-target-finder conn-dispatch-target-finder))
      (ignore-errors
        (conn-delete-targets))
      (ignore-errors
        (with-current-buffer (marker-buffer opoint)
          (unless (= (point) opoint)
            (conn--push-mark-ring opoint))))
      (set-marker opoint nil)
      (let ((inhibit-message conn-state-eval-inhibit-message))
        (message nil)))))

(cl-defmethod conn-perform-dispatch ((action conn-action)
                                     thing
                                     thing-arg
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let* ((`(,pt ,win ,thing-override)
                  (conn-dispatch-select-target)))
      (funcall action win pt
               (or thing-override thing)
               thing-arg))))

(cl-defmethod conn-perform-dispatch ((action conn-dispatch-transpose)
                                     thing
                                     thing-arg
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let ((`(,pt1 ,win1 ,thing-override1)
                 (conn-dispatch-select-target))
                (`(,pt2 ,win2 ,thing-override2)
                 (conn-dispatch-select-target)))
      (funcall action
               win1 pt1 (or thing-override1 thing)
               win2 pt2 (or thing-override2 thing)
               thing-arg))))

(cl-defmethod conn-perform-dispatch ((action conn-dispatch-kapply)
                                     thing thing-arg
                                     &key repeat &allow-other-keys)
  (let ((conn-label-select-always-prompt t))
    (conn-perform-dispatch-loop repeat
      (pcase-let* ((`(,pt ,win ,thing-override)
                    (conn-dispatch-select-target)))
        (while
            (condition-case err
                (progn
                  (funcall action win pt
                           (or thing-override thing)
                           thing-arg)
                  nil)
              (user-error (message (cadr err)) t))))))
  (unless conn-kapply-suppress-message
    (message "Kapply completed successfully after %s iterations"
             conn-dispatch-repeat-count)))

(defun conn-dispatch (&optional initial-arg)
  (interactive "P")
  (conn-eval-with-state 'conn-dispatch-state
      (conn-perform-dispatch
       & (conn-dispatch-action-argument)
       && (conn-thing-argument t)
       :repeat & (conn-dispatch-repeat-argument)
       :other-end & (conn-dispatch-other-end-argument nil)
       :restrict-windows & (conn-dispatch-restrict-windows-argument))
    :command-handler #'conn-handle-dispatch-command
    :prefix initial-arg
    :prompt "Dispatch"
    :pre (when (and (bound-and-true-p conn-posframe-mode)
                    (fboundp 'posframe-hide))
           (lambda (_) (posframe-hide " *conn-list-posframe*")))))

(defun conn--dispatch-bounds (bounds &optional repeat)
  (let (ovs subregions)
    (unwind-protect
        (progn
          (conn-eval-with-state 'conn-dispatch-mover-state
              (conn-perform-dispatch
               & (oclosure-lambda (conn-action
                                   (description "Bounds")
                                   (window-predicate
                                    (let ((win (selected-window)))
                                      (lambda (window) (eq win window)))))
                     (window pt thing thing-arg)
                   (with-selected-window window
                     (save-mark-and-excursion
                       (goto-char pt)
                       (pcase (conn-bounds-of-subr thing thing-arg)
                         ((and (conn-bounds-get :outer `(,beg . ,end))
                               bound)
                          (unless executing-kbd-macro
                            (push (make-overlay beg end) ovs)
                            (overlay-put (car ovs) 'face 'region))
                          (push bound subregions))
                         (_
                          (user-error "No %s found at point" thing))))))
               && (conn-thing-argument t)
               :repeat & (conn-dispatch-repeat-argument repeat)
               :other-end :no-other-end)
            :prefix (conn-bounds-arg bounds)
            :prompt "Bounds of Dispatch")
          (unless ovs (keyboard-quit))
          (cl-loop for bound in subregions
                   for (b . e) = (conn-bounds-get bound :outer)
                   minimize b into beg
                   maximize e into end
                   finally do
                   (setf (conn-bounds-get bounds :outer) (cons beg end)
                         (conn-bounds-get bounds :subregions) subregions))
          bounds)
      (mapc #'delete-overlay ovs))))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing dispatch)) arg)
  (conn-bounds
   cmd arg
   :outer (lambda (bounds)
            (conn-bounds-get (conn--dispatch-bounds bounds) :outer))
   :subregions (lambda (bounds)
                 (conn-bounds-get (conn--dispatch-bounds bounds t) :subregions))))

(defun conn-repeat-last-dispatch (invert-repeat)
  "Repeat the last dispatch command.

Prefix arg INVERT-REPEAT inverts the value of repeat in the last dispatch."
  (interactive "P")
  (when (conn-action-stale-p
         (conn-previous-dispatch-action (conn-ring-head conn-dispatch-ring)))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (let ((prev (conn-ring-head conn-dispatch-ring)))
    (conn-call-previous-dispatch
     prev
     :repeat (xor invert-repeat (conn-previous-dispatch-repeat prev)))))

(defun conn-last-dispatch-at-mouse (event &optional repeat)
  (interactive "e\nP")
  (unless (mouse-event-p event)
    (error "conn-last-dispatch-at-mouse must be bound to a mouse event"))
  (unless (conn-ring-list conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (when (conn-action-stale-p (conn-previous-dispatch-action
                              (conn-ring-head conn-dispatch-ring)))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (push `(no-record . (dispatch-mouse-repeat ,@(cdr event)))
        unread-command-events)
  (let ((conn-state-eval-inhibit-message t)
        (conn-kapply-suppress-message t))
    (conn-repeat-last-dispatch
     (and (conn-previous-dispatch-repeat (conn-ring-head conn-dispatch-ring))
          repeat))))

(defun conn-bind-last-dispatch-to-key ()
  "Bind last dispatch command to a key.

Prefix arg REPEAT inverts the value of repeat in the last dispatch."
  (interactive)
  (let* ((key-seq (read-key-sequence
                   (format "Bind last dispatch to key in %s: "
                           conn-current-state)))
         (binding (key-binding key-seq)))
    (when (and (not (equal key-seq "\^G"))
               (or (not binding)
                   (eq binding 'undefined)
                   (stringp binding)
                   (vectorp binding)
                   (yes-or-no-p (format "%s runs command %S.  Bind anyway? "
                                        (format-kbd-macro key-seq)
                                        binding))))
      (define-key (conn-get-minor-mode-map conn-current-state :bind-last)
                  key-seq (conn-dispatch-copy (conn-ring-head conn-dispatch-ring)))
      (message "Dispatch bound to %s" (format-kbd-macro key-seq)))))

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-push-button)
   (conn-anonymous-thing
    'button
    :description "all-buttons"
    :target-finder (lambda () 'conn-dispatch-all-buttons)
    :other-end :no-other-end)
   nil))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let ((target-finder
         (let ((targets (with-restriction (window-start) (window-end)
                          (conn--isearch-matches))))
           (lambda ()
             (cl-loop for (beg . end) in targets
                      do (conn-make-target-overlay beg (- end beg)))))))
    (unwind-protect ; In case this was a recursive isearch
        (isearch-exit)
      (conn-perform-dispatch
       (conn-make-action 'conn-dispatch-jump)
       (conn-anonymous-thing nil :target-finder (lambda () target-finder))
       nil
       :restrict-windows t
       :other-end :no-other-end))))

(defun conn-goto-char-2 ()
  "Jump to point defined by two characters and maybe a label."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-jump)
   nil nil
   :other-end :no-other-end))

;;;;; Dispatch Registers

(cl-defstruct (conn-dispatch-register
               (:constructor conn--make-dispatch-register (dispatch-command)))
  (dispatch-command nil :type conn-dispatch-context))

(cl-defmethod register-val-jump-to ((val conn-dispatch-register) arg)
  (conn-call-previous-dispatch
   val
   :repeat (xor arg (conn-previous-dispatch-repeat val))))

(cl-defmethod register-val-describe ((_val conn-dispatch-register) _arg)
  (princ "Dispatch Register"))

(defun conn-last-dispatch-to-register (register)
  "Store last dispatch command in REGISTER."
  (interactive (list (register-read-with-preview "Dispatch to register: ")))
  (set-register register (conn--make-dispatch-register
                          (conn-ring-head conn-dispatch-ring))))


;;;; Expand Region

(defcustom conn-expand-pulse-region t
  "Pulse region on expansion when mark is not active"
  :group 'conn
  :type 'boolean)

(defvar conn-expansion-functions nil
  "Functions which provide expansions for `conn-expand'.

Functions should return a list of (BEGIN . END) pairs representing
potential expansions.  Functions may return invalid expansions
(e.g. nil, invalid regions, etc.) and they will be filtered out.")

(defvar-local conn--current-expansions nil)
(defvar-local conn--current-expansions-tick nil)

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
  "j" 'conn-contract
  "h" 'conn-expand
  "l" 'conn-expand)

(conn-register-thing 'expansion)

(defun conn--expand-filter-regions (regions)
  (declare (important-return-value t))
  (let (result)
    (pcase-dolist ((and reg `(,beg . ,end))
                   (delete-dups regions))
      (when (and beg end
                 (/= beg end)
                 (<= beg (point) end))
        (push reg result)))
    result))

(defun conn--valid-expansions-p ()
  (declare (important-return-value t)
           (side-effect-free t))
  (and (eql conn--current-expansions-tick (buffer-chars-modified-tick))
       (or (and conn--current-expansions
                (region-active-p)
                (cl-loop for (beg . end) in conn--current-expansions
                         when (or (= beg (region-beginning))
                                  (= end (region-end)))
                         return t))
           (member (cons (region-beginning) (region-end))
                   conn--current-expansions))))

(defun conn--expand-create-expansions ()
  (unless (conn--valid-expansions-p)
    (setq conn--current-expansions-tick (buffer-chars-modified-tick)
          conn--current-expansions
          (compat-call sort
                       (thread-last
                         (mapcan #'funcall conn-expansion-functions)
                         (conn--expand-filter-regions))
                       :lessp (lambda (a b)
                                (or (> (car a) (car b))
                                    (< (cdr a) (cdr b))))
                       :in-place t))))

(defun conn-expand-exchange ()
  "Move point to the other end of the current expansion."
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)
    (conn-exchange-mark-command)))

(defun conn-expand (arg)
  "Expend region by semantic units.

If the region is active only the `point' is moved.
Expansions are provided by functions in `conn-expansion-functions'."
  (interactive "P")
  (if (consp arg)
      (progn
        (conn--push-ephemeral-mark)
        (setq arg (log (prefix-numeric-value arg) 4)))
    (setq arg (prefix-numeric-value arg)))
  (conn--expand-create-expansions)
  (if (< arg 0)
      (conn-contract (- arg))
    (dotimes (_ arg)
      (cond ((and (region-active-p)
                  (= (point) (region-beginning)))
             (cl-loop for (beg . _end) in conn--current-expansions
                      when (< beg (point)) return (goto-char beg)
                      finally (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (cl-loop for (_beg . end) in conn--current-expansions
                      when (> end (point)) return (goto-char end)
                      finally (user-error "No more expansions")))
            ((cl-loop for (beg . end) in conn--current-expansions
                      when (or (and (< beg (region-beginning))
                                    (>= end (region-end)))
                               (and (<= beg (region-beginning))
                                    (> end (region-end))))
                      return (progn
                               (goto-char (if (= (point) (region-beginning)) beg end))
                               (conn--push-ephemeral-mark
                                (if (= (point) (region-beginning)) end beg)))
                      finally (user-error "No more expansions"))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))

(defun conn-contract (arg)
  "Contract region by semantic units.

If the region is active only the `point' is moved.
Expansions and contractions are provided by functions in
`conn-expansion-functions'."
  (interactive "P")
  (if (consp arg)
      (setq arg (log (prefix-numeric-value arg) 4))
    (setq arg (prefix-numeric-value arg)))
  (conn--expand-create-expansions)
  (if (< arg 0)
      (conn-expand (- arg))
    (dotimes (_ arg)
      (cond ((and (region-active-p)
                  (= (point) (region-beginning)))
             (cl-loop for (beg . _end) in (reverse conn--current-expansions)
                      when (> beg (point)) return (goto-char beg)
                      finally (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (cl-loop for (_beg . end) in (reverse conn--current-expansions)
                      when (< end (point)) return (goto-char end)
                      finally (user-error "No more expansions")))
            ((cl-loop for (beg . end) in (reverse conn--current-expansions)
                      when (or (> beg (region-beginning))
                               (< end (region-end)))
                      return (progn
                               (goto-char (if (= (point) (region-beginning)) beg end))
                               (conn--push-ephemeral-mark (if (= (point) (region-end)) beg end)))
                      finally (user-error "No more contractions"))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))


;;;;; Bounds of expansion

(conn-define-state conn-expand-state (conn-mode-line-face-state)
  "State for expanding."
  :lighter "↔"
  :mode-line-face 'conn-read-thing-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-expand-state)
  "z" 'conn-expand-exchange
  "j" 'conn-contract
  "l" 'conn-expand
  "v" 'conn-toggle-mark-command
  "e" 'end
  "<mouse-3>" 'end
  "<mouse-1>" 'conn-expand
  "S-<mouse-1>" 'conn-contract
  "<escape>" 'end)

(defun conn--read-expand-message (_arg)
  (substitute-command-keys
   (concat
    "\\[conn-expand] expand; "
    "\\[conn-contract] contract; "
    "\\[conn-toggle-mark-command] toggle mark; "
    "\\[end] finish")))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing expansion)) arg)
  (call-interactively cmd)
  (conn-eval-with-state 'conn-expand-state
      (conn-bounds
       & cmd & arg
       :outer & (oclosure-lambda (conn-state-eval-argument
                                  (required t)
                                  (name 'conn--read-expand-message))
                    (command self)
                  (pcase command
                    ('conn-expand-exchange
                     (conn-expand-exchange))
                    ('conn-contract
                     (conn-contract (conn-state-eval-consume-prefix-arg)))
                    ('conn-expand
                     (conn-expand (conn-state-eval-consume-prefix-arg)))
                    ('conn-toggle-mark-command
                     (conn-toggle-mark-command))
                    ((or 'end 'exit-recursive-edit)
                     (conn-set-argument self (cons (region-beginning)
                                                   (region-end)))))))
    :prompt "Expansion"
    :prefix arg))


;;;; Narrow Ring

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(defvar conn-narrow-ring-max 14)

(cl-defstruct (conn-narrow-register
               (:constructor conn--make-narrow-register (narrow-ring)))
  (narrow-ring nil :type list))

(defun conn--narrow-ring-to-register ()
  (conn--make-narrow-register
   (cl-loop for (beg . end) in conn-narrow-ring
            collect (cons (copy-marker beg) (copy-marker end)))))

(cl-defmethod register-val-jump-to ((val conn-narrow-register) _arg)
  (let ((ring (conn-narrow-register-narrow-ring val)))
    (unless (eq (current-buffer) (marker-buffer (caar ring)))
      (user-error "Markers do not point to this buffer"))
    (setq conn-narrow-ring
          (cl-loop for (beg . end) in ring
                   collect (cons (copy-marker beg)
                                 (copy-marker end))))))

(cl-defmethod register-val-describe ((val conn-narrow-register) _arg)
  (thread-last
    (conn-narrow-register-narrow-ring val)
    (caar)
    (marker-buffer)
    (format "Narrowings In:  %s")
    (princ)))

(defun conn-narrow-ring-to-register (register)
  "Store narrow ring in REGISTER."
  (interactive (list (register-read-with-preview "Narrow ring to register: ")))
  (set-register register (conn--narrow-ring-to-register)))

(defun conn-push-region-to-narrow-register (beg end register)
  "Prepend region to narrow register."
  (interactive
   (list (region-beginning)
         (region-end)
         (register-read-with-preview "Push region to register: ")))
  (pcase (get-register register)
    ((and (cl-struct conn-narrow-register narrow-ring) struct)
     (setf (conn-narrow-register-narrow-ring struct)
           (cons (cons (conn--create-marker beg)
                       (conn--create-marker end))
                 narrow-ring)))
    (_
     (set-register register (conn--make-narrow-register
                             (list (cons (conn--create-marker beg)
                                         (conn--create-marker end))))))))

(defun conn-push-thing-to-narrow-register ( thing-cmd
                                            thing-arg
                                            register
                                            &optional
                                            subregions-p)
  "Prepend thing regions to narrow register."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim)
             (register-read-with-preview "Push region to register: ")
             & (conn-subregions-argument (use-region-p)))
     :prompt "Thing"))
  (let* ((bounds (conn-bounds-of thing-cmd thing-arg))
         (narrowings
          (if-let* ((_ subregions-p)
                    (subregions (conn-bounds-get bounds :subregions)))
              (cl-loop for bound in subregions
                       for (b . e) = (conn-bounds-get bound :outer)
                       collect (cons (conn--create-marker b)
                                     (conn--create-marker e)))
            (pcase-let ((`(,beg . ,end)
                         (conn-bounds-get bounds :outer)))
              (list (cons (conn--create-marker beg)
                          (conn--create-marker end)))))))
    (pcase (get-register register)
      ((and (cl-struct conn-narrow-register narrow-ring) struct)
       (setf (conn-narrow-register-narrow-ring struct)
             (nconc narrowings narrow-ring)))
      (_
       (set-register register (conn--make-narrow-register narrowings))))))

(defun conn-thing-to-narrow-ring (thing-cmd thing-arg &optional subregions-p)
  "Push thing regions to narrow ring."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim)
             & (conn-subregions-argument (use-region-p)))
     :prompt "Thing"))
  (pcase-let* (((and (conn-bounds-get :outer `(,beg . ,end))
                     (conn-bounds-get :subregions))
                (conn-bounds-of thing-cmd thing-arg)))
    (if (not subregions-p)
        (conn--narrow-ring-record beg end)
      (cl-loop for bound in subregions
               for (b . e) = (conn-bounds-get bound :outer)
               do (conn--narrow-ring-record b e)))))

(defun conn--narrow-ring-record (beg end)
  (let ((narrowing (cons (conn--create-marker beg)
                         (conn--create-marker end))))
    (setq conn-narrow-ring
          (cons narrowing (delete narrowing conn-narrow-ring)))
    (when-let* ((old (drop conn-narrow-ring-max conn-narrow-ring)))
      (cl-callf2 take conn-narrow-ring-max conn-narrow-ring)
      (cl-loop for (beg . end) in old do
               (set-marker beg nil)
               (set-marker end nil)))))

(defun conn-cycle-narrowings (arg)
  "Cycle to the ARGth region in `conn-narrow-ring'."
  (interactive "p")
  (unless conn-narrow-ring
    (user-error "Narrow ring empty"))
  (if (= arg 0)
      (conn-merge-narrow-ring)
    (let (start)
      (unless (ignore-errors
                (and (= (point-min) (caar conn-narrow-ring))
                     (= (point-max) (cdar conn-narrow-ring))))
        (setq start (point)
              arg (+ arg (if (< arg 0) 1 -1))))
      (dotimes (_ (mod arg (length conn-narrow-ring)))
        (setq conn-narrow-ring (nconc (cdr conn-narrow-ring)
                                      (list (car conn-narrow-ring)))))
      (pcase (car conn-narrow-ring)
        (`(,beg . ,end)
         (unless (or (null start)
                     (<= beg start end))
           (push-mark start t))
         (narrow-to-region beg end)
         (goto-char (point-min))
         (conn--push-ephemeral-mark (point-max)))))))

(defun conn-merge-narrow-ring (&optional interactive)
  "Merge overlapping narrowings in `conn-narrow-ring'."
  (interactive (list t))
  (setq conn-narrow-ring (nreverse (conn--merge-overlapping-regions
                                    conn-narrow-ring)))
  (when (and interactive (not executing-kbd-macro))
    (message "Narrow ring merged into %s region"
             (length conn-narrow-ring))))

(defun conn-clear-narrow-ring ()
  "Remove all narrowings from the `conn-narrow-ring'."
  (interactive)
  (cl-loop for (beg . end) in conn-narrow-ring
           do
           (set-marker beg nil)
           (set-marker end nil))
  (setq conn-narrow-ring nil))

(defun conn-pop-narrow-ring ()
  "Pop `conn-narrow-ring'."
  (interactive)
  (pcase (pop conn-narrow-ring)
    ((and `(,beg . ,end)
          (guard (= (point-min) beg))
          (guard (= (point-max) end)))
     (if conn-narrow-ring
         (narrow-to-region (caar conn-narrow-ring)
                           (cdar conn-narrow-ring))
       (widen))
     (set-marker beg nil)
     (set-marker end nil))))

(defun conn-copy-narrow-ring ()
  (setq conn-narrow-ring
        (cl-loop for (beg . end) in conn-narrow-ring
                 collect (cons (copy-marker (marker-position beg) t)
                               (copy-marker (marker-position end))))))


;;;;; Bounds of Narrow Ring

(defun conn--bounds-of-narrowings (_cmd _arg)
  (unless conn-narrow-ring
    (user-error "Narrow ring empty"))
  (cl-loop for (beg . end) in conn-narrow-ring
           minimize beg into narrow-beg
           maximize end into narrow-end
           collect (cons beg end) into narrowings
           finally return (cons (cons narrow-beg narrow-end)
                                narrowings)))

(conn-register-thing-commands
 'narrowing nil
 'narrow-to-region 'widen
 'conn-narrow-to-thing
 'conn-narrow-ring-prefix)


;;;; Commands

(autoload 'kmacro-ring-head "kmacro")

(defun conn-bind-last-kmacro-to-key ()
  "Like `kmacro-bind-to-key' but binds in `conn-get-overriding-map'.

This binding will be inactive during keyboard macro definition and
execution."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (if defining-kbd-macro
          (message "Cannot save macro while defining it."))
    (unless last-kbd-macro
      (error "No keyboard macro defined"))
    (let* ((key-seq (read-key-sequence "Bind last macro to key: "))
           (binding (key-binding key-seq)))
      (when (and (not (equal key-seq "\^G"))
                 (or (not binding)
                     (eq binding 'undefined)
                     (stringp binding)
                     (vectorp binding)
                     (yes-or-no-p (format "%s runs command %S.  Bind anyway? "
                                          (format-kbd-macro key-seq)
                                          binding))))
        (define-key (conn-get-minor-mode-map conn-current-state :bind-last)
                    key-seq
                    `(menu-item
                      "Keyboard Macro"
                      ,(let ((kmacro (kmacro-ring-head)))
                         (lambda (arg) (funcall kmacro arg)))
                      :filter ,(lambda (cmd)
                                 (unless (or executing-kbd-macro
                                             defining-kbd-macro)
                                   cmd))))
        (message "Keyboard macro bound to %s" (format-kbd-macro key-seq))))))

;;;;; Movement

(defun conn-goto-line (line)
  "Goto absolute line, 1 origin.

Respects the current restriction."
  (interactive "p")
  (if (> 0 line)
      (progn
        (goto-char (point-max))
        (cl-incf line))
    (goto-char (point-min))
    (cl-decf line))
  (forward-line line))

(defun conn-forward-defun (N)
  "Move forward by defuns.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(defun conn-backward-symbol (arg)
  "`forward-symbol' in reverse."
  (interactive "p")
  (forward-symbol (- arg)))

(defun conn-scroll-down (&optional arg)
  "`scroll-down-command' leaving point at the same relative window position.

Pulses line that was the first visible line before scrolling."
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (progn (beep) (message "Beginning of buffer"))
    (let ((start (window-start)))
      (scroll-down arg)
      (pulse-momentary-highlight-one-line start))))
(put 'conn-scroll-down 'scroll-command t)

(defun conn-scroll-up (&optional arg)
  "`scroll-up-command' leaving point at the same relative window position.

Pulses line that was the last visible line before scrolling."
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (let ((end (window-end)))
      (scroll-up arg)
      (pulse-momentary-highlight-one-line (1- end)))))
(put 'conn-scroll-up 'scroll-command t)

(defun conn-goto-string-backward (string)
  "Go to the first visible occurrence backward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (let ((conn-target-window-predicate nil)
               (pt (point)))
           (conn--read-string-with-timeout
            (lambda (beg _end) (< (window-start) beg pt))))))
  (let ((case-fold-search (conn--string-no-upper-case-p string)))
    (with-restriction (window-start) (window-end)
      (when-let* ((pos (or (save-excursion
                             (backward-char)
                             (cl-loop while (search-backward string nil t)
                                      when (conn--region-visible-p (match-beginning 0)
                                                                   (match-end 0))
                                      return (match-beginning 0)))
                           (user-error "\"%s\" not found." string))))
        (goto-char pos)))))

(defun conn-goto-string-forward (string)
  "Go to the first visible occurrence forward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (let ((conn-target-window-predicate nil)
               (pt (point)))
           (conn--read-string-with-timeout
            (lambda (beg _end) (< pt beg (window-end)))))))
  (with-restriction (window-start) (window-end)
    (let ((case-fold-search (conn--string-no-upper-case-p string)))
      (when-let* ((pos (or (save-excursion
                             (forward-char)
                             (cl-loop while (search-forward string nil t)
                                      when (conn--region-visible-p (match-beginning 0)
                                                                   (match-end 0))
                                      return (match-beginning 0)))
                           (user-error "\"%s\" not found." string))))
        (goto-char pos)))))

(defun conn-backward-line (N)
  "`forward-line' by N but backward."
  (interactive "p")
  (forward-line (- N)))

(defun conn-backward-whitespace (N)
  "`forward-whitespace' by N but backward."
  (interactive "p")
  (forward-whitespace (- N)))

(defun conn--end-of-inner-line-1 ()
  (let ((end (goto-char (line-end-position))))
    (when-let* ((cs (and (conn--point-in-comment-p)
                         (save-excursion
                           (comment-search-backward
                            (line-beginning-position) t)))))
      (goto-char cs))
    (skip-chars-backward " \t" (line-beginning-position))
    (when (bolp) (goto-char end))))

(defun conn-forward-inner-line (N)
  "Move forward by inner lines.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (interactive "p")
  (if (> N 0)
      (let ((pt (point)))
        (conn--end-of-inner-line-1)
        (unless (= pt (point)) (cl-decf N))
        (cl-loop until (or (>= 0 N)
                           (= (point) (point-max)))
                 do (forward-line 1)
                 unless (eolp)
                 do (cl-decf N))
        (conn--end-of-inner-line-1))
    (cl-callf abs N)
    (let ((pt (point)))
      (back-to-indentation)
      (unless (= pt (point)) (cl-decf N))
      (cl-loop until (or (>= 0 N)
                         (= (point) (point-max)))
               do (forward-line -1)
               unless (eolp)
               do (cl-decf N))
      (back-to-indentation))))

(defun conn-backward-inner-line (N)
  "Inverse of `conn-forward-inner-line'."
  (interactive "p")
  (conn-forward-inner-line (- N)))

(defun conn-end-of-inner-line (&optional N)
  "Move point to after the last non-whitespace or comment character in line.

Immediately repeating this command goes to the point at end
of line proper."
  (interactive "P")
  (if (null N)
      (let ((point (point))
            (mark (mark t)))
        (conn--end-of-inner-line-1)
        (when (and (= point (point))
                   (or (= mark (save-excursion
                                 (back-to-indentation)
                                 (point)))
                       (region-active-p)))
          (goto-char (line-end-position))
          (setq conn-this-command-thing 'outer-line)))
    (forward-line N)
    (setq conn-this-command-handler (conn-command-mark-handler 'forward-line)
          conn-this-command-thing 'line)))

(defun conn-beginning-of-inner-line (&optional N)
  "Move point to the first non-whitespace character in line.

Immediately repeating this command goes to the point at beginning
of line proper."
  (interactive "P")
  (if (null N)
      (let ((point (point))
            (mark (mark t)))
        (back-to-indentation)
        (when (and (= point (point))
                   (or (= mark (save-excursion
                                 (conn--end-of-inner-line-1)
                                 (point)))
                       (region-active-p)))
          (goto-char (line-beginning-position))
          (setq conn-this-command-thing 'outer-line)))
    (forward-line (- N))
    (setq conn-this-command-thing 'line
          conn-this-command-handler (conn-command-mark-handler 'forward-line))))

(defun conn-end-of-list ()
  "Move point to the end of the enclosing list."
  (interactive)
  (up-list 1 t t)
  (down-list -1 t))

(defun conn-beginning-of-list ()
  "Move point to the beginning of the enclosing list."
  (interactive)
  (backward-up-list nil t t)
  (down-list 1 t))


;;;;; Replace

(defvar conn-query-flag nil
  "Default value for conn-query-flag.

If flag is t then `conn-replace' and `conn-regexp-replace'
will query before replacing from-string, otherwise just replace all
instances of from-string.")

(defvar-keymap conn-replace-from-map
  "C-M-;" 'conn-replace-insert-separator)

(defvar-keymap conn-replace-to-map
  "C-RET" 'conn-query-replace
  "C-<return>" 'conn-query-replace)

(defun conn-query-replace ()
  "Invert value of `conn-query-flag' and exit minibuffer."
  (interactive)
  (setq conn-query-flag (not conn-query-flag))
  (exit-minibuffer))

(defun conn-replace-insert-separator ()
  "Insert `query-replace-from-to-separator'."
  (interactive)
  (when query-replace-from-to-separator
    (let ((separator-string
           (when query-replace-from-to-separator
             (if (char-displayable-p
                  (string-to-char (string-replace
                                   " " "" query-replace-from-to-separator)))
                 query-replace-from-to-separator
               " -> "))))
      (insert (propertize separator-string
                          'display separator-string
                          'face 'minibuffer-prompt
                          'separator t)))))

(defun conn-replace-read-default ()
  (let ((beg (region-beginning))
        (end (region-end)))
    (when (and (< (- end beg) 60)
               (<= end (save-excursion
                         (goto-char beg)
                         (pos-eol)))
               (not (use-region-p)))
      (buffer-substring-no-properties beg end))))

(defun conn-replace-read-regexp-default ()
  (when-let* ((default (conn-replace-read-default)))
    (regexp-quote default)))

(defun conn--replace-read-from ( prompt regions
                                 &optional regexp-flag delimited-flag)
  (minibuffer-with-setup-hook
      (minibuffer-lazy-highlight-setup
       :case-fold case-fold-search
       :filter (lambda (mb me)
                 (cl-loop for (beg . end) in regions
                          when (<= beg mb me end) return t))
       :highlight query-replace-lazy-highlight
       :regexp regexp-flag
       :regexp-function (or replace-regexp-function
                            delimited-flag
                            (and replace-char-fold
                                 (not regexp-flag)
                                 #'char-fold-to-regexp))
       :transform (lambda (string)
                    (let* ((split (query-replace--split-string string))
                           (from-string (if (consp split) (car split) split)))
                      (when (and case-fold-search search-upper-case)
                        (setq isearch-case-fold-search
                              (isearch-no-upper-case-p from-string regexp-flag)))
                      from-string)))
    (minibuffer-with-setup-hook
        (lambda ()
          (thread-last
            (current-local-map)
            (make-composed-keymap conn-replace-from-map)
            (use-local-map)))
      (if regexp-flag
          (let ((query-replace-read-from-regexp-default
                 (if-let* ((def (conn-replace-read-regexp-default)))
                     def
                   query-replace-read-from-regexp-default)))
            (query-replace-read-from prompt regexp-flag))
        (query-replace-read-from prompt regexp-flag)))))

(defun conn--replace-read-args ( prompt regexp-flag regions
                                 &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (conn--with-region-emphasis regions
    (save-mark-and-excursion
      (let* ((conn-query-flag conn-query-flag)
             (delimited-flag (and current-prefix-arg
                                  (not (eq current-prefix-arg '-))))
             (from (conn--replace-read-from prompt
                                            regions
                                            regexp-flag
                                            delimited-flag))
             (to (if (consp from)
                     (prog1 (cdr from) (setq from (car from)))
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (thread-last
                           (current-local-map)
                           (make-composed-keymap conn-replace-to-map)
                           (use-local-map)))
                     (query-replace-read-to from prompt regexp-flag)))))
        (list from to
              (or delimited-flag
                  (and (plist-member (text-properties-at 0 from)
                                     'isearch-regexp-function)
                       (get-text-property 0 'isearch-regexp-function from)))
              (and current-prefix-arg (eq current-prefix-arg '-))
              conn-query-flag)))))

(defun conn-replace ( thing-mover arg from-string to-string
                      &optional delimited backward query-flag subregions-p)
  "Perform a `replace-string' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg ,subregions-p)
                 (conn-eval-with-state 'conn-read-thing-state
                     (list && (conn-thing-argument-dwim t)
                           & (conn-subregions-argument (use-region-p)))
                   :prompt "Replace in Thing"))
                (bounds (conn-bounds-of thing-mover arg))
                (subregions (and subregions-p
                                 (conn-bounds-get bounds :subregions)))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  nil (if (and subregions-p subregions)
                          (cl-loop for bound in subregions
                                   collect (conn-bounds-get bound :outer))
                        (list (conn-bounds-get bounds :outer))))))
     (append (list thing-mover arg) common
             (list (and subregions-p subregions t)))))
  (pcase-let* (((and (conn-bounds-get :outer `(,beg . ,end))
                     bounds)
                (or (conn-bounds-of 'conn-bounds-of nil)
                    (conn-bounds-of thing-mover arg))))
    (deactivate-mark t)
    (save-excursion
      (if-let* ((subregions (and subregions-p
                                 (conn-bounds-get bounds :subregions))))
          (let* ((regions
                  (conn--merge-overlapping-regions
                   (cl-loop for bound in subregions
                            collect (conn-bounds-get bound :outer))
                   t))
                 (region-extract-function
                  (lambda (method)
                    (pcase method
                      ('nil
                       (cl-loop for (beg . end) in regions
                                collect (buffer-substring beg end)))
                      ('delete-only
                       (cl-loop for (beg . end) in regions
                                do (delete-region beg end)))
                      ('bounds regions)
                      (_
                       (prog1
                           (cl-loop for (beg . end) in regions
                                    collect (filter-buffer-substring beg end method))
                         (cl-loop for (beg . end) in regions
                                  do (delete-region beg end))))))))
            (perform-replace from-string to-string query-flag nil
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string query-flag nil
                         delimited nil nil beg end backward)))))

(defun conn-regexp-replace ( thing-mover arg from-string to-string
                             &optional delimited backward query-flag subregions-p)
  "Perform a `regexp-replace' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg ,subregions-p)
                 (conn-eval-with-state 'conn-read-thing-state
                     (list && (conn-thing-argument-dwim t)
                           & (conn-subregions-argument t))
                   :prompt "Replace Regexp in Thing"))
                (bounds (conn-bounds-of thing-mover arg))
                (subregions (and subregions-p
                                 (conn-bounds-get bounds :subregions)))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-)
                                  " backward"
                                " word")
                            ""))
                  t (if (and subregions-p subregions)
                        (cl-loop for bound in subregions
                                 collect (conn-bounds-get bound :outer))
                      (list (conn-bounds-get bounds :outer))))))
     (append (list thing-mover arg) common
             (list (and subregions-p subregions t)))))
  (pcase-let* (((and (conn-bounds-get :outer `(,beg . ,end))
                     bounds)
                (or (conn-bounds-of 'conn-bounds-of nil)
                    (conn-bounds-of thing-mover arg))))
    (deactivate-mark t)
    (save-excursion
      (if-let* ((subregions (and subregions-p
                                 (conn-bounds-get bounds :subregions))))
          (let* ((regions
                  (conn--merge-overlapping-regions
                   (cl-loop for bound in subregions
                            collect (conn-bounds-get bound :outer))
                   t))
                 (region-extract-function
                  (lambda (method)
                    (pcase method
                      ('nil
                       (cl-loop for (beg . end) in regions
                                collect (buffer-substring beg end)))
                      ('delete-only
                       (cl-loop for (beg . end) in regions
                                do (delete-region beg end)))
                      ('bounds regions)
                      (_
                       (prog1
                           (cl-loop for (beg . end) in regions
                                    collect (filter-buffer-substring beg end method))
                         (cl-loop for (beg . end) in regions
                                  do (delete-region beg end))))))))
            (perform-replace from-string to-string query-flag t
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string query-flag t
                         delimited nil nil beg end backward)))))


;;;;; Command Registers

(cl-defstruct (conn-command-register)
  (command nil :read-only t))

(cl-defmethod register-val-jump-to ((val conn-command-register) _arg)
  (let ((cmd (conn-command-register-command val)))
    (apply #'funcall-interactively
           (car cmd)
           (mapcar (lambda (e) (eval e t)) (cdr cmd)))))

(cl-defmethod register-val-describe ((val conn-command-register) _arg)
  (princ (format "Command:  %s"
                 (car (conn-command-register-command val)))))

(defun conn-command-to-register (register)
  "Store command in REGISTER."
  (interactive
   (list (register-read-with-preview "Command to register: ")))
  (set-register
   register
   (make-conn-command-register
    :command (let* ((print-level nil)
                    (cmds (cdr (mapcar #'prin1-to-string command-history))))
               (completing-read
                "Command: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function . ,#'identity))
                    (complete-with-action action cmds string pred)))
                nil t)))))


;;;;; Tab Registers

(cl-defstruct (conn-tab-register
               (:constructor nil)
               ( :constructor conn--make-tab-register
                 (&aux
                  (cookie
                   (with-memoization
                       (alist-get 'conn-tab-cookie
                                  (thread-first
                                    (funcall tab-bar-tabs-function)
                                    tab-bar--current-tab-find cdr))
                     (gensym "conn-tab-cookie")))
                  (frame (selected-frame)))))
  (cookie nil :read-only t)
  (frame nil :read-only t))

(defun conn--get-tab-index-by-cookie (cookie)
  (declare (important-return-value t))
  (seq-position (funcall tab-bar-tabs-function)
                cookie
                (lambda (tab c)
                  (eq c (alist-get 'conn-tab-cookie tab)))))

(cl-defmethod register-val-jump-to ((val conn-tab-register) _arg)
  (when-let* ((frame (conn-tab-register-frame val))
              (index (and (frame-live-p frame)
                          (with-selected-frame (conn-tab-register-frame val)
                            (conn--get-tab-index-by-cookie
                             (conn-tab-register-cookie val))))))
    (select-frame-set-input-focus frame)
    (tab-bar-select-tab (1+ index))))

(cl-defmethod register-val-describe ((val conn-tab-register) _arg)
  (princ (format "Tab:  %s"
                 (if (eq (selected-frame) (conn-tab-register-frame val))
                     (when-let* ((index (conn--get-tab-index-by-cookie
                                         (conn-tab-register-cookie val)))
                                 (tab (nth index (funcall tab-bar-tabs-function))))
                       (if (eq (car tab) 'current-tab)
                           (propertize "*CURRENT TAB*" 'face 'error)
                         (alist-get 'name tab)))
                   "on another frame"))))

(defun conn-tab-to-register (register)
  "Store tab in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-tab-register)))


;;;;; Isearch Commands

(defun conn-isearch-dispatch-region ()
  (interactive)
  (isearch-done)
  (conn-dispatch))

(defun conn-isearch-yank-region ()
  "Yank the current region to isearch."
  (interactive)
  (isearch-yank-internal (lambda () (mark t))))

(defun conn-isearch-open-recursive-edit ()
  "Open a recursive edit from within an isearch.

Exiting the recursive edit will resume the isearch."
  (interactive)
  (save-selected-window
    (with-isearch-suspended
     (recursive-edit))))

(cl-defun conn--isearch-in-thing (thing-cmd thing-arg &key backward regexp subregions-p)
  (pcase-let* ((bounds
                (conn-bounds-of thing-cmd thing-arg))
               (regions
                (mapcar (pcase-lambda (`(,beg . ,end))
                          (cons (conn--create-marker beg)
                                (conn--create-marker end nil t)))
                        (or (if (and subregions-p (conn-bounds-get bounds :subregions))
                                (conn--merge-overlapping-regions
                                 (cl-loop for bound in (conn-bounds-get bounds :subregions)
                                          collect (conn-bounds-get bound :outer))
                                 t)
                              (list (conn-bounds-get bounds :outer))))))
               (depth (recursion-depth))
               (in-regions-p (lambda (beg end)
                               (or (/= depth (recursion-depth))
                                   (cl-loop for (nbeg . nend) in regions
                                            thereis (<= nbeg beg end nend)))))
               (thing (upcase (symbol-name (or (conn-command-thing thing-cmd)
                                               thing-cmd))))
               (prefix (concat "[in " thing "] ")))
    (letrec ((setup
              (lambda ()
                (when (= depth (recursion-depth))
                  (add-function :after-while (local 'isearch-filter-predicate)
                                in-regions-p `((isearch-message-prefix . ,prefix)))
                  (remove-hook 'isearch-mode-hook setup t))))
             (cleanup
              (lambda ()
                (if (and (= depth (recursion-depth))
                         (not isearch-suspended))
                    (remove-hook 'isearch-mode-end-hook cleanup t)
                  (add-hook 'isearch-mode-hook setup nil t))
                (remove-function (local 'isearch-filter-predicate)
                                 in-regions-p))))
      (add-hook 'isearch-mode-end-hook cleanup nil t))
    (add-function :after-while (local 'isearch-filter-predicate) in-regions-p
                  `((isearch-message-prefix . ,prefix)))
    (if backward
        (isearch-backward regexp t)
      (isearch-forward regexp t))))

(defun conn-isearch-forward (thing-cmd thing-arg &optional regexp subregions-p)
  "Isearch forward within the bounds of a thing."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t)
             & current-prefix-arg
             & (conn-subregions-argument (use-region-p)))
     :prompt "Isearch in Thing"))
  (conn--isearch-in-thing thing-cmd thing-arg
                          :backward nil
                          :regexp regexp
                          :subregions-p subregions-p))

(defun conn-isearch-backward (thing-cmd thing-arg &optional regexp subregions-p)
  "Isearch backward within the bounds of a thing."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t)
             & current-prefix-arg
             & (conn-subregions-argument (use-region-p)))
     :prompt "Isearch in Thing"))
  (conn--isearch-in-thing thing-cmd thing-arg
                          :backward t
                          :regexp regexp
                          :subregions-p subregions-p))

(defun conn-isearch-region-forward (thing-cmd thing-arg &optional regexp)
  "Isearch forward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument t)
             & current-prefix-arg)
     :prompt "Thing"))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg
                            :backward nil
                            :regexp regexp)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-isearch-region-backward (thing-cmd thing-arg &optional regexp)
  "Isearch backward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument t)
             & current-prefix-arg)
     :prompt "Thing"))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg
                            :backward t
                            :regexp regexp)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-multi-isearch-project ()
  "Perform a `multi-isearch' within the files of a project."
  (interactive)
  (require 'project)
  (multi-isearch-files
   (seq-uniq (cons (buffer-file-name)
                   (project-files (project-current)))
             'file-equal-p)))

(defun conn-isearch-exit-and-mark ()
  "`isearch-exit' and set region to match."
  (interactive)
  (isearch-done)
  (conn--push-ephemeral-mark isearch-other-end))

(defun conn-isearch-exit-other-end ()
  (interactive)
  (if isearch-forward
      (isearch-repeat-backward)
    (isearch-repeat-forward))
  (isearch-done))


;;;;; Mark Commands

(defun conn-rectangle-mark ()
  "Toggle `rectangle-mark-mode'."
  (interactive)
  (if (region-active-p)
      (rectangle-mark-mode 'toggle)
    (activate-mark)
    (rectangle-mark-mode)))

(defun conn-toggle-mark-command (&optional arg)
  "Toggle `mark-active'.

With a prefix ARG activate `rectangle-mark-mode'."
  (interactive "P")
  (cond (arg (conn-rectangle-mark))
        (mark-active (deactivate-mark))
        (t (activate-mark))))

(defun conn-set-mark-command (&optional arg)
  "Toggle `mark-active' and push ephemeral mark at point.

With a prefix ARG activate `rectangle-mark-mode'.
Immediately repeating this command pushes a mark."
  (interactive "P")
  (cond (arg
         (rectangle-mark-mode 'toggle))
        ((eq last-command 'conn-set-mark-command)
         (if (region-active-p)
             (progn
               (push-mark nil t)
               (deactivate-mark)
               (message "Mark pushed and deactivated"))
           (activate-mark)
           (message "Mark activated")))
        (t
         (conn--push-ephemeral-mark)
         (activate-mark))))

(defun conn-exchange-mark-command (&optional arg)
  "`exchange-mark-and-point' avoiding activating the mark.

With a prefix ARG `push-mark' without activating it."
  (interactive "P")
  (cond (arg
         (push-mark (point) t nil)
         (message "Marker pushed"))
        (t
         (exchange-point-and-mark (not mark-active)))))


;;;;;; Mark Ring

(defvar-local conn-mark-ring nil
  "List of interesting former marks of the current buffer, most recent first.

Conn adds many uninteresting marks to the `mark-ring' and so to
ameliorate the problem implements this alternative mark ring which
filters out the uninteresting marks.  See also `conn-pop-mark-ring' and
`conn-unpop-mark-ring'.")

(defvar conn-mark-ring-max 40
  "Maximum length of `conn-mark-ring'.")

(defun conn-copy-mark-ring ()
  (when (conn-ring-p conn-mark-ring)
    (let ((new-ring (copy-conn-ring conn-mark-ring)))
      (setf (conn-ring-list new-ring)
            (cl-loop for mk in (conn-ring-list new-ring)
                     collect (copy-marker (marker-position mk)))
            (conn-ring-history new-ring)
            (cl-loop with old-list = (conn-ring-list conn-mark-ring)
                     with new-list = (conn-ring-list new-ring)
                     for elem in (conn-ring-history new-ring)
                     for pos = (seq-position old-list elem)
                     when pos collect (nth pos new-list))
            conn-mark-ring new-ring))))

(defun conn-delete-mark-ring ()
  (when (conn-ring-p conn-mark-ring)
    (mapc (conn-ring-cleanup conn-mark-ring)
          (conn-ring-list conn-mark-ring))
    (setf conn-mark-ring nil)))

(defun conn--push-mark-ring (location &optional back)
  (when (not conn-mark-ring)
    (setq conn-mark-ring
          (conn-make-ring conn-mark-ring-max
                          :cleanup (lambda (mk) (set-marker mk nil)))))
  (pcase-let ((ptb (conn-ring-tail conn-mark-ring))
              (ptf (conn-ring-head conn-mark-ring)))
    (cond
     ((and ptf (= location ptf))
      (when back (conn-ring-rotate-forward conn-mark-ring)))
     ((and ptb (= location ptb))
      (unless back (conn-ring-rotate-backward conn-mark-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-mark-ring
                                 (conn--create-marker location))
        (conn-ring-insert-front conn-mark-ring
                                (conn--create-marker location)))))))

(defun conn-pop-mark-ring ()
  "Like `pop-to-mark-command' but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-forward conn-mark-ring)
    (goto-char (conn-ring-head conn-mark-ring)))
  (deactivate-mark))

(defun conn-unpop-mark-ring ()
  "Like `pop-to-mark-command' in reverse but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-backward conn-mark-ring)
    (goto-char (conn-ring-head conn-mark-ring)))
  (deactivate-mark))


;;;;;; Movement Ring

(defvar-local conn-movement-ring nil
  "List of previous regions, most recent first.

See also `conn-pop-movement-ring' and `conn-unpop-movement-ring'.")

(defvar conn-movement-ring-max 10
  "Maximum length of `conn-movement-ring'.")

(defun conn-copy-movement-ring ()
  (when (conn-ring-p conn-movement-ring)
    (let ((new-ring (copy-conn-ring conn-movement-ring)))
      (setf (conn-ring-list new-ring)
            (cl-loop for (pt . mk) in (conn-ring-list new-ring)
                     collect (cons (copy-marker (marker-position pt) t)
                                   (copy-marker (marker-position mk))))
            (conn-ring-history new-ring)
            (cl-loop with old-list = (conn-ring-list conn-movement-ring)
                     with new-list = (conn-ring-list new-ring)
                     for elem in (conn-ring-history conn-movement-ring)
                     for pos = (seq-position old-list elem)
                     when pos collect (nth pos new-list))
            conn-movement-ring new-ring))))

(defun conn-push-region (point mark &optional back)
  (unless (conn-ring-p conn-movement-ring)
    (setq conn-movement-ring
          (conn-make-ring conn-movement-ring-max
                          :cleanup (pcase-lambda (`(,pt . ,mk))
                                     (set-marker pt nil)
                                     (set-marker mk nil)))))
  (pcase-let ((`(,ptf . ,mkf) (conn-ring-head conn-movement-ring))
              (`(,ptb . ,mkb) (conn-ring-tail conn-movement-ring)))
    (cond
     ((and ptf (= point ptf) (= mark mkf))
      (when back (conn-ring-rotate-backward conn-movement-ring)))
     ((and ptb (= point ptb) (= mark mkb))
      (unless back (conn-ring-rotate-forward conn-movement-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-movement-ring
                                 ;; TODO: Think through the marker
                                 ;; insertion type
                                 (cons (conn--create-marker point nil t)
                                       (conn--create-marker mark)))
        (conn-ring-insert-front conn-movement-ring
                                (cons (conn--create-marker point nil t)
                                      (conn--create-marker mark))))))))

(defun conn-unpop-movement-ring (arg)
  "Rotate backward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-pop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-region (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-backward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

(defun conn-pop-movement-ring (arg)
  "Rotate forward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-unpop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-region (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-forward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))


;;;;; Transpose

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE"
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-transpose-command)))

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "i" 'conn-backward-line
  "k" 'forward-line
  "u" 'forward-symbol
  "f" 'conn-dispatch)

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-mover-state))

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "TAB" 'repeat-dispatch
  "C-n" 'restrict-windows
  "SPC" 'scroll-up
  "DEL" 'scroll-down
  "C-o" 'other-window)

(defun conn--transpose-recursive-message ()
  (message
   (substitute-command-keys
    (concat
     "Define region. "
     "Press \\[exit-recursive-edit] to end and use current region."
     "Press \\[abort-recursive-edit] to abort."))))

(defvar conn--transpose-eldoc-prev-msg-fn)

(define-minor-mode conn-transpose-recursive-edit-mode
  "Find a region to transpose in a recursive edit."
  :global t
  :group 'conn
  (if conn-transpose-recursive-edit-mode
      (progn
        (setq conn--transpose-eldoc-prev-msg-fn eldoc-message-function
              eldoc-message-function #'ignore)
        (add-hook 'post-command-hook 'conn--transpose-recursive-message))
    (setq eldoc-message-function conn--transpose-eldoc-prev-msg-fn
          conn--transpose-eldoc-prev-msg-fn nil)
    (remove-hook 'post-command-hook 'conn--transpose-recursive-message)))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state
                                   'conn-transpose-recursive-edit-mode)
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(oclosure-define (conn-transpose-command
                  (:parent conn-dispatch-transpose))
  (buffer :type buffer)
  (point :type marker)
  (thing :type function))

(cl-defmethod conn-perform-dispatch ((action conn-transpose-command)
                                     thing
                                     thing-arg
                                     &key &allow-other-keys)
  (conn-perform-dispatch-loop nil
    (pcase-let* ((`(,pt ,win ,thing-override)
                  (save-mark-and-excursion
                    (conn-dispatch-select-target))))
      (funcall action win pt
               (or thing-override thing)
               thing-arg))))

(cl-defgeneric conn-perform-transpose (cmd arg))

(cl-defmethod conn-perform-transpose (cmd arg)
  (pcase cmd
    ((let 0 arg)
     (deactivate-mark t)
     (pcase-let* ((thing (conn-command-thing cmd))
                  (`(,beg1 . ,end1) (if (region-active-p)
                                        (cons (region-beginning) (region-end))
                                      (bounds-of-thing-at-point thing)))
                  (`(,beg2 . ,end2) (save-excursion
                                      (goto-char (mark t))
                                      (bounds-of-thing-at-point thing))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((let (and thing (pred identity))
       (conn-command-thing cmd))
     (deactivate-mark t)
     (transpose-subr (lambda (N) (forward-thing thing N))
                     (prefix-numeric-value arg)))
    (_ (error "Invalid transpose mover"))))

(cl-defmethod conn-perform-transpose ((_cmd (conn-thing recursive-edit)) _arg)
  (deactivate-mark t)
  (let ((bounds1 (region-bounds))
        (buf (current-buffer)))
    (conn-transpose-recursive-edit-mode 1)
    (unwind-protect
        (conn-with-recursive-stack 'conn-bounds-of-recursive-edit-state
          (recursive-edit))
      (conn-transpose-recursive-edit-mode -1))
    (conn--dispatch-transpose-subr
     buf (caar bounds1) (lambda (_) bounds1)
     (current-buffer) (point) (let ((bounds2 (region-bounds)))
                                (lambda (_) bounds2))
     nil)))

(cl-defmethod conn-perform-transpose ((_cmd (conn-thing dispatch)) arg)
  (while
      (condition-case err
          (progn
            (conn-eval-with-state 'conn-dispatch-transpose-state
                (conn-perform-dispatch
                 & (oclosure-lambda
                       (conn-transpose-command
                        (description "Transpose")
                        (no-history t)
                        (buffer (current-buffer))
                        (point (point))
                        (thing (and (use-region-p) 'region))
                        (window-predicate
                         (lambda (win)
                           (not (buffer-local-value 'buffer-read-only
                                                    (window-buffer win))))))
                       (window2 pt2 thing2 thing-arg)
                     (conn--dispatch-transpose-subr
                      buffer point (or thing thing2)
                      (window-buffer window2) pt2 thing2
                      thing-arg))
                 && (oclosure-lambda (conn-thing-argument
                                      (required t)
                                      (recursive-edit t))
                        (self cmd)
                      (pcase cmd
                        ((or 'conn-expand 'conn-contract))
                        (_
                         (conn-handle-thing-argument cmd self))))
                 :other-end :no-other-end
                 :restrict-windows & (conn-dispatch-restrict-windows-argument))
              :prompt "Transpose Dispatch"
              :prefix arg)
            nil)
        ;; TODO: make this display somehow
        (user-error (message "%s" (cadr err)) t))))

(defun conn-transpose-regions (mover arg)
  "Exchange regions defined by a thing command.

With argument ARG 0, exchange the things at point and mark.

If MOVER is \\='recursive-edit then exchange the current region and the
region after a `recursive-edit'."
  (interactive
   (conn-eval-with-state 'conn-transpose-state
       (list && (conn-thing-argument-dwim t))
     :prompt "Transpose"
     :prefix current-prefix-arg))
  (when conn-transpose-recursive-edit-mode
    (user-error "Recursive call to conn-transpose-regions"))
  (conn-perform-transpose mover arg))


;;;;; Line Commands

(defun conn-open-line (arg)
  "Open line below the current line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent))

(defun conn-open-line-above (arg)
  "Open line above the current line."
  (interactive "p")
  (forward-line (- (1- arg)))
  (move-beginning-of-line nil)
  (insert "\n")
  (forward-line -1)
  ;; FIXME: see crux smart open line
  (indent-according-to-mode))

(defun conn-open-line-and-indent (N)
  "Insert a newline, leave point before it and indent the new line.
With arg N, insert N newlines."
  (interactive "p")
  (open-line N)
  (indent-according-to-mode)
  (save-excursion
    (dotimes (_ N)
      (forward-line 1)
      (indent-according-to-mode))))

(defun conn-join-lines-in-region (beg end)
  "`delete-indentation' in region from BEG and END."
  (interactive (list (region-beginning)
                     (region-end)))
  (delete-indentation nil beg end)
  (indent-according-to-mode))

(defun conn-join-lines (thing-mover thing-arg &optional subregions-p)
  "`delete-indentation' in region from START and END."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t)
             & (conn-subregions-argument (use-region-p)))
     :prompt "Thing"))
  (save-mark-and-excursion
    (pcase (conn-bounds-of thing-mover thing-arg)
      ((and (guard subregions-p)
            (conn-bounds-get :subregions))
       (pcase-dolist ((conn-bounds-get :outer `(,beg . ,end))
                      subregions)
         (delete-indentation nil beg end)
         (indent-according-to-mode)))
      ((conn-bounds-get :outer `(,beg . ,end))
       (delete-indentation nil beg end)
       (indent-according-to-mode)))))


;;;;; Prepend/Append to Kill/Register

(defun conn-append-region (beg end &optional register kill-flag prepend)
  "Append region from BEG to END to most recent kill.

Optionally if REGISTER is specified append to REGISTER instead.
When called interactively with a non-nil prefix argument read register
interactively.

When KILL-FLAG is non-nil kill the region as well."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Append kill to register: "))))
  (let ((separator (and register-separator (get-register register-separator)))
        (text (filter-buffer-substring beg end kill-flag)))
    (pcase (and register (get-register register))
      ('nil (kill-append (concat separator text) prepend))
      ((and reg (cl-type marker))
       (let ((marker-type (marker-insertion-type reg)))
         (set-marker-insertion-type reg (not prepend))
         (unwind-protect
             (with-current-buffer (marker-buffer reg)
               (save-excursion
                 (goto-char reg)
                 (insert-for-yank (if prepend
                                      (concat text separator)
                                    (concat separator text))))
               (setq deactivate-mark t))
           (set-marker-insertion-type reg marker-type))))
      ((guard prepend)
       (prepend-to-register register beg end kill-flag))
      (_
       (append-to-register register beg end kill-flag))))
  (when (and (null kill-flag)
             (called-interactively-p 'interactive))
    (pulse-momentary-highlight-region beg end)))

(defun conn-prepend-region (beg end &optional register kill-flag)
  "Prepend region from BEG to END to most recent kill.

Optionally if REGISTER is specified prepend to REGISTER instead.
When called interactively with a non-nil prefix argument read register
interactively.

When KILL-FLAG is non-nil kill the region as well."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Prepend to register: "))))
  (conn-append-region beg end register kill-flag t)
  (when (and (null kill-flag)
             (called-interactively-p 'interactive))
    (pulse-momentary-highlight-region beg end)))

(defun conn-kill-append-region (beg end &optional register)
  "Kill current region and append it to the last kill.

With a prefix arg append to a register instead."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Append to register: "))))
  (conn-append-region beg end register t))

(defun conn-kill-prepend-region (beg end &optional register)
  "Kill current region and prepend it to the last kill.

With a prefix arg prepend to a register instead."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Prepend to register: "))))
  (conn-prepend-region beg end register t))

(defun conn-copy-thing (thing-mover arg &optional register trim)
  "Copy THING at point."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim)
             & (conn-subregions-argument (use-region-p))
             (when current-prefix-arg
               (register-read-with-preview "Register: "))
             & (conn-trim-argument))
     :prompt "Thing"))
  (pcase-let (((and (conn-bounds-get :outer)
                    (conn-bounds-get :trimmed))
               (conn-bounds-of thing-mover arg)))
    (if (and trim trimmed)
        (progn
          (conn-copy-region (car trimmed) (cdr trimmed) register)
          (unless executing-kbd-macro
            (pulse-momentary-highlight-region (car trimmed) (cdr trimmed))))
      (conn-copy-region (car outer) (cdr outer) register)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-region (car outer) (cdr outer))))))


;;;;; Narrowing Commands

(defun conn--narrow-to-region-1 (beg end &optional record)
  (narrow-to-region beg end)
  (when record (conn--narrow-ring-record beg end)))

(defun conn-narrow-to-thing (thing-mover arg &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t) t)
     :prompt "Thing"
     :prefix current-prefix-arg))
  (pcase-let (((conn-bounds-get :outer `(,beg . ,end))
               (conn-bounds-of thing-mover arg)))
    (unless (and (<= beg (point) end)
                 (<= beg (mark t) end))
      (deactivate-mark))
    (conn--narrow-to-region-1 beg end record)
    (when (called-interactively-p 'interactive)
      (message "Buffer narrowed"))))

(defun conn-narrow-indirect (thing-mover arg &optional interactive)
  "Narrow to THING at point.

Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t) t)
     :prompt "Thing"
     :prefix current-prefix-arg))
  (pcase-let (((conn-bounds-get :outer `(,beg . ,end))
               (conn-bounds-of thing-mover arg)))
    (conn--narrow-indirect beg end interactive)
    (when (called-interactively-p 'interactive)
      (message "Buffer narrowed indirect"))))

(defun conn-narrow-to-region (beg end &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive (list (region-beginning) (region-end) (list t)))
  (conn--narrow-to-region-1 beg end record)
  (when (called-interactively-p 'interactive)
    (message "Buffer narrowed")))


;;;;; Register Setting and Loading

(defvar conn--separator-history nil
  "History var for `conn-set-register-separator'.")

(defun conn-set-register-separator (string)
  "Set `register-separator' register to string STRING."
  (interactive
   (list (read-string "Separator: "
                      (let ((reg (get-register register-separator)))
                        (when (stringp reg) reg))
                      conn--separator-history nil t)))
  (set-register register-separator string))

;; register-load from consult
(defun conn-register-load (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (list (register-read-with-preview "Load register: ")
         current-prefix-arg))
  (condition-case err
      (jump-to-register reg arg)
    (user-error
     (unless (string-search "access aborted" (error-message-string err))
       (insert-register reg (not arg))))))

(defun conn-register-load-and-replace (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (list (register-read-with-preview "Load register: ")
         current-prefix-arg))
  (atomic-change-group
    (if (bound-and-true-p rectangle-mark-mode)
        (delete-rectangle (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))
    (conn-register-load reg arg)))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))


;;;;; Killing and Yanking Commands

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

(defvar-local conn--minibuffer-initial-region nil)

(defun conn--yank-region-to-minibuffer-hook ()
  (setq conn--minibuffer-initial-region
        (with-minibuffer-selected-window
          (ignore-errors (cons (region-beginning) (region-end))))))

(defun conn-yank-region-to-minibuffer (&optional quote-function)
  "Yank region from `minibuffer-selected-window' into minibuffer."
  (interactive
   (list (if current-prefix-arg
             (if conn-completion-region-quote-function
                 (pcase (car (read-multiple-choice
                              "Quote:"
                              '((?r "regexp-quote")
                                (?c "conn-completion-region-quote-function"))))
                   (?r 'regexp-quote)
                   (?c conn-completion-region-quote-function))
               'regexp-quote)
           'identity)))
  (insert-for-yank
   (pcase conn--minibuffer-initial-region
     (`(,beg . ,end)
      (with-minibuffer-selected-window
        (funcall (or quote-function 'identity)
                 (filter-buffer-substring beg end))))
     (_ (user-error "No region in buffer")))))

(defun conn-yank-replace (start end &optional kill-region)
  "`yank' replacing region between START and END.

If called interactively uses the region between point and mark.
If arg is non-nil, kill the region between START and END instead
of deleting it."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (atomic-change-group
    (conn--without-conn-maps
      (if kill-region
          (let ((str (filter-buffer-substring start end t)))
            (funcall (keymap-lookup nil conn-yank-keys t))
            (kill-new str))
        (funcall (or (keymap-lookup nil conn-delete-region-keys t)
                     'delete-region)
                 start end)
        (funcall (keymap-lookup nil conn-yank-keys t))))))

(defun conn-copy-region (start end &optional register)
  "Copy region between START and END as kill.

If REGISTER is given copy to REGISTER instead."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Copy to register: "))))
  (if register
      (if (bound-and-true-p rectangle-mark-mode)
          (copy-rectangle-to-register register start end)
        (copy-to-register register start end)
        (when (called-interactively-p 'interactive)
          (pulse-momentary-highlight-region start end)))
    (if (bound-and-true-p rectangle-mark-mode)
        (copy-rectangle-as-kill start end)
      (copy-region-as-kill start end)
      (when (called-interactively-p 'interactive)
        (pulse-momentary-highlight-region start end)))))

(defun conn-kill-region (&optional arg)
  "Kill region between START and END.

If START and END are equal delete char backward.

If ARG is an ordinary prefix argument (\\[universal-argument]) delete
the region instead of killing it.

If ARG is a numeric prefix argument kill region to a register."
  (interactive (list current-prefix-arg))
  (cond ((= (point) (mark t))
         (call-interactively (conn--without-conn-maps
                               (keymap-lookup nil conn-backward-delete-char-keys t))))
        ((numberp arg)
         (thread-first
           (concat "Kill "
                   (if (bound-and-true-p rectangle-mark-mode)
                       "Rectangle "
                     "")
                   "to register:")
           (register-read-with-preview)
           (copy-to-register nil nil t t)))
        ((bound-and-true-p rectangle-mark-mode)
         (kill-rectangle (region-beginning) (region-end)))
        (t (call-interactively
            (conn--without-conn-maps
              (keymap-lookup nil conn-kill-region-keys t))))))

(defun conn-completing-yank-replace (start end &optional arg)
  "Replace region from START to END with result of `yank-from-kill-ring'.

If ARG is non-nil `kill-region' instead of `delete-region'."
  (interactive
   (list (region-beginning)
         (region-end)
         current-prefix-arg))
  (let ((ov (make-overlay start end))
        exchange)
    (overlay-put ov 'conn-overlay t)
    (unwind-protect
        (progn
          (when (setq exchange (= (point) start))
            (exchange-point-and-mark (not mark-active)))
          (overlay-put ov 'invisible t)
          (call-interactively (or (command-remapping 'yank-from-kill-ring)
                                  'yank-from-kill-ring))
          (if arg
              (kill-region (overlay-start ov) (overlay-end ov))
            (delete-region (overlay-start ov) (overlay-end ov))))
      (when exchange
        (exchange-point-and-mark (not mark-active)))
      (delete-overlay ov))))

(defun conn-yank-replace-rectangle ()
  "Delete the current rectangle and `yank-rectangle'."
  (interactive)
  (save-mark-and-excursion
    (unless (>= (mark t) (point))
      (conn-exchange-mark-command))
    (delete-rectangle (region-beginning) (region-end))
    (yank-rectangle)))


;;;;; Duplicate Commands

(defun conn--duplicate-region-1 (beg end)
  (let* ((region (buffer-substring-no-properties beg end))
         (multiline (seq-contains-p region ?\n))
         (padding (if multiline "\n" " "))
         (regexp (if multiline "\n" "[\t ]")))
    (goto-char end)
    (unless (save-excursion
              (or (looking-back regexp 1)
                  (progn
                    (goto-char beg)
                    (looking-at regexp))))
      (insert padding)
      (cl-incf end))
    (insert region)
    (goto-char end)
    (conn--push-ephemeral-mark (+ (point) (length region)))))

(defun conn-duplicate-region (beg end N)
  "Duplicate the current region.

With prefix arg N duplicate region N times."
  (interactive
   (list (region-beginning)
         (region-end)
         (prefix-numeric-value current-prefix-arg)))
  (if (use-region-p)
      (duplicate-dwim)
    (let ((end (set-marker (make-marker) end)))
      (unwind-protect
          (dotimes (_ N)
            (conn--duplicate-region-1 beg end))
        (goto-char end)
        (set-marker end nil)
        (indent-region (region-beginning) (region-end))))))

(defun conn-duplicate (thing-mover thing-arg N)
  "Duplicate the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t)
             & (prefix-numeric-value current-prefix-arg))
     :prompt "Thing"))
  (pcase (conn-bounds-of thing-mover thing-arg)
    ((conn-bounds-get :outer `(,beg . ,end))
     (if (use-region-p)
         (duplicate-dwim)
       (let ((end (set-marker (make-marker) end))
             (len (- end beg))
             (start (point))
             (mark (mark t)))
         (unwind-protect
             (dotimes (_ N)
               (conn--duplicate-region-1 beg end))
           (goto-char (+ start (* len N)))
           (conn--push-ephemeral-mark (+ mark (* len N)))
           (set-marker end nil)))))))

(defun conn-duplicate-and-comment-region (beg end &optional arg)
  "Duplicate and comment the current region."
  (interactive
   (list (region-beginning)
         (region-end)
         (prefix-numeric-value current-prefix-arg)))
  (pcase-let* ((origin (point))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (pos-eol))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun conn-duplicate-and-comment (thing-mover thing-arg N)
  "Duplicate and comment the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t)
             & (prefix-numeric-value current-prefix-arg))
     :prompt "Thing"))
  (pcase (conn-bounds-of thing-mover thing-arg)
    ((and (conn-bounds-get :outer `(,beg . ,end))
          (let offset (- (point) end))
          (let mark-offset (- (point) (mark t)))
          (let region (buffer-substring-no-properties beg end)))
     (goto-char end)
     (comment-or-uncomment-region beg end)
     (setq end (if (bolp) (point) (pos-eol)))
     (dotimes (_ N)
       (goto-char end)
       (newline)
       (insert region)
       (setq end (point)))
     (goto-char (+ (point) offset))
     (conn--push-ephemeral-mark (- (point) mark-offset)))))


;;;;; Recenter

(defcustom conn-recenter-pulse t
  "Momentarily highlight region after `conn-recenter-on-region'."
  :group 'conn
  :type 'boolean)

(defvar conn-recenter-positions
  (list 'center 'top 'bottom)
  "Cycle order for `conn-recenter-on-region'.")

(defun conn-recenter-on-region ()
  "Recenter the screen on the current region.

Repeated invocations scroll the window according to the ordering
of `conn-recenter-positions'."
  (interactive)
  (if (eq this-command last-command)
      (put this-command :conn-positions
           (let ((ps (conn--command-property :conn-positions)))
             (append (cdr ps) (list (car ps)))))
    (put this-command :conn-positions conn-recenter-positions))
  (let ((beg (region-beginning))
        (end (region-end)))
    (pcase (car (conn--command-property :conn-positions))
      ('center
       (save-excursion
         (forward-line
          (if (> (point) (mark t))
              (- (/ (count-lines beg end) 2))
            (/ (count-lines beg end) 2)))
         (recenter))
       (when (not (pos-visible-in-window-p (point)))
         (if (> (point) (mark t))
             (recenter -1)
           (recenter 0))))
      ('top
       (save-excursion
         (goto-char beg)
         (recenter 0)))
      ('bottom
       (save-excursion
         (goto-char end)
         (recenter -1))))
    (when (and conn-recenter-pulse
               (not (region-active-p)))
      (pulse-momentary-highlight-region beg end))))

(defun conn-recenter-on-region-other-window ()
  "Recenter the current region in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (conn-recenter-on-region)))


;;;;; Misc Commands

(defun conn-comment-or-uncomment (thing-mover arg)
  "Toggle commenting of a region defined by a thing command."
  (interactive
   (conn-eval-with-state 'conn-read-thing-state
       (list && (conn-thing-argument-dwim t))
     :prompt "Thing"))
  (pcase-let (((conn-bounds-get :outer `(,beg . ,end))
               (conn-bounds-of thing-mover arg)))
    (if (comment-only-p beg end)
        (uncomment-region beg end)
      (let ((comment-empty-lines t))
        (comment-region beg end)))))

(defun conn-outline-insert-heading ()
  (interactive)
  (conn-with-recursive-stack 'conn-emacs-state
    (save-mark-and-excursion
      (save-current-buffer
        (outline-insert-heading)
        (recursive-edit)))))

(defun conn-shell-command-on-region (&optional arg)
  "Like `shell-command-on-region' but inverts the meaning of ARG."
  (interactive "P")
  (let ((current-prefix-arg (not arg)))
    (call-interactively 'shell-command-on-region)))

(defun conn-rgrep-region (beg end)
  "`rgrep' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (list (region-beginning)
         (region-end)))
  (let ((search-string
         (read-string "Search for: "
                      (regexp-quote (buffer-substring-no-properties beg end))
                      'grep-regexp-history)))
    (rgrep search-string)))

(defun conn-occur-region (beg end)
  "`occur' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (list (region-beginning)
         (region-end)))
  (let ((search-string
         (read-string "Search for: "
                      (regexp-quote (buffer-substring-no-properties beg end))
                      'grep-regexp-history)))
    (occur search-string)))


;;;;; Surround

(defface conn-read-surround-with-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(conn-define-state conn-surround-thing-state (conn-read-thing-state)
  :lighter "SURROUND")

(conn-define-state conn-surround-with-state (conn-mode-line-face-state)
  :lighter "WITH"
  :mode-line-face 'conn-read-surround-with-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-surround-with-state)
  "0" 'digit-argument
  "1" 'digit-argument
  "2" 'digit-argument
  "3" 'digit-argument
  "4" 'digit-argument
  "5" 'digit-argument
  "6" 'digit-argument
  "7" 'digit-argument
  "8" 'digit-argument
  "9" 'digit-argument
  "r" 'conn-read-pair
  "c" 'surround-comment
  "C" 'surround-uncomment
  "<remap> <self-insert-command>" 'surround-self-insert
  "C-q <t>" 'surround-self-insert
  "RET" 'conn-padding-flag
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(put 'conn-surround-overlay 'face 'region)
(put 'conn-surround-overlay 'priority (1+ conn-mark-overlay-priority))
(put 'conn-surround-overlay 'conn-overlay t)

(defun conn-surround-create-regions (regions)
  (cl-loop for (beg . end) in regions
           for ov = (make-overlay beg end)
           do (overlay-put ov 'category 'conn-surround-overlay)
           collect ov))

;;;;;; Surround With arg

(oclosure-define (conn-surround-with-argument
                  (:parent conn-state-eval-argument)))

(defun conn-surround-with-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-surround-with-argument
                    (required t))
      (self cmd)
    (conn-handle-surround-with-argument cmd self)))

(cl-defmethod conn-eval-argument ((arg conn-surround-with-argument))
  (conn-state-eval-argument-value arg))

(cl-defgeneric conn-handle-surround-with-argument (cmd arg)
  (:method (_ arg) arg))

(cl-defmethod conn-handle-surround-with-argument ((cmd (eql surround-self-insert))
                                                  arg)
  (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg))))

;;;;;; Padding Arg

(oclosure-define (conn-surround-padding-argument
                  (:parent conn-state-eval-argument)))

(defun conn-surround-padding-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-surround-padding-argument)
      (self cmd)
    (if (eq cmd 'conn-padding-flag)
        (conn-set-argument
         self (unless value
                (if (conn-state-eval-consume-prefix-arg)
                    (read-string "Padding: ")
                  " ")))
      self)))

(cl-defmethod conn-display-argument ((arg conn-surround-padding-argument))
  (concat "\\[conn-padding-flag] "
          (if-let* ((p (conn-state-eval-argument-value arg)))
              (propertize (format "padding <%s>" p)
                          'face 'eldoc-highlight-function-argument)
            "padding")))

;;;;;; Perform Surround

(cl-defgeneric conn-perform-surround (with arg &key &allow-other-keys))

(cl-defmethod conn-perform-surround :around (_with _arg &key regions &allow-other-keys)
  (dolist (ov regions)
    (goto-char (overlay-start ov))
    (conn--push-ephemeral-mark (overlay-end ov) nil t)
    (cl-call-next-method)))

(cl-defmethod conn-perform-surround :before (_with _arg &key &allow-other-keys)
  ;; Normalize point and mark
  (unless (= (point) (region-beginning))
    (exchange-point-and-mark)))

(cl-defmethod conn-perform-surround ((_with (eql surround-comment)) arg
                                     &key &allow-other-keys)
  (comment-region (region-beginning) (region-end) arg))

(cl-defmethod conn-perform-surround ((_with (eql surround-uncomment)) arg
                                     &key &allow-other-keys)
  (uncomment-region (region-beginning) (region-end) arg))

(defun conn--perform-surround-with-pair-subr (pair padding arg)
  (cl-labels ((ins-pair (open &optional close)
                (insert open)
                (exchange-point-and-mark)
                (save-excursion (insert (or close open)))
                (exchange-point-and-mark)))
    (pcase pair
      ('nil
       (dotimes (_ (prefix-numeric-value arg))
         (ins-pair last-input-event)))
      ((or `(,_cmd ,open ,close)
           `(,open ,close))
       (dotimes (_ (prefix-numeric-value arg))
         (ins-pair open close))))
    (when padding (ins-pair padding))))

(cl-defmethod conn-perform-surround ((_with (eql surround-self-insert)) arg
                                     &key padding &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (assoc last-input-event insert-pair-alist)
   padding arg))

(defun conn--make-surround-region (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'category 'conn-surround-overlay)
    ov))

(cl-defgeneric conn-prepare-surround (cmd arg &key &allow-other-keys)
  (declare (conn-anonymous-thing-property :prepare-surround-op)
           (important-return-value t))
  ( :method ((_ (eql nil)) _ &rest _ &key &allow-other-keys) nil)
  ( :method ((cmd (conn-thing anonymous-thing)) arg &rest keys &key &allow-other-keys)
    (if-let* ((op (conn-anonymous-thing-property cmd :prepare-surround-op)))
        (apply op arg keys)
      (apply #'conn-prepare-surround (conn-anonymous-thing-parent cmd) arg keys))))

(cl-defmethod conn-prepare-surround (cmd arg &key subregions trim &allow-other-keys)
  (let ((bounds (conn-bounds-of cmd arg))
        regions)
    (dolist (bound (or (when subregions
                         (conn-bounds-get bounds :subregions))
                       (list bounds)))
      (pcase (or (when trim
                   (conn-bounds-get bound :trimmed))
                 (conn-bounds-get bound :outer))
        ((and `(,beg . ,end) (pred identity))
         (push (conn--make-surround-region beg end) regions))))
    (list (nreverse regions))))

(defun conn-surround (&optional arg)
  (interactive "P")
  (atomic-change-group
    (save-mark-and-excursion
      (pcase-let* ((`(,regions . ,prep-keys)
                    (conn-eval-with-state 'conn-read-thing-state
                        (conn-prepare-surround
                         && (conn-thing-argument-dwim t)
                         :subregions & (conn-subregions-argument
                                        (use-region-p))
                         :trim & (conn-trim-argument t))
                      :prompt "Surround"
                      :prefix arg))
                   (cleanup (plist-get prep-keys :cleanup))
                   (success nil))
        (when regions
          (goto-char (overlay-start (car regions))))
        (unwind-protect
            (pcase-let ((`(,with ,with-arg . ,with-keys)
                         (conn-with-overriding-map (plist-get prep-keys :keymap)
                           (conn-eval-with-state 'conn-surround-with-state
                               (list && (conn-surround-with-argument)
                                     :padding & (conn-surround-padding-argument))
                             :prompt "Surround With"))))
              (apply #'conn-perform-surround
                     `(,with ,with-arg :regions ,regions ,@prep-keys ,@with-keys))
              (setq success t))
          (mapc #'delete-overlay regions)
          (when cleanup
            (funcall cleanup (if success :accept :cancel))))))))

;;;;;; Surround Read Pair

(defvar conn-read-pair-function 'conn-progressive-read-pair)

(defun conn-progressive-read-pair (collection)
  (let* ((collection (compat-call
                      sort (seq-uniq (mapcar #'copy-sequence collection))
                      :lessp (lambda (x y)
                               (or (< (length x) (length y))
                                   (and (= (length x) (length y))
                                        (string< x y))))
                      :in-place t))
         (narrowed collection)
         (prompt (propertize "Pair" 'face 'minibuffer-prompt))
         (so-far ""))
    (unwind-protect
        (while (not (length= narrowed 1))
          (with-current-buffer-window
              "*Conn Pairs*"
              `((display-buffer--maybe-same-window
                 display-buffer-reuse-window
                 display-buffer-below-selected)
                (window-height . completions--fit-window-to-buffer)
                ,(when temp-buffer-resize-mode
                   '(preserve-size . (nil . t)))
                (body-function
                 . ,#'(lambda (_window)
                        (insert (cl-loop for i from 0 below 10
                                         for item in narrowed
                                         concat (concat item "\n"))))))
              nil)
          (conn-with-dispatch-event-handler 'backspace
              (define-keymap
                "<remap> <backward-delete-char>" 'backspace)
              (lambda (cmd)
                (when (eq cmd 'backspace)
                  (when (length> so-far 0)
                    (cl-callf substring so-far 0 -1)
                    (setq narrowed collection)
                    (throw 'backspace nil))))
            (cl-callf thread-last
                so-far
              (conn-dispatch-read-event prompt t nil)
              (char-to-string)
              (concat so-far)))
          (cl-loop for item in narrowed
                   when (string-prefix-p so-far item)
                   do
                   (remove-text-properties 0 (1- (length item)) '(face) item)
                   (add-text-properties 0 (length so-far)
                                        '(face completions-highlight)
                                        item)
                   and collect item into next
                   finally do (if (null next)
                                  (setq so-far (substring so-far 0 -1))
                                (setq narrowed next))))
      (delete-window (get-buffer-window "*Conn Pairs*" 0)))
    (let ((result (car narrowed)))
      (remove-text-properties 0 (1- (length result)) '(face) result)
      result)))

(cl-defmethod conn-handle-surround-with-argument ((cmd (eql conn-read-pair))
                                                  arg)
  (conn-set-argument
   arg (list cmd (conn-state-eval-consume-prefix-arg))))

(defvar conn--surround-current-pair nil)

(cl-defmethod conn-perform-surround ((_with (eql conn-read-pair)) arg
                                     &key padding &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (with-memoization conn--surround-current-pair
     (let ((open (funcall conn-read-pair-function
                          (cl-loop for (open . _) in insert-pair-alist
                                   collect (pcase open
                                             ((pred stringp) open)
                                             (_ (string open)))))))
       (or (assoc open insert-pair-alist)
           (assq (aref open 0) insert-pair-alist))))
   padding
   arg))

(cl-defmethod conn-perform-surround :around ((_with (eql conn-read-pair))
                                             _arg &keys &allow-other-keys)
  (let ((conn--surround-current-pair nil))
    (cl-call-next-method)))


;;;;; Change

(conn-define-state conn-change-state (conn-read-thing-state)
  :lighter "CHG")

(cl-defgeneric conn-perform-change (cmd arg &optional kill)
  (declare (conn-anonymous-thing-property :change-op))
  ( :method ((cmd (conn-thing anonymous-thing)) arg &optional kill)
    (if-let* ((change-op (conn-anonymous-thing-property cmd :change-op)))
        (funcall change-op arg kill)
      (conn-perform-change (conn-anonymous-thing-parent cmd) arg kill))))

(cl-defmethod conn-perform-change (cmd arg &optional kill)
  (pcase-let (((conn-bounds-get :outer `(,beg . ,end))
               (conn-bounds-of cmd arg)))
    (goto-char beg)
    (cond (kill
           (funcall (conn--without-conn-maps
                      (keymap-lookup nil conn-kill-region-keys t))
                    beg end)
           (conn-push-state 'conn-emacs-state))
          (t
           (funcall (conn--without-conn-maps
                      (keymap-lookup nil conn-delete-region-keys t))
                    beg end)
           (if (eq 'conn-emacs-state (conn-peek-state))
               (conn-pop-state)
             (conn-push-state 'conn-emacs-state))))))

(defun conn-change-thing (cmd arg &optional kill)
  "Change region defined by CMD and ARG."
  (interactive
   (conn-eval-with-state 'conn-change-state
       (list && (oclosure-lambda (conn-thing-argument
                                  (required t)
                                  (value (when (use-region-p)
                                           (list 'region nil)))
                                  (set-flag (use-region-p)))
                    (self cmd)
                  (pcase cmd
                    ('conn-surround
                     (conn-set-argument
                      self (list cmd (conn-state-eval-consume-prefix-arg))))
                    (_ (conn-handle-thing-argument cmd self))))
             & current-prefix-arg)
     :prompt "Thing"))
  (conn-perform-change cmd arg kill))

(defun conn-change (&optional kill)
  "Change region between START and END.

If KILL is non-nil add region to the `kill-ring'.  When in
`rectangle-mark-mode' defer to `string-rectangle'."
  (interactive "P")
  (conn-perform-change 'region nil kill))

;;;;;; Change Surround

(define-error 'conn-no-surround "No surround at point" 'user-error)

(conn-define-state conn-change-surround-state (conn-surround-with-state)
  :lighter "CHG-SURROUND")

(keymap-set (conn-get-state-map 'conn-change-state) "s" 'conn-surround)

(oclosure-define (conn-change-surround-argument
                  (:parent conn-state-eval-argument)))

(defun conn-change-surround-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-change-surround-argument
                    (required t))
      (self cmd)
    (conn-handle-change-surround-argument cmd self)))

(cl-defgeneric conn-handle-change-surround-argument (cmd arg))

(cl-defmethod conn-handle-change-surround-argument (_cmd arg)
  arg)

(cl-defmethod conn-handle-change-surround-argument ((cmd (eql surround-self-insert)) arg)
  (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg))))

(cl-defgeneric conn-prepare-change-surround (cmd arg)
  (declare (conn-anonymous-thing-property :prepare-change-surround-op)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) arg)
    (if-let* ((op (conn-anonymous-thing-property cmd :prepare-change-surround-op)))
        (funcall op arg)
      (conn-prepare-change-surround (conn-anonymous-thing-parent cmd)))))

(cl-defmethod conn-prepare-change-surround ((_cmd (eql surround-self-insert)) arg)
  (catch 'return
    (save-mark-and-excursion
      (pcase-let* (((or `(,_cmd ,open ,close)
                        `(,open ,close))
                    (or (assoc last-input-event insert-pair-alist)
                        (list last-input-event last-input-event)))
                   (n (prefix-numeric-value arg)))
        (conn--push-ephemeral-mark)
        (conn--expand-create-expansions)
        (pcase-dolist (`(,beg . ,end) conn--current-expansions)
          (when (and (save-excursion
                       (goto-char beg)
                       (= n (skip-chars-forward (string open) (+ beg n))))
                     (save-excursion
                       (goto-char end)
                       (= (- n) (skip-chars-backward (string close) (- end n)))))
            (goto-char beg)
            (conn--push-ephemeral-mark end)
            (delete-char n)
            (exchange-point-and-mark)
            (delete-char (- n))
            (exchange-point-and-mark)
            (throw 'return (list (conn--make-surround-region
                                  (region-beginning)
                                  (region-end))))))))
    (signal 'conn-no-surround nil)))

(cl-defmethod conn-perform-change ((_cmd (eql conn-surround)) _arg
                                   &optional _kill)
  (save-mark-and-excursion
    (atomic-change-group
      (pcase-let* ((`(,ov . ,prep-keys)
                    (conn-eval-with-state 'conn-change-surround-state
                        (conn-prepare-change-surround
                         & (conn-change-surround-argument))
                      :prompt "Change Surrounding"))
                   (cleanup (plist-get prep-keys :cleanup))
                   (keymap (plist-get prep-keys :keymap))
                   (success nil))
        (unwind-protect
            (pcase-let ((`(,with ,with-arg . ,with-keys)
                         (conn-with-overriding-map keymap
                           (conn-eval-with-state 'conn-surround-with-state
                               (list && (conn-surround-with-argument)
                                     & (conn-surround-padding-argument))
                             :prompt "Surround With"))))
              (goto-char (overlay-start ov))
              (conn--push-ephemeral-mark (overlay-end ov) nil t)
              (apply #'conn-perform-surround
                     `(,with ,with-arg ,@prep-keys ,@with-keys))
              (setq success t))
          (delete-overlay ov)
          (when cleanup
            (funcall cleanup (if success :accept :cancel))))))))


;;;;; Transition Functions

(defvar conntext-state-hook nil)

(defun conntext-state ()
  (interactive)
  (run-hook-with-args-until-success 'conntext-state-hook))

(defun conn-one-command ()
  (interactive)
  (conn-push-state 'conn-one-command-state))

(defun conn-previous-emacs-state (arg)
  (interactive "p")
  (cond ((< arg 0)
         (conn-next-emacs-state (abs arg)))
        ((> arg 0)
         (push-mark nil t)
         (dotimes (_ (1- arg))
           (conn-ring-rotate-forward conn-emacs-state-ring))
         (if (and conn-emacs-state
                  (conn-ring-head conn-emacs-state-ring)
                  (= (point) (conn-ring-head conn-emacs-state-ring)))
             (progn
               (conn-ring-rotate-forward conn-emacs-state-ring)
               (goto-char (conn-ring-head conn-emacs-state-ring)))
           (goto-char (conn-ring-head conn-emacs-state-ring))
           (conn-push-state 'conn-emacs-state)))))

(defun conn-next-emacs-state (arg)
  (interactive "p")
  (cond ((< arg 0)
         (conn-previous-emacs-state (abs arg)))
        ((> arg 0)
         (push-mark nil t)
         (dotimes (_ arg)
           (conn-ring-rotate-backward conn-emacs-state-ring))
         (goto-char (conn-ring-head conn-emacs-state-ring))
         (conn-push-state 'conn-emacs-state))))

(defun conn-insert-state ()
  "Enter insert state for the current buffer."
  (interactive)
  (conn-push-state 'conn-emacs-state))

(defun conn-command-state ()
  "Enter command state for the current buffer."
  (interactive)
  (conn-push-state 'conn-command-state))

(defun conn-emacs-state-at-mark ()
  "Exchange point and mark then enter `conn-emacs-state'."
  (interactive)
  (conn-exchange-mark-command)
  (conn-push-state 'conn-emacs-state))

(defun conn-change-whole-line (&optional arg)
  "`kill-whole-line' and enter `conn-emacs-state'."
  (interactive "P")
  (kill-whole-line arg)
  (open-line 1)
  (indent-according-to-mode)
  (conn-push-state 'conn-emacs-state))

(defun conn-change-line ()
  "`kill-line' and enter `conn-emacs-state'."
  (interactive)
  (beginning-of-line)
  (call-interactively (keymap-lookup nil conn-kill-line-keys t))
  (conn-push-state 'conn-emacs-state))

(defun conn-emacs-state-open-line-above (&optional arg)
  "Open line above and enter `conn-emacs-state'.

If ARG is non-nil move up ARG lines before opening line."
  (interactive "p")
  (forward-line (- (1- arg)))
  (move-beginning-of-line nil)
  (insert "\n")
  (forward-line -1)
  ;; FIXME: see crux smart open line
  (indent-according-to-mode)
  (conn-push-state 'conn-emacs-state))

(defun conn-emacs-state-open-line (&optional arg)
  "Open line and enter `conn-emacs-state'.

If ARG is non-nil move down ARG lines before opening line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent)
  (conn-push-state 'conn-emacs-state))

(defun conn-emacs-state-overwrite (&optional arg)
  "Enter emacs state in `overwrite-mode'.

`overwrite-mode' will be turned off when when emacs state is exited.
If ARG is non-nil enter emacs state in `binary-overwrite-mode' instead."
  (interactive "P")
  (conn-push-state 'conn-emacs-state)
  (conn-state-defer
    (overwrite-mode -1))
  (if arg
      (binary-overwrite-mode 1)
    (overwrite-mode 1)))

(defun conn-emacs-state-overwrite-binary ()
  "Enter Emacs state in `binary-overwrite-mode'."
  (interactive)
  (conn-emacs-state-overwrite 1))


;;;;; Window Commands

(defun conn-other-buffer ()
  "Switch to the most recently selected buffer.

Repeated calls allow one to switch back and forth between another
buffer."
  (interactive)
  (switch-to-buffer nil))

(defun conn-other-place-prefix ()
  "Display next buffer in another place.

Choose from among the following options:

Window: `other-window-prefix'
Frame: `other-frame-prefix'
Tab: `other-tab-prefix'
Prompt: `conn-other-window-prompt-prefix'
Current Window: `conn-this-window-prefix'"
  (interactive)
  (pcase (car (read-multiple-choice
               "Place:"
               '((?w "window")
                 (?f "frame")
                 (?t "tab")
                 (?c "current window")
                 (?g "prompt"))))
    (?w (other-window-prefix))
    (?f (other-frame-prefix))
    (?t (other-tab-prefix))
    (?c (conn-this-window-prefix))
    (?g (conn-other-window-prompt-prefix))))

(defun conn-other-window-prefix (&optional this-window)
  (interactive "P")
  (if this-window
      (conn-this-window-prefix)
    (let ((windows (conn--get-windows nil 'nomini)))
      (if (length= windows 1)
          (other-window-prefix)
        (display-buffer-override-next-command
         (lambda (_ _)
           (cons (conn-prompt-for-window
                  (conn--get-windows nil 'nomini nil nil
                                     (lambda (win)
                                       (not (eq win (selected-window))))))
                 'reuse))
         nil "[select]")
        (message "Display next command in selected buffer…")))))

(defun conn-other-window-prompt-prefix ()
  "Display next buffer in a window selected by `conn-prompt-for-window'."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (cons (conn-prompt-for-window (conn--get-windows nil 'nomini) t)
           'reuse))
   nil "[select]")
  (message "Display next command in selected buffer…"))

(defun conn-this-window-prefix ()
  "Display next buffer in the currently selected window."
  (interactive)
  (display-buffer-override-next-command
   'display-buffer-same-window
   nil "[current-window]")
  (message "Display next command buffer in current window…"))

(defun conn-transpose-window (window)
  "Prompt for window and swap current window and other window."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window) (conn--get-windows nil 'nomini 'visible)))))
  (unless (eq window (selected-window))
    (if window
        (window-swap-states nil window)
      (user-error "No other visible windows"))))

(defun conn-throw-buffer ()
  "Send current buffer to another window and `switch-to-prev-buffer'."
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-prev-buffer)
    (save-selected-window
      (display-buffer
       buf
       (lambda (_ _)
         (cons (conn-prompt-for-window
                (delq (selected-window)
                      (conn--get-windows nil 'nomini)))
               'reuse))))))

(defun conn-yank-window (window)
  "Swap selected window and another window.

Currently selected window remains selected afterwards."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn--get-windows nil 'nomini 'visible)))))
  (unless (eq window (selected-window))
    (if window
        (save-selected-window (window-swap-states nil window))
      (user-error "No other visible windows"))))


;;;; WinControl

;; A simple version of hyperbole's hycontrol-windows

(defgroup conn-wincontrol nil
  "Conn-mode WinControl."
  :prefix "conn-wincontrol-"
  :group 'conn)

(defvar conn--wincontrol-help-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl " 'face 'minibuffer-prompt)
   "(arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) ", "
   "\\[conn-wincontrol-digit-argument-reset]: reset arg; "
   "\\[conn-wincontrol-exit]: exit):"
   "%s"))

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--wincontrol-preserve-arg nil)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)
(defvar conn--wincontrol-error-message nil)
(defvar conn--wincontrol-prev-eldoc-msg-fn)


;;;;; Wincontrol Internals

(defvar-keymap conn-window-resize-map
  "v" 'conn-wincontrol-maximize-vertically
  "r" 'conn-wincontrol-maximize-horizontally
  "m" 'maximize-window
  "b" 'balance-windows
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-window-resize-repeat-map
  :repeat t
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "o" 'other-window)

(defvar-keymap conn-wincontrol-next-window-repeat-map
  :repeat t
  "o" 'conn-wincontrol-next-window)

(defvar-keymap conn-wincontrol-scroll-repeat-map
  :repeat t
  "i" 'conn-wincontrol-scroll-down
  "k" 'conn-wincontrol-scroll-up
  "K" 'conn-wincontrol-other-window-scroll-up
  "I" 'conn-wincontrol-other-window-scroll-down)

(defvar-keymap conn-wincontrol-text-scale-repeat-map
  :repeat t
  "z" 'text-scale-decrease
  "Z" 'text-scale-increase)

(defvar-keymap conn-wincontrol-tab-repeat-map
  :repeat t
  "C" 'tab-bar-duplicate-tab
  "O" 'tab-new
  "o" 'tab-next
  "U" 'tab-close
  "u" 'tab-previous
  "B" 'tab-bar-move-window-to-tab
  "D" 'tab-bar-detach-tab)

(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "C-l" 'recenter-top-bottom
  "," (conn-remap-key "<conn-thing-map>" nil t)
  "-" 'conn-wincontrol-invert-argument
  "0" 'conn-wincontrol-digit-argument
  "1" 'conn-wincontrol-digit-argument
  "2" 'conn-wincontrol-digit-argument
  "3" 'conn-wincontrol-digit-argument
  "4" 'conn-wincontrol-digit-argument
  "5" 'conn-wincontrol-digit-argument
  "6" 'conn-wincontrol-digit-argument
  "7" 'conn-wincontrol-digit-argument
  "8" 'conn-wincontrol-digit-argument
  "9" 'conn-wincontrol-digit-argument
  ";" 'conn-wincontrol-exit-to-initial-win
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward
  "C-M-d" 'delete-other-frames
  "C-S-l" 'move-to-window-line-top-bottom
  "C-]" 'conn-wincontrol-abort
  "C-h" 'help-command
  "C-r" 'conn-wincontrol-isearch-backward
  "C-s" 'conn-wincontrol-isearch
  "C-u" 'conn-wincontrol-universal-arg
  "DEL" 'conn-wincontrol-backward-delete-arg
  "M-/" 'undelete-frame
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-<down>" 'windmove-swap-states-down
  "M-<left>" 'windmove-swap-states-left
  "M-<right>" 'windmove-swap-states-right
  "M-<up>" 'windmove-swap-states-up
  "M-DEL" 'conn-wincontrol-digit-argument-reset
  "M-I" 'windmove-swap-states-up
  "M-J" 'windmove-swap-states-left
  "M-K" 'windmove-swap-states-down
  "M-L" 'windmove-swap-states-right
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "M-i" 'conn-wincontrol-windmove-up
  "M-j" 'conn-wincontrol-windmove-left
  "M-k" 'conn-wincontrol-windmove-down
  "M-l" 'conn-wincontrol-windmove-right
  "M-o" 'other-frame
  "<down>" 'conn-wincontrol-windmove-down
  "<escape>" 'conn-wincontrol-exit
  "<left>" 'conn-wincontrol-windmove-left
  "<next>" 'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "<return>" 'conn-other-place-prefix
  "<right>" 'conn-wincontrol-windmove-right
  "<up>" 'conn-wincontrol-windmove-up
  "B" 'tab-bar-move-window-to-tab
  "C" 'tab-bar-duplicate-tab
  "D" 'tab-bar-detach-tab
  "F" 'toggle-frame-fullscreen
  "H" 'conn-kill-this-buffer
  "I" 'conn-wincontrol-other-window-scroll-down
  "J" 'bury-buffer
  "K" 'conn-wincontrol-other-window-scroll-up
  "L" 'unbury-buffer
  "O" 'tab-new
  "R" 'conn-wincontrol-isearch-other-window-backward
  "S" 'conn-wincontrol-isearch-other-window
  "U" 'tab-close
  "Z" 'text-scale-increase
  "T" 'tear-off-window
  "_" 'shrink-window-if-larger-than-buffer
  "`" 'conn-wincontrol-mru-window
  "b" 'switch-to-buffer
  "c" (conn-remap-key "C-c" nil t)
  "d" 'delete-window
  "e" 'conn-wincontrol-exit
  "f" 'conn-goto-window
  "g" (conn-remap-key "M-g" t t)
  "h" 'kill-buffer-and-window
  "i" 'conn-wincontrol-scroll-down
  "j" 'previous-buffer
  "k" 'conn-wincontrol-scroll-up
  "l" 'next-buffer
  "m" 'end-of-buffer
  "n" 'beginning-of-buffer
  "o" 'tab-next
  "p" 'conn-register-prefix
  "q" 'quit-window
  "r" 'conn-wincontrol-split-right
  "s" conn-window-resize-map
  "t" 'conn-transpose-window
  "u" 'tab-previous
  "v" 'conn-wincontrol-split-vertically
  "w" 'conn-throw-buffer
  "x" 'delete-other-windows
  "y" 'conn-yank-window
  "z" 'text-scale-decrease
  "<t>" 'conn-wincontrol-ignore)

(put 'conn-wincontrol-digit-argument-reset :advertised-binding (key-parse "M-DEL"))

(define-minor-mode conn-wincontrol-mode
  "Global minor mode for window control."
  :global t
  :lighter " WinC"
  :interactive nil
  :group 'conn-wincontrol
  (if conn-wincontrol-mode
      (conn--wincontrol-setup)
    (conn--wincontrol-exit)))

(defun conn-wincontrol ()
  "Enable `conn-wincontrol-mode'."
  (interactive)
  (if (= (minibuffer-depth) 0)
      (conn-wincontrol-mode 1)
    (user-error "Cannot activate wincontrol while minibuffer is active.")))

;; From transient
(defun conn--wincontrol-wrap-this-command ()
  (letrec ((command
            (when (symbolp this-command)
              (when-let* ((fn (symbol-function this-command))
                          ((autoloadp fn)))
                (autoload-do-load fn))
              this-command))
           (exit-and-debug
            (lambda (&rest args)
              (conn-wincontrol-mode -1)
              (apply #'debug args)))
           (advice
            (lambda (fn &rest args)
              (interactive
               (lambda (spec)
                 (let ((debugger exit-and-debug)
                       (abort t))
                   (unwind-protect
                       (prog1
                           (advice-eval-interactive-spec spec)
                         (setq abort nil))
                     (when (and abort command)
                       (remove-function (symbol-function command) advice))))))
              (unwind-protect
                  (let ((debugger exit-and-debug))
                    (apply fn args))
                (when command
                  (remove-function (symbol-function command) advice))))))
    (add-function :around (if command command this-command)
                  advice '((depth . -99)))))

(defun conn--wincontrol-pre-command ()
  (when (or conn--wincontrol-arg (< conn--wincontrol-arg-sign 0))
    (setq prefix-arg (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))
  (conn--wincontrol-wrap-this-command)
  (setq conn--wincontrol-error-message nil)
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message nil)))

(defun conn--wincontrol-post-command ()
  (unless conn--wincontrol-preserve-arg
    (setq conn--wincontrol-arg nil
          conn--wincontrol-arg-sign 1))
  (setq conn--wincontrol-preserve-arg nil)
  (cond
   ((not (eq conn-wincontrol-map (cadr overriding-terminal-local-map)))
    ;; Something else is using overriding-terminal-local-map,
    ;; e.g. isearch or transient, turn wincontrol off.
    (conn-wincontrol-mode -1))
   ((not (zerop (minibuffer-depth)))
    (conn--wincontrol-exit)
    (add-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit))
   (t (conn--wincontrol-message))))

(defun conn--wincontrol-new-frame (frame)
  (set-face-inverse-video 'mode-line t frame))

(defun conn--wincontrol-message ()
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message (substitute-command-keys conn--wincontrol-help-format)
             (format (if conn--wincontrol-arg "%s%s" "[%s1]")
                     (if (= conn--wincontrol-arg-sign -1) "-" "")
                     conn--wincontrol-arg)
             (concat
              (if conn--wincontrol-error-message " ")
              conn--wincontrol-error-message))))

(defun conn--wincontrol-setup (&optional preserve-state)
  (unless (memq conn-wincontrol-map overriding-terminal-local-map)
    (internal-push-keymap conn-wincontrol-map 'overriding-terminal-local-map)
    (add-hook 'post-command-hook 'conn--wincontrol-post-command)
    (add-hook 'pre-command-hook 'conn--wincontrol-pre-command)
    (add-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
    (add-function :override eldoc-message-function 'ignore)
    (unless preserve-state
      (setq conn--wincontrol-arg (when current-prefix-arg
                                   (prefix-numeric-value current-prefix-arg))
            conn--wincontrol-arg-sign 1
            conn--wincontrol-initial-window (selected-window)
            conn--wincontrol-initial-winconf (current-window-configuration)))
    (set-face-inverse-video 'mode-line t)
    (conn--wincontrol-message)))

(defun conn--wincontrol-exit ()
  (when (memq conn-wincontrol-map overriding-terminal-local-map)
    (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
    (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
    (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
    (remove-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (remove-function eldoc-message-function 'ignore)
    (set-face-inverse-video 'mode-line nil)))

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn--wincontrol-setup t)))

(defun conn-wincontrol-one-command ()
  "Execute one command in `conn-wincontrol-mode'."
  (interactive)
  (add-hook 'pre-command-hook
            (conn-anaphoricate pre
              (lambda ()
                (unless (memq this-command
                              '(conn-wincontrol-backward-delete-arg
                                conn-wincontrol-digit-argument-reset
                                conn-wincontrol-invert-argument
                                conn-wincontrol-digit-argument
                                conn-wincontrol-universal-arg))
                  (remove-hook 'pre-command-hook pre)
                  (conn-wincontrol-exit))))
            99)
  (conn-wincontrol))

(defun conn-wincontrol-ignore ()
  (interactive)
  (setq conn--wincontrol-error-message (propertize "Invalid Command" 'face 'error)
        conn--wincontrol-preserve-arg t))


;;;;; Wincontrol Prefix Arg

(defun conn-wincontrol-universal-arg ()
  "Multiply wincontrol prefix arg by 4."
  (interactive)
  (setq conn--wincontrol-arg (* 4 (or conn--wincontrol-arg 1))
        conn--wincontrol-preserve-arg t))

(defun conn-wincontrol-digit-argument ()
  (interactive)
  (setq conn--wincontrol-preserve-arg t)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (if conn--wincontrol-arg
        (setq conn--wincontrol-arg
              (+ (if (>= (or conn--wincontrol-arg 1) 0) digit (- digit))
                 (* 10 (or conn--wincontrol-arg 1))))
      (setq conn--wincontrol-arg digit)))
  (setq this-command 'conn-wincontrol-digit-argument))

(defun conn-wincontrol-invert-argument ()
  "Invert sign of wincontrol prefix arg."
  (interactive)
  (setq conn--wincontrol-preserve-arg t
        conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to nil and sign to +."
  (interactive)
  (setq conn--wincontrol-arg-sign 1
        conn--wincontrol-arg nil))

(defun conn-wincontrol-backward-delete-arg ()
  "Delete least significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-preserve-arg t
        conn--wincontrol-arg (floor conn--wincontrol-arg 10)))

(defun conn-wincontrol-prefix-arg ()
  (when conn--wincontrol-arg
    (setq conn--wincontrol-preserve-arg t)
    (* conn--wincontrol-arg-sign conn--wincontrol-arg)))


;;;;; Wincontrol Quiting

(defun conn-wincontrol-exit ()
  "Exit `conn-wincontrol-mode'."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)))

(defun conn-wincontrol-abort ()
  "Exit `conn-wincontrol-mode'."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)
    (when conn--wincontrol-initial-winconf
      (set-window-configuration conn--wincontrol-initial-winconf))))

(defun conn-wincontrol-exit-to-initial-win ()
  "Exit `conn-wincontrol-mode' and select initial window."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)
    (when (window-live-p conn--wincontrol-initial-window)
      (select-window conn--wincontrol-initial-window))))


;;;;; Wincontrol Isearch

(defun conn-wincontrol-isearch (arg)
  "`isearch-forward', resuming `conn-wincontrol-mode' afterward."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-forward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-backward (arg)
  "`isearch-backward', resuming `conn-wincontrol-mode' afterward."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-backward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window (arg)
  "`isearch-forward' in `other-window-for-scrolling'."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-forward arg))
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window-backward (arg)
  "`isearch-backward' in `other-window-for-scrolling'."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-backward arg))
      (conn--wincontrol-setup t))))


;;;;; Window Selection

(defun conn-wincontrol-next-window ()
  "`other-window' in cyclic order."
  (interactive)
  (other-window 1))

(defun conn-wincontrol-previous-window ()
  "`other-window' in reverse cyclic order."
  (interactive)
  (other-window -1))

(defun conn-goto-window ()
  "Prompt for a window and then select it."
  (interactive)
  (if-let* ((window (conn-prompt-for-window
                     (delq (selected-window)
                           (conn--get-windows
                            nil 'nomini
                            (if current-prefix-arg 'visible))))))
      (select-window window)
    (user-error "No other windows available to select")))

(defun conn-wincontrol-mru-window ()
  "Select most recently used window."
  (interactive)
  (when-let* ((mru (get-mru-window 0 nil t t)))
    (select-window mru)))


;;;;; Windmove

(defun conn-wincontrol-windmove-up ()
  "`windmove-up'."
  (interactive)
  (windmove-up))

(defun conn-wincontrol-windmove-down ()
  "`windmove-down'."
  (interactive)
  (windmove-down))

(defun conn-wincontrol-windmove-right ()
  "`windmove-right'."
  (interactive)
  (windmove-right))

(defun conn-wincontrol-windmove-left ()
  "`windmove-left'."
  (interactive)
  (windmove-left))

(defun conn-wincontrol-quit-other-window-for-scrolling ()
  "`quit-window' in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))


;;;;; Window Scroll Commands

(defun conn-wincontrol-other-window-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                         next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-down-command)
                   (command-remapping #'conn-scroll-down)
                   #'conn-scroll-down)))))

(defun conn-wincontrol-other-window-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                         next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-up-command)
                   (command-remapping #'conn-scroll-up)
                   #'conn-scroll-up)))))

(defun conn-wincontrol-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-down)))

(defun conn-wincontrol-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up)))


;;;;; Window Configuration Commands

(defun conn-wincontrol-widen-window ()
  (interactive)
  (enlarge-window-horizontally (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-narrow-window ()
  (interactive)
  (shrink-window-horizontally (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-heighten-window ()
  (interactive)
  (enlarge-window (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-shorten-window ()
  (interactive)
  (shrink-window (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-split-vertically ()
  "Split window vertically.
Uses `split-window-vertically'."
  (interactive)
  (select-window (split-window-vertically)))

(defun conn-wincontrol-split-right ()
  "Split window vertically.
Uses `split-window-right'."
  (interactive)
  (select-window (split-window-right)))

(defun conn-wincontrol-maximize-horizontally ()
  "Delete all adjacent windows horizontally.

Operates with the selected windows parent window."
  (interactive)
  (conn-wincontrol-maximize-vertically t))

(defun conn-wincontrol-maximize-vertically (&optional horizontal)
  "Delete all adjacent windows vertically.

Operates with the selected windows parent window."
  (interactive)
  (when (and (not (window-minibuffer-p (selected-window)))
             (not (window-parameter (selected-window) 'window-side))
             (window-combined-p (selected-window) horizontal))
    (cl-loop for sub = (thread-first
                         (selected-window)
                         window-parent
                         window-child)
             then (window-right sub)
             while sub
             unless (or (window-parameter sub 'no-delete-other-windows)
                        (eq sub (selected-window)))
             collect sub into to-delete
             finally (mapc #'delete-window to-delete))))

(static-if (<= 31 emacs-major-version)
    (progn
      (define-keymap
        :keymap conn-wincontrol-map
        "\\" 'window-layout-transpose
        "," 'rotate-windows-back
        "." 'rotate-windows
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise
        "|" 'window-layout-flip-leftright
        "_" 'window-layout-flip-topdown)

      (defvar-keymap conn-window-rotate-repeat-map
        :repeat t
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise
        "," 'rotate-windows-back
        "." 'rotate-windows)))

(defun conn-kill-this-buffer ()
  (interactive)
  (kill-buffer))


;;;; Keymaps

;;;;; Repeat Map

(defvar-keymap conn-reb-navigation-repeat-map
  :repeat t
  "C-s" 'reb-next-match
  "C-r" 'reb-prev-match)

(defvar-keymap conn-pop-mark-repeat-map
  :repeat t
  "o" 'conn-pop-mark-ring
  "u" 'conn-unpop-mark-ring)

(defvar-keymap conn-mru-window-repeat-map
  :repeat t
  "`" 'conn-wincontrol-mru-window)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)


;;;;; Mode Keymaps

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-mode)
              "C-c e" 'occur-edit-mode))

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-edit-mode)
              "C-c e" 'occur-cease-edit))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-movement-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state 'rectangle-mark-mode)
  "t" 'string-rectangle
  "z" 'rectangle-exchange-point-and-mark
  "C-y" 'conn-yank-replace-rectangle
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "<backspace>" 'clear-rectangle
  "d" 'open-rectangle
  "C-d" 'delete-whitespace-rectangle
  "#" 'rectangle-number-lines)
(conn-set-mode-map-depth 'rectangle-mark-mode -90 'conn-command-state)

(defvar-keymap conn-isearch-map
  "M-Y" 'conn-isearch-yank-region
  "M-<return>" 'conn-isearch-exit-and-mark
  "C-<return>" 'conn-isearch-exit-other-end
  "M-RET" 'conn-isearch-exit-and-mark
  "M-'" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch
  "C-'" 'conn-isearch-open-recursive-edit)


;;;;; Top-level Command State Maps

(defvar-keymap conn-default-region-map
  "m" 'conn-replace
  "u" 'conn-regexp-replace
  "'" 'conn-kapply-on-region-prefix
  "TAB" 'indent-rigidly
  "$" 'ispell-region
  "*" 'calc-grab-region
  ";" 'comment-or-uncomment-region
  "d" 'conn-duplicate-region
  "D" 'conn-duplicate-and-comment-region
  "b" 'conn-comment-or-uncomment
  "g" 'conn-rgrep-region
  "k" 'delete-region
  "RET" 'conn-join-lines
  "p" 'conn-sort-prefix
  "o" 'conn-occur-region
  "h" 'vc-region-history
  "s" 'conn-isearch-region-forward
  "r" 'conn-isearch-region-backward
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "N" 'conn-narrow-indirect
  "n" 'conn-narrow-to-thing
  "j" 'conn-kill-prepend-region
  "l" 'conn-kill-append-region
  "J" 'conn-append-region
  "L" 'conn-append-region)

(defvar-keymap conn-default-edit-map
  "l" 'duplicate-line
  "'" 'conn-kapply-on-thing-prefix
  "v" 'diff-buffer-with-file
  "F" 'conn-bind-last-dispatch-to-key
  "SPC" 'whitespace-cleanup
  "f" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "DEL" 'conn-change-whole-line
  "L" 'clone-indirect-buffer
  "k" 'conn-change-line
  "o" 'conn-emacs-state-open-line-above
  "j" 'conn-emacs-state-open-line
  "g" 'conn-emacs-state-overwrite
  "d" 'conn-duplicate
  "D" 'conn-duplicate-and-comment
  "b" 'conn-emacs-state-overwrite-binary
  "c" 'copy-from-above-command
  "y" 'yank-in-context
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule)

(defvar-keymap conn-search-map
  "h \\" 'conn-kapply-hightlight-prefix
  "s" 'conn-isearch-forward
  "r" 'conn-isearch-backward
  "o" 'occur
  "l" 'locate
  "m B" 'multi-isearch-buffers-regexp
  "m F" 'multi-isearch-files-regexp
  "m b" 'multi-isearch-buffers
  "m p" 'conn-multi-isearch-project
  "m f" 'multi-isearch-files)

(defvar-keymap conn-goto-map
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward
  "j" 'conn-unpop-movement-ring
  "l" 'conn-pop-movement-ring)

(defvar-keymap conn-movement-ring-repeat-map
  :repeat t
  "j" 'conn-unpop-movement-ring
  "l" 'conn-pop-movement-ring)

(defvar-keymap conn-global-mark-repeat-map
  :repeat t
  "p" 'pop-global-mark)

(defvar-keymap conn-error-repeat-map
  :repeat t
  "l" 'previous-error
  "j" 'next-error)


;;;;; Misc Maps

(defvar-keymap conn-indent-rigidly-map
  "l" 'indent-rigidly-right
  "j" 'indent-rigidly-left
  "L" 'indent-rigidly-right-to-tab-stop
  "J" 'indent-rigidly-left-to-tab-stop)


;;;;; Global Bindings

(defvar-keymap conn-dispatch-cycle-map
  :repeat t
  "l" 'conn-dispatch-cycle-ring-next
  "j" 'conn-dispatch-cycle-ring-previous)

(defvar-keymap conn-last-emacs-state-repeat-map
  :repeat t
  "M-p" 'conn-previous-emacs-state
  "M-n" 'conn-next-emacs-state)

(put 'conn-next-emacs-state 'repeat-check-key 'no)
(put 'conn-previous-emacs-state 'repeat-check-key 'no)

(defvar-keymap conn-local-mode-map
  "<conn-kbd-macro-query>" 'conn-kapply-kbd-macro-query
  "C-<escape>" 'exit-recursive-edit
  "C-x y" conn-dispatch-cycle-map
  "M-g o" 'conn-pop-mark-ring
  "M-g u" 'conn-unpop-mark-ring
  "M-g e" 'conn-previous-emacs-state
  "M-g E" 'conn-next-emacs-state
  "C-S-w" 'delete-region
  "C-." 'conn-dispatch
  "C->" 'conn-dispatch-on-buttons
  "C-x /" 'tab-bar-history-back
  "C-x 4 /" 'tab-bar-history-back
  "C-x 4 ?" 'tab-bar-history-forward
  "C-x 4 -" 'conn-window-resize-map
  "C-x ?" 'tab-bar-history-forward
  "C-x t s" 'tab-switch
  "C-x t a" 'conn-tab-to-register
  "C-`" 'other-window
  "C-x m" 'conn-kmacro-prefix
  "M-H" 'conn-wincontrol-maximize-horizontally
  "M-V" 'conn-wincontrol-maximize-vertically)

(define-keymap
  :keymap global-map
  "<conn-region-map>" conn-default-region-map
  "<conn-edit-map>" conn-default-edit-map
  "<conn-thing-map> ;" 'conn-mark-comment
  "<conn-thing-map> V" 'conn-mark-visual-line
  "<conn-thing-map> ," 'conn-goto-line
  "<conn-thing-map> <" 'conn-mark-before-point
  "<conn-thing-map> >" 'conn-mark-after-point
  "<conn-thing-map> /" 'conn-mark-filename
  "<conn-thing-map> U" 'conn-mark-uuid
  "<conn-thing-map> s" 'conn-mark-string
  "<conn-thing-map> @" 'conn-mark-email
  "<conn-thing-map> v" 'conn-mark-visible
  "<conn-thing-map> L" 'forward-line
  "<conn-thing-map> )" 'forward-list
  "<conn-thing-map> (" 'backward-list
  "<conn-thing-map> a" 'beginning-of-buffer
  "<conn-thing-map> e" 'end-of-buffer
  "<conn-thing-map> h" 'outline-previous-visible-heading)

(static-if (<= 30 emacs-major-version)
    (progn
      (keymap-global-set "<conn-edit-map> W" 'replace-regexp-as-diff)
      (keymap-global-set "<conn-edit-map> Q" 'multi-file-replace-regexp-as-diff)))

(define-keymap
  :keymap (or (alist-get 'conn-kmacro-applying-p minor-mode-map-alist)
              (setf (alist-get 'conn-kmacro-applying-p minor-mode-map-alist)
                    (make-sparse-keymap)))
  "<remap> <kbd-macro-query>" 'conn-kapply-kbd-macro-query)


;;;;; State Keymaps

(define-keymap
  :keymap (conn-get-state-map 'conn-emacs-state)
  "<escape>" 'conn-pop-state)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-common-state)
  "C-s" 'isearch-forward
  "s" 'isearch-forward
  "C-r" 'isearch-backward
  "C-M-s" 'isearch-forward-regexp
  "C-M-r" 'isearch-backward-regexp
  ";" 'comment
  "d" 'conn-forward-defun
  "t" 'conn-forward-inner-line
  "i" 'conn-backward-line
  "k" 'forward-line
  "h" 'conn-expand
  "p" 'forward-paragraph
  "," (conn-remap-key "<conn-thing-map>")
  "e" 'end-of-buffer)

(define-keymap
  :keymap (conn-get-state-map 'conn-movement-state)
  ">" 'forward-line
  "<" 'conn-backward-line
  "o" (conn-remap-key conn-forward-word-keys t)
  "O" 'forward-symbol
  "U" 'conn-backward-symbol
  "u" (conn-remap-key conn-backward-word-keys t)
  "(" (conn-remap-key conn-backward-list-keys t)
  ")" (conn-remap-key conn-forward-list-keys t)
  "[" (conn-remap-key conn-backward-up-list-keys t)
  "]" (conn-remap-key conn-down-list-keys t)
  "{" (conn-remap-key conn-backward-sentence-keys t)
  "}" (conn-remap-key conn-forward-sentence-keys t)
  "I" (conn-remap-key conn-backward-paragraph-keys t)
  "i" (conn-remap-key conn-previous-line-keys t)
  "J" 'conn-backward-inner-line
  "j" (conn-remap-key conn-backward-char-keys t)
  "K" (conn-remap-key conn-forward-paragraph-keys t)
  "k" (conn-remap-key conn-next-line-keys t)
  "L" 'conn-forward-inner-line
  "l" (conn-remap-key conn-forward-char-keys t)
  "M" (conn-remap-key conn-end-of-defun-keys t)
  "m" (conn-remap-key conn-forward-sexp-keys t)
  "N" (conn-remap-key conn-beginning-of-defun-keys t)
  "n" (conn-remap-key conn-backward-sexp-keys t))

(define-keymap
  :keymap (conn-get-state-map 'conn-menu-state)
  "s" (conn-remap-keymap "M-s" t)
  "g" (conn-remap-keymap "M-g" t)
  "c" (conn-remap-key "C-c" t)
  "x" (conn-remap-key "C-x" t)
  "C-4" (conn-remap-key "C-x 4" t)
  "C-5" (conn-remap-key "C-x 5" t))

(define-keymap
  :keymap (conn-get-state-map 'conn-command-state)
  :suppress t
  "S" 'conn-surround
  "C-<return>" 'conn-join-lines
  "<escape>" 'conn-pop-state
  "T" 'conn-change-thing
  "G" 'conn-copy-thing
  "D" 'conn-duplicate-region
  "P" 'conn-register-load-and-replace
  "+" 'conn-set-register-separator
  "H" 'conn-expand
  "b" (conn-remap-key "<conn-edit-map>")
  "Z" 'pop-to-mark-command
  "&" 'conn-other-buffer
  "e" 'conn-pop-state
  "E" 'conn-emacs-state-at-mark
  "t" 'conn-change
  "`" 'conn-wincontrol-mru-window
  "|" 'conn-shell-command-on-region
  "\\" 'conntext-state
  "." 'conn-other-window-prefix
  "/" (conn-remap-key conn-undo-keys t)
  ";" 'conn-wincontrol
  "'" 'conn-kapply-prefix
  "?" (conn-remap-key conn-undo-redo-keys t)
  "_" 'repeat-complex-command
  "SPC" 'conn-set-mark-command
  "M-y" 'conn-completing-yank-replace
  "C-M-l" 'conn-recenter-on-region
  "C-M-S-l" 'conn-recenter-on-region-other-window
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "C" 'conn-copy-region
  "d" (conn-remap-key conn-delete-char-keys t)
  "f" 'conn-dispatch
  "h" 'conn-wincontrol-one-command
  "," (conn-remap-key "<conn-thing-map>")
  "p" 'conn-register-prefix
  "q" 'conn-transpose-regions
  "r" (conn-remap-key "<conn-region-map>")
  "V" 'conn-rectangle-mark
  "v" 'conn-toggle-mark-command
  "w" 'conn-kill-region
  "W" 'widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" (conn-remap-key "C-y" t)
  "z" 'conn-exchange-mark-command)


;;;; Advice

(defun conn--toggle-input-method-ad (&rest app)
  (if (and conn-local-mode
           (not isearch-mode)
           (conn-state-get conn-current-state :suppress-input-method)
           conn--input-method)
      (unwind-protect
          (progn
            (remove-hook 'input-method-activate-hook 'conn--activate-input-method t)
            (activate-input-method conn--input-method)
            (deactivate-input-method))
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t))
    (apply app)))

(defun conn--read-from-suggestions-ad (&rest app)
  (if (and (mark t)
           (not (use-region-p))
           ;; Maybe not when its extremely large
           (> 10000 (abs (- (point) (mark t)))))
      (cons (buffer-substring-no-properties
             (region-beginning) (region-end))
            (apply app))
    (apply app)))

(defun conn--push-mark-ad (&rest _)
  (unless (or conn--ephemeral-mark
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring (mark-marker)))
  (setq conn--ephemeral-mark nil))

(defun conn--pop-mark-ad (&rest _)
  (unless (or conn--ephemeral-mark
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring (mark-marker) t))
  (setq conn--ephemeral-mark t))

(defun conn--set-mark-ad (&rest _)
  (setq conn--ephemeral-mark nil))

(defvar conn--saved-ephemeral-marks nil)

(defun conn--save-ephemeral-mark-ad (&rest _)
  (push conn--ephemeral-mark conn--saved-ephemeral-marks))

(defun conn--restore-ephemeral-mark-ad (&rest _)
  (setq conn--ephemeral-mark (pop conn--saved-ephemeral-marks)))

(defun conn--setup-advice ()
  (if conn-mode
      (progn
        (advice-add 'toggle-input-method :around #'conn--toggle-input-method-ad)
        (advice-add 'query-replace-read-from-suggestions :around
                    #'conn--read-from-suggestions-ad)
        (advice-add 'read-regexp-suggestions :around
                    #'conn--read-from-suggestions-ad)
        (advice-add 'push-mark :before #'conn--push-mark-ad)
        (advice-add 'pop-mark :before #'conn--pop-mark-ad)
        (advice-add 'set-mark :after #'conn--set-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'toggle-input-method #'conn--toggle-input-method-ad)
    (advice-remove 'query-replace-read-from-suggestions
                   #'conn--read-from-suggestions-ad)
    (advice-remove 'read-regexp-suggestions
                   #'conn--read-from-suggestions-ad)
    (advice-remove 'set-mark #'conn--set-mark-ad)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)))


;;;; Mode Definition

(defun conn--clone-buffer-setup ()
  (setq conn--mark-cursor nil)
  (dolist (ov (when (mark t)
                (conn--overlays-in-of-type (mark t) (1+ (mark t))
                                           'conn--mark-cursor)))
    (delete-overlay ov))
  (conn-copy-narrow-ring)
  (conn-copy-movement-ring)
  (conn-copy-mark-ring)
  (conn-copy-emacs-state-ring))

(defun conn--setup-keymaps ()
  (if conn-mode
      (progn
        (cl-pushnew 'conn--state-map emulation-mode-map-alists)
        (cl-pushnew 'conn--major-mode-map emulation-mode-map-alists)
        (cl-pushnew 'conn--minor-mode-maps emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (seq-difference '(conn--state-map
                            conn--major-mode-map
                            conn--minor-mode-maps)
                          emulation-mode-map-alists #'eq))))

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :lighter (:eval (conn--get-lighter))
  :group 'conn
  :keymap conn-local-mode-map
  (conn--input-method-mode-line)
  (if conn-local-mode
      (progn
        (setq conn-current-state nil)
        (kill-local-variable 'conn--state-stack)
        (make-local-variable 'conn-lighter)
        (setq-local conn--state-map (list (list 'conn-local-mode))
                    conn--major-mode-map (list (list 'conn-local-mode))
                    conn-emacs-state-ring
                    (conn-make-ring 8 :cleanup (lambda (mk) (set-marker mk nil))))
        ;; We would like to be able to do the same to
        ;; query-replace-read-from-regexp-default but it must be
        ;; either nil, a string, a list of strings, or a symbol with a
        ;; function definition.
        (if query-replace-read-from-default
            (add-function :around
                          (local 'query-replace-read-from-default)
                          (lambda (fn &rest args)
                            (or (conn-replace-read-default)
                                (when fn (apply fn args))))
                          `((name . conn-replace-default)))
          (setq query-replace-read-from-default 'conn-replace-read-default))
        (unless (mark t)
          (conn--push-ephemeral-mark (point) t nil))
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'isearch-mode-hook 'conn--isearch-input-method nil t)
        (setq conn--input-method current-input-method)
        (or (run-hook-with-args-until-success 'conn-setup-state-hook)
            (conn-push-state 'conn-emacs-state)))
    (conn--state-call-deferred)
    (conn--clear-overlays)
    (if (eq 'conn-replace-read-default query-replace-read-from-default)
        (setq query-replace-read-from-default 'conn-replace-read-default)
      (remove-function (local 'query-replace-read-from-default)
                       'conn-replace-default))
    (remove-hook 'change-major-mode-hook #'conn--clear-overlays t)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (remove-hook 'isearch-mode-hook 'conn--isearch-input-method t)
    (when (and conn--input-method (not current-input-method))
      (activate-input-method conn--input-method))))

(defun conn--initialize-buffer ()
  (conn-local-mode 1))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn--initialize-buffer
  :group 'conn
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (if conn-mode
        (progn
          ;; TODO: don't do this unconditionally
          (keymap-set minibuffer-mode-map "M-Y" 'conn-yank-region-to-minibuffer)
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50)
          (add-hook 'clone-buffer-hook 'conn--clone-buffer-setup)
          (add-hook 'clone-indirect-buffer-hook 'conn--clone-buffer-setup))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook)
      (remove-hook 'clone-buffer-hook 'conn--clone-buffer-setup)
      (remove-hook 'clone-indirect-buffer-hook 'conn--clone-buffer-setup)
      (when (eq (keymap-lookup minibuffer-mode-map "M-Y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "M-Y"))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook))))

(provide 'conn)


;;; Misc

(cl-pushnew (list nil (concat "^\\s-*("
                              (eval-when-compile
                                (regexp-opt
                                 '("conn-define-state"
                                   "conn-define-mark-command")
                                 t))
                              "\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                  2)
            lisp-imenu-generic-expression :test #'equal)


;;; Load Extensions

;;;; Calc

(with-eval-after-load 'calc
  (declare-function calc-dispatch "calc")

  (defun conn--calc-dispatch-ad (&rest app)
    (conn-with-recursive-stack 'conn-null-state
      (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))


;;;; Completion

(defun conn--exit-completion ()
  (conn-state-defer-once
    (completion-in-region-mode -1)))
(add-hook 'completion-in-region-mode-hook 'conn--exit-completion)


;;;; Eldoc

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'conn-dispatch))


;;;; Edebug

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-push-state 'conn-emacs-state)
      (conn-pop-state)))
  (add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state))


;;;; Outline

(declare-function outline-mark-subtree "outline")
(declare-function outline-next-heading "outline")
(declare-function outline-previous-heading "outline")
(declare-function outline-on-heading-p "outline")
(declare-function outline-up-heading "outline")

(conn-register-thing
 'heading
 :bounds-op (lambda ()
              (save-mark-and-excursion
                (outline-mark-subtree)
                (cons (region-beginning) (region-end))))
 :forward-op 'outline-next-visible-heading)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing heading)))
  (conn-dispatch-headings))

(conn-register-thing-commands
 'heading 'conn-discrete-thing-handler
 'conn-outline-state-prev-heading
 'outline-up-heading
 'outline-next-heading
 'outline-next-visible-heading
 'outline-previous-visible-heading
 'outline-previous-heading
 'outline-forward-same-level
 'outline-backward-same-level)

(cl-defmethod conn-bounds-of-subr ((_cmd (eql conn-outline-state-prev-heading)) arg)
  (conn-bounds-of-subr 'outline-previous-visible-heading arg))

(define-minor-mode conntext-outline-mode
  "Minor mode for contextual bindings in outline-mode."
  :global t
  (if conntext-outline-mode
      (add-hook 'conntext-state-hook 'conntext-outline-state -80)
    (remove-hook 'conntext-state-hook 'conntext-outline-state)))

(defun conn-outline-state ()
  (interactive)
  (conn-push-state 'conn-outline-state))

(defun conn-outline-state-prev-heading ()
  (interactive)
  (unless (progn
            (goto-char (pos-bol))
            (looking-at-p outline-regexp))
    (outline-previous-visible-heading 1))
  (conn-push-state 'conn-outline-state))

(defun conntext-outline-state ()
  (when (and outline-minor-mode
             (save-excursion
               (goto-char (pos-bol))
               (looking-at-p outline-regexp)))
    (goto-char (pos-bol))
    (conn-push-state 'conn-outline-state)
    t))

(define-keymap
  :keymap (conn-get-state-map 'conn-outline-state)
  :suppress t
  ;; TODO: write an insert heading command that works in this state
  "*" 'conn-outline-insert-heading
  "<backspace>" 'conn-scroll-down
  ";" 'conn-wincontrol
  "." 'point-to-register
  "/" (conn-remap-key conn-undo-keys t)
  "?" (conn-remap-key conn-undo-redo-keys t)
  "DEL" 'conn-scroll-down
  "SPC" 'conn-scroll-up
  "W" 'widen
  "<escape>" 'conn-pop-state
  "J" 'outline-promote
  "L" 'outline-demote
  "O" 'outline-move-subtree-down
  "C-SPC" 'conn-set-mark-command
  "U" 'outline-move-subtree-up
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'outline-show-branches
  "c" (conn-remap-key "C-c" t)
  "d h" 'outline-hide-by-heading-regexp
  "d s" 'outline-show-by-heading-regexp
  "e" 'conn-pop-state
  "f" 'conn-dispatch
  "g" (conn-remap-key "M-g" t)
  "h" 'conn-wincontrol-one-command
  "i" 'outline-previous-visible-heading
  "j" 'outline-backward-same-level
  "k" 'outline-next-visible-heading
  "l" 'outline-forward-same-level
  "m" 'outline-show-subtree
  "n" 'outline-hide-leaves
  "o" 'outline-hide-other
  "p" 'conn-register-prefix
  "q" 'conn-transpose-regions
  "r" (conn-remap-key "<conn-region-map>")
  "s" (conn-remap-key "M-s" t)
  "t" 'outline-hide-body
  "u" 'outline-up-heading
  "v" 'conn-toggle-mark-command
  "w" 'conn-kill-region
  "x" (conn-remap-key "C-x" t)
  "y" 'outline-show-all
  "z" 'conn-exchange-mark-command)


;;;; Dired

(conn-define-state conn-dired-dispatch-state (conn-dispatch-state)
  "State for dispatch in `dired-mode'."
  :cursor 'box
  :suppress-input-method t)

(defun conn-dired-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-eval-with-state 'conn-dired-dispatch-state
      (conn-perform-dispatch
       & (conn-dispatch-action-argument)
       && (conn-thing-argument)
       :repeat & (conn-dispatch-repeat-argument)
       :restrict-windows & (conn-dispatch-restrict-windows-argument)
       :other-end :no-other-end)
    :prefix initial-arg
    :prompt "Dired Dispatch"))

(define-keymap
  :keymap (conn-get-state-map 'conn-dired-dispatch-state)
  "f" 'conn-dispatch-dired-mark
  "w" 'conn-dispatch-dired-kill-line
  "d" 'conn-dispatch-dired-kill-subdir)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'dired-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'dired-find-alternate-file
  "b" 'dired-up-directory
  "k" 'dired-next-line
  "i" 'dired-previous-line
  "/" 'dired-undo
  "I" 'dired-tree-up
  "l" 'dired-next-dirline
  "j" 'dired-prev-dirline
  "m" 'dired-next-subdir
  "n" 'dired-prev-subdir
  ">" 'dired-next-marked-file
  "<" 'dired-prev-marked-file
  "K" 'dired-tree-down
  "F" 'dired-create-empty-file
  "TAB" 'dired-maybe-insert-subdir
  "M-TAB" 'dired-kill-subdir
  "w" 'dired-do-kill-lines
  "s" (conn-remap-key "M-s" t)
  "r" (conn-remap-key "%")
  "," (conn-remap-key "*")
  "x" (conn-remap-key "C-x" t)
  "f" 'conn-dired-dispatch-state
  "M-SPC" 'dired-toggle-marks
  "C-M-l" 'dired-do-redisplay
  "z" 'dired-goto-file
  ";" 'conn-wincontrol
  "`" 'conn-wincontrol-mru-window
  "v" 'dired-mark
  "c" 'dired-unmark
  "u" 'dired-do-delete
  "M-w" 'dired-copy-filename-as-kill
  "RET" 'dired-find-file
  "o" 'dired-find-file-other-window
  "M-o" 'conn-pop-mark-ring
  "M-u" 'conn-unpop-mark-ring
  "* p" 'dired-sort-toggle-or-edit
  "* e" 'dired-mark-executables
  "* l" 'dired-mark-symlinks
  "* d" 'dired-mark-directories
  "* r" 'dired-mark-files-regexp
  "% c" 'dired-do-copy-regexp
  "% h" 'dired-do-hardlink-regexp
  "% s" 'dired-do-symlink-regexp
  "% y" 'dired-do-relsymlink-regexp
  "% t" 'dired-flag-garbage-files
  "M-s s" 'dired-do-isearch
  "M-s c" 'dired-do-isearch-regexp
  "M-s q" 'dired-do-find-regexp
  "M-s r" 'dired-do-find-regexp-and-replace)

(defvar dired-subdir-alist)
(defvar dired-movement-style)
(defvar dired-mode-map)

(declare-function dired-mark "dired")
(declare-function dired-unmark "dired")
(declare-function dired-next-line "dired")
(declare-function dired-next-dirline "dired")
(declare-function dired-marker-regexp "dired")
(declare-function dired-kill-subdir "dired-aux")
(declare-function dired-kill-line "dired-aux")

(conn-set-mode-property 'dired-mode :hide-mark-cursor t)

(defun conn--dispatch-dired-lines ()
  (let ((dired-movement-style 'bounded))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-min))
        (while (/= (point)
                   (progn
                     (dired-next-line 1)
                     (point)))
          (conn-make-target-overlay (point) 0))))))

(defun conn--dispatch-dired-dirline ()
  (save-excursion
    (with-restriction (window-start) (window-end)
      (goto-char (point-min))
      (while (/= (point)
                 (progn
                   (dired-next-dirline 1)
                   (point)))
        (conn-make-target-overlay (point) 0)))))

(defun conn--dispatch-dired-subdir ()
  (let ((start (window-start))
        (end (window-end)))
    (save-excursion
      (pcase-dolist (`(,_ . ,marker) dired-subdir-alist)
        (when (<= start marker end)
          (goto-char marker)
          (conn-make-target-overlay
           (+ 2 marker) (- (line-end-position) marker 2)))))))

(conn-register-thing-commands
 'dired-line nil
 'dired-previous-line 'dired-next-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-line)))
  'conn--dispatch-dired-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-line)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'dired-subdir nil
 'dired-next-subdir 'dired-prev-subdir
 'dired-tree-up 'dired-tree-down)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-subdir)))
  'conn--dispatch-dired-subdir)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-subdir)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'dired-dirline nil
 'dired-next-dirline 'dired-prev-dirline)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-dirline)))
  'conn--dispatch-dired-dirline)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-dirline)))
  (conn-make-action 'conn-dispatch-jump))

(oclosure-define (conn-dispatch-dired-mark
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-mark)))
  (oclosure-lambda (conn-dispatch-dired-mark
                    (description "Mark")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg)
    (with-selected-window window
      (save-excursion
        (let ((regexp (dired-marker-regexp)))
          (goto-char pt)
          (goto-char (line-beginning-position))
          (if (looking-at regexp)
              (dired-unmark 1)
            (dired-mark 1)))))))

(oclosure-define (conn-dispatch-dired-kill-line
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-kill-line)))
  (oclosure-lambda (conn-dispatch-dired-kill-line
                    (description "Kill Line")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-line)))))

(oclosure-define (conn-dispatch-dired-kill-subdir
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-kill-subdir)))
  (oclosure-lambda (conn-dispatch-dired-kill-subdir
                    (description "Kill Subdir")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-subdir)))))


;;;; Magit

(conn-set-mode-property 'magit-section-mode :hide-mark-cursor t)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'magit-section-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "," 'magit-dispatch
  "i" 'magit-section-backward
  "k" 'magit-section-forward
  "w" 'magit-delete-thing
  "p" 'magit-reset-quickly
  "n" 'magit-gitignore
  "`" 'conn-wincontrol-mru-window
  "@" 'magit-am
  "x" (conn-remap-key "C-x" t))


;;;; Ibuffer

(conn-define-state conn-ibuffer-dispatch-state (conn-dispatch-mover-state)
  "State for dispatch in `ibuffer-mode'."
  :cursor '(bar . 4)
  :hide-mark-cursor t
  :suppress-input-method t)

(conn-set-mode-property 'ibuffer-mode :hide-mark-cursor t)

(defvar ibuffer-movement-cycle)
(defvar ibuffer-marked-char)

(declare-function ibuffer-backward-line "ibuffer")
(declare-function ibuffer-unmark-forward "ibuffer")
(declare-function ibuffer-mark-forward "ibuffer")
(declare-function ibuffer-current-mark "ibuffer")
(declare-function ibuffer-backward-filter-group "ibuffer")

(defun conn--dispatch-ibuffer-lines ()
  (let ((ibuffer-movement-cycle nil))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-max))
        (while (/= (point)
                   (progn
                     (ibuffer-backward-line)
                     (point)))
          (unless (get-text-property (point) 'ibuffer-filter-group-name)
            (conn-make-target-overlay (point) 0)))))))

(defun conn--dispatch-ibuffer-filter-group ()
  (let ((ibuffer-movement-cycle nil))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-max))
        (while (/= (point)
                   (progn
                     (ibuffer-backward-filter-group)
                     (point)))
          (conn-make-target-overlay (point) 0))))))

(conn-register-thing-commands
 'ibuffer-line nil
 'ibuffer-backward-line 'ibuffer-forward-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-line)))
  'conn--dispatch-ibuffer-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-line)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'ibuffer-filter-group nil
 'ibuffer-forward-filter-group
 'ibuffer-backward-filter-group)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-filter-group)))
  'conn--dispatch-ibuffer-filter-group)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-filter-group)))
  (conn-make-action 'conn-dispatch-jump))

(oclosure-define (conn-dispatch-ibuffer-mark
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-ibuffer-mark)))
  (oclosure-lambda (conn-dispatch-ibuffer-mark
                    (description "Mark")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'ibuffer-mode))))
      (window pt _thing _thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (if (or (null (ibuffer-current-mark))
                (= (ibuffer-current-mark) ? ))
            (ibuffer-mark-forward nil nil 1)
          (ibuffer-unmark-forward nil nil 1))))))

(keymap-set (conn-get-major-mode-map 'conn-dispatch-state 'ibuffer-mode)
            "f" 'conn-dispatch-ibuffer-mark)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'ibuffer-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  ";" 'conn-wincontrol
  "/" 'ibuffer-do-revert
  "`" 'conn-wincontrol-mru-window
  "y" 'ibuffer-yank
  "z" 'ibuffer-jump-to-buffer
  "r" (conn-remap-key "%")
  "," (conn-remap-key "*")
  "l" 'ibuffer-forward-filter-group
  "j" 'ibuffer-backward-filter-group
  "m" 'ibuffer-jump-to-filter-group
  "n" 'conn-ibuffer-filter-prefix
  "f" 'conn-dispatch
  "k" 'ibuffer-forward-line
  "i" 'ibuffer-backward-line
  "w" 'ibuffer-do-kill-lines
  "u" 'ibuffer-do-kill-on-deletion-marks
  "x" (conn-remap-key "C-x" t)
  "s" (conn-remap-key "M-s" t)
  "t a" 'ibuffer-do-sort-by-alphabetic
  "t f" 'ibuffer-do-sort-by-filename/process
  "t i" 'ibuffer-invert-sorting
  "t m" 'ibuffer-do-sort-by-major-mode
  "t s" 'ibuffer-do-sort-by-size
  "t v" 'ibuffer-do-sort-by-recency
  "M-s i r" 'ibuffer-do-isearch-regexp
  "M-s i s" 'ibuffer-do-isearch
  "M-s i o" 'ibuffer-do-occur
  "M-w" 'ibuffer-copy-filename-as-kill
  "<" 'ibuffer-forward-next-marked
  ">" 'ibuffer-backwards-next-marked
  "M-SPC" 'ibuffer-toggle-marks
  "C-M-l" 'ibuffer-redisplay
  "v" 'ibuffer-mark-forward
  "c" 'ibuffer-unmark-forward
  "C" 'ibuffer-unmark-backward
  "o" 'ibuffer-visit-buffer-other-window
  "RET" 'ibuffer-visit-buffer)


;;;; Markdown

(conn-register-thing
 'md-paragraph
 :forward-op 'markdown-forward-paragraph)

(conn-register-thing-commands
 'md-paragraph 'conn-continuous-thing-handler
 'markdown-forward-paragraph
 'markdown-backward-paragraph)

;; TODO: other markdown things


;;;; Treesit

(static-if (<= 30 emacs-major-version)
    (conn-register-thing-commands
     'defun 'conn-continuous-thing-handler
     'treesit-end-of-defun
     'treesit-beginning-of-defun))


;;;; Help

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'help-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'helpful-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Info

(declare-function Info-prev-reference "info")
(declare-function Info-follow-nearest-node "info")

(oclosure-define (conn-action-info-ref
                  (:parent conn-action)))

(defun conn-dispatch-on-info-refs ()
  (interactive)
  (conn-perform-dispatch
   (oclosure-lambda (conn-action-info-ref
                     (description "Info Refs")
                     (window-predicate
                      (lambda (win)
                        (eq 'Info-mode
                            (buffer-local-value 'major-mode
                                                (window-buffer win))))))
       (win pt _thing _thing-arg)
     (select-window win)
     (goto-char pt)
     (Info-follow-nearest-node))
   (lambda ()
     (dolist (win (conn--get-target-windows))
       (with-selected-window win
         (save-excursion
           (let ((last-pt (goto-char (window-end))))
             (while (and (> last-pt (progn
                                      (Info-prev-reference)
                                      (setq last-pt (point))))
                         (<= (window-start) (point) (window-end)))
               (conn-make-target-overlay (point) 0)))))))
   nil nil
   :other-end :no-other-end))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'Info-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "o" 'Info-history-back
  "u" 'Info-history-forward
  "m" 'Info-next
  "n" 'Info-prev
  "k" 'Info-scroll-up
  "i" 'Info-scroll-down
  "l" 'Info-forward-node
  "j" 'Info-backward-node
  "r" 'Info-up
  "a" 'execute-extended-command
  "p" 'Info-menu
  "z" 'Info-toc
  "f" 'dispatch-on-info-refs
  "v" 'Info-index
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Treemacs

(conn-set-mode-property 'treemacs-mode :hide-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'treemacs-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'treemacs-select-window
  "i" 'treemacs-previous-line
  "k" 'treemacs-next-line
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Messages

(conn-set-mode-property 'messages-buffer-mode :hide-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'messages-buffer-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "`" 'conn-wincontrol-mru-window
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Debugger mode

(conn-set-mode-property 'debugger-mode :hide-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'debugger-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Occur mode

(conn-set-mode-property 'occur-mode :hide-mark-cursor t)
(conn-set-mode-property 'occur-edit-mode :hide-mark-cursor nil)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'occur-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;;; Compile mode

(conn-set-mode-property 'compilation-mode :hide-mark-cursor t)
(static-if (<= 31 emacs-major-version)
    (conn-set-mode-property 'grep-edit-mode :hide-mark-cursor nil))
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'compilation-mode)
  "SPC <t>" conn-demap-key
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;; Footer
;; Local Variables:
;; outline-regexp: "^;;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn.el ends here
