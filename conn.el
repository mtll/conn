;;; conn.el --- A modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") (transient "0.8.7") (seq "2.23"))
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
(static-if (<= 31 emacs-major-version)
    (require 'subr-x)
  (eval-when-compile
    (require 'subr-x)))
(eval-when-compile
  (require 'inline)
  (require 'cl-lib)
  (require 'map))


;;;; Declerations

(defvar conn-mode)
(defvar conn-local-mode)
(defvar conn-bounds-of-command-alist)

(defvar-local conn--hide-mark-cursor nil)

(declare-function kmacro-p "kmacro")
(declare-function kmacro-step-edit-macro "kmacro")
(declare-function project-files "project")
(declare-function conn-dispatch-kapply-prefix "conn-transients")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")


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

(defcustom conn-lighter " Conn"
  "Default modeline lighter for conn-mode."
  :type '(choice string (const nil))
  :group 'conn-states)


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

(eval-and-compile
  (defun conn--stringify (&rest symbols-or-strings)
    "Concatenate all SYMBOLS-OR-STRINGS to create a new symbol."
    (cl-loop for s in symbols-or-strings
             concat (pcase s
                      ((pred stringp) s)
                      ('nil "")
                      ((pred symbolp) (symbol-name s)))))

  (defun conn--symbolicate (&rest symbols-or-strings)
    "Concatenate all SYMBOLS-OR-STRINGS to create a new symbol."
    (intern (apply #'conn--stringify symbols-or-strings))))

(defmacro conn-protected-let* (varlist &rest body)
  "Bind variables according to VARLIST then eval body as in `let*'.

In addition to what `let*' accepts, each element of VARLIST may also be
of the form (SYMBOL VALUEFORM . CLEANUP-FORMS), which binds SYMBOL to
VALUEFORM and if BODY exits non-locally runs CLEANUP-FORMS.

CLEANUP-FORMS are run in reverse order of their appearance in VARLIST."
  (declare (indent 1))
  (cl-with-gensyms (success unbound)
    (let (setf-forms
          let-forms
          unwind-body)
      (dolist (form varlist)
        (pcase form
          ((and `(,var ,binding . ,cleanup)
                (guard cleanup))
           (push `(setf ,var ,binding) setf-forms)
           (setq unwind-body (if unwind-body
                                 `(unwind-protect
                                      ,unwind-body
                                    (unless (eq ',unbound ,var)
                                      ,@cleanup))
                               `(unless (eq ',unbound ,var)
                                  ,@cleanup)))
           (push `(,var ',unbound) let-forms))
          (_ (push form let-forms))))
      `(let* (,@(nreverse let-forms)
              (,success nil))
         (unwind-protect
             (prog2
                 ,(macroexp-progn (reverse setf-forms))
                 ,(macroexp-progn body)
               (setq ,success t))
           (unless ,success
             ,unwind-body))))))

(defmacro conn-make-thing-command (name region-fn &rest body)
  (declare (indent 2))
  `(progn
     (defalias ',name ,region-fn)
     (advice-add ',name :around
                 (lambda (&rest app)
                   (save-mark-and-excursion
                     ,@body
                     (apply app))))))

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
      (with-work-buffer
        ;; Setup current buffer to correctly compute pixel width.
        (when buffer
          (dolist (v '(face-remapping-alist
                       char-property-alias-alist
                       default-text-properties))
            (if (local-variable-p v buffer)
                (set (make-local-variable v)
                     (buffer-local-value v buffer)))))
        ;; Avoid deactivating the region as side effect.
        (let (deactivate-mark)
          (insert string))
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


;;;;; Rings

(cl-defstruct (conn-ring
               (:constructor conn--make-ring (capacity cleanup)))
  "A ring that removes elements in least recently visited order."
  list
  history
  capacity
  cleanup)

(cl-defun conn-ring (capacity &key cleanup)
  (cl-assert (and (integerp capacity)
                  (> capacity 0)))
  (conn--make-ring capacity cleanup))

(define-inline conn-ring--visit (ring item)
  (inline-quote
   (setf (conn-ring-history ,ring)
         (cons ,item (delete ,item (conn-ring-history ,ring))))))

(defun conn-ring-insert-front (ring item)
  "Insert ITEM into front of RING."
  (setf (conn-ring-list ring)
        (cons item (delete item (conn-ring-list ring))))
  (conn-ring--visit ring item)
  (when-let* ((old (drop (conn-ring-capacity ring) (conn-ring-history ring))))
    (setf (conn-ring-history ring)
          (take (conn-ring-capacity ring) (conn-ring-history ring)))
    (dolist (mk old)
      (setf (conn-ring-list ring) (delete mk (conn-ring-list ring)))
      (when-let* ((cleanup (conn-ring-cleanup ring)))
        (funcall cleanup mk)))))

(defun conn-ring-insert-back (ring item)
  "Insert ITEM into back of RING."
  (conn-ring-insert-front ring item)
  (conn-ring-rotate-backward ring))

(defun conn-ring-rotate-forward (ring)
  "Rotate ring forward.

Takes (1 2 3 4) to (2 3 4 1)."
  (let ((head (car (setf (conn-ring-list ring)
                         (nconc (cdr (conn-ring-list ring))
                                (list (car (conn-ring-list ring))))))))
    (conn-ring--visit ring head)
    head))

(defun conn-ring-rotate-backward (ring)
  "Rotate ring backward.

Takes (1 2 3 4) to (4 1 2 3)."
  (let ((head (car (setf (conn-ring-list ring)
                         (nconc (last (conn-ring-list ring))
                                (butlast (conn-ring-list ring)))))))
    (conn-ring--visit ring head)
    head))

(defun conn-ring-head (ring)
  "Return the front element of RING.

If ring is (1 2 3 4) 1 would be returned."
  (car (conn-ring-list ring)))

(defun conn-ring-tail (ring)
  "Return the back element of RING.

If ring is (1 2 3 4) 4 would be returned."
  (car (last (conn-ring-list ring))))

(defun conn-ring-delete (ring elem)
  (setf (conn-ring-list ring) (delete elem (conn-ring-list ring))
        (conn-ring-history ring) (delete elem (conn-ring-history ring))))


;;;;; Keymap Utils

(defmacro conn--without-conn-maps (&rest body)
  "Run BODY without any state, mode, or local maps active."
  (declare (debug (body))
           (indent 0))
  `(let ((emulation-mode-map-alists
          (seq-difference emulation-mode-map-alists
                          '(conn--local-minor-mode-maps
                            conn--local-major-mode-map
                            conn--local-state-map
                            conn--local-override-map)
                          #'eq)))
     ,(macroexp-progn body)))

(defvar conn-demap-key
  `(menu-item
    "Demap key"
    nil
    :filter ,(lambda (_real-binding)
               (conn--without-conn-maps
                 (key-binding (vector last-input-event) t)))))

(defun conn-remap-key (from-keys &optional without-conn-maps)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  (let ((from-keys (key-parse from-keys)))
    `(menu-item
      ,(format "Remap %s" (key-description from-keys))
      ,(conn--without-conn-maps (key-binding from-keys t))
      :filter ,(lambda (_real-binding)
                 (if without-conn-maps
                     (conn--without-conn-maps
                       (key-binding from-keys t))
                   (key-binding from-keys t))))))

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
                                              (flatten-tree ,regions)
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

(defun conn--merge-regions (regions &optional points)
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

(defun conn-get-mode-property (mode propname)
  (cl-with-gensyms (key-missing)
    (cl-loop for mode in (conn--derived-mode-all-parents mode)
             for prop = (if-let* ((table (get mode :conn-properties)))
                            (gethash propname table key-missing)
                          key-missing)
             unless (eq key-missing prop) return prop)))

(defun conn-set-mode-property (mode prop value)
  (let ((table (or (get mode :conn-properties)
                   (put mode :conn-properties (make-hash-table :test 'eq)))))
    (puthash prop value table)))

(gv-define-setter conn-get-mode-property (value mode prop)
  `(conn-set-mode-property ,mode ,prop ,value))


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
               collect (cons (conn--create-marker (match-beginning 0))
                             (conn--create-marker (match-end 0)))
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

(defun conn--overlay-start-marker (ov)
  "Return marker pointing to the start of overlay OV."
  (conn--create-marker (overlay-start ov) (overlay-buffer ov)))

(defun conn--overlay-end-marker (ov)
  "Return marker pointing to the end of overlay OV."
  (conn--create-marker (overlay-end ov) (overlay-buffer ov)))

(defun conn--overlay-bounds-markers (ov)
  "Return (BEG . END) where BEG and END are markers at each end of OV."
  (cons (conn--overlay-start-marker ov)
        (conn--overlay-end-marker ov)))

(defun conn--clear-overlays ()
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (remove-overlays nil nil 'conn-overlay t)))


;;;;; Append/Prepend Keymaps

(defun conn--append-keymap-parent (keymap new-parent)
  "Append NEW-PARENT to KEYMAP\\='s parents."
  (if-let* ((parent (keymap-parent keymap)))
      (set-keymap-parent keymap (append parent (list new-parent)))
    (set-keymap-parent keymap (make-composed-keymap new-parent))))

(defun conn--remove-keymap-parent (keymap parent-to-remove)
  "Remove PARENT-TO-REMOVE from KEYMAP\\='s parents."
  (when-let* ((parent (keymap-parent keymap)))
    (setf (cdr parent) (delq parent-to-remove (cdr parent)))
    (unless (cdr parent)
      (set-keymap-parent keymap nil))))


;;;;; Thing Command Type

(defun conn-thing-command-p (cmd)
  (and (symbolp cmd)
       (not (and (not (alist-get cmd conn-bounds-of-command-alist))
                 (not (get cmd :conn-command-thing))))))

(cl-deftype conn-thing-command () '(satisfies conn-thing-command-p))


;;;; States

(defface conn-read-thing-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

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

(defvar conn-setup-state-hook nil)

(defvar-local conn-hide-mark-alist nil)

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar-local conn--state-stack nil
  "Previous conn states in buffer.")

(define-inline conn-state-p (state)
  "Return non-nil if STATE is a conn-state."
  (inline-quote
   (eq 'conn-state (type-of (get ,state :conn--state)))))

(cl-deftype conn-state () '(satisfies conn-state-p))

(define-inline conn--state-parents (state)
  "Return only the immediate parents for STATE."
  (inline-letevals (state)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (aref (get ,state :conn--state) 2)))))

(define-inline conn--state-all-children (state)
  "Return all parents for STATE."
  (inline-letevals (state)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (aref (get ,state :conn--state) 3)))))

(defconst conn--state-all-parents-cache (make-hash-table :test 'eq))

(define-inline conn--state-all-parents (state)
  "Return all parents of state.

The returned list is not fresh, don't modify it."
  (inline-letevals (state)
    (inline-quote
     (with-memoization (gethash ,state conn--state-all-parents-cache)
       (cl-check-type ,state conn-state)
       (cons ,state
             (merge-ordered-lists
              (mapcar 'conn--state-all-parents
                      (aref (get ,state :conn--state) 2))))))))

(define-inline conn-substate-p (state parent)
  "Return non-nil if STATE is a substate of PARENT."
  (inline-letevals (state parent)
    (inline-quote
     (memq ,parent (conn--state-all-parents ,state)))))

(defun conn-setup-commit-state ()
  (when (buffer-match-p "COMMIT_EDITMSG" (current-buffer))
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-commit-state -80)

(defun conn-setup-edmacro-state ()
  (when (buffer-match-p "\\*Edit Macro\\*" (current-buffer))
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-commit-state -80)

(defun conn-setup-dired-state ()
  (when (derived-mode-p 'dired-mode)
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-dired-state -50)

(defun conn-setup-null-state ()
  (when (derived-mode-p conn-null-state-modes)
    (conn-push-state 'conn-null-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-null-state -90)

(defun conn-setup-command-state ()
  (when (derived-mode-p conn-command-state-modes)
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-command-state)

(defun conn-setup-minibuffer-state ()
  (when (eq major-mode 'minibuffer-mode)
    (setf (alist-get 'conn-emacs-state conn-hide-mark-alist) t)
    (conn-push-state 'conn-emacs-state)
    (letrec ((hook (lambda ()
                     (conn--push-ephemeral-mark)
                     (remove-hook 'minibuffer-setup-hook hook))))
      (add-hook 'minibuffer-setup-hook hook))
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-command-state -95)


;;;;; State Keymap Impl

(defvar conn--state-maps (make-hash-table :test 'eq))
(defvar conn--override-maps (make-hash-table :test 'eq))
(defvar conn--major-mode-maps (make-hash-table :test 'eq))
(defvar conn--minor-mode-maps (make-hash-table :test 'eq))

(defvar-local conn--local-state-map)
(defvar-local conn--local-override-map)
(defvar-local conn--local-major-mode-map)
(defvar-local conn--local-minor-mode-maps)

(defun conn-get-state-map (state)
  "Return the state keymap for STATE."
  (cl-check-type state conn-state)
  (gethash state conn--state-maps))

(defconst conn--state-map-cache (make-hash-table :test 'eq))

(defun conn--compose-state-map (state)
  "Return composed state map for STATE.

The composed keymap is of the form:

(keymap
 (keymap . bindings)  ;; state map for STATE
 (keymap . bindings)  ;; state map for STATE parent
 ...)"
  (with-memoization (gethash state conn--state-map-cache)
    (make-composed-keymap
     (cl-loop for pstate in (conn--state-all-parents state)
              collect (gethash pstate conn--state-maps)))))

(defun conn-get-overriding-map (state)
  "Return the overriding keymap for STATE."
  (cl-check-type state conn-state)
  (gethash state conn--override-maps))

(defconst conn--override-map-cache (make-hash-table :test 'eq))

(defun conn--compose-overide-map (state)
  "Return composed override map for STATE.

Composed keymap is of the same form as returned by
`conn--compose-state-map'."
  (with-memoization (gethash state conn--override-map-cache)
    (make-composed-keymap
     (cl-loop for pstate in (conn--state-all-parents state)
              collect (gethash pstate conn--override-maps)))))

(defun conn-get-major-mode-map (state mode)
  "Return keymap for major MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (cl-check-type state conn-state)
  (cl-assert (symbolp mode))
  (or (when-let* ((mmode-table (gethash state conn--major-mode-maps)))
        (nth 1 (gethash mode mmode-table)))
      (let* ((keymap
              (make-composed-keymap
               (make-sparse-keymap)
               (cl-loop for parent in (cdr (conn--state-all-parents state))
                        collect (conn-get-major-mode-map parent mode)))))
        (setf (gethash mode (gethash state conn--major-mode-maps))
              keymap)
        (dolist (child (conn--state-all-children state))
          (conn-get-major-mode-map child mode))
        (nth 1 keymap))))

(defconst conn--major-mode-maps-cache (make-hash-table :test 'equal))

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
  (with-memoization
      (gethash (cons state (conn--derived-mode-all-parents major-mode))
               conn--major-mode-maps-cache)
    (make-composed-keymap
     (cl-loop for pmode in (conn--derived-mode-all-parents major-mode)
              collect (progn
                        (conn-get-major-mode-map state pmode)
                        (gethash pmode (gethash state conn--major-mode-maps)))))))

(defun conn-get-mode-map (state mode)
  "Return keymap for MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (cl-check-type state conn-state)
  (cl-assert (symbolp mode))
  (or (nth 1 (alist-get mode (cdr (gethash state conn--minor-mode-maps))))
      (let ((keymap
             (make-composed-keymap
              (make-sparse-keymap)
              (cl-loop for parent in (cdr (conn--state-all-parents state))
                       collect (conn-get-mode-map parent mode)))))
        (setf (alist-get mode (cdr (gethash state conn--minor-mode-maps)))
              keymap)
        (dolist (child (conn--state-all-children state))
          (conn-get-mode-map child mode))
        (conn--sort-mode-maps state)
        (nth 1 keymap))))

(defun conn--sort-mode-maps (state)
  (cl-check-type state conn-state)
  (let* ((parents (conn--state-all-parents state))
         (depths (delq nil (mapcar
                            (lambda (parent)
                              (car (gethash parent conn--minor-mode-maps)))
                            parents))))
    (setf (cdr (gethash state conn--minor-mode-maps))
          (compat-call
           sort (cdr (gethash state conn--minor-mode-maps))
           :key (lambda (cons)
                  (or (seq-some (lambda (table) (gethash (car cons) table))
                                depths)
                      0))
           :in-place t))))

(defun conn-set-mode-map-depth (state mode depth)
  (cl-check-type state conn-state)
  (cl-assert (symbolp mode))
  (cl-assert (<= -100 depth 100))
  (let ((map-depths (or (car (gethash state conn--minor-mode-maps))
                        (setf (car (gethash state conn--minor-mode-maps))
                              (make-hash-table :test 'eq)))))
    (unless (eql (gethash mode map-depths) depth)
      (setf (gethash mode map-depths) depth)
      (when (alist-get mode (gethash state conn--minor-mode-maps))
        (mapc #'conn--sort-mode-maps
              (cons state (conn--state-all-children state))))))
  nil)

(defun conn--rebuild-state-keymaps (state)
  "Rebuild all composed keymaps for STATE.

Called when the inheritance hierarchy for STATE changes."
  (let ((parents (conn--state-all-parents state)))
    (when-let* ((state-map (gethash state conn--state-map-cache)))
      (setf (cdr state-map)
            (cl-loop for pstate in parents
                     collect (gethash pstate conn--state-maps))))
    (when-let* ((override-map (gethash state conn--override-map-cache)))
      (setf (cdr override-map)
            (cl-loop for pstate in parents
                     collect (gethash pstate conn--override-maps))))
    (pcase-dolist (`(,mode . ,map) (cdr (gethash state conn--minor-mode-maps)))
      (setf (cdr map)
            (cl-loop for pstate in parents
                     collect (conn-get-mode-map pstate mode))))
    (maphash
     (lambda (mode map)
       (setf (cdr map)
             (cl-loop for pstate in parents
                      collect (conn-get-major-mode-map pstate mode))))
     (gethash state conn--major-mode-maps))))


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
        ((guard (and current-input-method conn--input-method))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title)
         (deactivate-input-method))
        ((guard current-input-method)
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
    (letrec ((hook
              (lambda ()
                (conn--activate-input-method)
                (add-hook 'input-method-activate-hook
                          #'conn--activate-input-method
                          nil t)
                (add-hook 'input-method-deactivate-hook
                          #'conn--deactivate-input-method
                          nil t)
                (remove-hook 'isearch-mode-end-hook hook))))
      (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
      (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
      (let ((overriding-terminal-local-map nil))
        (activate-input-method conn--input-method))
      (setq isearch-input-method-function input-method-function)
      (setq-local input-method-function nil)
      (isearch-update)
      (add-hook 'isearch-mode-end-hook hook))))
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


;;;;; State Properties

(defun conn-state-get (state property &optional no-inherit default)
  "Return the value of PROPERTY for STATE.

If PROPERTY is not set for STATE then check all of STATE's parents for
PROPERTY.  If no parent has that property either than nil is returned."
  (declare (compiler-macro conn--state-get-compiler-macro))
  (if (not no-inherit)
      (cl-with-gensyms (key-missing)
        (cl-loop for parent in (conn--state-all-parents state)
                 for table = (aref (get parent :conn--state) 1)
                 for prop = (gethash property table key-missing)
                 unless (eq prop key-missing) return prop
                 finally return default))
    (cl-check-type state conn-state)
    (gethash property (aref (get state :conn--state) 1) default)))

(defun conn--state-get-compiler-macro (_exp state property &optional no-inherit default)
  (cl-once-only (state property)
    (let ((with-parents
           `(cl-with-gensyms (key-missing)
              (cl-loop for parent in (conn--state-all-parents ,state)
                       for table = (aref (get parent :conn--state) 1)
                       for prop = (gethash ,property table key-missing)
                       unless (eq prop key-missing) return prop
                       finally return ,default)))
          (without-parents
           `(progn
              (cl-check-type ,state conn-state)
              (gethash ,property (aref (get ,state :conn--state) 1) ,default)))
          (no-inherit (macroexpand-all no-inherit macroexpand-all-environment)))
      (if (and (macroexp-const-p no-inherit)
               (if (consp no-inherit) (cadr no-inherit) no-inherit))
          (if no-inherit without-parents with-parents)
        `(if ,no-inherit ,without-parents ,with-parents)))))

(gv-define-setter conn-state-get (value state slot)
  `(conn-state-set ,state ,slot ,value))

(define-inline conn-state-has-property-p (state property)
  "Return t if PROPERTY is set for STATE."
  (inline-letevals (state property)
    (inline-quote
     (cl-with-gensyms (key-missing)
       (cl-check-type ,state conn-state)
       (not (eq (gethash ,property (aref (get ,state :conn--state) 1) key-missing)
                key-missing))))))

(define-inline conn-state-set (state property value)
  "Set the value of PROPERTY in STATE to VALUE.

Returns VALUE."
  (inline-letevals (state)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (puthash ,property ,value (aref (get ,state :conn--state) 1))))))

(define-inline conn-state-unset (state property)
  "Make PROPERTY unset in STATE.

If a property is unset in a state it will inherit the value of that
property from its parents."
  (inline-letevals (state)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (remhash (aref (get ,state :conn--state) 1) ,property)))))


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
  `(and (pred conn-state-p)
        ,@(mapcar
           (static-if (< emacs-major-version 30)
               (lambda (prop)
                 (cond ((consp elt)
                        `(app (pcase--flip conn-state-get ,(car prop))
                              ,(cadr prop)))
                       ((keywordp prop)
                        (let ((var (intern (substring (symbol-name prop) 1))))
                          `(app (pcase--flip conn-state-get ,prop) ,var)))
                       (t `(app (pcase--flip conn-state-get ',prop) ,prop))))
             (lambda (prop)
               (cond ((consp prop)
                      `(app (conn-state-get _ ,(car prop)) ,(cadr prop)))
                     ((keywordp prop)
                      (let ((var (intern (substring (symbol-name prop) 1))))
                        `(app (conn-state-get _ ,prop) ,var)))
                     (t `(app (conn-state-get _ ',prop) ,prop)))))
           properties)))

(pcase-defmacro conn-substate (parent)
  "Matches if EXPVAL is a substate of PARENT."
  `(and (pred conn-state-p)
        (guard (conn-state-p ',parent))
        (pred ,(static-if (< emacs-major-version 30)
                   `(pcase--flip conn-substate-p ',parent)
                 `(conn-substate-p _ ',parent)))))

(defmacro conn-with-recursive-state (state &rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms (buffer)
    `(let ((,buffer (current-buffer)))
       (conn-enter-recursive-state ,state)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer ,buffer
           (conn-exit-recurive-state))))))

(defmacro conn-without-recursive-state (&rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (stack buffer)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (conn-exit-recurive-state)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer ,buffer
           (conn-enter-state (car ,stack))
           (setq conn--state-stack ,stack))))))


;;;;; Cl-Generic Specializers

(cl-generic-define-generalizer conn--substate-generalizer
  90 (lambda (state) `(and (conn-state-p ,state) ,state))
  (lambda (state &rest _)
    (when state
      (mapcar (lambda (parent) `(conn-substate ,parent))
              (conn--state-all-parents state)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-substate)))
  "Support for (conn-substate STATE) specializers.
These match if the argument is a substate of STATE."
  (list conn--substate-generalizer))

(cl-generic-define-generalizer conn--state-generalizer
  15 (lambda (state) `(and (conn-state-p ,state) 'conn-state))
  (lambda (tag &rest _) (when tag (list tag))))

(cl-defmethod cl-generic-generalizers ((_specializer (eql conn-state)))
  "Support for conn-state specializers.
These match if the argument is a conn-state."
  (list conn--state-generalizer))


;;;;; Enter/Exit Functions

(defvar conn-exit-functions nil
  "Abnormal hook run when a state is exited.

Each function is passed the state being exited
(eg '`conn-command-state' or '`conn-emacs-state').

See also `conn-state-entry-functions'.")

(defvar conn-state-entry-functions nil
  "Abnormal hook run when a state is entered.

Each function is passed the state being entered
(eg '`conn-command-state' or '`conn-emacs-state').

See also `conn-exit-functions'.")

(defun conn--get-lighter ()
  (or conn-lighter
      (let ((lighter ""))
        (dolist (s conn--state-stack)
          (setq lighter (if s
                            (concat "→" (conn-state-get s :lighter) lighter)
                          (concat "→[" (substring lighter 1) "]"))))
        (setq conn-lighter (concat " [" (substring lighter 1) "]")))))

(cl-defgeneric conn-exit-state (state)
  "Exit conn state STATE.

Methods can be added to this generic function in order to run code every
time a state is exited.  Conn provides two additional specializers for
methods to facilitate this:

CONN-SUBSTATE: this takes the form (STATE (conn-substate PARENT-STATE))
in an argument list and specializes the method on states that inherit
from PARENT-STATE.

CONN-STATE: this takes the form (STATE conn-state) in a argument list
and specializes the method on all conn states."
  ( :method ((_state (eql nil))) "Noop" nil)
  ( :method ((_state conn-state)) "Noop" nil)
  ( :method (state) (error "Attempting to exit unknown state: %s" state)))

(cl-defmethod conn-exit-state :around ((state conn-state))
  (when (symbol-value state)
    (let ((success nil))
      (unwind-protect
          (progn
            (setq conn-current-state nil
                  cursor-type t)
            (set state nil)
            (cl-call-next-method)
            (setq success t))
        (unless success
          (conn-local-mode -1)
          (message "Error exiting state %s." (symbol-value state)))))
    (run-hook-wrapped
     'conn-state-exit-functions
     (lambda (fn)
       (condition-case err
           (funcall fn state)
         (t
          (remove-hook 'conn-state-exit-functions fn)
          (message "Error in conn-exit-functions %s" (car err))))))))

(cl-defgeneric conn-enter-state (state &key &allow-other-keys)
  "Enter conn state STATE.

Methods can be added to this generic function in order to run code every
time a state is entered.  Conn provides two additional specializers for
methods to facilitate this:

CONN-SUBSTATE: this takes the form (STATE (conn-substate PARENT-STATE))
in an argument list and specializes the method on states that inherit
from PARENT-STATE.

CONN-STATE: this takes the form (STATE conn-state) in a argument list
and specializes the method on all conn states."
  ( :method ((_state (eql nil)) &key &allow-other-keys) "Noop" nil)
  ( :method ((_state conn-state) &key &allow-other-keys) "Noop" nil)
  ( :method (state &key &allow-other-keys)
    (error "Attempting to enter unknown state: %s" state)))

(cl-defmethod conn-enter-state :around ((state conn-state))
  (unless (symbol-value state)
    (let ((success nil))
      (unwind-protect
          (progn
            (when (conn-state-get state :abstract t)
              (error "Attempting to enter abstract state"))
            (conn-exit-state conn-current-state)
            (set state t)
            (setf
             conn-current-state state
             conn--local-override-map `((conn-local-mode . ,(conn--compose-overide-map state)))
             conn--local-state-map `((conn-local-mode . ,(conn--compose-state-map state)))
             conn--local-major-mode-map `((conn-local-mode . ,(conn--compose-major-mode-map state)))
             conn--local-minor-mode-maps (gethash state conn--minor-mode-maps)
             conn--hide-mark-cursor (or (alist-get state conn-hide-mark-alist)
                                        (alist-get t conn-hide-mark-alist)
                                        (conn-get-mode-property major-mode :hide-mark-cursor)
                                        (conn-state-get state :hide-mark-cursor))
             cursor-type (or (conn-state-get state :cursor) t))
            (conn--activate-input-method)
            (cl-call-next-method)
            (unless executing-kbd-macro
              (force-mode-line-update))
            (setq conn-lighter nil)
            (setq success t))
        (unless success
          (conn-local-mode -1)
          (message "Error entering state %s." state))))
    (run-hook-wrapped
     'conn-state-entry-functions
     (lambda (fn)
       (condition-case err
           (funcall fn state)
         (t
          (remove-hook 'conn-state-entry-functions fn)
          (message "Error in conn-state-entry-functions: %s" (car err))))))))

(defun conn-push-state (state)
  (unless (symbol-value state)
    (conn-enter-state state)
    (push state conn--state-stack)))

(defun conn-pop-state ()
  (interactive)
  (if-let* ((state (cadr conn--state-stack)))
      (progn
        (conn-enter-state state)
        (pop conn--state-stack))
    (conn-push-state
     (conn-state-get conn-current-state :pop-alternate
                     t 'conn-command-state))))

(defun conn-peek-stack ()
  (cadr conn--state-stack))

(defun conn-enter-recursive-state (state)
  (conn-enter-state state)
  ;; Ensure the lighter gets updates even if we haven't changed state
  (setq conn-lighter nil)
  (push nil conn--state-stack)
  (push state conn--state-stack))

(defun conn-exit-recurive-state ()
  (interactive)
  (if-let* ((tail (memq nil conn--state-stack)))
      (progn
        (conn-enter-state (cadr tail))
        (setq conn--state-stack (cdr tail)
              ;; Ensure the lighter gets updates
              ;; even if we haven't changed state
              conn-lighter nil))
    (error "Not in a recursive state")))


;;;;; test-State Definitions

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

Code that should be run whenever a state is entered or exited can be
added as methods to `conn-enter-state' and `conn-exit-state', or added
to the abnormal hooks `conn-state-entry-functions' or
`conn-state-exit-functions'.

\(fn NAME PARENTS &optional DOC [KEYWORD VAL ...])"
  (declare (debug ( name form string-or-null-p
                    [&rest keywordp sexp]))
           (indent 2))
  (let ((doc (or (and (stringp (car properties))
                      (pop properties))
                 (conn--stringify "Non-nil when `" name "' is active."))))
    (cl-assert (plistp properties))
    `(progn
       (dolist (parent ',parents)
         (cl-check-type parent conn-state))
       (cl-assert
        (cl-loop for parent in ',parents
                 never (memq ',name (conn--state-all-parents parent)))
        nil "Cycle detected in %s inheritance hierarchy" ',name)
       (defvar-local ,name nil ,doc)
       (let ((state-props
              (cl-loop with kvs = (list ,@properties)
                       with table = (make-hash-table :test 'eq)
                       for (k v) on kvs by #'cddr
                       do (puthash k v table)
                       finally return table))
             (new-parents
              (merge-ordered-lists
               (mapcar 'conn--state-all-parents ',parents))))
         (if-let* ((record (get ',name :conn--state)))
             (let ((old-parents (cdr (conn--state-all-parents ',name)))
                   (all-children (cons ',name (aref record 3))))
               ;; We are redefining a state and must to take care to
               ;; do it transparently.
               (setf (aref record 1) state-props
                     (aref record 2) ',parents)
               ;; Remove all children from all former parents.  We
               ;; will take care of the case where a child inherits
               ;; from some parent through multiple paths next.
               (dolist (former (seq-difference old-parents new-parents))
                 (setf (aref (get former :conn--state) 3)
                       (seq-difference (aref (get former :conn--state) 3)
                                       all-children)))
               ;; Recompute all parents for all children and
               ;; re-register all children with all of their parents.
               ;; If we overzealously removed a child from some parent
               ;; in the previous step we will fix it here.
               (dolist (child all-children)
                 (remhash child conn--state-all-parents-cache)
                 (dolist (parent (cdr (conn--state-all-parents child)))
                   (cl-pushnew child (aref (get parent :conn--state) 3))))
               ;; Rebuild all keymaps for all children with the new
               ;; inheritance hierarchy.  Existing composed keymaps
               ;; are destructively modified so that local map
               ;; variables will not have to be updated in each buffer
               ;; before these changes take effect.
               (mapc #'conn--rebuild-state-keymaps all-children)
               (dolist (parent new-parents)
                 (pcase-dolist (`(,mode . ,_)
                                (cdr (gethash parent conn--minor-mode-maps)))
                   (conn-get-mode-map ',name mode))))
           (let ((record (make-record 'conn-state 3 nil)))
             (setf (aref record 1) state-props
                   (aref record 2) ',parents)
             (put ',name :conn--state record))
           (dolist (parent (cdr (conn--state-all-parents ',name)))
             (cl-pushnew ',name (aref (get parent :conn--state) 3)))
           (setf (gethash ',name conn--state-maps) (make-sparse-keymap)
                 (gethash ',name conn--override-maps) (make-sparse-keymap)
                 (gethash ',name conn--minor-mode-maps) (list nil)
                 (gethash ',name conn--major-mode-maps) (make-hash-table :test 'eq))
           (dolist (parent ',parents)
             (pcase-dolist (`(,mode . ,_)
                            (cdr (gethash parent conn--minor-mode-maps)))
               (conn-get-mode-map ',name mode)))))
       ',name)))

(conn-define-state conn-null-state ()
  "An empty state.

For use in buffers that should not have any other state."
  :lighter "NULL"
  :hide-mark-cursor t
  :cursor '(bar . 4))

(conn-define-state conn-movement-state ()
  "A `conn-mode' state moving in a buffer."
  :suppress-input-method t
  :abstract t)

(conn-define-state conn-menu-state ()
  "A `conn-mode' state for remapping key menus."
  :abstract t)

(conn-define-state conn-command-state
    (conn-menu-state conn-movement-state)
  "A `conn-mode' state for editing test."
  :pop-alternate 'conn-emacs-state
  :lighter "C"
  :suppress-input-method t
  :cursor 'box)

(conn-define-state conn-read-mover-common-state (conn-command-state)
  "Common elements of thing reading states."
  :suppress-input-method t
  :mode-line-face 'conn-read-thing-mode-line-face
  :abstract t)

(cl-defmethod conn-enter-state ((state (conn-substate conn-read-mover-common-state)))
  (when-let* ((face (conn-state-get state :mode-line-face)))
    (setf (alist-get 'mode-line face-remapping-alist) face))
  (cl-call-next-method))

(cl-defmethod conn-exit-state ((_state (conn-substate conn-read-mover-common-state)))
  (setf face-remapping-alist
        (delq (assq 'mode-line face-remapping-alist)
              face-remapping-alist))
  (cl-call-next-method))

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

;;;;; Emacs State

(defvar conn-emacs-state-register nil)

(defvar-local conn-emacs-state-ring nil)

(cl-defmethod conn-exit-state ((_state (conn-substate conn-emacs-state)))
  (unless (eql (point) (conn-ring-head conn-emacs-state-ring))
    (conn-ring-insert-front conn-emacs-state-ring (point-marker))
    (when conn-emacs-state-register
      (when-let* ((marker (get-register conn-emacs-state-register))
                  ((markerp marker)))
        (set-marker marker nil))
      (set-register conn-emacs-state-register (point-marker)))))

(defun conn-copy-emacs-state-ring ()
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

(cl-defstruct (conn-dispatch-label
               (:constructor conn--make-dispatch-label))
  "Store the state for a dispatch label."
  string
  narrowed-string
  overlay
  target
  setup-function
  padding-function)

(cl-defstruct (conn-window-label)
  "Store the state for a window label."
  string
  window
  state)

(defun conn-simple-labels (count &optional face)
  "Return a list of label strings of length COUNT.

If FACE is non-nil set label string face to FACE.  Otherwise label
strings have `conn-dispatch-label-face'."
  (named-let rec ((count count)
                  (labels (mapcar #'copy-sequence
                                  (take count conn-simple-label-characters))))
    (let* ((prefixes nil))
      (while (and labels
                  (> count (+ (length labels)
                              (* (length prefixes)
                                 (length conn-simple-label-characters)))))
        (push (pop labels) prefixes))
      (if (and (null labels) (> count 0))
          (let ((new-labels))
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

(defun conn--get-target-windows ()
  (if conn-target-window-predicate
      (conn--get-windows nil nil conn-dispatch-all-frames
                         nil conn-target-window-predicate)
    (list (selected-window))))


;;;;; Label Reading

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
returned.")

(cl-defgeneric conn-label-redisplay (label)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload.")

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (conn-dispatch-label-target label))

(cl-defmethod conn-label-reset ((label conn-dispatch-label))
  (setf (oref label narrowed-string) (oref label string)))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label))
  (setf (overlay-get (conn-dispatch-label-target label) 'conn-label) nil))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label) prefix-char)
  (pcase-let (((cl-struct conn-dispatch-label narrowed-string) label))
    (if (not (eql prefix-char (aref narrowed-string 0)))
        (setf (oref label narrowed-string) nil)
      (setf (oref label narrowed-string) (substring narrowed-string 1))
      label)))

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
  (set-window-parameter (oref label window)
                        'conn-label-string
                        (oref label string)))

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
  (let* ((prompt (propertize (or prompt "chars")
                             'face 'minibuffer-prompt))
         (prompt-flag (or conn-label-select-always-prompt
                          always-prompt))
         (current candidates)
         (no-matches (concat prompt (propertize "no matches" 'face 'error)))
         (init-prompt prompt))
    (while-no-input
      (mapc #'conn-label-redisplay candidates))
    (cl-loop
     (pcase current
       ('nil
        (setq current candidates
              prompt no-matches
              prompt-flag (or conn-label-select-always-prompt
                              always-prompt))
        (mapc #'conn-label-reset current)
        (while-no-input
          (mapc #'conn-label-redisplay candidates)))
       (`(,it . nil)
        (unless prompt-flag
          (cl-return (conn-label-payload it)))))
     (setq prompt-flag nil)
     (let ((next nil)
           (c (funcall char-reader prompt)))
       (setq prompt (concat (if (eq no-matches prompt) init-prompt prompt)
                            (string c))
             current (dolist (label current next)
                       (when-let* ((l (conn-label-narrow label c)))
                         (push l next))))
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
          (make-conn-window-label
           :string (propertize string 'face 'conn-window-label-face)
           :window window
           :state (list (window-point window)
                        (window-vscroll window)
                        (window-hscroll window)))
        (goto-char (window-start))))))

;; From ace-window
(defun conn--get-windows (&optional window minibuffer all-frames dedicated predicate)
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
          (conn-label-select labels #'read-char)
        (mapc #'conn-label-delete labels))))))


;;;; State Command Loops

(defvar conn-state-loop-last-command nil)
(defvar conn--loop-prefix-mag nil)
(defvar conn--loop-prefix-sign nil)
(defvar conn--loop-error-message nil)
(defvar conn--loop-message-timer nil)
(defvar conn--loop-message-function nil)

(oclosure-define conn-state-loop-callback)

(defun conn-state-loop-format-prefix-arg ()
  (cond (conn--loop-prefix-mag
         (number-to-string
          (* (if conn--loop-prefix-sign -1 1)
             conn--loop-prefix-mag)))
        (conn--loop-prefix-sign "[-1]")
        (t "[1]")))

(defun conn-state-loop-prefix-arg ()
  (cond (conn--loop-prefix-mag
         (* (if conn--loop-prefix-sign -1 1) conn--loop-prefix-mag))
        (conn--loop-prefix-sign -1)))

(defun conn-state-loop-consume-prefix-arg ()
  (prog1 (conn-state-loop-prefix-arg)
    (setf conn--loop-prefix-mag nil
          conn--loop-prefix-sign nil)))

(defun conn-state-loop-error (string)
  (setf conn--loop-error-message string))

(defun conn-state-loop-exit ()
  (throw 'state-loop-exit nil))

(defun conn-state-loop-abort ()
  (keyboard-quit))

(defun conn-state-loop-message (format-string args)
  (let ((inhibit-message nil)
        (message-log-max nil))
    (message format-string args))
  (setq conn--loop-message-timer
        (run-with-timer
         minibuffer-message-timeout
         nil
         (lambda ()
           (let ((inhibit-message nil)
                 (message-log-max nil))
             (funcall conn--loop-message-function))))))

(cl-defgeneric conn-with-state-loop (state
                                     callback
                                     &key
                                     case-function
                                     message-function
                                     initial-arg
                                     &allow-other-keys))

(cl-defmethod conn-with-state-loop (state
                                    (callback conn-state-loop-callback)
                                    &key
                                    case-function
                                    message-function
                                    initial-arg
                                    &allow-other-keys)
  (conn-with-recursive-state state
    (let ((inhibit-message t)
          (conn--loop-prefix-mag (when initial-arg (abs initial-arg)))
          (conn--loop-prefix-sign (when initial-arg (> 0 initial-arg)))
          (conn--loop-error-message "")
          (conn--loop-message-timer nil)
          (conn--loop-message-function
           (lambda ()
             (funcall message-function callback conn--loop-error-message))))
      (catch 'state-loop-exit
        (while t
          (unless conn--loop-message-timer
            (let ((inhibit-message nil)
                  (message-log-max nil))
              (funcall conn--loop-message-function)))
          (let ((cmd (prog1 (key-binding (read-key-sequence nil) t)
                       (setf conn--loop-error-message ""))))
            (when conn--loop-message-timer
              (cancel-timer conn--loop-message-timer)
              (setq conn--loop-message-timer nil))
            (message nil)
            (when (and (bound-and-true-p conn-posframe-mode)
                       (fboundp 'posframe-hide))
              (posframe-hide " *conn-list-posframe*"))
            (pcase cmd
              ('nil nil)
              ('digit-argument
               (let* ((char (if (integerp last-input-event)
                                last-input-event
                              (get last-input-event 'ascii-character)))
                      (digit (- (logand char ?\177) ?0)))
                 (setf conn--loop-prefix-mag
                       (if (integerp conn--loop-prefix-mag)
                           (+ (* 10 conn--loop-prefix-mag) digit)
                         (when (/= 0 digit) digit)))))
              ('forward-delete-arg
               (when conn--loop-prefix-mag
                 (setf conn--loop-prefix-mag
                       (mod conn--loop-prefix-mag
                            (expt 10 (floor (log conn--loop-prefix-mag 10)))))))
              ('backward-delete-arg
               (when conn--loop-prefix-mag
                 (setf conn--loop-prefix-mag (floor conn--loop-prefix-mag 10))))
              ('reset-arg
               (setf conn--loop-prefix-mag nil))
              ('negative-argument
               (setf conn--loop-prefix-sign (not conn--loop-prefix-sign)))
              ('keyboard-quit
               (conn-state-loop-abort))
              (cmd
               (condition-case err
                   (funcall case-function cmd callback)
                 (user-error (conn-state-loop-error (error-message-string err))))))
            (setf conn-state-loop-last-command cmd))))))
  (message nil)
  (funcall callback))


;;;; Read Things

;;;;; Read Mover State

(conn-define-state conn-read-mover-state (conn-read-mover-common-state)
  "A state for reading things."
  :lighter "M")

(define-keymap
  :keymap (conn-get-state-map 'conn-read-mover-state)
  "C-w" 'backward-delete-arg
  "C-d" 'forward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-h" 'help
  "," (conn-remap-key "<conn-thing-map>")
  "r" 'recursive-edit)

(put 'reset-arg :advertised-binding (key-parse "M-DEL"))

;;;;; Read Thing Command Loop

(oclosure-define (conn-read-mover-callback
                  (:parent conn-state-loop-callback))
  (recursive-edit :type boolean)
  (thing-cmd :mutable t :type symbol)
  (thing-arg :mutable t)
  (mark-flag :mutable t :type boolean))

(cl-defmethod conn-with-state-loop (state
                                    (callback conn-read-mover-callback)
                                    &key
                                    case-function
                                    message-function
                                    initial-arg
                                    &allow-other-keys)
  (cl-call-next-method
   state callback
   :case-function (or case-function 'conn-read-mover-command-case)
   :message-function (or message-function 'conn-read-mover-message)
   :initial-arg initial-arg))

(defun conn-read-mover-message (callback error-message)
  (message
   (substitute-command-keys
    (concat
     (propertize "Thing Mover" 'face 'minibuffer-prompt)
     " (arg: "
     (propertize (conn-state-loop-format-prefix-arg)
                 'face 'read-multiple-choice-face)
     "; \\[reset-arg] reset arg; \\[help] commands"
     (if (oref callback recursive-edit)
         (concat "; \\[recursive-edit] "
                 "recursive edit):")
       "):")
     " "
     (if (oref callback mark-flag)
         (concat (propertize "Mark Active"
                             'face 'eldoc-highlight-function-argument)
                 " ")
       "")
     (propertize error-message 'face 'error)))))

(defun conn-read-thing-mover (&optional arg recursive-edit)
  "Interactively read a thing command and arg.

ARG is the initial value for the arg to be returned.
RECURSIVE-EDIT allows `recursive-edit' to be returned as a thing
command."
  (conn-with-state-loop
   'conn-read-mover-state
   (oclosure-lambda (conn-read-mover-callback
                     (recursive-edit recursive-edit)
                     (mark-flag (region-active-p)))
       ()
     (unless (eq mark-flag (region-active-p))
       (if (region-active-p)
           (deactivate-mark t)
         (push-mark nil t t)))
     (list thing-cmd thing-arg))
   :initial-arg arg))

(defun conn-read-thing-mover-dwim (&optional arg recursive-edit)
  "Interactively read a thing command and arg.

ARG is the initial value for the arg to be returned.
RECURSIVE-EDIT allows `recursive-edit' to be returned as a thing
command.

If `use-region-p' returns non-nil this will always return
`conn-toggle-mark-command' as the mover."
  (if (use-region-p)
      (list 'conn-toggle-mark-command nil)
    (conn-read-thing-mover arg recursive-edit)))

(defun conn-read-thing-region (&optional arg)
  (pcase-let ((`(,cmd ,arg) (conn-read-thing-mover arg t)))
    (cons (get cmd :conn-command-thing)
          (conn-bounds-of-command cmd arg))))

(defun conn-read-thing-region-dwim (&optional arg)
  (pcase-let ((`(,cmd ,arg) (conn-read-thing-mover-dwim arg t)))
    (cons (get cmd :conn-command-thing)
          (conn-bounds-of-command cmd arg))))

(cl-defgeneric conn-read-mover-command-case (command callback))

(static-if (>= emacs-major-version 31)
    (progn
      (cl-defmethod conn-read-mover-command-case (_command _cont)
        (conn-state-loop-error "Invalid command"))

      (cl-defmethod conn-read-mover-command-case ((command conn-thing-command) callback)
        (setf (oref callback thing-cmd) command
              (oref callback thing-arg) (conn-state-loop-prefix-arg))
        (conn-state-loop-exit)))

  (cl-defmethod conn-read-mover-command-case (command callback)
    (if (or (alist-get command conn-bounds-of-command-alist)
            (when (symbolp command)
              (get command :conn-command-thing)))
        (progn
          (setf (oref callback thing-cmd) command
                (oref callback thing-arg) (conn-state-loop-prefix-arg))
          (conn-state-loop-exit))
      (conn-state-loop-error "Invalid command"))))

(cl-defmethod conn-read-mover-command-case ((command (eql recursive-edit))
                                            callback)
  (if (oref callback recursive-edit)
      (progn
        (setf (oref callback thing-cmd) command
              (oref callback thing-arg) (conn-state-loop-prefix-arg))
        (conn-state-loop-exit))
    (conn-state-loop-error "Invalid command")))

(cl-defmethod conn-read-mover-command-case ((_command (eql conn-set-mark-command))
                                            callback)
  (setf (oref callback mark-flag) (not (oref callback mark-flag))))

(defun conn--completing-read-mover (callback)
  (save-window-excursion
    (conn-read-mover-command-case
     (condition-case _
         (intern
          (completing-read
           "Command: "
           (lambda (string pred action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(cons 'affixation-function
                          'conn--dispatch-command-affixation)
                   (category . conn-dispatch-command))
               (complete-with-action action obarray string pred)))
           (lambda (sym)
             (and (functionp sym)
                  (not (eq sym 'help))
                  (conn-thing-command-p sym)))
           t))
       (quit nil))
     callback)))

(cl-defmethod conn-read-mover-command-case ((_command (eql execute-extended-command))
                                            callback)
  (conn--completing-read-dispatch callback))

(cl-defmethod conn-read-mover-command-case ((_command (eql help)) callback)
  (conn--completing-read-dispatch callback))


;;;; Bounds of Command

(defvar conn-bounds-of-command-alist
  `((conn-dispatch-state . conn-bounds-of-dispatch)
    (conn-toggle-mark-command . conn--bounds-of-region)
    (conn-expand-remote . conn--bounds-of-remote-expansion)
    (visible . conn--bounds-of-command-thing)
    (narrowing . conn--bounds-of-narrowings)
    (heading . ,(lambda (_cmd _arg)
                  (list (bounds-of-thing-at-point 'heading))))
    (conn-expand . conn--bounds-of-expansion)
    (conn-contract . conn--bounds-of-expansion)
    (recursive-edit . conn--bounds-of-recursive-edit)
    (exit-recursive-edit . conn--bounds-of-recursive-edit)
    (abort-recursive-edit . conn--bounds-of-recursive-edit)
    (isearch-forward . conn--bounds-of-isearch)
    (isearch-backward . conn--bounds-of-isearch)
    (isearch-forward-regexp . conn--bounds-of-isearch)
    (isearch-backward-regexp . conn--bounds-of-isearch)
    (conn-previous-emacs-state . conn--bounds-of-emacs-state)
    (conn-next-emacs-state . conn--bounds-of-emacs-state))
  "Alist of bounds-op functions for things or commands.

Has the form ((THING-OR-CMD . bounds-op) ...).")

(defvar conn-bounds-of-command-default
  'conn--bounds-of-thing-command-default
  "Default bounds-op for `conn-bounds-of-command'.")

(defvar conn--last-bounds-of-command nil)

(defun conn--last-bounds-of-command ()
  "Value of the most recent `conn-bounds-of-command' at this recursion depth."
  (alist-get (recursion-depth) conn--last-bounds-of-command))

(defun conn-bounds-of-command (cmd arg)
  "Return bounds of CMD with ARGS.

Bounds list is of the form ((BEG . END) . SUBREGIONS).  Commands may return
multiple SUBREGIONS when it makes sense to do so.  For example
`forward-sexp' with a ARG of 3 would return the BEG and END of the group
of 3 sexps moved over as well as the bounds of each individual sexp."
  (if-let* ((forward (get cmd 'forward-op)))
      (conn-bounds-of-command forward arg)
    (save-mark-and-excursion
      (setf (alist-get (recursion-depth) conn--last-bounds-of-command)
            (funcall
             (or (alist-get cmd conn-bounds-of-command-alist)
                 (cl-loop for depth from 0 below 30
                          for thing = (get cmd :conn-command-thing)
                          then (get thing :conn-command-thing)
                          while thing
                          for bounds = (alist-get (get thing :conn-command-thing)
                                                  conn-bounds-of-command-alist)
                          when bounds return bounds)
                 conn-bounds-of-command-default)
             cmd arg)))))


;;;;; Bounds of Command Providers

(defun conn--bounds-of-emacs-state (cmd arg)
  (setq arg (prefix-numeric-value arg))
  (when (> arg 0) (cl-decf arg))
  (when (eq cmd 'conn-next-emacs-state)
    (setq arg (- arg)))
  (let* ((ring (conn-ring-list conn-emacs-state-ring))
         (mk (nth (mod arg (length ring)) ring))
         (pt (point)))
    (list (cons (min pt mk) (max pt mk)))))

(defun conn--bounds-of-command-thing (cmd _arg)
  (let* ((thing (get cmd :conn-command-thing))
         (bounds (ignore-errors (bounds-of-thing-at-point thing))))
    (list bounds bounds)))

(defun conn--bounds-of-thing-command-default (cmd arg)
  (deactivate-mark t)
  (let ((current-prefix-arg (cond ((> (prefix-numeric-value arg) 0) 1)
                                  ((< (prefix-numeric-value arg) 0) -1)
                                  (t 0)))
        (conn-this-command-handler (or (conn-get-mark-handler cmd)
                                       'conn-discrete-thing-handler))
        (conn-this-command-thing (get cmd :conn-command-thing))
        (this-command cmd)
        (conn-this-command-start (point-marker))
        (regions))
    (catch 'done
      (ignore-errors
        (dotimes (_ (abs (prefix-numeric-value arg)))
          (call-interactively cmd)
          (unless (region-active-p)
            (funcall conn-this-command-handler conn-this-command-start))
          (move-marker conn-this-command-start (point))
          (let ((reg (cons (region-beginning) (region-end))))
            (if (equal reg (car regions))
                (throw 'done nil)
              (push reg regions))))))
    (cons (cons (seq-min (mapcar #'car regions))
                (seq-max (mapcar #'cdr regions)))
          (nreverse regions))))

(defun conn--bounds-of-region (_cmd _arg)
  (cons (cons (region-beginning) (region-end))
        (region-bounds)))

(conn-define-state conn-bounds-of-recursive-edit-state (conn-command-state)
  :lighter "R")

(cl-defmethod conn-enter-state ((_ (conn-substate conn-bounds-of-recursive-edit-state)))
  (setq buffer-read-only t)
  (cl-call-next-method))

(cl-defmethod conn-exit-state ((_ (conn-substate conn-bounds-of-recursive-edit-state)))
  (setq buffer-read-only nil)
  (cl-call-next-method))

(define-keymap
  :keymap (conn-get-state-map 'conn-bounds-of-recursive-edit-state)
  "<escape>" 'exit-recursive-edit
  "e" 'exit-recursive-edit
  "C-]" 'abort-recursive-edit)

(defun conn--bounds-of-recursive-edit (_cmd _arg)
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
          (conn-with-recursive-state 'conn-bounds-of-recursive-edit-state
            (funcall pre)
            (recursive-edit))
          (cons (cons (region-beginning) (region-end))
                (region-bounds)))
      (remove-hook 'pre-command-hook pre))))

(defun conn--bounds-of-window (_cmd _arg)
  (list (cons (window-start) (window-end))))

(defun conn--bounds-of-remote-expansion (_cmd arg)
  (conn--push-ephemeral-mark)
  (conn--bounds-of-expansion 'conn-expand arg))

(defun conn--bounds-of-isearch (cmd _arg)
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
    (list (cons (min start (point)) (max start (point))))))


;;;; Bounds of Things in Region

(defvar conn-bounds-of-things-in-region-alist nil
  "Alist of ((CMD . THING-IN-REGION-FN) ...).

THING-IN-REGION-FN is a function of three arguments (THING BEG END).
BEG and END define the region and THING is the things to find within the
region.")

(defvar conn-bounds-of-things-in-region-default
  'conn--things-in-region-default
  "Default `conn-bounds-of-things-in-region' finder.

Is function of three arguments (THING BEG END).
BEG and END define the region and THING is the things to find within the
region.")

(defun conn-bounds-of-things-in-region (thing bounds)
  "Bounds of the THINGs contained within the region BOUNDS.

BOUNDS is of the form returned by `region-bounds', which see."
  (save-mark-and-excursion
    (mapcan (pcase-lambda (`(,beg . ,end))
              (funcall (or (alist-get thing conn-bounds-of-things-in-region-alist)
                           (when (symbolp thing)
                             (alist-get (get thing :conn-command-thing)
                                        conn-bounds-of-things-in-region-alist))
                           conn-bounds-of-things-in-region-default)
                       thing beg end))
            bounds)))

(defun conn--things-in-region-default (thing beg end)
  (let ((thing (or (when (symbolp thing)
                     (get thing :conn-command-thing))
                   thing)))
    (ignore-errors
      (goto-char beg)
      (forward-thing thing 1)
      (cl-loop for bounds = (save-excursion
                              (forward-thing thing -1)
                              (bounds-of-thing-at-point thing))
               while (and bounds (< (car bounds) end))
               collect bounds into regions
               while (and (< (point) end)
                          (ignore-errors
                            (forward-thing thing 1)
                            t))
               finally return regions))))

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

(defun conn--kapply-compose-iterator (iterator &rest pipeline)
  (seq-reduce (pcase-lambda (iterator (or `(,ctor . ,args) ctor))
                (apply ctor iterator args))
              (delq nil pipeline)
              iterator))

(defun conn--kapply-infinite-iterator ()
  (lambda (_state) t))

(defun conn--kapply-highlight-iterator (beg end &optional sort-function read-patterns)
  (let ((patterns
         (when (and (boundp 'hi-lock-interactive-patterns)
                    (boundp 'hi-lock-interactive-lighters))
           (if read-patterns
               (mapcar (lambda (regexp)
                         (alist-get regexp hi-lock-interactive-lighters nil nil #'equal))
                       (completing-read-multiple
                        "Regexps for kapply: "
                        (mapcar (lambda (pattern)
                                  (cons (or (car (rassq pattern hi-lock-interactive-lighters))
                                            (car pattern))
                                        pattern))
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
  (deactivate-mark t)
  (conn--kapply-region-iterator
   (let ((regions (conn-bounds-of-things-in-region thing bounds)))
     (if (= (point) (region-end))
         (nreverse regions)
       regions))))

(defun conn--kapply-region-iterator (regions &optional sort-function)
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

(defun conn--kapply-query (iterator)
  (lambda (state)
    (pcase state
      (:record
       (let ((hl (make-overlay (point) (point))))
         (overlay-put hl 'priority 2000)
         (overlay-put hl 'face 'query-replace)
         (overlay-put hl 'conn-overlay t)
         (unwind-protect
             (cl-loop
              for callback = (funcall iterator state)
              until (or (null callback)
                        (progn
                          (recenter nil)
                          (move-overlay hl (region-beginning) (region-end) (current-buffer))
                          (y-or-n-p (format "Record here?"))))
              finally return callback)
           (delete-overlay hl))))
      ((or :cleanup :next)
       (funcall iterator state)))))

(defun conn--kapply-skip-empty (iterator)
  (lambda (state)
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
              (when (markerp end) (set-marker end nil))))))))))

(defun conn--kapply-every-nth (iterator N)
  (cl-assert (> N 0))
  (lambda (state)
    (pcase state
      (:cleanup
       (funcall iterator state))
      ((or :next :record)
       (dotimes (_ (1- N))
         (pcase (funcall iterator state)
           (`(,beg . ,end)
            (when (markerp beg) (set-marker beg nil))
            (when (markerp end) (set-marker end nil)))))
       (funcall iterator state)))))

(defun conn--kapply-skip-point-invisible (iterator)
  (lambda (state)
    (pcase state
      ((or :next :record)
       (cl-loop for ret = (funcall iterator state)
                until (or (null ret)
                          (not (invisible-p (car ret))))
                when (markerp ret) do (set-marker ret nil)
                finally return ret))
      (:cleanup (funcall iterator state)))))

(defun conn--kapply-skip-region-invisible (iterator)
  (lambda (state)
    (pcase state
      ((or :next :record)
       (cl-loop for ret = (funcall iterator state)
                until (or (null ret)
                          (conn--region-visible-p (car ret) (cdr ret)))
                do (pcase-let ((`(,beg . ,end) ret))
                     (when (markerp beg) (set-marker beg nil))
                     (when (markerp end) (set-marker end nil)))
                finally return ret))
      (:cleanup (funcall iterator state)))))

(defun conn--kapply-open-invisible (iterator)
  (let (restore)
    (lambda (state)
      (pcase state
        ((or :next :record)
         (cl-loop for ret = (funcall iterator state)
                  for res = (or (null ret)
                                (conn--open-invisible (car ret) (cdr ret)))
                  until res
                  do (pcase-let ((`(,beg . ,end) ret))
                       (when (markerp beg) (set-marker beg nil))
                       (when (markerp end) (set-marker end nil)))
                  finally return (prog1 ret
                                   (when (consp res)
                                     (setq restore (nconc res restore))))))
        (:cleanup
         (funcall iterator state)
         (mapc #'funcall restore))))))

(defun conn--kapply-relocate-to-region (iterator)
  (lambda (state)
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
      region)))

(defun conn--kapply-per-buffer-undo (iterator)
  (let (undo-handles)
    (lambda (state)
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
                    (prepare-change-group))))))))))

(defun conn--kapply-per-buffer-atomic-undo (iterator)
  (let (undo-handles)
    (lambda (state)
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
                    (prepare-change-group))))))))))

(defun conn--kapply-per-iteration-undo (iterator)
  (let (handle)
    (lambda (state)
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
             (activate-change-group handle))))))))

(defun conn--kapply-save-excursion (iterator)
  (let (saved-excursions)
    (lambda (state)
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
           (funcall iterator state)))))))

(defun conn--kapply-ibuffer-overview (iterator)
  (let ((msg (substitute-command-keys
              "\\<query-replace-map>Buffer is modified, save before continuing?\
 \\[act], \\[skip], \\[quit], \\[edit], \\[automatic], \\[help]"))
        buffers automatic)
    (lambda (state)
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
                          (help-mode))))
                     (_ (ding t)))))))))))))

(defun conn--kapply-save-restriction (iterator)
  (let (kapply-saved-restrictions)
    (lambda (state)
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
                                (or end (point-max)))))))))))

(defun conn--kapply-change-region (iterator)
  (lambda (state)
    (when-let* ((ret (funcall iterator state)))
      (delete-region (region-beginning) (region-end))
      ret)))

(defun conn--kapply-with-state (iterator conn-state)
  (let (buffer-stacks)
    (lambda (state)
      (prog1
          (funcall iterator state)
        (pcase state
          (:cleanup
           (pcase-dolist (`(,buf . ,stack) buffer-stacks)
             (with-current-buffer buf
               (setq conn--state-stack stack
                     conn-lighter nil)
               (conn-enter-state (car stack))
               (force-mode-line-update))))
          ((or :record :next)
           (when conn-local-mode
             (if-let* ((stack (alist-get (current-buffer) buffer-stacks)))
                 (setf conn--state-stack stack)
               (setf (alist-get (current-buffer) buffer-stacks)
                     conn--state-stack))
             (conn-enter-recursive-state conn-state))))))))

(defun conn--kapply-at-end (iterator)
  (lambda (state)
    (when-let* ((ret (funcall iterator state)))
      (conn-exchange-mark-command)
      ret)))

(defun conn--kapply-pulse-region (iterator)
  (lambda (state)
    (when-let* ((ret (funcall iterator state)))
      (when (eq state :record)
        (pulse-momentary-highlight-region (region-beginning)
                                          (region-end)
                                          'query-replace))
      ret)))

(defun conn--kapply-save-windows (iterator)
  (let (wconf)
    (lambda (state)
      (pcase state
        (:cleanup
         (funcall iterator state)
         (set-window-configuration wconf))
        ((or :record :next)
         (unless wconf (setq wconf (current-window-configuration)))
         (funcall iterator state))))))


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
           (user-error "New keyboard not defined"))
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


;;;; Mark Handlers

(defvar-local conn-mark-handler-overrides-alist nil
  "Buffer local overrides for command mark handlers.

Is an alist of the form ((CMD . MARK-HANDLER) ...).

For the meaning of MARK-HANDLER see `conn-get-mark-handler'.")

(defun conn-get-mark-handler (command)
  "Return the mark handler for COMMAND."
  (or (alist-get command conn-mark-handler-overrides-alist)
      (when (symbolp command)
        (get command :conn-mark-handler))))

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
    (setf conn--last-bounds-of-command
          (delq (assq (recursion-depth) conn--last-bounds-of-command)
                conn--last-bounds-of-command))
    (unless conn-this-command-thing
      (setq conn-this-command-thing (conn--command-property :conn-command-thing)))
    (when (and conn-local-mode
               (marker-position conn-this-command-start)
               (eq (current-buffer) (marker-buffer conn-this-command-start)))
      (when-let* (((not (region-active-p)))
                  (handler
                   (or conn-this-command-handler
                       (alist-get this-command conn-mark-handler-overrides-alist)
                       (conn--command-property :conn-mark-handler))))
        (with-demoted-errors "Error in Mark Handler: %s"
          (funcall handler conn-this-command-start)))
      (unless (or conn--movement-ring-rotating
                  (null conn--movement-mark)
                  (not (eql conn--movement-tick (buffer-chars-modified-tick)))
                  (eql (mark t) conn--movement-mark))
        (with-demoted-errors "Error in Movement Ring: %s"
          (conn-push-region (point-marker) (mark-marker)))))))

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


;;;; Dispatch State

(defvar conn-dispatch-ring)

(defvar conn--dispatch-must-prompt nil)

(defvar conn--dispatch-action-always-prompt nil)

(defface conn-dispatch-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a dispatch state."
  :group 'conn-faces)

(defvar conn-dispatch-default-target-finder
  (lambda () (conn-dispatch-read-n-chars :string-length 2))
  "Default target finder for dispatch.

A target finder function should return a list of overlays.")

(defvar conn-dispatch-target-finders-alist
  `((move-end-of-line . ,(lambda () 'conn-dispatch-lines))
    (conn-backward-symbol . ,(lambda () (conn-dispatch-all-things 'symbol)))
    (backward-word . ,(lambda () (conn-dispatch-all-things 'word))))
  "Default target finders for for things or commands.

Is an alist of the form (((or THING CMD) . TARGET-FINDER) ...).  When
determining the target finder for a command in `conn-dispatch-state'
actions associated with a command have higher precedence than actions
associated with a thing.

For the meaning of TARGET-FINDER see
`conn-dispatch-default-target-finder'.")

(defvar conn-dispatch-action-default 'conn-dispatch-goto
  "Default action function for `conn-dispatch-state'.")

(defvar conn-dispatch-default-action-alist
  (list (cons 'button 'conn-dispatch-push-button))
  "Default action functions for things or commands.

Is an alist of the form (((or THING CMD) . ACTION) ...).  When
determining the default action for a command in `conn-dispatch-state'
actions associated with a command have higher precedence than actions
associated with a command's thing.")

(defvar conn-dispatch-repeat-count nil)

(defvar conn-dispatch-other-end nil)

(defvar conn-dispath-no-other-end nil)

(defvar conn--dispatch-always-retarget nil)

(conn-define-state conn-dispatch-mover-state (conn-read-mover-common-state)
  "State for reading a dispatch command."
  :lighter "D"
  :mode-line-face 'conn-dispatch-mode-line-face)

(conn-define-state conn-dispatch-state (conn-dispatch-mover-state)
  "State for reading a dispatch command.")

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

(define-keymap
  :keymap conn-dispatch-read-event-map
  :parent conn-dispatch-common-map
  "C-/" 'undo
  "C-'" 'recursive-edit
  "<mouse-1>" 'act
  "DEL" 'backward-delete-char
  "<backspace>" 'backward-delete-char
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-d" 'forward-delete-arg
  "C-w" 'backward-delete-arg
  "C-f" 'retarget
  "M-f" 'always-retarget
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
  "C-w" 'backward-delete-arg
  "C-d" 'forward-delete-arg
  "TAB" 'repeat-dispatch
  "C-n" 'restrict-windows
  "<backspace>" 'backward-delete-arg
  "DEL" 'backward-delete-arg
  "f" 'conn-dispatch-over-or-goto
  "u" 'forward-symbol
  "i" 'forward-line
  "k" 'next-line
  "n" 'end-of-defun
  "," (conn-remap-key "<conn-thing-map>"))

(define-keymap
  :keymap (conn-get-overriding-map 'conn-dispatch-mover-state)
  "<remap> <conn-expand>" 'conn-expand-remote
  "<remap> <conn-contract>" 'conn-expand-remote
  ";" 'conn-forward-inner-line
  "<conn-thing-map> e" 'move-end-of-line
  "<conn-thing-map> a" 'move-beginning-of-line
  "O" 'conn-dispatch-all-words
  "U" 'conn-dispatch-all-symbols
  "b" 'conn-dispatch-buttons)

(define-keymap
  :keymap (conn-get-overriding-map 'conn-dispatch-state)
  "]" 'conn-dispatch-open-parens
  "M-r" (conn-remap-key "<conn-region-map>")
  "," (conn-remap-key "<conn-thing-map>"))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-state)
  "\\" 'conn-dispatch-kapply)

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-dispatch-mover-state 'lisp-data-mode)
  "." `(forward-sexp ,(lambda () (conn-make-string-target-overlays "("))))

(cl-defun conn-dispatch-register-command (name
                                          &key
                                          target-finder
                                          bounds-of-command
                                          default-action)
  (when target-finder
    (setf (alist-get name conn-dispatch-target-finders-alist) target-finder))
  (when bounds-of-command
    (setf (alist-get name conn-bounds-of-command-alist) bounds-of-command))
  (when default-action
    (setf (alist-get name conn-dispatch-default-action-alist) default-action)))

(defun conn--dispatch-default-action (command)
  (or (alist-get command conn-dispatch-default-action-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-default-action-alist)
      conn-dispatch-action-default))

(defun conn--dispatch-target-finder (command)
  (or (alist-get command conn-dispatch-target-finders-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-target-finders-alist)
      conn-dispatch-default-target-finder))

(defun conn--dispatch-restrict-windows (win)
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
                    (thing (format " (%s)" (or (get fun :conn-command-thing)
                                               "action"))))
               (put-text-property 0 (length binding)
                                  'face 'help-key-binding binding)
               (put-text-property 0 (length thing)
                                  'face 'completions-annotations thing)
               (list command-name "" (concat thing binding))))))

(defmacro conn-with-dispatch-event-handler (tag keymap handler &rest body)
  "\(fn (DESCRIPTION &rest CASE) &body BODY)"
  (declare (indent 3))
  (cl-once-only (keymap)
    (let ((body `(let ((conn--dispatch-read-event-handlers
                        (cons ,handler conn--dispatch-read-event-handlers)))
                   ,@body)))
      `(catch ,tag
         ,(if keymap
              `(progn
                 (internal-push-keymap ,keymap 'overriding-terminal-local-map)
                 (unwind-protect
                     ,body
                   (internal-pop-keymap ,keymap 'overriding-terminal-local-map)))
            body)))))

;;;;; Dispatch Command Loop

(oclosure-define (conn-dispatch-callback
                  (:parent conn-state-loop-callback))
  (no-other-end :mutable t :type boolean)
  (other-end :mutable t :type boolean)
  (always-retarget :mutable t :type boolean)
  (repeat :mutable t :type boolean)
  (non-repeatable :mutable t :type boolean)
  (restrict-windows :mutable t :type boolean)
  (thing-cmd :mutable t :type symbol)
  (thing-arg :mutable t)
  (action :mutable t :type conn-action)
  (target-finder :mutable t :type function))

(defun conn-dispatch-message (callback error-message)
  (let ((action-description
         (if (oref callback action)
             (concat (propertize
                      (conn-describe-action (oref callback action))
                      'face 'eldoc-highlight-function-argument)
                     " ")
           "")))
    (message
     (substitute-command-keys
      (concat
       (propertize "Targets" 'face 'minibuffer-prompt)
       " (arg: "
       (propertize
        (conn-state-loop-format-prefix-arg)
        'face 'read-multiple-choice-face)
       "; "
       (unless (oref callback no-other-end)
         (concat
          "\\[dispatch-other-end] "
          (propertize "other-end"
                      'face (when (oref callback other-end)
                              'eldoc-highlight-function-argument))
          "; "))
       (unless (oref callback non-repeatable)
         (concat
          "\\[repeat-dispatch] "
          (propertize
           "repeat"
           'face (when (oref callback repeat)
                   'eldoc-highlight-function-argument))
          "; "))
       "\\[restrict-windows] "
       (propertize
        "this win"
        'face (when (oref callback restrict-windows)
                'eldoc-highlight-function-argument))
       "): "
       action-description
       (propertize error-message 'face 'error))))))

(cl-defmethod conn-with-state-loop (state
                                    (callback conn-dispatch-callback)
                                    &key
                                    case-function
                                    message-function
                                    initial-arg
                                    &allow-other-keys)
  (require 'conn-transients)
  (let ((success nil))
    (unwind-protect
        (prog1 (cl-call-next-method
                state callback
                :case-function (or case-function 'conn-dispatch-command-case)
                :message-function (or message-function 'conn-dispatch-message)
                :initial-arg initial-arg)
          (setq success t))
      (unless success
        (conn-cancel-action (oref callback action))))))

(cl-defmethod conn-dispatch-command-case (command callback)
  (pcase command
    ('nil)
    ((pred conn--action-type-p)
     (conn-cancel-action (oref callback action))
     (if (cl-typep (oref callback action) command)
         (setf (oref callback action) nil)
       (setf (oref callback action) (condition-case _
                                        (conn-make-action command)
                                      (error nil)))))
    ((and (let target-finder
            (conn--dispatch-target-finder command))
          (or (pred conn-thing-command-p)
              (guard (not (eq target-finder conn-dispatch-default-target-finder)))))
     (setf (oref callback target-finder) (funcall target-finder)
           (oref callback thing-cmd) command
           (oref callback thing-arg) (conn-state-loop-consume-prefix-arg))
     (when (null (oref callback action))
       (setf (oref callback action)
             (conn-make-action (conn--dispatch-default-action command))))
     (conn-state-loop-exit))
    (_ (conn-state-loop-error "Invalid command"))))

(cl-defmethod conn-dispatch-command-case ((_command (eql dispatch-other-end))
                                          callback)
  (unless (oref callback no-other-end)
    (setf (oref callback other-end) (not (oref callback other-end)))))

(cl-defmethod conn-dispatch-command-case ((_command (eql repeat-dispatch))
                                          callback)
  (unless (oref callback non-repeatable)
    (setf (oref callback repeat) (not (oref callback repeat)))))

(cl-defmethod conn-dispatch-command-case ((_command (eql restrict-windows))
                                          callback)
  (setf (oref callback restrict-windows) (not (oref callback restrict-windows))))

(cl-defmethod conn-dispatch-command-case ((_command (eql conn-repeat-last-dispatch))
                                          callback)
  (when (conn-action-stale-p (oref (conn-ring-head conn-dispatch-ring)
                                   action))
    (conn-dispatch-ring-remove-stale)
    (conn-state-loop-error "Last dispatch action stale"))
  (if-let* ((prev (conn-ring-head conn-dispatch-ring)))
      (progn
        (setf (oref callback target-finder) (funcall (conn--dispatch-target-finder
                                                      (oref prev thing-cmd)))
              (oref callback thing-cmd) (oref prev thing-cmd)
              (oref callback thing-arg) (oref prev thing-arg)
              (oref callback action) (oref prev action)
              (oref callback other-end) (oref prev other-end)
              (oref callback always-retarget) (oref prev always-retarget))
        (setf conn--dispatch-always-retarget (oref prev always-retarget))
        (conn-state-loop-exit))
    (conn-state-loop-error "Dispatch ring empty")))

(cl-defmethod conn-dispatch-command-case ((_command (eql conn-dispatch-cycle-ring-next))
                                          _cont)
  (conn-dispatch-cycle-ring-next)
  (if (bound-and-true-p conn-posframe-mode)
      (conn-posframe--dispatch-ring-display-subr)
    (conn-state-loop-message "%s" (conn-describe-dispatch
                                   (conn-ring-head conn-dispatch-ring)))))

(cl-defmethod conn-dispatch-command-case ((_command (eql conn-dispatch-cycle-ring-previous))
                                          _cont)
  (conn-dispatch-cycle-ring-previous)
  (if (bound-and-true-p conn-posframe-mode)
      (conn-posframe--dispatch-ring-display-subr)
    (conn-state-loop-message "%s" (conn-describe-dispatch
                                   (conn-ring-head conn-dispatch-ring)))))

(cl-defmethod conn-dispatch-command-case ((_command (eql conn-dispatch-ring-describe-head))
                                          _cont)
  (conn-dispatch-ring-remove-stale)
  (if (bound-and-true-p conn-posframe-mode)
      (conn-posframe--dispatch-ring-display-subr)
    (conn-state-loop-message "%s" (conn-describe-dispatch
                                   (conn-ring-head conn-dispatch-ring)))))

(defun conn--completing-read-dispatch (callback)
  (save-window-excursion
    (conn-dispatch-command-case
     (condition-case _
         (intern
          (completing-read
           "Command: "
           (lambda (string pred action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(cons 'affixation-function
                          'conn--dispatch-command-affixation)
                   (category . conn-dispatch-command))
               (complete-with-action action obarray string pred)))
           (lambda (sym)
             (or (get sym :conn-command-thing)
                 (alist-get sym conn-bounds-of-command-alist)
                 (conn--action-type-p sym)))
           t))
       (quit nil))
     callback)))

(cl-defmethod conn-dispatch-command-case ((_command (eql help)) callback)
  (conn--completing-read-dispatch callback))

(cl-defmethod conn-dispatch-command-case ((_command (eql execute-extended-command))
                                          callback)
  (conn--completing-read-dispatch callback))


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

(defvar conn-dispatch-target-finder nil)

(defvar conn-target-sort-function 'conn-target-sort-nearest)

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
  (cl-loop for ov in (overlays-in beg end)
           when (and (eq (overlay-get ov 'category) category)
                     (or (null window)
                         (eq (overlay-get ov 'window) window)))
           collect ov))

(cl-defun conn-make-target-overlay ( pt length
                                     &key
                                     padding-function
                                     window
                                     bounds-op)
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
      (when bounds-op (overlay-put ov 'thing bounds-op))
      (push ov (alist-get window conn-targets))
      ov)))

(defun conn-delete-target-overlay (ov)
  (let ((win (overlay-get ov 'window)))
    (cl-assert (memq ov (alist-get win conn-targets)))
    (conn-label-delete (overlay-get ov 'conn-label))
    (setf (alist-get win conn-targets)
          (delq ov (alist-get win conn-targets))))
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
        (let* ((prompt (propertize "string: " 'face 'minibuffer-prompt))
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
  (and (eq (selected-frame) (window-frame win))
       (length< (alist-get win conn-targets)
                conn-pixelwise-label-target-limit)))

(defun conn--pixelwise-labels-target-p (target)
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

(defvar conn--dispatch-window-lines-cache (make-hash-table :test 'eq))

(defun conn--dispatch-window-lines (window)
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

(defvar conn--pixelwise-window-cache (make-hash-table :test 'eq))

(defun conn-dispatch-pixelwise-label-p (ov)
  (and (with-memoization
           (gethash (overlay-get ov 'window) conn--pixelwise-window-cache)
         (funcall conn-pixelwise-labels-window-predicate
                  (overlay-get ov 'window)))
       (funcall conn-pixelwise-labels-target-predicate ov)))

(defun conn-disptach-label-target (target string)
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
              (conn--make-dispatch-label
               :setup-function (if (conn-dispatch-pixelwise-label-p ov)
                                   'conn--dispatch-setup-label-pixelwise
                                 'conn--dispatch-setup-label-charwise)
               :padding-function (overlay-get target 'padding-function)
               :string str
               :narrowed-string str
               :overlay ov
               :target target))))))

(defun conn-dispatch-get-targets (&optional sort-function)
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
  (let* ((all-targets (conn-dispatch-get-targets conn-target-sort-function))
         (label-strings (conn-simple-labels (1+ conn-target-count)))
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
  (conn--ensure-window-labels)
  (pcase-let* ((`(,bchars ,achars) conn-disptach-stable-label-characters)
               (labels nil))
    (pcase-dolist (`(,win . ,targets)
                   (conn-dispatch-get-targets 'conn-target-sort-nearest))
      (let (before after)
        (dolist (tar targets)
          (if (> (window-point win) (overlay-start tar))
              (push tar before)
            (push tar after)))
        (push (conn--stable-label-subr win (nreverse after) achars) labels)
        (push (conn--stable-label-subr win (nreverse before) bchars) labels)))
    (apply #'nconc labels)))

(defun conn-dispatch-smart-labels ()
  (if (or executing-kbd-macro defining-kbd-macro)
      (conn-dispatch-stable-labels)
    (conn-dispatch-simple-labels)))

(defun conn--target-label-payload (overlay)
  (list (overlay-start overlay)
        (overlay-get overlay 'window)
        (overlay-get overlay 'thing)))

(defun conn--dispatch-read-event-prefix ()
  (let ((prefix
         (cl-loop for pfx in conn--dispatch-read-event-message-prefixes
                  for str = (pcase pfx
                              ((pred functionp) (funcall pfx))
                              ((pred stringp) pfx))
                  if str
                  if (length> result 0) concat "; " into result end
                  and concat str into result
                  finally return result)))
    (unless (length= prefix 0)
      (concat "(" prefix ") "))))

(defun conn-dispatch-read-event (&optional prompt inherit-input-method seconds
                                           inhibit-message-prefix)
  (let ((inhibit-message nil)
        (message-log-max 0)
        (prompt (concat prompt (when conn--loop-error-message
                                 (propertize conn--loop-error-message
                                             'face 'error)))))
    (catch 'return
      (if seconds
          (while-let ((ev (conn-with-input-method
                            (read-event prompt
                                        inherit-input-method
                                        seconds))))
            (when (characterp ev)
              (throw 'return ev)))
        (while t
          (pcase (thread-first
                   (if inhibit-message-prefix prompt
                     (concat (conn--dispatch-read-event-prefix) prompt))
                   (read-key-sequence-vector)
                   (key-binding t))
            ('dispatch-character-event
             (setq conn--dispatch-must-prompt nil)
             (setq unread-command-events `((no-record . ,last-input-event)))
             (throw 'return
                    (conn-with-input-method
                      (read-event
                       (if inhibit-message-prefix prompt
                         (concat (conn--dispatch-read-event-prefix) prompt))
                       inherit-input-method))))
            ('keyboard-quit
             (keyboard-quit))
            (cmd
             (unwind-protect
                 (catch 'dispatch-handle
                   (cl-loop for handler in conn--dispatch-read-event-handlers
                            do (funcall handler cmd)))
               (setf conn-state-loop-last-command cmd)))))))))

(cl-defgeneric conn-dispatch-select-target (target-finder))

(cl-defmethod conn-dispatch-select-target :around (_target-finder)
  (conn-with-dispatch-event-handler 'mouse-click
      nil
      (lambda (cmd)
        (when (and (eq cmd 'act)
                   (mouse-event-p last-input-event))
          (let* ((posn (event-start last-input-event))
                 (win (posn-window posn))
                 (pt (posn-point posn)))
            (when (and (not (posn-area posn))
                       (funcall conn-target-window-predicate win))
              (throw 'mouse-click (list pt win nil))))))
    (conn-dispatch-select-mode 1)
    (internal-push-keymap conn-dispatch-read-event-map
                          'overriding-terminal-local-map)
    (unwind-protect
        (let ((inhibit-message t))
          (cl-call-next-method))
      (internal-pop-keymap conn-dispatch-read-event-map
                           'overriding-terminal-local-map)
      (conn-dispatch-select-mode -1))))

(cl-defmethod conn-dispatch-select-target (target-finder)
  (unwind-protect
      (progn
        (when conn--dispatch-always-retarget
          (conn-dispatch-retarget conn-dispatch-target-finder))
        (conn-dispatch-update-targets target-finder)
        (thread-first
          (funcall conn-dispatch-label-function)
          (conn-label-select #'conn-dispatch-read-event
                             (concat "["
                                     (number-to-string conn-target-count)
                                     "] chars: ")
                             (or conn--dispatch-must-prompt
                                 conn--dispatch-action-always-prompt
                                 (> conn-dispatch-repeat-count 0)))
          (conn--target-label-payload)))
    (conn-delete-targets)))


;;;;; Dispatch Target Finders

(defun conn-target-sort-nearest (a b)
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
  (:method ((target-finder function)) (funcall target-finder))
  (:method ((target-finder symbol)) (funcall target-finder)))

(cl-defgeneric conn-dispatch-cleanup-target-state (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-retarget (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-retargetable-p (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-has-target-p (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-dispatch-targets-other-end-p (target-finder)
  (:method (_) nil))

(defclass conn-dispatch-target-window-predicate ()
  ((window-predicate :initform (lambda (&rest _) t)
                     :allocation :class))
  "Abstract type for target finders with a window predicate."
  :abstract t)

(cl-defmethod conn-dispatch-update-targets :around ((state conn-dispatch-target-window-predicate))
  (let ((conn-target-window-predicate conn-target-window-predicate))
    (when-let* ((pred (oref state window-predicate)))
      (add-function :before-while conn-target-window-predicate pred))
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
  (not (not (oref state string))))

(defclass conn-dispatch-read-n-chars (conn-dispatch-string-targets)
  ((string-length :initform 1 :initarg :string-length)
   (predicate :initform nil :initarg :predicate)))

(cl-defmethod conn-dispatch-update-targets ((state conn-dispatch-read-n-chars))
  (with-slots (string string-length predicate) state
    (if string
        (conn-make-string-target-overlays string predicate)
      (conn-with-input-method
        (let* ((prompt (if (> string-length 1)
                           (propertize "chars" 'face 'minibuffer-prompt)
                         (propertize "char" 'face 'minibuffer-prompt))))
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
                        (setf string (substring string 0 -1)))
                      (throw 'backspace nil)))
                (thread-last
                  (conn-dispatch-read-event
                   (concat prompt
                           (propertize ": " 'face 'minibuffer-prompt)
                           string)
                   t nil)
                  (char-to-string)
                  (concat string)
                  (setf string))))
            (conn-delete-targets))
          (conn-make-string-target-overlays string predicate))))))

(defclass conn-dispatch-read-with-timeout (conn-dispatch-string-targets)
  ((timeout :initform 0.5 :initarg :timeout)
   (predicate :initform nil :initarg :predicate)))

(cl-defmethod conn-dispatch-update-targets ((state conn-dispatch-read-with-timeout))
  (with-slots (string timeout predicate) state
    (if string
        (conn-make-string-target-overlays string predicate)
      (conn-with-input-method
        (let* ((prompt (propertize "string: " 'face 'minibuffer-prompt)))
          (setq string (char-to-string (conn-dispatch-read-event prompt t)))
          (while-no-input
            (conn-make-string-target-overlays string predicate))
          (while-let ((next-char (conn-dispatch-read-event
                                  (concat prompt string)
                                  t timeout)))
            (conn-delete-targets)
            (setq string (concat string (char-to-string next-char)))
            (while-no-input
              (conn-make-string-target-overlays string predicate))))))))

(defun conn-dispatch-read-string-with-timeout (&optional predicate)
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :predicate predicate))

(defclass conn-dispatch-focus-targets ()
  ((hidden :initform nil)
   (context-lines :initform 0 :initarg :context-lines))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(cl-defmethod conn-dispatch-cleanup-target-state ((state conn-dispatch-focus-targets))
  (mapc #'delete-overlay (oref state hidden)))

(cl-defmethod conn-dispatch-update-targets :after ((state conn-dispatch-focus-targets))
  (unless (oref state hidden)
    (conn-protected-let* ((hidden nil (mapc #'delete-overlay hidden))
                          (context-lines (oref state context-lines)))
      (pcase-dolist (`(,win . ,targets) conn-targets)
        (with-selected-window win
          (let* ((regions
                  (save-excursion
                    (cl-loop for tar in targets
                             collect
                             (or (overlay-get tar 'context)
                                 (progn
                                   (goto-char (overlay-start tar))
                                   (cons (pos-bol (- 1 context-lines))
                                         (pos-bol (+ 2 context-lines)))))))))
            (cl-loop for beg = (point-min) then next-beg
                     for (end . next-beg) in (compat-call
                                              sort (conn--merge-regions
                                                    (cons (cons (pos-bol) (pos-bol 2))
                                                          regions)
                                                    t)
                                              :key #'car
                                              :in-place t)
                     while end
                     do (push (make-overlay beg end) hidden)
                     finally (push (make-overlay beg (point-max)) hidden))
            (cl-loop for ov in hidden do (overlay-put ov 'invisible t)))
          (recenter)))
      (setf (oref state hidden) hidden)
      (sit-for 0))))

(defclass conn-dispatch-previous-emacs-state (conn-dispatch-focus-targets)
  ((context-lines :initform 1 :initarg :context-lines)))

(cl-defmethod conn-dispatch-update-targets ((_state conn-dispatch-previous-emacs-state))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (let ((points (conn-ring-list conn-emacs-state-ring)))
          (dolist (pt points)
            (conn-make-target-overlay pt 0)))))))

(defun conn-dispatch-chars-in-thing (thing)
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
  (let ((bounds-op (lambda (_arg)
                     (save-mark-and-excursion
                       (outline-mark-subtree)
                       (region-bounds)))))
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
                 :bounds-op bounds-op
                 :window win)))))))))

(defclass conn-dispatch-all-defuns (conn-dispatch-focus-targets)
  ())

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
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (goto-char (window-start))
          (pcase-dolist (`(,beg . ,end) (conn--visible-re-matches regexp))
            (conn-make-target-overlay beg (- end beg))))))))

(defun conn-dispatch-things-read-prefix (thing prefix-length)
  (conn-dispatch-read-n-chars
   :string-length prefix-length
   :predicate (lambda (beg _end)
                (save-excursion
                  (goto-char beg)
                  (pcase (ignore-errors (bounds-of-thing-at-point thing))
                    (`(,tbeg . ,_tend) (= beg tbeg)))))))

(defun conn-dispatch-things-with-prefix (thing prefix-string)
  (lambda ()
    (conn-make-string-target-overlays
     prefix-string
     (lambda (beg _end)
       (save-excursion
         (goto-char beg)
         (pcase (ignore-errors (bounds-of-thing-at-point thing))
           (`(,tbeg . ,_tend) (= beg tbeg))))))))

(defun conn-dispatch-things-with-re-prefix (thing prefix-regex)
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

(cl-defmethod conn-dispatch-targets-other-end-p ((_ (eql conn-dispatch-end-of-lines)))
  t)

(defun conn-dispatch-inner-lines ()
  (let ((bounds-op (lambda (arg)
                     (save-excursion
                       (goto-char (pos-bol))
                       (conn-bounds-of-command 'conn-forward-inner-line arg)))))
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
                 :bounds-op bounds-op)))))))))

(defun conn-dispatch-end-of-inner-lines ()
  (let ((bounds-op (lambda (arg)
                     (save-excursion
                       (goto-char (pos-bol))
                       (conn-bounds-of-command 'conn-forward-inner-line arg)))))
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
                 :bounds-op bounds-op)))))))))

(cl-defmethod conn-dispatch-targets-other-end-p
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
         :bounds-op 'char
         :padding-function 'conn--right-justify-padding)
        (vertical-motion 1)
        (while (<= (point) (window-end))
          (if (= (point) (point-max))
              ;; hack to get the label displayed on its own line
              (when-let* ((ov (conn-make-target-overlay
                               (point) 0
                               :bounds-op 'char)))
                (overlay-put ov 'after-string
                             (propertize " " 'display '(space :width 0))))
            (conn-make-target-overlay
             (point) 0
             :bounds-op 'char
             :padding-function 'conn--right-justify-padding))
          (vertical-motion 1))))))


;;;;; Dispatch Actions

(oclosure-define (conn-action
                  (:predicate conn-action-p)
                  (:copier conn-action--copy))
  (no-history :mutable t :type boolean)
  (description :type (or string nil))
  (window-predicate :type function)
  (target-predicate :type function)
  (always-retarget :type boolean)
  (always-prompt :type boolean))

(defvar conn-dispatch-change-groups nil)

(defun conn-dispatch-loop-buffer-undo (&rest buffers)
  (when conn-dispatch-looping
    (push (let ((cg (mapcan #'prepare-change-group
                            (or buffers (list (current-buffer))))))
            (dolist (b (or buffers (list (current-buffer))))
              (with-current-buffer b
                (undo-boundary)))
            (lambda (do)
              (pcase do
                (:cancel (cancel-change-group cg))
                (:accept (accept-change-group cg)))))
          conn-dispatch-change-groups)))

(defun conn--action-type-p (item)
  (when-let* ((class (and (symbolp item)
                          (cl--find-class item))))
    (and (oclosure--class-p class)
         (memq 'conn-action (oclosure--class-allparents class)))))

(cl-defgeneric conn-action-stale-p (action)
  (:method ((_action conn-action)) nil))

(cl-defgeneric conn-action-copy (action)
  (:method (action) (conn-action--copy action)))

(cl-defgeneric conn-action-cleanup (action)
  (:method (_action) "Noop" nil))

(cl-defgeneric conn-describe-action (action &optional short)
  ( :method ((action conn-action) &optional _)
    (oref action description)))

(cl-defgeneric conn-accept-action (action)
  (:method ((_ conn-action)) "Noop" nil))

(cl-defgeneric conn-cancel-action (action)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-make-action (type)
  (:method (type) (error "Unknown action type %s" type))
  (:method :after (_type) (conn-state-loop-consume-prefix-arg)))

(cl-defmethod conn-make-action :around (type)
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (or (cl-call-next-method)
            (error "Failed to construct %s" type))
      (set-window-configuration wconf))))

(defun conn--action-buffer-change-group ()
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
  (when (or (looking-at " ") (looking-back " " 1))
    (fixup-whitespace)
    (if (progn (beginning-of-line)
               (looking-at "\n"))
        (join-line)
      (indent-for-tab-command)))
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\s)*\n"))
    (join-line)))

(oclosure-define (conn-dispatch-goto
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-goto)))
  (oclosure-lambda (conn-dispatch-goto
                    (no-history t)
                    (description "Goto"))
      (window pt bounds-op bounds-arg)
    (select-window window)
    (unless (= pt (point))
      (let ((forward (< (point) pt)))
        (unless (region-active-p)
          (push-mark nil t))
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
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
      (window pt _bounds-op _bounds-arg)
    (select-window window)
    (if (button-at pt)
        (push-button pt)
      (when (fboundp 'widget-apply-action)
        (widget-apply-action (get-char-property pt 'button) pt)))))

(oclosure-define (conn-dispatch-copy-to
                  (:parent conn-action))
  (str :type string)
  (seperator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-to)))
  (oclosure-lambda (conn-dispatch-copy-to
                    (str (funcall region-extract-function nil))
                    (seperator (when (conn-state-loop-consume-prefix-arg)
                                 (read-string "Seperator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and seperator conn-dispatch-other-end)
             (insert seperator))
           (insert-for-yank str)
           (when (and seperator (not conn-dispatch-other-end))
             (insert seperator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length seperator)))
                                               (point)))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-to) &optional short)
  (if-let* ((sep (and (not short) (oref action seperator))))
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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point) (length str)) (point))))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-yank-to
                  (:parent conn-action))
  (str :type string)
  (seperator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-to)))
  (oclosure-lambda (conn-dispatch-yank-to
                    (str (current-kill 0))
                    (seperator (when (conn-state-loop-consume-prefix-arg)
                                 (read-string "Seperator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and seperator conn-dispatch-other-end)
             (insert seperator))
           (insert-for-yank str)
           (when (and seperator (not conn-dispatch-other-end))
             (insert seperator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length seperator)))
                                               (point)))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-yank-to) &optional short)
  (if-let* ((sep (and (not short) (oref action seperator))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-reading-yank-to
                  (:parent conn-action))
  (str :type string)
  (seperator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-reading-yank-to)))
  (oclosure-lambda (conn-dispatch-reading-yank-to
                    (str (read-from-kill-ring "Yank To: "))
                    (seperator (when (conn-state-loop-consume-prefix-arg)
                                 (read-string "Seperator: " nil nil nil t)))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and seperator conn-dispatch-other-end)
             (insert seperator))
           (insert-for-yank str)
           (when (and seperator (not conn-dispatch-other-end))
             (insert seperator))
           (unless executing-kbd-macro
             (pulse-momentary-highlight-region (- (point)
                                                  (+ (length str)
                                                     (length seperator)))
                                               (point)))))
        (unless executing-kbd-macro
          (pulse-momentary-highlight-region (- (point)
                                               (+ (length str)
                                                  (length seperator)))
                                            (point)))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-reading-yank-to) &optional short)
  (if-let* ((sep (and (not short) (oref action seperator))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-send
                  (:parent conn-action))
  (str :type string)
  (seperator :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send)))
  (let ((sep (when (conn-state-loop-consume-prefix-arg)
               (read-string "Seperator: " nil nil nil t)))
        (cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send
                      (change-group cg)
                      (str (funcall region-extract-function t))
                      (seperator sep)
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt bounds-op bounds-arg)
      (with-selected-window window
        (conn-dispatch-loop-buffer-undo)
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (goto-char (if conn-dispatch-other-end end beg))
             (when (and seperator conn-dispatch-other-end)
               (insert seperator))
             (insert-for-yank str)
             (when (and seperator (not conn-dispatch-other-end))
               (insert seperator))
             (unless executing-kbd-macro
               (pulse-momentary-highlight-region (- (point)
                                                    (+ (length str)
                                                       (length seperator)))
                                                 (point)))))
          (unless executing-kbd-macro
            (pulse-momentary-highlight-region (- (point)
                                                 (+ (length str)
                                                    (length seperator)))
                                              (point))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-send) &optional short)
  (if-let* ((sep (and (not short) (oref action seperator))))
      (format "Send <%s>" sep)
    "Send"))

(cl-defmethod conn-accept-action ((action conn-dispatch-send))
  (conn--action-accept-change-group (oref action change-group))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send))
  (conn--action-cancel-change-group (oref action change-group)))

(oclosure-define (conn-dispatch-send-replace
                  (:parent conn-action))
  (str :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send-replace
                      (description "Send and Replace")
                      (change-group cg)
                      (str (funcall region-extract-function t))
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt bounds-op bounds-arg)
      (with-selected-window window
        (conn-dispatch-loop-buffer-undo)
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (delete-region beg end)
             (insert-for-yank str)
             (unless executing-kbd-macro
               (pulse-momentary-highlight-region (- (point) (length str)) (point))))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send-replace))
  (conn--action-accept-change-group (oref action change-group))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send-replace))
  (conn--action-cancel-change-group (oref action change-group)))

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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end) (downcase-region beg end))
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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end) (upcase-region beg end))
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
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-mark-and-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end) (capitalize-region beg end))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-register-load
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-load)))
  (oclosure-lambda (conn-dispatch-register-load
                    (register (register-read-with-preview "Register: ")))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (with-undo-amalgamate
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (goto-char (if conn-dispatch-other-end end beg))
             (conn-register-load register))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-register-load) &optional short)
  (if short "Register"
    (format "Register <%c>" (oref action register))))

(oclosure-define (conn-dispatch-register-replace
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-replace)))
  (oclosure-lambda (conn-dispatch-register-replace
                    (register (register-read-with-preview "Register: ")))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (with-undo-amalgamate
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (delete-region beg end)
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-register-replace) &optional short)
  (if short "Register Replace"
    (format "Register Replace <%c>" (oref action register))))

(oclosure-define (conn-dispatch-kill
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill)))
  (oclosure-lambda (conn-dispatch-kill
                    (register (when (conn-state-loop-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
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
  (if-let* ((register (oref action register)))
      (if short "Kill to Reg"
        (format "Kill to Register <%c>" register))
    "Kill"))

(oclosure-define (conn-dispatch-kill-append (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill-append)))
  (oclosure-lambda (conn-dispatch-kill-append
                    (register (when (conn-state-loop-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (copy-to-register register beg end t)
               (kill-append str nil)
               (delete-region beg end))
             (conn--dispatch-fixup-whitespace)
             (message "Appended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-kill-append) &optional short)
  (if-let* ((register (oref action register)))
      (if short "Kill App to Reg"
        (format "Kill Append Register <%c>" register))
    "Kill Append"))

(oclosure-define (conn-dispatch-kill-prepend
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kill-prepend)))
  (oclosure-lambda (conn-dispatch-kill-prepend
                    (register (when (conn-state-loop-consume-prefix-arg)
                                (register-read-with-preview "Register: ")))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (conn-dispatch-loop-buffer-undo)
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (prepend-to-register register beg end t)
               (kill-append str t)
               (delete-region beg end))
             (conn--dispatch-fixup-whitespace)
             (message "Prepended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-kill-prepend) &optional short)
  (if-let* ((register (oref action register)))
      (if short "Kill Pre to Reg"
        (format "Kill Prepend Register <%c>" register))
    "Kill Prepend"))

(oclosure-define (conn-dispatch-copy-append
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-append)))
  (oclosure-lambda (conn-dispatch-copy-append
                    (register (when (conn-state-loop-consume-prefix-arg)
                                (register-read-with-preview "Register: "))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (append-to-register register beg end)
               (kill-append str nil))
             (message "Copy Appended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-append) &optional short)
  (if-let* ((register (oref action register)))
      (if short "Copy App to Reg"
        (format "Copy Append to Register <%c>" register))
    "Copy Append to Kill"))

(oclosure-define (conn-dispatch-copy-prepend
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-prepend)))
  (oclosure-lambda (conn-dispatch-copy-prepend
                    (register (when (conn-state-loop-consume-prefix-arg)
                                (register-read-with-preview "Register: "))))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (let ((str (filter-buffer-substring beg end)))
             (if register
                 (prepend-to-register register beg end)
               (kill-append str t))
             (message "Copy Prepended: %s" str)))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-describe-action ((action conn-dispatch-copy-prepend) &optional short)
  (if-let* ((register (oref action register)))
      (if short "Copy Pre to Reg"
        (format "Copy Prepend to Register <%c>" register))
    "Copy Prepend to Kill"))

(oclosure-define (conn-dispatch-yank-from
                  (:parent conn-action)
                  (:copier conn-dispatch-yank-from-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-yank-from))
  (not (buffer-live-p (marker-buffer (oref action opoint)))))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-yank-from))
  (set-marker (oref action opoint) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-yank-from))
  (conn-dispatch-yank-from-copy action (copy-marker (oref action opoint) t)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-from)))
  (oclosure-lambda (conn-dispatch-yank-from
                    (description "Yank From")
                    (opoint (copy-marker (point) t)))
      (window pt bounds-op bounds-arg)
    (let (str)
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (pulse-momentary-highlight-region beg end)
             (setq str (filter-buffer-substring beg end))))))
      (with-current-buffer (marker-buffer opoint)
        (conn-dispatch-loop-buffer-undo)
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
  (set-marker (oref action opoint) nil))

(oclosure-define (conn-dispatch-yank-from-replace
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-from-replace)))
  (oclosure-lambda (conn-dispatch-yank-from-replace
                    (description "Yank From and Replace"))
      (window pt bounds-op bounds-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (pulse-momentary-highlight-region beg end)
           (copy-region-as-kill beg end)
           (conn--dispatch-fixup-whitespace))
          (_ (user-error "Cannot find thing at point")))))
    (conn-dispatch-loop-buffer-undo)
    (delete-region (region-beginning) (region-end))
    (yank)))

(oclosure-define (conn-dispatch-take-replace
                  (:parent conn-action)
                  (:copier conn-dispatch-take-replace-copy (opoint)))
  (opoint :type marker)
  (change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take-replace))
  (not (buffer-live-p (marker-buffer (oref action opoint)))))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take-replace))
  (set-marker (oref action opoint) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take-replace))
  (conn-dispatch-take-replace-copy action (copy-marker (oref action opoint) t)))

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
        (window pt bounds-op bounds-arg)
      (conn-dispatch-loop-buffer-undo (current-buffer) (window-buffer window))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (car (funcall bounds-op bounds-arg))
            (`(,beg . ,end)
             (kill-region beg end)
             (conn--dispatch-fixup-whitespace))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer (marker-buffer opoint)
        (save-excursion
          (goto-char opoint)
          (yank))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take-replace))
  (set-marker (oref action opoint) nil)
  (conn--action-cancel-change-group (oref action change-group)))

(cl-defmethod conn-accept-action ((action conn-dispatch-take-replace))
  (conn--action-accept-change-group (oref action change-group)))

(oclosure-define (conn-dispatch-take
                  (:parent conn-action)
                  (:copier conn-dispatch-take-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take))
  (not (buffer-live-p (marker-buffer (oref action opoint)))))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take))
  (set-marker (oref action opoint) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take))
  (conn-dispatch-take-copy action (copy-marker (oref action opoint) t)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take)))
  (oclosure-lambda (conn-dispatch-take
                    (description "Take From")
                    (opoint (copy-marker (point) t))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt bounds-op bounds-arg)
    (conn-dispatch-loop-buffer-undo (current-buffer) (window-buffer window))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (car (funcall bounds-op bounds-arg))
          (`(,beg . ,end)
           (kill-region beg end)
           (conn--dispatch-fixup-whitespace))
          (_ (user-error "Cannot find thing at point")))))
    (with-current-buffer (marker-buffer opoint)
      (yank))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take))
  (set-marker (oref action opoint) nil))

(oclosure-define (conn-dispatch-over
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-over)))
  (oclosure-lambda (conn-dispatch-over
                    (description "Over")
                    (no-history t)
                    (window-predicate (let ((obuf (current-buffer)))
                                        (lambda (win)
                                          (eq (window-buffer win) obuf)))))
      (window pt bounds-op bounds-arg)
    (when (and (eq (window-buffer window) (current-buffer))
               (/= pt (point)))
      (unless (region-active-p)
        (push-mark nil t))
      (pcase (cons (or (car (funcall bounds-op bounds-arg))
                       (point))
                   (progn
                     (goto-char pt)
                     (car (funcall bounds-op bounds-arg))))
        ((and `((,beg1 . ,end1) . (,beg2 . ,end2))
              (or (guard (<= beg1 end1 beg2 end2))
                  (guard (>= end1 beg1 end2 beg2))
                  (guard (and (= beg1 beg2) (= end1 end2)))))
         (if (> beg2 end1)
             (progn
               (conn--push-ephemeral-mark beg1)
               (goto-char end2))
           (conn--push-ephemeral-mark end1)
           (goto-char beg2)))
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
      (window pt _bounds-op _bounds-arg)
    (with-current-buffer (window-buffer window)
      (unless (= pt (point))
        (unless (region-active-p)
          (push-mark nil t))
        (select-window window)
        (goto-char pt)))))

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
      (window1 pt1 bounds-op1 window2 pt2 bounds-op2 bounds-arg)
    (conn-dispatch-loop-buffer-undo (window-buffer window1)
                                    (window-buffer window2))
    (conn--dispatch-transpose-subr
     (window-buffer window1) pt1 bounds-op1
     (window-buffer window2) pt2 bounds-op2
     bounds-arg)))

(defun conn--dispatch-transpose-subr ( buffer1 pt1 bounds-op1
                                       buffer2 pt2 bounds-op2
                                       bounds-arg)
  (if (eq buffer1 buffer2)
      (with-current-buffer buffer1
        (save-excursion
          (pcase-let ((`(,beg1 . ,end1)
                       (progn
                         (goto-char pt1)
                         (or (car (funcall bounds-op1 bounds-arg))
                             (user-error "Cannot find thing at point"))))
                      (`(,beg2 . ,end2)
                       (progn
                         (goto-char pt2)
                         (or (car (funcall bounds-op2 bounds-arg))
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
          (pcase (car (funcall bounds-op1 bounds-arg))
            (`(,beg . ,end)
             (setq pt1 beg)
             (setq str1 (filter-buffer-substring beg end))
             (delete-region beg end))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer2
        (save-excursion
          (goto-char pt2)
          (pcase (car (funcall bounds-op2 bounds-arg))
            (`(,beg . ,end)
             (setq str2 (filter-buffer-substring beg end))
             (delete-region beg end)
             (insert str1))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (insert str2)))
      (accept-change-group cg))))

(cl-defmethod conn-dispatch-command-case ((_command (eql conn-dispatch-over-or-goto))
                                          callback)
  (conn-cancel-action (oref callback action))
  (setf (oref callback action)
        (condition-case _
            (pcase (oref callback action)
              ((cl-type conn-dispatch-over)
               (conn-make-action 'conn-dispatch-goto))
              ((cl-type conn-dispatch-goto) nil)
              (_ (conn-make-action 'conn-dispatch-over)))
          (error nil))))

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
  "<remap> <downcase-word>" 'conn-dispatch-downcase
  "<remap> <downcase-region>" 'conn-dispatch-downcase
  "<remap> <downcase-dwim>" 'conn-dispatch-downcase
  "<remap> <upcase-word>" 'conn-dispatch-upcase
  "<remap> <upcase-region>" 'conn-dispatch-upcase
  "<remap> <upcase-dwim>" 'conn-dispatch-upcase
  "<remap> <capitalize-word>" 'conn-dispatch-capitalize
  "<remap> <capitalize-region>" 'conn-dispatch-capitalize
  "<remap> <capitalize-dwim>" 'conn-dispatch-capitalize
  "<remap> <conn-register-prefix>" 'conn-dispatch-register-load
  "<remap> <conn-kill-append-region>" 'conn-dispatch-kill-append
  "<remap> <conn-kill-prepend-region>" 'conn-dispatch-kill-prepend
  "<remap> <conn-append-region>" 'conn-dispatch-copy-append
  "<remap> <conn-prepend-region>" 'conn-dispatch-copy-prepend)

;;;;; Perform Dispatch Loop

(define-minor-mode conn-dispatch-select-mode
  "Mode for dispatch event reading"
  :global t
  :lighter " SELECT"
  (if conn-dispatch-select-mode
      (when-let* ((face (conn-state-get 'conn-dispatch-state :mode-line-face)))
        (setf (alist-get 'mode-line face-remapping-alist) face))
    (setf face-remapping-alist
          (delq (assq 'mode-line face-remapping-alist)
                face-remapping-alist))))

(cl-defun conn-dispatch-handle-and-redisplay (&key (prompt t))
  (redisplay)
  (setq conn--dispatch-must-prompt prompt)
  (throw 'dispatch-redisplay nil))

(defun conn-dispatch-handle ()
  (throw 'dispatch-handle nil))

(defvar conn-dispatch-looping nil)

(defmacro conn-perform-dispatch-loop (repeat &rest body)
  (declare (indent 1))
  (let ((rep (gensym "repeat")))
    `(catch 'state-loop-exit
       (let* ((,rep nil)
              (conn-dispatch-looping t)
              (conn-dispatch-change-groups nil)
              (conn--loop-error-message nil)
              (conn--dispatch-read-event-message-prefixes
               `(,(when (conn-dispatch-retargetable-p conn-dispatch-target-finder)
                    (lambda ()
                      (when-let* ((binding
                                   (and ,rep
                                        (where-is-internal
                                         'always-retarget
                                         conn-dispatch-read-event-map
                                         t))))
                        (concat
                         (propertize (key-description binding)
                                     'face 'help-key-binding)
                         " "
                         (propertize "always retarget"
                                     'face (when conn--dispatch-always-retarget
                                             'eldoc-highlight-function-argument))))))
                 ,@conn--dispatch-read-event-message-prefixes)))
         (unwind-protect
             (while (or (setq ,rep ,repeat)
                        (< conn-dispatch-repeat-count 1))
               (catch 'dispatch-redisplay
                 (condition-case err
                     (progn
                       ,@body
                       (cl-incf conn-dispatch-repeat-count))
                   (user-error
                    (conn-state-loop-error (error-message-string err))))))
           (dolist (u conn-dispatch-change-groups)
             (funcall u :accept)))))))

(defmacro conn-with-dispatch-suspended (&rest body)
  (declare (indent 0))
  `(pcase-let ((`(,conn-target-window-predicate
                  ,conn-target-predicate
                  ,conn-target-sort-function)
                conn--dispatch-init-state)
               (conn-dispatch-looping nil)
               (conn-dispatch-change-groups nil)
               (inhibit-message nil)
               (recenter-last-op nil)
               (conn-dispatch-repeat-count nil)
               (conn-dispatch-other-end nil)
               (conn-state-loop-last-command nil)
               (conn--loop-prefix-mag nil)
               (conn--loop-prefix-sign nil)
               (conn--dispatch-read-event-handlers nil)
               (conn--dispatch-read-event-message-prefixes nil)
               (conn--dispatch-always-retarget nil)
               (select-mode conn-dispatch-select-mode))
     (conn-delete-targets)
     (message nil)
     (when select-mode
       (conn-dispatch-select-mode -1)
       (internal-pop-keymap conn-dispatch-read-event-map
                            'overriding-terminal-local-map))
     (unwind-protect
         ,(macroexp-progn body)
       (when select-mode
         (internal-push-keymap conn-dispatch-read-event-map
                               'overriding-terminal-local-map)
         (conn-dispatch-select-mode 1)))))

(cl-defgeneric conn-dispatch-select-command-case (command))

(cl-defmethod conn-dispatch-select-command-case :extra "Prefix" (cmd)
  (catch 'dont-handle
    (pcase cmd
      ('digit-argument
       (let* ((char (if (integerp last-input-event)
                        last-input-event
                      (get last-input-event 'ascii-character)))
              (digit (- (logand char ?\177) ?0)))
         (setf conn--loop-prefix-mag
               (if (integerp conn--loop-prefix-mag)
                   (+ (* 10 conn--loop-prefix-mag) digit)
                 (when (/= 0 digit) digit)))))
      ('forward-delete-arg
       (when conn--loop-prefix-mag
         (setf conn--loop-prefix-mag
               (mod conn--loop-prefix-mag
                    (expt 10 (floor (log conn--loop-prefix-mag 10)))))))
      ('backward-delete-arg
       (when conn--loop-prefix-mag
         (setf conn--loop-prefix-mag (floor conn--loop-prefix-mag 10))))
      ('reset-arg
       (setf conn--loop-prefix-mag nil))
      ('negative-argument
       (setf conn--loop-prefix-sign (not conn--loop-prefix-sign)))
      (_ (throw 'dont-handle nil)))
    (conn-dispatch-handle)))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql mwheel-scroll)))
  (mwheel-scroll last-input-event)
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql recursive-edit)))
  (conn-with-dispatch-suspended
    (recursive-edit))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql recenter-top-bottom)))
  (let ((this-command 'recenter-top-bottom)
        (last-command conn-state-loop-last-command))
    (recenter-top-bottom (conn-state-loop-prefix-arg)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql toggle-input-method)))
  (let ((inhibit-message nil))
    (toggle-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql set-input-method)))
  (let ((inhibit-message nil))
    (call-interactively 'set-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql isearch-forward)))
  (conn-with-dispatch-suspended
    (isearch-forward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql isearch-backward)))
  (conn-with-dispatch-suspended
    (isearch-backward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql isearch-forward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-forward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql isearch-backward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-backward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql scroll-up)))
  (let ((next-screen-context-lines (or (conn-state-loop-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up))
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql scroll-down)))
  (let ((next-screen-context-lines (or (conn-state-loop-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-down))
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_command (eql conn-goto-window)))
  (conn-goto-window
   (conn-prompt-for-window
    (delq (selected-window)
          (conn--get-windows nil 'nomini 'visible))))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql finish)))
  (conn-state-loop-exit))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql dispatch-other-end)))
  (unless conn-dispath-no-other-end
    (setf conn-dispatch-other-end (not conn-dispatch-other-end))
    (conn-dispatch-handle-and-redisplay :prompt nil)))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql retarget)))
  (conn-dispatch-retarget conn-dispatch-target-finder)
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql always-retarget)))
  (setq conn--dispatch-always-retarget (not conn--dispatch-always-retarget))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql restrict-windows)))
  (if (advice-function-member-p 'conn--dispatch-restrict-windows
                                conn-target-window-predicate)
      (remove-function conn-target-window-predicate
                       'conn--dispatch-restrict-windows)
    (add-function :after-while conn-target-window-predicate
                  'conn--dispatch-restrict-windows))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-dispatch-select-command-case ((_cmd (eql undo)))
  (when conn-dispatch-change-groups
    (funcall (pop conn-dispatch-change-groups) :cancel))
  (conn-dispatch-handle-and-redisplay))


;;;;; Dispatch Ring

(defvar conn-dispatch-ring-max 12)

(defvar conn-dispatch-ring
  (conn-ring conn-dispatch-ring-max
             :cleanup 'conn-dispatch--cleanup))

(oclosure-define (conn-dispatch
                  (:predicate conn-dispatch-p)
                  (:copier conn-dispatch--copy (action)))
  (action :type conn-action)
  (thing-cmd :type (or symbol function))
  (thing-arg)
  (description :type function)
  (repeat-count :mutable t :type integer)
  (repeat :type boolean)
  (always-retarget :type boolean)
  (other-end :type boolean)
  (restrict-windows :type boolean))

(defun conn-dispatch-copy (dispatch)
  (conn-dispatch--copy dispatch (conn-action-copy (oref dispatch action))))

(defun conn-dispatch--cleanup (dispatch)
  (conn-action-cleanup (oref dispatch action)))

(defun conn-describe-dispatch (dispatch)
  (funcall (oref dispatch description)))

(defun conn-dispatch-push-history (action target-finder thing-cmd thing-arg repeat)
  (conn-dispatch-ring-remove-stale)
  (unless (oref action no-history)
    (setf (oref action no-history) t)
    (conn-ring-insert-front
     conn-dispatch-ring
     (oclosure-lambda (conn-dispatch
                       (restrict-windows
                        (advice-function-member-p
                         'conn--dispatch-restrict-windows
                         conn-target-window-predicate))
                       (action action)
                       (thing-cmd thing-cmd)
                       (thing-arg thing-arg)
                       (repeat repeat)
                       (other-end conn-dispatch-other-end)
                       (always-retarget conn--dispatch-always-retarget)
                       (repeat-count conn-dispatch-repeat-count)
                       (description (lambda ()
                                      (concat
                                       (conn-describe-action action)
                                       " @ "
                                       (symbol-name thing-cmd)
                                       (format " <%s>" thing-arg)))))
         (invert-repeat)
       (interactive "P")
       (let ((conn-dispatch-repeat-count repeat-count))
         (conn-perform-dispatch action target-finder
                                thing-cmd thing-arg
                                :always-retarget always-retarget
                                :repeat (xor invert-repeat repeat)
                                :restrict-windows restrict-windows
                                :other-end other-end)
         (setf repeat-count conn-dispatch-repeat-count))))))

(defun conn-dispatch-ring-remove-stale ()
  (cl-loop for stale in (seq-filter
                         (lambda (disp)
                           (conn-action-stale-p (oref disp action)))
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


;;;;; Dispatch Commands

(cl-defgeneric conn-perform-dispatch (action
                                      target-finder
                                      thing-cmd
                                      thing-arg
                                      &key
                                      repeat
                                      restrict-windows
                                      other-end
                                      &allow-other-keys))

(cl-defmethod conn-perform-dispatch :around ((action conn-action)
                                             target-finder thing-cmd thing-arg
                                             &key
                                             repeat
                                             restrict-windows
                                             other-end
                                             no-other-end
                                             always-retarget
                                             &allow-other-keys)
  (let* ((opoint (point-marker))
         (eldoc-display-functions nil)
         (recenter-last-op nil)
         (conn-state-loop-last-command nil)
         (conn--dispatch-init-state
          (list conn-target-window-predicate
                conn-target-predicate
                conn-target-sort-function))
         (conn-target-window-predicate conn-target-window-predicate)
         (conn-target-predicate conn-target-predicate)
         (conn-target-sort-function conn-target-sort-function)
         (conn--dispatch-must-prompt nil)
         (conn--loop-prefix-mag nil)
         (conn--loop-prefix-sign nil)
         (conn--dispatch-read-event-handlers
          (cons #'conn-dispatch-select-command-case
                conn--dispatch-read-event-handlers))
         (conn-dispatch-target-finder target-finder)
         (conn-dispatch-repeat-count 0)
         (conn--dispatch-always-retarget
          (or always-retarget
              (oref action always-retarget)))
         (conn-dispath-no-other-end no-other-end)
         (conn-dispatch-other-end
          (unless no-other-end
            (xor (conn-dispatch-targets-other-end-p target-finder)
                 (or other-end conn-dispatch-other-end))))
         (conn--dispatch-action-always-prompt (oref action always-prompt))
         (conn--dispatch-read-event-message-prefixes
          `(,(lambda ()
               (propertize (conn-describe-action action t)
                           'face 'eldoc-highlight-function-argument))
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
            ,(unless conn-dispath-no-other-end
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
            (lambda ()
              (when-let* ((binding
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
    (catch 'exit-dispatch
      (unwind-protect
          (cl-call-next-method)
        (ignore-errors
          (conn-dispatch-cleanup-target-state target-finder))
        (ignore-errors
          (conn-delete-targets))
        (ignore-errors
          (with-current-buffer (marker-buffer opoint)
            (unless (= (point) opoint)
              (conn--push-mark-ring opoint))))
        (set-marker opoint nil)
        (let ((inhibit-message nil))
          (message nil))))
    (when (> conn-dispatch-repeat-count 0)
      (conn-dispatch-push-history action target-finder thing-cmd thing-arg repeat))))

(cl-defmethod conn-perform-dispatch ((action conn-action)
                                     target-finder
                                     thing-cmd
                                     thing-arg
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let* ((`(,pt ,win ,bounds-op-override)
                  (conn-dispatch-select-target target-finder)))
      (funcall action win pt
               (or bounds-op-override
                   (lambda (arg)
                     (conn-bounds-of-command thing-cmd arg)))
               thing-arg))))

(cl-defmethod conn-perform-dispatch ((action conn-dispatch-transpose)
                                     target-finder
                                     thing-cmd
                                     thing-arg
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let ((`(,pt1 ,win1 ,bounds-op-override1)
                 (conn-dispatch-select-target target-finder))
                (`(,pt2 ,win2 ,bounds-op-override2)
                 (conn-dispatch-select-target target-finder))
                (bounds-op (lambda (arg)
                             (conn-bounds-of-command thing-cmd arg))))
      (funcall action
               win1 pt1 (or bounds-op-override1 bounds-op)
               win2 pt2 (or bounds-op-override2 bounds-op)
               thing-arg))))

(cl-defun conn-do-dispatch-state (&key no-other-end
                                       non-repeatable
                                       initial-arg
                                       action
                                       restrict-windows
                                       case-function
                                       message-function
                                       state)
  (conn-with-state-loop
   (or state 'conn-dispatch-state)
   (oclosure-lambda (conn-dispatch-callback
                     (action action)
                     (non-repeatable non-repeatable)
                     (no-other-end no-other-end)
                     (restrict-windows restrict-windows))
       ()
     (conn-perform-dispatch action target-finder
                            thing-cmd thing-arg
                            :repeat repeat
                            :restrict-windows restrict-windows
                            :other-end other-end
                            :no-other-end no-other-end
                            :always-retarget always-retarget))
   :message-function (or message-function 'conn-dispatch-message)
   :case-function (or case-function 'conn-dispatch-command-case)
   :initial-arg initial-arg))

(defun conn-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-do-dispatch-state :initial-arg initial-arg))

(defun conn-bounds-of-dispatch (_cmd arg)
  (let ((regions nil))
    (conn-do-dispatch-state
     :state 'conn-dispatch-mover-state
     :initial-arg arg
     :no-other-end t
     :action (oclosure-lambda (conn-action
                               (description "Bounds")
                               (window-predicate
                                (let ((win (selected-window)))
                                  (lambda (window) (eq win window)))))
                 (_window pt bounds-op bounds-arg)
               (save-mark-and-excursion
                 (goto-char pt)
                 (pcase (car (funcall bounds-op bounds-arg))
                   ('nil nil)
                   (reg (push reg regions))))))
    (unless regions (keyboard-quit))
    (cl-loop for (b . e) in (compat-call sort
                                         (conn--merge-regions regions t)
                                         :key #'car :in-place t)
             minimize b into beg
             maximize e into end
             finally return (cons (cons beg end) regions))))

(defun conn-repeat-last-dispatch (invert-repeat)
  "Repeat the last dispatch command.

Prefix arg INVERT-REPEAT inverts the value of repeat in the last dispatch."
  (interactive "P")
  (when (conn-action-stale-p (oref (conn-ring-head conn-dispatch-ring) action))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (funcall (conn-ring-head conn-dispatch-ring) invert-repeat))

(defun conn-last-dispatch-at-mouse (event &optional repeat)
  (interactive "e\nP")
  (unless (mouse-event-p event)
    (error "conn-last-dispatch-at-mouse must be bound to a mouse event"))
  (unless (conn-ring-list conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (when (conn-action-stale-p (oref (conn-ring-head conn-dispatch-ring)
                                   action))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (setq unread-command-events `((no-record . ,event)))
  (let ((map (define-keymap (key-description `[,(car event)]) 'act)))
    (internal-push-keymap map 'overriding-terminal-local-map)
    (unwind-protect
        (conn-repeat-last-dispatch
         (and (oref (conn-ring-head conn-dispatch-ring) repeat)
              repeat))
      (internal-pop-keymap map 'overriding-terminal-local-map))))

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
      (define-key (conn-get-overriding-map conn-current-state)
                  key-seq (conn-dispatch-copy (conn-ring-head conn-dispatch-ring)))
      (message "Dispatch bound to %s" (format-kbd-macro key-seq)))))

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-push-button)
   'conn-dispatch-all-buttons
   nil nil))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let ((target-finder
         (let ((targets (with-restriction (window-start) (window-end)
                          (conn--isearch-matches))))
           (lambda ()
             (cl-loop for (beg . end) in targets
                      do (conn-make-target-overlay beg (- end beg)))))))
    ;; In case this was a recursive isearch
    (unwind-protect
        (isearch-exit)
      (conn-perform-dispatch (conn-make-action 'conn-dispatch-jump)
                             target-finder nil nil
                             :restrict-windows t))))

(defun conn-goto-char-2 ()
  "Jump to point defined by two characters and maybe a label."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-jump)
   nil nil nil))

(conn-dispatch-register-command
 'conn-dispatch-open-parens
 :bounds-of-command (lambda (_ arg)
                      (conn-bounds-of-command 'forward-sexp arg))
 :target-finder (lambda ()
                  (conn-dispatch-things-with-re-prefix 'sexp "\\s(")))

(conn-dispatch-register-command
 'conn-dispatch-all-buttons
 :bounds-of-command (lambda (_cmd _arg)
                      (list (bounds-of-thing-at-point 'button)))
 :target-finder (lambda () 'conn-dispatch-all-buttons))

(conn-dispatch-register-command
 'conn-dispatch-all-symbols
 :bounds-of-command (lambda (_ arg)
                      (conn-bounds-of-command 'forward-symbol arg))
 :target-finder (lambda ()
                  (conn-dispatch-all-things 'symbol)))

(conn-dispatch-register-command
 'conn-dispatch-all-words
 :bounds-of-command (lambda (_ arg)
                      (conn-bounds-of-command 'conn-forward-word arg))
 :target-finder (lambda ()
                  (conn-dispatch-all-things 'word)))

(conn-dispatch-register-command
 'conn-previous-emacs-state
 :target-finder 'conn-dispatch-previous-emacs-state
 :default-action 'conn-dispatch-jump)

(conn-dispatch-register-command
 'conn-forward-inner-line
 :target-finder (lambda () 'conn-dispatch-end-of-inner-lines)
 :default-action 'conn-dispatch-goto)

(conn-dispatch-register-command
 'move-end-of-line
 :target-finder (lambda () 'conn-dispatch-end-of-lines)
 :default-action 'conn-dispatch-goto)

;;;;; Dispatch Registers

(cl-defstruct (conn-dispatch-register
               (:constructor conn--make-dispatch-register (dispatch-command)))
  dispatch-command)

(cl-defmethod register-val-jump-to ((val conn-dispatch-register) arg)
  (funcall (conn-dispatch-register-dispatch-command val) arg))

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

(defun conn--expand-filter-regions (regions)
  (let (result)
    (pcase-dolist ((and reg `(,beg . ,end))
                   (delete-dups regions))
      (when (and beg end
                 (/= beg end)
                 (<= beg (region-beginning))
                 (>= end (region-end)))
        (push reg result)))
    result))

(defun conn--valid-expansions-p ()
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
                         (cons (cons (region-beginning) (region-end)))
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
                      when (or (< beg (region-beginning))
                               (> end (region-end)))
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

(conn-define-state conn-expand-state ()
  "State for expanding."
  :lighter "↔"
  :mode-line-face 'conn-read-thing-mode-line-face)

(cl-defmethod conn-enter-state ((state (conn-substate conn-expand-state)))
  (when-let* ((face (conn-state-get state :mode-line-face)))
    (setf (alist-get 'mode-line face-remapping-alist) face))
  (cl-call-next-method))

(cl-defmethod conn-exit-state ((_state (conn-substate conn-expand-state)))
  (setf face-remapping-alist
        (delq (assq 'mode-line face-remapping-alist)
              face-remapping-alist))
  (cl-call-next-method))

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

(defun conn--read-expand-case (command _cont)
  (ignore-error user-error
    (pcase command
      ('conn-expand-exchange
       (conn-expand-exchange))
      ('conn-contract
       (conn-contract (conn-state-loop-consume-prefix-arg)))
      ('conn-expand
       (conn-expand (conn-state-loop-consume-prefix-arg)))
      ('conn-toggle-mark-command
       (conn-toggle-mark-command))
      ((or 'end 'exit-recursive-edit)
       (conn-state-loop-exit))
      ((or 'quit 'keyboard-quit 'abort-recursive-edit)
       (conn-state-loop-abort))
      (_ (conn-state-loop-error "Invalid command")))))

(defun conn--read-expand-message (_cont error-message)
  (message
   (substitute-command-keys
    (concat (propertize "Expansion: " 'face 'minibuffer-prompt)
            "(arg: "
            (propertize (conn-state-loop-format-prefix-arg)
                        'face 'read-multiple-choice-face)
            "; "
            "\\[conn-expand] expand; "
            "\\[conn-contract] contract; "
            "\\[conn-toggle-mark-command] toggle mark; "
            "\\[end] finish): "
            (propertize error-message 'face 'error)))))

(defun conn--bounds-of-expansion (cmd arg)
  (call-interactively cmd)
  (conn-with-state-loop
   'conn-expand-state
   (oclosure-lambda (conn-state-loop-callback)
       ()
     (region-bounds))
   :message-function 'conn--read-expand-message
   :case-function 'conn--read-expand-case
   :initial-arg arg))


;;;;; Thing Definitions

;; Definitions for various things and thing commands.

(defun conn-register-thing (thing &rest rest)
  "Register a new THING.

THINGs may have several optional properties that control how they
function in various conn features

FORWARD-OP, BEG-OP, END-OP and BOUNDS-OP provide operations for
`thingatpt', which see.

TARGET-FINDER is a function that produces targets for
`conn-dispatch-state'.

DEFAULT-ACTION is the default action for THING in
`conn-dispatch-state'.

\(fn THING &key TARGET-FINDER DEFAULT-ACTION FORWARD-OP BEG-OP END-OP BOUNDS-OP)"
  (intern (symbol-name thing))
  (cl-loop for (prop val) on rest by #'cddr
           do (pcase prop
                (:dispatch-target-finder
                 (setf (alist-get thing conn-dispatch-target-finders-alist) val))
                (:default-action
                 (setf (alist-get thing conn-dispatch-default-action-alist) val))
                (:forward-op (put thing 'forward-op val))
                (:beg-op (put thing 'beginning-op val))
                (:end-op (put thing 'end-op val))
                (:bounds-op (put thing 'bounds-of-thing-at-point val))
                (_ (error "Unknown property: %s" prop)))))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER.

HANDLER will be run from the `post-command-hook' and should be a
function of one argument, the location of `point' before the command
ran.  HANDLER is responsible for calling `conn--push-ephemeral-mark' in
order to mark the region that should be defined by any of COMMANDS."
  (dolist (cmd commands)
    (put cmd :conn-command-thing thing)
    (put cmd :conn-mark-handler handler)))

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

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :dispatch-target-finder 'conn-dispatch-all-defuns)

(conn-register-thing
 'visual-line
 :dispatch-target-finder (lambda () 'conn-dispatch-visual-lines)
 :forward-op (lambda (&optional N)
               (let ((line-move-visual t))
                 (vertical-motion 0)
                 (line-move N t))))

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
 'visible nil
 'conn-scroll-up 'conn-scroll-down
 'scroll-up-command 'scroll-down-command
 'conn-mark-visible)

(conn-register-thing-commands
 'region nil
 'conn-toggle-mark-command)

(conn-register-thing
 'symbol
 :forward-op 'forward-symbol
 :dispatch-target-finder (lambda ()
                           (conn-dispatch-things-read-prefix 'symbol 1)))

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing
 'page
 :forward-op 'forward-page)

(conn-register-thing-commands
 'page 'conn-discrete-thing-handler
 'forward-page 'backward-page)

(defun conn-char-mark-handler (beg)
  (when current-prefix-arg
    (conn--push-ephemeral-mark beg)))

(conn-register-thing-commands
 'char 'conn-char-mark-handler
 'forward-char 'backward-char)

(conn-register-thing
 'word
 :forward-op 'forward-word
 :dispatch-target-finder (lambda () (conn-dispatch-things-read-prefix 'word 1)))

(conn-register-thing-commands
 'word 'conn-symbol-handler
 'forward-word 'backward-word
 'upcase-word 'downcase-word 'capitalize-word
 'upcase-dwim 'downcase-dwim 'capitalize-dwim)

(conn-register-thing
 'sexp
 :forward-op 'forward-sexp
 :dispatch-target-finder (lambda () (conn-dispatch-things-read-prefix 'sexp 1)))

(conn-register-thing-commands
 'sexp 'conn-continuous-thing-handler
 'forward-sexp 'backward-sexp)

(conn-register-thing
 'list
 :forward-op 'forward-list)

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

(conn-register-thing
 'whitespace
 :forward-op 'forward-whitespace)

(conn-register-thing-commands
 'whitespace 'conn-discrete-thing-handler
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing
 'sentence
 :forward-op 'forward-sentence
 :dispatch-target-finder (lambda () (conn-dispatch-all-things 'sentence)))

(conn-register-thing-commands
 'sentence 'conn-continuous-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing
 'paragraph
 :forward-op 'forward-paragraph
 :dispatch-target-finder (lambda () (conn-dispatch-all-things 'paragraph)))

(conn-register-thing-commands
 'paragraph 'conn-continuous-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-continuous-thing-handler
 'end-of-defun 'beginning-of-defun
 'conn-forward-defun)

(conn-register-thing
 'char
 :default-action 'conn-dispatch-jump
 :dispatch-target-finder 'conn-dispatch-read-string-with-timeout)

(conn-register-thing-commands
 'buffer 'conn-discrete-thing-handler
 'end-of-buffer 'beginning-of-buffer)

(conn-register-thing
 'line
 :dispatch-target-finder (lambda () 'conn-dispatch-lines))

(conn-register-thing-commands
 'line 'conn-continuous-thing-handler
 'forward-line 'conn-backward-line
 'conn-line-forward-op
 'conn-goto-line)

(conn-register-thing
 'line-column
 :forward-op 'next-line
 :dispatch-target-finder (lambda () 'conn-dispatch-columns)
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'line-column 'conn-jump-handler
 'next-line 'previous-line
 'rectangle-next-line 'rectangle-previous-line)

(conn-register-thing-commands 'line nil 'comment-line)

(conn-register-thing
 'outer-line
 :beg-op (lambda () (move-beginning-of-line nil))
 :end-op (lambda () (move-end-of-line nil))
 :dispatch-target-finder (lambda () 'conn-dispatch-lines))

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
 :forward-op 'conn-forward-inner-line
 :dispatch-target-finder (lambda () 'conn-dispatch-inner-lines))

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

(conn-register-thing
 'expansion
 :dispatch-target-finder 'conn-dispatch-read-string-with-timeout)

(conn-register-thing-commands
 'expansion nil
 'conn-expand 'conn-contract)

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'conn-beginning-of-list
 'conn-end-of-list)


;;;; Narrow Ring

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(defvar conn-narrow-ring-max 14)

(cl-defstruct (conn-narrow-register
               (:constructor conn--make-narrow-register (narrow-ring)))
  (narrow-ring nil))

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
    ((and (cl-struct conn-narrow-register narrow-ring)
          struct)
     (setf (conn-narrow-register-narrow-ring struct)
           (cons (cons (conn--create-marker beg)
                       (conn--create-marker end))
                 narrow-ring)))
    (_
     (set-register register (conn--make-narrow-register
                             (list (cons (conn--create-marker beg)
                                         (conn--create-marker end))))))))

(defun conn-push-thing-to-narrow-register (thing-cmd thing-arg register outer)
  "Prepend thing regions to narrow register."
  (interactive
   (append (conn-read-thing-mover-dwim)
           (list
            (register-read-with-preview "Push region to register: ")
            current-prefix-arg)))
  (pcase-let* ((`((,beg . ,end) . ,regions)
                (conn-bounds-of-command thing-cmd thing-arg))
               (narrowings
                (if outer
                    (list (cons (conn--create-marker beg)
                                (conn--create-marker end)))
                  (cl-loop for (b . e) in regions
                           collect (cons (conn--create-marker b)
                                         (conn--create-marker e))))))
    (pcase (get-register register)
      ((and (cl-struct conn-narrow-register narrow-ring)
            struct)
       (setf (conn-narrow-register-narrow-ring struct)
             (nconc narrowings narrow-ring)))
      (_
       (set-register register (conn--make-narrow-register narrowings))))))

(defun conn-thing-to-narrow-ring (thing-cmd thing-arg &optional outer)
  "Push thing regions to narrow ring."
  (interactive
   (append (conn-read-thing-mover-dwim)
           (list current-prefix-arg)))
  (pcase-let* ((`((,beg . ,end) . ,regions)
                (conn-bounds-of-command thing-cmd thing-arg)))
    (if outer
        (conn--narrow-ring-record beg end)
      (cl-loop for (b . e) in regions
               do (conn--narrow-ring-record b e)))))

(defun conn--narrow-ring-record (beg end)
  (let ((narrowing (cons (conn--create-marker beg)
                         (conn--create-marker end))))
    (setq conn-narrow-ring
          (cons narrowing (delete narrowing conn-narrow-ring)))
    (when-let* ((old (drop conn-narrow-ring-max conn-narrow-ring)))
      (setf conn-narrow-ring (take conn-narrow-ring-max conn-narrow-ring))
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
  (setq conn-narrow-ring (nreverse (conn--merge-regions conn-narrow-ring)))
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
        (define-key (conn-get-overriding-map conn-current-state)
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
    (setf N (abs N))
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
    (setq conn-this-command-handler (conn-get-mark-handler 'forward-line)
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
          conn-this-command-handler (conn-get-mark-handler 'forward-line))))

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
                      &optional delimited backward query-flag)
  "Perform a `replace-string' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover-dwim nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  nil (or (cdr regions) regions) nil)))
     (append (list thing-mover arg) common)))
  (pcase-let ((`((,beg . ,end) . ,regions)
               (or (conn--last-bounds-of-command)
                   (conn-bounds-of-command thing-mover arg))))
    (deactivate-mark t)
    (save-excursion
      (if regions
          (let* ((regions (conn--merge-regions regions t))
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
                             &optional delimited backward query-flag)
  "Perform a `regexp-replace' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover-dwim nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-)
                                  " backward"
                                " word")
                            ""))
                  t (or (cdr regions) regions) nil)))
     (append (list thing-mover arg) common)))
  (pcase-let ((`((,beg . ,end) . ,regions)
               (or (conn--last-bounds-of-command)
                   (conn-bounds-of-command thing-mover arg))))
    (deactivate-mark t)
    (save-excursion
      (if regions
          (let* ((regions (conn--merge-regions regions t))
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

(cl-defstruct (conn-tab-register (:constructor %conn--make-tab-register (cookie frame)))
  (cookie nil :read-only t)
  (frame nil :read-only t))

(defun conn--get-tab-index-by-cookie (cookie)
  (seq-position (funcall tab-bar-tabs-function)
                cookie
                (lambda (tab c)
                  (eq c (alist-get 'conn-tab-cookie tab)))))

(defun conn--make-tab-register ()
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-tab (tab-bar--current-tab-find tabs)))
    (%conn--make-tab-register
     (or (alist-get 'conn-tab-cookie current-tab)
         (setf (alist-get 'conn-tab-cookie (cdr current-tab))
               (gensym "conn-tab-cookie")))
     (selected-frame))))

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

(defun conn-isearch-yank-region ()
  "Yank the current region to isearch."
  (interactive)
  (isearch-yank-internal (lambda () (mark t))))

(defun conn-isearch-open-recursive-edit ()
  "Open a recursive edit from within an isearch.

Exiting the recursive edit will resume the isearch."
  (interactive)
  (thread-first
    (recursive-edit)
    (with-isearch-suspended)
    (save-selected-window)))

(defun conn--isearch-in-thing (thing-cmd thing-arg &optional backward regexp)
  (let* ((regions (conn-bounds-of-command thing-cmd thing-arg))
         (regions (mapcar (pcase-lambda (`(,beg . ,end))
                            (cons (conn--create-marker beg)
                                  (conn--create-marker end nil t)))
                          (or (conn--merge-regions (cdr regions) t)
                              regions)))
         (depth (recursion-depth))
         (in-regions-p (lambda (beg end)
                         (or (/= depth (recursion-depth))
                             (cl-loop for (nbeg . nend) in regions
                                      thereis (<= nbeg beg end nend)))))
         (thing (upcase (symbol-name (or (get thing-cmd :conn-command-thing)
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

(defun conn-isearch-forward (thing-cmd thing-arg &optional regexp)
  "Isearch forward within the bounds of a thing."
  (interactive
   (append (conn-read-thing-mover-dwim nil t)
           (list current-prefix-arg)))
  (conn--isearch-in-thing thing-cmd thing-arg nil regexp))

(defun conn-isearch-backward (thing-cmd thing-arg &optional regexp)
  "Isearch backward within the bounds of a thing."
  (interactive
   (append (conn-read-thing-mover-dwim nil t)
           (list current-prefix-arg)))
  (conn--isearch-in-thing thing-cmd thing-arg t regexp))

(defun conn-isearch-region-forward (thing-cmd thing-arg &optional regexp)
  "Isearch forward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (append (conn-read-thing-mover nil t)
           (list current-prefix-arg)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg nil regexp)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-isearch-region-backward (thing-cmd thing-arg &optional regexp)
  "Isearch backward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (append (conn-read-thing-mover nil t)
           (list current-prefix-arg)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg t)
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
          (conn-ring conn-mark-ring-max
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
          (conn-ring conn-movement-ring-max
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

(conn-define-state conn-transpose-state (conn-read-mover-state)
  :lighter "⍉")

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "i" 'conn-backward-line
  "k" 'forward-line
  "u" 'forward-symbol)

(conn-define-state conn-dispatch-transpose-state (conn-transpose-state
                                                  conn-dispatch-state))

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
     "Press \\[exit-recursive-edit] to end and use current region. "
     "Press \\[abort-recursive-edit] to abort."))))

(defun conn--transpose-message (_cont error-message)
  (message
   (substitute-command-keys
    (concat
     (propertize "Thing Mover" 'face 'minibuffer-prompt)
     " (arg: "
     (propertize (conn-state-loop-format-prefix-arg)
                 'face 'read-multiple-choice-face)
     "; \\[reset-arg] reset arg; \\[help] commands"
     "; \\[recursive-edit] recursive edit): "
     (propertize error-message 'face 'error)))))

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
  :keymap (conn-get-mode-map 'conn-command-state
                             'conn-transpose-recursive-edit-mode)
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(oclosure-define (conn-transpose-command
                  (:parent conn-dispatch-transpose))
  (buffer :type buffer)
  (point :type marker)
  (bounds-op :type function))

(cl-defmethod conn-perform-dispatch ((action conn-transpose-command)
                                     target-finder
                                     thing-cmd
                                     thing-arg
                                     &key &allow-other-keys)
  (let ((conn-target-window-predicate conn-target-window-predicate))
    (add-function :after-while conn-target-window-predicate
                  (lambda (win)
                    (not (buffer-local-value 'buffer-read-only
                                             (window-buffer win)))))
    (pcase-let* ((`(,pt ,win ,bounds-op-override)
                  (save-mark-and-excursion
                    (conn-dispatch-select-target target-finder))))
      (funcall action
               win pt (or bounds-op-override
                          (lambda (arg)
                            (conn-bounds-of-command thing-cmd arg)))
               thing-arg))))

(defun conn-transpose-regions (mover arg)
  "Exchange regions defined by a thing command.

With argument ARG 0, exchange the things at point and mark.

If MOVER is \\='recursive-edit then exchange the current region and the
region after a `recursive-edit'."
  (interactive
   (conn-with-state-loop
    'conn-transpose-state
    (oclosure-lambda (conn-read-mover-callback
                      (recursive-edit t)
                      (mark-flag (region-active-p)))
        ()
      (list thing-cmd thing-arg))
    :initial-arg current-prefix-arg
    :message-function 'conn--transpose-message
    :case-function (lambda (command callback)
                     (pcase command
                       ((or 'conn-expand 'conn-contract)
                        (conn-state-loop-error "Invalid command"))
                       (_ (conn-read-mover-command-case command callback))))))
  (when conn-transpose-recursive-edit-mode
    (user-error "Recursive call to conn-transpose-regions"))
  (pcase mover
    ('recursive-edit
     (deactivate-mark t)
     (let ((bounds1 (region-bounds))
           (buf (current-buffer)))
       (conn-transpose-recursive-edit-mode 1)
       (unwind-protect
           (recursive-edit)
         (conn-transpose-recursive-edit-mode -1))
       (conn--dispatch-transpose-subr
        buf (caar bounds1) (lambda (_) bounds1)
        (current-buffer) (point) (let ((bounds2 (region-bounds)))
                                   (lambda (_) bounds2))
        nil)))
    ('conn-dispatch-state
     (while
         (condition-case err
             (progn
               (conn-do-dispatch-state
                :state 'conn-dispatch-transpose-state
                :no-other-end t
                :non-repeatable t
                :initial-arg arg
                :message-function 'conn-dispatch-message
                :action (oclosure-lambda
                            (conn-transpose-command
                             (description "Transpose")
                             (no-history t)
                             (buffer (current-buffer))
                             (point (point))
                             (bounds-op
                              (prog1
                                  (when (use-region-p)
                                    (let ((bounds (region-bounds)))
                                      (lambda (_) bounds)))
                                (deactivate-mark t)))
                             (window-predicate
                              (lambda (win)
                                (not (buffer-local-value 'buffer-read-only
                                                         (window-buffer win))))))
                            (window2 pt2 bounds-op2 bounds-arg)
                          (conn--dispatch-transpose-subr
                           buffer point (or bounds-op bounds-op2)
                           (window-buffer window2) pt2 bounds-op2
                           bounds-arg)))
               nil)
           ;; TODO: make this display somehow
           (user-error (message "%s" (cadr err)) t))))
    ((let 0 arg)
     (deactivate-mark t)
     (pcase-let* ((thing (get mover :conn-command-thing))
                  (`(,beg1 . ,end1) (if (region-active-p)
                                        (cons (region-beginning) (region-end))
                                      (bounds-of-thing-at-point thing)))
                  (`(,beg2 . ,end2) (save-excursion
                                      (goto-char (mark t))
                                      (bounds-of-thing-at-point thing))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((let thing (get mover :conn-command-thing))
     (deactivate-mark t)
     (transpose-subr (lambda (N) (forward-thing thing N))
                     (prefix-numeric-value arg)))))


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

(defun conn-join-lines (thing-mover thing-arg)
  "`delete-indentation' in region from START and END."
  (interactive (conn-read-thing-mover-dwim))
  (save-mark-and-excursion
    (pcase (conn-bounds-of-command thing-mover thing-arg)
      (`((,beg . ,end) . ,_)
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

(defun conn-copy-thing (thing-mover arg &optional register)
  "Copy THING at point."
  (interactive
   (append (conn-read-thing-mover-dwim)
           (when current-prefix-arg
             (list (register-read-with-preview "Register: ")))))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
    (conn-copy-region beg end register)
    (unless executing-kbd-macro
      (pulse-momentary-highlight-region beg end))))


;;;;; Narrowing Commands

(defun conn--narrow-to-region-1 (beg end &optional record)
  (narrow-to-region beg end)
  (when record (conn--narrow-ring-record beg end)))

(defun conn-narrow-to-thing (thing-mover arg &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive
   (append (conn-read-thing-mover-dwim
            (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
            t)
           (list t)))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
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
   (append (conn-read-thing-mover-dwim
            (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
            t)
           (list t)))
  (pcase-let ((`((,beg . ,end) . ,_)
               (conn-bounds-of-command thing-mover arg)))
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

(defvar conn--seperator-history nil
  "History var for `conn-set-register-seperator'.")

(defun conn-set-register-seperator (string)
  "Set `register-seperator' register to string STRING."
  (interactive
   (list (read-string "Separator: "
                      (let ((reg (get-register register-separator)))
                        (when (stringp reg) reg))
                      conn--seperator-history nil t)))
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
  (insert (pcase conn--minibuffer-initial-region
            (`(,beg . ,end)
             (with-minibuffer-selected-window
               (funcall (or quote-function 'identity)
                        (buffer-substring-no-properties beg end))))
            (_ (user-error "No region in buffer")))))

(defun conn-yank-replace (start end &optional arg)
  "`yank' replacing region between START and END.

If called interactively uses the region between point and mark.
If arg is non-nil, kill the region between START and END instead
of deleting it."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (atomic-change-group
    (conn--without-conn-maps
      (if arg
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
   (append (conn-read-thing-mover-dwim nil t)
           (list (prefix-numeric-value current-prefix-arg))))
  (pcase (conn-bounds-of-command thing-mover thing-arg)
    (`((,beg . ,end) . ,_)
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
   (append (conn-read-thing-mover-dwim nil t)
           (list (prefix-numeric-value current-prefix-arg))))
  (pcase (conn-bounds-of-command thing-mover thing-arg)
    ((and `((,beg . ,end) . ,_)
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
    (when conn-recenter-pulse
      (pulse-momentary-highlight-region beg end))))

(defun conn-recenter-on-region-other-window ()
  "Recenter the current region in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (conn-recenter-on-region)))


;;;;; Misc Commands

(defun conn-comment-or-uncomment (thing-mover arg)
  "Toggle commenting of a region defined by a thing command."
  (interactive (conn-read-thing-mover-dwim))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
    (if (comment-only-p beg end)
        (uncomment-region beg end)
      (let ((comment-empty-lines t))
        (comment-region beg end)))))

(defun conn-outline-insert-heading ()
  (interactive)
  (conn-with-recursive-state 'conn-emacs-state
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


;;;;; Transition Functions

(defvar conntext-state-hook nil)

(defun conntext-state ()
  (interactive)
  (run-hook-with-args-until-success 'conntext-state-hook))

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
  (letrec ((hook (lambda (_state)
                   (overwrite-mode -1)
                   (remove-hook 'conn-state-exit-functions hook))))
    (add-hook 'conn-state-exit-functions hook))
  (if arg
      (binary-overwrite-mode 1)
    (overwrite-mode 1)))

(defun conn-emacs-state-overwrite-binary ()
  "Enter emacs state in `binary-overwrite-mode'."
  (interactive)
  (conn-emacs-state-overwrite 1))

(defun conn-change (start end &optional kill)
  "Change region between START and END.

If KILL is non-nil add region to the `kill-ring'.  When in
`rectangle-mark-mode' defer to `string-rectangle'."
  (interactive
   (list (region-beginning)
         (region-end)
         current-prefix-arg))
  (cond ((and (bound-and-true-p rectangle-mark-mode) kill)
         (copy-rectangle-as-kill start end)
         (call-interactively #'string-rectangle))
        ((bound-and-true-p rectangle-mark-mode)
         (call-interactively #'string-rectangle))
        (kill
         (funcall (conn--without-conn-maps
                    (keymap-lookup nil conn-kill-region-keys t))
                  start end)
         (conn-push-state 'conn-emacs-state))
        (t
         (funcall (conn--without-conn-maps
                    (keymap-lookup nil conn-delete-region-keys t))
                  start end)
         (if (eq 'conn-emacs-state (conn-peek-stack))
             (conn-pop-state)
           (conn-push-state 'conn-emacs-state)))))


;;;; WinControl

;; A simple version of hyperbole's hycontrol-windows

(defgroup conn-wincontrol nil
  "Conn-mode WinControl."
  :prefix "conn-wincontrol-"
  :group 'conn)

(defface conn-wincontrol-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a `conn-wincontrol-mode'."
  :group 'conn-faces)

(defvar conn--wincontrol-help-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) ", "
   "\\[conn-wincontrol-digit-argument-reset]: reset arg; "
   "\\[conn-wincontrol-exit]: exit"))

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--wincontrol-preserve-arg nil)
(defvar conn--previous-scroll-conservatively)
(defvar conn--wincontrol-help)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)
(defvar conn--wincontrol-prev-eldoc-msg-fn)


;;;;; Wincontrol Internals

(defvar-keymap conn-window-resize-map
  "i" 'conn-wincontrol-maximize-vertically
  "l" 'conn-wincontrol-maximize-horizontally
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

(defvar-keymap conn-wincontrol-scroll-repeat-map
  :repeat t
  "SPC" 'conn-wincontrol-scroll-up
  "DEL" 'conn-wincontrol-scroll-down
  "M-TAB" 'conn-wincontrol-other-window-scroll-up
  "TAB" 'conn-wincontrol-other-window-scroll-down)

(defvar-keymap conn-wincontrol-text-scale-repeat-map
  :repeat t
  "z" 'text-scale-decrease
  "Z" 'text-scale-increase)

(defvar-keymap conn-wincontrol-tab-repeat-map
  :repeat t
  "C" 'tab-bar-duplicate-tab
  "c" 'tab-bar-duplicate-tab
  "I" 'tab-new
  "i" 'tab-next
  "K" 'tab-close
  "k" 'tab-previous
  "b" 'tab-bar-move-window-to-tab
  "U" 'tab-bar-detach-tab
  "u" 'tab-bar-detach-tab)

(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-DEL" 'conn-wincontrol-digit-argument-reset
  "C-w" 'conn-wincontrol-backward-delete-arg
  "C-d" 'conn-wincontrol-forward-delete-arg
  "C-]" 'conn-wincontrol-abort
  "C-M-d" 'delete-other-frames
  "M-/" 'undelete-frame
  "M-o" 'other-frame
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "C-u" 'conn-wincontrol-universal-arg
  "-" 'conn-wincontrol-invert-argument
  "/" 'tab-bar-history-back
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
  "?" 'tab-bar-history-forward
  "_" 'shrink-window-if-larger-than-buffer
  "<down>" 'conn-wincontrol-windmove-down
  "<left>" 'conn-wincontrol-windmove-left
  "<right>" 'conn-wincontrol-windmove-right
  "<up>" 'conn-wincontrol-windmove-up
  "M-<down>" 'windmove-swap-states-down
  "M-<left>" 'windmove-swap-states-left
  "M-<right>" 'windmove-swap-states-right
  "M-<up>" 'windmove-swap-states-up
  "M-I" 'windmove-swap-states-up
  "M-J" 'windmove-swap-states-left
  "M-L" 'windmove-swap-states-right
  "M-K" 'windmove-swap-states-down
  "M-i" 'conn-wincontrol-windmove-up
  "M-j" 'conn-wincontrol-windmove-left
  "M-l" 'conn-wincontrol-windmove-right
  "M-k" 'conn-wincontrol-windmove-down
  "<next>" 'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "M-<tab>" 'conn-wincontrol-other-window-scroll-up
  "M-TAB" 'conn-wincontrol-other-window-scroll-up
  "DEL" 'conn-wincontrol-scroll-down
  "SPC" 'conn-wincontrol-scroll-up
  "TAB" 'conn-wincontrol-other-window-scroll-down
  "<tab>" 'conn-wincontrol-other-window-scroll-down
  "C-s" 'conn-wincontrol-isearch
  "C-r" 'conn-wincontrol-isearch-backward
  ";" 'conn-wincontrol-exit-to-initial-win
  "e" 'conn-wincontrol-exit
  "m" 'conn-wincontrol-quit-other-window-for-scrolling
  "C" 'tab-bar-duplicate-tab
  "c" (conn-remap-key "C-c" t)
  "d" 'delete-window
  "h" 'kill-buffer-and-window
  "H" 'conn-kill-this-buffer
  "<escape>" 'conn-wincontrol-exit
  "<return>" 'conn-other-place-prefix
  "F" 'toggle-frame-fullscreen
  "f" 'conn-goto-window
  "g" 'delete-other-windows
  "M" 'tab-swith
  "I" 'tab-new
  "i" 'tab-next
  "j" 'previous-buffer
  "J" 'bury-buffer
  "K" 'tab-close
  "k" 'tab-previous
  "l" 'next-buffer
  "L" 'unbury-buffer
  "b" 'tab-bar-move-window-to-tab
  "o" 'conn-wincontrol-next-window
  "O" 'tear-off-window
  "p" 'conn-register-prefix
  "x" (conn-remap-key "C-x" t)
  "r" 'conn-wincontrol-split-right
  "R" 'conn-wincontrol-isearch-other-window-backward
  "S" 'conn-wincontrol-isearch-other-window
  "s" conn-window-resize-map
  "`" 'conn-wincontrol-mru-window
  "w" 'conn-throw-buffer
  "u" 'conn-wincontrol-previous-window
  "U" 'tab-bar-detach-tab
  "v" 'conn-wincontrol-split-vertically
  "q" 'quit-window
  "t" 'conn-transpose-window
  "y" 'conn-yank-window
  "z" 'text-scale-decrease
  "Z" 'text-scale-increase)

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

(defun conn--wincontrol-pre-command ()
  (when (or conn--wincontrol-arg (< conn--wincontrol-arg-sign 0))
    (setq prefix-arg (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))
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

(defun conn--wincontrol-message ()
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message conn--wincontrol-help
             (format (if conn--wincontrol-arg "%s%s" "[%s1]")
                     (if (= conn--wincontrol-arg-sign -1) "-" "")
                     conn--wincontrol-arg))))

(defun conn--wincontrol-setup (&optional preserve-state)
  (internal-push-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (add-hook 'post-command-hook 'conn--wincontrol-post-command)
  (add-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (setq conn--previous-scroll-conservatively scroll-conservatively
        conn--wincontrol-prev-eldoc-msg-fn eldoc-message-function
        eldoc-message-function #'ignore
        scroll-conservatively 100)
  (unless preserve-state
    (setq conn--wincontrol-arg (when current-prefix-arg
                                 (prefix-numeric-value current-prefix-arg))
          conn--wincontrol-help (substitute-command-keys
                                 conn--wincontrol-help-format)
          conn--wincontrol-arg-sign 1
          conn--wincontrol-initial-window (selected-window)
          conn--wincontrol-initial-winconf (current-window-configuration)))
  (setf (alist-get 'mode-line face-remapping-alist)
        'conn-wincontrol-mode-line-face)
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (setf face-remapping-alist
        (delq (assq 'mode-line face-remapping-alist)
              face-remapping-alist)))

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn--wincontrol-setup t)))

(defun conn-wincontrol-one-command ()
  "Execute one command in `conn-wincontrol-mode'."
  (interactive)
  (letrec ((pre (lambda ()
                  (unless (memq this-command
                                '(conn-wincontrol-forward-delete-arg
                                  conn-wincontrol-backward-delete-arg
                                  conn-wincontrol-digit-argument-reset
                                  conn-wincontrol-invert-argument
                                  conn-wincontrol-digit-argument
                                  conn-wincontrol-universal-arg))
                    (remove-hook 'pre-command-hook pre)
                    (conn-wincontrol-exit)))))
    (add-hook 'pre-command-hook pre 99))
  (conn-wincontrol))


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
  (setq conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)
        conn--wincontrol-preserve-arg t))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to nil and sign to +."
  (interactive)
  (setq conn--wincontrol-arg nil)
  (setq conn--wincontrol-arg-sign 1))

(defun conn-wincontrol-backward-delete-arg ()
  "Delete least significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-preserve-arg t)
  (setq conn--wincontrol-arg (floor conn--wincontrol-arg 10)))

(defun conn-wincontrol-forward-delete-arg ()
  "Delete most significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-preserve-arg t)
  (setq conn--wincontrol-arg (thread-last
                               (log conn--wincontrol-arg 10)
                               (floor)
                               (expt 10)
                               (mod conn--wincontrol-arg))))

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

(defun conn-goto-window (window)
  "Prompt for a window and then select it."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn--get-windows nil 'nomini 'visible)))))
  (if (null window)
      (user-error "No other windows available to select")
    (select-window window)))

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
  :keymap (conn-get-mode-map 'conn-command-state 'rectangle-mark-mode)
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
(conn-set-mode-map-depth 'conn-command-state 'rectangle-mark-mode -90)

(defvar-keymap conn-isearch-map
  "M-Y" 'conn-isearch-yank-region
  "M-<return>" 'conn-isearch-exit-and-mark
  "C-<return>" 'conn-isearch-exit-other-end
  "M-RET" 'conn-isearch-exit-and-mark
  "M-\\" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch
  "C-'" 'conn-isearch-open-recursive-edit)


;;;;; Top-level Command State Maps

(defvar-keymap conn-default-region-map
  "m" 'conn-replace
  "u" 'conn-regexp-replace
  "\\" 'conn-kapply-on-region-prefix
  "TAB" 'indent-rigidly
  "$" 'ispell-region
  "*" 'calc-grab-region
  ";" 'comment-or-uncomment-region
  "e" 'conn-duplicate
  "D" 'conn-duplicate-and-comment
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
  "\\" 'conn-kapply-on-thing-prefix
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
  "d" 'duplicate-line
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
  "C-<escape>" 'exit-recursive-edit
  "C-x y" conn-dispatch-cycle-map
  "M-g o" 'conn-pop-mark-ring
  "M-g u" 'conn-unpop-mark-ring
  "M-g e" 'conn-previous-emacs-state
  "M-g E" 'conn-next-emacs-state
  "C-S-w" 'delete-region
  "C-." 'conn-dispatch-state
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
  "<conn-thing-map> e" 'end-of-buffer)

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
  :keymap (conn-get-state-map 'conn-read-mover-common-state)
  "C-s" 'isearch-forward
  "s" 'isearch-forward
  "C-r" 'isearch-backward
  "C-M-s" 'isearch-forward-regexp
  "C-M-r" 'isearch-backward-regexp
  "d" 'conn-forward-defun
  "y" 'forward-symbol
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
  "C-<return>" 'conn-join-lines
  "<escape>" 'conn-pop-state
  "T" 'conn-copy-thing
  "D" 'conn-duplicate-region
  "P" 'conn-register-load-and-replace
  "+" 'conn-set-register-seperator
  "H" 'conn-expand
  "b" (conn-remap-key "<conn-edit-map>")
  "Z" 'pop-to-mark-command
  "&" 'conn-other-buffer
  "e" 'conn-pop-state
  "E" 'conn-emacs-state-at-mark
  "t" 'conn-change
  "`" 'conn-wincontrol-mru-window
  "|" 'conn-shell-command-on-region
  "'" 'conntext-state
  "." 'conn-other-window-prefix
  "/" (conn-remap-key conn-undo-keys t)
  ";" 'conn-wincontrol
  "\\" 'conn-kapply-prefix
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
  "f" 'conn-dispatch-state
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
        (cl-pushnew 'conn--local-state-map emulation-mode-map-alists)
        (cl-pushnew 'conn--local-major-mode-map emulation-mode-map-alists)
        (cl-pushnew 'conn--local-minor-mode-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-override-map emulation-mode-map-alists)
        (conn--append-keymap-parent isearch-mode-map conn-isearch-map)
        (conn--append-keymap-parent search-map conn-search-map)
        (conn--append-keymap-parent goto-map conn-goto-map)
        (conn--append-keymap-parent indent-rigidly-map conn-indent-rigidly-map))
    (conn--remove-keymap-parent isearch-mode-map conn-isearch-map)
    (conn--remove-keymap-parent search-map conn-search-map)
    (conn--remove-keymap-parent goto-map conn-goto-map)
    (conn--remove-keymap-parent indent-rigidly-map conn-indent-rigidly-map)
    (setq emulation-mode-map-alists
          (seq-difference '(conn--local-state-map
                            conn--local-major-mode-map
                            conn--local-minor-mode-maps
                            conn--local-override-map)
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
        (setq-local conn-lighter (seq-copy conn-lighter)
                    conn--local-state-map (list (list 'conn-local-mode))
                    conn--local-override-map (list (list 'conn-local-mode))
                    conn--local-major-mode-map (list (list 'conn-local-mode))
                    conn-emacs-state-ring
                    (conn-ring 8 :cleanup (lambda (mk) (set-marker mk nil))))
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
    ;; conn-exit-state sets conn-current-state to nil before
    ;; anything else, so if we got here after an error in
    ;; conn-exit-state this prevents an infinite loop.
    (when conn-current-state
      (conn-exit-state conn-current-state))
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
    (conn-with-recursive-state 'conn-null-state
      (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))


;;;; Completion

(defun conn--exit-completion (_state)
  (completion-in-region-mode -1))
(add-hook 'conn-state-exit-functions 'conn--exit-completion)


;;;; Eldoc

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'conn-dispatch-state))


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
 :dispatch-target-finder 'conn-dispatch-headings
 :bounds-op (lambda ()
              (save-mark-and-excursion
                (outline-mark-subtree)
                (cons (region-beginning) (region-end))))
 :forward-op 'outline-next-visible-heading)

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
  "f" 'conn-dispatch-state
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

(conn-define-state conn-dired-dispatch-state (conn-emacs-state conn-dispatch-state)
  "State for dispatch in `dired-mode'."
  :cursor 'box
  :lighter "D"
  :suppress-input-method t)

(defun conn-dired-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-do-dispatch-state
   :state 'conn-dired-dispatch-state
   :no-other-end t
   :initial-arg initial-arg))

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
  (lambda ()
    (let ((dired-movement-style 'bounded))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (while (/= (point)
                     (progn
                       (dired-next-line 1)
                       (point)))
            (conn-make-target-overlay (point) 0)))))))

(defun conn--dispatch-dired-dirline ()
  (lambda ()
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-min))
        (while (/= (point)
                   (progn
                     (dired-next-dirline 1)
                     (point)))
          (conn-make-target-overlay (point) 0))))))

(defun conn--dispatch-dired-subdir ()
  (lambda ()
    (let ((start (window-start))
          (end (window-end)))
      (save-excursion
        (pcase-dolist (`(,_ . ,marker) dired-subdir-alist)
          (when (<= start marker end)
            (goto-char marker)
            (conn-make-target-overlay
             (+ 2 marker) (- (line-end-position) marker 2))))))))

(conn-register-thing
 'dired-line
 :dispatch-target-finder 'conn--dispatch-dired-lines
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'dired-line nil
 'dired-previous-line 'dired-next-line)

(conn-register-thing
 'dired-subdir
 :dispatch-target-finder 'conn--dispatch-dired-subdir
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'dired-subdir nil
 'dired-next-subdir 'dired-prev-subdir
 'dired-tree-up 'dired-tree-down)

(conn-register-thing
 'dired-dirline
 :dispatch-target-finder 'conn--dispatch-dired-dirline
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'dired-dirline nil
 'dired-next-dirline 'dired-prev-dirline)

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
      (window pt _bounds-op _bounds-arg)
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
      (window pt _bounds-op _bounds-arg)
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
      (window pt _bounds-op _bounds-arg)
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
  (lambda ()
    (let ((ibuffer-movement-cycle nil))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-max))
          (while (/= (point)
                     (progn
                       (ibuffer-backward-line)
                       (point)))
            (unless (get-text-property (point) 'ibuffer-filter-group-name)
              (conn-make-target-overlay (point) 0))))))))

(defun conn--dispatch-ibuffer-filter-group ()
  (lambda ()
    (let ((ibuffer-movement-cycle nil))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-max))
          (while (/= (point)
                     (progn
                       (ibuffer-backward-filter-group)
                       (point)))
            (conn-make-target-overlay (point) 0)))))))

(conn-register-thing
 'ibuffer-line
 :dispatch-target-finder 'conn--dispatch-ibuffer-lines
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'ibuffer-line nil
 'ibuffer-backward-line 'ibuffer-forward-line)

(conn-register-thing
 'ibuffer-filter-group
 :dispatch-target-finder 'conn--dispatch-ibuffer-filter-group
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'ibuffer-filter-group nil
 'ibuffer-forward-filter-group
 'ibuffer-backward-filter-group)

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
      (window pt _bounds-op _bounds-arg)
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
  "f" 'conn-dispatch-state
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
   nil nil))

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
  "f" 'conn-dispatch-state
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
  "f" 'conn-dispatch-state
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
  "f" 'conn-dispatch-state
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
  "f" 'conn-dispatch-state
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
  "f" 'conn-dispatch-state
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))


;;; Footer
;; Local Variables:
;; outline-regexp: "^;;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn.el ends here
