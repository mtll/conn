;;; conn.el --- A modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") (transient "0.6.0") (seq "2.23"))
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
(defvar conn-dispatch-window-predicate)

(defvar-local conn--hide-mark-cursor nil)

(declare-function kmacro-p "kmacro")
(declare-function kmacro-step-edit-macro "kmacro")
(declare-function project-files "project")
(declare-function conn-dispatch-kapply-prefix "conn-transients")


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

(defcustom conn-undo-keys (key-parse "C-/")
  "`undo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-undo-redo-keys (key-parse "C-?")
  "`undo-redo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-yank-keys (key-parse "C-y")
  "`yank' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-region-keys (key-parse "C-w")
  "`kill-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-region-keys (key-parse "C-S-w")
  "`delete-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sexp-keys (key-parse "C-M-f")
  "`forward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sexp-keys (key-parse "C-M-b")
  "`backward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-paragraph-keys (key-parse "M-{")
  "`backward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-paragraph-keys (key-parse "M-}")
  "`forward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-beginning-of-defun-keys (key-parse "C-M-a")
  "`beginning-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-end-of-defun-keys (key-parse "C-M-e")
  "`end-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-next-line-keys (key-parse "C-n")
  "`next-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-previous-line-keys (key-parse "C-p")
  "`previous-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-char-keys (key-parse "C-f")
  "`forward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-char-keys (key-parse "C-b")
  "`backward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-word-keys (key-parse "M-f")
  "`forward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-word-keys (key-parse "M-b")
  "`backward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sentence-keys (key-parse "M-a")
  "`backward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sentence-keys (key-parse "M-e")
  "`forward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-delete-char-keys (key-parse "DEL")
  "`backward-delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-char-keys (key-parse "C-d")
  "`delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-up-list-keys (key-parse "C-M-u")
  "`backward-up-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-down-list-keys (key-parse "C-M-d")
  "`down-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-list-keys (key-parse "C-M-n")
  "`forward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-list-keys (key-parse "C-M-p")
  "`backward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-line-keys (key-parse "C-k")
  "`kill-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))


;;;; Utilities

(defconst conn--hash-key-missing (make-symbol "key-missing"))

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

(defmacro conn--protected-let (varlist &rest body)
  "Bind variables according to VARLIST then eval body as in `let*'.

In addition to what `let*' accepts, each element of VARLIST may also be
of the form (SYMBOL VALUEFORM CLEANUP-FORM), which binds SYMBOL to
VALUEFORM and if BODY exits non-locally runs CLEANUP-FORM."
  (declare (indent 1))
  (cl-with-gensyms (success)
    `(let* ((,success nil)
            ,@(mapcar (lambda (form)
                        (if (consp form)
                            (take 2 form)
                          form))
                      varlist))
       (unwind-protect
           (prog1
               ,(macroexp-progn body)
             (setq ,success t))
         (unless ,success
           ,@(mapcan (lambda (form)
                       (when (consp form)
                         (drop 2 form)))
                     varlist))))))

(defmacro conn--with-advice (advice-forms &rest body)
  "Run BODY with ADVICE-FORMS temporarily applied.

ADVICE-FORMS are of the form (SYMBOL HOW FUNCTION PROPS), for the
meaning of these see `advice-add'."
  (declare (debug (form body))
           (indent 1))
  (cl-loop for (symbol how func props) in advice-forms
           collect `(advice-add ,symbol ,how ,func ,props) into adders
           collect `(advice-remove ,symbol ,func) into removers
           finally return `(unwind-protect
                               (progn
                                 ,@adders
                                 ,@body)
                             ,@removers)))

;; From repeat-mode
(defun conn--command-property (propname)
  "Return the value of the current commands PROPNAME property."
  (or (and (symbolp this-command)
           (get this-command propname))
      (and (symbolp real-this-command)
           (get real-this-command propname))))

;; This is string-pixel-width from emacs 31
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


;;;;; Rings

;; Expects no two items added to the ring to be eq to one another.  This
;; works well enough for now but maybe it should be fixed.
(cl-defstruct (conn-ring
               (:constructor conn--make-ring (capacity cleanup)))
  "A ring that removes elements in least recently visited order."
  list history capacity cleanup
  (size 0))

(defun conn-ring (capacity &key cleanup)
  (cl-assert (and (integerp capacity)
                  (> capacity 0)))
  (conn--make-ring capacity cleanup))

(define-inline conn-ring--visit (ring item)
  (inline-quote
   (setf (conn-ring-history ,ring)
         (cons ,item (delq ,item (conn-ring-history ,ring))))))

(defun conn-ring-insert-front (ring item)
  "Insert ITEM into front of RING."
  (push item (conn-ring-list ring))
  (conn-ring--visit ring item)
  (if (= (conn-ring-size ring) (conn-ring-capacity ring))
      (let ((old (nth (1- (conn-ring-size ring))
                      (conn-ring-history ring))))
        (setf (conn-ring-list ring) (delq old (conn-ring-list ring))
              (conn-ring-history ring) (take (conn-ring-size ring)
                                             (conn-ring-history ring)))
        (when-let* ((cleanup (conn-ring-cleanup ring)))
          (funcall cleanup old)))
    (cl-incf (conn-ring-size ring))))

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

(defun conn-ring-front (ring)
  "Return the front element of RING.

If ring is (1 2 3 4) 1 would be returned."
  (car (conn-ring-list ring)))

(defun conn-ring-back (ring)
  "Return the back element of RING.

If ring is (1 2 3 4) 4 would be returned."
  (car (last (conn-ring-list ring))))


;;;;; Keymap utils

(defvar conn-thing-map
  `(menu-item
    "Thing Map"
    nil
    :filter ,(lambda (_real-binding)
               (key-binding [conn-thing-map] t))))

(defvar conn-region-map
  `(menu-item
    "Region Map"
    nil
    :filter ,(lambda (_real-binding)
               (key-binding [conn-region-map] t))))

(defvar conn-edit-map
  `(menu-item
    "Edit Map"
    nil
    :filter ,(lambda (_real-binding)
               (key-binding [conn-edit-map] t))))

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

(defun conn-remap-key (from-keys &optional without-conn-maps)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  `(menu-item
    ,(format "Remap %s" (key-description from-keys))
    ,(conn--without-conn-maps (key-binding from-keys t))
    :filter ,(lambda (_real-binding)
               (if without-conn-maps
                   (conn--without-conn-maps
                     (key-binding from-keys t))
                 (key-binding from-keys t)))))

(defun conn-remap-keymap (from-keys &optional without-conn-maps)
  "Map to the keymap at FROM-KEYS.

If the binding at FROM-KEYS is for any reason not a keymap, say because
a minor mode has shadowed the keymap originally bound there, then map to
the original binding.  Also see `conn-remap-key'."
  `(menu-item
    ,(format "Remap %s Keymap" (key-description from-keys))
    ,(conn--without-conn-maps (key-binding from-keys t))
    :filter ,(lambda (real-binding)
               (let ((binding (if without-conn-maps
                                  (conn--without-conn-maps
                                    (key-binding from-keys t))
                                (key-binding from-keys t))))
                 (if (keymapp binding) binding real-binding)))))

(defmacro conntext-define (name &rest body)
  (declare (indent 1))
  `(progn
     (defun ,name () ,@body)
     (declare-function ,name nil)
     (list 'menu-item ,(symbol-name name) :conntext-key
           :filter (lambda (_) (,name)))))

(defun conntext-binding-form (conntext-function)
  (cl-check-type conntext-function symbol)
  `(menu-item (symbol-name ,conntext-function) :conntext-key
              :filter ,(lambda (_) (funcall conntext-function))))


;;;;; Region utils

(defmacro conn--with-region-emphasis (regions &rest body)
  "Run BODY with the text in the complement of REGIONS shadowed."
  (declare (debug (form form body))
           (indent 2))
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
  (and (not (invisible-p beg))
       (cl-loop for pt = (next-single-char-property-change
                          beg 'invisible nil end)
                while (and pt (< pt end))
                never (invisible-p pt))))

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


;;;;; Derived mode utils

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
  (cl-loop for mode in (conn--derived-mode-all-parents mode)
           for prop = (if-let* ((table (get mode :conn-properties)))
                          (gethash propname table conn--hash-key-missing)
                        conn--hash-key-missing)
           unless (eq conn--hash-key-missing prop) return prop))

(defun conn-set-mode-property (mode prop value)
  (let ((table (or (get mode :conn-properties)
                   (put mode :conn-properties (make-hash-table :test 'eq)))))
    (puthash prop value table)))

(gv-define-setter conn-get-mode-property (value mode prop)
  `(conn-set-mode-property ,mode ,prop ,value))


;;;;; Misc utils

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
                         (substring 0 20)))))
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

(defun conn--visible-matches (string &optional dir predicate)
  "Return all matches for STRING visible in the selected window.

If dir is \\='forward or \\='backward then restrict to matches before or
after point."
  (save-excursion
    (with-restriction
        (if (eq dir 'forward)  (point) (window-start))
        (if (eq dir 'backward) (point) (window-end))
      (goto-char (point-min))
      (let ((case-fold-search (conn--string-no-upper-case-p string))
            matches)
        (while (search-forward string nil t)
          (when (and (conn--region-visible-p (match-beginning 0) (match-end 0))
                     (or (null predicate)
                         (funcall predicate (match-beginning 0))))
            (push (match-beginning 0) matches)))
        (nreverse matches)))))

(defun conn--read-from-with-preview (prompt bounds &optional regexp-flag)
  "Read a from string with `minibuffer-lazy-highlight-setup' previews.

PROMPT is used as the minibuffer prompt when reading.

BOUNDS is a list of the form returned by `region-bounds' and defines the
limits of the highlighting.

REGEXP-FLAG means to treat the from string as a regexp for the purpose
of highlighting."
  (let ((default (conn--replace-read-default)))
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
                           (regexp-quote default)
                           'minibuffer-history)
            (let ((from (read-from-minibuffer
                         (format-prompt prompt default)
                         nil nil nil nil
                         (if default
                             (delete-dups
                              (cons default (query-replace-read-from-suggestions)))
                           (query-replace-read-from-suggestions))
                         t)))
              (or (and (length= from 0) default)
                  from)))))))


;;;;; Overlay utils

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

(defun conn-overlay-p (overlay)
  "Return t if OVERLAY is an overlay created by conn."
  (overlay-get overlay 'conn-overlay))

(defun conn--clear-overlays (&optional buffer)
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (cl-loop for ov being the overlays of buffer
             when (conn-overlay-p ov)
             do (delete-overlay ov))))


;;;;; Append/prepend keymaps

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


;;;;; Thing command type

(defun conn-thing-command-p (cmd)
  "Return non-nil if CMD is a dispatch action."
  (and (symbolp cmd)
       (not (not (get cmd :conn-command-thing)))))

(cl-deftype conn-thing-command () '(satisfies conn-thing-command-p))


;;;; States

(defcustom conn-buffer-state-setup-alist
  `(("COMMIT_EDITMSG" . nil)
    ((or (derived-mode . calc-mode)
         (derived-mode . calc-trail-mode)
         (derived-mode . calc-keypad-mode)
         (derived-mode . image-mode)
         (derived-mode . doc-view-mode)
         (derived-mode . pdf-view-mode))
     . conn-setup-null-state)
    ((derived-mode . dired-mode)
     . conn-setup-dired-state)
    ((major-mode . minibuffer-mode)
     . conn-setup-minibuffer-state)
    ((or (derived-mode . prog-mode)
         (derived-mode . text-mode)
         (derived-mode . conf-mode)
         (major-mode . fundamental-mode))
     . conn-setup-command-state))
  "Alist of the form ((CONDITION . STATE-SETUP-FN) ...).

Elements specify a STATE-SETUP-FN that should enter the default state
and optionally set any of the state variables `conn-state-for-emacs',
`conn-state-for-command', `conn-state-for-read-dispatch' or
`conn-state-for-read-mover' for buffers matching CONDITION.  For the
meaning of CONDITION see `buffer-match-p'."
  :type '(list (cons string symbol))
  :group 'conn-states)

(defvar-local conn-state-for-emacs 'conn-emacs-state)

(defvar-local conn-state-for-command 'conn-command-state)

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar-local conn-previous-state nil
  "Previous conn state in buffer.")

(defvar conn-next-state nil
  "Next conn state in buffer.

This variable will be bound to the state t be entered during
`conn-enter-state'.  In particular this will be bound when
`conn-enter-state' calls `conn-exit-state' and `conn-exit-functions'.")

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
     (with-memoization
         (gethash ,state conn--state-all-parents-cache)
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

(defun conn-setup-minibuffer-state ()
  (setq-local conn-state-for-emacs 'conn-minibuffer-state)
  (conn-enter-state 'conn-minibuffer-state)
  (letrec ((hook (lambda ()
                   (conn--push-ephemeral-mark)
                   (remove-hook 'minibuffer-setup-hook hook))))
    (add-hook 'minibuffer-setup-hook hook)))

(defun conn-setup-null-state ()
  (setq-local conn-state-for-emacs 'conn-null-state
              conn-state-for-command 'conn-null-state)
  (conn-enter-state 'conn-null-state))

(defun conn-setup-command-state ()
  (conn-enter-state conn-state-for-command))


;;;;; State keymaps

(defvar conn--state-maps (make-hash-table :test 'eq))
(defvar conn--override-maps (make-hash-table :test 'eq))
(defvar conn--major-mode-maps (make-hash-table :test 'eq))
(defvar conn--minor-mode-maps (make-hash-table :test 'eq))

(defvar-local conn--local-state-map)
(defvar-local conn--local-override-map)
(defvar-local conn--local-major-mode-map)
(defvar-local conn--local-minor-mode-maps)

(defun conn-get-overriding-map (state)
  "Return the overriding keymap for STATE."
  (cl-check-type state conn-state)
  (gethash state conn--override-maps))

(defconst conn--override-map-cache (make-hash-table :test 'eq))

(defun conn--compose-overide-map (state)
  "Return composed override map for STATE.

Composed keymap is of the same form as returned by
`conn--compose-state-map'."
  (with-memoization
      (gethash state conn--override-map-cache)
    (make-composed-keymap
     (cl-loop for pstate in (conn--state-all-parents state)
              collect (gethash pstate conn--override-maps)))))

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
  (with-memoization
      (gethash state conn--state-map-cache)
    (make-composed-keymap
     (cl-loop for pstate in (conn--state-all-parents state)
              collect (gethash pstate conn--state-maps)))))

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
  (conn-get-mode-map state mode)
  (let ((map-depths (or (car (gethash state conn--minor-mode-maps))
                        (setf (car (gethash state conn--minor-mode-maps))
                              (make-hash-table :test 'eq)))))
    (unless (eql (gethash mode map-depths) depth)
      (setf (gethash mode map-depths) depth)
      (mapc #'conn--sort-mode-maps
            (cons state (conn--state-all-children state)))))
  nil)

(defun conn-get-mode-map (state mode)
  "Return keymap for MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (cl-check-type state conn-state)
  (cl-assert (symbolp mode))
  (or (nth 1 (alist-get mode (cdr (gethash state conn--minor-mode-maps))))
      (prog1
          (let ((keymap (make-composed-keymap (make-sparse-keymap)))
                (alist (gethash state conn--minor-mode-maps)))
            (if (alist-get mode (cdr alist))
                (setf (alist-get mode alist) keymap)
              (setcdr alist (cons (cons mode keymap) (cdr alist))))
            (setf (cddr keymap)
                  (cl-loop for parent in (cdr (conn--state-all-parents state))
                           collect (conn-get-mode-map parent mode)))
            (dolist (child (conn--state-all-children state))
              (conn-get-mode-map child mode))
            (nth 1 keymap))
        (conn--sort-mode-maps state))))

(defun conn-get-major-mode-map (state mode)
  "Return keymap for major MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (cl-check-type state conn-state)
  (cl-assert (symbolp mode))
  (or (when-let* ((mmode-table (gethash state conn--major-mode-maps)))
        (nth 1 (gethash mode mmode-table)))
      (let* ((keymap (setf (gethash mode (gethash state conn--major-mode-maps))
                           (make-composed-keymap (make-sparse-keymap)))))
        (setf (cddr keymap)
              (cl-loop for parent in (cdr (conn--state-all-parents state))
                       collect (conn-get-major-mode-map parent mode)))
        (dolist (child (conn--state-all-children state))
          (conn-get-major-mode-map child mode))
        (nth 1 keymap))))

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


;;;;; State input methods

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
  ;; Ensure conn-local-mode is t since this can be run by conn--with-state
  ;; in buffers without conn-local-mode enabled.
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
    (letrec ((hook (lambda ()
                     (conn--activate-input-method)
                     (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
                     (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
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

(defmacro conn--with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  `(if conn--input-method
       (unwind-protect
           (progn
             (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
             (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
             (activate-input-method conn--input-method)
             ,@body)
         (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
         (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
         (conn--activate-input-method))
     ,@body))


;;;;; State properties

(define-inline conn-state-get (state property)
  "Return the value of PROPERTY for STATE.

If PROPERTY is not set for STATE then check all of STATE's parents for
PROPERTY.  If no parent has that property either than nil is returned."
  (inline-letevals (state property)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (cl-loop for parent in (conn--state-all-parents ,state)
                for table = (aref (get parent :conn--state) 1)
                for prop = (gethash ,property table conn--hash-key-missing)
                unless (eq prop conn--hash-key-missing) return prop)))))

(gv-define-setter conn-state-get (value state slot)
  `(conn-state-set ,state ,slot ,value))

(define-inline conn-state-has-property-p (state property)
  "Return t if PROPERTY is set for STATE."
  (inline-letevals (state property)
    (inline-quote
     (progn
       (cl-check-type ,state conn-state)
       (not (eq (gethash ,property (aref (get ,state :conn--state) 1)
                         conn--hash-key-missing)
                conn--hash-key-missing))))))

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


;;;;; State macros

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

(defmacro conn--with-state (transition-form &rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms ( saved-curr-state saved-prev-state
                     saved-cursor-type buffer)
    `(let ((,saved-curr-state conn-current-state)
           (,saved-prev-state conn-previous-state)
           (,buffer (current-buffer))
           (,saved-cursor-type cursor-type))
       (unwind-protect
           (progn
             ,(or transition-form
                  '(conn-exit-state conn-current-state))
             ,@body)
         (with-current-buffer ,buffer
           (if ,saved-curr-state
               (conn-enter-state ,saved-curr-state)
             (when conn-current-state
               (conn-exit-state conn-current-state))
             (setq cursor-type ,saved-cursor-type))
           (setq conn-previous-state ,saved-prev-state))))))


;;;;; cl-generic specializers

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

(cl-defmethod cl-generic-generalizers ((_specializer (eql 'conn-state)))
  "Support for conn-state specializers.
These match if the argument is a conn-state."
  (list conn--state-generalizer))


;;;;; Enter/exit functions

(defvar conn-exit-functions nil
  "Abnormal hook run when a state is exited.

Each function is passed the state being exited
(eg '`conn-command-state' or '`conn-emacs-state').

See also `conn-entry-functions'.")

(defvar conn-entry-functions nil
  "Abnormal hook run when a state is entered.

Each function is passed the state being entered
(eg '`conn-command-state' or '`conn-emacs-state').

See also `conn-exit-functions'.")

(cl-defgeneric conn-exit-state (state &key &allow-other-keys)
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
            (unless (conn-state-get state :transient)
              (setq conn-previous-state state))
            (set state nil)
            (cl-call-next-method)
            (setq success t))
        (unless success
          (conn-local-mode -1)
          (message "Error exiting state %s." (symbol-value state)))))
    (run-hook-wrapped
     'conn-exit-functions
     (lambda (fn)
       (condition-case err
           (funcall fn state)
         (t
          (remove-hook 'conn-exit-functions fn)
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

(cl-defmethod conn-enter-state :around ((state conn-state) &key &allow-other-keys)
  (unless (symbol-value state)
    (let ((success nil)
          (conn-next-state state))
      (unwind-protect
          (progn
            (conn-exit-state conn-current-state)
            (set state t)
            (setf
             conn-current-state state
             conn--local-override-map `((conn-local-mode . ,(conn--compose-overide-map state)))
             conn--local-state-map `((conn-local-mode . ,(conn--compose-state-map state)))
             conn--local-major-mode-map `((conn-local-mode . ,(conn--compose-major-mode-map state)))
             conn-lighter (or (conn-state-get state :lighter)
                              (default-value 'conn-lighter))
             conn--local-minor-mode-maps (gethash state conn--minor-mode-maps)
             conn--hide-mark-cursor (or (conn-get-mode-property major-mode :hide-mark-cursor)
                                        (conn-state-get state :hide-mark-cursor))
             cursor-type (or (conn-state-get state :cursor) t))
            (conn--activate-input-method)
            (cl-call-next-method)
            (unless executing-kbd-macro
              (force-mode-line-update))
            (setq success t))
        (unless success
          (conn-local-mode -1)
          (message "Error entering state %s." ',name))))
    (run-hook-wrapped
     'conn-entry-functions
     (lambda (fn)
       (condition-case err
           (funcall fn state)
         (t
          (remove-hook 'conn-entry-functions fn)
          (message "Error in conn-entry-functions: %s" (car err))))))))


;;;;; State Definitions

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
added as methods to `conn-enter-state' and `conn-exit-state', which see.

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
               (mapc #'conn--rebuild-state-keymaps all-children))
           (let ((record (make-record 'conn-state 3 nil)))
             (setf (aref record 1) state-props
                   (aref record 2) ',parents)
             (put ',name :conn--state record))
           (dolist (parent (cdr (conn--state-all-parents ',name)))
             (cl-pushnew ',name (aref (get parent :conn--state) 3)))
           (setf (gethash ',name conn--state-maps) (make-sparse-keymap)
                 (gethash ',name conn--override-maps) (make-sparse-keymap)
                 (gethash ',name conn--minor-mode-maps) (list nil)
                 (gethash ',name conn--major-mode-maps) (make-hash-table :test 'eq))))
       ',name)))

(conn-define-state conn-null-state ()
  "An empty state.

For use in buffers that should not have any other state."
  :hide-mark-cursor t
  :cursor '(bar . 4))

(conn-define-state conn-emacs-state ()
  "A `conn-mode' state for inserting text.

By default `conn-emacs-state' does not bind anything."
  :lighter " Emacs"
  :cursor '(bar . 4))

(conn-define-state conn-minibuffer-state (conn-emacs-state)
  "Default state for the minibuffer."
  :hide-mark-cursor t
  :cursor '(bar . 4))

(conn-define-state conn-movement-state ()
  "A `conn-mode' state moving in a buffer."
  :lighter " Move"
  :suppress-input-method t)

(cl-defmethod conn-enter-state ((_state (conn-substate conn-read-thing-common-state))
                                &key &allow-other-keys)
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line t))
  (cl-call-next-method))

(cl-defmethod conn-exit-state ((_state (conn-substate conn-read-thing-common-state)))
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line nil))
  (cl-call-next-method))

(conn-define-state conn-menu-state ()
  "A `conn-mode' state for remapping key menus.")

(conn-define-state conn-command-state
    (conn-menu-state conn-movement-state)
  "A `conn-mode' state for editing test."
  :lighter " Cmd"
  :suppress-input-method t
  :cursor 'box)

(conn-define-state conn-read-thing-common-state (conn-command-state)
  "Common elements of reading thing states."
  :suppress-input-method t
  :transient t)

(conn-define-state conn-org-edit-state ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  :lighter " OEdit"
  :suppress-input-method t)


;;;; Labels

;; Functions to provide the basic machinery for labeling a set of things
;; (buffer regions, windows, etc.) and prompting the user to select from
;; a set of labels.

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e" "r"
        "t" "y" "u" "i" "o" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defface conn-dispatch-label-face
  '((t (:inherit default :background "#ff8bd1" :foreground "black" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defvar conn-label-string-generator 'conn-simple-labels
  "Function to create label strings for a number of elements.")

(defvar conn-window-labeling-function 'conn-header-line-labels
  "Function to label windows for `conn-prompt-for-window'.

The function should accept a single argument, the list of windows to be
labeled and it should return a list of structs for `conn-label-select',
which see.")

(cl-defstruct (conn-dispatch-label)
  "Store the state for a dispatch label."
  string overlay prop target-overlay)

(cl-defstruct (conn-window-label)
  "Store the state for a window label."
  string window)

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
          (put-text-property 0 (length label)
                             'face (or face 'conn-dispatch-label-face)
                             label))
        (nreverse labels)))))

(defun conn--get-dispatch-windows (all-windows)
  (cond (all-windows
         (cl-loop for win in (conn--get-windows nil nil 'visible)
                  when (funcall conn-dispatch-window-predicate win)
                  collect win))
        ((funcall conn-dispatch-window-predicate (selected-window))
         (list (selected-window)))))


;;;;; Label reading

(cl-defgeneric conn-label-delete (label)
  "Delete the label LABEL.

This function is called on each label after a label has been selected
and allow labels to clean up after themselves.")

(cl-defgeneric conn-label-narrow (label prefix)
  "Narrow LABEL by PREFIX.

If LABEL contains PREFIX then the label state should be updated to
reflect that prefix has been processed and LABEL should be returned.  If
the label does not contain the prefix then the label state be updated
to reflect that the label is no longer active and nil should be
returned.")

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload.")

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (conn-dispatch-label-target-overlay label))

(cl-defmethod conn-label-reset ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label string overlay target-overlay)
               label))
    (with-current-buffer (overlay-buffer overlay)
      (overlay-put target-overlay 'display
                   (propertize (buffer-substring
                                (overlay-start target-overlay)
                                (overlay-end target-overlay))
                               'face 'conn-read-string-match-face))
      (conn--dispatch-setup-label
       overlay string (overlay-get target-overlay 'padding-function)))))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label)))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label) prefix-char)
  (pcase-let (((cl-struct conn-dispatch-label overlay prop target-overlay)
               label))
    (with-current-buffer (overlay-buffer overlay)
      (cond ((length= (overlay-get overlay prop) 0)
             nil)
            ((not (eql prefix-char (aref (overlay-get overlay prop) 0)))
             (move-overlay overlay (overlay-start overlay) (overlay-start overlay))
             (overlay-put overlay 'display nil)
             (overlay-put overlay 'after-string nil)
             (overlay-put overlay 'before-string nil)
             (overlay-put target-overlay 'display nil)
             (overlay-put target-overlay 'after-string nil)
             nil)
            (t
             (let ((new-string (substring (overlay-get overlay prop) 1)))
               (overlay-put overlay 'display nil)
               (overlay-put overlay 'before-string nil)
               (overlay-put overlay 'after-string nil)
               (conn--dispatch-setup-label
                overlay new-string (overlay-get target-overlay 'padding-function)))
             label)))))

(cl-defmethod conn-label-payload ((label conn-window-label))
  (conn-window-label-window label))

(cl-defmethod conn-label-reset ((label conn-window-label))
  (pcase-let (((cl-struct conn-window-label string window) label))
    (set-window-parameter window 'conn-label string)))

(cl-defmethod conn-label-delete ((label conn-window-label))
  (pcase-let (((cl-struct conn-window-label window string) label))
    (with-current-buffer (window-buffer window)
      (when (eq (car-safe (car-safe header-line-format))
                'conn-mode)
        (setq-local header-line-format (cadadr header-line-format))))
    (set-window-parameter window 'conn-label string)))

(cl-defmethod conn-label-narrow ((label conn-window-label) prefix-char)
  (pcase-let* (((cl-struct conn-window-label window) label)
               (string (window-parameter window 'conn-label)))
    (unless (or (length= string 0)
                (not (eql prefix-char (aref string 0))))
      (set-window-parameter window 'conn-label (substring string 1))
      label)))

(defun conn-label-select (candidates)
  "Select a label from CANDIDATES.

Prompts to user for prefix characters one at a time and narrows the
labels after each one.

Each of CANDIDATES should be a DFA that defines its transition functions
as methods of the `conn-label-narrow' and `conn-label-reset' generic
functions.  `conn-label-narrow' is called when user input is received
for the label to process and `conn-label-reset' is called when the user
has failed to select a label and the narrowing process must restart from
the beginning.  `conn-label-delete' allows labels to clean up after
themselves once the selection process has concluded."
  (let ((current candidates)
        (prompt "char:"))
    (cl-loop
     (pcase current
       ('nil
        (setq current candidates
              prompt "char: (no matches)")
        (mapc #'conn-label-reset current))
       (`(,it . nil)
        (cl-return (conn-label-payload it)))
       (_
        (setq prompt "char:")))
     (setq current (let ((next nil)
                         (c (conn-dispatch-read-event prompt)))
                     (dolist (label current next)
                       (when-let* ((l (conn-label-narrow label c)))
                         (push l next))))))))


;;;;; Target overlays

(defcustom conn-read-string-timeout 0.5
  "Timeout for string reading functions."
  :group 'conn
  :type 'number)

(defface conn-read-string-match-face
  '((t (:inherit isearch)))
  "Face for matches when reading strings."
  :group 'conn-faces)

(put 'conn-read-string-match 'conn-overlay t)
(put 'conn-read-string-match 'priority 2002)

(defun conn--make-target-overlay (pt length &optional thing padding-function)
  "Make a target overlay at PT of LENGTH.

Optionally the overlay may have an associated THING."
  (conn--protected-let
      ((eol (save-excursion
              (goto-char pt)
              (line-end-position)))
       (ov (make-overlay pt (min (+ pt length) eol) nil t)
           (delete-overlay ov)))
    (overlay-put ov 'conn-overlay t)
    (overlay-put ov 'thing thing)
    (overlay-put ov 'category 'conn-read-string-match)
    ;; Prevents overlay extending into invisible text ellipsis
    (when (> length 0)
      (overlay-put ov 'display
                   (propertize (buffer-substring (overlay-start ov)
                                                 (overlay-end ov))
                               'face 'conn-read-string-match-face)))
    (overlay-put ov 'window (selected-window))
    (overlay-put ov 'padding-function padding-function)
    ov))

(defun conn--string-preview-overlays-1 (win string &optional dir predicate)
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (with-selected-window win
      (dolist (pt (conn--visible-matches string dir predicate) ovs)
        (push (conn--make-target-overlay pt (length string)) ovs)))))

(defun conn--string-preview-overlays (string &optional dir all-windows predicate)
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (dolist (win (conn--get-dispatch-windows all-windows) ovs)
      (setq ovs (nconc (conn--string-preview-overlays-1 win string dir predicate)
                       ovs)))))

(defun conn--read-string-with-timeout-1 (&optional dir all-windows predicate)
  "Read a string with preview overlays and timeout `conn-read-string-timeout'.

Returns a cons of (STRING . OVERLAYS)."
  (conn--with-input-method
    (conn--protected-let
        ((prompt (propertize "string: " 'face 'minibuffer-prompt))
         (string (char-to-string (read-char prompt t)))
         (overlays nil (unless (eq overlays t)
                         (mapc #'delete-overlay overlays))))
      (setq overlays
            (while-no-input
              (conn--string-preview-overlays string dir all-windows predicate)))
      (while-let ((next-char (read-char (format (concat prompt "%s") string)
                                        t conn-read-string-timeout)))
        (setq string (concat string (char-to-string next-char)))
        (unless (eq overlays t)
          (mapc #'delete-overlay overlays))
        (setq overlays
              (while-no-input
                (conn--string-preview-overlays string dir all-windows predicate))))
      (message nil)
      (cons string overlays))))

(defun conn--read-string-with-timeout (&optional dir all-windows)
  (pcase-let ((`(,string . ,overlays)
               (conn--read-string-with-timeout-1 dir all-windows)))
    (mapc #'delete-overlay overlays)
    string))


;;;;; Window header-line labels

(defface conn-window-prompt-face
  '((default (:height 2.5 :foreground "#d00000"))
    (((background light)) (:height 2.5 :foreground "#d00000"))
    (((background dark)) (:height 2.5 :foreground "#7c0000")))
  "Face for conn window prompt overlay."
  :group 'conn-faces)

(defun conn--centered-header-label ()
  (let* ((window-width (window-width nil t))
         (label (window-parameter nil 'conn-label))
         (label-width (string-pixel-width label))
         (padding-width (floor (- window-width label-width) 2))
         (padding (propertize " " 'display `(space :width (,padding-width)))))
    (concat padding label)))

(defvar conn--window-label-pool
  (conn-simple-labels 30 'conn-window-prompt-face))

(defun conn--ensure-window-labels ()
  (let* ((windows (conn--get-windows nil 'nomini t))
         (window-count (length windows)))
    (when (length< conn--window-label-pool window-count)
      (setq conn--window-label-pool
            (conn-simple-labels (* 2 window-count) 'conn-window-prompt-face)))
    (cl-loop with available = (copy-sequence conn--window-label-pool)
             for win in windows
             for label = (window-parameter win 'conn-label)
             unless (and label
                         (when (member label available)
                           (setq available (delete label available))
                           t))
             collect win into unlabeled
             finally (dolist (win unlabeled)
                       (set-window-parameter win 'conn-label (pop available))))))

(defun conn-header-line-labels (windows)
  "Label WINDOWS using `head-line-format'."
  (let ((header-line-label
         '(conn-mode (:eval (conn--centered-header-label)))))
    (cl-loop for win in (conn--get-windows nil 'no-minibuff t)
             for string = (window-parameter win 'conn-label)
             when (memq win windows)
             collect
             (with-selected-window win
               (unless (equal header-line-label (car header-line-format))
                 (setq-local header-line-format
                             `(,header-line-label (nil ,header-line-format))))
               (make-conn-window-label :string string :window win)))))

;; From ace-window
(defun conn--get-windows (&optional window minibuffer all-frames dedicated)
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           unless (or ;; ignore child frames
                   (and (fboundp 'frame-parent) (frame-parent (window-frame window)))
                   ;; When `ignore-window-parameters' is nil, ignore windows whose
                   ;; `no-other-window’ or `no-delete-other-windows' parameter is non-nil.
                   (unless ignore-window-parameters
                     (window-parameter window 'no-other-window))
                   (and (null dedicated) (window-dedicated-p win)))
           collect win))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (cond
   ((length< windows 2)
    nil)
   ((and (length= windows 2)
         (not always-prompt))
    (if (eq (selected-window) (car windows))
        (cadr windows)
      (car windows)))
   (t
    (conn--ensure-window-labels)
    (let ((window-state
           (cl-loop for win in windows
                    collect (list (window-point win)
                                  (window-vscroll win)
                                  (window-hscroll win))
                    do (with-selected-window win
                         (goto-char (window-start)))))
          (labels (funcall conn-window-labeling-function windows)))
      (unwind-protect
          (conn-label-select labels)
        (mapc #'conn-label-delete labels)
        (cl-loop for win in windows
                 for (pt vscroll hscroll) in window-state
                 do (progn (set-window-point win pt)
                           (set-window-hscroll win hscroll)
                           (set-window-vscroll win vscroll))))))))


;;;; Read Things

;;;;; Read mover state

(defvar-local conn-state-for-read-mover 'conn-read-mover-state)

(conn-define-state conn-read-mover-state (conn-read-thing-common-state)
  "A state for reading things."
  :lighter " MOVER")

(define-keymap
  :keymap (conn-get-state-map 'conn-read-mover-state)
  "DEL" 'backward-delete-arg
  "C-d" 'forward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "<remap> <conn-forward-char>" 'forward-char
  "<remap> <conn-backward-char>" 'backward-char
  "C-h" 'help
  "h" conn-thing-map
  "e" 'recursive-edit)

(put 'reset-arg :advertised-binding (key-parse "M-DEL"))


;;;; Bounds of command

(defvar conn-bounds-of-command-alist
  `((conn-dispatch-on-things . conn--bounds-of-dispatch)
    (conn-toggle-mark-command . conn--bounds-of-region)
    (conn-expand-remote . conn--bounds-of-remote-expansion)
    (conn-expand . conn--bounds-of-expansion)
    (set-mark-command . conn--bounds-of-region)
    (conn-set-mark-command . conn--bounds-of-region)
    (visible . conn--bounds-of-command-thing)
    (narrowing . conn--bounds-of-narrowings)
    (dot . conn--bounds-of-dot)
    (conn-expand . ,(lambda (arg) (conn--bounds-of-expansion 'conn-expand arg)))
    (conn-contract . ,(lambda (arg) (conn--bounds-of-expansion 'conn-contract arg)))
    (recursive-edit . conn--bounds-of-dots))
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
  (setf (alist-get (recursion-depth) conn--last-bounds-of-command)
        (funcall (or (alist-get cmd conn-bounds-of-command-alist)
                     (when (symbolp cmd)
                       (alist-get (get cmd :conn-command-thing)
                                  conn-bounds-of-command-alist))
                     conn-bounds-of-command-default)
                 cmd arg)))

(defun conn-read-thing-mover (prompt &optional arg recursive-edit)
  "Interactively read a thing command and arg.

PROMPT is the prompt that will be displayed to the user.
ARG is the initial value for the arg to be returned.
RECURSIVE-EDIT allows `recursive-edit' to be returned as a thing
command.  See `conn-dot-mode' for how bounds of `recursive-edit'
are read."
  (let* ((thing-arg (when arg (abs (prefix-numeric-value arg))))
         (thing-sign (when arg (> 0 arg)))
         invalid keys cmd)
    (cl-flet
        ((read-command ()
           (setq keys (read-key-sequence
                       (format prompt
                               (format (if thing-arg "%s%s" "[%s1]")
                                       (if thing-sign "-" "")
                                       thing-arg)
                               (if invalid
                                   (propertize
                                    (format "%s is not a valid thing command"
                                            cmd)
                                    'face 'error)
                                 "")))
                 cmd (key-binding keys t)))
         (completing-read-command ()
           (save-window-excursion
             (setq cmd (condition-case _
                           (intern
                            (completing-read
                             "Command: "
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   `(metadata
                                     ,(cons 'affixation-function
                                            (conn--dispatch-make-command-affixation))
                                     (category . conn-dispatch-command))
                                 (complete-with-action action obarray string pred)))
                             (lambda (sym)
                               (and (functionp sym)
                                    (not (eq sym 'help))
                                    (conn-thing-command-p sym)))
                             t))
                         (quit nil))))))
      (conn--with-state (conn-enter-state
                         (or (conn--command-property :conn-read-state)
                             conn-state-for-read-mover))
        (setq prompt (substitute-command-keys
                      (concat (propertize prompt 'face 'minibuffer-prompt)
                              " (arg: "
                              (propertize "%s" 'face 'read-multiple-choice-face)
                              "; \\[reset-arg] reset arg; \\[help] commands"
                              (if recursive-edit
                                  (concat "; \\[recursive-edit] "
                                          "recursive edit)")
                                ")")
                              ": %s")))
        (read-command)
        (unwind-protect
            (cl-loop
             (catch 'continue
               (pcase cmd
                 ('keyboard-quit (keyboard-quit))
                 ('help
                  (throw 'continue (completing-read-command)))
                 ('digit-argument
                  (let ((digit (- (logand (elt keys 0) ?\177) ?0)))
                    (setq thing-arg (if thing-arg (+ (* 10 thing-arg) digit) digit))))
                 ('reset-arg
                  (setq thing-arg nil))
                 ('backward-delete-arg
                  (setq thing-arg (floor thing-arg 10)))
                 ('forward-delete-arg
                  (setq thing-arg (thread-last
                                    (log thing-arg 10)
                                    (floor)
                                    (expt 10)
                                    (mod thing-arg))))
                 ('negative-argument
                  (setq thing-sign (not thing-sign)))
                 ((guard (or (conn-thing-command-p cmd)
                             (alist-get cmd conn-bounds-of-command-alist)))
                  (cl-return
                   (list cmd (cond (thing-arg (* thing-arg (if thing-sign -1 1)))
                                   (thing-sign '-)))))
                 (_ (setq invalid t)))
               (read-command)))
          (message nil))))))

(defun conn-read-thing-region (prompt &optional arg)
  "Interactively read a thing region from the user.

See `conn-read-thing-mover' and `conn-bounds-of-command' for the region
is read."
  (pcase-let* ((`(,cmd ,arg) (conn-read-thing-mover prompt arg t)))
    (cons (get cmd :conn-command-thing)
          (conn-bounds-of-command cmd arg))))


;;;;; bounds-of-command providers

(defun conn--bounds-of-command-thing (cmd _arg)
  (let* ((thing (get cmd :conn-command-thing))
         (bounds (ignore-errors (bounds-of-thing-at-point thing))))
    (list bounds bounds)))

(defun conn--bounds-of-thing-command-default (cmd arg)
  (let ((current-prefix-arg (when (> 0 (prefix-numeric-value arg)) '-))
        (conn-this-command-handler (or (conn-get-mark-handler cmd)
                                       'conn-discrete-thing-handler))
        (conn-this-command-thing (get cmd :conn-command-thing))
        (this-command cmd)
        (conn-this-command-start (point-marker))
        (regions))
    (save-mark-and-excursion
      (dotimes (_ (abs (prefix-numeric-value arg)))
        (call-interactively cmd)
        (funcall conn-this-command-handler conn-this-command-start)
        (move-marker conn-this-command-start (point))
        (push (cons (region-beginning) (region-end)) regions))
      (cons (cons (seq-min (mapcar #'car regions))
                  (seq-max (mapcar #'cdr regions)))
            (nreverse regions)))))

(defun conn--bounds-of-region (_cmd _arg)
  (cons (cons (region-beginning) (region-end))
        (region-bounds)))

(defun conn--bounds-of-window (_cmd _arg)
  (list (cons (window-start) (window-end))))

(defun conn--bounds-of-remote-expansion (_cmd arg)
  (conn--push-ephemeral-mark)
  (conn--bounds-of-expansion 'conn-expand arg))


;;;; Bounds of things in region

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


;;;;; Dots

(defface conn-dot-face-1
  '((default (:inherit match)))
  "Face for dots."
  :group 'conn-faces)

(defface conn-dot-face-2
  '((default (:inherit highlight)))
  "Face for dots."
  :group 'conn-faces)

(defface conn-dot-face-3
  '((default (:inherit isearch)))
  "Face for dots."
  :group 'conn-faces)

(defcustom conn-dot-faces
  (list 'conn-dot-face-1 'conn-dot-face-2 'conn-dot-face-3)
  "List of faces for dots."
  :group 'conn
  :type '(list symbol))

(defvar conn--dots nil)

(put 'conn--dot-overlay 'conn-overlay t)
(put 'conn--dot-overlay 'evaporate t)

(defun conn--create-dot (beg end &optional buffer point)
  (with-current-buffer (or buffer (current-buffer))
    (cl-loop for ov in (overlays-in beg end)
             do (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
                  (let ((b (overlay-start ov))
                        (e (overlay-end ov)))
                    (when (or (< beg b end) (< beg e end))
                      (setq beg (min beg (overlay-start ov))
                            end (max end (overlay-end ov)))
                      (delete-overlay ov)))))
    (let ((dot (make-overlay beg end buffer))
          (faces
           (cl-loop for ov in (append (overlays-at (1- beg)) (overlays-at end))
                    when (eq 'conn--dot-overlay (overlay-get ov 'category))
                    collect (overlay-get ov 'face) into faces
                    finally return (seq-difference conn-dot-faces faces))))
      (overlay-put dot 'category 'conn--dot-overlay)
      (overlay-put dot 'point point)
      (overlay-put dot 'face (car faces))
      (push dot conn--dots))))

(defun conn--bounds-of-dot (_cmd _arg)
  (catch 'found
    (dolist (ov (overlays-at (point)))
      (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
        (throw 'found (list (conn--overlay-bounds-markers ov)))))))


;;;;;; Dot commands

(defun conn-mouse-click-dot (event)
  "Create dot at mouse click event."
  (interactive "e")
  (let* ((posn (event-start event))
         (point (posn-point posn)))
    (conn--create-dot point (1+ point) (window-buffer (posn-window posn)) t)))

(defun conn-point-to-dot (point)
  "Create dot at point."
  (interactive (list (point)))
  (conn--create-dot point (1+ point) nil t))

(defun conn-region-to-dot (bounds)
  "Create dot covering region BOUNDS.

BOUNDS is of the form returned by `region-bounds'."
  (interactive (list (region-bounds)))
  (pcase-dolist (`(,beg . ,end) bounds)
    (conn--create-dot beg end))
  (deactivate-mark t))

(defun conn-thing-to-dot (thing-cmd thing-arg)
  "Create dots at thing command region."
  (interactive (conn-read-thing-mover "Mover"))
  (pcase-dolist (`(,beg . ,end) (cdr (conn-bounds-of-command thing-cmd thing-arg)))
    (conn--create-dot beg end))
  (deactivate-mark t))

(defun conn-delete-dot-at-click (event)
  "Delete dot at mouse click event."
  (interactive "e")
  (let* ((posn (event-start event))
         (point (posn-point posn)))
    (with-current-buffer (window-buffer (posn-window posn))
      (dolist (ov (overlays-at point))
        (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
          (delete-overlay ov))))))

(defun conn-delete-dots ()
  "Delete dots at point or if the region is active then within region."
  (interactive)
  (cl-flet ((get-dots (ovs)
              (cl-loop for ov in ovs
                       when (eq (overlay-get ov 'category) 'conn--dot-overlay)
                       collect ov)))
    (dolist (ov (if (use-region-p)
                    (get-dots (overlays-in (region-beginning) (region-end)))
                  (or (get-dots (overlays-at (point)))
                      (get-dots (overlays-at (1- (point)))))))
      (delete-overlay ov))
    (deactivate-mark t)))


;;;;;; Dot mode

(defun conn--dot-mode-command-hook ()
  (when (overlay-buffer mouse-secondary-overlay)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (let* ((beg (overlay-start mouse-secondary-overlay))
             (end (overlay-end mouse-secondary-overlay))
             (ov (make-overlay beg (if (= beg end) (1+ beg) end))))
        (overlay-put ov 'category 'conn--dot-overlay)
        (push ov conn--dots)
        (delete-overlay mouse-secondary-overlay)))))

(defvar-keymap conn-dot-mode-map
  "C-w" 'conn-region-to-dot
  "C-d" 'conn-point-to-dot
  "t" 'conn-thing-to-dot
  "S-<mouse-1>" 'conn-mouse-click-dot
  "S-<down-mouse-1>" 'conn-mouse-click-dot
  "S-<drag-mouse-1>" 'conn-mouse-click-dot
  "S-<mouse-3>" 'conn-delete-dot-at-click
  "S-<down-mouse-3>" 'conn-delete-dot-at-click
  "DEL" 'conn-delete-dots
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(define-minor-mode conn-dot-mode
  "Minor mode for creating dots when finding the bounds of a recursive-edit."
  :global t
  :lighter " DOT"
  :interactive nil
  (if conn-dot-mode
      (progn
        (internal-push-keymap conn-dot-mode-map 'overriding-terminal-local-map)
        (delete-overlay mouse-secondary-overlay)
        (setq conn--dots nil
              buffer-read-only t)
        (add-hook 'post-command-hook 'conn--dot-mode-command-hook))
    (internal-pop-keymap conn-dot-mode-map 'overriding-terminal-local-map)
    (remove-hook 'post-command-hook 'conn--dot-mode-command-hook)
    (mapc #'delete-overlay conn--dots)
    (setq conn--dots nil
          buffer-read-only nil)))

(defun conn--bounds-of-dots (&rest _)
  (conn-dot-mode 1)
  (save-mark-and-excursion
    (unwind-protect
        (let ((dots
               (cl-loop for dot in (progn (recursive-edit) conn--dots)
                        when (overlay-buffer dot)
                        collect (if (overlay-get dot 'point)
                                    (cons (conn--overlay-start-marker dot)
                                          (conn--overlay-start-marker dot))
                                  (conn--overlay-bounds-markers dot)))))
          (if dots
              (cl-loop for (b . e) in dots
                       minimize b into beg
                       maximize e into end
                       finally return (cons (cons beg end) (nreverse dots)))
            (list (cons (region-beginning) (region-end)))))
      (conn-dot-mode -1)
      (deactivate-mark t))))


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


;;;;; Kapply query

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
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:finalize
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
      (:finalize
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
        (:finalize
         (when (consp points)
           (dolist (pt points)
             (set-marker pt nil))))
        ((or :record :next)
         (when-let* ((pt (pop points)))
           (cons pt pt)))))))

(defun conn--kapply-match-iterator ( string regions &optional
                                     sort-function regexp-flag delimited-flag)
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
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:finalize
         (when (consp matches)
           (mapc (pcase-lambda (`(,beg . ,end))
                   (set-marker beg nil)
                   (set-marker end nil))
                 matches)))
        ((or :next :record)
         (pop matches))))))


;;;;; Pipeline functions

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
              for cont = (funcall iterator state)
              until (or (null cont)
                        (progn
                          (recenter nil)
                          (move-overlay hl (region-beginning) (region-end) (current-buffer))
                          (y-or-n-p (format "Record here?"))))
              finally return cont)
           (delete-overlay hl))))
      ((or :finalize :next)
       (funcall iterator state)))))

(defun conn--kapply-skip-empty (iterator)
  (lambda (state)
    (pcase state
      (:finalize
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
      (:finalize
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
      (:finalize (funcall iterator state)))))

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
      (:finalize (funcall iterator state)))))

(defun conn--kapply-open-invisible (iterator)
  (let (restore)
    (lambda (state)
      (pcase state
        ((or :next :record)
         (prog1
             (cl-loop with search-invisible = 'open
                      for ret = (funcall iterator state)
                      until (or (null ret)
                                (isearch-filter-visible (car ret) (cdr ret)))
                      do (pcase-let ((`(,beg . ,end) ret))
                           (when (markerp beg) (set-marker beg nil))
                           (when (markerp end) (set-marker end nil)))
                      finally return ret)
           (setq restore (append isearch-opened-overlays restore)
                 isearch-opened-overlays nil)))
        (:finalize
         (funcall iterator state)
         (setq isearch-opened-overlays restore))))))

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
        (:finalize
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
        (:finalize
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
        (:finalize
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
        (:finalize
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
         (prog1 (funcall iterator state)
           (unless (alist-get (current-buffer) saved-excursions)
             (setf (alist-get (current-buffer) saved-excursions)
                   (cons (point-marker) (save-mark-and-excursion--save))))))))))

(defun conn--kapply-ibuffer-overview (iterator)
  (let ((msg (substitute-command-keys
              "\\<query-replace-map>Buffer is modified, save before continuing?\
 \\[act], \\[skip], \\[quit], \\[edit], \\[automatic], \\[help]"))
        buffers automatic)
    (lambda (state)
      (pcase state
        (:finalize
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
        (:finalize
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
  (let (buffer-states)
    (lambda (state)
      (prog1
          (funcall iterator state)
        (pcase state
          (:finalize
           (pcase-dolist (`(,buf ,state ,prev-state) buffer-states)
             (when state
               (with-current-buffer buf
                 (conn-enter-state state)
                 (setq conn-previous-state prev-state)))))
          ((or :record :next)
           (when conn-local-mode
             (unless (alist-get (current-buffer) buffer-states)
               (setf (alist-get (current-buffer) buffer-states)
                     (list conn-current-state conn-previous-state)))
             (conn-enter-state conn-state))))))))

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
        (:finalize
         (funcall iterator state)
         (set-window-configuration wconf))
        ((or :record :next)
         (unless wconf (setq wconf (current-window-configuration)))
         (funcall iterator state))))))


;;;;; Applier definitions

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
           (run-hook-wrapped 'conn-kmacro-apply-start-hook
                             (lambda (hook)
                               (ignore-errors (funcall hook))))
           (deactivate-mark)
           (unwind-protect
               (conn--with-advice (('kmacro-loop-setup-function :before-while ,iterator))
                 ,@body
                 (setq success t)
                 (message "Kapply completed successfully after %s iterations" ,iterations))
             (let ((conn-kmacro-apply-error (not success)))
               (funcall ,iterator :finalize)
               (run-hook-wrapped 'conn-kmacro-apply-end-hook
                                 (lambda (hook)
                                   (ignore-errors (funcall hook))))
               (isearch-clean-overlays))))))))

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


;;;; Mark handlers

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
              (when (= (point) beg) (conn--push-ephemeral-mark end))
              (when (= (point) end) (conn--push-ephemeral-mark beg)))))
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


;;;; Mark cursor

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :group 'conn)

(defface conn-mark-face
  '((default (:inherit cursor :background "#b8a2f0"))
    (((background light)) (:inherit cursor :background "#b8a2f0"))
    (((background dark)) (:inherit cursor :background "#a742b0")))
  "Face for conn mark cursor."
  :group 'conn-faces)

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.

Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--ephemeral-mark nil)

(put 'conn--mark-cursor 'permanent-local t)
(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)
(put 'conn--mark-cursor 'conn-overlay t)

(defun conn--mark-cursor-p (ov)
  (eq (overlay-get ov 'category) 'conn--mark-cursor))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.

For the meaning of MSG and ACTIVATE see `push-mark'."
  (if (not conn-local-mode)
      (push-mark location (not msg) activate)
    (push-mark location (not msg) activate)
    (setq conn--ephemeral-mark t)
    nil))

(defun conn--mark-cursor-redisplay (win)
  (let ((cursor (window-parameter win 'conn-mark-cursor)))
    (if (or (not conn-local-mode)
            conn--hide-mark-cursor
            (null (mark t)))
        (progn
          (when cursor (delete-overlay cursor))
          (set-window-parameter win 'conn-mark-cursor nil))
      (unless cursor
        (setq cursor (set-window-parameter
                      win 'conn-mark-cursor
                      (make-overlay (mark t) (1+ (mark t)) nil t)))
        (overlay-put cursor 'category 'conn--mark-cursor)
        (overlay-put cursor 'window win))
      (cond ((= (point-max) (mark t) (point))
             (when (overlay-get cursor 'after-string)
               (overlay-put cursor 'after-string nil))
             (unless (eql (overlay-start cursor) (overlay-end cursor))
               (move-overlay cursor (point-max) (point-max) (window-buffer win))))
            ((and (eql (overlay-buffer cursor) (current-buffer))
                  (eql (overlay-start cursor) (mark t))
                  (eql (overlay-end cursor) (1+ (mark t)))))
            ((and (eql (char-after (mark t)) ?\t)
                  (< 1 (save-excursion
                         (goto-char (mark t))
                         (let ((col (current-column)))
                           (- (indent-next-tab-stop col) col)))))
             (move-overlay cursor (mark t) (mark t) (window-buffer win))
             (overlay-put cursor 'after-string
                          (propertize " " 'face 'conn-mark-face)))
            ((= (mark t) (point-max))
             (move-overlay cursor (point-max) (point-max) (window-buffer win))
             (overlay-put cursor 'after-string
                          (propertize " " 'face 'conn-mark-face)))
            (t
             (move-overlay cursor (mark t) (1+ (mark t)) (window-buffer win))
             (overlay-put cursor 'after-string nil))))))

(defvar conn--movement-rotating nil)
(defvar conn--movement-tick nil)
(defvar conn--movement-mark nil)

(defun conn--mark-pre-command-hook ()
  (unless conn--hide-mark-cursor
    (set-marker conn-this-command-start (point))
    (setq conn-this-command-handler (or (alist-get this-command conn-mark-handler-overrides-alist)
                                        (conn--command-property :conn-mark-handler))
          conn-this-command-thing (conn--command-property :conn-command-thing)
          conn--movement-tick (buffer-chars-modified-tick)
          conn--movement-mark (mark t)
          conn--movement-rotating nil)))

(defun conn--mark-post-command-hook ()
  (unless conn--hide-mark-cursor
    (setf (alist-get (recursion-depth) conn--last-bounds-of-command) nil)
    (when (and conn-local-mode
               (eq (current-buffer) (marker-buffer conn-this-command-start)))
      (when (and conn-this-command-handler
                 (not (region-active-p)))
        (ignore-errors
          (funcall conn-this-command-handler conn-this-command-start)))
      (unless (or conn--movement-rotating
                  (not (eql conn--movement-tick (buffer-chars-modified-tick)))
                  (eql (mark t) conn--movement-mark))
        (conn-push-region conn-this-command-start conn--movement-mark)))))

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


;;;; Thing Dispatch

;; Thing dispatch provides a method of jumping to, marking or acting
;; on visible Things.

(cl-defstruct (conn--action)
  interactive description window-predicate filter)

(defun conn-action-p (command)
  "Return non-nil if COMMAND is a dispatch action."
  (not (not (get command :conn--action))))

(cl-deftype conn-action () '(satisfies conn-action-p))

(defvar conn--last-dispatch-command nil)


;;;;; Dispatch read thing

(defvar conn-dispatch-default-target-finder 'conn--dispatch-chars
  "Default target finder for dispatch.

A target finder function should return a list of overlays.")

(defvar conn-dispatch-target-finders-alist
  `((conn-backward-inner-line . conn--dispatch-inner-lines-end)
    (move-end-of-line . conn--dispatch-lines-end)
    (conn-backward-symbol . ,(lambda () (conn--dispatch-all-things 'symbol t)))
    (backward-word . ,(lambda () (conn--dispatch-all-things 'word t))))
  "Default target finders for for things or commands.

Is an alist of the form (((or THING CMD) . TARGET-FINDER) ...).  When
determining the target finder for a command in `conn-read-dispatch-state'
actions associated with a command have higher precedence than actions
associated with a thing.

For the meaning of TARGET-FINDER see
`conn-dispatch-default-target-finder'.")

(defvar conn-dispatch-action-default 'conn-dispatch-goto
  "Default action function for `conn-dispatch-on-things'.

For the meaning of action function see `conn-define-dispatch-action'.")

(defvar conn-dispatch-default-action-alist
  '((conn-expand . conn-dispatch-jump))
  "Default action functions for things or commands.

Is an alist of the form (((or THING CMD) . ACTION) ...).  When
determining the default action for a command in `conn-read-dispatch-state'
actions associated with a command have higher precedence than actions
associated with a command's thing.

For the meaning of ACTION see `conn-define-dispatch-action'.")

(defvar-local conn-state-for-read-dispatch 'conn-read-dispatch-state
  "Default state for performing `conn--dispatch-read-thing'.

The default may be overridden by setting the :conn-read-dispatch-state
of a command.")

(conn-define-state conn-dispatch-bounds-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter " DISPATCH")

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-bounds-state)
  "C-h" 'help
  "M-DEL" 'reset-arg
  "TAB" 'repeat-dispatch
  "'" 'repeat-dispatch
  "C-d" 'forward-delete-arg
  "DEL" 'backward-delete-arg
  "e" 'conn-dispatch-over)

(conn-define-state conn-read-dispatch-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter " DISPATCH")

(define-keymap
  :keymap (conn-get-state-map 'conn-read-dispatch-state)
  "C-h" 'help
  "M-DEL" 'reset-arg
  "TAB" 'repeat-dispatch
  "'" 'repeat-dispatch
  "C-d" 'forward-delete-arg
  "DEL" 'backward-delete-arg
  "\\" 'kapply)

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(define-keymap
  :keymap (conn-get-overriding-map conn-state-for-read-dispatch)
  "l" 'forward-line
  "u" 'forward-symbol
  "j" 'forward-char
  "," `(conn-expand-remote conn--dispatch-chars)
  "n" 'conn-forward-defun
  "J" 'conn-forward-inner-line
  "L" 'conn-backward-inner-line
  "O" `(forward-word ,(lambda () (conn--dispatch-all-things 'word t)))
  "U" `(forward-symbol ,(lambda () (conn--dispatch-all-things 'symbol t))))

(define-keymap
  :keymap (conn-get-mode-map conn-state-for-read-dispatch 'conn-dot-mode)
  "k" `(dot ,(lambda ()
               (mapcar (lambda (ov)
                         (conn--make-target-overlay
                          (overlay-start ov)
                          (- (overlay-end ov)
                             (overlay-start ov))))
                       conn--dots))))

(defun conn--dispatch-target-finder (command)
  (or (alist-get command conn-dispatch-target-finders-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-target-finders-alist)
      conn-dispatch-default-target-finder))

(defun conn--dispatch-default-action (command)
  (or (alist-get command conn-dispatch-default-action-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-default-action-alist)
      conn-dispatch-action-default))

(defun conn--dispatch-make-command-affixation ()
  (lambda (command-names)
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
                 (list command-name "" (concat thing binding)))))))

(defun conn--dispatch-read-thing (&optional default-action)
  (let* ((window-conf (current-window-configuration))
         (prompt nil)
         (action nil)
         (action-struct nil)
         (action-args nil)
         (keys nil)
         (cmd nil)
         (invalid nil)
         (handle nil)
         (saved-marker (save-mark-and-excursion--save))
         (thing-arg nil)
         (thing-sign nil)
         (repeat nil)
         (success nil)
         (repeat-indicator
          (propertize "repeatedly"
                      'face (when repeat
                              'eldoc-highlight-function-argument))))
    (cl-labels
        ((action-description ()
           (if-let* ((desc (and action-struct
                                (conn--action-description action-struct))))
               (propertize
                (if (stringp desc)
                    (apply #'format desc action-args)
                  (apply desc action-args))
                'face 'eldoc-highlight-function-argument)
             ""))
         (read-action-args ()
           (when handle
             (cancel-change-group handle)
             (save-mark-and-excursion--restore saved-marker)
             (setq saved-marker (save-mark-and-excursion--save)))
           (setq handle (prepare-change-group))
           (activate-change-group handle)
           (unwind-protect
               (when-let* ((int (conn--action-interactive action-struct)))
                 (funcall int))
             (set-window-configuration window-conf)))
         (completing-read-command ()
           (unwind-protect
               (setq keys nil
                     cmd (condition-case _
                             (intern
                              (completing-read
                               "Command: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata
                                       ,(cons 'affixation-function
                                              (conn--dispatch-make-command-affixation))
                                       (category . conn-dispatch-command))
                                   (complete-with-action action obarray string pred)))
                               (lambda (sym)
                                 (pcase sym
                                   ('help)
                                   ((and (pred functionp)
                                         (guard (or (get sym :conn-command-thing)
                                                    (conn-action-p sym))))
                                    t)
                                   (`(,_ ,_ . ,_) t)))
                               t))
                           (quit nil)))
             (set-window-configuration window-conf)))
         (read-command ()
           (setq keys (read-key-sequence
                       (format prompt
                               (format (if thing-arg "%s%s" "[%s1]")
                                       (if thing-sign "-" "")
                                       thing-arg)
                               repeat-indicator
                               (cond
                                (invalid
                                 (concat
                                  (action-description)
                                  " "
                                  (propertize "Not a valid thing command"
                                              'face 'error)))
                                (action (action-description))
                                (t ""))))
                 cmd (key-binding keys t)
                 invalid nil))
         (set-action (cmd)
           (if cmd
               (setq action cmd
                     action-struct (get cmd :conn--action)
                     action-args (read-action-args))
             (setq action nil
                   action-struct nil
                   action-args nil)))
         (read-dispatch ()
           (read-command)
           (while t
             (catch 'continue
               (pcase cmd
                 (`(,thing ,finder . ,default-action)
                  (unless action
                    (set-action (or default-action
                                    (conn--dispatch-default-action thing))))
                  (cl-return-from read-dispatch
                    (list thing
                          (when thing-arg
                            (* (if thing-sign -1 1) (or thing-arg 1)))
                          finder action action-args
                          (get action :conn-action-window-predicate)
                          repeat)))
                 ('keyboard-quit
                  (keyboard-quit))
                 ('kapply
                  (conn--with-state (conn-enter-state conn-previous-state)
                    (conn-dispatch-kapply-prefix
                     (lambda (action)
                       (set-window-configuration window-conf)
                       (run action))))
                  (setq quit-flag t))
                 ('repeat-dispatch
                  (setq repeat (not repeat)
                        repeat-indicator
                        (propertize repeat-indicator
                                    'face (when repeat
                                            'eldoc-highlight-function-argument))))
                 ('digit-argument
                  (let ((digit (- (logand (elt keys 0) ?\177) ?0)))
                    (setq thing-arg (if thing-arg (+ (* 10 thing-arg) digit) digit))))
                 ('backward-delete-arg
                  (setq thing-arg (floor thing-arg 10)))
                 ('forward-delete-arg
                  (setq thing-arg (when thing-arg
                                    (thread-last
                                      (log thing-arg 10)
                                      floor
                                      (expt 10)
                                      (mod thing-arg)))))
                 ('reset-arg
                  (setq thing-arg nil))
                 ('negative-argument
                  (setq thing-sign (not thing-sign)))
                 ('help
                  (throw 'continue (completing-read-command)))
                 ((and (let thing (or (alist-get cmd conn-bounds-of-command-alist)
                                      (when (symbolp cmd)
                                        (get cmd :conn-command-thing))))
                       (guard thing))
                  (unless action
                    (set-action (conn--dispatch-default-action cmd)))
                  (cl-return-from read-dispatch
                    (list cmd
                          (when thing-arg
                            (* (if thing-sign -1 1) (or thing-arg 1)))
                          (conn--dispatch-target-finder cmd)
                          action action-args
                          (get action :conn-action-window-predicate)
                          repeat)))
                 ((and cmd (pred conn-action-p))
                  (set-action (unless (eq cmd action) cmd)))
                 (_
                  (setq invalid t)))
               (read-command))))
         (run (action)
           (unwind-protect
               (conn--with-state (conn-enter-state
                                  (or (conn--command-property :conn-read-dispatch-state)
                                      conn-state-for-read-dispatch))
                 (setq prompt (substitute-command-keys
                               (concat (propertize "Targets" 'face 'minibuffer-prompt)
                                       " (arg: "
                                       (propertize "%s" 'face 'read-multiple-choice-face)
                                       "; \\[reset-arg] reset arg; "
                                       "\\[repeat-dispatch] %s; "
                                       "\\[help] commands): %s")))
                 (set-action action)
                 (prog1 (read-dispatch)
                   (setq success t)))
             (when handle
               (if success
                   (accept-change-group handle)
                 (cancel-change-group handle))
               (setq handle nil))
             (save-mark-and-excursion--restore saved-marker)
             (message nil))))
      (run default-action))))


;;;;; Dispatch window filtering

(defcustom conn-dispatch-thing-ignored-modes
  (list 'image-mode 'doc-view-mode 'pdf-view-mode)
  "List of modes to ignore when searching for dispatch candidates."
  :group 'conn
  :type '(list symbol))

(defvar conn-dispatch-window-predicate
  'conn-dispatch-ignored-mode
  "Predicates which windows must satisfy in order to be considered during
dispatch.

Each function should take a window and return nil if the window should
be ignored by during dispatch.")

(defun conn-dispatch-ignored-mode (win)
  "Return non-nil if the major mode of WIN's buffer is ignored by dispatch.

Ignored modes are those satisfying `provided-mode-derived-p' when called
with `conn-dispatch-thing-ignored-modes'."
  (not (apply #'provided-mode-derived-p
              (buffer-local-value 'major-mode (window-buffer win))
              conn-dispatch-thing-ignored-modes)))


;;;;; Label setup and padding

(defvar conn-default-label-padding-function 'conn--centered-padding
  "Default function for padding dispatch labels.

Target overlays may override this default by setting the
\\='padding-function overlay property.")

(defvar conn-pixelwise-dispatch-labels t)

(put 'conn-label-overlay 'priority 3000)
(put 'conn-label-overlay 'conn-overlay t)

(defun conn--right-justify-padding (overlay width height)
  (overlay-put overlay 'after-string
               (propertize
                " "
                'display `(space :width (,width) :height (,height)))))

(defun conn--left-justify-padding (overlay width height)
  (overlay-put overlay 'before-string
               (propertize
                " "
                'display `(space :width (,width) :height (,height)))))

(defun conn--centered-padding (overlay width height)
  (let* ((left (min 15 (floor width 2)))
         (right (max (- width 15) (ceiling width 2))))
    (overlay-put overlay 'before-string
                 (propertize
                  " "
                  'display `(space :width (,left) :height (,height))
                  'face 'conn-dispatch-label-face))
    (overlay-put overlay 'after-string
                 (propertize
                  " "
                  'display `(space :width (,right) :height (,height))
                  'face 'conn-dispatch-label-face))))

(defun conn--dispatch-setup-label-pixelwise (overlay label-string &optional padding-function)
  (let ((display-width (conn--string-pixel-width label-string
                                                 (window-buffer
                                                  (overlay-get overlay 'window))))
        (padding-width 0)
        (height nil)
        ;; display-line-numbers, line-prefix and wrap-prefix break
        ;; width calculations, temporarily disable them.
        (linum (prog1 display-line-numbers (setq-local display-line-numbers nil)))
        (line-pfx (prog1 line-prefix (setq-local line-prefix nil)))
        (wrap-pfx (prog1 wrap-prefix (setq-local wrap-prefix nil))))
    (unwind-protect
        (progn
          (unless (= (overlay-start overlay) (point-max))
            (let* ((target (overlay-get overlay 'target-overlay))
                   (beg (overlay-start overlay))
                   (line-end (save-excursion
                               (goto-char beg)
                               (line-end-position)))
                   (end nil)
                   (pt beg))
              (while (not end)
                (when (= line-end pt)
                  (if (not (invisible-p (1+ pt)))
                      (setq end pt)
                    (setq end (1+ pt))
                    (let ((str (buffer-substring pt end)))
                      (add-text-properties
                       0 (length str)
                       `(invisible ,(get-char-property pt 'invisible))
                       str)
                      (overlay-put overlay 'after-string str))))
                (pcase-let ((`(,w . ,h) (window-text-pixel-size
                                         (overlay-get overlay 'window)
                                         beg pt)))
                  (when (or (= pt (point-max))
                            (>= w display-width))
                    (setq padding-width (max (- w display-width) 0)
                          height h
                          end pt)))
                (dolist (ov (overlays-in pt (1+ pt)))
                  (when (and (eq 'conn-read-string-match
                                 (overlay-get ov 'category))
                             (or (/= (overlay-start target)
                                     (overlay-start ov))
                                 (/= (overlay-end target)
                                     (overlay-end ov))))
                    (setq end pt)))
                (cl-incf pt))
              (move-overlay overlay (overlay-start overlay) end)))
          (cond
           ((= (overlay-start overlay) (overlay-end overlay))
            (overlay-put overlay 'before-string label-string))
           ((overlay-get overlay 'after-string)
            (overlay-put overlay 'display label-string))
           (t
            (overlay-put overlay 'display label-string)
            (pcase-let* ((`(,_ . ,display-height)
                          (window-text-pixel-size
                           (overlay-get overlay 'window)
                           (overlay-start overlay)
                           (overlay-end overlay))))
              (if padding-function
                  (funcall padding-function overlay padding-width
                           (when (and height (/= height display-height))
                             (1- height)))
                (funcall conn-default-label-padding-function overlay padding-width
                         (when (and height (/= height display-height))
                           (1- height))))))))
      (setq-local display-line-numbers linum
                  line-prefix line-pfx
                  wrap-prefix wrap-pfx))))

(defun conn--dispatch-setup-label-charwise (overlay label-string &optional _padding-function)
  (unless (= (overlay-start overlay) (point-max))
    (move-overlay overlay
                  (overlay-start overlay)
                  (min (+ (overlay-start overlay)
                          (length label-string))
                       (save-excursion
                         (goto-char (overlay-start overlay))
                         (line-end-position))))
    (let* ((target (overlay-get overlay 'target-overlay))
           (beg (overlay-start overlay))
           (end nil)
           (line-end (save-excursion
                       (goto-char beg)
                       (line-end-position)))
           (pt beg))
      (while (not end)
        (when (= line-end pt)
          (if (not (invisible-p (1+ pt)))
              (setq end pt)
            (setq end (1+ pt))
            (let ((str (buffer-substring pt end)))
              (add-text-properties
               0 (length str)
               `(invisible ,(get-char-property pt 'invisible))
               str)
              (overlay-put overlay 'after-string str))))
        (when (or (= pt (point-max))
                  (= (- pt beg) (length label-string)))
          (setq end pt))
        (dolist (ov (overlays-in pt (1+ pt)))
          (when (and (eq 'conn-read-string-match
                         (overlay-get ov 'category))
                     (or (/= (overlay-start target)
                             (overlay-start ov))
                         (/= (overlay-end target)
                             (overlay-end ov))))
            (setq end pt)))
        (cl-incf pt))
      (move-overlay overlay (overlay-start overlay) end)))
  (if (= (overlay-start overlay) (overlay-end overlay))
      (overlay-put overlay 'before-string label-string)
    (overlay-put overlay 'display label-string)))

(defun conn--dispatch-setup-label (overlay label-string &optional padding-function)
  (if (if (functionp conn-pixelwise-dispatch-labels)
          (funcall conn-pixelwise-dispatch-labels)
        conn-pixelwise-dispatch-labels)
      (conn--dispatch-setup-label-pixelwise overlay label-string padding-function)
    (conn--dispatch-setup-label-charwise overlay label-string padding-function)))

(defun conn--dispatch-labels (label-strings target-overlays)
  (conn--protected-let ((labels nil (mapc #'conn-label-delete labels)))
    (pcase-dolist (`(,window . ,previews) target-overlays)
      (with-current-buffer (window-buffer window)
        (dolist (p previews)
          (conn--protected-let
              ((string (pop label-strings))
               (beg (overlay-end p))
               (ov (make-overlay beg beg) (delete-overlay ov)))
            (overlay-put ov 'category 'conn-label-overlay)
            (overlay-put ov 'window window)
            (overlay-put ov 'target-overlay p)
            (conn--dispatch-setup-label
             ov string (overlay-get p 'padding-function))
            (push (make-conn-dispatch-label :string string
                                            :overlay ov
                                            :prop (if (overlay-get ov 'display)
                                                      'display
                                                    'before-string)
                                            :target-overlay p)
                  labels)))))
    labels))


;;;;; Actions

(defmacro conn-define-dispatch-action (name arglist &rest rest)
  "\(fn NAME ARGLIST &key INTERACTIVE DESCRIPTION FILTER WINDOW-PREDICATE STATE &body BODY)"
  (declare (debug ( name lambda-expr
                    [&rest keywordp form]
                    def-body))
           (indent 2))
  (cl-with-gensyms (struct menu-item desc)
    (pcase-let* (((map :description :interactive :filter :window-predicate)
                  rest)
                 (body (cl-loop for sublist on rest by #'cddr
                                unless (keywordp (car sublist))
                                do (cl-return sublist))))
      `(progn
         (defun ,name ,arglist ,@body)
         (let* ((,desc ,(or description (symbol-name name)))
                (,struct (make-conn--action
                          :interactive ,interactive
                          :description ,desc
                          :window-predicate ,window-predicate
                          :filter ,filter))
                (,menu-item
                 (list 'menu-item ,desc ',name
                       :filter (lambda (_)
                                 (if-let* ((fn (conn--action-filter ,struct)))
                                     (pcase (funcall fn)
                                       ('this ',name)
                                       (res res))
                                   ',name)))))
           (put ',name :conn--action ,struct)
           (defvar ,name)
           (setq ,name ,menu-item))))))

(defun conn-action-filter (action)
  (conn--action-filter (get :conn--action action)))

(gv-define-setter conn-action-filter (val action)
  `(setf (conn--action-filter (get :conn--action ,action)) ,val))

(defun conn-action-interactive (action)
  (conn--action-interactive (get :conn--action action)))

(gv-define-setter conn-action-interactive (val action)
  `(setf (conn--action-interactive (get :conn--action ,action)) ,val))

(defun conn-action-window-predicate (action)
  (conn--action-window-predicate (get :conn--action action)))

(gv-define-setter conn-action-window-predicate (val action)
  `(setf (conn--action-window-predicate (get :conn--action ,action)) ,val))

(defun conn-action-description (action)
  (conn--action-description (get :conn--action action)))

(gv-define-setter conn-action-description (val action)
  `(setf (conn--action-description (get :conn--action ,action)) ,val))

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

(conn-define-dispatch-action conn-dispatch-yank-replace-to
    (window pt thing-cmd thing-arg str)
  :description "Yank Replace To"
  :interactive (lambda () (list (funcall region-extract-function nil)))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (delete-region beg end)
         (insert-for-yank str)
         (unless executing-kbd-macro
           (pulse-momentary-highlight-region (- (point) (length str)) (point))))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-yank-read-replace-to
    (window pt thing-cmd thing-arg str)
  :description "Yank Replace To"
  :interactive (lambda ()
                 (list (read-from-kill-ring "Yank Replace To from kill-ring: ")))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (delete-region beg end)
         (insert-for-yank str)
         (unless executing-kbd-macro
           (pulse-momentary-highlight-region (- (point) (length str)) (point))))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-yank-to
    (window pt _thing-cmd _thing-arg str)
  :description "Yank To"
  :interactive (lambda () (list (funcall region-extract-function nil)))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-region (- (point) (length str)) (point))))))

(conn-define-dispatch-action conn-dispatch-yank-read-to
    (window pt _thing-cmd _thing-arg str)
  :description "Yank To"
  :interactive (lambda () (list (read-from-kill-ring "Yank To from kill-ring: ")))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-region (- (point) (length str)) (point))))))

(conn-define-dispatch-action conn-dispatch-throw
    (window pt _thing-cmd _thing-arg str)
  :description "Throw"
  :interactive (lambda () (list (funcall region-extract-function t)))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-region (- (point) (length str)) (point))))))

(conn-define-dispatch-action conn-dispatch-dot (window pt thing-cmd thing-arg)
  :description "Dot"
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (when beg (conn--create-dot beg end)))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-remove-dot
    (window pt _thing-cmd _thing-arg)
  :description "Remove Dot"
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (conn-delete-dots))))

(conn-define-dispatch-action conn-dispatch-downcase (window pt thing-cmd thing-arg)
  :description "Downcase"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (downcase-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-upcase (window pt thing-cmd thing-arg)
  :description "Upcase"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (upcase-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-capitalize (window pt thing-cmd thing-arg)
  :description "Capitalize"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (capitalize-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-narrow-indirect (window pt thing-cmd thing-arg)
  :description "Narrow Indirect"
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (conn--narrow-indirect beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-comment (window pt thing-cmd thing-arg)
  :description "Comment"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (comment-or-uncomment-region beg end)
         (message "Commented %s" thing-cmd))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-duplicate (window pt thing-cmd thing-arg arg)
  :description "Duplicate"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  :interactive (lambda () (list (prefix-numeric-value current-prefix-arg)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (conn-duplicate-region beg end arg)
         (message "Duplicated %s" thing-cmd))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-duplicate-and-comment (window pt thing-cmd thing-arg arg)
  :description "Duplicate and Comment"
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  :interactive (lambda () (list (prefix-numeric-value current-prefix-arg)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (conn-duplicate-and-comment-region beg end arg)
         (message "Duplicated and commented %s" thing-cmd))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-register (window pt _thing-cmd _thing-arg register)
  :description "Register <%c>"
  :interactive (lambda () (list (register-read-with-preview "Register: ")))
  (with-selected-window window
    ;; If there is a keyboard macro in the register we would like to
    ;; amalgamate the undo
    (with-undo-amalgamate
      (save-excursion
        (goto-char pt)
        (conn-register-load register)))))

(conn-define-dispatch-action conn-dispatch-register-replace (window pt thing-cmd thing-arg register)
  :description "Register Replace <%c>"
  :interactive (lambda () (list (register-read-with-preview "Register: ")))
  (with-selected-window window
    ;; If there is a keyboard macro in the register we would like to
    ;; amalgamate the undo
    (with-undo-amalgamate
      (save-excursion
        (goto-char pt)
        (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
          (`(,beg . ,end)
           (delete-region beg end)
           (conn-register-load register))
          (_ (user-error "Cannot find %s at point" thing-cmd)))))))

(conn-define-dispatch-action conn-dispatch-kill (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Kill to Register <%c>" register)
                   "Kill"))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (if register
               (copy-to-register register beg end t)
             (kill-region beg end))
           (conn--dispatch-fixup-whitespace)
           (message "Killed: %s" str)))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-kill-append (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Kill Append Register <%c>" register)
                   "Kill Append"))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (if register
               (copy-to-register register beg end t)
             (kill-append str nil)
             (delete-region beg end))
           (conn--dispatch-fixup-whitespace)
           (message "Appended: %s" str)))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-kill-prepend (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Kill Prepend Register <%c>" register)
                   "Kill Prepend"))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (if register
               (prepend-to-register register beg end t)
             (kill-append str t)
             (delete-region beg end))
           (conn--dispatch-fixup-whitespace)
           (message "Prepended: %s" str)))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-copy-as-kill
    (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Copy to Register <%c>" register)
                   "Copy As Kill"))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (pulse-momentary-highlight-region beg end)
         (if register
             (copy-to-register register beg end)
           (kill-new (filter-buffer-substring beg end))))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-copy-append (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Copy Append to Register <%c>" register)
                   "Copy Append"))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (if register
               (append-to-register register beg end)
             (kill-append str nil))
           (message "Copy Appended: %s" str)))
        (_ (user-error "Cannot find %s at point"
                       (get thing-cmd :conn-command-thing)))))))

(conn-define-dispatch-action conn-dispatch-copy-prepend (window pt thing-cmd thing-arg register)
  :description (lambda (register)
                 (if register
                     (format "Copy Prepend to Register <%c>" register)
                   "Copy Prepend"))
  :interactive (lambda ()
                 (when current-prefix-arg
                   (list (register-read-with-preview "Register: "))))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (if register
               (prepend-to-register register beg end)
             (kill-append str t))
           (message "Copy Prepended: %s" str)))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-copy-replace (window pt thing-cmd thing-arg)
  :description "Copy Replace"
  :filter (lambda () (unless buffer-read-only 'this))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (pulse-momentary-highlight-region beg end)
         (copy-region-as-kill beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "Cannot find %s at point"
                       (get thing-cmd :conn-command-thing))))))
  (delete-region (region-beginning) (region-end))
  (yank))

(conn-define-dispatch-action conn-dispatch-cut-replace (window pt thing-cmd thing-arg)
  :description "Cut Replace"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (kill-region beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "Cannot find %s at point" thing-cmd)))))
  (delete-region (region-beginning) (region-end))
  (yank))

(conn-define-dispatch-action conn-dispatch-copy (window pt thing-cmd thing-arg)
  :description "Copy"
  :filter (lambda () (unless buffer-read-only 'this))
  (let (str)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
          (`(,beg . ,end)
           (pulse-momentary-highlight-region beg end)
           (setq str (filter-buffer-substring beg end))))))
    (if str
        (insert-for-yank str)
      (user-error "Cannot find %s at point"
                  (get thing-cmd :conn-command-thing)))))

(conn-define-dispatch-action conn-dispatch-cut (window pt thing-cmd thing-arg)
  :description "Cut"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (kill-region beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "Cannot find %s at point"
                       (get thing-cmd :conn-command-thing))))))
  (yank))

(conn-define-dispatch-action conn-dispatch-goto (window pt thing-cmd thing-arg)
  :description "Goto"
  (select-window window)
  (unless (= pt (point))
    (unless (region-active-p)
      (push-mark nil t))
    (goto-char pt)
    (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
      (`(,beg . ,end)
       (unless (region-active-p)
         (if (= (point) end)
             (conn--push-ephemeral-mark beg)
           (conn--push-ephemeral-mark end)))
       (unless (or (= pt beg) (= pt end))
         (goto-char beg)))
      (_ (user-error "Cannot find %s at point"
                     (get thing-cmd :conn-command-thing))))))

(conn-define-dispatch-action conn-dispatch-over (window pt thing-cmd thing-arg)
  :description "Over"
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point)))
    (unless (region-active-p)
      (push-mark nil t))
    (pcase (alist-get thing-cmd conn-dispatch-default-action-alist)
      ((or 'conn-dispatch-goto 'nil)
       (pcase (cons (or (bounds-of-thing-at-point
                         (get thing-cmd :conn-command-thing))
                        (point))
                    (progn
                      (goto-char pt)
                      (bounds-of-thing-at-point
                       (get thing-cmd :conn-command-thing))))
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
                 (goto-char beg))))))
      ('conn-dispatch-jump (conn-dispatch-jump window pt thing-cmd thing-arg))
      (_ (error "Can't jump to %s" thing-cmd)))))

(conn-define-dispatch-action conn-dispatch-jump (window pt _thing-cmd _thing-arg)
  :description "Jump"
  (with-current-buffer (window-buffer window)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (select-window window)
      (goto-char pt))))

(conn-define-dispatch-action conn-dispatch-transpose (window pt thing-cmd _thing-arg)
  :description "Transpose"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda (win)
                      (buffer-local-value 'buffer-read-only (window-buffer win)))
  (if (eq (current-buffer) (window-buffer window))
      (pcase (if (region-active-p)
                 (cons (region-beginning) (region-end))
               (bounds-of-thing-at-point
                (get thing-cmd :conn-command-thing)))
        (`(,beg1 . ,end1)
         (pcase (save-excursion
                  (goto-char pt)
                  (bounds-of-thing-at-point
                   (get thing-cmd :conn-command-thing)))
           (`(,beg2 . ,end2)
            (if (and (or (<= beg1 end1 beg2 end2)
                         (<= beg2 end2 beg1 end1))
                     (/= beg1 end1)
                     (/= beg2 end2)
                     (<= (point-min) (min beg2 end2 beg1 end1))
                     (> (point-max) (max beg2 end2 beg1 end1)))
                (progn
                  (goto-char pt)
                  (transpose-regions beg1 end1 beg2 end2))
              (user-error "Invalid regions")))
           (_ (user-error "Cannot find %s at point" thing-cmd))))
        (_ (user-error "Cannot find %s at point" thing-cmd)))
    (let ((cg1 (prepare-change-group))
          (cg2 (with-current-buffer (window-buffer window)
                 (prepare-change-group)))
          str1 str2 success)
      (unwind-protect
          (progn
            (pcase (if (region-active-p)
                       (cons (region-beginning) (region-end))
                     (bounds-of-thing-at-point
                      (get thing-cmd :conn-command-thing)))
              (`(,beg . ,end)
               (setq str1 (filter-buffer-substring beg end)))
              (_ (user-error "Cannot find %s at point" thing-cmd)))
            (with-selected-window window
              (save-excursion
                (goto-char pt)
                (pcase (bounds-of-thing-at-point
                        (get thing-cmd :conn-command-thing))
                  (`(,beg . ,end)
                   (setq str2 (filter-buffer-substring beg end))
                   (delete-region beg end)
                   (insert str1))
                  (_ (user-error "Cannot find %s at point" thing-cmd)))))
            (delete-region (region-beginning) (region-end))
            (insert str2)
            (setq success t))
        (if success
            (progn
              (accept-change-group cg1)
              (accept-change-group cg2))
          (cancel-change-group cg1)
          (cancel-change-group cg2))))))

(define-keymap
  :keymap (conn-get-mode-map 'conn-read-dispatch-state 'conn-dot-mode)
  "d" 'conn-dispatch-dot
  "w" 'conn-dispatch-remove-dot)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-dispatch-state)
  "C-y" 'conn-dispatch-yank-replace-to
  "M-y" 'conn-dispatch-yank-read-replace-to
  "y" 'conn-dispatch-yank-to
  "Y" 'conn-dispatch-yank-read-to
  "q" 'conn-dispatch-throw
  "<remap> <downcase-word>" 'conn-dispatch-downcase
  "<remap> <downcase-region>" 'conn-dispatch-downcase
  "<remap> <downcase-dwim>" 'conn-dispatch-downcase
  "<remap> <upcase-word>" 'conn-dispatch-upcase
  "<remap> <upcase-region>" 'conn-dispatch-upcase
  "<remap> <upcase-dwim>" 'conn-dispatch-upcase
  "<remap> <capitalize-word>" 'conn-dispatch-capitalize
  "<remap> <capitalize-region>" 'conn-dispatch-capitalize
  "<remap> <capitalize-dwim>" 'conn-dispatch-capitalize
  "r N" 'conn-dispatch-narrow-indirect
  "r n" 'conn-dispatch-narrow-indirect
  "X" 'conn-dispatch-narrow-indirect
  ";" 'conn-dispatch-comment
  "M-;" 'conn-dispatch-comment
  "r ;" 'conn-dispatch-comment
  "r e" 'conn-dispatch-duplicate
  "r d" 'conn-dispatch-duplicate-and-comment
  "p" 'conn-dispatch-register
  "P" 'conn-dispatch-register-replace
  "w" 'conn-dispatch-kill
  "]" 'conn-dispatch-kill-append
  "[" 'conn-dispatch-kill-prepend
  "a" 'conn-dispatch-copy-as-kill
  "{" 'conn-dispatch-copy-prepend
  "}" 'conn-dispatch-copy-append
  "f" 'conn-dispatch-copy-replace
  "d" 'conn-dispatch-cut-replace
  "c" 'conn-dispatch-copy
  "x" 'conn-dispatch-cut
  "g" 'conn-dispatch-goto
  "e" 'conn-dispatch-over
  "z" 'conn-dispatch-jump
  "t" 'conn-dispatch-transpose)


;;;;; Target Finders

(defun conn--dispatch-read-n-chars (N &optional dir all-windows)
  "Read a string of N chars with preview overlays.

Returns a cons of (STRING . OVERLAYS)."
  (cl-assert (> N 0))
  (conn--with-input-method
    (conn--protected-let
        ((prompt (propertize "chars: " 'face 'minibuffer-prompt))
         (string (char-to-string (conn-dispatch-read-event prompt t)))
         (overlays nil (unless (eq overlays t)
                         (mapc #'delete-overlay overlays))))
      (setq overlays
            (while-no-input
              (conn--string-preview-overlays string dir all-windows)))
      (dotimes (_ (1- N))
        (thread-last
          (conn-dispatch-read-event (format (concat prompt "%s") string) t)
          (char-to-string)
          (concat string)
          (setq string))
        (unless (eq overlays t)
          (mapc #'delete-overlay overlays))
        (setq overlays
              (while-no-input
                (conn--string-preview-overlays string dir all-windows))))
      (message nil)
      (cons string overlays))))

(defun conn-dispatch-read-event (&optional prompt inherit-input-method seconds)
  (catch 'char
    (while t
      (pcase (read-event prompt inherit-input-method seconds)
        ((and event
              `(mouse-1 . ,_)
              (let posn (event-start event))
              (let win (posn-window posn))
              (let pt (posn-point posn)))
         (when (and (funcall conn-dispatch-window-predicate win)
                    (not (posn-area posn)))
           (throw 'mouse-click (list pt win nil))))
        ((and ev (pred characterp))
         (throw 'char ev))
        ('nil (throw 'char nil))))))

(defun conn--dispatch-read-string-with-timeout (&optional dir all-windows predicate)
  "Read a string with preview overlays and timeout `conn-read-string-timeout'.

Returns a cons of (STRING . OVERLAYS)."
  (conn--with-input-method
    (conn--protected-let
        ((prompt (propertize "string: " 'face 'minibuffer-prompt))
         (string (char-to-string (conn-dispatch-read-event prompt t)))
         (overlays nil (unless (eq overlays t)
                         (mapc #'delete-overlay overlays))))
      (setq overlays
            (while-no-input
              (conn--string-preview-overlays string dir all-windows predicate)))
      (while-let ((next-char (conn-dispatch-read-event
                              (format (concat prompt "%s") string) t
                              conn-read-string-timeout)))
        (setq string (concat string (char-to-string next-char)))
        (unless (eq overlays t)
          (mapc #'delete-overlay overlays))
        (setq overlays
              (while-no-input
                (conn--string-preview-overlays string dir all-windows predicate))))
      (message nil)
      (cons string overlays))))

(defun conn--dispatch-chars ()
  (cl-loop for (_ . ovs) = (conn--dispatch-read-string-with-timeout nil t)
           when ovs return ovs
           do (message "(no matches)")))

(defun conn--dispatch-chars-in-thing (thing &optional all-windows)
  (cl-loop for (_ . ovs) = (conn--dispatch-read-string-with-timeout
                            nil all-windows
                            (lambda (pt)
                              (save-excursion
                                (goto-char pt)
                                (ignore-errors
                                  (bounds-of-thing-at-point thing)))))
           when ovs return ovs
           do (message "(no matches)")))

(defun conn--dispatch-2-chars ()
  (cl-loop for (_ . ovs) = (conn--dispatch-read-n-chars 2 nil t)
           when ovs return ovs
           do (message "(no matches)")))

(defun conn--dispatch-all-things-1 (thing)
  (conn--protected-let ((start (point))
                        (ovs nil (mapc #'delete-overlay ovs)))
    (save-excursion
      (goto-char (window-end))
      (while (and
              (/= (point)
                  (progn
                    (forward-thing thing -1)
                    (point)))
              (<= (window-start) (point)))
        (unless (or (= (point) start)
                    (and (= (point) (point-min))
                         (not (bounds-of-thing-at-point thing))))
          (push (conn--make-target-overlay (point) 0 thing) ovs))))
    ovs))

(defun conn--dispatch-all-things (thing &optional all-windows)
  (cl-loop for win in (conn--get-dispatch-windows all-windows)
           nconc (with-selected-window win
                   (conn--dispatch-all-things-1 thing))))

(defun conn--dispatch-all-buttons (&optional all-windows)
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (cl-loop for win in (conn--get-dispatch-windows all-windows)
             do (with-selected-window win
                  (with-restriction (window-start) (window-end)
                    (save-excursion
                      (goto-char (point-min))
                      (when (button-at (point))
                        (push (conn--make-target-overlay (point) 0 nil) ovs))
                      (while (forward-button 1 nil nil t)
                        (push (conn--make-target-overlay (point) 0 nil) ovs)))))
             finally return ovs)))

(defun conn--dispatch-re-matches (regexp &optional all-windows)
  (cl-loop for win in (conn--get-dispatch-windows all-windows)
           nconc (with-selected-window win
                   (with-restriction (window-start) (window-end)
                     (save-excursion
                       (goto-char (point-min))
                       (save-match-data
                         (cl-loop while (re-search-forward regexp nil t)
                                  collect (conn--make-target-overlay
                                           (match-beginning 0)
                                           (- (match-end 0)
                                              (match-beginning 0))))))))))

(defun conn--dispatch-things-with-prefix (thing prefix-length &optional all-windows)
  (conn--protected-let ((prefix "")
                        (predicate
                         (lambda (pt)
                           (save-match-data
                             (save-excursion
                               (goto-char pt)
                               (pcase (ignore-errors (bounds-of-thing-at-point thing))
                                 (`(,beg . ,_end) (= beg pt)))))))
                        (prompt "char: ")
                        (ovs nil (unless (eq ovs t)
                                   (mapc #'delete-overlay ovs))))
    (while (or (not ovs) (eq ovs t))
      (conn--with-input-method
        (while (length< prefix prefix-length)
          (setq prefix (thread-last
                         (conn-dispatch-read-event prompt t)
                         (char-to-string)
                         (concat prefix))
                prompt (concat "char: " prefix))
          (unless (eq ovs t)
            (mapc #'delete-overlay ovs))
          (if (length= prefix prefix-length)
              (setq ovs (conn--string-preview-overlays prefix nil all-windows predicate))
            (setq ovs (while-no-input
                        (conn--string-preview-overlays prefix nil all-windows predicate))))))
      (setq prefix ""
            prompt "char: (no matches)"))
    ovs))

(defun conn--dispatch-columns ()
  (conn--protected-let ((goal-column (or goal-column
                                         (current-column)))
                        (ovs nil (mapc #'delete-overlay ovs)))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (save-excursion
          (ignore-errors
            (while (< (point) (point-max))
              (line-move-visual 1)
              (push (conn--make-target-overlay (point) 0) ovs))))
        (save-excursion
          (ignore-errors
            (while (< (point-min) (point))
              (line-move-visual -1)
              (push (conn--make-target-overlay (point) 0) ovs))))))
    ovs))

(defun conn--dispatch-lines ()
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (dolist (win (conn--get-dispatch-windows t) ovs)
      (with-selected-window win
        (save-excursion
          (with-restriction (window-start) (window-end)
            (goto-char (point-min))
            (when (and (bolp)
                       (<= (+ (point) (window-hscroll)) (line-end-position))
                       (goto-char (+ (point) (window-hscroll)))
                       (not (invisible-p (point))))
              (push (conn--make-target-overlay
                     (point) 0 nil 'conn--right-justify-padding)
                    ovs))
            (while (/= (point) (point-max))
              (forward-line)
              (when (and (bolp)
                         (<= (+ (point) (window-hscroll))
                             (line-end-position) (point-max))
                         (goto-char (+ (point) (window-hscroll)))
                         (not (invisible-p (point)))
                         (not (invisible-p (1- (point)))))
                (if (= (point) (point-max))
                    (let ((ov (conn--make-target-overlay (point) 0)))
                      ;; hack to get the label displayed on its own line
                      (overlay-put ov 'after-string
                                   (propertize " " 'display '(space :width 0)))
                      (push ov ovs))
                  (push (conn--make-target-overlay
                         (point) 0 nil 'conn--right-justify-padding)
                        ovs))))))))))

(defun conn--dispatch-lines-end ()
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (dolist (win (conn--get-dispatch-windows t) ovs)
      (with-selected-window win
        (save-excursion
          (with-restriction (window-start) (window-end)
            (goto-char (point-min))
            (move-end-of-line nil)
            (when (and (eolp) (not (invisible-p (point))))
              (push (conn--make-target-overlay (point) 0) ovs))
            (while (/= (point) (point-max))
              (forward-line)
              (move-end-of-line nil)
              (when (and (eolp)
                         (not (invisible-p (point)))
                         (not (invisible-p (1- (point)))))
                (if (= (point-max) (point))
                    (let ((ov (conn--make-target-overlay (point) 0)))
                      ;; hack to get the label displayed on its own line
                      (overlay-put ov 'after-string
                                   (propertize " " 'display '(space :width 0)))
                      (push ov ovs))
                  (push (conn--make-target-overlay (point) 0) ovs))))))))))

(defun conn--dispatch-inner-lines (&optional end)
  (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
    (dolist (win (conn--get-dispatch-windows t) ovs)
      (with-selected-window win
        (save-excursion
          (with-restriction (window-start) (window-end)
            (goto-char (point-max))
            (while (let ((pt (point)))
                     (forward-line -1)
                     (if end
                         (conn-end-of-inner-line)
                       (conn-beginning-of-inner-line))
                     (/= (point) pt))
              (when (not (invisible-p (point)))
                (push (conn--make-target-overlay (point) 0) ovs)))))))))

(defun conn--dispatch-inner-lines-end ()
  (conn--dispatch-inner-lines t))


;;;;; Dispatch Registers

(cl-defstruct (conn-dispatch-register
               (:constructor conn--make-dispatch-register (dispatch-command)))
  (dispatch-command nil))

(cl-defmethod register-val-jump-to ((val conn-dispatch-register) arg)
  (let ((conn--last-dispatch-command
         (conn-dispatch-register-dispatch-command val)))
    (conn-repeat-last-dispatch arg)))

(cl-defmethod register-val-describe ((_val conn-dispatch-register) _arg)
  (princ "Dispatch Register"))

(static-if (<= 30 emacs-major-version)
    (cl-defmethod register-command-info ((_command (eql conn-last-dispatch-to-register)))
      (make-register-preview-info
       :types '(all)
       :msg "Copy dispatch to register `%s'"
       :act 'set
       :noconfirm (memq register-use-preview '(nil never))
       :smatch t)))

(defun conn-last-dispatch-to-register (register)
  "Store last dispatch command in REGISTER."
  (interactive (list (register-read-with-preview "Dispatch to register: ")))
  (set-register register (conn--make-dispatch-register conn--last-dispatch-command)))


;;;;; Dispatch Commands

(defun dispatch--find-target (finder)
  (catch 'mouse-click
    (let (target-ovs labels)
      (unwind-protect
          (progn
            (setf target-ovs (compat-call
                              sort
                              (seq-group-by (lambda (ov) (overlay-get ov 'window))
                                            (funcall finder))
                              :in-place t
                              :lessp (lambda (a _) (eq (selected-window) (car a)))))
            (setf (alist-get (selected-window) target-ovs)
                  (compat-call sort
                               (alist-get (selected-window) target-ovs)
                               :in-place t
                               :lessp (lambda (a b)
                                        (< (abs (- (overlay-start a) (point)))
                                           (abs (- (overlay-start b) (point)))))))
            (setf labels (conn--dispatch-labels
                          (or (funcall
                               conn-label-string-generator
                               (let ((sum 0))
                                 (dolist (p target-ovs sum)
                                   (setq sum (+ sum (length (cdr p)))))))
                              (user-error "No matching candidates"))
                          target-ovs))
            (let ((target (conn-label-select labels)))
              (list (overlay-start target)
                    (overlay-get target 'window)
                    (overlay-get target 'thing))))
        (pcase-dolist (`(_ . ,ovs) target-ovs)
          (mapc #'delete-overlay ovs))
        (mapc #'conn-label-delete labels)))))

(defun conn-dispatch-on-things ( thing-cmd thing-arg finder action action-args
                                 &optional predicate repeat)
  "Begin dispatching ACTION on a THING.

The user is first prompted for a either a THING or an ACTION
to be performed followed by a THING to perform it on.  If
no ACTION is selected the default ACTION is to go to the THING.

Once a THING has been selected the user is prompted for a string and
the THING at the location selected is acted upon.

The string is read with an idle timeout of `conn-read-string-timeout'
seconds."
  (interactive (conn--dispatch-read-thing))
  (setq conn--last-dispatch-command
        (list thing-cmd thing-arg finder action
              action-args predicate repeat))
  (let ((conn-dispatch-window-predicate conn-dispatch-window-predicate))
    (when predicate
      (add-function :after-while conn-dispatch-window-predicate predicate))
    (ignore-error quit
      (while
          (prog1 repeat
            (pcase-let ((`(,pt ,window ,thing) (dispatch--find-target finder)))
              (setf conn-this-command-thing
                    (or thing (ignore-errors
                                (get thing-cmd :conn-command-thing))))
              (apply action window pt thing-cmd thing-arg action-args)))
        (undo-boundary)))))

(defun conn--bounds-of-dispatch (_cmd _arg)
  (pcase-let* ((conn-state-for-read-dispatch 'conn-dispatch-bounds-state)
               (`(,thing-cmd ,thing-arg ,finder ,action ,action-args ,_ ,repeat)
                (conn--dispatch-read-thing))
               (regions nil)
               (win (selected-window))
               (conn-dispatch-window-predicate conn-dispatch-window-predicate))
    (add-function :before-while conn-dispatch-window-predicate
                  (lambda (window) (eq win window)))
    (save-mark-and-excursion
      (ignore-error quit
        (while
            (prog1 repeat
              (pcase-let ((`(,pt ,window ,thing) (dispatch--find-target finder)))
                (setf conn-this-command-thing
                      (or thing (ignore-errors
                                  (get thing-cmd :conn-command-thing))))
                (apply action window pt thing-cmd thing-arg action-args)
                (push (cons (region-beginning) (region-end)) regions))))))
    (setq regions (compat-call sort (conn--merge-regions regions t)
                               :key #'car :in-place t))
    (cons (cons (caar regions) (cdar (last regions))) regions)))

(defun conn-repeat-last-dispatch (repeat)
  "Repeat the last dispatch command.

Prefix arg REPEAT inverts the value of repeat in the last dispatch."
  (interactive "P")
  (pcase conn--last-dispatch-command
    (`(,thing-cmd ,thing-arg ,finder ,action ,action-args ,predicate ,rep)
     ;; If we invert repeat we don't want that reflected in
     ;; conn--last-dispatch-command so we let bind it around this
     ;; call.
     (let (conn--last-dispatch-command)
       (conn-dispatch-on-things thing-cmd thing-arg
                                finder
                                action action-args
                                predicate
                                (xor rep repeat))))
    (_ (user-error "No last dispatch command"))))

(defun conn-bind-last-dispatch-to-key (&optional repeat)
  "Bind last dispatch command to a key.

Prefix arg REPEAT inverts the value of repeat in the last dispatch."
  (interactive)
  (pcase conn--last-dispatch-command
    (`(,thing-cmd ,thing-arg ,finder ,action ,action-args ,predicate ,rep)
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
                     key-seq (lambda ()
                               (interactive)
                               (conn-dispatch-on-things
                                thing-cmd thing-arg
                                finder
                                action action-args
                                predicate (xor repeat rep))))
         (message "Dispatch bound to %s" (format-kbd-macro key-seq)))))
    (_ (error "No last dispatch"))))

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-dispatch-on-things
   nil nil
   (lambda () (conn--dispatch-all-buttons t))
   (lambda (win pt _thing _thing-arg)
     (select-window win)
     (push-button pt))
   nil))

(defun conn--dispatch-isearch-matches ()
  (with-restriction (window-start) (window-end)
    (cl-loop for (beg . end) in (conn--isearch-matches)
             for count from 1
             collect (conn--make-target-overlay beg (- end beg)) into matches
             finally return (cons count matches))))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (pcase-let ((`(,count . ,matches) (conn--dispatch-isearch-matches))
              (labels nil))
    (unwind-protect
        (progn
          (setf labels (conn--dispatch-labels
                        (funcall conn-label-string-generator count)
                        `((,(selected-window) . ,matches))))
          (let* ((target (conn-label-select labels)))
            (isearch-done)
            (goto-char (overlay-start target))))
      (mapc #'conn-label-delete labels)
      (mapc #'delete-overlay matches))))

(defun conn-goto-char-2 ()
  "Jump to point defined by two characters and maybe a label."
  (interactive)
  (conn-dispatch-on-things nil nil
                           'conn--dispatch-2-chars
                           'conn-dispatch-jump nil))


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

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
  "." 'conn-contract
  "," 'conn-expand)

(defun conn--expand-post-change-hook (&rest _)
  (setq conn--current-expansions nil)
  (remove-hook 'after-change-functions 'conn--expand-post-change-hook t))

(defun conn--expand-filter-regions (regions)
  (let (result)
    (pcase-dolist ((and reg `(,beg . ,end)) regions)
      (when (and beg end
                 (/= beg end)
                 (<= beg (region-beginning))
                 (>= end (region-end)))
        (cl-pushnew reg result :test #'equal)))
    result))

(defun conn--expand-create-expansions ()
  (add-hook 'after-change-functions 'conn--expand-post-change-hook nil t)
  (thread-last
    (mapcan #'funcall conn-expansion-functions)
    (cons (cons (region-beginning) (region-end)))
    (conn--expand-filter-regions)
    (seq-sort (pcase-lambda (`(,b1 . ,e1) `(,b2 . ,e2))
                (or (> b1 b2) (< e1 e2))))))

(defun conn--valid-expansions-p ()
  (or (and conn--current-expansions
           (region-active-p)
           (seq-find (pcase-lambda (`(,beg . _))
                       (= beg (region-beginning)))
                     conn--current-expansions)
           (seq-find (pcase-lambda (`(_ . ,end))
                       (= end (region-end)))
                     conn--current-expansions))
      (member (cons (region-beginning) (region-end))
              conn--current-expansions)))

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
  (when (consp arg)
    (conn--push-ephemeral-mark)
    (setq arg (log (prefix-numeric-value arg) 4)))
  (setq arg (prefix-numeric-value arg))
  (unless (conn--valid-expansions-p)
    (setq conn--current-expansions (conn--expand-create-expansions)))
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
            (t
             (pcase (catch 'found
                      (pcase-dolist ((and cons `(,beg . ,end))
                                     conn--current-expansions)
                        (when (or (< beg (region-beginning))
                                  (> end (region-end)))
                          (throw 'found cons))))
               (`(,beg . ,end)
                (goto-char (if (= (point) (region-beginning)) beg end))
                (conn--push-ephemeral-mark
                 (if (= (point) (region-beginning)) end beg)))
               ('nil
                (user-error "No more expansions")))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))

(defun conn-contract (arg)
  "Contract region by semantic units.

If the region is active only the `point' is moved.
Expansions and contractions are provided by functions in
`conn-expansion-functions'."
  (interactive "p")
  (unless (conn--valid-expansions-p)
    (setq conn--current-expansions (conn--expand-create-expansions)))
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
            (t
             (pcase (catch 'found
                      (pcase-dolist ((and cons `(,beg . ,end))
                                     (reverse conn--current-expansions))
                        (when (or (> beg (region-beginning))
                                  (< end (region-end)))
                          (throw 'found cons))))
               (`(,beg . ,end)
                (goto-char (if (= (point) (region-beginning)) beg end))
                (conn--push-ephemeral-mark (if (= (point) (region-end)) beg end)))
               ('nil
                (user-error "No more contractions")))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))


;;;;; Bounds of expansion

(defvar-keymap conn-read-expand-region-map
  :parent conn-expand-repeat-map
  "v" 'conn-toggle-mark-command
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit
  "<t>" 'ignore)

(defun conn--bounds-of-expansion (cmd arg)
  (save-mark-and-excursion
    (let ((current-prefix-arg arg))
      (call-interactively cmd)
      (let ((exit (set-transient-map
                   conn-read-expand-region-map (lambda () t) nil
                   (substitute-command-keys
                    (concat "\\<conn-read-expand-region-map>"
                            "Defining region. Press "
                            "\\[exit-recursive-edit] to finish, "
                            "\\[abort-recursive-edit] to abort.")))))
        (unwind-protect
            (recursive-edit)
          (funcall exit)))
      (list (cons (region-beginning) (region-end))))))


;;;;; Thing Definitions

;; Definitions for various things and thing commands.

(defun conn-register-thing (thing &rest rest)
  "Register a new THING.

THINGs may have several optional properties that control how they
function in various conn features

FORWARD-OP, BEG-OP, END-OP and BOUNDS-OP provide operations for
`thingatpt', which see.

TARGET-FINDER is a function that produces targets for
`conn-dispatch-on-things'.

DEFAULT-ACTION is the default action for THING in
`conn-dispatch-on-things'.

\(fn THING &key TARGET-FINDER DEFAULT-ACTION FORWARD-OP BEG-OP END-OP BOUNDS-OP)"
  (intern (symbol-name thing))
  (when-let* ((target-finder (plist-get rest :dispatch-target-finder)))
    (setf (alist-get thing conn-dispatch-target-finders-alist) target-finder))
  (when-let* ((action (plist-get rest :default-action)))
    (setf (alist-get thing conn-dispatch-default-action-alist) action))
  (when-let* ((forward (plist-get rest :forward-op)))
    (put thing 'forward-op forward))
  (when-let* ((beg (plist-get rest :beg-op)))
    (put thing 'beginning-op beg))
  (when-let* ((end (plist-get rest :end-op)))
    (put thing 'end-op end))
  (when-let* ((bounds (plist-get rest :bounds-op)))
    (put thing 'bounds-of-thing-at-point bounds)))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER.

HANDLER will be run from the `post-command-hook' and should be a
function of one argument, the location of `point' before the command
ran.  HANDLER is responsible for calling `conn--push-ephemeral-mark' in
order to mark the region that should be defined by any of COMMANDS."
  (dolist (cmd commands)
    (put cmd :conn-command-thing thing)
    (put cmd :conn-mark-handler handler)))

(defmacro conn-define-mark-command (name thing)
  `(progn
     (defun ,name ()
       (interactive)
       (pcase (ignore-errors (bounds-of-thing-at-point ',thing))
         (`(,beg . ,end)
          (goto-char beg)
          (conn--push-ephemeral-mark end))))
     (put ',name :conn-command-thing ',thing)
     (put ',name :conn-mark-handler #'ignore)))

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :dispatch-target-finder (lambda () (conn--dispatch-all-things 'defun t)))

(conn-register-thing
 'region
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing
 'dots
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing
 'buffer-after-point
 :bounds-op (lambda () (cons (point) (point-max))))

(conn-register-thing
 'buffer-before-point
 :bounds-op (lambda () (cons (point-min) (point))))

(conn-define-mark-command conn-mark-after-point buffer-after-point)
(conn-define-mark-command conn-mark-before-point buffer-before-point)

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

(conn-register-thing-commands
 'dots nil
 'conn-set-mark-command
 'set-mark-command)

(conn-register-thing
 'symbol
 :forward-op 'forward-symbol
 :dispatch-target-finder (lambda () (conn--dispatch-things-with-prefix 'symbol 2 t)))

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing
 'page
 :forward-op 'forward-page)

(conn-register-thing-commands
 'page 'conn-discrete-thing-handler
 'forward-page 'backward-page)

(conn-register-thing-commands
 'char nil
 'forward-char 'backward-char)

(conn-register-thing-commands
 'char 'conn--goto-string-handler
 'conn-forward-char 'conn-backward-char)

(conn-register-thing
 'word
 :forward-op 'forward-word
 :dispatch-target-finder (lambda () (conn--dispatch-things-with-prefix 'word 2 t)))

(conn-register-thing-commands
 'word 'conn-symbol-handler
 'forward-word 'backward-word
 'upcase-word 'downcase-word 'capitalize-word
 'upcase-dwim 'downcase-dwim 'capitalize-dwim)

(conn-register-thing
 'sexp
 :forward-op 'forward-sexp
 :dispatch-target-finder (lambda () (conn--dispatch-things-with-prefix 'sexp 1 t)))

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
 :dispatch-target-finder (lambda () (conn--dispatch-all-things 'sentence t)))

(conn-register-thing-commands
 'sentence 'conn-continuous-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing
 'paragraph
 :forward-op 'forward-paragraph
 :dispatch-target-finder (lambda () (conn--dispatch-all-things 'paragraph t)))

(conn-register-thing-commands
 'paragraph 'conn-continuous-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-continuous-thing-handler
 'end-of-defun 'beginning-of-defun
 'conn-forward-defun)

(conn-register-thing 'char :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'buffer 'conn-discrete-thing-handler
 'end-of-buffer 'beginning-of-buffer)

(defun conn-line-forward-op (N)
  "Move forward across one line.

With arg N do it that many times.  Negative arg -N means move backward N
lines.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (interactive "p")
  (cond ((> N 0)
         (forward-line N))
        ((< N 0)
         (let ((pt (point)))
           (beginning-of-line)
           (if (= pt (point))
               (forward-line N)
             (forward-line (1+ N)))))))

(conn-register-thing
 'line
 :forward-op 'conn-line-forward-op
 :dispatch-target-finder 'conn--dispatch-lines)

(conn-register-thing-commands
 'line 'conn-continuous-thing-handler
 'forward-line 'conn-backward-line
 'conn-line-forward-op)

(conn-register-thing
 'line-column
 :forward-op 'next-line
 :dispatch-target-finder 'conn--dispatch-columns
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
 :dispatch-target-finder 'conn--dispatch-lines)

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
 :dispatch-target-finder 'conn--dispatch-inner-lines)

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
 :dispatch-target-finder 'conn--dispatch-chars)

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
    (setq conn-narrow-ring ring)))

(cl-defmethod register-val-describe ((val conn-narrow-register) _arg)
  (thread-last
    (conn-narrow-register-narrow-ring val)
    (caar)
    (marker-buffer)
    (format "Narrowings In:  %s")
    (princ)))

(static-if (<= 30 emacs-major-version)
    (cl-defmethod register-command-info ((_command (eql conn-narrow-ring-to-register)))
      (make-register-preview-info
       :types '(all)
       :msg "Copy narrow ring to register `%s'"
       :act 'set
       :noconfirm (memq register-use-preview '(nil never))
       :smatch t)))

(defun conn-narrow-ring-to-register (register)
  "Store narrow ring in REGISTER."
  (interactive (list (register-read-with-preview "Narrow ring to register: ")))
  (set-register register (conn--narrow-ring-to-register)))

(static-if (<= 30 emacs-major-version)
    (cl-defmethod register-command-info ((_command (eql conn-push-region-to-narrow-register)))
      (make-register-preview-info
       :types '(all)
       :msg "Push region to narrow register `%s'"
       :act 'set
       :noconfirm (memq register-use-preview '(nil never))
       :smatch t)))

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
   (append (conn-read-thing-mover "Mover")
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
   (append (conn-read-thing-mover "Mover")
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
          (cons narrowing (delete narrowing conn-narrow-ring)))))

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
           do (set-marker beg nil)
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


;;;;; Bounds of narrow ring

(defun conn--bounds-of-narrowings (_cmd _arg)
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
                      (kmacro-ring-head)
                      :filter ,(lambda (cmd)
                                 (unless (or executing-kbd-macro
                                             defining-kbd-macro)
                                   cmd))))
        (message "Keyboard macro bound to %s" (format-kbd-macro key-seq))))))


;;;;; Movement

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

(defun conn-backward-char (string arg)
  "Behaves like `backward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].

If `current-prefix-arg' is 1 prompt for STRING and search backward for nearest
occurrence of STRING.  STRING will finish reading after
`conn-read-string-timeout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout 'backward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (backward-char arg)
    (setq this-command 'conn-goto-string-backward)
    (conn-goto-string-backward string)))

(defun conn-goto-string-backward (string)
  "Go to the first visible occurrence backward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (conn--read-string-with-timeout 'backward)))
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

(defun conn-forward-char (string arg)
  "Behaves like `forward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].

If `current-prefix-arg' is 1 prompt for STRING and search forward for nearest
occurrence of STRING.  STRING will finish reading after
`conn-read-string-timeout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout 'forward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (forward-char arg)
    (setq this-command 'conn-goto-string-forward)
    (conn-goto-string-forward string)))

(defun conn-goto-string-forward (string)
  "Go to the first visible occurrence forward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (conn--read-string-with-timeout 'forward)))
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

(defun conn--goto-string-handler (beg)
  (when (and (not (region-active-p))
             (memq this-command '(conn-goto-string-forward
                                  conn-goto-string-backward))
             (not (eq this-command last-command)))
    (push-mark beg t)))

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
of line proper.

With a non-nil prefix arg go `forward-line' by N instead."
  (interactive "P")
  (if N
      (progn
        (forward-line N)
        (setq conn-this-command-handler (conn-get-mark-handler 'forward-line)
              conn-this-command-thing 'line))
    (let ((point (point))
          (mark (mark t)))
      (conn--end-of-inner-line-1)
      (when (and (= point (point))
                 (or (= mark (save-excursion
                               (back-to-indentation)
                               (point)))
                     (region-active-p)))
        (goto-char (line-end-position))
        (setq conn-this-command-thing 'outer-line)))))

(defun conn-beginning-of-inner-line (&optional N)
  "Move point to the first non-whitespace character in line.

Immediately repeating this command goes to the point at beginning
of line proper.

With a non-nil prefix arg go `forward-line' by -N instead."
  (interactive "P")
  (if N
      (progn
        (forward-line (- N))
        (setq conn-this-command-thing 'line
              conn-this-command-handler (conn-get-mark-handler 'forward-line)))
    (let ((point (point))
          (mark (mark t)))
      (back-to-indentation)
      (when (and (= point (point))
                 (or (= mark (save-excursion
                               (conn--end-of-inner-line-1)
                               (point)))
                     (region-active-p)))
        (goto-char (line-beginning-position))
        (setq conn-this-command-thing 'outer-line)))))

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

If flag is t then `conn-replace-in-thing' and `conn-regexp-replace-in-thing'
will query before replacing from-string, otherwise just replace all
instances of from-string.")

(defvar-keymap conn-replace-map
  "C-RET" 'conn-query-replace
  "C-<return>" 'conn-query-replace
  "C-M-;" 'conn-replace-insert-separator)

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

(defun conn--replace-read-default ()
  (let* ((beg (region-beginning))
         (end (region-end)))
    (if (and (< (- end beg) 60)
             (<= end (save-excursion
                       (goto-char beg)
                       (pos-eol))))
        (buffer-substring-no-properties beg end)
      "")))

(defun conn--replace-read-args (prompt regexp-flag regions &optional noerror default)
  (unless noerror (barf-if-buffer-read-only))
  (conn--with-region-emphasis regions
      (save-mark-and-excursion
        (let* ((delimited-flag (and current-prefix-arg
                                    (not (eq current-prefix-arg '-))))
               (default (or default (conn--replace-read-default)))
               (query-replace-read-from-default
                (if default
                    (lambda () default)
                  query-replace-read-from-default))
               (query-replace-read-from-regexp-default
                (if default
                    (regexp-quote default)
                  query-replace-read-from-regexp-default))
               (conn-query-flag conn-query-flag)
               (from (minibuffer-with-setup-hook
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
                               (make-composed-keymap conn-replace-map)
                               (use-local-map)))
                         (query-replace-read-from prompt regexp-flag))))
               (to (if (consp from)
                       (prog1 (cdr from) (setq from (car from)))
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (thread-last
                             (current-local-map)
                             (make-composed-keymap conn-replace-map)
                             (use-local-map)))
                       (query-replace-read-to from prompt regexp-flag)))))
          (list from to
                (or delimited-flag
                    (and (plist-member (text-properties-at 0 from) 'isearch-regexp-function)
                         (get-text-property 0 'isearch-regexp-function from)))
                (and current-prefix-arg (eq current-prefix-arg '-))
                conn-query-flag)))))

(defun conn-replace-in-thing ( thing-mover arg from-string to-string
                               &optional delimited backward query-flag)
  "Perform a `replace-string' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover "Thing Mover" nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  nil (or (cdr regions) regions) nil)))
     (append (list thing-mover arg) common)))
  (save-excursion
    (pcase-let ((`((,beg . ,end) . ,regions)
                 (or (conn--last-bounds-of-command)
                     (conn-bounds-of-command thing-mover arg))))
      (deactivate-mark t)
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

(defun conn-regexp-replace-in-thing ( thing-mover arg from-string to-string
                                      &optional delimited backward query-flag)
  "Perform a `regexp-replace' within the bounds of a thing."
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover "Thing Mover" nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (conn--replace-read-args
                  (concat "Replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  t (or (cdr regions) regions) nil)))
     (append (list thing-mover arg) common)))
  (save-excursion
    (pcase-let ((`((,beg . ,end) . ,regions)
                 (or (conn--last-bounds-of-command)
                     (conn-bounds-of-command thing-mover arg))))
      (deactivate-mark t)
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

(static-if (<= 30 emacs-major-version)
    (cl-defmethod register-command-info ((_command (eql conn-command-to-register)))
      (make-register-preview-info
       :types '(all)
       :msg "Command to register `%s'"
       :act 'set
       :noconfirm (memq register-use-preview '(nil never))
       :smatch t)))

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

(static-if (<= 30 emacs-major-version)
    (cl-defmethod register-command-info ((_command (eql conn-tab-to-register)))
      (make-register-preview-info
       :types '(all)
       :msg "Tab to register `%s'"
       :act 'set
       :noconfirm (memq register-use-preview '(nil never))
       :smatch t)))

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

(defun conn-isearch-forward-in-thing (thing-cmd thing-arg)
  "Isearch forward within the bounds of a thing."
  (interactive (conn-read-thing-mover "Thing" nil t))
  (let* ((regions (conn-bounds-of-command thing-cmd thing-arg))
         (regions (or (conn--merge-regions (cdr regions) t)
                      (list regions)))
         (in-regions-p (lambda (beg end)
                         (cl-loop for (nbeg . nend) in regions
                                  thereis (<= nbeg beg end nend)))))
    (letrec ((cleanup (lambda ()
                        (remove-hook 'isearch-mode-end-hook cleanup)
                        (remove-function isearch-filter-predicate in-regions-p))))
      (add-hook 'isearch-mode-end-hook cleanup))
    (add-function :after-while isearch-filter-predicate in-regions-p
                  '((isearch-message-prefix . "[THING] ")))
    (isearch-forward nil t)))

(defun conn-isearch-backward-in-thing (thing-cmd thing-arg)
  "Isearch backward within the bounds of a thing."
  (interactive (conn-read-thing-mover "Thing" nil t))
  (let* ((regions (conn-bounds-of-command thing-cmd thing-arg))
         (regions (or (conn--merge-regions (cdr regions) t)
                      (list regions)))
         (in-regions-p (lambda (beg end)
                         (cl-loop for (nbeg . nend) in regions
                                  thereis (<= nbeg beg end nend)))))
    (letrec ((cleanup (lambda ()
                        (remove-hook 'isearch-mode-end-hook cleanup)
                        (remove-function isearch-filter-predicate in-regions-p))))
      (add-hook 'isearch-mode-end-hook cleanup))
    (add-function :after-while isearch-filter-predicate in-regions-p
                  '((isearch-message-prefix . "[THING] ")))
    (isearch-backward nil t)))

(defun conn-multi-isearch-project ()
  "Perform a `multi-isearch' within the files of a project."
  (interactive)
  (require 'project)
  (multi-isearch-files
   (seq-uniq (cons (buffer-file-name)
                   (project-files (project-current)))
             'file-equal-p)))

(defun conn-isearch-region-forward (beg end)
  "Isearch forward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (isearch-mode t)
  (with-isearch-suspended
   (setq isearch-new-string (buffer-substring-no-properties beg end)
         isearch-new-message (mapconcat #'isearch-text-char-description
                                        isearch-new-string ""))))

(defun conn-isearch-region-backward (beg end)
  "Isearch backward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (isearch-mode nil)
  (with-isearch-suspended
   (setq isearch-new-string (buffer-substring-no-properties beg end)
         isearch-new-message (mapconcat #'isearch-text-char-description
                                        isearch-new-string ""))))

(defun conn-isearch-exit-and-mark ()
  "`isearch-exit' and set region to match."
  (interactive)
  (isearch-done)
  (conn--push-ephemeral-mark isearch-other-end))


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


;;;;;; Mark ring

(defvar-local conn-mark-ring nil
  "List of interesting former marks of the current buffer, most recent first.

Conn adds many uninteresting marks to the `mark-ring' and so to
ameliorate the problem implements this alternative mark ring which
filters out the uninteresting marks.  See also `conn-pop-mark-ring' and
`conn-unpop-mark-ring'.")

(defvar conn-mark-ring-max 40
  "Maximum length of `conn-mark-ring'.")

(defun conn--push-mark-ring (location &optional back)
  (unless conn-mark-ring
    (setq conn-mark-ring (conn-ring conn-mark-ring-max
                                    :cleanup (lambda (mk) (set-marker mk nil)))))
  (pcase-let ((ptb (conn-ring-back conn-mark-ring))
              (ptf (conn-ring-front conn-mark-ring)))
    (cond
     ((and ptf (= location ptf))
      (when back (conn-ring-rotate-forward conn-mark-ring)))
     ((and ptb (= location ptb))
      (unless back (conn-ring-rotate-backward conn-mark-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-mark-ring (conn--create-marker location))
        (conn-ring-insert-front conn-mark-ring (conn--create-marker location)))))))

(defun conn-pop-mark-ring ()
  "Like `pop-to-mark-command' but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-forward conn-mark-ring)
    (goto-char (conn-ring-front conn-mark-ring)))
  (deactivate-mark))

(defun conn-unpop-mark-ring ()
  "Like `pop-to-mark-command' in reverse but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-backward conn-mark-ring)
    (goto-char (conn-ring-front conn-mark-ring)))
  (deactivate-mark))


;;;;;; Movement ring

(defvar-local conn-movement-ring nil
  "List of previous regions, most recent first.

See also `conn-pop-movement-ring' and `conn-unpop-movement-ring'.")

(defvar conn-movement-ring-max 10
  "Maximum length of `conn-movement-ring'.")

(defun conn-push-region (point mark &optional back)
  (unless conn-movement-ring
    (setq conn-movement-ring (conn-ring conn-movement-ring-max
                                        :cleanup (pcase-lambda (`(,pt . ,mk))
                                                   (set-marker pt nil)
                                                   (set-marker mk nil)))))
  (pcase-let ((`(,ptf . ,mkf) (conn-ring-front conn-movement-ring))
              (`(,ptb . ,mkb) (conn-ring-back conn-movement-ring)))
    (cond
     ((and ptf (= point ptf) (= mark mkf))
      (when back (conn-ring-rotate-backward conn-movement-ring)))
     ((and ptb (= point ptb) (= mark mkb))
      (unless back (conn-ring-rotate-forward conn-movement-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-movement-ring
                                 (cons (conn--create-marker point)
                                       (conn--create-marker mark)))
        (conn-ring-insert-front conn-movement-ring
                                (cons (conn--create-marker point)
                                      (conn--create-marker mark))))))))

(defun conn-unpop-movement-ring (arg)
  "Rotate backward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-rotating t)
  (cond ((< arg 0)
         (conn-pop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-region (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-backward conn-movement-ring))
         (pcase (conn-ring-front conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

(defun conn-pop-movement-ring (arg)
  "Rotate forward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-rotating t)
  (cond ((< arg 0)
         (conn-unpop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-region (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-forward conn-movement-ring))
         (pcase (conn-ring-front conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))


;;;;; Case Commands

(defun conn--apply-region-transform (transform-func)
  "Apply TRANSFORM-FUNC to region contents.

Handles rectangular regions."
  (save-mark-and-excursion
    (let ((case-fold-search nil))
      (if (and (fboundp 'apply-on-rectangle)
               (bound-and-true-p rectangle-mark-mode))
          (apply-on-rectangle
           (lambda (start-col end-col)
             (with-restriction
                 (+ (point) start-col) (+ (point) end-col)
               (goto-char (point-min))
               (funcall transform-func)))
           (region-beginning) (region-end))
        (with-restriction
            (region-beginning) (region-end)
          (goto-char (point-min))
          (funcall transform-func))))))

(defun conn--buffer-to-words ()
  (let ((subword-p (and (boundp 'subword-mode) subword-mode)))
    (subword-mode 1)
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\W+" nil t)
            (replace-match " "))
          (goto-char (point-min))
          (forward-word 1)
          (while (/= (point) (point-max))
            (unless (looking-at " ")
              (insert " "))
            (forward-word 1))
          (goto-char (point-min))
          (delete-horizontal-space)
          (goto-char (point-max))
          (delete-horizontal-space))
      (unless subword-p (subword-mode -1)))))

(defun conn-kebab-case-region ()
  "Transform region text to kebab-case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (conn--buffer-to-words)
     (downcase-region (point-min) (point-max))
     (while (re-search-forward " " nil t)
       (replace-match "-")))))

(defun conn-capital-snake-case-region ()
  "Transform region text to Capital_Snake_Case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (conn--buffer-to-words)
     (capitalize-region (point-min) (point-max))
     (while (re-search-forward " " nil t)
       (replace-match "_")))))

(defun conn-snake-case-region ()
  "Transform region text to snake_case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (conn--buffer-to-words)
     (downcase-region (point-min) (point-max))
     (while (re-search-forward " " nil t)
       (replace-match "_")))))

(defun conn-capital-case-region ()
  "Transform region text to CapitalCase."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (conn--buffer-to-words)
     (capitalize-region (point-min) (point-max))
     (while (re-search-forward " " nil t)
       (replace-match "")))))

(defun conn-camel-case-region ()
  "Transform region text to camelCase."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (conn--buffer-to-words)
     (capitalize-region (point-min) (point-max))
     (downcase-region (point-min) (1+ (point-min)))
     (goto-char (point-min))
     (while (re-search-forward " " nil t)
       (replace-match "")))))

(defun conn-case-to-words-region ()
  "Transform region text to individual words."
  (interactive)
  (conn--apply-region-transform 'conn--buffer-to-words))


;;;;; Transpose

(conn-define-state conn-read-transpose-state (conn-read-mover-state))
(put 'conn-transpose-regions :conn-read-state 'conn-read-transpose-state)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-transpose-state)
  "i" 'conn-backward-line
  "k" 'forward-line
  "u" 'forward-symbol
  "f" 'conn-dispatch-transpose)

(defun conn--transpose-message ()
  (message
   (substitute-command-keys
    (concat
     "Define region. "
     "Press \\[exit-recursive-edit] to end and use current region. "
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
        (add-hook 'post-command-hook 'conn--transpose-message))
    (setq eldoc-message-function conn--transpose-eldoc-prev-msg-fn
          conn--transpose-eldoc-prev-msg-fn nil)
    (remove-hook 'post-command-hook 'conn--transpose-message)))

(define-keymap
  :keymap (conn-get-mode-map 'conn-command-state
                             'conn-transpose-recursive-edit-mode)
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(defun conn-transpose-regions (mover arg)
  "Exchange regions defined by a thing command.

With argument ARG 0, exchange the things at point and mark.

If MOVER is \\='recursive-edit then exchange the current region and the
region after a `recursive-edit'."
  (interactive
   (conn-read-thing-mover
    "Mover"
    (when current-prefix-arg
      (prefix-numeric-value current-prefix-arg))
    t))
  (when conn-transpose-recursive-edit-mode
    (user-error "Recursive call to conn-transpose-regions"))
  (deactivate-mark t)
  (pcase mover
    ('recursive-edit
     (let ((beg (region-beginning))
           (end (region-end))
           (buf (current-buffer)))
       (conn-transpose-recursive-edit-mode 1)
       (unwind-protect
           (recursive-edit)
         (conn-transpose-recursive-edit-mode -1))
       (if (eq buf (current-buffer))
           (transpose-regions beg end (region-beginning) (region-end))
         (let ((str1 (filter-buffer-substring (region-beginning) (region-end) t))
               str2)
           (with-current-buffer buf
             (setq str2 (filter-buffer-substring beg end t))
             (insert str1))
           (insert str2)))))
    ('conn-dispatch-transpose
     (pcase-let* ((conn-state-for-read-dispatch 'conn-read-transpose-state)
                  (`(,thing-cmd ,thing-arg ,finder ,_ ,action-args ,predicate ,_)
                   (conn--dispatch-read-thing 'conn-dispatch-transpose)))
       (conn-dispatch-on-things thing-cmd thing-arg finder
                                'conn-dispatch-transpose
                                action-args predicate)))
    ((let 0 arg)
     (pcase-let* ((thing (get mover :conn-command-thing))
                  (`(,beg1 . ,end1) (if (region-active-p)
                                        (cons (region-beginning) (region-end))
                                      (bounds-of-thing-at-point thing)))
                  (`(,beg2 . ,end2) (save-excursion
                                      (goto-char (mark t))
                                      (bounds-of-thing-at-point thing))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((let thing (get mover :conn-command-thing))
     (transpose-subr (lambda (N) (forward-thing thing N))
                     (prefix-numeric-value arg)))))


;;;;; Line commands

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

(defun conn-join-lines-in-thing (thing-mover thing-arg)
  "`delete-indentation' in region from START and END."
  (interactive (conn-read-thing-mover "Thing Mover"))
  (save-mark-and-excursion
    (pcase (conn-bounds-of-command thing-mover thing-arg)
      (`((,beg . ,end) . ,_)
       (delete-indentation nil beg end)
       (indent-according-to-mode)))))


;;;;; Prepend/append to kill/register

(defun conn-append-region (beg end &optional register kill-flag)
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
  (if register
      (append-to-register register beg end kill-flag)
    (kill-append (pcase (alist-get register-separator register-alist)
                   ((and (pred stringp) sep)
                    (concat sep (filter-buffer-substring beg end kill-flag)))
                   (_ (filter-buffer-substring beg end kill-flag)))
                 nil))
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
  (if register
      (prepend-to-register register beg end kill-flag)
    (kill-append (pcase (alist-get register-separator register-alist)
                   ((and (pred stringp) sep)
                    (concat (filter-buffer-substring beg end kill-flag) sep))
                   (_ (filter-buffer-substring beg end kill-flag)))
                 t))
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
   (append (conn-read-thing-mover "Thing Mover")
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
   (append (conn-read-thing-mover
            "Thing Mover"
            (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
            t)
           (list t)))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
    (conn--narrow-to-region-1 beg end record)
    (when (called-interactively-p 'interactive)
      (message "Buffer narrowed"))))

(defun conn-narrow-indirect-to-thing (thing-mover arg &optional interactive)
  "Narrow to THING at point.

Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive
   (append (conn-read-thing-mover
            "Thing Mover"
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

(defun conn-narrow-indirect-to-region (beg end &optional record)
  "Narrow to THING at point.

Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (region-beginning) (region-end) (list t)))
  (conn--narrow-indirect beg end record)
  (when (called-interactively-p 'interactive)
    (message "Buffer narrowed indirect")))


;;;;; Register setting and loading

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
   (list
    (register-read-with-preview "Load register: ")
    current-prefix-arg))
  (when (use-region-p)
    (if (bound-and-true-p rectangle-mark-mode)
        (delete-rectangle (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end))))
  (condition-case err
      (jump-to-register reg arg)
    (user-error
     (unless (string-search "access aborted" (error-message-string err))
       (insert-register reg (not arg))))))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))


;;;;; Killing and yanking commands

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

(defvar-local conn--minibuffer-initial-region nil)

(defun conn-yank (&optional register)
  "Like `yank' but with a prefix argument `conn-register-load' instead."
  (interactive
   (list (when current-prefix-arg
           (register-read-with-preview "Register: "))))
  (if register
      (conn-register-load register)
    (funcall (conn--without-conn-maps
               (key-binding conn-yank-keys t)))))

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
  (conn--without-conn-maps
    (if arg
        (funcall (key-binding conn-kill-region-keys t) start end)
      (funcall (or (key-binding conn-delete-region-keys t)
                   'delete-region)
               start end))
    (funcall (key-binding conn-yank-keys t))))

(defun conn-copy-region (start end &optional register)
  "Copy region between START and END as kill.

If REGISTER is given copy to REGISTER instead."
  (interactive (list (region-beginning)
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
                               (key-binding conn-backward-delete-char-keys t))))
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
              (key-binding conn-kill-region-keys t))))))

(defun conn-completing-yank-replace (start end &optional arg)
  "Replace region from START to END with result of `yank-from-kill-ring'.

If ARG is non-nil `kill-region' instead of `delete-region'."
  (interactive (list (region-beginning)
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


;;;;; Duplicate commands

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
  (interactive (list (region-beginning)
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

(defun conn-duplicate-thing (thing-mover thing-arg N)
  "Duplicate the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive (append (conn-read-thing-mover "Thing Mover" nil t)
                       (list (prefix-numeric-value current-prefix-arg))))
  (pcase (conn-bounds-of-command thing-mover thing-arg)
    (`((,beg . ,end) . ,_)
     (if (use-region-p)
         (duplicate-dwim)
       (let ((end (set-marker (make-marker) end)))
         (unwind-protect
             (dotimes (_ N)
               (conn--duplicate-region-1 beg end))
           (goto-char end)
           (indent-region beg (point))
           (set-marker end nil)))))))

(defun conn-duplicate-and-comment-region (beg end &optional arg)
  "Duplicate and comment the current region."
  (interactive (list (region-beginning)
                     (region-end)
                     (prefix-numeric-value current-prefix-arg)))
  (pcase-let* ((origin (point))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun conn-duplicate-and-comment-thing (thing-mover thing-arg N)
  "Duplicate and comment the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive (append (conn-read-thing-mover "Thing Mover" nil t)
                       (list (prefix-numeric-value current-prefix-arg))))
  (pcase (conn-bounds-of-command thing-mover thing-arg)
    ((and `((,beg . ,end) . ,_)
          (let offset (- (point) end))
          (let mark-offset (- (point) (mark t)))
          (let region (buffer-substring-no-properties beg end)))
     (goto-char end)
     (comment-or-uncomment-region beg end)
     (setq end (if (bolp) (point) (line-end-position)))
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


;;;;; Misc commands

(defun conn-comment-or-uncomment-thing (thing-mover arg)
  "Toggle commenting of a region defined by a thing command."
  (interactive (conn-read-thing-mover "Thing Mover"))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
    (if (comment-only-p beg end)
        (uncomment-region beg end)
      (let ((comment-empty-lines t))
        (comment-region beg end)))))

(defun conn-shell-command-on-region (&optional arg)
  "Like `shell-command-on-region' but inverts the meaning of ARG."
  (interactive "P")
  (let ((current-prefix-arg (not arg)))
    (call-interactively 'shell-command-on-region)))

(defun conn-rgrep-region (beg end)
  "`rgrep' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (let ((search-string (read-string "Search for: "
                                    (regexp-quote (buffer-substring-no-properties beg end))
                                    'grep-regexp-history)))
    (rgrep search-string)))

(defun conn-occur-region (beg end)
  "`occur' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (let ((search-string (read-string "Search for: "
                                    (regexp-quote (buffer-substring-no-properties beg end))
                                    'grep-regexp-history)))
    (occur search-string)))

(defun conn-org-edit-insert-heading ()
  "Insert org heading."
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content))


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
                 (?g "prompt")
                 (?c "current window"))))
    (?w (other-window-prefix))
    (?f (other-frame-prefix))
    (?t (other-tab-prefix))
    (?g (conn-other-window-prompt-prefix))
    (?c (conn-this-window-prefix))))

(defun conn-other-window-prompt-prefix ()
  "Display next buffer in a window selected by `conn-prompt-for-window'."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (cons (conn-prompt-for-window (conn--get-windows nil 'nomini) t)
           'reuse))))

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
          (conn--get-windows nil 'nomini 'visible))))
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
         (cons (conn-prompt-for-window (conn--get-windows nil 'nomini))
               'reuse))))))

(defun conn-yank-window (window)
  "Swap selected window and another window.

Currently selected window remains selected afterwards."
  (interactive
   (list (conn-prompt-for-window
          (conn--get-windows nil 'nomini 'visible))))
  (unless (eq window (selected-window))
    (if window
        (save-selected-window (window-swap-states nil window))
      (user-error "No other visible windows"))))


;;;;; Transition Functions

(defun conn-previous-state ()
  (interactive)
  (conn-enter-state conn-previous-state))

(defun conn-org-edit-state ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive)
  (conn-enter-state 'conn-org-edit-state))

(defun conn-insert-state ()
  "Enter insert state for the current buffer."
  (interactive)
  (conn-enter-state conn-state-for-emacs))

(defun conn-command-state ()
  "Enter command state for the current buffer."
  (interactive)
  (conn-enter-state conn-state-for-command))

(defun conn-emacs-state-at-mark ()
  "Exchange point and mark then enter `conn-emacs-state'."
  (interactive)
  (conn-exchange-mark-command)
  (conn-enter-state conn-state-for-emacs))

(defun conn-change-whole-line (&optional arg)
  "`kill-whole-line' and enter `conn-emacs-state'."
  (interactive "P")
  (kill-whole-line arg)
  (open-line 1)
  (indent-according-to-mode)
  (conn-enter-state conn-state-for-emacs))

(defun conn-change-line ()
  "`kill-line' and enter `conn-emacs-state'."
  (interactive)
  (call-interactively (key-binding conn-kill-line-keys t))
  (conn-enter-state conn-state-for-emacs))

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
  (conn-enter-state conn-state-for-emacs))

(defun conn-emacs-state-open-line (&optional arg)
  "Open line and enter `conn-emacs-state'.

If ARG is non-nil move down ARG lines before opening line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent)
  (conn-enter-state conn-state-for-emacs))

(defun conn-emacs-state-overwrite (&optional arg)
  "Enter emacs state in `overwrite-mode'.

`overwrite-mode' will be turned off when when emacs state is exited.
If ARG is non-nil enter emacs state in `binary-overwrite-mode' instead."
  (interactive "P")
  (conn-enter-state conn-state-for-emacs)
  (letrec ((hook (lambda (_state)
                   (overwrite-mode -1)
                   (remove-hook 'conn-exit-functions hook))))
    (add-hook 'conn-exit-functions hook))
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
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (cond ((and (bound-and-true-p rectangle-mark-mode) kill)
         (copy-rectangle-as-kill start end)
         (call-interactively #'string-rectangle))
        ((bound-and-true-p rectangle-mark-mode)
         (call-interactively #'string-rectangle))
        (kill
         (funcall (conn--without-conn-maps
                    (key-binding conn-kill-region-keys t))
                  start end)
         (conn-enter-state conn-state-for-emacs))
        (t
         (funcall (conn--without-conn-maps
                    (key-binding conn-delete-region-keys t))
                  start end)
         (conn-enter-state conn-state-for-emacs))))

(defun conn-emacs-state-eol (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (end-of-line N)
  (conn-enter-state conn-state-for-emacs))

(defun conn-emacs-state-bol (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (beginning-of-line N)
  (conn-enter-state conn-state-for-emacs))

(defun conn-emacs-state-eoil (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-end-of-inner-line N)
  (conn-enter-state conn-state-for-emacs))

(defun conn-emacs-state-boil (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-beginning-of-inner-line N)
  (conn-enter-state conn-state-for-emacs))


;;;; WinControl

;; A simple version of hyperbole's hycontrol-windows

(defgroup conn-wincontrol nil
  "Conn-mode WinControl."
  :prefix "conn-wincontrol-"
  :group 'conn)

(defcustom conn-wincontrol-initial-help 'window-1
  "Initial help message printed during `conn-wincontrol-mode'."
  :group 'conn-wincontrol
  :type '(choice (const :tag "Window" window-1)
                 (const :tag "Frame" frame)
                 (const :tag "Tab" tab)
                 (const :tag "Short" nil)))

(defvar conn--wincontrol-help-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) ", "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-exit]: exit"))

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--previous-scroll-conservatively)
(defvar conn--wincontrol-help)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)
(defvar conn--wincontrol-prev-eldoc-msg-fn)


;;;;; Wincontrol internals

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
  "o" 'conn-wincontrol-next-window)

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
  "M-j" 'conn-wincontrol-windmove-right
  "M-l" 'conn-wincontrol-windmove-left
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
  "C" 'tab-bar-duplicate-tab
  "c" (conn-remap-key (key-parse "C-c"))
  "d" 'delete-window
  "h" 'kill-buffer-and-window
  "e" 'conn-wincontrol-exit
  "F" 'toggle-frame-fullscreen
  "f" 'conn-goto-window
  "g" 'delete-other-windows
  "m" 'conn-wincontrol-mru-window
  "I" 'tab-new
  "i" 'tab-next
  "j" 'previous-buffer
  "J" 'bury-buffer
  "K" 'tab-close
  "k" 'tab-previous
  "l" 'next-buffer
  "L" 'unbury-buffer
  "M" 'tab-bar-move-window-to-tab
  "o" 'conn-wincontrol-next-window
  "O" 'tear-off-window
  "p" 'conn-register-prefix
  "x" (conn-remap-key (key-parse "C-x"))
  "r" 'conn-wincontrol-split-right
  "R" 'conn-wincontrol-isearch-other-window-backward
  "S" 'conn-wincontrol-isearch-other-window
  "s" conn-window-resize-map
  "`" 'tab-switch
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
  ;; TODO: make inverse-video a custom option
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line t))
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line nil)))

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn--wincontrol-setup t)))

(defun conn-wincontrol-one-command ()
  "Execute one command in `conn-wincontrol-mode'."
  (interactive)
  (letrec ((pre-hook (lambda ()
                       (unless (memq this-command
                                     '(conn-wincontrol-forward-delete-arg
                                       conn-wincontrol-backward-delete-arg
                                       conn-wincontrol-digit-argument-reset
                                       conn-wincontrol-invert-argument
                                       conn-wincontrol-digit-argument
                                       conn-wincontrol-universal-arg))
                         (remove-hook 'pre-command-hook pre-hook)
                         (conn-wincontrol-exit)))))
    (add-hook 'pre-command-hook pre-hook 90))
  (conn-wincontrol))


;;;;; Wincontrol prefix arg

(defun conn-wincontrol-universal-arg ()
  "Multiply wincontrol prefix arg by 4."
  (interactive)
  (setq conn--wincontrol-arg (* 4 (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-digit-argument (N)
  "Append N to wincontrol prefix arg.

When called interactively N is `last-command-event'."
  (interactive (list (- (logand last-command-event ?\177) ?0)))
  (if conn--wincontrol-arg
      (setq conn--wincontrol-arg
            (+ (if (>= (or conn--wincontrol-arg 1) 0) N (- N))
               (* 10 (or conn--wincontrol-arg 1))))
    (setq conn--wincontrol-arg N))
  (setq this-command 'conn-wincontrol-digit-argument))

(defun conn-wincontrol-invert-argument ()
  "Invert sign of wincontrol prefix arg."
  (interactive)
  (setq conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to nil and sign to +."
  (interactive)
  (setq conn--wincontrol-arg nil)
  (setq conn--wincontrol-arg-sign 1))

(defun conn-wincontrol-backward-delete-arg ()
  "Delete least significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-arg (floor conn--wincontrol-arg 10)))

(defun conn-wincontrol-forward-delete-arg ()
  "Delete most significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-arg (thread-last
                               (log conn--wincontrol-arg 10)
                               (floor)
                               (expt 10)
                               (mod conn--wincontrol-arg))))


;;;;; Wincontrol quiting

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


;;;;; Wincontrol isearch

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


;;;;; Window selection

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
          (conn--get-windows nil 'nomini 'visible))))
  (select-window window))

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


;;;;; Window scroll commands

(defun conn-wincontrol-other-window-scroll-down (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (setq this-command 'conn-scroll-down)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines arg))
      (funcall (or (command-remapping #'scroll-down-command)
                   (command-remapping #'conn-scroll-down)
                   #'conn-scroll-down)))))

(defun conn-wincontrol-other-window-scroll-up (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (setq this-command 'conn-scroll-up)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines arg))
      (funcall (or (command-remapping #'scroll-up-command)
                   (command-remapping #'conn-scroll-up)
                   #'conn-scroll-up)))))

(defun conn-wincontrol-scroll-down (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (setq this-command 'conn-scroll-down)
  (let ((next-screen-context-lines arg))
    (conn-scroll-down)))

(defun conn-wincontrol-scroll-up (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (setq this-command 'conn-scroll-up)
  (let ((next-screen-context-lines arg))
    (conn-scroll-up)))


;;;;; Window configuration commands

(defun conn-wincontrol-widen-window ()
  (interactive)
  (enlarge-window-horizontally
   (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-narrow-window ()
  (interactive)
  (shrink-window-horizontally
   (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-heighten-window ()
  (interactive)
  (enlarge-window
   (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-shorten-window ()
  (interactive)
  (shrink-window
   (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-split-vertically ()
  "Split window vertically.
Uses `split-window-vertically'."
  (interactive)
  (select-window
   (split-window-vertically)))

(defun conn-wincontrol-split-right ()
  "Split window vertically.
Uses `split-window-right'."
  (interactive)
  (select-window
   (split-window-right)))

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
    (define-keymap
      :keymap conn-wincontrol-map
      "\\" 'transpose-window-layout
      "," 'rotate-windows-back
      "." 'rotate-windows
      "<" 'rotate-window-layout-counterclockwise
      ">" 'rotate-window-layout-clockwise
      "|" 'flip-window-layout-horizontally
      "_" 'flip-window-layout-vertically))


;;;; Keymaps

;;;;; Repeat map

(defvar-keymap conn-reb-navigation-repeat-map
  :repeat t
  "C-s" 'reb-next-match
  "C-r" 'reb-prev-match)

(defvar-keymap conn-pop-mark-repeat-map
  :repeat t
  "o" 'conn-pop-mark-ring
  "u" 'conn-unpop-mark-ring)

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "`" 'other-window)

(defvar-keymap conn-other-buffer-repeat-map
  :repeat t
  "&" 'conn-other-buffer)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)


;;;;; Mode Keymaps

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-mode) "C-c e" 'occur-edit-mode))

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-edit-mode) "C-c e" 'occur-cease-edit))

(with-eval-after-load 'compilation-mode
  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-movement-state 'compilation-mode)
    "<" 'previous-error-no-select
    ">" 'next-error-no-select))

(with-eval-after-load 'rectangle-mark-mode
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
    "#" 'rectangle-number-lines))

(defvar-keymap conn-isearch-map
  "M-Y" 'conn-isearch-yank-region
  "M-<return>" 'conn-isearch-exit-and-mark
  "M-RET" 'conn-isearch-exit-and-mark
  "M-\\" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch
  "C-'" 'conn-isearch-open-recursive-edit)


;;;;; Top-level command state maps

(defvar-keymap conn-default-region-map
  "\\" 'conn-kapply-on-region-prefix
  "TAB" 'indent-rigidly
  "$" 'ispell-region
  "*" 'calc-grab-region
  ";" 'comment-or-uncomment-region
  "e" 'conn-duplicate-region
  "d" 'conn-duplicate-and-comment-region
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule
  "b" 'conn-comment-or-uncomment-thing
  "'" 'conn-duplicate-and-comment-thing
  "," 'conn-duplicate-thing
  "g" 'conn-rgrep-region
  "k" 'delete-region
  "u" 'conn-join-lines-in-region
  "I" 'indent-rigidly
  "p" 'conn-sort-prefix
  "o" 'conn-occur-region
  "h" 'vc-region-history
  "s" 'conn-isearch-region-forward
  "r" 'conn-isearch-region-backward
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "n" 'conn-narrow-to-region
  "m" 'conn-narrow-indirect-to-region)

(defvar-keymap conn-search-map
  "h \\" 'conn-kapply-hightlight-prefix
  "s" 'conn-isearch-forward-in-thing
  "r" 'conn-isearch-backward-in-thing
  "o" 'occur
  "l" 'locate
  "m B" 'multi-isearch-buffers-regexp
  "m F" 'multi-isearch-files-regexp
  "m b" 'multi-isearch-buffers
  "m p" 'conn-multi-isearch-project
  "m f" 'multi-isearch-files)

(defvar-keymap conn-goto-map
  "p" 'pop-global-mark
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward
  "j" 'previous-error
  "l" 'next-error
  "y" 'imenu
  "b" 'goto-line
  "m" 'conn-unpop-movement-ring
  "n" 'conn-pop-movement-ring)

(defvar-keymap conn-movement-ring-repeat-map
  :repeat t
  "n" 'conn-unpop-movement-ring
  "m" 'conn-pop-movement-ring)

(defvar-keymap conn-global-mark-repeat-map
  :repeat t
  "p" 'pop-global-mark)

(defvar-keymap conn-error-repeat-map
  :repeat t
  "l" 'previous-error
  "j" 'next-error)

(defvar-keymap conn-default-edit-map
  "\\" 'conn-kapply-on-thing-prefix
  "z" 'conn-emacs-state-at-mark
  "v" 'diff-buffer-with-file
  "F" 'conn-bind-last-dispatch-to-key
  "RET" 'whitespace-cleanup
  "q" 'conn-replace-in-thing
  "r" 'conn-regexp-replace-in-thing
  "m" 'conn-narrow-indirect-to-thing
  "n" 'conn-narrow-to-thing
  "u" 'conn-join-lines-in-thing
  "f" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "DEL" 'conn-change-whole-line
  "," 'clone-indirect-buffer
  "h" 'conn-change-line
  "i" 'conn-emacs-state-open-line-above
  "k" 'conn-emacs-state-open-line
  "l" 'conn-emacs-state-eoil
  "e" 'conn-emacs-state-eol
  "j" 'conn-emacs-state-boil
  "a" 'conn-emacs-state-bol
  "g" 'conn-emacs-state-overwrite
  "b" 'conn-emacs-state-overwrite-binary
  "x" 'conn-narrow-ring-prefix
  "d" 'duplicate-dwim
  "w j" 'conn-kill-prepend-region
  "w l" 'conn-kill-append-region
  "c j" 'conn-append-region
  "c l" 'conn-append-region
  "c v" 'conn-copy-thing
  "c i" 'copy-from-above-command
  "y" 'yank-in-context)


;;;;; Misc maps

(defvar-keymap conn-indent-rigidly-map
  "l" 'indent-rigidly-right
  "j" 'indent-rigidly-left
  "L" 'indent-rigidly-right-to-tab-stop
  "J" 'indent-rigidly-left-to-tab-stop)


;;;;; Global bindings

(defvar-keymap conn--global-binding-map
  ;; "M-j" 'conn-open-line-and-indent
  ;; "C-o" 'conn-open-line-above
  ;; "M-o" 'conn-open-line
  ;; "C-x l" 'next-buffer
  ;; "C-x j" 'previous-buffer
  "M-g o" 'conn-pop-mark-ring
  "M-g u" 'conn-unpop-mark-ring
  "C-S-w" 'delete-region
  "C-." 'conn-dispatch-on-things
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
  "<conn-thing-map> <" 'conn-mark-before-point
  "<conn-thing-map> >" 'conn-mark-after-point
  "<conn-thing-map> /" 'conn-mark-filename
  "<conn-thing-map> U" 'conn-mark-uuid
  "<conn-thing-map> s" 'conn-mark-string
  "<conn-thing-map> @" 'conn-mark-email
  "<conn-thing-map> v" 'conn-mark-visible
  "<conn-thing-map> L" 'forward-line
  "<conn-thing-map> )" 'forward-list
  "<conn-thing-map> (" 'backward-list)

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
  :keymap (conn-get-state-map 'conn-read-thing-common-state)
  "h" conn-thing-map)

(define-keymap
  :keymap (conn-get-state-map 'conn-movement-state)
  :suppress t
  ">" 'forward-line
  "<" 'conn-backward-line
  "o" (conn-remap-key conn-forward-word-keys)
  "O" 'forward-symbol
  "U" 'conn-backward-symbol
  "u" (conn-remap-key conn-backward-word-keys)
  "(" (conn-remap-key conn-backward-list-keys)
  ")" (conn-remap-key conn-forward-list-keys)
  "[" (conn-remap-key conn-backward-up-list-keys)
  "]" (conn-remap-key conn-down-list-keys)
  "{" (conn-remap-key conn-backward-sentence-keys)
  "}" (conn-remap-key conn-forward-sentence-keys)
  "I" (conn-remap-key conn-backward-paragraph-keys)
  "i" (conn-remap-key conn-previous-line-keys)
  "J" 'conn-backward-inner-line
  "j" (conntext-define conntext-backward-char
        (let ((binding (key-binding conn-backward-char-keys t)))
          (if (eq binding 'backward-char)
              'conn-backward-char
            binding)))
  "K" (conn-remap-key conn-forward-paragraph-keys)
  "k" (conn-remap-key conn-next-line-keys)
  "L" 'conn-forward-inner-line
  "l" (conntext-define conntext-forward-char
        (let ((binding (key-binding conn-forward-char-keys t)))
          (if (eq binding 'forward-char)
              'conn-forward-char
            binding)))
  "M" (conn-remap-key conn-end-of-defun-keys)
  "m" (conn-remap-key conn-forward-sexp-keys)
  "N" (conn-remap-key conn-beginning-of-defun-keys)
  "n" (conn-remap-key conn-backward-sexp-keys))

(define-keymap
  :keymap (conn-get-state-map 'conn-menu-state)
  :suppress t
  "s" (conn-remap-keymap (key-parse "M-s"))
  "g" (conn-remap-keymap (key-parse "M-g"))
  "c" (conn-remap-key (key-parse "C-c"))
  "x" (conn-remap-key (key-parse "C-x"))
  "C-4" (conn-remap-key (key-parse "C-x 4"))
  "C-5" (conn-remap-key (key-parse "C-x 5")))

(define-keymap
  :keymap (conn-get-state-map 'conn-emacs-state)
  "<f8>" 'conn-command-state)

(define-keymap
  :keymap (conn-get-state-map 'conn-command-state)
  :suppress t
  "Z" 'pop-to-mark-command
  "P" 'conn-region-case-prefix
  "&" 'conn-other-buffer
  "e" 'conn-insert-state
  "E" 'conn-dispatch-on-buttons
  "q" 'conn-change
  ":" 'conn-wincontrol-one-command
  "`" 'other-window
  "|" 'conn-shell-command-on-region
  "'" 'repeat
  "." 'conn-other-place-prefix
  "/" (conn-remap-key conn-undo-keys)
  ";" 'conn-wincontrol
  "\\" 'conn-kapply-prefix
  "=" 'indent-relative
  "?" (conn-remap-key conn-undo-redo-keys)
  "_" 'repeat-complex-command
  "SPC" 'conn-set-mark-command
  "M-y" 'conn-completing-yank-replace
  "C-M-l" 'conn-recenter-on-region
  "C-M-S-l" 'conn-recenter-on-region-other-window
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" conn-edit-map
  "C" 'conn-copy-region
  "d" (conn-remap-key conn-delete-char-keys)
  "f" 'conn-dispatch-on-things
  "F" 'conn-repeat-last-dispatch
  "h" 'conn-wincontrol-one-command
  "," conn-thing-map
  "p" 'conn-register-prefix
  "t" 'conn-transpose-regions
  "r" conn-region-map
  "R" 'conn-rectangle-mark
  "V" 'conn-narrow-to-thing
  "v" 'conn-toggle-mark-command
  "w" 'conn-kill-region
  "W" 'widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" 'conn-yank
  "z" 'conn-exchange-mark-command)

(define-keymap
  :keymap (conn-get-state-map 'conn-org-edit-state)
  :suppress t
  "e" 'conn-insert-state
  "SPC" 'conn-scroll-up
  "<backspace>" 'conn-scroll-down
  "DEL" 'conn-scroll-down
  "." 'point-to-register
  "/" (conn-remap-key conn-undo-keys)
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "*" 'conn-org-edit-insert-heading
  "<" 'org-drag-element-backward
  ">" 'org-drag-element-forward
  "?" (conn-remap-key conn-undo-redo-keys)
  "f" 'conn-dispatch-on-things
  "C" 'org-toggle-comment
  "b" (conn-remap-key (key-parse "C-c C-v"))
  "c" (conn-remap-key (key-parse "C-c"))
  "r" (conn-remap-key (key-parse "C-c C-x"))
  "d" 'org-down-element
  "g" (conn-remap-keymap (key-parse "M-g"))
  "i" 'org-backward-heading-same-level
  "I" 'org-metaup
  "J" 'org-metaleft
  "j" 'org-previous-visible-heading
  "k" 'org-forward-heading-same-level
  "K" 'org-metadown
  "L" 'org-metaright
  "l" 'org-next-visible-heading
  "M" 'org-mark-subtree
  "m" 'org-forward-element
  "n" 'org-backward-element
  "N" 'org-toggle-narrow-to-subtree
  "O" 'org-next-block
  "p" 'conn-register-load
  "s" (conn-remap-keymap (key-parse "M-s"))
  "T" 'org-todo
  "t" 'org-sparse-tree
  "U" 'org-previous-block
  "u" 'org-up-element
  "W" 'widen
  "w" 'org-refile
  "x" (conn-remap-key (key-parse "C-x"))
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
           (not (save-excursion
                  (goto-char (region-beginning))
                  (search-forward "\n" (region-end) t))))
      (cons (buffer-substring-no-properties
             (region-beginning) (region-end))
            (apply app))
    (apply app)))

(defun conn--repeat-ad (&rest app)
  (unwind-protect
      (apply app)
    (setq conn-this-command-thing
          (conn--command-property :conn-command-thing)
          conn-this-command-handler
          (or (alist-get this-command conn-mark-handler-overrides-alist)
              (conn--command-property :conn-mark-handler)))))

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
        (advice-add 'repeat :around #'conn--repeat-ad)
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
    (advice-remove 'repeat #'conn--repeat-ad)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)))


;;;; Mode Definition

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
  :lighter (:eval conn-lighter)
  :group 'conn
  (conn--input-method-mode-line)
  (if conn-local-mode
      (progn
        (setq conn-current-state nil
              conn-previous-state nil)
        (setq-local conn-lighter (seq-copy conn-lighter)
                    conn--local-state-map (list (list 'conn-local-mode))
                    conn--local-override-map (list (list 'conn-local-mode))
                    conn--local-major-mode-map (list (list 'conn-local-mode)))
        (unless (mark t)
          (conn--push-ephemeral-mark (point) t nil))
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'isearch-mode-hook 'conn--isearch-input-method nil t)
        (setq conn--input-method current-input-method)
        (if-let* ((setup-fn
                   (alist-get (current-buffer) conn-buffer-state-setup-alist
                              nil nil #'buffer-match-p)))
            (funcall setup-fn)
          (conn-enter-state conn-state-for-emacs)))
    ;; conn-exit-state sets conn-current-state to nil before
    ;; anything else, so if we got here after an error in
    ;; conn-exit-state this prevents an infinite loop.
    (when conn-current-state
      (conn-exit-state conn-current-state))
    (conn--clear-overlays)
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
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50))
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
                                   "conn-define-dispatch-action"
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
    (conn--with-state (conn-enter-state 'conn-null-state)
      (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))


;;;; Corfu

(with-eval-after-load 'corfu
  (defun conn--exit-completion (_state)
    (completion-in-region-mode -1))
  (add-hook 'conn-exit-functions 'conn--exit-completion))


;;;; Org

(define-minor-mode conntext-org-mode
  "Conntext keys for org mode."
  :global t)

(with-eval-after-load 'org
  (defvar org-mode-map)
  (defvar org-link-any-re)
  (defvar org-outline-regexp)

  (declare-function org-backward-sentence "org")
  (declare-function org-element-type-p "org-element-ast")
  (declare-function org-forward-sentence "org")
  (declare-function org-element-contents-end "org-element")
  (declare-function org-element-parent "org-element-ast")
  (declare-function org-element-end "org-element")
  (declare-function org-element-at-point "org-element")
  (declare-function org-end-of-subtree "org")
  (declare-function org-at-heading-p "org")
  (declare-function org-with-limited-levels "org-macs")
  (declare-function org-in-regexp "org-macs")
  (declare-function org-backward-element "org")
  (declare-function org-mark-element "org")
  (declare-function org-agenda-remove-restriction-lock "org-agenda")
  (declare-function org-priority "org")
  (declare-function org-refile "org-refile")
  (declare-function org-speed-move-safe "org-keys")
  (declare-function org-entry-put "org")

  (conn-register-thing
   'org-link
   :dispatch-target-finder (lambda () (conn--dispatch-re-matches org-link-any-re t))
   :bounds-op (lambda () (org-in-regexp org-link-any-re)))

  (conn-register-thing
   'org-paragraph
   :dispatch-target-finder (lambda () (conn--dispatch-all-things 'org-paragraph t))
   :forward-op 'org-forward-paragraph)

  (conn-register-thing-commands
   'org-paragraph 'conn-continuous-thing-handler
   'org-forward-paragraph 'org-backward-paragraph)

  (conn-define-dispatch-action conn-open-org-link (window pt _thing-cmd _thing-arg)
    :description "Open Link"
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (org-open-at-point-global))))

  (setf (alist-get 'org-link conn-dispatch-default-action-alist)
        'conn-open-org-link)

  (defun conn-org-sentence-forward (arg)
    (interactive "p")
    (if (>= arg 0)
        (org-forward-sentence arg)
      (org-backward-sentence (abs arg))))

  (conn-register-thing
   'org-sentence
   :forward-op 'conn-org-sentence-forward)

  (conn-register-thing-commands
   'org-sentence 'conn-continuous-thing-handler
   'conn-org-sentence-forward
   'org-forward-sentence 'org-backward-sentence)

  (conn-register-thing
   'org-element
   :bounds-op (lambda ()
                (save-mark-and-excursion
                  (org-mark-element)
                  (cons (region-beginning) (region-end))))
   :beg-op 'org-backward-element
   :end-op 'org-forward-element)

  ;; FIXME: org-element all broken
  (conn-register-thing-commands
   'org-element
   (lambda (_beg)
     (cond ((eobp))
           ((org-with-limited-levels (org-at-heading-p))
            (conn--push-ephemeral-mark
             (save-excursion (org-end-of-subtree nil t))))
           (t
            (let* ((elem (org-element-at-point))
                   (end (org-element-end elem))
                   (parent (org-element-parent elem)))
              (cond ((and parent (= (org-element-contents-end parent) end))
                     (conn--push-ephemeral-mark (org-element-end parent)))
                    ((integer-or-marker-p end)
                     (conn--push-ephemeral-mark end)))))))
   'org-forward-element
   'org-backward-element
   'org-next-visible-heading
   'org-previous-visible-heading
   'org-forward-heading-same-level
   'org-backward-heading-same-level
   'org-up-element
   'org-up-heading)

  (conn-register-thing
   'org-heading
   :bounds-op (lambda () (bounds-of-thing-at-point 'org-element))
   :dispatch-target-finder (lambda () (conn--dispatch-all-things 'org-heading t))
   :forward-op 'org-next-visible-heading)

  (conn-register-thing-commands
   'org-heading 'conn-continuous-thing-handler
   'org-next-visible-heading
   'org-previous-visible-heading)

  (conn-register-thing-commands
   'org-element 'conn-discrete-thing-handler
   'org-forward-element
   'org-backward-element
   'org-next-visible-heading
   'org-previous-visible-heading
   'org-forward-heading-same-level
   'org-backward-heading-same-level
   'org-up-element
   'org-up-heading)

  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-movement-state 'org-mode)
    "=" 'conn-org-edit-state
    "^" 'org-up-element
    ")" 'org-next-visible-heading
    "(" 'org-previous-visible-heading
    "N" 'org-backward-element
    "M" 'org-forward-element
    "I" 'org-backward-paragraph
    "K" 'org-forward-paragraph)

  (defun conn-org-speed-next-heading ()
    (interactive)
    (org-speed-move-safe 'org-next-visible-heading))
  (defun conn-org-speed-previous-heading ()
    (interactive)
    (org-speed-move-safe 'org-previous-visible-heading))
  (defun conn-org-speed-forward-heading ()
    (interactive)
    (org-speed-move-safe 'org-forward-heading-same-level))
  (defun conn-org-speed-backward-heading ()
    (interactive)
    (org-speed-move-safe 'org-backward-heading-same-level))
  (defun conn-org-speed-up-heading ()
    (interactive)
    (org-speed-move-safe 'outline-up-heading))

  (conn-register-thing-commands
   'org-element 'conn-discrete-thing-handler
   'conn-org-speed-up-heading
   'conn-org-speed-next-heading
   'conn-org-speed-previous-heading
   'conn-org-speed-forward-heading
   'conn-org-speed-backward-heading
   'conn-org-speed-next-block
   'conn-org-speed-previous-block)

  (define-keymap
    :keymap (conn-get-mode-map 'conn-command-state 'conntext-org-mode)
    "TAB" (conntext-define conntext-org-edit-state
            (when (and (bolp) (looking-at org-outline-regexp))
              'conn-org-edit-state)))

  (define-keymap
    :keymap (conn-get-mode-map 'conn-emacs-state 'conntext-org-mode)
    "k" (conntext-define conntext-org-next-visible-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-speed-next-heading))
    "i" (conntext-define conntext-org-prev-visible-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-speed-previous-heading))
    "l" (conntext-define conntext-org-forward-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-speed-forward-heading))
    "j" (conntext-define conntext-org-backward-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-speed-backward-heading))
    "F" (conntext-define conntext-org-next-block
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-next-block))
    "B" (conntext-define conntext-org-prev-block
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-previous-block))
    "u" (conntext-define conntext-org-up-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-speed-up-heading))
    ;; "j" (conntext-define ""
    ;;       (when (and (bolp) (looking-at org-outline-regexp))
    ;;         'org-goto))
    "g" (conntext-define conntext-org-refile-goto
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-refile '(4)))))
    "c" (conntext-define conntext-org-edit-state
          (when (and (bolp) (looking-at org-outline-regexp))
            'conn-org-edit-state))
    "C" (conntext-define conntext-org-shifttab
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-shifttab))
    "SPC" (conntext-define conntext-org-outline-path
            (when (and (bolp) (looking-at org-outline-regexp))
              'org-display-outline-path))
    "s" (conntext-define conntext-org-narrow-subtree
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-toggle-narrow-to-subtree))
    "w" (conntext-define conntext-org-cut-subtree
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-cut-subtree))
    "=" (conntext-define conntext-org-columns
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-columns))
    "I" (conntext-define conntext-org-metaup
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-metaup))
    "K" (conntext-define conntext-org-metadown
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-metadown))
    "L" (conntext-define conntext-org-metaright
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-metaright))
    "J" (conntext-define conntext-org-metaleft
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-metaleft))
    "M-L" (conntext-define conntext-org-shiftmetaright
            (when (and (bolp) (looking-at org-outline-regexp))
              'org-shiftmetaright))
    "M-J" (conntext-define conntext-org-shiftmetaleft
            (when (and (bolp) (looking-at org-outline-regexp))
              'org-shiftmetaleft))
    "RET" (conntext-define conntext-org-insert-heading
            (when (and (bolp) (looking-at org-outline-regexp))
              (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content))))
    "^" (conntext-define conntext-org-sort
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-sort))
    "r" (conntext-define conntext-org-refile
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-refile))
    "a" (conntext-define conntext-org-archive-subtree
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-archive-subtree-default-with-confirmation))
    "@" (conntext-define conntext-org-mark-subtree
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-mark-subtree))
    "#" (conntext-define conntext-org-comment
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-toggle-comment))
    "p" (conntext-define conntext-org-clock-map
          (when (and (bolp) (looking-at org-outline-regexp))
            (define-keymap
              "i" 'org-clock-in
              "o" 'org-clock-out)))
    "t" (conntext-define conntext-org-todo
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-todo))
    "," (conntext-define conntext-org-priority
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-priority))))
    "0" (conntext-define conntext-org-priority-0
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-priority ?\ ))))
    "1" (conntext-define conntext-org-priority-1
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-priority ?A))))
    "2" (conntext-define conntext-org-priority-2
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-priority ?B))))
    "3" (conntext-define conntext-org-priority-3
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-priority ?C))))
    ":" (conntext-define conntext-org-set-tags
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-set-tags-command))
    "e" (conntext-define conntext-org-set-effort
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-set-effort))
    "E" (conntext-define conntext-org-inc-effor
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-inc-effort))
    "W" (conntext-define conntext-org-appt-warning
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda (m)
              (interactive "sMinutes before warning: ")
              (org-entry-put (point) "APPT_WARNTIME" m))))
    "v" (conntext-define conntext-org-agenda
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-agenda))
    "/" (conntext-define conntext-org-sparse-tree
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-sparse-tree))
    "o" (conntext-define conntext-org-open-at-point
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-open-at-point))
    "<" (conntext-define conntext-org-agenda-lock
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-agenda-set-restriction-lock 'subtree))))
    ">" (conntext-define conntext-org-agenda-unlock
          (when (and (bolp) (looking-at org-outline-regexp))
            (lambda ()
              (interactive)
              (org-agenda-remove-restriction-lock)))))
  (conn-set-mode-map-depth 'conn-emacs-state 'conntext-org-mode -81))

;; FIXME: figure this out
;; (with-eval-after-load 'polymode
;;   (defvar polymode-move-these-vars-from-old-buffer)
;;   (dolist (v '(conn--mark-cursor
;;                conn-current-state
;;                conn-command-state
;;                conn-emacs-state))
;;     (cl-pushnew v polymode-move-these-vars-from-old-buffer)))


;;;; Eldoc

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'conn-dispatch-on-things))


;;;; Edebug

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-enter-state 'conn-emacs-state)
      (when conn-previous-state
        (funcall conn-previous-state))))
  (add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state))


;;;; Outline

(define-minor-mode conntext-outline-mode
  "Minor mode for contextual bindings in outline-mode."
  :global t)

(with-eval-after-load 'outline
  (declare-function outline-mark-subtree "outline")
  (declare-function outline-next-heading "outline")
  (declare-function outline-previous-heading "outline")
  (declare-function outline-on-heading-p "outline")
  (declare-function outline-up-heading "outline")

  (conn-register-thing
   'heading
   :dispatch-target-finder (lambda () (conn--dispatch-all-things 'heading t))
   :bounds-op (lambda ()
                (save-mark-and-excursion
                  (outline-mark-subtree)
                  (cons (region-beginning) (region-end)))))

  (conn-register-thing-commands
   'heading 'conn-discrete-thing-handler
   'outline-up-heading
   'outline-next-heading
   'outline-next-visible-heading
   'outline-previous-visible-heading
   'outline-previous-heading
   'outline-forward-same-level
   'outline-backward-same-level)

  (defvar-keymap conntext-outline-map
    "/ h" 'outline-hide-by-heading-regexp
    "/ s" 'outline-show-by-heading-regexp
    "<" 'outline-promote
    ">" 'outline-demote
    "I" 'outline-move-subtree-up
    "K" 'outline-move-subtree-down
    "RET" 'outline-insert-heading
    "a" 'outline-show-all
    "c" 'outline-hide-entry
    "e" 'outline-show-entry
    "f" 'outline-hide-other
    "h" 'outline-hide-subtree
    "i" 'outline-previous-visible-heading
    "j" 'outline-backward-same-level
    "k" 'outline-next-visible-heading
    "b" 'outline-show-branches
    "l" 'outline-forward-same-level
    "v" 'outline-hide-leaves
    "m" 'outline-mark-subtree
    "o" 'outline-show-children
    "q" 'outline-hide-sublevels
    "s" 'outline-show-subtree
    "t" 'outline-hide-body
    "u" 'outline-up-heading)

  (defvar-keymap conntext-outline-edit-repeat-map
    :repeat t
    "<" 'outline-promote
    ">" 'outline-demote
    "I" 'outline-move-subtree-up
    "K" 'outline-move-subtree-down)

  (defvar-keymap conntext-outline-movement-repeat-map
    :repeat t
    "i" 'outline-previous-visible-heading
    "j" 'outline-backward-same-level
    "k" 'outline-next-visible-heading
    "l" 'outline-forward-same-level
    "u" 'outline-up-heading)

  (define-keymap
    :keymap (conn-get-mode-map 'conn-command-state 'conntext-outline-mode)
    "TAB" (conntext-define conntext-outline-map
            "Context outline map."
            (when (and (looking-at-p outline-regexp) (bolp))
              conntext-outline-map)))

  (conn-set-mode-map-depth 'conn-command-state 'conntext-outline-mode -80))


;;;; Dired

(conn-define-state conn-dired-dispatch-state (conn-emacs-state conn-read-dispatch-state)
  "State for dispatch in `dired-mode'."
  :cursor 'box
  :lighter " DISPATCH"
  :suppress-input-method t)

(defun conn-setup-dired-state ()
  (setq conn-state-for-read-dispatch 'conn-dired-dispatch-state)
  (conn-enter-state 'conn-emacs-state))

(with-eval-after-load 'dired
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
    (conn--protected-let ((dired-movement-style 'bounded)
                          (ovs nil (mapc #'delete-overlay ovs)))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (while (/= (point)
                     (progn
                       (dired-next-line 1)
                       (point)))
            (push (conn--make-target-overlay (point) 0) ovs))))
      ovs))

  (defun conn--dispatch-dired-dirline ()
    (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (while (/= (point)
                     (progn
                       (dired-next-dirline 1)
                       (point)))
            (push (conn--make-target-overlay (point) 0) ovs))))
      ovs))

  (defun conn--dispatch-dired-subdir ()
    (conn--protected-let ((start (window-start))
                          (end (window-end))
                          (ovs nil (mapc #'delete-overlay ovs)))
      (save-excursion
        (pcase-dolist (`(,_ . ,marker) dired-subdir-alist)
          (when (<= start marker end)
            (goto-char marker)
            (push (conn--make-target-overlay
                   (+ 2 marker) (- (line-end-position) marker 2))
                  ovs)))
        ovs)))

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

  (conn-define-dispatch-action conn-dispatch-dired-mark (window pt _thing-cmd _thing-arg)
    :description "Mark"
    :window-predicate (lambda (win)
                        (eq (buffer-local-value 'major-mode (window-buffer win))
                            'dired-mode))
    (with-selected-window window
      (save-excursion
        (let ((regexp (dired-marker-regexp)))
          (goto-char pt)
          (goto-char (line-beginning-position))
          (if (looking-at regexp)
              (dired-unmark 1)
            (dired-mark 1))))))

  (conn-define-dispatch-action conn-dispatch-dired-kill-line (window pt _thing-cmd _thing-arg)
    :description "Kill Line"
    :window-predicate (lambda (win)
                        (eq (buffer-local-value 'major-mode (window-buffer win))
                            'dired-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-line))))

  (conn-define-dispatch-action conn-dispatch-dired-kill-subdir (window pt _thing-cmd _thing-arg)
    :description "Kill Subdir"
    :window-predicate (lambda (win)
                        (eq (buffer-local-value 'major-mode (window-buffer win))
                            'dired-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-subdir))))

  (define-keymap
    :keymap (conn-get-state-map 'conn-dired-dispatch-state)
    "f" 'conn-dispatch-dired-mark
    "w" 'conn-dispatch-dired-kill-line
    "d" 'conn-dispatch-dired-kill-subdir)

  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'dired-mode)
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
    "s" (conn-remap-key (key-parse "M-s"))
    "r" (conn-remap-key (key-parse "%"))
    "," (conn-remap-key (key-parse "*"))
    "x" (conn-remap-key (key-parse "C-x"))
    "f" 'conn-dispatch-on-things
    "M-SPC" 'dired-toggle-marks
    "C-M-l" 'dired-do-redisplay
    "z" 'dired-goto-file
    ";" 'conn-wincontrol
    "`" 'other-window
    "SPC" 'scroll-up-command
    "DEL" 'scroll-down-command
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
    "M-s r" 'dired-do-find-regexp-and-replace))


;;;; Magit

(with-eval-after-load 'magit
  (conn-set-mode-property 'magit-section-mode :hide-mark-cursor t)

  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'magit-section-mode)
    "h" 'conn-wincontrol-one-command
    "," 'magit-dispatch
    "<f8>" 'conn-command-state
    "i" 'magit-section-backward
    "k" 'magit-section-forward
    "w" 'magit-delete-thing
    "p" 'magit-reset-quickly
    "n" 'magit-gitignore
    "`" 'other-window
    "@" 'magit-am
    "x" (conn-remap-key (key-parse "C-x"))))


;;;; Ibuffer

(conn-define-state conn-ibuffer-dispatch-state (conn-emacs-state conn-read-dispatch-state)
  "State for dispatch in `ibuffer-mode'."
  :cursor '(bar . 4)
  :lighter " DISPATCH"
  :hide-mark-cursor t
  :suppress-input-method t)

(with-eval-after-load 'ibuffer
  (conn-set-mode-property 'ibuffer-mode :hide-mark-cursor t)

  (defvar ibuffer-movement-cycle)
  (defvar ibuffer-marked-char)

  (declare-function ibuffer-backward-line "ibuffer")
  (declare-function ibuffer-unmark-forward "ibuffer")
  (declare-function ibuffer-mark-forward "ibuffer")
  (declare-function ibuffer-current-mark "ibuffer")
  (declare-function ibuffer-backward-filter-group "ibuffer")

  (defun conn--dispatch-ibuffer-lines ()
    (conn--protected-let ((ibuffer-movement-cycle nil)
                          (ovs nil (mapc #'delete-overlay ovs)))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-max))
          (while (/= (point)
                     (progn
                       (ibuffer-backward-line)
                       (point)))
            (unless (get-text-property (point) 'ibuffer-filter-group-name)
              (push (conn--make-target-overlay (point) 0) ovs))))
        ovs)))

  (defun conn--dispatch-ibuffer-filter-group ()
    (conn--protected-let ((ibuffer-movement-cycle nil)
                          (ovs nil (mapc #'delete-overlay ovs)))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-max))
          (while (/= (point)
                     (progn
                       (ibuffer-backward-filter-group)
                       (point)))
            (push (conn--make-target-overlay (point) 0) ovs)))
        ovs)))

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

  (conn-define-dispatch-action conn-dispatch-ibuffer-mark (window pt _thing-cmd _thing-arg)
    :description "Mark"
    :window-predicate (lambda (win)
                        (eq (buffer-local-value 'major-mode (window-buffer win))
                            'ibuffer-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (if (or (null (ibuffer-current-mark))
                (= (ibuffer-current-mark) ? ))
            (ibuffer-mark-forward nil nil 1)
          (ibuffer-unmark-forward nil nil 1)))))

  (keymap-set (conn-get-state-map 'conn-ibuffer-dispatch-state)
              "f" 'conn-dispatch-ibuffer-mark)

  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'ibuffer-mode)
    "h" 'conn-wincontrol-one-command
    "a" 'execute-extended-command
    ";" 'conn-wincontrol
    "/" 'ibuffer-do-revert
    "`" 'other-window
    "y" 'ibuffer-yank
    "z" 'ibuffer-jump-to-buffer
    "r" (conn-remap-key (key-parse "%"))
    "," (conn-remap-key (key-parse "*"))
    "l" 'ibuffer-forward-filter-group
    "j" 'ibuffer-backward-filter-group
    "m" 'ibuffer-jump-to-filter-group
    "n" 'conn-ibuffer-filter-prefix
    "f" 'conn-dispatch-on-things
    "k" 'ibuffer-forward-line
    "i" 'ibuffer-backward-line
    "w" 'ibuffer-do-kill-lines
    "u" 'ibuffer-do-kill-on-deletion-marks
    "x" (conn-remap-key (key-parse "C-x"))
    "s" (conn-remap-key (key-parse "M-s"))
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
    "SPC" 'scroll-up-command
    "DEL" 'scroll-down-command
    "v" 'ibuffer-mark-forward
    "c" 'ibuffer-unmark-forward
    "C" 'ibuffer-unmark-backward
    "o" 'ibuffer-visit-buffer-other-window
    "RET" 'ibuffer-visit-buffer))


;;;; Markdown

(with-eval-after-load 'markdown-mode
  (conn-register-thing
   'md-paragraph
   :forward-op 'markdown-forward-paragraph
   :modes '(markdown-mode))

  (conn-register-thing-commands
   'md-paragraph 'conn-continuous-thing-handler
   'markdown-forward-paragraph
   'markdown-backward-paragraph)

  ;; TODO: other markdown things
  )


;;;; Treesit

(static-if (<= 30 emacs-major-version)
    (with-eval-after-load 'treesit
      (conn-register-thing-commands
       'defun 'conn-continuous-thing-handler
       'treesit-end-of-defun
       'treesit-beginning-of-defun)))


;;;; Help

(with-eval-after-load 'help-mode
  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'help-mode)
    "h" 'conn-wincontrol-one-command
    "a" 'execute-extended-command
    "b" 'beginning-of-buffer
    "e" 'end-of-buffer
    "j" 'backward-button
    "l" 'forward-button
    "i" 'scroll-down
    "k" 'scroll-up
    "f" 'conn-dispatch-on-buttons
    "`" 'other-window
    ";" 'conn-wincontrol
    "x" (conn-remap-key (key-parse "C-x"))))

(with-eval-after-load 'helpful
  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'helpful-mode)
    "h" 'conn-wincontrol-one-command
    "a" 'execute-extended-command
    "b" 'beginning-of-buffer
    "e" 'end-of-buffer
    "j" 'backward-button
    "l" 'forward-button
    "i" 'scroll-down
    "k" 'scroll-up
    "f" 'conn-dispatch-on-buttons
    "`" 'other-window
    ";" 'conn-wincontrol
    "x" (conn-remap-key (key-parse "C-x"))))


;;;; Info

(with-eval-after-load 'info
  (declare-function Info-prev-reference "info")
  (declare-function Info-follow-nearest-node "info")

  (defun dispatch-on-info-refs ()
    (interactive)
    (conn-dispatch-on-things
     nil nil
     (lambda ()
       (conn--protected-let ((ovs nil (mapc #'delete-overlay ovs)))
         (cl-loop for win in (conn--get-dispatch-windows t)
                  do (with-selected-window win
                       (save-excursion
                         (let ((last-pt (goto-char (window-end))))
                           (while (and (> last-pt (progn
                                                    (Info-prev-reference)
                                                    (setq last-pt (point))))
                                       (<= (window-start) (point) (window-end)))
                             (push (conn--make-target-overlay (point) 0 nil) ovs)))))
                  finally return ovs)))
     (lambda (win pt _thing _thing-arg)
       (select-window win)
       (goto-char pt)
       (Info-follow-nearest-node))
     nil
     (lambda (win)
       (eq 'Info-mode (buffer-local-value 'major-mode (window-buffer win))))))

  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'Info-mode)
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
    "`" 'other-window
    ";" 'conn-wincontrol
    "x" (conn-remap-key (key-parse "C-x"))))


;;;; treemacs

(with-eval-after-load 'treemacs
  (conn-set-mode-property 'treemacs-mode :hide-mark-cursor t)
  (define-keymap
    :keymap (conn-get-major-mode-map 'conn-emacs-state 'treemacs-mode)
    "h" 'conn-wincontrol-one-command
    "a" 'execute-extended-command
    "`" 'treemacs-select-window
    "i" 'treemacs-previous-line
    "k" 'treemacs-next-line
    "f" 'conn-dispatch-on-things
    ";" 'conn-wincontrol
    "x" (conn-remap-key (key-parse "C-x"))))


;;;; Messages

(conn-set-mode-property 'messages-buffer-mode :hide-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'messages-buffer-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "`" 'other-window
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch-on-things
  ";" 'conn-wincontrol
  "x" (conn-remap-key (key-parse "C-x")))


;;; Footer
;; Local Variables:
;; outline-regexp: "^;;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn.el ends here
