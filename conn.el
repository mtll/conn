;;; conn.el --- Region oriented modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode and keyboard macro enhancement
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") (transient "0.6.0") (seq "2.24"))
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

;; A region oriented modal keybinding mode.

;;; Code:

;;;; Requires

(require 'compat)
(require 'subr-x)
(eval-when-compile
  (require 'eieio)
  (require 'cl-lib)
  (require 'map))

(declare-function extract-rectangle-bounds "rect")
(declare-function kmacro-p "kmacro")
(declare-function kmacro-step-edit-macro "kmacro")
(declare-function project-files "project")


;;;; Variables

;;;;; Declerations

(defvar conn-mode nil)
(defvar conn-local-mode)
(defvar conn-state)
(defvar conn-emacs-state)
(defvar conn-org-edit-state)
(defvar conn-emacs-state)
(defvar conn-state-map)
(defvar conn-wincontrol-mode)

(defvar-local conn--hide-mark-cursor nil)

(defvar conn-dispatch-target-finder-default 'conn--dispatch-chars
  "Default target finder for dispatch.

A target finder function should return a list of overlays.")

(defvar conn-dispatch-target-finders-alist
  `((conn-end-of-inner-line . conn--dispatch-inner-lines-end)
    (move-end-of-line . conn--dispatch-lines-end)
    (conn-backward-symbol . ,(apply-partially 'conn--dispatch-all-things 'symbol t))
    (backward-word . ,(apply-partially 'conn--dispatch-all-things 'word t)))
  "Default target finders for for things or commands.

Is an alist of the form (((or THING CMD) . TARGET-FINDER) ...).  When
determining the target finder for a command in
`conn-dispatch-read-thing-mode', which see, actions associated with a
command have higher precedence than actions associated with a thing.

For the meaning of TARGET-FINDER see
`conn-dispatch-target-finder-default'.")

(defvar conn-dispatch-action-default 'conn-dispatch-goto
  "Default action function for `conn-dispatch-on-things'.

For the meaning of action function see `conn-define-dispatch-action'.")

(defvar conn-dispatch-default-action-alist nil
  "Default action functions for things or commands.

Is an alist of the form (((or THING CMD) . ACTION) ...).  When
determining the default action for a command in
`conn-dispatch-read-thing-mode', which see, actions associated with a
command have higher precedence than actions associated with a command's
thing.

For the meaning of ACTION see `conn-define-dispatch-action'.")

(defvar-local conn-mark-handler-overrides-alist nil
  "Buffer local overrides for command mark handlers.

Is an alist of the form ((CMD . MARK-HANDLER) ...).

For the meaning of MARK-HANDLER see `conn-get-mark-handler'.")

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
  "H" 'conn-contract
  "h" 'conn-expand)

(defvar-keymap conn-mark-thing-map
  :prefix 'conn-mark-thing-map
  "L" 'forward-line
  ")" 'forward-list
  "(" 'backward-list)

;;;;; Custom Variables

(defgroup conn nil
  "A region oriented modal keybinding mode."
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
  "Modeline lighter for conn-mode."
  :type '(choice string (const nil))
  :group 'conn-states)

(defcustom conn-default-state 'conn-emacs-state
  "Default conn state for new buffers."
  :type 'symbol
  :group 'conn-states)

(defcustom conn-buffer-default-state-alist
  '(("COMMIT_EDITMSG" . conn-emacs-state)
    ((or (derived-mode . prog-mode)
         (derived-mode . text-mode)
         (derived-mode . conf-mode))
     . conn-state)
    ((or (derived-mode . calc-mode)
         (derived-mode . calc-trail-mode)
         (derived-mode . minibuffer-mode)
         (derived-mode . calc-keypad-mode)
         (derived-mode . image-mode)
         (derived-mode . doc-view-mode)
         (derived-mode . pdf-view-mode)
         (derived-mode . magit-status-mode))
     . conn-null-state))
  "Alist of the form ((CONDITION . STATE) ...).

Elements specify default STATE for buffers matching CONDITION.
For the meaning of CONDITION see `buffer-match-p'."
  :type '(list (cons string symbol))
  :group 'conn-states)

(defface conn-mark-face
  '((default (:inherit cursor :background "#b8a2f0"))
    (((background light)) (:inherit cursor :background "#b8a2f0"))
    (((background dark)) (:inherit cursor :background "#a742b0")))
  "Face for conn mark cursor."
  :group 'conn-faces)

(defface conn-window-prompt-face
  '((default (:height 2.5 :foreground "#d00000"))
    (((background light)) (:height 2.5 :foreground "#d00000"))
    (((background dark)) (:height 2.5 :foreground "#7c0000")))
  "Face for conn window prompt overlay."
  :group 'conn-faces)

(defface conn-read-string-match-face
  '((t (:inherit isearch)))
  "Face for matches when reading strings."
  :group 'conn-faces)

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :group 'conn)

(defcustom conn-read-string-timeout 0.5
  "Timeout for string reading functions."
  :group 'conn
  :type 'number)

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e" "r"
        "t" "y" "u" "i" "o" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defcustom conn-window-label-characters
  (list "j" "k" "l" "u" "i" "o" "d" "s" "g" "h" "w"
        "e" "r" "t" "y" "c" "v" "b" "n" "f" "m")
  "Chars to use for window label overlays."
  :group 'conn
  :type '(list integer))

(defface conn-dispatch-label-face
  '((t (:background "#ff8bd1" :foreground "black" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defcustom conn-dispatch-thing-ignored-modes
  (list 'image-mode 'doc-view-mode 'pdf-view-mode)
  "List of modes to ignore when searching for dispatch candidates."
  :group 'conn
  :type '(list symbol))

;;;;; State Variables

(defvar conn-states nil
  "All defined conn states.")

(defvar conn--state-objects nil
  "State object plist.")

(defvar conn-exit-functions nil
  "Abnormal hook run when a state is exited.

Each function is passed the state being exited
(eg '`conn-state' or '`conn-emacs-state').

See also `conn-entry-functions'.")

(defvar conn-entry-functions nil
  "Abnormal hook run when a state is entered.

Each function is passed the state being entered
(eg '`conn-state' or '`conn-emacs-state').

See also `conn-exit-functions'.")

(defvar-local conn--input-method nil
  "Input method for current buffer.")
(put 'conn--input-method 'permanent-local t)

(defvar-local conn--input-method-title nil
  "Title string of the current input method shown in mode line.")
(put 'conn--input-method-title 'permanent-local t)

(defvar-local conn--prev-mode-line-mule-info nil)
(put 'conn--prev-mode-line-mule-info 'risky-local-variable t)

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar-local conn-previous-state nil
  "Previous conn state in buffer.")

;;;;;; State Keymaps

(defvar conn--state-maps nil)

(defvar-local conn--local-maps nil)

(defvar-local conn--local-major-mode-maps nil)

(defvar conn--major-mode-maps nil)

(defvar conn--minor-mode-maps nil)

(defvar-local conn--local-minor-mode-maps nil)

;;;;; Mark Variables

(defvar conn-this-command-handler nil
  "Mark handler for current command.

Commands may set this variable if they need to change their handler
dynamically.")

(defvar conn-this-command-thing nil
  "`this-command''s thing.")

(defvar conn-this-command-start (make-marker)
  "Start position for current mark movement command.")

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.

Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--ephemeral-mark nil)

(defvar conn--saved-ephemeral-marks nil)

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

;;;;; Overlay Category Properties

;;;;;; Dots

(put 'conn--dot-overlay 'priority (cons nil conn-mark-overlay-priority))
(put 'conn--dot-overlay 'conn-overlay t)
(put 'conn--dot-overlay 'evaporate t)

;;;;;; Kapply Preview

(put 'kapply-preview 'priority 1900)
(put 'kapply-preview 'face 'match)

;;;;;; Mark Cursor

(put 'conn--mark-cursor 'permanent-local t)
(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)
(put 'conn--mark-cursor 'conn-overlay t)

;;;;;; Read String Overlays

(put 'conn-read-string-match 'conn-overlay t)
(put 'conn-read-string-match 'priority (+ 2 conn-mark-overlay-priority))

;;;;;; Label Overlay

(put 'conn-label-overlay 'priority 3000)
(put 'conn-label-overlay 'conn-overlay t)

;;;;; Command Histories

(defvar conn--seperator-history nil
  "History var for `conn-set-register-seperator'.")


;;;; Utilities

(eval-and-compile
  (defmacro conn--thread (form needle &rest forms)
    (declare (debug (form symbolp body))
             (indent 2))
    (if forms
        `(let ((,needle ,form))
           (conn--thread ,(car forms) ,needle ,@(cdr forms)))
      form))

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

(defmacro conn--with-advice (advice-forms &rest body)
  "Run BODY with ADVICE-FORMS temporarily applied.

ADVICE-FORMS are of the form (SYMBOL HOW FUNCTION . PROPS), for the
meaning of these see `advice-add'."
  (declare (debug (form body))
           (indent 1))
  (pcase-dolist (`(,symbol ,how ,function . ,props) (reverse advice-forms))
    (setq body (cl-with-gensyms (fn)
                 `(let ((,fn ,function))
                    (advice-add ,symbol ,how ,fn ,(car props))
                    (unwind-protect
                        ,(macroexp-progn body)
                      (advice-remove ,symbol ,fn))))))
  body)

(defmacro conn--without-conn-maps (&rest body)
  "Run BODY without any state, mode, or local maps active.

`conn-local-mode-map' and `conn-global-map' will still be active."
  (declare (debug (body))
           (indent 0))
  `(let ((emulation-mode-map-alists
          (seq-difference emulation-mode-map-alists
                          '(conn--local-minor-mode-maps
                            conn--local-major-mode-maps
                            conn--local-maps
                            conn--state-maps)
                          #'eq)))
     ,(macroexp-progn body)))

(defmacro conn--with-region-emphasis (regions &rest body)
  "Run BODY with the text in the complement of REGIONS shadowed."
  (declare (debug (form form body))
           (indent 2))
  (cl-with-gensyms (overlays)
    `(let (,overlays)
       (unwind-protect
           (progn
             (cl-loop for (beg end) on (append (list (point-min))
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

(defun conn-remap-key (from-keys)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  `(menu-item
    ,(format "Remap %s" (key-description from-keys))
    ,(conn--without-conn-maps (key-binding from-keys t))
    :filter ,(lambda (_real-binding)
               (conn--without-conn-maps
                 (key-binding from-keys t)))))

(defun conn-remap-keymap (from-keys)
  "Map to the keymap at FROM-KEYS.

If the binding at FROM-KEYS is for any reason not a keymap, say because
a minor mode has shadowed the keymap originally bound there, then map to
the original binding.  Also see `conn-remap-key'."
  `(menu-item
    ,(format "Remap %s Keymap" (key-description from-keys))
    ,(conn--without-conn-maps (key-binding from-keys t))
    :filter ,(lambda (real-binding)
               (conn--without-conn-maps
                 (let ((binding (key-binding from-keys t)))
                   (if (keymapp binding) binding real-binding))))))

;; From repeat-mode
(defun conn--command-property (propname)
  "Return the value of the current commands PROPNAME property."
  (or (and (symbolp this-command)
           (get this-command propname))
      (and (symbolp real-this-command)
           (get real-this-command propname))))

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

(defun conn--nnearest-first (list &optional buffer)
  "Move the region nearest point in LIST to the front.

LIST is either a list of markers or points, or of the form
((BEG . END) ...) where BEG and END are either markers or
points.

If BUFFER is non-nil find region nearest to point in BUFFER, else find
nearest in the current buffer.

This function destructively modifies LIST."
  (let* ((in-buffer (seq-filter
                     (pcase-lambda ((or `(,beg . ,_end) beg))
                       (or (not (markerp beg))
                           (eq (marker-buffer beg)
                               (or buffer (current-buffer)))))
                     list)))
    (if (null in-buffer)
        list
      (cl-loop with min = (car in-buffer)
               with min-dist = (pcase min
                                 (`(,beg . ,end)
                                  (min (abs (- (point) beg))
                                       (abs (- (point) end))))
                                 (pt (abs (- (point) pt))))
               for region in (cdr in-buffer)
               for new-dist = (pcase region
                                (`(,beg . ,end)
                                 (min (abs (- (point) beg))
                                      (abs (- (point) end))))
                                (pt (abs (- (point) pt))))
               when (< new-dist min-dist)
               do (setq min region
                        min-dist new-dist)
               finally return (cons min (delq min list))))))

(defun conn--create-marker (pos &optional buffer insertion-type)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    (set-marker-insertion-type marker insertion-type)
    marker))

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

(declare-function conn--derived-mode-all-parents "conn.el")
(if (version< "30" emacs-version)
    (defalias 'conn--derived-mode-all-parents 'derived-mode-all-parents)
  (defun conn--derived-mode-all-parents (mode)
    (let ((modes (list mode)))
      (while-let ((parent (get mode 'derived-mode-parent)))
        (push parent modes)
        (setq mode parent))
      (nreverse modes))))

(defun conn--derived-mode-property (property &optional buffer)
  "Check major mode in BUFFER and each `derived-mode-parent' for PROPERTY.
If BUFFER is nil check `current-buffer'."
  (cl-loop for mode in (thread-first
                         'major-mode
                         (buffer-local-value (or buffer (current-buffer)))
                         (conn--derived-mode-all-parents))
           for prop = (get mode property)
           when prop return prop))

(defun conn--merge-regions (regions &optional points)
  "Merge all overlapping regions in REGIONS."
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

(defmacro conn--with-state (transition-fn &rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms ( saved-state saved-prev-state
                     saved-cursor-type buffer)
    (cl-once-only (transition-fn)
      `(let ((,saved-state conn-current-state)
             (,saved-prev-state conn-previous-state)
             (,buffer (current-buffer))
             (,saved-cursor-type cursor-type))
         (unwind-protect
             (progn
               (cond (,transition-fn
                      (funcall ,transition-fn))
                     (conn-current-state
                      (conn-exit-state conn-current-state)))
               ,@body)
           (with-current-buffer ,buffer
             (if ,saved-state
                 (conn-enter-state ,saved-state)
               (when conn-current-state
                 (conn-exit-state conn-current-state))
               (setq cursor-type ,saved-cursor-type))
             (setq conn-previous-state ,saved-prev-state)))))))

(defmacro conn--with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  `(unwind-protect
       (progn
         (when conn--input-method
           (let ((input-method-activate-hook
                  (remove 'conn--activate-input-method
                          input-method-activate-hook)))
             (activate-input-method conn--input-method)))
         ,@body)
     (conn--activate-input-method)))

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

(defun conn--region-visible-p (beg end)
  "Return t if the region from BEG to END is visible."
  (and (not (invisible-p beg))
       (cl-loop for pt = (next-single-char-property-change
                          beg 'invisible nil end)
                while (and pt (< pt end))
                never (invisible-p pt))))

(defun conn--string-no-upper-case-p (string)
  "Return t if STRING contains no upper case characters."
  (cl-loop for char across string always (eq char (downcase char))))

(defun conn--visible-matches (string &optional dir)
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
          (when (conn--region-visible-p (match-beginning 0) (match-end 0))
            (push (match-beginning 0) matches)))
        (nreverse matches)))))

(defun conn--read-from-with-preview (prompt bounds &optional regexp-flag)
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

(defun conn-overlay-p (overlay)
  "Return t if OVERLAY is an overlay created by conn."
  (overlay-get overlay 'conn-overlay))

(defun conn--clear-overlays (&optional buffer)
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (mapc #'delete-overlay (conn--all-overlays #'conn-overlay-p nil nil buffer))))

(defun conn--all-overlays (predicate &optional start end buffer)
  "Get all overlays between START and END satisfying PREDICATE."
  (with-current-buffer (or buffer (current-buffer))
    (cl-loop for ov in (overlays-in (or start (if (use-region-p)
                                                  (region-beginning)
                                                (point-min)))
                                    (or end (if (use-region-p)
                                                (region-end)
                                              (point-max))))
             when (funcall predicate ov) collect ov)))


;;;; States

(defvar conn--state-slot-unbound (make-symbol "slot-unbound")
  "Uninterned symbol to mark an unbound state slot.")

(defun conn--setup-major-mode-maps ()
  "Setup the mode specific keymaps for the current major mode."
  (setq-local conn--local-major-mode-maps nil)
  (let* ((mmodes (if (get major-mode :conn-inhibit-inherit-maps)
                     (list major-mode)
                   (reverse (conn--derived-mode-all-parents major-mode))))
         mark-map-keys mode-map mode-mark-map)
    (dolist (state conn-states)
      (setq mark-map-keys
            (where-is-internal 'conn-mark-thing-map
                               (list (alist-get state conn--state-maps)
                                     (conn-get-local-map state))))
      (dolist (mode mmodes)
        (setq mode-map (conn-get-mode-map state mode)
              mode-mark-map (make-sparse-keymap))
        (let ((mark-map (conn-get-mode-things-map mode)))
          (when (cdr mark-map)
            (dolist (key mark-map-keys)
              (define-key mode-mark-map key mark-map))))
        (push (cons state (make-composed-keymap mode-mark-map mode-map))
              conn--local-major-mode-maps)))))

(defun conn-set-derived-mode-inherit-maps (mode inhibit-inherit-maps)
  "Set whether derived MODE inherits `conn-get-mode-map' keymaps from parents.
If INHIBIT-INHERIT-MAPS is non-nil then any maps defined using
`conn-get-mode-map' for parents of MODE will not be made active
when MODE is."
  (put mode :conn-inhibit-inherit-maps inhibit-inherit-maps))

(defun conn-get-mode-things-map (mode)
  "Get MODE keymap for STATE things.
If one does not exists assign a new sparse keymap for MODE things
in STATE and return it."
  (or (get mode :conn-mode-things)
      (put mode :conn-mode-things (make-sparse-keymap))))

(defun conn-get-state-map (state)
  (or (alist-get state conn--state-maps)
      (setf (alist-get state conn--state-maps)
            (make-sparse-keymap))))

(defun conn-get-mode-map (state mode)
  "Get MODE keymap for STATE.
If one does not exists assign a new sparse keymap for MODE
in STATE and return it."
  (or (alist-get mode (alist-get state conn--minor-mode-maps))
      (cond
       ((autoloadp (symbol-function mode))
        ;; We can't tell what sort of mode this is, add it to
        ;; `conn--minor-mode-maps' and setup a `with-eval-after-load' to
        ;; handle the case where it was actually a major-mode.
        (prog1
            (setf (alist-get mode (alist-get state conn--minor-mode-maps))
                  (define-keymap
                    :parent (make-composed-keymap
                             (cl-loop for parent in (conn-state-get state 'parents)
                                      collect (conn-get-mode-map parent mode)))))
          (with-eval-after-load (cadr (symbol-function mode))
            (when (get mode 'derived-mode-parent)
              (setf (alist-get mode (alist-get state conn--major-mode-maps))
                    (alist-get mode (alist-get state conn--minor-mode-maps)))
              (setf (alist-get mode (alist-get state conn--minor-mode-maps)) nil)
              (setf (alist-get state conn--minor-mode-maps)
                    (seq-remove (pcase-lambda (`(,m . ,_)) (eq m mode))
                                (alist-get state conn--minor-mode-maps)))))))
       ((get mode 'derived-mode-parent)
        (setf (alist-get mode (alist-get state conn--major-mode-maps))
              (define-keymap
                :parent (make-composed-keymap
                         (cl-loop for parent in (conn-state-get state 'parents)
                                  collect (conn-get-mode-map parent mode))))))
       (t
        (setf (alist-get mode (alist-get state conn--minor-mode-maps))
              (define-keymap
                :parent (make-composed-keymap
                         (cl-loop for parent in (conn-state-get state 'parents)
                                  collect (conn-get-mode-map parent mode)))))))))

(defun conn-get-local-map (state)
  "Get local keymap for STATE in current buffer.
If one does not exists assign a new sparse keymap for STATE
and return it."
  (or (alist-get state conn--local-maps)
      (setf (alist-get state conn--local-maps)
            (define-keymap
              :parent (make-composed-keymap
                       (cl-loop for parent in (conn-state-get state 'parents)
                                collect (conn-get-local-map parent)))))))

(defmacro conn--without-input-method-hooks (&rest body)
  "Run body without conn input method hooks active."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
         (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
         ,@body)
     (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
     (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)))

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property."
  ;; Ensure conn-local-mode is t since this can be run by conn--with-state
  ;; in buffers without conn-local-mode enabled.
  (when conn-local-mode
    (let (input-method-activate-hook
          input-method-deactivate-hook)
      (pcase (conn-state-get conn-current-state 'suppress-input-method)
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

(defun conn--toggle-input-method-ad (&rest app)
  (if (or (not conn-local-mode)
          (conn-state-get conn-current-state 'suppress-input-method))
      (apply app)
    (let ((current-input-method conn--input-method))
      (apply app))))

(defun conn--isearch-input-method ()
  "Ensure input method is enabled in isearch-mode."
  (when (and conn--input-method isearch-mode)
    (conn--without-input-method-hooks
      (let ((overriding-terminal-local-map nil))
        (activate-input-method conn--input-method))
      (setq isearch-input-method-function input-method-function)
      (setq-local input-method-function nil)
      (isearch-update))))
(put 'conn--isearch-input-method 'permanent-local-hook t)

(defun conn--input-method-mode-line ()
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

(defun conn--default-state-for-buffer (&optional buffer)
  "Get default state for BUFFER."
  (or (alist-get (or buffer (current-buffer))
                 conn-buffer-default-state-alist
                 nil nil #'buffer-match-p)
      conn-default-state))

(defsubst conn--get-state-object (state)
  (plist-get conn--state-objects state))

(gv-define-setter conn--get-state-object (value state)
  `(setf (plist-get conn--state-objects ,state) ,value))

(defvar conn--state-all-parents-cache nil)

(defun conn--state-all-parents (state)
  (with-memoization
      (plist-get conn--state-all-parents-cache state)
    (let ((queue (list (conn--get-state-object state)))
          (ancestory nil))
      (while
          (conn--thread
              (car (push (pop queue) ancestory))
              ancestors
            (cl-struct-slot-value (cl-type-of ancestors) 'parents ancestors)
            (mapcar #'conn--get-state-object ancestors)
            (seq-difference ancestors ancestory)
            (setq queue (append queue ancestors))))
      (nreverse ancestory))))

(defun conn-state-get (state slot)
  "Return the value in STATE of SLOT."
  (cl-loop for s in (conn--state-all-parents state)
           for val = (condition-case _
                         (cl-struct-slot-value (cl-type-of s) slot s)
                       (cl-struct-unknown-slot 'conn--slot-undefined))
           unless (or (eq val conn--state-slot-unbound)
                      (eq val 'conn--slot-undefined))
           return val))

(gv-define-setter conn-state-get (value state slot)
  `(conn-state-set ,state ,slot ,value))

(defsubst conn-state-set (state slot value)
  "Set the value of SLOT in STATE to VALUE.

Returns VALUE."
  (setf (cl-struct-slot-value state (conn--get-state-object state) slot)
        value))

(defsubst conn-state-unset (state slot)
  "Make SLOT unbound in STATE.

If a slot is unbound in a state it will inherit the value of that slot
from its parents."
  (setf (cl-struct-slot-value state (conn--get-state-object state) slot)
        conn--state-slot-unbound))

(cl-generic-define-generalizer conn--substate-generalizer
  95 ;; Make sure this is higher than derived-mode.  Why?  I don't know.
  (lambda (state)
    `(and (memq conn-states ,state) ,state))
  (lambda (state)
    (mapcar (lambda (state-obj) `(conn-substate ,(cl-type-of state-obj)))
            (conn--state-all-parents state))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-substate)))
  "Support for (conn-substate STATE) specializers."
  (list conn--substate-generalizer))

(cl-defgeneric conn-enter-state (state)
  "Enter conn state STATE.

This function takes care of calling `conn-exit-state' on the current
state."
  (:method ((_state (eql nil))) "Noop" nil)
  (:method (state) (error "Attempting to enter unknown state: %s" state)))

(cl-defgeneric conn-exit-state (state)
  "Exit conn state STATE."
  (:method ((_state (eql nil))) "Noop" nil)
  (:method (state) (error "Attempting to exit unknown state: %s" state)))

(cl-defgeneric conn-state-body (state)
  "Body function for STATE.

This function is run each time a state is entered or exited.  STATE is
the symbol naming the state entered or exited and it is bound to t or
nil respectively.

This function can be specialized on substates so that one can write
methods for a state that run whenever that state or one of its children
is entered or exited.  To do this write a method of the form:

(cl-defmethod conn-state-body ((state (conn-substate PARENT-STATE)))
  ...)"
  (:method (_state) "Noop" nil))

(defmacro conn-define-state (name doc &rest rest)
  "Define a conn state NAME.

Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.

NAME may be either a symbol or of the form (NAME OPTIONS...), where
options is (KEYWORD VALUE) and where KEYWORD is one of :keymap or
:inherit.

:KEYMAP is the state keymap for NAME.  KEYMAP is copied before being
assigned to the state.  To access the state keymap use either the NAME-map
variable or `conn-get-state-map'.

:INHERIT is a list of states from which NAME should inherit slots and
keymaps.

Keyword arguments define state slots.  The following slots have a
special meaning:

:LIGHTER is the mode-line lighter text for NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in
NAME.

:CURSOR is the `cursor-type' in NAME.

BODY contains code to be executed each time the state is enabled or
disabled.  When BODY is present it will become the body of a method of
`conn-state-body' specialized on NAME.  Conn also provides a
conn-substate specializer so that one may write methods for
`conn-state-body' specialized on a state and its substates.  To do this
write a method of the form:

(cl-defmethod conn-state-body ((state (conn-substate STATE-NAME)))
  ...)

\(fn NAME DOC [KEYWORD VAL ... &rest BODY])"
  (declare (debug ( name string-or-null-p
                    [&rest keywordp sexp]
                    def-body))
           (indent defun))
  (unless (stringp doc)
    (setq doc (format "Enter %S" (or (car-safe name) name))
          rest (cons doc rest)))
  (pcase-let* (((or (and name (pred symbolp))
                    `(,name . ,props))
                name)
               ((map :keymap :inherit) props)
               (keymap (car keymap))
               (keymap-name (conn--symbolicate name "-map"))
               (ctor (conn--symbolicate name "--make-state-object"))
               (`(,slots . ,body)
                (cl-loop for sublist on rest by #'cddr
                         if (not (keywordp (car sublist)))
                         return (cons slots sublist)
                         else
                         collect (list (intern (substring (symbol-name (car sublist)) 1))
                                       (cadr sublist))
                         into slots
                         finally return (cons slots nil))))
    `(progn
       (cl-defstruct (,name
                      (:constructor nil)
                      (:copier nil)
                      (:constructor ,ctor))
         (parents ',inherit :read-only t :type (list symbol))
         ,@slots)

       ;; Invalidate parents cache, we could have changed the
       ;; inheritance hierarchy out from under any child states.
       (setf conn--state-all-parents-cache nil)

       (setf (conn--get-state-object ',name) (,ctor))

       (cl-pushnew ',name conn-states)

       (defvar-local ,name nil
         ,(conn--stringify "Non-nil when `" name "' is active."))

       (defvar ,keymap-name
         (conn-get-state-map ',name)
         ,(conn--stringify "Keymap active in `" name "'."))

       ;; Is there a better way to do this?  We want to make the state
       ;; map a copy of the keymap we have been passed but we don't
       ;; want to invalidate all the pointers to our state keymap in
       ;; child state keymaps.
       ,(when keymap
          `(setcdr ,keymap-name (cdr (copy-keymap ,keymap))))

       (set-keymap-parent
        ,keymap-name (make-composed-keymap
                      (cl-loop for parent in ',inherit
                               collect (conn-get-state-map parent))))

       (cl-defmethod conn-exit-state ((state (eql ',name)))
         (when ,name
           (let ((success nil))
             (unwind-protect
                 (progn
                   (setq ,name nil
                         conn-current-state nil
                         conn-previous-state state
                         cursor-type t)
                   (conn-state-body ',name)
                   (setq success t))
               (unless success
                 (conn-local-mode -1)
                 (message "Error exiting state %s." ',name))))
           (run-hook-wrapped
            'conn-exit-functions
            (lambda (fn)
              (condition-case err
                  (funcall fn state)
                (t
                 (remove-hook 'conn-exit-functions fn)
                 (message "Error in conn-exit-functions %s" (car err))))))))

       (cl-defmethod conn-enter-state ((state (eql ',name)))
         (unless ,name
           (let ((success nil))
             (unwind-protect
                 (progn
                   (conn-exit-state conn-current-state)
                   (setq ,name t
                         conn-current-state state)
                   (setq-local conn-lighter (conn-state-get ',name 'lighter)
                               conn--local-minor-mode-maps (alist-get ',name conn--minor-mode-maps)
                               conn--hide-mark-cursor (conn-state-get ',name 'hide-mark-cursor)
                               cursor-type (or (conn-state-get ',name 'cursor) t))
                   (conn--activate-input-method)
                   (when (not executing-kbd-macro)
                     (force-mode-line-update))
                   (conn-state-body ',name)
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

       ,(when body
          `(cl-defmethod conn-state-body ((_ (eql ',name)))
             ,@body))

       (defun ,name ()
         ,doc
         (interactive)
         (conn-enter-state ',name)))))

(conn-define-state conn-null-state
  "An empty state.

For use in buffers that should not have any other state."
  :lighter ""
  :hide-mark-cursor t
  :cursor '(bar . 5))

(conn-define-state conn-emacs-state
  "A `conn-mode' state for inserting text.

By default `conn-emacs-state' does not bind anything."
  :lighter " emacs"
  :cursor '(hbar . 8))

(conn-define-state (conn-movement-state
                    (:keymap (define-keymap :suppress t)))
  "A `conn-mode' state moving in a buffer."
  :lighter " move"
  :suppress-input-method t)

(conn-define-state conn-menu-state
  "A `conn-mode' state for remapping key menus."
  :lighter " menu")

(conn-define-state (conn-state
                    (:keymap (define-keymap :suppress t))
                    (:inherit conn-menu-state conn-movement-state))
  "A `conn-mode' state for editing test."
  :lighter " conn"
  :suppress-input-method t
  :cursor 'box)

(conn-define-state (conn-org-edit-state
                    (:keymap (define-keymap :suppress t)))
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  :lighter " org-edit"
  :suppress-input-method t)


;;;; Labels

;; Functions to provide the basic machinery for labeling a set of things
;; (buffer regions, windows, etc.) and prompting the user to select from
;; a set of labels.

(defvar conn-label-string-generator 'conn-simple-labels
  "Function to create label strings for a number of elements.

Is a function of one arguments, the number of labels required.")

;; Each label is a state machine that should define its transition
;; functions as methods of the `conn-label-narrow' and
;; `conn-label-reset' generic functions.  `conn-label-narrow' is
;; called when user input is received for the label to process and
;; `conn-label-reset' is called when the user has failed to select a
;; label and the narrowing process must restart from the beginning.
;; `conn-label-narrow' allows labels to clean up after themselves
;; after the selection process has concluded.

(cl-defstruct (conn-dispatch-label)
  "Store the state for a dispatch label."
  string overlay bounds prop target-overlay)

(cl-defstruct (conn-window-label)
  "Store the state for a window label."
  string overlay window)

(cl-defgeneric conn-label-delete (label)
  "Delete the label LABEL.")

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
  (with-slots (string overlay bounds prop target-overlay) label
    (with-current-buffer (overlay-buffer overlay)
      (pcase-let ((`(,beg . ,end) bounds))
        (overlay-put target-overlay 'display
                     (propertize (buffer-substring
                                  (overlay-start target-overlay)
                                  (overlay-end target-overlay))
                                 'face 'conn-read-string-match-face))
        (move-overlay overlay beg end)
        (when-let* ((after-str (buffer-substring (overlay-start overlay)
                                                 (overlay-end overlay)))
                    (pos (string-search "\n" after-str)))
          (overlay-put overlay 'after-string (substring after-str pos)))
        (overlay-put overlay prop string)))))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label)))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label) prefix-char)
  (with-slots (overlay prop target-overlay) label
    (with-current-buffer (overlay-buffer overlay)
      (cond ((length= (overlay-get overlay prop) 0)
             nil)
            ((not (eql prefix-char (aref (overlay-get overlay prop) 0)))
             (move-overlay overlay (overlay-start overlay) (overlay-start overlay))
             (overlay-put overlay prop "")
             (overlay-put overlay 'after-string nil)
             (overlay-put target-overlay 'display nil)
             (overlay-put target-overlay 'after-string nil)
             nil)
            (t
             (move-overlay overlay
                           (overlay-start overlay)
                           (+ (overlay-start overlay)
                              (min (1- (length (overlay-get overlay prop)))
                                   (- (overlay-end overlay)
                                      (overlay-start overlay)))))
             (conn--dispatch-pad-label-overlay
              overlay prop (substring (overlay-get overlay prop) 1))
             label)))))

(cl-defmethod conn-label-payload ((label conn-window-label))
  (conn-window-label-window label))

(cl-defmethod conn-label-reset ((label conn-window-label))
  (with-slots (string overlay) label
    (overlay-put overlay 'before-string string)))

(cl-defmethod conn-label-delete ((label conn-window-label))
  (delete-overlay (conn-window-label-overlay label)))

(cl-defmethod conn-label-narrow ((label conn-window-label) prefix-char)
  (with-slots (string overlay) label
    (cond ((length= (overlay-get overlay 'before-string) 0)
           nil)
          ((not (eql prefix-char (aref (overlay-get overlay 'before-string) 0)))
           (overlay-put overlay 'before-string nil)
           nil)
          (t
           (overlay-put
            overlay 'before-string
            (substring (overlay-get overlay 'before-string) 1))
           label))))

(defun conn-label-select (candidates)
  "Select a label from CANDIDATES.

Prompts to user for prefix characters one at a time and narrows the
labels after each one."
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
                         (c (read-char prompt)))
                     (dolist (label current next)
                       (when-let* ((l (conn-label-narrow label c)))
                         (push l next))))))))

(defun conn--make-target-overlay (pt length &optional thing)
  "Make a target overlay at PT of LENGTH.

Optionally the overlay may have an associated THING."
  (let* ((eol (save-excursion
                (goto-char pt)
                (line-end-position)))
         (ov (make-overlay pt (min (+ pt length) eol) nil t)))
    (overlay-put ov 'conn-overlay t)
    (overlay-put ov 'thing thing)
    (overlay-put ov 'category 'conn-read-string-match)
    ;; Prevents overlay extending into invisible text ellipsis
    (overlay-put ov 'display
                 (propertize (buffer-substring (overlay-start ov)
                                               (overlay-end ov))
                             'face 'conn-read-string-match-face))
    (overlay-put ov 'window (selected-window))
    ov))

(defun conn--get-dispatch-windows (all-windows)
  (cond (all-windows
         (cl-loop for win in (conn--get-windows nil nil 'visible)
                  when (run-hook-with-args-until-failure
                        'conn-dispatch-window-predicates
                        win)
                  collect win))
        ((run-hook-with-args-until-failure
          'conn-dispatch-window-predicates
          (selected-window))
         (list (selected-window)))))

(defun conn--string-preview-overlays-1 (win string &optional dir)
  (with-selected-window win
    (cl-loop for pt in (conn--visible-matches string dir)
             collect (conn--make-target-overlay pt (length string)))))

(defun conn--string-preview-overlays (string &optional dir all-windows)
  (cl-loop for win in (conn--get-dispatch-windows all-windows)
           nconc (conn--string-preview-overlays-1 win string dir)))

(defun conn--read-string-with-timeout-1 (&optional dir all-windows)
  (conn--with-input-method
    (let* ((prompt (propertize "string: " 'face 'minibuffer-prompt))
           (string (char-to-string (read-char prompt t)))
           (overlays (conn--string-preview-overlays string dir all-windows))
           (success nil))
      (unwind-protect
          (progn
            (while-let ((next-char (read-char (format (concat prompt "%s") string) t
                                              conn-read-string-timeout)))
              (setq string (concat string (char-to-string next-char)))
              (mapc #'delete-overlay overlays)
              (setq overlays (conn--string-preview-overlays string dir all-windows)))
            (message nil)
            (setq success t)
            (cons string overlays))
        (unless success
          (mapc #'delete-overlay overlays))))))

(defun conn--read-string-with-timeout (&optional dir all-windows)
  (pcase-let ((`(,string . ,overlays)
               (conn--read-string-with-timeout-1 dir all-windows)))
    (mapc #'delete-overlay overlays)
    string))

(defun conn-simple-labels (count &optional labels)
  (named-let rec ((count count)
                  (labels (or labels
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
        (dolist (l labels)
          (put-text-property 0 (length l) 'face 'conn-dispatch-label-face l))
        (nreverse labels)))))

(defun conn--create-window-labels (labels windows)
  (unless (assq 'conn-mode (default-value 'mode-line-format))
    (set-default
     'mode-line-format
     (cons
      '(conn-mode (:eval (window-parameter (selected-window) 'conn-label)))
      (assq-delete-all
       'conn-mode
       (default-value 'mode-line-format)))))
  (let* ((labeled (seq-filter (lambda (win) (window-parameter win 'conn-label))
                              (conn--get-windows nil 'no-minibuff t)))
         (labels (thread-first
                   (lambda (win) (window-parameter win 'conn-label))
                   (mapcar labeled)
                   (thread-last (seq-difference labels)))))
    (cl-loop with scroll-margin = 0
             for win in (conn--get-windows nil 'no-minibuff t)
             for string = (or (window-parameter win 'conn-label)
                              (set-window-parameter win 'conn-label (pop labels)))
             when (memq win windows)
             collect (with-selected-window win
                       (let ((overlay (make-overlay (window-start) (window-end))))
                         (goto-char (window-start))
                         (forward-line)
                         (overlay-put overlay 'conn-overlay t)
                         (overlay-put overlay 'face 'shadow)
                         (overlay-put overlay 'window win)
                         (overlay-put overlay 'before-string
                                      (propertize string 'face 'conn-window-prompt-face))
                         (make-conn-window-label
                          :string string
                          :overlay overlay
                          :window win))))))

(defun conn--destroy-mode-line-labels ()
  (dolist (win (window-list-1 nil 'no-minibuf t))
    (set-window-parameter win 'conn-label nil)))

;; From ace-window
(defun conn--get-windows (&optional window minibuffer all-frames)
  (seq-remove (lambda (window)
                (or
                 ;; ignore child frames
                 (and (fboundp 'frame-parent) (frame-parent (window-frame window)))
                 ;; When `ignore-window-parameters' is nil, ignore windows whose
                 ;; `no-other-window’ or `no-delete-other-windows' parameter is non-nil.
                 (unless ignore-window-parameters
                   (window-parameter window 'no-other-window))))
              (window-list-1 window minibuffer all-frames)))

(defun conn--prompt-for-window (windows &optional dedicated)
  (when (or (and dedicated windows)
            (setq windows (seq-remove 'window-dedicated-p windows)))
    (if (length= windows 1)
        (car windows)
      (let ((window-state
             (cl-loop for win in windows
                      collect (list (window-point win)
                                    (window-vscroll win)
                                    (window-hscroll win))))
            (labels (conn--create-window-labels
                     (funcall conn-label-string-generator
                              (length (conn--get-windows nil 'nomini t)))
                     windows)))
        (unwind-protect
            (conn-label-select labels)
          (cl-loop for win in windows
                   for (pt vscroll hscroll) in window-state
                   do (progn (set-window-point win pt)
                             (set-window-hscroll win hscroll)
                             (set-window-vscroll win vscroll)))
          (mapc #'conn-label-delete labels)
          (unless conn-wincontrol-mode
            (conn--destroy-mode-line-labels)))))))


;;;; Read Things

(defvar conn-state-for-mover-reading 'conn-read-mover
  "State which should be active in `conn-read-thing-mover-mode'.")

(defvar conn-bounds-of-command-alist
  `((recursive-edit . conn--bounds-of-dots)
    (conn-toggle-mark-command . conn--bounds-of-region)
    (conn-expand . ,(apply-partially 'conn--bounds-of-expansion 'conn-expand))
    (conn-expand-remote . conn--bounds-of-remote-expansion)
    (conn-contract . ,(apply-partially 'conn--bounds-of-expansion 'conn-contract))
    (set-mark-command . conn--bounds-of-region)
    (conn-set-mark-command . conn--bounds-of-region)
    (visible . conn--bounds-of-window)
    (narrowing . conn--bounds-of-narrowings)
    (dot . conn--bounds-of-dot))
  "Alist of bounds-op functions for things or commands.

Has the form ((THING-OR-CMD . bounds-op) ...).")

(conn-define-state (conn-read-mover
                    (:keymap (define-keymap
                               "DEL" 'backward-delete-arg
                               "C-d" 'forward-delete-arg
                               "," 'reset-arg
                               "<remap> <conn-forward-char>" 'forward-char
                               "<remap> <conn-backward-char>" 'backward-char
                               "C-h" 'help
                               "t" 'conn-mark-thing-map
                               "e" 'recursive-edit))
                    (:inherit conn-state))
  "A state for reading things."
  :lighter " MOVER"
  (unless executing-kbd-macro
    (if conn-read-mover
        (set-face-inverse-video 'mode-line t)
      (set-face-inverse-video 'mode-line nil))))

(defvar conn-bounds-of-command-default
  'conn--bounds-of-thing-command-default
  "Default bounds-op for `conn-bounds-of-command'.")

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

(defvar conn-read-thing-mover-maps-alist nil)

(defvar conn-last-bounds-of-command nil)

(defvar conn--thing-overriding-maps nil)

(defvar-keymap conn-read-expand-region-map
  :parent conn-expand-repeat-map
  "v" 'conn-toggle-mark-command
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit
  "<t>" 'ignore)

(defun conn-last-bounds-of-command ()
  "Value of the most recent `conn-bounds-of-command' at this recursion depth."
  (alist-get (recursion-depth) conn-last-bounds-of-command))

(defun conn-bounds-of-command (cmd arg)
  "Return bounds of CMD with ARGS.

Bounds list is of the form (BEG END . SUBREGIONS).  Commands may return
multiple SUBREGIONS when it makes sense to do so.  For example
`forward-sexp' with a ARG of 3 would return the BEG and END of the group
of 3 sexps moved over as well as the bounds of each individual sexp."
  (setf (alist-get (recursion-depth) conn-last-bounds-of-command)
        (funcall (or (alist-get cmd conn-bounds-of-command-alist)
                     (ignore-errors
                       (alist-get (get cmd :conn-command-thing)
                                  conn-bounds-of-command-alist))
                     (apply-partially conn-bounds-of-command-default cmd))
                 arg)))

(defun conn--bounds-of-thing-command-default (cmd arg)
  (let ((current-prefix-arg nil)
        (conn-this-command-handler (or (conn-get-mark-handler cmd)
                                       'conn-discrete-thing-handler))
        (conn-this-command-thing (get cmd :conn-command-thing))
        (this-command cmd)
        (conn-this-command-start (point-marker))
        (regions))
    (save-mark-and-excursion
      (dotimes (_ (prefix-numeric-value arg))
        (call-interactively cmd)
        (funcall conn-this-command-handler conn-this-command-start)
        (move-marker conn-this-command-start (point))
        (push (cons (region-beginning) (region-end)) regions))
      (cons (cons (seq-min (mapcar #'car regions))
                  (seq-max (mapcar #'cdr regions)))
            regions))))

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

(defun conn--bounds-of-region (_arg)
  (cons (cons (region-beginning) (region-end))
        (region-bounds)))

(defun conn--bounds-of-window (_arg)
  (list (cons (window-start) (window-end))))

(defun conn--bounds-of-remote-expansion (arg)
  (conn--push-ephemeral-mark)
  (conn--bounds-of-expansion 'conn-expand arg))

(defun conn--bounds-of-dot (_arg)
  (catch 'found
    (dolist (ov (overlays-at (point)))
      (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
        (throw 'found (list (conn--overlay-bounds-markers ov)))))))

(defun conn--bounds-of-narrowings (_arg)
  (cl-loop for (beg . end) in conn-narrow-ring
           minimize beg into narrow-beg
           maximize end into narrow-end
           collect (cons beg end) into narrowings
           finally return (cons (cons narrow-beg narrow-end)
                                narrowings)))

(defun conn-bounds-of-things-in-region (thing beg end)
  "Return list of bounds of THING's in region from BEG to END."
  (save-mark-and-excursion
    (funcall (or (alist-get thing conn-bounds-of-things-in-region-alist)
                 (ignore-errors
                   (alist-get (get thing :conn-command-thing)
                              conn-bounds-of-things-in-region-alist))
                 conn-bounds-of-things-in-region-default)
             thing beg end)))

(defun conn--things-in-region-default (thing beg end)
  (let ((thing (or (ignore-errors (get thing :conn-command-thing))
                   thing)))
    (ignore-errors
      (goto-char beg)
      (forward-thing thing 1)
      (cl-loop for bounds = (save-excursion
                              (forward-thing thing -1)
                              (bounds-of-thing-at-point thing))
               while (and bounds (<= (cdr bounds) end))
               when (<= beg (car bounds))
               collect bounds into regions
               while (and (< (point) end)
                          (ignore-errors
                            (forward-thing thing 1)
                            t))
               finally return regions))))

(defun conn-read-thing-mover (prompt &optional arg recursive-edit)
  "Interactively read a thing command and arg.

PROMPT is the prompt that will be displayed to the user.
ARG is the initial value for the arg to be returned.
RECURSIVE-EDIT allows `recursive-edit' to be returned as a thing
command.  See `conn-dot-mode' for how bounds of `recursive-edit'
are read."
  (conn--with-state conn-state-for-mover-reading
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<" (conn--stringify conn-state-for-mover-reading "-map") ">"
                           (propertize prompt 'face 'minibuffer-prompt)
                           " (arg: "
                           (propertize "%s" 'face 'read-multiple-choice-face)
                           ", \\[reset-arg] reset arg; \\[help] commands"
                           (if recursive-edit
                               (concat "; \\[recursive-edit] "
                                       "recursive edit)")
                             ")")
                           ": %s")))
          (thing-arg (when arg (abs (prefix-numeric-value arg))))
          (thing-sign (when arg (> 0 arg)))
          invalid keys cmd)
         :read-command
         (setq keys (read-key-sequence
                     (format prompt
                             (format (if thing-arg "%s%s" "[%s1]")
                                     (if thing-sign "-" "")
                                     thing-arg)
                             (if invalid
                                 (propertize "Not a valid thing command"
                                             'face 'error)
                               "")))
               cmd (key-binding keys t))
         :test
         (pcase cmd
           ('keyboard-quit (keyboard-quit))
           ('help
            (save-window-excursion
              (setq cmd (intern
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
                                 (get sym :conn-command-thing)))
                          t))))
            (go :test))
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
           ((guard (or (ignore-errors (get cmd :conn-command-thing))
                       (alist-get cmd conn-bounds-of-command-alist)))
            (cl-return
             (list cmd (cond (thing-arg (* thing-arg (if thing-sign -1 1)))
                             (thing-sign '-)))))
           (_ (setq invalid t)))
         (go :read-command))
      (message nil))))

(defun conn-read-thing-region (prompt &optional arg)
  "Interactively read a thing region from the user.

See `conn-read-thing-mover' and `conn-bounds-of-command' for the region
is read."
  (pcase-let* ((`(,cmd ,arg) (conn-read-thing-mover prompt arg t)))
    (cons (get cmd :conn-command-thing)
          (conn-bounds-of-command cmd arg))))

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

(defun conn-mouse-click-dot (event)
  (interactive "e")
  (let* ((posn (event-start event))
         (point (posn-point posn)))
    (conn--create-dot point (1+ point) (window-buffer (posn-window posn)) t)))

(defun conn-point-to-dot (point)
  (interactive (list (point)))
  (conn--create-dot point (1+ point) nil t))

(defun conn-region-to-dot (bounds)
  (interactive (list (region-bounds)))
  (pcase-dolist (`(,beg . ,end) bounds)
    (conn--create-dot beg end))
  (deactivate-mark t))

(defun conn-delete-dot-at-click (event)
  (interactive "e")
  (let* ((posn (event-start event))
         (point (posn-point posn)))
    (with-current-buffer (window-buffer (posn-window posn))
      (dolist (ov (overlays-at point))
        (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
          (delete-overlay ov))))))

(defun conn-delete-dots ()
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

(defun conn-dot-mode-command-hook ()
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
  "S-<mouse-1>" 'conn-mouse-click-dot
  "S-<down-mouse-1>" 'conn-mouse-click-dot
  "S-<drag-mouse-1>" 'conn-mouse-click-dot
  "S-<mouse-3>" 'conn-delete-dot-at-click
  "S-<down-mouse-3>" 'conn-delete-dot-at-click
  "<escape>" 'exit-recursive-edit
  "DEL" 'conn-delete-dots
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(define-minor-mode conn-dot-mode
  "Minor mode for multiple dots."
  :global t
  :lighter " DOT"
  :interactive nil
  (if conn-dot-mode
      (progn
        (internal-push-keymap conn-dot-mode-map 'overriding-terminal-local-map)
        (delete-overlay mouse-secondary-overlay)
        (setq conn--dots nil
              buffer-read-only t)
        (add-hook 'post-command-hook 'conn-dot-mode-command-hook))
    (internal-pop-keymap conn-dot-mode-map 'overriding-terminal-local-map)
    (remove-hook 'post-command-hook 'conn-dot-mode-command-hook)
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


;;;; Advice

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
              (null conn-local-mode)
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring-right (mark-marker)))
  (setq conn--ephemeral-mark nil))

(defun conn--pop-mark-ad (&rest _)
  (unless (or conn--ephemeral-mark
              (null conn-local-mode)
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring-left (mark-marker)))
  (setq conn--ephemeral-mark t))

(defun conn--set-mark-ad (&rest _)
  (setq conn--ephemeral-mark nil))

(defun conn--save-ephemeral-mark-ad (&rest _)
  (push conn--ephemeral-mark conn--saved-ephemeral-marks))

(defun conn--restore-ephemeral-mark-ad (&rest _)
  (setq-local conn--ephemeral-mark (pop conn--saved-ephemeral-marks)))

(defun conn--setup-advice ()
  (if conn-mode
      (progn
        (advice-add 'toggle-input-method :around 'conn--toggle-input-method-ad)
        (advice-add 'query-replace-read-from-suggestions :around
                    'conn--read-from-suggestions-ad)
        (advice-add 'read-regexp-suggestions :around
                    'conn--read-from-suggestions-ad)
        (advice-add 'repeat :around #'conn--repeat-ad)
        (advice-add 'push-mark :before #'conn--push-mark-ad)
        (advice-add 'pop-mark :before #'conn--pop-mark-ad)
        (advice-add 'set-mark :before #'conn--set-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'toggle-input-method 'conn--toggle-input-method-ad)
    (advice-remove 'query-replace-read-from-suggestions
                   'conn--read-from-suggestions-ad)
    (advice-remove 'read-regexp-suggestions
                   'conn--read-from-suggestions-ad)
    (advice-remove 'set-mark #'conn--set-mark-ad)
    (advice-remove 'repeat #'conn--repeat-ad)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)))


;;;; Kapply

(defvar kmacro-step-edit-replace)

(defvar conn-kmacro-applying-p nil
  "Non-nil during kmacro application.")

(defvar conn-kmacro-apply-error nil
  "If non-nil contains the error encountered during macro application.")

(defvar conn-kmacro-apply-end-hook nil
  "Hook run after macro application has completed.")

(defvar conn-kmacro-apply-start-hook nil
  "Hook run before macro application begins.")

(defvar conn-kmacro-apply-iterator-hook nil
  "Hook run during each iteration of macro application.
If any function returns a nil value then macro application it halted.")

(defvar conn-kapply-preview-max-overlays 200)

(defvar conn--kapply-automatic-flag nil)

(defun conn--kapply-compose-iterator (iterator &rest pipeline)
  (seq-reduce (pcase-lambda (iterator (or `(,ctor . ,args) ctor))
                (apply ctor iterator args))
              (delq nil pipeline)
              iterator))

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
  (cond
   (flag
    (let (executing-kbd-macro defining-kbd-macro)
      (recursive-edit)))
   ((not executing-kbd-macro))
   ((not conn--kapply-automatic-flag)
    (cl-loop
     with msg = (substitute-command-keys
                 "Proceed with macro?\\<query-replace-map>\
 (\\[act], \\[skip], \\[exit], \\[recenter], \\[edit], \\[automatic]) ")
     do (pcase (let ((executing-kbd-macro nil)
                     (defining-kbd-macro nil))
                 (message "%s" msg)
                 (lookup-key query-replace-map (vector (read-event))))
          ('act (cl-return))
          ('skip
           (setq executing-kbd-macro "")
           (cl-return))
          ('exit
           (setq executing-kbd-macro t)
           (cl-return))
          ('recenter
           (recenter nil))
          ('edit
           (let (executing-kbd-macro defining-kbd-macro)
             (recursive-edit)))
          ('quit
           (setq quit-flag t)
           (cl-return))
          ('automatic
           (setq conn--kapply-automatic-flag t)
           (cl-return))
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
          (_ (ding)))))))

(defun conn--kapply-preview-overlays (regions &optional face)
  (let* ((nearest (pcase-lambda (`(,b1 . ,e1) `(,b2 . ,e2))
                    (< (min (abs (- b1 (point)))
                            (abs (- e1 (point))))
                       (min (abs (- b2 (point)))
                            (abs (- e2 (point)))))))
         (this-bufferp (lambda (a _)
                         (eq (current-buffer) (car a))))
         (regions (thread-first
                    (seq-group-by (pcase-lambda (`(,beg . ,_end))
                                    (marker-buffer beg))
                                  regions)
                    (sort :lessp this-bufferp))))
    (cl-loop repeat conn-kapply-preview-max-overlays
             for (b . e) in (nconc (sort (cdar regions) :lessp nearest)
                                   (mapcan #'cdr (cdr regions)))
             collect (let ((ov (make-overlay b e (marker-buffer b) t)))
                       (overlay-put ov 'category 'kapply-preview)
                       (when face (overlay-put ov 'face face))
                       ov))))

(defun conn--kapply-advance-region (region)
  (pcase region
    (`(,beg . ,end)
     (when-let* ((buffer (ignore-errors (marker-buffer beg))))
       (when (not (eq buffer (current-buffer)))
         (pop-to-buffer-same-window buffer)
         (deactivate-mark t)
         (unless (eq buffer (window-buffer (selected-window)))
           (error "Could not pop to buffer %s" buffer))))
     (goto-char beg)
     (when conn-local-mode
       (conn--push-ephemeral-mark end))
     (when (markerp beg) (set-marker beg nil))
     (when (markerp end) (set-marker end nil))
     t)))

(defun conn--kapply-infinite-iterator ()
  (lambda (_state) t))

(defun conn--kapply-highlight-iterator (beg end &optional order read-patterns)
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
    (setq matches (pcase order
                    ('forward (seq-sort-by #'car #'< matches))
                    ('reverse (seq-sort-by #'car #'> matches))
                    (_        (conn--nnearest-first
                               (seq-sort-by #'car #'< matches)))))
    (lambda (state)
      (pcase state
        (:finalize
         (pcase-dolist (`(,beg . ,end) matches)
           (set-marker beg nil)
           (set-marker end nil)))
        (_
         (conn--kapply-advance-region (pop matches)))))))

(defun conn--kapply-thing-iterator (thing beg end &optional order skip-empty nth)
  (prog1
      (thread-first
        (conn-bounds-of-things-in-region thing beg end)
        (conn--thread regions
            (if skip-empty
                (seq-remove (lambda (reg) (conn-thing-empty-p thing reg))
                            regions)
              regions))
        (conn--thread regions
            (if (or (null nth) (= 1 nth))
                regions
              (cl-loop with stack = regions
                       while stack
                       collect (car stack)
                       do (cl-loop repeat nth do (pop stack)))))
        (conn--kapply-region-iterator order))
    (deactivate-mark)))

(defun conn--kapply-region-iterator (regions &optional order skip-empty)
  (when skip-empty
    (setq regions (seq-remove (pcase-lambda (`(,beg . ,end))
                                (eq beg end))
                              regions)))
  (unless regions
    (user-error "No regions for kapply."))
  (pcase-dolist ((and reg `(,beg . ,end))
                 (setq regions
                       (pcase order
                         ('forward regions)
                         ('reverse (nreverse regions))
                         (_        (conn--nnearest-first regions)))))
    (if (markerp beg)
        (set-marker-insertion-type beg t)
      (setcar reg (conn--create-marker beg nil t)))
    (if (markerp end)
        (set-marker-insertion-type end nil)
      (setcdr reg (conn--create-marker end (marker-buffer (car reg))))))
  (let (overlays)
    (lambda (state)
      (pcase state
        (:finalize
         (mapc #'delete-overlay overlays)
         (pcase-dolist (`(,beg . ,end) regions)
           (set-marker beg nil)
           (set-marker end nil)))
        (:record
         (setq overlays (conn--kapply-preview-overlays (cdr regions) 'region))
         (conn--kapply-advance-region (pop regions)))
        (_
         (when overlays
           (mapc #'delete-overlay overlays)
           (setq overlays nil))
         (conn--kapply-advance-region (pop regions)))))))

(defun conn--kapply-point-iterator (points &optional order)
  (unless points
    (user-error "No points for kapply."))
  (let ((points
         (cl-loop for pt in (pcase order
                              ('forward points)
                              ('reverse (nreverse points))
                              (_        (conn--nnearest-first points)))
                  collect (if (markerp pt)
                              pt
                            (conn--create-marker pt)))))
    (lambda (state)
      (pcase state
        (:finalize
         (dolist (pt points)
           (set-marker pt nil)))
        (_
         (when-let* ((pt (pop points)))
           (conn--kapply-advance-region (cons pt pt))))))))

(defun conn--kapply-match-iterator ( string regions &optional
                                     regexp-flag order delimited-flag query-flag)
  (let (matches overlays)
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
    (setq matches (pcase order
                    ('forward matches)
                    ('reverse (nreverse matches))
                    (_        (conn--nnearest-first matches))))
    (lambda (state)
      (pcase state
        (:finalize
         (mapc #'delete-overlay overlays)
         (mapc (pcase-lambda (`(,beg . ,end))
                 (set-marker beg nil)
                 (set-marker end nil))
               matches))
        (:record
         (setq overlays (conn--kapply-preview-overlays (cdr matches)))
         (if query-flag
             (let ((hl (make-overlay (point) (point))))
               (overlay-put hl 'priority 2000)
               (overlay-put hl 'face 'query-replace)
               (overlay-put hl 'conn-overlay t)
               (unwind-protect
                   (cl-loop
                    with len = (length matches)
                    for cont = (conn--kapply-advance-region (pop matches))
                    for i from 1
                    until (or (null cont)
                              (progn
                                (recenter nil)
                                (move-overlay hl (region-beginning) (region-end) (current-buffer))
                                (y-or-n-p (format "[%s/%s] Record here?" i len))))
                    finally return cont)
                 (delete-overlay hl)))
           (conn--kapply-advance-region (pop matches))))
        (_
         (when overlays
           (mapc #'delete-overlay overlays)
           (setq overlays nil))
         (conn--kapply-advance-region (pop matches)))))))

(defun conn--kapply-per-buffer-undo (iterator &optional atomic)
  (let (undo-handles)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(_ . ,handle) undo-handles)
           (if (and atomic conn-kmacro-apply-error)
               (cancel-change-group handle)
             (accept-change-group handle)
             (undo-amalgamate-change-group handle))))
        (_
         (prog1
             (funcall iterator state)
           (unless (alist-get (current-buffer) undo-handles)
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
           (setq handle (prepare-change-group))
           (activate-change-group handle)))
        (:finalize
         (accept-change-group handle)
         (undo-amalgamate-change-group handle)
         (funcall iterator state))
        (_
         (accept-change-group handle)
         (undo-amalgamate-change-group handle)
         (prog1
             (funcall iterator state)
           (undo-boundary)
           (setq handle (prepare-change-group))
           (activate-change-group handle)))))))

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
        (_
         (prog1 (funcall iterator state)
           (unless (alist-get (current-buffer) saved-excursions)
             (setf (alist-get (current-buffer) saved-excursions)
                   (cons (point-marker) (save-mark-and-excursion--save))))))))))

(defun conn--kapply-ibuffer-overview (iterator)
  (let (buffers)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (when (and (not conn-kmacro-apply-error)
                    (length> buffers 1))
           (ibuffer t "*Kapply Ibuffer*"
                    `((predicate . (memq (current-buffer) ',buffers))))))
        (_
         (prog1 (funcall iterator state)
           (unless (memq (current-buffer) buffers)
             (push (current-buffer) buffers))))))))

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
        (_
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

(defun conn--kapply-with-state (iterator transition)
  (let (buffer-states)
    (lambda (state)
      (prog1
          (funcall iterator state)
        (pcase state
          (:finalize
           (pcase-dolist (`(,buf ,state ,prev-state) buffer-states)
             (when state
               (with-current-buffer buf
                 (funcall state)
                 (setq conn-previous-state prev-state)))))
          (_
           (when conn-local-mode
             (unless (alist-get (current-buffer) buffer-states)
               (setf (alist-get (current-buffer) buffer-states)
                     (list conn-current-state conn-previous-state)))
             (funcall transition))))))))

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
        (_
         (unless wconf (setq wconf (current-window-configuration)))
         (funcall iterator state))))))

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
    `(defun ,name ,arglist
       ,docstring
       (require 'kmacro)
       (let* ((undo-outer-limit nil)
              (undo-limit most-positive-fixnum)
              (undo-strong-limit most-positive-fixnum)
              (conn-kmacro-applying-p t)
              (conn--kapply-automatic-flag nil)
              (success nil)
              (,iterator (lambda (&optional state)
                           (when (funcall ,iterator (or state :loop))
                             (run-hook-with-args-until-failure
                              'conn-kmacro-apply-iterator-hook)))))
         (run-hook-wrapped 'conn-kmacro-apply-start-hook
                           (lambda (hook)
                             (ignore-errors (funcall hook))))
         (deactivate-mark)
         (unwind-protect
             (conn--with-advice (('kmacro-loop-setup-function :before-while ,iterator))
               ,@body
               (setq success t))
           (let ((conn-kmacro-apply-error (not success)))
             (funcall ,iterator :finalize)
             (run-hook-wrapped 'conn-kmacro-apply-end-hook
                               (lambda (hook)
                                 (ignore-errors (funcall hook))))))))))

(conn--define-kapply conn--kmacro-apply (iterator &optional count macro)
  (pcase-exhaustive macro
    ((pred kmacro-p)
     (funcall macro (or count 0)))
    ((or (pred stringp) (pred vectorp))
     (kmacro-call-macro (or count 0) nil nil macro))
    ('nil
     (when (funcall iterator :record)
       (kmacro-start-macro nil)
       (unwind-protect
           (progn
             (recursive-edit)
             (when (not defining-kbd-macro)
               (user-error "Not defining keyboard macro")))
         (when defining-kbd-macro (kmacro-end-macro nil)))
       (kmacro-call-macro (or count 0))))))

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


;;;; Mark

(defun conn-get-mark-handler (command)
  "Return the mark handler for COMMAND."
  (or (alist-get command conn-mark-handler-overrides-alist)
      (ignore-errors (get command :conn-mark-handler))))

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

MODES is a list of the modes in which THINGs may be present.

MARK-CMD is either a command which marks a THING at point or t if such a
command should be automatically created.

MARK-KEY is a key which should be bound to MARK-CMD in
`conn-mark-thing-map' (only in MODES if MODES is non-nil).

\(fn THING &key TARGET-FINDER DEFAULT-ACTION FORWARD-OP BEG-OP END-OP BOUNDS-OP MODES MARK-CMD MARK-KEY)"
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
    (put thing 'bounds-of-thing-at-point bounds))
  (when-let* ((inner-bounds-op (plist-get rest :inner-bounds-op)))
    (put thing :conn-inner-bounds-op inner-bounds-op))
  (cl-flet ((make-mark-command ()
              (let ((mark-command (conn--symbolicate "conn-mark-" thing)))
                (fset mark-command
                      (lambda ()
                        (interactive)
                        (pcase (bounds-of-thing-at-point thing)
                          (`(,beg . ,end)
                           (goto-char beg)
                           (if conn-local-mode
                               (conn--push-ephemeral-mark end)
                             (push-mark end t)))
                          (_ (user-error "Point not in %s" thing)))))
                (put mark-command :conn-command-thing thing)
                mark-command)))
    (let* ((mark-key (plist-get rest :mark-key))
           (mark-command (pcase (plist-get rest :mark-cmd)
                           ((or 't (guard (key-valid-p mark-key)))
                            (make-mark-command))
                           ((and cmd (pred symbolp) (pred functionp))
                            (put cmd :conn-command-thing thing)))))
      (when (key-valid-p mark-key)
        (if (plist-get rest :modes)
            (dolist (mode (put thing :conn-thing-modes
                               (ensure-list (plist-get rest :modes))))
              (keymap-set (conn-get-mode-things-map mode)
                          mark-key mark-command))
          (keymap-set conn-mark-thing-map mark-key mark-command))))))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER."
  (dolist (cmd commands)
    (put cmd :conn-command-thing thing)
    (put cmd :conn-mark-handler handler)))

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

;; TODO: see if this is fixable with vertico's candidate overlay
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

(defun conn--mark-pre-command-hook ()
  (unless conn--hide-mark-cursor
    (set-marker conn-this-command-start (point))
    (setq conn-this-command-handler (or (alist-get this-command conn-mark-handler-overrides-alist)
                                        (conn--command-property :conn-mark-handler))
          conn-this-command-thing (conn--command-property :conn-command-thing))))

(defun conn--mark-post-command-hook ()
  (unless conn--hide-mark-cursor
    (setf (alist-get (recursion-depth) conn-last-bounds-of-command) nil)
    (when (and conn-local-mode
               (eq (current-buffer) (marker-buffer conn-this-command-start))
               conn-this-command-thing
               conn-this-command-handler
               (not (region-active-p)))
      (with-demoted-errors "Error in mark hook: %S"
        (funcall conn-this-command-handler conn-this-command-start)))))

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

(defun conn-bounds-of-inner-thing (thing bounds)
  (when-let* ((inner-op (get thing :conn-inner-bounds-op)))
    (funcall inner-op (car bounds) (cdr bounds))))

(defun conn-thing-empty-p (thing bounds)
  (or (pcase (conn-bounds-of-inner-thing thing bounds)
        ('nil nil)
        ((and `(,beg . ,end) (guard (= beg end))) t))
      (= (car bounds) (cdr bounds))))

;;;;; Thing Definitions

;; Definitions for various things and thing commands.

(conn-register-thing 'url :mark-key "!")
(conn-register-thing 'email :mark-key "@")
(conn-register-thing 'uuid :mark-key "$")
(conn-register-thing 'string :mark-key "\"")
(conn-register-thing 'filename :mark-key "/")

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'defun t))

(conn-register-thing
 'region
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing
 'dots
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing
 'buffer-after-point
 :bounds-op (lambda () (cons (point) (point-max)))
 :mark-key ">")

(conn-register-thing
 'buffer-before-point
 :bounds-op (lambda () (cons (point-min) (point)))
 :mark-key "<")

(conn-register-thing
 'visible
 :bounds-op (lambda () (cons (window-start) (window-end))))

(conn-register-thing-commands
 'visible nil
 'conn-scroll-up 'conn-scroll-down
 'scroll-up-command 'scroll-down-command)

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
 :dispatch-target-finder (apply-partially 'conn--dispatch-things-with-prefix 'symbol 1 t))

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing
 'page
 :mark-key "p"
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
 :dispatch-target-finder (apply-partially 'conn--dispatch-things-with-prefix 'word 1 t))

(conn-register-thing-commands
 'word 'conn-symbol-handler
 'forward-word 'backward-word
 'upcase-word 'downcase-word 'capitalize-word
 'upcase-dwim 'downcase-dwim 'capitalize-dwim)

(conn-register-thing
 'sexp
 :forward-op 'forward-sexp
 :dispatch-target-finder (apply-partially 'conn--dispatch-things-with-prefix 'sexp 1 t))

(conn-register-thing-commands
 'sexp 'conn-continuous-thing-handler
 'forward-sexp 'backward-sexp)

(conn-register-thing
 'list
 :forward-op 'forward-list
 :inner-bounds-op (lambda (beg end)
                    (ignore-errors
                      (cons (save-excursion
                              (goto-char beg)
                              (down-list 1)
                              (point))
                            (save-excursion
                              (goto-char end)
                              (down-list -1)
                              (point))))))

(conn-register-thing-commands
 'list 'conn-continuous-thing-handler
 'forward-list 'backward-list)

(defun conn--list-mark-handler (beg)
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
 'list 'conn--list-mark-handler
 'up-list 'backward-up-list)

(defun conn--down-list-mark-handler (_beg)
  (condition-case _err
      (pcase (bounds-of-thing-at-point 'list)
        (`(,_ . ,end)
         (save-excursion
           (goto-char end)
           (down-list -1)
           (conn--push-ephemeral-mark (point)))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'down-list)

(conn-register-thing
 'whitespace
 :mark-key "SPC"
 :forward-op 'forward-whitespace)

(conn-register-thing-commands
 'whitespace 'conn-discrete-thing-handler
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing
 'sentence
 :forward-op 'forward-sentence
 :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'sentence t))

(conn-register-thing-commands
 'sentence 'conn-continuous-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing
 'paragraph
 :forward-op 'forward-paragraph
 :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'paragraph t))

(conn-register-thing-commands
 'paragraph 'conn-continuous-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-continuous-thing-handler
 'end-of-defun 'beginning-of-defun)

(conn-register-thing 'char :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'buffer 'conn-discrete-thing-handler
 'end-of-buffer 'beginning-of-buffer)

(defun conn-line-forward-op (N)
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
 :inner-bounds-op (lambda (beg end) (cons beg (1- end)))
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
 :dispatch-target-finder 'conn--dispatch-inner-lines)

(conn-register-thing-commands
 'inner-line 'conn-discrete-thing-handler
 'back-to-indentation
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
 'narrowing nil
 'narrow-to-region 'widen
 'conn-narrow-to-thing
 'conn-narrow-ring-prefix)


;;;; Thing Dispatch

;; Thing dispatch provides a method of jumping to, marking or acting
;; on visible Things.

(defvar conn-dispatch-state-for-reading 'conn-read-dispatch)

(conn-define-state (conn-read-dispatch
                    (:keymap (define-keymap
                               "C-h" 'help
                               "," 'reset-arg
                               "TAB" 'repeat-dispatch
                               "'" 'repeat-dispatch
                               "C-d" 'forward-delete-arg
                               "DEL" 'backward-delete-arg
                               "\\" 'kapply))
                    (:inherit conn-state))
  "State for reading a dispatch command."
  :ligher " DISPATCH"
  (unless executing-kbd-macro
    (if conn-read-dispatch
        (set-face-inverse-video 'mode-line t)
      (set-face-inverse-video 'mode-line nil))))

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(defvar conn--last-dispatch-command nil)

(defvar conn-dispatch-all-things-collector-default
  'conn--dispatch-all-things-1)

(defvar conn-dispatch-all-things-collector-alist nil)

(defvar conn-dispatch-window-predicates
  '(conn-dispatch-ignored-mode))

(defun conn--dispatch-target-finder (command)
  (or (alist-get command conn-dispatch-target-finders-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-target-finders-alist)
      conn-dispatch-target-finder-default))

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

(defun conn--dispatch-read-thing (&optional default-action continuation)
  (pcase-let*
      ((prompt (substitute-command-keys
                (concat "\\<" (conn--stringify conn-dispatch-state-for-reading "-map") ">"
                        (propertize "Targets" 'face 'minibuffer-prompt)
                        " (arg: "
                        (propertize "%s" 'face 'read-multiple-choice-face)
                        ", \\[reset-arg] reset arg; "
                        "\\[repeat-dispatch] %s; "
                        "\\[help] commands): %s")))
       (action default-action)
       (action-args nil)
       (keys nil)
       (cmd nil)
       (invalid nil)
       (`(,thing-arg ,thing-sign ,repeat) continuation)
       (repeat-indicator
        (propertize "repeatedly"
                    'face (when repeat
                            'eldoc-highlight-function-argument))))
    (cl-flet ((action-description (action)
                (if-let* ((desc (get action :conn-action-description)))
                    (propertize
                     (if (stringp desc)
                         (apply #'format desc action-args)
                       (apply desc action-args))
                     'face 'eldoc-highlight-function-argument)
                  ""))
              (read-action-args (action)
                (when (ignore-errors (get action :conn-action-interactive))
                  (funcall (get action :conn-action-interactive)))))
      (setq action-args (read-action-args action))
      (atomic-change-group
        (conn--with-state conn-dispatch-state-for-reading
          (unwind-protect
              (cl-prog
               nil
               :read-command
               (setq keys (read-key-sequence
                           (format prompt
                                   (format (if thing-arg "%s%s" "[%s1]")
                                           (if thing-sign "-" "")
                                           thing-arg)
                                   repeat-indicator
                                   (cond
                                    (invalid
                                     (concat
                                      (action-description action)
                                      " "
                                      (propertize "Not a valid thing command"
                                                  'face 'error)))
                                    (action (action-description action))
                                    (t ""))))
                     cmd (key-binding keys t)
                     invalid nil)
               :loop
               (pcase cmd
                 (`(,thing ,finder . ,default-action)
                  (cl-return
                   (list thing
                         (when thing-arg
                           (* (if thing-sign -1 1) (or thing-arg 1)))
                         finder
                         (or action
                             default-action
                             (conn--dispatch-default-action thing))
                         (if action
                             action-args
                           (read-action-args
                            (or default-action
                                (conn--dispatch-default-action thing))))
                         (conn--dispatch-window-predicate
                          action (get thing :conn-command-thing) cmd keys)
                         repeat)))
                 ('keyboard-quit
                  (keyboard-quit))
                 ('kapply
                  (run-with-timer 0 nil 'conn-dispatch-kapply-prefix
                                  (list thing-arg thing-sign repeat))
                  ;; It would probably be better to handle this as a
                  ;; special case in conn-dispatch-on-things instead
                  ;; of relying on quit to exit early.
                  (signal 'quit nil))
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
                  (conn--with-state conn-previous-state
                    (save-window-excursion
                      (setq keys nil
                            cmd (intern
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
                                                       (get sym :conn-action))))
                                       t)
                                      (`(,_ ,_ . ,_) t)))
                                  t)))))
                  (go :loop))
                 ((and (let thing (ignore-errors (get cmd :conn-command-thing)))
                       (guard thing))
                  (cl-return
                   (list cmd
                         (when thing-arg
                           (* (if thing-sign -1 1) (or thing-arg 1)))
                         (conn--dispatch-target-finder cmd)
                         (or action (conn--dispatch-default-action cmd))
                         (if action
                             action-args
                           (read-action-args (conn--dispatch-default-action cmd)))
                         (conn--dispatch-window-predicate action thing cmd keys)
                         repeat)))
                 ((guard (ignore-errors (get cmd :conn-action)))
                  (setq action (unless (eq cmd action) cmd)
                        action-args (read-action-args action)))
                 (_
                  (setq invalid t)))
               (go :read-command))
            (message nil)))))))

(defun conn-dispatch-ignored-mode (win)
  (not (apply #'provided-mode-derived-p
              (buffer-local-value 'major-mode (window-buffer win))
              conn-dispatch-thing-ignored-modes)))

(defun conn--dispatch-window-predicate (action _thing _binding _keys)
  (lambda (win)
    (let ((window-predicate (get action :conn-action-window-predicate)))
      (or (null window-predicate)
          (with-selected-window win
            (not (funcall window-predicate)))))))

(defun conn--dispatch-pad-label-overlay (overlay display-property display-string)
  (overlay-put overlay display-property nil)
  (let* ((pos (save-excursion
                (goto-char (overlay-start overlay))
                (skip-chars-forward "^\n" (overlay-end overlay))
                (point)))
         (next-line (buffer-substring pos (overlay-end overlay)))
         (pixels (max (- (car (window-text-pixel-size
                               (overlay-get overlay 'window)
                               (overlay-start overlay)
                               pos))
                         (progn
                           (overlay-put overlay display-property display-string)
                           (car (window-text-pixel-size
                                 (overlay-get overlay 'window)
                                 (overlay-start overlay)
                                 (overlay-end overlay)))))
                      0)))
    (overlay-put overlay 'after-string
                 (concat
                  (propertize
                   " "
                   'display `(space :width (,pixels)))
                  next-line))))

(defun conn--dispatch-labels (label-strings target-overlays)
  (let (labels success)
    (unwind-protect
        (progn
          (pcase-dolist (`(,window . ,previews) target-overlays)
            (with-current-buffer (window-buffer window)
              (dolist (p previews)
                (let* ((beg (overlay-end p))
                       (string (pop label-strings))
                       (next (thread-last
                               (conn--all-overlays
                                (lambda (ov)
                                  (and (eq 'conn-read-string-match
                                           (overlay-get ov 'category))
                                       (not (eq ov p))))
                                beg (+ beg (length string)))
                               (mapcar #'overlay-start)
                               (apply 'min (point-max))))
                       (end (min next (+ beg (length string))))
                       (ov (make-overlay beg end))
                       (prop (if (or (= beg next)
                                     (= beg (point-max)))
                                 'before-string
                               'display)))
                  (overlay-put ov 'category 'conn-label-overlay)
                  (overlay-put ov 'window window)
                  (conn--dispatch-pad-label-overlay ov prop string)
                  (push (make-conn-dispatch-label
                         :string string
                         :overlay ov
                         :bounds (cons beg end)
                         :prop prop
                         :target-overlay p)
                        labels)))))
          (setq success t)
          labels)
      (unless success (mapc #'conn-label-delete labels)))))

(defun conn--dispatch-fixup-whitespace ()
  (when (or (looking-at " ") (looking-back " " 1))
    (fixup-whitespace)
    (if (progn (beginning-of-line)
               (looking-at "
"))
        (join-line)
      (indent-for-tab-command)))
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\s)*\n"))
    (join-line)))

;;;;; Things

(defmacro conn-define-dispatch-thing (thing &rest rest)
  "\(fn THING &key KEY MODES TARGET-FINDER DEFAULT-ACTION &body BODY)"
  (declare (debug (name [&rest keywordp form]))
           (indent 1))
  (pcase-let* (((map :key :modes :target-finder :default-action)
                rest))
    (if modes
        `(dolist (mode ',(ensure-list modes))
           (keymap-set (conn-get-mode-map conn-dispatch-state-for-reading mode)
                       ,key `(,',thing
                              ,(or ,target-finder
                                   (conn--dispatch-target-finder ',thing))
                              .
                              ,',default-action)))
      `(keymap-set (conn-get-state-map conn-dispatch-state-for-reading)
                   ,key `(,',thing
                          ,(or ,target-finder
                               (conn--dispatch-target-finder ',thing))
                          .
                          ,',default-action)))))

(conn-define-dispatch-thing forward-line :key "l")
(conn-define-dispatch-thing forward-symbol :key "u")
(conn-define-dispatch-thing forward-char :key "j")

(conn-define-dispatch-thing conn-expand-remote
  :key "h"
  :target-finder 'conn--dispatch-chars)

(conn-define-dispatch-thing dot
  :key "k"
  :modes (conn-dot-mode)
  :target-finder (lambda ()
                   (mapcar (lambda (ov)
                             (conn--make-target-overlay
                              (overlay-start ov)
                              (- (overlay-end ov)
                                 (overlay-start ov))))
                           conn--dots)))

(conn-define-dispatch-thing forward-word
  :key "O"
  :target-finder (apply-partially 'conn--dispatch-all-things 'word t))

(conn-define-dispatch-thing forward-symbol
  :key "U"
  :target-finder (apply-partially 'conn--dispatch-all-things 'symbol t))

;;;;; Actions

(defmacro conn-define-dispatch-action (name arglist &rest rest)
  "\(fn NAME ARGLIST &key INTERACTIVE DESCRIPTION FILTER WINDOW-PREDICATE KEY MODES &body BODY)"
  (declare (debug ( name lambda-expr
                    [&rest keywordp form]
                    def-body))
           (indent 2))
  (pcase-let* (((map :description :interactive :filter :window-predicate :keys :modes)
                rest)
               (menu-item `( 'menu-item
                             ,(or description (symbol-name name))
                             ',name
                             ,@(when filter
                                 `(:filter (lambda (_)
                                             (pcase (funcall ,filter)
                                               ('this ',name)
                                               (res res)))))))
               (body (cl-loop for sublist on rest by #'cddr
                              unless (keywordp (car sublist))
                              do (cl-return sublist))))
    `(progn
       (defun ,name ,arglist ,@body)
       (put ',name :conn-action t)
       (put ',name :conn-action-interactive (lambda () ,interactive))
       (put ',name :conn-action-description ,(cadr menu-item))
       (put ',name :conn-action-window-predicate ,window-predicate)
       ,(when keys
          (if modes
              `(dolist (mode ',(ensure-list modes))
                 ,@(cl-loop for key in (ensure-list keys)
                            collect `(keymap-set
                                      (conn-get-mode-map conn-dispatch-state-for-reading mode)
                                      ,key (list ,@menu-item))))
            (macroexp-progn
             (cl-loop for key in (ensure-list keys)
                      collect `(keymap-set
                                (conn-get-state-map conn-dispatch-state-for-reading)
                                ,key (list ,@menu-item)))))))))

(conn-define-dispatch-action conn-dispatch-yank-replace-to
    (window pt thing-cmd thing-arg str)
  :description "Yank Replace To"
  :keys "C-y"
  :interactive (list (funcall region-extract-function nil))
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (delete-region beg end)
         (insert-for-yank str))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-yank-read-replace-to
    (window pt thing-cmd thing-arg str)
  :description "Yank Replace To"
  :keys "M-y"
  :interactive (list (read-from-kill-ring "Yank Replace To from kill-ring: "))
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (delete-region beg end)
         (insert-for-yank str))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-yank-to
    (window pt _thing-cmd _thing-arg str)
  :description "Yank To"
  :keys "y"
  :interactive (list (funcall region-extract-function nil))
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str))))

(conn-define-dispatch-action conn-dispatch-yank-read-to
    (window pt _thing-cmd _thing-arg str)
  :description "Yank To"
  :keys "Y"
  :interactive (list (read-from-kill-ring "Yank To from kill-ring: "))
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str))))

(conn-define-dispatch-action conn-dispatch-throw
    (window pt _thing-cmd _thing-arg str)
  :description "Throw"
  :keys "t"
  :interactive (list (funcall region-extract-function t))
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (insert-for-yank str))))

(conn-define-dispatch-action conn-dispatch-dot (window pt thing-cmd thing-arg)
  :description "Dot"
  :keys "d"
  :modes (conn-dot-mode)
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
  :keys "w"
  :modes (conn-dot-mode)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (conn-delete-dots))))

(conn-define-dispatch-action conn-dispatch-downcase (window pt thing-cmd thing-arg)
  :description "Downcase"
  :keys ("<remap> <downcase-word>"
         "<remap> <downcase-region>"
         "<remap> <downcase-dwim>")
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (downcase-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-upcase (window pt thing-cmd thing-arg)
  :description "Upcase"
  :keys ("<remap> <upcase-word>"
         "<remap> <upcase-region>"
         "<remap> <upcase-dwim>")
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (upcase-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-capitalize (window pt thing-cmd thing-arg)
  :description "Capitalize"
  :keys ("<remap> <capitalize-word>"
         "<remap> <capitalize-region>"
         "<remap> <capitalize-dwim>")
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-mark-and-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end) (capitalize-region beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-narrow-indirect (window pt thing-cmd thing-arg)
  :description "Narrow Indirect"
  :keys ("r N" "r n" "X")
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (conn--narrow-indirect beg end))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-comment (window pt thing-cmd thing-arg)
  :description "Comment"
  :keys (";" "M-;" "r ;")
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (comment-or-uncomment-region beg end)
         (message "Commented %s" thing-cmd))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-duplicate (window pt thing-cmd thing-arg)
  :description "Duplicate"
  :keys ("r e")
  :window-predicate (lambda () buffer-read-only)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (car (conn-bounds-of-command thing-cmd thing-arg))
        (`(,beg . ,end)
         (conn-duplicate-region beg end current-prefix-arg)
         (message "Duplicated %s" thing-cmd))
        (_ (user-error "Cannot find %s at point" thing-cmd))))))

(conn-define-dispatch-action conn-dispatch-duplicate-and-comment (window pt thing-cmd thing-arg arg)
  :description "Duplicate and Comment"
  :keys ("r d")
  :window-predicate (lambda () buffer-read-only)
  :interactive (list current-prefix-arg)
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
  :keys "p"
  :interactive (list (register-read-with-preview "Register: "))
  (with-selected-window window
    ;; If there is a keyboard macro in the register we would like to
    ;; amalgamate the undo
    (with-undo-amalgamate
      (save-excursion
        (goto-char pt)
        (conn-register-load register)))))

(conn-define-dispatch-action conn-dispatch-register-replace (window pt thing-cmd thing-arg register)
  :description "Register Replace <%c>"
  :keys "P"
  :interactive (list (register-read-with-preview "Register: "))
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
  :keys "w"
  :window-predicate (lambda () buffer-read-only)
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "]"
  :window-predicate (lambda () buffer-read-only)
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "["
  :window-predicate (lambda () buffer-read-only)
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "a"
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "}"
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "{"
  :interactive (list (when current-prefix-arg
                       (register-read-with-preview "Register: ")))
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
  :keys "f"
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
  :keys "d"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda () buffer-read-only)
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
  :keys ("c" "s")
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
  :keys "x"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda () buffer-read-only)
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
  :keys "g"
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
  :keys "e"
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
  :keys "z"
  (with-current-buffer (window-buffer window)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (select-window window)
      (goto-char pt))))

(conn-define-dispatch-action conn-dispatch-transpose (window pt thing-cmd _thing-arg)
  :description "Transpose"
  :keys "q"
  :filter (lambda () (unless buffer-read-only 'this))
  :window-predicate (lambda () buffer-read-only)
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

;;;;; Target Finders

(defun conn--dispatch-chars ()
  (cdr (conn--read-string-with-timeout-1 nil t)))

(defun conn--dispatch-all-things-1 (thing)
  (let ((last-point (point))
        ovs)
    (ignore-error scan-error
      (save-excursion
        (forward-thing thing -1)
        (while (and (/= (point) last-point)
                    (>= (point) (window-start)))
          (setq last-point (point))
          (unless (invisible-p (point))
            (push (point) ovs))
          (forward-thing thing -1))))
    (ignore-error scan-error
      (save-excursion
        (setq last-point (point))
        (forward-thing thing 1)
        (while (/= (point) last-point)
          (setq last-point (point))
          (unless (invisible-p (point))
            (pcase (bounds-of-thing-at-point thing)
              ((or 'nil (and `(,beg . ,_) (guard (> beg (window-end)))))
               (signal 'scan-error nil))
              ((and `(,beg . ,_)
                    (guard (or (null (car ovs))
                               (/= beg (car ovs)))))
               (push beg ovs))))
          (forward-thing thing 1))))
    (cl-loop for pt in ovs
             collect (conn--make-target-overlay pt 0 thing))))

(defun conn--dispatch-all-things (thing &optional all-windows)
  (cl-loop for win in (conn--get-dispatch-windows all-windows)
           nconc (with-selected-window win
                   (if-let* ((fn (alist-get thing conn-dispatch-all-things-collector-alist)))
                       (funcall fn)
                     (funcall conn-dispatch-all-things-collector-default thing)))))

(defun conn--dispatch-all-buttons (&optional all-windows)
  (let (ovs success)
    (unwind-protect
        (cl-loop for win in (conn--get-dispatch-windows all-windows)
                 do (with-selected-window win
                      (with-restriction (window-start) (window-end)
                        (save-excursion
                          (goto-char (point-min))
                          (when (button-at (point))
                            (push (conn--make-target-overlay (point) 1 nil) ovs))
                          (while (forward-button 1 nil nil t)
                            (push (conn--make-target-overlay (point) 1 nil) ovs)))))
                 finally return (progn (setq success t) ovs))
      (unless success (mapc #'delete-overlay ovs)))))

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

(defun conn--dispatch-things-with-prefix-1 (things prefix)
  (let ((case-fold-search (conn--string-no-upper-case-p prefix))
        (things (ensure-list things))
        ovs)
    (save-excursion
      (goto-char (window-end))
      (while (search-backward prefix (window-start) t)
        (unless (invisible-p (point))
          (dolist (thing things)
            (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point thing)))
              (when (and (eql (point) beg)
                         (conn--region-visible-p beg end)
                         (not (eql (point) (caar ovs))))
                (push (list (point) (length prefix) thing) ovs)))))))
    (cl-loop for ov in ovs
             collect (apply 'conn--make-target-overlay ov))))

(defun conn--dispatch-things-with-prefix (things prefix-length &optional all-windows)
  (let ((prefix "")
        ovs success)
    (conn--with-input-method
      (while (length< prefix prefix-length)
        (setq prefix (thread-last
                       (read-char (concat "char: " prefix) t)
                       (char-to-string)
                       (concat prefix)))))
    (unwind-protect
        (progn
          (dolist (win (conn--get-dispatch-windows all-windows))
            (setq ovs (nconc (with-selected-window win
                               (conn--dispatch-things-with-prefix-1 things prefix))
                             ovs)))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-columns ()
  (let ((col (current-column))
        (opoint (point))
        ovs success)
    (unwind-protect
        (progn
          (save-excursion
            (with-restriction (window-start) (window-end)
              (goto-char (point-min))
              (while (/= (point) (point-max))
                (when (and (>= col (window-hscroll))
                           (not (invisible-p (point)))
                           (not (ignore-errors (invisible-p (1- (point))))))
                  (move-to-column col)
                  (unless (= opoint (point))
                    (push (conn--make-target-overlay (point) 0) ovs)))
                (forward-line))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines ()
  (let (ovs success)
    (unwind-protect
        (progn
          (dolist (win (conn--get-dispatch-windows t))
            (with-selected-window win
              (save-excursion
                (with-restriction (window-start) (window-end)
                  (goto-char (point-min))
                  (when (and (bolp)
                             (<= (+ (point) (window-hscroll)) (line-end-position))
                             (goto-char (+ (point) (window-hscroll)))
                             (not (invisible-p (point))))
                    (push (conn--make-target-overlay (point) 0) ovs))
                  (while (/= (point) (point-max))
                    (forward-line)
                    (when (and (bolp)
                               (<= (+ (point) (window-hscroll))
                                   (line-end-position) (point-max))
                               (goto-char (+ (point) (window-hscroll)))
                               (not (invisible-p (point)))
                               (not (invisible-p (1- (point)))))
                      (push (conn--make-target-overlay (point) 0) ovs)))))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines-end ()
  (let (ovs success)
    (unwind-protect
        (progn
          (dolist (win (conn--get-dispatch-windows t))
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
                      (push (conn--make-target-overlay (point) 0) ovs)))))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-inner-lines (&optional end)
  (let (ovs success)
    (unwind-protect
        (progn
          (dolist (win (conn--get-dispatch-windows t))
            (with-selected-window win
              (save-excursion
                (with-restriction (window-start) (window-end)
                  (goto-char (point-min))
                  (when (and (bolp)
                             (progn
                               (if end
                                   (conn--end-of-inner-line-1)
                                 (back-to-indentation))
                               (not (eobp)))
                             (not (invisible-p (point))))
                    (push (conn--make-target-overlay (point) 0) ovs))
                  (while (/= (point) (point-max))
                    (forward-line)
                    (when (and (bolp)
                               (progn
                                 (if end
                                     (conn--end-of-inner-line-1)
                                   (back-to-indentation))
                                 (not (eobp)))
                               (not (invisible-p (point)))
                               (not (invisible-p (1- (point)))))
                      (push (conn--make-target-overlay (point) 0) ovs)))))))
          (setq success t)
          ovs)
      (unless success (mapc 'delete-overlay ovs)))))

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

(cl-defmethod register-command-info ((_command (eql conn-last-dispatch-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Copy dispatch to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))

(defun conn-last-dispatch-to-register (register)
  (interactive (list (register-read-with-preview "Dispatch to register: ")))
  (set-register register (conn--make-dispatch-register conn--last-dispatch-command)))

;;;;; Dispatch Commands

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
  (let ((conn-dispatch-window-predicates
         (if predicate
             (append conn-dispatch-window-predicates
                     (list predicate))
           conn-dispatch-window-predicates))
        target-ovs labels prefix window pt)
    (ignore-error quit
      (while
          (prog1 repeat
            (unwind-protect
                (setf target-ovs (thread-last
                                   (funcall finder)
                                   (seq-group-by (lambda (ov) (overlay-get ov 'window)))
                                   (seq-sort (lambda (a _) (eq (selected-window) (car a)))))
                      (alist-get (selected-window) target-ovs)
                      (seq-sort (lambda (a b)
                                  (< (abs (- (overlay-start a) (point)))
                                     (abs (- (overlay-start b) (point)))))
                                (alist-get (selected-window) target-ovs))
                      labels (conn--dispatch-labels
                              (or (funcall
                                   conn-label-string-generator
                                   (let ((sum 0))
                                     (dolist (p target-ovs sum)
                                       (setq sum (+ sum (length (cdr p)))))))
                                  (user-error "No matching %s"
                                              (or (ignore-errors
                                                    (get thing-cmd :conn-command-thing))
                                                  "candidates")))
                              target-ovs)
                      prefix (conn-label-select labels)
                      window (overlay-get prefix 'window)
                      pt (overlay-start prefix)
                      conn-this-command-thing (or (overlay-get prefix 'thing)
                                                  (ignore-errors
                                                    (get thing-cmd :conn-command-thing))))
              (pcase-dolist (`(_ . ,ovs) target-ovs)
                (mapc #'delete-overlay ovs))
              (mapc #'conn-label-delete labels))
            (apply action window pt thing-cmd thing-arg action-args))
        (undo-boundary)))))

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

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-dispatch-on-things
   nil nil
   (apply-partially 'conn--dispatch-all-buttons t)
   (lambda (win pt _thing)
     (select-window win)
     (push-button pt))
   nil))

(defun conn--dispatch-isearch-matches ()
  (with-restriction (window-start) (window-end)
    (cl-loop for (beg . end) in (conn--isearch-matches)
             collect (conn--make-target-overlay beg (- end beg)))))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let* ((prefix-ovs `((,(selected-window) . ,(conn--dispatch-isearch-matches))))
         (count (length (cdar prefix-ovs)))
         (labels (conn--dispatch-labels
                  (funcall conn-label-string-generator count)
                  prefix-ovs)))
    (unwind-protect
        (let* ((prefix (conn-label-select labels))
               (pt (overlay-start prefix)))
          (isearch-done)
          (goto-char pt))
      (mapc #'conn-label-delete labels)
      (mapc #'delete-overlay (cadr prefix-ovs)))))


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


;;;; Narrow Ring

(cl-defstruct (conn-narrow-register (:constructor %conn--make-narrow-register))
  (narrow-ring nil))

(defun conn--make-narrow-register ()
  (%conn--make-narrow-register
   :narrow-ring
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

(cl-defmethod register-command-info ((_command (eql conn-narrow-ring-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Copy narrow ring to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))

(defun conn-narrow-ring-to-register (register)
  "Store narrow ring in REGISTER."
  (interactive (list (register-read-with-preview "Narrow ring to register: ")))
  (set-register register (conn--make-narrow-register)))

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

(defun conn-thing-to-narrow-ring (thing-cmd thing-arg)
  (interactive (conn-read-thing-mover "Thing" nil t))
  (let ((regions (conn-bounds-of-command thing-cmd thing-arg)))
    (mapc (pcase-lambda (`(,beg . ,end))
            (conn--narrow-ring-record beg end))
          (or (conn--merge-regions (cdr regions) t)
              (list (car regions)))))
  (message "Added %s to narrow ring" (get thing-cmd :conn-command-thing)))

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


;;;; Commands

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
                       (query-replace-read-from prompt regexp-flag)))
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
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover "Thing Mover" nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (thread-last
                         (current-local-map)
                         (make-composed-keymap conn-replace-map)
                         (use-local-map)))
                   (conn--replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg
                                (if (eq current-prefix-arg '-) " backward" " word")
                              ""))
                    nil (or (cdr regions) regions) nil))))
     (append (list thing-mover arg) common)))
  (save-excursion
    (pcase-let ((`((,beg . ,end) . ,regions)
                 (or (conn-last-bounds-of-command)
                     (conn-bounds-of-command thing-mover arg))))
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
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn-read-thing-mover "Thing Mover" nil t))
                (regions (conn-bounds-of-command thing-mover arg))
                (common
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (thread-last
                         (current-local-map)
                         (make-composed-keymap conn-replace-map)
                         (use-local-map)))
                   (conn--replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg
                                (if (eq current-prefix-arg '-) " backward" " word")
                              ""))
                    t (or (cdr regions) regions) nil))))
     (append (list thing-mover arg) common)))
  (save-excursion
    (pcase-let ((`((,beg . ,end) . ,regions)
                 (or (conn-last-bounds-of-command)
                     (conn-bounds-of-command thing-mover arg))))
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

(cl-defmethod register-command-info ((_command (eql conn-command-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Command to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))

(defun conn-command-to-register (register arg)
  "Store command in REGISTER."
  (interactive
   (list (register-read-with-preview "Command to register: ")
         (abs (prefix-numeric-value current-prefix-arg))))
  (set-register
   register
   ;; TODO: use completing-read
   (make-conn-command-register
    :command (let* ((elt (nth arg command-history))
                    (print-level nil)
                    (minibuffer-history-position arg)
                    (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
               (unwind-protect
                   (read-from-minibuffer
                    "Command: " (prin1-to-string elt) read-expression-map t
                    (cons 'command-history arg))
                 (when (stringp (car command-history))
                   (pop command-history)))))))

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
                                         (conn-tab-register-cookie val))))
                       (thread-first
                         (nth index (funcall tab-bar-tabs-function))
                         (conn--thread tab
                             (if (eq (car tab) 'current-tab)
                                 (propertize "*CURRENT TAB*" 'face 'error)
                               (alist-get 'name tab)))))
                   "on another frame"))))

(cl-defmethod register-command-info ((_command (eql conn-tab-to-register)))
  (make-register-preview-info
   :types '(all)
   :msg "Tab to register `%s'"
   :act 'set
   :noconfirm (memq register-use-preview '(nil never))
   :smatch t))

(defun conn-tab-to-register (register)
  "Store tab in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-tab-register)))

;;;;; Isearch Commands

(defun conn-isearch-yank-region ()
  (interactive)
  (isearch-yank-internal (lambda () (mark t))))

(defun conn-isearch-open-recursive-edit ()
  (interactive)
  (thread-first
    (recursive-edit)
    (with-isearch-suspended)
    (save-selected-window)))

(defun conn-isearch-forward-in-thing (thing-cmd thing-arg)
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

;;;;; Editing Commands

(defun conn-forward-defun (N)
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(defun conn-transpose-regions (mover arg)
  (interactive
   (let ((conn--thing-overriding-maps
          (list (define-keymap "k" 'forward-line))))
     (conn-read-thing-mover
      "Mover"
      (when current-prefix-arg
        (prefix-numeric-value current-prefix-arg))
      t)))
  (deactivate-mark t)
  (pcase mover
    ('recursive-edit
     (let ((beg (region-beginning))
           (end (region-end))
           (buf (current-buffer)))
       (setq buffer-read-only t)
       (unwind-protect
           (recursive-edit)
         (setq buffer-read-only nil))
       (if (eq buf (current-buffer))
           (transpose-regions beg end (region-beginning) (region-end))
         (let ((str1 (filter-buffer-substring (region-beginning) (region-end) t))
               str2)
           (with-current-buffer buf
             (setq str2 (filter-buffer-substring beg end t))
             (insert str1))
           (insert str2)))))
    ((let 0 arg)
     (pcase-let* ((thing (get mover :conn-command-thing))
                  (`(,beg1 . ,end1) (if (region-active-p)
                                        (cons (region-beginning) (region-end))
                                      (bounds-of-thing-at-point thing)))
                  (`(,beg2 . ,end2) (save-excursion
                                      (goto-char (mark t))
                                      (bounds-of-thing-at-point thing))))
       (transpose-regions beg1 end1 beg2 end2)))
    (_
     (transpose-subr (apply-partially 'forward-thing (get mover :conn-command-thing))
                     (prefix-numeric-value arg)))))

(defun conn-open-line (arg)
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent))

(defun conn-open-line-above (arg)
  (interactive "p")
  (forward-line (- (1- arg)))
  (move-beginning-of-line nil)
  (insert "\n")
  (forward-line -1)
  ;; FIXME: see crux smart open line
  (indent-according-to-mode))

(defun conn-comment-or-uncomment-thing (thing-mover arg)
  (interactive (conn-read-thing-mover "Thing Mover"))
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
    (if (comment-only-p beg end)
        (uncomment-region beg end)
      (let ((comment-empty-lines t))
        (comment-region beg end)))))

(defun conn-backward-symbol (arg)
  "`forward-symbol' in reverse."
  (interactive "p")
  (forward-symbol (- arg)))

(defun conn-shell-command-on-region (&optional arg)
  "Like `shell-command-on-region' but inverts the meaning of ARG."
  (interactive "P")
  (let ((current-prefix-arg (not arg)))
    (call-interactively 'shell-command-on-region)))

(defun conn-yank-replace-rectangle ()
  (interactive)
  (save-mark-and-excursion
    (unless (>= (mark t) (point))
      (conn-exchange-mark-command))
    (delete-rectangle (region-beginning) (region-end))
    (yank-rectangle)))

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
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (conn-recenter-on-region)))

(defvar-local conn-mark-ring-right nil)
(defvar-local conn-mark-ring-left nil)

(defun conn--push-mark-ring-right (location)
  (unless (or (seq-contains-p conn-mark-ring-right location #'=)
              (seq-contains-p conn-mark-ring-left location #'=))
    (let ((old (nth mark-ring-max conn-mark-ring-right)))
      (setq conn-mark-ring-right
            (cons (pcase location
                    ((pred markerp) (copy-marker location))
                    ((pred numberp) (conn--create-marker location)))
                  (take (1- mark-ring-max) conn-mark-ring-right)))
      (when (and old (not (memq old conn-mark-ring-right)))
        (set-marker old nil)))))

(defun conn--push-mark-ring-left (location)
  (unless (or (seq-contains-p conn-mark-ring-left location #'=)
              (seq-contains-p conn-mark-ring-right location #'=))
    (let ((old (nth mark-ring-max conn-mark-ring-left)))
      (setq conn-mark-ring-left
            (cons (pcase location
                    ((pred markerp) (copy-marker location))
                    ((pred numberp) (conn--create-marker location)))
                  (take (1- mark-ring-max) conn-mark-ring-left)))
      (when (and old (not (memq old conn-mark-ring-left)))
        (set-marker old nil)))))

(defun conn-mark-ring-backward ()
  "Like `pop-to-mark-command' but uses `conn-mark-ring-right'.
Unfortunately conn adds many uninteresting marks to the `mark-ring',
so `conn-mark-ring-right' and the functions `conn-mark-ring-backward' and
`conn-mark-ring-forward' are provided which attempt to filter out
uninstersting marks."
  (interactive)
  (cond ((null (mark t))
         (user-error "No mark set in this buffer"))
        ((null conn-mark-ring-right)
         (user-error "No marks to unpop"))
        ((or conn--ephemeral-mark
             (= (point) (mark t)))
         (conn--push-mark-ring-left (point))
         (let ((conn-mark-ring-right conn-mark-ring-right))
           (push-mark (car conn-mark-ring-right)))
         (pop conn-mark-ring-right)
         (goto-char (mark t)))
        (t
         (conn--push-mark-ring-left (point))
         (goto-char (mark t))))
  (deactivate-mark))

(defun conn-mark-ring-forward ()
  "Like `pop-to-mark-command' in reverse but uses `conn-mark-ring-right'.
Unfortunately conn adds many uninteresting marks to the `mark-ring',
so `conn-mark-ring-right' and the functions `conn-mark-ring-backward' and
`conn-mark-ring-forward' are provided which attempt to filter out
uninstersting marks."
  (interactive)
  (cond ((null (mark t))
         (user-error "No mark set in this buffer"))
        ((null conn-mark-ring-left)
         (user-error "No marks to unpop"))
        ((= (point) (mark t))
         (push-mark (pop conn-mark-ring-left))
         (goto-char (mark t)))
        (t
         (conn--push-mark-ring-right (point))
         (goto-char (mark t))))
  (deactivate-mark))

(defvar-local conn--minibuffer-initial-region nil)

(defun conn--yank-region-to-minibuffer-hook ()
  (setq conn--minibuffer-initial-region
        (with-minibuffer-selected-window
          (ignore-errors (cons (region-beginning) (region-end))))))

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

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
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content))

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

(defun conn-append-region (beg end &optional register kill-flag)
  "Appne region from BEG to END to most recent kill.
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
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Prepend to register: "))))
  (conn-append-region beg end register t))

(defun conn-kill-prepend-region (beg end &optional register)
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
  (pcase-let ((`((,beg . ,end) . ,_) (conn-bounds-of-command thing-mover arg)))
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

(defun conn-backward-line (N)
  "`forward-line' by N but backward."
  (interactive "p")
  (forward-line (- N)))

(defun conn-backward-whitespace (N)
  "`forward-whitespace' by N but backward."
  (interactive "p")
  (forward-whitespace (- N)))

(defun conn-set-register-seperator (string)
  "Set `register-seperator' register to string STRING."
  (interactive
   (list (read-string "Separator: "
                      (let ((reg (get-register register-separator)))
                        (when (stringp reg) reg))
                      conn--seperator-history nil t)))
  (set-register register-separator string))

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
      (funcall (key-binding conn-delete-region-keys t) start end))
    (funcall (key-binding conn-yank-keys t))))

(defun conn--end-of-inner-line-1 ()
  (goto-char (line-end-position))
  (when-let* ((cs (and (conn--point-in-comment-p)
                       (save-excursion
                         (comment-search-backward
                          (line-beginning-position) t)))))
    (goto-char cs))
  (skip-chars-backward " \t" (line-beginning-position))
  (when (bolp) (skip-chars-forward " \t" (line-end-position))))

(defun conn-end-of-inner-line (&optional N)
  "Go to point after the last non-whitespace or comment character in line.
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
  "Go to first non-whitespace character in line.
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

(defun conn-duplicate-region (beg end arg)
  (interactive (list (region-beginning)
                     (region-end)
                     (prefix-numeric-value current-prefix-arg)))
  (if (use-region-p)
      (duplicate-dwim)
    (let ((end (set-marker (make-marker) end)))
      (unwind-protect
          (dotimes (_ arg)
            (conn--duplicate-region-1 beg end))
        (goto-char end)
        (set-marker end nil)
        (indent-region (region-beginning) (region-end))))))

(defun conn-duplicate-thing (thing-mover thing-arg N)
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
  (interactive (append (conn-read-thing-mover "Thing Mover" nil t)
                       (list (prefix-numeric-value current-prefix-arg))))
  (pcase (conn-bounds-of-command thing-mover thing-arg)
    (`((,beg . ,end) . ,_)
     (pcase-let* ((offset (- (point) end))
                  (mark-offset (- (point) (mark t)))
                  (region (buffer-substring-no-properties beg end)))
       (goto-char end)
       (comment-or-uncomment-region beg end)
       (setq end (if (bolp) (point) (line-end-position)))
       (dotimes (_ N)
         (goto-char end)
         (newline)
         (insert region)
         (setq end (point)))
       (goto-char (+ (point) offset))
       (conn--push-ephemeral-mark (- (point) mark-offset))))))

;;;;; Window Commands

(defun conn-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun conn-other-place-prefix ()
  (interactive)
  (pcase (car (read-multiple-choice
               "Place:"
               '((?w "window")
                 (?f "frame")
                 (?t "tab")
                 (?p "prompt")
                 (?c "current window"))))
    (?w (other-window-prefix))
    (?f (other-frame-prefix))
    (?t (other-tab-prefix))
    (?p (conn-other-window-prompt-prefix))
    (?c (conn-this-window-prefix))))

(defun conn-other-window-prompt-prefix ()
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (cons (conn--prompt-for-window (conn--get-windows nil 'nomini)) 'reuse))))

(defun conn-this-window-prefix ()
  (interactive)
  (display-buffer-override-next-command
   'display-buffer-same-window
   nil "[current-window]")
  (message "Display next command buffer in current window…"))

(defun conn-transpose-window (window)
  "Prompt for window and swap current window and other window."
  (interactive
   (list (conn--prompt-for-window
          (remove (selected-window) (conn--get-windows nil 'nomini 'visible)))))
  (if window
      (window-swap-states nil window)
    (user-error "No other visible windows")))

(defun conn-throw-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-prev-buffer)
    (save-selected-window
      (display-buffer
       buf
       (lambda (_ _)
         (cons (conn--prompt-for-window (conn--get-windows nil 'nomini))
               'reuse))))))

(defun conn-yank-window (window)
  (interactive
   (list (conn--prompt-for-window
          (remove (selected-window) (conn--get-windows nil 'nomini 'visible)))))
  (if window
      (save-selected-window (window-swap-states nil window))
    (user-error "No other visible windows")))

;;;;; Transition Functions

(defun conn-change-whole-line (&optional arg)
  (interactive "P")
  (kill-whole-line arg)
  (open-line 1)
  (indent-according-to-mode)
  (conn-enter-state 'conn-emacs-state))

(defun conn-change-line ()
  (interactive)
  (call-interactively (key-binding conn-kill-line-keys t))
  (conn-enter-state 'conn-emacs-state))

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
  (conn-enter-state 'conn-emacs-state))

(defun conn-emacs-state-open-line (&optional arg)
  "Open line and enter `conn-emacs-state'.

If ARG is non-nil move down ARG lines before opening line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent)
  (conn-enter-state 'conn-emacs-state))

(defun conn-emacs-state-overwrite (&optional arg)
  "Enter emacs state in `overwrite-mode'.
`overwrite-mode' will be turned off when when emacs state is exited.
If ARG is non-nil enter emacs state in `binary-overwrite-mode' instead."
  (interactive "P")
  (conn-enter-state 'conn-emacs-state)
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
         (conn-enter-state 'conn-emacs-state))
        (t
         (funcall (conn--without-conn-maps
                    (key-binding conn-delete-region-keys t))
                  start end)
         (conn-enter-state 'conn-emacs-state))))

(defun conn-emacs-state-eol (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (end-of-line N)
  (conn-enter-state 'conn-emacs-state))

(defun conn-emacs-state-bol (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (beginning-of-line N)
  (conn-enter-state 'conn-emacs-state))

(defun conn-emacs-state-eoil (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-end-of-inner-line N)
  (conn-enter-state 'conn-emacs-state))

(defun conn-emacs-state-boil (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-beginning-of-inner-line N)
  (conn-enter-state 'conn-emacs-state))


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

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--previous-scroll-conservatively)
(defvar conn--wincontrol-help)
(defvar conn--wincontrol-help-format)
(defvar conn--wincontrol-prev-eldoc-msg-fn)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)

(defconst conn--wincontrol-window-format-1
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Window: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[tab-bar-history-back] \\[tab-bar-history-forward]: undo/redo"
   "\n"
   "\\[enlarge-window] "
   "\\[shrink-window] "
   "\\[enlarge-window-horizontally] "
   "\\[shrink-window-horizontally]: "
   "heighten shorten widen narrow; "
   "\\[previous-buffer] \\[next-buffer] "
   "\\[conn-wincontrol-previous-window] \\[conn-wincontrol-next-window]"
   ": prev/next buffer/win; "
   "\\[conn-goto-window]: goto win"
   "\n"
   "\\[delete-window] \\[delete-other-windows]: delete win/other; "
   "\\[conn-wincontrol-split-vertically] \\[conn-wincontrol-split-right]: "
   "split vert/right; "
   "\\[conn-transpose-window] \\[conn-throw-buffer] \\[conn-yank-window]: "
   "transpose/throw/yank; "
   "\\[conn-wincontrol-mru-window]: last win"))

(defconst conn--wincontrol-window-format-2
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Window: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[conn-wincontrol-zoom-in] \\[conn-wincontrol-zoom-out]: zoom; "
   "\\[quit-window]: quit win"
   "\n"
   "\\[conn-wincontrol-other-window-scroll-down] \\[conn-wincontrol-other-window-scroll-up]"
   ": scroll other; "
   "\\[conn-wincontrol-isearch-other-window] \\[conn-wincontrol-isearch-other-window-backward]"
   ": isearch other; "
   "\\[conn-wincontrol-isearch] \\[conn-wincontrol-isearch-backward]"
   ": isearch; "
   "\\[unbury-buffer] \\[bury-buffer]: un/bury"
   "\n"
   "\\[shrink-window-if-larger-than-buffer]: shrink win to buf; "
   "\\[balance-windows] \\[maximize-window]: balance/max; "
   "\\[conn-wincontrol-maximize-vertically] \\[conn-wincontrol-maximize-horizontally]: "
   "max vert/horiz; "
   "\\[conn-register-load] \\[window-configuration-to-register]: load/store"))

(defconst conn--wincontrol-tab-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Tab: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[conn-tab-to-register]: store; "
   "\\[tab-switch]: switch"
   "\n"
   "\\[tab-bar-move-window-to-tab]: win to new tab; "
   "\\[tab-previous] \\[tab-next]: next/prev; "
   "\\[tab-new] \\[tab-bar-duplicate-tab] \\[tab-close]: "
   "new/clone/kill; "
   "\\[tab-bar-detach-tab]: tear off"))

(defconst conn--wincontrol-frame-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Frame: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit"
   "\n"
   "\\[other-frame]: other; "
   "\\[clone-frame]: clone; "
   "\\[undelete-frame]: undelete; "
   "\\[tear-off-window]: tear off; "
   "\\[toggle-frame-fullscreen]: fullscreen"
   "\n"
   "\\[conn-wincontrol-reverse] \\[conn-wincontrol-reflect]: reverse/reflect; "
   "\\[make-frame-command]: iconify/create; "
   "\\[delete-frame] \\[delete-other-frames]: delete/other"))

(defconst conn--wincontrol-simple-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl: " 'face 'minibuffer-prompt)
   "arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[conn-wincontrol-scroll-up] "
   "\\[conn-wincontrol-scroll-down]: "
   "scroll"))

;; TODO: what should be done with the arg stuff?
;;       only persist on certain commands maybe?
(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "C-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "C-w" 'conn-wincontrol-backward-delete-arg
  "C-d" 'conn-wincontrol-forward-delete-arg
  "C-0" 'delete-window
  "C-1" 'delete-other-windows
  "C-2" 'split-window-below
  "C-3" 'split-window-right
  "C-8" 'conn-tab-to-register
  "C-9" 'tab-close
  "C-]" 'conn-wincontrol-abort
  "C-M-0" 'kill-buffer-and-window
  "C-M-d" 'delete-other-frames
  "M-/" 'undelete-frame
  "M-o" 'other-frame
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "`" 'conn-wincontrol-next-window
  "C-u" 'conn-wincontrol-universal-arg
  "-" 'conn-wincontrol-invert-argument
  "," 'conn-wincontrol-digit-argument-reset
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
  "<" 'conn-wincontrol-reverse
  ">" 'conn-wincontrol-reflect
  "=" 'balance-windows
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
  "." 'conn-wincontrol-maximize-horizontally
  ";" 'conn-wincontrol-exit-to-initial-win
  "b" 'conn-tab-to-register
  "C" 'tab-bar-duplicate-tab
  "c" (conn-remap-key (key-parse "C-c"))
  "d" 'delete-window
  "e" 'conn-wincontrol-exit
  "C-g" 'conn-wincontrol-exit
  "F" 'toggle-frame-fullscreen
  "f" 'conn-goto-window
  "g" 'delete-other-windows
  "C-f" 'conn-wincontrol-help
  "C-b" 'conn-wincontrol-help-backward
  "H" 'maximize-window
  "h" 'enlarge-window
  "I" 'tab-new
  "i" 'tab-next
  "j" 'previous-buffer
  "J" 'bury-buffer
  "K" 'tab-close
  "k" 'tab-previous
  "l" 'next-buffer
  "L" 'unbury-buffer
  "M" 'tab-bar-move-window-to-tab
  "m" 'conn-wincontrol-maximize-vertically
  "n" 'shrink-window-horizontally
  "N" 'tab-bar-new-tab
  "o" 'conn-wincontrol-next-window
  "O" 'tear-off-window
  "p" 'conn-register-load
  "P" 'window-configuration-to-register
  "x" (conn-remap-key (key-parse "C-x"))
  "X" 'conn-wincontrol-mru-window
  "r" 'conn-wincontrol-split-right
  "R" 'conn-wincontrol-isearch-other-window-backward
  "s" 'shrink-window
  "S" 'conn-wincontrol-isearch-other-window
  "t" 'tab-switch
  "T" 'conn-throw-buffer
  "u" 'conn-wincontrol-previous-window
  "U" 'tab-bar-detach-tab
  "v" 'conn-wincontrol-split-vertically
  "w" 'enlarge-window-horizontally
  "q" 'conn-transpose-window
  "Q" 'quit-window
  "y" 'conn-yank-window
  "z" 'conn-wincontrol-zoom-out
  "Z" 'conn-wincontrol-zoom-in)

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
    (message conn--wincontrol-help-format
             (format (if conn--wincontrol-arg "%s%s" "[%s1]")
                     (if (= conn--wincontrol-arg-sign -1) "-" "")
                     conn--wincontrol-arg))))

(defun conn--wincontrol-setup (&optional preserve-state)
  (conn--destroy-mode-line-labels)
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
          conn--wincontrol-help conn-wincontrol-initial-help
          conn--wincontrol-arg-sign 1
          conn--wincontrol-initial-window (selected-window)
          conn--wincontrol-initial-winconf (current-window-configuration)))
  (conn-wincontrol-help)
  ;; TODO: make inverse-video a custom option
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line t))
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (conn--destroy-mode-line-labels)
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (unless executing-kbd-macro
    (set-face-inverse-video 'mode-line nil)))

(defun conn-wincontrol-one-command ()
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

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn--wincontrol-setup t)))

(defun conn-wincontrol-universal-arg ()
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
  "Invert wincontrol prefix arg."
  (interactive)
  (setq conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to 0."
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

(defun conn-wincontrol-help (&optional interactive)
  "Cycle to the next `conn-wincontrol-mode' help message."
  (interactive (list t))
  (when interactive
    (setq conn--wincontrol-help (pcase conn--wincontrol-help
                                  ('window-1 'window-2)
                                  ('window-2 'tab)
                                  ('tab 'frame)
                                  ('frame nil)
                                  (_ 'window-1))))
  (setq conn--wincontrol-help-format
        (substitute-command-keys
         (pcase conn--wincontrol-help
           ('window-1 conn--wincontrol-window-format-1)
           ('window-2 conn--wincontrol-window-format-2)
           ('tab conn--wincontrol-tab-format)
           ('frame conn--wincontrol-frame-format)
           (_ conn--wincontrol-simple-format)))))

(defun conn-wincontrol-help-backward (&optional interactive)
  "Cycle to the next `conn-wincontrol-mode' help message."
  (interactive (list t))
  (when interactive
    (setq conn--wincontrol-help (pcase conn--wincontrol-help
                                  ('window-1 nil)
                                  ('window-2 'window-1)
                                  ('tab 'window-2)
                                  ('frame 'tab)
                                  (_ 'frame))))
  (setq conn--wincontrol-help-format
        (substitute-command-keys
         (pcase conn--wincontrol-help
           ('window-1 conn--wincontrol-window-format-1)
           ('window-2 conn--wincontrol-window-format-2)
           ('tab conn--wincontrol-tab-format)
           ('frame conn--wincontrol-frame-format)
           (_ conn--wincontrol-simple-format)))))

(defun conn-wincontrol-isearch (arg)
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-forward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-backward (arg)
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-backward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window (arg)
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-forward arg))
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window-backward (arg)
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-backward arg))
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-next-window ()
  (interactive)
  (other-window 1))

(defun conn-wincontrol-previous-window ()
  (interactive)
  (other-window -1))

(defun conn-goto-window (window)
  (interactive
   (list (conn--prompt-for-window
          (remove (selected-window) (conn--get-windows nil 'nomini 'visible)))))
  (select-window window))

(defun conn-wincontrol-zoom-in (arg)
  (interactive "p")
  (text-scale-increase arg))

(defun conn-wincontrol-zoom-out (arg)
  (interactive "p")
  (text-scale-decrease arg))

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
  (interactive)
  (windmove-left))

(defun conn-wincontrol-quit-other-window-for-scrolling ()
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))

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

(defun conn-wincontrol-mru-window ()
  (interactive)
  (when-let* ((mru (get-mru-window 0 nil t t)))
    (select-window mru)))

(defun conn-wincontrol-split-vertically ()
  "Split window vertically.
Uses `split-window-vertically'."
  (interactive)
  (split-window-vertically))

(defun conn-wincontrol-split-right ()
  "Split window vertically.
Uses `split-window-right'."
  (interactive)
  (split-window-right))

(defun conn-wincontrol-maximize-horizontally ()
  (interactive)
  (conn-wincontrol-maximize-vertically t))

(defun conn-wincontrol-maximize-vertically (&optional horizontal)
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

(defun conn--wincontrol-split-window-state (state)
  (let (params windows)
    (dolist (elem state)
      (if (memq (car-safe elem) '(vc hc leaf))
          (push elem windows)
        (push elem params)))
    (cons (reverse params) (reverse windows))))

;; window-x can't come soon enough
(defun conn--wincontrol-reflect-window (state)
  (pcase-let* ((`(,params . ,windows)
                (conn--wincontrol-split-window-state state))
               (height  (alist-get 'normal-height params))
               (width   (alist-get 'normal-width  params))
               (pheight (* (alist-get 'pixel-width  params) (/ height width)))
               (theight (* (alist-get 'total-width  params) (/ height width)))
               (pwidth  (* (alist-get 'pixel-height params) (/ height width)))
               (twidth  (* (alist-get 'total-height params) (/ height width))))
    (setf (alist-get 'normal-width  params) height
          (alist-get 'normal-height params) width
          (alist-get 'pixel-height  params) pheight
          (alist-get 'pixel-width   params) pwidth
          (alist-get 'total-height  params) theight
          (alist-get 'total-width   params) twidth)
    (append (cl-loop for elem in params collect (pcase elem
                                                  ('vc 'hc)
                                                  ('hc 'vc)
                                                  (_   elem)))
            (mapcar #'conn--wincontrol-reflect-window windows))))

(defun conn--wincontrol-reverse-window (state &optional recursive)
  (pcase-let* ((`(,params . ,windows)
                (conn--wincontrol-split-window-state state)))
    (when (length> windows 1)
      (setf (alist-get 'last (cdar windows)) t
            windows (reverse windows)
            (car windows) (assq-delete-all 'last (car windows))))
    (append params
            (if recursive
                (cl-loop for win in windows
                         collect (conn--wincontrol-reverse-window win t))
              windows))))

(defun conn-wincontrol-reverse ()
  "Reverse order of windows in ARGth parent window.
When ARG is nil the root window is used."
  (interactive)
  (let ((window (window-main-window)))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reverse-window)
      (window-state-put window))))

(defun conn-wincontrol-reflect ()
  "Rotate all window arrangements within ARGth parent window of `selected-window'.
When ARG is nil the root window is used."
  (interactive)
  (let ((window (window-main-window)))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reflect-window)
      (window-state-put window))))


;;;; Keymaps

(defvar-keymap conn-list-movement-repeat-map
  :repeat t
  ")" 'forward-list
  "(" 'backward-list)

(defvar-keymap conn-reb-navigation-repeat-map
  :repeat t
  "C-s" 'reb-next-match
  "C-r" 'reb-prev-match)

(dolist (state '(conn-state conn-emacs-state))
  (keymap-set (conn-get-mode-map state 'occur-mode) "C-c e" 'occur-edit-mode))

(dolist (state '(conn-state conn-emacs-state))
  (keymap-set (conn-get-mode-map state 'occur-edit-mode) "C-c e" 'occur-cease-edit))

(defvar-keymap conn-region-map
  :prefix 'conn-region-map
  "\\" 'conn-kapply-on-region-prefix
  "RET" 'whitespace-cleanup
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
  "j" 'conn-join-lines-in-region
  "I" 'indent-rigidly
  "p" 'conn-sort-prefix
  "o" 'conn-occur-region
  "v" 'vc-region-history
  "s" 'conn-isearch-region-forward
  "r" 'conn-isearch-region-backward
  "y" 'yank-rectangle
  "q" 'conn-replace-in-thing
  "w" 'conn-regexp-replace-in-thing
  "DEL" 'clear-rectangle
  "n" 'conn-narrow-to-region
  "m" 'conn-narrow-indirect-to-region)

(when (version<= "30" emacs-version)
  (keymap-set conn-region-map "W" 'replace-regexp-as-diff)
  (keymap-set conn-region-map "Q" 'multi-file-replace-regexp-as-diff))

(defvar-keymap conn-window-resize-map
  :repeat t
  :prefix 'conn-window-resize-map
  "f" 'enlarge-window
  "d" 'shrink-window
  "j" 'shrink-window-horizontally
  "k" 'enlarge-window-horizontally)

(defvar-keymap conn-indent-rigidly-map
  "l" 'indent-rigidly-right
  "j" 'indent-rigidly-left
  "L" 'indent-rigidly-right-to-tab-stop
  "J" 'indent-rigidly-left-to-tab-stop)

(define-keymap
  :keymap isearch-mode-map
  "M-Y" 'conn-isearch-yank-region
  "M-<return>" 'conn-isearch-exit-and-mark
  "M-RET" 'conn-isearch-exit-and-mark
  "M-\\" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch
  "C-'" 'conn-isearch-open-recursive-edit)

(define-keymap
  :keymap (conn-get-mode-map 'conn-movement-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

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

(defvar-keymap conn-pop-mark-repeat-map
  :repeat t
  "o" 'conn-mark-ring-backward
  "u" 'conn-mark-ring-forward)

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "`" 'other-window)

(defvar-keymap conn-goto-map
  "l" 'pop-global-mark
  "k" 'goto-line
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'rectangle-mark-mode)
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

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

(defvar-keymap conn-edit-map
  :prefix 'conn-edit-map
  "m" 'conn-narrow-indirect-to-thing
  "n" 'conn-narrow-to-thing
  "j" 'conn-join-lines-in-thing
  "F" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "DEL" 'conn-change-whole-line
  "," 'clone-indirect-buffer
  "h" 'conn-change-line
  "i" 'conn-emacs-state-open-line-above
  "k" 'conn-emacs-state-open-line
  "o" 'conn-emacs-state-eoil
  "e" 'conn-emacs-state-eol
  "u" 'conn-emacs-state-boil
  "a" 'conn-emacs-state-bol
  "t" 'conn-emacs-state-overwrite
  "b" 'conn-emacs-state-overwrite-binary
  "v" 'conn-region-to-narrow-ring
  "f" 'conn-thing-to-narrow-ring
  "x" 'conn-narrow-ring-prefix
  "d" 'duplicate-dwim
  "w j" 'conn-kill-prepend-region
  "w l" 'conn-kill-append-region
  "c j" 'conn-append-region
  "c l" 'conn-append-region
  "c v" 'conn-copy-thing
  "c i" 'copy-from-above-command
  "y" 'yank-in-context)

(define-keymap
  :keymap conn-movement-state-map
  "e" 'conn-emacs-state
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
  "J" 'conn-beginning-of-inner-line
  "j" `(menu-item
        ""
        nil
        :filter ,(lambda (&rest _)
                   (let ((binding (key-binding conn-backward-char-keys t)))
                     (if (eq binding 'backward-char)
                         'conn-backward-char
                       binding))))
  "K" (conn-remap-key conn-forward-paragraph-keys)
  "k" (conn-remap-key conn-next-line-keys)
  "L" 'conn-end-of-inner-line
  "l" `(menu-item
        ""
        nil
        :filter ,(lambda (&rest _)
                   (let ((binding (key-binding conn-forward-char-keys t)))
                     (if (eq binding 'forward-char)
                         'conn-forward-char
                       binding))))
  "M" (conn-remap-key conn-end-of-defun-keys)
  "m" (conn-remap-key conn-forward-sexp-keys)
  "N" (conn-remap-key conn-beginning-of-defun-keys)
  "n" (conn-remap-key conn-backward-sexp-keys))

(define-keymap
  :keymap conn-menu-state-map
  "s" (conn-remap-keymap (key-parse "M-s"))
  "g" (conn-remap-keymap (key-parse "M-g"))
  "c" (conn-remap-key (key-parse "C-c"))
  "x" (conn-remap-key (key-parse "C-x"))
  "C-4" (conn-remap-key (key-parse "C-x 4"))
  "C-5" (conn-remap-key (key-parse "C-x 5")))

(defvar-keymap conn-other-buffer-repeat-map
  :repeat t
  "&" 'conn-other-buffer)

(define-keymap
  :keymap conn-state-map
  "P" 'conn-region-case-prefix
  "&" 'conn-other-buffer
  "\"" 'conn-yank-window
  "e" 'conn-emacs-state
  "E" 'conn-dispatch-on-buttons
  "t" 'conn-change
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
  "C-+" 'maximize-window
  "C--" 'shrink-window-if-larger-than-buffer
  "C-0" 'delete-window
  "C-1" 'delete-other-windows
  "C-2" 'split-window-below
  "C-3" 'split-window-right
  "C-8" 'conn-tab-to-register
  "C-9" 'quit-window
  "C-=" 'balance-windows
  "C-M-0" 'kill-buffer-and-window
  "C-M-l" 'conn-recenter-on-region
  "C-M-S-l" 'conn-recenter-on-region-other-window
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'conn-edit-map
  "C" 'conn-copy-region
  "d" (conn-remap-key conn-delete-char-keys)
  "f" 'conn-dispatch-on-things
  "F" 'conn-repeat-last-dispatch
  "h" 'conn-expand
  "H" 'conn-mark-thing-map
  "p" 'conn-register-prefix
  "q" 'conn-transpose-regions
  "r" 'conn-region-map
  "R" 'conn-rectangle-mark
  "Q" 'conn-transpose-window
  "T" 'conn-throw-buffer
  "V" 'conn-narrow-to-thing
  "v" 'conn-toggle-mark-command
  "w" 'conn-kill-region
  "W" 'widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" (conn-remap-key conn-yank-keys)
  "z" 'conn-exchange-mark-command)

(define-keymap
  :keymap conn-org-edit-state-map
  "e" 'conn-emacs-state
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

(defvar-keymap conn-global-map
  ;; "M-j" 'conn-open-line-and-indent
  ;; "C-o" 'conn-open-line-above
  ;; "M-o" 'conn-open-line
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

(defvar-keymap conn-mode-map
  "<remap> <kbd-macro-query>" 'conn-kapply-kbd-macro-query)

(defvar-keymap conn-local-mode-map
  "M-g o" 'conn-mark-ring-backward
  "M-g u" 'conn-mark-ring-forward)

(defun conn--setup-keymaps ()
  (if conn-mode
      (progn
        (cl-pushnew 'conn--state-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-major-mode-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-minor-mode-maps emulation-mode-map-alists)
        (set-keymap-parent search-map conn-search-map)
        (set-keymap-parent goto-map conn-goto-map)
        (set-keymap-parent indent-rigidly-map conn-indent-rigidly-map))
    (when (eq (keymap-parent search-map) conn-search-map)
      (set-keymap-parent search-map nil))
    (when (eq (keymap-parent search-map) conn-search-map)
      (set-keymap-parent indent-rigidly-map nil))
    (when (eq (keymap-parent goto-map) conn-goto-map)
      (set-keymap-parent goto-map nil))
    (setq emulation-mode-map-alists
          (seq-difference '(conn--state-maps
                            conn--local-maps
                            conn--local-major-mode-maps
                            conn--local-minor-mode-maps)
                          emulation-mode-map-alists #'eq))))


;;;; Mode Definition

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :keymap conn-local-mode-map
  :lighter (:eval conn-lighter)
  (conn--input-method-mode-line)
  (if conn-local-mode
      (progn
        (setq conn-current-state nil
              conn-previous-state nil)
        (setq-local conn-lighter (seq-copy conn-lighter))
        (unless (mark t)
          (conn--push-ephemeral-mark (point) t nil))
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'isearch-mode-end-hook 'conn--activate-input-method nil t)
        (add-hook 'isearch-mode-hook 'conn--isearch-input-method nil t)
        (setq conn--input-method current-input-method)
        (funcall (conn--default-state-for-buffer))
        (conn--setup-major-mode-maps))
    (dolist (state conn-states)
      (setf (buffer-local-value state (current-buffer)) nil))
    (setq conn-current-state nil
          conn-previous-state nil
          cursor-type t)
    (conn--clear-overlays)
    (remove-hook 'change-major-mode-hook #'conn--clear-overlays t)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (remove-hook 'isearch-mode-hook 'conn--isearch-input-method  t)
    (remove-hook 'isearch-mode-end-hook 'conn--activate-input-method t)
    (when (and conn--input-method (not current-input-method))
      (activate-input-method conn--input-method))))

(defun conn--initialize-buffer ()
  (conn-local-mode 1))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn--initialize-buffer
  :group 'conn
  :keymap conn-global-map
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (if conn-mode
        (progn
          ;; TODO: don't do this unconditionally
          (keymap-set minibuffer-mode-map "M-Y" 'conn-yank-region-to-minibuffer)
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50))
      (set-default
       'mode-line-format
       (assq-delete-all
        'conn-mode
        (default-value 'mode-line-format)))
      (when (eq (keymap-lookup minibuffer-mode-map "M-Y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "M-Y"))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook))))

(provide 'conn)


;;; Load Extensions

(with-eval-after-load 'calc
  (declare-function calc-dispatch "calc")

  (defun conn--calc-dispatch-ad (&rest app)
    (conn--with-state 'conn-null-state (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))

(with-eval-after-load 'corfu
  (defun conn--exit-completion (_state)
    (completion-in-region-mode -1))
  (add-hook 'conn-exit-functions 'conn--exit-completion))

(with-eval-after-load 'org
  (defvar org-mode-map)
  (declare-function org-backward-sentence "org")
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
  (defvar org-link-any-re)

  (conn-register-thing
   'org-link
   :dispatch-target-finder (lambda ()
                             (require 'org)
                             (conn--dispatch-re-matches org-link-any-re t))
   :bounds-op (lambda ()
                (require 'org)
                (org-in-regexp org-link-any-re))
   :mark-key "O"
   :modes '(org-mode))

  (conn-register-thing
   'org-paragraph
   :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'org-paragraph t)
   :forward-op 'org-forward-paragraph
   :mark-key "I"
   :modes '(org-mode))

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
   :forward-op 'conn-org-sentence-forward
   :mark-key "{"
   :modes '(org-mode))

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
   :mark-key "m"
   :beg-op 'org-backward-element
   :end-op 'org-forward-element
   :modes '(org-mode))

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
   :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'org-heading t)
   :forward-op 'org-next-visible-heading
   :modes '(org-mode))

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
    :keymap (conn-get-mode-map 'conn-movement-state 'org-mode)
    "=" 'conn-org-edit-state
    "^" 'org-up-element
    ")" 'org-next-visible-heading
    "(" 'org-previous-visible-heading
    "N" 'org-backward-element
    "M" 'org-forward-element
    "I" 'org-backward-paragraph
    "K" 'org-forward-paragraph))

(with-eval-after-load 'polymode
  (defvar polymode-move-these-vars-from-old-buffer)
  (dolist (v '(conn--mark-cursor
               conn-current-state
               conn-state
               conn-emacs-state))
    (cl-pushnew v polymode-move-these-vars-from-old-buffer)))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'conn-dispatch-on-things))

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-enter-state 'conn-emacs-state)
      (when conn-previous-state
        (funcall conn-previous-state))))
  (add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state))

(with-eval-after-load 'outline
  (declare-function outline-mark-subtree "outline")
  (declare-function outline-next-heading "outline")
  (declare-function outline-previous-heading "outline")
  (declare-function outline-on-heading-p "outline")
  (declare-function outline-up-heading "outline")

  (defun conn-forward-heading-op (N)
    (interactive "p")
    (cond ((< N 0)
           (dotimes (_ (abs N))
             (outline-previous-heading)))
          ((> N 0)
           (dotimes (_ N)
             (outline-next-heading)))))

  (conn-register-thing
   'heading
   :mark-cmd t
   :dispatch-target-finder (apply-partially 'conn--dispatch-all-things 'heading t)
   :bounds-op (lambda ()
                (save-mark-and-excursion
                  (outline-mark-subtree)
                  (cons (region-beginning) (region-end))))
   :forward-op 'conn-forward-heading-op)

  (keymap-set (conn-get-mode-map 'conn-state 'outline-minor-mode)
              "H H" 'conn-mark-heading)

  (conn-register-thing-commands
   'heading 'conn-continuous-thing-handler
   'conn-forward-heading-op)

  (conn-register-thing-commands
   'heading 'conn-discrete-thing-handler
   'outline-up-heading
   'outline-next-heading
   'outline-next-visible-heading
   'outline-previous-visible-heading
   'outline-previous-heading
   'outline-forward-same-level
   'outline-backward-same-level))

(with-eval-after-load 'treesit
  (conn-register-thing-commands
   'defun 'conn-continuous-thing-handler
   'treesit-end-of-defun
   'treesit-beginning-of-defun))

(with-eval-after-load 'dired
  (declare-function dired-mark "dired")
  (declare-function dired-unmark "dired")
  (declare-function dired-next-line "dired")
  (declare-function dired-next-dirline "dired")
  (declare-function dired-marker-regexp "dired")
  (declare-function dired-kill-subdir "dired-aux")
  (declare-function dired-kill-line "dired-aux")

  (defvar dired-subdir-alist)
  (defvar dired-movement-style)

  (defun conn--dispatch-dired-lines ()
    (let ((dired-movement-style 'bounded)
          ovs success)
      (unwind-protect
          (progn
            (save-excursion
              (with-restriction (window-start) (window-end)
                (goto-char (point-min))
                (while (/= (point)
                           (progn
                             (dired-next-line 1)
                             (point)))
                  (push (conn--make-target-overlay (point) 1) ovs))))
            (setq success t)
            ovs)
        (unless success (mapc #'delete-overlay ovs)))))

  (defun conn--dispatch-dired-dirline ()
    (let (ovs success)
      (unwind-protect
          (progn
            (save-excursion
              (with-restriction (window-start) (window-end)
                (goto-char (point-min))
                (while (/= (point)
                           (progn
                             (dired-next-dirline 1)
                             (point)))
                  (push (conn--make-target-overlay (point) 1) ovs))))
            (setq success t)
            ovs)
        (unless success (mapc #'delete-overlay ovs)))))

  (defun conn--dispatch-dired-subdir ()
    (let ((start (window-start))
          (end (window-end))
          ovs success)
      (unwind-protect
          (save-excursion
            (pcase-dolist (`(,_ . ,marker) dired-subdir-alist)
              (when (<= start marker end)
                (goto-char marker)
                (push (conn--make-target-overlay
                       (+ 2 marker) (- (line-end-position) marker 2))
                      ovs)))
            (setq success t)
            ovs)
        (unless success (mapc #'delete-overlay ovs)))))

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
    :key "f"
    :modes (dired-mode)
    :window-predicate (lambda () (eq major-mode 'dired-mode))
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
    :key "w"
    :modes (dired-mode)
    :window-predicate (lambda () (eq major-mode 'dired-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-line))))

  (conn-define-dispatch-action conn-dispatch-dired-kill-subdir (window pt _thing-cmd _thing-arg)
    :description "Kill Subdir"
    :key "d"
    :modes (dired-mode)
    :window-predicate (lambda () (eq major-mode 'dired-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-subdir))))

  (define-keymap
    :keymap (conn-get-mode-map 'conn-read-dispatch 'dired-mode)
    "f" 'conn-dispatch-dired-mark
    "w" 'conn-dispatch-dired-kill-line
    "d" 'conn-dispatch-dired-kill-subdir)

  (define-keymap
    :keymap (conn-get-mode-map 'conn-movement-state  'dired-mode)
    "m" 'dired-next-subdir
    "n" 'dired-prev-subdir
    "u" 'dired-next-dirline
    "k" 'dired-next-line
    "i" 'dired-previous-line))

(with-eval-after-load 'ibuffer
  (declare-function ibuffer-backward-line "ibuffer")
  (declare-function ibuffer-unmark-forward "ibuffer")
  (declare-function ibuffer-mark-forward "ibuffer")
  (declare-function ibuffer-current-mark "ibuffer")
  (declare-function ibuffer-backward-filter-group "ibuffer")

  (defvar ibuffer-movement-cycle)
  (defvar ibuffer-marked-char)

  (defun conn--dispatch-ibuffer-lines ()
    (let ((ibuffer-movement-cycle nil)
          ovs success)
      (unwind-protect
          (progn
            (save-excursion
              (with-restriction (window-start) (window-end)
                (goto-char (point-max))
                (while (/= (point)
                           (progn
                             (ibuffer-backward-line)
                             (point)))
                  (unless (get-text-property (point) 'ibuffer-filter-group-name)
                    (push (conn--make-target-overlay (point) 1) ovs)))))
            (setq success t)
            ovs)
        (unless success (mapc #'delete-overlay ovs)))))

  (defun conn--dispatch-ibuffer-filter-group ()
    (let ((ibuffer-movement-cycle nil)
          ovs success)
      (unwind-protect
          (progn
            (save-excursion
              (with-restriction (window-start) (window-end)
                (goto-char (point-max))
                (while (/= (point)
                           (progn
                             (ibuffer-backward-filter-group)
                             (point)))
                  (push (conn--make-target-overlay (point) 1) ovs))))
            (setq success t)
            ovs)
        (unless success (mapc #'delete-overlay ovs)))))

  (conn-register-thing
   'ibuffer-line
   :dispatch-target-finder 'conn--dispatch-ibuffer-lines)

  (conn-register-thing-commands
   'ibuffer-line nil
   'ibuffer-backward-line 'ibuffer-forward-line)

  (conn-register-thing
   'ibuffer-filter-group
   :dispatch-target-finder 'conn--dispatch-ibuffer-filter-group)

  (conn-register-thing-commands
   'ibuffer-filter-group nil
   'ibuffer-forward-filter-group
   'ibuffer-backward-filter-group)

  (conn-define-dispatch-action conn-dispatch-ibuffer-mark (window pt _thing-cmd _thing-arg)
    :description "Mark"
    :key "f"
    :modes (ibuffer-mode)
    :window-predicate (lambda () (eq major-mode 'ibuffer-mode))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (if (or (null (ibuffer-current-mark))
                (= (ibuffer-current-mark) ? ))
            (ibuffer-mark-forward nil nil 1)
          (ibuffer-unmark-forward nil nil 1)))))

  (define-keymap
    :keymap (conn-get-mode-map 'conn-read-dispatch 'ibuffer-mode)
    "f" 'conn-dispatch-ibuffer-mark
    "i" 'ibuffer-backward-line
    "k" 'ibuffer-forward-line
    "n" 'ibuffer-backward-filter-group
    "m" 'ibuffer-forward-filter-group))

;; Local Variables:
;; outline-regexp: "^;;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn.el ends here
