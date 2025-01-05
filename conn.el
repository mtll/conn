;;; conn.el --- Region oriented modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode and keyboard macro enhancement
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.0.0") (transient "0.6.0") (seq "2.24"))
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
;;
;;; Commentary:
;;
;; A region oriented modal keybinding mode.
;;
;;; Code:

;;;; Requires

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'map))

(declare-function extract-rectangle-bounds "rect")


;;;; Variables

;;;;; Declerations

(defvar conn-mode nil)
(defvar conn-local-mode)
(defvar conn-modes)
(defvar conn-state)
(defvar conn-emacs-state)
(defvar conn-org-edit-state)
(defvar conn-emacs-state)
(defvar conn-state-map)
(defvar conn-transition-hook)

(defvar conn-dispatch-providers-alist
  (list (cons t 'conn--dispatch-chars)))

(defvar conn-dispatch-default-actions-alist
  (list (cons t 'conn-dispatch-goto)))

(defvar conn-mark-handler-alist nil)

(defvar conn--mark-cursor-timer nil
  "`run-with-idle-timer' timer to update `mark' cursor.")

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
  "H" 'conn-contract
  "h" 'conn-expand)

(defvar-keymap conn-mark-thing-map
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

(defcustom conn-mark-update-delay 0.1
  "Update delay for mark cursor display."
  :type '(number :tag "seconds")
  :set (lambda (sym val)
         (set sym val)
         (when conn-mode
           (when conn--mark-cursor-timer
             (cancel-timer conn--mark-cursor-timer))
           (setq conn--mark-cursor-timer
                 (run-with-idle-timer
                  val t #'conn--mark-cursor-timer-func))))
  :group 'conn-marks)

(defcustom conn-lighter " Conn"
  "Modeline lighter for conn-mode."
  :type '(choice string (const nil))
  :group 'conn-states)

(defcustom conn-default-state 'conn-emacs-state
  "Default conn state for new buffers."
  :type 'symbol
  :group 'conn-states)

(defcustom conn-buffer-default-state-alist nil
  "Alist of the form ((CONDITION . STATE) ...).
Elements specify default STATE for buffers matching CONDITION.
CONDITION has the same meaning as in `buffer-match-p'."
  :type '(list (cons string symbol))
  :group 'conn-states)

(defface conn-mark-face
  '((default (:inherit cursor :background "#b8a2f0"))
    (((background light)) (:inherit cursor :background "#b8a2f0"))
    (((background dark)) (:inherit cursor :background "#a742b0")))
  "Face for mark."
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

(defcustom conn-ephemeral-mark-states
  nil
  "States in which ephemeral marks should be used."
  :type '(repeat symbol)
  :group 'conn-marks)

(defcustom conn-read-string-timeout 0.5
  "Timeout for string reading functions."
  :group 'conn
  :type 'number)

(defcustom conn-dispatch-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e" "r"
        "t" "y" "u" "i" "o" "c" "v" "b" "n" "m")
  "Chars to use for dispatch label overlays."
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

(defvar conn-window-label-sort-function 'conn--sort-window-mru
  "Sort function for window labels when prompting for a window.")

;;;;; State Variables

(defvar conn-states nil)

(defvar-local conn--input-method nil
  "Current input for buffer.")
(put 'conn--input-method 'permanent-local t)

(defvar-local conn--input-method-title nil
  "Title string of the current input method shown in mode line.")
(put 'conn--input-method-title 'permanent-local t)

(defvar-local conn--prev-mode-line-mule-info nil)
(put 'conn--prev-mode-line-mule-info 'risky-local-variable t)

(defvar conn-input-method-overriding-modes
  (list (list 'isearch-mode 'isearch-mode-hook 'isearch-mode-end-hook))
  "List of modes which override a state's input suppression property.
Each element may be either a symbol or a list of the form
(symbol . hooks).")

(defvar-local conn-current-state nil
  "Current conn state for buffer.")

(defvar-local conn-previous-state nil
  "Previous conn state for buffer.")

(defvar conn-in-modes
  (list 'occur-mode
        'grep-mode
        'occur-edit-mode
        'eshell-mode
        'edmacro-mode
        '(not minibuffer-mode
              dired-mode
              slime-xref-mode
              calc-mode
              calc-trail-mode
              calc-keypad-mode
              special-mode
              image-mode)
        t)
  "Modes in which `conn-local-mode' should be enabled.
Must be of a form accepted by `define-globalized-minor-mode'
:predicate argument.")

(defvar conn-enable-in-buffer-hook
  (list (lambda ()
          (require 'easy-mmode)
          (easy-mmode--globalized-predicate-p conn-in-modes)))
  "Hook to determine if `conn-local-mode' should be enabled in a buffer.
Each function is run without any arguments and if any of them return
non-nil `conn-local-mode' will be enabled in the buffer.")

(defvar conn-disable-in-buffer-hook nil
  "Hook to determine if `conn-local-mode' should be enabled in a buffer.
Each function is run without any arguments and if any of them return
nil `conn-local-mode' will be not enabled in the buffer.")

;;;;;; State Keymaps

(defvar conn--state-maps nil)

(defvar-local conn--local-maps nil)

(defvar-local conn--major-mode-maps nil)

(defvar conn--mode-maps nil)

(defvar-local conn--local-mode-maps nil)

;;;;; Mark Variables

(defvar conn-this-command-handler nil
  "Mark handler for current command.
Commands can set this variable if they need to change their handler
dynamically.")

(defvar conn-this-command-thing nil)

(defvar conn-this-command-start (make-marker)
  "Start position for current mark movement command.")

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.
Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--ephemeral-mark nil)

(defvar conn--saved-ephemeral-marks nil)

(defvar-local conn--mark-cursor nil
  "`mark' cursor overlay.")

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

(defcustom conn-backward-up-list-keys (key-parse "C-M-<up>")
  "`backward-up-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-down-list-keys (key-parse "C-M-<down>")
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

;;;;;; Mark Cursor

(put 'conn--mark-cursor 'permanent-local t)
(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)
(put 'conn--mark-cursor 'conn-overlay t)

;;;;;; Read String Overlays

(put 'conn-read-string-match 'conn-overlay t)
(put 'conn-read-string-match 'face 'conn-read-string-match-face)
(put 'conn-read-string-match 'priority 2001)

;;;;;; Label Overlay

(put 'conn-label-overlay 'face 'conn-read-string-match-face)
(put 'conn-label-overlay 'priority 3000)
(put 'conn-label-overlay 'conn-overlay t)

;;;;; Command Histories

(defvar conn--seperator-history nil
  "History var for `conn-set-register-seperator'.")

(defvar conn--read-string-timeout-history nil)

(defvar conn--read-string-history nil)

(defvar conn--read-regexp-history nil)


;;;; Utilities

(eval-and-compile
  (defmacro conn--thread (form needle &rest forms)
    (declare (indent 2)
             (debug (symbolp form)))
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
    (intern (apply #'conn--stringify symbols-or-strings)))

  (defun conn--string-fill (string col)
    (with-temp-buffer
      (insert string)
      (let ((fill-column col)
            (adaptive-fill-mode nil))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defmacro conn--with-advice (advice-forms &rest body)
  (declare (indent 1))
  (pcase-dolist (`(,symbol ,how ,function . ,props) (nreverse advice-forms))
    (setq body (let ((fn (gensym "advice")))
                 `(let ((,fn ,function))
                    (advice-add ,symbol ,how ,fn ,(car props))
                    (unwind-protect
                        ,(macroexp-progn body)
                      (advice-remove ,symbol ,fn))))))
  body)

(defmacro conn--without-conn-maps (&rest body)
  (declare (indent 0))
  `(let ((emulation-mode-map-alists (seq-difference
                                     emulation-mode-map-alists
                                     '(conn--local-mode-maps
                                       conn--major-mode-maps
                                       conn--local-maps
                                       conn--state-maps)
                                     #'eq)))
     ,(macroexp-progn body)))

(defmacro conn--with-region-emphasis (beg end &rest body)
  (declare (indent 2))
  (let ((beg-ol (gensym "beg-ol"))
        (end-ol (gensym "end-ol")))
    `(let ((,beg-ol (make-overlay (point-min) ,beg))
           (,end-ol (make-overlay ,end (point-max))))
       (unwind-protect
           (progn
             (overlay-put ,beg-ol 'face 'shadow)
             (overlay-put ,end-ol 'face 'shadow)
             ,@body)
         (delete-overlay ,beg-ol)
         (delete-overlay ,end-ol)))))

(defun conn-remapping-command (from-keys)
  `(menu-item
    ,(format "Remap %s" (key-description from-keys))
    ,(conn--without-conn-maps (key-binding from-keys t))
    :filter ,(lambda (&rest _)
               (conn--without-conn-maps
                 (key-binding from-keys t)))))

;; From orderless
(defun conn--escapable-split-on-char (string char)
  "Split STRING on CHAR, which can be escaped with backslash."
  (let ((quoted (concat "\\" char)))
    (mapcar
     (lambda (piece) (replace-regexp-in-string (string 0) char piece))
     (split-string (replace-regexp-in-string
                    (concat "\\\\\\" (substring quoted 0 (1- (length quoted)))
                            "\\|\\\\" quoted)
                    (lambda (x) (if (equal x quoted) (string 0) x))
                    string 'fixedcase 'literal)
                   (concat quoted "+")))))

;; From repeat-mode
(defun conn--command-property (property)
  (or (and (symbolp this-command)
           (get this-command property))
      (and (symbolp real-this-command)
           (get real-this-command property))))

;; From expand-region
(defun conn--point-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face)
            '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun conn--create-marker (pos &optional buffer)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    marker))

(defun conn--overlay-start-marker (ov)
  (conn--create-marker (overlay-start ov) (overlay-buffer ov)))

(defun conn--overlay-end-marker (ov)
  (conn--create-marker (overlay-end ov) (overlay-buffer ov)))

(defun conn--overlay-bounds-markers (ov)
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
  (cl-loop for mode in (conn--thread 'major-mode -mode->
                         (buffer-local-value -mode-> (or buffer (current-buffer)))
                         (conn--derived-mode-all-parents -mode->))
           for prop = (get mode property) when prop return prop))

(defun conn--merge-regions (regions)
  (let (merged)
    (pcase-dolist ((and region `(,beg1 . ,end1)) regions)
      (pcase (seq-find (pcase-lambda (`(,beg2 . ,end2))
                         (and (eq (marker-buffer beg2) (marker-buffer beg1))
                              (not (or (< end2 beg1) (< end1 beg2)))))
                       merged)
        ((and cons `(,beg2 . ,end2))
         (setcar cons (if (< beg1 beg2)
                          (prog1 beg1 (set-marker beg2 nil))
                        (prog1 beg2 (set-marker beg1 nil))))
         (setcdr cons (if (> end1 end2)
                          (prog1 end1 (set-marker end2 nil))
                        (prog1 end2 (set-marker end1 nil)))))
        ('nil (push region merged))))
    merged))

(defun conn--narrow-indirect (beg end &optional record)
  (let* ((line-beg (line-number-at-pos beg))
         (linenum  (- (line-number-at-pos end) line-beg))
         (name     (format "%s@%s+%s - %s"
                           (buffer-name (current-buffer)) line-beg linenum
                           (thread-first
                             (buffer-substring-no-properties beg end)
                             (string-trim)
                             (substring 0 20)))))
    (clone-indirect-buffer-other-window name t)
    (conn-narrow-to-region beg end record)
    (deactivate-mark)))

(defmacro conn--with-state (state &rest body)
  (declare (indent 1))
  (let ((saved-state (make-symbol "saved-state"))
        (saved-prev-state (make-symbol "saved-prev-state")))
    `(let ((,saved-state conn-current-state)
           (,saved-prev-state conn-previous-state)
           (conn-transition-hook))
       (unwind-protect
           (progn
             (,state)
             ,@body)
         (funcall ,saved-state)
         (setq conn-previous-state ,saved-prev-state)))))

(defmacro conn--with-input-method (&rest body)
  (declare (indent 0))
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

(defun conn--region-visible-p (beg end)
  (and (not (invisible-p beg))
       (cl-loop for pt = (next-single-char-property-change
                          beg 'invisible nil end)
                while (and pt (< end pt))
                never (invisible-p pt))))

(defun conn--string-no-upper-case-p (string)
  (cl-loop for char across string always (eq char (downcase char))))

(defun conn--visible-matches (string &optional dir)
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

(defun conn--read-from-with-preview (prompt beg end &optional regexp-flag)
  (let ((default (conn--replace-read-default)))
    (conn--with-region-emphasis beg end
      (minibuffer-with-setup-hook
          (minibuffer-lazy-highlight-setup
           :case-fold case-fold-search
           :filter (lambda (mb me) (<= beg mb me end))
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
                         'conn--read-regexp-history)
          (let ((from (read-from-minibuffer
                       (format-prompt prompt default)
                       nil nil nil nil
                       (if default
                           (delete-dups
                            (cons default (query-replace-read-from-suggestions)))
                         (query-replace-read-from-suggestions))
                       t)))
            (or (and (zerop (length from)) default)
                from)))))))

(defun conn-overlay-p (overlay)
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


;;;; Labels

(defun conn--make-preview-overlay (pt length &optional thing)
  (let* ((eol (save-excursion
                (goto-char pt)
                (line-end-position)))
         (ov (make-overlay pt (min (+ pt length) eol))))
    (overlay-put ov 'thing thing)
    (overlay-put ov 'category 'conn-read-string-match)
    (overlay-put ov 'window (selected-window))
    (overlay-put ov 'after-string
                 (propertize (make-string (- (+ pt length) (overlay-end ov)) ? )
                             'face 'conn-read-string-match-face))
    (overlay-put ov 'padding (overlay-get ov 'after-string))
    ov))

(defun conn--preview-get-windows (in-windows)
  (pcase-exhaustive in-windows
    ((and 'nil
          (guard (not (apply #'derived-mode-p conn-dispatch-thing-ignored-modes))))
     (list (selected-window)))
    ((pred functionp)
     (cl-loop for win in (window-list-1 nil nil 'visible)
              unless (or (apply #'provided-mode-derived-p
                                (buffer-local-value 'major-mode (window-buffer win))
                                conn-dispatch-thing-ignored-modes)
                         (not (funcall in-windows win)))
              collect win))
    ('t
     (cl-loop for win in (window-list-1 nil nil 'visible)
              unless (apply #'provided-mode-derived-p
                            (buffer-local-value 'major-mode (window-buffer win))
                            conn-dispatch-thing-ignored-modes)
              collect win))))

(defun conn--string-preview-overlays-1 (win string &optional dir)
  (with-selected-window win
    (cl-loop for pt in (conn--visible-matches string dir)
             collect (conn--make-preview-overlay pt (length string)))))

(defun conn--string-preview-overlays (string &optional dir in-windows)
  (cl-loop for win in (conn--preview-get-windows in-windows)
           nconc (conn--string-preview-overlays-1 win string dir)))

(defun conn--read-string-with-timeout-1 (&optional dir in-windows)
  (conn--with-input-method
    (let* ((prompt (propertize "string: " 'face 'minibuffer-prompt))
           (string (char-to-string (read-char prompt t)))
           (overlays (conn--string-preview-overlays string dir in-windows)))
      (condition-case _err
          (progn
            (while-let ((next-char (read-char (format (concat prompt "%s") string) t
                                              conn-read-string-timeout)))
              (setq string (concat string (char-to-string next-char)))
              (mapc #'delete-overlay overlays)
              (setq overlays (conn--string-preview-overlays string dir in-windows)))
            (message nil)
            (cons string overlays))
        ((quit error)
         (mapc #'delete-overlay overlays))))))

(defun conn--read-string-with-timeout (&optional dir in-windows)
  (pcase-let ((`(,string . ,overlays)
               (conn--read-string-with-timeout-1 dir in-windows)))
    (mapc #'delete-overlay overlays)
    string))

(defun conn--create-label-strings (count alphabet &optional labels)
  (let* ((labels (or labels (take count alphabet)))
         (prefixes nil))
    (while (and labels
                (> count (+ (length labels)
                            (* (length prefixes)
                               (length alphabet)))))
      (push (pop labels) prefixes))
    (if (and (null labels) (> count 0))
        (let ((new-labels))
          (dolist (a prefixes)
            (dolist (b alphabet)
              (push (concat a b) new-labels)))
          (conn--create-label-strings count
                                      conn-dispatch-label-characters
                                      new-labels))
      (catch 'term
        (let ((n (length labels)))
          (setq labels (nreverse labels))
          (dolist (prefix (nreverse prefixes))
            (dolist (c alphabet)
              (push (concat prefix c) labels)
              (when (= (cl-incf n) count)
                (throw 'term nil))))))
      (dolist (l labels)
        (put-text-property 0 (length l) 'face 'conn-dispatch-label-face l))
      (nreverse labels))))

(defun conn--read-labels (things labels label-fn payload)
  (let ((candidates (funcall label-fn labels things))
        (prompt "char:"))
    (unwind-protect
        (catch 'return
          (while t
            (pcase candidates
              ('nil
               (setq candidates (funcall label-fn labels things)
                     prompt "char: (no matches)"))
              (`(,it . nil)
               (throw 'return (overlay-get it payload)))
              (_
               (setq prompt "char:")))
            (setq candidates (conn--dispatch-narrow-labels prompt candidates))))
      (mapcar #'delete-overlay candidates))))

(defun conn--create-window-labels (labels windows)
  (cl-loop with scroll-margin = 0
           for win in windows
           for lbl in labels
           collect (with-selected-window win
                     (let ((overlay (make-overlay (window-start) (window-end))))
                       (goto-char (window-start))
                       (forward-line)
                       (overlay-put overlay 'conn-overlay t)
                       (overlay-put overlay 'face 'shadow)
                       (overlay-put overlay 'window win)
                       (overlay-put overlay 'before-string
                                    (propertize lbl 'face 'conn-window-prompt-face))
                       overlay))))

(defun conn--sort-window-mru (a b)
  "Sort windows for most to least recently used."
  (> (window-use-time a) (window-use-time b)))

(defun conn--prompt-for-window (windows)
  (when (setq windows (seq-sort conn-window-label-sort-function
                                (seq-remove 'window-dedicated-p windows)))
    (if (length= windows 1)
        (car windows)
      (let ((window-state
             (cl-loop for win in windows
                      collect (list (window-point win)
                                    (window-vscroll win)
                                    (window-hscroll win)))))
        (unwind-protect
            (conn--read-labels
             windows
             (conn--create-label-strings (length windows)
                                         conn-window-label-characters)
             'conn--create-window-labels
             'window)
          (cl-loop for win in windows
                   for (pt vscroll hscroll) in window-state do
                   (set-window-point win pt)
                   (set-window-hscroll win hscroll)
                   (set-window-vscroll win vscroll)))))))


;;;; Read Things

(defvar conn-bounds-of-command-alist nil)

(defvar conn-bounds-of-things-in-region-alist nil)

(defvar conn-bounds-of-things-in-region-default
  'conn--things-in-region-default)

(defvar-local conn-read-thing-mover-override-maps nil)

(defvar-local conn-read-thing-override-maps nil)

(define-minor-mode conn-read-thing-mode
  "Mode for reading a thing."
  :keymap (define-keymap "C-h" 'help)
  :lighter " THING"
  (if conn-read-thing-mode
      (progn
        (setq conn--thing-overriding-maps
              (thread-last
                (append conn-read-thing-override-maps
                        (list conn-read-thing-mode-map))
                (delq nil)
                (make-composed-keymap)))
        (internal-push-keymap conn--thing-overriding-maps
                              'overriding-terminal-local-map))
    (internal-pop-keymap conn--thing-overriding-maps
                         'overriding-terminal-local-map)))

(defvar conn--thing-overriding-maps nil)

(define-minor-mode conn-read-thing-mover-mode
  "Mode for reading a thing mover."
  :lighter " MOVER"
  :keymap (define-keymap
            "C-w" 'backward-delete-arg
            "C-d" 'forward-delete-arg
            "C-<backspace>" 'reset-arg
            "M-<backspace>" 'reset-arg
            "M-DEL" 'reset-arg
            "." 'reset-arg
            "<remap> <conn-forward-char>" 'forward-char
            "<remap> <conn-backward-char>" 'backward-char
            "b" 'beginning-of-buffer
            "C-h" 'help
            "t" conn-mark-thing-map
            "r" 'recursive-edit)
  (if conn-read-thing-mover-mode
      (progn
        (setq conn--thing-overriding-maps
              (thread-last
                (append conn-read-thing-mover-override-maps
                        (list conn-read-thing-mover-mode-map))
                (delq nil)
                (make-composed-keymap)))
        (internal-push-keymap conn--thing-overriding-maps
                              'overriding-terminal-local-map))
    (internal-pop-keymap conn--thing-overriding-maps
                         'overriding-terminal-local-map)))

(defvar-keymap conn-read-expand-region-map
  :parent conn-expand-repeat-map
  "v" 'conn-toggle-mark-command
  "e" 'exit-recursive-edit
  "C-g" 'abort-recursive-edit
  "<t>" 'ignore)

(defvar conn-last-bounds-of-command nil)

(defun conn-bounds-of-command (cmd arg)
  (setq conn-last-bounds-of-command
        (funcall (or (alist-get cmd conn-bounds-of-command-alist)
                     (apply-partially 'conn--bounds-of-thing-command-default cmd))
                 arg)))

(defun conn--bounds-of-thing-command-default (cmd arg)
  (let ((current-prefix-arg arg)
        (conn-this-command-handler (or (conn-get-mark-handler cmd)
                                       'conn-individual-thing-handler))
        (conn-this-command-thing (get cmd :conn-command-thing))
        (this-command cmd)
        (conn-this-command-start (point-marker)))
    (save-mark-and-excursion
      (call-interactively cmd)
      (funcall conn-this-command-handler conn-this-command-start)
      (append (list (region-beginning) (region-end))
              (conn-bounds-of-things-in-region
               conn-this-command-thing (region-beginning) (region-end))))))

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
      (list (region-beginning) (region-end)))))

(setf (alist-get 'conn-expand conn-bounds-of-command-alist)
      (apply-partially 'conn--bounds-of-expansion 'conn-expand))

(setf (alist-get 'conn-contract conn-bounds-of-command-alist)
      (apply-partially 'conn--bounds-of-expansion 'conn-contract))

(defun conn--bounds-of-region (_arg)
  (append (list (region-beginning) (region-end))
          (region-bounds)))

(setf (alist-get 'conn-toggle-mark-command conn-bounds-of-command-alist)
      'conn--bounds-of-region)

(setf (alist-get 'set-mark-command conn-bounds-of-command-alist)
      'conn--bounds-of-region)

(setf (alist-get 'conn-set-mark-command conn-bounds-of-command-alist)
      'conn--bounds-of-region)

(defun conn-bounds-of-things-in-region (thing beg end)
  (funcall (or (alist-get thing conn-bounds-of-things-in-region-alist)
               (ignore-errors
                 (alist-get (get thing :conn-command-thing)
                            conn-bounds-of-things-in-region-alist))
               conn-bounds-of-things-in-region-default)
           thing beg end))

(defun conn--things-in-region-default (thing beg end)
  (ignore-errors
    (save-excursion
      (goto-char beg)
      (forward-thing thing 1)
      (cl-loop for bounds = (save-excursion
                              (forward-thing thing -1)
                              (bounds-of-thing-at-point thing))
               while bounds collect bounds into regions
               while (and (< (point) end)
                          (ignore-errors
                            (forward-thing thing 1)
                            t))
               finally return regions))))

(defun conn--region-bounds (beg end)
  (cl-loop for reg in (region-bounds)
           when (<= beg (car reg) (cdr reg) end)
           collect reg))

(defun conn--read-thing-mover (prompt &optional arg recursive-edit)
  (conn--with-state conn-state
    (conn-read-thing-mover-mode 1)
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<conn-read-thing-mover-mode-map>"
                           prompt " (arg: "
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
            (conn-read-thing-mover-mode -1)
            (save-window-excursion
              (setq cmd (intern
                         (completing-read
                          "Command: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  ,(cons 'affixation-function
                                         (conn--dispatch-make-command-affixation
                                          conn-read-thing-mover-mode-map))
                                  (category . conn-dispatch-command))
                              (complete-with-action action obarray string pred)))
                          (lambda (sym)
                            (and (functionp sym)
                                 (not (eq sym 'help))
                                 (get sym :conn-command-thing)))
                          t))))
            (conn-read-thing-mover-mode 1)
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
           ((or 'conn-expand 'conn-contract)
            (cl-return
             (list cmd (cond (thing-arg (* thing-arg (if thing-sign -1 1)))
                             (thing-sign '-)))))
           ((guard (or (ignore-errors (get cmd :conn-command-thing))
                       (ignore-errors (get cmd :conn-command-bounds))
                       (alist-get cmd conn-bounds-of-command-alist)))
            (cl-return
             (list cmd (cond (thing-arg (* thing-arg (if thing-sign -1 1)))
                             (thing-sign '-)))))
           (_ (setq invalid t)))
         (go :read-command))
      (message nil)
      (conn-read-thing-mover-mode -1))))

(defun conn--read-thing-region (prompt &optional arg)
  (pcase-let* ((`(,cmd ,arg) (conn--read-thing-mover prompt arg t)))
    (cons (get cmd :conn-command-thing) (conn-bounds-of-command cmd arg))))

(defun conn--read-thing (prompt)
  (conn--with-state conn-state
    (conn-read-thing-mode 1)
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<conn-read-thing-command-map>"
                           prompt " (\\[help] commands, \\[recursive-edit] recursive edit): %s")))
          invalid keys cmd)
         :read-command
         (setq keys (read-key-sequence
                     (format prompt
                             (if invalid
                                 (propertize "Not a valid thing command"
                                             'face 'error)
                               "")))
               cmd (key-binding keys t))
         :test
         (pcase cmd
           ('keyboard-quit
            (keyboard-quit))
           ('help
            (conn-read-thing-mode -1)
            (save-window-excursion
              (setq cmd (intern
                         (completing-read
                          "Command: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  ,(cons 'affixation-function
                                         (conn--dispatch-make-command-affixation
                                          conn-read-thing-mode-map))
                                  (category . conn-dispatch-command))
                              (complete-with-action action obarray string pred)))
                          (lambda (sym)
                            (and (functionp sym)
                                 (not (eq sym 'help))
                                 (get sym :conn-command-thing)))
                          t))))
            (conn-read-thing-mode 1)
            (go :test))
           ((and (pred symbolp)
                 (guard (get cmd :conn-command-thing)))
            (cl-return (get cmd :conn-command-thing)))
           (_
            (setq invalid t)))
         (go :read-command))
      (message nil)
      (conn-read-thing-mode -1))))

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

(put 'conn--dot-overlay 'priority (1- conn-mark-overlay-priority))
(put 'conn--dot-overlay 'conn-overlay t)
(put 'conn--dot-overlay 'evaporate t)

(defvar conn--dots nil)

(defun conn--create-dot (beg end &optional buffer point)
  (with-current-buffer (or buffer (current-buffer))
    (cl-loop for ov in (overlays-in beg end) do
             (when (eq (overlay-get ov 'category) 'conn--dot-overlay)
               (let ((b (overlay-start ov))
                     (e (overlay-end ov)))
                 (when (or (< beg b end) (< beg e end))
                   (setq beg (min beg (overlay-start ov))
                         end (max end (overlay-end ov)))
                   (delete-overlay ov)))))
    (let ((dot (make-overlay beg end buffer))
          (faces (thread-last
                   (append (overlays-at (1- beg)) (overlays-at end))
                   (seq-filter (lambda (ov)
                                 (eq 'conn--dot-overlay (overlay-get ov 'category))))
                   (mapcar (lambda (ov) (overlay-get ov 'face)))
                   (seq-difference conn-dot-faces))))
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
              (seq-filter (lambda (ov)
                            (eq (overlay-get ov 'category) 'conn--dot-overlay))
                          ovs)))
    (dolist (ov (if (use-region-p)
                    (get-dots (overlays-in (region-beginning) (region-end)))
                  (or (get-dots (overlays-at (point)))
                      (get-dots (overlays-at (1- (point)))))))
      (delete-overlay ov))
    (deactivate-mark t)))

(defun conn--dot-mode-command-hook ()
  (when (overlay-buffer mouse-secondary-overlay)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (let* ((beg (overlay-start mouse-secondary-overlay))
             (end (overlay-end mouse-secondary-overlay))
             (ov (make-overlay beg (if (= beg end) (1+ beg) end))))
        (overlay-put ov 'category 'conn--dot-overlay)
        (push ov conn--dots)
        (delete-overlay mouse-secondary-overlay)))))

(defvar-keymap conn--dot-mode-map
  "C-w" 'conn-region-to-dot
  "C-d" 'conn-point-to-dot
  "S-<mouse-1>" 'conn-mouse-click-dot
  "S-<down-mouse-1>" 'conn-mouse-click-dot
  "S-<drag-mouse-1>" 'conn-mouse-click-dot
  "S-<mouse-3>" 'conn-delete-dot-at-click
  "S-<down-mouse-3>" 'conn-delete-dot-at-click
  "<escape>" 'exit-recursive-edit
  "DEL" 'conn-delete-dots)

(define-minor-mode conn--dot-mode
  "Minor mode for multiple dots."
  :global t
  :lighter ""
  :interactive nil
  (if conn--dot-mode
      (progn
        (internal-push-keymap conn--dot-mode-map 'overriding-terminal-local-map)
        (delete-overlay mouse-secondary-overlay)
        (setq conn--dots nil)
        (add-hook 'post-command-hook 'conn--dot-mode-command-hook))
    (internal-pop-keymap conn--dot-mode-map 'overriding-terminal-local-map)
    (remove-hook 'post-command-hook 'conn--dot-mode-command-hook)
    (mapc #'delete-overlay conn--dots)
    (setq conn--dots nil)))

(defun conn--bounds-of-dots (&rest _)
  (conn--dot-mode 1)
  (save-mark-and-excursion
    (unwind-protect
        (let ((dots (thread-last
                      (progn
                        (recursive-edit)
                        (nreverse conn--dots))
                      (seq-filter 'overlay-buffer)
                      (mapcar (lambda (ov)
                                (prog1
                                    (if (overlay-get ov 'point)
                                        (cons (conn--overlay-start-marker ov)
                                              (conn--overlay-start-marker ov))
                                      (conn--overlay-bounds-markers ov))
                                  (delete-overlay ov)))))))
          (append (list (region-beginning) (region-end)) dots))
      (conn--dot-mode -1)
      (deactivate-mark t))))

(setf (alist-get 'recursive-edit conn-bounds-of-command-alist)
      'conn--bounds-of-dots)


;;;; Advice

(defun conn--read-from-suggestions-ad (&rest app)
  (if (and (not (use-region-p))
           (not (save-excursion
                  (goto-char (region-beginning))
                  (search-forward "\n" (region-end) t))))
      (cons (buffer-substring-no-properties
             (region-beginning) (region-end))
            (apply app))
    (apply app)))

(defun conn--repeat-advice (&rest app)
  (unwind-protect
      (apply app)
    (setq conn-this-command-thing (conn--command-property :conn-command-thing)
          conn-this-command-handler (or (alist-get this-command conn-mark-handler-alist)
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
        (advice-add 'query-replace-read-from-suggestions :around
                    'conn--read-from-suggestions-ad)
        (advice-add 'read-regexp-suggestions :around
                    'conn--read-from-suggestions-ad)
        (advice-add 'repeat :around #'conn--repeat-advice)
        (advice-add 'push-mark :before #'conn--push-mark-ad)
        (advice-add 'pop-mark :before #'conn--pop-mark-ad)
        (advice-add 'set-mark :before #'conn--set-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'query-replace-read-from-suggestions
                   'conn--read-from-suggestions-ad)
    (advice-remove 'read-regexp-suggestions
                   'conn--read-from-suggestions-ad)
    (advice-remove 'set-mark #'conn--set-mark-ad)
    (advice-remove 'repeat #'conn--repeat-advice)
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
     do
     (pcase (let ((executing-kbd-macro nil)
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

(defun conn--kapply-advance-region (region)
  (pcase region
    (`(,beg . ,end)
     (when-let ((buffer (ignore-errors (marker-buffer beg))))
       (when (not (eq buffer (current-buffer)))
         (pop-to-buffer-same-window buffer)
         (deactivate-mark t)
         (unless (eq buffer (window-buffer (selected-window)))
           (error "Could not pop to buffer %s" buffer))))
     (goto-char beg)
     (conn--push-ephemeral-mark end)
     (when (markerp beg) (set-marker beg nil))
     (when (markerp end) (set-marker end nil))
     t)))

(defun conn--kapply-infinite-iterator ()
  (lambda (_state) t))

(defun conn--kapply-thing-iterator (thing beg end &optional reverse skip-empty nth)
  (prog1
      (conn--thread
          (conn-bounds-of-things-in-region thing beg end) -->
        (if skip-empty
            (seq-remove (lambda (reg) (conn-thing-empty-p thing reg)) -->)
          -->)
        (if (or (null nth) (= 1 nth))
            -->
          (cl-loop with stack = -->
                   while stack
                   collect (car stack)
                   do (cl-loop repeat nth do (pop stack))))
        (conn--kapply-region-iterator (if reverse (nreverse -->) -->)))
    (deactivate-mark)))

(defun conn--kapply-region-iterator (regions &optional reverse)
  (if reverse (setq regions (nreverse regions)))
  (pcase-dolist ((and reg `(,beg . ,end)) regions)
    (unless (markerp beg)
      (setcar reg (conn--create-marker beg)))
    (unless (markerp end)
      (setcdr reg (conn--create-marker end))))
  (lambda (state)
    (pcase state
      (:finalize
       (pcase-dolist (`(,beg . ,end) regions)
         (set-marker beg nil)
         (set-marker end nil)))
      (_
       (conn--kapply-advance-region (pop regions))))))

(defun conn--kapply-point-iterator (points &optional reverse)
  (setq points (cl-loop for pt in (if reverse
                                      (nreverse points)
                                    points)
                        collect (if (markerp pt)
                                    pt
                                  (conn--create-marker pt))))
  (lambda (state)
    (pcase state
      (:finalize
       (dolist (pt points) (set-marker pt nil)))
      (_
       (when-let ((pt (pop points)))
         (conn--kapply-advance-region (cons pt pt)))))))

(defun conn--kapply-matches (string beg end &optional regexp-flag reverse delimited-flag query-flag region-first-flag)
  (let ((matches (save-excursion
                   (goto-char beg)
                   (cl-loop
                    with matches = nil
                    while (replace-search string end regexp-flag
                                          delimited-flag case-fold-search)
                    for (mb me . _) = (match-data t)
                    do
                    (push (cons (conn--create-marker mb)
                                (conn--create-marker me))
                          matches)
                    finally return (if reverse matches (nreverse matches))))))
    (when-let ((first (and region-first-flag
                           (seq-find (pcase-lambda (`(,mb . ,me))
                                       (and (= mb (region-beginning))
                                            (= me (region-end))))
                                     matches))))
      (setq matches (cons first (delete first matches))))
    (lambda (state)
      (pcase state
        (:finalize
         (mapc (pcase-lambda (`(,beg . ,end))
                 (set-marker beg nil)
                 (set-marker end nil))
               matches))
        (:record
         (if query-flag
             (let ((hl (make-overlay (point) (point))))
               (overlay-put hl 'face 'query-replace)
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
         (conn--kapply-advance-region (pop matches)))))))

(defun conn--kapply-merge-undo (iterator)
  (let (undo-handles)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(_ . ,handle) undo-handles)
           (if conn-kmacro-apply-error
               (cancel-change-group handle))
           (accept-change-group handle)
           (undo-amalgamate-change-group handle)))
        (_
         (prog1
             (funcall iterator state)
           (unless (alist-get (current-buffer) undo-handles)
             (activate-change-group
              (setf (alist-get (current-buffer) undo-handles)
                    (prepare-change-group))))))))))

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
        (_
         (prog1 (funcall iterator state)
           (unless (alist-get (current-buffer) saved-excursions)
             (setf (alist-get (current-buffer) saved-excursions)
                   (cons (point-marker) (save-mark-and-excursion--save))))))))))

(defun conn--kapply-save-restriction (iterator)
  (let (kapply-saved-restrictions)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,buffer ,beg . ,end) kapply-saved-restrictions)
           (with-current-buffer buffer
             (widen)
             (narrow-to-region beg end))))
        (_
         (prog1
             (funcall iterator state)
           (if-let ((restriction (alist-get (current-buffer) kapply-saved-restrictions)))
               (progn
                 (widen)
                 (narrow-to-region (car restriction) (cdr restriction)))
             (setf (alist-get (current-buffer) kapply-saved-restrictions)
                   (cons (point-min-marker)
                         (point-max-marker))))))))))

(defun conn--kapply-change-region (iterator)
  (lambda (state)
    (when-let ((ret (funcall iterator state)))
      (delete-region (region-beginning) (region-end))
      ret)))

(defun conn--kapply-with-state (iterator transition)
  (let ((buffer-states nil))
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
    (when-let ((ret (funcall iterator state)))
      (conn-exchange-mark-command)
      ret)))

(defun conn--kapply-pulse-region (iterator)
  (lambda (state)
    (when-let ((ret (funcall iterator state)))
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
  (declare (doc-string 3) (indent 2))
  (let ((iterator (car arglist))
        (docstring (if (stringp (car body)) (pop body) "")))
    `(defun ,name ,arglist
       ,docstring
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
  (or (alist-get command conn-mark-handler-alist)
      (ignore-errors (get command :conn-mark-handler))))

(defun conn-register-thing (thing &rest rest)
  "Register a new THING.

\(fn THING &key FINDER DEFAULT-ACTION FORWARD-OP BEG-OP END-OP BOUNDS-OP MODES MARK-KEY)"
  (intern (symbol-name thing))
  (when-let ((finder (plist-get rest :dispatch-provider)))
    (setf (alist-get thing conn-dispatch-providers-alist) finder))
  (when-let ((action (plist-get rest :default-action)))
    (setf (alist-get thing conn-dispatch-default-actions-alist) action))
  (when-let ((forward (plist-get rest :forward-op)))
    (put thing 'forward-op forward))
  (when-let ((beg (plist-get rest :beg-op)))
    (put thing 'beginning-op beg))
  (when-let ((end (plist-get rest :end-op)))
    (put thing 'end-op end))
  (when-let ((bounds (plist-get rest :bounds-op)))
    (put thing 'bounds-of-thing-at-point bounds))
  (when-let ((inner-bounds-op (plist-get rest :inner-bounds-op)))
    (put thing :conn-inner-bounds-op inner-bounds-op))
  (when-let ((binding (plist-get rest :mark-key))
             (mark-command (conn--symbolicate "conn-mark-" thing)))
    (fset mark-command
          (lambda ()
            (interactive)
            (pcase (bounds-of-thing-at-point thing)
              (`(,beg . ,end)
               (goto-char beg)
               (conn--push-ephemeral-mark end))
              (_ (user-error "Point not in %s" thing)))))
    (put mark-command :conn-command-thing thing)
    (if (plist-get rest :modes)
        (dolist (mode (put thing :conn-thing-modes
                           (ensure-list (plist-get rest :modes))))
          (keymap-set (conn-get-mode-things-map mode)
                      binding mark-command))
      (keymap-set conn-mark-thing-map binding mark-command))))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER."
  (dolist (cmd commands)
    (put cmd :conn-command-thing thing))
  (apply 'conn-set-command-handler handler commands))

(defun conn-sequential-thing-handler (beg)
  (ignore-errors
    (pcase (abs (prefix-numeric-value current-prefix-arg))
      (0)
      ((let dir (pcase (- (point) beg)
                  (0 0)
                  ((pred (< 0)) 1)
                  ((pred (> 0)) -1)))
       (save-excursion
         (goto-char beg)
         (forward-thing conn-this-command-thing dir)
         (forward-thing conn-this-command-thing (- dir))
         (conn--push-ephemeral-mark))))))

(defun conn-individual-thing-handler (_beg)
  (pcase (ignore-errors (bounds-of-thing-at-point conn-this-command-thing))
    (`(,beg . ,end)
     (conn--push-ephemeral-mark (if (= (point) end) beg end)))))

(defun conn-jump-handler (beg)
  "Mark trail handler.
The mark trail handler pushes an ephemeral mark at the starting point
of the movement command unless `region-active-p'."
  (unless (= beg (point))
    (conn--push-ephemeral-mark beg)))

(defun conn-set-command-handler (handler &rest commands)
  "Register a thing movement command for THING."
  (dolist (cmd (ensure-list commands))
    (put cmd :conn-mark-handler handler)))

(defun conn--thing-parent (thing)
  (get thing :conn-thing-parent))

(defun conn--mark-cursor-p (ov)
  (eq (overlay-get ov 'category) 'conn--mark-cursor))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.
For the meaning of MSG and ACTIVATE see `push-mark'."
  (push-mark location (not msg) activate)
  (setq conn--ephemeral-mark t)
  nil)

(defun conn--hide-mark-cursor-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or (when-let ((hide (get conn-current-state :conn-hide-mark)))
          (if (functionp hide) (funcall hide) t))
        (when-let ((hide (conn--derived-mode-property :conn-hide-mark)))
          (if (functionp hide) (funcall hide) t)))))

(defun conn--mark-cursor-timer-func-1 (win)
  (when-let ((buf (window-buffer win)))
    (with-current-buffer buf
      (cond
       ((not conn-local-mode))
       ((conn--hide-mark-cursor-p) (conn--delete-mark-cursor))
       ((null (mark t)))
       ((null conn--mark-cursor)
        (setq conn--mark-cursor (make-overlay (mark t) (1+ (mark t)) nil t nil))
        (overlay-put conn--mark-cursor 'category 'conn--mark-cursor)
        (overlay-put conn--mark-cursor 'before-string
                     (when (and (= (mark-marker) (point-max))
                                (/= (point) (mark-marker)))
                       (propertize " " 'face 'conn-mark-face))))
       (t
        (move-overlay conn--mark-cursor (mark t) (1+ (mark t)))
        (overlay-put conn--mark-cursor 'before-string
                     (when (and (= (mark-marker) (point-max))
                                (/= (point) (mark-marker)))
                       (propertize " " 'face 'conn-mark-face))))))))

(defun conn--mark-cursor-timer-func ()
  (walk-windows #'conn--mark-cursor-timer-func-1 nil 'visible))

(defun conn-hide-mark-cursor (mmode-or-state &optional predicate)
  "Hide mark cursor in buffers with in MMODE-OR-STATE.
If PREDICATE is non-nil it is a function that will be called
to determine if mark cursor should be hidden in buffer.
If MMODE-OR-STATE is a mode it must be a major mode."
  (put mmode-or-state :conn-hide-mark (or predicate t)))

(defun conn-show-mark-cursor (mmode-or-state)
  "Show mark cursor in MMODE-OR-STATE.
If MMODE-OR-STATE is a mode it must be a major mode."
  (put mmode-or-state :conn-hide-mark nil))

(defun conn--mark-pre-command-hook ()
  (set-marker conn-this-command-start (point))
  (setq conn-this-command-handler (or (alist-get this-command conn-mark-handler-alist)
                                      (conn--command-property :conn-mark-handler))
        conn-this-command-thing (conn--command-property :conn-command-thing)))

(defun conn--mark-post-command-hook ()
  (when (and conn-local-mode
             (eq (current-buffer) (marker-buffer conn-this-command-start))
             conn-this-command-thing
             conn-this-command-handler
             (not (region-active-p)))
    (with-demoted-errors "Error in mark hook: %S"
      (funcall conn-this-command-handler conn-this-command-start))))

(defun conn--setup-mark ()
  (when conn--mark-cursor-timer
    (cancel-timer conn--mark-cursor-timer)
    (setq conn--mark-cursor-timer nil))
  (if conn-mode
      (progn
        (setq conn--mark-cursor-timer
              (run-with-idle-timer conn-mark-update-delay
                                   t #'conn--mark-cursor-timer-func)
              conn--prev-mark-even-if-inactive mark-even-if-inactive
              mark-even-if-inactive t)
        (add-hook 'pre-command-hook #'conn--mark-pre-command-hook)
        (add-hook 'post-command-hook #'conn--mark-post-command-hook))
    (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
    (remove-hook 'pre-command-hook #'conn--mark-pre-command-hook)
    (remove-hook 'post-command-hook #'conn--mark-post-command-hook)))

(defun conn--delete-mark-cursor ()
  (without-restriction
    (thread-last
      (conn--all-overlays 'conn--mark-cursor-p (point-min) (point-max))
      (mapc #'delete-overlay)))
  (setq conn--mark-cursor nil))

(defun conn-bounds-of-inner-thing (thing bounds)
  (when-let ((inner-op (get thing :conn-inner-bounds-op)))
    (funcall inner-op (car bounds) (cdr bounds))))

(defun conn-thing-empty-p (thing bounds)
  (or (pcase (conn-bounds-of-inner-thing thing bounds)
        ('nil nil)
        ((and `(,beg . ,end) (guard (= beg end))) t))
      (= (car bounds) (cdr bounds))))

;;;;; Thing Definitions

(conn-register-thing 'url :mark-key "!")
(conn-register-thing 'email :mark-key "@")
(conn-register-thing 'uuid :mark-key "$")
(conn-register-thing 'string :mark-key "\"")
(conn-register-thing 'filename :mark-key "/")

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :dispatch-provider (apply-partially 'conn--dispatch-all-things 'defun t))

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
 :dispatch-provider (apply-partially 'conn--dispatch-things-with-prefix 'symbol 1 t))

(conn-register-thing-commands
 'symbol 'conn-sequential-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing
 'page
 :mark-key "p"
 :forward-op 'forward-page)

(conn-register-thing-commands
 'page 'conn-individual-thing-handler
 'forward-page 'backward-page)

(conn-register-thing-commands
 'char nil
 'forward-char 'backward-char
 'conn-forward-char 'conn-backward-char)

(conn-register-thing
 'word
 :forward-op 'forward-word
 :dispatch-provider (apply-partially 'conn--dispatch-things-with-prefix 'word 1 t))

(conn-register-thing-commands
 'word 'conn-sequential-thing-handler
 'forward-word 'backward-word)

(conn-register-thing
 'sexp
 :forward-op 'forward-sexp
 :dispatch-provider (apply-partially 'conn--dispatch-things-with-prefix 'sexp 1 t))

(conn-register-thing-commands
 'sexp 'conn-sequential-thing-handler
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
 'list 'conn-sequential-thing-handler
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
 'whitespace 'conn-individual-thing-handler
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing
 'sentence
 :forward-op 'forward-sentence
 :dispatch-provider (apply-partially 'conn--dispatch-all-things 'sentence t))

(conn-register-thing-commands
 'sentence 'conn-sequential-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing
 'paragraph
 :forward-op 'forward-paragraph
 :dispatch-provider (apply-partially 'conn--dispatch-all-things 'paragraph t))

(conn-register-thing-commands
 'paragraph 'conn-sequential-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-sequential-thing-handler
 'end-of-defun 'beginning-of-defun)

(conn-register-thing 'char :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'buffer 'conn-individual-thing-handler
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
 :dispatch-provider 'conn--dispatch-lines)

(conn-register-thing-commands
 'line 'conn-sequential-thing-handler
 'forward-line 'conn-backward-line
 'conn-line-forward-op)

(conn-register-thing
 'line-column
 :forward-op 'next-line
 :dispatch-provider 'conn--dispatch-columns
 :default-action 'conn-dispatch-jump)

(conn-register-thing-commands
 'line-column 'conn-jump-handler
 'next-line 'previous-line
 'rectangle-next-line 'rectangle-previous-line)

(conn-register-thing
 'outer-line
 :beg-op (lambda () (move-beginning-of-line nil))
 :end-op (lambda () (move-end-of-line nil))
 :dispatch-provider 'conn--dispatch-lines)

(conn-register-thing-commands
 'outer-line 'conn-individual-thing-handler
 'move-beginning-of-line 'move-end-of-line)

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
 :dispatch-provider 'conn--dispatch-inner-lines)

(conn-register-thing-commands
 'inner-line 'conn-individual-thing-handler
 'back-to-indentation
 'conn-beginning-of-inner-line
 'conn-end-of-inner-line)


;;;; States

(defun conn--setup-major-mode-maps ()
  (setq conn--major-mode-maps nil)
  (let* ((mmodes (if (get major-mode :conn-inhibit-inherit-maps)
                     (list major-mode)
                   (reverse (conn--derived-mode-all-parents major-mode))))
         mark-map-keys mode-map)
    (dolist (state conn-states)
      (setq mark-map-keys
            (where-is-internal conn-mark-thing-map
                               (list (alist-get state conn--state-maps)
                                     (conn-get-local-map state))))
      (dolist (mode mmodes)
        (setq mode-map (conn-get-mode-map state mode))
        (push (cons state mode-map) conn--major-mode-maps)
        (let ((mark-map (conn-get-mode-things-map mode)))
          (when (cdr mark-map)
            (dolist (key mark-map-keys)
              (define-key mode-map key mark-map))))))))

(defun conn-set-derived-mode-inherit-maps (mode inhibit-inherit-maps)
  "Set whether derived MODE inherits `conn-get-mode-map' keymaps from parents.
If INHIBIT-INHERIT-MAPS is non-nil then any maps defined using
`conn-get-mode-map' for parents of MODE will not be made active
when MODE is."
  (put mode :conn-inhibit-inherit-maps inhibit-inherit-maps))

(defun conn-get-mode-map (state mode)
  "Get MODE keymap for STATE.
If one does not exists assign a new sparse keymap for MODE
in STATE and return it."
  (or (alist-get mode (alist-get state conn--mode-maps))
      (setf (alist-get mode (alist-get state conn--mode-maps))
            (make-sparse-keymap))))

(defun conn-get-mode-things-map (mode)
  "Get MODE keymap for STATE things.
If one does not exists assign a new sparse keymap for MODE things
in STATE and return it."
  (or (get mode :conn-mode-things)
      (put mode :conn-mode-things (make-sparse-keymap))))

(defun conn-get-local-map (state)
  "Get local keymap for STATE in current buffer.
If one does not exists assign a new sparse keymap for STATE
and return it."
  (or (alist-get state conn--local-maps)
      (setf (alist-get state conn--local-maps) (make-sparse-keymap))))

(defun conn-input-method-overriding-mode (mode &rest hooks)
  "Make a MODE ignore `conn-mode' input method supression.
If HOOKS are not specified checks are performed in MODE-hook to toggle
the input method.  If HOOKS are specified checks are performed in those
hooks instead."
  (let ((hooks (or hooks (list (conn--symbolicate mode "-hook")))))
    (add-to-list 'conn-input-method-overriding-modes (cons mode hooks))))

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property.
Also enable input methods when any `conn-input-method-overriding-mode'
is on or when `conn-input-method-always' is t."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (if (seq-find (pcase-lambda (`(,mode . _))
                    (symbol-value mode))
                  conn-input-method-overriding-modes)
        (when (and conn--input-method (not current-input-method))
          (activate-input-method conn--input-method))
      (pcase (get conn-current-state :conn-suppress-input-method)
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

(defun conn--deactivate-input-method ()
  "Disable input method in all states."
  (setq conn--input-method nil
        conn--input-method-title nil))

(defun conn-toggle-input-method ()
  (interactive)
  (if (and conn--input-method (not current-input-method))
      (let ((current-input-method conn--input-method))
        (deactivate-input-method)
        (setq conn--input-method nil
              conn--input-method-title nil))
    (call-interactively 'toggle-input-method)))

(defun conn--input-method-mode-line ()
  (cond
   (conn-local-mode
    (setq conn--prev-mode-line-mule-info mode-line-mule-info
          mode-line-mule-info
          `(""
            (conn--input-method
             (:propertize ("" conn--input-method-title)
                          help-echo (concat
                                     "Current input method: "
                                     conn--input-method
                                     "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
                          local-map ,mode-line-input-method-map
                          mouse-face mode-line-highlight))
            ,(propertize
              "%z"
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

(defmacro conn-define-state (name doc &rest rest)
  "Define a conn state NAME.
Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.

:LIGHTER-FACE is the face for the conn mode-line lighter in NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in
NAME.

:KEYMAP is a keymap for the state.

:CURSOR is the `cursor-type' for NAME.

:EPHEMERAL-MARKS if non-nil thing movement commands will push ephemeral
marks while in state NAME.

BODY contains code to be executed each time the state is enabled or
disabled.

\(fn NAME DOC &key CURSOR LIGHTER-FACE SUPPRESS-INPUT-METHOD KEYMAP EPHEMERAL-MARKS &rest BODY)"
  (declare (indent defun))
  (pcase-let* ((map-name (conn--symbolicate name "-map"))
               (cursor-name (conn--symbolicate name "-cursor-type"))
               (lighter-face-name (conn--symbolicate name "-lighter-face"))
               (enter (gensym "enter"))
               ((map :cursor
                     :lighter-face
                     :suppress-input-method
                     (:keymap keymap '(make-sparse-keymap))
                     :ephemeral-marks)
                rest)
               (body (cl-loop for sublist on rest by #'cddr
                              unless (keywordp (car sublist))
                              do (cl-return sublist))))
    `(progn
       (defvar-local ,name nil
         ,(conn--stringify "Non-nil when `" name "' is active."))

       (defvar ,map-name
         (setf (alist-get ',name conn--state-maps) ,keymap)
         ,(conn--stringify "Keymap active in `" name "'."))

       (defface ,lighter-face-name
         ',lighter-face
         ,(conn--stringify "Face for `" name "' mode line lighter.")
         :group 'conn-states)

       (defcustom ,cursor-name
         ,(if cursor `',cursor t)
         ,(conn--stringify "`cursor-type' for " name ".")
         :type '(choice
                 (const :tag "Frame default" t)
                 (const :tag "Filled box" box)
                 (cons :tag "Box with specified size"
                       (const box)
                       integer)
                 (const :tag "Hollow cursor" hollow)
                 (const :tag "Vertical bar" bar)
                 (cons :tag "Vertical bar with specified height"
                       (const bar)
                       integer)
                 (const :tag "Horizontal bar" hbar)
                 (cons :tag "Horizontal bar with specified width"
                       (const hbar)
                       integer))
         :group 'conn-states)

       ,(when ephemeral-marks
          `(cl-pushnew ',name conn-ephemeral-mark-states))

       (put ',name :conn-suppress-input-method ,suppress-input-method)
       (put ',name :conn-cursor-type ',cursor-name)
       (put ',name :conn-lighter-face ',lighter-face-name)

       (cl-pushnew ',name conn-states)

       (defun ,name ()
         ,doc
         (interactive)
         (when conn-current-state
           (funcall (get conn-current-state :conn-transition-fn) :exit))
         (funcall (get ',name :conn-transition-fn) :enter))

       (put ',name :conn-transition-fn
            (lambda (,enter)
              (when (xor ,name (and ,enter (not (eq ,enter :exit))))
                (if ,name
                    (setq ,name nil
                          conn-current-state nil
                          conn-previous-state ',name)
                  (setq ,name t
                        conn-current-state ',name
                        conn--local-mode-maps (alist-get ',name conn--mode-maps))
                  (when conn-lighter
                    (put-text-property 0 (length conn-lighter)
                                       'face ',lighter-face-name
                                       conn-lighter))
                  (conn--activate-input-method)
                  (if-let ((cursor (symbol-value (get conn-current-state :conn-cursor-type))))
                      (setq cursor-type cursor)
                    (setq cursor-type t))
                  (when (not executing-kbd-macro)
                    (force-mode-line-update)))
                ,@body
                (run-hook-wrapped
                 'conn-transition-hook
                 (lambda (hook)
                   (condition-case _err
                       (funcall hook)
                     (error
                      (remove-hook 'conn-transition-hook hook)
                      (message "Error in transition hook %s" hook)))))))))))

(conn-define-state conn-emacs-state
  "Activate `conn-emacs-state' in the current buffer.
A `conn-mode' state for inserting text.  By default `conn-emacs-state' does not
bind anything except transition commands."
  :lighter-face ((default (:inherit mode-line :background "#cae1ff"))
                 (((background light)) (:inherit mode-line :background "#cae1ff"))
                 (((background dark)) (:inherit mode-line :background "#49739f")))
  :ephemeral-marks nil
  :keymap (make-sparse-keymap))

(conn-define-state conn-state
  "Activate `conn-state' in the current buffer.
A `conn-mode' state for editing text."
  :lighter-face ((default (:inherit mode-line :background "#f3bdbd"))
                 (((background light)) (:inherit mode-line :background "#f3bdbd"))
                 (((background dark)) (:inherit mode-line :background "#8c3c3c")))
  :suppress-input-method t
  :ephemeral-marks t
  :keymap (define-keymap :suppress t))

(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . prog-mode) . conn-state))
(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . text-mode) . conn-state))
(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . conf-mode) . conn-state))

(conn-define-state conn-org-edit-state
  "Activate `conn-org-edit-state' in the current buffer.
A `conn-mode' state for structural editing of `org-mode' buffers."
  :lighter-face ((default (:inherit mode-line :background "#f5c5ff"))
                 (((background light)) (:inherit mode-line :background "#f5c5ff"))
                 (((background dark)) (:inherit mode-line :background "#85508c")))
  :suppress-input-method t
  :keymap (define-keymap :suppress t)
  :ephemeral-marks t)


;;;; Thing Dispatch

(defmacro conn-define-dispatch-action (name-and-description arglist &rest body)
  "\(fn (NAME DESCRIPTION) ARGLIST &body BODY)"
  (declare (indent 2))
  (pcase-exhaustive name-and-description
    (`(,name ,description)
     `(progn
        (defun ,name ,arglist ,@body)
        (put ',name :conn-action-description ,description)
        (put ',name :conn-action t)))))

(defvar conn-dispatch-all-things-collector-alist
  (list (cons t 'conn--dispatch-all-things-1)))

(defvar conn-dispatch-action-maps
  (list (define-keymap
          "[" 'conn-dispatch-kill-append
          "a" 'conn-dispatch-copy-append
          "]" 'conn-dispatch-kill-prepend
          "p" 'conn-dispatch-copy-prepend
          "w" 'conn-dispatch-kill
          "s" 'conn-dispatch-grab
          "y" 'conn-dispatch-yank
          "q" 'conn-dispatch-transpose
          "c" 'conn-dispatch-copy
          "f" 'conn-dispatch-yank-replace
          "d" 'conn-dispatch-grab-replace
          "g" 'conn-dispatch-goto
          "e" 'conn-dispatch-over
          "z" 'conn-dispatch-jump))
  "List of keymap containing dispatch actions.
All dispatch actions must be in a keymap in this list.")

(defvar conn-dispatch-over-action-maps
  (list (define-keymap
          "[" 'conn-dispatch-kill-append-over
          "{" 'conn-dispatch-kill-append-over
          "a" 'conn-dispatch-copy-append-over
          "A" 'conn-dispatch-copy-append-over
          "]" 'conn-dispatch-kill-prepend-over
          "}" 'conn-dispatch-kill-prepend-over
          "p" 'conn-dispatch-copy-prepend-over
          "P" 'conn-dispatch-copy-prepend-over
          "w" 'conn-dispatch-kill-over
          "W" 'conn-dispatch-kill-over
          "c" 'conn-dispatch-copy-over
          "C" 'conn-dispatch-copy-over))
  "List of keymap containing dispatch actions.
All dispatch actions must be in a keymap in this list.")

(defvar conn-dispatch-thing-override-maps
  (list (define-keymap
          "l" 'forward-line
          "u" 'forward-symbol
          "U" `(symbol
                ,(apply-partially 'conn--dispatch-all-things 'symbol t)
                . conn-dispatch-goto)
          "O" `(word
                ,(apply-partially 'conn--dispatch-all-things 'word t)
                . conn-dispatch-goto)))
  "List of keymaps containing thing commands that overrides default bindings.
Members of these keymaps can be either a command with a thing property or
a list of the form (THING DISAPTCH-FINDER . DEFAULT-ACTION).")

(defun conn--dispatch-finder (command)
  (or (alist-get command conn-dispatch-providers-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-providers-alist)
      (alist-get t conn-dispatch-providers-alist)))

(defun conn--dispatch-default-action (command)
  (or (alist-get command conn-dispatch-default-actions-alist)
      (alist-get (get command :conn-command-thing) conn-dispatch-default-actions-alist)
      (alist-get t conn-dispatch-default-actions-alist)))

(setf (alist-get 'conn-end-of-inner-line conn-dispatch-providers-alist)
      'conn--dispatch-inner-lines-end)

(setf (alist-get 'move-end-of-line conn-dispatch-providers-alist)
      'conn--dispatch-lines-end)

(setf (alist-get 'conn-backward-symbol conn-dispatch-providers-alist)
      (apply-partially 'conn--dispatch-all-things 'symbol t))

(setf (alist-get 'backward-word conn-dispatch-providers-alist)
      (apply-partially 'conn--dispatch-all-things 'word t))

(defun conn--dispatch-fixup-whitespace ()
  (when (or (looking-at " ") (looking-back " " 1))
    (fixup-whitespace)
    (indent-for-tab-command))
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\s)*\n"))
    (join-line)))

(conn-define-dispatch-action (conn-dispatch-kill "Kill")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Killed: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-kill-over "Kill")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (conn-dispatch-over window pt thing)
    (kill-region (region-beginning) (region-end))
    (conn--dispatch-fixup-whitespace)))

(conn-define-dispatch-action (conn-dispatch-kill-append "Kill Append")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str nil)
           (delete-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Appended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-kill-append-over "Kill Append")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (conn-dispatch-over window pt thing)
    (let ((str (filter-buffer-substring (region-beginning) (region-end))))
      (kill-append str nil)
      (delete-region (region-beginning) (region-end))
      (conn--dispatch-fixup-whitespace))))

(conn-define-dispatch-action (conn-dispatch-kill-prepend "Kill Prepend")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str t)
           (delete-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Prepended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-kill-prepend-over "Kill Prepend")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (conn-dispatch-over window pt thing)
    (let ((str (filter-buffer-substring (region-beginning) (region-end))))
      (kill-append str t)
      (delete-region (region-beginning) (region-end))
      (conn--dispatch-fixup-whitespace))))

(conn-define-dispatch-action (conn-dispatch-copy "Copy")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (pulse-momentary-highlight-region beg end)
           (kill-new str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy-over "Copy")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (save-mark-and-excursion
      (conn-dispatch-over window pt thing)
      (copy-region-as-kill (region-beginning) (region-end))
      (pulse-momentary-highlight-region (region-beginning) (region-end)))))

(conn-define-dispatch-action (conn-dispatch-copy-append "Copy Append")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str nil)
           (message "Copy Appended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy-append-over "Copy Append")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (save-mark-and-excursion
      (conn-dispatch-over window pt thing)
      (let ((str (filter-buffer-substring (region-beginning) (region-end))))
        (kill-append str nil)
        (pulse-momentary-highlight-region (region-beginning) (region-end))
        (conn--dispatch-fixup-whitespace)))))

(conn-define-dispatch-action (conn-dispatch-copy-prepend "Copy Prepend")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str t)
           (message "Copy Prepended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy-prepend-over "Copy Prepend")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point))
             (or (bounds-of-thing-at-point thing)
                 (user-error "No %s at point" thing)))
    (save-mark-and-excursion
      (conn-dispatch-over window pt thing)
      (let ((str (filter-buffer-substring (region-beginning) (region-end))))
        (kill-append str t)
        (pulse-momentary-highlight-region (region-beginning) (region-end))
        (conn--dispatch-fixup-whitespace)))))

(conn-define-dispatch-action (conn-dispatch-yank-replace "Yank")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (pulse-momentary-highlight-region beg end)
         (copy-region-as-kill beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "No thing at point")))))
  (delete-region (region-beginning) (region-end))
  (yank))

(conn-define-dispatch-action (conn-dispatch-grab-replace "Grab Replace")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (kill-region beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "No thing at point")))))
  (delete-region (region-beginning) (region-end))
  (yank))

(conn-define-dispatch-action (conn-dispatch-grab "Grab")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (kill-region beg end)
         (conn--dispatch-fixup-whitespace))
        (_ (user-error "No thing at point")))))
  (yank))

(conn-define-dispatch-action (conn-dispatch-yank "Yank")
    (window pt thing)
  (let (str)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
          (`(,beg . ,end)
           (pulse-momentary-highlight-region beg end)
           (setq str (filter-buffer-substring beg end))))))
    (if str
        (insert str)
      (user-error "No thing at point"))))

(conn-define-dispatch-action (conn-dispatch-goto "Goto")
    (window pt thing)
  (with-current-buffer (window-buffer window)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (select-window window)
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (unless (region-active-p)
           (if (= (point) end)
               (conn--push-ephemeral-mark beg)
             (conn--push-ephemeral-mark end)))
         (unless (or (= pt beg) (= pt end))
           (goto-char beg)))))))

(conn-define-dispatch-action (conn-dispatch-over "Over")
    (window pt thing)
  (when (and (eq (window-buffer window) (current-buffer))
             (/= pt (point)))
    (unless (region-active-p)
      (push-mark nil t))
    (pcase (alist-get thing conn-dispatch-default-actions-alist)
      ((or 'conn-dispatch-goto 'nil)
       (pcase (cons (or (bounds-of-thing-at-point thing)
                        (point))
                    (progn
                      (goto-char pt)
                      (bounds-of-thing-at-point thing)))
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
      ('conn-dispatch-jump (conn-dispatch-jump window pt thing))
      (_ (error "Can't jump to %s" thing)))))

(conn-define-dispatch-action (conn-dispatch-jump "Jump")
    (window pt _thing)
  (with-current-buffer (window-buffer window)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (select-window window)
      (goto-char pt))))

(conn-define-dispatch-action (conn-dispatch-narrow-ring "Narrow Ring")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn--dispatch-thing-bounds thing current-prefix-arg)
        (`(,beg . ,end)
         (conn--narrow-ring-record beg end))))))

(conn-define-dispatch-action (conn-dispatch-transpose "Transpose")
    (window pt thing)
  (if (eq (current-buffer) (window-buffer window))
      (pcase (if (region-active-p)
                 (cons (region-beginning) (region-end))
               (bounds-of-thing-at-point thing))
        (`(,beg1 . ,end1)
         (pcase (save-excursion
                  (goto-char pt)
                  (bounds-of-thing-at-point thing))
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
           (_ (user-error "No thing at point"))))
        (_ (user-error "No thing at point")))
    (let ((cg1 (prepare-change-group))
          (cg2 (with-current-buffer (window-buffer window)
                 (prepare-change-group)))
          str1 str2 success)
      (unwind-protect
          (progn
            (pcase (if (region-active-p)
                       (cons (region-beginning) (region-end))
                     (bounds-of-thing-at-point thing))
              (`(,beg . ,end)
               (setq str1 (filter-buffer-substring beg end)))
              (_ (user-error "No thing at point")))
            (with-selected-window window
              (save-excursion
                (goto-char pt)
                (pcase (bounds-of-thing-at-point thing)
                  (`(,beg . ,end)
                   (setq str2 (filter-buffer-substring beg end))
                   (delete-region beg end)
                   (insert str1))
                  (_ (user-error "No thing at point")))))
            (delete-region (region-beginning) (region-end))
            (insert str2)
            (setq success t))
        (if success
            (progn
              (accept-change-group cg1)
              (accept-change-group cg2))
          (cancel-change-group cg1)
          (cancel-change-group cg2))))))

(defun conn--dispatch-make-window-predicate (thing)
  (if-let ((modes (get thing :conn-thing-modes)))
      (lambda (win)
        (apply #'provided-mode-derived-p
               (buffer-local-value 'major-mode (window-buffer win))
               modes))
    (lambda (_) t)))

(defun conn--dispatch-thing-bounds (thing arg)
  (if (get thing 'forward-op)
      (save-excursion
        (forward-thing thing 1)
        (forward-thing thing -1)
        (cons (point) (progn (forward-thing thing arg) (point))))
    (bounds-of-thing-at-point thing)))

(defun conn--dispatch-make-command-affixation (keymap)
  (lambda (command-names)
    (with-selected-window (or (minibuffer-selected-window) (selected-window))
      (cl-loop
       for command-name in command-names
       collect
       (let* ((fun (and (stringp command-name) (intern-soft command-name)))
              (binding (where-is-internal fun
                                          (cons keymap (current-active-maps t))
                                          t))
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

(defun conn--dispatch-narrow-labels (prompt overlays)
  (unwind-protect
      (let ((c (read-char prompt))
            (narrowed))
        (save-current-buffer
          (dolist (ov overlays)
            (set-buffer (overlay-buffer ov))
            (let ((prop (if (overlay-get ov 'before-string) 'before-string 'display)))
              (if (not (eql c (aref (overlay-get ov prop) 0)))
                  (when-let ((prefix (overlay-get ov 'prefix-overlay)))
                    (overlay-put prefix 'face nil)
                    (overlay-put prefix 'after-string nil))
                (conn--thread (overlay-get ov prop) -->
                  (substring --> 1)
                  (overlay-put ov prop -->))
                (move-overlay ov
                              (overlay-start ov)
                              (+ (overlay-start ov)
                                 (min (length (overlay-get ov prop))
                                      (- (overlay-end ov)
                                         (overlay-start ov)))))
                (let ((after-str (buffer-substring (overlay-start ov) (overlay-end ov))))
                  (if-let ((pos (string-search "\n" after-str)))
                      (overlay-put ov 'after-string (substring after-str pos))
                    (overlay-put ov 'after-string nil)))
                (push ov narrowed)))))
        (mapcar #'copy-overlay narrowed))
    (mapc #'delete-overlay overlays)))

(defun conn--dispatch-label-overlays (labels prefix-overlays)
  (let (overlays success)
    (unwind-protect
        (progn
          (pcase-dolist (`(,window . ,prefixes) prefix-overlays)
            (with-current-buffer (window-buffer window)
              (dolist (p prefixes)
                (let* ((beg (overlay-end p))
                       (label (pop labels))
                       (next (thread-last
                               (conn--all-overlays
                                (lambda (ov)
                                  (and (eq 'conn-read-string-match
                                           (overlay-get ov 'category))
                                       (not (eq ov p))))
                                beg (+ beg (length label)))
                               (mapcar #'overlay-start)
                               (apply 'min (point-max))))
                       (end (min next (+ beg (length label))))
                       (ov (make-overlay beg end)))
                  (push ov overlays)
                  (overlay-put p 'after-string (overlay-get p 'padding))
                  (overlay-put p 'face 'conn-read-string-match-face)
                  (let ((after-str (buffer-substring (overlay-start ov) (overlay-end ov))))
                    (when-let ((pos (string-search "\n" after-str)))
                      (overlay-put ov 'after-string (substring after-str pos))))
                  (overlay-put ov 'prefix-overlay p)
                  (overlay-put ov 'category 'conn-label-overlay)
                  (overlay-put ov 'window window)
                  (overlay-put ov (if (or (= beg next)
                                          (= beg (point-max)))
                                      'before-string
                                    'display)
                               label)))))
          (setq success t)
          overlays)
      (unless success (mapc #'delete-overlay overlays)))))

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
    (cl-loop for pt in ovs collect
             (conn--make-preview-overlay pt 1 thing))))

(defun conn--dispatch-all-things (thing &optional in-windows)
  (cl-loop for win in (conn--preview-get-windows
                       (pcase in-windows
                         ('t (conn--dispatch-make-window-predicate thing))
                         ('nil nil)
                         ((pred functionp)
                          (let ((thing-pred (conn--dispatch-make-window-predicate thing)))
                            (lambda (win)
                              (and (funcall thing-pred win)
                                   (funcall in-windows win)))))))
           nconc (with-selected-window win
                   (if-let ((fn (alist-get thing conn-dispatch-all-things-collector-alist)))
                       (funcall fn)
                     (funcall (alist-get t conn-dispatch-all-things-collector-alist) thing)))))

(defun conn--dispatch-re-matches (regexp &optional in-windows)
  (cl-loop for win in (conn--preview-get-windows in-windows)
           nconc (with-selected-window win
                   (with-restriction (window-start) (window-end)
                     (save-excursion
                       (goto-char (point-min))
                       (save-match-data
                         (cl-loop while (re-search-forward regexp nil t)
                                  collect
                                  (conn--make-preview-overlay
                                   (match-beginning 0) (- (match-end 0)
                                                          (match-beginning 0))))))))))

(defun conn--dispatch-things-with-prefix-1 (things prefix)
  (let ((case-fold-search (conn--string-no-upper-case-p prefix))
        (things (ensure-list things))
        ovs)
    (save-excursion
      (goto-char (window-end))
      (while (search-backward prefix (window-start) t)
        (dolist (thing things)
          (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point thing)))
            (when (and (eql (point) beg)
                       (conn--region-visible-p beg end)
                       (not (eql (point) (caar ovs))))
              (push (list (point) (length prefix) thing) ovs))))))
    (cl-loop for ov in ovs collect
             (apply 'conn--make-preview-overlay ov))))

(defun conn--dispatch-things-with-prefix (things prefix-length &optional in-windows)
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
          (dolist (thing (ensure-list things))
            (dolist (win (conn--preview-get-windows
                          (pcase in-windows
                            ('t (conn--dispatch-make-window-predicate thing))
                            ('nil nil)
                            ((pred functionp)
                             (let ((thing-pred (conn--dispatch-make-window-predicate thing)))
                               (lambda (win)
                                 (and (funcall thing-pred win)
                                      (funcall in-windows win))))))))
              (setq ovs (nconc (with-selected-window win
                                 (conn--dispatch-things-with-prefix-1 things prefix))
                               ovs))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-columns ()
  (let ((col (current-column))
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
                  (push (conn--make-preview-overlay (point) 1) ovs))
                (forward-line))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines ()
  (let (ovs success)
    (unwind-protect
        (progn
          (dolist (win (window-list-1 nil nil 'visible))
            (with-selected-window win
              (unless (apply #'derived-mode-p conn-dispatch-thing-ignored-modes)
                (save-excursion
                  (with-restriction (window-start) (window-end)
                    (goto-char (point-min))
                    (when (and (bolp)
                               (<= (+ (point) (window-hscroll)) (line-end-position))
                               (goto-char (+ (point) (window-hscroll)))
                               (not (invisible-p (point))))
                      (push (conn--make-preview-overlay (point) 1) ovs))
                    (while (/= (point) (point-max))
                      (forward-line)
                      (when (and (bolp)
                                 (<= (+ (point) (window-hscroll))
                                     (line-end-position) (point-max))
                                 (goto-char (+ (point) (window-hscroll)))
                                 (not (invisible-p (point)))
                                 (not (invisible-p (1- (point)))))
                        (push (conn--make-preview-overlay (point) 1) ovs))))))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines-end ()
  (let (ovs success)
    (unwind-protect
        (progn
          (dolist (win (window-list-1 nil nil 'visible))
            (with-selected-window win
              (unless (apply #'derived-mode-p conn-dispatch-thing-ignored-modes)
                (save-excursion
                  (with-restriction (window-start) (window-end)
                    (goto-char (point-min))
                    (move-end-of-line nil)
                    (when (and (eolp) (not (invisible-p (point))))
                      (push (conn--make-preview-overlay (point) 1) ovs))
                    (while (/= (point) (point-max))
                      (forward-line)
                      (move-end-of-line nil)
                      (when (and (eolp)
                                 (not (invisible-p (point)))
                                 (not (invisible-p (1- (point)))))
                        (push (conn--make-preview-overlay (point) 1) ovs))))))))
          (setq success t)
          ovs)
      (unless success (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-inner-lines (&optional end)
  (let (ovs)
    (dolist (win (window-list-1 nil nil 'visible) ovs)
      (with-selected-window win
        (unless (apply #'derived-mode-p conn-dispatch-thing-ignored-modes)
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
                (push (conn--make-preview-overlay (point) 1) ovs))
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
                  (push (conn--make-preview-overlay (point) 1) ovs))))))))))

(defun conn--dispatch-inner-lines-end ()
  (conn--dispatch-inner-lines t))

(defvar conn--dispatch-overriding-map nil)

(defvar conn-dispatch-action-override-maps nil)

(define-minor-mode conn-dispatch-read-thing-mode
  "Read a thing for dispatch."
  :lighter " DISPATCH"
  :keymap (define-keymap
            "C-h" 'help
            "." 'reset-arg
            "C-d" 'forward-delete-arg
            "C-w" 'backward-delete-arg)
  (if conn-dispatch-read-thing-mode
      (thread-first
        (setq conn--dispatch-overriding-map
              (make-composed-keymap
               (reverse
                (append (list conn-dispatch-read-thing-mode-map)
                        conn-dispatch-thing-override-maps
                        conn-dispatch-action-override-maps))))
        (internal-push-keymap 'overriding-terminal-local-map))
    (internal-pop-keymap conn--dispatch-overriding-map
                         'overriding-terminal-local-map)))

(defun conn--dispatch-read-thing (action-maps &optional default-action)
  (let ((conn-dispatch-action-override-maps action-maps)
        (prompt (substitute-command-keys
                 (concat "\\<conn-dispatch-read-thing-mode-map>Thing (arg: "
                         (propertize "%s" 'face 'read-multiple-choice-face)
                         ", \\[reset-arg] reset arg; "
                         "\\[help] commands): %s")))
        (action default-action)
        keys cmd invalid thing-arg thing-sign)
    (conn--with-state conn-state
      (conn-dispatch-read-thing-mode 1)
      (unwind-protect
          (cl-prog
           nil
           :read-command
           (setq keys (read-key-sequence
                       (format prompt
                               (format (if thing-arg "%s%s" "[%s1]")
                                       (if thing-sign "-" "")
                                       thing-arg)
                               (cond
                                (invalid
                                 (propertize "Not a valid thing command"
                                             'face 'error))
                                (action (get action :conn-action-description))
                                (t ""))))
                 cmd (key-binding keys t)
                 invalid nil)
           :loop
           (pcase cmd
             (`(,thing ,finder . ,default-action)
              (cl-return
               (list thing finder (or action default-action)
                     (* (if thing-sign -1 1) (or thing-arg 1))
                     current-prefix-arg)))
             ('keyboard-quit
              (keyboard-quit))
             ('digit-argument
              (let ((digit (- (logand (elt keys 0) ?\177) ?0)))
                (setq thing-arg (if thing-arg (+ (* 10 thing-arg) digit) digit))))
             ('backward-delete-arg
              (setq thing-arg (floor thing-arg 10)))
             ('forward-delete-arg
              (setq thing-arg (thread-last
                                (log thing-arg 10)
                                floor
                                (expt 10)
                                (mod thing-arg))))
             ('reset-arg
              (setq thing-arg nil))
             ('negative-argument
              (setq thing-sign (not thing-sign)))
             ('help
              (conn-dispatch-read-thing-mode -1)
              (save-window-excursion
                (setq keys nil
                      cmd (intern
                           (completing-read
                            "Command: "
                            (lambda (string pred action)
                              (if (eq action 'metadata)
                                  `(metadata
                                    ,(cons 'affixation-function
                                           (conn--dispatch-make-command-affixation
                                            conn--dispatch-overriding-map))
                                    (category . conn-dispatch-command))
                                (complete-with-action action obarray string pred)))
                            (lambda (sym)
                              (and (functionp sym)
                                   (not (eq sym 'help))
                                   (or (get sym :conn-command-thing)
                                       (where-is-internal sym (list conn--dispatch-overriding-map) t))))
                            t))))
              (conn-dispatch-read-thing-mode 1)
              (go :loop))
             ((let (and thing (pred identity)) (get cmd :conn-command-thing))
              (cl-return
               (list thing (conn--dispatch-finder cmd)
                     (or action (conn--dispatch-default-action cmd))
                     (* (if thing-sign -1 1) (or thing-arg 1))
                     current-prefix-arg)))
             ((guard (where-is-internal cmd (list conn--dispatch-overriding-map) t))
              (setq action (unless (eq cmd action) cmd)))
             (_
              (setq invalid t)))
           (go :read-command))
        (message nil)
        (conn-dispatch-read-thing-mode -1)))))

(defun conn-dispatch-on-things (thing finder action arg &optional repeat)
  "Begin dispatching ACTION on a THING.

The user is first prompted for a either a THING or an ACTION
to be performed followed by a THING to perform it on.  If
no ACTION is selected the default ACTION is to go to the THING.

Actions and things are selected via keybindings.  Actions are
bound in the keymaps in `conn-dispatch-action-maps' which are
active during prompting.  Things are associated with movement
commands and pressing the binding for a movement command selects
that commands THING (e.g. forward-sexp will select sexp as the
THING to operate on).

Once a THING has been selected the user is prompted for a string and
the THING at the location selected is acted upon.

The string is read with an idle timeout of `conn-read-string-timeout'
seconds."
  (interactive
   (conn--dispatch-read-thing conn-dispatch-action-maps))
  (let ((current-prefix-arg arg)
        prefix-ovs labels)
    (unwind-protect
        (cl-loop
         initially do
         (setf prefix-ovs (thread-last
                            (funcall finder)
                            (seq-group-by (lambda (ov) (overlay-get ov 'window)))
                            (seq-sort (lambda (a _) (eq (selected-window) (car a)))))
               (alist-get (selected-window) prefix-ovs)
               (seq-sort (lambda (a b)
                           (< (abs (- (overlay-start a) (point)))
                              (abs (- (overlay-start b) (point)))))
                         (alist-get (selected-window) prefix-ovs))
               labels (or (conn--create-label-strings
                           (let ((sum 0))
                             (dolist (p prefix-ovs sum)
                               (setq sum (+ sum (length (cdr p))))))
                           conn-dispatch-label-characters)
                          (user-error "No matching candidates")))
         do
         (let* ((prefix (conn--read-labels
                         prefix-ovs
                         labels
                         'conn--dispatch-label-overlays
                         'prefix-overlay))
                (window (overlay-get prefix 'window))
                (pt (overlay-start prefix)))
           (setq conn-this-command-thing
                 (or (overlay-get prefix 'thing) thing))
           (funcall action window pt conn-this-command-thing))
         while repeat)
      (pcase-dolist (`(_ . ,ovs) prefix-ovs)
        (mapc #'delete-overlay ovs)))))

(defun conn-dispatch-over-things (thing finder action arg &optional repeat)
  "See `conn-dispatch-on-things', with default action `conn-dispatch-over'."
  (interactive
   (conn--dispatch-read-thing conn-dispatch-over-action-maps
                              'conn-dispatch-over))
  (conn-dispatch-on-things thing finder action arg repeat))

(defun conn--dispatch-isearch-matches ()
  (with-restriction (window-start) (window-end)
    (cl-loop for (beg . end) in (conn--isearch-matches)
             collect (conn--make-preview-overlay beg (- end beg)))))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let* ((prefix-ovs `((,(selected-window) . ,(conn--dispatch-isearch-matches))))
         (count (length (cdar prefix-ovs))))
    (unwind-protect
        (let* ((labels (conn--create-label-strings
                        count conn-dispatch-label-characters))
               (prefix (conn--read-labels prefix-ovs
                                          labels
                                          'conn--dispatch-label-overlays
                                          'prefix-overlay))
               (pt (overlay-start prefix)))
          (isearch-done)
          (goto-char pt))
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
      (cl-loop for (beg . end) in (reverse conn--current-expansions)
               when (and (= (point) beg)
                         (/= (mark t) end))
               return (goto-char end)
               when (and (= (point) end)
                         (/= (mark t) beg))
               return (goto-char beg))
    (conn-exchange-mark-command)))

(defun conn-expand (arg)
  "Expend region by semantic units.

If the region is active only the `point' is moved.
Expansions are provided by functions in `conn-expansion-functions'."
  (interactive "p")
  (unless (conn--valid-expansions-p)
    (setq conn--current-expansions (conn--expand-create-expansions)))
  (if (< arg 0)
      (conn-contract (- arg))
    (dotimes (_ arg)
      (cond ((and (region-active-p)
                  (= (point) (region-beginning)))
             (catch 'term
               (pcase-dolist (`(,beg . _) conn--current-expansions)
                 (when (< beg (point)) (throw 'term (goto-char beg))))
               (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (catch 'term
               (pcase-dolist (`(_ . ,end) conn--current-expansions)
                 (when (> end (point)) (throw 'term (goto-char end))))
               (user-error "No more expansions")))
            (t
             (pcase (seq-find (pcase-lambda (`(,beg . ,end))
                                (or (< beg (region-beginning))
                                    (> end (region-end))))
                              conn--current-expansions)
               (`(,beg . ,end)
                (goto-char (if (= (point) (region-beginning)) beg end))
                (conn--push-ephemeral-mark (if (= (point) (region-end)) beg end)))
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
             (catch 'term
               (pcase-dolist (`(,beg . _) (reverse conn--current-expansions))
                 (when (> beg (point)) (throw 'term (goto-char beg))))
               (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (catch 'term
               (pcase-dolist (`(_ . ,end) (reverse conn--current-expansions))
                 (when (< end (point)) (throw 'term (goto-char end))))
               (user-error "No more expansions")))
            (t
             (pcase (seq-find (pcase-lambda (`(,beg . ,end))
                                (or (> beg (region-beginning))
                                    (< end (region-end))))
                              (reverse conn--current-expansions))
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

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

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

(defun conn-narrow-ring-to-register (register)
  "Store narrow ring in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-narrow-register)))

(defun conn--narrow-ring-record (beg end)
  (conn--thread
      (cons (conn--create-marker beg) (conn--create-marker end))
      -->
    (setq conn-narrow-ring (cons --> (delete --> conn-narrow-ring)))))

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

(defun conn-thing-to-narrow-ring (thing-mover arg &optional pulse)
  "Add region defined by THING-MOVER called with ARG to narrow ring.
With prefix arg REGISTER add to narrow ring register instead."
  (interactive
   (progn
     (deactivate-mark)
     (append
      (conn--read-thing-mover "Thing Mover" nil t)
      (list t))))
  (pcase-let ((`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg)))
    (conn--narrow-ring-record beg end)
    (when (and pulse (not executing-kbd-macro))
      (pulse-momentary-highlight-region beg end 'region))))

(defun conn-region-to-narrow-ring (&optional pulse)
  "Add the region from BEG to END to the narrow ring.
Interactively defaults to the current region.
With prefix arg REGISTER add to narrow ring register instead."
  (interactive (list t))
  (let ((beg (region-beginning))
        (end (region-end)))
    (conn--narrow-ring-record beg end)
    (when (and pulse (not executing-kbd-macro))
      (pulse-momentary-highlight-region beg end 'region))))

(defun conn-clear-narrow-ring ()
  "Remove all narrowings from the `conn-narrow-ring'."
  (interactive)
  (cl-loop for (beg . end) in conn-narrow-ring do
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

(defun conn-isearch-in-narrow-p (beg end)
  (cl-loop for narrowing in conn-narrow-ring
           thereis (<= (car narrowing) beg end (cdr narrowing))))

(defun conn-isearch-narrow-ring-forward ()
  "`isearch-forward' restricted to regions in `conn-narrow-ring'."
  (interactive)
  (let ((isearch-filter-predicate isearch-filter-predicate))
    (add-function :after-while isearch-filter-predicate 'conn-isearch-in-narrow-p
                  '((isearch-message-prefix . "[NARROW] ")))
    (isearch-forward)))

(defun conn-isearch-narrow-ring-backward ()
  "`isearch-backward' restricted to regions in `conn-narrow-ring'."
  (interactive)
  (let ((isearch-filter-predicate isearch-filter-predicate))
    (add-function :after-while isearch-filter-predicate 'conn-isearch-in-narrow-p
                  '((isearch-message-prefix . "[NARROW] ")))
    (isearch-backward)))

  
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
    (if (or (< (- end beg) 32)
            (<= end (save-excursion
                      (goto-char beg)
                      (pos-eol))))
        (buffer-substring-no-properties beg end)
      "")))

(defun conn--replace-read-args (prompt regexp-flag beg end &optional noerror)
  (unless noerror (barf-if-buffer-read-only))
  (conn--with-region-emphasis beg end
    (save-mark-and-excursion
      (let* ((delimited-flag (and current-prefix-arg
                                  (not (eq current-prefix-arg '-))))
             (default (conn--replace-read-default))
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
                        :filter (lambda (mb me) (<= beg mb me end))
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

(defun conn-replace-in-thing (thing-mover arg from-string to-string
                                          &optional delimited backward query-flag)
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn--read-thing-mover "Thing Mover" nil t))
                (`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg))
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
                    nil beg end))))
     (append (list thing-mover arg) common)))
  (pcase-let ((`(,beg ,end . ,_) conn-last-bounds-of-command))
    (save-window-excursion
      (save-excursion
        (perform-replace from-string to-string query-flag nil
                         delimited nil nil beg end backward)))))

(defun conn-regexp-replace-in-thing (thing-mover arg from-string to-string
                                                 &optional delimited backward query-flag)
  (interactive
   (pcase-let* ((`(,thing-mover ,arg)
                 (conn--read-thing-mover "Thing Mover" nil t))
                (`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg))
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
                    t beg end))))
     (append (list thing-mover arg) common)))
  (pcase-let ((`(,beg ,end . ,_) conn-last-bounds-of-command))
    (save-window-excursion
      (save-excursion
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

(defun conn-command-to-register (register arg)
  "Store command in REGISTER."
  (interactive
   (list (register-read-with-preview "Command to register: ")
         (abs (prefix-numeric-value current-prefix-arg))))
  (set-register
   register
   (make-conn-command-register
    :command (let* ((elt (nth (1- arg) command-history))
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
  (when-let ((frame (conn-tab-register-frame val))
             (index (and (frame-live-p frame)
                         (with-selected-frame (conn-tab-register-frame val)
                           (conn--get-tab-index-by-cookie
                            (conn-tab-register-cookie val))))))
    (select-frame-set-input-focus frame)
    (tab-bar-select-tab (1+ index))))

(cl-defmethod register-val-describe ((val conn-tab-register) _arg)
  (princ (format "Tab:  %s"
                 (if (eq (selected-frame) (conn-tab-register-frame val))
                     (when-let ((index (conn--get-tab-index-by-cookie
                                        (conn-tab-register-cookie val))))
                       (conn--thread index -->
                         (nth --> (funcall tab-bar-tabs-function))
                         (if (eq (car -->) 'current-tab)
                             (propertize "*CURRENT TAB*" 'face 'error)
                           (alist-get 'name -->))))
                   "on another frame"))))

(defun conn-tab-to-register (register)
  "Store tab in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-tab-register)))

;;;;; Isearch Commands

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

;;;;; Kmacro Commands

(defun conn-kmacro-replace-region (string beg end)
  (interactive
   (nconc
    (list (filter-buffer-substring (region-beginning) (region-end)))
    (take 2 (cdr (conn--read-thing-region "Define Region")))))
  (conn--with-region-emphasis beg end
    (thread-first
      (conn--kapply-matches string beg end nil nil current-prefix-arg nil t)
      conn--kapply-merge-undo
      conn--kapply-save-restriction
      conn--kapply-save-excursion
      conn--kapply-change-region
      (conn--kapply-with-state 'conn-emacs-state)
      conn--kmacro-apply)))

(defun conn-kmacro-emacs-on-region (string beg end)
  (interactive
   (nconc
    (list (filter-buffer-substring (region-beginning) (region-end)))
    (take 2 (cdr (conn--read-thing-region "Define Region")))))
  (conn--with-region-emphasis beg end
    (thread-first
      (conn--kapply-matches string beg end nil nil current-prefix-arg nil t)
      conn--kapply-merge-undo
      conn--kapply-save-restriction
      conn--kapply-save-excursion
      (conn--kapply-with-state 'conn-emacs-state)
      conn--kmacro-apply)))

(defun conn-kmacro-conn-on-region (string beg end)
  (interactive
   (nconc
    (list (filter-buffer-substring (region-beginning) (region-end)))
    (take 2 (cdr (conn--read-thing-region "Define Region")))))
  (conn--with-region-emphasis beg end
    (thread-first
      (conn--kapply-matches string beg end nil nil current-prefix-arg nil t)
      conn--kapply-merge-undo
      conn--kapply-save-restriction
      conn--kapply-save-excursion
      (conn--kapply-with-state 'conn-state)
      conn--kmacro-apply)))

(defun conn-kmacro-replace-rectangle ()
  (interactive)
  (require 'rect)
  (thread-first
    (conn--kapply-region-iterator
     (extract-rectangle-bounds (region-beginning) (region-end)))
    conn--kapply-merge-undo
    conn--kapply-save-restriction
    conn--kapply-save-excursion
    conn--kapply-change-region
    (conn--kapply-with-state 'conn-emacs-state)
    conn--kmacro-apply))

(defun conn-kmacro-emacs-on-rectangle ()
  (interactive)
  (require 'rect)
  (thread-first
    (conn--kapply-region-iterator
     (extract-rectangle-bounds (region-beginning) (region-end)))
    conn--kapply-merge-undo
    conn--kapply-save-restriction
    conn--kapply-save-excursion
    (conn--kapply-with-state 'conn-emacs-state)
    conn--kmacro-apply))

(defun conn-kmacro-conn-on-rectangle ()
  (interactive)
  (require 'rect)
  (thread-first
    (conn--kapply-region-iterator
     (extract-rectangle-bounds (region-beginning) (region-end)))
    conn--kapply-merge-undo
    conn--kapply-save-restriction
    conn--kapply-save-excursion
    (conn--kapply-with-state 'conn-state)
    conn--kmacro-apply))

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
     (conn--read-thing-mover "Mover"
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

(defun conn-comment-or-uncomment-region-and-empty (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (comment-normalize-vars)
  (if (comment-only-p beg end)
      (uncomment-region beg end)
    (let ((comment-empty-lines t))
      (comment-region beg end))))

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

(defun conn-org-tree-edit-insert-heading ()
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
      (when-let ((pos (or (save-excursion
                            (backward-char)
                            (catch 'term
                              (while (search-backward string nil t)
                                (when (conn--region-visible-p (match-beginning 0)
                                                              (match-end 0))
                                  (throw 'term (match-beginning 0))))))
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
      (when-let ((pos (or (save-excursion
                            (forward-char)
                            (catch 'term
                              (while (search-forward string nil t)
                                (when (conn--region-visible-p (match-beginning 0)
                                                              (match-end 0))
                                  (throw 'term (match-beginning 0))))))
                          (user-error "\"%s\" not found." string))))
        (goto-char pos)))))

(defun conn--goto-string-handler (beg)
  (when (and (not (region-active-p))
             (memq this-command '(conn-goto-string-forward
                                  conn-goto-string-backward))
             (not (eq this-command last-command)))
    (push-mark beg t)))

(conn-set-command-handler 'conn--goto-string-handler
                          'conn-forward-char
                          'conn-backward-char)

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
   (append (conn--read-thing-mover "Thing Mover")
           (when current-prefix-arg
             (list (register-read-with-preview "Register: ")))))
  (pcase-let ((`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg)))
    (conn-copy-region beg end register)
    (unless executing-kbd-macro
      (pulse-momentary-highlight-region beg end))))

(defun conn-narrow-to-region (thing-mover arg &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive
   (append (conn--read-thing-mover "Thing Mover"
                                   (when current-prefix-arg
                                     (prefix-numeric-value current-prefix-arg))
                                   t)
           (list t)))
  (pcase-let ((`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg)))
    (narrow-to-region beg end)
    (when record (conn--narrow-ring-record beg end))
    (when (called-interactively-p 'interactive)
      (message "Buffer narrowed"))))

(defun conn-narrow-indirect-to-region (thing-mover arg &optional interactive)
  "Narrow to THING at point.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive
   (append (conn--read-thing-mover "Thing Mover"
                                   (when current-prefix-arg
                                     (prefix-numeric-value current-prefix-arg))
                                   t)
           (list t)))
  (pcase-let ((`(,beg ,end . ,_) (conn-bounds-of-command thing-mover arg)))
    (conn--narrow-indirect beg end interactive)
    (when (called-interactively-p 'interactive)
      (message "Buffer narrowed indirect"))))

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

(defun conn-join-lines (start end)
  "`delete-indentation' in region from START and END."
  (interactive (list (region-beginning)
                     (region-end)))
  (delete-indentation nil start end)
  (indent-according-to-mode))

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
  (when-let ((cs (and (conn--point-in-comment-p)
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
                     " ")
                   "to register:")
           (register-read-with-preview)
           (copy-to-register nil nil t t)))
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

(defun conn-duplicate-region (beg end &optional arg)
  "Duplicates the current region ARG times.
Attempts to intelligently insert separating whitespace between
regions."
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

(defun conn-duplicate-and-comment-region (beg end &optional arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive (list (region-beginning)
                     (region-end)
                     (prefix-numeric-value current-prefix-arg)))
  (comment-normalize-vars)
  (save-mark-and-excursion
    (conn-duplicate-region beg end arg)
    (comment-region (region-beginning)
                    (region-end))))

;;;;; Window Commands

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
     (cons (conn--prompt-for-window (window-list-1 nil 'nomini)) 'reuse))))

(defun conn-this-window-prefix ()
  (interactive)
  (display-buffer-override-next-command
   'display-buffer-same-window
   nil "[current-window]")
  (message "Display next command buffer in current window…"))

;;;;; Transition Functions

(defun conn-change-whole-line (&optional arg)
  (interactive "P")
  (kill-whole-line arg)
  (open-line 1)
  (indent-according-to-mode)
  (conn-emacs-state))

(defun conn-change-line ()
  (interactive)
  (call-interactively (key-binding conn-kill-line-keys t))
  (conn-emacs-state))

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
  (conn-emacs-state))

(defun conn-emacs-state-open-line (&optional arg)
  "Open line and enter `conn-emacs-state'.

If ARG is non-nil move down ARG lines before opening line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent)
  (conn-emacs-state))

(defun conn-emacs-state-overwrite (&optional arg)
  "Enter emacs state in `overwrite-mode'.
`overwrite-mode' will be turned off when when emacs state is exited.
If ARG is non-nil enter emacs state in `binary-overwrite-mode' instead."
  (interactive "P")
  (let ((hook (make-symbol "emacs-state-overwrite-hook")))
    (conn-emacs-state)
    (fset hook (lambda ()
                 (unless (eq conn-current-state 'conn-emacs-state)
                   (overwrite-mode -1)
                   (remove-hook 'conn-transition-hook hook))))
    (add-hook 'conn-transition-hook hook)
    (if arg
        (binary-overwrite-mode 1)
      (overwrite-mode 1))))

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
         (conn-emacs-state))
        (t
         (funcall (conn--without-conn-maps
                    (key-binding conn-delete-region-keys t))
                  start end)
         (conn-emacs-state))))

(defun conn-emacs-state-eol (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (end-of-line N)
  (conn-emacs-state))

(defun conn-emacs-state-bol (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (beginning-of-line N)
  (conn-emacs-state))

(defun conn-emacs-state-eoil (&optional N)
  "Move point to end of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-end-of-inner-line N)
  (conn-emacs-state))

(defun conn-emacs-state-boil (&optional N)
  "Move point to beginning of line and enter `conn-emacs-state'."
  (interactive "P")
  (conn-beginning-of-inner-line N)
  (conn-emacs-state))


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
   (propertize "Window: " 'face 'bold)
   "prefix arg: "
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
   "\\[conn-wincontrol-transpose-window] \\[conn-wincontrol-yank-window]: transpose/yank; "
   "\\[conn-wincontrol-mru-window]: last win"))

(defconst conn--wincontrol-window-format-2
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Window: " 'face 'bold)
   "prefix arg: "
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
   (propertize "Tab: " 'face 'bold)
   "prefix arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[conn-tab-to-register]: store"
   "\n"
   "\\[tab-bar-move-window-to-tab]: win to new tab; "
   "\\[tab-previous] \\[tab-next]: next/prev; "
   "\\[conn-wincontrol-tab-new] \\[tab-bar-duplicate-tab] \\[conn-wincontrol-tab-close]: "
   "new/clone/kill; "
   "\\[tab-bar-detach-tab]: tear off"))

(defconst conn--wincontrol-frame-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "Frame: " 'face 'bold)
   "prefix arg: "
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
   "\\[iconify-or-deiconify-frame] \\[make-frame-command]: iconify/create; "
   "\\[delete-frame] \\[delete-other-frames]: delete/other"))

(defconst conn--wincontrol-simple-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl: " 'face 'bold)
   "prefix arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) "; "
   "\\[conn-wincontrol-digit-argument-reset]: reset; "
   "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
   "\\[conn-wincontrol-exit]: exit; "
   "\\[conn-wincontrol-scroll-up] "
   "\\[conn-wincontrol-scroll-down]: "
   "scroll"))

(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "C-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-DEL" 'conn-wincontrol-digit-argument-reset
  "C-w" 'conn-wincontrol-backward-delete-arg
  "C-d" 'conn-wincontrol-forward-delete-arg
  "C-0" 'delete-window
  "C-1" 'delete-other-windows
  "C-2" 'split-window-below
  "C-3" 'split-window-right
  "C-8" 'conn-tab-to-register
  "C-9" 'tab-close
  "C-g" 'conn-wincontrol-abort
  "C-M-0" 'kill-buffer-and-window
  "C-M-d" 'delete-other-frames
  "M-1" 'iconify-or-deiconify-frame
  "M-2" 'make-frame-command
  "M-/" 'undelete-frame
  "M-o" 'other-frame
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "C-u" 'conn-wincontrol-universal-arg
  "`" 'quit-window
  "-" 'conn-wincontrol-invert-argument
  "." 'conn-wincontrol-digit-argument-reset
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
  "<next>" 'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "<right>" 'conn-wincontrol-windmove-right
  "<up>" 'conn-wincontrol-windmove-up
  "<tab>" 'conn-wincontrol-other-window-scroll-up
  "TAB" 'conn-wincontrol-other-window-scroll-up
  "DEL" 'conn-wincontrol-scroll-down
  "SPC" 'conn-wincontrol-scroll-up
  "M-TAB" 'conn-wincontrol-other-window-scroll-down
  "M-<tab>" 'conn-wincontrol-other-window-scroll-down
  "C-s" 'conn-wincontrol-isearch
  "C-r" 'conn-wincontrol-isearch-backward
  "," 'conn-wincontrol-maximize-horizontally
  ";" 'conn-wincontrol-exit-to-initial-win
  "b" 'conn-tab-to-register
  "C" 'tab-bar-duplicate-tab
  "c" (conn-remapping-command (key-parse "C-c"))
  "d" 'delete-window
  "e" 'conn-wincontrol-exit
  "F" 'toggle-frame-fullscreen
  "f" 'conn-goto-window
  "g" 'delete-other-windows
  "C-f" 'conn-wincontrol-help
  "C-b" 'conn-wincontrol-help-backward
  "H" 'maximize-window
  "h" 'enlarge-window
  "i" 'conn-wincontrol-tab-new
  "I" 'tab-next
  "j" 'previous-buffer
  "J" 'bury-buffer
  "k" 'conn-wincontrol-tab-close
  "K" 'tab-previous
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
  "x" (conn-remapping-command (key-parse "C-x"))
  "X" 'conn-wincontrol-mru-window
  "r" 'conn-wincontrol-split-right
  "R" 'conn-wincontrol-isearch-other-window-backward
  "s" 'shrink-window
  "S" 'conn-wincontrol-isearch-other-window
  "t" 'tab-switch
  "u" 'conn-wincontrol-previous-window
  "U" 'tab-bar-detach-tab
  "v" 'conn-wincontrol-split-vertically
  "w" 'enlarge-window-horizontally
  "q" 'conn-wincontrol-transpose-window
  "y" 'conn-wincontrol-yank-window
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
  (conn-wincontrol-mode 1))

(defun conn--wincontrol-pre-command ()
  (when (or conn--wincontrol-arg (< conn--wincontrol-arg-sign 0))
    (setq prefix-arg (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message nil)))

(defun conn--wincontrol-post-command ()
  (cond
   ((not (eq conn-wincontrol-map (cadr overriding-terminal-local-map)))
    ;; Something else is using overriding-terminal-local-map
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
  (dolist (state conn-states)
    (set-face-foreground (get state :conn-lighter-face)
                         (face-foreground 'mode-line)))
  (invert-face 'mode-line)
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (dolist (state conn-states)
    (set-face-foreground (get state :conn-lighter-face)
                         'unspecified))
  (invert-face 'mode-line))

(defun conn-wincontrol-one-command ()
  (interactive)
  (let ((pre-hook (make-symbol "one-command-pre-hook")))
    (fset pre-hook (lambda ()
                     (unless (memq this-command '(conn-wincontrol-forward-delete-arg
                                                  conn-wincontrol-backward-delete-arg
                                                  conn-wincontrol-digit-argument-reset
                                                  conn-wincontrol-invert-argument
                                                  conn-wincontrol-digit-argument
                                                  conn-wincontrol-universal-arg))
                       (remove-hook 'pre-command-hook pre-hook)
                       (conn-wincontrol-exit))))
    (conn-wincontrol)
    (add-hook 'pre-command-hook pre-hook 90)))

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
      (setq conn--wincontrol-arg (+ (if (>= (or conn--wincontrol-arg 1) 0) N (- N))
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
  (setq conn--wincontrol-arg (conn--thread (log conn--wincontrol-arg 10)
                                 -->
                               (floor -->)
                               (expt 10 -->)
                               (mod conn--wincontrol-arg -->))))

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
          (remove (selected-window) (window-list-1 nil 'nomini 'visible)))))
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
  (when-let ((mru (get-mru-window 0 nil t t)))
    (select-window mru)))

(defun conn-wincontrol-transpose-window ()
  "Prompt for window and swap current window and other window."
  (interactive)
  (window-swap-states nil (get-mru-window 0 nil t t)))

(defun conn-wincontrol-yank-window ()
  (interactive)
  (save-selected-window
    (window-swap-states nil (get-mru-window 0 nil t t))))

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

(defun conn-wincontrol-tab-close ()
  "Close current tab.
See `tab-close'."
  (interactive)
  (tab-bar-close-tab))

(defun conn-wincontrol-tab-new ()
  "Create a new tab."
  (interactive)
  (tab-new))

(defun conn-wincontrol-maximize-horizontally ()
  (interactive)
  (enlarge-window-horizontally (window-max-delta nil t)))

(defun conn-wincontrol-maximize-vertically ()
  (interactive)
  (enlarge-window (window-max-delta)))

(defun conn--wincontrol-split-window-state (state)
  (let (params windows)
    (dolist (elem state)
      (if (memq (car-safe elem) '(vc hc leaf))
          (push elem windows)
        (push elem params)))
    (cons (reverse params) (reverse windows))))

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

;; FIXME: vertical columns shrink horizontally when reversed for some reason
(defun conn--wincontrol-reverse-window (state &optional recursive)
  (pcase-let* ((`(,params . ,windows)
                (conn--wincontrol-split-window-state state)))
    (when (length> windows 1)
      (setf (alist-get 'last (cdar windows)) t
            windows (reverse windows)
            (car windows) (assq-delete-all 'last (car windows))))
    (append params
            (if recursive
                (cl-loop for win in windows collect
                         (conn--wincontrol-reverse-window win t))
              windows))))

(defun conn-wincontrol-reverse (arg)
  "Reverse order of windows in ARGth parent window.
When ARG is nil the root window is used."
  (interactive "P")
  (let ((window
         (or (when arg
               (cl-loop with main = (window-main-window)
                        for win = (selected-window) then (window-parent win)
                        repeat (abs arg)
                        while (and win (not (eq win main)))
                        finally return win))
             (window-main-window))))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reverse-window (and arg (< arg 0)))
      (window-state-put window))))

(defun conn-wincontrol-reflect (arg)
  "Rotate all window arrangements within ARGth parent window of `selected-window'.
When ARG is nil the root window is used."
  (interactive "P")
  (let ((window
         (or (when arg
               (cl-loop with main = (window-main-window)
                        for win = (selected-window) then (window-parent win)
                        repeat (abs arg)
                        while (and win (not (eq win main)))
                        finally return win))
             (window-main-window))))
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
  "RET" 'whitespace-cleanup
  "TAB" 'indent-rigidly
  "$" 'ispell-region
  "*" 'calc-grab-region
  ";" 'comment-or-uncomment-region
  ":" 'conn-comment-or-uncomment-region-and-empty
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule
  "c" 'conn-region-case-prefix
  "D" 'conn-duplicate-and-comment-region
  "d" 'conn-duplicate-region
  "g" 'conn-rgrep-region
  "i" 'clone-indirect-buffer
  "k" 'delete-region
  "l" 'conn-join-lines
  "m t" 'conn-kmacro-replace-region
  "m e" 'conn-kmacro-emacs-on-region
  "m c" 'conn-kmacro-conn-on-region
  "I" 'indent-rigidly
  "N" 'conn-narrow-indirect-to-region
  "n" 'conn-narrow-to-region
  "s" 'conn-sort-prefix
  "o" 'conn-occur-region
  "V" 'vc-region-history
  "y" 'yank-rectangle
  "j" 'join-line
  "q" 'conn-replace-in-thing
  "u" 'conn-regexp-replace-in-thing
  "DEL" 'clear-rectangle)

(when (version<= "30" emacs-version)
  (keymap-set conn-region-map "U" 'replace-regexp-as-diff)
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
  "M-<return>" 'conn-isearch-exit-and-mark
  "M-\\" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

(defvar-keymap conn-search-map
  "s" 'conn-isearch-region-forward
  "r" 'conn-isearch-region-backward
  "o" 'occur
  "l" 'locate
  "m B" 'multi-isearch-buffers-regexp
  "m F" 'multi-isearch-files-regexp
  "m b" 'multi-isearch-buffers
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
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'rectangle-mark-mode)
  "C-y" 'conn-yank-replace-rectangle
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "d" 'open-rectangle
  "<backspace>" 'clear-rectangle
  "C-d" 'delete-whitespace-rectangle
  "#" 'rectangle-number-lines
  "r m t" 'conn-kmacro-replace-rectangle
  "r m e" 'conn-kmacro-emacs-on-rectangle
  "r m c" 'conn-kmacro-conn-on-rectangle)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

(defvar-keymap conn-edit-map
  "F" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "DEL" 'conn-change-whole-line
  "h" 'conn-change-line
  "_" 'conn-command-to-register
  "o" 'conn-open-line-and-indent
  "n" 'conn-open-line-above
  "m" 'conn-open-line
  "i" 'conn-emacs-state-open-line-above
  "k" 'conn-emacs-state-open-line
  "l" 'conn-emacs-state-eoil
  "e" 'conn-emacs-state-eol
  "j" 'conn-emacs-state-boil
  "a" 'conn-emacs-state-bol
  "t" 'conn-emacs-state-overwrite
  "b" 'conn-emacs-state-overwrite-binary
  "v" 'conn-region-to-narrow-ring
  "f" 'conn-thing-to-narrow-ring
  "x" 'conn-narrow-ring-prefix
  "s" 'conn-surround-thing
  "d" 'duplicate-dwim
  "w j" 'conn-kill-prepend-region
  "w l" 'conn-kill-append-region
  "c j" 'conn-append-region
  "c l" 'conn-append-region
  "c v" 'conn-copy-thing
  "y" 'yank-in-context)

(defvar-keymap conn-movement-map
  ">" 'forward-line
  "<" 'conn-backward-line
  "o" (conn-remapping-command conn-forward-word-keys)
  "O" 'forward-symbol
  "U" 'conn-backward-symbol
  "u" (conn-remapping-command conn-backward-word-keys)
  "(" (conn-remapping-command conn-backward-list-keys)
  ")" (conn-remapping-command conn-forward-list-keys)
  "[" (conn-remapping-command conn-backward-up-list-keys)
  "]" (conn-remapping-command conn-down-list-keys)
  "{" (conn-remapping-command conn-backward-sentence-keys)
  "}" (conn-remapping-command conn-forward-sentence-keys)
  "I" (conn-remapping-command conn-backward-paragraph-keys)
  "i" (conn-remapping-command conn-previous-line-keys)
  "J" 'conn-beginning-of-inner-line
  "j" `(menu-item
        ""
        nil
        :filter ,(lambda (&rest _)
                   (let ((binding (key-binding conn-backward-char-keys t)))
                     (if (eq binding 'forward-char)
                         'conn-backward-char
                       binding))))
  "K" (conn-remapping-command conn-forward-paragraph-keys)
  "k" (conn-remapping-command conn-next-line-keys)
  "L" 'conn-end-of-inner-line
  "l" `(menu-item
        ""
        nil
        :filter ,(lambda (&rest _)
                   (let ((binding (key-binding conn-forward-char-keys t)))
                     (if (eq binding 'forward-char)
                         'conn-forward-char
                       binding))))
  "M" (conn-remapping-command conn-end-of-defun-keys)
  "m" (conn-remapping-command conn-forward-sexp-keys)
  "N" (conn-remapping-command conn-beginning-of-defun-keys)
  "n" (conn-remapping-command conn-backward-sexp-keys))

(define-keymap
  :keymap conn-state-map
  :parent conn-movement-map
  "e" 'conn-emacs-state
  "t" 'conn-change
  ":" 'conn-wincontrol-one-command
  "<remap> <toggle-input-method>" 'conn-toggle-input-method
  "`" 'other-window
  "|" 'conn-shell-command-on-region
  "'" 'conn-other-place-prefix
  "+" 'conn-set-register-seperator
  "." 'repeat
  "/" (conn-remapping-command conn-undo-keys)
  ";" 'conn-wincontrol
  "<tab>" 'indent-region
  "TAB" 'indent-region
  "=" 'indent-relative
  "?" (conn-remapping-command conn-undo-redo-keys)
  "\"" 'conn-surround-thing
  "\\" 'conn-kapply-prefix
  "_" 'repeat-complex-command
  "SPC" 'conn-set-mark-command
  "M-0" 'tab-close
  "M-1" 'delete-other-windows-vertically
  "M-2" 'tab-new
  "M-3" 'make-frame-command
  "M-7" 'kill-buffer
  "M-8" 'tear-off-window
  "M-9" 'tab-detach
  "M-R" 'conn-wincontrol-maximize-horizontally
  "M-V" 'conn-wincontrol-maximize-vertically
  "M-y" 'conn-completing-yank-replace
  "C-+" 'maximize-window
  "C--" 'shrink-window-if-larger-than-buffer
  "C-0" 'delete-window
  "C-1" 'delete-other-windows
  "C-2" 'split-window-below
  "C-3" 'split-window-right
  "C-4" (conn-remapping-command (key-parse "C-x 4"))
  "C-5" (conn-remapping-command (key-parse "C-x 5"))
  "C-8" 'conn-tab-to-register
  "C-9" 'quit-window
  "C-=" 'balance-windows
  "C-M-0" 'kill-buffer-and-window
  "C-M-l" 'conn-recenter-on-region
  "C-M-S-l" 'conn-recenter-on-region-other-window
  "C-t" (conn-remapping-command (key-parse "C-x t"))
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" conn-edit-map
  "c" (conn-remapping-command (key-parse "C-c"))
  "C" 'conn-copy-region
  "d" (conn-remapping-command conn-delete-char-keys)
  "f" 'conn-dispatch-on-things
  "F" 'conn-dispatch-over-things
  "g" (conn-remapping-command (key-parse "M-g"))
  "h" 'conn-expand
  "H" conn-mark-thing-map
  "p" 'conn-register-load
  "P" 'conn-register-prefix
  "q" 'conn-transpose-regions
  "r" 'conn-region-map
  "R" 'conn-rectangle-mark
  "s" (conn-remapping-command (key-parse "M-s"))
  "V" 'conn-narrow-to-region
  "v" 'conn-toggle-mark-command
  "w" 'conn-kill-region
  "W" 'widen
  "X" 'conn-narrow-ring-prefix
  "x" (conn-remapping-command (key-parse "C-x"))
  "Y" 'yank-from-kill-ring
  "y" (conn-remapping-command conn-yank-keys)
  "z" 'conn-exchange-mark-command)

(define-keymap
  :keymap conn-org-edit-state-map
  "e" 'conn-emacs-state
  "SPC" 'conn-scroll-up
  "<backspace>" 'conn-scroll-down
  "DEL" 'conn-scroll-down
  "." 'point-to-register
  "/" (conn-remapping-command conn-undo-keys)
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "*" 'conn-org-tree-edit-insert-heading
  "<" 'org-promote-subtree
  ">" 'org-demote-subtree
  "?" (conn-remapping-command conn-undo-redo-keys)
  "f" 'conn-dispatch-on-things
  "F" 'conn-dispatch-over-things
  "C" 'org-toggle-comment
  "c" (conn-remapping-command (key-parse "C-c"))
  "b" conn-edit-map
  "g" (conn-remapping-command (key-parse "M-g"))
  "i" 'org-backward-heading-same-level
  "I" 'org-metaup
  "J" 'org-metaleft
  "j" 'org-previous-visible-heading
  "k" 'org-forward-heading-same-level
  "K" 'org-metadown
  "L" 'org-metaright
  "l" 'org-next-visible-heading
  "M" 'org-mark-subtree
  "m" 'org-backward-element
  "n" 'org-forward-element
  "N" 'org-toggle-narrow-to-subtree
  "O" 'org-next-block
  "p" 'conn-register-load
  "s" (conn-remapping-command (key-parse "M-s"))
  "T" 'org-todo
  "t" 'org-sparse-tree
  "U" 'org-previous-block
  "u" 'org-up-element
  "W" 'widen
  "w" 'org-refile
  "x" (conn-remapping-command (key-parse "C-x"))
  "z" 'conn-exchange-mark-command)

(define-keymap
  :keymap global-map
  "C-S-w" 'delete-region
  "C-x /" 'tab-bar-history-back
  "C-x 4 /" 'tab-bar-history-back
  "C-x 4 ?" 'tab-bar-history-forward
  "C-x 4 -" 'conn-window-resize-map
  "C-x ?" 'tab-bar-history-forward
  "C-x n n" 'conn-narrow-to-region
  "C-x n N" 'conn-narrow-indirect-to-region
  "C-x r \\" 'conn-set-register-seperator
  "C-x r ." 'conn-last-macro-dispatch-to-register
  "C-x r !" 'kmacro-to-register
  "C-x r W" 'conn-unset-register
  "C-x t j" 'conn-register-load
  "C-x t s" 'tab-switch
  "C-x t a" 'conn-tab-to-register
  "C-`" 'other-window
  "C-x m" 'conn-kmacro-prefix)

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
        (cl-pushnew 'conn--major-mode-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-mode-maps emulation-mode-map-alists)
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
                            conn--major-mode-maps
                            conn--local-mode-maps)
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
        (pcase-dolist (`(_ . ,hooks) conn-input-method-overriding-modes)
          (dolist (hook hooks)
            (add-hook hook 'conn--activate-input-method nil t)))
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'clone-indirect-buffer-hook #'conn--delete-mark-cursor nil t)
        (setq conn--input-method current-input-method)
        (conn--setup-major-mode-maps)
        (funcall (conn--default-state-for-buffer)))
    (when conn-current-state
      (funcall (get conn-current-state :conn-transition-fn) :exit))
    (conn--clear-overlays)
    (pcase-dolist (`(_ . ,hooks) conn-input-method-overriding-modes)
      (dolist (hook hooks)
        (remove-hook hook #'conn--activate-input-method t)))
    (remove-hook 'change-major-mode-hook #'conn--clear-overlays t)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (remove-hook 'clone-indirect-buffer-hook #'conn--delete-mark-cursor t)
    (when (and conn--input-method (not current-input-method))
      (activate-input-method conn--input-method))))

(defun conn-initialize-buffer ()
  "Maybe initialize `conn-local-mode' in current buffer.
Check `conn-enable-in-buffer-hook' and `conn-disable-in-buffer-hook' to
determine if `conn-local-mode' should be enabled."
  (when (ignore-errors
          (and (run-hook-with-args-until-success 'conn-enable-in-buffer-hook)
               (run-hook-with-args-until-failure 'conn-disable-in-buffer-hook)))
    (conn-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn-initialize-buffer
  :group 'conn
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (if conn-mode
        (progn
          (keymap-set minibuffer-mode-map "C-M-y" 'conn-yank-region-to-minibuffer)
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50))
      (when (eq (keymap-lookup minibuffer-mode-map "C-M-y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "C-M-y"))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook))))

(provide 'conn)


;;; Load Extensions

(with-eval-after-load 'corfu
  (defun conn--exit-completion ()
    (completion-in-region-mode -1))
  (add-hook 'conn-transition-hook 'conn--exit-completion))

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
  (defvar org-link-any-re)

  (conn-register-thing
   'org-paragraph
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'org-paragraph t)
   :forward-op 'org-forward-paragraph
   :mark-key "I"
   :modes 'org-mode)

  (conn-register-thing-commands
   'org-paragraph 'conn-sequential-thing-handler
   'org-forward-paragraph 'org-backward-paragraph)

  (conn-register-thing
   'org-link
   :dispatch-provider (lambda () (conn--dispatch-re-matches org-link-any-re t))
   :bounds-op (lambda () (org-in-regexp org-link-any-re))
   :mark-key "O"
   :modes 'org-mode)

  (conn-define-dispatch-action (conn-open-org-link "Open Link")
      (window pt _thing)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (org-open-at-point-global))))

  (setf (alist-get 'org-link conn-dispatch-default-actions-alist)
        'conn-open-org-link)

  (conn-register-thing-commands
   'org-link 'conn-individual-thing-handler
   'org-next-link 'org-previous-link)

  (conn-register-thing-commands
   'org-link nil
   'org-insert-link-global 'org-store-link 'org-insert-link)

  (defun conn-org-sentence-forward (arg)
    (interactive "p")
    (if (>= arg 0)
        (org-forward-sentence arg)
      (org-backward-sentence (abs arg))))

  (conn-register-thing
   'org-sentence
   :forward-op 'conn-org-sentence-forward
   :mark-key "{"
   :modes 'org-mode)

  (conn-register-thing-commands
   'org-sentence 'conn-sequential-thing-handler
   'conn-org-sentence-forward
   'org-forward-sentence 'org-backward-sentence)

  (conn-register-thing
   'org-element
   :mark-key "m"
   :beg-op 'org-backward-element
   :end-op 'org-forward-element
   :modes 'org-mode)

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
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'org-heading t)
   :forward-op 'org-next-visible-heading
   :modes 'org-mode
   :mark-key "H")

  (conn-register-thing-commands
   'org-heading 'conn-sequential-thing-handler
   'org-next-visible-heading
   'org-previous-visible-heading)

  (conn-register-thing-commands
   'org-element 'conn-individual-thing-handler
   'org-forward-element
   'org-backward-element
   'org-next-visible-heading
   'org-previous-visible-heading
   'org-forward-heading-same-level
   'org-backward-heading-same-level
   'org-up-element
   'org-up-heading)

  (define-keymap
    :keymap (conn-get-mode-map 'conn-state 'org-mode)
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
    (add-to-list 'polymode-move-these-vars-from-old-buffer v)))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'paredit-forward
                     'paredit-forward-up
                     'paredit-backward
                     'paredit-backward-up
                     'conn-dispatch-on-things))

(with-eval-after-load 'paredit
  (declare-function paredit-forward-down "paredit")
  (declare-function paredit-backward-down "paredit")
  (declare-function paredit-forward-up "paredit")
  (declare-function paredit-backward-up "paredit")

  (define-keymap
    :keymap (conn-get-mode-map 'conn-state 'paredit-mode)
    "]" 'paredit-forward-down
    "[" 'paredit-backward-down
    "(" 'paredit-backward-up
    ")" 'paredit-forward-up)

  (defun conn-paredit-list-handler (beg)
    (cond ((> (point) beg)
           (save-excursion
             (forward-thing 'sexp -1)
             (conn--push-ephemeral-mark (point))))
          ((< (point) beg)
           (save-excursion
             (forward-thing 'sexp 1)
             (conn--push-ephemeral-mark (point))))))

  (conn-register-thing-commands
   'list 'conn-paredit-list-handler
   'paredit-forward-up
   'paredit-backward-up)

  (defun conn-paredit-down-list-handler (beg)
    (cond ((> (point) beg)
           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (conn--push-ephemeral-mark (point))))
          ((< (point) beg)
           (save-excursion
             (paredit-backward-up)
             (paredit-forward-down)
             (conn--push-ephemeral-mark (point))))))

  (conn-register-thing-commands
   'list 'conn-paredit-down-list-handler
   'paredit-forward-down
   'paredit-backward-down)

  (defun conn-paredit-sexp-handler (beg)
    (pcase (save-excursion
             (goto-char beg)
             (ignore-errors (bounds-of-thing-at-point 'list)))
      ((and `(,b1 . ,e1) (guard (< b1 (point) e1)))
       (conn-sequential-thing-handler beg))
      ((and `(,b1 . ,_) (guard (/= beg b1)))
       (save-excursion
         (cond ((> (point) beg)
                (while (> (point) beg) (forward-thing 'sexp -1)))
               ((< (point) beg)
                (while (< (point) beg) (forward-thing 'sexp 1))))
         (conn--push-ephemeral-mark)))
      (_ (conn-sequential-thing-handler beg))))

  (conn-register-thing-commands
   'sexp 'conn-paredit-sexp-handler
   'paredit-forward
   'paredit-backward))

(with-eval-after-load 'smartparens
  (declare-function sp-forward-sexp "smartparens")
  (declare-function sp-backward-sexp "smartparens")
  (declare-function sp-end-of-sexp "smartparens")
  (declare-function sp-beginning-of-sexp "smartparens")

  (define-keymap
    :keymap (conn-get-mode-map 'conn-state 'smartparens-mode)
    "]" 'sp-down-sexp
    "[" 'sp-backward-down-sexp
    ")" 'sp-up-sexp
    "(" 'sp-backward-up-sexp)

  (defun conn-sp-list-handler (beg)
    (cond ((> (point) beg)
           (save-excursion
             (sp-backward-sexp)
             (conn--push-ephemeral-mark (point))))
          ((< (point) beg)
           (save-excursion
             (sp-forward-sexp)
             (conn--push-ephemeral-mark (point))))))

  (conn-register-thing-commands
   'list 'conn-sp-list-handler
   'sp-up-sexp 'sp-backward-up-sexp)

  (defun conn-sp-down-list-handler (beg)
    (cond ((> (point) beg)
           (save-excursion
             (sp-beginning-of-sexp)
             (conn--push-ephemeral-mark (point))))
          ((< (point) beg)
           (save-excursion
             (sp-end-of-sexp)
             (conn--push-ephemeral-mark (point))))))

  (conn-register-thing-commands
   'list 'conn-sp-down-list-handler
   'sp-down-sexp 'sp-backward-down-sexp)

  (defun conn-sp-sexp-handler (beg)
    (pcase (save-excursion
             (goto-char beg)
             (ignore-errors (bounds-of-thing-at-point 'list)))
      ((and `(,b1 . ,e1) (guard (< b1 (point) e1)))
       (conn-sequential-thing-handler beg))
      ((and `(,b1 . ,_) (guard (/= beg b1)))
       (save-excursion
         (cond ((> (point) beg)
                (while (> (point) beg) (sp-backward-sexp)))
               ((< (point) beg)
                (while (< (point) beg) (sp-forward-sexp))))
         (conn--push-ephemeral-mark)))
      (_ (conn-sequential-thing-handler beg))))

  (conn-register-thing-commands
   'sexp 'conn-sp-sexp-handler
   'sp-forward-sexp 'sp-backward-sexp))

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-emacs-state)
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
   :mark-key "H"
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'heading t)
   :bounds-op (lambda ()
                (save-mark-and-excursion
                  (unless (outline-on-heading-p)
                    (outline-up-heading 1))
                  (outline-mark-subtree)
                  (cons (region-beginning) (region-end))))
   :forward-op 'conn-forward-heading-op)

  (conn-register-thing-commands
   'heading 'conn-sequential-thing-handler
   'conn-forward-heading-op)

  (conn-register-thing-commands
   'heading 'conn-individual-thing-handler
   'outline-up-heading
   'outline-next-heading
   'outline-next-visible-heading
   'outline-previous-visible-heading
   'outline-previous-heading
   'outline-forward-same-level
   'outline-backward-same-level))

(with-eval-after-load 'treesit
  (conn-register-thing-commands
   'defun 'conn-sequential-thing-handler
   'treesit-end-of-defun
   'treesit-beginning-of-defun))

;; Local Variables:
;; outline-regexp: ";;;;* [^    \n]"
;; End:
;;; conn.el ends here
