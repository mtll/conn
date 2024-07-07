;;; conn.el --- Region oriented modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode and keyboard macro enhancement
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") (transient "0.6.0") (seq "2.24"))
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

(require 'replace)
(require 'easy-mmode)
(require 'compat)
(require 'transient)
(require 'thingatpt)
(require 'rect)
(require 'isearch)
(require 'repeat)
(require 'kmacro)
(require 'sort)
(require 'map)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))


;;;; Variables

;;;;; Declerations

(defvar conn-mode nil)
(defvar conn-local-mode)
(defvar conn-modes)
(defvar conn-state)
(defvar conn-emacs-state)
(defvar conn-org-edit-state)
(defvar conn-emacs-state)
(defvar kmacro-step-edit-replace)
(defvar conn-state-map)
(defvar conn-transition-hook)

(defvar conn-dispatch-providers-alist
  '((t . conn--dispatch-chars)))

(defvar conn-dispatch-default-actions-alist
  '((t . conn-dispatch-goto)))

(defvar conn-mark-handler-alist nil)

(defvar conn--mark-cursor-timer nil
  "`run-with-idle-timer' timer to update `mark' cursor.")

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
  "H" 'conn-contract
  "h" 'conn-expand)

;;;;; Custom Variables

(defgroup conn nil
  "A region oriented modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-marks nil
  "Conn-mode marks."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-states nil
  "Conn-mode states."
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

(defface conn-pulse-face
  '((default (:background "#c6ebd9"))
    (((background dark)) (:background "#449066"))
    (((background light)) (:background "#d6eeab")))
  "Face for kapply pulse."
  :group 'conn)

(defface conn-mark-face
  '((default (:inherit cursor :background "#b8a2f0"))
    (((background light)) (:inherit cursor :background "#b8a2f0"))
    (((background dark)) (:inherit cursor :background "#a742b0")))
  "Face for mark."
  :group 'conn-marks)

(defface conn-window-prompt-face
  '((default (:height 2.5 :foreground "#d00000"))
    (((background light)) (:height 2.5 :foreground "#d00000"))
    (((background dark)) (:height 2.5 :foreground "#7c0000")))
  "Face for conn window prompt overlay."
  :group 'conn-mode)

(defface conn-read-string-match-face
  '((t (:inherit isearch)))
  "Face for matches when reading strings."
  :group 'conn)

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :group 'conn)

(defcustom conn-ephemeral-mark-states
  nil
  "States in which ephemeral marks should be used."
  :type '(repeat symbol)
  :group 'conn-marks)

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

(defcustom conn-read-pair-split-char "\t"
  "String on which to split `conn-insert-pair' brackets."
  :group 'conn
  :type 'string)

(defcustom conn-expand-pulse-region t
  "Pulse region on expansion when mark is not active"
  :group 'conn
  :type 'boolean)

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
  :group 'conn)

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
  '(occur-mode
    grep-mode
    occur-edit-mode
    eshell-mode
    edmacro-mode
    (not minibuffer-mode
         dired-mode
         slime-xref-mode
         calc-mode
         calc-trail-mode
         calc-keypad-mode
         special-mode)
    t)
  "Modes in `conn-local-mode' should be enabled.
Must be of a form accepted by `define-globalized-minor-mode'
:predicate argument.")

(defvar conn-enable-in-buffer-hook
  (list (lambda () (easy-mmode--globalized-predicate-p conn-in-modes)))
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

(defvar conn--transition-maps nil)

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

(defvar conn-yank-keys (key-parse "C-y"))
(defvar conn-kill-region-keys (key-parse "C-w"))
(defvar conn-delete-region-keys (key-parse "C-S-w"))
(defvar conn-forward-sexp-keys (key-parse "C-M-f"))
(defvar conn-backward-sexp-keys (key-parse "C-M-b"))
(defvar conn-backward-paragraph-keys (key-parse "M-{"))
(defvar conn-forward-paragraph-keys (key-parse "M-}"))
(defvar conn-beginning-of-defun-keys (key-parse "C-M-a"))
(defvar conn-end-of-defun-keys (key-parse "C-M-e"))
(defvar conn-next-line-keys (key-parse "C-n"))
(defvar conn-previous-line-keys (key-parse "C-p"))
(defvar conn-forward-word-keys (key-parse "M-f"))
(defvar conn-backward-word-keys (key-parse "M-b"))
(defvar conn-backward-sentence-keys (key-parse "M-a"))
(defvar conn-forward-sentence-keys (key-parse "M-e"))
(defvar conn-backward-delete-char-keys (key-parse "DEL"))
(defvar conn-delete-char-keys (key-parse "C-d"))
(defvar conn-backward-up-list-keys (key-parse "C-M-<up>"))
(defvar conn-down-list-keys (key-parse "C-M-<down>"))
(defvar conn-forward-list-keys (key-parse "C-M-n"))
(defvar conn-backward-list-keys (key-parse "C-M-p"))

(defvar-keymap conn-mark-thing-map
  "L" 'forward-line
  ")" 'forward-list
  "(" 'backward-list)

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
  (defmacro conn--thread (needle form &rest forms)
    (declare (indent 2)
             (debug (symbolp form body)))
    (if forms
        `(let ((,needle ,form))
           (conn--thread ,needle ,@forms))
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

(defmacro conn--without-conn-maps (&rest body)
  (declare (indent 0))
  `(let ((emulation-mode-map-alists (seq-difference
                                     emulation-mode-map-alists
                                     '(conn--transition-maps
                                       conn--local-mode-maps
                                       conn--major-mode-maps
                                       conn--local-maps
                                       conn--state-maps)
                                     #'eq)))
     ,(macroexp-progn body)))

(defmacro conn--with-advice (symbol how function &rest body)
  (declare (indent 3))
  (let ((fn (gensym "advice")))
    `(let ((,fn ,function))
       (advice-add ,symbol ,how ,fn)
       (unwind-protect
           ,(macroexp-progn body)
         (advice-remove ,symbol ,fn)))))

(defun conn-remapping-command (from-keys)
  `(menu-item
    ""
    nil
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

(defun conn--read-buffers (&optional predicate)
  "Return a list of buffers specified interactively, one by one."
  (cl-loop for buf = (read-buffer "Buffer: " nil t
                                  (pcase-lambda (`(,name . ,buf))
                                    (and
                                     (if predicate (funcall predicate buf) t)
                                     (not (member name selected)))))
           until (equal buf "") collect buf into selected
           finally (cl-return selected)))

(defun conn--beginning-of-region-or-restriction ()
  (if (use-region-p) (region-beginning) (point-min)))

(defun conn--end-of-region-or-restriction ()
  (if (use-region-p) (region-end) (point-max)))

(defun conn--create-marker (pos &optional buffer)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    marker))

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
  (catch 'term
    (dolist (mode (conn--thread -mode->
                      'major-mode
                    (buffer-local-value -mode-> (or buffer (current-buffer)))
                    (conn--derived-mode-all-parents -mode->)))
      (when-let ((prop (get mode property)))
        (throw 'term prop)))))

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

(defvar-keymap conn-read-thing-region-command-map
  "C-h" 'help
  "C-w" 'backward-delete-arg
  "C-d" 'forward-delete-arg
  "C-<backspace>" 'reset-arg
  "M-<backspace>" 'reset-arg
  "M-DEL" 'reset-arg
  "t" conn-mark-thing-map
  "." 'reset-arg
  "r" 'conn-define-region-in-recursive-edit)

(defvar-keymap conn-read-expand-region-map
  :parent conn-expand-repeat-map
  "v" 'conn-toggle-mark-command
  "r" 'exit-recursive-edit
  "C-g" 'abort-recursive-edit
  "<t>" 'ignore)

(defun conn--read-thing-region (prompt)
  (conn--with-state conn-state
    (internal-push-keymap conn-read-thing-region-command-map
                          'overriding-terminal-local-map)
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<conn-read-thing-region-command-map>"
                           prompt " (arg: "
                           (propertize "%s" 'face 'transient-value)
                           ", \\[reset-arg] reset arg; "
                           "\\[help] commands; "
                           "\\[conn-define-region-in-recursive-edit] "
                           "recursive edit): %s")))
          thing-arg thing-sign invalid keys cmd)
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
           ('keyboard-quit
            (keyboard-quit))
           ('help
            (internal-pop-keymap conn-read-thing-region-command-map
                                 'overriding-terminal-local-map)
            (save-window-excursion
              (setq cmd (intern
                         (completing-read
                          "Command: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  ,(cons 'affixation-function
                                         (conn--dispatch-make-command-affixation
                                          conn-read-thing-region-command-map))
                                  (category . conn-dispatch-command))
                              (complete-with-action action obarray string pred)))
                          (lambda (sym)
                            (and (functionp sym)
                                 (not (eq sym 'help))
                                 (get sym :conn-command-thing)))
                          t))))
            (internal-push-keymap conn-read-thing-region-command-map
                                  'overriding-terminal-local-map)
            (go :test))
           ('digit-argument
            (let ((digit (- (logand (elt keys 0) ?\177) ?0)))
              (setq thing-arg (if thing-arg (+ (* 10 thing-arg) digit) digit))))
           ('reset-arg
            (setq thing-arg nil))
           ('backward-delete-arg
            (setq thing-arg (floor thing-arg 10)))
           ('forward-delete-arg
            (setq thing-arg (conn--thread -->
                                (log thing-arg 10)
                              (floor -->)
                              (expt 10 -->)
                              (mod thing-arg -->))))
           ('negative-argument
            (setq thing-sign (not thing-sign)))
           ((or 'conn-expand 'conn-contract)
            (save-mark-and-excursion
              (let ((current-prefix-arg
                     (cond (thing-arg (* thing-arg (if thing-sign -1 1)))
                           (thing-sign '-))))
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
                (cl-return (cons 'region (region-bounds))))))
           ('conn-define-region-in-recursive-edit
            (save-mark-and-excursion
              (message "Defining region in recursive edit")
              (internal-pop-keymap conn-read-thing-region-command-map
                                   'overriding-terminal-local-map)
              (recursive-edit)
              (cl-return (cons 'region (region-bounds)))))
           ((guard (not (get cmd :conn-command-thing)))
            (setq invalid t))
           ((app conn-get-mark-handler
                 (and conn-this-command-handler
                      (pred functionp)))
            (cl-return (cons (get cmd :conn-command-thing)
                             (conn-bounds-of-things cmd thing-arg))))
           ((let 'region (get cmd :conn-command-thing))
            (cl-return (cons 'region (region-bounds))))
           ((and (let thing (get cmd :conn-command-thing))
                 (let `(,beg . ,end) (bounds-of-thing-at-point thing)))
            (cl-return (cons thing (list (cons beg end))))))
         (go :read-command))
      (message nil)
      (internal-pop-keymap conn-read-thing-region-command-map
                           'overriding-terminal-local-map))))

(defvar-keymap conn-read-thing-command-map
  "C-h" 'help)

(defun conn--read-thing (prompt)
  (conn--with-state conn-state
    (internal-push-keymap conn-read-thing-command-map
                          'overriding-terminal-local-map)
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<conn-read-thing-command-map>"
                           prompt " (\\[help] commands): %s")))
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
            (internal-pop-keymap conn-read-thing-command-map
                                 'overriding-terminal-local-map)
            (save-window-excursion
              (setq cmd (intern
                         (completing-read
                          "Command: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  ,(cons 'affixation-function
                                         (conn--dispatch-make-command-affixation
                                          conn-read-thing-command-map))
                                  (category . conn-dispatch-command))
                              (complete-with-action action obarray string pred)))
                          (lambda (sym)
                            (and (functionp sym)
                                 (not (eq sym 'help))
                                 (get sym :conn-command-thing)))
                          t))))
            (internal-push-keymap conn-read-thing-command-map
                                  'overriding-terminal-local-map)
            (go :test))
           ((guard (not (get cmd :conn-command-thing)))
            (setq invalid t))
           ((let thing (get cmd :conn-command-thing))
            (cl-return thing)))
         (go :read-command))
      (message nil)
      (internal-pop-keymap conn-read-thing-command-map
                           'overriding-terminal-local-map))))

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
               when (funcall isearch-filter-predicate
                             (match-beginning 0) (match-end 0))
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
  (cl-loop for char across string
           always (eq char (downcase char))))

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
  (cl-loop for win in (pcase-exhaustive in-windows
                        ('nil (list (selected-window)))
                        ('t (window-list-1 nil nil 'visible))
                        ((pred listp)
                         (cl-loop for win in (window-list-1 nil nil 'visible)
                                  when (memq (buffer-local-value
                                              'major-mode (window-buffer win))
                                             in-windows)
                                  collect win))
                        ((pred functionp)
                         (cl-loop for win in (window-list-1 nil nil 'visible)
                                  when (funcall in-windows win) collect win)))
           unless (memq (buffer-local-value 'major-mode (window-buffer win))
                        conn-dispatch-thing-ignored-modes)
           collect win))

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

(defun conn--read-from-with-preview (prompt &optional regexp-flag default bounds)
  (minibuffer-with-setup-hook
      (minibuffer-lazy-highlight-setup
       :case-fold case-fold-search
       :filter (when bounds
                 (lambda (beg end)
                   (and (<= (car bounds) beg (cdr bounds))
                        (<= (car bounds) end (cdr bounds)))))
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
        (read-regexp (format-prompt prompt (and (length> (ensure-list default) 0)
                                                (query-replace-descr default)))
                     (mapcar #'regexp-quote (ensure-list default))
                     'conn--read-regexp-history)
      (read-string (format-prompt prompt (and (length> (ensure-list default) 0)
                                              (query-replace-descr default)))
                   nil 'conn--read-string-history default))))

(defun conn-overlay-p (overlay)
  (overlay-get overlay 'conn-overlay))

(defun conn--clear-overlays (&optional buffer)
  "Delete all conn overlays in BUFFER."
  (without-restriction
    (mapc #'delete-overlay (conn--all-overlays #'conn-overlay-p nil nil buffer))))

(defun conn--all-overlays (predicate &optional start end buffer)
  "Get all overlays between START and END satisfying PREDICATE."
  (with-current-buffer (or buffer (current-buffer))
    (cl-loop for ov in (overlays-in (or start (conn--beginning-of-region-or-restriction))
                                    (or end   (conn--end-of-region-or-restriction)))
             when (funcall predicate ov) collect ov)))


;;;; Advice

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
        (advice-add 'repeat :around #'conn--repeat-advice)
        (advice-add 'push-mark :before #'conn--push-mark-ad)
        (advice-add 'pop-mark :before #'conn--pop-mark-ad)
        (advice-add 'set-mark :before #'conn--set-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'set-mark #'conn--set-mark-ad)
    (advice-remove 'repeat #'conn--repeat-advice)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)))


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
            (pcase (bounds-of-thing-at-point ',thing)
              (`(,beg . ,end)
               (goto-char beg)
               (conn--push-ephemeral-mark end))
              (_ (user-error "Point not in %s" ',thing)))))
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
  (save-restriction
    (widen)
    (dolist (ov (conn--all-overlays 'conn--mark-cursor-p
                                    (point-min) (point-max)))
      (delete-overlay ov)))
  (setq conn--mark-cursor nil))

(defun conn-bounds-of-things (cmd arg)
  (let (regions)
    (save-mark-and-excursion
      (ignore-errors
        (cl-loop with current-prefix-arg = nil
                 with conn-this-command-handler = (or (conn-get-mark-handler cmd)
                                                      'conn-individual-thing-handler)
                 with conn-this-command-thing = (get cmd :conn-command-thing)
                 for conn-this-command-start = (point-marker)
                 repeat (prefix-numeric-value arg)
                 do (progn
                      (call-interactively cmd)
                      (funcall conn-this-command-handler conn-this-command-start))
                 while (and (/= (point) conn-this-command-start)
                            (= (point) (save-excursion
                                         (goto-char (mark t))
                                         (call-interactively cmd)
                                         (point))))
                 do
                 (push (cons (region-beginning) (region-end)) regions))))
    (nreverse regions)))

(defun conn-bounds-of-thing-region (thing arg)
  (condition-case _err
      (save-mark-and-excursion
        (pcase-let* ((cmd (or (get thing 'forward-op)
                              (get thing 'beginning-op)))
                     (current-prefix-arg arg)
                     (conn-this-command-start (point-marker))
                     (conn-this-command-handler (conn-get-mark-handler cmd))
                     (conn-this-command-thing thing)
                     (`(,beg . ,end) (bounds-of-thing-at-point thing)))
          (goto-char beg)
          (forward-thing thing arg)
          (funcall conn-this-command-handler conn-this-command-start)
          (cons (region-beginning) (if (< arg 0) end (region-end)))))
    (error (bounds-of-thing-at-point thing))))

(defun conn-bounds-of-inner-thing (thing bounds)
  (when-let ((inner-op (get thing :conn-inner-bounds-op)))
    (funcall inner-op (car bounds) (cdr bounds))))

(cl-defgeneric conn-thing-empty-p (thing bounds)
  (or (= (car bounds) (cdr bounds))
      (pcase (conn-bounds-of-inner-thing thing bounds)
        ('nil nil)
        ((and `(,beg . ,end) (guard (= beg end))) t))))

;;;;; Thing Definitions

(conn-register-thing 'url :mark-key "!")
(conn-register-thing 'email :mark-key "@")
(conn-register-thing 'uuid :mark-key "$")
(conn-register-thing 'string :mark-key "\"")
(conn-register-thing 'filename :mark-key "/")

(defun conn-forward-defun (N)
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :dispatch-provider (apply-partially 'conn--dispatch-all-things 'defun t))

(conn-register-thing
 'region
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
 'conn-toggle-mark-command
 'conn-set-mark-command)

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
 'end-of-defun 'beginning-of-defun
 'conn-forward-defun)

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


;;;; Kmacro Apply

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

(defun conn--kapply-ensure-region-buffer (region)
  (when-let ((buffer (and region (marker-buffer (car region)))))
    (when (not (eq buffer (current-buffer)))
      (pop-to-buffer-same-window buffer)
      (deactivate-mark t)
      (unless (eq buffer (window-buffer (selected-window)))
        (error "Could not pop to buffer %s" buffer))))
  region)

(defun conn--kapply-infinite-iterator ()
  (lambda (state)
    (unless (eq state :finalize)
      (cons (point) (mark t)))))

(defun conn--kapply-thing-iterator (thing beg end &optional reverse skip-empty)
  (let (regions)
    (save-mark-and-excursion
      (with-restriction beg end
        (goto-char (point-min))
        (cl-loop with current-prefix-arg = 1
                 with conn-this-command-handler = (or (conn-get-mark-handler
                                                       (or (get thing 'forward-op)
                                                           (get thing 'end-op)))
                                                      'conn-individual-thing-handler)
                 with conn-this-command-thing = thing
                 for conn-this-command-start = (point-marker)
                 while (< (point) (point-max))
                 do
                 (forward-thing thing 1)
                 (funcall conn-this-command-handler conn-this-command-start)
                 while (and (/= (point) conn-this-command-start)
                            (= (point) (save-excursion
                                         (goto-char (region-beginning))
                                         (forward-thing thing 1)
                                         (point))))
                 for reg = (cons (region-beginning) (region-end))
                 unless (and skip-empty (conn-thing-empty-p thing reg))
                 do (push reg regions))))
    (conn--kapply-region-iterator (if reverse regions (nreverse regions)))))

(defun conn--kapply-region-iterator (regions &optional reverse)
  (when reverse (setq regions (reverse regions)))
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
       (conn--kapply-ensure-region-buffer (pop regions))))))

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
       (dolist (pt points)
         (set-marker pt nil)))
      (_
       (when-let ((pt (pop points)))
         (conn--kapply-ensure-region-buffer (cons pt pt)))))))

(defun conn--kapply-merge-undo (iterator &optional undo-on-error)
  (let (undo-handles)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(_ . ,handle) undo-handles)
           (if (and conn-kmacro-apply-error undo-on-error)
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
    (let ((ret (funcall iterator state)))
      (when (and (not (eq state :finalize))
                 (consp ret))
        (delete-region (car ret) (cdr ret)))
      ret)))

(defun conn--kapply-with-state (iterator transition)
  (let ((buffer-states nil))
    (lambda (state)
      (let ((ret (funcall iterator state)))
        (cond
         ((eq state :finalize)
          (pcase-dolist (`(,buf ,state ,prev-state) buffer-states)
            (when state
              (with-current-buffer buf
                (funcall state)
                (setq conn-previous-state prev-state)))))
         ((consp ret)
          (when conn-local-mode
            (unless (alist-get (current-buffer) buffer-states)
              (setf (alist-get (current-buffer) buffer-states)
                    (list conn-current-state conn-previous-state)))
            (funcall transition))))
        ret))))

(defun conn--kapply-at-end (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      (`(,beg . ,end) (cons end beg))
      (ret ret))))

(defun conn--kapply-pulse-region (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,end)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-region beg end 'conn-pulse-face)
       ret)
      (ret ret))))

(defun conn--kapply-save-windows (iterator)
  (let (wconf)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (set-window-configuration wconf))
        (_
         (unless wconf
           (setq wconf (current-window-configuration)))
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
       (cl-letf* ((undo-outer-limit nil)
                  (undo-limit most-positive-fixnum)
                  (undo-strong-limit most-positive-fixnum)
                  (conn-kmacro-applying-p t)
                  (conn-kmacro-apply-error nil)
                  (kmacro-loopfn (symbol-function 'kmacro-loop-setup-function))
                  (,iterator (lambda (&optional state)
                               (pcase (funcall ,iterator (or state :loop))
                                 (`(,beg . ,end)
                                  (goto-char beg)
                                  (conn--push-ephemeral-mark end)
                                  (when (markerp beg) (set-marker beg nil))
                                  (when (markerp end) (set-marker end nil))
                                  (and (run-hook-with-args-until-failure
                                        'conn-kmacro-apply-iterator-hook)
                                       (funcall kmacro-loopfn))))))
                  ((symbol-function 'kmacro-loop-setup-function) ,iterator))
         (unwind-protect
             (condition-case err
                 (progn
                   (run-hooks 'conn-kmacro-apply-start-hook)
                   (deactivate-mark)
                   ,@body)
               (t
                (setq conn-kmacro-apply-error err)
                (signal (car err) (cdr err))))
           (funcall ,iterator :finalize)
           (run-hook-wrapped 'conn-kmacro-apply-end-hook
                             (lambda (hook)
                               (ignore-errors (funcall hook)))))))))

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

(defun conn-get-transition-map (state)
  "Get transition map for STATE."
  (or (alist-get state conn--transition-maps)
      (setf (alist-get state conn--transition-maps)
            (make-sparse-keymap))))

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

:TRANSITIONS is a list of transition key bindings to be bound in NAME's
transition map.  It is of the form ((KEY . TRANSITION-FUNCTION) ...).

:EPHEMERAL-MARKS if non-nil thing movement commands will push ephemeral
marks while in state NAME.

BODY contains code to be executed each time the state is enabled or
disabled.

\(fn NAME DOC &key CURSOR LIGHTER-FACE SUPPRESS-INPUT-METHOD KEYMAP TRANSITIONS EPHEMERAL-MARKS &rest BODY)"
  (declare (indent defun))
  (pcase-let* ((map-name (conn--symbolicate name "-map"))
               (transition-map-name (conn--symbolicate name "-transition-map"))
               (cursor-name (conn--symbolicate name "-cursor-type"))
               (lighter-face-name (conn--symbolicate name "-lighter-face"))
               (enter (gensym "enter"))
               ((map :cursor
                     :lighter-face
                     :suppress-input-method
                     (:keymap keymap '(make-sparse-keymap))
                     (:transitions transitions '(make-sparse-keymap))
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

       (defvar ,transition-map-name
         (setf (alist-get ',name conn--transition-maps) ,transitions)
         ,(conn--string-fill (conn--stringify
                              "Keymap for commands that transition from `"
                              name "' to other states.")
                             70))

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
bind anything except transition commands.

See `conn-emacs-state-transition-map' for keybindings to enter other states
from Emacs state.  See `conn-emacs-state-map' for commands bound by Emacs state."
  :lighter-face ((default              (:inherit mode-line :background "#cae1ff"))
                 (((background light)) (:inherit mode-line :background "#cae1ff"))
                 (((background dark))  (:inherit mode-line :background "#49739f")))
  :ephemeral-marks nil
  :keymap (make-sparse-keymap))

(conn-define-state conn-state
  "Activate `conn-state' in the current buffer.
A `conn-mode' state for editing text.

See `conn-state-transition-map' for keybindings to enter other states
from conn state.  See `conn-state-map' for commands bound by conn state."
  :lighter-face ((default              (:inherit mode-line :background "#f3bdbd"))
                 (((background light)) (:inherit mode-line :background "#f3bdbd"))
                 (((background dark))  (:inherit mode-line :background "#8c3c3c")))
  :suppress-input-method t
  :ephemeral-marks t
  :keymap (define-keymap :suppress t)
  :transitions (define-keymap
                 "e" 'conn-emacs-state
                 "t" 'conn-change))

(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . prog-mode) . conn-state))
(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . text-mode) . conn-state))
(add-to-list 'conn-buffer-default-state-alist
             '((derived-mode . conf-mode) . conn-state))

(conn-define-state conn-org-edit-state
  "Activate `conn-org-edit-state' in the current buffer.
A `conn-mode' state for structural editing of `org-mode' buffers.

See `conn-org-edit-state-transition-map' for keybindings to enter
other states from org-tree-edit state.  See
`conn-org-edit-state-map' for commands bound by org-tree-edit
state."
  :lighter-face ((default              (:inherit mode-line :background "#f5c5ff"))
                 (((background light)) (:inherit mode-line :background "#f5c5ff"))
                 (((background dark))  (:inherit mode-line :background "#85508c")))
  :suppress-input-method t
  :keymap (define-keymap :suppress t)
  :ephemeral-marks t
  :transitions (define-keymap "e" 'conn-emacs-state))


;;;; Thing Dispatch

(cl-defmacro conn-define-dispatch-action ((name description) arglist &rest body)
  "\(fn (NAME description) arglist &body body)"
  (declare (indent 2))
  `(progn
     (defun ,name ,arglist ,@body)
     (put ',name :conn-action-description ,description)
     (put ',name :conn-action t)))

(defvar conn-dispatch-all-things-collector-alist
  '((t . conn--dispatch-all-things-1)))

(defvar-keymap conn-dispatch-command-map
  "C-h" 'help
  "." 'reset-arg
  "C-d" 'forward-delete-arg
  "C-w" 'backward-delete-arg)

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
          "z" 'conn-dispatch-jump))
  "List of keymap containing dispatch actions.
All dispatch actions must be in a keymap in this list.")

(defvar conn-dispatch-thing-override-maps
  (list (define-keymap
          "l" 'forward-line
          "u" 'forward-symbol
          "U" `(symbol
                ,(apply-partially 'conn--dispatch-all-things 'symbol)
                . conn-dispatch-goto)
          "O" `(word
                ,(apply-partially 'conn--dispatch-all-things 'word)
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
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Killed: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-kill-append "Kill Append")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str nil)
           (delete-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Appended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-kill-prepend "Kill Prepend")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str t)
           (delete-region beg end)
           (conn--dispatch-fixup-whitespace)
           (message "Prepended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy "Copy")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-new str)
           (message "Copied: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy-append "Copy Append")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str nil)
           (message "Copy Appended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-copy-prepend "Copy Prepend")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (let ((str (filter-buffer-substring beg end)))
           (kill-append str t)
           (message "Copy Prepended: %s" str)))
        (_ (user-error "No thing at point"))))))

(conn-define-dispatch-action (conn-dispatch-yank-replace "Yank")
    (window pt thing)
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
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
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
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
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
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
        (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
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
      (pcase (conn-bounds-of-thing-region thing (prefix-numeric-value current-prefix-arg))
        (`(,beg . ,end)
         (unless (region-active-p)
           (if (= (point) end)
               (conn--push-ephemeral-mark beg)
             (conn--push-ephemeral-mark end)))
         (unless (or (= pt beg) (= pt end))
           (goto-char beg)))))))

(conn-define-dispatch-action (conn-dispatch-jump "Jump")
    (window pt _thing)
  (with-current-buffer (window-buffer window)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (select-window window)
      (goto-char pt))))

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
    (let (str1 str2)
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
      (insert str2))))

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
                (conn--thread -->
                    (overlay-get ov prop)
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
  (let (overlays)
    (condition-case _err
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
          overlays)
      (t (mapc #'delete-overlay overlays)))))

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
  (cl-loop for win in (conn--preview-get-windows in-windows)
           nconc (with-selected-window win
                   (if-let ((fn (alist-get thing conn-dispatch-all-things-collector-alist)))
                       (funcall fn)
                     (funcall (alist-get t conn-dispatch-all-things-collector-alist) thing)))))

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
        ovs)
    (conn--with-input-method
      (while (length< prefix prefix-length)
        (setq prefix (thread-last
                       (read-char (concat "char: " prefix) t)
                       (char-to-string)
                       (concat prefix)))))
    (condition-case _err
        (dolist (win (conn--preview-get-windows in-windows) ovs)
          (setq ovs (nconc (with-selected-window win
                             (conn--dispatch-things-with-prefix-1 things prefix))
                           ovs)))
      (t (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-columns ()
  (let ((col (current-column))
        ovs)
    (condition-case _err
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
          ovs)
      (t (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines ()
  (let (ovs)
    (condition-case _err
        (dolist (win (window-list-1 nil nil 'visible) ovs)
          (with-selected-window win
            (unless (memq major-mode conn-dispatch-thing-ignored-modes)
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
      (t (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-lines-end ()
  (let (ovs)
    (condition-case _err
        (dolist (win (window-list-1 nil nil 'visible) ovs)
          (with-selected-window win
            (unless (memq major-mode conn-dispatch-thing-ignored-modes)
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
      (t (mapc #'delete-overlay ovs)))))

(defun conn--dispatch-inner-lines (&optional end)
  (let (ovs)
    (dolist (win (window-list-1 nil nil 'visible) ovs)
      (with-selected-window win
        (unless (memq major-mode conn-dispatch-thing-ignored-modes)
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
   (let ((keymap (make-composed-keymap
                  (append (list conn-dispatch-command-map)
                          conn-dispatch-thing-override-maps
                          conn-dispatch-action-maps)))
         (prompt (substitute-command-keys
                  (concat "\\<conn-dispatch-command-map>Thing (arg: "
                          (propertize "%s" 'face 'transient-value)
                          ", \\[reset-arg] reset arg; "
                          "\\[help] commands): %s")))
         keys cmd invalid action thing-arg thing-sign)
     (conn--with-state conn-state
       (internal-push-keymap keymap 'overriding-terminal-local-map)
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
               (setq thing-arg (conn--thread -->
                                   (log thing-arg 10)
                                 (floor -->)
                                 (expt 10 -->)
                                 (mod thing-arg -->))))
              ('reset-arg
               (setq thing-arg nil))
              ('negative-argument
               (setq thing-sign (not thing-sign)))
              ('help
               (internal-pop-keymap keymap 'overriding-terminal-local-map)
               (save-window-excursion
                 (setq keys nil
                       cmd (intern
                            (completing-read
                             "Command: "
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   `(metadata
                                     ,(cons 'affixation-function
                                            (conn--dispatch-make-command-affixation keymap))
                                     (category . conn-dispatch-command))
                                 (complete-with-action action obarray string pred)))
                             (lambda (sym)
                               (and (functionp sym)
                                    (not (eq sym 'help))
                                    (or (get sym :conn-command-thing)
                                        (where-is-internal sym (list keymap) t))))
                             t))))
               (internal-push-keymap keymap 'overriding-terminal-local-map)
               (go :loop))
              ((let (and thing (pred identity)) (get cmd :conn-command-thing))
               (cl-return
                (list thing (conn--dispatch-finder cmd)
                      (or action (conn--dispatch-default-action cmd))
                      (* (if thing-sign -1 1) (or thing-arg 1))
                      current-prefix-arg)))
              ((guard (where-is-internal cmd conn-dispatch-action-maps t))
               (setq action (unless (eq cmd action) cmd)))
              (_
               (setq invalid t)))
            (go :read-command))
         (message nil)
         (internal-pop-keymap keymap 'overriding-terminal-local-map)))))
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
        (dolist (ov ovs)
          (delete-overlay ov))))))

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
      (dolist (ov (cdar prefix-ovs)) (delete-overlay ov)))))


;;;; Expand Region

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
               do (cl-return (goto-char end))
               when (and (= (point) end)
                         (/= (mark t) beg))
               do (cl-return (goto-char beg)))
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


;;;; Surround Thing

(defun conn--read-pair ()
  (pcase (conn--escapable-split-on-char
          (minibuffer-with-setup-hook
              (lambda ()
                (when (boundp 'electric-pair-mode)
                  (electric-pair-mode -1)))
            (read-string "Pair: " nil 'conn-pair-history))
          conn-read-pair-split-char)
    (`(,front ,back . nil) (cons front back))
    (`(,str) (conn--thread -->
                 (lambda (char)
                   (pcase (alist-get char insert-pair-alist)
                     (`(,close . nil) (list char close))
                     (`(,open ,close) (list open close))
                     (_               (list char char))))
               (seq-map --> str)
               (apply #'seq-mapn 'string -->)
               (cons (car -->) (nreverse (cadr -->)))))
    (_ (user-error "Unknown pair format."))))

(defun conn-insert-pair ()
  (let ((beg (region-beginning))
        (end (region-end)))
    (pcase-let ((`(,open . ,close) (conn--read-pair)))
      (goto-char end)
      (insert close)
      (goto-char beg)
      (insert-before-markers open))))

(defun conn-change-pair-outward (arg)
  "`conn-delete-pair-outward' with ARG then `conn-insert-pair' with STRING."
  (atomic-change-group
    (conn-delete-pair-outward arg)
    (conn-insert-pair)))

(defun conn-change-pair-inward (arg)
  "`conn-delete-pair-inward' with ARG then `conn-insert-pair' with STRING."
  (atomic-change-group
    (conn-delete-pair-inward arg)
    (conn-insert-pair)))

(defun conn-delete-pair-inward (arg)
  "Delete ARG chars at `point' and `mark'."
  (let ((end (> (point) (mark-marker))))
    (when end (exchange-point-and-mark t))
    (delete-region (point) (+ (point) arg))
    (delete-region (- (mark-marker) arg) (mark-marker))))

(defun conn-delete-pair-outward (arg)
  "Delete ARG chars at `point' and `mark'."
  (let ((end (> (point) (mark-marker))))
    (when end (exchange-point-and-mark t))
    (delete-region (- (point) arg) (point))
    (delete-region (mark-marker) (+ (mark-marker) arg))))

(defun conn--surround-region (action _beg _end arg)
  (pcase action
    ('surround (conn-insert-pair))
    ('delete
     (pcase (car (read-multiple-choice
                  "Direction:" `((?i "inward" "Delete surrounding chars inward")
                                 (?o "outward" "Delete surrounding chars outward"))))
       (?i (conn-delete-pair-inward arg))
       (?o (conn-delete-pair-outward arg))))
    ('change
     (pcase (car (read-multiple-choice
                  "Direction:" `((?i "inward" "Change surrounding chars inward")
                                 (?o "outward" "Change surrounding chars outward"))))
       (?i (conn-change-pair-inward arg))
       (?o (conn-change-pair-outward arg))))))
(put 'region :conn-thing-surrounder 'conn--surround-region)

(defun conn--surround-thing (action beg end arg)
  (goto-char beg)
  (conn--push-ephemeral-mark end)
  (pcase action
    ('surround (conn-insert-pair))
    ('delete (conn-delete-pair-inward arg))
    ('change (conn-change-pair-inward arg))))

(defun conn-surround-thing (thing beg end arg)
  (interactive
   (let ((regions (conn--read-thing-region "Define Region")))
     (list (car regions)
           (caadr regions)
           (cdar (last (cdr regions)))
           (list (prefix-numeric-value current-prefix-arg)))))
  (save-mark-and-excursion
    (funcall (or (get thing :conn-thing-surrounder)
                 'conn--surround-thing)
             (pcase (car (read-multiple-choice
                          "Action:" `((?s "surround")
                                      (?d "delete")
                                      (?c "change"))))
               (?s 'surround)
               (?d 'delete)
               (?c 'change))
             beg end arg)))


;;;; Narrow Ring

(defvar conn-narrow-ring-max 16
  "Maximum number of narrowings to keep in `conn-narrow-ring'.")

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(cl-defstruct (conn-narrow-register (:constructor %conn--make-narrow-register))
  (narrow-ring nil :read-only t))

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
  (unless (seq-find (pcase-lambda (`(,b . ,e))
                      (and (= beg b) (= end e)))
                    conn-narrow-ring)
    (setq conn-narrow-ring
          (take conn-narrow-ring-max
                (cons (cons (conn--create-marker beg)
                            (conn--create-marker end))
                      conn-narrow-ring)))))

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
  (let (merged)
    (pcase-dolist ((and region `(,beg1 . ,end1)) conn-narrow-ring)
      (pcase (seq-find (pcase-lambda (`(,beg2 . ,end2))
                         (not (or (< beg2 end2 beg1 end1)
                                  (< beg1 end1 beg2 end2))))
                       merged)
        ((and cons `(,beg2 . ,end2))
         (setcar cons (if (< beg1 beg2)
                          (prog1 beg1 (set-marker beg2 nil))
                        (prog1 beg2 (set-marker beg1 nil))))
         (setcdr cons (if (> end1 end2)
                          (prog1 end1 (set-marker end2 nil))
                        (prog1 end2 (set-marker end1 nil)))))
        ('nil
         (push region merged))))
    (setq conn-narrow-ring (nreverse merged))
    (when (and interactive (not executing-kbd-macro))
      (message "Narrow ring merged into %s region"
               (length conn-narrow-ring)))))

(defun conn-region-to-narrow-ring (beg end &optional pulse)
  "Add the region from BEG to END to the narrow ring.
Interactively defaults to the current region."
  (interactive
   (progn
     (deactivate-mark)
     (let ((regions (cdr (conn--read-thing-region "Define Region"))))
       (list (caar regions)
             (cdar (last regions))
             t))))
  (conn--narrow-ring-record beg end)
  (when (and pulse (not executing-kbd-macro))
    (pulse-momentary-highlight-region beg end 'region)))

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
                       (conn--thread -->
                           index
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

(defun conn-isearch-backward-thing (thing)
  "Isearch forward for THING.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing "Thing")))
  (pcase (bounds-of-thing-at-point thing)
    (`(,beg . ,end) (conn-isearch-region-backward beg end))
    (_              (user-error "No %s found" thing))))

(defun conn-isearch-forward-thing (thing)
  "Isearch backward for THING.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing "Thing")))
  (pcase (bounds-of-thing-at-point thing)
    (`(,beg . ,end) (conn-isearch-region-forward beg end))
    (_              (user-error "No %s found" thing))))

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

(defvar-keymap conn--read-mover-map
  "C-w" 'backward-delete-arg
  "C-d" 'forward-delete-arg
  "C-<backspace>" 'reset-arg
  "M-<backspace>" 'reset-arg
  "M-DEL" 'reset-arg
  "." 'reset-arg
  "r" 'conn-define-region-in-recursive-edit)

(defun conn-transpose-regions (mover arg)
  (interactive
   (conn--with-state conn-state
    (internal-push-keymap conn--read-mover-map
                          'overriding-terminal-local-map)
    (unwind-protect
        (cl-prog
         ((prompt (substitute-command-keys
                   (concat "\\<conn--read-mover-map> (arg: "
                           (propertize "%s" 'face 'transient-value)
                           ", \\[reset-arg] reset arg; "
                           "\\[conn-define-region-in-recursive-edit] "
                           "recursive edit): %s")))
          thing-arg thing-sign invalid keys cmd)
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
           ('keyboard-quit
            (keyboard-quit))
           ('help
            (internal-pop-keymap conn-read-thing-region-command-map
                                 'overriding-terminal-local-map)
            (save-window-excursion
              (setq cmd (intern
                         (completing-read
                          "Command: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  ,(cons 'affixation-function
                                         (conn--dispatch-make-command-affixation
                                          conn-read-thing-region-command-map))
                                  (category . conn-dispatch-command))
                              (complete-with-action action obarray string pred)))
                          (lambda (sym)
                            (and (functionp sym)
                                 (not (eq sym 'help))
                                 (get sym :conn-command-thing)))
                          t))))
            (internal-push-keymap conn-read-thing-region-command-map
                                  'overriding-terminal-local-map)
            (go :test))
           ('digit-argument
            (let ((digit (- (logand (elt keys 0) ?\177) ?0)))
              (setq thing-arg (if thing-arg (+ (* 10 thing-arg) digit) digit))))
           ('reset-arg
            (setq thing-arg nil))
           ('backward-delete-arg
            (setq thing-arg (floor thing-arg 10)))
           ('forward-delete-arg
            (setq thing-arg (conn--thread -->
                                (log thing-arg 10)
                              (floor -->)
                              (expt 10 -->)
                              (mod thing-arg -->))))
           ('negative-argument
            (setq thing-sign (not thing-sign)))
           ('conn-define-region-in-recursive-edit
            (cl-return (list 'recursive-edit nil)))
           ((guard (not (get cmd :conn-command-thing)))
            (setq invalid t))
           ((guard (get cmd :conn-command-thing))
            (cl-return (list cmd (prefix-numeric-value thing-arg)))))
         (go :read-command))
      (message nil)
      (internal-pop-keymap conn--read-mover-map
                           'overriding-terminal-local-map))))
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
     (transpose-subr mover arg))))

(defun conn-replace-in-thing (beg end from-string to-string &optional delimited backward)
  (interactive
   (cl-letf* ((regions (cdr (conn--read-thing-region "Define Region")))
              (beg (caar regions))
              (end (cdar regions))
              ((symbol-function 'replace--region-filter)
               (lambda (_bounds)
                 (lambda (b e)
                   (and (<= beg b end) (<= beg e end)))))
              ((symbol-function 'use-region-p)
               (lambda () t))
              (common
               (conn--with-advice
                   'minibuffer-lazy-highlight-setup :around
                   (lambda (&rest args)
                     (cl-letf (((symbol-function 'use-region-p)
                                (lambda () t)))
                       (apply args)))
                 (query-replace-read-args
                  (concat "Query replace"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  nil))))
     (append (list beg end) common)))
  (save-window-excursion
    (save-excursion
      (perform-replace from-string to-string t nil delimited nil nil beg end backward))))

(defun conn-regexp-replace-in-thing (beg end from-string to-string &optional delimited backward)
  (interactive
   (cl-letf* ((regions (cdr (conn--read-thing-region "Define Region")))
              (beg (caar regions))
              (end (cdar regions))
              ((symbol-function 'replace--region-filter)
               (lambda (_bounds)
                 (lambda (b e)
                   (and (<= beg b end) (<= beg e end)))))
              ((symbol-function 'use-region-p)
               (lambda () t))
              (common
               (conn--with-advice
                   'minibuffer-lazy-highlight-setup :around
                   (lambda (&rest args)
                     (cl-letf (((symbol-function 'use-region-p)
                                (lambda () t)))
                       (apply args)))
                 (query-replace-read-args
                  (concat "Query replace regexp"
                          (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                            ""))
                  t))))
     (append (list beg end) common)))
  (save-window-excursion
    (save-excursion
      (perform-replace from-string to-string t t delimited nil nil beg end backward))))

(defun conn-replace-region-in-thing (beg end from-string to-string &optional delimited backward)
  (interactive
   (cl-letf* ((regions (cdr (conn--read-thing-region "Define Region")))
              (beg (caar regions))
              (end (cdar regions))
              ((symbol-function 'replace--region-filter)
               (lambda (_bounds)
                 (lambda (b e) (and (<= beg b end) (<= beg e end)))))
              (common
               (minibuffer-with-setup-hook
                   'conn-yank-region-to-minibuffer
                 (conn--with-advice
                     'minibuffer-lazy-highlight-setup :around
                     (lambda (&rest args)
                       (cl-letf (((symbol-function 'use-region-p)
                                  (lambda () t)))
                         (apply args)))
                   (query-replace-read-args
                    (concat "Query replace"
                            (if current-prefix-arg
                                (if (eq current-prefix-arg '-) " backward" " word")
                              ""))
                    nil)))))
     (append (list beg end) common)))
  (conn-replace-in-thing beg end from-string to-string delimited backward))

(defun conn-regexp-replace-region-in-thing (beg end from-string to-string &optional delimited backward)
  (interactive
   (cl-letf* ((regions (cdr (conn--read-thing-region "Define Region")))
              (beg (caar regions))
              (end (cdar regions))
              ((symbol-function 'replace--region-filter)
               (lambda (_bounds)
                 (lambda (b e)
                   (and (<= beg b end) (<= beg e end)))))
              (common
               (minibuffer-with-setup-hook
                   'conn-yank-region-to-minibuffer
                 (conn--with-advice
                     'minibuffer-lazy-highlight-setup :around
                     (lambda (&rest args)
                       (cl-letf (((symbol-function 'use-region-p)
                                  (lambda () t)))
                         (apply args)))
                   (query-replace-read-args
                    (concat "Query replace regexp"
                            (if current-prefix-arg
                                (if (eq current-prefix-arg '-) " backward" " word")
                              ""))
                    t)))))
     (append (list beg end) common)))
  (conn-replace-in-thing beg end from-string to-string delimited backward))

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

(defun conn-yank-lines-as-rectangle ()
  "Yank the lines of the previous kill as if they were a rectangle."
  (interactive)
  (rectangle--insert-for-yank
   (with-temp-buffer
     (yank)
     (string-lines (buffer-string)))))

(defun conn-yank-replace-rectangle ()
  (interactive)
  (save-mark-and-excursion
    (unless (>= (mark t) (point))
      (conn-exchange-mark-command))
    (delete-rectangle (region-beginning) (region-end))
    (yank-rectangle)))

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
         (recenter -1))))))

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

(defun conn-yank-region-to-minibuffer (&optional quote-function)
  "Yank region from `minibuffer-selected-window' into minibuffer."
  (interactive (list (if current-prefix-arg
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

(defun conn-query-replace-region ()
  "Run `query-replace' with the region as initial contents."
  (interactive)
  (save-mark-and-excursion
    (unless (eql (point) (region-beginning))
      (conn-exchange-mark-command))
    (minibuffer-with-setup-hook 'conn-yank-region-to-minibuffer
      (call-interactively #'query-replace))))

(defun conn-query-replace-regexp-region ()
  "Run `query-replace-regexp' with the region as initial contents.
Also ensure point is at start of region beforehand."
  (interactive)
  (save-mark-and-excursion
    (unless (eql (point) (region-beginning))
      (conn-exchange-mark-command))
    (minibuffer-with-setup-hook
        (apply-partially 'conn-yank-region-to-minibuffer 'regexp-quote)
      (call-interactively #'query-replace-regexp))))

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

(defun conn-pop-state ()
  "Transition to the previous state."
  (interactive)
  (when conn-previous-state
    (funcall conn-previous-state)))

(defun conn--apply-region-transform (transform-func)
  "Apply TRANSFORM-FUNC to region contents.
Handles rectangular regions."
  (save-mark-and-excursion
    (let ((case-fold-search nil))
      (if (null rectangle-mark-mode)
          (with-restriction
              (region-beginning) (region-end)
            (goto-char (point-min))
            (funcall transform-func))
        (apply-on-rectangle
         (lambda (start-col end-col)
           (with-restriction
               (+ (point) start-col) (+ (point) end-col)
             (goto-char (point-min))
             (funcall transform-func)))
         (region-beginning) (region-end))))))

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

(defun conn-kill-append-region (beg end &optional register)
  "Kill region from BEG to END and append it to most recent kill.
Optionally if REGISTER is specified append kill to REGISTER instead.
When called interactively with a non-nil prefix argument read register
interactively."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Append kill to register: "))))
  (if register
      (append-to-register register beg end t)
    (kill-append (pcase (alist-get register-separator register-alist)
                   ((and (pred stringp) sep)
                    (concat sep (filter-buffer-substring beg end t)))
                   (_ (filter-buffer-substring beg end t)))
                 nil)))

(defun conn-kill-prepend-region (beg end &optional register)
  "Kill region from BEG to END and prepend it to most recent kill.
Optionally if REGISTER is specified prepend kill to REGISTER instead.
When called interactively with a non-nil prefix argument read register
interactively."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Prepend kill to register: "))))
  (if register
      (prepend-to-register register beg end t)
    (kill-append (pcase (alist-get register-separator register-alist)
                   ((and (pred stringp) sep)
                    (concat (filter-buffer-substring beg end t) sep))
                   (_ (filter-buffer-substring beg end t)))
                 t)))

(defun conn-copy-thing (beg end &optional register)
  "Copy THING at point."
  (interactive
   (let ((regions (cdr (conn--read-thing-region "Define Region"))))
     (list (caar regions)
           (cdar (last regions))
           (when current-prefix-arg
             (list (register-read-with-preview "Register: "))))))
  (conn-copy-region beg end register)
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end)))

(defun conn-narrow-to-region (beg end &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive
   (let ((regions (cdr (conn--read-thing-region "Define Region"))))
     (list (caar regions)
           (cdar (last regions))
           t)))
  (narrow-to-region beg end)
  (when record (conn--narrow-ring-record beg end))
  (when (called-interactively-p 'interactive)
    (message "Buffer narrowed")))

(defun conn-narrow-indirect-to-region (beg end &optional interactive)
  "Narrow to THING at point.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive
   (let ((regions (cdr (conn--read-thing-region "Define Region"))))
     (list (caar regions)
           (cdar (last regions))
           t)))
  (conn--narrow-indirect beg end interactive)
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

(defun conn-toggle-mark-command (&optional arg)
  "Toggle `mark-active'.
With a prefix ARG activate `rectangle-mark-mode'."
  (interactive "P")
  (cond (arg
         (if (region-active-p)
             (rectangle-mark-mode 'toggle)
           (activate-mark)
           (rectangle-mark-mode)))
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
  (if arg
      (funcall (conn--without-conn-maps
                 (key-binding conn-kill-region-keys t))
               start end)
    (funcall (conn--without-conn-maps
               (key-binding conn-delete-region-keys t))
             start end))
  (funcall (conn--without-conn-maps
             (key-binding conn-yank-keys t))))

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
    (if (rectangle-mark-mode)
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
      (if rectangle-mark-mode
          (copy-rectangle-to-register register start end)
        (copy-to-register register start end)
        (when (called-interactively-p 'interactive)
          (pulse-momentary-highlight-region start end)))
    (if rectangle-mark-mode
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
         (conn--thread -->
             (concat "Kill "
                     (if rectangle-mark-mode "Rectangle " " ")
                     "to register:")
           (register-read-with-preview -->)
           (copy-to-register --> nil nil t t)))
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
  (message "Display next command buffer in current window..."))

;;;;; Transition Functions

(defun conn-quoted-insert-overwrite ()
  "Overwrite char after point using `quoted-insert'."
  (interactive)
  (save-excursion
    (let ((overwrite-mode 'overwrite-mode-binary))
      (message "%s" (propertize "Overwrite Char:"
                                'face 'minibuffer-prompt))
      (call-interactively #'quoted-insert))))

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
  (cond ((and rectangle-mark-mode kill)
         (copy-rectangle-as-kill start end)
         (call-interactively #'string-rectangle))
        (rectangle-mark-mode
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

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--previous-scroll-conservatively)
(defvar conn--wincontrol-help)
(defvar conn--wincontrol-help-format)
(defvar conn--wincontrol-prev-eldoc-msg-fn)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)

(defcustom conn-wincontrol-initial-help 'window
  "Initial help message printed during `conn-wincontrol-mode'."
  :group 'conn
  :type '(choice (const :tag "Window" window)
                 (const :tag "Frame" frame)
                 (const :tag "Short" nil)))

(defun conn--wincontrol-window-format-1 ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Window: " 'face 'bold)
    "prefix arg: "
    (propertize "%s" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
    "\\[conn-wincontrol-quit]: quit; "
    "\\[tab-bar-history-back] \\[tab-bar-history-forward]: undo/redo"
    "\n"
    "\\[enlarge-window] "
    "\\[shrink-window] "
    "\\[enlarge-window-horizontally] "
    "\\[shrink-window-horizontally]: "
    "heighten shorten widen narrow; "
    "\\[previous-buffer] \\[next-buffer] "
    "\\[conn-wincontrol-previous-window] \\[conn-wincontrol-next-window]"
    ": prev/next buffer/win"
    "\n"
    "\\[delete-window] \\[delete-other-windows]: delete win/other; "
    "\\[conn-wincontrol-split-vertically] \\[conn-wincontrol-split-right]: "
    "split vert/right; "
    "\\[conn-wincontrol-transpose-window] \\[conn-wincontrol-yank-window]: transpose/yank; "
    "\\[conn-wincontrol-mru-window]: last win")))

(defun conn--wincontrol-window-format-2 ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Window: " 'face 'bold)
    "prefix arg: "
    (propertize "%s" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
    "\\[conn-wincontrol-quit]: quit; "
    "\\[conn-wincontrol-zoom-in] \\[conn-wincontrol-zoom-out]: zoom; "
    "\\[quit-window]: quit"
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
    "\\[conn-register-load] \\[window-configuration-to-register]: load/store")))

(defun conn--wincontrol-tab-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Tab: " 'face 'bold)
    "prefix arg: "
    (propertize "%s" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
    "\\[conn-wincontrol-quit]: quit; "
    "\\[conn-tab-to-register]: store"
    "\n"
    "\\[tab-bar-move-window-to-tab]: win to new tab; "
    "\\[tab-previous] \\[tab-next]: next/prev; "
    "\\[conn-wincontrol-tab-new] \\[tab-bar-duplicate-tab] \\[conn-wincontrol-tab-close]: "
    "new/clone/kill; "
    "\\[tab-bar-detach-tab]: tear off")))

(defun conn--wincontrol-frame-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Frame: " 'face 'bold)
    "prefix arg: "
    (propertize "%s" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
    "\\[conn-wincontrol-quit]: quit"
    "\n"
    "\\[other-frame]: other; "
    "\\[clone-frame]: clone; "
    "\\[undelete-frame]: undelete; "
    "\\[tear-off-window]: tear off; "
    "\\[toggle-frame-fullscreen]: fullscreen"
    "\n"
    "\\[conn-wincontrol-reverse] \\[conn-wincontrol-reflect]: reverse/reflect; "
    "\\[iconify-or-deiconify-frame] \\[make-frame-command]: iconify/create; "
    "\\[delete-frame] \\[delete-other-frames]: delete/other")))

(defun conn--wincontrol-simple-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "WinControl: " 'face 'bold)
    "prefix arg: "
    (propertize "%s" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help] \\[conn-wincontrol-help-backward]: help; "
    "\\[conn-wincontrol-quit]: quit; "
    "\\[conn-wincontrol-scroll-up] "
    "\\[conn-wincontrol-scroll-down]: "
    "scroll")))

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
  ";" 'conn-wincontrol-quit-to-initial-win
  "b" 'switch-to-buffer
  "C" 'tab-bar-duplicate-tab
  "c" 'conn-wincontrol-mru-window
  "d" 'delete-window
  "e" 'conn-tab-to-register
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
  "q" 'conn-wincontrol-quit
  "r" 'conn-wincontrol-split-right
  "R" 'conn-wincontrol-isearch-other-window-backward
  "s" 'shrink-window
  "S" 'conn-wincontrol-isearch-other-window
  "t" 'tab-switch
  "u" 'conn-wincontrol-previous-window
  "U" 'tab-bar-detach-tab
  "v" 'conn-wincontrol-split-vertically
  "w" 'enlarge-window-horizontally
  "x" 'conn-wincontrol-transpose-window
  "y" 'conn-wincontrol-yank-window
  "z" 'conn-wincontrol-zoom-out
  "Z" 'conn-wincontrol-zoom-in)

(define-minor-mode conn-wincontrol-mode
  "Global minor mode for window control."
  :global t
  :lighter " WinC"
  :init-value nil
  :interactive nil
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
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (dolist (state conn-states)
    (set-face-foreground (get state :conn-lighter-face)
                         'unspecified))
  (invert-face 'mode-line))

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
  (setq conn--wincontrol-arg (conn--thread -->
                                 (log conn--wincontrol-arg 10)
                               (floor -->)
                               (expt 10 -->)
                               (mod conn--wincontrol-arg -->))))

(defun conn-wincontrol-quit ()
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

(defun conn-wincontrol-quit-to-initial-win ()
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
                                  ('tab      'frame)
                                  ('frame    nil)
                                  (_         'window-1))))
  (setq conn--wincontrol-help-format
        (pcase conn--wincontrol-help
          ('window-1 (conn--wincontrol-window-format-1))
          ('window-2 (conn--wincontrol-window-format-2))
          ('tab      (conn--wincontrol-tab-format))
          ('frame    (conn--wincontrol-frame-format))
          (_         (conn--wincontrol-simple-format)))))

(defun conn-wincontrol-help-backward (&optional interactive)
  "Cycle to the next `conn-wincontrol-mode' help message."
  (interactive (list t))
  (when interactive
    (setq conn--wincontrol-help (pcase conn--wincontrol-help
                                  ('window-1 nil)
                                  ('window-2 'window-1)
                                  ('tab      'window-2)
                                  ('frame    'tab)
                                  (_         'frame))))
  (setq conn--wincontrol-help-format
        (pcase conn--wincontrol-help
          ('window-1 (conn--wincontrol-window-format-1))
          ('window-2 (conn--wincontrol-window-format-2))
          ('tab      (conn--wincontrol-tab-format))
          ('frame    (conn--wincontrol-frame-format))
          (_         (conn--wincontrol-simple-format)))))

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
                (conn--wincontrol-split-window-state state)))
    (let* ((height  (alist-get 'normal-height params))
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
            (alist-get 'total-width   params) twidth))
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
         (when arg
           (cl-loop for win = (selected-window) then (window-parent win)
                    repeat (abs arg) while win finally (cl-return win)))))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reverse-window (and arg (< arg 0)))
      (window-state-put window))))

(defun conn-wincontrol-reflect (arg)
  "Rotate all window arrangements within ARGth parent window of `selected-window'.
When ARG is nil the root window is used."
  (interactive "P")
  (let ((window
         (when arg
           (cl-loop for win = (selected-window) then (window-parent win)
                    repeat (abs arg) while win finally (cl-return win)))))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reflect-window)
      (window-state-put window))))


;;;; Transients

;;;;; Classes

(defclass conn-transient-switches (transient-switches)
  ((required :initarg :required :initform nil))
  "Class used for sets of mutually exclusive command-line switches.
Does not allow a null value.")

(cl-defmethod transient-infix-read ((obj conn-transient-switches))
  "Cycle through the mutually exclusive switches.
The last value is \"don't use any of these switches\"."
  (let ((choices (mapcar (apply-partially #'format (oref obj argument-format))
                         (oref obj choices))))
    (if-let ((value (oref obj value)))
        (or (cadr (member value choices))
            (when (oref obj required) (car choices)))
      (car choices))))

(cl-defmethod transient-format-value ((obj conn-transient-switches))
  (with-slots (value argument-format choices) obj
    (format
     (propertize "%s" 'face 'transient-delimiter)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face
                    (if (equal (format argument-format choice) value)
                        'transient-argument
                      'transient-inactive-value)))
      choices
      (propertize "|" 'face 'transient-delimiter)))))

;;;;; Kmacro Prefix

(defun conn--kmacro-display (macro &optional trunc)
  (pcase macro
    ((or 'nil '[] "") "nil")
    (_ (let* ((m (format-kbd-macro macro))
              (l (length m))
              (z (and trunc (> l trunc))))
         (format "%s%s"
                 (if z (substring m 0 (1- trunc)) m)
                 (if z "..." ""))))))

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
     (when (length> kmacro-ring 1)
       (conn--thread -->
           (car (last kmacro-ring))
         (kmacro--keys -->)
         (conn--kmacro-display --> 15)
         (concat --> ", ")))
     (propertize (conn--kmacro-display last-kbd-macro 15)
                 'face 'transient-value)
     (if (kmacro-ring-empty-p)
         ""
       (conn--thread -->
           (car kmacro-ring)
         (kmacro--keys -->)
         (conn--kmacro-display --> 15)
         (concat ", " -->))))))

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

(transient-define-infix conn--set-counter-format-infix ()
  "Set `kmacro-counter-format'."
  :class 'transient-lisp-variable
  :set-value (lambda (_ format) (kmacro-set-format format))
  :variable 'kmacro-counter-format
  :reader (lambda (&rest _)
            (read-string "Macro Counter Format: ")))

(transient-define-prefix conn-kmacro-prefix ()
  "Transient menu for kmacro functions."
  [:description
   conn--kmacro-ring-display
   :if-not conn--in-kbd-macro-p
   [("l" "List Macros" list-keyboard-macros
     :if (lambda () (version<= "30" emacs-version)))
    ("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("w" "Swap" kmacro-swap-ring :transient t)
    ("o" "Pop" kmacro-delete-ring-head :transient t)]
   [("i" "Insert Counter" kmacro-insert-counter)
    ("c" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix :transient t)]
   [:if
    (lambda () (version<= "30" emacs-version))
    ("q<" "Quit Counter Less" kmacro-quit-counter-less)
    ("q>" "Quit Counter Greater" kmacro-quit-counter-greater)
    ("q=" "Quit Counter Equal" kmacro-quit-counter-equal)]]
  [:if
   (lambda () (version<= "30" emacs-version))
   :description
   "Counter Registers"
   [("rs" "Save Counter Register" kmacro-reg-save-counter)
    ("rl" "Load Counter Register" kmacro-reg-load-counter)]
   [("r<" "Register Add Counter <" kmacro-reg-add-counter-less)
    ("r>" "Register Add Counter >" kmacro-reg-add-counter-greater)
    ("r=" "Register Add Counter =" kmacro-reg-add-counter-equal)]]
  ["Commands"
   :if-not conn--in-kbd-macro-p
   [("m" "Record Macro" kmacro-start-macro)
    ("k" "Call Macro" kmacro-call-macro)
    ("a" "Append to Macro" (lambda ()
                             (interactive)
                             (kmacro-start-macro '(4))))
    ("A" "Append w/o Executing" (lambda ()
                                  (interactive)
                                  (kmacro-start-macro '(16))))
    ("d" "Name Last Macro" kmacro-name-last-macro)]
   [("e" "Edit Macro" kmacro-edit-macro)
    ("E" "Edit Lossage" kmacro-edit-lossage)
    ("s" "Register Save" kmacro-to-register)
    ("c" "Apply Macro on Lines" apply-macro-to-region-lines)
    ("S" "Step Edit Macro" kmacro-step-edit-macro)]]
  [:if
   conn--in-kbd-macro-p
   ["Commands"
    ("q" "Query" kbd-macro-query)
    ("d" "Redisplay" kmacro-redisplay)]
   [:description
    conn--kmacro-counter-display
    ("i" "Insert Counter" kmacro-insert-counter)
    ("c" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)]])

;;;;; Kapply Prefix

(defun conn-recursive-edit-kmacro (arg)
  "Edit last keyboard macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive "P")
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-macro (not arg))
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
              (goto-char (match-beginning 1))
              (delete-region (match-beginning 1) (match-end 1))
              (insert (substitute-command-keys "\\[exit-recursive-edit]"))))
          (delete-other-windows)
          (conn-local-mode 1)
          (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
          (unwind-protect
              (recursive-edit)
            (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
            (kill-buffer buffer)))))))

(defun conn-recursive-edit-lossage ()
  "Edit lossage macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive)
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-lossage)
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (with-current-buffer buffer
          (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
            (goto-char (match-beginning 1))
            (delete-region (match-beginning 1) (match-end 1))
            (insert (substitute-command-keys "\\[exit-recursive-edit]")))
          (delete-other-windows)
          (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
          (unwind-protect
              (recursive-edit)
            (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
            (kill-buffer buffer)))))))

(defun conn--push-macro-ring (macro)
  (interactive
   (list (get-register (register-read-with-preview "Kmacro: "))))
  (unless (or (null macro)
              (stringp macro)
              (vectorp macro)
              (kmacro-p macro))
    (user-error "Invalid keyboard macro"))
  (kmacro-push-ring macro)
  (kmacro-swap-ring))

(transient-define-argument conn--kapply-empty-infix ()
  "Include empty regions in dispatch."
  :class 'transient-switch
  :key "o"
  :description "Skip Empty"
  :argument "skip")

(transient-define-argument conn--kapply-macro-infix ()
  "Dispatch `last-kbd-macro'.
APPLY simply executes the macro at each region.  APPEND executes
the macro and records additional keys on the first iteration.
STEP-EDIT uses `kmacro-step-edit-macro' to edit the macro before
dispatch."
  :class 'conn-transient-switches
  :description "Last Kmacro"
  :key "k"
  :argument "last-kmacro="
  :argument-format "last-kmacro=%s"
  :argument-regexp "\\(last-kmacro=\\(apply\\|append\\|step-edit\\)\\)"
  :choices '("apply" "step-edit" "append"))

(transient-define-argument conn--kapply-matches-infix ()
  "Restrict dispatch to only some isearch matches.
AFTER means only those matchs after, and including, the current match.
BEFORE means only those matches before, and including, the current match."
  :class 'conn-transient-switches
  :description "Restrict Matches Inclusive"
  :if-not (lambda () (bound-and-true-p multi-isearch-buffer-list))
  :key "j"
  :argument "matches="
  :argument-format "matches=%s"
  :argument-regexp "\\(matches=\\(after\\|before\\)\\)"
  :choices '("after" "before"))

(transient-define-argument conn--kapply-state-infix ()
  "Dispatch in a specific state."
  :class 'conn-transient-switches
  :required t
  :description "In State"
  :key "g"
  :argument "state="
  :argument-format "state=%s"
  :argument-regexp "\\(state=\\(emacs\\|conn\\)\\)"
  :choices '("conn" "emacs")
  :init-value (lambda (obj)
                (oset obj value
                      (format "state=%s"
                              (pcase conn-current-state
                                ('conn-emacs-state "emacs")
                                (_ "conn"))))))

(transient-define-argument conn--kapply-region-infix ()
  "How to dispatch on each region.
START means place the point at the start of the region before
each iteration.  END means place the point at the end of the
region before each iteration.  CHANGE means delete the region
before each iteration."
  :class 'conn-transient-switches
  :required t
  :key "t"
  :description "Regions"
  :argument "region="
  :argument-format "region=%s"
  :argument-regexp "\\(region=\\(start\\|change\\|end\\)\\)"
  :choices '("start" "end" "change")
  :init-value (lambda (obj) (oset obj value "region=start")))

(transient-define-argument conn--kapply-order-infix ()
  "Dispatch on regions from last to first."
  :class 'transient-switch
  :key "u"
  :description "Order"
  :argument "reverse")

(transient-define-argument conn--kapply-save-excursion-infix ()
  "Save the point and mark in each buffer during dispatch."
  :class 'transient-switch
  :key "se"
  :description "Excursions"
  :argument "excursions"
  :init-value (lambda (obj) (oset obj value "excursions")))

(transient-define-argument conn--kapply-save-restriction-infix ()
  "Save and restore the current restriction in each buffer during dispatch."
  :class 'transient-switch
  :key "sr"
  :description "Restrictions"
  :argument "restrictions"
  :init-value (lambda (obj) (oset obj value "restrictions")))

(transient-define-argument conn--kapply-merge-undo-infix ()
  "Merge all macro iterations into a single undo in each buffer."
  :class 'transient-switch
  :key "su"
  :description "Merge Undo"
  :argument "undo"
  :init-value (lambda (obj) (oset obj value "undo")))

(transient-define-argument conn--kapply-save-windows-infix ()
  "Save and restore current window configuration during dispatch."
  :class 'transient-switch
  :key "sw"
  :description "Windows"
  :argument "windows")

(transient-define-suffix conn--kapply-regexp-region-suffix (args)
  :transient 'transient--do-exit
  :key "ru"
  :description "Regexp in Region"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread -->
      (pcase-let* ((`(,beg . ,end) (cadr (conn--read-thing-region "Define Region")))
                   (regexp (minibuffer-with-setup-hook
                               (lambda ()
                                 (conn-yank-region-to-minibuffer 'regexp-quote))
                             (conn--read-from-with-preview
                              "Regexp" t nil (cons beg end))))
                   (regions))
        (save-excursion
          (with-restriction beg end
            (goto-char beg)
            (while (re-search-forward regexp nil t)
              (push (cons (match-beginning 0) (match-end 0)) regions))))
        regions)
    (conn--kapply-region-iterator --> (not (member "reverse" args)))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-string-region-suffix (args)
  :transient 'transient--do-exit
  :key "rq"
  :description "String in Region"
  (interactive (list (transient-args transient-current-command)))
  (deactivate-mark)
  (conn--thread -->
      (pcase-let* ((`(,beg . ,end) (cadr (conn--read-thing-region "Define Region")))
                   (string (minibuffer-with-setup-hook
                               (lambda ()
                                 (conn-yank-region-to-minibuffer))
                             (conn--read-from-with-preview
                              "String" nil nil (cons beg end))))
                   (regions))
        (save-excursion
          (with-restriction beg end
            (goto-char beg)
            (while (search-forward string nil t)
              (push (cons (match-beginning 0) (match-end 0)) regions))))
        regions)
    (conn--kapply-region-iterator --> (not (member "reverse" args)))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-regexp-suffix (args)
  :transient 'transient--do-exit
  :key "qu"
  :description "Regexp"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread -->
      (pcase-let* ((`(,beg . ,end) (cadr (conn--read-thing-region "Define Region")))
                   (regexp (conn--read-from-with-preview
                            "Regexp" t nil (cons beg end)))
                   (regions))
        (save-excursion
          (with-restriction beg end
            (goto-char beg)
            (while (re-search-forward regexp nil t)
              (push (cons (match-beginning 0) (match-end 0)) regions))))
        regions)
    (conn--kapply-region-iterator --> (not (member "reverse" args)))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-string-suffix (args)
  "Apply keyboard macro to every occurance of a string within a region.
The region is read by prompting for a command with a `:conn-command-thing'
property."
  :transient 'transient--do-exit
  :key "qq"
  :description "String"
  (interactive (list (transient-args transient-current-command)))
  (deactivate-mark)
  (conn--thread -->
      (pcase-let* ((`(,beg . ,end) (cadr (conn--read-thing-region "Define Region")))
                   (string (conn--read-from-with-preview
                            "String" nil nil (cons beg end)))
                   (regions))
        (save-excursion
          (with-restriction beg end
            (goto-char beg)
            (while (search-forward string nil t)
              (push (cons (match-beginning 0) (match-end 0)) regions))))
        regions)
    (conn--kapply-region-iterator --> (not (member "reverse" args)))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-things-suffix (args)
  "Apply keyboard macro on the current region.
If the region is discontiguous (e.g. a rectangular region) then
apply to each contiguous component of the region."
  :transient 'transient--do-exit
  :key "f"
  :description "Things"
  (interactive (list (transient-args transient-current-command)))
  (pcase-let ((`(,thing . ,regions) (conn--read-thing-region "Things")))
    (deactivate-mark)
    (conn--thread -->
      (if (member "skip" args)
          (seq-remove (lambda (reg) (conn-thing-empty-p thing reg))
                      regions)
        regions)
      (conn--kapply-region-iterator --> (member "reverse" args))
      (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
      (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
      (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
      (pcase-exhaustive (transient-arg-value "state=" args)
        ("conn" (conn--kapply-with-state --> 'conn-state))
        ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
      (pcase-exhaustive (transient-arg-value "region=" args)
        ("change" (conn--kapply-change-region -->))
        ("end" (conn--kapply-at-end -->))
        ("start" -->))
      (conn--kapply-pulse-region -->)
      (if (member "windows" args) (conn--kapply-save-windows -->) -->)
      (pcase (transient-arg-value "last-kmacro=" args)
        ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
        ("append" (conn--kmacro-apply-append -->))
        ("step-edit" (conn--kmacro-apply-step-edit -->))
        (_ (conn--kmacro-apply -->))))))

(transient-define-suffix conn--kapply-things-in-region-suffix (args)
  "Apply keyboard macro on the current region.
If the region is discontiguous (e.g. a rectangular region) then
apply to each contiguous component of the region."
  :transient 'transient--do-exit
  :key "v"
  :description "Things in Region"
  (interactive (list (transient-args transient-current-command)))
  (deactivate-mark)
  (conn--thread -->
      (conn--kapply-thing-iterator (conn--read-thing "Things")
                                   (region-beginning)
                                   (region-end)
                                   (member "reverse" args)
                                   (member "skip" args))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-iterate-suffix (args)
  "Apply keyboard macro a specified number of times."
  :transient 'transient--do-exit
  :key "i"
  :description "Iterate"
  (interactive (list (transient-args transient-current-command)))
  (let ((count (read-number "Iterations: " 0)))
    (conn--thread -->
        (conn--kapply-infinite-iterator)
      (if (member "undo" args) (conn--kapply-merge-undo -->) -->)
      (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
      (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
      (pcase-exhaustive (transient-arg-value "state=" args)
        ("conn" (conn--kapply-with-state --> 'conn-state))
        ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
      (pcase (transient-arg-value "last-kmacro=" args)
        ("apply" (conn--kmacro-apply --> count last-kbd-macro))
        ("append" (conn--kmacro-apply-append --> count))
        ("step-edit" (conn--kmacro-apply-step-edit --> count))
        (_ (conn--kmacro-apply --> count))))))

(transient-define-suffix conn--kapply-regions-suffix (iterator args)
  "Apply keyboard macro on regions."
  :transient 'transient--do-exit
  :key "v"
  :description "Regions"
  (interactive (list (oref transient-current-prefix scope)
                     (transient-args transient-current-command)))
  (conn--thread -->
      (funcall iterator (member "reverse" args))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-isearch-suffix (args)
  "Apply keyboard macro on current isearch matches."
  :transient 'transient--do-exit
  :key "m"
  :description "Matches"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread -->
      (let ((matches
             (if (bound-and-true-p multi-isearch-buffer-list)
                 (mapcan 'conn--isearch-matches
                         (append
                          (remq (current-buffer) multi-isearch-buffer-list)
                          (list (current-buffer))))
               (conn--isearch-matches
                (current-buffer)
                (pcase (transient-arg-value "matches=" args)
                  ("after" 'after)
                  ("before" 'before))))))
        (isearch-done)
        (cl-loop for (beg . end) in matches
                 collect (cons (conn--create-marker beg)
                               (conn--create-marker end))))
    (conn--kapply-region-iterator --> (member "reverse" args))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-suffix conn--kapply-text-property-suffix (prop value args)
  "Apply keyboard macro on regions of text with a specified text property."
  :transient 'transient--do-exit
  :key "x"
  :description "Text Prop"
  (interactive
   (let* ((prop (intern (completing-read
                         "Property: "
                         (cl-loop for prop in (text-properties-at (point))
                                  by #'cddr collect prop)
                         nil t)))
          (vals (mapcar (lambda (s) (cons (format "%s" s) s))
                        (ensure-list (get-text-property (point) prop))))
          (val (alist-get (completing-read "Value: " vals) vals
                          nil nil #'string=)))
     (list prop val (transient-args transient-current-command))))
  (conn--thread -->
      (save-excursion
        (goto-char (point-min))
        (let (regions)
          (while-let ((match (text-property-search-forward prop value t)))
            (push (cons (prop-match-beginning match)
                        (prop-match-end match))
                  regions))
          regions))
    (conn--kapply-region-iterator --> (not (member "reverse" args)))
    (if (member "undo" args) (conn--kapply-merge-undo --> t) -->)
    (if (member "restriction" args) (conn--kapply-save-restriction -->) -->)
    (if (member "excursions" args) (conn--kapply-save-excursion -->) -->)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--kapply-with-state --> 'conn-state))
      ("emacs" (conn--kapply-with-state --> 'conn-emacs-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--kapply-change-region -->))
      ("end" (conn--kapply-at-end -->))
      ("start" -->))
    (conn--kapply-pulse-region -->)
    (if (member "windows" args) (conn--kapply-save-windows -->) -->)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--kmacro-apply --> 0 last-kbd-macro))
      ("append" (conn--kmacro-apply-append -->))
      ("step-edit" (conn--kmacro-apply-step-edit -->))
      (_ (conn--kmacro-apply -->)))))

(transient-define-prefix conn-kapply-prefix ()
  "Transient menu for keyboard macro application on regions."
  [:description
   conn--kmacro-ring-display
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)]
   [("c" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("g" "Push Register" conn--push-macro-ring :transient t)]
   [("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (conn-recursive-edit-kmacro arg)
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (conn-recursive-edit-lossage)
       (transient-resume))
     :transient transient--do-suspend)]]
  [:description
   "Options:"
   [(conn--kapply-order-infix)
    (conn--kapply-state-infix)
    (conn--kapply-empty-infix)]
   [(conn--kapply-region-infix)
    (conn--kapply-macro-infix)]]
  [[:description
    "Apply Kmacro On:"
    (conn--kapply-text-property-suffix)
    (conn--kapply-iterate-suffix)
    (conn--kapply-regexp-suffix)
    (conn--kapply-string-suffix)]
   [:description
    ""
    (conn--kapply-things-suffix)
    (conn--kapply-things-in-region-suffix)
    (conn--kapply-regexp-region-suffix)
    (conn--kapply-string-region-suffix)]
   [:description
    "Save State:"
    (conn--kapply-merge-undo-infix)
    (conn--kapply-save-windows-infix)
    (conn--kapply-save-restriction-infix)
    (conn--kapply-save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-kapply-prefix))

(transient-define-prefix conn-isearch-kapply-prefix ()
  "Transient menu for keyboard macro application on isearch matches."
  [:description
   conn--kmacro-ring-display
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)]
   [("c" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("g" "Push Register" conn--push-macro-ring :transient t)]
   [("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (conn-recursive-edit-kmacro arg)
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (conn-recursive-edit-lossage)
       (transient-resume))
     :transient transient--do-suspend)]]
  [:description
   "Options:"
   [(conn--kapply-order-infix)
    (conn--kapply-region-infix)
    (conn--kapply-state-infix)]
   [(conn--kapply-matches-infix)
    (conn--kapply-macro-infix)]]
  [[:description
    "Apply Kmacro On:"
    (conn--kapply-isearch-suffix)]
   [:description
    "Save State:"
    (conn--kapply-merge-undo-infix)
    (conn--kapply-save-windows-infix)
    (conn--kapply-save-restriction-infix)
    (conn--kapply-save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-isearch-kapply-prefix))

(transient-define-prefix conn-regions-kapply-prefix (iterator)
  "Transient menu for keyboard macro application on regions."
  [:description
   conn--kmacro-ring-display
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)]
   [("c" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("g" "Push Register" conn--push-macro-ring :transient t)]
   [("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (conn-recursive-edit-kmacro arg)
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (conn-recursive-edit-lossage)
       (transient-resume))
     :transient transient--do-suspend)]]
  [:description
   "Options:"
   [(conn--kapply-order-infix)
    (conn--kapply-state-infix)]
   [(conn--kapply-region-infix)
    (conn--kapply-macro-infix)]]
  [[:description
    "Apply Kmacro On:"
    (conn--kapply-regions-suffix)]
   [:description
    "Save State:"
    (conn--kapply-merge-undo-infix)
    (conn--kapply-save-windows-infix)
    (conn--kapply-save-restriction-infix)
    (conn--kapply-save-excursion-infix)]]
  (interactive (list nil))
  (unless iterator (user-error "No regions"))
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-regions-kapply-prefix nil nil :scope iterator))

;;;;; Narrow Ring Prefix

(defun conn--narrow-ring-restore-state (state)
  (widen)
  (pcase-let ((`(,point ,mark ,min ,max ,narrow-ring) state))
    (narrow-to-region min max)
    (goto-char point)
    (save-mark-and-excursion--restore mark)
    (conn-clear-narrow-ring)
    (setq conn-narrow-ring
          (cl-loop for (beg . end) in narrow-ring
                   collect (cons (conn--create-marker beg)
                                 (conn--create-marker end))))))

(defun conn--format-narrowing (narrowing)
  (if (long-line-optimizations-p)
      (pcase-let ((`(,beg . ,end) narrowing))
        (format "(%s . %s)"
                (marker-position beg)
                (marker-position end)))
    (save-restriction
      (widen)
      (pcase-let ((`(,beg . ,end) narrowing))
        (format "%s+%s"
                (line-number-at-pos (marker-position beg) t)
                (count-lines (marker-position beg)
                             (marker-position end)))))))

(defun conn--narrow-ring-display ()
  (ignore-errors
    (concat
     (propertize "Narrow Ring: " 'face 'transient-heading)
     (propertize (format "[%s]" (length conn-narrow-ring))
                 'face 'transient-value)
     " - "
     (when (length> conn-narrow-ring 2)
       (format "%s, "  (conn--format-narrowing
                        (car (last conn-narrow-ring)))))
     (pcase (car conn-narrow-ring)
       ('nil (propertize "nil" 'face 'transient-value))
       ((and reg `(,beg . ,end)
             (guard (and (= (point-min) beg)
                         (= (point-max) end))))
        (propertize (conn--format-narrowing  reg)
                    'face 'transient-value))
       (reg
        (propertize (conn--format-narrowing reg)
                    'face 'bold)))
     (when (cdr conn-narrow-ring)
       (format ", %s"  (conn--format-narrowing
                        (cadr conn-narrow-ring)))))))

(transient-define-prefix conn-narrow-ring-prefix ()
  "Transient menu for narrow ring function."
  [:description
   conn--narrow-ring-display
   [("i" "Isearch forward" conn-isearch-narrow-ring-forward)
    ("I" "Isearch backward" conn-isearch-narrow-ring-backward)
    ("s" "Register Store" conn-narrow-ring-to-register :transient t)
    ("l" "Register Load" conn-register-load :transient t)]
   [("m" "Merge" conn-merge-narrow-ring :transient t)
    ("w" "Widen"
     (lambda ()
       (interactive)
       (widen)
       (conn-recenter-on-region)))
    ("c" "Clear" conn-clear-narrow-ring)
    ("v" "Add Region" conn-region-to-narrow-ring)
    ("N" "In Indired Buffer"
     (lambda ()
       (interactive)
       (let ((beg (point-min))
             (end (point-max))
             (buf (current-buffer))
             (win (selected-window)))
         (widen)
         (conn--narrow-indirect beg end)
         (with-current-buffer buf
           (if (eq (window-buffer win) buf)
               (with-selected-window win
                 (conn--narrow-ring-restore-state (oref transient-current-prefix scope)))
             (conn--narrow-ring-restore-state (oref transient-current-prefix scope)))))))]
   [("n" "Cycle Next" conn-cycle-narrowings :transient t)
    ("p" "Cycle Previous"
     (lambda (arg)
       (interactive "p")
       (conn-cycle-narrowings (- arg)))
     :transient t)
    ("d" "Pop" conn-pop-narrow-ring :transient t)
    ("a" "Abort Cycling"
     (lambda ()
       (interactive)
       (conn--narrow-ring-restore-state (oref transient-current-prefix scope))))]]
  (interactive)
  (transient-setup
   'conn-narrow-ring-prefix nil nil
   :scope (list (point) (save-mark-and-excursion--save)
                (point-min) (point-max)
                (cl-loop for (beg . end) in conn-narrow-ring
                         collect (cons (marker-position beg)
                                       (marker-position end))))))

;;;;; Register Prefix

(transient-define-prefix conn-register-prefix ()
  "Transient menu for register functions."
  ["Register Store:"
   [("v" "Point" point-to-register)
    ("m" "Macro" kmacro-to-register)
    ("t" "Tab" conn-tab-to-register)]
   [("f" "Frameset" frameset-to-register)
    ("r" "Rectangle" copy-rectangle-to-register)
    ("w" "Window Configuration" window-configuration-to-register)]]
  ["Register:"
   [("e" "Set Seperator" conn-set-register-seperator)
    ("i" "Increment" increment-register :transient t)
    ("s" "List" list-registers :transient t)]
   [("l" "Load" conn-register-load)
    ("u" "Unset" conn-unset-register :transient t)]])

;;;;; Fill Prefix

(transient-define-infix conn--set-fill-column-infix ()
  "Set `fill-column'."
  :class 'transient-lisp-variable
  :variable 'fill-column
  :set-value (lambda (_ val) (set-fill-column val))
  :reader (lambda (&rest _)
            (read-number (format "Change fill-column from %s to: " fill-column)
                         (current-column))))

(transient-define-infix conn--set-fill-prefix-infix ()
  "Toggle `fill-prefix'."
  :class 'transient-lisp-variable
  :set-value #'ignore
  :variable 'fill-prefix
  :reader (lambda (&rest _)
            (set-fill-prefix)
            (substring-no-properties fill-prefix)))

(transient-define-infix conn--auto-fill-infix ()
  "Toggle `auto-fill-function'."
  :class 'transient-lisp-variable
  :set-value #'ignore
  :variable 'auto-fill-function
  :reader (lambda (&rest _) (auto-fill-mode 'toggle)))

(transient-define-prefix conn-fill-prefix ()
  "Transient menu for fill functions."
  [["Fill:"
    ("r" "Region" fill-region)
    ("i" "Paragraph" fill-paragraph)
    ("k" "Region as Paragraph" fill-region-as-paragraph)]
   ["Options:"
    ("c" "Column" conn--set-fill-column-infix)
    ("p" "Prefix" conn--set-fill-prefix-infix)
    ("a" "Auto Fill Mode" conn--auto-fill-infix)]])

;;;;; Sort Prefix

(transient-define-infix conn--case-fold-infix ()
  "Toggle `sort-fold-case'."
  :class 'transient-lisp-variable
  :variable 'sort-fold-case
  :reader (lambda (&rest _) (not sort-fold-case)))

(transient-define-prefix conn-sort-prefix ()
  "Transient menu for buffer sorting functions."
  [["Sort Region: "
    ("a" "sort pages" sort-pages)
    ("c" "sort columns" sort-columns)
    ("l" "sort lines" sort-lines)
    ("o" "org sort" org-sort
     :if (lambda () (eq major-mode 'org-mode)))]
   [("f" "case fold" conn--case-fold-infix)
    ("n" "sort numeric fields" sort-numeric-fields)
    ("p" "sort paragraphs" sort-paragraphs)
    ("r" "sort regexp fields" sort-regexp-fields)]])

;;;;; Case Prefix

(transient-define-prefix conn-region-case-prefix ()
  "Transient menu for case in region."
  ["Change Case"
   [("k" "kebab-case" conn-kebab-case-region)
    ("a" "CapitalCase" conn-capital-case-region)
    ("m" "camelCase" conn-camel-case-region)]
   [("n" "Snake_Case" conn-capital-snake-case-region)
    ("s" "snake_case" conn-snake-case-region)
    ("w" "individual words" conn-case-to-words-region)]
   [("u" "UPCASE" upcase-region)
    ("c" "Capitalize" capitalize-region)
    ("d" "downcase" downcase-region)]])


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
  "f" 'conn-isearch-region-forward
  "b" 'conn-isearch-region-backward
  "DEL" (conn-remapping-command conn-delete-region-keys)
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
  "c" 'conn-copy-thing
  "D" 'conn-duplicate-and-comment-region
  "d" 'conn-duplicate-region
  "e c" 'conn-split-region-on-regexp
  "g" 'conn-rgrep-region
  "i" 'clone-indirect-buffer
  "j" 'conn-join-lines
  "I" 'indent-rigidly
  "N" 'conn-narrow-indirect-to-region
  "n" 'conn-narrow-to-region
  "s" 'conn-sort-prefix
  "o" 'conn-occur-region
  "q" 'conn-replace-region-in-thing
  "u" 'conn-regexp-replace-region-in-thing
  "V" 'vc-region-history
  "y" 'yank-rectangle
  "Y" 'conn-yank-lines-as-rectangle
  "l" 'join-line)

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
  "s" 'conn-isearch-forward-thing
  "r" 'conn-isearch-backward-thing
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
  "r DEL" 'delete-rectangle
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "d" 'open-rectangle
  "<backspace>" 'clear-rectangle
  "C-d" 'delete-whitespace-rectangle
  "#" 'rectangle-number-lines)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

(defvar-keymap conn-edit-map
  "f" 'conn-fill-prefix
  "c" 'conn-region-case-prefix
  "TAB" 'indent-for-tab-command
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
  "x" 'conn-narrow-ring-prefix
  "s" 'conn-surround-thing
  "d" 'duplicate-dwim
  "w p" 'conn-kill-prepend-region
  "w a" 'conn-kill-append-region
  "y" 'yank-in-context
  "q" 'conn-replace-in-thing
  "u" 'conn-regexp-replace-in-thing)

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
  "j" 'conn-backward-char
  "K" (conn-remapping-command conn-forward-paragraph-keys)
  "k" (conn-remapping-command conn-next-line-keys)
  "L" 'conn-end-of-inner-line
  "l" 'conn-forward-char
  "M" (conn-remapping-command conn-end-of-defun-keys)
  "m" (conn-remapping-command conn-forward-sexp-keys)
  "N" (conn-remapping-command conn-beginning-of-defun-keys)
  "n" (conn-remapping-command conn-backward-sexp-keys))

(define-keymap
  :keymap conn-state-map
  :parent conn-movement-map
  "<remap> <toggle-input-method>" 'conn-toggle-input-method
  "`" 'other-window
  "|" 'conn-shell-command-on-region
  "'" 'conn-other-place-prefix
  "+" 'conn-set-register-seperator
  "." 'repeat
  "/" 'undo-only
  ";" 'conn-wincontrol
  "<tab>" 'indent-region
  "TAB" 'indent-region
  "=" 'indent-relative
  "?" 'undo-redo
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
  "C-t" (conn-remapping-command (key-parse "C-x t"))
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" conn-edit-map
  "c" (conn-remapping-command (key-parse "C-c"))
  "C" 'conn-copy-region
  "d" (conn-remapping-command conn-delete-char-keys)
  "f" 'conn-dispatch-on-things
  "g" (conn-remapping-command (key-parse "M-g"))
  "h" 'conn-expand
  "H" conn-mark-thing-map
  "p" 'conn-register-load
  "P" 'conn-register-prefix
  "q" 'conn-transpose-regions
  "r" 'conn-region-map
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
  "SPC" 'conn-scroll-up
  "<backspace>" 'conn-scroll-down
  "DEL" 'conn-scroll-down
  "." 'point-to-register
  "/" 'undo-only
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "*" 'conn-org-tree-edit-insert-heading
  "<" 'org-promote-subtree
  ">" 'org-demote-subtree
  "?" 'undo-redo
  "f" 'conn-dispatch-on-things
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
  "C-`" 'other-window)

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
        (cl-pushnew 'conn--transition-maps emulation-mode-map-alists)
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
                            conn--local-mode-maps
                            conn--transition-maps)
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

  (conn-register-thing
   'org-paragraph
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'org-paragraph t)
   :forward-op 'org-forward-paragraph
   :mark-key "I"
   :modes 'org-mode)

  (conn-register-thing-commands
   'org-paragraph 'conn-sequential-thing-handler
   'org-forward-paragraph 'org-backward-paragraph)

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
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'org-element '(org-mode))
   :beg-op 'org-backward-element
   :end-op 'org-forward-element
   :modes 'org-mode)

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
   :dispatch-provider (apply-partially 'conn--dispatch-all-things 'org-heading '(org-mode))
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
                     'paredit-backward-up))

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

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-emacs-state)
      (conn-pop-state)))
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
;;; conn.el ends here
