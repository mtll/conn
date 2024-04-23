;;; conn.el --- Modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode and keyboard macro enhancement
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4"))
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
;; Modal keybinding mode
;;
;;; Code:

;;;; Requires

(require 'transient)
(require 'compat)
(require 'thingatpt)
(require 'rect)
(require 'isearch)
(require 'repeat)
(require 'hi-lock)
(require 'kmacro)
(require 'sort)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))


;;;; Variables

;;;;; Declerations

;; Some of the following custom vars need these
;; vars so we need to declare them up here.
(defvar conn-mode nil)
(defvar conn-local-mode)
(defvar conn-modes)
(defvar conn-local-map)
(defvar conn-emacs-state)
(defvar kmacro-step-edit-replace)

(defvar conn--mark-cursor-timer nil
  "`run-with-idle-timer' timer to update `mark' cursor.")

(defvar-keymap conn-mode-map
  :doc "`conn-mode' keymap which is always active.
This keymap is active even in buffers which do not have
`conn-local-mode' turned on.")

;;;;; Custom Variables

(defgroup conn-mode nil
  "Modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-dots nil
  "Conn-mode dots."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-marks nil
  "Conn-mode marks."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-states nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-key-remappings nil
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
  "Alist of the form ((REGEXP . STATE) ...).
Defines default STATE for buffers matching REGEXP."
  :type '(list (cons string symbol))
  :group 'conn-states)

(defface conn-dot-face
  '((default              (:background "#c6ebd9"))
    (((background dark))  (:background "#449066"))
    (((background light)) (:background "#c6ebd9")))
  "Face for dots."
  :group 'conn-dots)

(defface conn-pulse-face
  '((default (:inherit conn-dot-face)))
  "Face for dispatch pulse."
  :group 'conn)

(defface conn-mark-face
  '((default              (:inherit cursor :background "#dfa0f0"))
    (((background light)) (:inherit cursor :background "#dfa0f0"))
    (((background dark))  (:inherit cursor :background "#a742b0")))
  "Face for mark."
  :group 'conn-marks)

(defface conn-window-prompt-face
  '((default              (:height 5.0 :foreground "#d00000"))
    (((background light)) (:height 5.0 :foreground "#d00000"))
    (((background dark))  (:height 5.0 :foreground "#7c0000")))
  "Face for conn window prompt overlay."
  :group conn-mode)

;; Isearch uses a priority of 1001 for
;; its lazy highlighting, we want to be
;; less than that by default.
(defcustom conn-dot-overlay-priority 1000
  "Priority of dot overlays."
  :type 'integer
  :group 'conn-dots)

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :group 'conn)

(defcustom conn-repeating-cursor-color
  "#a60000"
  "Cursor color while repeat map is active."
  :type 'color
  :group 'conn)

(defcustom conn-ephemeral-mark-states
  nil
  "States in which ephemeral marks should be used."
  :type '(repeat symbol)
  :group 'conn-marks)

(defcustom conn-other-window-prompt-threshold 4
  "Number of windows before conn-other-window prompts for window."
  :type 'integer
  :group conn-mode)

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

(defcustom conn-region-case-style-actions
  (list 'conn-kebab-case-region
        'conn-capital-snake-case-region
        'conn-snake-case-region
        'conn-capital-case-region
        'conn-camel-case-region)
  "List of actions cycled through by `conn-region-case-style-cycle'.
Supported values are:
`conn-kebab-case-region'
`conn-capital-snake-case-region'
`conn-snake-case-region'
`conn-capital-case-region'
`conn-camel-case-region'."
  :group 'conn
  :type '(repeat symbol))

(defcustom conn-read-pair-split-string "	"
  "String on which to split `conn-insert-pair' brackets."
  :group 'conn
  :type 'string)

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
    t))

(defvar conn-mode-buffers
  '("\\*Edit Macro\\*"))

(defvar conn-enable-in-buffer-hook
  (list (apply-partially 'easy-mmode--globalized-predicate-p conn-in-modes)
        'conn-mode-buffer-predicate)
  "Hook to determine if `conn-local-mode' should be enabled in a buffer.
Each function is run without any arguments and if any of them return
non-nil `conn-local-mode' will be enabled in the buffer.")

(defvar conn-disable-in-buffer-hook
  nil
  "Hook to determine if `conn-local-mode' should be enabled in a buffer.
Each function is run without any arguments and if any of them return
nil `conn-local-mode' will be not enabled in the buffer.")

;;;;;; State Keymaps

(defvar conn--state-maps nil)

(defvar-local conn--aux-maps nil)

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

(defvar conn-this-command-start nil
  "Start position for current mark movement command.")

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.
Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--local-mark-thing-map (make-sparse-keymap))

(defvar-local conn--mark-unpop-ring nil)

(defvar-local conn--ephemeral-mark nil)

(defvar conn--saved-ephemeral-marks nil)

(defvar-local conn--mark-cursor nil
  "`mark' cursor overlay.")
(put 'conn--mark-cursor 'permanent-local t)
(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)

;;;;; Dot Undo Ring Variables

(defvar-local conn--dot-undoing nil)

(defvar-local conn--dot-undo-ring nil)

(defvar-local conn--dot-undone nil)

(defvar-local conn--dot-this-undo nil)

(defvar conn-dot-undo-ring-max 32
  "Maximum size of the dot undo ring.")

;;;;; Aux Map Variables

(defvar conn--aux-bindings nil)

(defvar-keymap conn-mark-thing-map
  :prefix 'conn-mark-thing-map)

(defvar conn--aux-update-flag nil)

(defvar-local conn--aux-map-history nil)

(defvar conn--aux-map-history-size 8)

(defvar conn--last-remapping nil)

;;;;; Dot Overlay Properties

(put 'conn--dot 'evaporate t)
(put 'conn--dot 'priority conn-dot-overlay-priority)
(put 'conn--dot 'face 'conn-dot-face)
(put 'conn--dot 'evaporate t)

;;;;; Command Histories

(defvar conn--seperator-history nil
  "History var for `conn-set-register-seperator'.")

(defvar-local conn--mode-line-indicator "")


;;;; Utilities

(eval-and-compile
  (defun conn--thread-1 (needle first &rest forms)
    (if (car forms)
        `((setq ,needle ,first)
          ,@(apply #'conn--thread-1 needle forms))
      (list first)))

  (defmacro conn--thread (needle form &rest forms)
    (declare (indent 2))
    (if forms
        `(let ((,needle ,form))
           ,@(apply #'conn--thread-1 needle forms))
      form))

  (defun conn--stringify (&rest symbols-or-strings)
    "Concatenate all SYMBOLS-OR-STRINGS to create a new symbol."
    (conn--thread <>
        (lambda (e)
          (cl-etypecase e
            (string e)
            (symbol (symbol-name e))))
      (mapcar <> symbols-or-strings)
      (apply #'concat <>)))

  (defun conn--symbolicate (&rest symbols-or-strings)
    "Concatenate all SYMBOLS-OR-STRINGS to create a new symbol."
    (intern (apply #'conn--stringify symbols-or-strings))))

(defmacro conn--without-conn-maps (&rest body)
  (declare (indent 0))
  `(let ((emulation-mode-map-alists (seq-difference
                                     emulation-mode-map-alists
                                     '(conn--transition-maps
                                       conn--local-mode-maps
                                       conn--major-mode-maps
                                       conn--local-maps
                                       conn--aux-maps
                                       conn--state-maps)
                                     #'eq)))
     ,(macroexp-progn body)))

;; From repeat-mode
(defun conn--command-property (property)
  (or (and (symbolp this-command)
           (get this-command property))
      (and (symbolp real-this-command)
           (get real-this-command property))))

;; From expand-region
(defun conn--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face)
            '(font-lock-comment-face font-lock-comment-delimiter-face))))

;; From misearch
(defun conn-read-matching-dot-buffers ()
  "Return a list of buffers whose names match specified regexp.
Uses `read-regexp' to read the regexp."
  ;; Most code from `multi-occur-in-matching-buffers'
  ;; and `kill-matching-buffers'.
  (let ((bufregexp
         (read-regexp "Search in buffers whose names match regexp")))
    (when bufregexp
      (delq nil (mapcar (lambda (buf)
                          (when (string-match bufregexp (buffer-name buf))
                            buf))
                        (buffer-list))))))

;; From misearch
(defun conn-read-dot-buffers ()
  "Return a list of buffers specified interactively, one by one."
  ;; Most code from `multi-occur'.
  (let* ((collection (mapcar #'buffer-name
                             (seq-filter #'conn--dots-active-p (buffer-list)))))
    (completing-read-multiple "First buffer: " collection nil t)))

(defun conn--beginning-of-region-or-restriction ()
  (if (use-region-p) (region-beginning) (point-min)))

(defun conn--end-of-region-or-restriction ()
  (if (use-region-p) (region-end) (point-max)))

(defun conn--create-marker (pos &optional buffer)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    marker))

(defun conn--derived-mode-property (property &optional buffer)
  "Check major mode in BUFFER and each `derived-mode-parent' for PROPERTY.
If BUFFER is nil check `current-buffer'."
  (let* ((modes (conn--thread mode
                    'major-mode
                  (buffer-local-value mode (or buffer (current-buffer)))
                  (derived-mode-all-parents mode)))
         result)
    (while (and modes (not result))
      (setq result (get (pop modes) property)))
    result))

(defun conn--narrow-indirect (beg end)
  (let* ((line-beg (line-number-at-pos beg))
         (linenum  (- (line-number-at-pos end) line-beg))
         (name     (format "%s@%s+%s - %s"
                           (buffer-name (current-buffer)) line-beg linenum
                           (thread-first
                             (buffer-substring-no-properties beg end)
                             (string-trim)
                             (substring 0 20))))
         (buffer   (clone-indirect-buffer-other-window name nil)))
    (pop-to-buffer buffer)
    (narrow-to-region beg end)
    (deactivate-mark)))

;; From embark
(defun conn--command-name (cmd)
  "Return an appropriate name for CMD.
If CMD is a symbol, use its symbol name; for lambdas, use the
first line of the documentation string; for keyboard macros use
`key-description'; otherwise use the word \"unnamed\"."
  (concat ; fresh copy, so we can freely add text properties
   (cond
    ((or (stringp cmd) (vectorp cmd)) (key-description cmd))
    ((stringp (car-safe cmd)) (car cmd))
    ((eq (car-safe cmd) 'menu-item) (eval (cadr cmd)))
    ((keymapp cmd)
     (propertize (if (symbolp cmd) (format "+%s" cmd) "<keymap>")
                 'face 'embark-keymap))
    ((symbolp cmd)
     (let ((name (symbol-name cmd)))
       (if (string-prefix-p "conn-" name) ; direct action mode
           (format "%s" (string-remove-prefix "conn-" name))
         name)))
    ((when-let (doc (and (functionp cmd) (ignore-errors (documentation cmd))))
       (save-match-data
         (when (string-match "^\\(.*\\)$" doc)
           (match-string 1 doc)))))
    (t "<unnamed>"))))

(defun conn--get-map-bindings (prefix map)
  (let ((prefix-map (if (= 0 (seq-length prefix))
                        map
                      (keymap-lookup map (key-description prefix))))
        binds)
    (cond
     ((or (null prefix-map) (numberp prefix-map)))
     ((keymapp prefix-map)
      (map-keymap
       (lambda (key def)
         (cond
          ((and (numberp key)
                (= key 27)
                (keymapp def))
           (map-keymap
            (lambda (key2 def2)
              (unless (memq def (list 'undefined 'self-insert-command 'digit-argument
                                      'negative-argument 'embark-keymap-help nil))
                (push (cons (vconcat (vector key key2)) def2) binds)))
            def))
          (t (push (cons (vector key) def) binds))))
       (keymap-canonicalize prefix-map))))
    (nreverse binds)))

(defvar-keymap conn-read-thing-command-mark-map
  "h" 'conn--local-mark-thing-map
  "C-h" 'conn--completing-read-thing)

(defun conn--completing-read-thing ()
  (let* ((prefix (key-description
                  (car (where-is-internal 'conn--local-mark-thing-map
                                          (list conn-read-thing-command-mark-map)))))
         (cmds (mapcar (pcase-lambda (`(,key . ,def))
                         (cons (concat (propertize
                                        (concat prefix " " (key-description key))
                                        'face 'help-key-binding)
                                       ": "
                                       (conn--command-name def))
                               def))
                      (conn--get-map-bindings nil conn--local-mark-thing-map))))
    (alist-get (completing-read "Command: " cmds) cmds nil nil #'equal)))

(defun conn--completing-read-thing-keymap ()
  (let* ((keymap (copy-keymap conn-read-thing-command-mark-map))
         (mark-map-keys (where-is-internal 'conn--local-mark-thing-map
                                           (list keymap))))
    (when mark-map-keys
      (dolist (key mark-map-keys)
        (define-key keymap key conn--local-mark-thing-map)))
    keymap))

(defun conn--read-thing-command ()
  (with-temp-message ""
    (let ((keymap (conn--completing-read-thing-keymap)))
      (internal-push-keymap keymap 'overriding-terminal-local-map)
      (unwind-protect
          (let ((key (thread-first
                       (concat
                        (propertize "Thing Command\n" 'face 'bold)
                        (propertize "C-h" 'face 'help-key-binding)
                        ": completing-read mark thing map")
                       (read-key-sequence)
                       (key-binding t))))
            (while (not (get key :conn-command-thing))
              (pcase key
                ('keyboard-quit
                 (keyboard-quit))
                ('conn--completing-read-thing
                 (internal-pop-keymap keymap 'overriding-terminal-local-map)
                 (setq key (condition-case nil
                               (conn--completing-read-thing)
                             (quit)))
                 (internal-push-keymap keymap 'overriding-terminal-local-map))
                (_
                 (setq key (thread-first
                             (concat
                              (propertize "Thing Command\n" 'face 'bold)
                              (propertize "C-h" 'face 'help-key-binding)
                              ": completing-read mark thing map\n"
                              (propertize "Not a valid thing command"
                                          'face 'error))
                             (read-key-sequence)
                             (key-binding t))))))
            (get key :conn-command-thing))
        (internal-pop-keymap keymap 'overriding-terminal-local-map)))))

(defun conn--isearch-matches-in-buffer (&optional buffer restrict)
  (with-current-buffer (or buffer (current-buffer))
    (let (bound)
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
        (setq bound (if isearch-forward (point-max) (point-min)))
        (cl-loop for match = (isearch-search-string isearch-string bound t)
                 while match
                 when (funcall isearch-filter-predicate
                               (match-beginning 0) (match-end 0))
                 collect (cons (conn--create-marker (match-beginning 0))
                               (conn--create-marker (match-end 0))))))))

(defun conn--read-string-preview-overlays (string &optional dir)
  (let (ovs)
    (save-excursion
      (with-restriction
          (if (eq dir 'forward)  (point) (window-start))
          (if (eq dir 'backward) (point) (window-end))
        (goto-char (point-max))
        (while (search-backward string nil t)
          (push (make-overlay (point) (+ (point) (length string)))
                ovs)
          (overlay-put (car ovs) 'face 'isearch))))
    ovs))

(defvar conn-read-string-timout 0.5)

(defun conn--read-string-with-timeout (timeout &optional dir)
  (let* ((string (char-to-string (read-char "char 0: " t)))
         (overlays (conn--read-string-preview-overlays string dir))
         next-char)
    (unwind-protect
        (while (setq next-char (read-char (format "char (%s):" string) t timeout))
          (setq string (concat string (char-to-string next-char)))
          (mapc #'delete-overlay overlays)
          (setq overlays (conn--read-string-preview-overlays string dir)))
      (mapc #'delete-overlay overlays)
      (message nil))
    string))

(defun conn--create-window-prompt-overlay (window id)
  (with-current-buffer (window-buffer window)
    (let ((overlay (make-overlay (window-start window)
                                 (window-end window))))
      (overlay-put overlay 'face 'shadow)
      (overlay-put overlay 'window window)
      (overlay-put overlay 'before-string
                   (propertize (number-to-string id)
                               'face 'conn-window-prompt-face))
      overlay)))

(defun conn--all-visible-windows ()
  (let (wins)
    (walk-windows (lambda (win) (push win wins)) 'nomini 'visible)
    wins))

(defun conn--prompt-for-window (windows)
  (when (setq windows (seq-remove 'window-dedicated-p windows))
    (if (length= windows 1)
        (car windows)
      (let ((overlays (seq-map-indexed #'conn--create-window-prompt-overlay
                                       windows))
            num)
        (unwind-protect
            (while (or (not num)
                       (< num 0)
                       (length< windows num))
              (setq num (if (length> windows 10)
                            (read-number "Window: ")
                          (- (logand (read-char "Window: ") ?\177) ?0))))
          (dolist (ov overlays)
            (delete-overlay ov)))
        (nth num windows)))))


;;;; Extensions

(defvar conn--extensions nil)

(defun conn--setup-extensions ()
  "Run when `conn-mode' is turned on or off to turn extensions on or off."
  (run-hook-with-args 'conn--extensions conn-mode))

(defmacro conn-define-extension (name &rest body)
  "Define a Conn extension.

\(fn NAME [DOCSTRING] &rest body)"
  (declare (doc-string 2) (indent 1))
  (let (doc)
    (when (stringp (car body))
      (setq doc (pop body)))
    `(progn
       (defvar ,name nil)

       (defun ,name (&optional enable)
         ,(or doc "")
         (interactive (list (not ,name)))
         (cond ((eq enable 'toggle)
                (setq enable (not ,name)))
               ((numberp enable)
                (setq enable (< 0 enable))))
         (when (xor enable ,name)
           (let ((fn (get ',name :conn-feature-function)))
             (when conn-mode (funcall fn enable))
             (if enable
                 (progn
                   (when (called-interactively-p 'interactive)
                     (message ,(conn--stringify name " enabled.")))
                   (add-hook 'conn--extensions fn))
               (when (called-interactively-p 'interactive)
                 (message ,(conn--stringify name " disabled.")))
               (remove-hook 'conn--extensions fn))))
         enable)

       (when-let ((body-fn (get ',name :conn-feature-function)))
         (funcall body-fn nil)
         (remove-hook 'conn--extensions body-fn)
         (put ',name :conn-feature-function nil))

       (let ((body-sym (make-symbol ,(conn--stringify name "-body-fn"))))
         (fset body-sym (lambda (enable)
                          (setq ,name (when enable t))
                          ,@body))
         (put ',name :conn-feature-function body-sym))

       ',name)))


;;;; Mark

(defmacro conn--thing-bounds-command (thing)
  (let ((name (conn--symbolicate "conn-mark-" thing)))
    `(progn
       (defun ,name ()
         (interactive)
         (pcase (bounds-of-thing-at-point ',thing)
           (`(,beg . ,end)
            (goto-char beg)
            (conn--push-ephemeral-mark end)))
         (activate-mark))
       (put ',name :conn-command-thing ',thing)
       ',name)))

(defmacro conn-register-thing (thing &rest rest)
  "Register a new THING.

\(fn THING &key FORWARD-OP BEG-OP END-OP BOUNDS-OP MODES MARK-KEY EXPAND-KEY)"
  (declare (indent 1))
  (unless (or (intern-soft (format "forward-%s" thing))
              (get thing 'forward-op)
              (memq :forward-op rest)
              (get thing 'bounds-of-thing-at-point)
              (memq :bounds-op rest)
              (and (or (get thing 'beginning-op)
                       (memq :beg-op rest))
                   (or (get thing 'end-op)
                       (memq :end-op rest))))
    (error "%s definition requires at least one of: %s, %s, or (%s and %s)"
           thing :forward-op :bounds-op :beg-op :end-op))
  (macroexp-progn
   (nconc
    `((intern ,(symbol-name thing)))
    (when-let ((forward (plist-get rest :forward-op)))
      `((put ',thing 'forward-op ,forward)))
    (when-let ((beg (plist-get rest :beg-op)))
      `((put ',thing 'beginning-op ,beg)))
    (when-let ((end (plist-get rest :end-op)))
      `((put ',thing 'end-op ,end)))
    (when-let ((bounds (plist-get rest :bounds-op)))
      `((put ',thing 'bounds-of-thing-at-point ,bounds)))
    (when-let ((binding (plist-get rest :mark-key)))
      `((let ((mark-command (conn--thing-bounds-command ,thing)))
           ,(if-let ((modes (plist-get rest :modes)))
                `(dolist (mode (ensure-list ,modes))
                   (setf (alist-get ,binding (get mode :conn-mode-things)
                                    nil nil #'equal)
                         mark-command))
              `(keymap-set conn-mark-thing-map ,binding mark-command))))))))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER."
  (dolist (cmd commands)
    (put cmd :conn-command-thing thing))
  (apply 'conn-set-command-handler handler commands))

(defmacro conn-define-thing-handler (name lambda-list &rest rest)
  "Define a thing movement command mark handler constructor.
Defines a constructor function NAME which takes LAMBDA-LIST as its
arguments, the first of which will should be the thing.  The first
time NAME is called with a unique THING it creates a closure over
LAMBDA, assigns it to the function value of an uninterned symbol
and associates that symbol with THING.  The symbol associated with
THING is always returned.

\(fn NAME LAMBDA-LIST [DOCSTRING] LAMBDA)"
  (declare (doc-string 3) (indent defun))
  (let ((docstring (if (stringp (car rest)) (pop rest) ""))
        (lambda (car rest))
        (sym (gensym "sym"))
        (ts (gensym "tsymbol"))
        (ss (gensym "ssymbol"))
        (thing (car lambda-list)))
    `(progn
       (defvar ,name)
       (if (boundp ',name)
           (conn--thread <>
               (pcase-lambda  (`(,,ts . ,,ss))
                 (let ((,thing ,ts))
                   (fset ,ss ,lambda)
                   (cons ,ss ,thing)))
             (mapcar <> ,name)
             (setf ,name <>))
         (setq ,name nil))

       (defun ,name ,lambda-list
         ,docstring
         (or (alist-get ,thing ,name)
             (let ((,sym (make-symbol (conn--stringify ',name "-" ,thing))))
               (fset ,sym ,lambda)
               (setf (alist-get ,thing ,name) ,sym)))))))

(conn-define-thing-handler conn-sequential-thing-handler (thing)
  "Return a continuous mark handler for THING.
If one has already been created return it, otherwise create a new one.
Continuous handlers will mark all THINGs when moving over multiple
THINGs at once unless `region-active-p'."
  (lambda (beg)
    (unless (or (region-active-p)
                (= (point) beg)
                (= 0 (prefix-numeric-value current-prefix-arg)))
      (let* ((dir (cl-signum (- (point) beg)))
             (dist (* dir (prefix-numeric-value current-prefix-arg))))
        (save-excursion
          (when (> (abs dist) 1)
            (forward-thing thing (- (+ dist (- dir)))))
          (funcall (or (get thing (if (> dir 0) 'beginning-op 'end-op))
                       (lambda () (forward-thing thing (- dir)))))
          (conn--push-ephemeral-mark))))))

(conn-define-thing-handler conn-individual-thing-handler (thing)
  "Return a discrete mark handler for THING.
If one has already been created return it, otherwise create a new one.
Discrete handlers will only mark the last THING when moving over
multiple THINGs at once unless `region-active-p'."
  (lambda (_)
    (unless (region-active-p)
      (pcase (bounds-of-thing-at-point thing)
        (`(,beg . ,end)
         (conn--push-ephemeral-mark (if (= (point) end) beg end)))
        (_ (conn--push-ephemeral-mark (point)))))))

(defun conn-jump-handler (beg)
  "Mark trail handler.
The mark trail handler pushes an ephemeral mark at the starting point
of the movement command unless `region-active-p'."
  (unless (or (region-active-p)
              (eq beg (point)))
    (conn--push-ephemeral-mark beg)))

(defun conn-set-command-handler (handler &rest commands)
  "Register a thing movement command for THING."
  (dolist (cmd (ensure-list commands))
    (put cmd :conn-mark-handler handler)))

(defun conn--mark-cursor-p (ov)
  (eq (overlay-get ov 'category) 'conn--mark-cursor))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.
For the meaning of MSG and ACTIVATE see `push-mark'."
  (push-mark location (not msg) activate)
  (setq conn--ephemeral-mark t)
  nil)

(defun conn--update-cursor (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if-let ((cursor (symbol-value (get conn-current-state :conn-cursor-type))))
        (setq cursor-type cursor)
      (setq cursor-type t))))

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
        (overlay-put conn--mark-cursor 'category 'conn--mark-cursor))
       (t
        (move-overlay conn--mark-cursor (mark t) (1+ (mark t)))
        (overlay-put conn--mark-cursor 'after-string
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
  (setq conn-this-command-start
        (when (memq conn-current-state conn-ephemeral-mark-states)
          (setq conn-this-command-start (point)))))

(defun conn--mark-post-command-hook ()
  (with-demoted-errors "error marking thing: %s"
    (when-let ((_ conn-this-command-start)
               (handler (or conn-this-command-handler
                            (conn--command-property :conn-mark-handler))))
      (funcall handler conn-this-command-start)))
  (setq conn-this-command-handler nil))

(defun conn--setup-mark ()
  (when conn--mark-cursor-timer
    (cancel-timer conn--mark-cursor-timer)
    (setq conn--mark-cursor-timer nil))
  (when conn-mode
    (setq conn--mark-cursor-timer
          (run-with-idle-timer conn-mark-update-delay
                               t #'conn--mark-cursor-timer-func))))

(defun conn--delete-mark-cursor ()
  (save-restriction
    (widen)
    (dolist (ov (conn--all-overlays 'conn--mark-cursor-p
                                    (point-min) (point-max)))
      (delete-overlay ov)))
  (setq conn--mark-cursor nil))


;;;; Macro Dispatch

(defvar conn-macro-dispatch-p nil
  "Non-nil during macro dispatch.")

(defvar conn-dispatch-error nil
  "If non-nil contains the error encountered during macro dispatch.")

(defvar conn-macro-dispatch-buffer-start-hook nil
  "Hook run in each buffer when it is first encountered during macro dispatch.")

(defvar conn-macro-dispatch-buffer-end-hook nil
  "Hook run in each buffer encountered after macro dispatch finishes.")

(defvar conn-macro-dispatch-end-hook nil
  "Hook run after macro dispatch has completed.")

(defvar conn-macro-dispatch-start-hook nil
  "Hook run before macro dispatch begins.")

(defvar conn-macro-dispatch-iterator-hook nil
  "Hook run during each iteration of macro dispatch.
If any function returns a nil value then dispatch it halted.")

(defun conn--region-iterator (regions &optional reverse)
  (when reverse (setq regions (reverse regions)))
  (dolist (reg regions)
    (unless (markerp (car reg))
      (setcar reg (conn--create-marker (car reg))))
    (unless (markerp (cdr reg))
      (setcdr reg (conn--create-marker (cdr reg)))))
  (lambda (state)
    (pcase state
      (:finalize
       (pcase-dolist (`(,beg . ,end) regions)
         (set-marker beg nil)
         (set-marker end nil)))
      (_
       (pop regions)))))

(defun conn--point-iterator (points &optional reverse)
  (setq points (mapcar (lambda (pt)
                         (if (markerp pt)
                             pt
                           (conn--create-marker pt)))
                       (if reverse (nreverse points) points)))
  (lambda (state)
    (pcase state
      (:finalize
       (dolist (pt points)
         (set-marker pt nil)))
      (_
       (when-let ((pt (pop points)))
         (cons pt pt))))))

(defun conn--dot-iterator (dots &optional reverse)
  (when reverse (setq dots (reverse dots)))
  (dolist (dot dots)
    (overlay-put dot 'evaporate nil))
  (lambda (state)
    (pcase state
      (:finalize
       (dolist (dot dots)
         (overlay-put dot 'evaporate t)))
      (_
       (pop dots)))))

(defun conn--dispatch-save-buffer-state (iterator)
  (let (dispatch-undo-handles
        dispatch-saved-excursions
        dispatch-saved-restrictions)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,buffer . ,handle) dispatch-undo-handles)
           (if conn-dispatch-error
               (cancel-change-group handle)
             (accept-change-group handle)
             (undo-amalgamate-change-group handle))
           (with-current-buffer buffer
             (pcase-let ((`(,pt . ,saved)
                          (alist-get buffer dispatch-saved-excursions))
                         (`(,beg . ,end)
                          (alist-get buffer dispatch-saved-restrictions)))
               (widen)
               (narrow-to-region beg end)
               (goto-char pt)
               (set-marker pt nil)
               (save-mark-and-excursion--restore saved))
             (run-hooks conn-macro-dispatch-buffer-end-hook))))
        (_
         (let* ((ret (funcall iterator state))
                (buffer (and (consp ret)
                             (marker-buffer (car ret)))))
           (when (and buffer (not (eq buffer (current-buffer))))
             (pop-to-buffer buffer)
             (deactivate-mark t)
             (unless (eq buffer (window-buffer (selected-window)))
               (error "Could not pop to buffer %s" buffer)))
           (if-let ((restriction (alist-get (current-buffer) dispatch-saved-restrictions)))
               (progn
                 (widen)
                 (narrow-to-region (car restriction) (cdr restriction)))
             (setf (alist-get (current-buffer) dispatch-saved-restrictions)
                   (cons (point-min-marker)
                         (point-max-marker))))
           (unless (alist-get (current-buffer) dispatch-saved-excursions)
             (setf (alist-get (current-buffer) dispatch-saved-excursions)
                   (cons (point-marker) (save-mark-and-excursion--save))))
           (unless (alist-get (current-buffer) dispatch-undo-handles)
             (activate-change-group
              (setf (alist-get (current-buffer) dispatch-undo-handles)
                    (prepare-change-group)))
             (run-hooks 'conn-macro-dispatch-buffer-start-hook))
           ret))))))

(defun conn--dispatch-change-region (iterator)
  (lambda (state)
    (let ((ret (funcall iterator state)))
      (when (and (not (eq state :finalize))
                 (consp ret))
        (delete-region (car ret) (cdr ret)))
      ret)))

(defun conn--dispatch-with-state (iterator transition)
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
          (unless (alist-get (current-buffer) buffer-states)
            (setf (alist-get (current-buffer) buffer-states)
                  (list conn-current-state conn-previous-state)))
          (funcall transition)))
        ret))))

(defun conn--dispatch-relocate-dots (iterator)
  (let (primed new-dots old-dots)
    (lambda (state)
      (pcase state
        (:finalize
         (pcase-dolist (`(,buffer . ,dots)
                        (if conn-dispatch-error old-dots new-dots))
           (with-current-buffer buffer
             (apply 'conn--create-dots dots)))
         (dolist (list (list new-dots old-dots))
           (pcase-dolist (`(,_ . ,dots) list)
             (pcase-dolist (`(,beg . ,end) dots)
               (set-marker beg nil)
               (set-marker end nil))))
         (funcall iterator state))
        (_
         (if primed
             (push (cons (conn--create-marker (region-beginning)
                                              (current-buffer))
                         (conn--create-marker (region-end)
                                              (current-buffer)))
                   (alist-get (current-buffer) new-dots))
           (setq primed t))
         (pcase (funcall iterator state)
           ((and (pred conn-dotp) dot)
            (let* ((buffer (overlay-buffer dot))
                   (beg (conn--create-marker (overlay-start dot) buffer))
                   (end (conn--create-marker (overlay-end dot) buffer)))
              (conn--delete-dot dot)
              (push (cons beg end) (alist-get buffer old-dots))
              (cons (copy-marker beg) (copy-marker end))))
           (ret ret)))))))

(defun conn--dispatch-stationary-dots (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and (pred conn-dotp) dot)
       (let ((beg (conn--create-marker (overlay-start dot)
                                       (overlay-buffer dot)))
             (end (conn--create-marker (overlay-end dot)
                                       (overlay-buffer dot))))
         (cons beg end)))
      (ret ret))))

(defun conn--dispatch-remove-dots (iterator)
  (let (old-dots)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (when conn-dispatch-error
           (pcase-dolist (`(,buffer . ,dots) old-dots)
             (with-current-buffer buffer
               (apply 'conn--create-dots dots))))
         (pcase-dolist (`(,_ . ,dots) old-dots)
           (pcase-dolist (`(,beg . ,end) dots)
             (set-marker beg nil)
             (set-marker end nil))))
        (_
         (pcase (funcall iterator state)
           ((and (pred conn-dotp) dot)
            (let* ((buffer (overlay-buffer dot))
                   (beg (conn--create-marker (overlay-start dot) buffer))
                   (end (conn--create-marker (overlay-end dot) buffer)))
              (conn--delete-dot dot)
              (push (cons (copy-marker beg)
                          (copy-marker end))
                    (alist-get buffer old-dots))
              (cons beg end)))
           (ret ret)))))))

(defun conn--dispatch-at-end (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      (`(,beg . ,end) (cons end beg))
      (ret ret))))

(defun conn--dispatch-skip-empty (iterator)
  (lambda (state)
    (let ((ret (funcall iterator state)))
      (unless (eq state :finalize)
        (while (and ret (= (car ret) (cdr ret)))
          (setq ret (funcall iterator state))))
      ret)))

(defun conn--pulse-on-record (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,end)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-region beg end 'conn-pulse-face)
       ret)
      (ret ret))))

(defun conn--pulse-line-on-record (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,_)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-one-line beg 'conn-pulse-face)
       ret)
      (ret ret))))

(defmacro conn--define-dispatcher (name arglist &rest body)
  "Define a macro dispatcher.
The iterator must be the first argument in ARGLIST.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (doc-string 3) (indent 2))
  (let ((iterator (car arglist))
        (sym (make-symbol "loop-function-symbol"))
        (docstring (if (stringp (car body)) (pop body) "")))
    `(defun ,name ,arglist
       ,docstring
       (save-window-excursion
         (let* ((undo-outer-limit nil)
                (undo-limit most-positive-fixnum)
                (undo-strong-limit most-positive-fixnum)
                (conn-macro-dispatch-p t)
                (conn-dispatch-error nil)
                (,sym (make-symbol "kmacro-loop-function"))
                (,iterator (lambda (&optional state)
                             (pcase (funcall ,iterator (or state :loop))
                               (`(,beg . ,end)
                                (goto-char beg)
                                (conn--push-ephemeral-mark end)
                                (when (markerp beg) (set-marker beg nil))
                                (when (markerp end) (set-marker end nil))
                                (and (run-hook-with-args-until-failure
                                      'conn-macro-dispatch-iterator-hook)
                                     t))))))
           (run-hook-wrapped 'conn-macro-dispatch-start-hook
                             (lambda (hook)
                               (ignore-errors (funcall hook))))
           (fset ,sym ,iterator)
           (advice-add 'kmacro-loop-setup-function :before-while ,sym)
           (unwind-protect
               (condition-case err
                   ,(macroexp-progn body)
                 (t
                  (setq conn-dispatch-error err)
                  (signal (car err) (cdr err))))
             (advice-remove 'kmacro-loop-setup-function ,sym)
             (funcall ,iterator :finalize)
             (run-hook-wrapped 'conn-macro-dispatch-end-hook
                               (lambda (hook)
                                 (ignore-errors (funcall hook))))))))))

(conn--define-dispatcher conn--macro-dispatch (iterator &optional macro)
  (pcase-exhaustive macro
    ((pred kmacro-p)
     (funcall macro 0))
    ((or (pred stringp) (pred vectorp))
     (kmacro-call-macro 0 nil nil macro))
    ('nil
     (when (funcall iterator :record)
       (kmacro-start-macro nil)
       (unwind-protect
           (recursive-edit)
         (if (not defining-kbd-macro)
             (user-error "Not defining keyboard macro")
           (kmacro-end-macro 0)))))))

(conn--define-dispatcher conn--macro-dispatch-append (iterator &optional dont-exec)
  (when (funcall iterator :record)
    (kmacro-start-macro (if dont-exec '(16) '(4)))
    (unwind-protect
        (recursive-edit)
      (when (not defining-kbd-macro)
        (user-error "Not defining keyboard macro"))
      (kmacro-end-macro 0))))

(conn--define-dispatcher conn--macro-dispatch-step-edit (iterator)
  (when (funcall iterator :record)
    (let ((sym (make-symbol "kbd-terminate-hook"))
          apply)
      (fset sym (lambda () (setq apply kmacro-step-edit-replace)))
      (add-hook 'kbd-macro-termination-hook sym)
      (unwind-protect
          (kmacro-step-edit-macro)
        (remove-hook 'kbd-macro-termination-hook sym))
      (unless apply
        (user-error "Keyboard macro edit aborted")))
    (kmacro-call-macro 0)))


;;;; Dots

;;;;; Dot Functions

(defun conn--propertize-dot-candidates (dots)
  (mapcar
   (lambda (dot)
     (with-current-buffer (overlay-buffer dot)
       (let* ((beg (overlay-start dot))
              (end (overlay-end dot))
              (str (substring (buffer-substring beg end)
                              0 (min 120 (- end beg)))))
         (add-text-properties 0 1 `(conn-dot-cand ,dot) str)
         (cons str dot))))
   dots))

(defun conn--completing-read-dot (dots)
  (let ((table (conn--propertize-dot-candidates dots)))
    (alist-get (completing-read "Dots: " table nil t)
               table nil nil 'string=)))

(defun conn--text-property-to-dots ()
  (goto-char (point-min))
  (let (dots match)
    (while (setq match (text-property-search-forward 'conn-dot-text))
      (push (cons (prop-match-beginning match)
                  (prop-match-end match))
            dots))
    (apply #'conn--create-dots dots))
  (remove-text-properties (point-min) (point-max) '(conn-dot-text nil)))

(defun conn--dot-to-text-property (dot)
  (let ((beg (overlay-start dot))
        (end (overlay-end dot)))
    (conn--delete-dot dot)
    (put-text-property beg end 'conn-dot-text t)))

(defmacro conn--with-dots-as-text-properties (dots &rest body)
  (declare (indent 1))
  `(unwind-protect
       (progn
         (mapc 'conn--dot-to-text-property ,(ensure-list dots))
         ,(macroexp-progn body))
     (conn--text-property-to-dots)))

(defun conn--sorted-overlays (typep &optional sort-predicate start end buffer)
  "Get all dots between START and END sorted by starting position."
  (unless sort-predicate (setq sort-predicate #'<))
  (let ((overlays (conn--all-overlays typep start end buffer)))
    (pcase sort-predicate
      ('< overlays)
      ('> (nreverse overlays))
      (_ (sort overlays sort-predicate)))))

(defun conn--clear-overlays ()
  "Delete all conn overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (conn--all-overlays
           (lambda (ov)
             (memq (overlay-get ov 'category)
                   '(conn--dot conn--mark-cursor)))))))

(defun conn--dot-after-change-function (&rest _)
  (setq conn--dot-undo-ring nil))

(defun conn--dot-post-command ()
  (when conn--dot-this-undo
    (setq conn--dot-undone nil)
    (push conn--dot-this-undo conn--dot-undo-ring)
    (when (> (length conn--dot-undo-ring)
             conn-dot-undo-ring-max)
      (setq conn--dot-undo-ring
            (seq-take conn--dot-undo-ring conn-dot-undo-ring-max)))
    (setq conn--dot-this-undo nil)))

(defun conn--dot-before-point (point)
  (unless (= point (point-min))
    (seq-find #'conn-dotp (overlays-in (1- point) point))))

(defun conn--dot-after-point (point)
  (unless (= point (point-max))
    (seq-find #'conn-dotp (overlays-in point (1+ point)))))

(defun conn--for-each-dot (func &optional sort-predicate start end)
  "Apply FUNC to each dot.
Optionally between START and END and sorted by SORT-PREDICATE."
  (when-let ((dots (if sort-predicate
                       (conn--sorted-overlays #'conn-dotp sort-predicate start end)
                     (conn--all-overlays #'conn-dotp start end))))
    (mapc func dots)))

(defun conn--move-dot (dot start end)
  (let ((old-start (overlay-start dot))
        (old-end (overlay-end dot)))
    (move-overlay dot start end)
    (unless (or conn--dot-undoing
                conn-macro-dispatch-p)
      (push `(move (,start . ,end) . (,old-start . ,old-end))
            conn--dot-this-undo))))

(defun conn--delete-dot (dot)
  (unless (or conn--dot-undoing conn-macro-dispatch-p)
    (push `(delete ,(overlay-start dot) . ,(overlay-end dot))
          conn--dot-this-undo))
  (overlay-put dot 'dot nil)
  (delete-overlay dot))

(defun conn--create-dots (&rest bounds)
  (pcase-dolist (`(,start . ,end) bounds)
    (with-current-buffer (if (markerp start)
                             (marker-buffer start)
                           (current-buffer))
      (let* ((overlaps (conn--all-overlays #'conn-dotp start end))
             (start (apply #'min start (mapcar #'overlay-start overlaps)))
             (end (apply #'max end (mapcar #'overlay-end overlaps)))
             (overlay (make-overlay start end nil nil t)))
        (mapc #'conn--delete-dot overlaps)
        (overlay-put overlay 'category 'conn--dot)
        (unless (or conn--dot-undoing
                    conn-macro-dispatch-p)
          (push `(create ,start . ,end) conn--dot-this-undo))))))

(defun conn--remove-dots (&optional start end)
  (mapc #'conn--delete-dot (conn--all-overlays #'conn-dotp start end)))

(defun conn--all-overlays (predicate &optional start end buffer)
  "Get all overlays between START and END satisfying PREDICATE."
  (with-current-buffer (or buffer (current-buffer))
    (seq-filter predicate
                (overlays-in (or start (conn--beginning-of-region-or-restriction))
                             (or end   (conn--end-of-region-or-restriction))))))

(defun conn-dotp (overlay)
  "Return t if OVERLAY is a dot."
  (when (overlayp overlay)
    (eq (overlay-get overlay 'category) 'conn--dot)))

(defun conn--clear-dots-in-buffers (buffers)
  "Delete all dots in BUFFERS."
  (dolist (buf buffers)
    (with-current-buffer buf
      (save-restriction
        (widen)
        (conn--remove-dots)))))

(defun conn--dots-active-p (&optional buffer)
  "Return t if there are any dots in BUFFER; obeys current restriction.

If BUFFER is nil use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let (result)
      (save-excursion
        (goto-char (point-min))
        (goto-char (next-overlay-change (point)))
        (while (and (not (setq result (conn--all-overlays
                                       #'conn-dotp
                                       (max (point-min) (1- (point)))
                                       (min (point-max) (1+ (point))))))
                    (/= (point) (point-max)))
          (goto-char (next-overlay-change (point))))
        result))))


;;;; Advice

(defun conn--define-key-advice (keymap key &rest _)
  (when (and (memq keymap (conn--without-conn-maps (current-active-maps)))
             (member (if (stringp key) (key-parse key) key)
                     (mapcar #'symbol-value conn--aux-bindings)))
    (setq conn--aux-update-flag t)))

(defun conn--pop-to-mark-command-ad (&rest _)
  (unless (or (null (mark t))
              (= (point) (mark)))
    (add-to-history 'conn--mark-unpop-ring
                    (conn--create-marker (mark t))
                    mark-ring-max
                    'keep-duplicates)))

(defun conn--push-mark-ad (fn &rest args)
  (if conn--ephemeral-mark
      (let ((mark-ring nil))
        (apply fn args)
        (when (car mark-ring)
          (set-marker (car mark-ring) nil)))
    (apply fn args))
  (setq conn--ephemeral-mark nil))

(defun conn--save-ephemeral-mark-ad (&rest _)
  (push conn--ephemeral-mark conn--saved-ephemeral-marks))

(defun conn--restore-ephemeral-mark-ad (&rest _)
  (setq-local conn--ephemeral-mark (pop conn--saved-ephemeral-marks)))

(defun conn--setup-advice ()
  (if conn-mode
      (progn
        (advice-add 'define-key :after #'conn--define-key-advice)
        (advice-add 'push-mark :around #'conn--push-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad)
        (advice-add 'pop-to-mark-command :before #'conn--pop-to-mark-command-ad))
    (advice-remove 'define-key #'conn--define-key-advice)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)
    (advice-remove 'pop-to-mark-command 'conn--pop-to-mark-command-ad)))


;;;; State Functionality

;;;;; Remapping Functions

(defun conn--modes-mark-map ()
  (setq conn--local-mark-thing-map (copy-keymap conn-mark-thing-map))
  (let (selectors)
    (dolist (mode local-minor-modes)
      (setq selectors (nconc (get mode :conn-mode-things) selectors)))
    (dolist (mode (derived-mode-all-parents major-mode))
      (setq selectors (nconc (get mode :conn-mode-things) selectors)))
    (when selectors
      (setq selectors (nreverse selectors))
      (pcase-dolist (`(,binding . ,command) selectors)
        (keymap-set conn--local-mark-thing-map binding command))))
  conn--local-mark-thing-map)

(defun conn--generate-aux-map (keymaps)
  (let ((aux-map (setf (alist-get conn-current-state conn--aux-maps)
                       (make-sparse-keymap)))
        (state-map (list (alist-get conn-current-state conn--state-maps))))
    (conn--without-conn-maps
      (dolist (sentinal conn--aux-bindings)
        (when-let ((def (lookup-key keymaps (symbol-value sentinal) t)))
          (dolist (key (where-is-internal sentinal state-map nil t))
            (define-key aux-map key def)))))
    (let ((mark-map (conn--modes-mark-map)))
      (dolist (key (where-is-internal 'conn-mark-thing-map state-map nil t t))
        (define-key aux-map key mark-map)))
    aux-map))

(defun conn--update-aux-map (&optional force)
  (when (and conn-local-mode
             conn-current-state
             (not conn-emacs-state))
    (let ((active (conn--without-conn-maps (current-active-maps)))
          (current-remappings (mapcar #'symbol-value conn--aux-bindings)))
      (cond
       ((or conn--aux-update-flag
            (not (equal conn--last-remapping current-remappings))
            force)
        (let ((aux-map (conn--generate-aux-map active))
              (key (cons conn-current-state active)))
          (setf (alist-get conn-current-state conn--aux-maps) aux-map
                conn--aux-map-history (list (cons key aux-map)))))
       (t
        (let* ((key (cons conn-current-state active))
               (aux-map (or (alist-get key conn--aux-map-history nil nil #'equal)
                            (setf (alist-get key conn--aux-map-history nil nil #'equal)
                                  (conn--generate-aux-map active)))))
          (setf (alist-get conn-current-state conn--aux-maps) aux-map
                conn--aux-map-history (seq-take conn--aux-map-history
                                                conn--aux-map-history-size)))))
      (setq conn--aux-update-flag nil
            conn--last-remapping current-remappings))))

(defmacro conn-define-remapping-command (name from-keys &optional aux-map-omit)
  "Define a command NAME that remaps to FROM-KEYS.
Placing NAME in a keymap will cause conn to remap it to the
result of FROM-KEYS.  For example conn uses this to map C-c,
C-x, M-s and M-g into various state maps."
  `(progn
     (defcustom ,name
       (key-parse ,from-keys)
       ,(string-fill
         (conn--stringify
          "Key sequence for `" name "' to remap.\n"
          "Set this variable to change `" name "''s remapping.  "
          "The key sequence must satisfy `key-valid-p'.")
         70)
       :type 'string
       :group 'conn-key-remappings)

     (defun ,name (&optional interactive-p)
       ,(string-fill
         (conn--stringify
          "Conn remapping command.  "
          "Conn will remap this command to the value of `" name "'.  "
          "If this function is called interactively it will `user-error'.  "
          "If called from Emacs lisp it will `call-interactively' "
          "the binding of the key sequence in `" name "'.")
         70)
       (interactive "p")
       (pcase (conn--without-conn-maps (key-binding ,name t))
         ((and (pred commandp) cmd)
          (if interactive-p (call-interactively cmd) cmd))
         (_ (error "Key not bound to a command %s." ,name))))

     ,(unless aux-map-omit
        `(cl-pushnew ',name conn--aux-bindings))))

(conn-define-remapping-command conn-C-x-keys                "C-x")
(conn-define-remapping-command conn-C-c-keys                "C-c")
(conn-define-remapping-command conn-M-s-keys                "M-s")
(conn-define-remapping-command conn-M-g-keys                "M-g")
(conn-define-remapping-command conn-C-x-4-keys              "C-x 4")
(conn-define-remapping-command conn-C-x-5-keys              "C-x 5")
(conn-define-remapping-command conn-C-x-t-keys              "C-x t")
(conn-define-remapping-command conn-delete-char-keys        "C-d")
(conn-define-remapping-command conn-yank-keys               "C-y")
(conn-define-remapping-command conn-kill-region-keys        "C-w")
(conn-define-remapping-command conn-backward-delete-keys    "DEL")
(conn-define-remapping-command conn-delete-region-keys      "C-S-w" t)
(conn-define-remapping-command conn-forward-sexp-keys       "C-M-f")
(conn-define-remapping-command conn-backward-sexp-keys      "C-M-b")
(conn-define-remapping-command conn-forward-word-keys       "M-f")
(conn-define-remapping-command conn-backward-word-keys      "M-b")
(conn-define-remapping-command conn-forward-paragraph-keys  "M-}")
(conn-define-remapping-command conn-backward-paragraph-keys "M-{")
(conn-define-remapping-command conn-forward-sentence-keys   "M-e")
(conn-define-remapping-command conn-backward-sentence-keys  "M-a")
(conn-define-remapping-command conn-beginning-of-defun-keys "C-M-a")
(conn-define-remapping-command conn-end-of-defun-keys       "C-M-e")
(conn-define-remapping-command conn-next-line-keys          "C-n")
(conn-define-remapping-command conn-previous-line-keys      "C-p")

;;;;; Per State Buffer Colors

(defvar conn-buffer-colors)

(defun conn--buffer-color-setup ()
  (if conn-buffer-colors
      (progn
        (buffer-face-mode 1)
        (if-let ((face (get conn-current-state :conn-buffer-face)))
            (buffer-face-set face)
          (buffer-face-set 'default)))
    (buffer-face-set 'default)
    (buffer-face-mode -1)))

(conn-define-extension conn-buffer-colors
  "Buffer background face for states."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (conn--buffer-color-setup)))
  (if conn-buffer-colors
      (add-hook 'conn-local-mode #'conn--buffer-color-setup)
    (remove-hook 'conn-local-mode #'conn--buffer-color-setup)))

;;;;; Per State Cursor Colors

(defvar conn--default-cursor-color)

(defun conn--cursor-color-setup (&rest _)
  (let ((state (buffer-local-value 'conn-current-state
                                   (window-buffer (selected-window)))))
    (set-frame-parameter
     nil 'cursor-color
     (or (ignore-errors (face-background (get state :conn-cursor-face)))
         conn--default-cursor-color))))

(conn-define-extension conn-cursor-colors
  "Cursor background face for states."
  (if conn-cursor-colors
      (progn
        (setq conn--default-cursor-color (frame-parameter nil 'cursor-color))
        (conn--cursor-color-setup)
        (add-hook 'conn-transition-hook 'conn--cursor-color-setup)
        (add-hook 'window-state-change-functions 'conn--cursor-color-setup))
    (remove-hook 'conn-transition-hook 'conn--cursor-color-setup)
    (remove-hook 'window-state-change-functions 'conn--cursor-color-setup)
    (when (boundp 'conn--default-cursor-color)
      (modify-all-frames-parameters
       `((cursor-color . ,conn--default-cursor-color))))))

;;;;; Conn-Define-State Macro

(defun conn--setup-major-mode-maps ()
  (setq conn--major-mode-maps nil)
  (if (get major-mode :conn-inhibit-inherit-maps)
      (pcase-dolist (`(,state . ,maps) conn--mode-maps)
        (let ((map (or (alist-get major-mode maps)
                       (setf (alist-get major-mode maps)
                             (make-sparse-keymap)))))
          (push (cons state map) conn--major-mode-maps)))
    (let ((mmodes (reverse (derived-mode-all-parents major-mode))))
      (pcase-dolist (`(,state . ,maps) conn--mode-maps)
        (dolist (mode mmodes)
          (let ((map (or (alist-get mode maps)
                         (setf (alist-get mode maps)
                               (make-sparse-keymap)))))
            (push (cons state map) conn--major-mode-maps)))))))

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

(defun conn-input-method-overriding-mode (mode &rest hooks)
  "Make a MODE ignore `conn-mode' input method supression.
If HOOKS are not specified checks are performed in MODE-hook to toggle
the input method.  If HOOKS are specified checks are performed in those
hooks instead."
  (let ((hooks (or hooks (list (conn--symbolicate mode "-hook")))))
    (add-to-list 'conn-input-method-overriding-modes (cons mode hooks))))

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property.
Also enable input methods when any `conn-input-method-overriding-mode' is on."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (if (seq-find (pcase-lambda (`(,mode . _))
                    (symbol-value mode))
                  conn-input-method-overriding-modes)
        (when (and conn--input-method (not current-input-method))
          (activate-input-method conn--input-method))
      (pcase (get conn-current-state :conn-suppress-input-method)
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
                                     ,(purecopy "Current input method: ")
                                     conn--input-method
                                     ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
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
  (or (alist-get (current-buffer) conn-buffer-default-state-alist
                 nil nil #'buffer-match-p)
      (conn--derived-mode-property :conn-default-state buffer)
      conn-default-state))

(defun set-default-conn-state (modes-or-buffers state)
  "Set default STATE for each MODES-OR-BUFFERS.
Modes are symbols tested against `major-mode'.
Buffers are strings matched using `buffer-match-p'."
  (dolist (var (ensure-list modes-or-buffers))
    (cl-etypecase var
      (symbol (put var :conn-default-state state))
      (string (setf (alist-get var conn-buffer-default-state-alist
                               nil nil #'equal)
                    state)))))

(defmacro conn-define-state (name doc &rest body)
  "Define a conn state NAME.
Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.

:INDICATOR is a mode-line construct that will be displayed on the left
of the mode-line in state NAME.  Indicator is only displayed when
`conn-mode-line-indicator-mode' is non-nil.

:LIGHTER-FACE is the face for the conn mode-line lighter in NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in
NAME.

:KEYMAP is a keymap for the state.

:CURSOR is the `cursor-type' for NAME.

:CURSOR-FACE is the face for the cursor in NAME.  Only the the background
color of the face is used.  Only has an effect when `conn-cursor-colors'
is enabled.

:TRANSITIONS is a list of transition key bindings to be bound in NAME's
transition map.  It is of the form ((KEY . TRANSITION-FUNCTION) ...).

:EPHEMERAL-MARKS if non-nil thing movement commands will push ephemeral
marks while in state NAME.

:BUFFER-FACE is the default face for the buffer while in state NAME.
only has an effect when `conn-buffer-colors' is enabled.

BODY contains code to be executed each time the state is enabled or
disabled.

\(fn NAME DOC &key CURSOR LIGHTER-FACE SUPPRESS-INPUT-METHOD KEYMAP TRANSITIONS INDICATOR EPHEMERAL-MARKS BUFFER-FACE &rest BODY)"
  (declare (indent defun))
  (let* ((map-name (conn--symbolicate name "-map"))
         (transition-map-name (conn--symbolicate name "-transition-map"))
         (cursor-name (conn--symbolicate name "-cursor-type"))
         (lighter-face-name (conn--symbolicate name "-lighter-face"))
         (indicator-name (conn--symbolicate name "-indicator"))
         (buffer-face-name (conn--symbolicate name "-buffer-face"))
         (cursor-face-name (conn--symbolicate name "-cursor-face"))
         (enter (gensym "enter"))
         keyw
         lighter-face
         suppress-input-method
         ephemeral-marks
         (keymap '(make-sparse-keymap))
         cursor
         (transitions '(make-sparse-keymap))
         (indicator "")
         buffer-face
         cursor-face)
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase-exhaustive keyw
        (:cursor (setq cursor (pop body)))
        (:lighter-face (setq lighter-face (pop body)))
        (:suppress-input-method (setq suppress-input-method (pop body)))
        (:keymap (setq keymap (pop body)))
        (:transitions (setq transitions (pop body)))
        (:indicator (setq indicator (pop body)))
        (:ephemeral-marks (setq ephemeral-marks (pop body)))
        (:buffer-face (setq buffer-face (pop body)))
        (:cursor-face (setq cursor-face (pop body)))))
    `(progn
       (defvar-local ,name nil
         ,(conn--stringify "Non-nil when `" name "' is active."))

       (defvar ,map-name ,keymap
         ,(conn--stringify "Keymap active in `" name "'."))

       (defvar ,transition-map-name ,transitions
         ,(string-fill (conn--stringify
                        "Keymap for commands that transition from `"
                        name "' to other states.")
                       70))

       (defface ,lighter-face-name
         ',lighter-face
         ,(conn--stringify "Face for `" name "' mode line indicator.")
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
                       integer)
                 (const :tag "None " nil))
         :group 'conn-states)

       (defcustom ,indicator-name
         '(:propertize ,indicator face ,lighter-face-name)
         ,(conn--stringify "`" name "' mode line indicator string.")
         :type '(list string symbol)
         :risky t
         :group 'conn-states)

       (defface ,cursor-face-name
         ',cursor-face
         ,(conn--stringify "Cursor face in `" name "'.\n"
                           "Note only the background is used.")
         :group 'conn-states)

       (defface ,buffer-face-name
         ',buffer-face
         ,(conn--stringify "Face for `" name "' buffers.")
         :group 'conn-states)

       ,(when ephemeral-marks
          `(cl-pushnew ',name conn-ephemeral-mark-states))

       (put ',name :conn-suppress-input-method ,suppress-input-method)
       (put ',name :conn-cursor-type ',cursor-name)
       (put ',name :conn-cursor-face ',cursor-face-name)
       (put ',name :conn-indicator ',indicator-name)
       (put ',name :conn-buffer-face ',buffer-face-name)
       (put ',name :conn-lighter-face ',lighter-face-name)

       (cl-pushnew ',name conn-states)
       (push (cons ',name ,map-name) conn--state-maps)
       (push (cons ',name ,transition-map-name) conn--transition-maps)

       (defun ,name ()
         ,doc
         (interactive)
         (when conn-current-state
           (funcall (get conn-current-state :conn-transition-fn) :exit))
         (funcall (get ',name :conn-transition-fn) :enter))

       (put ',name :conn-transition-fn
            (lambda (,enter)
              (when (eq ,enter :exit) (setq ,enter nil))
              (unless (and ,enter (eq conn-current-state ',name))
                (if (not ,enter)
                    (progn
                      (setq ,name nil)
                      (setq conn-current-state nil)
                      (setq conn-previous-state ',name))
                  (setq conn-current-state ',name)
                  (setq ,name t)
                  (when conn-lighter
                    (setq-local conn-lighter
                                (propertize conn-lighter
                                            'face ',lighter-face-name)))
                  (when conn-buffer-colors
                    (buffer-face-set ',buffer-face-name))
                  (conn--activate-input-method)
                  (setq conn--local-mode-maps (alist-get conn-current-state
                                                         conn--mode-maps))
                  (conn--update-cursor)
                  (conn--update-mode-line-indicator)
                  (when (not executing-kbd-macro)
                    (force-mode-line-update)))
                ,@body
                (run-hooks 'conn-transition-hook)))))))


;;;; State Definitions

(defvar-keymap conn-common-map
  :doc "Keymap for bindings shared between dot and conn states.")

(conn-define-state conn-emacs-state
  "Activate `conn-emacs-state' in the current buffer.
A `conn-mode' state for inserting text.  By default `conn-emacs-state' does not
bind anything except transition commands.

See `conn-emacs-state-transition-map' for keybindings to enter other states
from Emacs state.  See `conn-emacs-state-map' for commands bound by Emacs state."
  :lighter-face ((default              (:inherit mode-line :background "#cae1ff"))
                 (((background light)) (:inherit mode-line :background "#cae1ff"))
                 (((background dark))  (:inherit mode-line :background "#49739f")))
  :cursor-face ((default               (:background "#00517d"))
                 (((background light)) (:background "#00517d"))
                 (((background dark))  (:background "#b6d6e7")))
  :buffer-face ((t :inherit default))
  :indicator " E "
  :cursor box
  :ephemeral-marks nil)

(conn-define-state conn-state
  "Activate `conn-state' in the current buffer.
A `conn-mode' state for editing text.

See `conn-state-transition-map' for keybindings to enter other states
from conn state.  See `conn-state-map' for commands bound by conn state."
  :lighter-face ((default              (:inherit mode-line :background "#f3bdbd"))
                 (((background light)) (:inherit mode-line :background "#f3bdbd"))
                 (((background dark))  (:inherit mode-line :background "#8c3c3c")))
  :cursor-face ((default               (:background "#7d0002"))
                 (((background light)) (:background "#7d0002"))
                 (((background dark))  (:background "#eba4a4")))
  :buffer-face ((t :inherit default :background "#f7eee1"))
  :suppress-input-method t
  :indicator " C "
  :ephemeral-marks t
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (define-keymap
                 "f"       'conn-emacs-state
                 "\\"      'conn-dispatch-prefix
                 "t"       'conn-change
                 "F i"     'conn-emacs-state-open-line-above
                 "F k"     'conn-emacs-state-open-line
                 "F l"     'conn-emacs-state-eol
                 "F j"     'conn-emacs-state-bol
                 "F o"     'conn-emacs-state-overwrite
                 "F u"     'conn-emacs-state-overwrite-binary
                 "M-TAB"   'conn-emacs-state-and-complete
                 "M-<tab>" 'conn-emacs-state-and-complete))
(set-default-conn-state '(prog-mode text-mode conf-mode) 'conn-state)

(conn-define-state conn-dot-state
  "Activate `conn-dot-state' in the current buffer.
A `conn-mode' state for dispatching keyboard macros on buffer regions.

See `conn-dot-state-transition-map' for keybindings to enter other states
from dot state.  See `conn-dot-state-map' for commands bound by dot state."
  :lighter-face ((default              (:inherit mode-line :background "#c3eac9"))
                 (((background light)) (:inherit mode-line :background "#c3eac9"))
                 (((background dark))  (:inherit mode-line :background "#4f7555")))
  :buffer-face ((t :inherit default :background "#f6fff9"))
  :cursor-face ((default               (:background "#267d00"))
                 (((background light)) (:background "#267d00"))
                 (((background dark))  (:background "#b2e5a6")))
  :suppress-input-method t
  :indicator " D "
  :ephemeral-marks t
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (define-keymap
                 "\\" 'conn-dispatch-prefix
                 "f"  'conn-emacs-state
                 "Q"  'conn-dot-quit)
  (if conn-dot-state
      (progn
        (setq conn--dot-undo-ring nil)
        (conn--for-each-dot
         (lambda (dot)
           (push `(create ,(overlay-start dot) . ,(overlay-end dot))
                 conn--dot-this-undo)))
        (add-hook 'post-command-hook #'conn--dot-post-command t t)
        (add-hook 'after-change-functions #'conn--dot-after-change-function t t))
    (setq conn--dot-undo-ring nil)
    (remove-hook 'after-change-functions #'conn--dot-after-change-function t)
    (remove-hook 'post-command-hook #'conn--dot-post-command t)))

(conn-define-state conn-org-tree-edit-state
  "Activate `conn-org-tree-edit-state' in the current buffer.
A `conn-mode' state for structural editing of `org-mode' buffers.

See `conn-org-tree-edit-state-transition-map' for keybindings to enter
other states from org-tree-edit state.  See
`conn-org-tree-edit-state-map' for commands bound by org-tree-edit
state."
  :lighter-face ((default              (:inherit mode-line :background "#f5c5ff"))
                 (((background light)) (:inherit mode-line :background "#f5c5ff"))
                 (((background dark))  (:inherit mode-line :background "#85508c")))
  :buffer-face ((t :inherit default :background "#fff6ff"))
  :cursor-face ((default               (:background "#7d0077"))
                 (((background light)) (:background "#7d0077"))
                 (((background dark))  (:background "#f1b9ee")))
  :suppress-input-method t
  :indicator (:propertize " T " face conn-org-tree-edit-state-lighter-face)
  :keymap (define-keymap :suppress t)
  :transitions (define-keymap
                 "f"   'conn-emacs-state
                 "F i" 'conn-emacs-state-open-line-above
                 "F k" 'conn-emacs-state-open-line
                 "F l" 'conn-emacs-state-eol
                 "F j" 'conn-emacs-state-bol
                 "F o" 'conn-emacs-state-overwrite
                 "F u" 'conn-emacs-state-overwrite-binary))
(put 'conn-org-tree-edit-state :conn-hide-mark t)


;;;; Commands

;;;;; Tab Registers

(cl-defstruct (conn-tab-register (:constructor %conn--make-tab-register (cookie)))
  (cookie nil :read-only t))

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
               (gensym "conn-tab-cookie"))))))

(cl-defmethod register-val-jump-to ((val conn-tab-register) _arg)
  (when-let ((index (conn--get-tab-index-by-cookie
                     (conn-tab-register-cookie val))))
    (tab-bar-select-tab (1+ index))))

(cl-defmethod register-val-describe ((val conn-tab-register) _arg)
  (princ (format "Tab:  %s"
                 (when-let ((index (conn--get-tab-index-by-cookie
                                    (conn-tab-register-cookie val))))
                   (conn--thread <>
                       index
                     (nth <> (funcall tab-bar-tabs-function))
                     (if (eq (car <>) 'current-tab)
                         (propertize "*CURRENT TAB*" 'face 'error)
                       (alist-get 'name <>)))))))

(defun conn-tab-to-register (register)
  "Store tab in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-tab-register)))

;;;;; Dot Commands

(defun conn-transpose-region-and-dot (dot)
  "Tranpose region and DOT.
When called interactively and there are multiple dots in the current
buffers completing read DOT."
  (interactive
   (let ((dots (conn--all-overlays #'conn-dotp)))
     (cond
      ((null dots)
       (user-error "No dots active"))
      ((length= dots 1)
       dots)
      (t (list (conn--completing-read-dot dots))))))
  (let ((beg (conn--create-marker (overlay-start dot)))
        (end (overlay-end dot)))
    (save-mark-and-excursion
      (conn--with-dots-as-text-properties (list dot)
        (transpose-regions (region-beginning) (region-end) beg end)))))

(defun conn-sort-dots ()
  "Sort all dots in the current buffer by the text they contain.
Obeys `sort-case-fold'."
  (interactive)
  (let* ((sort-lists (mapcar (lambda (dot)
                               (let ((key (cons (overlay-start dot)
                                                (overlay-end dot))))
                                 (cons key key)))
                             (conn--sorted-overlays #'conn-dotp '>)))
         (old (reverse sort-lists))
         (case-fold-search sort-fold-case))
    (when sort-lists
      (save-mark-and-excursion
        (conn--with-dots-as-text-properties
            (conn--all-overlays #'conn-dotp)
          (setq sort-lists
                (sort sort-lists
                      (lambda (a b)
                        (> 0 (compare-buffer-substrings
                              nil (car (car a)) (cdr (car a))
                              nil (car (car b)) (cdr (car b)))))))
          (with-buffer-unmodified-if-unchanged
            (sort-reorder-buffer sort-lists old)))))))

(defun conn-remove-dot ()
  "Remove dot at point.
If the region is active remove all dots in region."
  (interactive)
  (if (use-region-p)
      (conn--remove-dots (region-beginning) (region-end))
    (save-mark-and-excursion
      (if-let ((dot (conn--dot-before-point (point))))
          (progn
            (conn--delete-dot dot)
            (when (called-interactively-p 'interactive)
              (message "Dot removed")))
        (when (called-interactively-p 'interactive)
          (message "No dot at point"))))))

(defun conn-remove-all-dots (&optional multi-buffer)
  "Remove all dots.

With a plain prefix argument (\\[universal-argument]), prompt for a
regular expression and remove all dots in all buffers whose name
matches the expression.

With a numerical prefix argument read buffers using `completing-read'."
  (interactive "P")
  (cond ((consp multi-buffer)
         (conn--clear-dots-in-buffers (conn-read-matching-dot-buffers)))
        (multi-buffer
         (conn--clear-dots-in-buffers (conn-read-dot-buffers)))
        (t (conn--remove-dots
            (conn--beginning-of-region-or-restriction)
            (conn--end-of-region-or-restriction))))
  (when (called-interactively-p 'interactive)
    (message "Dots removed")))

(defun conn-dot-undo ()
  "Undo last dot change.
Dot undo ring is invalidated whenever the buffer or state changes."
  (interactive)
  (unless conn--dot-undo-ring
    (user-error "Dot undo ring is empty"))
  (let ((this-undo (car (push (pop conn--dot-undo-ring) conn--dot-undone)))
        (conn--dot-undoing t))
    (dolist (action (sort this-undo
                          ;; We need to undo creation first
                          ;; to avoid possible overlaps
                          (lambda (a1 _a2)
                            (eq (car a1) 'create))))
      (pcase action
        (`(create ,beg . ,end)
         (let ((dot (or (conn--dot-after-point beg)
                        (error "Dot undo ring corrupted"))))
           (conn--delete-dot dot))
         (goto-char beg)
         (conn--push-ephemeral-mark end))
        (`(delete ,beg . ,end)
         (conn--create-dots (cons beg end))
         (goto-char beg)
         (conn--push-ephemeral-mark end))
        (`(move (,to-beg . ,_to-end) . (,from-beg . ,from-end))
         (let ((dot (or (conn--dot-after-point to-beg)
                        (error "Dot undo ring corrupted"))))
           (conn--move-dot dot from-beg from-end))
         (goto-char from-beg)
         (conn--push-ephemeral-mark from-end))))
    (when (called-interactively-p 'interactive)
      (message "Dots undone"))))

(defun conn-dot-redo ()
  "Redo last dot change.
Dot undo ring is invalidated whenever the buffer or state changes."
  (interactive)
  (unless conn--dot-undone
    (user-error "No further redo information"))
  (let ((this-redo (car (push (pop conn--dot-undone) conn--dot-undo-ring)))
        (conn--dot-undoing t))
    (dolist (action (sort this-redo
                          ;; And here we need to redo deletion first
                          (lambda (a1 _a2)
                            (eq (car a1) 'delete))))
      (pcase action
        (`(create ,beg . ,end)
         (conn--create-dots (cons beg end))
         (goto-char beg)
         (conn--push-ephemeral-mark end))
        (`(delete ,beg . ,end)
         (let ((dot (or (conn--dot-after-point beg)
                        (error "Dot undo ring corrupted"))))
           (conn--delete-dot dot))
         (goto-char beg)
         (conn--push-ephemeral-mark end))
        (`(move (,to-beg . ,to-end) . (,from-beg . ,_from-end))
         (let ((dot (or (conn--dot-after-point from-beg)
                        (error "Dot undo ring corrupted"))))
           (conn--move-dot dot to-beg to-end))
         (goto-char to-beg)
         (conn--push-ephemeral-mark to-end))))
    (when (called-interactively-p 'interactive)
      (message "Dots redone"))))

(defun conn-first-dot ()
  "Go to the end of the first dot in buffer."
  (interactive)
  (when-let ((dot (save-excursion
                    (goto-char (point-min))
                    (conn--next-dot-1)
                    (conn--dot-before-point (point)))))
    (goto-char (overlay-start dot))
    (conn--push-ephemeral-mark (overlay-end dot))))

(defun conn-last-dot ()
  "Go to the end of the last dot in buffer."
  (interactive)
  (when-let ((dot (save-excursion
                    (goto-char (point-max))
                    (conn--previous-dot-1)
                    (conn--dot-after-point (point)))))
    (goto-char (overlay-end dot))
    (conn--push-ephemeral-mark (overlay-start dot))))

(defun conn-remove-dot-backward (arg)
  "Remove nearest dot within the range `point-min' to `point'.
If region is active remove all dots in region."
  (interactive "p")
  (if (use-region-p)
      (conn--remove-dots (region-beginning) (region-end))
    (let ((dot (or (conn--dot-before-point (point))
                   (when (conn--previous-dot-1)
                     (conn--next-dot-1)))))
      (while (and (> arg 1) dot)
        (conn--delete-dot dot)
        (setq dot (or (conn--dot-before-point (point))
                      (when (conn--previous-dot-1)
                        (conn--next-dot-1)))
              arg (1- arg)))
      (when dot
        (goto-char (overlay-start dot))
        (conn--push-ephemeral-mark (overlay-end dot))
        (conn--delete-dot dot)))))

(defun conn-remove-dot-forward (arg)
  "Remove nearest dot within the range `point' to `point-max'."
  (interactive "p")
  (let ((dot (or (conn--dot-after-point (point))
                 (when (conn--next-dot-1)
                   (conn--previous-dot-1)))))
    (while (and (> arg 1) dot)
      (conn--delete-dot dot)
      (setq dot (or (conn--dot-after-point (point))
                    (when (conn--next-dot-1)
                      (conn--previous-dot-1)))
            arg (1- arg)))
    (when dot
      (goto-char (overlay-end dot))
      (conn--push-ephemeral-mark (overlay-start dot))
      (conn--delete-dot dot))))

(defun conn-dot-region (bounds)
  "Dot current region."
  (interactive (list (region-bounds)))
  (apply #'conn--create-dots bounds)
  (deactivate-mark))

(defun conn-dot-region-forward (start end &optional arg)
  "Dot region and `search-foward' for string matching region.
If ARG is non-nil repeat ARG times.
If region is already a dot `search-forward', dot, and `search-forward' again."
  (interactive (list (region-beginning)
                     (region-end)
                     (prefix-numeric-value current-prefix-arg)))
  (let ((str (buffer-substring-no-properties start end)))
    (goto-char end)
    (conn--create-dots (cons start end))
    (cl-decf arg)
    (search-forward str)
    (dotimes (_ arg)
      (conn--create-dots (cons (match-beginning 0) (match-end 0)))
      (search-forward str))
    (conn--push-ephemeral-mark (match-beginning 0)))
  (when (called-interactively-p 'interactive)
    (message "Region dotted forward")))

(defun conn-dot-region-backward (start end &optional arg)
  "Dot region and `search-backward' for string matching region.
If ARG is non-nil repeat ARG times.
If region is already a dot `search-backward', dot, and `search-backward' again."
  (interactive (list (region-beginning)
                     (region-end)
                     (prefix-numeric-value current-prefix-arg)))
  (let ((str (buffer-substring-no-properties start end)))
    (goto-char start)
    (conn--create-dots (cons start end))
    (cl-decf arg)
    (search-backward str)
    (dotimes (_ arg)
      (conn--create-dots (cons (match-beginning 0) (match-end 0)))
      (search-backward str))
    (conn--push-ephemeral-mark (match-end 0)))
  (when (called-interactively-p 'interactive)
    (message "Region dotted backward")))

(defun conn-dot-skip-forward (start end &optional arg)
  "`search-forward', skipping this region."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (let ((str (buffer-substring-no-properties start end)))
    (unless (= (point) end)
      (exchange-point-and-mark t))
    (dotimes (_ (or (and (numberp arg) arg) 1))
      (search-forward str))
    (conn--push-ephemeral-mark (match-beginning 0)))
  (when (called-interactively-p 'interactive)
    (message "Region skipped forward")))

(defun conn-dot-skip-backward (start end &optional arg)
  "`search-backward', skipping this region."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (with-demoted-errors "%s"
    (let ((str (buffer-substring-no-properties start end)))
      (unless (= (point) start)
        (exchange-point-and-mark t))
      (dotimes (_ (or (and (numberp arg) arg) 1))
        (search-backward str))
      (conn--push-ephemeral-mark (match-end 0))))
  (when (called-interactively-p 'interactive)
    (message "Region skipped backward")))

(defun conn-add-dots-matching-literal (string &optional start end refine)
  "Dot all occurrences of STRING in region from START to END.
If REFINE is non-nil only dot occurrences in dots.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (read-string "String: "
                                  (ignore-errors
                                    (list (buffer-substring-no-properties
                                           (region-beginning)
                                           (region-end)))))
                     (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)
                     current-prefix-arg))
  (conn-add-dots-matching-regexp (regexp-quote string) start end refine))

(defun conn-add-dots-matching-regexp (regexp &optional start end refine)
  "Dot things matching REGEXP in region from START to END.
If REFINE is non-nil only dot thing withing dots in
region from START to END.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (read-regexp "Regexp: "
                                  (list ""
                                        (ignore-errors
                                          (list (regexp-quote
                                                 (buffer-substring-no-properties
                                                  (region-beginning)
                                                  (region-end)))))))
                     (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)
                     current-prefix-arg))
  (setq start (or start (point-min))
        end (or end (point-max)))
  (let (new-dots)
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (cond
         ((not refine)
          (conn--create-dots (cons (match-beginning 0) (match-end 0))))
         ((conn-isearch-in-dot-p (match-beginning 0) (match-end 0))
          (push (cons (match-beginning 0) (match-end 0)) new-dots))))
      (when refine
        (conn--remove-dots start end)
        (apply #'conn--create-dots new-dots)))))

(defun conn-dot-lines (start end)
  "Dot each line in region from START to END.

When called START is `region-beginning' and END is `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (save-excursion
    (goto-char start)
    (conn--create-dots (cons (line-beginning-position)
                             (line-end-position)))
    (while (> end (progn (forward-line) (point)))
      (conn--create-dots (cons (line-beginning-position)
                               (line-end-position))))))

(defun conn-remove-dots-outside-region (start end)
  "Remove all dots outside region from START to END.

When called interactively operates within `region-bounds'."
  (interactive (list (region-beginning) (region-end)))
  (conn--for-each-dot #'conn--delete-dot nil (point-min) start)
  (conn--for-each-dot #'conn--delete-dot nil end (point-max)))

(defun conn-split-region-on-regexp (regexp start end)
  "Split region from START to END into dots on REGEXP.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (read-regexp "Regexp" nil)
                     (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)))
  (let (dots
        (search-invisible 'open))
    (save-excursion
      (goto-char start)
      (push (point) dots)
      (while (re-search-forward regexp end t)
        (push (match-beginning 0) dots)
        (push (match-end 0) dots)
        (when (= (match-beginning 0) (match-end 0))
          (forward-char)))
      (push end dots))
    (conn--remove-dots start end)
    (cl-loop for (beg end) on dots by #'cddr
             do (conn--create-dots (cons beg end)))))

(defun conn-split-dots-on-regexp (regexp start end)
  "Split all dots in region START to END on regexp.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (read-regexp "Regexp" "[[:blank:]]")
                     (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)))
  (conn--for-each-dot
   (lambda (dot)
     (let ((start (overlay-start dot))
           (end (overlay-end dot)))
       (conn--delete-dot dot)
       (conn-split-region-on-regexp regexp start end)))
   nil start end))

(defun conn--previous-dot-1 ()
  "Perform one iteration for `conn-previous-dot-end'."
  (let* ((pt (previous-overlay-change (point)))
         (ov (conn--dot-after-point pt)))
    (while (and (or (not ov)
                    (/= pt (overlay-start ov)))
                (/= pt (point-min)))
      (setq pt (previous-overlay-change pt)
            ov (conn--dot-after-point pt)))
    (if ov
        (progn (goto-char pt) ov)
      (message "No more dots")
      nil)))

(defun conn--next-dot-1 ()
  "Perform one iteration for `conn-next-dot-end'."
  (let* ((pt (next-overlay-change (point)))
         (ov (conn--dot-before-point pt)))
    (while (and (or (not ov)
                    (/= pt (overlay-end ov)))
                (/= pt (point-max)))
      (setq pt (next-overlay-change pt)
            ov (conn--dot-before-point pt)))
    (if ov
        (progn
          (goto-char pt)
          ov)
      (message "No more dots")
      nil)))

(defun conn-next-dot (arg)
  "Move point forward ARG dots."
  (interactive "p")
  (cond ((> arg 0)
         (dotimes (_ arg)
           (conn--next-dot-1)))
        ((< arg 0)
         (dotimes (_ (abs arg))
           (conn--previous-dot-1)))))
(put 'dot 'forward-op 'conn-next-dot)

(defun conn-previous-dot (arg)
  "Move point backward ARG dots."
  (interactive "p")
  (conn-next-dot (- arg)))

(defun conn-dot-point (point)
  "Insert dot at point."
  (interactive (list (point)))
  (conn--create-dots (cons point (1+ point))))

(defun conn-dot-at-click (event)
  "Insert dot at mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((start-posn (event-start event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn)))
    (with-current-buffer (window-buffer start-window)
      (conn-dot-point start-point)
      (goto-char start-point))))

(defun conn-dot-text-property (start end &optional refine)
  "Dot each region between START and END with text property PROP equal to VAL.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)
                     current-prefix-arg))
  (let* ((prop (intern (completing-read
                        "Property: "
                        (cl-loop for prop in (text-properties-at (point))
                                 by #'cddr
                                 collect prop)
                        nil t)))
         (vals (mapcar (lambda (s) (cons (message "%s" s) s))
                       (ensure-list (get-text-property (point) prop))))
         (val (alist-get (completing-read "Value: " vals) vals
                         nil nil #'string=))
         new-dots)
    (save-excursion
      (with-restriction
          start end
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward prop val t))
            (cond ((null refine)
                   (conn--create-dots (cons (prop-match-beginning match)
                                            (prop-match-end match))))
                  ((conn-isearch-in-dot-p (prop-match-beginning match)
                                          (prop-match-end match))
                   (push (cons (prop-match-beginning match)
                               (prop-match-end match))
                         new-dots)))))
        (when refine
          (conn--remove-dots start end)
          (apply #'conn--create-dots new-dots))))))

(defun conn-dot-trim-regexp (regexp start end)
  "Trim regexp from beginning and end of all dots.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (read-regexp "Regexp" "[[:blank:]]+")
                     (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)))
  (conn--for-each-dot
   (lambda (dot)
     (let ((start (overlay-start dot))
           (end (overlay-end dot)))
       (goto-char start)
       (when (looking-at regexp)
         (setq start (match-end 0)))
       (goto-char end)
       (when (looking-back regexp start t)
         (setq end (match-beginning 0)))
       (if (>= start end)
           (conn--delete-dot dot)
         (move-overlay dot start end))))
   '> start end))

(defun conn-query-remove-dots ()
  "Prompt to delete each dot.
y delete the dot.
n keeps the dot.
d deletes the remaining dots.
k keeps the remaining dots."
  (interactive)
  (save-excursion
    (catch 'keep
      (conn--for-each-dot
       (let ((message (format "%s (%s)es; (%s)o; (%s)elete rest; (%s)eep rest"
                              (propertize "Delete:" 'face 'bold)
                              (propertize "y" 'face 'help-key-binding)
                              (propertize "n" 'face 'help-key-binding)
                              (propertize "d" 'face 'help-key-binding)
                              (propertize "k" 'face 'help-key-binding)))
             rest)
         (lambda (dot)
           (goto-char (overlay-start dot))
           (if rest
               (conn--delete-dot dot)
             (while (pcase (read-char message)
                      (?y (conn--delete-dot dot) nil)
                      (?n nil)
                      (?d (conn--delete-dot dot)
                          (setq rest t)
                          nil)
                      (?k (throw 'keep nil))
                      (_ t))))))
       #'<))))

(defun conn-remove-dots-after (point)
  "Clear all dots after POINT."
  (interactive (list (point)))
  (conn--remove-dots point (point-max))
  (when (called-interactively-p 'interactive)
    (message "Dots after point removed")))

(defun conn-remove-dots-before (point)
  "Clear all dots before POINT."
  (interactive (list (point)))
  (conn--remove-dots (point-min) point)
  (when (called-interactively-p 'interactive)
    (message "Dots before point removed")))

(defun conn-dot-all-things-in-region (thing)
  "Dot all THINGs in region.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
  (unless thing (error "Unknown thing command"))
  (save-excursion (forward-thing thing))
  (save-excursion
    (with-restriction
        (region-beginning) (region-end)
      (goto-char (point-min))
      (when-let ((bounds (bounds-of-thing-at-point thing)))
        (conn--create-dots bounds))
      (let (bounds)
        (while (and (/= (point) (point-max))
                    (/= (point) (progn
                                  (forward-thing thing)
                                  (point)))
                    (setq bounds (bounds-of-thing-at-point thing)))
          (conn--create-dots bounds)
          (setq bounds (bounds-of-thing-at-point thing)))))))

(defun conn-shell-command-on-dots (command arg)
  "Run `shell-command-on-region' on each dot."
  (interactive
   (list (read-shell-command "Shell command on dots: ")
         current-prefix-arg))
  (save-mark-and-excursion
    (dolist (dot (conn--all-overlays #'conn-dotp))
      (let ((beg (overlay-start dot))
            (end (overlay-end dot)))
        (shell-command-on-region beg end
                                 command arg arg
                                 shell-command-default-error-buffer
                                 t nil)
        (conn-exchange-mark-command)
        (when (and (looking-back "\n" 1) arg)
          (delete-char 1))))))

;;;;; Isearch commands

(defun conn-isearch-in-dot-p (beg end)
  "Whether or not region from BEG to END is entirely within a dot.
Meant to be used as `isearch-filter-predicate'."
  (when-let ((ov (conn--dot-after-point beg)))
    (>= (overlay-end ov) end)))

(defun conn-isearch-not-in-dot-p (beg end)
  "Inverse of `conn-isearch-in-dot-p'."
  (not (conn-isearch-in-dot-p beg end)))

(defun conn-isearch-in-dot-toggle ()
  "Restrict isearch text within dots."
  (interactive)
  (if (advice-function-member-p #'conn-isearch-in-dot-p isearch-filter-predicate)
      (advice-remove isearch-filter-predicate #'conn-isearch-in-dot-p)
    (advice-add isearch-filter-predicate :after-while #'conn-isearch-in-dot-p
                '((isearch-message-prefix . "[DOT] "))))
  (isearch-update))

(defun conn--isearch-add-dots-1 (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (advice-function-member-p #'conn-isearch-in-dot-p
                                    isearch-filter-predicate)
      (conn--remove-dots (point-min) (point-max)))
    (apply #'conn--create-dots (conn--isearch-matches-in-buffer))))

(defun conn-isearch-add-dots ()
  "Dot all isearch matches."
  (interactive)
  (if (or (not (boundp 'multi-isearch-buffer-list))
          (not multi-isearch-buffer-list))
      (conn--isearch-add-dots-1 (current-buffer))
    (dolist (buf multi-isearch-buffer-list)
      (when buf
        (conn--isearch-add-dots-1 buf))))
  (isearch-update))

(defun conn-isearch-remove-dots (&optional partial-match)
  "Remove all dots exactly matching isearch search string.
If PARTIAL-MATCH is non-nil remove all dots containing an isearch match.
Interactively PARTIAL-MATCH is the prefix argument."
  (interactive "P")
  (save-excursion
    (dolist (dot (conn--all-overlays #'conn-dotp))
      (goto-char (overlay-start dot))
      (when (and (isearch-search-string isearch-string (overlay-end dot) t)
                 (funcall isearch-filter-predicate
                          (match-beginning 0) (match-end 0))
                 (or (not partial-match)
                     (and (= (match-beginning 0) (overlay-start dot))
                          (= (match-end 0) (overlay-end dot)))))
        (conn--delete-dot dot)))))

(defun conn-isearch-split-dots (&optional refine)
  "Split dots on isearch matches."
  (interactive "P")
  (let ((forward isearch-forward)
        dots)
    (save-excursion
      (unwind-protect
          (progn
            ;; we need to ensure we are searching forward or the
            ;; bound wont be correct for isearch-search-string
            (unless forward (isearch-repeat-forward))
            (dolist (dot (conn--all-overlays #'conn-dotp))
              (goto-char (overlay-start dot))
              (unless refine (push (point) dots))
              (while (isearch-search-string isearch-string (overlay-end dot) t)
                (when (funcall isearch-filter-predicate
                               (match-beginning 0) (match-end 0))
                  (push (match-beginning 0) dots)
                  (push (match-end 0) dots)))
              (unless refine (push (overlay-end dot) dots))))
        (unless forward (isearch-repeat-backward))))
    (conn--remove-dots)
    (cl-loop for (beg end) on dots by #'cddr
             when (/= beg end)
             do (conn--create-dots (cons beg end)))))

(defun conn-isearch-refine-dots ()
  "Clear dots and add new dots at isearch matches within previous dots."
  (interactive)
  (conn-isearch-split-dots t))

;;;;; Editing Commands

(defun conn--maybe-push-mark (ring &optional location)
  (when (and (mark t)
             (not conn--ephemeral-mark)
             (/= (or location (point)) (mark t)))
    (let ((old (nth mark-ring-max (symbol-value ring)))
          (history-delete-duplicates t))
      (add-to-history ring (copy-marker (mark-marker)) mark-ring-max)
      (when (and old (not (memq old (symbol-value ring))))
        (set-marker old nil))))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  (setq conn--ephemeral-mark nil))

(defun conn-pop-to-mark-command ()
  (interactive)
  (if (null (mark t))
      (user-error "No mark set in this buffer")
    (if (null mark-ring)
        (user-error "No marks to pop")
      (conn--maybe-push-mark 'mark-ring)
      (conn--maybe-push-mark 'conn--mark-unpop-ring (car mark-ring))
      (set-marker (car mark-ring) nil)
      (pop mark-ring)
      (goto-char (mark t)))
    (deactivate-mark)))

(defun conn-unpop-to-mark-command ()
  (interactive)
  (if (null (mark t))
      (user-error "No mark set in this buffer")
    (if (null conn--mark-unpop-ring)
        (user-error "No marks to unpop")
      (conn--maybe-push-mark 'conn--mark-unpop-ring)
      (conn--maybe-push-mark 'mark-ring (car conn--mark-unpop-ring))
      (set-marker (car conn--mark-unpop-ring) nil)
      (pop conn--mark-unpop-ring)
      (goto-char (mark t)))
    (deactivate-mark)))

(defun conn-toggle-sort-fold-case ()
  "Toggle the value of `sort-fold-case'."
  (interactive)
  (message "Sort fold case: %s"
           (setq sort-fold-case (not sort-fold-case))))

(defvar-local conn--minibuffer-initial-region nil)

(defun conn--yank-region-to-minibuffer-hook ()
  (setq conn--minibuffer-initial-region
        (with-minibuffer-selected-window
          (ignore-errors (cons (region-beginning) (region-end))))))

(defun conn-yank-region-to-minibuffer (&optional quote-function)
  "Yank region from `minibuffer-selected-window' into minibuffer."
  (interactive (list (pcase current-prefix-arg
                       ('(4) conn-completion-region-quote-function)
                       ('nil 'identity)
                       (_    'regexp-quote))))
  (insert (pcase-exhaustive conn--minibuffer-initial-region
            (`(,beg . ,end)
             (with-minibuffer-selected-window
               (funcall (or quote-function 'identity)
                        (buffer-substring-no-properties beg end))))
            (_ (user-error "No region in buffer")))))

(defun conn-query-replace-region ()
  "Run `query-replace' with the region as initial contents."
  (interactive)
  (save-mark-and-excursion
    (unless (eq (point) (region-beginning))
      (conn-exchange-mark-command))
    (minibuffer-with-setup-hook 'conn-yank-region-to-minibuffer
      (call-interactively #'query-replace))))

(defun conn-query-replace-regexp-region ()
  "Run `query-replace-regexp' with the region as initial contents.
Also ensure point is at start of region beforehand."
  (interactive)
  (save-mark-and-excursion
    (unless (eq (point) (region-beginning))
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

(defun conn-narrow-to-end-of-buffer ()
  "Narrow to the region between `point' and `point-max'."
  (interactive)
  (narrow-to-region (point) (point-max)))

(defun conn-narrow-to-end-of-buffer-indirect ()
  "Narrow to the region between `point' and `point-max' in an indirect buffer.
See `clone-indirect-buffer'."
  (interactive)
  (conn--narrow-indirect (point) (point-max)))

(defun conn-narrow-to-beginning-of-buffer ()
  "Narrow to the region between `point-min' and `point'."
  (interactive)
  (narrow-to-region (point-min) (point)))

(defun conn-narrow-to-beginning-of-buffer-indirect ()
  "Narrow to the region between `point-min' and `point' in an indirect buffer.
See `clone-indirect-buffer'."
  (interactive)
  (conn--narrow-indirect (point-min) (point)))

(defun conn-isearch-backward-symbol-at-point ()
  "Isearch backward for symbol at point."
  (interactive)
  (funcall-interactively #'isearch-forward-symbol-at-point -1)
  (isearch-repeat-backward))

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

(defun conn-isearch-backward-thing (thing)
  "Isearch forward for THING.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
  (pcase (bounds-of-thing-at-point thing)
    (`(,beg . ,end) (conn-isearch-region-backward beg end))
    (_              (user-error "No %s found" thing))))

(defun conn-isearch-forward-thing (thing)
  "Isearch backward for THING.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
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
         isearch-new-message (mapconcat 'isearch-text-char-description
                                        isearch-new-string ""))))

(defun conn-isearch-region-backward (beg end)
  "Isearch backward for region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive (list (region-beginning)
                     (region-end)))
  (isearch-mode nil)
  (with-isearch-suspended
   (setq isearch-new-string (buffer-substring-no-properties beg end)
         isearch-new-message (mapconcat 'isearch-text-char-description
                                        isearch-new-string ""))))

(defun conn-org-tree-edit-insert-heading ()
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content))

(defun conn-backward-char (string arg)
  "Behaves like `backward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for STRING and search backward for nearest
occurrence of STRING.  STRING will finish reading after
`conn-read-string-timout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout
                         conn-read-string-timout 'backward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (backward-char arg)
    (setq this-command 'conn-goto-string-backward)
    (conn-goto-string-backward string)))

(defun conn-goto-string-backward (string)
  "Go to the first visible occurrence backward of STRING in buffer.
When called interactively reads STRING with timeout
`conn-read-string-timout'."
  (interactive
   (list (conn--read-string-with-timeout
          conn-read-string-timout 'backward)))
  (with-restriction (window-start) (window-end)
    (when-let ((pos (or (save-excursion
                          (backward-char)
                          (and (search-backward string nil t)
                               (match-beginning 0)))
                        (user-error "\"%s\" not found." string))))
      (goto-char pos))))

(defun conn-forward-char (string arg)
  "Behaves like `forward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for STRING and search forward for nearest
occurrence of STRING.  STRING will finish reading after
`conn-read-string-timout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout
                         conn-read-string-timout 'forward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (forward-char arg)
    (setq this-command 'conn-goto-string-forward)
    (conn-goto-string-forward string)))

(defun conn-goto-string-forward (string)
  "Go to the first visible occurrence forward of STRING in buffer.
When called interactively reads STRING with timeout
`conn-read-string-timout'."
  (interactive
   (list (conn--read-string-with-timeout
          conn-read-string-timout 'forward)))
  (with-restriction (window-start) (window-end)
    (when-let ((pos (or (save-excursion
                          (forward-char)
                          (and (search-forward string nil t)
                               (match-beginning 0)))
                        (user-error "\"%s\" not found." string))))
      (goto-char pos))))

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
  (save-excursion
    (let* ((case-fold-search nil))
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

(defun conn-region-case-dwim ()
  "Cycle case in region.

not (or downcase Capitalize UPCASE) ->
downcase -> Capitalize -> UPCASE -> downcase."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (let ((str (buffer-string)))
       (cond ((string= str (capitalize str))
              (upcase-region (point-min) (point-max)))
             ((string= str (downcase str))
              (capitalize-region (point-min) (point-max)))
             (t (downcase-region (point-min) (point-max)))))
     (current-buffer))))

(defun conn-region-case-style-cycle ()
  "Change case style of region in a smart way.
Repeated calls cycle through the actions in
`conn-region-case-style-actions'."
  (interactive)
  (if (eq this-command last-command)
      (let ((cycle (get this-command :conn-cycle-state)))
        (put this-command :conn-cycle-state
             (append (cdr cycle) (list (car cycle))))
        (funcall (car cycle)))
    (put this-command :conn-cycle-state
         (append (cdr conn-region-case-style-actions)
                 (list (car conn-region-case-style-actions))))
    (funcall (car conn-region-case-style-actions))
    (let ((cycle (get this-command :conn-cycle-state)))
      (put this-command :conn-cycle-state
           (append (cdr cycle) (list (car cycle))))
      (funcall (car cycle)))))

(defun conn-kebab-case-region ()
  "Transform region text to capital-snake-case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
       (replace-match "\\1-\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "\\1-\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "_" nil t)
       (replace-match "-" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "-+" nil t)
       (replace-match "-" nil nil))
     (downcase-region (point-min) (point-max))
     (current-buffer))))

(defun conn-capital-snake-case-region ()
  "Transform region text to Capital_Snake_Case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (search-forward "-" nil t)
       (replace-match "_" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "_+" nil t)
       (replace-match "_" nil nil))
     (capitalize-region (point-min) (point-max))
     (current-buffer))))

(defun conn-snake-case-region ()
  "Transform region text to snake_case."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "-" nil t)
       (replace-match "_" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "_+" nil t)
       (replace-match "_" nil nil))
     (downcase-region (point-min) (point-max))
     (current-buffer))))

(defun conn-capital-case-region ()
  "Transform region text to CapitalCase."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (capitalize-region (point-min) (point-max))
     (goto-char (point-min))
     (while (re-search-forward "[-_]+" nil t)
       (replace-match "" nil nil))
     (current-buffer))))

(defun conn-camel-case-region ()
  "Transform region text to camelCase."
  (interactive)
  (conn--apply-region-transform
   (lambda ()
     (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "\\1_\\2" nil nil))
     (capitalize-region (point-min) (point-max))
     (goto-char (point-min))
     (while (re-search-forward "\\([A-Z]+\\)\\([A-Z][a-z]\\)" nil t)
       (replace-match "" nil nil))
     (downcase-region (point-min) (1+ (point-min)))
     (while (re-search-forward "[-_]+" nil t)
       (replace-match "" nil nil))
     (current-buffer))))

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

(defun conn-mark-thing (thing)
  "Mark THING at point.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (goto-char (cdr bounds))
    (conn--push-ephemeral-mark (car bounds))
    (activate-mark t)))

(defun conn-narrow-to-thing (thing)
  "Narrow indirect buffer to THING at point.
See `clone-indirect-buffer' for meaning of indirect buffer.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (narrow-to-region (car bounds) (cdr bounds))))

(defun conn-narrow-indirect-to-thing (thing)
  "Narrow to THING at point.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive (list (conn--read-thing-command)))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (conn--narrow-indirect (car bounds) (cdr bounds))))

(defun conn-narrow-to-visible ()
  "Narrow buffer to the visible portion of the selected window."
  (interactive)
  (narrow-to-region (window-start) (window-end))
  (message "Narrowed to visible region"))

(defun conn-narrow-indirect-to-visible ()
  "Narrow indirect buffer to the visible portion of the selected window.
See `clone-indirect-buffer'."
  (interactive)
  (conn--narrow-indirect (window-start) (window-end))
  (message "Narrowed to visible region"))

(defun conn-narrow-indirect-to-region (beg end)
  "Narrow to region from BEG to END in an indirect buffer in another window.
See `clone-indirect-buffer' for meaning of indirect buffer."
  (interactive (list (region-beginning) (region-end)))
  (conn--narrow-indirect beg end))

(defun conn--read-pair ()
  (pcase (string-split (minibuffer-with-setup-hook
                           (lambda ()
                             (when (boundp 'electric-pair-mode)
                               (electric-pair-mode -1)))
                         (read-string "Pair: " nil 'conn-pair-history))
                       conn-read-pair-split-string)
    (`(,front ,back . nil) (cons front back))
    (`(,str) (conn--thread <>
                 (lambda (char)
                   (pcase (alist-get char insert-pair-alist)
                     (`(,close . nil) (list char close))
                     (`(,open ,close) (list open close))
                     (_               (list char char))))
               (seq-map <> str)
               (apply #'seq-mapn 'string <>)
               (cons (car <>) (nreverse (cadr <>)))))
    (_ (user-error "Unknown pair format."))))

(defun conn-insert-pair (brackets beg end)
  "Insert BRACKETS at BEG and END.
Brackets are matched using `insert-pair-alist'.  If BRACKETS contains
`conn-read-pair-split-string' then split BRACKETS on
`conn-read-pair-split-string' and use the first part as the beginning
brackets and the second part as the end brackets.
When called interactively inserts STRING at `point' and `mark'."
  (interactive (list (conn--read-pair)
                     (region-beginning)
                     (region-end)))
  (save-mark-and-excursion
    (pcase-let ((`(,open . ,close) brackets))
      (goto-char end)
      (insert close)
      (goto-char beg)
      (insert open))))

(defun conn-change-pair (brackets arg)
  "Call `conn-delete-pair' with ARG then call `conn-insert-pair' with STRING."
  (interactive (list (conn--read-pair) current-prefix-arg))
  (conn-delete-pair (or arg 1))
  (conn-insert-pair brackets
                    (region-beginning)
                    (region-end)))

(defun conn-delete-pair (arg)
  "Delete ARG chars at `point' and `mark'."
  (interactive "p")
  (save-mark-and-excursion
    (let ((end (> (point) (mark-marker))))
      (when end (exchange-point-and-mark t))
      (delete-region (point) (+ (point) arg))
      (delete-region (- (mark-marker) arg) (mark-marker)))))

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
               (deactivate-mark)
               (push-mark-command nil t)
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

(defun conn-highlight-region (start end)
  "`highlight-phrase' in region from START and END."
  (interactive (list (region-beginning)
                     (region-end)))
  (let* ((regexp (regexp-quote (buffer-substring-no-properties start end)))
         (hi-lock-auto-select-face t)
         (face (hi-lock-read-face-name)))
    (unless hi-lock-mode (hi-lock-mode 1))
    (or (facep face) (setq face 'hi-yellow))
    (hi-lock-set-pattern
     regexp face nil nil
     (if (and case-fold-search search-upper-case)
         (isearch-no-upper-case-p regexp t)
       case-fold-search))))

(defun conn-yank-replace (start end &optional arg)
  "`yank' replacing region between START and END.
If called interactively uses the region between point and mark.
If arg is non-nil, kill the region between START and END instead
of deleting it."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (if arg
      (funcall (conn-kill-region-keys) start end)
    (funcall (conn-delete-region-keys) start end))
  (funcall (conn-yank-keys)))

(defun conn-isearch-exit-and-mark ()
  "`isearch-exit' and set region to match."
  (interactive)
  (isearch-exit)
  (conn--push-ephemeral-mark isearch-other-end))

(defun conn--end-of-inner-line-1 ()
  (goto-char (line-end-position))
  (when-let ((cs (and (conn--point-is-in-comment-p)
                      (save-excursion
                        (comment-search-backward
                         (line-beginning-position) t)))))
    (goto-char cs))
  (skip-chars-backward " \t" (line-beginning-position))
  (when (bolp) (skip-chars-forward " \t" (line-end-position))))

(defun conn-end-of-inner-line (&optional N)
  "Go to point after the last non-whitespace or comment character in line.
Immediately repeating this command goes to the point at end
of line proper."
  (interactive "P")
  (let ((point (point))
        (mark (mark t)))
    (when N (forward-line N))
    (conn--end-of-inner-line-1)
    (when (and (= point (point))
               (or (= mark (save-excursion
                             (back-to-indentation)
                             (point)))
                   (region-active-p)))
      (goto-char (line-end-position))
      (setq conn-this-command-handler
            (conn-individual-thing-handler 'outer-line)))))

(defun conn-beginning-of-inner-line (&optional N)
  "Go to first non-whitespace character in line.
Immediately repeating this command goes to the point at beginning
of line proper."
  (interactive "P")
  (let ((point (point))
        (mark (mark t)))
    (when N (forward-line (- N)))
    (back-to-indentation)
    (when (and (= point (point))
               (or (= mark (save-excursion
                             (conn--end-of-inner-line-1)
                             (point)))
                   (region-active-p)))
      (goto-char (line-beginning-position))
      (setq conn-this-command-handler
            (conn-individual-thing-handler 'outer-line)))))

(defun conn-xref-definition-prompt ()
  "`xref-find-definitions' but always prompt."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'xref-find-definitions)))

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

(defun conn-kill-whole-line (&optional arg)
  "Kill current line but exclude the trailing newline.
With prefix ARG, kill that many lines starting from the current line."
  (interactive "P")
  (cond (arg (kill-whole-line (prefix-numeric-value arg)))
        ((and (bolp) (eolp)) (delete-line))
        (t (kill-whole-line 0))))

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
        (copy-to-register register start end))
    (if rectangle-mark-mode
        (copy-rectangle-as-kill start end)
      (copy-region-as-kill start end)))
  (pulse-momentary-highlight-region start end))

(defun conn-kill-region (&optional arg)
  "Kill region between START and END.

If START and END are equal delete char backward.

If ARG is an ordinary prefix argument (\\[universal-argument]) delete
the region instead of killing it.

If ARG is a numeric prefix argument kill region to a register."
  (interactive (list current-prefix-arg))
  (cond ((= (point) (mark t))
         (call-interactively (conn-backward-delete-keys)))
        ((numberp arg)
         (conn--thread <>
             (concat "Kill "
                     (if rectangle-mark-mode "Rectangle " " ")
                     "to register:")
           (register-read-with-preview <>)
           (copy-to-register <> nil nil t t)))
        (t (call-interactively (conn-kill-region-keys)))))

(defun conn-completing-yank-replace (start end &optional arg)
  "Replace region from START to END with result of `yank-from-kill-ring'.

If ARG is non-nil `kill-region' instead of `delete-region'."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (let ((ov (make-overlay start end))
        exchange)
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
      (setq end (1+ end)))
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
  (save-mark-and-excursion
    (conn-duplicate-region beg end arg)
    (comment-region (region-beginning)
                    (region-end))))

;;;;; Window Commands

(defun conn-swap-windows (&optional no-select)
  "Swap selected window and another window.
If NO-SELECT is non-nil the window containing the buffer in the other
window will be selected at the end of this command.  Otherwise the
selected window will be the window containing the current buffer."
  (interactive "P")
  (when-let ((win1 (selected-window))
             (win2 (conn--prompt-for-window
                    (remove win1 (conn--all-visible-windows)))))
    (window-swap-states win1 win2)
    (when no-select
      (select-window win1)
      (select-frame-set-input-focus (window-frame win1)))))

(defun conn-swap-buffers ()
  "Swap the buffers of the selected window and another window."
  (interactive)
  (conn-swap-windows t))

(defun conn-other-window (&optional all-frames)
  "Select other window.
When the number of windows is greater than or equal to
`conn-other-window-prompt-threshold' prompt for the window to select.
Otherwise behave like `other-window'."
  (interactive "P")
  (if-let ((other-windows (remove (selected-window)
                                  (if all-frames
                                      (conn--all-visible-windows)
                                    (window-list nil 'no-mini))))
           (_ (or all-frames
                  (not (length< other-windows
                                conn-other-window-prompt-threshold))))
           (win (conn--prompt-for-window other-windows)))
      (progn
        (select-frame-set-input-focus (window-frame win))
        (select-window win))
    (other-window 1)))

(defun conn-other-place-prefix (&optional arg)
  "One of `other-window-prefix', `other-tab-prefix' or `other-frame-prefix'.
If ARG is nil `other-window-prefix',
if ARG is \\[universal-argument] `other-frame-prefix',
if ARG is anything else `other-tab-prefix'."
  (interactive "P")
  (pcase arg
    ('(4) (other-frame-prefix))
    ('nil (other-window-prefix))
    (_    (other-tab-prefix))))

;;;;;; Wincontrol Mode

;; A simple version of hyperbole's hycontrol-windows

(defvar conn--wincontrol-arg 1)
(defvar conn--previous-scroll-conservatively)
(defvar conn--wincontrol-help)
(defvar conn--wincontrol-help-format)
(defvar conn--wincontrol-prev-eldoc-msg-fn)

(defcustom conn-wincontrol-initial-help 'window
  "Initial help message printed during `conn-wincontrol-mode'."
  :group 'conn
  :type '(choice (const :tag "Window" window)
                 (const :tag "Frame" frame)
                 (const :tag "Short" nil)))

(defcustom conn-wincontrol-arg-limit 1000
  "Limit for prefix arg in `conn-wincontrol-mode'."
  :group 'conn
  :type 'integer)

(defun conn--wincontrol-window-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Window: " 'face 'bold)
    "prefix arg: "
    (propertize "%d" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help]: help; "
    "\\[conn-wincontrol-off]: quit; "
    "\\[conn-wincontrol-heighten] "
    "\\[conn-wincontrol-shorten] "
    "\\[conn-wincontrol-widen] "
    "\\[conn-wincontrol-narrow]: "
    "heighten shorten widen narrow"
    "\n"
    "\\[conn-wincontrol-windmove-up] "
    "\\[conn-wincontrol-windmove-down] "
    "\\[conn-wincontrol-windmove-left] "
    "\\[conn-wincontrol-windmove-right]: "
    "move; "
    "\\[conn-wincontrol-scroll-up] "
    "\\[conn-wincontrol-scroll-down]: "
    "scroll; "
    "\\[unbury-buffer] \\[bury-buffer]: un/bury; "
    "\\[delete-window] \\[delete-other-windows]: delete win/other; "
    "\\[conn-wincontrol-swap-windows] \\[conn-swap-buffers]: swap/grab"
    "\n"
    "\\[conn-register-load] \\[window-configuration-to-register]: load/store; "
    "\\[conn-wincontrol-clone-buffer]: clone; "
    "\\[conn-wincontrol-split-vertically] \\[conn-wincontrol-split-right]: "
    "split vert/right; "
    "\\[text-scale-set]: scale; "
    "\\[balance-windows] \\[maximize-window]: balance/max; "
    "\\[tab-bar-history-back] \\[tab-bar-history-forward]: undo/redo")))

(defun conn--wincontrol-tab-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Tab: " 'face 'bold)
    "prefix arg: "
    (propertize "%d" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help]: help; "
    "\\[conn-wincontrol-off]: quit; "
    "\\[tab-bar-move-window-to-tab]: win to new tab"
    "\n"
    "\\[conn-tab-to-register]: store; "
    "\\[tab-previous] \\[tab-next]: next/prev; "
    "\\[tab-bar-new-tab] \\[tab-bar-duplicate-tab] \\[conn-wincontrol-tab-close]: "
    "new/clone/kill; "
    "\\[conn-tab-group]: group; "
    "\\[tab-bar-detach-tab]: tear off")))

(defun conn--wincontrol-frame-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "Frame: " 'face 'bold)
    "prefix arg: "
    (propertize "%d" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help]: help; "
    "\\[conn-wincontrol-off]: quit; "
    "\\[toggle-frame-fullscreen]: fullscreen; "
    "\\[clone-frame]: clone; "
    "\\[undelete-frame]: undelete"
    "\n"
    "\\[conn-wincontrol-reverse] \\[conn-wincontrol-reflect]: reverse/reflect; "
    "\\[other-frame]: other; "
    "\\[iconify-or-deiconify-frame] \\[make-frame-command]: iconify/create; "
    "\\[tear-off-window]: tear off; "
    "\\[delete-frame] \\[delete-other-frames]: delete/other")))

(defun conn--wincontrol-simple-format ()
  (substitute-command-keys
   (concat
    "\\<conn-wincontrol-map>"
    (propertize "WinControl: " 'face 'bold)
    "prefix arg: "
    (propertize "%d" 'face 'transient-value) "; "
    "\\[conn-wincontrol-digit-argument-reset]: reset; "
    "\\[conn-wincontrol-help]: help; "
    "\\[conn-wincontrol-off]: quit")))

(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "C-0"     'delete-window
  "C-1"     'delete-other-windows
  "C-2"     'split-window-below
  "C-3"     'split-window-right
  "C-6"     'conn-swap-buffers
  "C-7"     'conn-swap-windows
  "C-8"     'conn-tab-to-register
  "C-9"     'tab-close
  "C-g"     'conn-wincontrol-off
  "C-M-0"   'kill-buffer-and-window
  "C-M-d"   'delete-other-frames
  "M-1"     'iconify-or-deiconify-frame
  "M-2"     'make-frame-command
  "M-/"     'undelete-frame
  "M-`"     'other-frame
  "M-c"     'clone-frame
  "M-d"     'delete-frame
  "+"       'maximize-window
  "-"       'conn-wincontrol-invert-argument
  "."       'conn-wincontrol-digit-argument-reset
  "/"       'tab-bar-history-back
  "0"       'conn-wincontrol-digit-argument
  "1"       'conn-wincontrol-digit-argument
  "2"       'conn-wincontrol-digit-argument
  "3"       'conn-wincontrol-digit-argument
  "4"       'conn-wincontrol-digit-argument
  "5"       'conn-wincontrol-digit-argument
  "6"       'conn-wincontrol-digit-argument
  "7"       'conn-wincontrol-digit-argument
  "8"       'conn-wincontrol-digit-argument
  "9"       'conn-wincontrol-digit-argument
  "<"       'conn-wincontrol-reverse
  ">"       'conn-wincontrol-reflect
  "="       'balance-windows
  "?"       'tab-bar-history-forward
  "_"       'shrink-window-if-larger-than-buffer
  "<down>"  'conn-wincontrol-windmove-down
  "<left>"  'conn-wincontrol-windmove-left
  "<next>"  'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "<right>" 'conn-wincontrol-windmove-right
  "<tab>"   'conn-wincontrol-scroll-up
  "<up>"    'conn-wincontrol-windmove-up
  "TAB"     'conn-wincontrol-scroll-up
  "DEL"     'conn-wincontrol-scroll-down
  "SPC"     'conn-wincontrol-scroll-up
  "M-<tab>" 'conn-wincontrol-scroll-down
  "M-TAB"   'conn-wincontrol-scroll-down
  "a"       'conn-wincontrol-off
  "b"       'switch-to-buffer
  "c"       'conn-wincontrol-clone-buffer
  "C"       'tab-bar-duplicate-tab
  "D"       'delete-other-windows
  "d"       'delete-window
  "e"       'conn-tab-to-register
  "f"       'toggle-frame-fullscreen
  "G"       'conn-tab-group
  "g"       'conn-swap-buffers
  "H"       'conn-wincontrol-help
  "h"       'conn-wincontrol-heighten
  "i"       'conn-wincontrol-windmove-up
  "I"       'tab-bar-move-window-to-tab
  "j"       'conn-wincontrol-windmove-left
  "J"       'tab-previous
  "K"       'conn-wincontrol-tab-close
  "k"       'conn-wincontrol-windmove-down
  "l"       'conn-wincontrol-windmove-right
  "L"       'tab-next
  "n"       'conn-wincontrol-narrow
  "N"       'tab-bar-new-tab
  "o"       'tear-off-window
  "O"       'tab-bar-detach-tab
  "p"       'conn-register-load
  "P"       'window-configuration-to-register
  "q"       'conn-wincontrol-off
  "r"       'conn-wincontrol-split-right
  "s"       'conn-wincontrol-shorten
  "u"       'bury-buffer
  "U"       'unbury-buffer
  "v"       'conn-wincontrol-split-vertically
  "w"       'conn-wincontrol-widen
  "x"       'conn-wincontrol-swap-windows
  "z"       'text-scale-set)

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
  (when (null conn--wincontrol-arg)
    (setq conn--wincontrol-arg 1))
  (setq prefix-arg conn--wincontrol-arg)
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
    (conn-wincontrol-mode -1)
    (add-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit))
   (t (conn--wincontrol-message))))

(defun conn--wincontrol-message ()
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message conn--wincontrol-help-format conn--wincontrol-arg)))

(defun conn--wincontrol-setup ()
  (internal-push-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (add-hook 'post-command-hook 'conn--wincontrol-post-command)
  (add-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (setq conn--previous-scroll-conservatively scroll-conservatively
        conn--wincontrol-help conn-wincontrol-initial-help
        conn--wincontrol-prev-eldoc-msg-fn eldoc-message-function
        eldoc-message-function #'ignore
        scroll-conservatively 100
        conn--wincontrol-arg (mod (prefix-numeric-value current-prefix-arg)
                                  conn-wincontrol-arg-limit))
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
    (conn-wincontrol-mode 1)))

(defun conn-wincontrol-digit-argument (N)
  "Append N to wincontrol prefix arg.
When called interactively N is `last-command-event'."
  (interactive (list (- (logand last-command-event ?\177) ?0)))
  (let ((arg (+ (if (>= conn--wincontrol-arg 0) N (- N))
                (* 10 conn--wincontrol-arg))))
    (setq conn--wincontrol-arg (if (>= arg conn-wincontrol-arg-limit) N arg)
          this-command 'conn-wincontrol-digit-argument)))

(defun conn-wincontrol-invert-argument ()
  "Invert wincontrol prefix arg."
  (interactive)
  (setq conn--wincontrol-arg (- conn--wincontrol-arg)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to 0."
  (interactive)
  (setq conn--wincontrol-arg 0))

(defun conn-wincontrol-off ()
  "Exit `conn-wincontrol-mode'."
  (interactive)
  (conn-wincontrol-mode -1))

(defun conn-wincontrol-help (&optional interactive)
  "Cycle to the next `conn-wincontrol-mode' help message."
  (interactive (list t))
  (when interactive
    (setq conn--wincontrol-help (pcase conn--wincontrol-help
                                  ('window 'tab)
                                  ('tab    'frame)
                                  ('frame  nil)
                                  (_       'window))))
  (setq conn--wincontrol-help-format
        (pcase conn--wincontrol-help
          ('window (conn--wincontrol-window-format))
          ('tab    (conn--wincontrol-tab-format))
          ('frame  (conn--wincontrol-frame-format))
          (_       (conn--wincontrol-simple-format)))))

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

(defun conn-wincontrol-widen ()
  "`enlarge-window-horizontally' by `conn--wincontrol-arg' units.."
  (interactive)
  (enlarge-window-horizontally conn--wincontrol-arg))

(defun conn-wincontrol-narrow ()
  "`shrink-window-horizontally' by `conn--wincontrol-arg' units."
  (interactive)
  (shrink-window-horizontally conn--wincontrol-arg))

(defun conn-wincontrol-heighten ()
  "`enlarge-window' by `conn--wincontrol-arg' units."
  (interactive)
  (enlarge-window conn--wincontrol-arg))

(defun conn-wincontrol-shorten ()
  "`shrink-window' by `conn--wincontrol-arg' units."
  (interactive)
  (shrink-window conn--wincontrol-arg))

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

(defun conn-wincontrol-swap-windows ()
  "Prompt for window and swap current window and other window.
Uses `conn-swap-windows'."
  (interactive)
  (conn-swap-windows))

(defun conn-wincontrol-clone-buffer ()
  "Clone indirect buffer in a new window.
Uses `clone-indirect-buffer-other-window'."
  (interactive)
  (clone-indirect-buffer-other-window nil t))

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
           (width   (alist-get 'normal-width params))
           (pheight (round (* (alist-get 'pixel-width params)
                              (/ 1 width)
                              (/ height 1))))
           (theight (round (* (alist-get 'total-width params)
                              (/ 1 width)
                              (/ height 1))))
           (pwidth  (round (* (alist-get 'pixel-height params)
                              (/ 1 height)
                              (/ width 1))))
           (twidth  (round (* (alist-get 'total-height params)
                              (/ 1 height)
                              (/ width 1)))))
      (setf (alist-get 'normal-width params)  height
            (alist-get 'normal-height params) width
            (alist-get 'pixel-height params) pheight
            (alist-get 'pixel-width params) pwidth
            (alist-get 'total-height params) theight
            (alist-get 'total-width params) twidth))
    (append (mapcar (lambda (elem)
                      (pcase elem
                        ('vc 'hc)
                        ('hc 'vc)
                        (_ elem)))
                    params)
            (mapcar 'conn--wincontrol-reflect-window windows))))

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
                (mapcar (lambda (win)
                          (conn--wincontrol-reverse-window win t))
                        windows)
              windows))))

(defun conn-wincontrol-reverse (arg)
  "Reflect windows in frame root window.
If ARG is not +/-1 or 0 reflect windows in selected window parent window.
If ARG is negative reverse windows recursively."
  (interactive "p")
  (let ((window (unless (or (= arg 0) (= (abs arg) 1))
                  (window-parent (selected-window)))))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reverse-window (< arg 0))
      (window-state-put window))))

(defun conn-wincontrol-reflect (arg)
  "Rotate windows in frame root window.
If ARG is not +/-1 or 0 rotate windows in selected window parent window."
  (interactive "p")
  (let ((window (unless (or (= arg 0) (= (abs arg) 1))
                  (window-parent (selected-window)))))
    (thread-first
      (window-state-get window)
      (conn--wincontrol-reflect-window)
      (window-state-put window))))

;;;;; Transition Functions

(defun conn-emacs-state-and-complete ()
  "Enter `conn-emacs-state' and call `completion-at-point'."
  (interactive)
  (conn-emacs-state)
  (completion-at-point))

(defun conn-dot-quit ()
  "Pop state and clear all dots."
  (interactive)
  (conn--remove-dots)
  (conn-pop-state))

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
         (funcall (conn-kill-region-keys) start end)
         (conn-emacs-state))
        (t
         (funcall (conn-delete-region-keys) start end)
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
  (back-to-indentation)
  (conn-emacs-state))

;;;;; Thing Definitions

(conn-register-thing page
  :mark-key "p"
  :forward-op 'forward-page)

(conn-register-thing-commands
 'page (conn-individual-thing-handler 'page)
 'forward-page 'backward-page)

(conn-register-thing dot
  :beg-op (lambda () (conn-previous-dot 1))
  :end-op (lambda () (conn-next-dot 1)))

(conn-register-thing-commands
 'dot (conn-individual-thing-handler 'dot)
 'conn-next-dot 'conn-previous-dot)

(conn-register-thing word
  :forward-op 'forward-word)

(conn-register-thing-commands
 'word (conn-sequential-thing-handler 'word)
 'forward-word 'backward-word)

(conn-register-thing sexp
  :forward-op 'forward-sexp)

(conn-register-thing-commands
 'sexp (conn-sequential-thing-handler 'sexp)
 'forward-sexp 'backward-sexp)

(conn-register-thing-commands
 'sexp (conn-individual-thing-handler 'sexp)
 'up-list 'backward-up-list)

(conn-register-thing whitespace
  :mark-key "SPC"
  :forward-op 'forward-whitespace)

(conn-register-thing-commands
 'whitespace (conn-individual-thing-handler 'whitespace)
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing sentence
  :forward-op 'forward-sentence)

(conn-register-thing-commands
 'sentence (conn-sequential-thing-handler 'sentence)
 'forward-sentence 'backward-sentence)

(conn-register-thing paragraph
  :forward-op 'forward-paragraph)

(conn-register-thing-commands
 'paragraph (conn-sequential-thing-handler 'paragraph)
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun (conn-sequential-thing-handler 'defun)
 'end-of-defun 'beginning-of-defun)

(conn-register-thing buffer
  :bounds-op (lambda () (cons (point-min) (point-max))))

(conn-register-thing-commands
 'buffer (conn-individual-thing-handler 'buffer)
 'end-of-buffer 'beginning-of-buffer)

(conn-register-thing line
  :forward-op (lambda (N)
                (cond ((> N 0)
                       (forward-line N))
                      ((< N 0)
                       (let ((pt (point)))
                         (beginning-of-line)
                         (if (= pt (point))
                             (forward-line N)
                           (forward-line (1+ N))))))))

(conn-register-thing-commands
 'line (conn-sequential-thing-handler 'line)
 'forward-line 'conn-backward-line)

(conn-register-thing-commands
 'line 'conn-jump-handler
 'next-line 'previous-line)

(conn-register-thing outer-line
  :beg-op (lambda () (move-beginning-of-line nil))
  :end-op (lambda () (move-end-of-line nil)))

(conn-register-thing-commands
 'outer-line (conn-individual-thing-handler 'outer-line)
 'move-beginning-of-line 'move-end-of-line)

(conn-register-thing inner-line
  :beg-op 'back-to-indentation
  :end-op 'conn--end-of-inner-line-1)

(conn-register-thing-commands
 'inner-line (conn-individual-thing-handler 'inner-line)
 'back-to-indentation
 'conn-beginning-of-inner-line
 'conn-end-of-inner-line)


;;;; Transient Menus

(defun conn-recursive-edit-kmacro ()
  "Edit last keyboard macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive)
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-macro)
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (delete-other-windows)
        (conn-local-mode 1)
        (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
        (unwind-protect
            (if isearch-mode
                (with-isearch-suspended
                 (recursive-edit))
              (recursive-edit))
          (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
          (kill-buffer buffer)
          (transient-resume))))))

(defun conn-recursive-edit-lossage ()
  "Edit lossage macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive)
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-lossage)
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (delete-other-windows)
        (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
        (unwind-protect
            (if isearch-mode
                (with-isearch-suspended
                 (recursive-edit))
              (recursive-edit))
          (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
          (kill-buffer buffer)
          (transient-resume))))))

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
       (conn--thread <>
           (car (last kmacro-ring))
         (kmacro--keys <>)
         (conn--kmacro-display <> 15)
         (concat <> ", ")))
     (propertize (conn--kmacro-display last-kbd-macro 15)
                 'face 'transient-value)
     (if (kmacro-ring-empty-p)
         ""
       (conn--thread <>
           (car kmacro-ring)
         (kmacro--keys <>)
         (conn--kmacro-display <> 15)
         (concat ", " <>))))))

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

(defun conn--kmacro-ring-empty-p ()
  ;; Avoid the messages kmacro-ring-empty-p dispays
  (while (and (null last-kbd-macro) kmacro-ring)
    (kmacro-pop-ring1))
  (null last-kbd-macro))

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
   [("i" "Insert Counter" kmacro-insert-counter)
    ("s" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix :transient t)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("w" "Swap" kmacro-swap-ring :transient t)
    ("o" "Pop" kmacro-delete-ring-head :transient t)]]
  ["Commands:"
   :if-not conn--in-kbd-macro-p
   [("k" "Call Macro" kmacro-call-macro)
    ("a" "Append to Macro" (lambda ()
                             (interactive)
                             (kmacro-start-macro '(4))))
    ("A" "Append w/o Executing" (lambda ()
                                  (interactive)
                                  (kmacro-start-macro '(16))))
    ("r" "Record Macro" kmacro-start-macro)
    ("d" "Name Last Macro" kmacro-name-last-macro)]
   [("l" "Edit Macro" kmacro-edit-macro)
    ("L" "Edit Lossage" kmacro-edit-lossage)
    ("m" "Kmacro to Register" kmacro-to-register)
    ("c" "Apply Macro on Lines" apply-macro-to-region-lines)
    ("q" "Step Edit Macro" kmacro-step-edit-macro)]]
  [:if
   conn--in-kbd-macro-p
   ["Commands:"
    ("q" "Query" kbd-macro-query)
    ("d" "Redisplay" kmacro-redisplay)]
   [:description
    conn--kmacro-counter-display
    ("i" "Insert Counter" kmacro-insert-counter)
    ("s" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)]])

(transient-define-prefix conn-register-prefix ()
  "Transient menu for register functions."
  ["Register Store:"
   [("p" "Point" point-to-register)
    ("m" "Macro" kmacro-to-register)
    ("t" "Tab" conn-tab-to-register)]
   [("f" "Frameset" frameset-to-register)
    ("r" "Rectangle" copy-rectangle-to-register)
    ("w" "Window Configuration" window-configuration-to-register)]]
  ["Register:"
   [("l" "Load" conn-register-load)
    ("u" "Unset" conn-unset-register :transient t)]
   [("i" "Increment" increment-register :transient t)
    ("s" "List" list-registers :transient t)]])

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

(transient-define-infix conn--case-fold-infix ()
  :class 'transient-lisp-variable
  :variable 'sort-fold-case
  :reader (lambda (&rest _)
            (not sort-fold-case)))

(transient-define-prefix conn-sort-prefix ()
  "Transient menu for buffer sorting functions."
  [["Sort Region: "
    ("a" "sort pages" sort-pages)
    ("c" "sort columns" sort-columns)
    ("l" "sort lines" sort-lines)]
   [("f" "case fold" conn--case-fold-infix)
    ("n" "sort numeric fields" sort-numeric-fields)
    ("p" "sort paragraphs" sort-paragraphs)
    ("r" "sort regexp fields" sort-regexp-fields)]])

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

(defun conn--set-macro-ring-head (macro)
  (interactive
   (list (get-register (register-read-with-preview "Kmacro: "))))
  (unless (or (null macro)
              (stringp macro)
              (vectorp macro)
              (kmacro-p macro))
    (user-error "Invalid keyboard macro"))
  (kmacro-push-ring macro)
  (kmacro-swap-ring))

(transient-define-argument conn--dispatch-dot-read-buffers-infix ()
  "How to read additional buffers on which to dispatch.
CRM means read buffers with `completing-read-multiple',
MATCH-REGEXP means dispatch on buffers matching a regexp."
  :class 'conn-transient-switches
  :description "Read Dot Buffers"
  :key "b"
  :argument "buffers="
  :argument-format "buffers=%s"
  :argument-regexp "\\(buffers=\\(CRM\\|match-regexp\\)\\)"
  :choices '("CRM" "match-regexp")
  :if 'conn--dots-active-p)

(transient-define-argument conn--dispatch-dots-infix ()
  "What to do with dots after dispatching on them.
REMOVE means delete the dots, to-region means move the dots to the
current region after each macro has finished executing, and KEEP
means keep the dots in their original position."
  :class 'conn-transient-switches
  :description "Dots"
  :key "c"
  :argument "dots"
  :argument-format "dots=%s"
  :argument-regexp "\\(dots=\\(remove\\|to-region\\|keep\\)\\)"
  :choices '("remove" "to-region" "keep")
  :required t
  :if 'conn--dots-active-p
  :init-value (lambda (obj) (oset obj value "dots=remove")))

(transient-define-argument conn--dispatch-macro-infix ()
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
  :choices '("apply" "append" "step-edit"))

(transient-define-argument conn--dispatch-matches-infix ()
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

(transient-define-argument conn--dispatch-state-infix ()
  "Dispatch in a specific state."
  :class 'conn-transient-switches
  :required t
  :description "In State"
  :key "i"
  :argument "state="
  :argument-format "state=%s"
  :argument-regexp "\\(state=\\(emacs\\|conn\\|dot\\)\\)"
  :choices '("conn" "emacs" "dot")
  :init-value (lambda (obj)
                (oset obj value
                      (format "state=%s"
                              (pcase conn-current-state
                                ('conn-emacs-state "emacs")
                                (_ "conn"))))))

(transient-define-argument conn--dispatch-region-infix ()
  "How to dispatch on each region.
START means place the point at the start of the region before
each iteration.  END means place the point at the end of the
region before each iteration.  CHANGE means delete the region
before each iteration."
  :class 'conn-transient-switches
  :required t
  :key "r"
  :description "Regions"
  :argument "region="
  :argument-format "region=%s"
  :argument-regexp "\\(region=\\(start\\|change\\|end\\)\\)"
  :choices '("start" "end" "change")
  :init-value (lambda (obj) (oset obj value "region=start")))

(transient-define-argument conn--dispatch-order-infix ()
  "Dispatch on regions from last to first."
  :class 'transient-switch
  :key "o"
  :description "Order"
  :argument "reverse")

(transient-define-argument conn--dispatch-empty-infix ()
  "Include empty regions in dispatch."
  :class 'transient-switch
  :key "u"
  :description "Include Empty"
  :argument "empty")

(transient-define-suffix conn--dispatch-suffix (args)
  "Dispatch on the current region.
If the region is discontiguous (e.g. a rectangular region) then
dispatch on each contiguous component of the region."
  :transient 'transient--do-exit
  :key "d"
  :description "On Regions"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread <>
      (region-bounds)
    (conn--region-iterator <> (member "reverse" args))
    (if (member "empty" args) <> (conn--dispatch-skip-empty <>))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--dispatch-point-and-mark-suffix (args)
  "Dispatch on the current point and mark."
  :transient 'transient--do-exit
  :key "v"
  :description "On Point and Mark"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread <>
      (list (point) (mark t))
    (conn--point-iterator <> (member "reverse" args))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--dot-dispatch-suffix (args)
  "Dispatch on dots in the selected buffers."
  :transient 'transient--do-exit
  :if 'conn--dots-active-p
  :key "e"
  :description "On Dots"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread <>
      (pcase (transient-arg-value "buffer=" args)
        ("CRM" (conn-read-dot-buffers))
        ("match-regexp" (conn-read-matching-dot-buffers))
        (_ (list (current-buffer))))
    (mapcan (apply-partially 'conn--sorted-overlays
                             #'conn-dotp '< nil nil)
            <>)
    (conn--dot-iterator <> (member "reverse" args))
    (pcase-exhaustive (transient-arg-value "dots=" args)
      ("keep" (conn--dispatch-stationary-dots <>))
      ("to-region" (conn--dispatch-relocate-dots <>))
      ("remove" (conn--dispatch-remove-dots <>)))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--regions-dispatch-suffix (iterator args)
  :transient 'transient--do-exit
  :key "d"
  :description "On Regions"
  (interactive (list (oref transient-current-prefix scope)
                     (transient-args transient-current-command)))
  (conn--thread <>
      (funcall iterator (member "reverse" args))
    (if (member "empty" args) <> (conn--dispatch-skip-empty <>))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--lines-dispatch-suffix (args)
  "Dispatch on each line between `point' and `mark'."
  :transient 'transient--do-exit
  :key "c"
  :description "On Lines"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread <>
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end))
              (emptyp (member "empty" args))
              regions)
          (goto-char beg)
          (move-beginning-of-line 1)
          (while (< (point) end)
            (when-let ((eol (line-end-position))
                       (_ (or emptyp (not (= (point) eol)))))
              (push (cons (point) eol) regions))
            (forward-line))
          regions))
    (conn--region-iterator <> (not (member "reverse" args)))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--isearch-dispatch-suffix (args)
  "Dispatch on current isearch matches."
  :transient 'transient--do-exit
  :key "d"
  :description "On Matches"
  (interactive (list (transient-args transient-current-command)))
  (conn--thread <>
      (prog1
          (if (bound-and-true-p multi-isearch-buffer-list)
              (mapcan 'conn--isearch-matches-in-buffer
                      (append
                       (remq (current-buffer) multi-isearch-buffer-list)
                       (list (current-buffer))))
            (conn--isearch-matches-in-buffer
             (current-buffer)
             (pcase (transient-arg-value "matches=" args)
               ("after" 'after)
               ("before" 'before))))
        (isearch-exit))
    (conn--region-iterator <> (member "reverse" args))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-suffix conn--text-property-dispatch-suffix (prop value args)
  "Dispatch on regions of text with a text property."
  :transient 'transient--do-exit
  :key "t"
  :description "On Text Prop"
  (interactive
   (let* ((prop (intern (completing-read
                         "Property: "
                         (cl-loop for prop in (text-properties-at (point))
                                  by #'cddr
                                  collect prop)
                         nil t)))
          (vals (mapcar (lambda (s) (cons (message "%s" s) s))
                        (ensure-list (get-text-property (point) prop))))
          (val (alist-get (completing-read "Value: " vals) vals
                          nil nil #'string=)))
     (list prop val (transient-args transient-current-command))))
  (conn--thread <>
      (save-excursion
        (goto-char (point-min))
        (let (regions match)
          (while (setq match (text-property-search-forward
                              prop value t))
            (push (cons (prop-match-beginning match)
                        (prop-match-end match))
                  regions))
          regions))
    (conn--region-iterator <> (not (member "reverse" args)))
    (conn--dispatch-save-buffer-state <>)
    (pcase-exhaustive (transient-arg-value "state=" args)
      ("conn" (conn--dispatch-with-state <> 'conn-state))
      ("emacs" (conn--dispatch-with-state <> 'conn-emacs-state))
      ("dot" (conn--dispatch-with-state <> 'conn-dot-state)))
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (conn--dispatch-change-region <>))
      ("end" (conn--dispatch-at-end <>))
      ("start" <>))
    (conn--pulse-on-record <>)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (conn--macro-dispatch <> last-kbd-macro))
      ("append" (conn--macro-dispatch-append <>))
      ("step-edit" (conn--macro-dispatch-step-edit <>))
      (_ (conn--macro-dispatch <>)))))

(transient-define-prefix conn-dispatch-prefix ()
  "Transient menu for macro dispatch on regions."
  [:description
   conn--kmacro-ring-display
   [("s" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("l" "Edit Macro" conn-recursive-edit-kmacro
     :transient transient--do-suspend)
    ("L" "Edit Lossage" conn-recursive-edit-lossage
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("g" "Push Register" conn--set-macro-ring-head :transient t)]]
  [:description
   "Dispatch"
   [(conn--dispatch-suffix)
    (conn--dot-dispatch-suffix)
    (conn--lines-dispatch-suffix)
    (conn--dispatch-point-and-mark-suffix)
    (conn--text-property-dispatch-suffix)]
   [(conn--dispatch-macro-infix)
    (conn--dispatch-region-infix)
    (conn--dispatch-state-infix)
    (conn--dispatch-dots-infix)
    (conn--dispatch-dot-read-buffers-infix)
    (conn--dispatch-empty-infix)
    (conn--dispatch-order-infix)]])

(transient-define-prefix conn-isearch-dispatch-prefix ()
  "Transient menu for macro dispatch on regions."
  [:description
   conn--kmacro-ring-display
   [("s" "Set Counter"
     (lambda ()
       (interactive)
       (with-isearch-suspended
        (call-interactively 'kmacro-set-counter)))
     :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("l" "Edit Macro" conn-recursive-edit-kmacro
     :transient transient--do-suspend)
    ("L" "Edit Lossage" conn-recursive-edit-lossage
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("g" "Push Register" conn--set-macro-ring-head :transient t)]]
  [:description
   "Dispatch"
   [(conn--isearch-dispatch-suffix)
    (conn--dot-dispatch-suffix)]
   [(conn--dispatch-macro-infix)
    (conn--dispatch-region-infix)
    (conn--dispatch-state-infix)
    (conn--dispatch-matches-infix)
    (conn--dispatch-dots-infix)
    (conn--dispatch-dot-read-buffers-infix)
    (conn--dispatch-order-infix)]])

(transient-define-prefix conn-regions-dispatch-prefix (iterator)
  "Transient menu for macro dispatch on regions."
  [:description
   conn--kmacro-ring-display
   [("s" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)
    ("l" "Edit Macro" conn-recursive-edit-kmacro
     :transient transient--do-suspend)
    ("L" "Edit Lossage" conn-recursive-edit-lossage
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("g" "Push Register" conn--set-macro-ring-head :transient t)]]
  [:description
   "Dispatch"
   [(conn--regions-dispatch-suffix)]
   [(conn--dispatch-macro-infix)
    (conn--dispatch-region-infix)
    (conn--dispatch-state-infix)
    (conn--dispatch-empty-infix)
    (conn--dispatch-order-infix)]]
  (interactive (list nil))
  (unless iterator (user-error "No regions"))
  (transient-setup 'conn-regions-dispatch-prefix nil nil :scope iterator))


;;;; Keymaps

(defvar-keymap conn-reb-navigation-repeat-map
  :repeat t
  "C-s" 'reb-next-match
  "C-r" 'reb-prev-match)

(dolist (state conn-states)
  (define-keymap
    :keymap (conn-get-mode-map state 'conn-macro-dispatch-p)
    "<remap> <kmacro-end-macro>"                'exit-recursive-edit
    "<remap> <kmacro-end-or-call-macro>"        'exit-recursive-edit
    "<remap> <kmacro-end-and-call-macro>"       'exit-recursive-edit
    "<remap> <kmacro-end-or-call-macro-repeat>" 'exit-recursive-edit))

(dolist (state '(conn-state conn-emacs-state conn-dot-state))
  (keymap-set (conn-get-mode-map state 'occur-mode) "C-c e" 'occur-edit-mode))

(dolist (state '(conn-state conn-emacs-state conn-dot-state))
  (keymap-set (conn-get-mode-map state 'occur-edit-mode) "C-c e" 'occur-cease-edit))

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "`" 'conn-other-window)

(defvar-keymap conn-region-map
  :prefix 'conn-region-map
  "DEL" 'conn-delete-region-keys
  "$"   'ispell-region
  "*"   'calc-grab-region
  ","   'conn-isearch-region-backward
  "."   'conn-isearch-region-forward
  ";"   'comment-or-uncomment-region
  "|"   'shell-command-on-region
  "["   'conn-delete-pair
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule
  "b"   'conn-command-at-point-and-mark
  "c"   'conn-region-case-map
  "D"   'conn-duplicate-and-comment-region
  "d"   'conn-duplicate-region
  "e"   'eval-region
  "g"   'conn-rgrep-region
  "I"   'indent-rigidly
  "j"   'conn-join-lines
  "m"   'conn-macro-at-point-and-mark
  "N"   'conn-narrow-indirect-to-region
  "n"   'narrow-to-region
  "o"   'conn-occur-region
  "p"   'conn-change-pair
  "r"   'conn-query-replace-regexp-region
  "s"   'conn-sort-prefix
  "u"   'conn-insert-pair
  "v"   'vc-region-history
  "w"   'conn-query-replace-region
  "y"   'yank-rectangle)

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

(defvar-keymap conn-dot-remove-repeat-map
  :repeat t
  "DEL" 'conn-remove-dot-backward
  "d"   'conn-remove-dot-forward)

(defvar-keymap conn-dot-movement-repeat-map
  :repeat t
  "l" 'conn-next-dot
  "j" 'conn-previous-dot)

(defvar-keymap conn-dot-edit-map
  :prefix 'conn-dot-edit-map
  :doc "Dot this map."
  "RET" 'conn-dot-lines
  "DEL" 'conn-remove-dot-backward
  "["   'conn-remove-dots-before
  "]"   'conn-remove-dots-after
  "{"   'conn-first-dot
  "}"   'conn-last-dot
  "l"   'conn-next-dot
  "j"   'conn-previous-dot
  "c"   'conn-split-dots-on-regexp
  "D"   'conn-remove-all-dots
  "d"   'conn-remove-dot-forward
  "p"   'conn-dot-text-property
  "q"   'conn-query-remove-dots
  "r"   'conn-add-dots-matching-regexp
  "s"   'conn-sort-dots
  "t"   'conn-dot-trim-regexp
  "w"   'conn-add-dots-matching-literal)

(defvar-keymap conn-dot-region-repeat-map
  :repeat t
  "j" 'conn-dot-region-backward
  "l" 'conn-dot-region-forward
  "o" 'conn-dot-skip-forward
  "u" 'conn-dot-skip-backward
  "J" 'conn-dot-skip-backward
  "L" 'conn-dot-skip-forward)

(defvar-keymap conn-dot-region-map
  :parent conn-region-map
  "l" 'conn-dot-region-forward
  "j" 'conn-dot-region-backward
  "o" 'conn-remove-dots-outside-region
  "c" 'conn-split-region-on-regexp)

(define-keymap
  :keymap isearch-mode-map
  "M-<return>" 'conn-isearch-exit-and-mark
  "M-\\"       'conn-isearch-dispatch-prefix
  "M-E"        'conn-isearch-add-dots
  "M-R"        'conn-isearch-refine-dots
  "M-W"        'conn-isearch-remove-dots
  "M-S"        'conn-isearch-split-dots)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

(defvar-keymap conn-search-map
  ","     'conn-isearch-backward-symbol-at-point
  "."     'isearch-forward-symbol-at-point
  "TAB"   'conn-isearch-backward-symbol
  "M-TAB" 'conn-isearch-forward-symbol
  "0"     'xref-find-references
  "x"     'conn-xref-definition-prompt
  "a"     'xref-find-apropos
  "i"     'imenu
  "o"     'occur
  "l"     'locate
  "h ,"   'conn-highlight-region
  "s B"   'multi-isearch-buffers-regexp
  "s F"   'multi-isearch-files-regexp
  "s b"   'multi-isearch-buffers
  "s f"   'multi-isearch-files)

(defvar-keymap conn-goto-map
  "Y" 'pop-global-mark
  "k" 'goto-line
  ">" 'next-error
  "<" 'previous-error)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'rectangle-mark-mode)
  "r DEL" 'delete-rectangle
  "*"     'calc-grab-rectangle
  "+"     'calc-grab-sum-down
  "_"     'calc-grab-sum-across
  "y"     'yank-rectangle)

(defvar-keymap conn-tab-bar-history-mode-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

(defvar-keymap conn-region-case-map
  :prefix 'conn-region-case-map
  "m" 'conn-region-case-style-cycle
  "k" 'conn-kebab-case-region
  "i" 'conn-capital-case-region
  "o" 'conn-camel-case-region
  "l" 'conn-capital-snake-case-region
  "j" 'conn-snake-case-region
  "u" 'conn-region-case-dwim)

(defvar-keymap conn-edit-map
  :prefix 'conn-edit-map
  "RET" 'whitespace-cleanup
  "SPC" 'conn-transpose-region-and-dot
  "TAB" 'indent-rigidly
  ";"   'comment-line
  "."   'conn-isearch-forward-thing
  ","   'conn-isearch-backward-thing
  "b"   'regexp-builder
  "c"   'clone-indirect-buffer
  "d"   'duplicate-dwim
  "f"   'conn-fill-prefix
  "h"   'conn-mark-thing-map
  "I"   'copy-from-above-command
  "j"   'join-line
  "k"   'transpose-lines
  "K"   'transpose-paragraphs
  "l"   'transpose-chars
  "m"   'transpose-sexps
  "N"   'conn-narrow-indirect-to-thing
  "n"   'conn-narrow-to-thing
  "o"   'transpose-words
  "p"   'conn-kmacro-prefix
  "q"   'indent-for-tab-command
  "r"   'query-replace-regexp
  "u"   'conn-mark-thing
  "V"   'conn-narrow-indirect-to-visible
  "v"   'conn-narrow-to-visible
  "w"   'query-replace
  "y"   'yank-in-context)

(define-keymap
  :keymap conn-dot-state-map
  "M-<down-mouse-1>" 'conn-dot-at-click
  "M-/"              'conn-dot-undo
  "<return>"         'conn-dot-lines
  "RET"              'conn-dot-lines
  "<backspace>"      'conn-remove-dot-backward
  "DEL"              'conn-remove-dot-backward
  "M-?"              'conn-dot-redo
  "C-p"              'conn-previous-dot
  "C-n"              'conn-next-dot
  "C-M-p"            'conn-first-dot
  "C-M-n"            'conn-last-dot
  "|"                'conn-shell-command-on-dots
  "{"                'conn-first-dot
  "}"                'conn-last-dot
  "["                'conn-remove-dots-before
  "]"                'conn-remove-dots-after
  "c"                'conn-split-dots-on-regexp
  "D"                'conn-remove-all-dots
  "d"                'conn-remove-dot-forward
  "E"                'conn-dot-point
  "e"                'conn-dot-region
  "q"                'conn-dot-edit-map
  "r"                conn-dot-region-map
  "t"                'conn-dot-all-things-in-region
  "y"                'conn-add-dots-matching-regexp)

(define-keymap
  :keymap conn-state-map
  "C-t"   'conn-C-x-t-keys
  "C-y"   'conn-yank-replace
  "M-y"   'conn-completing-yank-replace
  "|"     'shell-command-on-region
  "="     'indent-relative
  "$"     'ispell-word
  "*"     'calc-dispatch
  ")"     'up-list
  "("     'backward-up-list
  "["     'conn-kill-prepend-region
  "\""    'conn-insert-pair
  "<tab>" 'indent-region
  "TAB"   'indent-region
  "]"     'conn-kill-append-region
  "'"     'conn-other-place-prefix
  "B"     'ibuffer
  "C"     'conn-copy-region
  "c"     'conn-C-c-keys
  "d"     'conn-delete-char-keys
  "E"     'conn-dot-region
  "Q"     'conn-dot-edit-map
  "q"     'conn-edit-map
  "R"     conn-dot-region-map
  "r"     'conn-region-map
  "T"     'conn-dot-all-things-in-region
  "w"     'conn-kill-region
  "y"     'conn-yank-keys
  "Y"     'yank-from-kill-ring)

(define-keymap
  :keymap conn-common-map
  "<remap> <toggle-input-method>" 'conn-toggle-input-method
  "C-1"   'delete-other-windows
  "C-2"   'split-window-below
  "C-3"   'split-window-right
  "C-4"   'conn-C-x-4-keys
  "C-5"   'conn-C-x-5-keys
  "C-6"   'conn-swap-buffers
  "C-7"   'conn-swap-windows
  "C-8"   'conn-tab-to-register
  "C-9"   'tab-close
  "C-0"   'delete-window
  "C--"   'shrink-window-if-larger-than-buffer
  "C-="   'balance-windows
  "M-0"   'quit-window
  "M-1"   'delete-other-windows-vertically
  "M-2"   'make-frame-command
  "M-7"   'kill-this-buffer
  "M-8"   'tear-off-window
  "M-9"   'tab-detach
  "C-M-0" 'kill-buffer-and-window
  "SPC"   'conn-set-mark-command
  "_"     'repeat-complex-command
  "+"     'conn-set-register-seperator
  ","     'isearch-backward
  "."     'isearch-forward
  "/"     'undo-only
  ";"     'execute-extended-command
  ":"     'execute-extended-command-for-buffer
  "<"     'conn-backward-line
  ">"     'forward-line
  "?"     'undo-redo
  "`"     'conn-other-window
  "~"     'conn-swap-windows
  "a"     'conn-wincontrol
  "b"     'switch-to-buffer
  "g"     'conn-M-g-keys
  "h"     'repeat
  "I"     'conn-backward-paragraph-keys
  "i"     'conn-previous-line-keys
  "J"     'conn-beginning-of-inner-line
  "j"     'conn-backward-char
  "K"     'conn-forward-paragraph-keys
  "k"     'conn-next-line-keys
  "L"     'conn-end-of-inner-line
  "l"     'conn-forward-char
  "M"     'conn-end-of-defun-keys
  "m"     'conn-forward-sexp-keys
  "N"     'conn-beginning-of-defun-keys
  "n"     'conn-backward-sexp-keys
  "O"     'conn-forward-sentence-keys
  "o"     'conn-forward-word-keys
  "P"     'conn-register-prefix
  "p"     'conn-register-load
  "s"     'conn-M-s-keys
  "U"     'conn-backward-sentence-keys
  "u"     'conn-backward-word-keys
  "V"     'narrow-to-region
  "v"     'conn-toggle-mark-command
  "W"     'widen
  "x"     'conn-C-x-keys
  "z"     'conn-exchange-mark-command)

(define-keymap
  :keymap conn-org-tree-edit-state-map
  "SPC"         'conn-scroll-up
  "<backspace>" 'conn-scroll-down
  "DEL"         'conn-scroll-down
  "M-;"         'org-toggle-comment
  "."           'point-to-register
  "/"           'undo-only
  ";"           'execute-extended-command
  ":"           'execute-extended-command-for-buffer
  "*"           'conn-org-tree-edit-insert-heading
  "<"           'org-promote-subtree
  ">"           'org-demote-subtree
  "?"           'undo-redo
  "q s"         'org-sort
  "q c"         'org-columns
  "b"           'switch-to-buffer
  "c"           'conn-C-c-keys
  "g"           'conn-M-g-keys
  "i"           'org-backward-element
  "I"           'org-metaup
  "J"           'org-metaleft
  "j"           'org-previous-visible-heading
  "k"           'org-forward-element
  "K"           'org-metadown
  "L"           'org-metaright
  "l"           'org-next-visible-heading
  "M"           'org-mark-subtree
  "m"           'org-backward-element
  "n"           'org-forward-element
  "N"           'org-toggle-narrow-to-subtree
  "O"           'org-next-block
  "p"           'conn-register-load
  "s"           'conn-M-s-keys
  "T"           'org-todo
  "t"           'org-sparse-tree
  "U"           'org-previous-block
  "u"           'org-up-element
  "W"           'widen
  "w"           'org-refile
  "x"           'conn-C-x-keys
  "z"           'conn-exchange-mark-command)

(defvar-keymap conn-ctl-x-r-map
  "\\" 'conn-set-register-seperator
  "."  'conn-last-macro-dispatch-to-register
  ","  'conn-dot-state-to-register
  "!"  'kmacro-to-register
  "W"  'conn-unset-register)

(defvar-keymap conn-c-x-4-map
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward
  "-" 'conn-window-resize-map)

(define-keymap
  :keymap conn-mode-map
  "C-<backspace>" 'conn-kill-whole-line
  "C-`"           'conn-other-window
  "C-S-w"         'delete-region
  "C-x /"         'tab-bar-history-back
  "C-x 4"         conn-c-x-4-map
  "C-x ?"         'tab-bar-history-forward
  "C-x n M-<"     'conn-narrow-to-beginning-of-buffer-indirect
  "C-x n <"       'conn-narrow-to-beginning-of-buffer
  "C-x n M->"     'conn-narrow-to-end-of-buffer-indirect
  "C-x n >"       'conn-narrow-to-end-of-buffer
  "C-x n t"       'conn-narrow-to-thing
  "C-x n T"       'conn-narrow-indirect-to-thing
  "C-x n v"       'conn-narrow-to-visible
  "C-x n V"       'conn-narrow-indirect-to-visible
  "C-x n N"       'conn-narrow-indirect-to-region
  "C-x r"         conn-ctl-x-r-map
  "C-x t j"       'conn-register-load
  "C-x t s"       'tab-switch
  "C-x r a"       'conn-tab-to-register
  "C-x t a"       'conn-tab-to-register
  "M-RET"         'conn-open-line-and-indent
  "M-O"           'conn-pop-to-mark-command
  "M-U"           'conn-unpop-to-mark-command)

(defun conn--setup-keymaps ()
  (if conn-mode
      (progn
        (cl-pushnew 'conn--state-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--aux-maps emulation-mode-map-alists)
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
                            conn--aux-maps
                            conn--local-maps
                            conn--major-mode-maps
                            conn--local-mode-maps
                            conn--transition-maps)
                          emulation-mode-map-alists #'eq))))


;;;; Mode Definition

(defun conn--update-mode-line-indicator ()
  "Update Conn mode-line indicator."
  (setq conn--mode-line-indicator
        (or (get conn-current-state :conn-indicator) "")))

(defun conn-mode-buffer-predicate ()
  (seq-find (lambda (condition)
              (buffer-match-p condition (current-buffer)))
            conn-mode-buffers))

(define-minor-mode conn-mode-line-indicator-mode
  "Display Conn state indicator at the beginning of the mode line."
  :group 'conn
  :global t)

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :keymap (make-sparse-keymap)
  :lighter (:eval conn-lighter)
  (conn--input-method-mode-line)
  (if conn-local-mode
      (progn
        ;; Since eldoc clobbers mode-line-format structure we need to
        ;; flatten the mode-line-format tree before searching it.
        (unless (seq-contains-p (flatten-tree mode-line-format)
                                'conn-mode-line-indicator-mode
                                #'eq)
          (push '(conn-mode-line-indicator-mode (:eval conn--mode-line-indicator))
                mode-line-format))
        (unless (mark t)
          (conn--push-ephemeral-mark (point) t nil))
        (pcase-dolist (`(,_ . ,hooks) conn-input-method-overriding-modes)
          (dolist (hook hooks)
            (add-hook hook 'conn--activate-input-method nil t)))
        (add-hook 'pre-command-hook #'conn--mark-pre-command-hook nil t)
        (add-hook 'post-command-hook #'conn--mark-post-command-hook nil t)
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'clone-indirect-buffer-hook #'conn--delete-mark-cursor nil t)
        (setq conn--input-method current-input-method)
        (conn--setup-major-mode-maps)
        (funcall (conn--default-state-for-buffer))
        (run-with-timer 0.05 nil #'conn--update-aux-map))
    (without-restriction
      (conn--remove-dots))
    (when conn-current-state
      (funcall (get conn-current-state :conn-transition-fn) :exit))
    (setq conn-current-state nil)
    (conn--clear-overlays)
    (setq-local mode-line-format
                (assq-delete-all
                 'conn-mode-line-indicator-mode
                 mode-line-format))
    (pcase-dolist (`(,_ . ,hooks) conn-input-method-overriding-modes)
      (dolist (hook hooks)
        (remove-hook hook #'conn--activate-input-method t)))
    (remove-hook 'pre-command-hook #'conn--mark-pre-command-hook t)
    (remove-hook 'post-command-hook #'conn--mark-post-command-hook t)
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
  (when (and (run-hook-with-args-until-success 'conn-enable-in-buffer-hook)
             (run-hook-with-args-until-failure 'conn-disable-in-buffer-hook))
    (conn-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn-initialize-buffer
  :keymap conn-mode-map
  :group 'conn
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (conn--setup-extensions)
    (if conn-mode
        (progn
          (keymap-set minibuffer-mode-map "C-M-y" 'conn-yank-region-to-minibuffer)
          (setq conn--prev-mark-even-if-inactive mark-even-if-inactive
                mark-even-if-inactive t)
          (add-hook 'post-command-hook #'conn--update-aux-map)
          (add-hook 'window-configuration-change-hook #'conn--update-cursor)
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50))
      (when (eq (keymap-lookup minibuffer-mode-map "C-M-y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "C-M-y"))
      (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
      (remove-hook 'post-command-hook #'conn--update-aux-map)
      (remove-hook 'window-configuration-change-hook #'conn--update-cursor)
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

  (conn-register-thing org-paragraph
    :forward-op 'org-forward-paragraph
    :expand-key "I"
    :modes 'org-mode)

  (conn-register-thing-commands
   'org-paragraph (conn-sequential-thing-handler 'org-paragraph)
   'org-forward-paragraph 'org-backward-paragraph)

  (conn-register-thing org-sentence
    :forward-op (lambda (arg)
                  (if (>= arg 0)
                      (org-forward-sentence arg)
                    (org-backward-sentence (abs arg))))
    :expand-key "{"
    :modes 'org-mode)

  (conn-register-thing-commands
   'org-sentence (conn-sequential-thing-handler 'org-sentence)
   'org-forward-sentence 'org-backward-sentence)

  (conn-register-thing org-element
    :expand-key "K"
    :beg-op 'org-backward-element
    :end-op 'org-forward-element
    :modes 'org-mode)

  (conn-register-thing-commands
   'org-element (conn-individual-thing-handler 'org-element)
   'org-forward-element
   'org-backward-element
   'org-next-visible-heading
   'org-previous-visible-heading
   'org-up-element)

  (keymap-set (conn-get-mode-map 'conn-state 'org-mode)
              "T" 'conn-org-tree-edit-state)

  (define-keymap
    :keymap (conn-get-mode-map 'conn-state 'org-mode)
    "^" 'org-up-element
    ")" 'org-next-visible-heading
    "(" 'org-previous-visible-heading
    "N" 'org-backward-paragraph
    "M" 'org-forward-paragraph))

(with-eval-after-load 'polymode
  (defvar polymode-move-these-vars-from-old-buffer)
  (dolist (v '(conn--mark-cursor
               conn-current-state
               conn-state
               conn-emacs-state
               conn-dot-state))
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
  (dolist (state '(conn-state conn-dot-state))
    (define-keymap
      :keymap (conn-get-mode-map state 'paredit-mode)
      ")" 'paredit-forward-up
      "(" 'paredit-backward-up))

  (conn-register-thing paredit-sexp
    :forward-op 'paredit-forward
    :expand-key "m"
    :modes 'paredit-mode)

  (conn-register-thing-commands
   'paredit-sexp (conn-sequential-thing-handler 'paredit-sexp)
   'paredit-forward
   'paredit-backward
   'paredit-forward-up
   'paredit-backward-up)

  (conn-register-thing-commands
   'sexp (conn-individual-thing-handler 'paredit-sexp)
   'paredit-forward-up 'paredit-backward-up))

(with-eval-after-load 'zones
  (defvar zz-add-zone-anyway-p)
  ;; Make this command add narrowings to izone var
  (defun conn-narrow-to-thing (thing)
    "Narrow to THING at point.
Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
    (interactive (list (conn--read-thing-command)))
    (when-let ((bounds (bounds-of-thing-at-point thing)))
      (let ((zz-add-zone-anyway-p t))
        (narrow-to-region (car bounds) (cdr bounds)))))

  (transient-append-suffix
    'conn-register-prefix
    '(0 0 -1)
    '("z" "Izones" izones-to-register)))

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-emacs-state)
      (conn-pop-state)))
  (add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state))

(with-eval-after-load 'outline
  (declare-function outline-up-heading "outline")
  (declare-function outline-end-of-subtree "outline")

  (conn-register-thing heading
    :mark-key "H"
    :beg-op (lambda ()
              (unless (looking-at outline-regexp)
                (outline-up-heading 1)))
    :end-op (lambda ()
              (unless (looking-at outline-regexp)
                (outline-up-heading 1))
              (outline-end-of-subtree)))

  (conn-register-thing-commands
   'heading (conn-individual-thing-handler 'heading)
   'outline-up-heading
   'outline-next-heading
   'outline-previous-heading
   'outline-forward-same-level
   'outline-backward-same-level))
;;; conn.el ends here
