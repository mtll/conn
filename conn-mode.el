;;; conn-mode.el --- Modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn-mode.el
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

(defvar conn--mark-cursor-timer nil
  "`run-with-idle-timer' timer to update `mark' cursor.")

(defvar-keymap conn-global-map
  :doc "`conn-mode' keymap which is always active.
This keymap is active even in buffers which do not have
`conn-local-mode' turned on.")

;;;;; Customizable Variables

(defgroup conn-mode nil
  "Modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-dots nil
  "Conn-mode dots."
  :prefix "conn-"
  :group 'conn-mode)

(defgroup conn-marks nil
  "Conn-mode marks."
  :prefix "conn-"
  :group 'conn-mode)

(defgroup conn-states nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn-mode)

(defgroup conn-key-remappings nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn-mode)

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
  :group 'conn-mode)

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
  :group 'conn-mode)

(defcustom conn-repeating-cursor-color
  "#a60000"
  "Cursor color while repeat map is active."
  :type 'color
  :group 'conn-mode)

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
  :group 'conn-mode
  :type 'symbol)

(defcustom conn-region-case-style-actions
  '(conn-kebab-case-region
    conn-capital-snake-case-region
    conn-snake-case-region
    conn-capital-case-region
    conn-camel-case-region)
  "List of actions cycled through by `conn-region-case-style-cycle'.
Supported values are:
`conn-kebab-case-region'
`conn-capital-snake-case-region'
`conn-snake-case-region'
`conn-capital-case-region'
`conn-camel-case-region'."
  :group 'conn-mode
  :type '(repeat symbol))

(defcustom conn-read-pair-split-string "	"
  "String on which to split `conn-insert-pair' brackets."
  :group 'conn-mode
  :type 'string)

;;;;; Internal Vars

(defvar conn-states nil)

(defvar-local conn--input-method nil
  "Current input for buffer.")

(defvar conn-input-method-overriding-modes
  (list (list 'isearch-mode 'isearch-mode-hook 'isearch-mode-end-hook))
  "List of modes which override a state's input suppression property.
Each element may be either a symbol or a list of the form
(symbol . hooks).")

(defvar-local conn-current-state nil
  "Current conn state for buffer.")

(defvar-local conn-previous-state nil
  "Previous conn state for buffer.")

(defvar conn-this-thing-handler nil
  "Mark handler for current command.
Commands can set this variable if they need to change their handler
dynamically.")

(defvar conn-this-thing-start nil
  "Start position for current mark movement command.")

;; Keymaps
(defvar conn--state-maps nil)
(defvar-local conn--aux-maps nil)
(defvar-local conn--local-maps nil)
(defvar-local conn--major-mode-maps nil)
(defvar conn--mode-maps nil)
(defvar-local conn--local-mode-maps nil)
(defvar conn--transition-maps nil)

(defvar conn--prev-mark-even-if-inactive nil)

(defvar conn-dot-macro-dispatch-p nil
  "Non-nil during dot macro dispatch.")

(defvar-local conn--unpop-ring nil)

(defvar conn--saved-ephemeral-marks nil)

(defvar-local conn--ephemeral-mark nil)

(defvar-local conn--mark-cursor nil
  "`mark' cursor overlay.")
(put 'conn--mark-cursor 'permanent-local t)
(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)

(defvar-local conn--handle-mark nil)

(defvar conn-macro-dispatch-p nil
  "Non-nil during macro dispatch.

See `conn--dispatch-on-regions'.")

(defvar conn--dispatch-error nil)

(defvar-local conn--dot-undoing nil)
(defvar-local conn--dot-undo-ring nil)
(defvar-local conn--dot-undone nil)
(defvar-local conn--dot-this-undo nil)

(defvar conn-dot-undo-ring-max 32
  "Maximum size of the dot undo ring.")

(defvar conn--repat-check-key-prev-val)

(defvar conn--aux-bindings nil)

(defvar conn--goto-char-last-char nil)

(defvar-keymap conn-mark-thing-map
  :prefix 'conn-mark-thing-map
  "`" 'conn-mark-thing)

(defvar conn--aux-update-flag nil)

(defvar-local conn--aux-map-history nil)

(defvar conn--aux-map-history-size 8)

(defvar conn--last-remapping nil)

(defvar conn--read-string-timout 0.5)

(defvar conn-enable-in-buffer-hook nil
  "Hook to determine if `conn-local-mode' should be enabled in a buffer.
Each function is run without any arguments and if any of them return nil
`conn-local-mode' will not be enabled in the buffer, otherwise
`conn-local-mode' will be enabled.")

(put 'conn--dot 'evaporate t)
(put 'conn--dot 'priority conn-dot-overlay-priority)
(put 'conn--dot 'face 'conn-dot-face)
(put 'conn--dot 'evaporate t)

;;;;; Command Histories

(defvar conn-thing-history nil
  "History list for conn thing commands.")

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
    (conn--thread needle
        (lambda (e)
          (cl-etypecase e
            (string e)
            (symbol (symbol-name e))))
      (mapcar needle symbols-or-strings)
      (apply #'concat needle)))

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

(defmacro conn--save-window-configuration (&rest body)
  (let ((wind (gensym "window-conf")))
    `(let ((,wind (current-window-configuration)))
       (unwind-protect
           ,(macroexp-progn body)
         (set-window-configuration ,wind)))))

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

;; From thingatpt+
(defun conn--defined-thing-p (thing)
  (when (consp thing) (setq thing  (car thing)))
  (when (stringp thing) (setq thing  (intern thing)))
  (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
        (beginning-op  (get thing 'beginning-op))
        (end-op        (get thing 'end-op))
        (bounds-fn     (get thing 'bounds-of-thing-at-point))
        (thing-fn      (get thing 'thing-at-point)))
    (or (functionp forward-op)
        (and (functionp beginning-op)  (functionp end-op))
        (functionp bounds-fn)
        (functionp thing-fn))))

(defun conn--movement-thing-p (thing)
  (when (consp thing) (setq thing  (car thing)))
  (when (stringp thing) (setq thing  (intern thing)))
  (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))

;; From thingatpt+
(defun conn--things (predicate)
  (let (types)
    (mapatoms
     (lambda (tt)
       (when (funcall predicate tt) (push (symbol-name tt) types))))
    (dolist (typ  '("thing" "buffer" "point")) ; Remove types that do not make sense.
      (setq types (delete typ types)))
    (sort types #'string-lessp)))

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
  (let* ((modes (conn--thread mode 'major-mode
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
                           (substring (string-trim (buffer-substring-no-properties beg end)) 0 20)))
         (buffer   (clone-indirect-buffer-other-window name nil)))
    (pop-to-buffer buffer)
    (narrow-to-region beg end)
    (deactivate-mark)))


;;;; Extensions

(defvar conn--extensions nil)

(defun conn--setup-extensions ()
  "Run when `conn-mode' is turned on or off to turn shims on or off."
  (run-hook-with-args 'conn--extensions conn-mode))

(defmacro conn-define-extension (name &rest body)
  "Define a Conn extension.

\(fn NAME [DOCSTRING] &rest body)"
  (declare (indent 1))
  (let (doc)
    (when (stringp (car body))
      (setq doc (pop body)))
    `(progn
       (defvar ,name nil)

       (defun ,name (enable)
         ,(or doc "")
         (interactive (list (not ,name)))
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
               (remove-hook 'conn--extensions fn)))))

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

;;;;; Repeat Cursor Extension

(let (original-cursor-color)
  (defun conn--repeat-cursor-message-ad (map)
    (when (and map (not original-cursor-color))
      (setq original-cursor-color (face-background 'cursor)))
    (modify-all-frames-parameters
     `((cursor-color . ,(if map
                            conn-repeating-cursor-color
                          original-cursor-color))))
    (unless map (setq original-cursor-color nil))))

(conn-define-extension conn-repeat-cursor
  "Change the cursor color when a repeat map is active."
  (if conn-repeat-cursor
      (add-function :after repeat-echo-function 'conn--repeat-cursor-message-ad)
    (remove-function repeat-echo-function 'conn--repeat-cursor-message-ad)))


;;;; Mark

(defmacro conn--thing-expander-command (thing)
  (let ((name (conn--symbolicate "conn-" thing "-expand")))
    `(progn
       (defun ,name (N)
         (interactive "p")
         (activate-mark)
         (let ((end (region-end)))
           (if (= (point) end)
               (progn
                 (forward-thing ',thing N)
                 (save-excursion
                   (conn-exchange-mark-command)
                   (forward-thing ',thing (- N))
                   (conn--push-ephemeral-mark)))
             (progn
               (save-excursion
                 (conn-exchange-mark-command)
                 (forward-thing ',thing N)
                 (conn--push-ephemeral-mark))
               (forward-thing ',thing (- N))))))
       (put ',name :conn-command-thing ',thing)
       ',name)))

(defmacro conn--thing-bounds-command (thing)
  (let ((name (conn--symbolicate "conn-mark-" thing)))
    `(progn
       (defun ,name ()
         (interactive)
         (pcase (bounds-of-thing-at-point ',thing)
           (`(,beg . ,end)
            (goto-char beg)
            (conn--push-ephemeral-mark end))))
       (put ',name :conn-command-thing ',thing)
       ',name)))

(defmacro conn-register-thing (thing &rest rest)
  "Define a new thing.

\(fn THING &key FORWARD-OP BEG-OP END-OP BOUNDS-OP COMMANDS MODES MARK-KEY EXPAND-KEY)"
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
    (when-let ((commands (plist-get rest :commands)))
      (let ((cmds (gensym "commands")))
        `((let ((,cmds ,commands))
            ,@(nconc
               `((dolist (cmd ,cmds)
                   (put cmd :conn-command-thing ',thing)))
               (when-let ((handler (plist-get rest :handler)))
                 `((conn-set-mark-handler ,cmds ,handler))))))))
    (when-let ((binding (plist-get rest :mark-key)))
      (if-let ((modes (ensure-list (plist-get rest :modes))))
          (let ((forms))
            (dolist (mode modes)
              (push
               `(setf (alist-get ,binding (get ',mode :conn-mode-things)
                                 nil nil #'equal)
                      (conn--thing-bounds-command ,thing))
               forms))
            forms)
        `((keymap-set conn-mark-thing-map ,binding
                      (conn--thing-bounds-command ,thing)))))
    (when-let ((_ (or (get thing 'forward-op)
                      (plist-get rest :forward-op)))
               (binding (plist-get rest :expand-key)))
      (if-let ((modes (ensure-list (plist-get rest :modes))))
          (let ((forms))
            (dolist (mode ',modes)
              (push
               `(setf (alist-get ,binding (get ',mode :conn-mode-things)
                                 nil nil #'equal)
                      (conn--thing-expander-command ,thing))
               forms))
            forms)
        `((keymap-set conn-mark-thing-map ,binding
                      (conn--thing-expander-command ,thing))))))))

(defmacro conn-define-thing-handler (name args &rest rest)
  "Define a thing movement command mark handler constructor.

Defines a constructor function NAME which takes THING as its first
argument and any number of addition optional ARGS.  The first time
NAME is called with a unique THING it creates a closure over
LAMBDA-FORM, assigns it to the function value of an uninterned symbol
and associates that symbol with THING.  The symbol associated with
THING is always returned.

\(fn NAME (THING &rest ARGS) [DOCSTRING] LAMBDA-FORM)"
  (declare (indent defun))
  (let ((docstring (if (stringp (car rest)) (pop rest) ""))
        (lambda-form (car rest))
        (sym (gensym "sym"))
        (ts (gensym "tsymbol"))
        (ss (gensym "ssymbol"))
        (thing (car args)))
    `(progn
       (defvar ,name)
       (if (boundp ',name)
           (conn--thread needle
               (pcase-lambda  (`(,,ts . ,,ss))
                 (let ((,thing ,ts))
                   (fset ,ss ,lambda-form)
                   (cons ,ss ,thing)))
             (mapcar needle ,name)
             (setf ,name needle))
         (setq ,name nil))

       (defun ,name ,args
         ,docstring
         (or (alist-get ,thing ,name)
             (let ((,sym (make-symbol (conn--stringify ',name "-" ,thing))))
               (fset ,sym ,lambda-form)
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

(defun conn-set-mark-handler (commands handler)
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
  (when-let ((_ (memq conn-current-state conn-ephemeral-mark-states))
             (handler (conn--command-property :conn-mark-handler)))
    (setq conn-this-thing-handler handler
          conn-this-thing-start (point))))

(defun conn--mark-post-command-hook ()
  (with-demoted-errors "error marking thing: %s"
    (when conn-this-thing-handler
      (funcall conn-this-thing-handler conn-this-thing-start)))
  (setq conn-this-thing-handler nil
        conn-this-thing-start nil))

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
    (dolist (ov (conn--all-overlays 'conn--mark-cursor-p (point-min) (point-max)))
      (delete-overlay ov)))
  (setq conn--mark-cursor nil))


;;;; Macro Dispatch

(defun conn--dispatch-multi-buffer (iterator &optional init finalize)
  (let ((dispatch-undo-handles nil))
    (lambda (&optional state)
      (when (eq state :finalize)
        (if conn--dispatch-error
            (pcase-dolist (`(,buffer . ,handle) dispatch-undo-handles)
              (cancel-change-group handle)
              (when finalize
                (with-current-buffer buffer (funcall finalize))))
          (pcase-dolist (`(,buffer . ,handle) dispatch-undo-handles)
            (accept-change-group handle)
            (undo-amalgamate-change-group handle)
            (when finalize
              (with-current-buffer buffer (funcall finalize))))))
      (pcase (funcall iterator state)
        ((and `(,beg . ,end)
              (let buffer (marker-buffer beg))
              ret)
         (cond ((not (eq buffer (marker-buffer end)))
                (error "Markers point to different buffers"))
               ((not (eq buffer (current-buffer)))
                (pop-to-buffer-same-window buffer)
                (if (and (not (eq buffer (window-buffer (selected-window))))
                         (eq (current-buffer) buffer))
                    (error "Could not switch to buffer %s" buffer)
                  ret))
               (t
                (unless (alist-get (current-buffer) dispatch-undo-handles)
                  (let ((handle (setf (alist-get (current-buffer) dispatch-undo-handles)
                                      (prepare-change-group))))
                    (activate-change-group handle))
                  (when init (funcall init)))
                ret)))
        (ret ret)))))

(defun conn--dispatch-single-buffer (iterator &optional init finalize)
  (let ((handle nil))
    (lambda (&optional state)
      (cond ((eq state :finalize)
             (if conn--dispatch-error
                 (cancel-change-group handle)
               (accept-change-group handle)
               (undo-amalgamate-change-group handle))
             (when finalize (funcall finalize)))
            ((not handle)
             (setq handle (prepare-change-group))
             (activate-change-group handle)
             (when init (funcall init))))
      (funcall iterator state))))

(defun conn--dispatch-with-state (iterator transition)
  (let ((buffer-states nil))
    (lambda (&optional state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,buf ,state ,prev-state) buffer-states)
           (when state
             (with-current-buffer buf
               (funcall state)
               (setq conn-previous-state prev-state)))))
        ((let ret (funcall iterator state))
         (unless (alist-get (current-buffer) buffer-states)
           (setf (alist-get (current-buffer) buffer-states)
                 (list conn-current-state conn-previous-state)))
         (funcall transition)
         ret)))))

(defun conn--region-iterator (regions &optional at-end)
  (lambda (&optional state)
    (if (eq state :finalize)
        (pcase-dolist (`(,beg . ,end) regions)
          (set-marker beg nil)
          (set-marker end nil))
      (if at-end
          (pcase (pop regions)
            (`(,beg . ,end) (cons end beg)))
        (pop regions)))))

(defun conn--pulse-on-record (iterator)
  (lambda (&optional state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,end)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-region beg end 'conn-pulse-face)
       ret)
      (ret ret))))

(defun conn--dot-iterator (dots)
  (dolist (dot dots)
    (overlay-put dot 'evaporate nil))
  (lambda (&optional state)
    (if (eq state :finalize)
        (when dots
          (dolist (dot dots)
            (overlay-put dot 'evaporate t)))
      (pop dots))))

(defun conn--dispatch-relocate-dots (iterator)
  (let (primed new-dots old-dots)
    (lambda (&optional state)
      (if (eq state :finalize)
          (progn
            (if conn--dispatch-error
                (pcase-dolist (`(,buffer . ,dots) old-dots)
                  (with-current-buffer buffer
                    (apply 'conn--create-dots dots)))
              (pcase-dolist (`(,buffer . ,dots) new-dots)
                (with-current-buffer buffer
                  (apply 'conn--create-dots dots))))
            (pcase-dolist (`(,_ . ,dots) new-dots)
              (pcase-dolist (`(,beg . ,end) dots)
                (set-marker beg nil)
                (set-marker end nil)))
            (pcase-dolist (`(,_ . ,dots) old-dots)
              (pcase-dolist (`(,beg . ,end) dots)
                (set-marker beg nil)
                (set-marker end nil)))
            (funcall iterator state))
        (if primed
            (push (cons (conn--create-marker (region-beginning)
                                             (current-buffer))
                        (conn--create-marker (region-end)
                                             (current-buffer)))
                  (alist-get (current-buffer) new-dots))
          (setq primed t))
        (pcase (funcall iterator state)
          ((and (pred conn-dotp) dot)
           (let ((beg (conn--create-marker (overlay-start dot)
                                           (overlay-buffer dot)))
                 (end (conn--create-marker (overlay-end dot)
                                           (overlay-buffer dot))))
             (push (cons beg end) (alist-get (overlay-buffer dot) old-dots))
             (conn--delete-dot dot)
             (cons (copy-marker beg) (copy-marker end))))
          (ret ret))))))

(defun conn--dispatch-stationary-dots (iterator)
  (lambda (&optional state)
    (pcase (funcall iterator state)
      ((and (pred conn-dotp) dot)
       (let ((beg (conn--create-marker (overlay-start dot)
                                       (overlay-buffer dot)))
             (end (conn--create-marker (overlay-end dot)
                                       (overlay-buffer dot))))
         (cons beg end)))
      (ret ret))))

(defun conn--dispatch-at-end (iterator)
  (lambda (&optional state)
    (pcase (funcall iterator state)
      (`(,beg . ,end) (cons end beg))
      (ret ret))))

(defun conn--macro-dispatch (iterator &optional kmacro append)
  (let* ((undo-outer-limit nil)
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (conn-macro-dispatch-p t)
         (conn--dispatch-error nil)
         (sym (make-symbol "conn--kmacro-iterator"))
         (start (point)))
    (fset sym (lambda (&optional state)
                (pcase (funcall iterator state)
                  (`(,beg . ,end)
                   (goto-char beg)
                   (when (markerp beg) (set-marker beg nil))
                   (conn--push-ephemeral-mark end)
                   (when (markerp end) (set-marker end nil))
                   t))))
    (advice-add #'kmacro-loop-setup-function :before-while sym)
    (unwind-protect
        (condition-case err
            (pcase kmacro
              ((pred kmacro-p)
               (funcall kmacro 0))
              ((pred identity)
               (kmacro-call-macro 0 nil nil kmacro))
              ('nil
               (deactivate-mark t)
               (pcase (funcall iterator :record)
                 (`(,beg . ,end)
                  (goto-char beg)
                  (conn--push-ephemeral-mark end))
                 (_ (error "iterator unknown return")))
               (kmacro-start-macro append)
               (unwind-protect
                   (recursive-edit)
                 (if (not defining-kbd-macro)
                     (user-error "Not defining keyboard macro")
                   (kmacro-end-macro 0)))))
          (t
           (setq conn--dispatch-error err)
           (signal (car err) (cdr err)))
          (:success
           (let ((mark (mark t)))
             (push-mark start t nil)
             (conn--push-ephemeral-mark mark))))
      (advice-remove 'kmacro-loop-setup-function sym)
      (funcall iterator :finalize))))


;;;; Dots

;;;;; Dot Registers

(cl-defstruct (conn-dot-register (:constructor %conn--make-dot-register (data)))
  (data nil :read-only t))

(defun conn--make-dot-register ()
  (let ((buffers (mapcar #'get-buffer (conn-read-dot-buffers)))
        dots curr)
    (dolist (buf buffers)
      (setq curr (list buf))
      (with-current-buffer buf
        (conn--for-each-dot
         (lambda (dot)
           (push (cons (conn--create-marker (overlay-start dot) buf)
                       (conn--create-marker (overlay-end dot) buf))
                 (cdr curr)))))
      (push curr dots))
    (%conn--make-dot-register dots)))

(cl-defmethod register-val-jump-to ((val conn-dot-register) _arg)
  (pcase-dolist (`(,buf . ,dots) (conn-dot-register-data val))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (conn--remove-dots (point-min) (point-max))
        (apply #'conn--create-dots dots)))))

(cl-defmethod register-val-describe ((val conn-dot-register) _arg)
  (princ (format "dot state in buffers:\n   %s"
                 (mapcar (lambda (buf)
                           (buffer-name (car buf)))
                         (conn-dot-register-data val)))))

(defun conn-dot-state-to-register (register)
  "Store current dots in REGISTER."
  (interactive (list (register-read-with-preview "Dot state to register: ")))
  (set-register register (conn--make-dot-register)))

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

(defun conn--sorted-overlays (typep &optional predicate start end buffer)
  "Get all dots between START and END sorted by starting position."
  (unless predicate (setq predicate #'<))
  (let ((overlays (conn--all-overlays typep start end buffer)))
    (pcase predicate
      ('< overlays)
      ('> (nreverse overlays))
      (_ (sort overlays predicate)))))

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
  (unless (or conn--dot-undoing conn-dot-macro-dispatch-p)
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
  (when (and (memq keymap (conn--without-conn-maps (current-active-maps t)))
             (member (if (stringp key) (key-parse key) key)
                     (mapcar #'symbol-value conn--aux-bindings)))
    (setq conn--aux-update-flag t)))

(defun conn--pop-to-mark-command-ad (&rest _)
  (unless (or (null (mark t))
              (= (point) (mark)))
    (add-to-history 'conn--unpop-ring
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
  (let ((selectors)
        (keymap))
    (dolist (mode local-minor-modes)
      (setq selectors (nconc (get mode :conn-mode-things)
                             selectors)))
    (dolist (mode (derived-mode-all-parents major-mode))
      (setq selectors (nconc (get mode :conn-mode-things) selectors)))
    (when selectors
      (setq keymap (make-sparse-keymap)
            selectors (nreverse selectors))
      (pcase-dolist (`(,binding . ,command) selectors)
        (keymap-set keymap binding command))
      keymap)))

(defun conn--generate-aux-map ()
  (let ((aux-map (setf (alist-get conn-current-state conn--aux-maps)
                       (make-sparse-keymap)))
        (state-map (alist-get conn-current-state conn--state-maps)))
    (conn--without-conn-maps
      (dolist (sentinal conn--aux-bindings)
        (when-let ((def (key-binding (symbol-value sentinal) t)))
          (dolist (key (where-is-internal sentinal (list state-map) nil t))
            (define-key aux-map key def)))))
    (let ((mark-map (conn--modes-mark-map)))
      (dolist (key (where-is-internal 'conn-mark-thing-map (list state-map) nil t t))
        (define-key aux-map key mark-map)))
    aux-map))

(defun conn--update-aux-map (&optional force)
  (when (and conn-local-mode
             conn-current-state
             (not conn-emacs-state))
    (pcase-let ((active (conn--without-conn-maps (current-active-maps t)))
                (current-remappings (mapcar #'symbol-value conn--aux-bindings)))
      (cond
       ((or conn--aux-update-flag
            (not (equal conn--last-remapping current-remappings))
            force)
        (let ((aux-map (conn--generate-aux-map))
              (key (cons conn-current-state active)))
          (setf (alist-get conn-current-state conn--aux-maps) aux-map
                conn--aux-map-history (list (cons key aux-map)))))
       (t
        (let* ((key (cons conn-current-state active))
               (aux-map (or (alist-get key conn--aux-map-history nil nil #'equal)
                            (conn--generate-aux-map)))
               (new (cons key aux-map)))
          (setf (alist-get conn-current-state conn--aux-maps) aux-map
                conn--aux-map-history (conn--thread needle
                                          conn--aux-map-history
                                        (delete new needle)
                                        (cons new needle)
                                        (seq-take needle 8))))))
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
(conn-define-remapping-command conn-switch-buffer-keys      "C-x b" t)
(conn-define-remapping-command conn-switch-tab-keys         "C-x t s" t)
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

;;;;; Buffer Color Mode

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

;;;;; Cursor Color Mode

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
  "Make a MODE ignore conn state input method supression.
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
        ('nil (setq conn--input-method current-input-method))
        ((guard (and current-input-method conn--input-method))
         (deactivate-input-method))
        ((guard current-input-method)
         (setq conn--input-method current-input-method)
         (deactivate-input-method))))))

(defun conn--deactivate-input-method ()
  "Disable input method in all states."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (setq conn--input-method nil)))

(defun conn--default-state-for-buffer (&optional buffer)
  "Get default state for BUFFER."
  (or (cdr (assoc (buffer-name buffer)
                  (buffer-local-value 'conn-buffer-default-state-alist
                                      (or buffer (current-buffer)))
                  #'buffer-match-p))
      (conn--derived-mode-property :conn-default-state buffer)
      conn-default-state))

(defun set-default-conn-state (modes-or-buffers state)
  "Set default STATE for each MODES-OR-BUFFERS.

Modes are symbols tested against `major-mode'.
Buffers are strings matched using `buffer-match-p'."
  (dolist (var (ensure-list modes-or-buffers))
    (cl-etypecase var
      (symbol (put var :conn-default-state state))
      (string (push (cons var state) conn-buffer-default-state-alist)))))

(defmacro conn-define-state (name doc &rest body)
  "Define a conn state NAME.
Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.  DOC is required.

:INDICATOR is a mode-line construct that will be displayed on the left
of the mode-line in state NAME.  Indicator is only displayed when
`conn-mode-line-indicator-mode' is non-nil.

:LIGHTER-FACE is the face for the conn mode-line lighter in state NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in state NAME.

:KEYMAP is a keymap for the state.

:CURSOR is the `cursor-type' for state NAME.

:TRANSITIONS is a list of transition key bindings to be bound in NAME's
transition map.  It is of the form ((KEY . TRANSITION-FUNCTION) ...).

:EPHEMERAL-MARKS if non-nil thing movement commands will push ephemeral marks
while in state NAME.

:BUFFER-FACE is the default face for the buffer while in state NAME.
only has an effect when `conn-buffer-colors' is non-nil.

BODY contains code to be executed each time the transition function is executed.

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
  :cursor-face ((default              (:background "#00517d"))
                 (((background light)) (:background "#00517d"))
                 (((background dark))  (:background "#b6d6e7")))
  :buffer-face ((t :inherit default))
  :indicator " E "
  :cursor box
  :ephemeral-marks nil
  :transitions (define-keymap "<escape>" 'conn-state))

(conn-define-state conn-state
  "Activate `conn-state' in the current buffer.
A `conn-mode' state for editing text.

See `conn-state-transition-map' for keybindings to enter other states
from conn state.  See `conn-state-map' for commands bound by conn state."
  :lighter-face ((default              (:inherit mode-line :background "#f3bdbd"))
                 (((background light)) (:inherit mode-line :background "#f3bdbd"))
                 (((background dark))  (:inherit mode-line :background "#8c3c3c")))
  :cursor-face ((default              (:background "#7d0002"))
                 (((background light)) (:background "#7d0002"))
                 (((background dark))  (:background "#eba4a4")))
  :buffer-face ((t :inherit default :background "#f7eee1"))
  :suppress-input-method t
  :indicator " C "
  :ephemeral-marks t
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (define-keymap
                 "f"        'conn-emacs-state
                 "<escape>" 'conn-dot-state
                 "\\"       'conn-region-dispatch
                 "|"        'conn-region-dispatch-menu
                 "t"        'conn-change
                 "F i"      'conn-emacs-state-open-line-above
                 "F k"      'conn-emacs-state-open-line
                 "F l"      'conn-emacs-state-eol
                 "F j"      'conn-emacs-state-bol
                 "F o"      'conn-emacs-state-overwrite
                 "F u"      'conn-emacs-state-overwrite-binary
                 "M-TAB"    'conn-emacs-state-and-complete
                 "M-<tab>"  'conn-emacs-state-and-complete))

(set-default-conn-state '(prog-mode text-mode conf-mode) 'conn-state)

(conn-define-state conn-dot-state
  "Activate `conn-dot-state' in the current buffer.
A `conn-mode' state for dispatching keyboard macros on buffer regions.

See `conn-dot-state-transition-map' for keybindings to enter other states
from dot state.  See `conn-dot-state-map' for commands bound by dot state."
  :lighter-face ((default              (:inherit mode-line :background "#d1ead5"))
                 (((background light)) (:inherit mode-line :background "#d1ead5"))
                 (((background dark))  (:inherit mode-line :background "#4f7555")))
  :buffer-face ((t :inherit default :background "#f6fff9"))
  :cursor-face ((default              (:background "#267d00"))
                 (((background light)) (:background "#267d00"))
                 (((background dark))  (:background "#b2e5a6")))
  :suppress-input-method t
  :indicator " D "
  :ephemeral-marks t
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (define-keymap
                 "<escape>" 'conn-state
                 "\\"       'conn-dots-dispatch
                 "|"        'conn-dots-dispatch-menu
                 "f"        'conn-emacs-state
                 "Q"        'conn-dot-quit)
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
  :cursor-face ((default              (:background "#7d0077"))
                 (((background light)) (:background "#7d0077"))
                 (((background dark))  (:background "#f1b9ee")))
  :suppress-input-method t
  :indicator (:propertize " T " face conn-org-tree-edit-state-lighter-face)
  :keymap (define-keymap :suppress t)
  :transitions (define-keymap
                 "<escape>" 'conn-state
                 "f"        'conn-emacs-state
                 "F i"      'conn-emacs-state-open-line-above
                 "F k"      'conn-emacs-state-open-line
                 "F l"      'conn-emacs-state-eol
                 "F j"      'conn-emacs-state-bol
                 "F o"      'conn-emacs-state-overwrite
                 "F u"      'conn-emacs-state-overwrite-binary))
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
  (princ (format "Tab:\n   %s"
                 (when-let ((index (conn--get-tab-index-by-cookie
                                    (conn-tab-register-cookie val))))
                   (conn--thread needle
                       index
                     (nth needle (funcall tab-bar-tabs-function))
                     (alist-get 'name needle))))))

(defun conn-tab-to-register (register)
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

(defun conn-yank-to-dots (&optional at-end)
  "Yank from `kill-ring' at the front of each dot.
If AT-END is non-nil yank at the end of each dot instead."
  (interactive "P")
  (save-mark-and-excursion
    (if at-end
        (thread-first
          (conn--sorted-overlays #'conn-dotp '<)
          (conn--dot-iterator)
          (conn--dispatch-stationary-dots)
          (conn--dispatch-at-end)
          (conn--dispatch-single-buffer)
          (conn--dispatch-with-state 'conn-emacs-state)
          (conn--pulse-on-record)
          (conn--macro-dispatch conn-yank-keys))
      (thread-first
        (conn--sorted-overlays #'conn-dotp '<)
        (conn--dot-iterator)
        (conn--dispatch-stationary-dots)
        (conn--dispatch-single-buffer)
        (conn--dispatch-with-state 'conn-emacs-state)
        (conn--pulse-on-record)
        (conn--macro-dispatch conn-yank-keys)))))

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
  "Undo last dot change."
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
  "Redo last dot change."
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

(defun conn-first-dot (&optional start)
  "Go to the end of the first dot in buffer.
If start is non-nil go to the start of last dot instead."
  (interactive "P")
  (when-let ((dot (save-excursion
                    (goto-char (point-min))
                    (conn--next-dot-1))))
    (if start
        (progn
          (goto-char (overlay-start dot))
          (conn--push-ephemeral-mark (overlay-end dot)))
      (goto-char (overlay-end dot))
      (conn--push-ephemeral-mark (overlay-start dot)))))

(defun conn-last-dot (&optional start)
  "Go to the end of the last dot in buffer.
If start is non-nil go to the start of last do instead."
  (interactive "P")
  (when-let ((dot (save-excursion
                    (goto-char (point-max))
                    (conn--previous-dot-1)
                    (when-let ((ov (conn--dot-after-point (point))))
                      (goto-char (overlay-end ov))))))
    (if start
        (progn
          (goto-char (overlay-start dot))
          (conn--push-ephemeral-mark (overlay-end dot)))
      (goto-char (overlay-end dot))
      (conn--push-ephemeral-mark (overlay-start dot)))))

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

(defun conn-dot-word-at-point ()
  "Dot the word at point."
  (interactive)
  (pcase (bounds-of-thing-at-point 'word)
    (`(,beg . ,end)
     (conn-add-dots-matching-regexp
      (concat "\\b" (regexp-quote (buffer-substring beg end)) "\\b")))))

(defun conn-dot-sexp-at-point ()
  "Dot the s-expression at point."
  (interactive)
  (pcase (bounds-of-thing-at-point 'sexp)
    (`(,beg . ,end)
     (conn-add-dots-matching-regexp
      (concat "\\_<" (regexp-quote (buffer-substring beg end)) "\\_>")))))

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
  (interactive (list (read-string "String: ")
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
  (interactive (list (read-regexp "Regexp: ")
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

(defun conn-add-dots-matching-region (start end &optional refine)
  "Dot all occurrences of string within region from START to END.
If REFINE is non-nil only dot occurrences in dots.

When called interactively uses point and mark."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (conn-add-dots-matching-literal
   (buffer-substring-no-properties start end) nil nil refine))

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

(defun conn-split-dots-on-newline (start end)
  "Split dots in region START to END on newlines.

When region is active operates within `region-bounds', otherwise operates
between `point-min' and `point-max'."
  (interactive (list (conn--beginning-of-region-or-restriction)
                     (conn--end-of-region-or-restriction)))
  (conn-split-dots-on-regexp "\n" start end))

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

(defun conn-kill-to-dots (start end)
  "Kill region from START to END and insert region to each dot.
When called interactively START and END default to point and mark."
  (interactive (list (region-beginning)
                     (region-end)))
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (conn--for-each-dot
     (lambda (dot)
       (save-mark-and-excursion
         (goto-char (overlay-start dot))
         (insert str))))))

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
  "Prompt to keep each dot."
  (interactive)
  (save-excursion
    (conn--for-each-dot
     (lambda (dot)
       (goto-char (overlay-start dot))
       (unless (y-or-n-p "Keep this dot?")
         (conn--delete-dot dot)))
     #'<)))

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

(defun conn--read-thing-command ()
  (with-temp-message ""
    (let ((key (conn--thread key
                   (read-key-sequence "Movement Command:")
                 (key-description key)
                 (keymap-lookup nil key))))
      (while (not (get key :conn-command-thing))
        (when (eq 'keyboard-quit key) (keyboard-quit))
        (setq key (conn--thread key
                      (read-key-sequence "Not a valid movement command\nMovement Command:")
                    (key-description key)
                    (keymap-lookup nil key))))
      (get key :conn-command-thing))))

(defun conn-dot-all-things-in-region (thing)
  "Dot all THINGs in region.

THING is something with a forward-op as defined by thingatpt."
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

(defun conn--isearch-matches-in-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (if isearch-forward (point-min) (point-max)))
          (end (if isearch-forward (point-max) (point-min)))
          matches)
      (save-excursion
        (goto-char beg)
        (while (isearch-search-string isearch-string end t)
          (when (funcall isearch-filter-predicate
                         (match-beginning 0) (match-end 0))
            (push (cons (conn--create-marker (match-beginning 0))
                        (conn--create-marker (match-end 0)))
                  matches))))
      matches)))

(defun conn-isearch-dispatch ()
  "Macro dispatch on isearch matches."
  (interactive)
  (if (or (not (boundp 'multi-isearch-buffer-list))
          (not multi-isearch-buffer-list))
      (let ((regions (nreverse (conn--isearch-matches-in-buffer (current-buffer)))))
        (isearch-exit)
        (save-mark-and-excursion
          (thread-first
            (conn--region-iterator regions)
            (conn--dispatch-single-buffer)
            (conn--dispatch-with-state 'conn-state)
            (conn--pulse-on-record)
            (conn--macro-dispatch))))
    (let ((regions (mapcan 'conn--isearch-matches-in-buffer
                           multi-isearch-buffer-list)))
      (isearch-exit)
      (save-mark-and-excursion
        (save-window-excursion
          (thread-first
            (conn--region-iterator regions)
            (conn--dispatch-multi-buffer)
            (conn--dispatch-with-state 'conn-state)
            (conn--pulse-on-record)
            (conn--macro-dispatch)))))))

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
    (conn-dot-state)
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
  "Split region from START to END into dots on REGEXP."
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
      (conn--maybe-push-mark 'conn--unpop-ring (car mark-ring))
      (set-marker (car mark-ring) nil)
      (pop mark-ring)
      (goto-char (mark t)))
    (deactivate-mark)))

(defun conn-unpop-to-mark-command ()
  (interactive)
  (if (null (mark t))
      (user-error "No mark set in this buffer")
    (if (null conn--unpop-ring)
        (user-error "No marks to unpop")
      (conn--maybe-push-mark 'conn--unpop-ring)
      (conn--maybe-push-mark 'mark-ring (car conn--unpop-ring))
      (set-marker (car conn--unpop-ring) nil)
      (pop conn--unpop-ring)
      (goto-char (mark t)))
    (deactivate-mark)))

(defun conn-yank-region-to-minibuffer (&optional quote-function)
  "Yank region from `minibuffer-selected-window' into minibuffer.
Interactively defaults to the region in buffer."
  (interactive (list (pcase current-prefix-arg
                       ('(4) conn-completion-region-quote-function)
                       (_    'regexp-quote))))
  (insert (with-minibuffer-selected-window
            (funcall (or quote-function 'identity)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))))

(defun conn-toggle-sort-fold-case ()
  "Toggle the value of `sort-fold-case'."
  (interactive)
  (message "Sort fold case: %s"
           (setq sort-fold-case (not sort-fold-case))))

(defun conn-query-replace-region ()
  "Run `query-replace' with the region as initial contents."
  (interactive)
  (save-mark-and-excursion
    (unless (eq (point) (region-beginning))
      (conn-exchange-mark-command))
    (minibuffer-with-setup-hook
        (:append (lambda () (conn-yank-region-to-minibuffer)))
      (call-interactively #'query-replace))))

(defun conn-query-replace-regexp-region ()
  "Run `query-replace-regexp' with the region as initial contents.
Also ensure point is at START before running `query-replace-regexp'."
  (interactive)
  (save-mark-and-excursion
    (unless (eq (point) (region-beginning))
      (conn-exchange-mark-command))
    (minibuffer-with-setup-hook
        (:append (lambda () (conn-yank-region-to-minibuffer 'regexp-quote)))
      (call-interactively #'query-replace-regexp))))

(defun conn-dispatch-text-property (start end prop value &optional reverse)
  "Dispatch on text with text property PROP with value VALUE.
When called interatively the choices for PROP and VALUE are extracted
from the text properties at point."
  (interactive
   (let* ((start (conn--beginning-of-region-or-restriction))
          (end (conn--end-of-region-or-restriction))
          (prop (intern (completing-read
                         "Property: "
                         (cl-loop for prop in (text-properties-at (point))
                                  by #'cddr
                                  collect prop)
                         nil t)))
          (vals (mapcar (lambda (s) (cons (message "%s" s) s))
                        (ensure-list (get-text-property (point) prop))))
          (val (alist-get (completing-read "Value: " vals) vals
                          nil nil #'string=)))
     (list start end prop val current-prefix-arg)))
  (let* (regions)
    (save-excursion
      (with-restriction
          start end
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward prop value t))
            (push (cons (prop-match-beginning match)
                        (prop-match-end match))
                  regions)))))
    (save-window-excursion
      (conn--thread regions
          (if reverse (nreverse regions) regions)
        (conn--region-iterator regions)
        (conn--dispatch-single-buffer regions nil)
        (conn--dispatch-with-state regions 'conn-state)
        (conn--pulse-on-record regions)
        (conn--macro-dispatch regions)))))

(defun conn-macro-at-point-and-mark ()
  "Dispatch dot macro at point and mark."
  (interactive)
  (save-mark-and-excursion
    (thread-first
      (list (cons (point-marker) (point-marker))
            (cons (conn--create-marker (mark t))
                  (conn--create-marker (mark t))))
      (conn--region-iterator)
      (conn--dispatch-single-buffer)
      (conn--dispatch-with-state conn-current-state)
      (conn--macro-dispatch))))

(defun conn-scroll-down (&optional arg)
  "`scroll-down-command' leaving point at the same relative window position."
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (progn (beep) (message "Beginning of buffer"))
    (scroll-down arg)))

(defun conn-scroll-up (&optional arg)
  "`scroll-up-command' leaving point at the same relative window position."
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (scroll-up arg)))

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
  "Narrow to the region between `point' and `point-max'."
  (interactive)
  (conn--narrow-indirect (point) (point-max)))

(defun conn-narrow-to-beginning-of-buffer ()
  "Narrow to the region between `point-min' and `point'."
  (interactive)
  (narrow-to-region (point-min) (point)))

(defun conn-narrow-to-beginning-of-buffer-indirect ()
  "Narrow to the region between `point-min' and `point'."
  (interactive)
  (conn--narrow-indirect (point-min) (point)))

(defun conn-isearch-backward-symbol-at-point ()
  "Isearch backward for symbol at point."
  (interactive)
  (funcall-interactively #'isearch-forward-symbol-at-point -1)
  (isearch-repeat-backward))
(put 'conn-isearch-backward-symbol-at-point 'repeat-map 'conn-isearch-repeat-map)

(defun conn-command-at-point-and-mark ()
  "Run the next command at both the point and the mark."
  (interactive)
  (let ((before (make-symbol "conn-at-point-and-mark-before"))
        (after (make-symbol "conn-at-point-and-mark-after"))
        (prefix current-prefix-arg)
        command)
    (fset before (lambda ()
                   (remove-hook 'pre-command-hook before t)
                   (add-hook 'post-command-hook after -90 t)
                   (setq prefix-arg prefix
                         command this-command)))
    (fset after (lambda ()
                  (remove-hook 'post-command-hook after t)
                  (let ((current-prefix-arg prefix))
                    (save-mark-and-excursion
                      (exchange-point-and-mark (not mark-active))
                      (call-interactively command)))))
    (add-hook 'pre-command-hook before -80 t)))

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
(put 'conn-isearch-region-forward 'repeat-map 'conn-isearch-repeat-map)

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
(put 'conn-isearch-region-backward 'repeat-map 'conn-isearch-repeat-map)

(defun conn-org-tree-edit-insert-heading ()
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content))

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

(defun conn-backward-char (string arg)
  "Behaves like `backward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for STRING and search backward for nearest
occurrence of STRING.  STRING will finish reading after
`conn--read-string-timout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout
                         conn--read-string-timout 'backward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (backward-char arg)
    (setq this-command 'conn-goto-string-backward)
    (conn-goto-string-backward string)))

(defun conn-goto-string-backward (string &optional interactive)
  (interactive
   (list (conn--read-string-with-timeout
          conn--read-string-timout 'backward)
         current-prefix-arg))
  (with-restriction (window-start) (window-end)
    (when-let ((pos (or (save-excursion
                          (backward-char)
                          (and (search-backward string nil t)
                               (match-beginning 0)))
                        (user-error "\"%s\" not found." string))))
      (unless (and (eq this-command last-command)
                   (equal string (get 'conn-goto-string-backward
                                      :last-string)))
        (push-mark nil t)
        (put 'conn-goto-string-backward :last-string string))
      (goto-char pos))))

(defun conn-forward-char (string arg)
  "Behaves like `forward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for STRING and search forward for nearest
occurrence of STRING.  STRING will finish reading after
`conn--read-string-timout' seconds.
This command should only be called interactively."
  (declare (interactive-only t))
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4))
                        (conn--read-string-with-timeout
                         conn--read-string-timout 'forward)))
                     (prefix-numeric-value current-prefix-arg)))
  (if (null string)
      (forward-char arg)
    (setq this-command 'conn-goto-string-forward)
    (conn-goto-string-forward string)))

(defun conn-goto-string-forward (string)
  (interactive
   (list (conn--read-string-with-timeout
          conn--read-string-timout 'forward)))
  (with-restriction (window-start) (window-end)
    (when-let ((pos (or (save-excursion
                          (forward-char)
                          (and (search-forward string nil t)
                               (match-beginning 0)))
                        (user-error "\"%s\" not found." string))))
      (unless (and (eq this-command last-command)
                   (equal string (get 'conn-goto-string-forward
                                      :last-string)))
        (push-mark nil t)
        (put 'conn-goto-string-forward :last-string string))
      (goto-char pos))))

(defun conn-pop-state ()
  "Transition to the previous state."
  (interactive)
  (when conn-previous-state
    (funcall conn-previous-state)))

(defun conn-toggle-minibuffer-focus ()
  "Toggle input focus between minibuffer and `other-window'."
  (interactive)
  (cond ((not (active-minibuffer-window))
         (user-error "Minibuffer is not active"))
        ((eq (selected-window) (active-minibuffer-window))
         (when-let ((win (minibuffer-selected-window)))
           (select-window win)
           (message "Switched to %s" (current-buffer))))
        (t
         (select-window (active-minibuffer-window))
         (message "Switched to *MINIBUFFER*"))))

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
  "Mark THING at point."
  (interactive (list (conn--read-thing-command)))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (goto-char (cdr bounds))
    (conn--push-ephemeral-mark (car bounds))
    (activate-mark t)))

(defun conn-narrow-to-thing (thing)
  "Narrow indirect buffer to THING at point.
See `clone-indirect-buffer' for meaning of indirect buffer."
  (interactive (list (conn--read-thing-command)))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (narrow-to-region (car bounds) (cdr bounds))))

(defun conn-narrow-indirect-to-thing (thing)
  "Narrow to THING at point."
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
    (`(,str) (conn--thread needle
                 (lambda (char)
                   (pcase (alist-get char insert-pair-alist)
                     (`(,close . nil) (list char close))
                     (`(,open ,close) (list open close))
                     (_               (list char char))))
               (seq-map needle str)
               (apply #'seq-mapn 'string needle)
               (cons (car needle) (nreverse (cadr needle)))))
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
  (goto-char end)
  (funcall (conn-yank-keys))
  (if arg
      (kill-region start end)
    (delete-region start end)))

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
  "Go to point after the last non-whitespace and non-comment character in line.
Immediately repeating this command goes to the point at end of line proper."
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
      (setq conn-this-thing-handler
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
      (setq conn-this-thing-handler
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
         (conn--thread needle
             (concat "Kill "
                     (if rectangle-mark-mode "Rectangle " " ")
                     "to register:")
           (register-read-with-preview needle)
           (copy-to-register needle nil nil t t)))
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

(defun conn-switch-to-buffer-or-tab (&optional tab)
  "Calls `conn-switch-buffer-keys'.
With a prefix ARG calls `conn-switch-tab-keys'."
  (interactive "P")
  (let ((command (if tab (conn-switch-tab-keys) (conn-switch-buffer-keys))))
    (setq this-command command)
    (call-interactively command)))

;;;;; Window Commands

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
  (when (setq windows (seq-remove (lambda (win) (window-dedicated-p win))
                                  windows))
    (if (length= windows 1)
        (car windows)
      (let ((overlays (seq-map-indexed #'conn--create-window-prompt-overlay
                                       windows))
            num)
        (unwind-protect
            (progn
              (if (length> windows 10)
                  (while (and (not (setq num (read-number "Window: ")))
                              (>= num 0)
                              (length> windows num)))
                (while (and (not (setq num (- (read-char "Window: ") ?0)))
                            (>= num 0)
                            (length> windows num))))
              (nth num windows))
          (dolist (ov overlays)
            (delete-overlay ov)))))))

(defun conn-swap-windows (&optional no-select)
  "Swap selected window and another window.
If NO-SELECT is non-nil the window containing the buffer in the other
window will be selected at the end of this command.  Otherwise the
selected window will be the window containing the current buffer."
  (interactive "P")
  (when-let ((win1 (selected-window))
             (buf1 (window-buffer win1))
             (other-windows (remove win1 (conn--all-visible-windows)))
             (win2 (conn--prompt-for-window other-windows))
             (buf2 (window-buffer win2)))
    (set-window-buffer win2 buf1)
    (set-window-buffer win1 buf2)
    (unless no-select
      (select-window win2)
      (select-frame-set-input-focus (window-frame win2)))))

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

(defun conn-buffer-to-other-window ()
  "Send buffer in selected window to another window and `bury-buffer'."
  (interactive)
  (when-let ((win (thread-last
                    (window-list nil 'no-mini)
                    (remove (selected-window))
                    (conn--prompt-for-window))))
    (set-window-buffer win (current-buffer))
    (bury-buffer)))

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

(defvar conn--wincontrol-help-format)

(defvar conn--wincontrol-prev-eldoc-msg-fn)

(defcustom conn-wincontrol-initial-help 'window
  "Initial help message printed during `conn-wincontrol-mode'."
  :group 'conn-mode
  :type '(choice (const :tag "Window" window)
                 (const :tag "Frame" frame)
                 (const :tag "Short" nil)))

(defcustom conn-wincontrol-arg-limit 1000
  "Limit for prefix arg in `conn-wincontrol-mode'."
  :group 'conn-mode
  :type 'integer)

(defvar conn--wincontrol-window-format
  (concat
   (propertize "Win Control: " 'face 'bold)       "prefix arg: "
   (propertize "%d" 'face 'transient-value)       "; "
   (propertize "." 'face 'help-key-binding)       ": reset; "
   (propertize "h s w n" 'face 'help-key-binding) ": heighten/shorten/widen/narrow; "
   (propertize "H" 'face 'help-key-binding)       ": help; "
   (propertize "q" 'face 'help-key-binding)       ": quit"
   "\n"
   (propertize "i j k l" 'face 'help-key-binding) ": move; "
   (propertize "SPC DEL" 'face 'help-key-binding) ": scroll; "
   (propertize "u U" 'face 'help-key-binding)     ": un/bury; "
   (propertize "d D" 'face 'help-key-binding)     ": delete win/other; "
   (propertize "x t" 'face 'help-key-binding)     ": swap/throw buf"
   "\n"
   (propertize "m" 'face 'help-key-binding)   ": store; "
   (propertize "p" 'face 'help-key-binding)   ": load; "
   (propertize "c" 'face 'help-key-binding)   ": clone; "
   (propertize "v r" 'face 'help-key-binding) ": split vert/right; "
   (propertize "z Z" 'face 'help-key-binding) ": zoom; "
   (propertize "= +" 'face 'help-key-binding) ": balance/max; "
   (propertize "/ ?" 'face 'help-key-binding) ": undo/redo"))

(defvar conn--wincontrol-tab-and-frame-format
  (concat
   (propertize "Tab+Frame Control: " 'face 'bold) "prefix arg: "
   (propertize "%d" 'face 'transient-value)       "; "
   (propertize "." 'face 'help-key-binding)       ": reset; "
   (propertize "f" 'face 'help-key-binding)       ": fullscreen; "
   (propertize "H" 'face 'help-key-binding)       ": help; "
   (propertize "q" 'face 'help-key-binding)       ": quit"
   "\n"
   (propertize "J L" 'face 'help-key-binding)       ": tab next/prev; "
   (propertize "C-t g C-w" 'face 'help-key-binding) ": tab new/duplicate/close; "
   (propertize "o O" 'face 'help-key-binding)       ": tear off win/tab"
   "\n"
   (propertize "e" 'face 'help-key-binding)         ": tab store; "
   (propertize "C-d C-M-d" 'face 'help-key-binding) ": delete frame/other; "
   (propertize "C-/" 'face 'help-key-binding)       ": undelete; "
   (propertize "C" 'face 'help-key-binding)         ": clone"))

(defvar conn--wincontrol-simple-format
  (concat
   (propertize "Win Control: " 'face 'bold) "prefix arg: "
   (propertize "%d" 'face 'transient-value) "; "
   (propertize "H" 'face 'help-key-binding) ": help; "
   (propertize "q" 'face 'help-key-binding) ": quit"))

(defvar-keymap conn-wincontrol-map
  :suppress 'nodigits
  "q"   'conn-wincontrol-off
  "C-g" 'conn-wincontrol-off
  "a"   'conn-wincontrol-off
  "H"   'conn-wincontrol-toggle-help

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
  "-" 'conn-wincontrol-invert-argument
  "." 'conn-wincontrol-digit-argument-reset

  "w" 'conn-woncontrol-widen
  "n" 'conn-woncontrol-narrow
  "h" 'conn-woncontrol-heighten
  "s" 'conn-woncontrol-shorten

  "i" 'conn-wincontrol-windmove-up
  "j" 'conn-wincontrol-windmove-left
  "k" 'conn-wincontrol-windmove-down
  "l" 'conn-wincontrol-windmove-right

  "<up>"    'conn-wincontrol-windmove-up
  "<left>"  'conn-wincontrol-windmove-left
  "<down>"  'conn-wincontrol-windmove-down
  "<right>" 'conn-wincontrol-windmove-right

  "u" 'bury-buffer
  "U" 'unbury-buffer

  "b" 'conn-wincontrol-switch-buffer-or-tab

  "x" 'conn-wincontrol-swap-windows
  "t" 'conn-buffer-to-other-window

  "d"     'delete-window
  "C-d"   'delete-frame
  "D"     'delete-other-windows
  "C-M-d" 'delete-other-frames

  "o"   'tear-off-window
  "c"   'conn-wincontrol-clone-buffer
  "C"   'clone-frame

  "DEL"     'conn-wincontrol-scroll-down
  "M-TAB"   'conn-wincontrol-scroll-down
  "M-<tab>" 'conn-wincontrol-scroll-down
  "<prior>" 'conn-wincontrol-scroll-down

  "SPC"    'conn-wincontrol-scroll-up
  "TAB"    'conn-wincontrol-scroll-up
  "<tab>"  'conn-wincontrol-scroll-up
  "<next>" 'conn-wincontrol-scroll-up

  "v" 'conn-wincontrol-split-vertically
  "r" 'conn-wincontrol-split-right

  "z" 'text-scale-decrease
  "Z" 'text-scale-increase

  "m" 'window-configuration-to-register
  "p" 'conn-register-load

  "=" 'balance-windows
  "+" 'maximize-window
  "f" 'toggle-frame-fullscreen

  "/"   'tab-bar-history-back
  "?"   'tab-bar-history-forward
  "C-/" 'undelete-frame

  "J" 'tab-previous
  "L" 'tab-next

  "C-t" 'conn-wincontrol-tab-new
  "e"   'conn-tab-to-register
  "g"   'conn-wincontrol-tab-duplicate
  "O"   'conn-wincontrol-tab-detach
  "C-W" 'conn-wincontrol-tab-close)

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
    (message (pcase conn--wincontrol-help-format
               ('frame  conn--wincontrol-tab-and-frame-format)
               ('window conn--wincontrol-window-format)
               (_       conn--wincontrol-simple-format))
             conn--wincontrol-arg)))

(defun conn--wincontrol-setup ()
  (internal-push-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (add-hook 'post-command-hook 'conn--wincontrol-post-command)
  (add-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (setq conn--previous-scroll-conservatively scroll-conservatively
        conn--wincontrol-help-format conn-wincontrol-initial-help
        conn--wincontrol-prev-eldoc-msg-fn eldoc-message-function
        eldoc-message-function #'ignore
        scroll-conservatively 100
        conn--wincontrol-arg (mod (prefix-numeric-value current-prefix-arg)
                                  conn-wincontrol-arg-limit))
  (invert-face 'mode-line)
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (setq scroll-conservatively conn--previous-scroll-conservatively
        eldoc-message-function conn--wincontrol-prev-eldoc-msg-fn)
  (invert-face 'mode-line))

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn-wincontrol-mode 1)))

(defun conn--wincontrol-toggle-in-transient ()
  (let ((sym (make-symbol "transient-exit-hook-fn")))
    (fset sym (lambda ()
                (remove-hook 'transient-exit-hook sym)
                (conn-wincontrol-mode 1)))
    (add-hook 'transient-exit-hook sym))
  (conn-wincontrol-mode -1))

(defun conn--wincontrol-toggle-in-isearch ()
  (let ((sym (make-symbol "isearch-exit-hook-fn")))
    (fset sym (lambda ()
                (remove-hook 'isearch-mode-end-hook sym)
                (conn-wincontrol-mode 1)))
    (add-hook 'isearch-mode-end-hook sym))
  (conn-wincontrol-mode -1))

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

(defun conn-wincontrol-toggle-help ()
  "Cycle to the next `conn-wincontrol-mode' help message."
  (interactive)
  (setq conn--wincontrol-help-format
        (pcase conn--wincontrol-help-format
          ('frame  nil)
          ('window 'frame)
          (_       'window))))

(defun conn-wincontrol-scroll-down (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (let ((next-screen-context-lines arg))
    (conn-scroll-down)))

(defun conn-wincontrol-scroll-up (arg)
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive "p")
  (let ((next-screen-context-lines arg))
    (conn-scroll-up)))

(defun conn-wincontrol-widen ()
  "`enlarge-window-horizontally' by `wincontrol--prefix-arg' units.."
  (interactive)
  (enlarge-window-horizontally conn--wincontrol-arg))

(defun conn-wincontrol-narrow ()
  "`shrink-window-horizontally' by `wincontrol--prefix-arg' units."
  (interactive)
  (shrink-window-horizontally conn--wincontrol-arg))

(defun conn-wincontrol-heighten ()
  "`enlarge-window' by `wincontrol--prefix-arg' units."
  (interactive)
  (enlarge-window conn--wincontrol-arg))

(defun conn-wincontrol-shorten ()
  "`shrink-window' by `wincontrol--prefix-arg' units."
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

(defun conn-wincontrol-switch-buffer-or-tab (arg)
  "If arg is 1 `conn-switch-buffer-keys', else `conn-switch-tab-keys'."
  (interactive "p")
  (conn-switch-to-buffer-or-tab (not (= arg 1))))

(defun conn-wincontrol-tab-new ()
  "Create new tab.
See `tab-new'."
  (interactive)
  (tab-new))

(defun conn-wincontrol-tab-duplicate ()
  "Duplicate current tab.
See `tab-duplicate'"
  (interactive)
  (tab-duplicate))

(defun conn-wincontrol-tab-detach ()
  "Move current tab to a new frame.
See `tab-detach'."
  (interactive)
  (tab-detach))

(defun conn-wincontrol-tab-close ()
  "Close current tab.
See `tab-close'."
  (interactive)
  (tab-close))

;;;;; Transition Functions

(defun conn-emacs-state-and-complete ()
  (interactive)
  (conn-emacs-state)
  (completion-at-point))

(defun conn-dot-quit ()
  "Pop state and clear all dots."
  (interactive)
  (conn--remove-dots)
  (conn-pop-state))

(defun conn-region-dispatch (&optional reverse)
  "Macro dispatch on active region.
If REVERSE is non-nil dispatch from last to first region."
  (interactive "P")
  (let ((iterator
         (conn--thread regions
             (mapcar (pcase-lambda (`(,beg . ,end))
                       (cons (conn--create-marker beg)
                             (conn--create-marker end)))
                     (region-bounds))
           (if reverse (nreverse regions) regions)
           (conn--region-iterator regions)
           (conn--dispatch-single-buffer regions)
           (conn--dispatch-with-state regions conn-current-state)
           (conn--pulse-on-record regions))))
    (if rectangle-mark-mode-map
        (progn
          (save-mark-and-excursion
            (conn--macro-dispatch iterator))
          (deactivate-mark t))
      (conn--macro-dispatch iterator))))

(defun conn-dots-dispatch (&optional macro init-fn)
  "Begin recording dot macro for current buffer, initially in conn-state."
  (interactive)
  (save-window-excursion
    (thread-first
      (conn--sorted-overlays #'conn-dotp '<)
      (conn--dot-iterator)
      (conn--dispatch-relocate-dots)
      (conn--dispatch-single-buffer)
      (conn--dispatch-with-state (or init-fn 'conn-state))
      (conn--pulse-on-record)
      (conn--macro-dispatch macro))))

(defun conn-isearch-dots-dispatch ()
  "Exit isearch mode and `conn-dots-dispatch'."
  (interactive)
  (isearch-done)
  (call-interactively 'conn-dots-dispatch))

(defun conn-isearch-dots-dispatch-macro ()
  "Exit isearch mode and `conn-dots-dispatch-macro'."
  (interactive)
  (isearch-done)
  (call-interactively 'conn-dots-dispatch-menu))

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

(conn-set-mark-handler '(next-line previous-line) 'conn-jump-handler)

(conn-register-thing page
  :handler (conn-individual-thing-handler 'page)
  :mark-key "p"
  :forward-op 'forward-page
  :commands '(forward-page backward-page))

(conn-register-thing dot
  :handler (conn-individual-thing-handler 'dot)
  :expand-key "."
  :beg-op (lambda () (conn-previous-dot 1))
  :end-op (lambda () (conn-next-dot 1))
  :commands '(conn-next-dot conn-previous-dot))

(conn-register-thing word
  :handler (conn-sequential-thing-handler 'word)
  :expand-key "o"
  :forward-op 'forward-word
  :commands '(forward-word backward-word))

(conn-register-thing sexp
  :handler (conn-sequential-thing-handler 'sexp)
  :expand-key "m"
  :forward-op 'forward-sexp
  :commands '(forward-sexp backward-sexp))

(conn-register-thing sexp
  :handler (conn-individual-thing-handler 'sexp)
  :commands '(up-list backward-up-list))

(conn-register-thing whitespace
  :handler (conn-individual-thing-handler 'whitespace)
  :expand-key "S-SPC"
  :mark-key "SPC"
  :forward-op 'forward-whitespace
  :commands '(forward-whitespace conn-backward-whitespace))

(conn-register-thing sentence
  :handler (conn-sequential-thing-handler 'sentence)
  :forward-op 'forward-sentence
  :expand-key "{"
  :commands '(forward-sentence backward-sentence))

(conn-register-thing paragraph
  :handler (conn-sequential-thing-handler 'paragraph)
  :expand-key "K"
  :forward-op 'forward-paragraph
  :commands '(forward-paragraph backward-paragraph))

(conn-register-thing defun
  :handler (conn-sequential-thing-handler 'defun)
  :expand-key "M"
  :commands '(end-of-defun beginning-of-defun))

(conn-register-thing buffer
  :handler (conn-individual-thing-handler 'buffer)
  :bounds-op (lambda () (cons (point-min) (point-max)))
  :commands '(end-of-buffer beginning-of-buffer))

(conn-register-thing line
  :handler (conn-sequential-thing-handler 'line)
  :expand-key ">"
  :forward-op (lambda (N)
                (cond ((> N 0)
                       (forward-line N))
                      ((< N 0)
                       (let ((pt (point)))
                         (beginning-of-line)
                         (if (= pt (point))
                             (forward-line N)
                           (forward-line (1+ N)))))))
  :commands '(forward-line conn-backward-line))

(conn-register-thing outer-line
  :handler (conn-individual-thing-handler 'outer-line)
  :beg-op (lambda () (move-beginning-of-line nil))
  :end-op (lambda () (move-end-of-line nil))
  :commands '(move-beginning-of-line
              move-end-of-line))

(conn-register-thing inner-line
  :handler (conn-individual-thing-handler 'inner-line)
  :beg-op 'back-to-indentation
  :end-op 'conn--end-of-inner-line-1
  :commands '(back-to-indentation
              conn-beginning-of-inner-line
              conn-end-of-inner-line))


;;;; Transient Menus

(defun conn--kmacro-display (macro &optional trunc)
  (let* ((m (format-kbd-macro macro))
         (l (length m))
         (z (and trunc (> l trunc))))
    (format "%s%s%s"
            (if (= kmacro-counter 0)
                ""
              (format "[%s] "
                      (format kmacro-counter-format-start kmacro-counter)))
            (if z (substring m 0 (1- trunc)) m)
            (if z "..." ""))))

(defun conn--kmacro-ring-format ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Ring:  " 'face 'bold)
     (propertize (conn--kmacro-display last-kbd-macro 20) 'face 'transient-value)
     (if (kmacro-ring-empty-p)
         ""
       (concat " "
               (propertize (conn--kmacro-display (kmacro--keys (car kmacro-ring)) 20)
                           'face 'transient-value))))))

(defun conn--kmacro-counter-format ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Counter: " 'face 'bold)
     (propertize (format "%s" kmacro-counter) 'face 'transient-value))))

(defun conn--in-kbd-macro-p ()
  (or defining-kbd-macro executing-kbd-macro))

(transient-define-infix conn--set-counter-format-infix ()
  :class 'transient-lisp-variable
  :set-value (lambda (_ format) (kmacro-set-format format))
  :variable 'kmacro-counter-format
  :reader (lambda (&rest _)
            (read-string "Macro Counter Format: ")))

(transient-define-prefix conn-kmacro-menu ()
  "Transient menu for kmacro functions."
  [[:description
    conn--kmacro-counter-format
    ("i" "Insert Counter" kmacro-insert-counter)
    ("s" "Set Counter" kmacro-set-counter :transient t)
    ("a" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" conn--set-counter-format-infix)]
   [:description
    conn--kmacro-ring-format
    :if-not conn--in-kbd-macro-p
    ("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("~" "Swap" kmacro-swap-ring :transient t)
    ("w" "Pop" kmacro-delete-ring-head :transient t)]]
  ["Commands:"
   :if-not conn--in-kbd-macro-p
   [("c" "Call Macro" kmacro-call-macro)
    ("r" "Record Macro" kmacro-start-macro)
    ("e" "Edit Macro" kmacro-edit-macro)
    ("!" "Kmacro to Register" kmacro-to-register)]
   [("d" "Name Last Macro" kmacro-name-last-macro)
    ("l" "Edit Macro Lossage" kmacro-edit-lossage)
    ("@" "Apply Macro on Lines" apply-macro-to-region-lines)]]
  ["Commands:"
   :if conn--in-kbd-macro-p
   ("q" "Query" kbd-macro-query :if conn--in-kbd-macro-p)
   ("r" "Stop Recording Macro" kmacro-end-macro :if conn--in-kbd-macro-p)
   ("d" "Redisplay" kmacro-redisplay :if conn--in-kbd-macro-p)])

(transient-define-infix conn--set-fill-column-infix ()
  :class 'transient-lisp-variable
  :variable 'fill-column
  :set-value (lambda (_ val) (set-fill-column val))
  :reader (lambda (&rest _)
            (read-number (format "Change fill-column from %s to: " fill-column)
                         (current-column))))

(transient-define-infix conn--set-fill-prefix-infix ()
  :class 'transient-lisp-variable
  :set-value #'ignore
  :variable 'fill-prefix
  :reader (lambda (&rest _)
            (set-fill-prefix)
            (substring-no-properties fill-prefix)))

(transient-define-infix conn--auto-fill-infix ()
  :class 'transient-lisp-variable
  :set-value #'ignore
  :variable 'auto-fill-function
  :reader (lambda (&rest _) (auto-fill-mode 'toggle)))

(transient-define-prefix conn-fill-menu ()
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

(transient-define-prefix conn-sort-menu ()
  "Transient menu for buffer sorting functions."
  [["Sort Region: "
    ("a" "sort pages" sort-pages)
    ("c" "sort columns" sort-columns)
    ("l" "sort lines" sort-lines)]
   [("f" "case fold" conn--case-fold-infix)
    ("n" "sort numeric fields" sort-numeric-fields)
    ("p" "sort paragraphs" sort-paragraphs)
    ("r" "sort regexp fields" sort-regexp-fields)]])

(transient-define-argument conn--read-buffer-infix ()
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "\\(\\(completing-read-multiple\\|matching-regexp\\)\\)"
  :choices '("completing-read-multiple" "buffers-matching-regexp"))

(transient-define-argument conn--dispatch-macro-infix ()
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "\\(\\(last-kbd-macro\\|register-read-with-preview\\)\\)"
  :choices '("last-kbd-macro" "register-read-with-preview"))

(transient-define-infix conn--reverse-switch ()
  :argument "t"
  :shortarg "r"
  :description "Reverse"
  :init-value (lambda (obj) (oset obj value nil)))

(defun conn--dot-dispatch-title ()
  (let ((count 0))
    (conn--for-each-dot (lambda (_) (cl-incf count)))
    (concat (propertize "Dispatch on Dots: " 'face 'bold)
            (propertize (format "%d" count)
                        'face 'transient-value))))

(transient-define-suffix conn--dot-dispatch-suffix ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((multi-buffer t)
         (args (transient-args (oref transient-current-prefix command)))
         (dots (cond ((member "completing-read-multiple" args)
                      (mapcan (lambda (buffer)
                                (conn--sorted-overlays #'conn-dotp '< nil nil buffer))
                              (conn-read-dot-buffers)))
                     ((member "buffers-matching-regexp" args)
                      (mapcan (lambda (buffer)
                                (conn--sorted-overlays #'conn-dotp '< nil nil buffer))
                              (conn-read-matching-dot-buffers)))
                     (t (setq multi-buffer nil)
                        (conn--sorted-overlays #'conn-dotp '<))))
         (macro (cond ((member "register-read-with-preview" args)
                       (register-read-with-preview "Keyboard Macro: "))
                      ((member "last-kbd-macro" args)
                       last-kbd-macro))))
    (unless (or (null macro)
                (stringp macro)
                (vectorp macro)
                (kmacro-p macro))
      (user-error "Resiter is not a keyboard macro"))
    (when (member "t" args)
      (setq dots (nreverse dots)))
    (save-window-excursion
      (conn--thread dots
          dots
        (conn--dot-iterator dots)
        (conn--dispatch-relocate-dots dots)
        (if multi-buffer
            (conn--dispatch-multi-buffer dots)
          (conn--dispatch-single-buffer dots))
        (conn--dispatch-with-state dots 'conn-state)
        (conn--pulse-on-record dots)
        (conn--macro-dispatch dots macro)))))

(transient-define-prefix conn-dots-dispatch-menu (macro buffers)
  "Transient menu for macro dispatch on dots."
  [[:description
    conn--dot-dispatch-title
    ("d" "Dispatch" conn--dot-dispatch-suffix)]
   ["Options:"
    (conn--reverse-switch)
    ("m" "Macro" conn--dispatch-macro-infix :unsavable t :always-read t)
    ("b" "Read Buffers" conn--read-buffer-infix :unsavable t :always-read t)]])

(transient-define-suffix conn--region-dispatch-suffix ()
  :transient 'transient--do-exit
  (interactive)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (regions (mapcar (pcase-lambda (`(,beg . ,end))
                            (cons (conn--create-marker beg)
                                  (conn--create-marker end)))
                          (region-bounds)))
         (macro (cond ((member "register-read-with-preview" args)
                       (register-read-with-preview "Keyboard Macro: "))
                      ((member "last-kbd-macro" args)
                       last-kbd-macro))))
    (unless (or (null macro)
                (stringp macro)
                (vectorp macro)
                (kmacro-p macro))
      (user-error "Register is not a keyboard macro"))
    (when (member "t" args)
      (setq regions (nreverse regions)))
    (save-window-excursion
      (thread-first
        (conn--region-iterator regions)
        (conn--dispatch-single-buffer)
        (conn--dispatch-with-state 'conn-state)
        (conn--pulse-on-record)
        (conn--macro-dispatch macro)))))

(defun conn--dispatch-options-format ()
  (concat
   (propertize "Last KBD Macro: " 'face 'bold)
   (propertize (if last-kbd-macro
                   (conn--kmacro-display last-kbd-macro 20)
                 "nil")
               'face 'transient-value)))

(transient-define-prefix conn-region-dispatch-menu (macro)
  "Transient menu for macro dispatch on regions."
  [["Dispatch: "
    ("d" "Dispatch" conn--dot-dispatch-suffix)]
   [:description
    conn--dispatch-options-format
    (conn--reverse-switch)
    ("m" "Macro" conn--dispatch-macro-infix :unsavable t :always-read t)]])


;;;; Keymaps

(defvar-keymap conn-reb-navigation-repeat-map
  :repeat t
  "C-s" 'reb-next-match
  "C-r" 'reb-prev-match)

(dolist (state conn-states)
  (define-keymap
    :keymap (conn-get-mode-map state 'conn-macro-dispatch-p)
    "<remap> <conn-dots-dispatch>"              'exit-recursive-edit
    "<remap> <conn-dots-dispatch-macro>"        'exit-recursive-edit
    "<remap> <conn-region-dispatch>"            'exit-recursive-edit
    "<remap> <conn-region-dispatch-macro>"      'exit-recursive-edit
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

(defvar-keymap conn-isearch-repeat-map
  :repeat t
  "." 'isearch-repeat-forward
  "," 'isearch-repeat-backward)

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
  "r"   'conn-query-replace-region
  "s"   'conn-sort-menu
  "u"   'conn-insert-pair
  "v"   'vc-region-history
  "w"   'conn-query-replace-regexp-region
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

(defvar-keymap conn-dot-this-map
  :prefix 'conn-dot-this-map
  :doc "Dot this map."
  "p" 'conn-dot-text-property
  "o" 'conn-dot-word-at-point
  "m" 'conn-dot-sexp-at-point
  "w" 'conn-remove-dot-backward
  "d" 'conn-remove-dot-forward)

(defvar-keymap conn-dot-region-repeat-map
  :repeat t
  "j" 'conn-dot-region-backward
  "l" 'conn-dot-region-forward
  "J" 'conn-dot-skip-backward
  "L" 'conn-dot-skip-forward)

(defvar-keymap conn-dot-region-map
  :parent conn-region-map
  "l" 'conn-dot-region-forward
  "j" 'conn-dot-region-backward
  "e" 'conn-add-dots-matching-region
  "a" 'conn-dot-all-things-in-region)

(define-keymap
  :keymap isearch-mode-map
  "M-<return>" 'conn-isearch-exit-and-mark
  "M-|"        'conn-isearch-dispatch
  "M-E"        'conn-isearch-add-dots
  "M-R"        'conn-isearch-refine-dots
  "M-W"        'conn-isearch-remove-dots
  "M-S"        'conn-isearch-split-dots
  "M-("        'conn-isearch-dots-dispatch
  "M-D"        'conn-isearch-dots-dispatch
  "M-)"        'conn-isearch-dots-dispatch-menu
  "M-M"        'conn-isearch-dots-dispatch-menu)

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
  "b"   'regexp-builder
  "c"   'clone-indirect-buffer
  "d"   'duplicate-dwim
  "f"   'conn-fill-menu
  "h"   'conn-mark-thing-map
  "I"   'copy-from-above-command
  "j"   'join-line
  "K"   'transpose-paragraphs
  "k"   'transpose-lines
  "l"   'transpose-chars
  "m"   'transpose-sexps
  "o"   'transpose-words
  "q"   'indent-for-tab-command
  "r"   'query-replace
  "w"   'query-replace-regexp
  "V"   'conn-narrow-indirect-to-visible
  "v"   'conn-narrow-to-visible
  "N"   'conn-narrow-indirect-to-thing
  "n"   'conn-narrow-to-thing
  "y"   'yank-in-context)

(define-keymap
  :keymap conn-dot-state-map
  "M-<down-mouse-1>" 'conn-dot-at-click
  "M-/"              'conn-dot-undo
  "<return>"         'conn-dot-lines
  "<backspace>"      'conn-kill-to-dots
  "M-?"              'conn-dot-redo
  "C-p"              'conn-previous-dot
  "C-n"              'conn-next-dot
  "C-M-p"            'conn-first-dot
  "C-M-n"            'conn-last-dot
  "{"                'conn-first-dot
  "}"                'conn-last-dot
  "#"                'conn-add-dots-matching-regexp
  "$"                'conn-add-dots-matching-literal
  "%"                'conn-query-remove-dots
  "!"                'conn-dots-dispatch
  "@"                'conn-dots-dispatch-menu
  "_"                'conn-remove-dots-outside-region
  "="                'conn-dot-trim-regexp
  "["                'conn-remove-dots-before
  "]"                'conn-remove-dots-after
  "c"                'conn-split-dots-on-regexp
  "C"                'conn-split-region-on-regexp
  "D"                'conn-remove-all-dots
  "d"                'conn-remove-dot-forward
  "E"                'conn-dot-point
  "e"                'conn-dot-region
  "q"                'conn-dot-this-map
  "r"                conn-dot-region-map
  "t"                'conn-dot-all-things-in-region
  "w"                'conn-remove-dot-backward
  "y"                'conn-yank-to-dots)

(define-keymap
  :keymap conn-state-map
  "C-y"   'conn-yank-replace
  "M-y"   'conn-completing-yank-replace
  "="     'indent-relative
  "$"     'ispell-word
  "*"     'calc-dispatch
  ")"     'up-list
  "("     'backward-up-list
  "["     'conn-kill-prepend-region
  "\""    'conn-insert-pair
  "TAB"   'indent-region
  "<tab>" 'indent-region
  "]"     'conn-kill-append-region
  "'"     'conn-other-place-prefix
  "c"     'conn-C-c-keys
  "d"     'conn-delete-char-keys
  "q"     'conn-edit-map
  "r"     'conn-region-map
  "w"     'conn-kill-region
  "y"     'conn-yank-keys
  "Y"     'yank-from-kill-ring)

(define-keymap
  :keymap conn-common-map
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
  "M-8"   'tear-off-window
  "M-9"   'tab-detach
  "C-M-0" 'kill-buffer-and-window
  "SPC"   'conn-set-mark-command
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
  "~"     'conn-buffer-to-other-window
  "a"     'conn-wincontrol
  "b"     'conn-switch-to-buffer-or-tab
  "B"     'ibuffer
  "C"     'conn-copy-region
  "c"     'conn-C-c-keys
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
  "P"     'point-to-register
  "p"     'conn-register-load
  "R"     'conn-dot-region
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
  "SPC" 'conn-scroll-up
  "DEL" 'conn-scroll-down
  "M-;" 'org-toggle-comment
  "."   'point-to-register
  "/"   'undo-only
  ";"   'execute-extended-command
  ":"   'execute-extended-command-for-buffer
  "*"   'conn-org-tree-edit-insert-heading
  "<"   'org-promote-subtree
  ">"   'org-demote-subtree
  "?"   'undo-redo
  "^"   'org-sort
  "_"   'org-columns
  "a"   'switch-to-buffer
  "c"   'conn-C-c-keys
  "g"   'conn-M-g-keys
  "i"   'org-backward-element
  "I"   'org-metaup
  "J"   'org-metaleft
  "j"   'org-previous-visible-heading
  "k"   'org-forward-element
  "K"   'org-metadown
  "L"   'org-metaright
  "l"   'org-next-visible-heading
  "M"   'org-mark-subtree
  "m"   'org-backward-element
  "n"   'org-forward-element
  "N"   'org-toggle-narrow-to-subtree
  "O"   'org-next-block
  "p"   'conn-register-load
  "Q"   'kill-buffer-and-window
  "s"   'conn-M-s-keys
  "t"   'org-todo
  "U"   'org-previous-block
  "u"   'org-up-element
  "w"   'org-refile
  "x"   'conn-C-x-keys
  "z"   'conn-exchange-mark-command)

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

(defvar-keymap conn-local-map
  :doc "Keymap for `conn-local-mode'."
  "C-v" 'conn-scroll-up
  "M-v" 'conn-scroll-down)

(define-keymap
  :keymap conn-global-map
  "C-<backspace>" 'conn-kill-whole-line
  "C-`"           'conn-other-window
  "<pause>"       'conn-toggle-minibuffer-focus
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
  "C-x m"         'conn-kmacro-menu
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

(define-minor-mode conn-mode-line-indicator-mode
  "Display Conn state indicator at the beginning of the mode line."
  :group 'conn-mode
  :global t)

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :keymap conn-local-map
  :lighter (:eval conn-lighter)
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

(defun conn--initialize-buffer ()
  "Initialize conn STATE in BUFFER."
  (when (run-hook-with-args-until-failure 'conn-enable-in-buffer-hook)
    (conn-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn--initialize-buffer
  :keymap conn-global-map
  :group 'conn-mode
  :predicate '(occur-mode
               grep-mode
               occur-edit-mode
               eshell-mode
               (not minibuffer-mode
                    dired-mode
                    slime-xref-mode
                    calc-mode
                    calc-trail-mode
                    calc-keypad-mode
                    special-mode)
               t)
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (conn--setup-extensions)
    (if conn-mode
        (progn
          (keymap-set minibuffer-mode-map "C-M-y" 'conn-yank-region-to-minibuffer)
          (put 'isearch-forward-symbol-at-point 'repeat-map 'conn-isearch-repeat-map)
          (setq conn--prev-mark-even-if-inactive mark-even-if-inactive
                mark-even-if-inactive t)
          (add-hook 'post-command-hook #'conn--update-aux-map)
          (add-hook 'window-configuration-change-hook #'conn--update-cursor))
      (when (eq (keymap-lookup minibuffer-mode-map "C-M-y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "C-M-y"))
      (when (eq 'conn-isearch-repeat-map (get 'isearch-forward-symbol-at-point 'repeat-map))
        (put 'isearch-forward-symbol-at-point 'repeat-map nil))
      (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
      (remove-hook 'post-command-hook #'conn--update-aux-map)
      (remove-hook 'window-configuration-change-hook #'conn--update-cursor))))

(provide 'conn-mode)

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
    :handler (conn-sequential-thing-handler 'org-paragraph)
    :forward-op 'org-forward-paragraph
    :commands '(org-forward-paragraph org-backward-paragraph)
    :expand-key "I"
    :modes 'org-mode)

  (conn-register-thing org-sentence
    :handler (conn-sequential-thing-handler 'org-sentence)
    :forward-op (lambda (arg)
                  (if (>= arg 0)
                      (org-forward-sentence arg)
                    (org-backward-sentence (abs arg))))
    :commands '(org-forward-sentence org-backward-sentence)
    :expand-key "{"
    :modes 'org-mode)

  (conn-register-thing org-element
    :handler (conn-individual-thing-handler 'org-element)
    :expand-key "K"
    :beg-op 'org-backward-element
    :end-op 'org-forward-element
    :commands '(org-forward-element
                org-backward-element
                org-next-visible-heading
                org-previous-visible-heading
                org-up-element)
    :modes 'org-mode)

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
    :handler (conn-sequential-thing-handler 'paredit-sexp)
    :forward-op 'paredit-forward
    :expand-key "m"
    :modes 'paredit-mode
    :commands '(paredit-forward
                paredit-backward
                paredit-forward-up
                paredit-backward-up))

  (conn-register-thing sexp
    :handler (conn-individual-thing-handler 'paredit-sexp)
    :commands '(paredit-forward-up paredit-backward-up)))

(with-eval-after-load 'zones
  (defvar zz-add-zone-anyway-p)
  ;; Make this command add narrowings to izone var
  (defun conn-narrow-to-thing (thing)
    "Narrow to THING at point."
    (interactive (list (conn--read-thing-command)))
    (when-let ((bounds (bounds-of-thing-at-point thing)))
      (let ((zz-add-zone-anyway-p t))
        (narrow-to-region (car bounds) (cdr bounds))))))

(with-eval-after-load 'edebug
  (defvar edebug-mode)
  (defun conn--edebug-toggle-emacs-state ()
    (if edebug-mode
        (conn-emacs-state)
      (conn-pop-state)))
  (add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state))
;;; conn-mode.el ends here
