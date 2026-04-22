;;; conn-read-args.el -*- lexical-binding: t -*-
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

;;; Commentary

;;; Code

(require 'mule-util)
(require 'conn-vars)
(require 'conn-utils)
(require 'conn-states)
(require 'conn-quick-ref)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn-thing-pretty-print "conn-things")

(defvar conn-read-args-inhibit-message nil
  "Value for `inhibit-message' in `conn-read-args' message functions.")

(defvar conn-read-args-last-prefix nil)

(defvar conn--read-args-prefix-mag nil)
(defvar conn--read-args-prefix-sign nil)

(defvar conn--read-args-error-message nil)
(defvar conn--read-args-error-flag nil)

(defvar conn--read-args-message nil)
(defvar conn--read-args-message-timeout nil)

(defvar-keymap conn-read-args-map
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
  "?" 'reference
  "M-?" 'reference
  "C-h k" 'conn-describe-key
  "C-h c" 'conn-describe-key
  "C-h o" 'conn-describe-symbol
  "<escape>" 'keyboard-quit
  "C-g" 'keyboard-quit
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(defvar conn-read-args-reference-page
  (conn-reference-page
    :depth 80
    (:heading "Read Args")
    ((("backward delete arg" backward-delete-arg))
     (("reset arg" reset-arg)))
    ((("available commands with completion"
       execute-extended-command)))))

(defun conn-read-args-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond (conn--read-args-prefix-mag
         (* (if conn--read-args-prefix-sign -1 1)
            conn--read-args-prefix-mag))
        (conn--read-args-prefix-sign -1)))

(defun conn-read-args-consume-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'.

Resets the current prefix argument."
  (prog1 (conn-read-args-prefix-arg)
    (setf conn--read-args-prefix-mag nil
          conn--read-args-prefix-sign nil)))

(defun conn-read-args-message (format-string &rest args)
  "Print FORMAT-STRING with ARGS during `conn-read-args'.

The duration of the message display is controlled by
`minibuffer-message-timeout'."
  (unless executing-kbd-macro
    (let ((inhibit-message conn-read-args-inhibit-message)
          (message-log-max nil))
      (setq conn--read-args-message (apply #'format format-string args)
            conn--read-args-message-timeout (time-add nil minibuffer-message-timeout)))))

(defun conn-read-args-error (format-string &rest args)
  "Print an error message of FORMAT-STRING with ARGS."
  (setq conn--read-args-error-message (apply #'format format-string args)
        conn--read-args-error-flag t)
  (let ((message-log-max t))
    (apply #'message format-string args))
  (throw 'conn-read-args-error nil))

(defun conn--read-args-get-message ()
  (let ((msg (concat
              (when conn--read-args-message
                (format "[%s] " conn--read-args-message))
              (propertize conn--read-args-error-message
                          'face 'error))))
    (unless (equal msg "") msg)))

(defun conn-read-args-prompt-line (prompt)
  (substitute-command-keys
   (concat
    (propertize prompt 'face 'minibuffer-prompt)
    " (arg: "
    (propertize
     (cond (conn--read-args-prefix-mag
            (number-to-string
             (* (if conn--read-args-prefix-sign -1 1)
                conn--read-args-prefix-mag)))
           (conn--read-args-prefix-sign "[-1]")
           (t "[1]"))
     'face 'read-multiple-choice-face)
    "; \\[reference] reference"
    ")"
    (when-let* ((msg (conn--read-args-get-message)))
      (concat ": " msg)))))

(defun conn--read-args-prompt (prompt arguments)
  (message "%s"
           (concat
            (conn-read-args-prompt-line prompt)
            (when-let* ((args (flatten-tree
                               (mapcar #'conn-argument-display arguments))))
              (conn-<
                (compat-call
                 sort args
                 :key (lambda (str)
                        (or (get-text-property 0 'conn-read-args-display-depth str)
                            0)))
                (string-join "   ")
                (:> (concat "\n")))))))

(defun conn-read-args-display-columns (column-count separator-width)
  (lambda (prompt arguments)
    (message
     "%s"
     (let ((to-display
            (flatten-tree
             (cl-loop for arg in arguments
                      collect (conn-argument-display arg))))
           (prompt-line (conn-read-args-prompt-line prompt)))
       (if (length> to-display column-count)
           (conn--with-work-buffer
             (insert prompt-line "\n")
             (conn-to-vtable to-display
                             column-count
                             (current-buffer)
                             :separator-width separator-width
                             :use-header-line nil)
             (buffer-substring (point-min) (1- (point-max))))
         (concat prompt-line "\n"
                 (string-join to-display
                              (make-string separator-width ?\ ))))))))

;; From embark
(defun conn--read-args-bindings (args &optional keymap)
  (let ((result nil))
    (cl-labels ((predicate (item)
                  (cl-loop for arg in args
                           thereis (conn-argument-predicate arg item)))
                (bindings (keymap)
                  (map-keymap
                   (lambda (_key def)
                     (pcase (keymap--menu-item-binding def)
                       ((and (pred keymapp) keymap)
                        (bindings keymap))
                       (`(,(and desc (pred stringp))
                          . ,(and item (pred predicate)))
                        (push (cons desc item) result))
                       ((and item (pred predicate))
                        (push (cons (conn-thing-pretty-print item) item)
                              result))))
                   (keymap-canonicalize keymap))))
      (if keymap
          (bindings keymap)
        (mapc #'bindings (current-active-maps)))
      result)))

(defun conn--read-args-command-affixation (command args &optional keymap)
  (let* ((binding (where-is-internal command keymap t))
         (binding (pcase binding
                    ((pred stringp) binding)
                    ((pred vectorp) (key-description binding))))
         (annotation
          (cl-loop for arg in args
                   thereis (conn-argument-completion-annotation arg command))))
    (list (if binding
              (propertize (concat binding " ")
                          'face 'help-key-binding)
            "")
          (if annotation
              (propertize annotation
                          'face 'completions-annotations)
            ""))))

(defun conn--read-args-completing-read (args &optional keymap)
  (let ((inhibit-message nil))
    (message nil))
  (let* ((table (conn--read-args-bindings args keymap))
         (affixations (make-hash-table :test 'equal))
         (metadata
          `(metadata
            (affixation-function
             . ,(lambda (cands)
                  (cl-loop for c in cands
                           collect (cons c (gethash c affixations))))))))
    (conn--where-is-with-remaps
      (pcase-dolist (`(,name . ,command) table)
        (setf (gethash name affixations)
              (conn--read-args-command-affixation command args keymap)))
      (condition-case _
          (alist-get (completing-read
                      "Command: "
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            metadata
                          (complete-with-action action table string pred)))
                      nil t)
                     table nil nil #'equal)
        (quit nil)))))

(defun conn--read-args-describe-key (arguments message-function)
  (funcall message-function "Key Sequence...")
  (let ((cmd (key-binding (let ((inhibit-quit t))
                            (read-key-sequence nil))
                          t)))
    (if (arrayp cmd)
        (conn-quick-reference
         (conn-reference-page
           (:eval (format "Keyboard macro: \"%s\""
                          (conn--kmacro-display cmd)))))
      (catch 'break
        (dolist (a arguments)
          (ignore
           (conn-argument-command-documentation
            a cmd (lambda (&rest pages)
                    (apply #'conn-quick-reference pages)
                    (throw 'break nil)))))))))

(defun conn--read-args-describe-symbol (arguments)
  (let ((cmd (conn--read-args-completing-read arguments)))
    (catch 'break
      (dolist (a arguments)
        (ignore
         (conn-argument-command-documentation
          a cmd (lambda (&rest pages)
                  (apply #'conn-quick-reference pages)
                  (throw 'break nil))))))))

(defmacro conn-read-args-return (&rest body)
  "Evaluate body and return the result from the current `conn-read-args'.

This skips executing the body of the `conn-read-args' form entirely."
  (declare (indent 0)
           (debug (def-body)))
  `(throw 'conn-read-args-return
          (list (lambda () ,@body))))

(cl-defun conn--read-args (state
                           arglist
                           callback
                           &key
                           (command-handler (conn-read-args-command-handler))
                           (display-handler #'conn--read-args-prompt)
                           around
                           overriding-map
                           prompt
                           prefix
                           pre
                           post
                           reference)
  (let ((arguments (if command-handler
                       (cons command-handler arglist)
                     arglist))
        (prefix (when prefix (prefix-numeric-value prefix)))
        (prompt (or prompt "Read Args"))
        (quit-event (car (last (current-input-mode))))
        (argument-values nil)
        (maps nil)
        (keyseq nil))
    (cl-labels
        ((continue-p ()
           (cl-loop for arg in arguments
                    thereis (conn-argument-required-p arg)))
         (display-message ()
           (unless executing-kbd-macro
             (when (and conn--read-args-message-timeout
                        (time-less-p conn--read-args-message-timeout nil))
               (setq conn--read-args-message nil
                     conn--read-args-message-timeout nil))
             (let ((inhibit-message conn-read-args-inhibit-message)
                   (message-log-max nil)
                   (scroll-conservatively 100))
               (funcall display-handler prompt arguments)))
           (setf conn--read-args-error-message ""))
         (update-args (cmd)
           (catch 'break
             (let ((break nil))
               (dolist (a arguments)
                 (conn-argument-update a cmd (lambda () (setq break t)))
                 (when break (throw 'break t))))))
         (read-command ()
           (let (partial-keymap cmd reading)
             (dlet ((conn-wincontrol-mode nil)
                    (conn-wincontrol-one-command-mode nil))
               (cl-loop
                repeat 10 do
                (setq keyseq (let ((inhibit-quit t))
                               (read-key-sequence nil))
                      cmd (key-binding keyseq t))
                (if (arrayp cmd)
                    (conn-add-unread-events cmd)
                  (cl-return))
                finally (progn
                          (discard-input)
                          (error "Keyboard macro recursion limit exceeded"))))
             (cond ((eql (aref keyseq 0) quit-event)
                    (setq cmd 'keyboard-quit))
                   ((and (null cmd)
                         (eql help-char (aref keyseq (1- (length keyseq)))))
                    (setq cmd 'execute-extended-command
                          partial-keymap (key-binding (seq-subseq keyseq 0 -1))))
                   ((and (symbolp cmd)
                         (autoloadp (symbol-function cmd)))
                    (autoload-do-load (symbol-function cmd))))
             (while (eq cmd 'execute-extended-command)
               (setq cmd (conn--read-args-completing-read arguments
                                                          partial-keymap)
                     reading t))
             (if (or (eq cmd 'undefined)
                     (null cmd))
                 (progn
                   (conn-read-args-error (if reading "Quit" "%s is undefined")
                                         (key-description keyseq))
                   (read-command))
               cmd)))
         (set-error-message (cstr &rest args)
           (setf conn--read-args-error-message
                 (apply #'format cstr args)))
         (execute-command (cmd)
           (when pre (funcall pre cmd))
           (pcase cmd
             ((or 'keyboard-quit
                  'keyboard-escape-quit)
              (signal 'quit nil))
             ('reference
              (apply #'conn-quick-reference
                     reference
                     (mapcar #'conn-argument-get-reference
                             arguments)))
             ((or 'describe-key 'conn-describe-key)
              (conn--read-args-describe-key
               arguments
               (lambda (str)
                 (let ((conn--read-args-error-message str))
                   (display-message)))))
             ((or 'describe-symbol 'conn-describe-symbol)
              (conn--read-args-describe-symbol arguments))
             ((pred identity)
              (or (update-args cmd)
                  (set-error-message "Invalid command: %s <%s>"
                                     (if (symbolp cmd) cmd "_")
                                     (key-description keyseq)))))
           (when post (funcall post cmd)))
         (setup-keymaps ()
           (setf (cdar maps)
                 (thread-last
                   (mapcar #'conn-argument-compose-keymap arguments)
                   (cons overriding-map)
                   (delq nil)
                   (make-composed-keymap)))
           (conn->f emulation-mode-map-alists
             (delq maps)
             (cons maps)))
         (loop ()
           (conn-with-recursive-stack state
             (let ((conn--read-args-prefix-mag (when prefix (abs prefix)))
                   (conn--read-args-prefix-sign (when prefix (> 0 prefix)))
                   (conn--read-args-error-message "")
                   (conn--read-args-error-flag nil)
                   (conn--read-args-message nil)
                   (conn--read-args-message-timeout nil)
                   (emulation-mode-map-alists emulation-mode-map-alists)
                   (inhibit-message t)
                   (minibuffer-message-clear-timeout nil))
               (setq maps `((,conn-current-state . nil)))
               (while (continue-p)
                 (catch 'conn-read-args-error
                   (setup-keymaps)
                   (display-message)
                   (execute-command (read-command))))
               (setq unread-command-events nil ;should this be smarter?
                     conn-read-args-last-prefix (conn-read-args-prefix-arg))))))
      (apply
       (catch 'conn-read-args-return
         (conn--unwind-protect-all
           (let ((conn-read-args-last-prefix nil))
             (if around (funcall around #'loop) (loop))
             (setq argument-values (mapcar #'conn-argument-payload
                                           arglist)))
           (unless argument-values
             (mapc #'conn-argument-cancel arguments))
           (unless executing-kbd-macro
             (let ((inhibit-message conn-read-args-inhibit-message)
                   (message-log-max nil)
                   (scroll-conservatively 100))
               (message nil))))
         (mapc #'conn-argument-accept arguments)
         (cons callback argument-values))))))

(defmacro conn-read-args (state-and-keys varlist &rest body)
  "Eval BODY with value in VARLIST read in STATE.

VARLIST is a list of the form ((PATTERN ARGUMENT) ...) where PATTERN is
a pattern accepted by `pcase-let'.

The execution of a `conn-read-args' form proceeds as follows:

First each ARGUMENT is evaluated.  Then if an AROUND function has been
specified it is called with a continuation which the AROUND function
should call to continue to the read args loop after doing whatever setup
is desired.

The arg reading loop continues while `conn-argument-required-p' returns
non-nil for at least one argument.

The loop then prompts the user for a command via
`read-key-sequence'.  If a PRE function was given then it is
called with the command that has been read.

Then the default command handler and the COMMAND-HANDLER function, if
provided, are called with the current command.  If the command handler
chooses to handle the command and call `conn-read-args-handle' then
POST is called with the current command, and the current iteration of
the loop ends.

If the current command is not handled by a command handler then
`conn-argument-update' is called for each ARGUMENT with ARGUMENT, the
command, and an updater function.  The updater function is a function of
one argument which when called updates the value of ARGUMENT to the
supplied value.  Once updater has been called `conn-argument-update' is
not called with any more ARGUMENTs, POST is called with the current
command and the current iteration of the loop end.

If no command handler handles the current command and no argument
calls an updater then an invalid command message is printed.

Once the loop ends `conn-argument-payload' is called on each
argument and the result is bound to the corresponding pattern form by
`pcase-let' and BODY then runs.

OVERRIDING-MAP if non-nil should be a keymap which will be active during
the read args loop and take precedence over the ARGUMENT keymaps.  Note
that `conn-read-args-map' will still take precedence over
OVERRIDING-MAP.

REFERENCE if non-nil is a `conn-reference-page's or a list of pages to be
displayed as interactive help.

PREFIX is the initial value of the prefix argument.

DISPLAY-HANLDER if non-nil should be a function which will be called
each iteration with a prompt and a list of all ARGUMENTs and display the
echo area help message.

\(fn (STATE &key COMMAND-HANDLER DISPLAY-HANDLER AROUND OVERRIDING-MAP PROMPT PREFIX PRE POST REFERENCE) VARLIST &rest BODY)"
  (declare (indent 2)
           (debug (([sexp &rest keywordp form])
                   ([&rest sexp form])
                   def-body)))
  (pcase-let* (((or `(,state . ,keys) state) state-and-keys))
    (cl-check-type state symbol)
    `(conn--read-args
      ',state
      (list ,@(mapcar #'cadr varlist))
      (pcase-lambda ,(mapcar #'car varlist) ,@body)
      ,@keys)))

;;;; Argument Types

(cl-defstruct (conn-argument
               ( :constructor conn-argument
                 (value &aux (required nil) (set-flag nil))))
  (value nil)
  (set-flag nil :type boolean)
  (required nil :type boolean :read-only t)
  (name nil :type (or string function nil) :read-only t)
  (annotation nil :type (or nil string function) :read-only t)
  (keymap nil :type keymap :read-only t)
  (reference nil :type (or list conn--reference-page)))

(cl-defgeneric conn-argument-cancel (argument)
  ( :method (_arg) nil))

(cl-defgeneric conn-argument-accept (argument)
  ( :method (_arg) nil))

(cl-defgeneric conn-argument-required-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (and (conn-argument-required arg)
         (not (conn-argument-set-flag arg)))))

(cl-defgeneric conn-argument-update (argument form break)
  ( :method (_arg _form _break) nil))

(cl-defgeneric conn-argument-payload (argument)
  "Extract ARGUMENT's value."
  (declare (important-return-value t))
  ( :method (arg) arg)
  ( :method ((arg conn-argument))
    (conn-argument-value arg)))

(cl-defgeneric conn-argument-display (argument)
  "Display string in `conn-read-args-message' for ARGUMENT.

Return value should be a string or a list of strings, each of which will
be displayed in the echo area during `conn-read-args'."
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (pcase (conn-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       str))))

(cl-defgeneric conn-argument-compose-keymap (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (conn-argument-keymap arg)))

(cl-defgeneric conn-argument-predicate (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg _val) nil))

(cl-defgeneric conn-argument-completion-annotation (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (&rest _) nil)
  ( :method ((arg conn-argument) value)
    (when-let* ((ann (conn-argument-annotation arg))
                (_ (conn-argument-predicate arg value)))
      (pcase ann
        ((and (pred stringp) str)
         (concat " (" str ")"))
        ((and fn (pred functionp)
              (let (and str (pred stringp))
                (funcall fn arg)))
         (concat " (" str ")"))))))

(cl-defgeneric conn-argument-get-reference (arg)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (_arg) nil)
  ( :method ((arg conn-argument))
    (conn-argument-reference arg)))

(cl-defgeneric conn-argument-command-documentation (arg command break)
  (declare (side-effect-free t))
  (:method (_arg _cmd _break) nil))

(eval-and-compile
  (defun conn--argument-expand-method (method fixed-args body)
    (when body
      (let ((qualifiers nil)
            (arglist nil))
        (while (cl-generic--method-qualifier-p (car body))
          (push (pop body) qualifiers))
        (setf arglist (pop body))
        `(cl-defmethod ,method
           ,@(nreverse qualifiers)
           ,(append fixed-args arglist)
           (ignore ,@(cdr (cl--generic-split-args fixed-args)))
           ,@body))))

  (defun conn--define-argument-command (argument-and-command
                                        docstring
                                        body)
    (unless (assq :predicate body)
      (setf (alist-get :predicate body)
            `(() t)))
    (cond ((stringp docstring)
           (setf (alist-get :documentation body)
                 `((break)
                   (funcall break (conn-reference-page
                                    (:eval (substitute-command-keys ,docstring)))))))
          ((eq :documentation (car-safe docstring))
           (push docstring body)))
    (macroexpand-all
     `(progn ,@body)
     `((:update . ,(lambda (&rest body)
                     (conn--argument-expand-method
                      'conn-argument-update
                      argument-and-command
                      body)))
       (:documentation . ,(lambda (&rest body)
                            (conn--argument-expand-method
                             'conn-argument-command-documentation
                             argument-and-command
                             body)))
       (:annotation . ,(lambda (&rest body)
                         (conn--argument-expand-method
                          'conn-argument-annotation
                          argument-and-command
                          body)))
       (:predicate . ,(lambda (&rest body)
                        (conn--argument-expand-method
                         'conn-argument-predicate
                         argument-and-command
                         body)))
       ,@macroexpand-all-environment))))

(defmacro conn-define-argument-command (argument-and-command
                                        docstring
                                        &rest
                                        body)
  (declare (indent 1))
  (pcase argument-and-command
    (`((,_handler ,_spec) ,_cmd))
    (_ (error "Invalid argument form")))
  (conn--define-argument-command argument-and-command docstring body))

;;;;; Anonymous Argument

(oclosure-define (conn-anonymous-argument)
  (predicate :type (or nil function))
  (value :type t :mutable t)
  (set-flag :type boolean :mutable t)
  (required :type boolean)
  (name :type (or nil string function))
  (annotation :type (or nil string function))
  (keymap :type keymap)
  (reference :type (or list conn--reference-page))
  (documentation :type function))

(defalias 'conn-anonymous-argument-name
  'conn-anonymous-argument--name)

(defalias 'conn-anonymous-argument-required
  'conn-anonymous-argument--required)

(defalias 'conn-anonymous-argument-set-flag
  'conn-anonymous-argument--set-flag)

(defalias 'conn-anonymous-argument-value
  'conn-anonymous-argument--value)

(defalias 'conn-anonymous-argument-keymap
  'conn-anonymous-argument--keymap)

(defalias 'conn-anonymous-argument-reference
  'conn-anonymous-argument--reference)

(defalias 'conn-anonymous-argument-documentation
  'conn-anonymous-argument--documentation)

(cl-defmethod conn-argument-required-p ((arg conn-anonymous-argument))
  (and (conn-anonymous-argument-required arg)
       (not (conn-anonymous-argument-set-flag arg))))

(cl-defmethod conn-argument-update ((arg conn-anonymous-argument)
                                    form
                                    break)
  (funcall arg form break))

(cl-defmethod conn-argument-payload ((arg conn-anonymous-argument))
  (conn-anonymous-argument-value arg))

(cl-defmethod conn-argument-display ((arg conn-anonymous-argument))
  (pcase (conn-anonymous-argument-name arg)
    ((and (pred stringp) str)
     str)
    ((and fn (pred functionp)
          (let (and str (pred stringp))
            (funcall fn arg)))
     str)))

(cl-defmethod conn-argument-compose-keymap ((arg conn-anonymous-argument))
  (conn-anonymous-argument-keymap arg))

(cl-defmethod conn-argument-predicate ((arg conn-anonymous-argument)
                                       cmd)
  (when-let* ((pred (conn-anonymous-argument--predicate arg)))
    (funcall pred cmd)))

(cl-defmethod conn-argument-completion-annotation ((arg conn-anonymous-argument)
                                                   value)
  (when-let* ((ann (conn-anonymous-argument--annotation arg))
              (_ (funcall (conn-anonymous-argument--predicate arg) value)))
    (pcase ann
      ((and (pred stringp) str)
       (concat " (" str ")"))
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       (concat " (" str ")")))))

(cl-defmethod conn-argument-get-reference ((arg conn-anonymous-argument))
  (conn-anonymous-argument-reference arg))

(cl-defmethod conn-argument-command-documentation ((arg conn-anonymous-argument)
                                                   cmd
                                                   break)
  (funcall break (conn-anonymous-argument-documentation arg cmd)))

;;;;; Composite Argument

(cl-defstruct (conn-composite-argument
               (:include conn-argument)
               (:constructor conn-composite-argument (value))))

(cl-defmethod conn-argument-required-p ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           thereis (conn-argument-required-p a)))

(cl-defmethod conn-argument-update ((arg conn-composite-argument)
                                    form
                                    break)
  (dolist (a (conn-argument-value arg))
    (conn-argument-update a form break)))

(cl-defmethod conn-argument-payload ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           collect (conn-argument-payload a)))

(cl-defmethod conn-argument-display ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           collect (conn-argument-display a)))

(cl-defmethod conn-argument-compose-keymap ((arg conn-composite-argument))
  (make-composed-keymap
   (cl-loop for a in (conn-composite-argument-value arg)
            for map = (conn-argument-compose-keymap a)
            when map collect map)))

(cl-defmethod conn-argument-predicate ((arg conn-composite-argument)
                                       cmd)
  (cl-loop for a in (conn-composite-argument-value arg)
           thereis (conn-argument-predicate a cmd)))

(cl-defmethod conn-argument-completion-annotation ((arg conn-composite-argument)
                                                   value)
  (cl-loop for a in (conn-composite-argument-value arg)
           thereis (conn-argument-completion-annotation a value)))

(cl-defmethod conn-argument-get-reference ((arg conn-composite-argument))
  (mapcar #'conn-argument-get-reference
          (conn-composite-argument-value arg)))

(cl-defmethod conn-argument-accept ((arg conn-composite-argument))
  (mapc #'conn-argument-accept
        (conn-composite-argument-value arg)))

(cl-defmethod conn-argument-cancel ((arg conn-composite-argument))
  (mapc #'conn-argument-cancel
        (conn-composite-argument-value arg)))

(cl-defmethod conn-argument-command-documentation ((arg conn-composite-argument)
                                                   cmd
                                                   break)
  (dolist (arg (conn-composite-argument-value arg))
    (ignore
     (conn-argument-command-documentation arg cmd break))))

;;;;; Boolean Argument

(cl-defstruct (conn-boolean-argument
               (:include conn-argument)
               ( :constructor conn-boolean-argument
                 (name
                  toggle-command
                  keymap
                  &key
                  value
                  annotation
                  reference
                  documentation)))
  (toggle-command nil :type (or symbol list) :read-only t)
  (documentation nil :type (or string function nil) :read-only t))

(cl-defmethod conn-argument-update ((arg conn-boolean-argument)
                                    cmd
                                    break)
  (let ((toggles (conn-boolean-argument-toggle-command arg)))
    (when (if (consp toggles)
              (memq cmd toggles)
            (eq cmd toggles))
      (cl-callf not (conn-boolean-argument-value arg))
      (funcall break))))

(cl-defmethod conn-argument-command-documentation ((arg conn-boolean-argument)
                                                   cmd
                                                   break)
  (let ((toggles (conn-boolean-argument-toggle-command arg))
        (doc (conn-boolean-argument-documentation arg)))
    (when (if (consp toggles)
              (memq cmd toggles)
            (eq cmd toggles))
      (cl-typecase doc
        (string (funcall break (conn-reference-page (:eval doc))))
        (function (funcall doc cmd break))
        (conn--reference-page doc)))))

(cl-defmethod conn-argument-predicate ((arg conn-boolean-argument)
                                       cmd)
  (let ((toggles (conn-boolean-argument-toggle-command arg)))
    (if (consp toggles)
        (memq cmd toggles)
      (eq cmd toggles))))

(cl-defmethod conn-argument-display ((arg conn-boolean-argument))
  (concat
   (substitute-command-keys
    (mapconcat (lambda (cmd) (format "\\[%s]" cmd))
               (ensure-list (conn-boolean-argument-toggle-command arg))
               ", "))
   " "
   (propertize (conn-boolean-argument-name arg)
               'face (when (conn-argument-value arg)
                       'conn-argument-active-face))))

;;;;; Cycling Argument

(defun conn-format-cycling-argument (choice)
  (format "%s" (or (car-safe choice) choice)))

(cl-defstruct (conn-cycling-argument
               (:include conn-argument)
               ( :constructor conn-cycling-argument
                 (name
                  choices
                  commands
                  &key
                  documentation
                  keymap
                  (formatter #'conn-format-cycling-argument)
                  required
                  annotation
                  reference
                  display-prefix
                  (value (car choices))
                  &aux
                  (cycling-commands (ensure-list commands)))))
  (display-prefix nil :type (or nil string))
  (choices nil :type list :read-only t)
  (cycling-commands nil :type list :read-only t)
  (formatter #'conn-format-cycling-argument
             :type function :read-only t)
  (documentation nil :type (or string function nil) :read-only t))

(cl-defmethod conn-argument-update ((arg conn-cycling-argument)
                                    cmd
                                    break)
  (when (memq cmd (conn-cycling-argument-cycling-commands arg))
    (pcase (memq (conn-cycling-argument-value arg)
                 (conn-cycling-argument-choices arg))
      (`(,_ ,next . ,_)
       (setf (conn-cycling-argument-value arg) next)
       (funcall break))
      (`(,_ . nil)
       (setf (conn-cycling-argument-value arg)
             (car (conn-cycling-argument-choices arg)))
       (funcall break)))))

(cl-defmethod conn-argument-payload ((arg conn-cycling-argument))
  (let ((val (conn-argument-value arg)))
    (or (cdr-safe val) val)))

(cl-defmethod conn-argument-predicate ((arg conn-cycling-argument)
                                       sym)
  (eq sym (conn-cycling-argument-cycling-commands arg)))

(cl-defmethod conn-argument-display ((arg conn-cycling-argument))
  (let ((choices (conn-cycling-argument-choices arg))
        (name (conn-cycling-argument-name arg))
        (formatter (conn-cycling-argument-formatter arg))
        (value (if (consp (conn-cycling-argument-value arg))
                   (car (conn-cycling-argument-value arg))
                 (conn-cycling-argument-value arg))))
    (concat
     (substitute-command-keys
      (concat
       (string-join
        (cl-loop for cmd in (ensure-list
                             (conn-cycling-argument-cycling-commands arg))
                 collect (format "\\[%s]" cmd))
        ", ")
       " "))
     (cond
      ((>= (seq-count #'identity choices) 3)
       (if value
           (concat (propertize "(" 'face 'shadow)
                   (propertize (funcall formatter value)
                               'face 'conn-argument-active-face)
                   (propertize (concat "|"
                                       (truncate-string-ellipsis)
                                       ")")
                               'face 'shadow))
         name))
      (value
       (concat
        (conn-cycling-argument-display-prefix arg)
        (propertize "(" 'face 'shadow)
        (let ((cs choices)
              result)
          (cl-loop
           (when-let* ((choice (pop cs))
                       (str (funcall formatter choice)))
             (when (eq choice value)
               (cl-callf propertize str 'face 'conn-argument-active-face))
             (cl-callf concat result str)
             (when (car cs)
               (cl-callf concat result (propertize "|" 'face 'shadow))))
           (unless cs (cl-return result))))
        (propertize ")" 'face 'shadow)))
      (t name)))))

(cl-defmethod conn-argument-command-documentation ((arg conn-cycling-argument)
                                                   cmd
                                                   break)
  (let ((commands (conn-cycling-argument-cycling-commands arg))
        (doc (conn-cycling-argument-documentation arg)))
    (when (memq cmd commands)
      (cl-typecase doc
        (string (funcall break (conn-reference-page (:eval doc))))
        (function (funcall doc cmd break))
        (conn--reference-page doc)))))

;;;;; Read Argument

(cl-defstruct (conn-read-argument
               (:include conn-argument)
               ( :constructor conn-read-argument
                 (name
                  toggle-command
                  keymap
                  reader
                  &key
                  formatter
                  value
                  reference
                  annotation
                  always-read
                  documentation)))
  (reader nil :type function :read-only t)
  (formatter nil :type function :read-only t)
  (toggle-command nil :type (or symbol list) :read-only t)
  (always-read nil :type boolean :read-only t)
  (documentation nil :type (or string function nil) :read-only t))

(cl-defmethod conn-argument-command-documentation ((arg conn-read-argument)
                                                   cmd
                                                   break)
  (let ((toggles (conn-read-argument-toggle-command arg))
        (doc (conn-read-argument-documentation arg)))
    (when (if (consp toggles)
              (memq cmd toggles)
            (eq cmd toggles))
      (cl-typecase doc
        (string (funcall break (conn-reference-page (:eval doc))))
        (function (funcall doc cmd break))
        (conn--reference-page doc)))))

(cl-defmethod conn-argument-update ((arg conn-read-argument)
                                    cmd
                                    break)
  (let ((toggles (conn-read-argument-toggle-command arg)))
    (condition-case err
        (when (if (consp toggles)
                  (memq cmd toggles)
                (eq cmd toggles))
          (setf (conn-argument-value arg)
                (unless (and (conn-argument-value arg)
                             (not (conn-read-argument-always-read arg)))
                  (funcall (conn-read-argument-reader arg)
                           (conn-argument-value arg))))
          (funcall break))
      (quit (conn-read-args-error "Quit"))
      (error (conn-read-args-error (error-message-string err))))))

(cl-defmethod conn-argument-predicate ((arg conn-read-argument)
                                       sym)
  (let ((toggles (conn-read-argument-toggle-command arg)))
    (if (consp toggles)
        (memq sym toggles)
      (eq sym toggles))))

(cl-defmethod conn-argument-display ((arg conn-read-argument))
  (let ((key-string
         (substitute-command-keys
          (mapconcat (lambda (cmd) (format "\\[%s]" cmd))
                     (ensure-list (conn-read-argument-toggle-command arg))
                     ", "))))
    (if-let* ((fn (conn-read-argument-formatter arg)))
        (funcall fn
                 key-string
                 (conn-read-argument-name arg)
                 (conn-argument-value arg))
      (concat key-string " "
              (propertize
               (conn-read-argument-name arg)
               'face (when (conn-argument-value arg)
                       'conn-argument-active-face))))))

;;;;; Command Handler

(cl-defstruct (conn-read-args-command-handler
               (:include conn-argument)
               ( :constructor conn-read-args-command-handler)))

(cl-defmethod conn-argument-completion-annotation ((arg conn-read-args-command-handler)
                                                   value)
  (when (conn-argument-predicate arg value)
    " (command)"))

(cl-defmethod conn-argument-get-reference ((_arg conn-read-args-command-handler))
  (list conn-read-args-reference-page))

(cl-defmethod conn-argument-compose-keymap ((_arg conn-read-args-command-handler))
  conn-read-args-map)

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql recenter-top-bottom)))
  "Recenter the screen."
  ( :update (break)
    (let ((this-command 'recenter-top-bottom)
          (last-command 'recenter-top-bottom))
      (recenter-top-bottom (conn-read-args-consume-prefix-arg))
      (unless executing-kbd-macro
        (pulse-momentary-highlight-one-line))
      (funcall break))))

(cl-defmethod conn-argument-update :before ((_arg conn-read-args-command-handler)
                                            cmd
                                            _break)
  (unless (eq cmd 'recenter-top-bottom)
    (setq recenter-last-op nil)))

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql digit-argument)))
  "Add a digit to the next prefix argument."
  ( :update (break)
    (let* ((char (if (integerp last-input-event)
                     last-input-event
                   (get last-input-event 'ascii-character)))
           (digit (- (logand char ?\177) ?0)))
      (setf conn--read-args-prefix-mag
            (if (integerp conn--read-args-prefix-mag)
                (+ (* 10 conn--read-args-prefix-mag) digit)
              digit)))
    (funcall break)))

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql backward-delete-arg)))
  "Delete the most recent prefix argument digit."
  ( :update (break)
    (when conn--read-args-prefix-mag
      (cl-callf floor conn--read-args-prefix-mag 10))
    (funcall break)))

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql reset-arg)))
  "Reset the prefix argument."
  ( :update (break)
    (setf conn--read-args-prefix-mag nil)
    (funcall break)))

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql negative-argument)))
  "Invert the sign of the current prefix argument."
  ( :update (break)
    (cl-callf not conn--read-args-prefix-sign)
    (funcall break)))

(conn-define-argument-command ((arg conn-read-args-command-handler)
                               (cmd (eql keyboard-quit)))
  "Call `keyboard-quit'."
  ( :update (_break) (signal 'quit nil)))

;;;;; Protected Argument

(cl-defstruct (conn-protected-argument
               (:constructor conn--protected-argument (value cleanup)))
  (value nil :type t :read-only t)
  (cleanup #'ignore :type function :read-only t))

(defmacro conn-protect-argument (value &rest cleanup-body)
  (declare (indent 1))
  `(conn--protected-argument ,value (lambda () ,@cleanup-body)))

(cl-defmethod conn-argument-cancel ((arg conn-protected-argument))
  (funcall (conn-protected-argument-cleanup arg)))

;;;;; Finished Argument

(cl-defstruct (conn-finished-argument
               (:include conn-argument)
               ( :constructor conn-finished-argument
                 (&optional
                  (name "finish")
                  (finish-command 'finish)
                  (keymap (define-keymap
                            "RET" 'finish
                            "<return>" 'finish))
                  &aux
                  (required t))))
  (finish-command nil :type symbol :read-only t))

(cl-defmethod conn-argument-update ((arg conn-finished-argument)
                                    cmd
                                    break)
  (when (eq cmd (conn-finished-argument-finish-command arg))
    (setf (conn-argument-set-flag arg) t)
    (funcall break)))

(cl-defmethod conn-argument-predicate ((arg conn-finished-argument)
                                       cmd)
  (eq cmd (conn-finished-argument-finish-command arg)))

(cl-defmethod conn-argument-display ((arg conn-finished-argument))
  (concat
   (substitute-command-keys
    (format "\\[%s]" (conn-finished-argument-finish-command arg)))
   " "
   (conn-finished-argument-name arg)))

(provide 'conn-read-args)
