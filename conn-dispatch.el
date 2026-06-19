;;; conn-dispatch.el --- Dispatch -*- lexical-binding: t -*-
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

(require 'eieio)
(require 'conn-utils)
(require 'conn-states)
(require 'conn-read-args)
(require 'conn-things)
(require 'conn-jump-ring)

;;;; Labels

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e"
        "r" "t" "y" "u" "i" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defcustom conn-simple-label-input-method nil
  "Input method for simple label chars."
  :group 'conn
  :type '(choice string symbol))

(defface conn-dispatch-action-pulse-face
  '((t (:inherit pulse-highlight-start-face)))
  "Face for highlight pulses after dispatch actions."
  :group 'conn-faces)

(defface conn-dispatch-undo-pulse-face
  '((t (:inherit pulse-highlight-start-face)))
  "Face for highlight pulses after dispatch actions."
  :group 'conn-faces)

(defface conn--dispatch-action-current-pulse-face
  '((t (:inherit conn-dispatch-undo-pulse-face)))
  "Face for current action pulse, do not customize."
  :group 'conn-faces)

(defface conn-target-preview-face
  '((t (:inherit isearch)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-face
  '((t (:inherit default :foreground "#000000" :background "#ff8bd1" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-alt-face
  '((t (:inherit default :foreground "#000000" :background "#ffc5e8" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-multi-face
  '((t (:inherit default :foreground "#000000" :background "#8bd6ff" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-multi-alt-face
  '((t (:inherit default :foreground "#000000" :background "#c5ebff" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defvar conn-window-label-display-functions
  (list #'conn-setup-minibuffer-label)
  "Abnormal hook run to label windows.

Each function in this list will be called with with a window and a
string, and should return either a window label or nil.  Each function
will be called in turn until one returns a label.  If every function
returns nil then `conn-window-label-default-display-function' will be
called to label the window.")

(defvar conn-window-label-default-display-function
  #'conn-setup-header-line-label
  "Default function to label windows.

The function should accept a window and a string and return a window
label.")

(defvar conn-dispatch-label-input-method nil
  "Input method to use when reading labels during `conn-label-select'.

This variable should be set by the labeling function.")

(defvar conn-target-window-predicate)

(defvar conn-dispatch-all-frames 'visible
  "What frames to consider during dispatch.

The value of this variable will be passed as the ALL-FRAMES argument to
`conn-get-windows', which see.")

(defvar conn--dispatch-redisplay-prompt-flag nil)

(defvar-local conn--saved-header-line-format nil)

(cl-defstruct (conn-dispatch-label
               ( :constructor conn-dispatch-label
                 (&key
                  string
                  prefix
                  suffix
                  setup-function
                  padding-function
                  overlay
                  target
                  &aux
                  (narrowed-string string))))
  "State for a dispatch label.

STRING is the label string to use for the label.

PREFIX and SUFFIX are either strings to be displayed before and after
the label string or nil.

TARGET is the dispatch target which is being labeled.

OVERLAY is the overlay to use to display the label.

SETUP-FUNCTION should take a label and setup OVERLAY to display the
label.

PADDING-FUNCTION should take three arguments OVERLAY, WIDTH, and FACE
and pad overlay with WIDTH space and face FACE."
  (prefix nil :type (or string nil))
  (suffix nil :type (or string nil))
  (string nil :type string)
  (narrowed-string nil :type string)
  (overlay nil :type overlay)
  (target nil :type overlay)
  (setup-function nil :type function)
  (padding-function nil :type function))

(cl-defstruct (conn-window-label
               (:constructor nil)
               (:constructor conn-window-label (window string)))
  "State for a window label."
  (window nil :type window)
  (string nil :type string))

(defvar conn-minibuffer-label-format-string
  (concat
   (propertize
    (if (display-graphic-p)
        (propertize " %s " 'display '(space-width 0.33))
      "%s")
    'face 'conn-minibuffer-window-label-face)
   " "))

(cl-defstruct (conn-minibuffer-label
               (:include conn-window-label)
               (:constructor nil)
               ( :constructor conn-minibuffer-label
                 ( window
                   string
                   &aux
                   (overlay (with-current-buffer (window-buffer window)
                              (make-overlay (point-min)
                                            (point-max)
                                            (window-buffer window)))))))
  "State for a minibuffer window label."
  (overlay nil :type overlay))

(defun conn-setup-minibuffer-label (win str)
  (when (window-minibuffer-p win)
    (conn-minibuffer-label win str)))

(cl-defstruct (conn-window-header-line-label
               (:constructor nil)
               (:include conn-window-label)
               ( :constructor conn-window-header-line-label
                 (string
                  window
                  &aux
                  (state (list (window-point window)
                               (window-vscroll window)
                               (window-hscroll window))))))
  "State for a window label."
  (state nil :type list))

(defun conn-get-dispatch-windows ()
  "Return the list of windows for the current dispatch."
  (declare (important-return-value t))
  (conn-get-windows nil nil conn-dispatch-all-frames
                    nil conn-target-window-predicate))

(defun conn-simple-labels (count)
  "Return a list of label strings of length COUNT.

Label strings are constructed by concatenating elements of
`conn-simple-label-characters'."
  (declare (side-effect-free t)
           (important-return-value t))
  (let* ((blen (floor (1+ (log count (length conn-simple-label-characters)))))
         (buckets (make-vector blen nil))
         (i 0))
    (setf (aref buckets i) (conn->
                             (take count conn-simple-label-characters)
                             (mapcar #'copy-sequence)
                             (nreverse)))
    (cl-loop
     (let ((prefixes nil))
       (while (and (aref buckets i)
                   (> count (+ (length (aref buckets i))
                               (* (length prefixes)
                                  (length conn-simple-label-characters)))))
         (push (pop (aref buckets i)) prefixes))
       (if (and (null (aref buckets i)) (> count 0))
           (progn
             (dolist (a prefixes)
               (dolist (b conn-simple-label-characters)
                 (push (concat a b) (aref buckets (1+ i)))))
             (cl-callf nreverse (aref buckets i))
             (incf i))
         (catch 'done
           (let ((n (length (aref buckets i))))
             (dolist (prefix prefixes)
               (dolist (c conn-simple-label-characters)
                 (push (concat prefix c) (aref buckets (1+ i)))
                 (when (= (incf n) count)
                   (throw 'done nil))))))
         (cl-callf nreverse (aref buckets i))
         (cl-return (cl-loop for bucket across buckets
                             nconc bucket)))))))

(defvar conn-dispatch-hide-labels nil
  "When non-nil hide dispatch labels.")

(defvar conn-dispatch-input-buffer nil
  "Buffer in which to read input events during dispatch.")

(defmacro conn-with-dispatch-input-buffer (&rest body)
  "Execute body with `conn-dispatch-input-buffer' bound."
  (declare (indent 0))
  `(let ((conn-dispatch-input-buffer
          (generate-new-buffer " *conn-dispatch-input*" t)))
     (unwind-protect
         (progn
           (let ((im (or current-input-method
                         conn--input-method)))
             (with-current-buffer conn-dispatch-input-buffer
               (activate-input-method im)))
           ,@body)
       (when (buffer-live-p conn-dispatch-input-buffer)
         (kill-buffer conn-dispatch-input-buffer)))))

(cl-defgeneric conn-label-delete (label)
  "Delete the label LABEL.

This function is called on each label after a label has been selected
and so that labels can clean up after themselves."
  (:method (_label) "Noop" nil))

(cl-defgeneric conn-label-narrow (label prefix)
  "Narrow LABEL by PREFIX.

If LABEL contains PREFIX then the label state should be updated to
reflect that prefix has been processed and LABEL should be returned.  If
the label does not contain the prefix then the label state be updated
to reflect that the label is no longer active and nil should be
returned."
  (declare (important-return-value t)))

(cl-defgeneric conn-label-completed-p (label))

(cl-defgeneric conn-label-clear (label)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-setup (label)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-display (label)
  (:method (_) "Noop" nil))

(defun conn-redisplay-labels (labels)
  (mapc #'conn-label-clear labels)
  (unless conn-dispatch-hide-labels
    (mapc #'conn-label-setup labels)
    (mapc #'conn-label-display labels)))

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload."
  (declare (important-return-value t)))

(cl-defmethod conn-label-display ((label conn-minibuffer-label))
  (pcase-let (((cl-struct conn-minibuffer-label window overlay)
               label))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'before-string
                 (format conn-minibuffer-label-format-string
                         (window-parameter window 'conn-label-string)))))

(cl-defmethod conn-label-payload ((label conn-window-label))
  (conn-window-label-window label))

(cl-defmethod conn-label-reset ((label conn-window-label))
  (set-window-parameter (conn-window-label-window label)
                        'conn-label-string
                        (conn-window-label-string label)))

(cl-defmethod conn-label-reset ((label conn-minibuffer-label))
  (cl-call-next-method)
  (pcase-let (((cl-struct conn-minibuffer-label window overlay)
               label))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'before-string
                 (format conn-minibuffer-label-format-string
                         (window-parameter window 'conn-label-string)))))

(cl-defmethod conn-label-delete ((label conn-window-label))
  (pcase-let (((cl-struct conn-window-label
                          window
                          string)
               label))
    (set-window-parameter window 'conn-label-string string)))

(cl-defmethod conn-label-delete ((label conn-minibuffer-label))
  (delete-overlay (conn-minibuffer-label-overlay label))
  (cl-call-next-method))

(cl-defmethod conn-label-delete ((label conn-window-header-line-label))
  (pcase-let* (((cl-struct conn-window-header-line-label
                           window
                           state)
                label)
               (`(,pt ,vscroll ,hscroll) state))
    (with-current-buffer (window-buffer window)
      (when conn--saved-header-line-format
        (setf header-line-format (car conn--saved-header-line-format)
              conn--saved-header-line-format nil)))
    (set-window-point window pt)
    (set-window-hscroll window hscroll)
    (set-window-vscroll window vscroll)
    (set-window-parameter window 'conn-window-labeled-p nil)
    (cl-call-next-method)))

(cl-defmethod conn-label-narrow ((label conn-window-label)
                                 prefix-char)
  (pcase-let* (((cl-struct conn-window-label window) label)
               (string (window-parameter window 'conn-label-string)))
    (unless (or (length= string 0)
                (not (eql prefix-char (string-to-char string))))
      (set-window-parameter window 'conn-label-string (substring string 1))
      label)))

(cl-defmethod conn-label-narrow ((_label conn-minibuffer-label)
                                 _prefix-char)
  (pcase (cl-call-next-method)
    ((and (cl-struct conn-minibuffer-label
                     window
                     overlay)
          label)
     (overlay-put overlay 'before-string
                  (format conn-minibuffer-label-format-string
                          (window-parameter window 'conn-label-string)))
     label)))

(cl-defmethod conn-label-completed-p ((label conn-window-label))
  (string-empty-p (window-parameter (conn-window-label-window label)
                                    'conn-label-string)))

(defun conn-label-select (candidates &optional prompt always-prompt)
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
  (let ((prompt (propertize (or prompt "Chars: ")
                            'face 'minibuffer-prompt))
        (prompt-flag always-prompt)
        (current candidates)
        (partial nil))
    (cl-loop
     (pcase current
       ('nil
        (setf current candidates
              partial nil
              prompt-flag always-prompt)
        (conn-read-args-message "No matches")
        (mapc #'conn-label-reset current))
       ((and `(,it . nil)
             (guard (not (or prompt-flag
                             (and partial
                                  (not (conn-label-completed-p it)))))))
        (cl-return (conn-label-payload it))))
     (setf prompt-flag nil)
     (cl-loop
      (let ((c (conn-dispatch-read-char
                prompt
                conn-dispatch-label-input-method)))
        (unless conn-dispatch-hide-labels
          (cl-callf2 seq-keep
              (lambda (l) (conn-label-narrow l c))
              current)
          (setf partial t)
          (cl-return)))))))

(defvar conn--dispatch-read-char-handlers nil)

(eval-and-compile
  (defun conn--expand-dispatch-handler (tag body)
    (cl-with-gensyms (self methods depth keymap)
      (macroexpand-all
       `(let ((,methods nil)
              (,depth 0)
              (,keymap nil))
          ,@body
          (push (cons (conn-dispatch-handler
                       (alist-get :update ,methods #'ignore)
                       (alist-get :predicate ,methods #'ignore)
                       :display (alist-get :display ,methods #'ignore)
                       :annotation (alist-get :annotation ,methods #'ignore)
                       :command-reference (alist-get :command-reference ,methods #'ignore)
                       :keymap ,keymap)
                      (cons ,depth ',tag))
                conn--dispatch-read-char-handlers))
       `(,@(cl-loop for method in '(:annotation
                                    :command-reference
                                    :predicate
                                    :display)
                    collect (cons method
                                  (let ((m method))
                                    (lambda (arglist &rest body)
                                      `(setf (alist-get ,m ,methods)
                                             (lambda ,arglist ,@body))))))
         (:update . ,(lambda (arglist &rest body)
                       (macroexpand-all
                        `(setf (alist-get :update ,methods)
                               (lambda ,(cons self arglist)
                                 (ignore ,self)
                                 ,@body))
                        `((:return
                           . ,(lambda (&optional value)
                                `(conn-<
                                   conn--dispatch-read-char-handlers
                                   (:> (alist-get ,self) cdr)
                                   (throw ,value))))
                          ,@macroexpand-all-environment))))
         (:depth . ,(lambda (n) `(setf ,depth ,n)))
         (:keymap . ,(lambda (map) `(setf ,keymap ,map)))
         ,@macroexpand-all-environment))))

  (defun conn--with-dispatch-handlers (tag body)
    (macroexpand-all
     `(let ((conn--dispatch-read-char-handlers
             conn--dispatch-read-char-handlers))
        (catch ',tag ,@body))
     `((:handler . ,(lambda (&rest body)
                      (conn--expand-dispatch-handler tag body)))
       (:with . ,(cl-function
                  (lambda (exp &key depth)
                    `(push (cons ,exp (cons ,(or depth 0) ',tag))
                           conn--dispatch-read-char-handlers))))
       ,@macroexpand-all-environment))))

(defmacro conn-with-dispatch-handlers (&rest body)
  "Bind dispatch event handlers.

Two macros are locally defined within body for binding handlers:

- (:with HANDLER &key DEPTH) binds HANDLER. HANDLER should be a
  dispatch command handler defined with
  `define-conn-dispatch-handler-command'. The optional keyword
  argument depth should be an integer between -100 and 100, and
  specifies the sort depth for the handler. By default depth is 0.

- (:handler &rest HANDLER-DEF) defines and binds an anonymous handler.  The
  anonymous handler is defined with the following macros defined locally
  within HANDLER-DEF:

  - (:update (COMMAND BREAK) &rest BODY) defines the update method for
    the handler. BREAK is the break function which should be called when
    the handler handles the command, see also `conn-read-args'. Inside
    BODY the macro (:return &optional VALUE) is defined locally which
    causes the enclosing `conn-with-dispatch-handler' form to return
    VALUE.

  - (:predicate (COMMAND) &rest BODY) defines the predicate method for
    the handler.  See also `conn-argument-predicate'.

  - (:display () &rest BODY) defines the display method for the
    handler. See also `conn-argument-display'.

  - (:annotation (COMMAND) &rest BODY) defines the annotation method for
    the handler.  See also `conn-argument-annotation'.

  - (:command-reference (COMMAND BREAK) &rest BODY) defines the command
    reference method for the handler.  See also
    `conn-argument-command-reference'.

  - (:keymap KEYMAP) defines the keymap for the command handler."
  (declare (indent 0))
  (conn--with-dispatch-handlers (gensym "handler") body))

;;;;; Window Header-line Labels

(defface conn-window-label-face
  '((t (:inherit help-key-binding :height 2.5)))
  "Face for conn window prompt overlay."
  :group 'conn-faces)

(defface conn-minibuffer-window-label-face
  '((t (:bold t :inherit highlight)))
  "Face for wincontrol mode-line window labels."
  :group 'conn-faces)

(defvar conn--window-label-pool nil)

(defun conn-simple-window-labels ()
  (setf conn-dispatch-label-input-method conn-simple-label-input-method)
  (let* ((windows (conn-get-windows nil t t))
         (window-count (length windows)))
    (when (or (null conn--window-label-pool)
              (length< conn--window-label-pool window-count))
      (setf conn--window-label-pool
            (conn-simple-labels (max (ceiling (* 1.67 window-count))
                                     (length conn-simple-label-characters)))))
    (cl-loop with available = (copy-sequence conn--window-label-pool)
             for win in windows
             for label = (window-parameter win 'conn-label-string)
             when (cond* ((null label))
                         ((member label available)
                          (cl-callf2 delete label available)
                          nil)
                         ((bind-and* (new (seq-find (lambda (str)
                                                      (string-prefix-p label str))
                                                    available)))
                          (cl-callf2 delq new available)
                          (not (set-window-parameter
                                win 'conn-label-string new))))
             collect win into unlabeled
             finally (dolist (win unlabeled)
                       (set-window-parameter win 'conn-label-string
                                             (pop available))))))

(defvar conn-window-label-function #'conn-simple-window-labels)

(defun conn--centered-header-label ()
  (when (window-parameter nil 'conn-window-labeled-p)
    (let* ((window-width (window-width nil t))
           (label (propertize (window-parameter nil 'conn-label-string)
                              'face 'conn-window-label-face))
           (label-width (conn--string-pixel-width label (window-buffer)))
           (padding-width (floor (- window-width label-width) 2))
           (padding (propertize " " 'display `(space :width (,padding-width)))))
      (concat padding label))))

(defvar conn-header-line-label-format
  '("" (:eval (conn--centered-header-label))))
(put 'conn-header-line-label-format 'risky-local-variable t)

(defun conn-setup-header-line-label (window string)
  (set-window-parameter window 'conn-window-labeled-p t)
  (with-selected-window window
    (unless conn--saved-header-line-format
      (setf conn--saved-header-line-format (list header-line-format)
            header-line-format conn-header-line-label-format))
    (prog1 (conn-window-header-line-label string window)
      (goto-char (window-start)))))

(defun conn--setup-minibuffer-label (window string)
  (set-window-parameter window 'conn-window-labeled-p t)
  (conn-minibuffer-label window string))

(defun conn-get-window-labels (windows)
  (cl-loop for win in windows
           when (window-parameter win 'conn-label-string)
           collect
           (or (run-hook-with-args-until-success
                'conn-window-label-display-functions
                win it)
               (funcall conn-window-label-default-display-function
                        win it))))

;; From ace-window
(defun conn--dispatch-window-predicate (window &optional ignore-dedicated)
  (not (or ;; ignore child frames
        (and (fboundp 'frame-parent)
             (frame-parent (window-frame window)))
        ;; When `ignore-window-parameters' is nil, ignore
        ;; windows whose `no-other-window’ or
        ;; `no-delete-other-windows' parameter is non-nil.
        (unless ignore-window-parameters
          (window-parameter window 'no-other-window))
        (and ignore-dedicated
             (window-dedicated-p window)))))

(defun conn-get-windows (&optional window
                                   minibuffer
                                   all-frames
                                   ignore-dedicated
                                   predicate)
  "Return a list of windows.

Arguments WINDOW, MINIBUFFER, and ALL-FRAMES have the same meaning as in
`window-list-1', which see.

If IGNORE-DEDICATED is non-nil then ignored windows for which
`window-dedicated-p' returns non-nil.

If PREDICATE is non-nil then it should be a function of one argument, a
window, and should return non-nil if that window should be considered
for dispatch."
  (declare (important-return-value t))
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           when (and (conn--dispatch-window-predicate win ignore-dedicated)
                     (or (null predicate)
                         (funcall predicate win)))
           collect win))

(eval-and-compile
  (defun conn--with-window-labels (var labels body)
    (cl-with-gensyms (timer)
      `(conn-with-dispatch-input-buffer
         (let* ((,var ,labels)
                (conn-dispatch-label-input-method nil)
                (,timer (run-with-idle-timer
                         0 t (lambda ()
                               (while-no-input
                                 (conn-redisplay-labels ,var))))))
           (unwind-protect
               (progn
                 (while-no-input
                   (conn-redisplay-labels ,var))
                 ,@body)
             (when ,timer (cancel-timer ,timer))
             (mapc #'conn-label-delete ,var)))))))

(defmacro conn-with-window-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,labels)
     (conn--with-window-labels var labels body))))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (declare (important-return-value t))
  (when windows
    (funcall conn-window-label-function)
    (conn-with-window-labels
        (labels (conn-get-window-labels windows))
      (conn-with-dispatch-handlers
        (:handler
         ( :predicate (cmd)
           (or (eq cmd 'act)
               (eq cmd 'repeat-dispatch-at-mouse)))
         ( :update (cmd _break)
           (when (or (and (eq cmd 'act)
                          (mouse-event-p last-input-event))
                     (and (eq cmd 'repeat-dispatch-at-mouse)
                          (eq 'dispatch-mouse-repeat
                              (event-basic-type last-input-event))))
             (let* ((posn (event-start last-input-event))
                    (win (posn-window posn)))
               (when (not (posn-area posn))
                 (:return win))))))
        (conn-label-select labels nil always-prompt)))))

;;;;; Label Event Handlers

(eval-and-compile
  (defun conn--dispatch-expand-handler-update (handler update-body)
    (macroexp-unprogn
     (macroexpand-all
      (macroexp-progn update-body)
      `((:return
         . ,(lambda (&optional value)
              `(throw (cdr (alist-get ,handler
                                      conn--dispatch-read-char-handlers))
                      ,value)))
        ,@macroexpand-all-environment)))))

(defmacro define-conn-dispatch-handler-command (argument-and-command
                                                docstring
                                                &rest
                                                body)
  (declare (indent 1))
  (pcase argument-and-command
    (`((,handler ,_spec) ,_cmd)
     (pcase (alist-get :update body)
       ((and `(,_args . ,update-body) cons)
        (setf (cdr cons)
              (conn--dispatch-expand-handler-update handler update-body)))))
    (_ (error "Invalid argument form")))
  `(define-conn-argument-command ,argument-and-command
     ,docstring
     ,@body))

(defvar conn-dispatch-char-argument-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map)
                          (cons #x100 (max-char))
                          'dispatch-character-event)
    (cl-loop for i from ?\s below 256
             do (define-key map (vector i) 'dispatch-character-event))
    (define-keymap
      :keymap map
      "DEL" 'restart
      "<backspace>" 'restart
      "<escape>" 'finish
      "C-q" 'quoted-insert)))

(cl-defstruct (conn-dispatch-char-argument
               (:include conn-argument)
               ( :constructor conn-dispatch-char-argument
                 (input-method
                  &aux
                  (required t)
                  (keymap conn-dispatch-char-argument-map))))
  (input-method nil :read-only t))

(define-conn-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql dispatch-character-event)))
  "Narrow labels by character."
  (:predicate)
  ( :update (break)
    (setf conn--read-args-error-message nil
          conn--dispatch-redisplay-prompt-flag nil)
    (conn-add-unread-events (this-single-command-raw-keys))
    (pcase (conn--dispatch-read-char-1
            (conn-dispatch-char-argument-input-method arg))
      ((pred (eql (car (last (current-input-mode)))))
       (signal 'quit nil))
      (ev (setf (conn-argument--value arg) ev
                (conn-argument--set-flag arg) t)))
    (funcall break)))

(define-conn-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql quoted-insert)))
  "Narrow labels by character read with quoted-insert."
  ( :update (break)
    (let ((char (read-quoted-char
                 (propertize "Quoted Char: "
                             'face 'minibuffer-prompt))))
      (unless (eq char (car (last (current-input-mode))))
        (setf (conn-argument--value arg) char
              (conn-argument--set-flag arg) t)))
    (funcall break)))

(define-conn-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql restart)))
  "Narrow labels by character."
  ( :update (break)
    (setf (conn-argument--value arg) 8
          (conn-argument--set-flag arg) t)
    (funcall break)))

(cl-defstruct (conn-dispatch-handler
               (:include conn-argument)
               ( :constructor conn-dispatch-handler
                 (update
                  predicate
                  &key
                  keymap
                  annotation
                  command-reference
                  display)))
  (update #'ignore :type function :read-only t)
  (predicate #'ignore :type function :read-only t)
  (display #'ignore :type function :read-only t))

(cl-defmethod conn-argument-update ((handler conn-dispatch-handler)
                                    cmd
                                    break)
  (when (funcall (conn-dispatch-handler-predicate handler) cmd)
    (funcall (conn-dispatch-handler-update handler)
             handler cmd break)))

(cl-defmethod conn-argument-display ((arg conn-dispatch-handler))
  (funcall (conn-dispatch-handler-display arg)))

(cl-defmethod conn-argument-keymap ((arg conn-dispatch-handler))
  (conn-dispatch-handler-keymap arg))

(cl-defmethod conn-argument-predicate ((arg conn-dispatch-handler)
                                       cmd)
  (funcall (conn-dispatch-handler-predicate arg) cmd))

(cl-defmethod conn-argument-annotation ((arg conn-dispatch-handler)
                                        value)
  (and-let* ((ann (conn-dispatch-handler-annotation arg))
             (_ (funcall (conn-dispatch-handler-predicate arg) value)))
    (funcall ann value)))

(cl-defmethod conn-argument-command-reference ((arg conn-dispatch-handler)
                                               cmd
                                               break)
  (and (funcall (conn-dispatch-handler-predicate arg) cmd)
       (funcall (conn-dispatch-handler-command-reference arg) cmd break)))

;;;; Dispatch State

(defvar conn-dispatch-target-finder nil
  "Current target finder for dispatch.")

(defvar conn-dispatch-ring)

(defvar conn-dispatch-always-prompt nil
  "Always prompt for user to select a target.
If nil then if there is only one target it is selected automatically.")

(defvar conn-dispatch-repeating nil
  "Whether the current dispatch is being performed in a loop.")

(defvar conn-dispatch-iteration-count nil
  "Number of iterations performed so far in the current dispatch loop.")

(defvar conn-dispatch-quit-flag nil
  "Non-nil when the current dispatch was not exited normally.")

(defvar conn-dispatch-other-end nil
  "Function which returns the other end state.

The function is called by `conn-dispatch-other-end-p' and
`conn-dispatch-no-other-end-p' with the value 1 and returns 1 if other
end is non-nil, -1 if other end is nil and 0 if the current targets have
no other end.")

(defvar conn-dispatch-action nil
  "The current dispatch action.")

(defvar conn--dispatch-change-groups nil
  "List of change groups for each iteration of the current dispatch.")

(defvar conn-targets nil
  "Alist of (WINDOW . TARGETS) for the current dispatch.")

(defvar conn-target-count nil
  "Alist of (WINDOW . TARGET-COUNT) for the current dispatch.")

(defvar conn-target-sort-function 'conn-target-sort-adjacent-then-nearest
  "Sort function for targets in each window.

Labels are sorted first by length and then lexicographically by the
position of the characters in `conn-simple-label-characters'.  Then the
target that is sorted into first position by `conn-target-sort-function'
will received the first label, the second the second label and so one.
Thus a target sorted before another target has higher priority for a
shorter label.

A sort function should take two targets as arguments and return non-nil
if the first should be sorted before the second.")

(defvar conn-target-window-predicate 'conn-dispatch-ignored-mode-p
  "Predicate windows must satisfy in order to be considered by dispatch.")

(defvar conn-target-predicate
  (lambda (pt length window)
    (not (conn--overlays-in-of-type pt (+ pt length)
                                    'conn-target-overlay
                                    window)))
  "Predicate a buffer position must satisfy in order to be a valid target.

A target predicate function should take POINT, LENGTH, and WINDOW as
arguments and return non-nil if a target at POINT of LENGTH in WINDOW's
buffer is a valid target.")

(defvar conn--dispatch-prev-state nil
  "Holds state to be restored during `conn-with-dispatch-suspended'.

3-tuple of `conn-target-window-predicate', `conn-target-predicate', and
`conn-target-sort-function'.")

(defvar conn--dispatch-label-state nil
  "The state for `conn-dispatch-label-function' during dispatch.")

(defvar conn--dispatch-suspend-labels #'ignore
  "Function of no arguments to suspend display of dispatch labels.

Bound by `conn-with-dispatch-labels'.")

(defvar conn-dispatch-in-progress nil
  "Non-nil when dispatch is active.")

(defvar conn--dispatch-buffer-opoints nil
  "Hash table mapping buffers to buffer point at beginning of dispatch.
When dispatch exits normally each buffers opoint is pushed to that
buffers `conn-jump-ring' if opoint differs from point.")

(defvar conn-dispatch-label-function)

(defvar conn-dispatch-action-reference)

(defun conn--with-dispatch (body &optional suspend)
  (when suspend
    (conn-target-finder-suspend conn-dispatch-target-finder)
    (funcall conn--dispatch-suspend-labels))
  (unwind-protect
      (conn-protected-let*
          ((prev conn--dispatch-prev-state)
           (conn--dispatch-prev-state
            (list conn-target-window-predicate
                  conn-target-predicate
                  conn-target-sort-function
                  inhibit-message))
           (conn--dispatch-read-char-handlers nil)
           (conn-dispatch-hide-labels nil)
           (conn-dispatch-quit-flag nil)
           (conn-target-window-predicate conn-target-window-predicate)
           (conn-target-predicate conn-target-predicate)
           (conn-target-sort-function conn-target-sort-function)
           (conn-dispatch-label-function conn-dispatch-label-function)
           (conn-dispatch-other-end (and (not suspend) #'+))
           (conn-dispatch-action-reference nil)
           (conn-targets nil)
           (conn-target-count nil)
           (conn-dispatch-repeating nil)
           (conn-dispatch-iteration-count 0)
           (conn-dispatch-target-finder nil)
           (conn--dispatch-label-state nil)
           (conn--dispatch-change-groups nil)
           (conn--dispatch-buffer-opoints
            (and (not suspend)
                 (make-hash-table :test 'eq)))
           (conn-dispatch-in-progress (not suspend))
           (inhibit-message t)
           ( conn-dispatch-action nil
             (when (and (not suspend)
                        conn-dispatch-action)
               (conn-action-cancel conn-dispatch-action))))
        (when suspend
          (pcase-setq `(,conn-target-window-predicate
                        ,conn-target-predicate
                        ,conn-target-sort-function
                        ,inhibit-message)
                      prev
                      conn--dispatch-prev-state nil))
        (unwind-protect
            (funcall body)
          (unless suspend
            (conn-clear-targets))))
    (when suspend
      (conn--mark-targets 'conn-old-target))))

(defmacro conn-with-dispatch (&rest body)
  (declare (indent 0))
  `(conn--with-dispatch
    (lambda ()
      (conn-with-dispatch-input-buffer
        ,@body))))

(defmacro conn-with-dispatch-suspended (&rest body)
  "Execute BODY with dispatch suspended."
  (declare (indent 0))
  `(progn
     (unless conn-dispatch-in-progress
       (error "Trying to suspend dispatch when state not active"))
     (conn--with-dispatch (lambda () (message nil) ,@body) 'suspend)
     (ignore-errors (conn-dispatch-redisplay))))

(define-inline conn-dispatch-other-end-p ()
  (inline-quote (> 0 (funcall conn-dispatch-other-end 1))))

(define-inline conn-dispatch-no-other-end-p ()
  (inline-quote (= 0 (funcall conn-dispatch-other-end 1))))

(cl-defstruct (conn-dispatch-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-dispatch-thing-argument
                 (&optional
                  recursive-edit
                  in-region
                  &aux
                  (required t)))))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-thing-argument)
                                       (_cmd (conn-thing dispatch)))
  nil)

(defvar-keymap conn-toggle-label-argument-map
  "SPC" 'toggle-labels)

(define-conn-state conn-dispatch-targets-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter "DISPATCH")

(define-conn-state conn-dispatch-bounds-state (conn-dispatch-targets-state))

(define-conn-state conn-dispatch-state (conn-dispatch-targets-state)
  "State for reading a dispatch command.")

(define-conn-state conn-dispatch-thingatpt-state (conn-dispatch-state))

(defvar-keymap conn-dispatch-transform-argument-map)

(conn-add-keymap-reference
 conn-dispatch-transform-argument-map
 (conn-reference-page
   :name 'conn-dispatch-transform
   (:heading "Transformations")
   ((("anchored" conn-dispatch-bounds-anchored)
     ("between" conn-dispatch-bounds-between)
     ("reset" conn-transform-reset))
    (("trim" conn-bounds-trim)
     ("untrim left" conn-bounds-untrim-left)
     ("untrim right" conn-bounds-untrim-right))
    (("upto next/prev"
      conn-bounds-upto-next
      conn-bounds-upto-previous)
     ("over" conn-dispatch-bounds-over)))))

(defun conn-dispatch-transform-argument (&optional value)
  (conn-transform-argument
   value
   :keymap conn-dispatch-transform-argument-map))

(cl-defstruct (conn-dispatch-target-argument
               (:include conn-thing-argument)
               (:constructor conn-dispatch-target-argument
                             (&aux (required t)))))

(cl-defmethod conn-argument-keymap ((arg conn-dispatch-target-argument))
  (make-composed-keymap conn-recursive-edit-thing-map
                        (conn-thing-argument-keymap arg)))

(defvar-keymap conn-separator-argument-map
  "+" 'separator)

(defun conn-separator-argument (&optional initial-value)
  (cl-assert (or (null initial-value)
                 (stringp initial-value)
                 (eq 'default initial-value)))
  (conn-read-argument
   "separator"
   'separator
   conn-separator-argument-map
   (lambda (val)
     (cond* ((or (stringp val)
                 (eq 'default val))
             nil)
            ((bind* (s (read-string "Separator (RET for default): "
                                    nil 'conn-separator-history nil t))))
            ((string-empty-p s) 'default)
            (t s)))
   :value initial-value
   :command-reference "Read a separator to be inserted between each string."))

(defvar-keymap conn-dispatch-replace-argument-map)

(cl-defstruct (conn-dispatch-to-how-argument
               (:include conn-composite-argument)
               ( :constructor conn--dispatch-to-how-argument
                 (replace
                  separator
                  &aux
                  (value (list replace separator)))))
  (replace nil)
  (separator nil))

(cl-defsubst conn-dispatch-to-how-argument (&key (replace t)
                                                 (separator 'default))
  (declare (important-return-value t)
           (side-effect-free t))
  (conn-action-slot
   (conn--dispatch-to-how-argument
    (conn-boolean-argument "replace"
                           'dispatch-replace
                           conn-dispatch-replace-argument-map
                           :value replace)
    (conn-separator-argument separator))
   :read t))

(cl-defmethod conn-argument-display ((arg conn-dispatch-to-how-argument))
  (cl-symbol-macrolet ((replace (conn-dispatch-to-how-argument-replace arg))
                       (separator (conn-dispatch-to-how-argument-separator arg)))
    (mapcar #'conn-argument-display
            (list replace
                  (and (conn-argument--value replace)
                       separator)))))

(cl-defstruct (conn-dispatch-marker-argument
               (:include conn-argument)
               (:constructor conn-dispatch-marker-argument
                             ( &optional type
                               &aux (start (point-marker)))))
  (type nil :type boolean :read-only t)
  (start nil :type marker :read-only t))

(cl-defmethod conn-argument-value ((arg conn-dispatch-marker-argument))
  (copy-marker (point) (conn-dispatch-marker-argument-type arg)))

(define-inline conn--dispatch-marker-argument-cleanup (arg)
  (inline-quote
   (let ((mk (conn-dispatch-marker-argument-start ,arg)))
     (with-current-buffer (marker-buffer mk)
       (goto-char mk))
     (set-marker mk nil))))

(cl-defmethod conn-argument-accept ((arg conn-dispatch-marker-argument))
  (conn--dispatch-marker-argument-cleanup arg))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-marker-argument))
  (conn--dispatch-marker-argument-cleanup arg))

;;;;;; Dispatch Quick Ref

(conn-add-keymap-reference
 (conn-get-state-map 'conn-dispatch-targets-state)
 (conn-reference-page
   :name 'conn-targets
   :depth -10
   (:heading "Special Target Bindings")
   ((("symbol" forward-symbol))
    (("line" forward-line))
    (("column" next-line)))))

(defvar conn-dispatch-other-end-reference
  "Operate with point at the other end of the target.")

(defvar conn-this-win-argument-reference
  "Restrict dispatch matches to the selected window.")

(defvar conn-repeat-argument-reference
  "Perform the current dispatch repeatedly.")

;;;;;; Action

(cl-defstruct (conn-action (:conc-name conn-action--))
  (function #'ignore :type function)
  (slots nil :type list :read-only t)
  (no-history nil :type boolean :read-only t)
  (repeat nil :read-only t)
  (description (lambda (&rest _) "") :type function :read-only t)
  (reference #'ignore :type function :read-only t)
  (window-predicate #'always :type function :read-only t)
  (target-predicate #'always :type function :read-only t))

(defalias 'conn-action-function 'conn-action--function)
(defalias 'conn-action-repeat 'conn-action--repeat)
(defalias 'conn-action-window-predicate 'conn-action--window-predicate)
(defalias 'conn-action-target-predicate 'conn-action--target-predicate)
(defalias 'conn-action-no-history 'conn-action--no-history)

(define-inline conn-action-description (action)
  (inline-letevals (action)
    (inline-quote
     (let ((desc (conn-action--description ,action)))
       (cl-typecase desc
         (string desc)
         (function (apply desc (conn-action--slots ,action))))))))

(defun conn-action-reference (action)
  (and-let* ((ref (conn-action--reference action)))
    (when (cl-functionp ref)
      (cl-callf apply ref (conn-action--slots action)))
    (cl-typecase ref
      (conn--reference-page ref)
      (string
       (conn-reference-page
         :depth -70
         (:heading (concat "Action: " (conn-action-description action)))
         (:eval ref))))))

(define-inline conn-call-action (action)
  (inline-letevals (action)
    (inline-quote
     (apply (conn-action--function ,action)
            (conn-action--slots ,action)))))

(cl-defstruct (conn-action-slot
               (:conc-name conn-action-slot--)
               ( :constructor conn-action-slot
                 (value
                  &key
                  read
                  copy
                  stale
                  cleanup
                  accept
                  cancel
                  stay-live
                  &aux
                  (live (not read)))))
  value
  (live nil :type boolean)
  (stay-live nil :type boolean :read-only t)
  (read nil :type boolean)
  (stale #'ignore :type function :read-only t)
  (cleanup #'ignore :type function :read-only t)
  (copy #'identity :type function :read-only t)
  (accept #'identity :type function :read-only t)
  (cancel #'ignore :type function :read-only t))

(eval-and-compile
  (defun conn--action (slots body)
    (let* ((syms (cl-loop for (var . _) in slots
                          collect (gensym var)))
           (ignore nil)
           (bindings
            (cl-loop for i below (length slots)
                     for (var . _) in slots
                     for sym in syms
                     unless (eql ?_ (string-to-char (symbol-name var)))
                     collect `(,var (conn-action-slot--value ,sym))
                     and do (push sym ignore)))
           (options nil))
      (cl-callf nreverse ignore)
      (while (pcase (car body)
               (`(,(and (or :description
                            :reference
                            :repeat
                            :no-history
                            :window-predicate
                            :target-predicate)
                        key)
                  ,value)
                (cl-callf append options
                  (if (or (eq key :description)
                          (eq key :reference))
                      `(,key
                        (lambda (,@syms)
                          (ignore ,@ignore)
                          (cl-symbol-macrolet ,bindings
                            ,value)))
                    (car body)))
                (pop body))))
      (let ((slots (mapcar (lambda (s) `(list ,@(cdr s))) slots)))
        `(make-conn-action
          :slots (cl-loop for (slot . options) in (list ,@slots)
                          if (conn-action-slot-p slot) collect slot
                          else collect (apply #'conn-action-slot slot options))
          :function (lambda (,@syms)
                      (ignore ,@ignore)
                      (cl-symbol-macrolet ,bindings
                        ,@body))
          ,@options)))))

(defmacro conn-action (slots &rest body)
  ;; If the body of an action captures then all copies share the same
  ;; captures.
  (declare (indent 1))
  (conn--action slots body))

(eval-and-compile
  (defun conn--set-action-property (f _args)
    `(function-put ',f :conn-dispatch-action t))
  (setf (alist-get 'conn-dispatch-action defun-declarations-alist)
        (list #'conn--set-action-property)))

(defun conn-action-stale-p (action)
  (cl-loop for slot in (conn-action--slots action)
           thereis (condition-case _
                       (funcall (conn-action-slot--stale slot)
                                (conn-action-slot--value slot))
                     (error t))))

(defun conn-action-copy (action)
  "Copy ACTION.

This function copies ACTION's slots by calling each slot's copier on
that slot's value and otherwise performs a shallow copy."
  (make-conn-action
   :slots (cl-loop for slot in (conn-action--slots action)
                   do (cl-assert (not (conn-action-slot--live slot))
                                 nil "Trying to copy live slot")
                   for copy = (copy-conn-action-slot slot)
                   do (cl-callf2 funcall
                          (conn-action-slot--copy copy)
                          (conn-action-slot--value copy))
                   collect copy)
   :function (conn-action--function action)
   :repeat (conn-action-repeat action)
   :description (conn-action--description action)
   :window-predicate (conn-action-window-predicate action)
   :target-predicate (conn-action-target-predicate action)
   :reference (conn-action--reference action)))

(defun conn-action-cleanup (action)
  "Run ACTION's slots cleanup functions."
  (dolist (slot (conn-action--slots action))
    (when (conn-action-slot--live slot)
      (funcall (conn-action-slot--cleanup slot)
               (conn-action-slot--value slot)))
    (setf (conn-action-slot--live slot) nil
          (conn-action-slot--value slot) nil)))

(defun conn-action-cancel (action)
  "Run ACTION's slots cancel functions."
  (dolist (slot (conn-action--slots action))
    (cond ((conn-action-slot--read slot)
           (conn-argument-cancel (conn-action-slot--value slot))
           (setf (conn-action-slot--read slot) nil))
          ((conn-action-slot--live slot)
           (setf (conn-action-slot--value slot)
                 (funcall (conn-action-slot--cancel slot)
                          (conn-action-slot--value slot)))
           (unless (conn-action-slot--stay-live slot)
             (setf (conn-action-slot--live slot) nil))))))

(defun conn-action-accept (action)
  "Run ACTION's slots accept functions."
  (dolist (slot (conn-action--slots action))
    (when (conn-action-slot--live slot)
      (setf (conn-action-slot--value slot)
            (funcall (conn-action-slot--accept slot)
                     (conn-action-slot--value slot)))
      (unless (conn-action-slot--stay-live slot)
        (setf (conn-action-slot--live slot) nil)))))

(defun conn-action-setup (action repeat)
  (when conn-dispatch-action
    (pcase-dolist (`(,_ . ,undo-fn)
                   (pop conn--dispatch-change-groups))
      (funcall undo-fn :undo))
    (dolist (undo (cl-shiftf conn--dispatch-change-groups nil))
      (pcase-dolist (`(,_ . ,undo-fn) undo)
        (funcall undo-fn :accept)))
    (conn-action-accept conn-dispatch-action))
  (add-function :after-while conn-target-window-predicate
                (conn-action-window-predicate action)
                '((name . action-predicate)))
  (add-function :after-while conn-target-predicate
                (conn-action-target-predicate action)
                '((name . action-predicate)))
  (setf conn-dispatch-action action)
  (setf conn-dispatch-repeating
        (and repeat (conn-action-repeatable-p action))))

(defun conn--action-buffer-change-group ()
  (declare (important-return-value t))
  (let ((change-group (prepare-change-group)))
    (activate-change-group change-group)
    (list change-group (point) (mark t) mark-active)))

(defun conn--action-accept-change-group (change-group)
  (pcase-let ((`(,handle ,_saved-point ,_saved-mark)
               change-group))
    (accept-change-group handle)
    nil))

(defun conn--action-cancel-change-group (change-group)
  (pcase change-group
    (`(,handle ,saved-point ,saved-mark ,saved-mark-active)
     (cancel-change-group handle)
     (goto-char saved-point)
     (let ((omark (marker-position (mark-marker)))
           (cur-mark-active mark-active))
       (set-marker (mark-marker) saved-mark)
       (setf mark-active saved-mark-active)
       (if saved-mark-active
           (when (or (not cur-mark-active)
                     (not (= omark saved-mark)))
             (run-hooks 'activate-mark-hook))
         (when cur-mark-active
           (run-hooks 'deactivate-mark-hook)))))))

(cl-defgeneric conn-get-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t)))

(defun conn-action-change-group ()
  (declare (important-return-value t))
  (conn-action-slot (conn--action-buffer-change-group)
                    :accept #'conn--action-accept-change-group
                    :cancel #'conn--action-cancel-change-group))

(defun conn-action-marker ()
  (declare (important-return-value t))
  (conn-action-slot (conn-dispatch-marker-argument t)
                    :read t
                    :stale (lambda (mk) (not (buffer-live-p (marker-buffer mk))))
                    :cleanup (lambda (mk) (set-marker mk nil))
                    :copy #'copy-marker))

(defun conn-action-separator ()
  (declare (important-return-value t))
  (conn-action-slot (conn-separator-argument)
                    :read t))

(cl-defstruct (conn-dispatch-replace-argument
               (:include conn-argument)
               (:constructor
                conn--dispatch-replace-argument
                (&aux (keymap conn-dispatch-replace-argument-map))))
  (string nil))

(defun conn-action-replace ()
  (declare (important-return-value t))
  (conn-action-slot (conn--dispatch-replace-argument)
                    :read t
                    :accept #'conn--action-accept-change-group
                    :cancel #'conn--action-cancel-change-group))

(cl-defmethod conn-argument-update ((arg conn-dispatch-replace-argument)
                                    (_cmd (eql dispatch-replace))
                                    break)
  (cl-symbol-macrolet ((cg (conn-argument--value arg)))
    (if cg
        (progn
          (conn--action-cancel-change-group (cl-shiftf cg nil))
          (setf (conn-dispatch-replace-argument-string arg) nil))
      (setf cg (conn--action-buffer-change-group))
      (conn-read-args (conn-copy-state
                       :prompt "Replace Thing")
          ((`(,rthing ,rarg) (conn-thing-argument-dwim))
           (rtransform (conn-transform-argument)))
        (pcase (conn-bounds-of rthing rarg)
          ((conn-bounds `(,beg . ,end) rtransform)
           (goto-char beg)
           (setf (conn-dispatch-replace-argument-string arg)
                 (filter-buffer-substring beg end 'delete)))
          (_ (error "No region to replace")))))
    (funcall break)))

(define-conn-argument-command ((arg conn-dispatch-replace-argument)
                               (cmd (eql dispatch-replace)))
  "Read and replace a thing at point.")

(cl-defmethod conn-argument-display ((arg conn-dispatch-replace-argument))
  (concat (substitute-command-keys "\\[dispatch-replace]")
          " "
          (propertize "replace"
                      'face (when (conn-argument--value arg)
                              'conn-argument-active-face))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-replace-argument))
  (conn--action-cancel-change-group (conn-argument--value arg)))

(cl-defmethod conn-argument-accept ((arg conn-dispatch-replace-argument))
  (when-let* ((str (conn-dispatch-replace-argument-string arg)))
    (kill-new str)))

(defun conn-action-repeatable-p (action)
  (declare (important-return-value t))
  (or (eq (conn-action-repeat action) t)
      (eq (conn-action-repeat action) nil)))

(defvar-keymap conn-dispatch-repeat-argument-map
  "SPC" 'repeat-dispatch)

(cl-defstruct (conn-dispatch-action-argument
               (:include conn-argument))
  (repeat nil)
  (action-command nil)
  (arguments nil :type list))

(defun conn-dispatch-action-argument ()
  (make-conn-dispatch-action-argument
   :keymap conn-dispatch-repeat-argument-map))

(conn-add-keymap-reference
 (conn-get-state-map 'conn-dispatch-state)
 (conn-reference-page
   :name 'conn-dispatch-actions
   :depth -50
   (:heading "Actions")
   ((("take" conn-dispatch-take)
     ("send" conn-dispatch-send)
     ("kapply" conn-dispatch-kapply))
    (("copy from" conn-dispatch-copy-from)
     ("copy to" conn-dispatch-copy-to)
     ("transpose" conn-dispatch-transpose))
    (("register load" conn-dispatch-register-load)
     ("repeat command at" conn-dispatch-repeat-command)
     ("yank to/read"
      conn-dispatch-yank-to
      conn-dispatch-reading-yank-to)))))

(cl-defmethod conn-argument-reference ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-dispatch-action-argument-value arg)))
    (conn-action-reference action)))

(cl-defmethod conn-argument-command-reference ((_arg conn-dispatch-action-argument)
                                               cmd
                                               break)
  (pcase cmd
    ('repeat-dispatch
     (funcall break
              (conn-reference-page
                ,(substitute-command-keys
                  "\\<conn-dispatch-char-argument-map>Perform the next dispatch in a loop.
End the loop and accept changes with \\[finish].
Abort the loop and undo all changes with \\[keyboard-quit]."))))
    ((and (pred symbolp)
          (guard (function-get cmd :conn-dispatch-action)))
     (if-let* ((docstring (documentation cmd)))
         (funcall break (conn-reference-page ,docstring))
       (funcall break (conn-reference-page
                        ,(format "Action `%s'." cmd)))))))

(cl-defmethod conn-argument-update ((arg conn-dispatch-action-argument)
                                    cmd
                                    break)
  (cl-symbol-macrolet
      ((arguments (conn-dispatch-action-argument-arguments arg))
       (action (conn-argument--value arg))
       (set-flag (conn-argument--set-flag arg))
       (action-command (conn-dispatch-action-argument-action-command arg)))
    (pcase cmd
      ((and 'repeat-dispatch)
       (when (or (null (conn-argument--value arg))
                 (conn-action-repeatable-p (conn-argument--value arg)))
         (cl-callf not (conn-dispatch-action-argument-repeat arg)))
       (funcall break))
      ((guard (function-get cmd :conn-dispatch-action))
       (when action
         (conn-action-cancel action))
       (setf action nil
             set-flag nil
             arguments nil)
       (when-let* ((_ (not (eq cmd (cl-shiftf action-command nil))))
                   (new-action (or (atomic-change-group
                                     (save-window-excursion
                                       (funcall cmd)))
                                   (user-error "Failed to construct %s" cmd))))
         (setf action-command cmd
               action new-action
               set-flag t)
         (dolist (slot (conn-action--slots new-action))
           (when (conn-action-slot--read slot)
             (push (conn-action-slot--value slot) arguments)))
         (unless (eq t (conn-dispatch-action-argument-repeat arg))
           (setf (conn-dispatch-action-argument-repeat arg)
                 (when (conn-action-repeatable-p new-action)
                   (and (conn-action-repeat new-action) 'auto)))))
       (funcall break))
      (_
       (dolist (a arguments)
         (conn-argument-update a cmd break))))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-argument--value arg)))
    (conn-action-cancel action)))

(cl-defmethod conn-argument-accept ((arg conn-dispatch-action-argument))
  (mapc #'conn-argument-accept
        (conn-dispatch-action-argument-arguments arg)))

(cl-defmethod conn-argument-value ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-dispatch-action-argument-value arg)))
    (dolist (slot (conn-action--slots action))
      (when (conn-action-slot--read slot)
        (setf (conn-action-slot--read slot) nil
              (conn-action-slot--live slot) t)
        (cl-callf conn-argument-value
            (conn-action-slot--value slot)))))
  (list (conn-dispatch-action-argument-value arg)
        (conn-dispatch-action-argument-repeat arg)))

(cl-defmethod conn-argument-predicate ((arg conn-dispatch-action-argument)
                                       sym)
  (or (function-get sym :conn-dispatch-action)
      (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
               thereis (conn-argument-predicate a sym))))

(cl-defmethod conn-argument-annotation ((arg conn-dispatch-action-argument)
                                        sym)
  (or (and (function-get sym :conn-dispatch-action)
           " (action)")
      (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
               thereis (conn-argument-annotation a sym))))

(cl-defmethod conn-argument-keymap ((arg conn-dispatch-action-argument))
  (make-composed-keymap
   (cons (cl-call-next-method)
         (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
                  collect (conn-argument-keymap a)))))

(cl-defmethod conn-argument-display ((arg conn-dispatch-action-argument))
  (list
   (concat (substitute-command-keys "\\[repeat-dispatch] ")
           (propertize
            "repeat"
            'face (when (conn-dispatch-action-argument-repeat arg)
                    'eldoc-highlight-function-argument)))
   (and-let* ((action (conn-argument--value arg)))
     (concat (propertize "Do"
                         'face 'bold
                         'conn-read-args-display-depth -50)
             ": "
             (propertize (or (conn-action-description action)
                             "unnamed action")
                         'face 'eldoc-highlight-function-argument)))
   (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
            collect (mapcar
                     (lambda (str)
                       (propertize str 'conn-read-args-display-depth -49))
                     (flatten-tree (conn-argument-display a))))))

;;;;;; Command Handler

(defvar-keymap conn-dispatch-command-handler-map)

(conn-add-keymap-reference
 conn-dispatch-command-handler-map
 (conn-reference-page
   :name 'conn-dispatch-commands
   (((:heading "History:")
     ("previous dispatch" conn-dispatch-cycle-ring-previous)
     ("next dispatch" conn-dispatch-cycle-ring-next))
    ((:heading "Last Dispatch:")
     ("repeat" conn-repeat-last-dispatch)
     ("describe" conn-dispatch-ring-describe-head)))
   (((:heading "Select")
     ("toggle repeat" repeat-dispatch)
     ("toggle other end" other-end)
     ("restrict matches to the selected window" restrict-windows)))))

(cl-defstruct (conn-dispatch-command-handler
               (:include conn-read-args-command-handler)
               ( :constructor conn-dispatch-command-handler
                 (&aux
                  (keymap conn-dispatch-command-handler-map)))))

(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")

(define-conn-argument-command ((arg conn-dispatch-command-handler)
                               (cmd (eql conn-dispatch-cycle-ring-next)))
  "Cycle the dispatch ring to the next most recent dispatch."
  ( :update (break)
    (condition-case err
        (progn
          (conn-dispatch-cycle-ring-next)
          (if (bound-and-true-p conn-posframe-mode)
              (conn-posframe--dispatch-ring-display-subr)
            (conn-read-args-message "%s" (conn-describe-dispatch
                                          (conn-ring-head conn-dispatch-ring))))
          (funcall break))
      (user-error
       (conn-read-args-error (error-message-string err))))))

(define-conn-argument-command ((arg conn-dispatch-command-handler)
                               (cmd (eql conn-dispatch-cycle-ring-previous)))
  "Cycle the dispatch ring to the least recent dispatch."
  ( :update (break)
    (condition-case err
        (progn
          (conn-dispatch-cycle-ring-previous)
          (if (bound-and-true-p conn-posframe-mode)
              (conn-posframe--dispatch-ring-display-subr)
            (conn-read-args-message "%s" (conn-describe-dispatch
                                          (conn-ring-head conn-dispatch-ring))))
          (funcall break))
      (user-error
       (conn-read-args-error (error-message-string err))))))

(define-conn-argument-command ((arg conn-dispatch-command-handler)
                               (cmd (eql conn-dispatch-ring-describe-head)))
  "Print a description of the dispatch at the head of the dispatch ring."
  ( :update (break)
    (condition-case err
        (progn
          (conn-dispatch-ring-remove-stale)
          (if-let* ((head (conn-ring-head conn-dispatch-ring)))
              (progn
                (if (bound-and-true-p conn-posframe-mode)
                    (conn-posframe--dispatch-ring-display-subr)
                  (conn-read-args-message "%s" (conn-describe-dispatch head)))
                (funcall break))
            (conn-read-args-error "Dispatch ring empty")))
      (user-error
       (conn-read-args-error (error-message-string err))))))

;;;;; Bounds of Dispatch

(cl-defgeneric conn-bounds-of-dispatch (thing arg location)
  (declare (conn-anonymous-thing-property :bounds-of-dispatch)))

(cl-defmethod conn-bounds-of-dispatch (thing arg location)
  (and-let* ((bounds (save-excursion
                       (goto-char location)
                       (conn-bounds-of thing arg))))
    (setf (conn-bounds-get bounds :origin) (point))
    bounds))

(cl-defgeneric conn-dispatch-bounds-over (bounds)
  "Transform bounds to begin at the start of the thing at point and end at
the end of the thing dispatched on.  Can only be used during
`conn-dispatch'."
  (declare (important-return-value t)
           (conn-anonymous-thing-property :over)
           (conn-bounds-transformation "over")))

(cl-defmethod conn-dispatch-bounds-over (bounds)
  (pcase bounds
    ((and (conn-bounds `(,beg . ,end))
          (conn-bounds-get :origin))
     (save-excursion
       (goto-char origin)
       (pcase (conn-bounds-of bounds (conn-bounds-arg bounds))
         ((conn-bounds `(,obeg . ,oend))
          (conn-make-transformed-bounds
           'conn-dispatch-bounds-over
           bounds
           (if (< oend end)
               (cons (max oend end) (min obeg beg))
             (cons (min obeg beg) (max oend end)))))
         (_ bounds))))
    (_ bounds)))

(cl-defgeneric conn-dispatch-bounds-anchored (bounds)
  "Transform bounds to begin at point and end the bound most distant from
point.  If `conn-dispatch-other-end' is non-nil then end at the bound
nearest to point.  Can only be used during `conn-dispatch'."
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-anchored)
           (conn-bounds-transformation "anchored" :no-reformat t)))

(cl-defmethod conn-dispatch-bounds-anchored (bounds)
  (pcase bounds
    ((and (conn-bounds-get :origin nil
                           (and origin (pred identity)))
          (conn-bounds `(,beg . ,end)))
     (conn-make-transformed-bounds
      'conn-dispatch-bounds-anchored
      bounds
      (if (< beg origin)
          (if (conn-dispatch-other-end-p)
              (cons beg origin)
            (cons (min origin end)
                  (max origin end)))
        (cons origin
              (if (conn-dispatch-other-end-p) end beg)))))
    (_ bounds)))

(cl-defgeneric conn-dispatch-bounds-between (bounds)
  "Dispatch on a second thing and transform bounds to be the largest region
created from the bounds of the two things.  The new beg and end are
taken to be the points where point would be after dispatching on each
thing.  Can only be used during `conn-dispatch'."
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-between)
           (conn-bounds-transformation "between")))

(define-inline conn-dispatch-bounds (bounds &optional transforms)
  (inline-quote
   (pcase (conn-transform-bounds ,bounds ,transforms)
     ((and (guard (conn-dispatch-other-end-p))
           (conn-bounds `(,',beg . ,',end)))
      (cons end beg))
     ((conn-bounds bd)
      bd))))

(pcase-defmacro conn-dispatch-bounds (pattern &optional transforms)
  `(and (pred conn-bounds-p)
        (app ,(static-if (< emacs-major-version 30)
                  `(pcase--flip conn-dispatch-bounds ,transforms)
                `(conn-dispatch-bounds _ ,transforms))
             ,pattern)))

;;;;; Dispatch Window Filtering

(defcustom conn-dispatch-thing-ignored-modes
  (list 'image-mode 'doc-view-mode 'pdf-view-mode)
  "List of modes to ignore when searching for dispatch candidates."
  :group 'conn
  :type '(list symbol))

(defun conn-dispatch-ignored-mode-p (win)
  "Return non-nil if the major mode of WIN's buffer is ignored by dispatch.

Ignored modes are those satisfying `provided-mode-derived-p' when called
with `conn-dispatch-thing-ignored-modes'."
  (not (apply #'provided-mode-derived-p
              (buffer-local-value 'major-mode (window-buffer win))
              conn-dispatch-thing-ignored-modes)))

(defun conn--dispatch-restrict-windows (win)
  (declare (side-effect-free t))
  (eq win (selected-window)))

;;;;; Dispatch Target Overlays

(defcustom conn-read-string-timeout 0.5
  "Timeout for string reading functions."
  :group 'conn
  :type 'number)

(defface conn-target-overlay-face
  '((t (:inherit lazy-highlight)))
  "Face for matches when reading strings."
  :group 'conn-faces)

(put 'conn-target-overlay 'priority 2002)
(put 'conn-target-overlay 'face 'conn-target-overlay-face)

(cl-defun conn-make-target-overlay (pt
                                    length
                                    &key
                                    point
                                    padding-function
                                    window
                                    thing
                                    properties)
  "Make a target overlay at PT of LENGTH.

Optionally the overlay may have an associated THING."
  (unless window (setf window (selected-window)))
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
         (old (conn--overlays-in-of-type pt (+ pt (max length 1))
                                         'conn-old-target window))
         (throw-on-input nil)
         (ov (if composition-start
                 (if (> length 0)
                     (make-overlay composition-start composition-end nil nil t)
                   (make-overlay composition-start composition-start nil nil t))
               (make-overlay pt (min (+ pt length) (cdr line-bounds)) nil nil t))
             (delete-overlay ov)))
      (when point (overlay-put ov 'point point))
      (overlay-put ov 'category 'conn-target-overlay)
      (overlay-put ov 'window window)
      (overlay-put ov 'padding-function padding-function)
      (catch 'done
        (dolist (o old)
          (when-let* ((str (overlay-get o 'label-string)))
            (setf (overlay-get ov 'label-string) str
                  (overlay-get o 'label-string) nil
                  (overlay-get ov 'label-face) (overlay-get o 'label-face))
            (throw 'done nil))))
      (when thing (overlay-put ov 'thing thing))
      (cl-loop for (prop val) on properties by #'cddr
               do (overlay-put ov prop val))
      (push ov (alist-get window conn-targets))
      ov)))

(defun conn-make-string-target-overlays (string
                                         &optional
                                         predicate
                                         fixed-length
                                         thing)
  (when (length> string 0)
    (pcase-dolist (`(,beg . ,end)
                   (conn--visible-matches string predicate))
      (conn-make-target-overlay
       beg (or fixed-length (- end beg))
       :thing thing))))

(defun conn-make-re-target-overlays (regexp
                                     &optional
                                     predicate
                                     fixed-length
                                     thing)
  (when (length> regexp 0)
    (pcase-dolist (`(,beg . ,end)
                   (conn--visible-re-matches regexp predicate))
      (conn-make-target-overlay
       beg (or fixed-length (- end beg))
       :thing thing))))

;;;;; Dispatch Labels

(defvar conn-dispatch-label-function #'conn-dispatch-simple-labels
  "Function responsible for labeling all `conn-targets'.

A labeling function may take an optional argument STATE and should
return either a list of label strings or a list of the form
(:state NEW-STATE . LABELS).  If the labeling function returns such a
list then NEW-STATE will be passed to the labeling function the next
time it is called.  The first time the labeling function is called STATE
will be nil.")

(defvar conn-default-label-padding-function #'conn--centered-padding
  "Default function for padding dispatch labels.

Target overlays may override this default by setting the
\\='padding-function overlay property.")

(defvar conn-pixelwise-labels-window-predicate
  #'conn--pixelwise-labels-window-p
  "Predicate a window must satisfy for pixelwise labels to be used.")

(defvar conn-dispatch-pixelwise-labels-line-limit 250
  "Maximum distance from beginning of line for pixelwise labeling.")

(defvar conn-pixelwise-labels-target-predicate
  #'conn--pixelwise-labels-target-p
  "Predicate a target must satisfy for pixelwise labels to be used.")

(defvar conn--label-start-time nil
  "Time when labeling began.")

(defconst conn--pixelwise-window-cache (make-hash-table :test 'eq)
  "Cache of line lengths in each dispatch window.")

(defun conn--pixelwise-labels-window-p (win)
  (declare (important-return-value t))
  (eq (selected-frame) (window-frame win)))

(defvar conn-pixelwise-label-timeout 0.1
  "Maximum time in seconds to spend creating pixelwise labels.
If labeling takes longer than this amount of time then fall back to
characterwise labels for all remaining targets.")

(defun conn--pixelwise-labels-target-p (target)
  (declare (important-return-value t))
  (and (time-less-p (time-since conn--label-start-time)
                    conn-pixelwise-label-timeout)
       (< (save-excursion
            (goto-char (overlay-start target))
            (- (point) (pos-bol)))
          conn-dispatch-pixelwise-labels-line-limit)))

(put 'conn-label-overlay 'priority 3000)

(defun conn--flush-left-padding (overlay width face)
  (when (> width 0)
    (overlay-put overlay 'after-string
                 (propertize " "
                             'display `(space :width (,width))
                             'face face))))

(defun conn--flush-right-padding (overlay width face)
  (when (> width 0)
    (overlay-put overlay 'before-string
                 (propertize " "
                             'display `(space :width (,width))
                             'face face))))

(defun conn--centered-padding (overlay width face)
  (let ((left (min 15 (floor width 2)))
        (right (max (- width 15) (ceiling width 2))))
    (when (> left 0)
      (overlay-put overlay 'before-string
                   (propertize
                    " "
                    'display `(space :width (,left))
                    'face face)))
    (when (> right 0)
      (overlay-put overlay 'after-string
                   (propertize
                    " "
                    'display `(space :width (,right))
                    'face face)))))

(defun conn--dispatch-eovl (pt window)
  (declare (important-return-value t))
  (or (cl-loop for (beg end . _) in (conn--dispatch-window-lines window)
               when (<= beg pt end) return end)
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (vertical-motion (cons (window-width) 0))
          (point)))))

(defun conn--dispatch-setup-label-pixelwise (label)
  (clrhash conn--pixelwise-window-cache)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay
                           target
                           padding-function)
                label)
               (full-string (concat prefix string suffix))
               (window (overlay-get overlay 'window))
               (display-width nil)
               (padding-width 0)
               (ov nil)
               (pixelwise t))
    (unwind-protect
        (progn
          ;; display-line-numbers, line-prefix and wrap-prefix break
          ;; width calculations, temporarily disable them.
          (setf ov (make-overlay (point-min) (point-max)))
          (overlay-put ov 'priority most-positive-fixnum)
          (overlay-put ov 'display-line-numbers-disable t)
          (overlay-put ov 'line-prefix "")
          (overlay-put ov 'wrap-prefix "")
          (pcase (get-text-property (overlay-start target) 'display)
            (`(raise ,amount)
             (add-display-text-property 0 (length full-string)
                                        'raise amount
                                        full-string))
            (`(height ,amount)
             (add-display-text-property 0 (length full-string)
                                        'height amount
                                        full-string))
            ((or (and (pred listp) disp)
                 (and (pred vectorp) vec
                      (let disp (seq-into vec 'list))))
             (when-let* ((r (assq 'raise disp)))
               (add-display-text-property 0 (length full-string)
                                          'raise (cadr r)
                                          full-string))
             (when-let* ((h (assq 'height disp)))
               (add-display-text-property 0 (length full-string)
                                          'height (cadr h)
                                          full-string))))
          (setf display-width
                (conn--string-pixel-width full-string (window-buffer window)))
          (unless (= (overlay-start overlay) (point-max))
            (let* ((win (overlay-get target 'window))
                   (beg (overlay-end target))
                   (beg-width nil)
                   (end nil)
                   (line-end (conn--dispatch-eovl beg win))
                   (pt beg))
              ;; Find the end of the label overlay.  Barring
              ;; exceptional conditions we want the label overlay to
              ;; be wider than the label string.
              (while (not end)
                (cond*
                 ;; If we are at the end of a line than end the label overlay.
                 ((= line-end pt)
                  (if (and (not (invisible-p pt))
                           (/= pt beg))
                      (setf end pt)
                    ;; If we are at the end of the line and the label
                    ;; overlay has width 0 then we need to expand the
                    ;; label overlay to include the EOL and append it
                    ;; as an after overlay.  Ensure we preserve the
                    ;; invisibility property when we do so.
                    (setf end (1+ pt))
                    (let ((str (buffer-substring pt end)))
                      (add-text-properties
                       0 (length str)
                       `(invisible ,(get-char-property pt 'invisible win))
                       str)
                      (overlay-put overlay 'after-string str))))
                 ((and (/= pt (point-min))
                       (or (and (/= pt beg)
                                (get-char-property (1+ pt) 'before-string)
                                (not (eq (get-char-property (1+ pt) 'before-string)
                                         (get-char-property pt 'before-string))))
                           (and (/= pt beg)
                                (get-char-property pt 'after-string)
                                (not (eq (get-char-property pt 'after-string)
                                         (get-char-property (1+ pt) 'after-string))))
                           (and (pcase (get-text-property pt 'display)
                                  ('nil)
                                  ((pred stringp) t)
                                  (`(,(or 'image 'slice `(margin nil))
                                     . ,_)
                                   t))
                                (= pt (next-single-property-change
                                       (1- pt) 'display nil (1+ pt))))))
                  (setf end (1+ pt)
                        pixelwise nil))
                 ((and (get-char-property pt 'after-string)
                       (not (eq (get-char-property pt 'after-string)
                                (get-char-property (1+ pt) 'after-string))))
                  (setf end (1+ pt)
                        pixelwise nil))
                 ;; If the label overlay is wider than the label
                 ;; string we are done.
                 ((match* (or (constrain width (= pt (point-max)))
                              (>= width display-width))
                          (save-excursion
                            (with-restriction beg pt
                              (- (car (window-text-pixel-size
                                       window beg pt))
                                 ;; Subtract the width of any
                                 ;; before strings
                                 (with-memoization beg-width
                                   (car (window-text-pixel-size
                                         window beg beg)))))))
                  (setf padding-width (max (- width display-width) 0)
                        end pt))
                 ((cl-loop for ov in (conn--overlays-in-of-type
                                      pt (1+ pt) 'conn-target-overlay window)
                           thereis (not (eq ov target)))
                  (setf end pt))
                 ((get-text-property pt 'composition)
                  (setf pt (next-single-property-change
                            pt 'composition nil line-end)))
                 (t (incf pt))))
              (move-overlay overlay (overlay-start overlay) end)))
          (if (= (overlay-start overlay) (overlay-end overlay))
              (overlay-put overlay 'setup (list 'before-string full-string))
            (overlay-put overlay 'setup (list 'display full-string))
            (when (and pixelwise
                       (not (overlay-get overlay 'after-string)))
              (overlay-put overlay 'padding
                           (list (or padding-function
                                     conn-default-label-padding-function)
                                 overlay
                                 padding-width
                                 (get-text-property 0 'face full-string))))))
      (when ov (delete-overlay ov)))))

(defun conn--dispatch-setup-label-pixelwise-before (label)
  (clrhash conn--pixelwise-window-cache)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay
                           target
                           padding-function)
                label)
               (full-string (concat prefix string suffix))
               (window (overlay-get overlay 'window))
               (display-width nil)
               (padding-width 0)
               (pixelwise t)
               (beg-width nil)
               (ov nil))
    (unwind-protect
        (progn
          ;; display-line-numbers, line-prefix and wrap-prefix break
          ;; width calculations, temporarily disable them.
          (setf ov (make-overlay (point-min) (point-max)))
          (overlay-put ov 'priority most-positive-fixnum)
          (overlay-put ov 'display-line-numbers-disable t)
          (overlay-put ov 'line-prefix "")
          (overlay-put ov 'wrap-prefix "")
          (pcase (get-text-property (min (1+ (overlay-start target))
                                         (overlay-end target))
                                    'display)
            (`(raise ,amount)
             (add-display-text-property 0 (length full-string)
                                        'raise amount
                                        full-string))
            (`(height ,amount)
             (add-display-text-property 0 (length full-string)
                                        'height amount
                                        full-string))
            ((or (and (pred listp) disp)
                 (and (pred vectorp) vec
                      (let disp (seq-into vec 'list))))
             (when-let* ((r (assq 'raise disp)))
               (add-display-text-property 0 (length full-string)
                                          'raise (cadr r)
                                          full-string))
             (when-let* ((h (assq 'height disp)))
               (add-display-text-property 0 (length full-string)
                                          'height (cadr h)
                                          full-string))))
          (setf display-width
                (conn--string-pixel-width full-string (window-buffer window)))
          (unless (= (overlay-start overlay) (point-min))
            (let* ((beg (save-excursion
                          (goto-char (overlay-start target))
                          (when (and word-wrap
                                     (or (looking-back "[ \t]" 1)
                                         (aref (char-category-set
                                                (char-before (point)))
                                               ?|)))
                            (backward-char))
                          (point)))
                   (end nil)
                   (line-beg (save-excursion
                               (goto-char beg)
                               (vertical-motion 0)
                               (when (= beg (point))
                                 (vertical-motion -1))
                               (point)))
                   (pt beg))
              (while (not end)
                (cond*
                 ((= line-beg pt)
                  (setf end pt))
                 ((and (/= pt (point-min))
                       (or (and (get-text-property pt 'before-string)
                                (= pt (next-single-property-change
                                       (1- pt) 'before-string nil (1+ pt))))
                           (and (pcase (get-text-property (1- pt) 'display)
                                  ('nil)
                                  ((pred stringp) t)
                                  (`(,(or 'image 'slice `(margin nil))
                                     . ,_)
                                   t))
                                (= pt (next-single-property-change
                                       (1- pt) 'display nil (1+ pt))))))
                  (setf end (min beg (1+ pt))))
                 ((and (/= pt (point-min))
                       (get-char-property (1- pt) 'after-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'after-string nil (1+ pt))))
                  (setf end pt))
                 ((match* (or (constrain width (= pt (point-max)))
                              (>= width display-width))
                          (save-excursion
                            (with-restriction pt beg
                              (- (car (window-text-pixel-size
                                       window pt beg))
                                 (with-memoization beg-width
                                   (car (window-text-pixel-size
                                         window pt pt)))))))
                  (setf padding-width (max (- width display-width) 0)
                        end pt))
                 ((conn--overlays-in-of-type (1- pt) pt
                                             'conn-target-overlay
                                             window)
                  (setf end pt))
                 ((get-text-property (1- pt) 'composition)
                  (setf pt (previous-single-property-change
                            (1- pt) 'composition nil line-beg)))
                 (t (decf pt))))
              (move-overlay overlay end beg)))
          (if (= (overlay-start overlay) (overlay-end overlay))
              (overlay-put overlay 'setup (list 'before-string full-string))
            (overlay-put overlay 'setup (list 'display full-string))
            (when (and pixelwise
                       (not (overlay-get overlay 'after-string)))
              (overlay-put overlay 'padding
                           (list (or padding-function
                                     conn-default-label-padding-function)
                                 overlay
                                 padding-width
                                 (get-text-property 0 'face full-string))))))
      (when ov (delete-overlay ov)))))

(defun conn--dispatch-setup-label-charwise (label)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay)
                label)
               (full-string (concat prefix string suffix))
               (win (overlay-get overlay 'window)))
    (unless (= (overlay-start overlay) (point-max))
      (let* ((beg (overlay-start overlay))
             (end nil)
             (line-end (conn--dispatch-eovl beg win))
             (pt beg))
        (while (not end)
          (cond
           ((= line-end pt)
            (if (and (not (invisible-p pt))
                     (/= pt beg))
                (setf end pt)
              (setf end (1+ pt))
              (let ((str (buffer-substring pt end)))
                (add-text-properties
                 0 (length str)
                 `(invisible ,(get-char-property pt 'invisible win))
                 str)
                (overlay-put overlay 'after-string str))))
           ((or (= pt (point-max))
                (= (- pt beg) (length full-string)))
            (setf end pt))
           ((and (/= beg pt)
                 (conn--overlays-in-of-type pt (1+ pt)
                                            'conn-target-overlay
                                            win))
            (setf end pt))
           ((or (and (get-text-property pt 'display)
                     (= pt (next-single-char-property-change
                            (1- pt) 'display nil (1+ pt))))
                (and (get-text-property pt 'after-string)
                     (= pt (next-single-char-property-change
                            (1- pt) 'after-string nil (1+ pt))))
                (and (get-text-property pt 'before-string)
                     (= pt (next-single-char-property-change
                            (1- pt) 'before-string nil (1+ pt)))))
            (setf end (max beg (1- pt))))
           ((get-text-property pt 'composition)
            (setf pt (next-single-property-change
                      pt 'composition nil line-end)))
           (t (incf pt))))
        (move-overlay overlay (overlay-start overlay) end)))
    (if (= (overlay-start overlay) (overlay-end overlay))
        (overlay-put overlay 'setup (list 'before-string full-string))
      (overlay-put overlay 'setup (list 'display full-string)))))

(defun conn--dispatch-setup-label-charwise-before (label)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay
                           target)
                label)
               (full-string (concat prefix string suffix)))
    (unless (= (overlay-start overlay) (point-min))
      (let* ((win (overlay-get overlay 'window))
             (beg (overlay-start target))
             (end nil)
             (line-beg (save-excursion
                         (goto-char beg)
                         (vertical-motion 0)
                         (point)))
             (pt beg))
        (save-excursion
          (while (not end)
            (goto-char pt)
            (cond
             ((= line-beg pt)
              (setf end pt))
             ((or (= pt (point-min))
                  (= (abs (- pt beg)) (length full-string)))
              (setf end pt))
             ((conn--overlays-in-of-type (1- pt) pt
                                         'conn-target-overlay
                                         win)
              (setf end pt))
             ((or (and (get-char-property pt 'display)
                       (= pt (next-single-char-property-change
                              (1- pt) 'display nil (1+ pt))))
                  (and (get-char-property pt 'after-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'after-string nil (1+ pt))))
                  (and (get-char-property pt 'before-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'before-string nil (1+ pt)))))
              (setf end (1+ pt)))
             ((get-text-property (1- pt) 'composition)
              (setf pt (previous-single-property-change
                        (1- pt) 'composition nil line-beg)))
             (t (decf pt)))))
        (move-overlay overlay end beg)))
    (if (= (overlay-start overlay) (overlay-end overlay))
        (overlay-put overlay 'setup (list 'before-string full-string))
      (overlay-put overlay 'setup (list 'display full-string)))))

(defun conn-before-string-label (label)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay)
                label)
               (full-string (concat prefix string suffix)))
    (overlay-put overlay 'before-string full-string)))

(defconst conn--dispatch-window-lines-cache (make-hash-table :test 'eq))

(defun conn--dispatch-window-lines (&optional window)
  (declare (important-return-value t))
  (unless window (setf window (selected-window)))
  (with-memoization (gethash window conn--dispatch-window-lines-cache)
    (let (lines prev)
      (with-selected-window window
        (save-excursion
          (goto-char (window-start window))
          (setf prev (point-marker))
          (set-marker-insertion-type prev t)
          (while (and (<= prev (window-end window))
                      (not (eobp)))
            (let ((eovl (save-excursion
                          (vertical-motion (cons (window-width) 0))
                          (point-marker))))
              (set-marker-insertion-type eovl t)
              (vertical-motion 1)
              (if (= (point) eovl)
                  (push (cons prev (cons eovl t))
                        lines)
                (push (cons prev (cons eovl nil))
                      lines))
              (setf prev (point-marker))
              (set-marker-insertion-type prev t)))))
      (nreverse lines))))

(defun conn-dispatch-get-display-line (&optional point)
  "Return the number of visual lines between POINT and selected window start.
If POINT is not visible in the currently selected window then return nil."
  (cl-loop for line from 0
           for (beg end . exclusive) in (conn--dispatch-window-lines)
           when (<= beg
                    (or point (point))
                    (if exclusive (1- end) end))
           return line))

(defun conn-dispatch-pixelwise-label-p (ov)
  "Return non-nil if OV should receive a pixelwise label."
  (declare (important-return-value t))
  (or (not (display-graphic-p))
      (and (funcall conn-pixelwise-labels-window-predicate
                    (overlay-get ov 'window))
           (funcall conn-pixelwise-labels-target-predicate ov))))

(defun conn-dispatch-create-label (target string)
  "Return a label for TARGET with label STRING."
  (declare (important-return-value t))
  (let ((window (overlay-get target 'window)))
    (conn-protected-let*
        ((beg (overlay-end target))
         (ov (make-overlay beg beg (overlay-buffer target))
             (delete-overlay ov))
         (face (or (overlay-get target 'label-face)
                   'conn-dispatch-label-face))
         (str (propertize string 'face face)))
      (setf (overlay-get ov 'category) 'conn-label-overlay
            (overlay-get ov 'window) window)
      (funcall
       (or (overlay-get target 'label-ctor)
           #'conn-dispatch-label)
       :setup-function (cond ((overlay-get target 'no-hide)
                              #'conn-before-string-label)
                             ((conn-dispatch-pixelwise-label-p ov)
                              (if (overlay-get target 'label-before)
                                  #'conn--dispatch-setup-label-pixelwise-before
                                #'conn--dispatch-setup-label-pixelwise))
                             (t
                              (if (overlay-get target 'label-before)
                                  #'conn--dispatch-setup-label-charwise-before
                                #'conn--dispatch-setup-label-charwise)))
       :padding-function (overlay-get target 'padding-function)
       :string str
       :prefix (and-let* ((pfx (overlay-get target 'label-prefix)))
                 (propertize pfx 'face face))
       :suffix (and-let* ((sfx (overlay-get target 'label-suffix)))
                 (propertize sfx 'face face))
       :overlay ov
       :target target))))

(defun conn-dispatch-simple-labels (&optional state)
  "Create simple labels for all targets."
  (declare (important-return-value t))
  (setf conn-dispatch-label-input-method conn-simple-label-input-method)
  (pcase-let ((`(,pool ,size ,in-use)
               (or state
                   (list nil 0 (make-hash-table :test 'equal))))
              (count (cl-loop for (_ . c) in conn-target-count sum c))
              (unlabeled nil)
              (labels nil))
    (clrhash in-use)
    (when (> count size)
      ;; Use the in-use table to find labels that are being removed
      ;; to be used as prefixes for new labels.  Later this will be
      ;; used to ensure any target having its label string reused in
      ;; this way maintains the same prefix.
      (dolist (str pool)
        (puthash str 'recycle in-use))
      (setf size (max (ceiling (* 1.8 count))
                      (let ((len (length conn-simple-label-characters)))
                        (+ (- len 3)
                           (* (- len 3) 3))))
            pool (conn-simple-labels size))
      (dolist (str pool)
        (remhash str in-use)))
    (pcase-dolist (`(,win . ,targets) conn-targets)
      (dolist (tar (if (and (eq win (selected-window))
                            conn-target-sort-function)
                       (funcall conn-target-sort-function targets)
                     targets))
        (if-let* ((str (overlay-get tar 'label-string))
                  (_ (not (eq t (gethash str in-use)))))
            ;; Try to reuse a target's existing label.
            (progn
              (when (eq 'recycle (gethash str in-use))
                ;; This target has had its label string reused as a
                ;; prefix for new labels, ensure that it gets a new
                ;; label that has its old label as a prefix.
                (setf str (concat str (car conn-simple-label-characters))
                      (overlay-get tar 'label-string) str))
              (puthash str t in-use)
              (push (conn-dispatch-create-label tar str) labels))
          (push tar unlabeled))))
    (cl-callf nreverse unlabeled)
    (cl-loop for str in pool
             while unlabeled
             unless (gethash str in-use)
             do (let ((tar (pop unlabeled)))
                  (overlay-put tar 'label-string str)
                  (push (conn-dispatch-create-label tar str) labels)))
    `(:state ,(list pool size in-use) ,@labels)))

(defun conn-dispatch-get-labels ()
  (pcase (if conn--dispatch-label-state
             (funcall conn-dispatch-label-function
                      conn--dispatch-label-state)
           (funcall conn-dispatch-label-function))
    (`(:state ,state . ,labels)
     (setf conn--dispatch-label-state state)
     labels)
    (labels labels)))

(defun conn--with-dispatch-labels (labels body)
  (maphash (lambda (_win mks)
             (pcase-dolist (`(,a ,b . ,_) mks)
               (set-marker a nil)
               (set-marker b nil)))
           conn--dispatch-window-lines-cache)
  (clrhash conn--dispatch-window-lines-cache)
  (let ((timer (run-with-idle-timer
                0 t (lambda ()
                      (while-no-input
                        (conn-redisplay-labels labels))))))
    (unwind-protect
        (conn-with-dispatch-handlers
          (:handler
           ( :command-reference (cmd break)
             (when (eq cmd 'toggle-labels)
               (funcall break (conn-reference-page
                                "Toggle display of labels."))))
           (:predicate (cmd) (eq cmd 'toggle-labels))
           (:keymap conn-toggle-label-argument-map)
           ( :display ()
             (concat
              "\\[toggle-labels] "
              (propertize
               "hide labels"
               'face (when conn-dispatch-hide-labels
                       'eldoc-highlight-function-argument))))
           ( :update (_cmd _break)
             (cl-callf not conn-dispatch-hide-labels)
             (conn-dispatch-redisplay)))
          (dolist (window (conn-get-dispatch-windows))
            (ignore (conn--dispatch-window-lines window)))
          (let ((conn--dispatch-suspend-labels
                 (lambda ()
                   (when timer (cancel-timer (cl-shiftf timer nil)))
                   (mapc #'conn-label-delete (cl-shiftf labels nil))
                   (setf conn--dispatch-suspend-labels #'ignore))))
            (while-no-input
              (conn-redisplay-labels labels))
            (funcall body labels)))
      (when timer (cancel-timer timer))
      (mapc #'conn-label-delete labels))))

(defmacro conn-with-dispatch-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,val)
     `(let ((conn-dispatch-label-input-method nil)
            (conn--label-start-time (current-time)))
        (conn--with-dispatch-labels ,val (lambda (,var) ,@body))))
    (_ (error "Unexpected binder form"))))

;;;;; Dispatch Loop

(defun conn--dispatch-push-undo-case (depth body)
  (push (cons depth body)
        (car conn--dispatch-change-groups))
  (conn--compat-callf sort (car conn--dispatch-change-groups)
    :key #'car
    :in-place t))

(defmacro conn-dispatch-undo-case (&rest cases)
  "Add an undo case to the current iterations undo list.

CASES is a list of the form (PATTERN CODE...).  PATTERN is a `pcase'
pattern.  The first PATTERN to match the current undo signal will have
its corresponding CODE run.  The undo signal will be one of:

  :undo     A single iteration of the undo loop is being undone.
            This signal will only be received by an undo case when
            the iteration that was current when it was added to the
            undo list is being undone.
  :cancel   A quit or error was signaled during dispatch and all
            iterations of the dispatch loop are being undone.
  :accept   Dispatch is exiting normally.

DEPTH controls were in the undo list the undo case will be put.  Lesser
depths will be sorted before greater depths.
`conn-dispatch-change-group' undo cases have a depth of 0.

\(fn [:depth DEPTH] &rest BODY)"
  (declare (indent 0))
  (let ((depth 0))
    (when (eq :depth (car cases))
      (setf depth (nth 1 cases)
            cases (drop 2 cases)))
    (cl-assert (<= -100 depth 100))
    (cl-with-gensyms (buf signal)
      `(conn--dispatch-push-undo-case
        ,depth
        (let ((,buf (current-buffer)))
          (lambda (,signal)
            (with-current-buffer ,buf
              (pcase ,signal ,@cases))))))))

(defun conn-dispatch-redisplay (&optional maybe-dont-prompt)
  (unless maybe-dont-prompt
    (setf conn--dispatch-redisplay-prompt-flag t))
  (throw 'dispatch-redisplay nil))

(defun conn-dispatch-select-window (window)
  (let ((frame (window-frame window)))
    (unless (eq frame (selected-frame))
      (select-frame-set-input-focus frame)
      (raise-frame frame)))
  (select-window window)
  (with-memoization (gethash (current-buffer) conn--dispatch-buffer-opoints)
    (point-marker))
  window)

(defun conn-dispatch-goto-char (position &optional nopush)
  (goto-char position)
  (recenter (conn-dispatch-get-display-line))
  (when-let* ((mk (and (not nopush)
                       (gethash (current-buffer)
                                conn--dispatch-buffer-opoints))))
    (unless (region-active-p)
      (push-mark mk))
    (unless (or (not conn-jump-ring-mode)
                (gethash conn-jump-ring conn--dispatch-buffer-opoints))
      (conn-push-jump-ring mk)
      (setf (gethash conn-jump-ring conn--dispatch-buffer-opoints) t))
    (set-marker mk (point))))

(defun conn--dispatch-loop ()
  (let ((success nil)
        (owconf (current-window-configuration))
        (oframe (selected-frame))
        (opoint (point)))
    (setf (gethash (current-buffer) conn--dispatch-buffer-opoints)
          (point-marker))
    (conn--unwind-protect-all
      (progn
        (redisplay)
        (catch 'dispatch-exit
          (while (or conn-dispatch-repeating
                     (< conn-dispatch-iteration-count 1))
            (condition-case err
                (catch 'dispatch-undo
                  (conn-with-dispatch-handlers
                    (when (conn-action-repeatable-p conn-dispatch-action)
                      (:handler
                       ( :predicate (cmd) (eq cmd 'repeat-dispatch))
                       ( :display ()
                         (when conn-dispatch-repeating
                           (concat
                            "\\[repeat-dispatch] "
                            (propertize
                             "repeat"
                             'face 'conn-argument-active-face))))
                       ( :update (_cmd break)
                         (cl-callf not conn-dispatch-repeating)
                         (funcall break))))
                    (let ((frame (selected-frame))
                          (wconf (current-window-configuration))
                          (pt (point)))
                      (push nil conn--dispatch-change-groups)
                      (conn-dispatch-undo-case
                        :depth 100
                        (:undo (redisplay)))
                      (conn-dispatch-undo-case
                        :depth -90
                        (:undo
                         (unless (and (equal wconf (current-window-configuration))
                                      (= (point) pt))
                           (select-frame frame)
                           (set-window-configuration wconf)
                           (goto-char pt))))
                      (conn-call-action conn-dispatch-action)
                      (incf conn-dispatch-iteration-count)
                      (conn-dispatch-undo-case
                        (:undo (decf conn-dispatch-iteration-count))))))
              (user-error
               (pcase-dolist (`(,_ . ,undo-fn)
                              (pop conn--dispatch-change-groups))
                 (funcall undo-fn :undo))
               (let ((message-log-max t))
                 (message (error-message-string err)))
               (setf conn--read-args-error-message
                     (error-message-string err))))))
        (setf success t))
      (setf conn-dispatch-quit-flag (not success))
      (dolist (undo conn--dispatch-change-groups)
        (pcase-dolist (`(,_ . ,undo-fn) undo)
          (funcall undo-fn (if success :accept :cancel))))
      (maphash
       (lambda (_buf mk)
         (when (markerp mk) (set-marker mk nil)))
       conn--dispatch-buffer-opoints)
      (unless success
        (select-frame oframe)
        (set-window-configuration owconf)
        (goto-char opoint))
      (clrhash conn--dispatch-window-lines-cache))))

(defvar conn-dispatch-selecting nil)

(defvar conn-dispatch-select-mode-line
  (propertize " Select" 'face 'font-lock-warning-face)
  "String displayed in the mode line during dispatch selecting.")
(put 'conn-dispatch-select-mode-line 'risky-local-variable t)

(setf (alist-get 'conn-dispatch-selecting minor-mode-alist)
      (list 'conn-dispatch-select-mode-line))

(defun conn-select-target ()
  "Prompt the user to select a target during dispatch.

Returns a list of (POINT WINDOW THING ARG TRANSFORM)."
  (unwind-protect
      (let ((conn-dispatch-selecting t))
        (dolist (win (conn-get-dispatch-windows))
          (with-selected-window win
            (add-to-invisibility-spec 'conn-dispatch-invisible)
            (when-let* ((line (conn-dispatch-get-display-line)))
              (recenter line))))
        (cl-loop
         (catch 'dispatch-redisplay
           (unwind-protect
               (cl-return
                (conn-target-finder-select conn-dispatch-target-finder))
             (conn-target-finder-step conn-dispatch-target-finder)))
         (redisplay)))
    (dolist (win (conn-get-dispatch-windows))
      (with-selected-window win
        (remove-from-invisibility-spec 'conn-dispatch-invisible)))))

(defun conn-dispatch-action-pulse (beg end)
  "Momentarily highlight the region between BEG and END."
  (require 'pulse)
  (declare-function pulse-momentary-highlight-overlay "pulse")
  (unless executing-kbd-macro
    (set-face-background
     'conn--dispatch-action-current-pulse-face
     (face-background 'pulse-highlight-start-face
                      nil
                      'default))
    (let ((ov (make-overlay beg end nil t)))
      (overlay-put ov 'pulse-delete t)
      (pulse-momentary-highlight-overlay
       ov 'conn--dispatch-action-current-pulse-face))))

(defun conn-dispatch-change-group (&rest buffers)
  "Create a dispatch change group for the current buffer.
If BUFFERS are specified, create a change group for the specified
BUFFERS instead.

A change group will cause modifications in BUFFERS to be undone when the
current iteration of the dispatch loop is undone.  Any dispatch action
which modifies a buffer should create a change group in that buffer
before performing any modifications.

Buffer change groups have depth 0.  See `conn-dispatch-undo-case' for
the meaning of depth."
  (unless conn-dispatch-in-progress
    (error "No dispatch in progress"))
  (if buffers
      (setf buffers (delete-dups buffers))
    (setf buffers (list (current-buffer))))
  (conn-protected-let*
      ((cg (mapcan #'prepare-change-group
                   (or buffers (list (current-buffer))))
           (cancel-change-group cg))
       (saved-pos (cl-loop for buf in buffers
                           collect (with-current-buffer buf
                                     (point)))))
    (activate-change-group cg)
    (when (and conn--dispatch-change-groups
               (length> conn--dispatch-change-groups 1))
      (dolist (b (or buffers (list (current-buffer))))
        (with-current-buffer b
          (undo-boundary))))
    (conn-dispatch-undo-case
      ((or :cancel :undo)
       (cancel-change-group cg)
       (cl-loop for buf in buffers
                for pt in saved-pos
                do (with-current-buffer buf
                     (goto-char pt))))
      (:accept
       (accept-change-group cg)))))

(defun conn-dispatch-undo-pulse (beg end)
  "Highlight an undo between BEG and END."
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (face-attribute 'conn-dispatch-undo-pulse-face :background))
  (pulse-momentary-highlight-region
   (min beg end) (max beg end)
   'conn--dispatch-action-current-pulse-face))

(defun conn--dispatch-read-char-message (arguments prompt suffix)
  (conn-<
    (mapcar #'conn-argument-display arguments)
    flatten-tree
    ( :as strs
      (compat-call
       sort strs
       :key (lambda (str)
              (or (get-text-property 0 'conn-read-args-display-depth str)
                  0))))
    (string-join "; ")
    (or "")
    substitute-command-keys
    ( :as argstr
      (concat (propertize prompt 'face 'minibuffer-prompt)
              (unless (string-empty-p argstr)
                (concat " (" argstr ")"))
              (when suffix (concat ": " suffix))
              (when conn--read-args-error-message
                (concat " " (propertize conn--read-args-error-message
                                        'face 'error)))))
    (message "%s")))

(defun conn--dispatch-read-char-1 (&optional use-input-method seconds)
  (condition-case _
      (with-current-buffer conn-dispatch-input-buffer
        (pcase use-input-method
          ((or 't 'nil)
           (read-event nil use-input-method seconds))
          (im
           (let ((pim current-input-method))
             (unwind-protect
                 (progn
                   (activate-input-method im)
                   (read-event nil t seconds))
               (activate-input-method pim))))))
    (quit (car (last (current-input-mode))))))

(defvar conn-dispatch-read-char-pre-functions nil
  ":pre functions for `conn-read-args' during selection.")

(define-conn-state conn-dispatch-read-char-state ()
  "State for reading label characters during dispatch."
  :suppress-input-method t
  :cursor 'hollow)

(conn-add-keymap-reference
 (conn-get-state-map 'conn-dispatch-read-char-state)
 (conn-reference-page
   :name 'conn-dispatch-action-commands
   (((:heading "Action Commands")
     ("toggle repeat" repeat-dispatch)
     ("undo" undo))
    (""
     ("act at mouse click" act)
     ("toggle other end" other-end)))))

(conn-add-keymap-reference
 (conn-get-state-map 'conn-dispatch-read-char-state)
 (conn-reference-page
   :name 'conn-dispatch-read-char-commands
   :depth 50
   (:heading "Miscellaneous Commands")
   ((("isearch forward" isearch-forward)
     ("isearch forward regexp" isearch-forward-regexp)
     ("recursive edit" recursive-edit))
    (("quoted insert" quoted-insert)
     ("toggle input method" toggle-input-method)
     ("set input method" set-input-method)))))

(defun conn-dispatch-read-char (&optional
                                prompt
                                use-input-method
                                seconds
                                prompt-suffix)
  (declare (important-return-value t))
  (cond*
   ((null seconds)
    (let ((message-fn (lambda (prompt arguments &optional _elide)
                        (conn--dispatch-read-char-message
                         arguments
                         prompt
                         prompt-suffix)))
          conn-read-args-message-delay)
      (conn-read-args (conn-dispatch-read-char-state
                       :prompt prompt
                       :command-handler (conn-dispatch-read-char-handlers)
                       :display-handler message-fn
                       :pre (lambda (cmd)
                              (run-hook-with-args
                               'conn-dispatch-read-char-pre-functions
                               cmd)))
          ((char (conn-dispatch-char-argument use-input-method)))
        char)))
   ((bind* (ev (let ((inhibit-message nil)
                     (scroll-conservatively 100))
                 (conn--dispatch-read-char-message nil prompt prompt-suffix)
                 (conn--dispatch-read-char-1 use-input-method seconds)))))
   ((eql ev (car (last (current-input-mode))))
    (signal 'quit nil))
   ((characterp ev) ev)))

(cl-defstruct (conn-dispatch-read-char-handlers
               (:include conn-composite-argument)
               ( :constructor conn-dispatch-read-char-handlers
                 (&aux
                  (value (conn->
                           (compat-call sort
                                        conn--dispatch-read-char-handlers
                                        :key #'cadr)
                           (mapcar #'car)))))))

(defvar-keymap conn-dispatch-prefix-arg-map
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(cl-defstruct (conn-dispatch-prefix-arg
               (:include conn-argument)
               ( :constructor conn-dispatch-prefix-arg
                 (&aux
                  (keymap conn-dispatch-prefix-arg-map)))))

(cl-defmethod conn-argument-display ((_arg conn-dispatch-prefix-arg))
  (propertize
   (cond (conn--read-args-prefix-mag
          (number-to-string
           (* (if conn--read-args-prefix-sign -1 1)
              conn--read-args-prefix-mag)))
         (conn--read-args-prefix-sign "[-1]")
         (t "[1]"))
   'face 'read-multiple-choice-face
   'conn-read-args-display-depth -40))

(define-conn-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql negative-argument)))
  "Invert the prefix argument."
  ( :update (break)
    (cl-callf not conn--read-args-prefix-sign)
    (funcall break)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql universal-argument)))
  "Multiply the prefix argument by four."
  ( :update (break)
    (if conn--read-args-prefix-mag
        (cl-callf * conn--read-args-prefix-mag 4)
      (setf conn--read-args-prefix-mag 4))
    (funcall break)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql digit-argument)))
  "Append a digit to the current prefix argument."
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

(define-conn-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql reset-arg)))
  "Reset the current prefix argument."
  ( :update (break)
    (setf conn--read-args-prefix-mag nil
          conn--read-args-prefix-sign nil)
    (funcall break)))

(defvar-keymap conn-read-char-input-method-map
  "C-\\" 'toggle-input-method
  "C-M-\\" 'set-input-method
  "C-y" 'yank-input-method)

(cl-defstruct (conn-read-char-input-method
               (:include conn-argument)
               ( :constructor conn-read-char-input-method
                 (&aux
                  (keymap conn-read-char-input-method-map)))))

(cl-defmethod conn-argument-display ((_arg conn-read-char-input-method))
  (and-let* ((im (buffer-local-value 'current-input-method-title
                                     conn-dispatch-input-buffer)))
    (propertize im
                'face 'read-multiple-choice-face
                'conn-read-args-display-depth -30)))

(define-conn-dispatch-handler-command ((arg conn-read-char-input-method)
                                       (cmd (eql set-input-method)))
  "Set the current dispatch input method."
  ( :update (_break)
    (let ((inhibit-message nil)
          (message-log-max t)
          (buf conn-dispatch-input-buffer))
      (conn-with-dispatch-suspended
        (with-current-buffer buf
          (activate-input-method
           (read-input-method-name
            (format-prompt "Select input method" nil)
            nil t)))))))

(define-conn-dispatch-handler-command ((arg conn-read-char-input-method)
                                       (cmd (eql toggle-input-method)))
  "Toggle the current dispatch input method."
  ( :update (break)
    (let ((inhibit-message nil)
          (message-log-max t)
          (arg (conn-read-args-consume-prefix-arg))
          (buf conn-dispatch-input-buffer))
      (if arg
          (conn-with-dispatch-suspended
            (with-current-buffer buf
              (toggle-input-method arg)))
        (with-current-buffer buf
          (toggle-input-method))
        (funcall break)))))

(define-conn-dispatch-handler-command ((arg conn-read-char-input-method)
                                       (cmd (eql yank-input-method)))
  "Set the current dispatch input method to the input method in the current
buffer."
  ( :update (break)
    (let ((im (or current-input-method
                  conn--input-method)))
      (with-current-buffer conn-dispatch-input-buffer
        (activate-input-method im)))
    (funcall break)))

(defvar-keymap conn-dispatch-select-command-handler-map
  "C-r" 'recursive-edit
  "<mouse-1>" 'act
  "<mouse-3>" 'undo
  "C-t" 'change-target-finder
  "C-a" 'change-action
  "C-o" 'conn-goto-window
  "C-s" 'isearch-forward
  "C-M-s" 'isearch-regexp-forward
  "C-v" 'scroll-up-command
  "M-v" 'scroll-down-command
  "M-?" 'reference
  "?" 'reference
  "C-h k" 'conn-describe-key
  "C-h c" 'conn-describe-key
  "C-h o" 'conn-describe-symbol
  "C-w" 'restrict-windows
  "M-SPC" 'repeat-dispatch
  "S-SPC" 'repeat-dispatch
  "M-s" conn-search-remap
  "M-g" conn-goto-remap
  "M-h" conn-edit-remap
  (key-description conn-undo-keys) 'undo
  "<dispatch-mouse-repeat>" 'repeat-dispatch-at-mouse
  "C-z" 'other-end
  "<remap> <mouse-drag-region>" 'undefined)

(cl-defstruct (conn-dispatch-select-command-handler
               (:include conn-argument)
               ( :constructor conn-dispatch-select-command-handler
                 (&aux (keymap conn-dispatch-select-command-handler-map)))))

(cl-defmethod conn-argument-display ((_handler conn-dispatch-select-command-handler))
  (list
   (propertize (conn-thing-pretty-print (oref conn-dispatch-target-finder thing))
               'conn-read-args-display-depth -45)
   "\\[reference] help"
   (when (conn-dispatch-other-end-p)
     (concat
      "\\[other-end] "
      (propertize
       "other end"
       'face 'conn-argument-active-face)))
   (when (advice-function-member-p 'restrict-windows
                                   conn-target-window-predicate)
     (concat
      "\\[restrict-windows] "
      (propertize
       "this win"
       'face 'eldoc-highlight-function-argument)))))

(cl-defmethod conn-argument-reference ((_handler conn-dispatch-select-command-handler))
  (list (conn-action-reference conn-dispatch-action)
        (conn-target-finder-reference conn-dispatch-target-finder)))

(eieio-declare-slots thing arg transform)

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql act)))
  "Reset the current prefix argument."
  ( :update (_break)
    (when (mouse-event-p last-input-event)
      (let* ((posn (event-start last-input-event))
             (win (posn-window posn))
             (pt (posn-point posn)))
        (when (and (not (posn-area posn))
                   (funcall conn-target-window-predicate win))
          (:return
           (list pt
                 win
                 (oref conn-dispatch-target-finder thing)
                 (oref conn-dispatch-target-finder arg)
                 (oref conn-dispatch-target-finder transform))))))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql repeat-dispatch-at-mouse)))
  "Reset the current prefix argument."
  ( :update (_break)
    (when (eq 'dispatch-mouse-repeat
              (event-basic-type last-input-event))
      (let* ((posn (event-start last-input-event))
             (win (posn-window posn))
             (pt (posn-point posn)))
        (when (and (not (posn-area posn))
                   (funcall conn-target-window-predicate win))
          (:return
           (list pt
                 win
                 (oref conn-dispatch-target-finder thing)
                 (oref conn-dispatch-target-finder arg)
                 (oref conn-dispatch-target-finder transform))))))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql change-target-finder)))
  "Change the current target finder."
  ( :update (_break)
    (conn-read-args (conn-dispatch-targets-state
                     :prompt "New Targets"
                     :around (lambda (cont)
                               (catch 'dispatch-redisplay
                                 (conn-with-dispatch-suspended
                                   (funcall cont)))))
        ((`(,thing ,arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument)))
      (conn-target-finder-setup
       (conn-get-target-finder thing arg transform))
      (conn-dispatch-redisplay))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql change-action)))
  "Change the current target finder."
  ( :update (_break)
    (conn-read-args (conn-dispatch-state
                     :prompt "New Action"
                     :around (lambda (cont)
                               (catch 'dispatch-redisplay
                                 (conn-with-dispatch-suspended
                                   (funcall cont)))))
        ((`(,action ,repeat)
          (make-conn-dispatch-action-argument
           :keymap conn-dispatch-repeat-argument-map
           :repeat conn-dispatch-repeating))
         (_finish (conn-finished-argument)))
      (if action
          (progn
            (conn-action-setup action repeat)
            (throw 'dispatch-undo nil))
        (conn-dispatch-redisplay)))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql mwheel-scroll)))
  "Mouse wheel scroll."
  ( :update (_break)
    (when (bound-and-true-p mouse-wheel-mode)
      (mwheel-scroll last-input-event))
    (conn-dispatch-redisplay)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql recursive-edit)))
  "Enter a recursive edit with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (conn-with-recursive-stack 'conn-command-state
        (recursive-edit)))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql recenter-top-bottom)))
  "Recenter the display."
  ( :update (_break)
    (let ((this-command 'recenter-top-bottom)
          (last-command 'recenter-top-bottom))
      (recenter-top-bottom (conn-read-args-prefix-arg))
      (unless executing-kbd-macro
        (pulse-momentary-highlight-one-line)))
    (conn-dispatch-redisplay)))

(defun conn--dispatch-recenter-hook (cmd)
  (unless (eq cmd 'recenter-top-bottom)
    (setf recenter-last-op nil)))
(add-hook 'conn-dispatch-read-char-pre-functions
          'conn--dispatch-recenter-hook)

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-forward)))
  "isearch forward with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-forward))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-backward)))
  "isearch backward with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-backward))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-forward-regexp)))
  "isearch forward regexp with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-forward-regexp))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-backward-regexp)))
  "isearch backward regexp with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-backward-regexp))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql end-of-buffer)))
  "Go to the end of the buffer."
  ( :update (_break)
    (goto-char (point-max))
    (conn-dispatch-redisplay)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql beginning-of-buffer)))
  "Go to the beginning of the buffer."
  ( :update (_break)
    (goto-char (point-min))
    (conn-dispatch-redisplay)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql scroll-up-command)))
  "Scroll the window up."
  ( :update (break)
    (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                         next-screen-context-lines)))
      (condition-case err
          (progn
            (scroll-up)
            (conn-dispatch-redisplay))
        (end-of-buffer
         (conn-read-args-error (error-message-string err))
         (funcall break))))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql scroll-down-command)))
  "Scroll the window down."
  ( :update (break)
    (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                         next-screen-context-lines)))
      (condition-case err
          (progn
            (scroll-down)
            (conn-dispatch-redisplay))
        (beginning-of-buffer
         (conn-read-args-error (error-message-string err))
         (funcall break))))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql conn-goto-window)))
  "Select a different window."
  ( :update (break)
    (if-let* ((windows (delq (selected-window)
                             (let ((conn-target-window-predicate
                                    conn-target-window-predicate))
                               (remove-function conn-target-window-predicate
                                                'restrict-windows)
                               (conn-get-dispatch-windows)))))
        (progn
          (conn-dispatch-select-window (conn-prompt-for-window windows))
          (conn-dispatch-redisplay 'maybe-dont-prompt))
      (funcall break))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql finish)))
  "Complete the current dispatch."
  (:update (_break) (throw 'dispatch-exit nil)))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql other-end)))
  "Toggle other end."
  ( :update (break)
    (unless (conn-dispatch-no-other-end-p)
      (if (advice-function-member-p 'toggle conn-dispatch-other-end)
          (remove-function conn-dispatch-other-end 'toggle)
        (add-function :filter-return conn-dispatch-other-end
                      #'- '((name . toggle)
                            (depth . 100))))
      (funcall break))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql restrict-windows)))
  "Restrict targets to the selected window."
  ( :update (_break)
    (cond ((advice-function-member-p 'restrict-windows
                                     conn-target-window-predicate)
           (remove-function conn-target-window-predicate
                            'restrict-windows)
           (conn-dispatch-redisplay))
          ((length> conn-targets 1)
           (add-function :after-while conn-target-window-predicate
                         'conn--dispatch-restrict-windows
                         '((name . restrict-windows)))
           (conn-dispatch-redisplay)))))

(define-conn-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql undo)))
  "Undo the most recent iteration of dispatch."
  ( :update (break)
    (if (null (cdr conn--dispatch-change-groups))
        (progn
          (conn-read-args-error "Nothing to undo")
          (funcall break))
      (dolist (group (prog1 (take 2 conn--dispatch-change-groups)
                       (cl-callf2 drop 2 conn--dispatch-change-groups)))
        (pcase-dolist (`(,_ . ,undo-fn) group)
          (funcall undo-fn :undo)))
      (throw 'dispatch-undo nil))))

;;;;; Dispatch Target Finders

(defclass conn-target-finder-base ()
  ((current-update-handlers :initform nil)
   (update-handlers :allocation :class
                    :initform nil)
   (default-update-handler :allocation :class)
   (window-predicate :initform #'always
                     :initarg :window-predicate)
   (reference :initform nil
              :initarg :reference)
   (description :initform nil
                :initarg :description)
   (label-function :initform nil
                   :initarg :label-function)
   (label-sort-function :initform nil
                        :initarg :label-sort-function)
   (other-end :initform nil
              :initarg :other-end)
   (always-prompt :initform nil
                  :initarg :always-prompt)
   (prompt :initform nil
           :initarg :prompt)
   (thing :initarg :thing)
   (arg :initarg :arg)
   (transform :initarg :transform))
  "Parent class for all dispatch target finders."
  :abstract t)

(eval-and-compile
  (defun define--conn-target-finder (name
                                     superclasses
                                     slots
                                     docstring
                                     properties)
    (let ((update-method (alist-get :update-method properties))
          (default-handler (alist-get :default-update-handler properties)))
      `(progn
         (defclass ,name ,(append superclasses '(conn-target-finder-base))
           ,(append slots
                    (list
                     (pcase default-handler
                       (`#',function
                        `(default-update-handler
                          :allocation :class
                          :initform #',function))
                       (`(,arglist . ,body)
                        `(default-update-handler
                          :allocation :class
                          :initform (lambda ,arglist ,@body)))
                       ('nil)
                       (_ (error "Malformed default update handler definition")))))
           ,@(when docstring (list docstring)))
         :autoload-end
         ,(pcase update-method
            (`((,state-var) . ,body)
             `(cl-defmethod conn-target-finder-update ((,state-var ,name))
                ,@body))
            (`#',function
             `(cl-defmethod conn-target-finder-update ((state ,name))
                (,function state)))
            ('nil)
            (_ (error "Malformed update method definition")))))))

(defmacro define-conn-target-finder (name superclasses slots &rest rest)
  "Define a target finder.

\(fn NAME SUPERCLASSES SLOTS [DOC-STRING] [UPDATE-METHOD DEFAULT-UPDATE-HANDLER])"
  (declare (doc-string 4)
           (indent 3)
           (autoload-macro expand)
           (debug ( form form form
                    [&rest (&define
                            [&or :update-method :default-update-handler]
                            lambda-list
                            def-body)])))
  (let (docstring)
    (when (stringp (car rest))
      (setf docstring (pop rest)))
    (define--conn-target-finder name superclasses slots docstring rest)))

(defun conn-target-finder-reference (target-finder)
  (and-let* ((ref (oref target-finder reference)))
    (cl-typecase ref
      (conn--reference-page ref)
      (t
       (conn-reference-page
         :depth -70
         (:heading
          (format "Target Finder: %s"
                  (conn-thing-pretty-print (oref target-finder thing))))
         (:eval ref))))))

(defun conn-add-update-handler (target-finder function &optional depth)
  (unless depth (setf depth 0))
  (cl-assert (and (integerp depth)
                  (<= -100 depth 100)))
  (setf (alist-get function (oref-default target-finder update-handlers))
        depth)
  (conn--compat-callf sort
      (oref-default target-finder update-handlers)
    :key #'cdr))

(cl-defmethod conn-target-finder-update ((state conn-target-finder-base))
  (conn-dispatch-call-update-handlers state))

(defun conn--find-update-handler (ctors default)
  (if-let* ((fn (caar ctors)))
      (funcall fn (lambda () (conn--find-update-handler (cdr ctors) default)))
    default))

(defun conn-dispatch-call-update-handlers (target-finder &rest args)
  (let ((ufns (oref target-finder current-update-handlers))
        (default nil)
        (handler-ctors nil))
    (dolist (win (nreverse (conn-get-dispatch-windows)))
      (with-selected-window win
        (apply
         (with-memoization (alist-get (current-buffer) ufns)
           (or (conn--find-update-handler
                (with-memoization handler-ctors
                  (oref target-finder update-handlers))
                (with-memoization default
                  (oref target-finder default-update-handler)))
               #'ignore))
         target-finder
         args)))
    (setf (oref target-finder current-update-handlers) ufns)))

(defun conn-target-sort-nearest (targets)
  (declare (side-effect-free t)
           (important-return-value t))
  (compat-call
   sort targets
   :lessp (lambda (a b)
            (< (abs (- (overlay-end a) (point)))
               (abs (- (overlay-end b) (point)))))))

(defun conn-target-sort-adjacent-then-nearest (targets)
  (declare (side-effect-free t)
           (important-return-value t))
  (compat-call
   sort (conn-target-sort-nearest targets)
   :lessp (lambda (a b)
            (and (save-excursion
                   (goto-char (overlay-end a))
                   (not (eolp)))
                 (delq a (conn--overlays-in-of-type
                          (overlay-end a)
                          (+ 2 (overlay-end a))
                          'conn-target-overlay
                          (selected-window)))
                 (not (delq b (conn--overlays-in-of-type
                               (overlay-end b)
                               (+ 2 (overlay-end b))
                               'conn-target-overlay
                               (selected-window))))))))

(defun conn-dispatch-prompt-p ()
  (or conn--dispatch-redisplay-prompt-flag
      conn-dispatch-always-prompt
      (> conn-dispatch-iteration-count 0)
      (oref conn-dispatch-target-finder always-prompt)))

(cl-defgeneric conn-target-finder-select (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-target-finder-select :before (target-finder)
  (let ((old nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(,_ . ,targets) conn-targets)
            (cl-callf2 nconc targets old))
          (setf conn-targets nil
                conn-target-count nil)
          (conn-target-finder-update target-finder)
          (pcase-dolist ((and cons `(,window . ,targets))
                         conn-targets)
            (cl-loop for tar in targets
                     if (<= (window-start window)
                            (overlay-start tar)
                            (overlay-end tar)
                            (window-end window))
                     collect tar into filtered
                     and sum 1 into count
                     else do (delete-overlay tar)
                     finally (setf (alist-get window conn-target-count) count
                                   (cdr cons) filtered))))
      (mapc #'delete-overlay old)))
  (conn-target-finder-label-faces target-finder))

(cl-defmethod conn-target-finder-select :around (target-finder)
  (let ((conn-target-sort-function
         (or (oref target-finder label-sort-function)
             conn-target-sort-function)))
    (let ((inhibit-message t))
      (cl-call-next-method))))

(defvar-keymap conn-kapply-kbd-macro-query-map
  "C-e" 'exit
  "RET" 'exit
  "<return>" 'exit
  "C-n" 'skip)

(cl-defmethod conn-target-finder-select (_target-finder)
  (let ((after nil)
        (conn--dispatch-redisplay-prompt-flag nil))
    (conn-with-dispatch-labels
        (labels (conn-dispatch-get-labels))
      (prog1
          (conn-with-dispatch-handlers
            ( :with (conn-dispatch-select-command-handler))
            (when executing-kbd-macro
              (:handler
               ( :display ()
                 (list "\\[exit] exit macro"
                       "\\[skip] skip iteration"))
               ( :predicate (cmd)
                 (or (eq cmd 'exit) (eq cmd 'skip)))
               (:keymap conn-kapply-kbd-macro-query-map)
               ( :update (cmd _break)
                 (pcase cmd
                   ('skip
                    (setf after (lambda () (setf executing-kbd-macro "")))
                    (:return))
                   ('exit
                    (setf after (lambda () (setf executing-kbd-macro t)))
                    (:return))))))
            (let ((executing-kbd-macro nil)
                  (defining-kbd-macro nil))
              (conn-label-select
               labels
               (concat
                (and-let* ((prompt (oref conn-dispatch-target-finder prompt)))
                  (concat prompt " "))
                (cl-loop for (_ . c) in conn-target-count
                         sum c into count
                         finally return (format "[%s]" count)))
               (conn-dispatch-prompt-p))))
        (when after
          (funcall after)
          (throw 'dispatch-exit nil))))))

(cl-defgeneric conn-target-finder-shelve (target-finder))

(cl-defmethod conn-target-finder-shelve (target-finder)
  target-finder)

(cl-defgeneric conn-get-target-finder (cmd arg transform)
  (declare (conn-anonymous-thing-property :target-finder)))

(cl-defmethod conn-get-target-finder :around (thing arg transform)
  (let ((tf (cl-call-next-method)))
    (unless (slot-boundp tf 'thing)
      (oset tf thing thing))
    (unless (slot-boundp tf 'arg)
      (oset tf arg arg))
    (unless (slot-boundp tf 'transform)
      (oset tf transform transform))
    tf))

(cl-defgeneric conn-target-finder-setup (target-finder))

(cl-defmethod conn-target-finder-setup (tf)
  (unless conn-dispatch-in-progress
    (error "No dispatch in progress"))
  (when conn-dispatch-target-finder
    (conn-target-finder-clear conn-dispatch-target-finder))
  (if-let* ((predicate (oref tf window-predicate)))
      (add-function :after-while conn-target-window-predicate predicate
                    '((name . target-finder)))
    (remove-function conn-target-window-predicate 'target-finder))
  (if-let* ((label-function (oref tf label-function)))
      (add-function :override conn-dispatch-label-function
                    label-function
                    '((name . target-finder)))
    (remove-function conn-dispatch-label-function 'target-finder))
  (add-function :filter-return conn-dispatch-other-end
                (pcase (oref tf other-end)
                  (:no-other-end (lambda (&rest _) 0))
                  ('nil #'+)
                  ('t #'-))
                '((name . target-finder)))
  (setf conn-dispatch-target-finder tf))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing t))
                                      &rest _)
  (conn-dispatch-read-n-chars :string-length 2))

(cl-defgeneric conn-target-finder-update (target-finder))

(cl-defmethod conn-target-finder-update (target-finder)
  (when (functionp target-finder)
    (funcall target-finder)))

(cl-defgeneric conn-target-finder-label-faces (target-finder))

(cl-defmethod conn-target-finder-label-faces (_target-finder)
  (let ((face1 'conn-dispatch-label-face)
        (face2 'conn-dispatch-label-alt-face)
        (multi-face1 'conn-dispatch-label-multi-face)
        (multi-face2 'conn-dispatch-label-multi-alt-face))
    (pcase-dolist (`(,_window . ,targets) conn-targets)
      (dolist (tar (compat-call sort targets :key #'overlay-start))
        (cond ((overlay-get tar 'label-face))
              ((and-let* ((thing (overlay-get tar 'thing))
                          (val (conn-thing-get thing :multi-thing-p)))
                 (or (not (cl-functionp val))
                     (funcall val thing tar)))
               (overlay-put tar 'label-face multi-face1)
               (cl-rotatef multi-face1 multi-face2))
              (t
               (overlay-put tar 'label-face face1)
               (cl-rotatef face1 face2)))))))

(defun conn-clear-targets ()
  (conn-target-finder-clear conn-dispatch-target-finder))

(cl-defgeneric conn-target-finder-clear (target-finder))

(cl-defmethod conn-target-finder-clear (_target-finder)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (delete-overlay target)))
  (setf conn-targets nil
        conn-target-count nil))

(defun conn--mark-targets (property)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (overlay-put target 'category property)
      (overlay-put target 'face nil)))
  (setf conn-target-count nil))

(cl-defgeneric conn-target-finder-suspend (target-finder))

(cl-defmethod conn-target-finder-suspend (_target-finder)
  (conn--mark-targets 'conn-suspended-target))

(cl-defgeneric conn-target-finder-step (target-finder))

(cl-defmethod conn-target-finder-step (_target-finder)
  (conn--mark-targets 'conn-old-target))

(defclass conn-dispatch-target-key-labels-mixin ()
  ()
  "Abstract type for target finders which use key-bindings as labels.

The `conn-target-finder-update' method for any class inheriting from
this class should set the \\='label-key overlay property of each target
to the key binding for that target."
  :abstract t)

(defun conn-dispatch-key-labels ()
  (declare (important-return-value t))
  (let ((labels nil))
    (pcase-dolist (`(,_window . ,targets) conn-targets)
      (dolist (tar targets)
        (push (conn-dispatch-create-label
               tar (overlay-get tar 'label-key))
              labels)))
    labels))

(cl-defmethod conn-target-finder-select ((_tf conn-dispatch-target-key-labels-mixin))
  (conn-with-dispatch-labels
      (labels (conn-dispatch-key-labels))
    (conn-with-dispatch-handlers
      (:with (conn-dispatch-select-command-handler))
      (:handler
       ( :predicate (obj)
         (conn-dispatch-label-p obj))
       ( :update (obj _break)
         (if (conn-dispatch-label-p obj)
             (:return (conn-label-payload obj))
           (conn-read-args-error "Invalid key %s" obj)))
       ( :keymap
         (let ((map (make-sparse-keymap)))
           (cl-loop for label in labels
                    for key = (conn-dispatch-label-string label)
                    do (keymap-set map key label))
           map)))
      (while t (ignore (conn-dispatch-read-char "Register"))))))

(defclass conn-dispatch-retargetable-mixin ()
  ((always-retarget :initform nil
                    :initarg :always-retarget)
   (retarget-tick :initform -1))
  :abstract t)

(cl-defgeneric conn-target-finder-retarget (state)
  (:method ((_ conn-dispatch-retargetable-mixin)) nil))

(cl-defmethod conn-target-finder-setup ((tf conn-dispatch-retargetable-mixin))
  (oset tf retarget-tick -1)
  (cl-call-next-method))

(defvar-keymap conn-dispatch-retargeting-argument-map
  "C-M-r" 'always-retarget
  "M-r" 'retarget)

(cl-defmethod conn-target-finder-select :around ((state conn-dispatch-retargetable-mixin))
  (conn-with-dispatch-handlers
    (:handler
     ( :keymap conn-dispatch-retargeting-argument-map)
     ( :display ()
       (list
        (unless (oref state always-retarget)
          "\\[retarget] retarget")
        (when conn-dispatch-repeating
          (concat
           "\\[always-retarget] "
           (propertize "always retarget"
                       'face (when (oref state always-retarget)
                               'eldoc-highlight-function-argument))))))
     ( :predicate (cmd)
       (or (eq cmd 'always-retarget)
           (eq cmd 'retarget)))
     ( :update (cmd _break)
       (pcase cmd
         ('always-retarget
          (cl-callf not (oref state always-retarget))
          (conn-dispatch-redisplay))
         ('retarget
          (conn-target-finder-retarget state)
          (conn-dispatch-redisplay 'maybe-dont-prompt)))))
    (when (or (oref state always-retarget)
              (= (oref state retarget-tick)
                 conn-dispatch-iteration-count))
      (conn-target-finder-retarget state))
    (prog1 (cl-call-next-method)
      (let ((prev (cl-shiftf (oref state retarget-tick)
                             conn-dispatch-iteration-count)))
        (conn-dispatch-undo-case
          (:undo (setf (oref state retarget-tick) prev)))))))

(define-conn-target-finder conn-dispatch-string-targets
    (conn-dispatch-retargetable-mixin)
    ((string :initform nil
             :initarg :string)
     (predicate :initform nil
                :initarg :predicate)
     (regex-p :initform nil
              :initarg :regex-p))
  ( :default-update-handler (state &optional len)
    (let ((string (oref state string))
          (predicate (oref state predicate)))
      (if (oref state regex-p)
          (conn-make-re-target-overlays string predicate len)
        (conn-make-string-target-overlays string predicate len)))))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(defvar conn-dispatch-read-n-chars-re-alist
  `((?, . "[^a-zA-A]"))
  "Alist of (CHAR . REGEXP) for read-n-chars targets.")

(define-conn-target-finder conn-dispatch-read-n-chars
    (conn-dispatch-string-targets)
    ((string-length :initform 1
                    :initarg :string-length)
     (regex-p :initform t)
     (literal :initform 1
              :initarg :literal))
  ( :update-method (state)
    (if (oref state string)
        (conn-dispatch-call-update-handlers state 0)
      (let* ((string-length (oref state string-length))
             (literal (oref state literal))
             (prompt (if (> string-length 1)
                         (propertize (format "%d Chars" string-length)
                                     'face 'minibuffer-prompt)
                       (propertize "1 Char" 'face 'minibuffer-prompt)))
             (char-count 0)
             (literal-count 0)
             (quote-flag nil))
        (while (or (< literal-count literal)
                   (< char-count string-length))
          (unless (= literal-count 0)
            (while-no-input
              (conn-dispatch-call-update-handlers state)))
          (catch 'dispatch-redisplay
            (conn-with-dispatch-handlers
              (:handler
               (:predicate (cmd) (memq cmd '(backspace quote)))
               (:keymap (define-keymap
                          "<backspace>" 'backspace
                          "<remap> <quoted-insert>" 'quote))
               ( :update (cmd _break)
                 (pcase cmd
                   ('backspace
                    (when (length> (oref state string) 0)
                      (cl-callf substring (oref state string) 0 -1))
                    (:return))
                   ('quote
                    (setf quote-flag t)
                    (:return)))))
              (let ((char (conn-dispatch-read-char
                           prompt t nil
                           (oref state string))))
                (cl-callf concat (oref state string)
                  (or (and (not quote-flag)
                           (alist-get char conn-dispatch-read-n-chars-re-alist))
                      (progn
                        (incf literal-count)
                        (regexp-quote (char-to-string char))))))
              (incf char-count)
              (setf quote-flag nil)))
          (conn-clear-targets))
        (setf (oref state prompt) (oref state string))
        (conn-dispatch-call-update-handlers state 0)))))

(defvar-keymap conn-dispatch-read-string-target-keymap
  "M-e" 'read-string)

(defun conn-dispatch-lazy-update (updater)
  (let ((unwind (make-symbol "unwind"))
        (after-change (make-symbol "after-change"))
        (pred (make-symbol "pred"))
        (timer (timer-create)))
    (fset pred (lambda (win) (not (window-minibuffer-p win))))
    (fset unwind
          (lambda ()
            (remove-function conn-target-window-predicate pred)
            (remove-hook 'after-change-functions after-change t)
            (remove-hook 'minibuffer-exit-hook unwind t)
            (conn-clear-targets)))
    (fset after-change
          (lambda (_beg _end _len)
            (cancel-timer timer)
            (let ((inhibit-redisplay t)
                  (string (minibuffer-contents)))
              (timer-set-function
               timer
               (lambda ()
                 (conn-clear-targets)
                 (unless (equal string "")
                   (with-minibuffer-selected-window
                     (funcall updater string)))))
              (timer-set-idle-time timer 0.05 nil)
              (timer-activate-when-idle timer t))))
    (lambda ()
      (add-function :before-while conn-target-window-predicate pred)
      (add-hook 'minibuffer-exit-hook unwind nil t)
      (add-hook 'after-change-functions after-change nil t))))

(define-conn-target-finder conn-dispatch-read-with-timeout
    (conn-dispatch-string-targets)
    ((timeout :initform 0.5 :initarg :timeout))
  ( :update-method (state)
    (unless (oref state string)
      (catch 'string-read
        (let ((timeout (oref state timeout))
              (prompt (propertize "String" 'face 'minibuffer-prompt)))
          (conn-with-dispatch-handlers
            (:handler
             (:keymap conn-dispatch-read-string-target-keymap)
             (:predicate (cmd) (eq cmd 'read-string))
             (:display () "\\[read-string] edit string")
             ( :update (_cmd _break)
               (let* ((newstr
                       (minibuffer-with-setup-hook
                           (conn-dispatch-lazy-update
                            (lambda (str)
                              (setf (oref state string) str)
                              (while-no-input
                                (conn-dispatch-call-update-handlers state))))
                         (let ((inhibit-message nil))
                           (with-current-buffer conn-dispatch-input-buffer
                             (read-string
                              "String: " (oref state string)
                              'conn-read-string-target-history
                              nil t))))))
                 (unless (equal newstr "")
                   (setf (oref state string) newstr
                         (oref state prompt) newstr)
                   (throw 'string-read nil)))))
            (setf (oref state string)
                  (char-to-string (conn-dispatch-read-char prompt t))))
          (while-no-input
            (conn-dispatch-call-update-handlers state))
          (while-let ((next-char (conn-dispatch-read-char
                                  prompt t timeout
                                  (oref state string))))
            (conn-clear-targets)
            (cl-callf concat (oref state string)
              (char-to-string next-char))
            (conn-dispatch-call-update-handlers state))
          (setf (oref state prompt) (oref state string))))
      (conn-clear-targets))
    (conn-dispatch-call-update-handlers state 0)))

(defclass conn-dispatch-focus-mixin ()
  ((hidden :initform nil)
   (hide :initform t
         :initarg :hide)
   (context-lines :initform 0
                  :initarg :context-lines)
   (cursor-location :initform nil)
   (separator-p :initarg :separator)
   (fringe-indicator :initform (propertize " "
                                           'display (list 'left-fringe
                                                          'right-triangle))
                     :initarg :fringe-indicator)
   (always-prompt :initform t))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(defvar-keymap conn-dispatch-toggle-focus-map)

(defun conn-focus-targets-remove-overlays (state)
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil))

(cl-defmethod conn-target-finder-clear ((state conn-dispatch-focus-mixin))
  (conn-focus-targets-remove-overlays state)
  (cl-call-next-method))

(cl-defmethod conn-target-finder-suspend ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs)
    (setf (alist-get win (oref state hidden)) nil))
  (cl-call-next-method))

(cl-defmethod conn-target-finder-step ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (dolist (ov ovs)
      (overlay-put ov 'before-string nil)))
  (cl-call-next-method))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-focus-mixin))
  (conn-focus-targets-remove-overlays state))

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,win ,tick . ,ovs) (oref state hidden))
    (unless (eql tick (buffer-chars-modified-tick (window-buffer win)))
      (mapc #'delete-overlay ovs)
      (setf (alist-get win (oref state hidden)) nil))))

(cl-defmethod conn-target-finder-update :after ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,win . ,targets) conn-targets)
    (pcase-let* ((`(,tick . ,old-hidden)
                  (alist-get win (oref state hidden)))
                 (context-lines (oref state context-lines))
                 (regions (list (cons (pos-bol) (pos-bol 2))))
                 (fringe-indicator (oref state fringe-indicator))
                 (line nil))
      (with-selected-window win
        (setf line (1- (count-screen-lines (window-start) (point) t)))
        (if (not (oref state hide))
            (progn
              (mapc #'delete-overlay old-hidden)
              (setf (alist-get win (oref state hidden)) nil)
              (recenter line))
          (conn-protected-let* ((hidden nil (mapc #'delete-overlay hidden)))
            (if (eql tick (buffer-chars-modified-tick (window-buffer win)))
                (dolist (ov old-hidden)
                  (overlay-put ov 'before-string fringe-indicator))
              (mapc #'delete-overlay old-hidden)
              (save-excursion
                (dolist (tar targets)
                  (push (or (overlay-get tar 'context)
                            (progn
                              (goto-char (overlay-start tar))
                              (let ((beg (pos-bol (- 1 context-lines)))
                                    (end (pos-bol (+ 2 context-lines))))
                                (cons (max (if (invisible-p end) (1- beg) beg)
                                           (point-min))
                                      end))))
                        regions)))
              (cl-callf conn--merge-overlapping-regions regions t)
              (conn--compat-callf sort regions :key #'car :in-place t)
              (cl-loop for region in regions
                       for (beg . end) = region
                       sum (count-lines beg end) into lines
                       do (setf (car region) (max (1- beg) (point-min))
                                (cdr region) (1- end))
                       finally (let ((diff (- (ceiling (window-screen-lines))
                                              lines)))
                                 (when (> diff 0)
                                   (save-excursion
                                     (goto-char (caar regions))
                                     (setf (caar regions) (pos-bol (- diff)))))))
              (cl-loop
               for beg = (point-min) then next-beg
               for (end . next-beg) in regions
               while end do
               (let ((ov (make-overlay beg end)))
                 (push ov hidden)
                 (overlay-put ov 'invisible 'conn-dispatch-invisible)
                 (overlay-put ov 'window win)
                 (overlay-put ov 'before-string fringe-indicator))
               finally
               (let ((ov (make-overlay beg (point-max))))
                 (push ov hidden)
                 (overlay-put ov 'window win)
                 (overlay-put ov 'invisible 'conn-dispatch-invisible)))
              (setf (alist-get win (oref state hidden))
                    (cons (buffer-chars-modified-tick) hidden))
              (recenter line)))))))
  (redisplay))

(define-conn-target-finder conn-dispatch-focus-thing-at-point
    (conn-dispatch-string-targets
     conn-dispatch-focus-mixin)
    ((context-lines :initform 1
                    :initarg :context-lines)
     (window-predicate :initarg :window-predicate
                       :initform (lambda (win)
                                   (eq (window-buffer win)
                                       (current-buffer)))))
  ( :default-update-handler (state)
    (let* ((line-height (window-height))
           (string (oref state string))
           (prev (point))
           (line-count 0)
           (matches nil)
           (thing (conn-anonymous-thing
                    '(region)
                    :bounds-op ( :method (_self _arg)
                                 (and-let* ((bd (assq (point) matches)))
                                   (conn-make-bounds 'region nil bd))))))
      (save-excursion
        (while (and (< line-count line-height)
                    (search-backward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (push (cons beg end) matches)
            (conn-make-target-overlay beg 0 :thing thing)
            (when (<= beg (pos-eol) prev)
              (incf line-count))
            (setf prev beg))))
      (setf line-count 0)
      (save-excursion
        (while (and (< line-count line-height)
                    (search-forward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (conn-make-target-overlay beg 0 :thing thing)
            (when (<= prev (pos-bol) beg)
              (incf line-count))
            (setf prev beg)))))))

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-focus-thing-at-point))
  (conn-focus-targets-remove-overlays state))

(define-conn-target-finder conn-dispatch-jump-ring
    (conn-dispatch-focus-mixin)
    ((context-lines :initform 2
                    :initarg :context-lines)
     (window-predicate :initform (lambda (win) (eq win (selected-window))))
     (other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (let ((points (conn-ring-list conn-jump-ring)))
      (dolist (pt points)
        (unless (invisible-p pt)
          (conn-make-target-overlay pt 0)))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(define-conn-target-finder conn-dispatch-mark-register
    (conn-dispatch-focus-mixin
     conn-dispatch-target-key-labels-mixin)
    ((context-lines :initform 1
                    :initarg :context-lines)
     (other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (pcase-dolist (`(,key . ,obj) register-alist)
      (when (and (markerp obj)
                 (eq (current-buffer) (marker-buffer obj)))
        (conn-make-target-overlay
         obj 0
         :properties `(label-key ,(key-description (vector key)))))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(define-conn-target-finder conn-dispatch-previous-emacs-state
    (conn-dispatch-focus-mixin)
    ((context-lines :initform 1
                    :initarg :context-lines)
     (window-predicate :initform (lambda (win) (eq win (selected-window))))
     (other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (dolist (pt (conn-ring-list conn-emacs-state-ring))
      (unless (invisible-p pt)
        (conn-make-target-overlay pt 0))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(define-conn-target-finder conn-dispatch-headings
    (conn-dispatch-focus-mixin)
    ((cache :initform nil))
  ( :default-update-handler (state)
    (cl-symbol-macrolet ((cache (oref state cache)))
      (when (or (derived-mode-p (list 'outline-mode))
                (bound-and-true-p outline-minor-mode))
        (unless (and-let* ((cached (alist-get (current-buffer) cache)))
                  (= (car cached) (buffer-chars-modified-tick)))
          (let ((pts nil))
            (setf (alist-get (current-buffer) cache)
                  (cons (buffer-chars-modified-tick)
                        (progn
                          (save-excursion
                            (catch 'break
                              (while t
                                (unless (outline-next-visible-heading -1)
                                  (throw 'break nil))
                                (push (point) pts))))
                          (save-excursion
                            (catch 'break
                              (while t
                                (unless (outline-next-visible-heading 1)
                                  (throw 'break nil))
                                (push (point) pts))))
                          pts)))))
        (dolist (pt (cdr (alist-get (current-buffer) cache)))
          (conn-make-target-overlay pt 0))))))

(define-conn-target-finder conn-dispatch-all-defuns
    (conn-dispatch-focus-mixin)
    ((cache :initform nil)
     (window-predicate :initform (lambda (win)
                                   (eq (window-buffer win)
                                       (current-buffer)))))
  ( :default-update-handler (state)
    (cl-symbol-macrolet ((cache (oref state cache)))
      (unless (and-let* ((cached (alist-get (current-buffer) cache)))
                (= (car cached) (buffer-chars-modified-tick)))
        (setf (alist-get (current-buffer) cache)
              (cons (buffer-chars-modified-tick)
                    (let ((pts nil))
                      (conn-for-each-visible (point-min) (point-max)
                        (goto-char (point-max))
                        (while (beginning-of-defun)
                          (push (point) pts)))
                      pts))))
      (dolist (pt (cdr (alist-get (current-buffer) cache)))
        (conn-make-target-overlay pt 0)))))

(define-conn-target-finder conn-all-things-targets
    ()
    ((all-things :initarg :all-things))
  ( :default-update-handler (state)
    (let ((thing (oref state all-things)))
      (conn-for-each-visible
          (max (1- (window-start))
               (point-min))
          (window-end)
        (goto-char (point-max))
        (while (and (/= (point) (point-min))
                    (/= (point) (progn
                                  (forward-thing thing -1)
                                  (point))))
          (when (thing-at-point thing)
            (conn-make-target-overlay (point) 0)))))))

(define-conn-target-finder conn-dispatch-button-targets
    ()
    ((other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-max))
      (while (not (bobp))
        (goto-char (previous-single-char-property-change (point) 'button))
        (when (get-char-property (point) 'button)
          (conn-make-target-overlay (point) 0))))))

(define-conn-target-finder conn-dispatch-regexp-targets
    ()
    ((regexp :initarg :regexp)
     (fixed-length :initform nil
                   :initarg :fixed-length))
  ( :default-update-handler (state)
    (let ((regexp (oref state regexp))
          (fixed-length (oref state fixed-length)))
      (save-excursion
        (goto-char (window-start))
        (pcase-dolist (`(,beg . ,end) (conn--visible-re-matches regexp))
          (conn-make-target-overlay
           beg (or fixed-length (- end beg))
           :thing (conn-anonymous-thing
                    '(point)
                    :bounds-op ( :method (_self _arg)
                                 (save-match-data
                                   (when (looking-at regexp)
                                     (conn-make-bounds
                                      'point nil
                                      (cons (point) (match-end 0)))))))))))))

(defun conn-dispatch-things-read-prefix (thing prefix-length)
  (declare (important-return-value t))
  (conn-dispatch-read-n-chars
   :literal 0
   :string-length prefix-length
   :predicate (lambda (beg _end)
                (save-excursion
                  (goto-char beg)
                  (pcase (ignore-errors (conn-bounds-of thing nil))
                    ((conn-bounds `(,tbeg . ,_tend))
                     (= beg tbeg)))))
   :reference (conn-reference-page
                (:heading "Thing With Prefix")
                ,(string-fill
                  (format "Read a prefix string of %s characters for a %s target."
                          prefix-length thing)
                  80))))

(define-conn-target-finder conn-dispatch-things-with-prefix-targets
    ()
    ((prefix-string :initarg :prefix-string)
     (prefix-thing :initarg :prefix-thing)
     (fixed-length :initform nil
                   :initarg :fixed-length))
  ( :default-update-handler (state)
    (let ((thing (oref state prefix-thing))
          (prefix (oref state prefix-string))
          (fixed-length (oref state fixed-length)))
      (conn-make-string-target-overlays
       prefix
       (lambda (beg _end)
         (save-excursion
           (goto-char beg)
           (pcase (ignore-errors (conn-bounds-of thing nil))
             ((conn-bounds `(,tbeg . ,_tend))
              (= beg tbeg)))))
       fixed-length))))

(define-conn-target-finder conn-dispatch-things-with-re-prefix-targets
    ()
    ((prefix-regexp :initarg :prefix-regexp)
     (skip-prefix :initarg :skip-prefix
                  :initform nil)
     (fixed-length :initform nil
                   :initarg :fixed-length)
     (prefix-thing :initarg :prefix-thing))
  ( :default-update-handler (state)
    (let ((thing (oref state prefix-thing))
          (prefix (oref state prefix-regexp))
          (fixed-length (oref state fixed-length))
          (skip-prefix (oref state skip-prefix)))
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-re-matches
                      prefix
                      (lambda (beg end)
                        (save-excursion
                          (goto-char beg)
                          (pcase (ignore-errors (conn-bounds-of thing nil))
                            ((conn-bounds `(,tbeg . ,tend))
                             (and (= tbeg beg) (<= end tend))))))))
        (conn-make-target-overlay
         beg (or fixed-length (- end beg))
         :point (when skip-prefix end))))))

(define-conn-target-finder conn-dispatch-things-matching-re-targets
    ()
    ((regexp :initarg :prefix-regexp)
     (match-thing :initarg :match-thing)
     (fixed-length :initform nil
                   :initarg :fixed-length)
     (update-function
      :allocation :class
      :initform #'conn--things-matching-re-update))
  ( :default-update-handler (state)
    (let ((thing (oref state match-thing))
          (regexp (oref state regexp))
          (fixed-length (oref state fixed-length)))
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-re-matches
                      regexp
                      (lambda (beg end)
                        (save-excursion
                          (goto-char beg)
                          (pcase (ignore-errors (conn-bounds-of thing nil))
                            ((conn-bounds `(,tbeg . ,tend))
                             (and (<= tbeg beg) (<= tend end))))))))
        (conn-make-target-overlay beg (or fixed-length (- end beg)))))))

(define-conn-target-finder conn-dispatch-column-targets
    ()
    ((goal-column :initform nil)
     (window-predicate :initform (lambda (win)
                                   (eq win (selected-window)))))
  ( :default-update-handler (state)
    (let ((goal-col
           (with-memoization (oref state goal-column)
             ;; From `line-move-visual'
             (let ((posn (posn-at-point))
                   (lnum-width (line-number-display-width t))
                   x-pos)
               (cond
                ;; Handle the `overflow-newline-into-fringe' case
                ;; (left-fringe is for the R2L case):
                ((memq (nth 1 posn) '(right-fringe left-fringe))
                 (window-width))
                ((car (posn-x-y posn))
                 (setf x-pos (- (car (posn-x-y posn)) lnum-width))
                 ;; In R2L lines, the X pixel coordinate is measured from the
                 ;; left edge of the window, but columns are still counted
                 ;; from the logical-order beginning of the line, i.e. from
                 ;; the right edge in this case.  We need to adjust for that.
                 (if (eq (current-bidi-paragraph-direction) 'right-to-left)
                     (setf x-pos (- (window-body-width nil t) 1 x-pos)))
                 (/ (float x-pos)
                    (frame-char-width))))))))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (while (/= (point) (point-max))
            (vertical-motion (cons goal-col 0))
            (unless (and (eolp) (= (point) (point-min)))
              (conn-make-target-overlay
               (point) 0
               :thing 'point
               :padding-function (lambda (ov width _face)
                                   (conn--flush-left-padding ov width nil))))
            (vertical-motion 1)))))))

(cl-defmethod conn-target-finder-label-faces ((_ conn-dispatch-column-targets))
  nil)

(define-conn-target-finder conn-dispatch-line-targets
    () ()
  ( :default-update-handler (_state)
    (let ((col (if (= 0 (window-hscroll)) 0 1)))
      (save-excursion
        (goto-char (window-start))
        (vertical-motion (cons col 0))
        (when (< (point) (window-end))
          (conn-make-target-overlay
           (point) 0
           :padding-function #'conn--flush-left-padding))
        (while (progn
                 (vertical-motion (cons col 1))
                 (< (point) (window-end)))
          (conn-make-target-overlay
           (point) 0
           :padding-function #'conn--flush-left-padding))))))

(cl-defmethod conn-target-finder-label-faces ((_ conn-dispatch-line-targets))
  nil)

(define-conn-target-finder conn-dispatch-end-of-line-targets
    ()
    ((other-end :initform t))
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
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
              (conn-make-target-overlay
               (point) 0
               ;; hack to get the label displayed on its own line
               :properties `(after-string
                             ,(propertize " " 'display '(space :width 0))))
            (conn-make-target-overlay (point) 0)))))))

(define-conn-target-finder conn-dispatch-inner-line-targets
    () ()
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-max))
      (while (let ((pt (point)))
               (forward-line -1)
               (back-to-indentation)
               (/= (point) pt))
        (unless (= (pos-bol) (pos-eol))
          (conn-make-target-overlay
           (point) 0
           :thing 'conn-forward-inner-line))))))

(define-conn-target-finder conn-dispatch-end-of-inner-line-targets
    ()
    ((other-end :initform t))
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-min))
      (while (not (eobp))
        (conn--end-of-inner-line-1)
        (cond
         ((= (pos-bol) (pos-eol)))
         ((= (pos-bol) (point))
          (conn-make-target-overlay
           (point) 0
           :point (save-excursion
                    (back-to-indentation)
                    (point))
           :thing 'conn-forward-inner-line))
         (t
          (conn-make-target-overlay
           (point) 0
           :point (save-excursion
                    (back-to-indentation)
                    (point))
           :properties `(label-before t)
           :thing 'conn-forward-inner-line)))
        (forward-line 1)))))

(define-conn-target-finder conn-dispatch-visual-line-targets
    () ()
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-min))
      (if (looking-at-p "\n")
          (vertical-motion 1)
        (vertical-motion 0))
      (conn-make-target-overlay
       (point) 0
       :padding-function (lambda (ov width _face)
                           (conn--flush-left-padding ov width nil)))
      (vertical-motion 1)
      (cl-loop
       (when (bolp)
         (conn-make-target-overlay
          (point) 0
          :padding-function (lambda (ov width _face)
                              (conn--flush-left-padding ov width nil))))
       (when (eobp) (cl-return nil))
       (vertical-motion 1)))))

(define-conn-target-finder conn-dispatch-end-of-visual-line-targets
    () ()
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-min))
      (if (looking-at-p "\n")
          (vertical-motion 2)
        (vertical-motion 1))
      (conn-make-target-overlay
       (if (bolp) (1- (point)) (point))
       0
       :padding-function (lambda (ov width _face)
                           (conn--flush-left-padding ov width nil))
       :properties `(label-before t))
      (cl-loop
       (conn-make-target-overlay
        (if (bolp) (1- (point)) (point))
        0
        :properties `(label-before t)
        :padding-function (lambda (ov width _face)
                            (conn--flush-left-padding ov width nil)))
       (when (eobp) (cl-return nil))
       (vertical-motion 1)))))

;;;;; Dispatch Labels

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (pcase-let* (((cl-struct conn-dispatch-label string target)
                label)
               (start (overlay-start target))
               (point (or (overlay-get target 'point) start))
               (win (overlay-get target 'window))
               (face (overlay-get target 'label-face)))
    (conn-dispatch-undo-case
      (:undo
       (conn-make-target-overlay
        start 0
        :window win
        :properties `( label-face ,face
                       label-string ,string
                       category conn-old-target))))
    (list point
          win
          (or (overlay-get target 'thing)
              (oref conn-dispatch-target-finder thing))
          (oref conn-dispatch-target-finder arg)
          (oref conn-dispatch-target-finder transform))))

(cl-defmethod conn-label-reset ((label conn-dispatch-label))
  (setf (conn-dispatch-label-narrowed-string label)
        (conn-dispatch-label-string label)))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label)))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label)
                                 prefix-char)
  (if (not (thread-first
             (conn-dispatch-label-narrowed-string label)
             (string-to-char)
             (eql prefix-char)))
      (setf (conn-dispatch-label-narrowed-string label) nil)
    (cl-callf substring (conn-dispatch-label-narrowed-string label) 1)
    label))

(cl-defmethod conn-label-completed-p ((label conn-dispatch-label))
  (string-empty-p (conn-dispatch-label-narrowed-string label)))

(cl-defmethod conn-label-clear ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label overlay target)
               label))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'before-string nil)
    (overlay-put overlay 'after-string nil)
    (overlay-put target 'face nil)))

(cl-defmethod conn-label-setup ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label
                          overlay
                          target
                          narrowed-string
                          setup-function)
               label))
    (unless (length< narrowed-string 1)
      (overlay-put target 'face 'conn-target-overlay-face)
      (with-selected-window (overlay-get overlay 'window)
        (let ((throw-on-input nil))
          (funcall setup-function label))))))

(cl-defmethod conn-label-display ((label conn-dispatch-label))
  (pcase-let* (((cl-struct conn-dispatch-label overlay) label)
               (setup (cl-shiftf (overlay-get overlay 'setup) nil))
               (padding (cl-shiftf (overlay-get overlay 'padding) nil)))
    (when setup (apply #'overlay-put overlay setup))
    (when padding (apply padding))))

;;;;; Dispatch Actions

(cl-defmethod conn-get-default-action ((_cmd (conn-thing t)))
  (conn-dispatch-jump))

(cl-defmethod conn-get-default-action ((_cmd (conn-thing line-column)))
  (conn-dispatch-jump))

(defvar conn-dispatch-button-functions nil)

(defun conn-dispatch-button-handler-default (pt)
  (save-excursion
    (goto-char pt)
    (cond ((thing-at-point 'url t)
           (browse-url-at-point))
          ((button-at pt)
           (push-button pt)
           t)
          ((fboundp 'widget-apply-action)
           (ignore-errors
             (widget-apply-action (get-char-property pt 'button) pt)
             t)))))

(add-hook 'conn-dispatch-button-functions 'conn-dispatch-button-handler-default 50)

(defun conn-dispatch-push-button ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "Push Button")
    (:reference "Push the selected button.")
    (:no-history t)
    (pcase-let ((`(,pt ,window ,_thing ,_arg ,_transform)
                 (conn-select-target)))
      (with-selected-window window
        (run-hook-with-args-until-success 'conn-dispatch-button-functions pt)))))

(defun conn-dispatch-copy-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument-dwim))
       (ftransform (conn-transform-argument)))
    (conn-action ((str (pcase (conn-bounds-of fthing farg)
                         ((conn-bounds `(,beg . ,end) ftransform)
                          (conn-dispatch-action-pulse beg end)
                          (filter-buffer-substring beg end))
                         (_ (user-error "No %s found"
                                        (conn-thing-pretty-print fthing)))))
                  (replace-and-separator
                   (conn-dispatch-to-how-argument)))
      (:description "Copy To")
      (:window-predicate
       (lambda (win)
         (not
          (buffer-local-value 'buffer-read-only
                              (window-buffer win)))))
      (:reference
       "Copy the current region to the region selected by dispatch.  By default
this action copies the current region before the region selected by
dispatch but if OTHER-END is non-nil then it copies the current region
after the region selected by dispatch.")
      (pcase-let ((`(,replace ,separator) replace-and-separator)
                  (`(,pt ,window ,thing ,arg ,transform)
                   (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-mark-and-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-dispatch-bounds `(,beg . ,end) transform)
               (if (and replace (<= beg (point) end))
                   (conn-dispatch-goto-char beg 'nopush)
                 (goto-char beg))
               (cond (replace
                      (delete-region beg end))
                     ((and separator (< end beg))
                      (insert (conn-kill-separator-for-strings str separator))))
               (insert-for-yank str)
               (conn-dispatch-action-pulse
                (- (point) (length str))
                (point))
               (when (and separator (not replace) (not (< end beg)))
                 (insert (conn-kill-separator-for-strings str separator))))
              (_ (user-error "Cannot find thing at point")))))))))

(defun conn-dispatch-yank-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ((str (current-kill 0))
                (replace-and-separator
                 (conn-dispatch-to-how-argument)))
    (:description "Yank To")
    (:window-predicate
     (lambda (win)
       (not
        (buffer-local-value 'buffer-read-only
                            (window-buffer win)))))
    (:reference
     "Yank the most recent kill to the region selected by dispatch.  By
default this action inserts the text before the region selected by
dispatch but if OTHER-END is non-nil then it inserts the text after the
region selected by dispatch.")
    (pcase-let ((`(,replace ,separator) replace-and-separator)
                (`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (if (and replace (<= beg (point) end))
                 (conn-dispatch-goto-char beg 'nopush)
               (goto-char beg))
             (cond (replace
                    (delete-region beg end))
                   ((and separator (< end beg))
                    (insert (conn-kill-separator-for-strings str separator))))
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str))
              (point))
             (when (and separator (not replace) (not (< end beg)))
               (insert (conn-kill-separator-for-strings str separator))))
            (_ (user-error "Cannot find thing at point"))))))))

(defun conn-dispatch-reading-yank-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ((str (read-from-kill-ring "Yank To: "))
                (replace-and-separator
                 (conn-dispatch-to-how-argument)))
    (:description "Yank To")
    (:window-predicate
     (lambda (win)
       (not
        (buffer-local-value 'buffer-read-only
                            (window-buffer win)))))
    (:reference
     "Select a string from the kill ring and insert it at the region selected
by dispatch.  By default this action inserts the string before the
region selected by dispatch but if OTHER-END is non-nil then it inserts
the string after the region selected by dispatch.")
    (pcase-let ((`(,replace ,separator) replace-and-separator)
                (`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (if (and replace (<= beg (point) end))
                 (conn-dispatch-goto-char beg 'nopush)
               (goto-char beg))
             (cond (replace
                    (delete-region beg end))
                   ((and separator (< end beg))
                    (insert (conn-kill-separator-for-strings str separator))))
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str))
              (point))
             (when (and separator (not replace) (not (< end beg)))
               (insert (conn-kill-separator-for-strings str separator))))
            (_ (user-error "Cannot find thing at point"))))))))

(defun conn-dispatch-send ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (declare-function conn-kill-thing "conn-commands")
  (conn-action ((_cg (conn-action-change-group))
                (str (conn-read-args (conn-kill-state
                                      :prompt "Send Thing")
                         ((`(,thing ,arg)
                           (conn-thing-argument-dwim))
                          (transform (conn-transform-argument))
                          (fixup (conn-reformat-argument
                                  (not (region-active-p))))
                          (check-bounds
                           (conn-boolean-argument
                            "check bounds"
                            'check-bounds
                            conn-check-bounds-argument-map
                            :value t)))
                       (save-excursion
                         (conn-kill-thing thing arg transform
                                          nil nil nil nil
                                          fixup check-bounds)
                         (current-kill 0))))
                (replace-and-separator
                 (conn-dispatch-to-how-argument)))
    (:description "Send")
    (:window-predicate
     (lambda (win)
       (not
        (buffer-local-value 'buffer-read-only
                            (window-buffer win)))))
    (:reference
     "Delete a thing at point and replace a region selected by dispatch with
it.")
    (pcase-let ((`(,replace ,separator) replace-and-separator)
                (`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (if (and replace (<= beg (point) end))
                 (conn-dispatch-goto-char beg 'nopush)
               (goto-char beg))
             (cond (replace
                    (delete-region beg end))
                   ((and separator (< end beg))
                    (insert (conn-kill-separator-for-strings str separator))))
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point))
             (when (and separator (not replace) (not (< end beg)))
               (insert (conn-kill-separator-for-strings str separator))))
            (_ (user-error "Cannot find thing at point"))))))))

(defun conn-dispatch-register-load ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ((register (register-read-with-preview "Register: ")
                          :read t)
                (replace (conn-boolean-argument
                          "replace"
                          'dispatch-replace
                          conn-dispatch-replace-argument-map)
                         :read t))
    (:description (format "Register <%c>" register))
    (:reference
     "Replace region selected by dispatch with contents of register.")
    (declare-function conn-register-load "conn-commands")
    (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (when replace (delete-region beg end))
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(defun conn-dispatch-copy-from ()
  "Copy a thing from somewhere else.
\\<conn-dispatch-replace-argument-map>Replace a thing at point with \\[dispatch-replace].
\\<conn-separator-argument-map>Insert a separator between multiple copies with \\[separator]."
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ((cg (conn-action-replace))
                (opoint (conn-action-marker))
                (separator (conn-action-separator))
                (str nil)
                (init nil))
    (:description "Copy From")
    (:reference
     "Replace current region with text in region selected by dispatch.")
    (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (with-selected-window window
        (pcase (conn-bounds-of-dispatch thing arg pt)
          ((conn-bounds `(,beg . ,end) transform)
           (conn-dispatch-action-pulse beg end)
           (setf str (filter-buffer-substring beg end)))
          (_ (user-error "Cannot find thing at point"))))
      (cl-flet* ((insert-sep ()
                   (cond ((or (null cg) init)
                          (insert (conn-kill-separator-for-strings str separator)))
                         ((and cg (not init))
                          (setf init t)
                          (conn-dispatch-undo-case
                            (:undo (setf init nil))))))
                 (do ()
                   (conn-dispatch-change-group)
                   (goto-char opoint)
                   (when (and separator
                              (not (conn-dispatch-other-end-p)))
                     (insert-sep))
                   (insert-for-yank str)
                   (when (and separator
                              (conn-dispatch-other-end-p))
                     (insert-sep))))
        (with-current-buffer (marker-buffer opoint)
          (if (= (point) opoint)
              (do)
            (save-excursion (do))))))))

(defun conn-dispatch-take ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ((separator (conn-action-separator))
                (opoint (conn-action-marker))
                (cg (conn-action-replace))
                (init nil))
    (:description "Take From")
    (:window-predicate
     (lambda (win)
       (not
        (buffer-local-value 'buffer-read-only
                            (window-buffer win)))))
    (:reference
     "Kill the thing selected by dispatch and yank it at point.")
    (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target))
                (str nil))
      (conn-dispatch-change-group (marker-buffer opoint)
                                  (window-buffer window))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing arg)
            ((and bounds (conn-bounds `(,beg . ,end) transform))
             (setf str (filter-buffer-substring beg end 'delete))
             (when conn-kill-reformat-function
               (funcall conn-kill-reformat-function bounds)))
            (_ (user-error "Cannot find thing at point")))))
      (cl-flet ((insert-sep ()
                  (cond ((or (null cg) init)
                         (insert (conn-kill-separator-for-strings str separator)))
                        ((and cg (not init))
                         (setf init t)
                         (conn-dispatch-undo-case
                           (:undo (setf init nil)))))))
        (with-current-buffer (marker-buffer opoint)
          (when (and separator
                     (not (conn-dispatch-other-end-p)))
            (insert-sep))
          (insert-for-yank str)
          (when (and separator
                     (conn-dispatch-other-end-p))
            (insert-sep)))))))

(defun conn-dispatch-jump ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "Jump")
    (:no-history t)
    (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                 (conn-select-target)))
      (conn-dispatch-select-window window)
      (conn-dispatch-change-group)
      (if (or (conn-dispatch-other-end-p)
              transform)
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,_end) transform)
             (unless (= beg (point))
               (conn-dispatch-goto-char beg))))
        (unless (= pt (point))
          (conn-dispatch-goto-char pt))))))

(defun conn-dispatch-repeat-command ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (when command-history
    (conn-action
        ((command
          (conn-read-argument
           "command"
           'read-previous-command
           (define-keymap "+" 'read-previous-command)
           (lambda (_cmd) (conn-read-from-command-history))
           :formatter (lambda (key-string name _val)
                        (concat
                         key-string
                         " "
                         (propertize
                          name 'face 'conn-argument-active-face)))
           :value (car conn-command-history)
           :always-read t)
          :read t))
      (:window-predicate
       (lambda (win)
         (not (buffer-local-value 'buffer-read-only
                                  (window-buffer win)))))
      (:description "Repeat")
      (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                   (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-mark-and-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-dispatch-bounds `(,beg . ,_end) transform)
               (goto-char beg)
               (let ((conn-repeating-command t))
                 (when (commandp (car command))
                   (setf this-command (car command)))
                 (apply #'funcall-interactively command)))
              (_ (user-error "Cannot find thing at point")))))))))

(defun conn-dispatch-highlight-symbol ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "Highlight Symbol")
    (declare-function conn-toggle-highlight-at-point "conn-commands")
    (declare-function conn--unhighlight-at-point "conn-commands")
    (pcase-let ((`(,pt ,window ,_thing ,_arg ,_transform)
                 (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-mark-and-excursion
          (goto-char pt)
          (conn-toggle-highlight-at-point))
        (conn-dispatch-undo-case
          ((or :cancel :undo)
           (goto-char pt)
           (conn--unhighlight-at-point)))))))

(defun conn-dispatch-dwim ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "DWIM")
    (declare-function conn--dwim-at-point-filter "conn-commands")
    (pcase-let ((`(,pt ,window ,_thing ,_arg ,_transform)
                 (conn-select-target)))
      (with-selected-window window
        (if-let* ((cmd (save-excursion
                         (goto-char pt)
                         (conn--dwim-at-point-filter nil))))
            (save-mark-and-excursion
              (conn-dispatch-change-group)
              (goto-char pt)
              (call-interactively cmd))
          (user-error "No DWIM action at point"))))))

(defun conn-dispatch-dwim-alt ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "DWIM alt")
    (declare-function conn--alt-dwim-at-point-filter "conn-commands")
    (pcase-let ((`(,pt ,window ,_thing ,_arg ,_transform)
                 (conn-select-target)))
      (with-selected-window window
        (if-let* ((cmd (save-excursion
                         (goto-char pt)
                         (conn--alt-dwim-at-point-filter nil))))
            (save-mark-and-excursion
              (conn-dispatch-change-group)
              (goto-char pt)
              (call-interactively cmd))
          (user-error "No DWIM action at point"))))))

(defun conn-dispatch-transpose ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-action ()
    (:description "Transpose")
    (:window-predicate
     (lambda (win)
       (not (buffer-local-value 'buffer-read-only
                                (window-buffer win)))))
    (:reference
     "Transpose two things selected by dispatch.")
    (pcase-let* ((conn-dispatch-always-prompt t)
                 (`(,pt1 ,win1 ,thing1 ,arg1 ,transform1)
                  (conn-select-target))
                 (ov (make-overlay pt1 pt1 (window-buffer win1))))
      (unwind-protect
          (let ((conn-target-predicate conn-target-predicate))
            (conn-dispatch-change-group (window-buffer win1))
            (add-function :before-while conn-target-predicate
                          (lambda (pt _length window)
                            (not (and (eq window win1)
                                      (eql pt pt1)))))
            (overlay-put ov 'window win1)
            (with-selected-window win1
              (save-excursion
                (goto-char pt1)
                (if (or (eobp) (eolp))
                    (overlay-put ov 'after-string
                                 (propertize " " 'face 'conn-target-preview-face))
                  (move-overlay ov pt1 (1+ pt1))
                  (overlay-put ov 'face 'isearch))))
            (pcase-let ((`(,pt2 ,win2 ,thing2 ,arg2 ,transform2)
                         (conn-select-target)))
              (unless (eq (window-buffer win1) (window-buffer win2))
                (conn-dispatch-change-group (window-buffer win2)))
              (delete-overlay ov)
              (conn--dispatch-transpose-subr
               (conn-transform-bounds
                (conn-bounds-of-dispatch thing1 arg1 pt1)
                transform1)
               (conn-transform-bounds
                (conn-bounds-of-dispatch thing2 arg2 pt2)
                transform2))))
        (delete-overlay ov)))))

(defun conn--dispatch-transpose-subr (bounds1 bounds2)
  (pcase-let* (((conn-bounds `(,beg1 . ,end1)) bounds1)
               (buffer1 (conn-bounds-buffer bounds1))
               ((conn-bounds `(,beg2 . ,end2)) bounds2)
               (buffer2 (conn-bounds-buffer bounds2)))
    (cl-psetf beg1 (min beg1 end1)
              end1 (max beg1 end1)
              beg2 (min beg2 end2)
              end2 (max beg2 end2))
    (if (eq buffer1 buffer2)
        (with-current-buffer buffer1
          (if (and (or (<= beg1 end1 beg2 end2)
                       (<= beg2 end2 beg1 end1))
                   (/= beg1 end1)
                   (/= beg2 end2)
                   (<= (point-min) (min beg2 end2 beg1 end1))
                   (> (point-max) (max beg2 end2 beg1 end1)))
              (transpose-regions beg1 end1 beg2 end2)
            (user-error "Invalid regions")))
      (conn-protected-let* ((cg (nconc (prepare-change-group buffer1)
                                       (prepare-change-group buffer2))
                                (cancel-change-group cg))
                            (str1)
                            (str2))
        (activate-change-group cg)
        (with-current-buffer buffer1
          (setf str1 (filter-buffer-substring beg1 end1))
          (delete-region beg1 end1))
        (with-current-buffer buffer2
          (setf str2 (filter-buffer-substring beg2 end2))
          (goto-char (min beg2 end2))
          (delete-region beg2 end2)
          (insert str1))
        (with-current-buffer buffer1
          (save-excursion
            (goto-char (min beg1 end1))
            (insert str2)))
        (accept-change-group cg)))))

;;;;; Dispatch Ring

(cl-defstruct (conn-previous-dispatch
               ( :constructor conn-make-dispatch
                 (action
                  &aux
                  (repeat conn-dispatch-repeating)
                  (restrict-windows (advice-function-member-p
                                     'restrict-windows
                                     conn-target-window-predicate))
                  (other-end conn-dispatch-other-end)
                  (target-finder
                   (conn-target-finder-shelve conn-dispatch-target-finder))))
               (:copier conn--copy-previous-dispatch))
  "State necessary to restart a dispatch."
  (action nil :type conn-action)
  (other-end nil :type symbol)
  (repeat nil :type boolean)
  (restrict-windows nil :type boolean)
  (target-finder nil))

(defvar conn-dispatch-ring-max 12
  "Maximum number of previous dispatches to keep in `conn-dispatch-ring'.")

(defvar conn-dispatch-ring
  (conn-make-ring conn-dispatch-ring-max
                  :cleanup 'conn-dispatch-ring--cleanup)
  "Ring of previous dispatches.")

(define-conn-dispatch-handler-command ((arg conn-dispatch-command-handler)
                                       (cmd (eql conn-repeat-last-dispatch)))
  "Cycle the dispatch ring to the next most recent dispatch."
  ( :update (_break)
    (cond* ((bind* (prev (conn-ring-extract-head conn-dispatch-ring))))
           ((null prev) (conn-read-args-error "Dispatch ring empty"))
           ((conn-action-stale-p (conn-previous-dispatch-action prev))
            (conn-dispatch-ring-remove-stale)
            (conn-read-args-error "Last dispatch action stale"))
           (t
            (conn-read-args-return
              (conn-dispatch-setup-previous
               prev (conn-read-args-consume-prefix-arg)))))))

(defun conn-previous-dispatch-copy (dispatch)
  (declare (important-return-value t))
  (let ((copy (conn--copy-previous-dispatch dispatch)))
    (cl-callf clone (conn-previous-dispatch-target-finder copy))
    (cl-callf conn-action-copy (conn-previous-dispatch-action copy))
    copy))

(defun conn-dispatch-ring--cleanup (dispatch)
  (conn-action-cleanup (conn-previous-dispatch-action dispatch)))

(defun conn-describe-dispatch (dispatch)
  (declare (side-effect-free t))
  (let ((thing (oref (conn-previous-dispatch-target-finder dispatch)
                     thing))
        (arg (oref (conn-previous-dispatch-target-finder dispatch)
                   arg))
        (transform (oref (conn-previous-dispatch-target-finder dispatch)
                         transform)))
    (format "%s @ %s <%s%s>"
            (or (conn-action-description
                 (conn-previous-dispatch-action dispatch))
                "unnamed action")
            (conn-thing-pretty-print thing)
            arg
            (if transform
                (concat
                 "   "
                 (mapconcat (lambda (tf)
                              (or (get tf :conn-transform-description) ""))
                            transform " > "))
              ""))))

(defun conn-dispatch-push-history (dispatch)
  (conn-dispatch-ring-remove-stale)
  (conn-ring-insert-front conn-dispatch-ring dispatch))

(defun conn-dispatch-ring-remove-stale ()
  (cl-loop for dispatch in (conn-ring-list conn-dispatch-ring)
           when (conn-action-stale-p
                 (conn-previous-dispatch-action dispatch))
           do (conn-ring-delq dispatch conn-dispatch-ring)))

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

(defun conn-dispatch-setup-previous (prev-dispatch &optional invert-repeat)
  (when (conn-action-stale-p
         (conn-previous-dispatch-action prev-dispatch))
    (error "Action stale"))
  (conn-with-dispatch
    (pcase-let (((cl-struct conn-previous-dispatch
                            action
                            (other-end conn-dispatch-other-end)
                            repeat
                            target-finder
                            restrict-windows)
                 prev-dispatch))
      (conn-with-dispatch-handlers
        (:handler
         ( :display ()
           (and-let* ((desc (conn-action-description conn-dispatch-action)))
             (propertize desc
                         'face 'conn-argument-active-face
                         'conn-read-args-display-depth -50))))
        ( :with (conn-dispatch-prefix-arg))
        ( :with (conn-read-char-input-method))
        (conn-target-finder-setup target-finder)
        (conn-action-setup action (xor repeat invert-repeat))
        (when restrict-windows
          (add-function :after-while conn-target-window-predicate
                        'conn--dispatch-restrict-windows
                        '((name . restrict-windows))))
        (conn--dispatch-loop)
        (conn-action-accept conn-dispatch-action)
        (setf (conn-previous-dispatch-repeat prev-dispatch)
              conn-dispatch-repeating)
        (setf (conn-previous-dispatch-other-end prev-dispatch)
              conn-dispatch-other-end)
        (setf (conn-previous-dispatch-target-finder prev-dispatch)
              conn-dispatch-target-finder)
        (setf (conn-previous-dispatch-restrict-windows prev-dispatch)
              (advice-function-member-p
               'restrict-windows
               conn-target-window-predicate))
        (conn-dispatch-push-history prev-dispatch)))))

;;;;; Dispatch Commands

(cl-defun conn-dispatch-setup (action
                               thing
                               arg
                               transform
                               &key
                               repeat
                               restrict-windows
                               other-end
                               must-prompt)
  (when (null action)
    (setf action (conn-get-default-action thing)))
  (conn-with-dispatch
    (when must-prompt
      (setf conn-dispatch-always-prompt t))
    (setf conn-dispatch-other-end (pcase other-end
                                    (:no-other-end (lambda (&rest _) 0))
                                    ('t #'-)
                                    ('nil #'+)))
    (conn-with-dispatch-handlers
      (:handler
       ( :display ()
         (and-let* ((desc (conn-action-description conn-dispatch-action)))
           (propertize desc
                       'face 'conn-argument-active-face
                       'conn-read-args-display-depth -50))))
      ( :with (conn-dispatch-prefix-arg))
      ( :with (conn-read-char-input-method))
      (conn-action-setup (or action (conn-get-default-action thing))
                         repeat)
      (conn-target-finder-setup
       (conn-get-target-finder thing arg transform))
      (when restrict-windows
        (add-function :after-while conn-target-window-predicate
                      'conn--dispatch-restrict-windows
                      '((name . restrict-windows))))
      (conn--dispatch-loop)
      (conn-action-accept conn-dispatch-action)
      (unless (conn-action-no-history action)
        (let ((prev (conn-make-dispatch action)))
          (conn-dispatch-push-history prev)
          (conn-push-command-history 'conn-dispatch-setup-previous
                                     (conn-previous-dispatch-copy prev)))))))

(defvar-keymap conn-restrict-windows-argument-map
  "C-w" 'restrict-windows)

(defun conn-dispatch ()
  "Perform a dispatch.

Interactively read the arguments for `conn-dispatch-setup' with
`conn-read-args'.

INITIAL-ARG is the initial value of the prefix argument during
`conn-read-args'.  Interactively the current prefix argument."
  (interactive)
  (conn-read-args (conn-dispatch-state
                   :command-handler (conn-dispatch-command-handler)
                   :prompt "Dispatch"
                   :display-handler (conn-read-args-display-columns 5 3)
                   :pre (lambda (_)
                          (when (and (bound-and-true-p conn-posframe-mode)
                                     (fboundp 'posframe-hide))
                            (posframe-hide " *conn-list-posframe*"))))
      ((`(,action ,repeat) (conn-dispatch-action-argument))
       (`(,thing ,arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument))
       (other-end (conn-boolean-argument "other-end"
                                         'other-end
                                         conn-other-end-argument-map))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map)))
    (conn-dispatch-setup
     action thing arg transform
     :repeat repeat
     :other-end other-end
     :restrict-windows restrict-windows)))

(defun conn-dispatch-thing-at-point ()
  "Dispatch on matches for a thing at point.

Interactively reads the argument `conn-dispatch-setup'.  After reading
the arguments the THING, ARG, and TRANSFORM argument are first used to
find the region they define at point and then dispatch proceeds on
regions matching the contents of the region at point.

INITIAL-ARG is the initial value of the prefix argument during
`conn-read-args'.  Interactively the current prefix argument."
  (interactive)
  (let ((conn-dispatch-always-prompt t))
    (conn-read-args (conn-dispatch-thingatpt-state
                     :command-handler (conn-dispatch-command-handler)
                     :prompt "Dispatch"
                     :pre (lambda (_)
                            (when (and (bound-and-true-p conn-posframe-mode)
                                       (fboundp 'posframe-hide))
                              (posframe-hide " *conn-list-posframe*"))))
        ((`(,action ,repeat) (conn-dispatch-action-argument))
         (`(,thing ,arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument))
         (other-end
          (conn-boolean-argument "other-end"
                                 'other-end
                                 conn-other-end-argument-map
                                 :command-reference conn-dispatch-other-end-reference))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map
                                 :command-reference conn-this-win-argument-reference)))
      (conn-dispatch-setup
       action
       (conn-anonymous-thing
         (list thing)
         :target-finder
         ( :method (self arg)
           (pcase (ignore-errors (conn-bounds-of self arg))
             ((conn-bounds `(,beg . ,end))
              (conn-dispatch-focus-thing-at-point
               :string (buffer-substring-no-properties beg end))))))
       arg transform
       :repeat repeat
       :other-end other-end
       :restrict-windows restrict-windows))))

(defun conn-repeat-last-dispatch (invert-repeat)
  "Repeat the last dispatch command.

Prefix arg INVERT-REPEAT inverts the value of repeat in the last dispatch."
  (interactive "P")
  (when (conn-action-stale-p
         (conn-previous-dispatch-action (conn-ring-head conn-dispatch-ring)))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (let ((prev (conn-ring-extract-head conn-dispatch-ring)))
    (conn-dispatch-setup-previous prev invert-repeat)))

(defun conn-last-dispatch-at-mouse (event &optional repeat)
  "Perform the previous dispatch at a mouse click.

If REPEAT is non-nil then invert the value of repeat for the last
dispatch.  Interactively repeat is the prefix argument.

EVENT must be a mouse event.

This command must be bound to a mouse key."
  (interactive "e\nP")
  (unless (mouse-event-p event)
    (error "conn-last-dispatch-at-mouse must be bound to a mouse event"))
  (unless (conn-ring-list conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (when (conn-action-stale-p (conn-previous-dispatch-action
                              (conn-ring-head conn-dispatch-ring)))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (conn-add-unread-events `(dispatch-mouse-repeat ,@(cdr event)))
  (conn-repeat-last-dispatch
   (and (conn-previous-dispatch-repeat (conn-ring-head conn-dispatch-ring))
        repeat)))

(defun conn-bind-last-dispatch-to-key ()
  "Bind previous dispatch to a key.

Calling a bound dispatch with a prefix arg inverts the value of repeat
for the dispatch."
  (interactive)
  (let* ((key-seq (read-key-sequence
                   (format "Bind last dispatch to key in %s: "
                           conn-current-state)))
         (binding (key-binding key-seq)))
    (when (and (not (eql (aref key-seq 0)
                         (car (last (current-input-mode)))))
               (or (not binding)
                   (eq binding 'undefined)
                   (stringp binding)
                   (vectorp binding)
                   (yes-or-no-p (format "%s runs command %S.  Bind anyway? "
                                        (format-kbd-macro key-seq)
                                        binding))))
      (define-key (conn-get-minor-mode-map conn-current-state :bind-last)
                  key-seq (let ((disp (conn-previous-dispatch-copy
                                       (conn-ring-head conn-dispatch-ring))))
                            (lambda ()
                              (interactive)
                              (conn-dispatch-setup-previous disp))))
      (message "Dispatch bound to %s" (format-kbd-macro key-seq)))))

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-dispatch-setup
   (conn-dispatch-push-button)
   (conn-anonymous-thing
     '(button)
     :pretty-print ( :method (_self) "all-buttons")
     :target-finder ( :method (_self &rest _)
                      (conn-dispatch-button-targets)))
   nil nil
   :must-prompt t))

(define-conn-target-finder conn-isearch-targets () ()
  ( :update-method (state)
    (conn-dispatch-call-update-handlers state))
  ( :default-update-handler (state)
    (with-restriction (window-start) (window-end)
      (let* ((matches (conn--isearch-matches))
             (thing (conn-anonymous-thing
                      '(region)
                      :bounds-op ( :method (_self arg)
                                   (and-let* ((bd (assq (point) matches)))
                                     (conn-make-bounds 'region arg bd))))))
        (pcase-dolist (`(,beg . ,end) matches)
          (conn-make-target-overlay
           beg (- end beg)
           :thing thing))))))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (when (equal isearch-string "")
    (if (null (if isearch-regexp regexp-search-ring search-ring))
        (error "No previous search string")
      (setf isearch-string
	    (car (if isearch-regexp regexp-search-ring search-ring))
	    isearch-case-fold-search isearch-last-case-fold-search)))
  (unwind-protect
      (let ((conn-dispatch-always-prompt t)
            (regexp-search-ring
             (if isearch-regexp
                 (cons isearch-string regexp-search-ring)
               regexp-search-ring))
            (search-ring
             (if isearch-regexp
                 search-ring
               (cons isearch-string search-ring)))
            (opoint))
        (with-isearch-suspended
         (conn-dispatch-setup
          (conn-dispatch-jump)
          (conn-anonymous-thing
            '(region)
            :target-finder ( :method (_self &rest _)
                             (conn-isearch-targets
                              :prompt isearch-string
                              :window-predicate (let ((owin (selected-window)))
                                                  (lambda (win) (eq win owin))))))
          nil nil
          :repeat nil
          :restrict-windows t
          :other-end nil)
         (setf opoint (point)))
        (goto-char opoint))
    (save-mark-and-excursion
      (isearch-exit))))

(defun conn-goto-char-2 ()
  "Jump to point defined by two characters and maybe a label."
  (interactive)
  (conn-dispatch-setup
   (conn-dispatch-jump)
   nil nil nil
   :other-end :no-other-end))

;;;;; Dispatch Bounds

(defun conn--dispatch-bounds (bounds &optional subregions-p)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix (conn-bounds-arg bounds)
                   :prompt "Bounds of Dispatch")
      ((`(,thing ,arg) (conn-dispatch-thing-argument t))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               :value subregions-p))
       (transform (conn-dispatch-transform-argument)))
    (let (ovs subregions)
      (unwind-protect
          (progn
            (conn-dispatch-setup
             (conn-action ()
               (:no-history t)
               (:description "Bounds")
               (:window-predicate
                (let ((win (selected-window)))
                  (lambda (window) (eq win window))))
               (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                            (conn-select-target)))
                 (with-selected-window window
                   (pcase (conn-bounds-of-dispatch thing arg pt)
                     ((and (conn-dispatch-bounds `(,beg . ,end) transform)
                           bound)
                      (when repeat
                        (push (make-overlay beg end) ovs)
                        (conn-dispatch-undo-case
                          ((or :cancel :undo)
                           (delete-overlay (pop ovs))))
                        (overlay-put (car ovs) 'face 'region))
                      (push bound subregions)
                      (conn-dispatch-undo-case
                        ((or :cancel :undo)
                         (pop subregions))))
                     (_
                      (user-error "No %s found at point"
                                  (conn-thing-pretty-print thing)))))))
             thing arg transform
             :repeat repeat
             :other-end :no-other-end)
            (cl-loop for bound in subregions
                     for (b . e) = (conn-bounds bound)
                     minimize b into beg
                     maximize e into end
                     finally do
                     (setf (conn-bounds bounds) (cons beg end)
                           (conn-bounds-get bounds :subregions) subregions))
            (if subregions-p subregions (conn-bounds bounds)))
        (mapc #'delete-overlay ovs)))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing dispatch))
                              arg)
  (conn-make-bounds
   cmd arg
   (conn-bounds-delay bounds
     (conn--dispatch-bounds bounds))
   :subregions (conn-bounds-delay bounds
                 (conn--dispatch-bounds bounds t))))

(cl-defmethod conn-dispatch-bounds-between (bounds)
  (pcase bounds
    ((conn-dispatch-bounds `(,beg . ,end))
     (let (obeg oend ov)
       (unwind-protect
           (progn
             (setf ov (make-overlay beg end))
             (overlay-put ov 'face 'region)
             (conn-dispatch-setup
              (conn-action ()
                (:no-history t)
                (:description "Bounds")
                (:window-predicate
                 (let ((win (selected-window)))
                   (lambda (window) (eq win window))))
                (:reference
                 "Bounds between the previous region and this region.")
                (pcase-let ((`(,pt ,window ,thing ,arg ,transform)
                             (conn-select-target)))
                  (with-selected-window window
                    (pcase (conn-bounds-of-dispatch thing arg pt)
                      ((conn-dispatch-bounds `(,beg . ,end) transform)
                       (setf obeg beg
                             oend end))
                      (_ (user-error "No %s found at point" thing))))))
              (conn-bounds-thing bounds)
              (conn-bounds-arg bounds)
              (when (conn-transformed-bounds-p bounds)
                (conn-transformed-bounds-transforms bounds))))
         (delete-overlay ov))
       (conn-make-transformed-bounds
        'conn-dispatch-bounds-between
        bounds
        (if (< (min obeg oend) (min beg end))
            (cons obeg end)
          (cons beg oend)))))))

;;;;; Dispatch Registers

(cl-defstruct (conn-dispatch-register
               (:constructor conn--make-dispatch-register (dispatch)))
  (dispatch nil :type conn-previous-dispatch))

(cl-defmethod register-val-jump-to ((val conn-dispatch-register)
                                    arg)
  (let ((prev (conn-dispatch-register-dispatch val)))
    (conn-dispatch-setup-previous prev arg)))

(cl-defmethod register-val-describe ((val conn-dispatch-register)
                                     _verbose)
  (princ "Dispatch: ")
  (princ (conn-describe-dispatch (conn-dispatch-register-dispatch val))))

(defun conn-last-dispatch-to-register (register)
  "Store last dispatch command in REGISTER."
  (interactive (list (register-read-with-preview "Dispatch to register: ")))
  (if-let* ((head (conn-ring-head conn-dispatch-ring)))
      (set-register register (conn--make-dispatch-register
                              (conn-previous-dispatch-copy head)))
    (user-error "Dispatch ring empty")))

;;;; Thing Target Finders

(conn-register-thing 'dispatch)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing defun))
                                      &rest _)
  (conn-dispatch-all-defuns))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing visual-line))
                                      &rest _)
  (conn-dispatch-visual-line-targets))

(conn-register-thing-commands '(dispatch) nil 'conn-dispatch)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing symbol))
                                      &rest _)
  (conn-dispatch-things-read-prefix 'symbol 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing word))
                                      &rest _)
  (conn-dispatch-things-read-prefix 'word 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sexp))
                                      &rest _)
  (conn-dispatch-things-read-prefix 'sexp 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sentence))
                                      &rest _)
  (conn-all-things-targets :all-things 'sentence))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing paragraph))
                                      &rest _)
  (conn-all-things-targets :all-things 'paragraph))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing char))
                                      &rest _)
  (conn-dispatch-read-n-chars :string-length 2
                              :thing 'forward-char))

(cl-defmethod conn-get-target-finder ((_cmd (eql forward-char))
                                      &rest _)
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line))
                                      &rest _)
  (conn-dispatch-line-targets))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column))
                                      &rest _)
  (conn-dispatch-column-targets))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing outer-line))
                                      &rest _)
  (conn-dispatch-line-targets))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing inner-line))
                                      &rest _)
  (conn-dispatch-inner-line-targets))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line))
                                      &rest _)
  (conn-dispatch-end-of-inner-line-targets))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line-dwim))
                                      &rest _)
  (conn-dispatch-end-of-inner-line-targets))

(provide 'conn-dispatch)
