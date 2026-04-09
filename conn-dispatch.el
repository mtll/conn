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

;;; Commentary

;;; Code

(require 'eieio)
(require 'conn-states)
(require 'conn-things)
(require 'conn-jump-ring)

(defvar outline-heading-end-regexp)
(defvar treesit-defun-type-regexp)
(defvar conn-wincontrol-mode)
(defvar conn-wincontrol-one-command-mode)

(declare-function face-remap-remove-relative "face-remap")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")
(declare-function conn-scroll-up "conn-commands")
(declare-function conn-scroll-down "conn-commands")
(declare-function conn-register-load "conn-commands")
(declare-function conn-end-of-inner-line "conn-commands")
(declare-function conn-beginning-of-inner-line "conn-commands")
(declare-function conn-kill-thing "conn-commands")
(declare-function conn-toggle-highlight-at-point "conn-commands")
(declare-function conn--unhighlight-at-point "conn-commands")

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

(defvar conn-window-label-function 'conn-header-line-labels
  "Function to label windows for `conn-prompt-for-window'.

The function should accept a single argument, the list of windows to be
labeled and it should return a list of structs for `conn-label-select',
which see.")

(defvar conn-dispatch-label-input-method nil)

(defvar conn-target-window-predicate)

(defvar conn-dispatch-all-frames 'visible)

(cl-defstruct (conn-dispatch-label)
  "State for a dispatch label."
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
               ( :constructor conn-window-label
                 (string
                  window
                  &aux
                  (state (list (window-point window)
                               (window-vscroll window)
                               (window-hscroll window))))))
  "State for a window label."
  (string nil :type string)
  (window nil :type window)
  (state nil :type list))

(defun conn--get-target-windows ()
  (declare (important-return-value t))
  (if conn-target-window-predicate
      (conn--get-windows nil nil conn-dispatch-all-frames
                         nil conn-target-window-predicate)
    (list (selected-window))))

(defun conn-simple-labels (count)
  "Return a list of label strings of length COUNT.

If FACE is non-nil set label string face to FACE.  Otherwise label
strings have `conn-dispatch-label-face'."
  (declare (side-effect-free t)
           (important-return-value t))
  (let* ((blen (floor (1+ (log count (length conn-simple-label-characters)))))
         (buckets (make-vector blen nil))
         (i 0))
    (setf (aref buckets i) (thread-last
                             (take count conn-simple-label-characters)
                             (copy-sequence)
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
             (cl-incf i))
         (catch 'done
           (let ((n (length (aref buckets i))))
             (dolist (prefix prefixes)
               (dolist (c conn-simple-label-characters)
                 (push (concat prefix c) (aref buckets (1+ i)))
                 (when (= (cl-incf n) count)
                   (throw 'done nil))))))
         (cl-callf nreverse (aref buckets i))
         (cl-return (cl-loop for bucket across buckets
                             nconc bucket)))))))

;;;;; Label Reading

(defmacro conn-define-dispatch-handler-command (argument-and-command
                                                docstring
                                                &rest
                                                body)
  (declare (indent 1))
  (pcase argument-and-command
    (`((,handler ,_spec) ,_cmd)
     (pcase (alist-get :update body)
       (`(,_args . ,update-body)
        (setf (cdr (alist-get :update body))
              (macroexp-unprogn
               (macroexpand-all
                (macroexp-progn update-body)
                `((:return
                   . ,(lambda (&optional value)
                        `(throw (cdr (alist-get ,handler
                                                conn--dispatch-read-char-handlers))
                                ,value))))))))))
    (_ (error "Invalid argument form")))
  `(conn-define-argument-command ,argument-and-command
     ,docstring
     ,@body))

;;;;;; Event Handlers

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

(conn-define-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql dispatch-character-event)))
  "Narrow labels by character."
  (:predicate)
  ( :update (break)
    (setq conn--read-args-error-message nil
          conn--dispatch-redisplay-prompt-flag nil)
    (conn-add-unread-events (this-single-command-raw-keys))
    (pcase (conn--dispatch-read-char-1
            (conn-dispatch-char-argument-input-method arg))
      ((pred (eql (car (last (current-input-mode)))))
       (signal 'quit nil))
      (ev (setf (conn-argument-value arg) ev
                (conn-argument-set-flag arg) t)))
    (funcall break)))

(conn-define-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql quoted-insert)))
  "Narrow labels by character read with quoted-insert."
  ( :update (break)
    (let ((char (read-quoted-char
                 (propertize "Quoted Char: "
                             'face 'minibuffer-prompt))))
      (unless (eq char (car (last (current-input-mode))))
        (setf (conn-argument-value arg) char
              (conn-argument-set-flag arg) t)))
    (funcall break)))

(conn-define-argument-command ((arg conn-dispatch-char-argument)
                               (cmd (eql restart)))
  "Narrow labels by character."
  ( :update (break)
    (setf (conn-argument-value arg) 8
          (conn-argument-set-flag arg) t)
    (funcall break)))

(cl-defstruct (conn-dispatch-read-char-handler
               (:include conn-argument)
               ( :constructor conn-dispatch-read-char-handler
                 (update
                  predicate
                  &key
                  keymap
                  annotation
                  documentation
                  display
                  reference)))
  (update #'ignore :type function :read-only t)
  (predicate #'ignore :type function :read-only t)
  (documentation #'ignore :type function :read-only t)
  (display #'ignore :type function :read-only t))

(cl-defmethod conn-argument-update ((handler conn-dispatch-read-char-handler)
                                    cmd
                                    break)
  (when (funcall (conn-dispatch-read-char-handler-predicate handler) cmd)
    (funcall (conn-dispatch-read-char-handler-update handler)
             handler cmd break)))

(cl-defmethod conn-argument-display ((arg conn-dispatch-read-char-handler))
  (funcall (conn-dispatch-read-char-handler-display arg)))

(cl-defmethod conn-argument-compose-keymap ((arg conn-dispatch-read-char-handler))
  (conn-dispatch-read-char-handler-keymap arg))

(cl-defmethod conn-argument-predicate ((arg conn-dispatch-read-char-handler)
                                       cmd)
  (funcall (conn-dispatch-read-char-handler-predicate arg) cmd))

(cl-defmethod conn-argument-completion-annotation ((arg conn-dispatch-read-char-handler)
                                                   value)
  (when-let* ((ann (conn-dispatch-read-char-handler-annotation arg))
              (_ (funcall (conn-dispatch-read-char-handler-predicate arg) value)))
    (funcall ann value)))

(cl-defmethod conn-argument-get-reference ((arg conn-dispatch-read-char-handler))
  (conn-dispatch-read-char-handler-reference arg))

(cl-defmethod conn-argument-command-documentation ((arg conn-dispatch-read-char-handler)
                                                   cmd)
  (when (funcall (conn-dispatch-read-char-handler-predicate arg) cmd)
    (funcall (conn-dispatch-read-char-handler-documentation arg) cmd)))

(defvar conn--dispatch-read-char-handlers nil)

(eval-and-compile
  (defun conn--expand-dispatch-handler (tag body)
    (cl-with-gensyms (self)
      `(push (cons (conn-dispatch-read-char-handler
                    ,(pcase (alist-get :update body)
                       (`(,args . ,update-body)
                        (macroexpand-all
                         `(lambda ,(cons self args)
                            (ignore ,self)
                            ,@update-body)
                         `((:return
                            . ,(lambda (&optional value)
                                 `(throw (cdr (alist-get ,self conn--dispatch-read-char-handlers))
                                         ,value)))))))
                    ,(if-let* ((v (alist-get :predicate body)))
                         `(lambda ,@v)
                       '#'ignore)
                    :reference ,(car (alist-get :reference body))
                    :display ,(if-let* ((v (alist-get :display body)))
                                  `(lambda ,@v)
                                '#'ignore)
                    :annotation ,(if-let* ((v (alist-get :annotation body)))
                                     `(lambda ,@v)
                                   '#'ignore)
                    :documentation ,(if-let* ((v (alist-get :documentation body)))
                                        `(lambda ,@v)
                                      '#'ignore)
                    :keymap ,(car (alist-get :keymap body)))
                   (cons ,(or (car (alist-get :depth body)) 0) ',tag))
             conn--dispatch-read-char-handlers))))

(defmacro conn-with-dispatch-handlers (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (tag)
    (macroexpand-all
     `(let ((conn--dispatch-read-char-handlers
             conn--dispatch-read-char-handlers))
        (catch ',tag ,@body))
     `((:handler . ,(lambda (&rest body)
                      (conn--expand-dispatch-handler tag body)))
       (:with . ,(cl-function
                  (lambda (exp &key depth)
                    `(push (cons ,exp (cons ,(or depth 0) ',tag))
                           conn--dispatch-read-char-handlers))))))))

;;;;;; Labels


(defvar conn-dispatch-hide-labels nil)

(defvar conn-dispatch-input-buffer nil
  "Buffer that was current when dispatch began.

All events read by `conn-dispatch-read-char' are read with this buffer
current.")

(defmacro conn-with-dispatch-input-buffer (&rest body)
  (declare (indent 0))
  `(let ((conn-dispatch-input-buffer
          (generate-new-buffer " *conn-dispatch-input*" t)))
     (unwind-protect
         ,(macroexp-progn body)
       (when (buffer-live-p conn-dispatch-input-buffer)
         (kill-buffer conn-dispatch-input-buffer)))))

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

(cl-defgeneric conn-label-completed-p (label))

(cl-defgeneric conn-label-display (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-clear (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-setup (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

(defun conn-redisplay-labels (labels)
  (mapc #'conn-label-clear labels)
  (mapc #'conn-label-setup labels)
  (mapc #'conn-label-display labels))

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload."
  (declare (important-return-value t)))

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

(cl-defmethod conn-label-narrow ((label conn-window-label)
                                 prefix-char)
  (pcase-let* (((cl-struct conn-window-label window) label)
               (string (window-parameter window 'conn-label-string)))
    (unless (or (length= string 0)
                (not (eql prefix-char (string-to-char string))))
      (set-window-parameter window 'conn-label-string (substring string 1))
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
  (let* ((prompt (propertize (or prompt "Chars")
                             'face 'minibuffer-prompt))
         (prompt-flag always-prompt)
         (current candidates)
         (partial nil))
    (while-no-input
      (conn-redisplay-labels candidates))
    (cl-loop
     (pcase current
       ('nil
        (setq current candidates
              partial nil
              prompt-flag always-prompt)
        (conn-read-args-message "No matches")
        (mapc #'conn-label-reset current)
        (while-no-input
          (conn-redisplay-labels candidates)))
       ((and `(,it . nil)
             (guard (not (or prompt-flag
                             (and partial
                                  (not (conn-label-completed-p it)))))))
        (cl-return (conn-label-payload it))))
     (while-no-input
       (conn-redisplay-labels candidates))
     (setq prompt-flag nil)
     (while (let ((c (conn-dispatch-read-char
                      prompt
                      conn-dispatch-label-input-method)))
              (not (unless conn-dispatch-hide-labels
                     (cl-callf2 seq-keep
                         (lambda (l) (conn-label-narrow l c))
                         current)
                     (setq partial t))))))))

;;;;; Window Header-line Labels

(defface conn-window-label-face
  '((t (:inherit help-key-binding :height 2.5)))
  "Face for conn window prompt overlay."
  :group 'conn-faces)

(defun conn--centered-header-label ()
  (when (window-parameter (selected-window) 'conn-window-labeled-p)
    (let* ((window-width (window-width nil t))
           (label (window-parameter nil 'conn-label-string))
           (label-width (conn--string-pixel-width
                         label
                         (window-buffer (selected-window))))
           (padding-width (floor (- window-width label-width) 2))
           (padding (propertize " " 'display `(space :width (,padding-width)))))
      (concat padding label))))

(defvar conn--window-label-pool nil)

(defun conn--simple-window-labels ()
  (setq conn-dispatch-label-input-method conn-simple-label-input-method)
  (let* ((windows (conn--get-windows nil 'nomini t))
         (window-count (length windows)))
    (when (or (null conn--window-label-pool)
              (length< conn--window-label-pool window-count))
      (setq conn--window-label-pool
            (conn-simple-labels (max (ceiling (* 1.67 window-count))
                                     (length conn-simple-label-characters)))))
    (cl-loop with available = (copy-sequence conn--window-label-pool)
             for win in windows
             for label = (window-parameter win 'conn-label-string)
             when (cond ((null label))
                        ((member label available)
                         (cl-callf2 delete label available)
                         nil)
                        ((not
                          (when-let* ((new (seq-find (lambda (str)
                                                       (string-prefix-p label str))
                                                     available)))
                            (cl-callf2 delq new available)
                            (set-window-parameter win 'conn-label-string new)))))
             collect win into unlabeled
             finally (dolist (win unlabeled)
                       (set-window-parameter win 'conn-label-string
                                             (pop available))))))

(defun conn--setup-header-line-label (window string)
  "Label WINDOWS using `head-line-format'."
  (let ((header-line-label
         '(conn-mode (:eval (conn--centered-header-label)))))
    (set-window-parameter window 'conn-window-labeled-p t)
    (with-selected-window window
      (unless (equal header-line-label (car header-line-format))
        (setq-local header-line-format
                    `(,header-line-label (nil ,header-line-format))))
      (prog1
          (conn-window-label (propertize string 'face 'conn-window-label-face)
                             window)
        (goto-char (window-start))))))

(defun conn-header-line-labels (windows)
  (conn--simple-window-labels)
  (cl-loop for win in windows
           collect (conn--setup-header-line-label
                    win (window-parameter win 'conn-label-string))))

;; From ace-window
(defun conn--dispatch-window-predicate (window &optional dedicated)
  (not (or ;; ignore child frames
        (and (fboundp 'frame-parent)
             (frame-parent (window-frame window)))
        ;; When `ignore-window-parameters' is nil, ignore
        ;; windows whose `no-other-window’ or
        ;; `no-delete-other-windows' parameter is non-nil.
        (unless ignore-window-parameters
          (window-parameter window 'no-other-window))
        (and (null dedicated) (window-dedicated-p window)))))

(defun conn--get-windows (&optional window
                                    minibuffer
                                    all-frames
                                    dedicated
                                    predicate)
  (declare (important-return-value t))
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           when (and (conn--dispatch-window-predicate dedicated)
                     (or (null predicate) (funcall predicate win)))
           collect win))

(defmacro conn-with-window-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,val)
     `(conn-with-dispatch-input-buffer
        (let* ((conn-dispatch-label-input-method nil)
               (,var ,val))
          (unwind-protect
              ,(macroexp-progn body)
            (mapc #'conn-label-delete ,var)))))
    (_ (error "Unexpected binding form %s" binder))))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (declare (important-return-value t))
  (when windows
    (conn-with-window-labels
        (labels (funcall conn-window-label-function windows))
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

;;;; Dispatch State

(defface conn-dispatch-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a dispatch state."
  :group 'conn-faces)

(defvar conn-dispatch-target-finder nil)

(defvar conn-dispatch-ring)

(defvar conn-dispatch-always-prompt nil)

(defvar conn--dispatch-redisplay-prompt-flag nil)

(defvar conn-dispatch-repeating nil)

(defvar conn-dispatch-iteration-count nil)

(defvar conn-dispatch-quit-flag nil)

(defvar conn-dispatch-other-end nil)

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

(conn-define-state conn-dispatch-targets-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :cursor 'hollow
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(conn-define-state conn-dispatch-bounds-state (conn-dispatch-targets-state)
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(conn-define-state conn-dispatch-state (conn-dispatch-targets-state)
  "State for reading a dispatch command.")

(conn-define-state conn-dispatch-thingatpt-state (conn-dispatch-state))

(defvar-keymap conn-dispatch-transform-argument-map)

(defun conn-dispatch-transform-argument (&optional value)
  (conn-transform-argument value :keymap conn-dispatch-transform-argument-map))

(cl-defstruct (conn-dispatch-target-argument
               (:include conn-thing-argument)
               (:constructor conn-dispatch-target-argument
                             (&aux (required t)))))

(cl-defmethod conn-argument-compose-keymap ((arg conn-dispatch-target-argument))
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
     (if (or (stringp val)
             (eq 'default val))
         nil
       (let ((s (with-current-buffer conn-dispatch-input-buffer
                  (read-string "Separator (RET for default): " nil
                               'conn-separator-history nil t))))
         (if (equal s "") 'default s))))
   :value initial-value
   :documentation "Read a separator to be inserted each string."))

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
  (conn--dispatch-to-how-argument
   (conn-boolean-argument "replace"
                          'dispatch-replace
                          conn-dispatch-replace-argument-map
                          :value replace)
   (conn-separator-argument separator)))

(cl-defmethod conn-argument-display ((arg conn-dispatch-to-how-argument))
  (cl-symbol-macrolet ((replace (conn-dispatch-to-how-argument-replace arg))
                       (separator (conn-dispatch-to-how-argument-separator arg)))
    (mapcar #'conn-argument-display
            (list replace
                  (and (conn-argument-value replace)
                       separator)))))

(cl-defstruct (conn-dispatch-point-argument
               (:include conn-argument)
               (:constructor conn-dispatch-point-argument ())))

(cl-defmethod conn-argument-extract-value ((_arg conn-dispatch-point-argument))
  (copy-marker (point) t))

;;;;;; Dispatch Quick Ref

(defvar conn-dispatch-thing-reference-list
  (conn-reference-quote
    (("symbol" forward-symbol)
     ("line" forward-line)
     ("column" next-line))))

(defvar conn-dispatch-thing-transforms-ref-list
  (conn-reference-quote
    (("anchored" conn-dispatch-bounds-anchored)
     ("between" conn-dispatch-bounds-between)
     ("trim" conn-bounds-trim)
     ("over" conn-dispatch-bounds-over)
     ("reset" conn-transform-reset))))

(defvar conn-dispatch-thing-reference
  (conn-reference-page
    (:heading "Extra Thing Bindings")
    ((:keymap (list (conn-get-state-map 'conn-dispatch-targets-state)))
     (:splice (conn-quick-ref-to-cols
               conn-dispatch-thing-reference-list 3)))
    (:heading "Transforms")
    ((:splice (conn-quick-ref-to-cols
               conn-dispatch-thing-transforms-ref-list 3)))))

(defvar conn-dispatch-action-reference nil)

(defvar conn-dispatch-action-ref-list
  (conn-reference-quote
    (("copy from" conn-dispatch-copy-from)
     ("send" conn-dispatch-send)
     ("kapply" conn-dispatch-kapply)
     ("yank to/read"
      conn-dispatch-yank-to
      conn-dispatch-reading-yank-to)
     ("copy to" conn-dispatch-copy-to)
     ("transpose" conn-dispatch-transpose)
     ("register load" conn-dispatch-register-load)
     ("repeat command at" conn-dispatch-repeat-command)
     ("take" conn-dispatch-take))))

(defvar conn-dispatch-command-reference
  (conn-reference-page
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

(defvar conn-dispatch-other-end-documentation
  "Operate with point at the other end of the target.")

(defvar conn-this-win-argument-documentation
  "Restrict dispatch matches to the selected window.")

(defvar conn-repeat-argument-documentation
  "Perform the current dispatch repeatedly.")

(defun conn-dispatch-reference ()
  (list conn-dispatch-command-reference
        conn-dispatch-thing-reference))

(defun conn-dispatch-select-action-reference ()
  (conn-reference-page
    (:splice conn-dispatch-action-reference)
    (((:heading "Action Commands")
      ("toggle repeat" repeat-dispatch)
      ("undo" undo))
     (""
      ("act at mouse click" act)
      ("toggle other end" other-end)))))

(defun conn-dispatch-select-target-reference ()
  (conn-reference-page
    (:splice (oref conn-dispatch-target-finder reference))
    (((:heading "Targeting Commands")
      ("retarget" retarget)
      ("always retarget" always-retarget)
      ("change target finder" change-target-finder)
      (:keymap conn-toggle-label-argument-map)
      ("toggle hide labels" toggle-labels))
     ((:heading "Window Commands")
      ("goto window" conn-goto-window)
      ("scroll up" scroll-up-command)
      ("scroll down" scroll-down-command)
      ("restrict to selected" restrict-windows)))))

(defvar conn-misc-reference-list
  (conn-reference-quote
    (("isearch forward" isearch-forward)
     ("isearch forward regexp" isearch-forward-regexp)
     ("recursive edit" recursive-edit)
     ("quoted insert" quoted-insert)
     ("toggle input method" toggle-input-method)
     ("set input method" set-input-method))))

(defvar conn-dispatch-select-misc-reference
  (conn-reference-page
    :depth 50
    (:heading "Miscellaneous Commands")
    (:eval (conn-quick-ref-to-cols
            conn-misc-reference-list 2))))

;;;;;; Action

(defvar-keymap conn-dispatch-repeat-argument-map
  "TAB" 'repeat-dispatch)

(cl-defstruct (conn-dispatch-action-argument
               (:include conn-argument))
  (repeat nil)
  (action-command nil)
  (arguments nil :type list))

(defun conn-dispatch-action-argument (&optional required)
  (make-conn-dispatch-action-argument
   :keymap conn-dispatch-repeat-argument-map
   :required required))

(cl-defmethod conn-argument-get-reference ((arg conn-dispatch-action-argument))
  (let* ((action (conn-dispatch-action-argument-value arg))
         (ref (conn-action-get-reference action)))
    (conn-reference-page
      :depth -50
      (:splice ref)
      (:heading (when ref "Action Bindings"))
      ((:splice (conn-quick-ref-to-cols
                 conn-dispatch-action-ref-list 3))))))

(cl-defmethod conn-argument-command-documentation ((_arg conn-dispatch-action-argument)
                                                   cmd
                                                   break)
  (pcase cmd
    ('repeat-dispatch
     (funcall break
              (conn-reference-page
                (:eval
                 (substitute-command-keys
                  "\\<conn-dispatch-char-argument-map>Perform the next dispatch in a loop.
Complete the loop with \\[finish].
Abort the loop and undo all changes with \\[keyboard-quit].")))))
    ((and (guard (function-get cmd :conn-dispatch-action)))
     (if-let* ((docstring (documentation cmd)))
         (funcall break (conn-reference-page (:eval docstring)))
       (funcall break (conn-reference-page
                        (:eval (format "Action `%s'." cmd))))))))

(cl-defmethod conn-argument-update ((arg conn-dispatch-action-argument)
                                    cmd
                                    break)
  (cl-symbol-macrolet
      ((arguments (conn-dispatch-action-argument-arguments arg))
       (action (conn-argument-value arg))
       (set-flag (conn-argument-set-flag arg))
       (action-command (conn-dispatch-action-argument-action-command arg)))
    (pcase cmd
      ((and 'repeat-dispatch)
       (when (or (null (conn-argument-value arg))
                 (conn-action-repeatable-p (conn-argument-value arg)))
         (cl-callf not (conn-dispatch-action-argument-repeat arg)))
       (funcall break))
      ((guard (function-get cmd :conn-dispatch-action))
       (conn-action-cancel (conn-argument-value arg))
       (mapc #'conn-argument-cancel arguments)
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
         (cl-loop for desc in (oclosure--class-slots
                               (cl--find-class
                                (oclosure-type new-action)))
                  for val = (slot-value new-action
                                        (cl--slot-descriptor-name desc))
                  when (and (oclosure--slot-mutable-p desc)
                            (conn-argument-p val))
                  do (push val arguments))
         (setf (conn-dispatch-action-argument-repeat arg)
               (pcase (conn-dispatch-action-argument-repeat arg)
                 ((guard (not (conn-action-repeatable-p new-action))))
                 ((or 'auto 'nil)
                  (and (conn-action--action-auto-repeat new-action)
                       'auto))
                 (_ (conn-dispatch-action-argument-repeat arg)))))
       (funcall break))
      (_
       (dolist (a arguments)
         (conn-argument-update a cmd break))))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-action-argument))
  (mapc #'conn-argument-cancel
        (conn-dispatch-action-argument-arguments arg))
  (conn-action-cancel (conn-argument-value arg)))

(cl-defmethod conn-argument-accept ((arg conn-dispatch-action-argument))
  (mapc #'conn-argument-accept
        (conn-dispatch-action-argument-arguments arg)))

(cl-defmethod conn-argument-extract-value ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-dispatch-action-argument-value arg)))
    (cl-loop for desc in (oclosure--class-slots
                          (cl--find-class
                           (oclosure-type action)))
             for slot = (cl--slot-descriptor-name desc)
             when (and (oclosure--slot-mutable-p desc)
                       (conn-argument-p (slot-value action slot)))
             do (cl-callf conn-argument-extract-value
                    (slot-value action slot))))
  (list (conn-dispatch-action-argument-value arg)
        (conn-dispatch-action-argument-repeat arg)))

(cl-defmethod conn-argument-predicate ((arg conn-dispatch-action-argument)
                                       sym)
  (or (function-get sym :conn-dispatch-action)
      (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
               thereis (conn-argument-predicate a sym))))

(cl-defmethod conn-argument-completion-annotation ((arg conn-dispatch-action-argument)
                                                   sym)
  (or (and (function-get sym :conn-dispatch-action)
           " (action)")
      (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
               thereis (conn-argument-completion-annotation a sym))))

(cl-defmethod conn-argument-compose-keymap ((arg conn-dispatch-action-argument))
  (make-composed-keymap
   (cons (cl-call-next-method)
         (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
                  collect (conn-argument-compose-keymap a)))))

(cl-defmethod conn-argument-display ((arg conn-dispatch-action-argument))
  (list
   (concat (substitute-command-keys "\\[repeat-dispatch] ")
           (propertize
            "repeat"
            'face (when (conn-dispatch-action-argument-repeat arg)
                    'eldoc-highlight-function-argument)))
   (when-let* ((action (conn-argument-value arg)))
     (concat (propertize "Do"
                         'face 'bold
                         'conn-read-args-display-depth -50)
             ": "
             (propertize (conn-action-display action)
                         'face 'eldoc-highlight-function-argument)))
   (cl-loop for a in (conn-dispatch-action-argument-arguments arg)
            collect (mapcar
                     (lambda (str)
                       (propertize str 'conn-read-args-display-depth -49))
                     (flatten-tree (conn-argument-display a))))))

;;;;;; Command Handler

(cl-defstruct (conn-dispatch-command-handler
               (:include conn-read-args-command-handler)
               ( :constructor conn-dispatch-command-handler
                 (&aux
                  (reference conn-dispatch-command-reference)))))

(cl-defmethod conn-argument-get-reference ((_arg conn-read-args-command-handler))
  (list conn-dispatch-command-reference
        (cl-call-next-method)))

(conn-define-argument-command ((arg conn-dispatch-command-handler)
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

(conn-define-argument-command ((arg conn-dispatch-command-handler)
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

(conn-define-argument-command ((arg conn-dispatch-command-handler)
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

(cl-defgeneric conn-bounds-of-dispatch (thing arg location))

(cl-defmethod conn-bounds-of-dispatch (thing arg location)
  (when-let* ((bounds (save-excursion
                        (goto-char location)
                        (conn-bounds-of thing arg))))
    (setf (conn-bounds-get bounds :origin) (point))
    bounds))

(cl-defgeneric conn-dispatch-bounds-over (bounds)
  (declare (important-return-value t)
           (conn-anonymous-thing-property :over)
           (conn-bounds-transformation
            "over"
            "Transform bounds to begin at the start of the thing at point and end at
the end of the thing dispatched on.  Can only be used during
`conn-dispatch'.")))

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
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-anchored)
           (conn-bounds-transformation
            "anchored"
            "Transform bounds to begin at point and end the bound most distant from
point.  If `conn-dispatch-other-end' is non-nil then end at the bound
nearest to point.  Can only be used during `conn-dispatch'."
            :no-reformat t)))

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
        (if (conn-dispatch-other-end-p)
            (cons origin end)
          (cons origin beg)))))
    (_ bounds)))

(cl-defgeneric conn-dispatch-bounds-between (bounds)
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-between)
           (conn-bounds-transformation
            "between"
            "Dispatch on a second thing and transform bounds to be the largest region
created from the bounds of the two things.  The new beg and end are
taken to be the points where point would be after dispatching on each
thing.  Can only be used during `conn-dispatch'.")))

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
  '((t (:inherit isearch)))
  "Face for matches when reading strings."
  :group 'conn-faces)

(defvar conn-targets nil
  "Alist of (WINDOW . TARGETS).")

(defvar conn-target-count nil
  "Alist of (WINDOW . TARGET-COUNT).")

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
  "Predicate which windows must satisfy in order to be considered during
dispatch.")

(defvar conn-target-predicate
  (lambda (pt length window)
    (not (conn--overlays-in-of-type pt (+ pt length)
                                    'conn-target-overlay
                                    window)))
  "Predicate which a buffer position must satisfy in order to be a valid
target.

A target predicate function should take POINT, LENGTH, and WINDOW as
arguments and return non-nil if a target at POINT of LENGTH in WINDOW's
buffer is a valid target.")

(defvar conn--dispatch-prev-state nil
  "Holds state to be restored during `conn-with-dispatch-suspended'.

3-tuple of `conn-target-window-predicate', `conn-target-predicate', and
`conn-target-sort-function'.")

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
         (old (conn--overlays-in-of-type pt (+ pt (max length 1))
                                         'conn-old-target window))
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

(defvar conn-dispatch-label-function 'conn-dispatch-simple-labels
  "Function responsible for labeling all `conn-targets'.

A labeling function may take an optional argument STATE and should
return either a list of label strings or a list of the form
(:state NEW-STATE . LABELS).  If the labeling function returns such a
list then NEW-STATE will be passed to the labeling function the next
time it is called.  The first time the labeling function is called STATE
will be nil.")

(defvar conn--dispatch-label-state nil
  "The state for `conn-dispatch-label-function' during dispatch.")

(defvar conn-default-label-padding-function 'conn--centered-padding
  "Default function for padding dispatch labels.

Target overlays may override this default by setting the
\\='padding-function overlay property.")

(defvar conn-pixelwise-labels-window-predicate
  'conn--pixelwise-labels-window-p)

(defvar conn-dispatch-pixelwise-labels-line-limit 250
  "Maximum position in a line for pixelwise labeling.")

(defvar conn-pixelwise-labels-target-predicate
  'conn--pixelwise-labels-target-p)

(defvar conn--label-start-time nil)

(defconst conn--pixelwise-window-cache (make-hash-table :test 'eq))

(defun conn--pixelwise-labels-window-p (win)
  (declare (important-return-value t))
  (eq (selected-frame) (window-frame win)))

(defun conn--pixelwise-labels-target-p (target)
  (declare (important-return-value t))
  (and (< (save-excursion
            (goto-char (overlay-start target))
            (- (point) (pos-bol)))
          conn-dispatch-pixelwise-labels-line-limit)
       (time-less-p (float-time (time-since conn--label-start-time))
                    0.01)))

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
  (let* ((left (min 15 (floor width 2)))
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
  (or (cl-loop for (beg _ . end) in (conn--dispatch-window-lines window)
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
          (setq ov (make-overlay (point-min) (point-max)))
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
          (setq display-width
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
                  (setq end (1+ pt)
                        pixelwise nil))
                 ((and (get-char-property pt 'after-string)
                       (not (eq (get-char-property pt 'after-string)
                                (get-char-property (1+ pt) 'after-string))))
                  (setq end (1+ pt)
                        pixelwise nil))
                 ;; If the label overlay is wider than the label
                 ;; string we are done.
                 ((let ((width
                         (save-excursion
                           (with-restriction beg pt
                             (- (car (window-text-pixel-size window beg pt))
                                ;; Subtract the width of any
                                ;; before strings
                                (with-memoization beg-width
                                  (car (window-text-pixel-size window beg beg))))))))
                    ;; FIXME: This doesn't handle zero length
                    ;;        overlays with after strings.
                    (when (or (= pt (point-max))
                              (>= width display-width))
                      (setq padding-width (max (- width display-width) 0)
                            end pt))))
                 ((cl-loop for ov in (conn--overlays-in-of-type
                                      pt (1+ pt) 'conn-target-overlay window)
                           thereis (not (eq ov target)))
                  (setq end pt))
                 ((get-text-property pt 'composition)
                  (setq pt (next-single-property-change
                            pt 'composition nil line-end)))
                 (t (cl-incf pt))))
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
               (ov nil))
    (unwind-protect
        (progn
          ;; display-line-numbers, line-prefix and wrap-prefix break
          ;; width calculations, temporarily disable them.
          (setq ov (make-overlay (point-min) (point-max)))
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
          (setq display-width
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
                (cond
                 ((= line-beg pt)
                  (setq end pt))
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
                  (setq end (min beg (1+ pt))))
                 ((and (/= pt (point-min))
                       (get-char-property (1- pt) 'after-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'after-string nil (1+ pt))))
                  (setq end pt))
                 ((let ((width
                         (save-excursion
                           (with-restriction pt beg
                             (- (car (window-text-pixel-size window pt beg))
                                (car (window-text-pixel-size window pt pt)))))))
                    (when (or (= pt (point-min))
                              (>= width display-width))
                      (setq padding-width (max (- width display-width) 0)
                            end pt))))
                 ((conn--overlays-in-of-type (1- pt) pt
                                             'conn-target-overlay
                                             window)
                  (setq end pt))
                 ((get-text-property (1- pt) 'composition)
                  (setq pt (previous-single-property-change
                            (1- pt) 'composition nil line-beg)))
                 (t (cl-decf pt))))
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
                (setq end pt)
              (setq end (1+ pt))
              (let ((str (buffer-substring pt end)))
                (add-text-properties
                 0 (length str)
                 `(invisible ,(get-char-property pt 'invisible win))
                 str)
                (overlay-put overlay 'after-string str))))
           ((or (= pt (point-max))
                (= (- pt beg) (length full-string)))
            (setq end pt))
           ((and (/= beg pt)
                 (conn--overlays-in-of-type pt (1+ pt)
                                            'conn-target-overlay
                                            win))
            (setq end pt))
           ((or (and (get-text-property pt 'display)
                     (= pt (next-single-char-property-change
                            (1- pt) 'display nil (1+ pt))))
                (and (get-text-property pt 'after-string)
                     (= pt (next-single-char-property-change
                            (1- pt) 'after-string nil (1+ pt))))
                (and (get-text-property pt 'before-string)
                     (= pt (next-single-char-property-change
                            (1- pt) 'before-string nil (1+ pt)))))
            (setq end (max beg (1- pt))))
           ((get-text-property pt 'composition)
            (setq pt (next-single-property-change
                      pt 'composition nil line-end)))
           (t (cl-incf pt))))
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
              (setq end pt))
             ((or (= pt (point-min))
                  (= (abs (- pt beg)) (length full-string)))
              (setq end pt))
             ((conn--overlays-in-of-type (1- pt) pt
                                         'conn-target-overlay
                                         win)
              (setq end pt))
             ((or (and (get-char-property pt 'display)
                       (= pt (next-single-char-property-change
                              (1- pt) 'display nil (1+ pt))))
                  (and (get-char-property pt 'after-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'after-string nil (1+ pt))))
                  (and (get-char-property pt 'before-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'before-string nil (1+ pt)))))
              (setq end (1+ pt)))
             ((get-text-property (1- pt) 'composition)
              (setq pt (previous-single-property-change
                        (1- pt) 'composition nil line-beg)))
             (t (cl-decf pt)))))
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
  (unless window (setq window (selected-window)))
  (with-memoization (gethash window conn--dispatch-window-lines-cache)
    (let (lines prev)
      (with-selected-window window
        (save-excursion
          (goto-char (window-start window))
          (setq prev (point))
          (while (and (<= prev (window-end window))
                      (not (eobp)))
            (let ((eovl (save-excursion
                          (vertical-motion (cons (window-width) 0))
                          (point))))
              (vertical-motion 1)
              (if (= (point) eovl)
                  (push (cons prev (cons (1- eovl) eovl))
                        lines)
                (push (cons prev (cons eovl eovl))
                      lines))
              (setq prev (point))))))
      (nreverse lines))))

(defun conn-dispatch-get-display-line (&optional point)
  (cl-loop for line from 0
           for (beg end . _) in (conn--dispatch-window-lines)
           when (<= beg (or point (point)) end)
           return line))

(defun conn-dispatch-pixelwise-label-p (ov)
  (declare (important-return-value t))
  (and (display-graphic-p)
       (funcall conn-pixelwise-labels-window-predicate
                (overlay-get ov 'window))
       (funcall conn-pixelwise-labels-target-predicate ov)))

(defun conn-dispatch-create-label (target string)
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
           #'make-conn-dispatch-label)
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
       :prefix (when-let* ((pfx (overlay-get target 'label-prefix)))
                 (propertize pfx 'face face))
       :suffix (when-let* ((sfx (overlay-get target 'label-suffix)))
                 (propertize sfx 'face face))
       :narrowed-string str
       :overlay ov
       :target target))))

(defun conn-dispatch-simple-labels (&optional state)
  "Create simple labels for all targets."
  (declare (important-return-value t))
  (setq conn-dispatch-label-input-method conn-simple-label-input-method)
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
     (setq conn--dispatch-label-state state)
     labels)
    (labels labels)))

(defun conn--with-dispatch-labels (labels body)
  (clrhash conn--dispatch-window-lines-cache)
  (unwind-protect
      (conn-with-dispatch-handlers
        (:handler
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
        (dolist (window (conn--get-target-windows))
          (ignore (conn--dispatch-window-lines window)))
        (funcall body labels))
    (mapc #'conn-label-delete labels)))

(defmacro conn-with-dispatch-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,val)
     `(let ((conn-dispatch-label-input-method nil)
            (conn--label-start-time (float-time (current-time))))
        (conn--with-dispatch-labels ,val (lambda (,var) ,@body))))
    (_ (error "Unexpected binder form"))))

;;;;; Dispatch Loop

(defvar conn-dispatch-in-progress nil)

(defvar conn--dispatch-change-groups nil)

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
      (setq depth (nth 1 cases)
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
  (redisplay)
  (unless maybe-dont-prompt
    (setq conn--dispatch-redisplay-prompt-flag t))
  (throw 'dispatch-redisplay nil))

(defvar conn--current-dispatch-buffers)

(defun conn-dispatch-select-window (window)
  (prog1 (select-window window)
    (or (gethash (current-buffer) conn--current-dispatch-buffers)
        (setf (gethash (current-buffer) conn--current-dispatch-buffers)
              (point-marker)))))

(defun conn-dispatch-goto-char (position &optional nopush)
  (goto-char position)
  (recenter (conn-dispatch-get-display-line))
  (when-let* ((mk (and (not nopush)
                       (gethash (current-buffer)
                                conn--current-dispatch-buffers))))
    (unless (region-active-p)
      (push-mark mk))
    (unless (or (not conn-jump-ring-mode)
                (gethash conn-jump-ring conn--current-dispatch-buffers))
      (conn-push-jump-ring mk)
      (setf (gethash conn-jump-ring conn--current-dispatch-buffers) t))
    (set-marker mk (point))))

(cl-defgeneric conn-dispatch-perform-action (action))

(cl-defmethod conn-dispatch-perform-action (action)
  (let ((success nil)
        (owconf (current-window-configuration))
        (oframe (selected-frame))
        (opoint (point))
        (conn-dispatch-action-reference
         (conn-action-get-reference action))
        (conn--dispatch-label-state nil)
        (conn--dispatch-change-groups nil)
        (conn--read-args-error-message nil)
        (conn--current-dispatch-buffers (make-hash-table :test 'eq)))
    (setf (gethash (current-buffer) conn--current-dispatch-buffers)
          (point-marker))
    (conn--unwind-protect-all
      (progn
        (redisplay)
        (catch 'dispatch-exit
          (while (or conn-dispatch-repeating
                     (< conn-dispatch-iteration-count 1))
            (condition-case err
                (conn-with-dispatch-handlers
                  (when (conn-action-repeatable-p action)
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
                  (catch 'dispatch-undo
                    (let ((frame (selected-frame))
                          (wconf (current-window-configuration))
                          (pt (point))
                          (label-state conn--dispatch-label-state))
                      (push nil conn--dispatch-change-groups)
                      (conn-dispatch-undo-case
                        :depth 100
                        (:undo (redisplay)))
                      (unwind-protect
                          (funcall action)
                        (unless (or (equal wconf (current-window-configuration))
                                    (null (car conn--dispatch-change-groups)))
                          (conn-dispatch-undo-case
                            :depth -90
                            (:undo
                             (select-frame frame)
                             (set-window-configuration wconf)
                             (goto-char pt)
                             (setq conn--dispatch-label-state label-state)
                             (cl-decf conn-dispatch-iteration-count)))))
                      (cl-incf conn-dispatch-iteration-count))))
              (user-error
               (pcase-dolist (`(,_ . ,undo-fn)
                              (pop conn--dispatch-change-groups))
                 (funcall undo-fn :undo))
               (let ((message-log-max t))
                 (message (error-message-string err)))
               (setf conn--read-args-error-message
                     (error-message-string err))))))
        (setq success t))
      (setq conn-dispatch-quit-flag (not success))
      (dolist (undo conn--dispatch-change-groups)
        (pcase-dolist (`(,_ . ,undo-fn) undo)
          (funcall undo-fn (if success :accept :cancel))))
      (if success
          (conn-action-accept action)
        (conn-action-cancel action))
      (maphash
       (lambda (_buf mk)
         (when (markerp mk) (set-marker mk nil)))
       conn--current-dispatch-buffers)
      (unless success
        (select-frame oframe)
        (set-window-configuration owconf)
        (goto-char opoint)))))

(defun conn-select-target ()
  "Prompt the user to select a target during dispatch.

Returns a list of (POINT WINDOW THING ARG TRANSFORM)."
  (unwind-protect
      (progn
        (dolist (win (conn--get-target-windows))
          (with-selected-window win
            (add-to-invisibility-spec 'conn-dispatch-invisible)))
        (conn-target-finder-select conn-dispatch-target-finder))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (remove-from-invisibility-spec 'conn-dispatch-invisible)
        (when-let* ((line (conn-dispatch-get-display-line)))
          (recenter line))))))

(defun conn-dispatch-action-pulse (beg end)
  "Momentarily highlight the region between BEG and END."
  (require 'pulse)
  (unless executing-kbd-macro
    (set-face-background
     'conn--dispatch-action-current-pulse-face
     (face-background 'pulse-highlight-start-face
                      nil
                      'default))
    (pulse-momentary-highlight-region
     beg end
     'conn--dispatch-action-current-pulse-face)))

(defun conn--dispatch-push-undo-case (depth body)
  (push (cons depth body)
        (car conn--dispatch-change-groups))
  (conn--compat-callf sort (car conn--dispatch-change-groups)
    :key #'car
    :in-place t))

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
      (setq buffers (delete-dups buffers))
    (setq buffers (list (current-buffer))))
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
      (:accept (accept-change-group cg)))))

(defun conn-dispatch-undo-pulse (beg end)
  "Highlight an undo between BEG and END."
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (face-attribute 'conn-dispatch-undo-pulse-face :background))
  (pulse-momentary-highlight-region
   (min beg end) (max beg end)
   'conn--dispatch-action-current-pulse-face))

(defun conn--dispatch-read-char-prefix (arguments prompt suffix)
  (conn-<
    (mapcar #'conn-argument-display arguments)
    flatten-tree
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

(defvar conn-dispatch-read-char-pre-functions nil)

(defun conn-dispatch-read-char (&optional
                                prompt
                                use-input-method
                                seconds
                                prompt-suffix)
  (declare (important-return-value t))
  (let ((message-fn (lambda (prompt arguments)
                      (conn--dispatch-read-char-prefix
                       arguments
                       prompt
                       prompt-suffix))))
    (if seconds
        (let ((inhibit-message nil)
              (scroll-conservatively 100))
          (conn--dispatch-read-char-prefix
           (mapcar #'car
                   (compat-call sort conn--dispatch-read-char-handlers
                                :key #'cadr))
           prompt prompt-suffix)
          (and-let* ((ev (conn--dispatch-read-char-1 use-input-method seconds)))
            (cond ((eql ev (car (last (current-input-mode))))
                   (signal 'quit nil))
                  ((characterp ev) ev))))
      (conn-read-args (nil
                       :prompt prompt
                       :command-handler (conn-dispatch-read-char-handlers)
                       :display-handler message-fn
                       :pre (lambda (cmd)
                              (run-hook-with-args
                               'conn-dispatch-read-char-pre-functions
                               cmd)))
          ((char (conn-dispatch-char-argument use-input-method)))
        char))))

(defmacro conn-with-dispatch-suspended (&rest body)
  "Execute BODY with dispatch suspended."
  (declare (indent 0))
  `(progn
     (unless conn-dispatch-in-progress
       (error "Trying to suspend dispatch when state not active"))
     (conn-target-finder-suspend-targets conn-dispatch-target-finder)
     (pcase-let ((`(,conn-target-window-predicate
                    ,conn-target-predicate
                    ,conn-target-sort-function)
                  conn--dispatch-prev-state)
                 (conn--dispatch-prev-state nil)
                 (conn-dispatch-in-progress nil)
                 (conn--dispatch-redisplay-prompt-flag nil)
                 (conn-dispatch-label-function nil)
                 (conn-dispatch-quit-flag nil)
                 (conn-dispatch-repeating nil)
                 (conn-dispatch-action-reference nil)
                 (conn-targets nil)
                 (conn--dispatch-label-state nil)
                 (conn-dispatch-target-finder nil)
                 (conn--dispatch-change-groups nil)
                 (conn-dispatch-iteration-count nil)
                 (conn-dispatch-other-end nil)
                 (conn--dispatch-read-char-handlers nil)
                 (conn-dispatch-hide-labels nil)
                 (conn-dispatch-input-buffer nil))
       (message nil)
       ,@body)))

(cl-defstruct (conn-dispatch-read-char-handlers
               (:include conn-composite-argument)
               ( :constructor conn-dispatch-read-char-handlers
                 (&aux
                  (value (mapcar #'car
                                 (compat-call sort
                                              conn--dispatch-read-char-handlers
                                              :key #'cadr)))))))

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
   'face 'read-multiple-choice-face))

(conn-define-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql negative-argument)))
  "Invert the prefix argument."
  ( :update (break)
    (cl-callf not conn--read-args-prefix-sign)
    (funcall break)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql universal-argument)))
  "Multiply the prefix argument by four."
  ( :update (break)
    (if conn--read-args-prefix-mag
        (cl-callf * conn--read-args-prefix-mag 4)
      (setq conn--read-args-prefix-mag 4))
    (funcall break)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
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

(conn-define-dispatch-handler-command ((arg conn-dispatch-prefix-arg)
                                       (cmd (eql reset-arg)))
  "Reset the current prefix argument."
  ( :update (break)
    (setq conn--read-args-prefix-mag nil
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
  (when-let* ((im (buffer-local-value 'current-input-method-title
                                      conn-dispatch-input-buffer)))
    (propertize im 'face 'read-multiple-choice-face)))

(conn-define-dispatch-handler-command ((arg conn-read-char-input-method)
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
            nil t)))))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-read-char-input-method)
                                       (cmd (eql toggle-input-method)))
  "Toggle the current dispatch input method."
  ( :update (break)
    (let ((inhibit-message nil)
          (message-log-max t)
          (arg (conn-read-args-consume-prefix-arg))
          (buf conn-dispatch-input-buffer))
      (if arg
          (progn
            (conn-with-dispatch-suspended
              (with-current-buffer buf
                (toggle-input-method arg)))
            (conn-dispatch-redisplay))
        (with-current-buffer buf
          (toggle-input-method))
        (funcall break)))))

(conn-define-dispatch-handler-command ((arg conn-read-char-input-method)
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
  "C-o" 'conn-goto-window
  "C-s" 'isearch-forward
  "C-M-s" 'isearch-regexp-forward
  "C-v" 'scroll-up-command
  "M-v" 'scroll-down-command
  "M-?" 'help
  "?" 'help
  "C-w" 'restrict-windows
  "M-TAB" 'repeat-dispatch
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
   "\\[help] help"
   (when (conn-dispatch-other-end-p)
     (concat
      "\\[other-end] "
      (propertize
       "other end"
       'face 'conn-argument-active-face)))
   (when (advice-function-member-p
          'conn--dispatch-restrict-windows
          conn-target-window-predicate)
     (concat
      "\\[restrict-windows] "
      (propertize
       "this win"
       'face 'eldoc-highlight-function-argument)))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql act)))
  "Reset the current prefix argument."
  ( :update (_break)
    (when (mouse-event-p last-input-event)
      (let* ((posn (event-start last-input-event))
             (win (posn-window posn))
             (pt (posn-point posn)))
        (when (and (not (posn-area posn))
                   (funcall conn-target-window-predicate win))
          (:return (list pt win nil)))))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
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
          (:return (list pt win nil)))))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql change-target-finder)))
  "Change the current target finder."
  ( :update (_break)
    (conn-read-args (conn-dispatch-targets-state
                     :prompt "New Targets"
                     :reference (list conn-dispatch-thing-reference)
                     :around (lambda (cont)
                               (conn-with-dispatch-suspended
                                 (funcall cont))))
        ((`(,thing ,arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument)))
      (conn-target-finder-setup
       (conn-get-target-finder thing arg transform))
      (conn-dispatch-redisplay))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql help)))
  "Display help."
  ( :update (break)
    (conn-quick-reference
     (conn-dispatch-select-action-reference)
     (conn-dispatch-select-target-reference)
     conn-dispatch-select-misc-reference)
    (funcall break)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql mwheel-scroll)))
  "Mouse wheel scroll."
  ( :update (_break)
    (when (bound-and-true-p mouse-wheel-mode)
      (mwheel-scroll last-input-event))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql recursive-edit)))
  "Enter a recursive edit with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (conn-with-recursive-stack 'conn-command-state
        (recursive-edit)))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
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
    (setq recenter-last-op nil)))
(add-hook 'conn-dispatch-read-char-pre-functions
          'conn--dispatch-recenter-hook)

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-forward)))
  "isearch forward with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-forward))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-backward)))
  "isearch backward with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-backward))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-forward-regexp)))
  "isearch forward regexp with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-forward-regexp))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql isearch-backward-regexp)))
  "isearch backward regexp with dispatch suspended."
  ( :update (_break)
    (conn-with-dispatch-suspended
      (isearch-backward-regexp))
    (conn-dispatch-redisplay)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql scroll-up-command)))
  "Scroll the window up."
  ( :update (_break)
    (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                         next-screen-context-lines)))
      (ignore-error end-of-buffer
        (scroll-up))
      (conn-dispatch-redisplay))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql scroll-down-command)))
  "Scroll the window down."
  ( :update (_break)
    (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                         next-screen-context-lines)))
      (ignore-error beginning-of-buffer
        (scroll-down))
      (conn-dispatch-redisplay))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql conn-goto-window)))
  "Select a different window."
  ( :update (break)
    (if-let* ((windows (delq (selected-window) (conn--get-target-windows))))
        (progn
          (conn-dispatch-select-window (conn-prompt-for-window windows))
          (conn-dispatch-redisplay 'maybe-dont-prompt))
      (funcall break))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql finish)))
  "Complete the current dispatch."
  ( :update (_break)
    (throw 'dispatch-exit nil)))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
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

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql restrict-windows)))
  "Restrict targets to the selected window."
  ( :update (_break)
    (cond ((advice-function-member-p 'conn--dispatch-restrict-windows
                                     conn-target-window-predicate)
           (remove-function conn-target-window-predicate
                            'conn--dispatch-restrict-windows)
           (conn-dispatch-redisplay))
          ((length> conn-targets 1)
           (add-function :after-while conn-target-window-predicate
                         'conn--dispatch-restrict-windows)
           (conn-dispatch-redisplay)))))

(conn-define-dispatch-handler-command ((arg conn-dispatch-select-command-handler)
                                       (cmd (eql undo)))
  "Undo the most recent iteration of dispatch."
  ( :update (_break)
    (dolist (group (prog1 (take 2 conn--dispatch-change-groups)
                     (cl-callf2 drop 2 conn--dispatch-change-groups)))
      (pcase-dolist (`(,_ . ,undo-fn) group)
        (funcall undo-fn :undo)))
    (throw 'dispatch-undo nil)))

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
  :abstract t)

(eval-and-compile
  (defun conn--define-target-finder (name
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
                       (_ (error "Malformed default handler definition")))))
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

(defmacro conn-define-target-finder (name superclasses slots &rest rest)
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
      (setq docstring (pop rest)))
    (conn--define-target-finder name superclasses slots docstring rest)))

(defun conn-add-update-handler (target-finder function &optional depth)
  (unless depth (setq depth 0))
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
    (dolist (win (nreverse (conn--get-target-windows)))
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
                 (delq a (conn--overlays-in-of-type (overlay-end a)
                                                    (+ 2 (overlay-end a))
                                                    'conn-target-overlay
                                                    (selected-window)))
                 (not (delq b (conn--overlays-in-of-type (overlay-end b)
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
            (dolist (target targets)
              (push target old)))
          (setq conn-targets nil
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
      (cl-loop
       (catch 'dispatch-redisplay
         (unwind-protect
             (cl-return (cl-call-next-method))
           (conn-target-finder-suspend-targets target-finder)))))))

(cl-defmethod conn-target-finder-select (_target-finder)
  (let ((after nil)
        (conn--dispatch-redisplay-prompt-flag nil))
    (conn-with-dispatch-labels
        (labels (conn-dispatch-get-labels))
      (prog1
          (conn-with-dispatch-handlers
            ( :with (conn-dispatch-select-command-handler)
              :depth -95)
            (when executing-kbd-macro
              (:handler
               (:depth 50)
               ( :display ()
                 (list "\\[exit] exit macro"
                       "\\[skip] skip iteration"))
               ( :predicate (cmd)
                 (or (eq cmd 'exit) (eq cmd 'skip)))
               (:keymap (define-keymap
                          "C-e" 'exit
                          "RET" 'exit
                          "<return>" 'exit
                          "C-n" 'skip))
               ( :update (cmd _break)
                 (pcase cmd
                   ('skip
                    (setq after (lambda () (setq executing-kbd-macro "")))
                    (:return))
                   ('exit
                    (setq after (lambda () (setq executing-kbd-macro t)))
                    (:return))))))
            (let ((executing-kbd-macro nil)
                  (defining-kbd-macro nil))
              (conn-label-select
               labels
               (concat
                (when-let* ((prompt (oref conn-dispatch-target-finder prompt)))
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
    (conn-target-finder-clear-targets conn-dispatch-target-finder))
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
        (unless (overlay-get tar 'label-face)
          (if-let* ((thing (overlay-get tar 'thing))
                    (_ (and (conn-anonymous-thing-p thing)
                            (pcase (conn-thing-get thing :multi-thing-p)
                              ((and fn (pred functionp))
                               (funcall fn thing tar))
                              (val val)))))
              (progn
                (overlay-put tar 'label-face multi-face1)
                (cl-rotatef multi-face1 multi-face2))
            (overlay-put tar 'label-face face1)
            (cl-rotatef face1 face2)))))))

(defun conn-clear-targets ()
  (conn-target-finder-clear-targets conn-dispatch-target-finder))

(cl-defgeneric conn-target-finder-clear-targets (target-finder))

(cl-defmethod conn-target-finder-clear-targets (_target-finder)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (delete-overlay target)))
  (setq conn-targets nil
        conn-target-count nil))

(cl-defgeneric conn-target-finder-suspend-targets (target-finder))

(cl-defmethod conn-target-finder-suspend-targets (_target-finder)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (overlay-put target 'category 'conn-old-target)
      (overlay-put target 'face nil)))
  (setq conn-target-count nil))

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

(cl-defmethod conn-target-finder-select ((_target-finder
                                          conn-dispatch-target-key-labels-mixin))
  (conn-with-dispatch-labels
      (labels (conn-dispatch-key-labels))
    (conn-with-dispatch-handlers
      ( :with (conn-dispatch-select-command-handler)
        :depth -95)
      (:handler
       (:depth 100)
       ( :update (obj _break)
         (if (conn-dispatch-label-p obj)
             (:return (conn-label-payload obj))
           (conn-read-args-error "Invalid key")))
       ( :keymap
         (let ((map (make-sparse-keymap)))
           (cl-loop for label in labels
                    for key = (conn-dispatch-label-string label)
                    do (keymap-set map key label))
           (while-no-input
             (conn-redisplay-labels labels))
           map)))
      (ignore (conn-dispatch-read-char "Register"))
      (while t
        (ignore (conn-dispatch-read-char "Register"))))))

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
    (if (or (oref state always-retarget)
            (= (oref state retarget-tick)
               conn-dispatch-iteration-count))
        (conn-target-finder-retarget state)
      (oset state retarget-tick conn-dispatch-iteration-count))
    (cl-call-next-method)))

(conn-define-target-finder conn-dispatch-string-targets
    (conn-dispatch-retargetable-mixin)
    ((string :initform nil
             :initarg :string)
     (predicate :initform nil
                :initarg :predicate)
     (regex-p :initform nil
              :initarg :regex-p))
  ( :default-update-handler (state &optional len)
    (while-no-input
      (let ((string (oref state string))
            (predicate (oref state predicate))
            (thing (oref state thing)))
        (if (oref state regex-p)
            (conn-make-re-target-overlays string predicate len thing)
          (conn-make-string-target-overlays string predicate len thing))))))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(defvar conn-dispatch-read-n-chars-re-alist
  `((?, . "[^a-zA-A]")))

(conn-define-target-finder conn-dispatch-read-n-chars
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
                        (cl-incf literal-count)
                        (regexp-quote (char-to-string char))))))
              (cl-incf char-count)
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

(conn-define-target-finder conn-dispatch-read-with-timeout
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
   (hide :initform t :initarg :hide)
   (context-lines :initform 0 :initarg :context-lines)
   (cursor-location :initform nil)
   (separator-p :initarg :separator)
   (fringe-indicator
    :initform (propertize " " 'display (list 'left-fringe
                                             'right-triangle))
    :initarg :fringe-indicator)
   (always-prompt
    :initform t))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(defvar-keymap conn-dispatch-toggle-focus-map)

(defun conn-focus-targets-remove-overlays (state)
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil))

(cl-defmethod conn-target-finder-clear-targets ((state conn-dispatch-focus-mixin))
  (conn-focus-targets-remove-overlays state)
  (cl-call-next-method))

(cl-defmethod conn-target-finder-suspend-targets ((state conn-dispatch-focus-mixin))
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
               while end
               do (let ((ov (make-overlay beg end)))
                    (push ov hidden)
                    (overlay-put ov 'invisible 'conn-dispatch-invisible)
                    (overlay-put ov 'window win)
                    (overlay-put ov 'before-string fringe-indicator))
               finally (let ((ov (make-overlay beg (point-max))))
                         (push ov hidden)
                         (overlay-put ov 'window win)
                         (overlay-put ov 'invisible 'conn-dispatch-invisible)))
              (setf (alist-get win (oref state hidden))
                    (cons (buffer-chars-modified-tick) hidden))
              (recenter line)))))))
  (redisplay))

(conn-define-target-finder conn-dispatch-focus-thing-at-point
    (conn-dispatch-string-targets
     conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initarg :window-predicate
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
                                 (when-let* ((bd (assq (point) matches)))
                                   (conn-make-bounds 'region nil bd))))))
      (save-excursion
        (while (and (< line-count line-height)
                    (search-backward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (push (cons beg end) matches)
            (conn-make-target-overlay beg 0 :thing thing)
            (when (<= beg (pos-eol) prev)
              (cl-incf line-count))
            (setq prev beg))))
      (setq line-count 0)
      (save-excursion
        (while (and (< line-count line-height)
                    (search-forward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (conn-make-target-overlay beg 0 :thing thing)
            (when (<= prev (pos-bol) beg)
              (cl-incf line-count))
            (setq prev beg)))))))

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-focus-thing-at-point))
  (conn-focus-targets-remove-overlays state))

(conn-define-target-finder conn-dispatch-jump-ring
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initform (lambda (win) (eq win (selected-window))))
     (other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (let ((points (conn-ring-list conn-jump-ring)))
      (dolist (pt points)
        (unless (invisible-p pt)
          (conn-make-target-overlay pt 0)))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(conn-define-target-finder conn-dispatch-global-mark
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (other-end :initform :no-other-end)
     (window-predicate
      :initform (let ((cache nil))
                  (lambda (win)
                    (if-let* ((val (assq win cache)))
                        (cdr var)
                      (setf (alist-get win cache)
                            (cl-loop with buf = (window-buffer win)
                                     for mk in global-mark-ring
                                     thereis (eq buf (marker-buffer mk)))))))))
  ( :default-update-handler (_state)
    (dolist (mk global-mark-ring)
      (when (and (eq (current-buffer) (marker-buffer mk))
                 (not (invisible-p mk)))
        (conn-make-target-overlay mk 0))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(conn-define-target-finder conn-dispatch-mark-register
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

(conn-define-target-finder conn-dispatch-previous-emacs-state
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initform (lambda (win) (eq win (selected-window))))
     (other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (dolist (pt (conn-ring-list conn-emacs-state-ring))
      (unless (invisible-p pt)
        (conn-make-target-overlay pt 0))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(conn-define-target-finder conn-dispatch-headings
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
          (conn-make-target-overlay
           pt 0
           :properties '(no-hide t)))))))

(conn-define-target-finder conn-dispatch-all-defuns
    (conn-dispatch-focus-mixin)
    ((cache :initform nil)
     (window-predicate
      :initform (lambda (win)
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

(conn-define-target-finder conn-all-things-targets
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
          (conn-make-target-overlay (point) 0))))))

(conn-define-target-finder conn-dispatch-button-targets
    ()
    ((other-end :initform :no-other-end))
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-max))
      (while (not (bobp))
        (goto-char (previous-single-char-property-change (point) 'button))
        (when (get-char-property (point) 'button)
          (conn-make-target-overlay (point) 0))))))

(conn-define-target-finder conn-dispatch-regexp-targets
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
   :reference (conn-reference-quote
                ((:heading "Thing With Prefix")
                 (:eval (string-fill
                         (format "Read a prefix string of %s characters for a %s target."
                                 prefix-length thing)
                         72))))))

(conn-define-target-finder conn-dispatch-things-with-prefix-targets
    ()
    ((prefix-string :initarg :prefix-string)
     (fixed-length :initform nil
                   :initarg :fixed-length))
  ( :default-update-handler (state)
    (let ((thing (oref state thing))
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

(conn-define-target-finder conn-dispatch-things-with-re-prefix-targets
    ()
    ((prefix-regexp :initarg :prefix-regexp)
     (skip-prefix :initarg :skip-prefix
                  :initform nil)
     (fixed-length :initform nil
                   :initarg :fixed-length))
  ( :default-update-handler (state)
    (let ((thing (oref state thing))
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

(conn-define-target-finder conn-dispatch-things-matching-re-targets
    ()
    ((regexp :initarg :prefix-regexp)
     (fixed-length :initform nil
                   :initarg :fixed-length)
     (update-function
      :allocation :class
      :initform #'conn--things-matching-re-update))
  ( :default-update-handler (state)
    (let ((thing (oref state thing))
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

(conn-define-target-finder conn-dispatch-column-targets
    ()
    ((window-predicate
      :initform (lambda (win) (eq win (selected-window)))))
  ( :default-update-handler (_state)
    (let ((col-width nil))
      ;; From `line-move-visual'
      (let ((posn (posn-at-point))
            (lnum-width (line-number-display-width t))
            x-pos)
        (cond
         ;; Handle the `overflow-newline-into-fringe' case
         ;; (left-fringe is for the R2L case):
         ((memq (nth 1 posn) '(right-fringe left-fringe))
          (setq col-width (window-width)))
         ((car (posn-x-y posn))
          (setq x-pos (- (car (posn-x-y posn)) lnum-width))
          ;; In R2L lines, the X pixel coordinate is measured from the
          ;; left edge of the window, but columns are still counted
          ;; from the logical-order beginning of the line, i.e. from
          ;; the right edge in this case.  We need to adjust for that.
          (if (eq (current-bidi-paragraph-direction) 'right-to-left)
              (setq x-pos (- (window-body-width nil t) 1 x-pos)))
          (setq col-width (/ (float x-pos)
                             (frame-char-width))))))
      (save-excursion
        (with-restriction (window-start) (window-end)
          (goto-char (point-min))
          (while (/= (point) (point-max))
            (vertical-motion (cons col-width 0))
            (unless (and (eolp) (= (point) (point-min)))
              (conn-make-target-overlay
               (point) 0
               :thing 'point
               :padding-function (lambda (ov width _face)
                                   (conn--flush-left-padding ov width nil))))
            (vertical-motion 1)))))))

(cl-defmethod conn-target-finder-label-faces ((_ conn-dispatch-column-targets))
  nil)

(conn-define-target-finder conn-dispatch-line-targets
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

(conn-define-target-finder conn-dispatch-end-of-line-targets
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

(conn-define-target-finder conn-dispatch-inner-line-targets
    () ()
  ( :default-update-handler (_state)
    (let ((thing (conn-anonymous-thing
                   '(conn-forward-inner-line)
                   :pretty-print ( :method (_self) "inner-line")
                   :bounds-op ( :method (_self _arg)
                                (save-excursion
                                  (goto-char (pos-bol))
                                  (cl-call-next-method))))))
      (conn-for-each-visible (window-start) (window-end)
        (goto-char (point-max))
        (while (let ((pt (point)))
                 (forward-line -1)
                 (conn-beginning-of-inner-line)
                 (/= (point) pt))
          (when (not (invisible-p (point)))
            (conn-make-target-overlay
             (point) 0
             :thing thing)))))))

(conn-define-target-finder conn-dispatch-end-of-inner-line-targets
    ()
    ((other-end :initform t))
  ( :default-update-handler (_state)
    (let ((thing (conn-anonymous-thing
                   '(conn-forward-inner-line)
                   :pretty-print ( :method (_self) "end-of-inner-line")
                   :bounds-op ( :method (_self _arg)
                                (save-excursion
                                  (goto-char (pos-bol))
                                  (cl-call-next-method))))))
      (conn-for-each-visible (window-start) (window-end)
        (goto-char (point-min))
        (when (looking-at-p "\n")
          (forward-line 1))
        (while (not (eobp))
          (conn--end-of-inner-line-1)
          (conn-make-target-overlay
           (point) 0
           :properties `(label-before t)
           :thing thing)
          (forward-line 1))))))

(conn-define-target-finder conn-dispatch-visual-line-targets
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

(conn-define-target-finder conn-dispatch-end-of-visual-line-targets
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
                       label-string ,string))))
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
    (unless (or conn-dispatch-hide-labels
                (length< narrowed-string 1))
      (overlay-put target 'face 'conn-target-overlay-face)
      (with-selected-window (overlay-get overlay 'window)
        (funcall setup-function label)))))

(cl-defmethod conn-label-display ((label conn-dispatch-label))
  (pcase-let* (((cl-struct conn-dispatch-label overlay) label)
               (setup (cl-shiftf (overlay-get overlay 'setup) nil))
               (padding (cl-shiftf (overlay-get overlay 'padding) nil)))
    (when setup (apply #'overlay-put overlay setup))
    (when padding (apply padding))))

;;;;; Dispatch Actions

(cl-defstruct (conn-dispatch-replace-argument
               (:include conn-argument)
               (:constructor
                conn-dispatch-replace-argument
                (&aux (keymap conn-dispatch-replace-argument-map))))
  (string nil))

(cl-defmethod conn-argument-update ((arg conn-dispatch-replace-argument)
                                    (_cmd (eql dispatch-replace))
                                    break)
  (cl-symbol-macrolet ((cg (conn-argument-value arg)))
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

(conn-define-argument-command ((arg conn-dispatch-replace-argument)
                               (cmd (eql dispatch-replace)))
  "Read and replace a thing at point.")

(cl-defmethod conn-argument-display ((arg conn-dispatch-replace-argument))
  (concat (substitute-command-keys "\\[dispatch-replace]")
          " "
          (propertize "replace"
                      'face (when (conn-argument-value arg)
                              'conn-argument-active-face))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-replace-argument))
  (conn--action-cancel-change-group (conn-argument-value arg)))

(cl-defmethod conn-argument-accept ((arg conn-dispatch-replace-argument))
  (conn--action-accept-change-group (conn-argument-value arg))
  (when-let* ((str (conn-dispatch-replace-argument-string arg)))
    (kill-new str)))

(oclosure-define (conn-action
                  (:predicate conn-action-p)
                  (:copier conn-action--copy))
  (action-auto-repeat :type boolean)
  (action-no-history :mutable t :type boolean)
  (action-description :type (or string nil))
  (action-window-predicate :type function)
  (action-target-predicate :type function)
  (action-reference :type string)
  (action-not-repeatable :type boolean))

(defalias 'conn-action-no-history 'conn-action--action-no-history)
(defalias 'conn-action-auto-repeat 'conn-action--action-auto-repeat)
(defalias 'conn-action-description 'conn-action--action-description)
(defalias 'conn-action-window-predicate 'conn-action--action-window-predicate)
(defalias 'conn-action-target-predicate 'conn-action--action-target-predicate)
(defalias 'conn-action-reference 'conn-action--action-reference)

(defun conn-action-repeatable-p (action)
  (not (conn-action--action-not-repeatable action)))

(defun conn-action-get-reference (action)
  (when-let* ((doc-string (and action
                               (conn-action-reference action))))
    (conn-reference-quote
      ((:heading (concat "Current Action: "
                         (conn-action-display action)))
       (:eval doc-string)))))

(eval-and-compile
  (defun conn--set-action-property (f _args)
    `(function-put ',f :conn-dispatch-action t))
  (setf (alist-get 'conn-dispatch-action defun-declarations-alist)
        (list #'conn--set-action-property)))

(cl-defgeneric conn-make-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t)))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing t)))
  (conn-dispatch-jump))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing line-column)))
  (conn-dispatch-jump))

(cl-defgeneric conn-action-stale-p (action)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method ((_action conn-action)) nil))

(cl-defgeneric conn-action-copy (action)
  (declare (important-return-value t))
  (:method (action) (conn-action--copy action)))

(cl-defgeneric conn-action-cleanup (action)
  (:method (_action) "Noop" nil))

(cl-defgeneric conn-action-display (action &optional short)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method ((action conn-action) &optional _)
    (conn-action-description action)))

(cl-defgeneric conn-action-accept (action)
  (:method ((action conn-action)) action))

(cl-defgeneric conn-action-cancel (action)
  (:method (_) "Noop" nil))

(defun conn--action-buffer-change-group ()
  (declare (important-return-value t))
  (let ((change-group (prepare-change-group)))
    (activate-change-group change-group)
    (list change-group (point) (mark t) mark-active)))

(defun conn--action-accept-change-group (change-group)
  (pcase-let ((`(,handle ,_saved-point ,_saved-mark) change-group))
    (accept-change-group handle)))

(defun conn--action-cancel-change-group (change-group)
  (pcase change-group
    (`(,handle ,saved-point ,saved-mark ,saved-mark-active)
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
           (run-hooks 'deactivate-mark-hook)))))))

(oclosure-define (conn-change-group-action
                  (:parent conn-action))
  (action-change-group :mutable t))

(cl-defmethod conn-action-accept ((action conn-change-group-action))
  (conn--action-accept-change-group
   (conn-change-group-action--action-change-group action))
  (cl-call-next-method))

(cl-defmethod conn-action-cancel ((action conn-change-group-action))
  (conn--action-cancel-change-group
   (conn-change-group-action--action-change-group action))
  (setf (conn-change-group-action--action-change-group action) nil))

(defvar conn-dispatch-button-functions nil)

(defun conn-dispatch-button-handler-default (pt)
  (cond ((button-at pt)
         (push-button pt)
         t)
        ((fboundp 'widget-apply-action)
         (ignore-errors
           (widget-apply-action (get-char-property pt 'button) pt)
           t))))

(add-hook 'conn-dispatch-button-functions 'conn-dispatch-button-handler-default 50)

(defun conn-dispatch-push-button ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-action
                    (action-description "Push Button")
                    (action-no-history t)
                    (action-reference
                     "Push the selected button."))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (conn-dispatch-select-window window)
      (run-hook-with-args-until-success 'conn-dispatch-button-functions pt))))

(oclosure-define (conn-dispatch-copy-to
                  (:parent conn-action))
  (str :type string)
  (replace-and-separator :mutable t))

(defun conn-dispatch-copy-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument-dwim))
       (ftransform (conn-transform-argument)))
    (let ((str (pcase (conn-bounds-of fthing farg)
                 ((conn-bounds `(,beg . ,end) ftransform)
                  (conn-dispatch-action-pulse beg end)
                  (filter-buffer-substring beg end))
                 (_ (user-error "No %s found" (conn-thing-pretty-print fthing))))))
      (oclosure-lambda (conn-dispatch-copy-to
                        (action-description "Copy To")
                        (str str)
                        (replace-and-separator
                         (conn-dispatch-to-how-argument))
                        (action-window-predicate
                         (lambda (win)
                           (not
                            (buffer-local-value 'buffer-read-only
                                                (window-buffer win)))))
                        (action-reference
                         "Copy the current region to the region selected by dispatch.  By default
this action copies the current region before the region selected by
dispatch but if OTHER-END is non-nil then it copies the current region
after the region selected by dispatch."))
          ()
        (pcase-let* ((`(,replace ,separator) replace-and-separator)
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
                (_ (user-error "Cannot find thing at point"))))))))))

(oclosure-define (conn-dispatch-yank-to
                  (:parent conn-change-group-action))
  (str :type string)
  (replace-and-separator :mutable t))

(defun conn-dispatch-yank-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-yank-to
                    (action-description "Yank To")
                    (str (current-kill 0))
                    (replace-and-separator (conn-dispatch-to-how-argument))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win)))))
                    (action-reference
                     "Yank the most recent kill to the region selected by dispatch.  By
default this action inserts the text before the region selected by
dispatch but if OTHER-END is non-nil then it inserts the text after the
region selected by dispatch."))
      ()
    (pcase-let* ((`(,replace ,separator) replace-and-separator)
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

(oclosure-define (conn-dispatch-reading-yank-to
                  (:parent conn-action))
  (str :type string)
  (replace-and-separator :mutable t))

(defun conn-dispatch-reading-yank-to ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (let ((str (read-from-kill-ring "Yank To: ")))
    (oclosure-lambda (conn-dispatch-reading-yank-to
                      (action-description "Yank To")
                      (replace-and-separator (conn-dispatch-to-how-argument))
                      (str str)
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win)))))
                      (action-reference
                       "Select a string from the kill ring and insert it at the region selected
by dispatch.  By default this action inserts the string before the
region selected by dispatch but if OTHER-END is non-nil then it inserts
the string after the region selected by dispatch."))
        ()
      (pcase-let* ((`(,replace ,separator) replace-and-separator)
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
              (_ (user-error "Cannot find thing at point")))))))))

(oclosure-define (conn-dispatch-send
                  (:parent conn-change-group-action))
  (str :type string)
  (replace-and-separator :mutable t))

(defun conn-dispatch-send ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send
                      (action-description "Send")
                      (action-change-group cg)
                      (replace-and-separator (conn-dispatch-to-how-argument))
                      (str
                       (conn-read-args (conn-kill-state
                                        :prompt "Send Thing")
                           ((`(,thing ,arg)
                             (conn-thing-argument-dwim))
                            (transform (conn-transform-argument))
                            (fixup (conn-reformat-argument
                                    (not (region-active-p))))
                            (check-bounds
                             (conn-boolean-argument "check bounds"
                                                    'check-bounds
                                                    conn-check-bounds-argument-map
                                                    :value t)))
                         (save-excursion
                           (conn-kill-thing thing arg transform
                                            nil nil nil nil
                                            fixup check-bounds)
                           (current-kill 0))))
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win)))))
                      (action-reference
                       "Delete a thing at point and replace a region selected by dispatch with
it."))
        ()
      (pcase-let* ((`(,replace ,separator) replace-and-separator)
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
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-action-accept ((action conn-dispatch-send))
  (conn--action-accept-change-group
   (conn-dispatch-send--action-change-group action))
  (cl-call-next-method))

(cl-defmethod conn-action-cancel ((action conn-dispatch-send))
  (conn--action-cancel-change-group
   (conn-dispatch-send--action-change-group action)))

(oclosure-define (conn-dispatch-register-load
                  (:parent conn-action))
  (register :type integer)
  (replace :mutable t))

(defun conn-dispatch-register-load ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-register-load
                    (register (register-read-with-preview "Register: "))
                    (replace
                     (conn-boolean-argument "replace"
                                            'dispatch-replace
                                            conn-dispatch-replace-argument-map))
                    (action-reference
                     "Replace region selected by dispatch with contents of register."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (when replace
               (delete-region beg end))
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-display ((action conn-dispatch-register-load)
                                   &optional
                                   short)
  (if short "Register"
    (format "Register <%c>"
            (conn-dispatch-register-load--register action))))

(oclosure-define (conn-dispatch-copy-from
                  (:parent conn-change-group-action)
                  ( :copier
                    conn-dispatch-copy-from-copy
                    (action-opoint)))
  (action-opoint :mutable t)
  (separator :mutable t))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-copy-from))
  (not (ignore-errors
         (thread-first
           (conn-dispatch-copy-from--action-opoint action)
           marker-buffer
           buffer-live-p))))

(cl-defmethod conn-action-cleanup ((action conn-dispatch-copy-from))
  (ignore-errors
    (set-marker (conn-dispatch-copy-from--action-opoint action) nil)))

(cl-defmethod conn-action-copy ((action conn-dispatch-copy-from))
  (conn-<
    (conn-dispatch-copy-from--action-opoint action)
    (copy-marker t)
    (:> (conn-dispatch-copy-from-copy action))))

(defun conn-dispatch-copy-from ()
  "Copy a thing from somewhere else.
\\<conn-dispatch-replace-argument-map>Replace a thing at point with \\[dispatch-replace].
\\<conn-separator-argument-map>Insert a separator between multiple copies with \\[separator]."
  (declare (conn-dispatch-action)
           (important-return-value t))
  (let ((str nil)
        (init nil))
    (oclosure-lambda (conn-dispatch-copy-from
                      (separator (conn-separator-argument))
                      (action-change-group (conn-dispatch-replace-argument))
                      (action-description "Copy From")
                      (action-opoint (conn-dispatch-point-argument))
                      (action-reference
                       "Replace current region with text in region selected by dispatch."))
        ()
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target)))
        (with-selected-window window
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (conn-dispatch-action-pulse beg end)
             (setq str (filter-buffer-substring beg end)))
            (_ (user-error "Cannot find thing at point"))))
        (cl-flet ((do ()
                    (conn-dispatch-change-group)
                    (goto-char action-opoint)
                    (when separator
                      (cond ((or (null action-change-group) init)
                             (insert (conn-kill-separator-for-strings str separator)))
                            ((and action-change-group (null init))
                             (setq init t)
                             (conn-dispatch-undo-case
                               (:undo (setq init nil))))))
                    (insert-for-yank str)))
          (with-current-buffer (marker-buffer action-opoint)
            (if (= (point) action-opoint)
                (do)
              (save-excursion (do)))))))))

(cl-defmethod conn-action-cancel ((action conn-dispatch-copy-from))
  (ignore-errors
    (set-marker (conn-dispatch-copy-from--action-opoint action) nil))
  (conn--action-cancel-change-group
   (conn-dispatch-copy-from--action-change-group action)))

(cl-defmethod conn-action-accept ((action conn-dispatch-copy-from))
  (conn--action-accept-change-group
   (conn-dispatch-copy-from--action-change-group action))
  (cl-call-next-method))

(oclosure-define (conn-dispatch-take
                  (:parent conn-change-group-action)
                  ( :copier
                    conn-dispatch-take-copy
                    (action-opoint)))
  (action-opoint :mutable t)
  (separator :mutable t))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take))
  (not (ignore-errors
         (thread-first
           (conn-dispatch-take--action-opoint action)
           marker-buffer
           buffer-live-p))))

(cl-defmethod conn-action-cleanup ((action conn-dispatch-take))
  (ignore-errors
    (set-marker (conn-dispatch-take--action-opoint action) nil)))

(cl-defmethod conn-action-copy ((action conn-dispatch-take))
  (conn-<
    (conn-dispatch-take--action-opoint action)
    (copy-marker t)
    (:> (conn-dispatch-take-copy action))))

(defun conn-dispatch-take ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (let ((init nil))
    (oclosure-lambda (conn-dispatch-take
                      (separator (conn-separator-argument))
                      (action-opoint (conn-dispatch-point-argument))
                      (action-change-group (conn-dispatch-replace-argument))
                      (action-description "Take From")
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win)))))
                      (action-reference
                       "Kill the thing selected by dispatch and yank it at point."))
        ()
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target))
                   (str nil))
        (conn-dispatch-change-group (current-buffer) (window-buffer window))
        (with-selected-window window
          (save-excursion
            (goto-char pt)
            (pcase (conn-bounds-of thing arg)
              ((and bounds (conn-bounds `(,beg . ,end) transform))
               (setq str (filter-buffer-substring beg end 'delete))
               (when conn-kill-reformat-function
                 (funcall conn-kill-reformat-function bounds)))
              (_ (user-error "Cannot find thing at point")))))
        (with-current-buffer (marker-buffer action-opoint)
          (when separator
            (cond ((or (null action-change-group) init)
                   (insert (conn-kill-separator-for-strings str separator)))
                  ((and action-change-group (null init))
                   (setq init t)
                   (conn-dispatch-undo-case
                     (:undo (setq init nil))))))
          (insert-for-yank str))))))

(defun conn-dispatch-jump ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-action
                    (action-no-history t)
                    (action-description "Jump"))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
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

(oclosure-define (conn-dispatch-repeat-command
                  (:parent conn-action))
  (command :type list :mutable t))

(defun conn-dispatch-repeat-command ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (when command-history
    (oclosure-lambda
        (conn-dispatch-repeat-command
         (command
          (conn-read-argument
           "command"
           'read-previous-command
           (define-keymap "+" 'read-previous-command)
           (lambda (_cmd) (conn-read-from-command-history))
           :formatter (lambda (key-string name _val)
                        (concat
                         key-string
                         " "
                         (propertize name
                                     'face 'conn-argument-active-face)))
           :value (car conn-command-history)
           :always-read t))
         (action-window-predicate
          (lambda (win)
            (not (buffer-local-value 'buffer-read-only
                                     (window-buffer win))))))
        ()
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-mark-and-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-dispatch-bounds `(,beg . ,_end) transform)
               (goto-char beg)
               (let ((conn-repeating-command t))
                 (when (commandp (car command))
                   (setq this-command (car command)))
                 (apply #'funcall-interactively command)))
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-action-display ((_action conn-dispatch-repeat-command)
                                   &optional
                                   _short)
  "Repeat")

(oclosure-define (conn-dispatch-highlight-symbol
                  (:parent conn-action)))

(defun conn-dispatch-highlight-symbol ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-highlight-symbol)
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
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

(cl-defmethod conn-action-display ((_action conn-dispatch-highlight-symbol)
                                   &optional
                                   _short)
  "Highlight Symbol")

(defun conn-dispatch-transpose ()
  (declare (conn-dispatch-action)
           (important-return-value t))
  (oclosure-lambda (conn-action
                    (action-description "Transpose")
                    (action-window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win)))))
                    (action-reference
                     "Transpose two things selected by dispatch."))
      ()
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
          (setq str1 (filter-buffer-substring beg1 end1))
          (delete-region beg1 end1))
        (with-current-buffer buffer2
          (setq str2 (filter-buffer-substring beg2 end2))
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
                                     'conn--dispatch-restrict-windows
                                     conn-target-window-predicate))
                  (other-end conn-dispatch-other-end)
                  (target-finder
                   (conn-target-finder-shelve conn-dispatch-target-finder))))
               (:copier conn--copy-previous-dispatch))
  (action nil :type conn-action)
  (other-end nil :type symbol)
  (repeat nil :type boolean)
  (restrict-windows nil :type boolean)
  (target-finder nil))

(defvar conn-dispatch-ring-max 12)

(defvar conn-dispatch-ring
  (conn-make-ring conn-dispatch-ring-max
                  :cleanup 'conn-dispatch--cleanup))

(conn-define-dispatch-handler-command ((arg conn-dispatch-command-handler)
                                       (cmd (eql conn-repeat-last-dispatch)))
  "Cycle the dispatch ring to the next most recent dispatch."
  ( :update (_break)
    (if-let* ((prev (conn-ring-extract-head conn-dispatch-ring)))
        (if (conn-action-stale-p (conn-previous-dispatch-action prev))
            (progn
              (conn-dispatch-ring-remove-stale)
              (conn-read-args-error "Last dispatch action stale"))
          (conn-read-args-return
            (conn-dispatch-setup-previous
             prev (conn-read-args-consume-prefix-arg))))
      (conn-read-args-error "Dispatch ring empty"))))

(defun conn-previous-dispatch-copy (dispatch)
  (declare (important-return-value t))
  (let ((copy (conn--copy-previous-dispatch dispatch)))
    (setf (conn-previous-dispatch-action copy)
          (conn-action-copy (conn-previous-dispatch-action dispatch)))
    copy))

(defun conn-dispatch--cleanup (dispatch)
  (conn-action-cleanup (conn-previous-dispatch-action dispatch)))

(defun conn-describe-dispatch (dispatch)
  (declare (side-effect-free t))
  (pcase-let ((thing
               (oref (conn-previous-dispatch-target-finder dispatch)
                     thing))
              (arg
               (oref (conn-previous-dispatch-target-finder dispatch)
                     arg))
              (transform
               (oref (conn-previous-dispatch-target-finder dispatch)
                     transform)))
    (format "%s @ %s <%s%s>"
            (conn-action-display
             (conn-previous-dispatch-action dispatch))
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
  (unless (conn-action-no-history (conn-previous-dispatch-action dispatch))
    (add-to-history 'command-history `(conn-dispatch-setup-previous
                                       (conn-ring-head conn-dispatch-ring)))
    (conn-ring-insert-front conn-dispatch-ring dispatch)))

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
  (pcase-let (((cl-struct conn-previous-dispatch
                          action
                          (other-end conn-dispatch-other-end)
                          repeat
                          target-finder
                          restrict-windows)
               prev-dispatch))
    (let ((conn-dispatch-hide-labels nil)
          (conn-wincontrol-mode nil)
          (conn-wincontrol-one-command-mode nil)
          (conn-dispatch-quit-flag nil)
          (conn--dispatch-prev-state
           (list conn-target-window-predicate
                 conn-target-predicate
                 conn-target-sort-function))
          (conn-target-window-predicate conn-target-window-predicate)
          (conn-target-predicate conn-target-predicate)
          (conn-target-sort-function conn-target-sort-function)
          (conn-dispatch-label-function conn-dispatch-label-function)
          (conn-dispatch-target-finder nil)
          (conn-dispatch-repeating
           (and (xor repeat invert-repeat)
                (conn-action-repeatable-p action)))
          (conn-dispatch-iteration-count 0))
      (conn-with-dispatch-handlers
        (:handler
         (:depth -99)
         ( :display ()
           (propertize (conn-action-display action t)
                       'face 'conn-argument-active-face)))
        ( :with (conn-dispatch-prefix-arg)
          :depth -97)
        ( :with (conn-read-char-input-method)
          :depth -96)
        (conn-with-dispatch-input-buffer
          (conn--unwind-protect-all
            (let ((conn-dispatch-in-progress t))
              (let ((im (or current-input-method
                            conn--input-method)))
                (with-current-buffer conn-dispatch-input-buffer
                  (activate-input-method im)))
              (conn-target-finder-setup target-finder)
              (when-let* ((predicate (conn-action-window-predicate action)))
                (add-function :after-while conn-target-window-predicate predicate))
              (when-let* ((predicate (conn-action-target-predicate action)))
                (add-function :after-while conn-target-predicate predicate))
              (when restrict-windows
                (add-function :after-while conn-target-window-predicate
                              'conn--dispatch-restrict-windows))
              (conn-dispatch-perform-action action)
              (setf (conn-previous-dispatch-repeat prev-dispatch)
                    conn-dispatch-repeating)
              (setf (conn-previous-dispatch-other-end prev-dispatch)
                    conn-dispatch-other-end)
              (setf (conn-previous-dispatch-target-finder prev-dispatch)
                    conn-dispatch-target-finder)
              (setf (conn-previous-dispatch-restrict-windows prev-dispatch)
                    (advice-function-member-p
                     'conn--dispatch-restrict-windows
                     conn-target-window-predicate))
              (conn-dispatch-push-history prev-dispatch))
            (conn-clear-targets)))))))

;;;;; Dispatch Commands

(cl-defun conn-dispatch-setup (action
                               thing
                               arg
                               transform
                               &key
                               repeat
                               restrict-windows
                               other-end)
  (when (null action)
    (setq action (conn-make-default-action thing)))
  (conn-protected-let*
      ((action action (conn-action-cancel action))
       (conn-dispatch-hide-labels nil)
       (conn-wincontrol-mode nil)
       (conn-wincontrol-one-command-mode nil)
       (conn-dispatch-quit-flag nil)
       (conn--dispatch-prev-state
        (list conn-target-window-predicate
              conn-target-predicate
              conn-target-sort-function))
       (conn-target-window-predicate conn-target-window-predicate)
       (conn-target-predicate conn-target-predicate)
       (conn-target-sort-function conn-target-sort-function)
       (conn-dispatch-label-function conn-dispatch-label-function)
       (conn-dispatch-other-end (pcase other-end
                                  (:no-other-end (lambda (&rest _) 0))
                                  ('t #'-)
                                  ('nil #'+)))
       (conn-dispatch-repeating
        (and repeat (conn-action-repeatable-p action)))
       (conn-dispatch-iteration-count 0)
       (conn-dispatch-target-finder nil))
    (conn-with-dispatch-handlers
      (:handler
       (:depth -99)
       ( :display ()
         (propertize (conn-action-display action t)
                     'face 'conn-argument-active-face)))
      ( :with (conn-dispatch-prefix-arg)
        :depth -97)
      ( :with (conn-read-char-input-method)
        :depth -96)
      (conn-with-dispatch-input-buffer
        (conn--unwind-protect-all
          (let ((conn-dispatch-in-progress t))
            (let ((im (or current-input-method
                          conn--input-method)))
              (with-current-buffer conn-dispatch-input-buffer
                (activate-input-method im)))
            (conn-target-finder-setup
             (conn-get-target-finder thing arg transform))
            (when-let* ((predicate (conn-action-window-predicate action)))
              (add-function :after-while conn-target-window-predicate predicate))
            (when-let* ((predicate (conn-action-target-predicate action)))
              (add-function :after-while conn-target-predicate predicate))
            (when restrict-windows
              (add-function :after-while conn-target-window-predicate
                            'conn--dispatch-restrict-windows))
            (conn-dispatch-perform-action action)
            (unless (conn-action-no-history action)
              (let ((prev (conn-make-dispatch action)))
                (conn-dispatch-push-history prev)
                (conn-push-command-history 'conn-dispatch-setup-previous prev))))
          (conn-clear-targets))))))

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
                   :prefix current-prefix-arg
                   :prompt "Dispatch"
                   :reference (conn-dispatch-reference)
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
                     :prefix current-prefix-arg
                     :command-handler (conn-dispatch-command-handler)
                     :prompt "Dispatch"
                     :reference (conn-dispatch-reference)
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
                                 conn-other-end-argument-map))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map)))
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
   nil nil))

(conn-define-target-finder conn-isearch-targets
    (conn-dispatch-focus-mixin)
    ()
  ( :update-method (state)
    (conn-focus-targets-remove-overlays state)
    (conn-dispatch-call-update-handlers state))
  ( :default-update-handler (state)
    (let* ((matches nil)
           (thing (conn-anonymous-thing
                    '(region)
                    :bounds-op ( :method (_self arg)
                                 (when-let* ((bd (assq (point) matches)))
                                   (conn-make-bounds 'region arg bd)))))
           (screen-lines (ceiling (* 2 (window-screen-lines)))))
      (cl-flet ((collect ()
                  (cl-loop
                   with bound = (if isearch-forward
                                    (point-max)
                                  (point-min))
                   with case-fold-search = isearch-case-fold-search
                   with count = 1
                   with line = (if isearch-forward (pos-eol) (pos-bol))
                   while (and (< count screen-lines)
                              (isearch-search-string isearch-string bound t))
                   when (funcall isearch-filter-predicate
                                 (match-beginning 0)
                                 (match-end 0))
                   do (progn
                        (cond ((and isearch-forward
                                    (> (point) line))
                               (setq line (pos-eol))
                               (cl-incf count))
                              ((and (not isearch-forward)
                                    (< (point) line))
                               (setq line (pos-bol))
                               (cl-incf count)))
                        (push (cons (match-beginning 0) (match-end 0))
                              matches)
                        (conn-make-target-overlay
                         (match-beginning 0)
                         (- (match-end 0) (match-beginning 0))
                         :thing thing))
                   when (and (= (match-beginning 0) (match-end 0))
                             (not (if isearch-forward (eobp) (bobp))))
                   do (forward-char (if isearch-forward 1 -1)))))
        (save-excursion
          (let ((isearch-forward t))
            (collect)))
        (save-excursion
          (let (isearch-forward)
            (collect)))))))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (unwind-protect
      (let ((regexp-search-ring
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
                              :window-predicate (let ((owin (selected-window)))
                                                  (lambda (win) (eq win owin)))
                              :context-lines (ceiling (window-screen-lines) 2.5))))
          nil nil
          :repeat nil
          :restrict-windows t
          :other-end nil)
         (setq opoint (point)))
        (goto-char opoint))
    (save-mark-and-excursion
      (isearch-exit))))

(defun conn-dispatch-isearch-with-action ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let (ovs action repeat other-end)
    (unwind-protect
        (progn
          (with-restriction (window-start) (window-end)
            (cl-loop for (beg . end) in (conn--isearch-matches)
                     do (let ((ov (make-overlay beg end)))
                          (push ov ovs)
                          (overlay-put ov 'face 'lazy-highlight))))
          (let ((regexp-search-ring
                 (if isearch-regexp
                     (cons isearch-string regexp-search-ring)
                   regexp-search-ring))
                (search-ring
                 (if isearch-regexp
                     search-ring
                   (cons isearch-string search-ring))))
            (with-isearch-suspended
             (conn-read-args (conn-dispatch-state
                              :prompt "Dispatch on Isearch")
                 ((`(,act ,rep) (conn-dispatch-action-argument t))
                  (oe (conn-boolean-argument "other-end"
                                             'other-end
                                             conn-other-end-argument-map)))
               (setq action act
                     repeat rep
                     other-end oe)))))
      (mapc #'delete-overlay ovs))
    (unwind-protect
        (let ((regexp-search-ring
               (if isearch-regexp
                   (cons isearch-string regexp-search-ring)
                 regexp-search-ring))
              (search-ring
               (if isearch-regexp
                   search-ring
                 (cons isearch-string search-ring)))
              (opoint nil))
          (with-isearch-suspended
           (save-selected-window
             (conn-dispatch-setup
              action
              (conn-anonymous-thing
                '(region)
                :target-finder ( :method (_self &rest _)
                                 (conn-isearch-targets
                                  :window-predicate (let ((owin (selected-window)))
                                                      (lambda (win) (eq win owin)))
                                  :context-lines (floor (window-screen-lines) 2.5))))
              nil nil
              :repeat repeat
              :restrict-windows t
              :other-end other-end))
           (setq opoint (point)))
          (when opoint (goto-char opoint)))
      (save-mark-and-excursion
        (isearch-exit)))))

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
                   :prompt "Bounds of Dispatch"
                   :reference (list conn-dispatch-command-reference
                                    conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-dispatch-thing-argument t))
       (transform (conn-dispatch-transform-argument))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               :value subregions-p)))
    (let (ovs subregions)
      (unwind-protect
          (progn
            (conn-dispatch-setup
             (oclosure-lambda (conn-action
                               (action-no-history t)
                               (action-description "Bounds")
                               (action-window-predicate
                                (let ((win (selected-window)))
                                  (lambda (window) (eq win window)))))
                 ()
               (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
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
                      (user-error "No %s found at point" thing))))))
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
             (setq ov (make-overlay beg end))
             (overlay-put ov 'face 'region)
             (conn-dispatch-setup
              (oclosure-lambda (conn-action
                                (action-no-history t)
                                (action-description "Bounds")
                                (action-window-predicate
                                 (let ((win (selected-window)))
                                   (lambda (window) (eq win window))))
                                (action-reference
                                 "Bounds between the previous region and this region."))
                  ()
                (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                              (conn-select-target)))
                  (with-selected-window window
                    (pcase (conn-bounds-of-dispatch thing arg pt)
                      ((conn-dispatch-bounds `(,beg . ,end) transform)
                       (setq obeg beg
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
  (conn-dispatch-all-defuns
   :reference (conn-reference-quote
                ((:heading "Defun Targets")
                 "Dispatch on defuns.  Hides buffer regions outside defun definition
lines."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing visual-line))
                                      &rest _)
  (conn-dispatch-visual-line-targets
   :reference (conn-reference-quote
                ((:heading "Visual Lines Targets")
                 "Dispatch on visual lines."))))

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
  (conn-all-things-targets
   :all-things 'sentence
   :reference (conn-reference-quote
                ((:heading "Sentence Targets")
                 "Dispatch on sentences."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing paragraph))
                                      &rest _)
  (conn-all-things-targets
   :all-things 'paragraph
   :reference (conn-reference-quote
                ((:heading "Paragraph Targets")
                 "Dispatch on paragraphs."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing char))
                                      &rest _)
  (conn-dispatch-read-n-chars :string-length 2
                              :thing 'forward-char))

(cl-defmethod conn-get-target-finder ((_cmd (eql forward-char))
                                      &rest _)
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :reference (conn-reference-quote
                ((:heading "String Targets")
                 "Display on matches for a string read with a timeout.  Other end puts
point at the end of a match."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line))
                                      &rest _)
  (conn-dispatch-line-targets
   :reference (conn-reference-quote
                ((:heading "Line Targets")
                 "Dispatch on a line."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column))
                                      &rest _)
  (conn-dispatch-column-targets
   :reference (conn-reference-quote
                ((:heading "Column Targets")
                 "Dispatch on a column.  Bounds are from point to selected column."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing outer-line))
                                      &rest _)
  (conn-dispatch-line-targets
   :reference (conn-reference-quote
                ((:heading "Outer Line Targets")
                 "Dispatch on an outer line."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing inner-line))
                                      &rest _)
  (conn-dispatch-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an inner line."))))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line))
                                      &rest _)
  (conn-dispatch-end-of-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an outer line.  Other end defaults to non-nil."))))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line-dwim))
                                      &rest _)
  (conn-dispatch-end-of-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an outer line.  Other end defaults to non-nil."))))

(provide 'conn-dispatch)
