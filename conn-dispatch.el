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
(eval-when-compile
  (require 'cl-lib))

(defvar outline-heading-end-regexp)
(defvar treesit-defun-type-regexp)

(declare-function face-remap-remove-relative "face-remap")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")
(declare-function conn-scroll-up "conn-commands")
(declare-function conn-scroll-down "conn-commands")
(declare-function conn-register-load "conn-commands")
(declare-function conn-end-of-inner-line "conn-commands")
(declare-function conn-beginning-of-inner-line "conn-commands")
(declare-function conn-kill-thing "conn-commands")

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
  '((t (:inherit default :foreground "black" :background "#ff8bd1" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-alt-face
  '((t (:inherit default :foreground "black" :background "#ffc5e8" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-multi-face
  '((t (:inherit default :foreground "black" :background "#8bd6ff" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defface conn-dispatch-label-multi-alt-face
  '((t (:inherit default :foreground "black" :background "#c5ebff" :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defvar conn-dispatch-action-pulse-color nil)

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
  (let* ((blen (ceiling (log count (length conn-simple-label-characters))))
         (buckets (make-vector blen nil))
         (i 0))
    (setf (aref buckets i) (thread-last
                             (take count conn-simple-label-characters)
                             (copy-sequence)
                             (mapcar #'copy-sequence)))
    (cl-loop
     (cl-callf nreverse (aref buckets i))
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

(defvar conn--dispatch-event-handler-maps nil)
(defvar conn--dispatch-read-char-handlers nil)
(defvar conn--dispatch-read-char-message-prefixes nil)

(eval-and-compile
  (defun conn--with-dispatch-event-handlers (body)
    (let* ((tag (gensym "tag"))
           (return-expander
            `(:return . ,(lambda (&optional result)
                           `(throw ',tag ,result))))
           (handler-expander
            `(:handler . ,(lambda (&rest rest)
                            `(push
                              ,(pcase rest
                                 (`(#',fn) `#',fn)
                                 (_ (macroexpand-all
                                     `(lambda ,@rest)
                                     (cons return-expander
                                           macroexpand-all-environment))))
                              conn--dispatch-read-char-handlers))))
           (msg-expander
            `(:message . ,(lambda (depth &rest rest)
                            (cl-assert (<= -100 depth 100))
                            `(progn
                               (push
                                (cons ,depth ,(pcase rest
                                                (`(#',fn) `#',fn)
                                                (_ `(lambda ,@rest))))
                                conn--dispatch-read-char-message-prefixes)
                               (conn--compat-callf sort
                                   conn--dispatch-read-char-message-prefixes
                                 :key #'car)))))
           (keymap-expander
            `(:keymap . ,(lambda (keymap)
                           `(push ,keymap conn--dispatch-event-handler-maps))))
           (body (macroexpand-all (macroexp-progn body)
                                  `(,handler-expander
                                    ,msg-expander
                                    ,keymap-expander
                                    ,@macroexpand-all-environment))))
      `(let ((conn--dispatch-event-handler-maps
              conn--dispatch-event-handler-maps)
             (conn--dispatch-read-char-message-prefixes
              conn--dispatch-read-char-message-prefixes)
             (conn--dispatch-read-char-handlers
              conn--dispatch-read-char-handlers))
         (catch ',tag ,@(macroexp-unprogn body))))))

(defmacro conn-with-dispatch-event-handlers (&rest body)
  (declare (indent 0))
  (conn--with-dispatch-event-handlers body))

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

(cl-defgeneric conn-label-partial-p (label))

(cl-defgeneric conn-label-redisplay (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

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

(cl-defmethod conn-label-partial-p ((label conn-window-label))
  (not (or (string-empty-p (window-parameter (conn-window-label-window label)
                                             'conn-label-string))
           (eq (window-parameter (conn-window-label-window label)
                                 'conn-label-string)
               (conn-window-label-string label)))))

(defvar conn-label-select-always-prompt nil)

(defvar conn-dispatch-hide-labels nil)

(defun conn-label-select (candidates
                          char-reader
                          &optional
                          prompt
                          always-prompt)
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
         (prompt-suffix "")
         (prompt-flag (or conn-label-select-always-prompt
                          always-prompt))
         (current candidates))
    (while-no-input
      (mapc #'conn-label-redisplay candidates))
    (cl-loop
     (pcase current
       ('nil
        (setq current candidates
              prompt-suffix ""
              prompt-flag (or conn-label-select-always-prompt
                              always-prompt))
        (conn-read-args-message "No matches")
        (mapc #'conn-label-reset current)
        (while-no-input
          (mapc #'conn-label-redisplay candidates)))
       ((and `(,it . nil)
             (guard (not (or prompt-flag
                             (conn-label-partial-p it)))))
        (cl-return (conn-label-payload it))))
     (while-no-input
       (mapc #'conn-label-redisplay candidates))
     (setq prompt-flag nil)
     (let ((next nil)
           (c (funcall char-reader prompt)))
       (setq prompt-suffix (concat prompt-suffix (string c))
             current (dolist (label current next)
                       (when-let* ((l (conn-label-narrow label c)))
                         (push l next))))))))

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

(defvar conn--window-label-pool
  (mapcar (lambda (str)
            (propertize str 'face 'conn-window-label-face))
          (conn-simple-labels 30)))

(defun conn--simple-window-labels ()
  (setq conn-dispatch-label-input-method conn-simple-label-input-method)
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
(defun conn--get-windows (&optional window
                                    minibuffer
                                    all-frames
                                    dedicated
                                    predicate)
  (declare (important-return-value t))
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           unless (or ;; ignore child frames
                   (and (fboundp 'frame-parent)
                        (frame-parent (window-frame window)))
                   ;; When `ignore-window-parameters' is nil, ignore
                   ;; windows whose `no-other-window’ or
                   ;; `no-delete-other-windows' parameter is non-nil.
                   (unless ignore-window-parameters
                     (window-parameter window 'no-other-window))
                   (and (null dedicated) (window-dedicated-p win))
                   (and predicate (not (funcall predicate win))))
           collect win))

(defmacro conn-with-window-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,val)
     `(let ((,var ,val))
        (unwind-protect
            ,(macroexp-progn body)
          (mapc #'conn-label-delete ,var))))
    (_ (error "Unexpected binding form %s" binder))))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (declare (important-return-value t))
  (let (conn-label-select-always-prompt)
    (cond
     ((null windows) nil)
     (t (conn-with-window-labels
            (labels (funcall conn-window-label-function windows))
          (conn-with-dispatch-event-handlers
            ( :handler (cmd)
              (when (or (and (eq cmd 'act)
                             (mouse-event-p last-input-event))
                        (eq 'dispatch-mouse-repeat
                            (event-basic-type last-input-event)))
                (let* ((posn (event-start last-input-event))
                       (win (posn-window posn)))
                  (when (not (posn-area posn))
                    (:return win)))))
            (conn-label-select
             labels
             (lambda (prompt)
               (conn-dispatch-read-char prompt 'label))
             nil
             always-prompt)))))))

;;;; Dispatch State

(defface conn-dispatch-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a dispatch state."
  :group 'conn-faces)

(defvar conn-dispatch-target-finder nil)

(defvar conn-dispatch-ring)

(defvar conn--dispatch-must-prompt nil)
(defvar conn--dispatch-action-always-prompt nil)
(defvar conn--dispatch-always-retarget nil)

(defvar conn--dispatch-remap-cookies nil)

(defvar conn-dispatch-repeating nil)
(defvar conn-dispatch-iteration-count nil)

(defvar conn-dispatch-other-end nil)
(defvar conn-dispatch-no-other-end nil)

(defvar dispatch-quit-flag nil)

(defvar conn-dispatch-read-char-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map)
                          (cons #x100 (max-char))
                          'dispatch-character-event)
    (cl-loop for i from ?\s below 256
             do (define-key map (vector i) 'dispatch-character-event))
    (define-keymap
      :keymap map
      "C-\\" 'toggle-input-method
      "C-M-\\" 'set-input-method
      "C-z" 'other-end
      "DEL" 'restart
      "<backspace>" 'restart
      "C-r" 'recursive-edit
      "<mouse-1>" 'act
      "<mouse-3>" 'undo
      "C-/" 'undo
      "M-DEL" 'reset-arg
      "M-<backspace>" 'reset-arg
      "C-t" 'change-target-finder
      "<escape>" 'finish
      "C-o" 'conn-goto-window
      "C-s" 'isearch-forward
      "C-M-s" 'isearch-regexp-forward
      "C-v" 'scroll-up-command
      "M-v" 'scroll-down-command
      "C-q" 'quoted-insert
      "?" 'help
      "C-g" 'keyboard-quit
      "C-w" 'restrict-windows
      "RET" 'ignore
      "<return>" 'ignore
      "M-TAB" 'repeat-dispatch)))

(defvar-keymap conn-toggle-label-argument-map
  "SPC" 'toggle-labels)

(conn-define-state conn-dispatch-targets-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(cl-defmethod conn-enter-state :around ((_state (conn-substate conn-dispatch-targets-state))
                                        &optional _type)
  (if (or defining-kbd-macro executing-kbd-macro)
      (error "Dispatch not available in keyboard macros")
    (cl-call-next-method)))

(conn-define-state conn-dispatch-bounds-state (conn-dispatch-targets-state)
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(conn-define-state conn-dispatch-state (conn-dispatch-targets-state)
  "State for reading a dispatch command.")

(conn-define-state conn-dispatch-thingatpt-state (conn-dispatch-state))

(defvar-keymap conn-dispatch-transform-argument-map
  "a" 'conn-dispatch-bounds-anchored
  "b" 'conn-dispatch-bounds-between
  "x" 'conn-bounds-trim
  "c" 'conn-dispatch-bounds-over
  "X" 'conn-transform-reset)

(defun conn-dispatch-transform-argument (&optional value)
  (conn-transform-argument value :keymap conn-dispatch-transform-argument-map))

(defvar conn--dispatch-thing-predicate nil)

(cl-defstruct (conn-dispatch-target-argument
               (:include conn-thing-argument)
               (:constructor conn-dispatch-target-argument
                             (&aux
                              (required t)
                              (recursive-edit t)))))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-target-argument)
                                       sym)
  (and (cl-call-next-method)
       (funcall conn--dispatch-thing-predicate sym)))

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
       (let ((s (conn-with-input-method
                  (read-string "Separator (RET for default): " nil
                               'conn-separator-history nil t))))
         (if (equal s "") 'default s))))
   :value initial-value))

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
    (("copy from/replace"
      conn-dispatch-copy-from
      conn-dispatch-copy-from-replace)
     ("send/replace"
      conn-dispatch-send
      conn-dispatch-send-replace)
     ("kapply" conn-dispatch-kapply)
     ("yank to/replace"
      conn-dispatch-yank-to
      conn-dispatch-yank-to-replace)
     ("yank read/replace"
      conn-dispatch-reading-yank-to
      conn-dispatch-reading-yank-to-replace)
     ("copy to/replace"
      conn-dispatch-copy-to
      conn-dispatch-copy-to-replace)
     ("transpose" conn-dispatch-transpose)
     ("register load/replace"
      conn-dispatch-register-load
      conn-dispatch-register-load-replace)
     ("take/replace"
      conn-dispatch-grab
      conn-dispatch-replace))))

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
  (curr nil))

(defun conn-dispatch-action-argument ()
  (setq conn--dispatch-thing-predicate #'always)
  (make-conn-dispatch-action-argument
   :keymap conn-dispatch-repeat-argument-map))

(cl-defmethod conn-argument-get-reference ((arg conn-dispatch-action-argument))
  (let* ((action (conn-dispatch-action-argument-value arg))
         (ref (conn-action-get-reference action)))
    (conn-reference-page
      :depth -50
      (:splice ref)
      (:heading (when ref "Action Bindings"))
      ((:splice (conn-quick-ref-to-cols
                 conn-dispatch-action-ref-list 3))))))

(cl-defmethod conn-argument-update ((arg conn-dispatch-action-argument)
                                    cmd updater)
  (cl-symbol-macrolet ((curr (conn-dispatch-action-argument-curr arg)))
    (pcase cmd
      ('repeat-dispatch
       (cl-callf not (conn-dispatch-action-argument-repeat arg))
       (funcall updater arg))
      ((guard (conn-argument-predicate arg cmd))
       (conn-cancel-action (conn-argument-value arg))
       (condition-case err
           (if-let* ((_(not (eq curr cmd)))
                     (action (or (atomic-change-group
                                   (save-window-excursion
                                     (funcall cmd)))
                                 (error "Failed to construct %s" cmd))))
               (progn
                 (setf curr cmd)
                 (setf conn--dispatch-thing-predicate
                       (or (conn-action--action-thing-predicate action)
                           #'always))
                 (setf (conn-argument-value arg) action)
                 (setf (conn-dispatch-action-argument-repeat arg)
                       (pcase (conn-dispatch-action-argument-repeat arg)
                         ((or 'auto 'nil)
                          (and (conn-action--action-auto-repeat action)
                               'auto))
                         (_ (conn-dispatch-action-argument-repeat arg)))))
             (setf conn--dispatch-thing-predicate #'always
                   (conn-argument-value arg) nil
                   curr nil))
         (error
          (conn-read-args-error (error-message-string err))
          (setf (conn-argument-value arg) nil)))
       (funcall updater arg)))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-action-argument))
  (conn-cancel-action (conn-argument-value arg)))

(cl-defmethod conn-argument-extract-value ((arg conn-dispatch-action-argument))
  (list (conn-argument-value arg)
        (conn-dispatch-action-argument-repeat arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-action-argument)
                                       sym)
  (function-get sym :conn-dispatch-action))

(cl-defmethod conn-argument-completion-annotation ((_arg conn-dispatch-action-argument)
                                                   sym)
  (when (function-get sym :conn-dispatch-action)
    " (action)"))

(cl-defmethod conn-argument-display ((arg conn-dispatch-action-argument))
  (list
   (concat (substitute-command-keys "\\[repeat-dispatch] ")
           (propertize
            "repeat"
            'face (when (conn-dispatch-action-argument-repeat arg)
                    'eldoc-highlight-function-argument)))
   (when-let* ((action (conn-argument-value arg)))
     (propertize (conn-action-pretty-print action)
                 'face 'eldoc-highlight-function-argument))))

;;;;;; Command Handler

(cl-defgeneric conn-dispatch-command-handler (cmd)
  ( :method (_)))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-cycle-ring-next)))
  (condition-case err
      (progn
        (conn-dispatch-cycle-ring-next)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch
                                        (conn-ring-head conn-dispatch-ring))))
        (conn-read-args-handle))
    (user-error
     (conn-read-args-error (error-message-string err)))))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-cycle-ring-previous)))
  (condition-case err
      (progn
        (conn-dispatch-cycle-ring-previous)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch
                                        (conn-ring-head conn-dispatch-ring))))
        (conn-read-args-handle))
    (user-error
     (conn-read-args-error (error-message-string err)))))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-ring-describe-head)))
  (condition-case err
      (progn
        (conn-dispatch-ring-remove-stale)
        (if-let* ((head (conn-ring-head conn-dispatch-ring)))
            (progn
              (if (bound-and-true-p conn-posframe-mode)
                  (conn-posframe--dispatch-ring-display-subr)
                (conn-read-args-message "%s" (conn-describe-dispatch head)))
              (conn-read-args-handle))
          (conn-read-args-error "Dispatch ring empty")))
    (user-error
     (conn-read-args-error (error-message-string err)))))

;;;;; Bounds of Dispatch

(defun conn-bounds-of-dispatch (thing arg location)
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
nearest to point.  Can only be used during `conn-dispatch'.")))

(cl-defmethod conn-dispatch-bounds-anchored (bounds)
  (pcase bounds
    ((and (conn-bounds-get :origin nil
                           (and origin (pred identity)))
          (conn-bounds `(,beg . ,end)))
     (conn-make-transformed-bounds
      'conn-dispatch-bounds-anchored
      bounds
      (if (< beg origin)
          (if conn-dispatch-other-end
              (cons (min origin end)
                    (max origin end))
            (cons beg origin))
        (if conn-dispatch-other-end
            (cons origin beg)
          (cons origin end)))))
    (_ bounds)))

(cl-defmethod conn-dispatch-bounds-anchored ((bounds (conn-thing point)))
  bounds)

(cl-defmethod conn-dispatch-bounds-anchored ((bounds (conn-thing char)))
  bounds)

(cl-defgeneric conn-dispatch-bounds-between (bounds)
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-between)
           (conn-bounds-transformation
            "between"
            "Dispatch on a second thing and transform bounds to be the largest region
created from the bounds of the two things.  The new beg and end are
taken to be the points where point would be after dispatching on each
thing.  Can only be used during `conn-dispatch'.")))

(cl-defgeneric conn-dispatch-bounds (bounds &optional transforms))

(cl-defmethod conn-dispatch-bounds (bounds &optional transforms)
  (pcase (conn-transform-bounds bounds transforms)
    ((and (guard conn-dispatch-other-end)
          (conn-bounds `(,beg . ,end)))
     (cons end beg))
    ((conn-bounds bd) bd)))

(cl-defmethod conn-dispatch-bounds ((bounds (conn-thing point))
                                    &optional transforms)
  (conn-bounds
   (conn-transform-bounds
    (pcase bounds
      ((and (conn-bounds-get :origin nil
                             (and origin (pred identity)))
            (conn-bounds `(,beg . ,end)))
       (conn-make-bounds
        'point (conn-bounds-arg bounds)
        (cons (if conn-dispatch-other-end end beg) origin)))
      (_ bounds))
    transforms)))

(cl-defmethod conn-dispatch-bounds ((bounds (conn-thing char))
                                    &optional transforms)
  (conn-bounds
   (conn-transform-bounds
    (pcase bounds
      ((and (conn-bounds-get :origin nil
                             (and origin (pred identity)))
            (conn-bounds `(,beg . ,end)))
       (conn-make-bounds
        'point (conn-bounds-arg bounds)
        (cons (if conn-dispatch-other-end end beg) origin)))
      (_ bounds))
    transforms)))

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

(defun conn-dispatch-ignored-mode (win)
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
  "Alist of (WINODW . TARGET-COUNT).")

(defvar conn-target-sort-function 'conn-target-nearest-op
  "Sort function for targets in each window.

Labels are sorted first by length and then lexicographically by the
position of the characters in `conn-simple-label-characters'.  Then the
target that is sorted into first position by `conn-target-sort-function'
will received the first label, the second the second label and so one.
Thus a target sorted before another target has higher priority for a
shorter label.

A sort function should take two targets as arguments and return non-nil
if the first should be sorted before the second.")

(defvar conn-target-window-predicate 'conn-dispatch-ignored-mode
  "Predicate which windows must satisfy in order to be considered during
dispatch.

Each function should take a window and return nil if the window should
be ignored by during dispatch.")

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

(put 'conn-target-overlay 'conn-overlay t)
(put 'conn-target-overlay 'priority 2002)

(cl-defun conn-make-target-overlay (pt
                                    length
                                    &key
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
      (overlay-put ov 'conn-overlay t)
      (overlay-put ov 'category 'conn-target-overlay)
      (overlay-put ov 'face 'conn-target-overlay-face)
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

(defvar conn-pixelwise-label-target-limit 500
  "Maximum number of targets in a window for pixelwise labeling.")

(defvar conn-pixelwise-labels-window-predicate
  'conn--pixelwise-labels-window-p)

(defvar conn-dispatch-pixelwise-labels-line-limit 750
  "Maximum position in a line for pixelwise labeling.")

(defvar conn-pixelwise-labels-target-predicate
  'conn--pixelwise-labels-target-p)

(defun conn--pixelwise-labels-window-p (win)
  (declare (important-return-value t))
  (and (eq (selected-frame) (window-frame win))
       (< (alist-get win conn-target-count)
          conn-pixelwise-label-target-limit)))

(defun conn--pixelwise-labels-target-p (target)
  (declare (important-return-value t))
  (cl-loop with ov-beg = (overlay-start target)
           for (beg . end) in (conn--dispatch-window-lines
                               (overlay-get target 'window))
           when (and (<= beg ov-beg)
                     (< ov-beg end))
           return (< (- ov-beg beg)
                     conn-dispatch-pixelwise-labels-line-limit)))

(put 'conn-label-overlay 'priority 3000)
(put 'conn-label-overlay 'conn-overlay t)

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

(defun conn--dispatch-eol (pt window)
  (declare (important-return-value t))
  (cl-loop for (beg . end) in (conn--dispatch-window-lines window)
           when (and (<= beg pt) (< pt end))
           return (1- end)))

(defun conn--dispatch-setup-label-pixelwise (label)
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
               (display-width
                (conn--string-pixel-width full-string (window-buffer window)))
               (padding-width 0)
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
          (unless (= (overlay-start overlay) (point-max))
            (let* ((win (overlay-get target 'window))
                   (beg (overlay-end target))
                   (beg-width nil)
                   (end nil)
                   (line-end
                    (or (conn--dispatch-eol beg win)
                        (save-excursion
                          (goto-char beg)
                          (pos-eol))))
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
                 ((and (/= beg pt)
                       (conn--overlays-in-of-type pt (1+ pt)
                                                  'conn-target-overlay
                                                  window))
                  (setq end pt))
                 ((and (get-char-property pt 'after-string)
                       (= pt (next-single-char-property-change
                              (1- pt) 'after-string nil (1+ pt))))
                  (setq end (1+ pt)))
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
            (overlay-put overlay 'before-string full-string))
           ((overlay-get overlay 'after-string)
            (overlay-put overlay 'display full-string))
           (t
            (overlay-put overlay 'display full-string)
            (if padding-function
                (funcall padding-function
                         overlay
                         padding-width
                         (overlay-get target 'label-face))
              (funcall conn-default-label-padding-function
                       overlay
                       padding-width
                       (overlay-get target 'label-face))))))
      (when ov (delete-overlay ov)))))

(defun conn--dispatch-setup-label-charwise (label)
  (pcase-let* (((cl-struct conn-dispatch-label
                           prefix
                           suffix
                           (narrowed-string string)
                           overlay
                           target)
                label)
               (full-string (concat prefix string suffix)))
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
                (= (- pt beg) (length full-string)))
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
        (overlay-put overlay 'before-string full-string)
      (overlay-put overlay 'display full-string))))

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

(defun conn--dispatch-window-lines (window)
  (declare (important-return-value t))
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

(defconst conn--pixelwise-window-cache (make-hash-table :test 'eq))

(defun conn-dispatch-pixelwise-label-p (ov)
  (declare (important-return-value t))
  (and (with-memoization
           (gethash (overlay-get ov 'window) conn--pixelwise-window-cache)
         (funcall conn-pixelwise-labels-window-predicate
                  (overlay-get ov 'window)))
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
      (make-conn-dispatch-label
       :setup-function (cond ((overlay-get target 'no-hide)
                              'conn-before-string-label)
                             ((conn-dispatch-pixelwise-label-p ov)
                              'conn--dispatch-setup-label-pixelwise)
                             (t
                              'conn--dispatch-setup-label-charwise))
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
      (dolist (tar (if (eq win (selected-window))
                       (compat-call sort targets
                                    :lessp conn-target-sort-function)
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

(defun conn--dispatch-read-char-prefix (keymap)
  (declare (important-return-value t))
  (and-let* ((prefix
              (flatten-tree
               (cl-loop for (_ . pfx) in conn--dispatch-read-char-message-prefixes
                        for str = (pcase pfx
                                    ((pred functionp) (funcall pfx keymap))
                                    ((pred stringp) pfx))
                        if str collect str))))
    (concat " (" (string-join prefix "; ") ")")))

(defun conn-dispatch-prompt-p ()
  (or conn--dispatch-must-prompt
      conn--dispatch-action-always-prompt
      (> conn-dispatch-iteration-count 0)
      (conn-target-finder-prompt-p conn-dispatch-target-finder)))

(defvar conn--previous-labels-cleanup nil)

(defun conn-cleanup-labels ()
  (when conn--previous-labels-cleanup
    (funcall conn--previous-labels-cleanup)))

(defun conn-dispatch-get-labels ()
  (pcase (if conn--dispatch-label-state
             (funcall conn-dispatch-label-function
                      conn--dispatch-label-state)
           (funcall conn-dispatch-label-function))
    (`(:state ,state . ,labels)
     (setq conn--dispatch-label-state state)
     labels)
    (labels labels)))

(defmacro conn-with-dispatch-labels (binder &rest body)
  (declare (indent 1))
  (pcase binder
    (`(,var ,val)
     `(progn
        (conn-cleanup-labels)
        (let ((,var ,val)
              (conn-dispatch-hide-labels nil))
          (unwind-protect
              (conn-with-dispatch-event-handlers
                ( :handler (cmd)
                  (when (eq cmd 'toggle-labels)
                    (cl-callf not conn-dispatch-hide-labels)
                    (while-no-input
                      (mapc #'conn-label-redisplay ,var))
                    (conn-dispatch-handle)))
                ( :message -50 (keymap)
                  (when-let* ((_ conn-dispatch-hide-labels)
                              (binding
                               (where-is-internal 'toggle-labels keymap t)))
                    (concat
                     (propertize (key-description binding)
                                 'face 'help-key-binding)
                     " "
                     (propertize
                      "hide labels"
                      'face 'eldoc-highlight-function-argument))))
                (:keymap conn-toggle-label-argument-map)
                (let ((fn (make-symbol "cleanup")))
                  (fset fn (lambda (&rest _)
                             (unwind-protect
                                 (mapc #'conn-label-delete ,(car binder))
                               (setq conn--previous-labels-cleanup nil)
                               (remove-hook 'pre-redisplay-functions fn))))
                  (setq conn--previous-labels-cleanup fn))
                ,@body)
            (clrhash conn--pixelwise-window-cache)
            (clrhash conn--dispatch-window-lines-cache)
            (when conn--previous-labels-cleanup
              (add-hook 'pre-redisplay-functions
                        conn--previous-labels-cleanup))))))
    (_ (error "Unexpected binder form"))))

(cl-defgeneric conn-target-finder-select (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-target-finder-select :before (target-finder)
  (conn-cleanup-labels)
  (when conn--dispatch-always-retarget
    (conn-target-finder-retarget target-finder))
  (let ((old nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(,_ . ,targets) conn-targets)
            (dolist (target targets)
              (overlay-put target 'category 'conn-old-target)
              (push target old)))
          (setq conn-targets nil
                conn-target-count nil)
          (conn-target-finder-update target-finder)
          (pcase-dolist ((and cons `(,window . ,targets))
                         (cl-callf nreverse conn-targets))
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

(cl-defmethod conn-target-finder-select :around (_target-finder)
  (let ((conn--dispatch-remap-cookies nil)
        (conn-dispatch-label-input-method nil))
    (conn-with-dispatch-event-handlers
      ( :handler (cmd)
        (when (or (and (eq cmd 'act)
                       (mouse-event-p last-input-event))
                  (eq 'dispatch-mouse-repeat
                      (event-basic-type last-input-event)))
          (let* ((posn (event-start last-input-event))
                 (win (posn-window posn))
                 (pt (posn-point posn)))
            (when (and (not (posn-area posn))
                       (funcall conn-target-window-predicate win))
              (:return (list pt win nil))))))
      (unwind-protect
          (progn
            (conn-dispatch-select-mode 1)
            (let ((inhibit-message t))
              (cl-call-next-method)))
        (conn-dispatch-select-mode -1)))))

(cl-defmethod conn-target-finder-select (target-finder)
  (conn-with-dispatch-labels
      (labels (conn-dispatch-get-labels))
    (conn-label-select
     labels
     (lambda (prompt) (conn-dispatch-read-char prompt 'label))
     (cl-loop for (_ . c) in conn-target-count
              sum c into count
              finally return (conn-target-finder-prompt-string
                              target-finder
                              (format "[%s]" count)))
     (conn-dispatch-prompt-p))))

;;;;; Dispatch Loop

(defvar conn-dispatch-in-progress nil
  "Non-nil inside while execution of `conn-dispatch-perform-action'.

`conn-with-dispatch-suspended' binds this variable to nil.")

(defvar conn-dispatch-current-action nil
  "The `oclosure-type' of the current dispatch action.")

(defvar conn--dispatch-change-groups nil)

(defvar conn--dispatch-current-thing nil)

(defvar conn-dispatch-amalgamate-undo nil
  "Controls undo amalgamation of multiple dispatch loop iterations.

When this variable is non-nil all iterations of a dispatch loop will be
amalgamated into a single undo that will be undone all at once. When
this variable is nil each iteration of a dispatch loop will be undone
separately.")

(defvar conn-dispatch-input-buffer nil
  "Buffer that was current when dispatch began.

All events read by `conn-dispatch-read-char' are read with this buffer
current.")

(defmacro conn-dispatch-undo-case (depth &rest cases)
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
`conn-dispatch-change-group' undo cases have a depth of 0."
  (declare (indent 1))
  (cl-assert (<= -100 depth 100))
  (cl-with-gensyms (buf signal)
    `(conn--dispatch-push-undo-case
      ,depth
      (let ((,buf (current-buffer)))
        (lambda (,signal)
          (with-current-buffer ,buf
            (pcase ,signal ,@cases)))))))

(define-minor-mode conn-dispatch-select-mode
  "Mode for selecting targets during dispatch."
  :global t
  :lighter " SELECT"
  :group 'conn
  (if conn-dispatch-select-mode
      (with-memoization (alist-get (current-buffer) conn--dispatch-remap-cookies)
        (face-remap-add-relative
         'mode-line
         (conn-state-get 'conn-dispatch-state :mode-line-face)))
    (unwind-protect
        (conn-cleanup-labels)
      (pcase-dolist (`(,buf . ,cookie) conn--dispatch-remap-cookies)
        (with-current-buffer buf
          (face-remap-remove-relative cookie))))))

(cl-defgeneric conn-dispatch-perform-action (action repeat))

(cl-defmethod conn-dispatch-perform-action (action repeat)
  (let ((success nil)
        (owconf (current-window-configuration))
        (oframe (selected-frame))
        (opoint (point))
        (conn-dispatch-current-action (oclosure-type action))
        (conn--dispatch-label-state nil)
        (conn--dispatch-change-groups nil)
        (conn--read-args-error-message nil)
        (conn-dispatch-in-progress t))
    (conn--unwind-protect-all
      (progn
        (redisplay)
        (catch 'dispatch-exit
          (while (or repeat (< conn-dispatch-iteration-count 1))
            (condition-case err
                (conn-with-dispatch-event-handlers
                  ( :handler (obj)
                    (when (eq obj 'repeat-dispatch)
                      (cl-callf not repeat)
                      (cl-callf not conn-dispatch-repeating)
                      (conn-dispatch-handle)))
                  ( :message -51 (keymap)
                    (when-let* ((_ repeat)
                                (binding
                                 (where-is-internal 'repeat-dispatch keymap t)))
                      (concat
                       (propertize (key-description binding)
                                   'face 'help-key-binding)
                       " "
                       (propertize
                        "repeat"
                        'face 'conn-argument-active-face))))
                  (catch 'dispatch-undo
                    (let ((frame (selected-frame))
                          (wconf (current-window-configuration))
                          (pt (point))
                          (label-state conn--dispatch-label-state))
                      (push nil conn--dispatch-change-groups)
                      (conn-dispatch-undo-case 100
                        (:undo (redisplay)))
                      (unwind-protect
                          (funcall action)
                        (unless (or (equal wconf (current-window-configuration))
                                    (null (car conn--dispatch-change-groups)))
                          (conn-dispatch-undo-case -90
                            (:undo
                             (select-frame frame)
                             (set-window-configuration wconf)
                             (goto-char pt)
                             (setq conn--dispatch-label-state label-state)))))
                      (cl-incf conn-dispatch-iteration-count))))
              (user-error
               (pcase-dolist (`(,_ . ,undo-fn)
                              (pop conn--dispatch-change-groups))
                 (funcall undo-fn :undo))
               (setf conn--read-args-error-message
                     (error-message-string err))))))
        (setq success (not dispatch-quit-flag)))
      (dolist (undo conn--dispatch-change-groups)
        (pcase-dolist (`(,_ . ,undo-fn) undo)
          (funcall undo-fn (if success :accept :cancel))))
      (if success
          (conn-accept-action action)
        (conn-cancel-action action))
      (unless success
        (select-frame oframe)
        (set-window-configuration owconf)
        (goto-char opoint)))
    (when dispatch-quit-flag (keyboard-quit))))

(defun conn-select-target ()
  "Prompt the user to select a target during dispatch.

Returns a list of (POINT WINDOW THING ARG TRANSFORM)."
  (cl-loop
   (catch 'dispatch-redisplay
     (pcase-let* ((emulation-mode-map-alists
                   `(((conn-dispatch-select-mode
                       . ,(make-composed-keymap
                           (conn-target-finder-keymaps
                            conn-dispatch-target-finder))))
                     ,@emulation-mode-map-alists))
                  (`(,pt ,win ,thing-override)
                   (conn-target-finder-select
                    conn-dispatch-target-finder))
                  (`(,thing ,arg ,transform)
                   conn--dispatch-current-thing))
       (cl-return
        (list pt
              win
              (or thing-override thing)
              arg
              transform))))))

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
  (if buffers
      (setq buffers (delete-dups buffers))
    (setq buffers (list (current-buffer))))
  (when conn-dispatch-in-progress
    (let ((cg (mapcan #'prepare-change-group
                      (or buffers (list (current-buffer)))))
          (saved-pos (cl-loop for buf in buffers
                              collect (with-current-buffer buf
                                        (point)))))
      (when (and conn--dispatch-change-groups
                 (not conn-dispatch-amalgamate-undo)
                 (length> conn--dispatch-change-groups 1))
        (dolist (b (or buffers (list (current-buffer))))
          (with-current-buffer b
            (undo-boundary))))
      (conn-dispatch-undo-case 0
        ((or :cancel :undo)
         (cancel-change-group cg)
         (cl-loop for buf in buffers
                  for pt in saved-pos
                  do (with-current-buffer buf
                       (goto-char pt))))
        (:accept (accept-change-group cg))))))

(defun conn-dispatch-undo-pulse (beg end)
  "Highlight an undo between BEG and END."
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (face-attribute 'conn-dispatch-undo-pulse-face :background))
  (pulse-momentary-highlight-region
   (min beg end) (max beg end)
   'conn--dispatch-action-current-pulse-face))

(defun conn-dispatch-read-char (&optional
                                prompt
                                inherit-input-method
                                seconds
                                prompt-suffix)
  (declare (important-return-value t))
  (let* ((inhibit-message conn-read-args-inhibit-message)
         (message-log-max nil)
         (prompt-suffix (unless inhibit-message prompt-suffix))
         (error-msg (when (and conn--read-args-error-message
                               (not inhibit-message))
                      (propertize conn--read-args-error-message
                                  'face 'error)))
         (keymap (make-composed-keymap conn--dispatch-event-handler-maps
                                       conn-dispatch-read-char-map))
         (quit-event (car (last (current-input-mode)))))
    (cl-flet ((read-ev (prompt &optional seconds)
                (with-current-buffer (or conn-dispatch-input-buffer
                                         (current-buffer))
                  (let ((scroll-conservatively 100))
                    (pcase inherit-input-method
                      ('nil
                       (read-event prompt nil seconds))
                      ('label
                       (let ((pim current-input-method)
                             (default-input-method default-input-method)
                             (input-method-history input-method-history)
                             (conn-disable-input-method-hooks t))
                         (unwind-protect
                             (progn
                               (activate-input-method
                                conn-dispatch-label-input-method)
                               (read-event prompt t seconds))
                           (activate-input-method pim))))
                      (_
                       (read-event prompt t seconds)))))))
      (if seconds
          (cl-loop
           (when-let* ((ev (read-ev (unless inhibit-message
                                      (concat prompt
                                              ": "
                                              prompt-suffix
                                              (when prompt-suffix " ")
                                              error-msg))
                                    seconds)))
             (cl-return (and (characterp ev) ev))))
        (cl-loop
         (pcase (let ((scroll-conservatively 100))
                  (conn-with-overriding-map keymap
                    (thread-first
                      (unless inhibit-message
                        (concat prompt
                                (conn--dispatch-read-char-prefix keymap)
                                ": "
                                prompt-suffix
                                (when prompt-suffix " ")
                                error-msg))
                      (read-key-sequence-vector)
                      (key-binding t))))
           ((guard (eql quit-event
                        (aref (this-command-keys-vector) 0)))
            (keyboard-quit))
           ('restart (cl-return 8))
           ('ignore)
           ('quoted-insert
            (let ((char (read-quoted-char
                         (propertize "Quoted Char: "
                                     'face 'minibuffer-prompt))))
              (unless (eq char quit-event)
                (cl-return char))))
           ('dispatch-character-event
            (setq conn--read-args-error-message nil
                  conn--dispatch-must-prompt nil)
            (conn-add-unread-events (this-single-command-raw-keys))
            (cl-return
             (read-ev (unless inhibit-message
                        (concat prompt
                                (conn--dispatch-read-char-prefix keymap)
                                ": "
                                prompt-suffix)))))
           (cmd
            (setq conn--read-args-error-message nil)
            (let ((unhandled nil))
              (unwind-protect
                  (catch 'dispatch-handle
                    (cl-loop for handler in conn--dispatch-read-char-handlers
                             do (funcall handler cmd))
                    (setq unhandled t))
                (if unhandled
                    (setq error-msg (propertize
                                     (format "Invalid command <%s>" cmd)
                                     'face 'error))
                  (setf conn-read-args-last-command cmd)
                  (setq conn--read-args-error-message nil)))
              (when (and unhandled (eq cmd 'keyboard-quit))
                (keyboard-quit))))))))))

(cl-defun conn-dispatch-handle-and-redisplay (&optional (must-prompt t))
  (redisplay)
  (cl-callf or conn--dispatch-must-prompt must-prompt)
  (throw 'dispatch-redisplay #'ignore))

(defun conn-dispatch-handle ()
  (throw 'dispatch-handle t))

(defmacro conn-with-dispatch-suspended (&rest body)
  "Execute BODY with dispatch suspended."
  (declare (indent 0))
  (cl-with-gensyms (select-mode)
    `(progn
       (unless (conn-substate-p conn-current-state
                                'conn-dispatch-targets-state)
         (error "Trying to suspend dispatch when state not active"))
       (conn-target-finder-suspend conn-dispatch-target-finder)
       (conn-cleanup-labels)
       (pcase-let ((`(,conn-target-window-predicate
                      ,conn-target-predicate
                      ,conn-target-sort-function)
                    conn--dispatch-prev-state)
                   (conn-dispatch-repeating nil)
                   (conn-dispatch-action-reference nil)
                   (conn-targets nil)
                   (conn--dispatch-label-state nil)
                   (conn-dispatch-target-finder nil)
                   (conn-dispatch-in-progress nil)
                   (conn--dispatch-change-groups nil)
                   (inhibit-message nil)
                   (recenter-last-op nil)
                   (conn-dispatch-iteration-count nil)
                   (conn-dispatch-other-end nil)
                   (conn-read-args-last-command nil)
                   (conn--read-args-prefix-mag nil)
                   (conn--read-args-prefix-sign nil)
                   (conn--dispatch-read-char-handlers nil)
                   (conn--dispatch-read-char-message-prefixes nil)
                   (conn--dispatch-always-retarget nil)
                   (,select-mode conn-dispatch-select-mode))
         (message nil)
         (unwind-protect
             (progn
               (if ,select-mode (conn-dispatch-select-mode -1))
               (conn-without-recursive-stack
                 ,@body))
           (if ,select-mode (conn-dispatch-select-mode 1)))))))

(cl-defgeneric conn-handle-dispatch-select-command (command)
  "Command handler active during `conn-dispatch-select-mode'."
  (:method (_cmd) nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql change-target-finder)))
  (conn-read-args (conn-dispatch-targets-state
                   :prompt "New Targets"
                   :reference (list conn-dispatch-thing-reference)
                   :around (lambda (cont)
                             (conn-with-dispatch-suspended
                               (funcall cont))))
      ((`(,thing ,arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument)))
    (conn-target-finder-cleanup conn-dispatch-target-finder)
    (setq conn-dispatch-target-finder (conn-get-target-finder thing arg)
          conn--dispatch-current-thing (list thing arg transform))
    (conn-dispatch-handle-and-redisplay)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql help)))
  (conn-with-overriding-map conn-dispatch-read-char-map
    (conn-quick-reference
     (conn-dispatch-select-action-reference)
     (conn-dispatch-select-target-reference)
     conn-dispatch-select-misc-reference))
  (conn-dispatch-handle))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql mwheel-scroll)))
  (when (bound-and-true-p mouse-wheel-mode)
    (mwheel-scroll last-input-event))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recursive-edit)))
  (conn-with-dispatch-suspended
    (conn-with-recursive-stack 'conn-command-state
      (recursive-edit)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recenter-top-bottom)))
  (let ((this-command 'recenter-top-bottom)
        (last-command conn-read-args-last-command))
    (recenter-top-bottom (conn-read-args-prefix-arg)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql toggle-input-method)))
  (with-current-buffer conn-dispatch-input-buffer
    (let ((inhibit-message nil)
          (message-log-max 1000))
      (toggle-input-method (conn-read-args-consume-prefix-arg))))
  (conn-dispatch-handle))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql set-input-method)))
  (with-current-buffer conn-dispatch-input-buffer
    (let ((inhibit-message nil)
          (message-log-max 1000))
      (activate-input-method
       (read-input-method-name
        (format-prompt "Select input method" nil)
        nil t))))
  (conn-dispatch-handle))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-forward)))
  (conn-with-dispatch-suspended
    (isearch-forward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-backward)))
  (conn-with-dispatch-suspended
    (isearch-backward))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-forward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-forward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql isearch-backward-regexp)))
  (conn-with-dispatch-suspended
    (isearch-backward-regexp))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-up-command)))
  (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-down-command)))
  (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-down))
  (conn-dispatch-handle-and-redisplay))

(defun conn-dispatch-goto-window (window)
  (select-window window)
  (with-memoization (alist-get (current-buffer) conn--dispatch-remap-cookies)
    (face-remap-add-relative
     'mode-line
     (conn-state-get 'conn-dispatch-state :mode-line-face))))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql conn-goto-window)))
  (if-let* ((windows (delq (selected-window) (conn--get-target-windows))))
      (progn
        (conn-dispatch-goto-window (conn-prompt-for-window windows))
        (conn-dispatch-handle-and-redisplay nil))
    (conn-dispatch-handle)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql finish)))
  (throw 'dispatch-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql keyboard-quit)))
  (setq dispatch-quit-flag t)
  (throw 'dispatch-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql other-end)))
  (unless conn-dispatch-no-other-end
    (cl-callf not conn-dispatch-other-end)
    (conn-dispatch-handle-and-redisplay nil)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql retarget)))
  (conn-target-finder-retarget conn-dispatch-target-finder)
  (conn-dispatch-handle-and-redisplay nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql always-retarget)))
  (setq conn--dispatch-always-retarget (not conn--dispatch-always-retarget))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql restrict-windows)))
  (cond ((advice-function-member-p 'conn--dispatch-restrict-windows
                                   conn-target-window-predicate)
         (remove-function conn-target-window-predicate
                          'conn--dispatch-restrict-windows)
         (conn-dispatch-handle-and-redisplay))
        ((length> conn-targets 1)
         (add-function :after-while conn-target-window-predicate
                       'conn--dispatch-restrict-windows)
         (conn-dispatch-handle-and-redisplay))))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql undo)))
  (when conn--dispatch-change-groups
    (if-let* ((curr (pop conn--dispatch-change-groups)))
        (pcase-dolist (`(,_ . ,undo-fn) curr)
          (funcall undo-fn :undo))
      (pcase-dolist (`(,_ . ,undo-fn)
                     (pop conn--dispatch-change-groups))
        (funcall undo-fn :undo))))
  (throw 'dispatch-undo nil))

;;;;; Dispatch Labels

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (pcase-let* (((cl-struct conn-dispatch-label string target)
                label)
               (start (overlay-start target))
               (win (overlay-get target 'window))
               (face (overlay-get target 'label-face)))
    (conn-dispatch-undo-case 0
      (:undo
       (conn-make-target-overlay
        start 0
        :window win
        :properties `( label-face ,face
                       label-string ,string))))
    (list start win (overlay-get target 'thing))))

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

(cl-defmethod conn-label-partial-p ((label conn-dispatch-label))
  (not (or (string-empty-p (conn-dispatch-label-narrowed-string label))
           (eq (conn-dispatch-label-narrowed-string label)
               (conn-dispatch-label-string label)))))

(cl-defmethod conn-label-redisplay ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label
                          overlay
                          target
                          narrowed-string
                          setup-function)
               label))
    (with-current-buffer (overlay-buffer overlay)
      (cond (conn-dispatch-hide-labels
             (overlay-put overlay 'display nil)
             (overlay-put overlay 'after-string nil)
             (overlay-put overlay 'before-string nil)
             (overlay-put overlay 'face nil))
            ((length> narrowed-string 0)
             (overlay-put overlay 'display nil)
             (overlay-put overlay 'before-string nil)
             (overlay-put overlay 'after-string nil)
             (funcall setup-function label)
             (overlay-put target 'face 'conn-target-overlay-face))
            (t
             (move-overlay overlay (overlay-start overlay) (overlay-start overlay))
             (overlay-put overlay 'display nil)
             (overlay-put overlay 'after-string nil)
             (overlay-put overlay 'before-string nil)
             (overlay-put target 'after-string nil)
             (overlay-put target 'face nil))))))

;;;;; Dispatch Target Finders

(defface conn-dispatch-context-separator-face
  '((t (:inherit (shadow tooltip) :extend t)))
  "Face for context region separator."
  :group 'conn-faces)

(defclass conn-target-finder-base ()
  ((current-update-handlers :initform nil)
   (update-handlers :allocation :class
                    :initform nil)
   (default-update-handler :allocation :class)
   (window-predicate :initform #'always
                     :initarg :window-predicate)
   (reference :initform nil
              :initarg :reference))
  :abstract t)

(defvar conn-dispatch-post-update-functions nil
  "Abnormal hook run after a target finder updates in each window.

Each function in the hook is called with the window that has been
updated.")

(defvar conn-dispatch-pre-update-functions nil
  "Abnormal hook run before a target finder updates in each window.

Each function in the hook is called with the window that is about to be
updated.")

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

(defun conn-dispatch-call-update-handlers (target-finder &rest args)
  (let ((ufns (oref target-finder current-update-handlers))
        (handler-ctors nil))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (apply
         (with-memoization (alist-get (current-buffer) ufns)
           (cl-labels ((find-handler (ctors)
                         (if-let* ((fn (caar ctors)))
                             (funcall fn (lambda () (find-handler (cdr ctors))))
                           (oref target-finder default-update-handler))))
             (or (find-handler (with-memoization handler-ctors
                                 (oref target-finder update-handlers)))
                 #'ignore)))
         target-finder
         args)))
    (setf (oref target-finder current-update-handlers) ufns)))

(defun conn-target-nearest-op (a b)
  (declare (side-effect-free t)
           (important-return-value t))
  (< (abs (- (overlay-end a) (point)))
     (abs (- (overlay-end b) (point)))))

(cl-defgeneric conn-get-target-finder (cmd arg)
  (declare (conn-anonymous-thing-property :target-finder)
           (important-return-value t)))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing t))
                                      _arg)
  (conn-dispatch-read-n-chars :string-length 2))

(cl-defgeneric conn-target-finder-prompt-string (target-finder prompt)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (_target-finder prompt) prompt))

(cl-defgeneric conn-target-finder-update (target-finder))

(cl-defmethod conn-target-finder-update (target-finder)
  (when (functionp target-finder)
    (funcall target-finder)))

(cl-defmethod conn-target-finder-update :around (_target-finder)
  (dolist (win (conn--get-target-windows))
    (run-hook-with-args 'conn-dispatch-pre-update-functions win))
  (cl-call-next-method)
  (dolist (win (conn--get-target-windows))
    (run-hook-with-args 'conn-dispatch-post-update-functions win)))

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
                            (length> (conn-anonymous-thing-property
                                      thing :bounds)
                                     1))))
              (progn
                (overlay-put tar 'label-face multi-face1)
                (cl-rotatef multi-face1 multi-face2))
            (overlay-put tar 'label-face face1)
            (cl-rotatef face1 face2)))))))

(defun conn-cleanup-targets ()
  (conn-target-finder-cleanup conn-dispatch-target-finder))

(cl-defgeneric conn-target-finder-cleanup (target-finder))

(cl-defmethod conn-target-finder-cleanup (_target-finder)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (delete-overlay target)))
  (setq conn-targets nil
        conn-target-count nil))

(cl-defgeneric conn-target-finder-suspend (target-finder))

(cl-defmethod conn-target-finder-suspend (_target-finder)
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (overlay-put target 'category 'conn-old-target)
      (overlay-put target 'face nil)))
  (setq conn-target-count nil))

(cl-defgeneric conn-target-finder-other-end (target-finder)
  "Default value for :other-end parameter in `conn-dispatch-perform-action'."
  (declare (conn-anonymous-thing-property :has-other-end-p)
           (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-target-finder-save-state (target-finder)
  "Return a list of functions to restore TARGET-FINDER\\='s state.

Each function should take an uninitialized target finder of the same
type and initialize it to contain the same state as TARGET-FINDER."
  (declare (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-target-finder-keymaps (target-finder)
  (declare (important-return-value t))
  ( :method (_) nil))

(cl-defgeneric conn-target-finder-message-prefixes (target-finder)
  (declare (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-target-finder-prompt-p (target-finder)
  ( :method (_target-finder) nil))

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
    (conn-with-dispatch-event-handlers
      ( :handler (obj)
        (when (conn-dispatch-label-p obj)
          (:return (conn-label-payload obj))))
      ( :keymap
        (let ((map (make-sparse-keymap)))
          (cl-loop for label in labels
                   for key = (conn-dispatch-label-string label)
                   do (keymap-set map key label))
          (mapc #'conn-label-redisplay labels)
          map))
      (ignore (conn-dispatch-read-char "Register"))
      (while t
        (ignore
         (conn-dispatch-read-char
          "Register" nil nil
          (propertize "Invalid key" 'face 'error)))))))

(defclass conn-dispatch-retargetable-mixin ()
  ()
  :abstract t)

(defvar-keymap conn-dispatch-retargeting-argument-map
  "M-f" 'always-retarget
  "C-f" 'retarget)

(cl-defgeneric conn-target-finder-retarget (target-finder)
  (:method (_) nil))

(cl-defgeneric conn-dispatch-has-targets-p (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-target-finder-keymaps ((_ conn-dispatch-retargetable-mixin))
  conn-dispatch-retargeting-argument-map)

(cl-defmethod conn-target-finder-message-prefixes ((state conn-dispatch-retargetable-mixin))
  (nconc
   (list (when-let* ((binding
                      (and (conn-dispatch-has-targets-p state)
                           (not conn--dispatch-always-retarget)
                           (where-is-internal 'retarget nil t))))
           (concat
            (propertize (key-description binding)
                        'face 'help-key-binding)
            " retarget"))
         (when-let* ((binding
                      (and (conn-dispatch-has-targets-p state)
                           conn-dispatch-repeating
                           (where-is-internal 'always-retarget nil t))))
           (concat
            (propertize (key-description binding)
                        'face 'help-key-binding)
            " "
            (propertize "always retarget"
                        'face (when conn--dispatch-always-retarget
                                'eldoc-highlight-function-argument)))))
   (cl-call-next-method)))

(conn-define-target-finder conn-dispatch-string-targets
    (conn-dispatch-retargetable-mixin)
    ((string :initform nil
             :initarg :string)
     (thing :initform nil
            :initarg :thing)
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

(cl-defmethod conn-target-finder-prompt-string ((target-finder conn-dispatch-string-targets)
                                                prompt)
  (if-let* ((str (oref target-finder string)))
      (concat str " " prompt)
    prompt))

(cl-defmethod conn-target-finder-save-state ((target-finder conn-dispatch-string-targets))
  (cons (let ((str (oref target-finder string)))
          (lambda (tf) (setf (oref tf string) str)))
        (cl-call-next-method)))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(cl-defmethod conn-dispatch-has-targets-p ((state conn-dispatch-string-targets))
  (and (oref state string) t))

(conn-define-target-finder conn-dispatch-read-n-chars
    (conn-dispatch-string-targets)
    ((string-length :initform 1
                    :initarg :string-length))
  ( :update-method (state)
    (cl-symbol-macrolet ((string (oref state string)))
      (if string
          (conn-dispatch-call-update-handlers state 0)
        (let* ((string-length (oref state string-length))
               (prompt (if (> string-length 1)
                           (propertize (format "%d Chars" string-length)
                                       'face 'minibuffer-prompt)
                         (propertize "1 Char" 'face 'minibuffer-prompt))))
          (while (length< string string-length)
            (when (length> string 0)
              (while-no-input
                (conn-dispatch-call-update-handlers state)))
            (catch 'dispatch-redisplay
              (conn-with-dispatch-event-handlers
                ( :handler (cmd)
                  (when (eq cmd 'backspace)
                    (when (length> string 0)
                      (cl-callf substring string 0 -1))
                    (:return)))
                (:keymap (define-keymap "<backspace>" 'backspace))
                (conn-threadf->
                    string
                  (conn-dispatch-read-char prompt t nil)
                  (char-to-string)
                  (concat string))))
            (conn-cleanup-targets))
          (conn-dispatch-call-update-handlers state 0))))))

(defvar-keymap conn-dispatch-read-string-target-keymap
  "M-e" 'read-string)

(conn-define-target-finder conn-dispatch-read-with-timeout
    (conn-dispatch-string-targets)
    ((timeout :initform 0.5 :initarg :timeout))
  ( :update-method (state)
    (cl-symbol-macrolet ((string (oref state string)))
      (let ((timeout (oref state timeout)))
        (unless string
          (conn-with-dispatch-event-handlers
            (:keymap conn-dispatch-read-string-target-keymap)
            ( :handler (cmd)
              (when (eq cmd 'read-string)
                (let ((str (conn-with-dispatch-suspended
                             (read-string
                              "String: " string
                              'conn-read-string-target-history
                              nil t))))
                  (unless (equal str "")
                    (setq string str)
                    (:return)))))
            (let* ((prompt (propertize "String" 'face 'minibuffer-prompt)))
              (setf string (char-to-string (conn-dispatch-read-char prompt t)))
              (while-no-input
                (conn-dispatch-call-update-handlers state))
              (while-let ((next-char (conn-dispatch-read-char
                                      prompt t timeout string)))
                (conn-cleanup-targets)
                (setf string (concat string (char-to-string next-char)))
                (conn-dispatch-call-update-handlers state))))
          (conn-cleanup-targets)))
      (conn-dispatch-call-update-handlers state 0))))

(defclass conn-dispatch-focus-mixin ()
  ((hidden :initform nil)
   (context-lines :initform 0 :initarg :context-lines)
   (cursor-location :initform nil)
   (separator-p :initarg :separator))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(cl-defmethod conn-target-finder-prompt-p ((_state conn-dispatch-focus-mixin))
  t)

(cl-defmethod conn-target-finder-cleanup ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-target-finder-suspend ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recenter-top-bottom))
                                                   &context (conn-dispatch-target-finder
                                                             conn-dispatch-focus-mixin))
  (conn-threadf<-
      (alist-get (selected-window)
                 (oref conn-dispatch-target-finder cursor-location))
    (memq recenter-positions)
    (cadr)
    (or (car recenter-positions)))
  (let ((inhibit-message nil)
        (message-log-max 1000))
    (message "%s" (oref conn-dispatch-target-finder cursor-location)))
  (unless executing-kbd-macro
    (pulse-momentary-highlight-one-line))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil))

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,win ,tick . ,ovs) (oref state hidden))
    (unless (eql tick (buffer-chars-modified-tick (window-buffer win)))
      (mapc #'delete-overlay ovs)
      (setf (alist-get win (oref state hidden)) nil))))

(cl-defmethod conn-target-finder-update :after ((state conn-dispatch-focus-mixin))
  (pcase-dolist (`(,win . ,targets) conn-targets)
    (pcase-let ((`(,tick . ,old-hidden)
                 (alist-get win (oref state hidden))))
      (with-selected-window win
        (unless (eql tick (buffer-chars-modified-tick (window-buffer win)))
          (mapc #'delete old-hidden)
          (conn-protected-let*
              ((hidden (list (make-overlay (point-min) (point-min)))
                       (mapc #'delete-overlay hidden))
               (context-lines (oref state context-lines))
               (separator-p (if (slot-boundp state 'separator-p)
                                (oref state separator-p)
                              (> context-lines 0))))
            (let ((regions (list (cons (pos-bol) (pos-bol 2)))))
              (save-excursion
                (dolist (tar targets)
                  (push (or (overlay-get tar 'context)
                            (progn
                              (goto-char (overlay-start tar))
                              (let ((beg (pos-bol (- 1 context-lines)))
                                    (end (pos-bol (+ 2 context-lines))))
                                (cons (if (invisible-p end) (max 1 (1- beg)) beg)
                                      end))))
                        regions)))
              (cl-callf conn--merge-overlapping-regions regions t)
              (conn--compat-callf sort regions :key #'car :in-place t)
              (cl-loop for beg = (point-min) then next-beg
                       for (end . next-beg) in regions
                       while end
                       do (let ((ov (make-overlay beg end)))
                            (push ov hidden)
                            (overlay-put ov 'invisible t)
                            (overlay-put ov 'window win)
                            (when (and separator-p (/= end (point-max)))
                              (overlay-put
                               (car hidden)
                               'before-string
                               (propertize
                                (format " %s\n"
                                        (when (memq display-line-numbers
                                                    '(nil relative visual))
                                          (line-number-at-pos end)))
                                'face 'conn-dispatch-context-separator-face))))
                       finally (let ((ov (make-overlay beg (point-max))))
                                 (push ov hidden)
                                 (overlay-put ov 'window win)
                                 (overlay-put ov 'invisible t))))
            (setf (alist-get win (oref state hidden))
                  (cons (buffer-chars-modified-tick) hidden))))
        (let ((this-scroll-margin
               (min (max 0 scroll-margin)
                    (truncate (/ (window-body-height) 4.0)))))
          (pcase (alist-get win (oref state cursor-location))
            ('middle (recenter nil))
            ('top (recenter this-scroll-margin))
            ('bottom (recenter (- -1 this-scroll-margin)))
            (_ (setf (alist-get win (oref state cursor-location)) 'middle)
               (recenter nil)))))))
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
    (let ((line-height (window-height))
          (string (oref state string))
          (prev (point))
          (line-count 0))
      (save-excursion
        (while (and (< line-count line-height)
                    (search-backward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (overlay-put (conn-make-target-overlay beg 0)
                         'thing (conn-anonymous-thing
                                  'region
                                  :bounds-op ( :method (_self _arg)
                                               (conn-make-bounds
                                                'region nil
                                                (cons beg end)))))
            (when (<= beg (pos-eol) prev)
              (cl-incf line-count))
            (setq prev beg))))
      (setq line-count 0)
      (save-excursion
        (while (and (< line-count line-height)
                    (search-forward string nil t))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (conn-make-target-overlay
             beg 0
             :thing (conn-anonymous-thing
                      'region
                      :bounds-op ( :method (_self _arg)
                                   (conn-make-bounds
                                    'region nil
                                    (cons beg end)))))
            (when (<= prev (pos-bol) beg)
              (cl-incf line-count))
            (setq prev beg)))))))

(conn-define-target-finder conn-dispatch-jump-ring
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initform (lambda (win) (eq win (selected-window)))))
  ( :default-update-handler (_state)
    (let ((points (conn-ring-list conn-jump-ring)))
      (dolist (pt points)
        (unless (invisible-p pt)
          (conn-make-target-overlay pt 0)))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-jump-ring))
  :no-other-end)

(conn-define-target-finder conn-dispatch-global-mark
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
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

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-global-mark))
  :no-other-end)

(conn-define-target-finder conn-dispatch-mark-register
    (conn-dispatch-focus-mixin
     conn-dispatch-target-key-labels-mixin)
    ((context-lines :initform 1
                    :initarg :context-lines))
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

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-mark-register))
  :no-other-end)

(conn-define-target-finder conn-dispatch-previous-emacs-state
    (conn-dispatch-focus-mixin)
    ((context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initform (lambda (win) (eq win (selected-window)))))
  ( :default-update-handler (_state)
    (dolist (pt (conn-ring-list conn-emacs-state-ring))
      (unless (invisible-p pt)
        (conn-make-target-overlay pt 0))))
  ( :update-method (state)
    (unless conn-targets
      (conn-dispatch-call-update-handlers state))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-previous-emacs-state))
  :no-other-end)

(defun conn-dispatch-chars-in-thing (thing)
  (declare (important-return-value t))
  (conn-dispatch-read-with-timeout
   :thing thing
   :timeout conn-read-string-timeout
   :predicate (lambda (beg _end)
                (goto-char beg)
                (ignore-errors
                  (conn-bounds-of thing nil)))))

(conn-define-target-finder conn-dispatch-headings
    (conn-dispatch-focus-mixin)
    ()
  ( :default-update-handler (state)
    (when (or (derived-mode-p (list 'outline-mode))
              (bound-and-true-p outline-minor-mode))
      (let ((heading-regexp (concat "^\\(?:" outline-regexp "\\).*")))
        (save-excursion
          (pcase-dolist (`(,beg . ,end)
                         (conn--visible-regions (point-min) (point-max)))
            (goto-char beg)
            (while (re-search-forward heading-regexp end t)
              (when (looking-at-p outline-heading-end-regexp)
                (conn-make-target-overlay
                 (match-beginning 0) 0)))))))))

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
        (conn-make-target-overlay
         pt 0
         :properties '(no-hide t))))))

(conn-define-target-finder conn-all-things-targets
    ()
    ((thing :initarg :thing))
  ( :default-update-handler (state)
    (let ((thing (oref state thing)))
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
    () ()
  ( :default-update-handler (_state)
    (conn-for-each-visible (window-start) (window-end)
      (goto-char (point-min))
      (when (get-char-property (point) 'button)
        (conn-make-target-overlay (point) 0))
      (while (not (eobp))
        (goto-char (next-single-char-property-change (point) 'button))
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
                    'point
                    :bounds-op ( :method (_self _arg)
                                 (save-match-data
                                   (when (looking-at regexp)
                                     (conn-make-bounds
                                      'point nil
                                      (cons (point) (match-end 0)))))))))))))

(defun conn-dispatch-things-read-prefix (thing prefix-length)
  (declare (important-return-value t))
  (conn-dispatch-read-n-chars
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
    ((thing :initarg :thing)
     (prefix-string :initarg :prefix-string)
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
    ((thing :initarg :thing)
     (prefix-regexp :initarg :prefix-regexp)
     (fixed-length :initform nil
                   :initarg :fixed-length))
  ( :default-update-handler (state)
    (let ((thing (oref state thing))
          (prefix (oref state prefix-regexp))
          (fixed-length (oref state fixed-length)))
      (pcase-dolist (`(,beg . ,end)
                     (conn--visible-re-matches
                      prefix
                      (lambda (beg end)
                        (save-excursion
                          (goto-char beg)
                          (pcase (ignore-errors (conn-bounds-of thing nil))
                            ((conn-bounds `(,tbeg . ,tend))
                             (and (= tbeg beg) (<= end tend))))))))
        (conn-make-target-overlay beg (or fixed-length (- end beg)))))))

(conn-define-target-finder conn-dispatch-things-matching-re-targets
    ()
    ((thing :initarg :thing)
     (regexp :initarg :prefix-regexp)
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
    () ()
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
      (conn-for-each-visible (window-start) (window-end)
        (goto-char (point-min))
        (while (/= (point) (point-max))
          (vertical-motion (cons col-width 0))
          (unless (and (eolp) (= (point) (point-min)))
            (conn-make-target-overlay
             (point) 0
             :thing 'point
             :padding-function #'conn--flush-left-padding))
          (forward-line 1))))))

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
    () ()
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

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-end-of-line-targets))
  t)

(conn-define-target-finder conn-dispatch-inner-line-targets
    () ()
  ( :default-update-handler (_state)
    (let ((thing (conn-anonymous-thing
                   'conn-forward-inner-line
                   :pretty-print ( :method (_self) "inner-line")
                   :bounds-op ( :method (_self _arg)
                                (goto-char (pos-bol))
                                (cl-call-next-method)))))
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
    () ()
  ( :default-update-handler (_state)
    (let ((thing (conn-anonymous-thing
                   'conn-forward-inner-line
                   :pretty-print ( :method (_self) "end-of-inner-line")
                   :bounds-op ( :method (_self _arg)
                                (goto-char (pos-bol))
                                (cl-call-next-method)))))
      (conn-for-each-visible (window-start) (window-end)
        (goto-char (point-max))
        (while (let ((pt (point)))
                 (forward-line -1)
                 (conn-end-of-inner-line)
                 (/= (point) pt))
          (when (not (invisible-p (point)))
            (conn-make-target-overlay
             (point) 0
             :thing thing)))))))

(cl-defmethod conn-target-finder-other-end
  ((_ conn-dispatch-end-of-inner-line-targets))
  t)

(conn-define-target-finder conn-dispatch-visual-line-targets
    () ()
  ( :default-update-handler (_state)
    (save-excursion
      (goto-char (window-start))
      (vertical-motion 0)
      (conn-make-target-overlay
       (point) 0
       :thing 'point
       :padding-function (lambda (ov width _face)
                           (conn--flush-left-padding ov width nil)))
      (vertical-motion 1)
      (while (<= (point) (window-end))
        (if (= (point) (point-max))
            (conn-make-target-overlay
             (point) 0
             :thing 'point
             ;; hack to get the label displayed on its own line
             :properties `(after-string
                           ,(propertize " " 'display '(space :width 0))))
          (conn-make-target-overlay
           (point) 0
           :thing 'point
           :padding-function (lambda (ov width _face)
                               (conn--flush-left-padding ov width nil))))
        (vertical-motion 1)))))

;;;;; Dispatch Actions

(oclosure-define (conn-action
                  (:predicate conn-action-p)
                  (:copier conn-action--copy))
  (action-auto-repeat :type boolean)
  (action-no-history :mutable t :type boolean)
  (action-description :type (or string nil))
  (action-window-predicate :type function)
  (action-target-predicate :type function)
  (action-thing-predicate :type function)
  (action-always-retarget :type boolean)
  (action-always-prompt :type boolean)
  (action-reference :type string))

(defalias 'conn-action-no-history 'conn-action--action-no-history)
(defalias 'conn-action-auto-repeat 'conn-action--action-auto-repeat)
(defalias 'conn-action-description 'conn-action--action-description)
(defalias 'conn-action-window-predicate 'conn-action--action-window-predicate)
(defalias 'conn-action-target-predicate 'conn-action--action-target-predicate)
(defalias 'conn-action-thing-predicate 'conn-action--action-thing-predicate)
(defalias 'conn-action-always-retarget 'conn-action--action-always-retarget)
(defalias 'conn-action-always-prompt 'conn-action--action-always-prompt)
(defalias 'conn-action-reference 'conn-action--action-reference)

(defun conn-action-get-reference (action)
  (when-let* ((doc-string (and action
                               (conn-action-reference action))))
    (conn-reference-quote
      ((:heading (concat "Current Action: "
                         (conn-action-pretty-print action)))
       (:eval doc-string)))))

(eval-and-compile
  (defun conn--set-action-property (f _args val)
    `(function-put ',f :conn-dispatch-action ',val))
  (setf (alist-get 'conn-dispatch-action defun-declarations-alist)
        (list #'conn--set-action-property)))

(cl-defgeneric conn-make-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t)))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing t)))
  (conn-dispatch-goto))

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

(cl-defgeneric conn-action-pretty-print (action &optional short)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method ((action conn-action) &optional _)
    (conn-action-description action)))

(cl-defgeneric conn-accept-action (action)
  (:method ((action conn-action)) action))

(cl-defgeneric conn-cancel-action (action)
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
           (run-hooks 'deactivate-mark-hook))))
     (setf (cdr change-group) nil
           (car change-group) nil))))

(defun conn-dispatch-insert-separator (separator)
  (pcase separator
    ('nil)
    ('register
     (insert (get-register register-separator))
     (fixup-whitespace))
    ('space
     (insert " ")
     (fixup-whitespace))
    ('newline
     (unless (or (bobp)
                 (save-excursion
                   (forward-line -1)
                   (looking-at-p (rx (seq (* (syntax whitespace))
                                          eol)))))
       (insert "\n")))
    (_
     (insert separator))))

(oclosure-define (conn-dispatch-goto
                  (:parent conn-action)))

(defun conn-dispatch-goto ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-goto
                    (action-description "Goto")
                    (action-reference
                     "Goes to the start of the selected thing.  If other-end is non-nil then
goes to the end of the thing."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (select-window window)
      (conn-dispatch-change-group)
      (conn-push-jump-ring (point))
      (unless (and (= pt (point))
                   (region-active-p))
        (let ((forward (< (point) pt)))
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (if (region-active-p)
                 (goto-char (if (and forward (not (eq thing 'point)))
                                end
                              beg))
               (push-mark nil t)
               (goto-char beg)))
            (_ (user-error "Cannot find thing at point"))))))))

(oclosure-define (conn-dispatch-push-button
                  (:parent conn-action)))

(defun conn-dispatch-push-button ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-push-button
                    (action-description "Push Button")
                    (action-no-history t)
                    (action-reference
                     "Push the selected button."))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (select-window window)
      (if (button-at pt)
          (push-button pt)
        (when (fboundp 'widget-apply-action)
          (widget-apply-action (get-char-property pt 'button) pt))))))

(oclosure-define (conn-dispatch-copy-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(defun conn-dispatch-copy-to ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument))
       (ftransform (conn-transform-argument))
       (separator (conn-separator-argument 'default)))
    (let ((str (pcase (conn-bounds-of fthing farg)
                 ((conn-bounds `(,beg . ,end) ftransform)
                  (conn-dispatch-action-pulse beg end)
                  (filter-buffer-substring beg end))
                 (_ (user-error "No %s found" (conn-thing-pretty-print fthing))))))
      (oclosure-lambda (conn-dispatch-copy-to
                        (str str)
                        (separator
                         (if (eq separator 'default)
                             (cond ((eq fthing 'region))
                                   ((seq-contains-p str ?\n #'eql)
                                    'newline)
                                   (t 'space))
                           separator))
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
        (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                      (conn-select-target)))
          (with-selected-window window
            (conn-dispatch-change-group)
            (save-mark-and-excursion
              (pcase (conn-bounds-of-dispatch thing arg pt)
                ((conn-dispatch-bounds `(,beg . ,end) transform)
                 (goto-char beg)
                 (when (and separator (< end beg))
                   (conn-dispatch-insert-separator separator))
                 (insert-for-yank str)
                 (when (and separator (not (< end beg)))
                   (conn-dispatch-insert-separator separator))
                 (conn-dispatch-action-pulse
                  (- (point) (length str))
                  (point)))
                (_ (user-error "Cannot find thing at point"))))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-copy-to)
                                        &optional
                                        short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-copy-to--separator action))))
      (format "Copy To <%s>" sep)
    "Copy To"))

(oclosure-define (conn-dispatch-copy-to-replace
                  (:parent conn-action))
  (str :type string))

(defun conn-dispatch-copy-to-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument-dwim))
       (ftransform (conn-transform-argument)))
    (oclosure-lambda (conn-dispatch-copy-to-replace
                      (action-description "Copy and Replace To")
                      (str (pcase (conn-bounds-of fthing farg)
                             ((conn-bounds `(,beg . ,end) ftransform)
                              (conn-dispatch-action-pulse beg end)
                              (filter-buffer-substring beg end))
                             (_ (user-error "Cannot find %s at point"
                                            (conn-thing-pretty-print fthing)))))
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win)))))
                      (action-reference
                       "Copy the current region to the region selected, replacing it."))
        ()
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-mark-and-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-bounds `(,beg . ,end) transform)
               (goto-char beg)
               (delete-region beg end)
               (insert-for-yank str)
               (conn-dispatch-action-pulse
                (- (point) (length str))
                (point)))
              (_ (user-error "Cannot find %s"
                             (conn-thing-pretty-print thing))))))))))

(oclosure-define (conn-dispatch-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(defun conn-dispatch-yank-to-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-yank-to-replace
                    (action-description "Yank and Replace To")
                    (str (current-kill 0))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win)))))
                    (action-reference
                     "Yank the the last killed text from the kill ring and replace the region
selected by dispatch with it."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (delete-region beg end)
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point)))
            (_ (user-error "Cannot find thing at point"))))))))

(oclosure-define (conn-dispatch-reading-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(defun conn-dispatch-reading-yank-to-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-reading-yank-to-replace
                    (action-description "Yank and Replace To")
                    (str (read-from-kill-ring "Yank: "))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win)))))
                    (action-reference
                     "Select a string from the kill list and replace the region selected by
dispatch with it."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (delete-region beg end)
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point)))
            (_ (user-error "Cannot find thing at point"))))))))

(oclosure-define (conn-dispatch-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(defun conn-dispatch-yank-to ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-yank-to
                    (str (current-kill 0))
                    (separator
                     (cond ((conn-read-args-consume-prefix-arg)
                            (read-string "Separator: " nil nil nil t))
                           ((and register-separator
                                 (get-register register-separator))
                            'register)
                           (t 'default)))
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
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (when (and separator (< end beg))
               (conn-dispatch-insert-separator separator))
             (insert-for-yank str)
             (when (and separator (not (< end beg)))
               (conn-dispatch-insert-separator separator))
             (conn-dispatch-action-pulse
              (- (point) (length str))
              (point)))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-yank-to)
                                        &optional
                                        short)
  (if-let* ((sep (and (not short) (conn-dispatch-yank-to--separator action))))
      (format "Yank To <%s>" (if (eq sep 'register)
                                 (get-register register-separator)
                               sep))
    "Yank To"))

(oclosure-define (conn-dispatch-reading-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(defun conn-dispatch-reading-yank-to ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (let ((str (read-from-kill-ring "Yank To: ")))
    (oclosure-lambda (conn-dispatch-reading-yank-to
                      (str str)
                      (separator
                       (cond ((conn-read-args-consume-prefix-arg)
                              (read-string "Separator: " nil nil nil t))
                             ((and register-separator
                                   (get-register register-separator))
                              'register)
                             (t 'default)))
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
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-dispatch-bounds `(,beg . ,end) transform)
               (goto-char beg)
               (when (and separator (< end beg))
                 (conn-dispatch-insert-separator separator))
               (insert-for-yank str)
               (when (and separator (not (< end beg)))
                 (conn-dispatch-insert-separator separator))
               (conn-dispatch-action-pulse
                (- (point) (length str))
                (point)))
              (_ (user-error "Cannot find thing at point")))
            (conn-dispatch-action-pulse
             (- (point) (length str))
             (point))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-reading-yank-to)
                                        &optional
                                        short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-reading-yank-to--separator action))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-send
                  (:parent conn-action))
  (str :type string)
  (separator :type string)
  (action-change-group))

(defun conn-dispatch-send ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (conn-read-args (conn-kill-state
                   :prompt "Send Thing")
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (fixup (when conn-kill-reformat-function
                (conn-reformat-argument)))
       (check-bounds
        (conn-boolean-argument "check bounds"
                               'check-bounds
                               conn-check-bounds-argument-map
                               t))
       (separator (conn-separator-argument 'default)))
    (let* ((cg (conn--action-buffer-change-group))
           (str (progn
                  (conn-kill-thing thing arg transform
                                   nil nil nil
                                   fixup check-bounds)
                  (current-kill 0))))
      (oclosure-lambda (conn-dispatch-send
                        (action-description "Send")
                        (action-change-group cg)
                        (str str)
                        (separator
                         (if (eq separator 'default)
                             (cond ((eq thing 'region))
                                   ((seq-contains-p str ?\n #'eql) 'newline)
                                   (t 'space))
                           separator))
                        (action-window-predicate
                         (lambda (win)
                           (not
                            (buffer-local-value 'buffer-read-only
                                                (window-buffer win)))))
                        (action-reference
                         "Delete a thing at point and insert it at the region selected
by dispatch.  By default this action inserts the string before the
region selected by dispatch but if OTHER-END is non-nil then it inserts
the string after the region selected by dispatch."))
          ()
        (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                      (conn-select-target)))
          (with-selected-window window
            (conn-dispatch-change-group)
            (save-excursion
              (pcase (conn-bounds-of-dispatch thing arg pt)
                ((conn-dispatch-bounds `(,beg . ,end) transform)
                 (goto-char beg)
                 (when (< end beg)
                   (conn-dispatch-insert-separator separator))
                 (insert-for-yank str)
                 (when (not (< end beg))
                   (conn-dispatch-insert-separator separator))
                 (conn-dispatch-action-pulse
                  (- (point) (length str))
                  (point)))
                (_ (user-error "Cannot find thing at point")))
              (conn-dispatch-action-pulse
               (- (point) (length str))
               (point)))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send))
  (conn--action-accept-change-group
   (conn-dispatch-send--action-change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send))
  (conn--action-cancel-change-group
   (conn-dispatch-send--action-change-group action)))

(oclosure-define (conn-dispatch-send-replace
                  (:parent conn-action))
  (str :type string)
  (action-change-group))

(defun conn-dispatch-send-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send-replace
                      (action-description "Send and Replace")
                      (action-change-group cg)
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
                                                    t)))
                         (progn
                           (conn-kill-thing thing arg transform
                                            nil nil nil
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
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (save-excursion
            (pcase (conn-bounds-of-dispatch thing arg pt)
              ((conn-bounds `(,beg . ,end) transform)
               (goto-char beg)
               (delete-region beg end)
               (insert-for-yank str)
               (conn-dispatch-action-pulse
                (- (point) (length str)) (point)))
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send-replace))
  (conn--action-accept-change-group
   (conn-dispatch-send-replace--action-change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send-replace))
  (conn--action-cancel-change-group
   (conn-dispatch-send-replace--action-change-group action)))

(oclosure-define (conn-dispatch-register-load
                  (:parent conn-action))
  (register :type integer))

(defun conn-dispatch-register-load ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-register-load
                    (register (register-read-with-preview "Register: "))
                    (action-reference
                     "Load register at point selected by dispatch."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (with-selected-window window
        (conn-dispatch-change-group)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-dispatch-bounds `(,beg . ,_end) transform)
             (goto-char beg)
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-register-load)
                                        &optional
                                        short)
  (if short "Register"
    (format "Register <%c>" (conn-dispatch-register-load--register action))))

(oclosure-define (conn-dispatch-register-load-replace
                  (:parent conn-action))
  (register :type integer))

(defun conn-dispatch-register-load-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-register-load-replace
                    (register (register-read-with-preview "Register: "))
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
             (delete-region beg end)
             (conn-register-load register))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-register-load-replace)
                                        &optional
                                        short)
  (if short "Register Replace"
    (format "Register Replace <%c>"
            (conn-dispatch-register-load-replace--register action))))

(oclosure-define (conn-dispatch-copy-from
                  (:parent conn-action)
                  (:copier conn-dispatch-copy-from-copy (action-opoint)))
  (action-opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-copy-from))
  (thread-first
    (conn-dispatch-copy-from--action-opoint action)
    marker-buffer
    buffer-live-p
    not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-copy-from))
  (set-marker (conn-dispatch-copy-from--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-copy-from))
  (conn-dispatch-copy-from-copy
   action
   (copy-marker (conn-dispatch-copy-from--action-opoint action) t)))

(cl-defmethod conn-cancel-action ((action conn-dispatch-copy-from))
  (set-marker (conn-dispatch-copy-from--action-opoint action) nil))

(defun conn-dispatch-copy-from ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-copy-from
                    (action-description "Copy From")
                    (action-opoint (copy-marker (point) t))
                    (action-reference
                     "Copy text in region selected by dispatch to point."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (let (str)
        (with-selected-window window
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (conn-dispatch-action-pulse beg end)
             (setq str (filter-buffer-substring beg end)))
            (_ (user-error "Cannot find thing at point"))))
        (with-current-buffer (marker-buffer action-opoint)
          (conn-dispatch-change-group)
          (cond ((null str)
                 (user-error "Cannot find thing at point"))
                ((/= (point) action-opoint)
                 (save-excursion
                   (goto-char action-opoint)
                   (insert-for-yank str)))
                (t
                 (goto-char action-opoint)
                 (insert-for-yank str))))))))

(oclosure-define (conn-dispatch-copy-from-replace
                  (:parent conn-action))
  (action-opoint :type marker)
  (action-change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-copy-from-replace))
  (when-let* ((mk (conn-dispatch-copy-from-replace--action-opoint action)))
    (thread-first mk marker-buffer buffer-live-p not)))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-copy-from-replace))
  (set-marker (conn-dispatch-copy-from-replace--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-copy-from-replace))
  (conn-thread<-
    (conn-dispatch-copy-from-replace--action-opoint action)
    (copy-marker t)
    (:-> (conn-dispatch-replace-copy action))))

(defun conn-dispatch-copy-from-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Replace Thing")
      ((`(,rthing ,rarg) (conn-thing-argument-dwim-always))
       (rtransform (conn-transform-argument)))
    (pcase (conn-bounds-of rthing rarg)
      ((conn-bounds `(,rbeg . ,rend) rtransform)
       (let ((cg (conn--action-buffer-change-group)))
         (delete-region rbeg rend)
         (oclosure-lambda (conn-dispatch-copy-from-replace
                           (action-description "Copy From and Replace")
                           (action-opoint (copy-marker (point) t))
                           (action-change-group cg)
                           (action-reference
                            "Replace current region with text in region selected by dispatch."))
             ()
           (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                         (conn-select-target)))
             (with-selected-window window
               (pcase (conn-bounds-of-dispatch thing arg pt)
                 ((conn-bounds `(,beg . ,end) transform)
                  (conn-dispatch-action-pulse beg end)
                  (copy-region-as-kill beg end))
                 (_ (user-error "Cannot find thing at point"))))
             (with-current-buffer (marker-buffer action-opoint)
               (save-excursion
                 (conn-dispatch-change-group)
                 (goto-char action-opoint)
                 (yank)))))))
      (_ (error "No region to replace")))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-copy-from-replace))
  (set-marker (conn-dispatch-copy-from-replace--action-opoint action) nil)
  (conn--action-cancel-change-group
   (conn-dispatch-copy-from-replace--action-change-group action)))

(cl-defmethod conn-accept-action ((action conn-dispatch-copy-from-replace))
  (conn--action-accept-change-group
   (conn-dispatch-copy-from-replace--action-change-group action)))

(oclosure-define (conn-dispatch-replace
                  (:parent conn-action)
                  (:copier conn-dispatch-replace-copy (action-opoint)))
  (action-opoint :type marker)
  (action-change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-replace))
  (not (thread-first
         (conn-dispatch-replace--action-opoint action)
         marker-buffer
         buffer-live-p)))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-replace))
  (set-marker (conn-dispatch-replace--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-replace))
  (conn-thread<-
    (conn-dispatch-replace--action-opoint action)
    (copy-marker t)
    (:-> (conn-dispatch-replace-copy action))))

(defun conn-dispatch-replace ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (conn-read-args (conn-copy-state
                   :prompt "Replace Thing")
      ((`(,rthing ,rarg) (conn-thing-argument-dwim-always))
       (rtransform (conn-transform-argument)))
    (pcase (conn-bounds-of rthing rarg)
      ((conn-bounds `(,rbeg . ,rend) rtransform)
       (let ((cg (conn--action-buffer-change-group)))
         (delete-region rbeg rend)
         (oclosure-lambda (conn-dispatch-replace
                           (action-description "Grab From and Replace")
                           (action-change-group cg)
                           (action-opoint (copy-marker rbeg t))
                           (action-window-predicate
                            (lambda (win)
                              (not
                               (buffer-local-value 'buffer-read-only
                                                   (window-buffer win)))))
                           (action-reference
                            "Replace a thing at point with another thing selected by dispatch."))
             ()
           (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                         (conn-select-target)))
             (conn-dispatch-change-group (current-buffer) (window-buffer window))
             (with-selected-window window
               (save-excursion
                 (goto-char pt)
                 (pcase (conn-bounds-of thing arg)
                   ((and bounds (conn-bounds `(,beg . ,end) transform))
                    (kill-region beg end)
                    (when conn-kill-reformat-function
                      (funcall conn-kill-reformat-function bounds)))
                   (_ (user-error "Cannot find thing at point")))))
             (with-current-buffer (marker-buffer action-opoint)
               (save-excursion
                 (goto-char action-opoint)
                 (yank)))))))
      (_ (error "No region to replace")))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-replace))
  (set-marker (conn-dispatch-replace--action-opoint action) nil)
  (conn--action-cancel-change-group
   (conn-dispatch-replace--action-change-group action)))

(cl-defmethod conn-accept-action ((action conn-dispatch-replace))
  (conn--action-accept-change-group
   (conn-dispatch-replace--action-change-group action)))

(oclosure-define (conn-dispatch-grab
                  (:parent conn-action)
                  (:copier conn-dispatch-grab-copy (action-opoint)))
  (action-opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-grab))
  (not (thread-first
         (conn-dispatch-grab--action-opoint action)
         marker-buffer
         buffer-live-p)))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-grab))
  (set-marker (conn-dispatch-grab--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-grab))
  (conn-thread<-
    (conn-dispatch-grab--action-opoint action)
    (copy-marker t)
    (:-> (conn-dispatch-grab-copy action))))

(defun conn-dispatch-grab ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-grab
                    (action-description "Grab From")
                    (action-opoint (copy-marker (point) t))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win)))))
                    (action-reference
                     "Kill the thing selected by dispatch and yank it at point."))
      ()
    (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                  (conn-select-target)))
      (conn-dispatch-change-group (current-buffer) (window-buffer window))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing arg)
            ((and bounds (conn-bounds `(,beg . ,end) transform))
             (kill-region beg end)
             (when conn-kill-reformat-function
               (funcall conn-kill-reformat-function bounds)))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer (marker-buffer action-opoint)
        (yank)))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-grab))
  (set-marker (conn-dispatch-grab--action-opoint action) nil))

(oclosure-define (conn-dispatch-jump
                  (:parent conn-action)))

(defun conn-dispatch-jump ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-jump
                    (action-description "Jump"))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (select-window window)
      (conn-dispatch-change-group)
      (conn-push-jump-ring (point))
      (unless (= pt (point))
        (unless (region-active-p)
          (push-mark nil t))
        (goto-char pt)))))

(oclosure-define (conn-dispatch-repeat-command
                  (:parent conn-action))
  (command :type list))

(defun conn-dispatch-repeat-command ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (when command-history
    (oclosure-lambda (conn-dispatch-repeat-command
                      (command (car command-history))
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
               (eval command))
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-repeat-command)
                                        &optional
                                        short)
  (if short "Repeat Cmd"
    (format "Repeat <%s>" (car (oref action command)))))

(oclosure-define (conn-dispatch-transpose
                  (:parent conn-action)))

(defun conn-dispatch-transpose ()
  (declare (conn-dispatch-action t)
           (important-return-value t))
  (oclosure-lambda (conn-dispatch-transpose
                    (action-description "Transpose")
                    (action-always-retarget t)
                    (action-window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win)))))
                    (action-reference
                     "Transpose two things selected by dispatch."))
      ()
    (pcase-let* ((`(,pt1 ,win1 ,thing1 ,arg1 ,transform1)
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
               (window-buffer win1) pt1 thing1 arg1 transform1
               (window-buffer win2) pt2 thing2 arg2 transform2)))
        (delete-overlay ov)))))

(defun conn--dispatch-transpose-subr ( buffer1 pt1 thing1 arg1 transform1
                                       buffer2 pt2 thing2 arg2 transform2)
  (if (eq buffer1 buffer2)
      (with-current-buffer buffer1
        (save-excursion
          (pcase-let* (((and bounds1
                             (conn-bounds `(,beg1 . ,end1) transform1))
                        (progn
                          (goto-char pt1)
                          (or (conn-bounds-of thing1 arg1)
                              (user-error "Cannot find thing at point"))))
                       ((conn-bounds `(,beg2 . ,end2) transform2)
                        (progn
                          (goto-char pt2)
                          (or (conn-bounds-of (or thing2 bounds1) arg2)
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
                          (bounds1 nil)
                          (str1)
                          (str2))
      (activate-change-group cg)
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (pcase (setq bounds1 (conn-bounds-of thing1 arg1))
            ((conn-bounds `(,beg . ,end) transform1)
             (setq pt1 beg)
             (setq str1 (filter-buffer-substring beg end))
             (delete-region beg end))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer2
        (save-excursion
          (goto-char pt2)
          (pcase (conn-bounds-of (or thing2 bounds1) arg2)
            ((conn-bounds `(,beg . ,end) transform2)
             (setq str2 (filter-buffer-substring beg end))
             (delete-region beg end)
             (insert str1))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (insert str2)))
      (accept-change-group cg))))

;;;;; Dispatch Ring

(cl-defstruct (conn-previous-dispatch
               ( :constructor conn-make-dispatch
                 (action
                  &aux
                  (thing-state conn--dispatch-current-thing)
                  (restrict-windows (advice-function-member-p
                                     'conn--dispatch-restrict-windows
                                     conn-target-window-predicate))
                  (other-end (if conn-dispatch-no-other-end
                                 :no-other-end
                               conn-dispatch-other-end))
                  (always-retarget conn--dispatch-always-retarget)
                  (setup-function
                   (let ((fns (conn-target-finder-save-state
                               conn-dispatch-target-finder)))
                     (lambda ()
                       (dolist (fn fns)
                         (funcall fn conn-dispatch-target-finder)))))))
               (:copier conn--copy-previous-dispatch))
  (action nil :type conn-action)
  (thing-state nil :type list)
  (other-end nil :type symbol)
  (always-retarget nil :type boolean)
  (repeat nil :type boolean)
  (restrict-windows nil :type boolean)
  (setup-function nil :type function))

(defvar conn-dispatch-ring-max 12)

(defvar conn-dispatch-ring
  (conn-make-ring conn-dispatch-ring-max
                  :cleanup 'conn-dispatch--cleanup))

(cl-defmethod conn-dispatch-command-handler ((_cmd (eql conn-repeat-last-dispatch)))
  (if-let* ((prev (conn-ring-extract-head conn-dispatch-ring)))
      (if (conn-action-stale-p (conn-previous-dispatch-action prev))
          (progn
            (conn-dispatch-ring-remove-stale)
            (conn-read-args-error "Last dispatch action stale"))
        (conn-read-args-return
          (conn-dispatch-setup-previous
           prev
           :repeat (xor (conn-read-args-consume-prefix-arg)
                        (conn-previous-dispatch-repeat prev)))))
    (conn-read-args-error "Dispatch ring empty")))

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
  (format "%s @ %s <%s%s>"
          (conn-action-pretty-print
           (conn-previous-dispatch-action dispatch))
          (conn-thing-pretty-print
           (car (conn-previous-dispatch-thing-state dispatch)))
          (nth 1 (conn-previous-dispatch-thing-state dispatch))
          (if-let* ((ts (nth 2 (conn-previous-dispatch-thing-state dispatch))))
              (concat
               "   "
               (mapconcat (lambda (tf)
                            (or (get tf :conn-transform-description) ""))
                          ts " > "))
            "")))

(defun conn-dispatch-push-history (dispatch)
  (conn-dispatch-ring-remove-stale)
  (unless (conn-action-no-history (conn-previous-dispatch-action dispatch))
    (add-to-history 'command-history `(conn-dispatch-setup-previous
                                       (conn-ring-head conn-dispatch-ring)))
    (conn-ring-insert-front conn-dispatch-ring dispatch)))

(defun conn-dispatch-ring-remove-stale ()
  (cl-loop for stale in (seq-filter
                         (lambda (dispatch)
                           (conn-action-stale-p
                            (conn-previous-dispatch-action dispatch)))
                         (conn-ring-list conn-dispatch-ring))
           do (conn-ring-delq stale conn-dispatch-ring)))

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

(defun conn-dispatch-setup-previous (prev-dispatch &rest override-keys)
  (pcase-let (((cl-struct conn-previous-dispatch
                          (thing-state
                           `(,thing ,arg ,thing-transform))
                          action
                          other-end
                          always-retarget
                          repeat
                          restrict-windows
                          setup-function)
               prev-dispatch))
    (apply #'conn-dispatch-setup
           `( ,action ,thing ,arg ,thing-transform
              ,@override-keys
              :always-retarget ,always-retarget
              :repeat ,repeat
              :restrict-windows ,restrict-windows
              :other-end ,other-end
              :setup-function ,setup-function))))

;;;;; Dispatch Commands

(cl-defun conn-dispatch-setup (action
                               thing
                               arg
                               transform
                               &rest
                               keys
                               &key
                               repeat
                               restrict-windows
                               other-end
                               always-retarget
                               setup-function)
  (when (null action)
    (setq action (conn-make-default-action thing)))
  (when (or defining-kbd-macro executing-kbd-macro)
    (error "Dispatch not available in keyboard macros"))
  (conn-protected-let*
      ((action action (conn-cancel-action action))
       (dispatch-quit-flag nil)
       (conn-dispatch-action-reference
        (conn-action-get-reference action))
       (conn--dispatch-current-thing (list thing arg transform))
       (eldoc-display-functions nil)
       (recenter-last-op nil)
       (conn-read-args-last-command nil)
       (conn-dispatch-repeating (and repeat t))
       (conn--dispatch-prev-state
        (list conn-target-window-predicate
              conn-target-predicate
              conn-target-sort-function))
       (conn-target-window-predicate conn-target-window-predicate)
       (conn-target-predicate conn-target-predicate)
       (conn-target-sort-function conn-target-sort-function)
       (conn--dispatch-must-prompt nil)
       (conn--read-args-prefix-mag nil)
       (conn--read-args-prefix-sign nil)
       (conn--dispatch-action-always-prompt (conn-action-always-prompt action))
       (conn-dispatch-target-finder
        (conn-get-target-finder thing arg))
       (conn-dispatch-iteration-count 0)
       (conn--dispatch-always-retarget
        (or always-retarget
            (conn-action-always-retarget action)))
       (target-other-end (conn-target-finder-other-end
                          conn-dispatch-target-finder))
       (conn-dispatch-no-other-end
        (or (eq other-end :no-other-end)
            (eq target-other-end :no-other-end)))
       (conn-dispatch-other-end
        (unless conn-dispatch-no-other-end
          (xor target-other-end (or other-end conn-dispatch-other-end))))
       (conn-dispatch-input-buffer (current-buffer))
       (prev-input-method current-input-method)
       (default-input-method default-input-method)
       (input-method-history input-method-history))
    (conn-with-dispatch-event-handlers
      ( :handler #'conn-handle-dispatch-select-command)
      ( :message -99 (_)
        (when-let* ((im (buffer-local-value 'current-input-method-title
                                            conn-dispatch-input-buffer)))
          (propertize im 'face 'read-multiple-choice-face)))
      ( :message -100 (_)
        (propertize
         (cond (conn--read-args-prefix-mag
                (number-to-string
                 (* (if conn--read-args-prefix-sign -1 1)
                    conn--read-args-prefix-mag)))
               (conn--read-args-prefix-sign "[-1]")
               (t "[1]"))
         'face 'read-multiple-choice-face))
      ( :message -95 (_)
        (propertize (conn-action-pretty-print action t)
                    'face 'conn-argument-active-face))
      ( :message -90 (keymap)
        (when-let* ((binding (where-is-internal 'help keymap t)))
          (concat
           (propertize (key-description binding)
                       'face 'help-key-binding)
           " help")))
      ( :message 0 (_keymap)
        (conn-target-finder-message-prefixes
         conn-dispatch-target-finder))
      (unless conn-dispatch-no-other-end
        ( :message 10 (keymap)
          (when-let* ((_ conn-dispatch-other-end)
                      (binding
                       (where-is-internal 'other-end keymap t)))
            (concat
             (propertize (key-description binding)
                         'face 'help-key-binding)
             " "
             (propertize
              "other end"
              'face 'conn-argument-active-face)))))
      ( :message 15 (keymap)
        (when-let* ((_ (advice-function-member-p
                        'conn--dispatch-restrict-windows
                        conn-target-window-predicate))
                    (binding (where-is-internal 'restrict-windows keymap t)))
          (concat
           (propertize (key-description binding)
                       'face 'help-key-binding)
           " "
           (propertize
            "this win"
            'face 'eldoc-highlight-function-argument))))
      ( :handler (cmd)
        (pcase cmd
          ('universal-argument
           (if conn--read-args-prefix-mag
               (cl-callf * conn--read-args-prefix-mag 4)
             (setq conn--read-args-prefix-mag 4))
           (conn-dispatch-handle))
          ('reset-arg
           (setq conn--read-args-prefix-mag nil
                 conn--read-args-prefix-sign nil)
           (conn-dispatch-handle))
          ('digit-argument
           (let* ((char (if (integerp last-input-event)
                            last-input-event
                          (get last-input-event 'ascii-character)))
                  (digit (- (logand char ?\177) ?0)))
             (setf conn--read-args-prefix-mag
                   (if (integerp conn--read-args-prefix-mag)
                       (+ (* 10 conn--read-args-prefix-mag) digit)
                     digit)))
           (conn-dispatch-handle))
          ('negative-argument
           (cl-callf not conn--read-args-prefix-sign)
           (conn-dispatch-handle))))
      (when-let* ((predicate (conn-action-window-predicate action)))
        (add-function :after-while conn-target-window-predicate predicate))
      (when-let* ((predicate (conn-action-target-predicate action)))
        (add-function :after-while conn-target-predicate predicate))
      (when restrict-windows
        (add-function :after-while conn-target-window-predicate
                      'conn--dispatch-restrict-windows))
      (when-let* ((predicate (ignore-error invalid-slot-name
                               (oref conn-dispatch-target-finder
                                     window-predicate))))
        (add-function :after-while conn-target-window-predicate predicate))
      (conn-with-recursive-stack 'conn-dispatch-state
        (let ((conn-disable-input-method-hooks t))
          (conn--unwind-protect-all
            (progn
              (activate-input-method conn--input-method)
              (when setup-function (funcall setup-function))
              (conn-dispatch-perform-action action repeat)
              (conn-dispatch-push-history (conn-make-dispatch action)))
            (with-current-buffer conn-dispatch-input-buffer
              (activate-input-method prev-input-method))
            (conn-cleanup-targets)
            (conn-cleanup-labels)
            (let ((inhibit-message conn-read-args-inhibit-message))
              (message nil))))))))

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
                   :command-handler #'conn-dispatch-command-handler
                   :prefix current-prefix-arg
                   :prompt "Dispatch"
                   :reference (conn-dispatch-reference)
                   :pre (lambda (_)
                          (when (and (bound-and-true-p conn-posframe-mode)
                                     (fboundp 'posframe-hide))
                            (posframe-hide " *conn-list-posframe*"))))
      ((`(,thing ,arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument))
       (other-end (conn-boolean-argument "other-end"
                                         'other-end
                                         conn-other-end-argument-map))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map))
       (`(,action ,repeat) (conn-dispatch-action-argument)))
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
  (cl-letf ((conn--dispatch-action-always-prompt t)
            ((symbol-function 'conn-get-target-finder)))
    (advice-add 'conn-get-target-finder :override
                (lambda (cmd arg)
                  (pcase (ignore-errors (conn-bounds-of cmd arg))
                    ((conn-bounds `(,beg . ,end))
                     (conn-dispatch-focus-thing-at-point
                      :string (buffer-substring-no-properties beg end))))))
    (conn-read-args (conn-dispatch-thingatpt-state
                     :prefix current-prefix-arg
                     :command-handler #'conn-dispatch-command-handler
                     :prompt "Dispatch"
                     :reference (conn-dispatch-reference)
                     :pre (lambda (_)
                            (when (and (bound-and-true-p conn-posframe-mode)
                                       (fboundp 'posframe-hide))
                              (posframe-hide " *conn-list-posframe*"))))
        ((`(,thing ,arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument))
         (other-end
          (conn-boolean-argument "other-end"
                                 'other-end
                                 conn-other-end-argument-map))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map))
         (`(,action ,repeat) (conn-dispatch-action-argument)))
      (conn-dispatch-setup
       action thing arg transform
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
    (conn-dispatch-setup-previous
     prev
     :repeat (xor invert-repeat (conn-previous-dispatch-repeat prev)))))

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
  (let* ((key-seq (read-key-sequence-vector
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
     'button
     :pretty-print ( :method (_self) "all-buttons")
     :target-finder ( :method (_self _arg)
                      (conn-dispatch-button-targets))
     :has-other-end-p ( :method (_self) :no-other-end))
   nil nil))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (cl-flet ((target-thing (beg end)
              (conn-anonymous-thing
                'point
                :bounds-op ( :method (_self _arg)
                             (conn-make-bounds
                              'point nil
                              (cons beg end))))))
    (let ((targets (with-restriction (window-start) (window-end)
                     (conn--isearch-matches))))
      (unwind-protect ;In case this was a recursive isearch
          (isearch-exit)
        (conn-dispatch-setup
         (conn-dispatch-goto)
         (conn-anonymous-thing
           'region
           :target-finder ( :method (_self _arg)
                            (lambda ()
                              (cl-loop for (beg . end) in targets
                                       do (conn-make-target-overlay
                                           beg (- end beg)
                                           :thing (target-thing beg end))))))
         nil nil
         :restrict-windows t
         :other-end nil)))))

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
      ((`(,thing ,arg) (conn-thing-argument t))
       (transform (conn-dispatch-transform-argument))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               subregions-p)))
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
                        (conn-dispatch-undo-case 0
                          ((or :cancel :undo) (delete-overlay (pop ovs))))
                        (overlay-put (car ovs) 'face 'region))
                      (push bound subregions)
                      (conn-dispatch-undo-case 0
                        ((or :cancel :undo) (pop subregions))))
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
   (oclosure-lambda (conn-bounds-delay)
       (bounds)
     (conn--dispatch-bounds bounds))
   :subregions (oclosure-lambda (conn-bounds-delay)
                   (bounds)
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
                (conn-transformed-bounds-transforms bounds))
              :other-end nil))
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
    (conn-dispatch-setup-previous
     prev
     :repeat (xor arg (conn-previous-dispatch-repeat prev)))))

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
                                      _arg)
  (conn-dispatch-all-defuns
   :reference (conn-reference-quote
                ((:heading "Defun Targets")
                 "Dispatch on defuns.  Hides buffer regions outside defun definition
lines."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing visual-line))
                                      _arg)
  (conn-dispatch-visual-line-targets
   :reference (conn-reference-quote
                ((:heading "Visual Lines Targets")
                 "Dispatch on visual lines."))))

(conn-register-thing-commands 'dispatch nil 'conn-dispatch)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing symbol))
                                      _arg)
  (conn-dispatch-things-read-prefix 'symbol 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing word))
                                      _arg)
  (conn-dispatch-things-read-prefix 'word 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sexp))
                                      _arg)
  (conn-dispatch-things-read-prefix 'sexp 1))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing sentence))
                                      _arg)
  (conn-all-things-targets
   :thing 'sentence
   :reference (conn-reference-quote
                ((:heading "Sentence Targets")
                 "Dispatch on sentences."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing paragraph))
                                      _arg)
  (conn-all-things-targets
   :thing 'paragraph
   :reference (conn-reference-quote
                ((:heading "Paragraph Targets")
                 "Dispatch on paragraphs."))))

(cl-defmethod conn-get-target-finder ((_cmd (eql forward-char))
                                      _arg)
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :reference (conn-reference-quote
                ((:heading "String Targets")
                 "Display on matches for a string read with a timeout.  Other end puts
point at the end of a match."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line))
                                      _arg)
  (conn-dispatch-line-targets
   :reference (conn-reference-quote
                ((:heading "Line Targets")
                 "Dispatch on a line."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column))
                                      _arg)
  (conn-dispatch-column-targets
   :reference (conn-reference-quote
                ((:heading "Column Targets")
                 "Dispatch on a column.  Bounds are from point to selected column."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing outer-line))
                                      _arg)
  (conn-dispatch-line-targets
   :reference (conn-reference-quote
                ((:heading "Outer Line Targets")
                 "Dispatch on an outer line."))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing inner-line))
                                      _arg)
  (conn-dispatch-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an inner line."))))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line))
                                      _arg)
  (conn-dispatch-end-of-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an outer line.  Other end defaults to non-nil."))))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line-dwim))
                                      _arg)
  (conn-dispatch-end-of-inner-line-targets
   :reference (conn-reference-quote
                ((:heading "Inner Line Targets")
                 "Dispatch on an outer line.  Other end defaults to non-nil."))))

(provide 'conn-dispatch)
