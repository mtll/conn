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
(require 'conn-mark)
(require 'conn-things)
(require 'conn-kapply)
(eval-when-compile
  (require 'cl-lib)
  (require 'map))

(defvar outline-heading-end-regexp)
(defvar treesit-defun-type-regexp)

(declare-function face-remap-remove-relative "face-remap")
(declare-function conn--kmacro-display "conn-transient")
(declare-function kmacro--keys "kmacro")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")
(declare-function conn-scroll-up "conn-commands")
(declare-function conn-scroll-down "conn-commands")
(declare-function conn-register-load "conn-commands")
(declare-function conn-end-of-inner-line "conn-commands")
(declare-function conn-beginning-of-inner-line "conn-commands")
(declare-function conn-dispatch-kapply-prefix "conn-transients")
(declare-function conn-kill-thing "conn-commands")

;;;; Labels

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e"
        "r" "t" "y" "u" "i" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defface conn-dispatch-action-pulse-face
  '((t (:inherit pulse-highlight-start-face)))
  "Face for highlight pulses after dispatch actions."
  :group 'conn-faces)

(defface conn-dispatch-undo-pulse-face
  '((t (:background "#afb28dfb8dfb")))
  "Face for highlight pulses after dispatch actions."
  :group 'conn-faces)

(defface conn--dispatch-action-current-pulse-face
  '((t (:inherit conn-dispatch-undo-pulse-face)))
  "Face for current action pulse, do not customize."
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

(defvar conn-window-labeling-function 'conn-header-line-label
  "Function to label windows for `conn-prompt-for-window'.

The function should accept a single argument, the list of windows to be
labeled and it should return a list of structs for `conn-label-select',
which see.")

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
                 ( string window
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
  (let ((buckets (make-vector 4 nil)))
    (setf (aref buckets 0) (thread-last
                             (take count conn-simple-label-characters)
                             (reverse)
                             (mapcar #'copy-sequence)))
    (named-let loop ((curr 0))
      (let* ((prefixes nil))
        (while (and (aref buckets curr)
                    (> count (+ (length (aref buckets curr))
                                (* (length prefixes)
                                   (length conn-simple-label-characters)))))
          (push (pop (aref buckets curr)) prefixes))
        (if (and (null (aref buckets curr)) (> count 0))
            (progn
              (dolist (a prefixes)
                (dolist (b conn-simple-label-characters)
                  (push (concat a b) (aref buckets (1+ curr)))))
              (loop (1+ curr)))
          (catch 'done
            (let ((n (length (aref buckets curr))))
              (dolist (prefix prefixes)
                (dolist (c conn-simple-label-characters)
                  (push (concat prefix c) (aref buckets (1+ curr)))
                  (when (= (cl-incf n) count)
                    (throw 'done nil))))))
          (let ((result (cl-loop for bucket across buckets
                                 nconc (nreverse bucket))))
            result))))))

;;;;; Label Reading

(defvar conn--dispatch-event-handler-maps nil)
(defvar conn--dispatch-read-event-handlers nil)
(defvar conn--dispatch-read-event-message-prefixes nil)

(defmacro conn-with-dispatch-event-handler (handler-args &rest body)
  "Add an event handler for dispatch.

\(fn (&key HANDLER KEYMAP MESSAGE-FN) &body BODY)"
  (declare (indent 1))
  (pcase-let* (((map :handler :keymap :message-fn)
                handler-args)
               (tag (make-symbol "handle"))
               (menv `((:return . ,(lambda (&optional result)
                                     `(throw ',tag ,result)))
                       ,@macroexpand-all-environment))
               (handler (macroexpand-all handler menv))
               (body (macroexpand-all (macroexp-progn body) menv)))
    `(let (,@(when keymap
               `((conn--dispatch-event-handler-maps
                  (cons ,keymap conn--dispatch-event-handler-maps))))
           ,@(when message-fn
               `((conn--dispatch-read-event-message-prefixes
                  (cons ,message-fn conn--dispatch-read-event-message-prefixes))))
           (conn--dispatch-read-event-handlers
            (cons ,handler conn--dispatch-read-event-handlers)))
       (catch ',tag ,@(macroexp-unprogn body)))))

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

(cl-defgeneric conn-label-redisplay (label)
  "Redisplay LABEL."
  (:method (_) "Noop" nil))

(cl-defgeneric conn-label-reset (label)
  "Reset LABEL to its initial state.")

(cl-defgeneric conn-label-payload (label)
  "Return LABEL\'s payload."
  (declare (important-return-value t)))

(cl-defmethod conn-label-payload ((label conn-dispatch-label))
  (let ((overlay (conn-dispatch-label-target label)))
    (list (overlay-start overlay)
          (overlay-get overlay 'window)
          (overlay-get overlay 'thing))))

(cl-defmethod conn-label-reset ((label conn-dispatch-label))
  (setf (conn-dispatch-label-narrowed-string label)
        (conn-dispatch-label-string label)))

(cl-defmethod conn-label-delete ((label conn-dispatch-label))
  (delete-overlay (conn-dispatch-label-overlay label)))

(cl-defmethod conn-label-narrow ((label conn-dispatch-label) prefix-char)
  (if (thread-first
        (conn-dispatch-label-narrowed-string label)
        (aref 0) (eql prefix-char) not)
      (setf (conn-dispatch-label-narrowed-string label) nil)
    (cl-callf substring (conn-dispatch-label-narrowed-string label) 1)
    label))

(cl-defmethod conn-label-redisplay ((label conn-dispatch-label))
  (pcase-let (((cl-struct conn-dispatch-label
                          overlay
                          target
                          narrowed-string
                          setup-function)
               label))
    (with-current-buffer (overlay-buffer overlay)
      (if (length> narrowed-string 0)
          (progn
            (overlay-put overlay 'display nil)
            (overlay-put overlay 'before-string nil)
            (overlay-put overlay 'after-string nil)
            (funcall setup-function label)
            (overlay-put target 'face 'conn-target-overlay-face))
        (move-overlay overlay (overlay-start overlay) (overlay-start overlay))
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'after-string nil)
        (overlay-put overlay 'before-string nil)
        (overlay-put target 'after-string nil)
        (overlay-put target 'face nil)))))

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

(cl-defmethod conn-label-narrow ((label conn-window-label) prefix-char)
  (pcase-let* (((cl-struct conn-window-label window) label)
               (string (window-parameter window 'conn-label-string)))
    (unless (or (length= string 0)
                (not (eql prefix-char (aref string 0))))
      (set-window-parameter window 'conn-label-string (substring string 1))
      label)))

(defvar conn-label-select-always-prompt nil)

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
       (`(,it . nil)
        (unless prompt-flag
          (cl-return (conn-label-payload it)))))
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
           (label-width (string-pixel-width label))
           (padding-width (floor (- window-width label-width) 2))
           (padding (propertize " " 'display `(space :width (,padding-width)))))
      (concat padding label))))

(defvar conn--window-label-pool
  (conn-simple-labels 30))

(defun conn--ensure-window-labels ()
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

(defun conn-header-line-label (window string)
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

;; From ace-window
(defun conn--get-windows (&optional window
                                    minibuffer
                                    all-frames
                                    dedicated
                                    predicate)
  (declare (important-return-value t))
  (cl-loop for win in (window-list-1 window minibuffer all-frames)
           unless (or ;; ignore child frames
                   (and (fboundp 'frame-parent) (frame-parent (window-frame window)))
                   ;; When `ignore-window-parameters' is nil, ignore windows whose
                   ;; `no-other-window’ or `no-delete-other-windows' parameter is non-nil.
                   (unless ignore-window-parameters
                     (window-parameter window 'no-other-window))
                   (and (null dedicated) (window-dedicated-p win))
                   (and predicate (not (funcall predicate win))))
           collect win))

(defun conn-prompt-for-window (windows &optional always-prompt)
  "Label and prompt for a window among WINDOWS."
  (declare (important-return-value t))
  (cond
   ((null windows) nil)
   (t
    (conn--ensure-window-labels)
    (let ((labels
           (cl-loop for win in windows
                    collect (funcall conn-window-labeling-function
                                     win (window-parameter win 'conn-label-string))))
          (conn-label-select-always-prompt always-prompt))
      (unwind-protect
          (conn-with-dispatch-event-handler
              (:handler
               (lambda (cmd)
                 (when (or (and (eq cmd 'act)
                                (mouse-event-p last-input-event))
                           (eq 'dispatch-mouse-repeat
                               (event-basic-type last-input-event)))
                   (let* ((posn (event-start last-input-event))
                          (win (posn-window posn)))
                     (when (and (not (posn-area posn))
                                (funcall conn-target-window-predicate win))
                       (:return win))))))
            (conn-label-select labels #'conn-dispatch-read-event))
        (mapc #'conn-label-delete labels))))))

;;;; Dispatch State

(defface conn-dispatch-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a dispatch state."
  :group 'conn-faces)

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

(defvar conn-dispatch-read-event-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map)
                          (cons #x100 (max-char))
                          'dispatch-character-event)
    (cl-loop for i from ?\s below 256
             do (define-key map (vector i) 'dispatch-character-event))
    (keymap-set map "C-g" 'keyboard-quit)
    map))

(conn-define-state conn-dispatch-targets-state (conn-read-thing-common-state)
  "State for reading a dispatch command."
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(cl-defmethod conn-enter-state :around ((_state (conn-substate conn-dispatch-targets-state)))
  (if (or defining-kbd-macro executing-kbd-macro)
      (error "Dispatch not available in keyboard macros")
    (cl-call-next-method)))

(conn-define-state conn-dispatch-bounds-state (conn-dispatch-targets-state)
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face)

(conn-define-state conn-dispatch-state (conn-dispatch-targets-state)
  "State for reading a dispatch command.")

(conn-define-state conn-dispatch-thingatpt-state (conn-dispatch-state))

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(defvar conn--dispatch-thing-predicate nil)

(oclosure-define (conn-dispatch-target-argument
                  (:parent conn-thing-argument)))

(defun conn-dispatch-target-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-target-argument
                    (required t)
                    (recursive-edit t))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-target-argument)
                                       sym)
  (and (cl-call-next-method)
       (funcall conn--dispatch-thing-predicate sym)))

(defvar-keymap conn-dispatch-transform-map
  "V" 'conn-dispatch-bounds-between
  "x" 'conn-bounds-trim
  "v" 'conn-dispatch-bounds-over
  "X" 'conn-transform-reset)

(defun conn-dispatch-transform-argument (&optional initial)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-transform-argument
                    (value initial)
                    (keymap conn-dispatch-transform-map))
      (self cmd)
    (let* ((next (conn-transform-command-handler cmd value)))
      (pcase cmd
        ('conn-transform-reset
         (conn-set-argument self nil))
        ((guard (not (eq next value)))
         (conn-set-argument self next))
        (_ self)))))

(defvar-keymap conn-dispatch-separator-argument-map
  "TAB" 'separator
  "M-TAB" 'unset-separator)

(oclosure-define (conn-dispatch-separator-argument
                  (:parent conn-read-args-argument)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-separator-argument)
                                       sym)
  (or (eq sym 'separator)
      (eq sym 'unset-separator)
      (cl-call-next-method)))

(cl-defmethod conn-argument-display ((arg conn-dispatch-separator-argument))
  (when-let* ((sep (conn-read-args-argument-value arg)))
    (propertize (if (eq sep 'default)
                    "Separator: default"
                  (format "Separator: <%s>" sep))
                'face 'eldoc-highlight-function-argument)))

(defvar conn-separator-history nil)

(defun conn-dispatch-separator-argument (&optional value)
  (oclosure-lambda (conn-dispatch-separator-argument
                    (value value)
                    (keymap conn-dispatch-separator-argument-map))
      (self cmd)
    (pcase cmd
      ('unset-separator
       (conn-read-args-consume-prefix-arg)
       (conn-set-argument self nil))
      ('separator
       (conn-set-argument
        self
        (if (eq value 'default)
            (read-string "Separator: " nil 'conn-separator-history)
          'default)))
      (_ self))))

;;;;;; Dispatch Quick Ref

(defvar conn-dispatch-thing-ref-list
  (conn-reference-quote
    (("symbol" forward-symbol)
     ("line" forward-line)
     ("column" next-line)
     ("defun"
      (:eval (conn-quick-ref-find-remap
              conn-end-of-defun-remap
              (conn-get-state-map 'conn-dispatch-targets-state)))))))

(defvar conn-dispatch-thing-transforms-ref-list
  (conn-reference-quote
    (("between" conn-dispatch-bounds-between)
     ("trim" conn-bounds-trim)
     ("over" conn-dispatch-bounds-over)
     ("reset" conn-transform-reset))))

(defvar conn-dispatch-thing-ref
  (conn-reference-page "Things"
    (:heading "Extra Thing Bindings")
    ((:keymap (list (conn-get-state-map 'conn-dispatch-targets-state)))
     (:splice (conn-quick-ref-to-cols conn-dispatch-thing-ref-list)))
    (:heading "Transforms")
    ((:splice (conn-quick-ref-to-cols conn-dispatch-thing-transforms-ref-list)))))

(defvar conn-dispatch-action-ref-list
  (conn-reference-quote
    (("copy from/replace"
      conn-dispatch-copy-from
      conn-dispatch-copy-from-replace)
     ("send/replace"
      conn-dispatch-send
      conn-dispatch-send-replace)
     ("kapply" conn-dispatch-kapply)
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
      conn-dispatch-take
      conn-dispatch-take-replace))))

(defvar conn-dispatch-action-ref
  (conn-reference-page "Actions"
    ((:splice (conn-quick-ref-to-cols conn-dispatch-action-ref-list 3)))))

(defvar conn-dispatch-command-ref
  (conn-reference-page "Misc Commands"
    (((:heading "History:")
      ("next/prev"
       conn-dispatch-cycle-ring-next
       conn-dispatch-cycle-ring-previous))
     ((:heading "Last Dispatch:")
      ("repeat" conn-repeat-last-dispatch)
      ("describe" conn-dispatch-ring-describe-head)))
    (((:heading "Other Args")
      ("dispatch repeatedly" repeat-dispatch)
      ("exchange point and mark after selecting THING"
       dispatch-other-end)
      ("restrict matches to the selected window"
       restrict-windows)))))

(defvar conn-dispatch-select-ref
  (conn-reference-page "Selection Commands"
    (((:heading "Targeting Commands")
      ("retarget" retarget)
      ("always retarget" always-retarget)
      ("change target finder" change-target-finder))
     ((:heading "Window Commands")
      ("goto window" conn-goto-window)
      ("scroll up" scroll-up)
      ("scroll down" scroll-down)
      ("restrict" restrict-windows)))
    (((:heading "Misc")
      ("dispatch at click" act)
      ("undo" undo)
      ("recursive edit" recursive-edit))
     (""
      ("isearch forward" isearch-regexp-forward)
      ("isearch backward" isearch-regexp-backward)))))

(defvar conn-dispatch-reference
  (list conn-dispatch-action-ref
        conn-dispatch-command-ref
        conn-dispatch-thing-ref))

(defvar conn-dispatch-target-reference
  (list conn-dispatch-thing-ref))

;;;;;; Action

(oclosure-define (conn-dispatch-action-argument
                  (:parent conn-read-args-argument)
                  (:copier conn-set-action-argument (repeat value)))
  (repeat :type boolean))

(defvar-keymap conn-dispatch-action-map
  "TAB" 'repeat-dispatch
  "'" 'conn-dispatch-kapply
  "t" 'conn-dispatch-copy-to
  "T" 'conn-dispatch-copy-to-replace
  "C-y" 'conn-dispatch-yank-to-replace
  "M-y" 'conn-dispatch-reading-yank-to-replace
  "y" 'conn-dispatch-yank-to
  "Y" 'conn-dispatch-reading-yank-to
  "f" 'conn-dispatch-copy-from
  "F" 'conn-dispatch-copy-from-replace
  "s" 'conn-dispatch-send
  "S" 'conn-dispatch-send-replace
  "d" 'conn-dispatch-take
  "D" 'conn-dispatch-take-replace
  "q" 'conn-dispatch-transpose
  "." 'conn-dispatch-register-load
  ">" 'conn-dispatch-register-load-replace)

(defun conn-dispatch-action-argument ()
  (declare (important-return-value t))
  (setq conn--dispatch-thing-predicate #'always)
  (oclosure-lambda (conn-dispatch-action-argument
                    (keymap conn-dispatch-action-map))
      (self cmd)
    (pcase cmd
      ('repeat-dispatch
       (conn-set-action-argument self (not repeat) value))
      ((guard (conn-argument-predicate self cmd))
       (conn-cancel-action value)
       (condition-case err
           (if-let* ((_(not (cl-typep value cmd)))
                     (action (conn-make-action cmd)))
               (progn
                 (setq conn--dispatch-thing-predicate
                       (or (conn-action--action-thing-predicate action)
                           #'always))
                 (conn-set-action-argument
                  self
                  (pcase repeat
                    ((or 'auto 'nil)
                     (and (conn-action--action-auto-repeat action)
                          'auto))
                    (_ repeat))
                  action))
             (conn-set-action-argument self repeat nil))
         (error
          (conn-read-args-error (error-message-string err))
          (conn-set-action-argument self repeat nil))))
      (_ self))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-action-argument))
  (conn-cancel-action (conn-read-args-argument-value arg)))

(cl-defmethod conn-argument-value ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-read-args-argument-value arg)))
    (conn-accept-action action))
  (list (conn-read-args-argument-value arg)
        (conn-dispatch-action-argument--repeat arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-action-argument)
                                       sym)
  (conn--action-type-p sym))

(cl-defmethod conn-argument-completion-annotation ((_arg conn-dispatch-action-argument)
                                                   sym)
  (when (conn--action-type-p sym)
    " (action)"))

(cl-defmethod conn-argument-display ((arg conn-dispatch-action-argument))
  (list
   (concat "\\[repeat-dispatch] "
           (propertize
            "repeat"
            'face (when (conn-dispatch-action-argument--repeat arg)
                    'eldoc-highlight-function-argument)))
   (when-let* ((action (conn-read-args-argument-value arg)))
     (propertize (conn-action-pretty-print action)
                 'face 'eldoc-highlight-function-argument))))

;;;;;; Other End

(defvar-keymap conn-dispatch-other-end-map
  "z" 'dispatch-other-end)

(oclosure-define (conn-dispatch-other-end-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-other-end-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-other-end-argument
                    (value value)
                    (keymap conn-dispatch-other-end-map))
      (self cmd)
    (if (eq cmd 'dispatch-other-end)
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-other-end-argument)
                                       (_sym (eql dispatch-other-end)))
  t)

(cl-defmethod conn-argument-display ((arg conn-dispatch-other-end-argument))
  (concat "\\[dispatch-other-end] "
          (propertize "other-end"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Repeat

(defvar-keymap conn-dispatch-repeat-arg-map
  "TAB" 'repeat-dispatch)

(oclosure-define (conn-dispatch-repeat-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-repeat-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-repeat-argument
                    (value value)
                    (keymap conn-dispatch-repeat-arg-map))
      (self cmd)
    (pcase cmd
      ('repeat-dispatch
       (conn-set-argument self (not value)))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-repeat-argument)
                                       (_sym (eql dispatch-repeat)))
  t)

(cl-defmethod conn-argument-display ((arg conn-dispatch-repeat-argument))
  (concat "\\[repeat-dispatch] "
          (propertize "repeat"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Restrict Windows

(defvar-keymap conn-dispatch-restrict-windows-map
  "C-w" 'restrict-windows)

(oclosure-define (conn-dispatch-restrict-windows-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-restrict-windows-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-restrict-windows-argument
                    (value value)
                    (keymap conn-dispatch-restrict-windows-map))
      (self cmd)
    (if (eq cmd 'restrict-windows)
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-restrict-windows-argument)
                                       (_sym (eql restrict-windows)))
  t)

(cl-defmethod conn-argument-display ((arg conn-dispatch-restrict-windows-argument))
  (concat "\\[restrict-windows] "
          (propertize "this-win"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Command Handler

(cl-defgeneric conn-dispatch-command-handler (cmd)
  ( :method (_)))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-cycle-ring-next)))
  (condition-case _
      (progn
        (conn-dispatch-cycle-ring-next)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch
                                        (conn-ring-head conn-dispatch-ring))))
        (conn-read-args-handle))
    (user-error (conn-read-args-error "Dispatch ring empty"))))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-cycle-ring-previous)))
  (condition-case _
      (progn
        (conn-dispatch-cycle-ring-previous)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch
                                        (conn-ring-head conn-dispatch-ring))))
        (conn-read-args-handle))
    (user-error (conn-read-args-error "Dispatch ring empty"))))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-ring-describe-head)))
  (conn-dispatch-ring-remove-stale)
  (if-let* ((head (conn-ring-head conn-dispatch-ring)))
      (progn
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch head)))
        (conn-read-args-handle))
    (conn-read-args-error "Dispatch ring empty")))

;;;;; Bounds of Dispatch

(defun conn-bounds-of-dispatch (thing arg location)
  (when-let* ((bounds (save-excursion
                        (goto-char location)
                        (conn-bounds-of thing arg))))
    (setf (conn-bounds-get bounds :origin) (point))
    bounds))

(put 'conn-dispatch-bounds-over :conn-bounds-transformation t)
(put 'conn-dispatch-bounds-over :conn-transform-description "over")

(cl-defgeneric conn-dispatch-bounds-over (bounds)
  (declare (important-return-value t)
           (conn-anonymous-thing-property :over)))

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

(put 'conn-dispatch-bounds-between :conn-bounds-transformation t)
(put 'conn-dispatch-bounds-between :conn-transform-description "between")

(cl-defgeneric conn-dispatch-bounds-between (bounds)
  (declare (important-return-value t)
           (conn-anonymous-thing-property :dispatch-between)))

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
        (cons (if conn-dispatch-other-end end beg)
              origin)))
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
       (conn-make-bounds 'point (conn-bounds-arg bounds)
                         (cons (if conn-dispatch-other-end end beg)
                               origin)))
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
                  (overlay-get ov 'label-face)
                  (overlay-get o 'label-face))
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
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-matches string predicate))
          (conn-make-target-overlay
           beg (or fixed-length (- end beg))
           :thing thing))))))

(defun conn-make-re-target-overlays (regexp
                                     &optional
                                     predicate
                                     fixed-length
                                     thing)
  (when (length> regexp 0)
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches regexp predicate))
          (conn-make-target-overlay
           beg (or fixed-length (- end beg))
           :thing thing))))))

;;;;; Dispatch Labels

(defvar conn-dispatch-target-finder nil)

(defvar conn-dispatch-label-function 'conn-dispatch-simple-labels
  "Function responsible for labeling all `conn-targets'.

A labeling function should take a single argument STATE and
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

(defun conn--right-justify-padding (overlay width face)
  (when (> width 0)
    (overlay-put overlay 'after-string
                 (propertize " "
                             'display `(space :width (,width))
                             'face face))))

(defun conn--left-justify-padding (overlay width face)
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
               (full-string (concat prefix string suffix)))
    (let* ((window (overlay-get overlay 'window))
           (display-width
            (conn--string-pixel-width full-string (window-buffer window)))
           (padding-width 0)
           ;; display-line-numbers, line-prefix and wrap-prefix break
           ;; width calculations, temporarily disable them.
           (old-state (buffer-local-set-state
                       display-line-numbers nil
                       line-prefix nil
                       wrap-prefix nil)))
      (unwind-protect
          (progn
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
                ;; exceptional conditions, which see the test clauses of
                ;; the following cond form, we want the label overlay to
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
        (buffer-local-restore-state old-state)))))

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

(cl-defstruct (conn-simple-label-state)
  (pool nil :type list)
  (in-use nil :type hash-table)
  (size 0 :type fixnum))

(defun conn-dispatch-simple-labels (state)
  "Create simple labels for all targets."
  (declare (important-return-value t))
  (unless state
    (setq state (make-conn-simple-label-state
                 :pool nil
                 :in-use (make-hash-table :test 'equal)
                 :size 0)))
  (cl-symbol-macrolet ((pool (conn-simple-label-state-pool state))
                       (in-use (conn-simple-label-state-in-use state))
                       (size (conn-simple-label-state-size state)))
    (let ((sel-win (selected-window))
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
          (puthash str t in-use))
        (setf size (max (ceiling (* 1.8 count))
                        (let ((len (length conn-simple-label-characters)))
                          (+ (- len 3)
                             (* (- len 3) 3))))
              pool (conn-simple-labels size))
        (dolist (str pool)
          (remhash str in-use)))
      (pcase-dolist (`(,win . ,targets) conn-targets)
        (when-let* ((ov (buffer-local-value 'conn--mark-cursor
                                            (window-buffer win))))
          (delete-overlay ov))
        (dolist (tar (if (eq win (selected-window))
                         (compat-call sort targets
                                      :lessp conn-target-sort-function)
                       targets))
          (if-let* ((str (overlay-get tar 'label-string)))
              ;; Try to reuse a target's existing label.
              (progn
                (when (gethash str in-use)
                  ;; This target has had its label string reused as a
                  ;; prefix for new labels, ensure that it gets a new
                  ;; label that has its old label as a prefix.
                  (setf str (concat str (car conn-simple-label-characters))
                        (overlay-get tar 'label-string) str))
                (puthash str t in-use)
                (unless (and (eq win sel-win)
                             (<= (overlay-start tar)
                                 (point)
                                 (overlay-end tar)))
                  (push (conn-dispatch-create-label tar str) labels)))
            (push tar unlabeled))))
      (cl-callf nreverse unlabeled)
      (cl-loop for str in pool
               while unlabeled
               unless (gethash str in-use)
               do (let ((tar (pop unlabeled)))
                    (overlay-put tar 'label-string str)
                    (unless (and (eq (overlay-get tar 'window) sel-win)
                                 (<= (overlay-start tar)
                                     (point)
                                     (overlay-end tar)))
                      (push (conn-dispatch-create-label tar str) labels))))
      `(:state ,state ,@labels))))

(defun conn--dispatch-read-event-prefix (keymap)
  (declare (important-return-value t))
  (and-let* ((prefix
              (flatten-tree
               (cl-loop for pfx in conn--dispatch-read-event-message-prefixes
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

(defmacro conn-with-dispatch-labels (binder &rest body)
  (declare (indent 1))
  `(progn
     (conn-cleanup-labels)
     (let (,binder)
       (unwind-protect
           (progn ,@body)
         (clrhash conn--pixelwise-window-cache)
         (clrhash conn--dispatch-window-lines-cache)
         (let ((fn (make-symbol "cleanup")))
           (fset fn (lambda (&rest _)
                      (unwind-protect
                          (mapc #'conn-label-delete ,(car binder))
                        (setq conn--previous-labels-cleanup nil)
                        (remove-hook 'pre-redisplay-functions fn))))
           (add-hook 'pre-redisplay-functions fn)
           (setq conn--previous-labels-cleanup fn))))))

(cl-defgeneric conn-target-finder-select (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-target-finder-select :before (target-finder)
  (conn-cleanup-labels)
  (when conn--dispatch-always-retarget
    (conn-target-finder-retarget target-finder))
  (let ((old nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(_ . ,targets) conn-targets)
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
  (let ((conn--dispatch-remap-cookies nil))
    (conn-with-dispatch-event-handler
        (:handler
         (lambda (cmd)
           (when (or (and (eq cmd 'act)
                          (mouse-event-p last-input-event))
                     (eq 'dispatch-mouse-repeat
                         (event-basic-type last-input-event)))
             (let* ((posn (event-start last-input-event))
                    (win (posn-window posn))
                    (pt (posn-point posn)))
               (when (and (not (posn-area posn))
                          (funcall conn-target-window-predicate win))
                 (:return (list pt win nil)))))))
      (conn-dispatch-select-mode 1)
      (unwind-protect
          (let ((inhibit-message t))
            (cl-call-next-method))
        (conn-dispatch-select-mode -1)))))

(cl-defmethod conn-target-finder-select (_target-finder)
  (conn-with-dispatch-labels
      (labels (pcase (funcall conn-dispatch-label-function
                              conn--dispatch-label-state)
                (`(:state ,state . ,labels)
                 (setq conn--dispatch-label-state state)
                 labels)
                (labels labels)))
    (conn-label-select
     labels
     #'conn-dispatch-read-event
     (cl-loop for (_ . c) in conn-target-count
              sum c into count
              finally return (format "Label [%s]" count))
     (conn-dispatch-prompt-p))))

;;;;; Dispatch Loop

(defvar conn-dispatch-in-progress nil)
(defvar conn--dispatch-undo-change-groups nil)

(defvar conn--dispatch-current-thing nil)

(define-minor-mode conn-dispatch-select-mode
  "Mode for dispatch event reading"
  :global t
  :lighter " SELECT"
  :group 'conn
  (if conn-dispatch-select-mode
      (progn
        (with-memoization (alist-get (current-buffer) conn--dispatch-remap-cookies)
          (face-remap-add-relative
           'mode-line
           (conn-state-get 'conn-dispatch-state :mode-line-face)))
        (setq conn--hide-mark-cursor t))
    (setq conn--hide-mark-cursor nil)
    (unwind-protect
        (conn-cleanup-labels)
      (pcase-dolist (`(,buf . ,cookie) conn--dispatch-remap-cookies)
        (with-current-buffer buf
          (face-remap-remove-relative cookie))))))

(defun conn--dispatch-loop (repeat body)
  (let* ((success nil)
         (conn--dispatch-label-state nil)
         (conn-dispatch-repeating (and repeat t))
         (conn--dispatch-undo-change-groups nil)
         (conn--read-args-error-message nil)
         (conn--dispatch-read-event-message-prefixes
          `(,(car conn--dispatch-read-event-message-prefixes)
            ,@(cdr conn--dispatch-read-event-message-prefixes))))
    (unwind-protect
        (progn
          (redisplay)
          (catch 'dispatch-select-exit
            (let ((conn-dispatch-in-progress t))
              (while (or conn-dispatch-repeating
                         (< conn-dispatch-iteration-count 1))
                (catch 'dispatch-redisplay
                  (condition-case err
                      (progn
                        (push nil conn--dispatch-undo-change-groups)
                        (funcall body)
                        (cl-incf conn-dispatch-iteration-count))
                    (user-error
                     (pcase-dolist (`(,_ . ,undo-fn)
                                    (pop conn--dispatch-undo-change-groups))
                       (funcall undo-fn :undo))
                     (setf conn--read-args-error-message
                           (error-message-string err))))))))
          (setq success (not dispatch-quit-flag)))
      (dolist (undo conn--dispatch-undo-change-groups)
        (pcase-dolist (`(,_ . ,undo-fn) undo)
          (funcall undo-fn (if success :accept :cancel)))))
    (when dispatch-quit-flag (keyboard-quit))))

(defmacro conn-dispatch-loop (repeat &rest body)
  (declare (indent 1))
  `(conn--dispatch-loop ,repeat (lambda () ,@body)))

(defun conn-select-target ()
  (catch 'return
    (while t
      (funcall
       (catch 'dispatch-change-target-finder
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
           (throw 'return
                  (list pt win (or thing-override thing) arg transform))))))))

(defmacro conn-dispatch-change-target-finder (&rest body)
  (declare (indent 0))
  `(throw 'dispatch-change-target-finder
          (lambda () ,@body)))

(defun conn-dispatch-action-pulse (beg end)
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
        (car conn--dispatch-undo-change-groups))
  (conn--compat-callf sort (car conn--dispatch-undo-change-groups)
    :key #'car
    :in-place t))

(defmacro conn-dispatch-undo-case (depth &rest body)
  (declare (indent 1))
  (cl-assert (<= -100 depth 100))
  (cl-with-gensyms (do buf)
    `(conn--dispatch-push-undo-case
      ,depth
      (let ((,buf (current-buffer)))
        (lambda (,do)
          (with-current-buffer ,buf
            (pcase ,do ,@body)))))))

(defun conn-dispatch-loop-undo-boundary (&rest buffers)
  (unless buffers (setq buffers (list (current-buffer))))
  (when conn-dispatch-in-progress
    (let ((cg (mapcan #'prepare-change-group
                      (or buffers (list (current-buffer)))))
          (saved-pos (cl-loop for buf in buffers
                              collect (with-current-buffer buf
                                        (cons (point) (mark t))))))
      (when (and conn--dispatch-undo-change-groups
                 (not conn-dispatch-amalgamate-undo))
        (dolist (b (or buffers (list (current-buffer))))
          (with-current-buffer b
            (undo-boundary))))
      (conn-dispatch-undo-case 0
        ((or :cancel :undo)
         (cancel-change-group cg)
         (cl-loop for buf in buffers
                  for (pt . mk) in saved-pos
                  do (with-current-buffer buf
                       (goto-char pt)
                       (conn--push-ephemeral-mark mk))))
        (:accept (accept-change-group cg))))))

(defun conn-dispatch-undo-pulse (beg end)
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (face-attribute 'conn-dispatch-undo-pulse-face :background))
  (pulse-momentary-highlight-region
   (min beg end) (max beg end)
   'conn--dispatch-action-current-pulse-face))

(defun conn-dispatch-read-event (&optional prompt
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
                                       conn-dispatch-read-event-map)))
    (catch 'return
      (if seconds
          (while-let ((ev (conn-with-input-method
                            (read-event (unless inhibit-message
                                          (concat prompt
                                                  ": " prompt-suffix
                                                  (when prompt-suffix " ")
                                                  error-msg))
                                        inherit-input-method seconds))))
            (when (characterp ev)
              (throw 'return ev)))
        (while t
          (pcase (conn-with-overriding-map keymap
                   (thread-first
                     (unless inhibit-message
                       (concat prompt
                               (conn--dispatch-read-event-prefix keymap)
                               ": " prompt-suffix
                               (when prompt-suffix " ") error-msg))
                     (read-key-sequence-vector)
                     (key-binding t)))
            ('restart (throw 'return 8))
            ('dispatch-character-event
             (setq conn--read-args-error-message nil
                   conn--dispatch-must-prompt nil)
             (push `(no-record . ,last-input-event) unread-command-events)
             (throw 'return
                    (conn-with-input-method
                      (read-event
                       (unless inhibit-message
                         (concat prompt
                                 (conn--dispatch-read-event-prefix keymap)
                                 ": " prompt-suffix))
                       inherit-input-method))))
            (cmd
             (setq conn--read-args-error-message nil)
             (let ((unhandled nil))
               (catch 'dispatch-handle
                 (cl-loop for handler in conn--dispatch-read-event-handlers
                          do (funcall handler cmd))
                 (setq unhandled t))
               (if unhandled
                   (setq error-msg (propertize
                                    (format "Invalid command <%s>" cmd)
                                    'face 'error))
                 (setf conn-read-args-last-command cmd)
                 (setq conn--read-args-error-message nil))
               (when (and unhandled (eq cmd 'keyboard-quit))
                 (keyboard-quit))))))))))

(cl-defun conn-dispatch-handle-and-redisplay (&key (prompt t))
  (redisplay)
  (setq conn--dispatch-must-prompt prompt)
  (throw 'dispatch-redisplay nil))

(defun conn-dispatch-handle ()
  (throw 'dispatch-handle t))

(defmacro conn-with-dispatch-suspended (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (select-mode)
    `(progn
       (conn-target-finder-suspend conn-dispatch-target-finder)
       (conn-cleanup-labels)
       (pcase-let ((`(,conn-target-window-predicate
                      ,conn-target-predicate
                      ,conn-target-sort-function)
                    conn--dispatch-prev-state)
                   (conn-targets nil)
                   (conn--dispatch-label-state nil)
                   (conn-dispatch-target-finder nil)
                   (conn-dispatch-in-progress nil)
                   (conn--dispatch-undo-change-groups nil)
                   (inhibit-message nil)
                   (recenter-last-op nil)
                   (conn-dispatch-iteration-count nil)
                   (conn-dispatch-other-end nil)
                   (conn-read-args-last-command nil)
                   (conn--read-args-prefix-mag nil)
                   (conn--read-args-prefix-sign nil)
                   (conn--dispatch-read-event-handlers nil)
                   (conn--dispatch-read-event-message-prefixes nil)
                   (conn--dispatch-always-retarget nil)
                   (,select-mode conn-dispatch-select-mode))
         (message nil)
         (if ,select-mode (conn-dispatch-select-mode -1))
         (unwind-protect
             ,(macroexp-progn body)
           (if ,select-mode (conn-dispatch-select-mode 1)))))))

(cl-defgeneric conn-handle-dispatch-select-command (command)
  (:method (_cmd) nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql change-target-finder)))
  (conn-dispatch-change-target-finder
    (conn-read-args (conn-dispatch-targets-state
                     :prompt "New Targets"
                     :reference (list conn-dispatch-thing-ref)
                     :around (lambda (cont)
                               (conn-with-dispatch-suspended
                                 (save-window-excursion
                                   (funcall cont)))))
        ((`(,thing ,thing-arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument)))
      (conn-target-finder-cleanup conn-dispatch-target-finder)
      (setq conn-dispatch-target-finder (conn-get-target-finder thing thing-arg))
      (list thing thing-arg transform))))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql help)))
  (require 'conn-quick-ref)
  (defvar conn-dispatch-select-ref)
  (conn-with-overriding-map conn-dispatch-read-event-map
    (conn-quick-reference (list conn-dispatch-select-ref)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql mwheel-scroll)))
  (require 'mwheel)
  (mwheel-scroll last-input-event)
  (goto-char (window-start (selected-window)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recursive-edit)))
  (conn-with-dispatch-suspended
    (recursive-edit))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recenter-top-bottom)))
  (let ((this-command 'recenter-top-bottom)
        (last-command conn-read-args-last-command))
    (recenter-top-bottom (conn-read-args-prefix-arg)))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql toggle-input-method)))
  (let ((inhibit-message nil))
    (toggle-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql set-input-method)))
  (let ((inhibit-message nil))
    (call-interactively 'set-input-method))
  (conn-dispatch-handle-and-redisplay :prompt nil))

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

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-up)))
  (let ((next-screen-context-lines (or (conn-read-args-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql scroll-down)))
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
        (conn-dispatch-handle-and-redisplay :prompt nil))
    (conn-dispatch-handle)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql finish)))
  (throw 'dispatch-select-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql keyboard-quit)))
  (setq dispatch-quit-flag t)
  (throw 'dispatch-select-exit nil))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql dispatch-other-end)))
  (unless conn-dispatch-no-other-end
    (cl-callf not conn-dispatch-other-end)
    (conn-dispatch-handle-and-redisplay :prompt nil)))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql retarget)))
  (conn-target-finder-retarget conn-dispatch-target-finder)
  (conn-dispatch-handle-and-redisplay :prompt nil))

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
  (when conn--dispatch-undo-change-groups
    ;; pop the current loop's change group first
    (dotimes (_ 2)
      (pcase-dolist (`(,_ . ,undo-fn)
                     (pop conn--dispatch-undo-change-groups))
        (funcall undo-fn :undo))))
  (conn-dispatch-handle-and-redisplay))

;;;;; Dispatch Target Finders

(defface conn-dispatch-context-separator-face
  '((t (:inherit (shadow tooltip) :extend t)))
  "Face for context region separator."
  :group 'conn-faces)

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
  (conn-dispatch-read-n-chars
   :string-length 2
   :hide-target-overlays t))

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

(defclass conn-dispatch-target-key-labels ()
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
                                          conn-dispatch-target-key-labels))
  (conn-with-dispatch-labels
      (labels (conn-dispatch-key-labels))
    (conn-with-dispatch-event-handler
        (:handler
         (lambda (obj)
           (when (conn-dispatch-label-p obj)
             (:return obj)))
         :keymap
         (let ((map (make-sparse-keymap)))
           (cl-loop for label in labels
                    for key = (conn-dispatch-label-string label)
                    do (keymap-set map key label))
           (mapc #'conn-label-redisplay labels)
           map))
      (conn-label-payload
       (progn
         (ignore (conn-dispatch-read-event "Register"))
         (while t
           (ignore
            (conn-dispatch-read-event
             "Register" nil nil
             (propertize "Invalid key" 'face 'error)))))))))

(defclass conn-dispatch-target-window-predicate ()
  ((window-predicate :initform (lambda (&rest _) t)
                     :allocation :class))
  "Abstract type for target finders with a window predicate."
  :abstract t)

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-target-window-predicate))
  (let ((pred (oref state window-predicate)))
    (unless (advice-function-member-p pred conn-target-window-predicate)
      (add-function :before-while conn-target-window-predicate pred)))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(cl-defmethod conn-target-finder-cleanup ((state conn-dispatch-target-window-predicate))
  (remove-function conn-target-window-predicate
                   (oref state window-predicate))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(defclass conn-dispatch-retargetable-target () ()
  :abstract t)

(defvar-keymap conn-dispatch-retargetable-map
  "M-f" 'always-retarget
  "C-f" 'retarget)

(cl-defgeneric conn-target-finder-retarget (target-finder)
  (:method (_) nil))

(cl-defgeneric conn-dispatch-has-targets-p (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-target-finder-keymaps ((_ conn-dispatch-retargetable-target))
  conn-dispatch-retargetable-map)

(cl-defmethod conn-target-finder-message-prefixes ((state conn-dispatch-retargetable-target))
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

(defclass conn-dispatch-string-targets (conn-dispatch-retargetable-target)
  ((string :initform nil
           :initarg :string))
  "Abstract type for target finders targeting a string."
  :abstract t)

(cl-defmethod conn-target-finder-save-state ((target-finder conn-dispatch-string-targets))
  (cons (let ((str (oref target-finder string)))
          (lambda (tf) (setf (oref tf string) str)))
        (cl-call-next-method)))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(cl-defmethod conn-dispatch-has-targets-p ((state conn-dispatch-string-targets))
  (and (oref state string) t))

(defclass conn-dispatch-read-n-chars (conn-dispatch-string-targets)
  ((string-length :initform 1 :initarg :string-length)
   (predicate :initform nil :initarg :predicate)
   (hide-target-overlays :initform nil :initarg :hide-target-overlays)))

(cl-defmethod conn-target-finder-update ((state conn-dispatch-read-n-chars))
  (if-let* ((string (oref state string)))
      (conn-make-string-target-overlays
       string
       (oref state predicate)
       (if (oref state hide-target-overlays)
           0
         (length string)))
    (let* ((string-length (oref state string-length))
           (predicate (oref state predicate))
           (prompt (if (> string-length 1)
                       (propertize (format "%d Chars" string-length)
                                   'face 'minibuffer-prompt)
                     (propertize "1 Char" 'face 'minibuffer-prompt))))
      (while (length< string string-length)
        (when (length> string 0)
          (while-no-input
            (conn-make-string-target-overlays string predicate)))
        (catch 'dispatch-redisplay
          (conn-with-dispatch-event-handler
              (:handler
               (lambda (cmd)
                 (when (eq cmd 'backspace)
                   (when (length> string 0)
                     (cl-callf substring string 0 -1))
                   (:return)))
               :keymap
               (define-keymap "<backspace>" 'backspace))
            (cl-callf thread-last
                string
              (conn-dispatch-read-event prompt t nil)
              (char-to-string)
              (concat string))))
        (conn-cleanup-targets))
      (conn-make-string-target-overlays
       string
       predicate
       (if (oref state hide-target-overlays)
           0
         (length string)))
      (setf (oref state string) string)))
  (cl-call-next-method))

(defclass conn-dispatch-read-with-timeout (conn-dispatch-string-targets)
  ((timeout :initform 0.5 :initarg :timeout)
   (thing :initform nil :initarg :thing)
   (predicate :initform nil :initarg :predicate)
   (regex-p :initform nil :initarg :regex-p)))

(cl-defmethod conn-target-finder-update ((state conn-dispatch-read-with-timeout))
  (let ((string (oref state string))
        (timeout (oref state timeout))
        (predicate (oref state predicate))
        (thing (or (oref state thing)
                   (conn-anonymous-thing
                     'point
                     :bounds-op ( :method (_self _arg)
                                  (save-match-data
                                    (when (looking-at
                                           (if (oref state regex-p)
                                               (oref state string)
                                             (regexp-quote (oref state string))))
                                      (conn-make-bounds
                                       'point nil
                                       (cons (point) (match-end 0)))))))))
        (search-function (if (oref state regex-p)
                             #'conn-make-re-target-overlays
                           #'conn-make-string-target-overlays)))
    (if string
        (funcall search-function string predicate nil thing)
      (let* ((prompt (propertize "String" 'face 'minibuffer-prompt)))
        (setq string (char-to-string (conn-dispatch-read-event prompt t)))
        (while-no-input
          (funcall search-function string predicate nil thing))
        (while-let ((next-char (conn-dispatch-read-event
                                prompt t timeout string)))
          (conn-cleanup-targets)
          (setq string (concat string (char-to-string next-char)))
          (while-no-input
            (funcall search-function string predicate nil thing)))))
    (setf (oref state string) string)))

(defclass conn-dispatch-focus-targets ()
  ((hidden :initform nil)
   (context-lines :initform 0 :initarg :context-lines)
   (cursor-location :initform nil)
   (separator-p :initarg :separator))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(cl-defmethod conn-target-finder-prompt-p ((_state conn-dispatch-focus-targets))
  t)

(cl-defmethod conn-target-finder-cleanup ((state conn-dispatch-focus-targets))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-target-finder-suspend ((state conn-dispatch-focus-targets))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recenter-top-bottom))
                                                   &context (conn-dispatch-target-finder
                                                             conn-dispatch-focus-targets))
  (cl-callf thread-first
      (alist-get (selected-window)
                 (oref conn-dispatch-target-finder cursor-location))
    (memq recenter-positions)
    (cadr)
    (or (car recenter-positions)))
  (unless executing-kbd-macro
    (pulse-momentary-highlight-one-line))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-target-finder-retarget ((state conn-dispatch-focus-targets))
  (pcase-dolist (`(,_win ,_tick . ,ovs) (oref state hidden))
    (mapc #'delete-overlay ovs))
  (setf (oref state hidden) nil))

(cl-defmethod conn-target-finder-update :before ((state conn-dispatch-focus-targets))
  (pcase-dolist (`(,win ,tick . ,ovs) (oref state hidden))
    (unless (eql tick (buffer-chars-modified-tick (window-buffer win)))
      (mapc #'delete-overlay ovs)
      (setf (alist-get win (oref state hidden)) nil))))

(cl-defmethod conn-target-finder-update ((state conn-dispatch-focus-targets))
  (cl-call-next-method)
  (let ((redisplay nil))
    (pcase-dolist (`(,win . ,targets) conn-targets)
      (pcase-let ((`(,tick . ,old-hidden)
                   (alist-get win (oref state hidden))))
        (unless (eql tick (buffer-chars-modified-tick (window-buffer win)))
          (mapc #'delete old-hidden)
          (conn-protected-let* ((hidden (list (make-overlay (point-min) (point-min)))
                                        (mapc #'delete-overlay hidden))
                                (context-lines (oref state context-lines))
                                (separator-p (if (slot-boundp state 'separator-p)
                                                 (oref state separator-p)
                                               (> context-lines 0))))
            (with-selected-window win
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
              (let ((this-scroll-margin
                     (min (max 0 scroll-margin)
		          (truncate (/ (window-body-height) 4.0)))))
                (pcase (alist-get win (oref state cursor-location))
                  ('middle (recenter nil))
                  ('top (recenter this-scroll-margin))
                  ('bottom (recenter (- -1 this-scroll-margin)))
                  (_
                   (setf (alist-get win (oref state cursor-location)) 'middle)
                   (recenter nil)))))
            (setf (alist-get win (oref state hidden))
                  (cons (buffer-chars-modified-tick) hidden)
                  redisplay
                  (or redisplay (and hidden t)))))))
    (when redisplay (redisplay))))

(defclass conn-dispatch-focus-thing-at-point (conn-dispatch-string-targets
                                              conn-dispatch-focus-targets
                                              conn-dispatch-target-window-predicate)
  ((context-lines
    :initform 1
    :initarg :context-lines)
   (window-predicate
    :initform (lambda (win)
                (eq (window-buffer win)
                    (window-buffer (selected-window)))))))

(cl-defmethod conn-target-finder-update ((state conn-dispatch-focus-thing-at-point))
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
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
  (cl-call-next-method))

(defclass conn-dispatch-mark-ring (conn-dispatch-focus-targets
                                   conn-dispatch-target-window-predicate)
  ((context-lines
    :initform 1
    :initarg :context-lines)
   (window-predicate
    :initform (lambda (win) (eq win (selected-window))))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-mark-ring))
  :no-other-end)

(cl-defmethod conn-target-finder-update ((_state conn-dispatch-mark-ring))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (let ((points (conn-ring-list conn-mark-ring)))
          (dolist (pt points)
            (unless (invisible-p pt)
              (conn-make-target-overlay pt 0))))))
    (cl-call-next-method)))

(defclass conn-dispatch-global-mark (conn-dispatch-focus-targets
                                     conn-dispatch-target-window-predicate)
  ((context-lines
    :initform 1
    :initarg :context-lines)
   (window-predicate
    :initform (lambda (win)
                (cl-loop with buf = (window-buffer win)
                         for mk in global-mark-ring
                         thereis (eq buf (marker-buffer mk)))))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-global-mark))
  :no-other-end)

(cl-defmethod conn-target-finder-update ((_state conn-dispatch-global-mark))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (dolist (mk global-mark-ring)
          (when (and (eq (current-buffer) (marker-buffer mk))
                     (not (invisible-p mk)))
            (conn-make-target-overlay mk 0)))))
    (cl-call-next-method)))

(defclass conn-dispatch-mark-register (conn-dispatch-focus-targets
                                       conn-dispatch-target-window-predicate
                                       conn-dispatch-target-key-labels)
  ((context-lines
    :initform 1
    :initarg :context-lines)
   (window-predicate
    :initform (lambda (win)
                (cl-loop with buf = (window-buffer win)
                         for (_ . obj) in register-alist
                         thereis (and (markerp obj)
                                      (eq buf (marker-buffer obj))))))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-mark-register))
  :no-other-end)

(cl-defmethod conn-target-finder-update ((_state conn-dispatch-mark-register))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,key . ,obj) register-alist)
          (when (and (markerp obj)
                     (eq (current-buffer) (marker-buffer obj))
                     (not (invisible-p obj)))
            (conn-make-target-overlay
             obj 0
             :properties `(label-key ,(key-description (vector key))))))))
    (cl-call-next-method)))

(defclass conn-dispatch-previous-emacs-state (conn-dispatch-focus-targets
                                              conn-dispatch-target-window-predicate)
  ((context-lines :initform 1 :initarg :context-lines)
   (window-predicate :initform (lambda (win) (eq win (selected-window))))))

(cl-defmethod conn-target-finder-other-end ((_ conn-dispatch-previous-emacs-state))
  :no-other-end)

(cl-defmethod conn-target-finder-update ((_state conn-dispatch-previous-emacs-state))
  (unless conn-targets
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (let ((points (conn-ring-list conn-emacs-state-ring)))
          (dolist (pt points)
            (unless (invisible-p pt)
              (conn-make-target-overlay pt 0)))))))
  (cl-call-next-method))

(defun conn-dispatch-chars-in-thing (thing)
  (declare (important-return-value t))
  (conn-dispatch-read-with-timeout
   :thing thing
   :timeout conn-read-string-timeout
   :predicate (lambda (beg _end)
                (goto-char beg)
                (ignore-errors
                  (conn-bounds-of thing nil)))))

(defclass conn-dispatch-headings (conn-dispatch-focus-targets
                                  conn-dispatch-target-window-predicate)
  ((window-predicate
    :initform (lambda (win)
                (let ((buf (window-buffer win)))
                  (or (and (boundp 'outline-minor-mode)
                           (buffer-local-value 'outline-minor-mode buf))
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buf)
                       'outline-mode)))))))

(cl-defmethod conn-target-finder-update ((_state conn-dispatch-headings))
  (dolist (win (conn--get-target-windows))
    (with-current-buffer (window-buffer win)
      (let ((heading-regexp (concat "^\\(?:" outline-regexp "\\).*")))
        (save-excursion
          (pcase-dolist (`(,beg . ,end)
                         (conn--visible-regions (point-min) (point-max)))
            (goto-char beg)
            (while (re-search-forward heading-regexp end t)
              (when (looking-at-p outline-heading-end-regexp)
                (conn-make-target-overlay
                 (match-beginning 0) 0
                 :window win))))))))
  (cl-call-next-method))

(defclass conn-dispatch-all-defuns (conn-dispatch-focus-targets
                                    conn-dispatch-target-window-predicate)
  ((window-predicate
    :initform (lambda (win) (eq win (selected-window))))
   (cache :initform nil)))

(defvar-local conn-extract-defuns-function
  'conn--dispatch-extract-defuns-default)

(defun conn--dispatch-extract-defuns-default ()
  (let ((pts nil))
    (conn-for-each-visible (point-min) (point-max)
      (goto-char (point-max))
      (while (beginning-of-defun)
        (push (point) pts)))
    (lambda ()
      (dolist (pt pts)
        (conn-make-target-overlay
         pt 0
         :properties '(no-hide t))))))

(cl-defmethod conn-target-finder-update ((state conn-dispatch-all-defuns))
  (let ((cache (oref state cache)))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (if-let* ((cached (alist-get (window-buffer win) cache))
                  (_ (= (buffer-chars-modified-tick)
                        (car cached))))
            (funcall (cdr cached))
          (let ((setup (funcall conn-extract-defuns-function)))
            (setf (alist-get (window-buffer win) cache)
                  (cons (buffer-chars-modified-tick) setup))
            (funcall setup))))))
  (cl-call-next-method))

(defun conn-dispatch-all-things (thing)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (conn-for-each-visible
            (max (1- (window-start))
                 (point-min))
            (window-end)
          (goto-char (point-max))
          (while (and (/= (point) (progn
                                    (forward-thing thing -1)
                                    (point)))
                      (/= (point) (point-min)))
            (conn-make-target-overlay (point) 0)))))))

(defun conn-dispatch-all-buttons ()
  (declare (important-return-value t))
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (conn-for-each-visible (window-start) (window-end)
        (goto-char (point-min))
        (when (get-char-property (point) 'button)
          (conn-make-target-overlay (point) 0))
        (while (not (eobp))
          (goto-char (next-single-char-property-change (point) 'button))
          (when (get-char-property (point) 'button)
            (conn-make-target-overlay (point) 0)))))))

(defun conn-dispatch-re-matches (regexp &optional fixed-length)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (save-excursion
          (goto-char (window-start))
          (pcase-dolist (`(,beg . ,end) (conn--visible-re-matches regexp))
            (conn-make-target-overlay beg (or fixed-length (- end beg)))))))))

(defun conn-dispatch-things-read-prefix (thing prefix-length)
  (declare (important-return-value t))
  (conn-dispatch-read-n-chars
   :string-length prefix-length
   :hide-target-overlays t
   :predicate (lambda (beg _end)
                (save-excursion
                  (goto-char beg)
                  (pcase (ignore-errors (conn-bounds-of thing nil))
                    ((conn-bounds `(,tbeg . ,_tend))
                     (= beg tbeg)))))))

(defun conn-dispatch-things-with-prefix (thing prefix-string &optional fixed-length)
  (declare (important-return-value t))
  (lambda ()
    (conn-make-string-target-overlays
     prefix-string
     (lambda (beg _end)
       (save-excursion
         (goto-char beg)
         (pcase (ignore-errors (conn-bounds-of thing nil))
           ((conn-bounds `(,tbeg . ,_tend))
            (= beg tbeg)))))
     fixed-length)))

(defun conn-dispatch-things-with-re-prefix (thing prefix-regex &optional fixed-length)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches
                        prefix-regex
                        (lambda (beg end)
                          (save-excursion
                            (goto-char beg)
                            (pcase (ignore-errors (conn-bounds-of thing nil))
                              ((conn-bounds `(,tbeg . ,tend))
                               (and (= tbeg beg) (<= end tend))))))))
          (conn-make-target-overlay beg (or fixed-length (- end beg))))))))

(defun conn-dispatch-things-matching-re (thing regexp &optional fixed-length)
  (declare (important-return-value t))
  (lambda ()
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches
                        regexp
                        (lambda (beg end)
                          (save-excursion
                            (goto-char beg)
                            (pcase (ignore-errors (conn-bounds-of thing nil))
                              ((conn-bounds `(,tbeg . ,tend))
                               (and (<= tbeg beg) (<= tend end))))))))
          (conn-make-target-overlay beg (or fixed-length (- end beg))))))))

(defun conn-dispatch-columns ()
  (let* ((line-move-visual nil)
         (goal-column (or goal-column (current-column)))
         (padding-function
          (when (= goal-column (window-hscroll))
            (lambda (ov width _face)
              (conn--right-justify-padding ov width nil)))))
    (cl-macrolet ((goto-col ()
                    `(if (zerop goal-column)
                         0
                       (move-to-column goal-column))))
      (save-excursion
        (pcase-dolist (`(,vbeg . ,vend)
                       (conn--visible-regions (window-start) (window-end)))
          (goto-char vbeg)
          (unless (zerop goal-column)
            (move-to-column goal-column))
          (unless (or (and (bolp) (not (bobp))
                           (invisible-p (1- (point))))
                      (< (goto-col) (window-hscroll)))
            (conn-make-target-overlay
             (point) 0
             :padding-function padding-function
             :thing 'point))
          (while (< (point) vend)
            (forward-line)
            (unless (zerop goal-column)
              (move-to-column goal-column))
            (unless (or (and (bolp) (invisible-p (1- (point))))
                        (< (goto-col) (window-hscroll)))
              (conn-make-target-overlay
               (point) 0
               :padding-function padding-function
               :thing 'point))))))))

(cl-defmethod conn-target-finder-label-faces ((_ (eql conn-dispatch-columns)))
  nil)

(defun conn-dispatch-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (let* ((line-move-visual nil)
             (goal-column (window-hscroll))
             (padding-function
              (lambda (ov width _face)
                (conn--right-justify-padding ov width nil))))
        (cl-macrolet ((goto-col ()
                        `(if (zerop goal-column)
                             0
                           (move-to-column goal-column))))
          (save-excursion
            (pcase-dolist (`(,vbeg . ,vend)
                           (conn--visible-regions (window-start) (window-end)))
              (goto-char vbeg)
              (unless (or (and (bolp) (not (bobp))
                               (invisible-p (1- (point))))
                          (/= goal-column (goto-col)))
                (conn-make-target-overlay
                 (point) 0
                 :padding-function padding-function))
              (while (< (point) vend)
                (forward-line)
                (unless (zerop goal-column)
                  (move-to-column goal-column))
                (unless (or (and (bolp)
                                 (invisible-p (1- (point))))
                            (/= goal-column (goto-col)))
                  (conn-make-target-overlay
                   (point) 0
                   :padding-function padding-function))))))))))

(cl-defmethod conn-target-finder-label-faces ((_ (eql conn-dispatch-lines)))
  nil)

(defun conn-dispatch-end-of-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
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
              (conn-make-target-overlay (point) 0))))))))

(cl-defmethod conn-target-finder-other-end ((_ (eql conn-dispatch-end-of-lines)))
  t)

(defun conn-dispatch-inner-lines ()
  (let ((thing (conn-anonymous-thing
                 'conn-forward-inner-line
                 :pretty-print ( :method (_self) "inner-line")
                 :bounds-op ( :method (_self _arg)
                              (goto-char (pos-bol))
                              (cl-call-next-method)))))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (conn-for-each-visible (window-start) (window-end)
          (goto-char (point-max))
          (while (let ((pt (point)))
                   (forward-line -1)
                   (conn-beginning-of-inner-line)
                   (/= (point) pt))
            (when (not (invisible-p (point)))
              (conn-make-target-overlay
               (point) 0
               :thing thing))))))))

(defun conn-dispatch-end-of-inner-lines ()
  (let ((thing (conn-anonymous-thing
                 'conn-forward-inner-line
                 :pretty-print ( :method (_self) "end-of-inner-line")
                 :bounds-op ( :method (_self _arg)
                              (goto-char (pos-bol))
                              (cl-call-next-method)))))
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (conn-for-each-visible (window-start) (window-end)
          (goto-char (point-max))
          (while (let ((pt (point)))
                   (forward-line -1)
                   (conn-end-of-inner-line)
                   (/= (point) pt))
            (when (not (invisible-p (point)))
              (conn-make-target-overlay
               (point) 0
               :thing thing))))))))

(cl-defmethod conn-target-finder-other-end
  ((_ (eql conn-dispatch-end-of-inner-lines)))
  t)

(defun conn-dispatch-visual-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (save-excursion
        (goto-char (window-start))
        (vertical-motion 0)
        (conn-make-target-overlay
         (point) 0
         :thing 'point
         :padding-function (lambda (ov width _face)
                             (conn--right-justify-padding ov width nil)))
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
                                 (conn--right-justify-padding ov width nil))))
          (vertical-motion 1))))))

;;;;; Dispatch Actions

(defvar conn-dispatch-amalgamate-undo nil)

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
  (action-always-prompt :type boolean))

(defalias 'conn-action-no-history 'conn-action--action-no-history)
(defalias 'conn-action-auto-repeat 'conn-action--action-auto-repeat)
(defalias 'conn-action-description 'conn-action--action-description)
(defalias 'conn-action-window-predicate 'conn-action--action-window-predicate)
(defalias 'conn-action-target-predicate 'conn-action--action-target-predicate)
(defalias 'conn-action-thing-predicate 'conn-action--action-thing-predicate)
(defalias 'conn-action-always-retarget 'conn-action--action-always-retarget)
(defalias 'conn-action-always-prompt 'conn-action--action-always-prompt)

(defun conn--action-type-p (item)
  (declare (important-return-value t)
           (side-effect-free t))
  (when-let* ((class (and (symbolp item)
                          (cl--find-class item))))
    (and (oclosure--class-p class)
         (memq 'conn-action (oclosure--class-allparents class)))))

(cl-defgeneric conn-make-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t)))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing t)))
  (conn-make-action 'conn-dispatch-goto))

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

(cl-defgeneric conn-make-action (type)
  (declare (important-return-value t))
  (:method (type) (error "Unknown action type %s" type))
  (:method :after (_type) (conn-read-args-consume-prefix-arg)))

(cl-defmethod conn-make-action :around (type)
  (or (atomic-change-group
        (save-window-excursion
          (cl-call-next-method)))
      (error "Failed to construct %S" type)))

(defun conn--action-buffer-change-group ()
  (declare (important-return-value t))
  (let ((change-group (prepare-change-group)))
    (activate-change-group change-group)
    (list change-group (point) (mark t) mark-active)))

(defun conn--action-accept-change-group (change-group)
  (pcase-let ((`(,handle ,_saved-point ,_saved-mark) change-group))
    (accept-change-group handle)))

(defun conn--action-cancel-change-group (change-group)
  (pcase-let ((`(,handle ,saved-point ,saved-mark ,saved-mark-active)
               change-group))
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
          (run-hooks 'deactivate-mark-hook))))))

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

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-goto)))
  (oclosure-lambda (conn-dispatch-goto
                    (action-description "Goto"))
      (window pt thing thing-arg transform)
    (select-window window)
    (conn-dispatch-loop-undo-boundary)
    (unless (and (= pt (point))
                 (region-active-p))
      (let ((forward (< (point) pt)))
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-dispatch-bounds `(,beg . ,end) transform)
           (if (region-active-p)
               (goto-char (if (and forward (not (eq thing 'point)))
                              end
                            beg))
             (conn--push-ephemeral-mark end)
             (goto-char beg)))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-push-button
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-push-button)))
  (oclosure-lambda (conn-dispatch-push-button
                    (action-description "Push Button")
                    (action-no-history t))
      (window pt _thing _thing-arg _transform)
    (select-window window)
    (if (button-at pt)
        (push-button pt)
      (when (fboundp 'widget-apply-action)
        (widget-apply-action (get-char-property pt 'button) pt)))))

(oclosure-define (conn-dispatch-copy-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-to)))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument))
       (ftransform (conn-transform-argument))
       (separator (conn-dispatch-separator-argument 'default)))
    (let ((str (pcase (conn-bounds-of fthing farg)
                 ((conn-bounds `(,beg . ,end) ftransform)
                  (save-mark-and-excursion
                    (goto-char beg)
                    (conn--push-ephemeral-mark end)
                    (conn-dispatch-action-pulse beg end)
                    (funcall region-extract-function nil)))
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
                                                (window-buffer win))))))
          (window pt thing thing-arg transform)
        (with-selected-window window
          (conn-dispatch-loop-undo-boundary)
          (save-mark-and-excursion
            (pcase (conn-bounds-of-dispatch thing thing-arg pt)
              ((conn-dispatch-bounds `(,beg . ,end) transform)
               (goto-char beg)
               (when (and separator (< end beg))
                 (conn-dispatch-insert-separator separator))
               (conn--push-ephemeral-mark)
               (insert-for-yank str)
               (when (and separator (not (< end beg)))
                 (conn-dispatch-insert-separator separator))
               (conn-dispatch-action-pulse
                (- (point) (length str))
                (point)))
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-copy-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-copy-to--separator action))))
      (format "Copy To <%s>" sep)
    "Copy To"))

(oclosure-define (conn-dispatch-copy-to-replace
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-to-replace)))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,fthing ,farg) (conn-thing-argument-dwim))
       (ftransform (conn-transform-argument)))
    (oclosure-lambda (conn-dispatch-copy-to-replace
                      (action-description "Copy and Replace To")
                      (str (pcase (conn-bounds-of fthing farg)
                             ((conn-bounds `(,beg . ,end) ftransform)
                              (save-mark-and-excursion
                                (goto-char beg)
                                (conn--push-ephemeral-mark end)
                                (conn-dispatch-action-pulse beg end)
                                (funcall region-extract-function nil)))
                             (_ (user-error "Cannot find %s at point"
                                            (conn-thing-pretty-print fthing)))))
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-mark-and-excursion
          (pcase (conn-bounds-of-dispatch thing thing-arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (delete-region beg end)
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str))
              (point)))
            (_ (user-error "Cannot find %s"
                           (conn-thing-pretty-print thing)))))))))

(oclosure-define (conn-dispatch-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-to-replace)))
  (oclosure-lambda (conn-dispatch-yank-to-replace
                    (action-description "Yank and Replace To")
                    (str (current-kill 0))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (insert-for-yank str)
           (conn-dispatch-action-pulse
            (- (point) (length str)) (point)))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-reading-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-reading-yank-to-replace)))
  (oclosure-lambda (conn-dispatch-reading-yank-to-replace
                    (action-description "Yank and Replace To")
                    (str (read-from-kill-ring "Yank: "))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (insert-for-yank str)
           (conn-dispatch-action-pulse
            (- (point) (length str)) (point)))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-to)))
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
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
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
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-yank-to) &optional short)
  (if-let* ((sep (and (not short) (conn-dispatch-yank-to--separator action))))
      (format "Yank To <%s>" (if (eq sep 'register)
                                 (get-register register-separator)
                               sep))
    "Yank To"))

(oclosure-define (conn-dispatch-reading-yank-to
                  (:parent conn-action))
  (str :type string)
  (separator :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-reading-yank-to)))
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
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing thing-arg pt)
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
           (point)))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-reading-yank-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-reading-yank-to--separator action))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-send
                  (:parent conn-action))
  (str :type string)
  (separator :type string)
  (action-change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send)))
  (conn-read-args (conn-kill-state
                   :prompt "Send Thing")
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (fixup (conn-fixup-whitespace-argument
               (not (region-active-p))))
       (check-bounds (conn-check-bounds-argument))
       (separator (conn-dispatch-separator-argument 'default)))
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
                                                (window-buffer win))))))
          (window pt thing thing-arg transform)
        (with-selected-window window
          (conn-dispatch-loop-undo-boundary)
          (save-excursion
            (pcase (conn-bounds-of-dispatch thing thing-arg pt)
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
             (point))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send))
  (conn--action-accept-change-group (conn-dispatch-send--action-change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send))
  (conn--action-cancel-change-group (conn-dispatch-send--action-change-group action)))

(oclosure-define (conn-dispatch-send-replace
                  (:parent conn-action))
  (str :type string)
  (action-change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send-replace
                      (action-description "Send and Replace")
                      (action-change-group cg)
                      (str
                       (conn-read-args (conn-kill-state
                                        :prompt "Send Thing")
                           ((`(,thing ,thing-arg)
                             (conn-thing-argument-dwim))
                            (transform (conn-transform-argument))
                            (fixup (conn-fixup-whitespace-argument
                                    (not (region-active-p))))
                            (check-bounds (conn-check-bounds-argument t)))
                         (progn
                           (conn-kill-thing thing thing-arg transform
                                            nil nil nil
                                            fixup check-bounds)
                           (current-kill 0))))
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (pcase (conn-bounds-of-dispatch thing thing-arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (delete-region beg end)
             (insert-for-yank str)
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point)))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send-replace))
  (conn--action-accept-change-group (conn-dispatch-send-replace--action-change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send-replace))
  (conn--action-cancel-change-group (conn-dispatch-send-replace--action-change-group action)))

(oclosure-define (conn-dispatch-register-load
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-load)))
  (oclosure-lambda (conn-dispatch-register-load
                    (register (register-read-with-preview "Register: ")))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (save-excursion
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-dispatch-bounds `(,beg . ,_end) transform)
           (goto-char beg)
           (conn-register-load register))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-register-load) &optional short)
  (if short "Register"
    (format "Register <%c>" (conn-dispatch-register-load--register action))))

(oclosure-define (conn-dispatch-register-load-replace
                  (:parent conn-action))
  (register :type integer))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-register-load-replace)))
  (oclosure-lambda (conn-dispatch-register-load-replace
                    (register (register-read-with-preview "Register: ")))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      ;; If there is a keyboard macro in the register we would like to
      ;; amalgamate the undo
      (save-excursion
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (conn-register-load register))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-register-load-replace) &optional short)
  (if short "Register Replace"
    (format "Register Replace <%c>" (conn-dispatch-register-load-replace--register action))))

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

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-from)))
  (oclosure-lambda (conn-dispatch-copy-from
                    (action-description "Copy From")
                    (action-opoint (copy-marker (point) t)))
      (window pt thing thing-arg transform)
    (let (str)
      (with-selected-window window
        (pcase (conn-bounds-of-dispatch thing thing-arg pt)
          ((conn-bounds `(,beg . ,end) transform)
           (conn-dispatch-action-pulse beg end)
           (setq str (filter-buffer-substring beg end)))
          (_ (user-error "Cannot find thing at point"))))
      (with-current-buffer (marker-buffer action-opoint)
        (conn-dispatch-loop-undo-boundary)
        (cond ((null str)
               (user-error "Cannot find thing at point"))
              ((/= (point) action-opoint)
               (save-excursion
                 (goto-char action-opoint)
                 (insert-for-yank str)))
              (t
               (goto-char action-opoint)
               (insert-for-yank str)))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-copy-from))
  (set-marker (conn-dispatch-copy-from--action-opoint action) nil))

(oclosure-define (conn-dispatch-copy-from-replace
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-from-replace)))
  (oclosure-lambda (conn-dispatch-copy-from-replace
                    (action-description "Copy From and Replace"))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (pcase (conn-bounds-of-dispatch thing thing-arg pt)
        ((conn-bounds `(,beg . ,end) transform)
         (conn-dispatch-action-pulse beg end)
         (copy-region-as-kill beg end))
        (_ (user-error "Cannot find thing at point"))))
    (conn-dispatch-loop-undo-boundary)
    (delete-region (region-beginning) (region-end))
    (yank)))

(oclosure-define (conn-dispatch-take-replace
                  (:parent conn-action)
                  (:copier conn-dispatch-take-replace-copy (action-opoint)))
  (action-opoint :type marker)
  (action-change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take-replace))
  (thread-first
    (conn-dispatch-take-replace--action-opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take-replace))
  (conn-thread-first
    (conn-dispatch-take-replace--action-opoint action)
    (copy-marker t)
    (-<> conn-dispatch-take-replace-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (delete-region (region-beginning) (region-end))
    (oclosure-lambda (conn-dispatch-take-replace
                      (action-description "Take From and Replace")
                      (action-change-group cg)
                      (action-opoint (copy-marker (point) t))
                      (action-window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (conn-dispatch-loop-undo-boundary (current-buffer) (window-buffer window))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((and bounds (conn-bounds `(,beg . ,end) transform))
             (kill-region beg end)
             (funcall conn-kill-fixup-whitespace-function bounds))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer (marker-buffer action-opoint)
        (save-excursion
          (goto-char action-opoint)
          (yank))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--action-opoint action) nil)
  (conn--action-cancel-change-group
   (conn-dispatch-take-replace--action-change-group action)))

(cl-defmethod conn-accept-action ((action conn-dispatch-take-replace))
  (conn--action-accept-change-group
   (conn-dispatch-take-replace--action-change-group action)))

(oclosure-define (conn-dispatch-take
                  (:parent conn-action)
                  (:copier conn-dispatch-take-copy (action-opoint)))
  (action-opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take))
  (thread-first
    (conn-dispatch-take--action-opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--action-opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take))
  (conn-thread-first
    (conn-dispatch-take--action-opoint action)
    (copy-marker t)
    (-<> conn-dispatch-take-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take)))
  (oclosure-lambda (conn-dispatch-take
                    (action-description "Take From")
                    (action-opoint (copy-marker (point) t))
                    (action-window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (conn-dispatch-loop-undo-boundary (current-buffer) (window-buffer window))
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((and bounds (conn-bounds `(,beg . ,end) transform))
           (kill-region beg end)
           (funcall conn-kill-fixup-whitespace-function bounds))
          (_ (user-error "Cannot find thing at point")))))
    (with-current-buffer (marker-buffer action-opoint)
      (yank))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--action-opoint action) nil))

(oclosure-define (conn-dispatch-jump
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-jump)))
  (oclosure-lambda (conn-dispatch-jump
                    (action-description "Jump"))
      (window pt _thing _thing-arg _transform)
    (select-window window)
    (conn-dispatch-loop-undo-boundary)
    (unless (= pt (point))
      (unless (region-active-p)
        (push-mark nil t))
      (goto-char pt))))

(oclosure-define (conn-dispatch-kapply
                  (:parent conn-action))
  (macro :mutable t))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-kapply)))
  (require 'conn-transients)
  (let ((action nil)
        (setup (make-symbol "setup")))
    (fset setup (lambda ()
                  (conn-without-recursive-stack
                    (conn-dispatch-kapply-prefix
                     (lambda (kapply-action)
                       (setf action kapply-action))))
                  (remove-hook 'post-command-hook setup)))
    (add-hook 'post-command-hook setup -99)
    (add-hook 'transient-post-exit-hook 'exit-recursive-edit)
    (unwind-protect
        (recursive-edit)
      (remove-hook 'post-command-hook setup)
      (remove-hook 'transient-post-exit-hook 'exit-recursive-edit))
    action))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-kapply) &optional short)
  (if short "Kapply"
    (concat "Kapply"
            (when-let* ((macro (oref action macro)))
              (concat " <" (conn--kmacro-display (kmacro--keys macro)) ">")))))

(oclosure-define (conn-dispatch-repeat-command
                  (:parent conn-action))
  (command :type list))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-repeat-command)))
  (when command-history
    (oclosure-lambda (conn-dispatch-repeat-command
                      (command (car command-history))
                      (action-window-predicate
                       (lambda (win)
                         (not (buffer-local-value 'buffer-read-only
                                                  (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-mark-and-excursion
          (pcase (conn-bounds-of-dispatch thing thing-arg pt)
            ((conn-dispatch-bounds `(,beg . ,end) transform)
             (goto-char beg)
             (conn--push-ephemeral-mark end)
             (eval command))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-repeat-command) &optional short)
  (if short "Repeat Cmd"
    (format "Repeat <%s>" (car (oref action command)))))

(oclosure-define (conn-dispatch-transpose
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-transpose)))
  (oclosure-lambda (conn-dispatch-transpose
                    (action-description "Transpose")
                    (action-always-retarget t)
                    (action-window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
      ( window1 pt1 thing1 thing-arg1 transform1
        window2 pt2 thing2 thing-arg2 transform2)
    (conn-dispatch-loop-undo-boundary (window-buffer window1)
                                      (window-buffer window2))
    (conn--dispatch-transpose-subr
     (window-buffer window1) pt1 thing1 thing-arg1 transform1
     (window-buffer window2) pt2 thing2 thing-arg2 transform2)))

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
               (:constructor
                conn-make-dispatch
                ( action
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
                   (let ((fns (conn-target-finder-save-state conn-dispatch-target-finder)))
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
          (conn-action-pretty-print (conn-previous-dispatch-action dispatch))
          (conn-thing-pretty-print (car (conn-previous-dispatch-thing-state dispatch)))
          (nth 1 (conn-previous-dispatch-thing-state dispatch))
          (if-let* ((ts (nth 2 (conn-previous-dispatch-thing-state dispatch))))
              (concat
               "; "
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
                           `(,thing ,thing-arg ,thing-transform))
                          action
                          other-end
                          always-retarget
                          repeat
                          restrict-windows
                          setup-function)
               prev-dispatch))
    (apply #'conn-dispatch-setup
           `( ,action ,thing ,thing-arg ,thing-transform
              ,@override-keys
              :always-retarget ,always-retarget
              :repeat ,repeat
              :restrict-windows ,restrict-windows
              :other-end ,other-end
              :setup-function ,setup-function))))

;;;;; Dispatch Commands

(cl-defun conn-dispatch-setup (action
                               thing
                               thing-arg
                               thing-transform
                               &rest keys
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
  (let* ((dispatch-quit-flag nil)
         (conn--dispatch-current-thing (list thing thing-arg thing-transform))
         (opoint (point-marker))
         (eldoc-display-functions nil)
         (recenter-last-op nil)
         (conn-read-args-last-command nil)
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
         (conn--dispatch-read-event-handlers
          (cons #'conn-handle-dispatch-select-command
                conn--dispatch-read-event-handlers))
         (conn--dispatch-action-always-prompt (conn-action-always-prompt action))
         (conn-dispatch-target-finder
          (conn-get-target-finder thing thing-arg))
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
         (conn--dispatch-read-event-message-prefixes
          `(,(propertize (conn-action-pretty-print action t)
                         'face 'eldoc-highlight-function-argument)
            ,(lambda (_keymap)
               (conn-target-finder-message-prefixes
                conn-dispatch-target-finder))
            ,(unless conn-dispatch-no-other-end
               (lambda (keymap)
                 (when-let* ((binding
                              (where-is-internal 'dispatch-other-end keymap t)))
                   (concat
                    (propertize (key-description binding)
                                'face 'help-key-binding)
                    " "
                    (propertize
                     "other end"
                     'face (when conn-dispatch-other-end
                             'eldoc-highlight-function-argument))))))
            ,(lambda (keymap)
               (when-let* (((or (length> conn-targets 1)
                                (advice-function-member-p 'conn--dispatch-restrict-windows
                                                          conn-target-window-predicate)))
                           (binding
                            (where-is-internal 'restrict-windows keymap t)))
                 (concat
                  (propertize (key-description binding)
                              'face 'help-key-binding)
                  " "
                  (propertize
                   "this win"
                   'face (when (advice-function-member-p
                                'conn--dispatch-restrict-windows
                                conn-target-window-predicate)
                           'eldoc-highlight-function-argument)))))
            ,@conn--dispatch-read-event-message-prefixes)))
    (when-let* ((predicate (conn-action-window-predicate action)))
      (add-function :after-while conn-target-window-predicate predicate))
    (when-let* ((predicate (conn-action-target-predicate action)))
      (add-function :after-while conn-target-predicate predicate))
    (when restrict-windows
      (add-function :after-while conn-target-window-predicate
                    'conn--dispatch-restrict-windows))
    (when setup-function (funcall setup-function))
    (conn--unwind-protect-all
      (progn
        (conn-dispatch-perform-action action repeat)
        (conn-dispatch-push-history (conn-make-dispatch action)))
      (conn-cleanup-targets)
      (conn-cleanup-labels)
      (progn
        (with-current-buffer (marker-buffer opoint)
          (if dispatch-quit-flag
              (goto-char opoint)
            (unless (eql (point) (marker-position opoint))
              (conn--push-mark-ring opoint))))
        (set-marker opoint nil)
        (let ((inhibit-message conn-read-args-inhibit-message))
          (message nil))))))

(cl-defgeneric conn-dispatch-perform-action (action repeat))

(cl-defmethod conn-dispatch-perform-action ((action conn-action) repeat)
  (conn-dispatch-loop repeat
    (pcase-let* ((`(,pt ,win ,thing ,arg ,transform)
                  (conn-select-target)))
      (funcall action win pt thing arg transform))))

(cl-defmethod conn-dispatch-perform-action ((action conn-dispatch-transpose)
                                            repeat)
  (conn-dispatch-loop repeat
    (pcase-let ((`(,pt1 ,win1 ,thing1 ,arg1 ,transform1)
                 (conn-select-target)))
      (let ((conn-target-predicate conn-target-predicate))
        (add-function :before-while conn-target-predicate
                      (lambda (pt _length window)
                        (not (and (eq window win1)
                                  (eql pt pt1)))))
        (pcase-let ((`(,pt2 ,win2 ,thing2 ,arg2 ,transform2)
                     (conn-select-target)))
          (funcall action
                   win1 pt1 thing1 arg1 transform1
                   win2 pt2 thing2 arg2 transform2))))))

(cl-defmethod conn-dispatch-perform-action ((action conn-dispatch-kapply)
                                            repeat)
  (let ((conn-label-select-always-prompt t))
    (conn-dispatch-loop repeat
      (pcase-let* ((`(,pt ,win ,thing ,arg ,transform)
                    (conn-select-target)))
        (while
            (condition-case err
                (progn
                  (funcall action win pt thing arg transform)
                  nil)
              (user-error (message (cadr err)) t))))))
  (unless conn-kapply-suppress-message
    (message "Kapply completed successfully after %s iterations"
             conn-dispatch-iteration-count)))

(defun conn-dispatch (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-dispatch-state
                   :command-handler #'conn-dispatch-command-handler
                   :prefix initial-arg
                   :prompt "Dispatch"
                   :reference conn-dispatch-reference
                   :pre (lambda (_)
                          (when (and (bound-and-true-p conn-posframe-mode)
                                     (fboundp 'posframe-hide))
                            (posframe-hide " *conn-list-posframe*"))))
      ((`(,thing ,thing-arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument))
       (other-end (conn-dispatch-other-end-argument nil))
       (restrict-windows (conn-dispatch-restrict-windows-argument))
       (`(,action ,repeat) (conn-dispatch-action-argument)))
    (conn-dispatch-setup
     action thing thing-arg transform
     :repeat repeat
     :other-end other-end
     :restrict-windows restrict-windows)))

(defun conn-dispatch-thing-at-point (&optional initial-arg)
  (interactive "P")
  (cl-letf ((conn--dispatch-action-always-prompt t)
            ((symbol-function 'conn-get-target-finder)))
    (advice-add 'conn-get-target-finder :override
                (lambda (cmd arg)
                  (pcase (ignore-errors (conn-bounds-of cmd arg))
                    ((conn-bounds `(,beg . ,end))
                     (conn-dispatch-focus-thing-at-point
                      :string (buffer-substring-no-properties beg end))))))
    (conn-read-args (conn-dispatch-thingatpt-state
                     :prefix initial-arg
                     :command-handler #'conn-dispatch-command-handler
                     :prompt "Dispatch"
                     :reference conn-dispatch-reference
                     :pre (lambda (_)
                            (when (and (bound-and-true-p conn-posframe-mode)
                                       (fboundp 'posframe-hide))
                              (posframe-hide " *conn-list-posframe*"))))
        ((`(,thing ,thing-arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument))
         (other-end (conn-dispatch-other-end-argument nil))
         (restrict-windows (conn-dispatch-restrict-windows-argument))
         (`(,action ,repeat) (conn-dispatch-action-argument)))
      (conn-dispatch-setup
       action thing thing-arg transform
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
  (interactive "e\nP")
  (unless (mouse-event-p event)
    (error "conn-last-dispatch-at-mouse must be bound to a mouse event"))
  (unless (conn-ring-list conn-dispatch-ring)
    (user-error "Dispatch ring empty"))
  (when (conn-action-stale-p (conn-previous-dispatch-action
                              (conn-ring-head conn-dispatch-ring)))
    (conn-dispatch-ring-remove-stale)
    (user-error "Last dispatch action stale"))
  (push `(no-record . (dispatch-mouse-repeat ,@(cdr event)))
        unread-command-events)
  (let ((conn-read-args-inhibit-message t)
        (conn-kapply-suppress-message t))
    (conn-repeat-last-dispatch
     (and (conn-previous-dispatch-repeat (conn-ring-head conn-dispatch-ring))
          repeat))))

(defun conn-bind-last-dispatch-to-key ()
  "Bind last dispatch command to a key.

Prefix arg REPEAT inverts the value of repeat in the last dispatch."
  (interactive)
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
   (conn-make-action 'conn-dispatch-push-button)
   (conn-anonymous-thing
     'button
     :pretty-print ( :method (_self) "all-buttons")
     :target-finder ( :method (_self _arg) 'conn-dispatch-all-buttons)
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
         (conn-make-action 'conn-dispatch-goto)
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
   (conn-make-action 'conn-dispatch-jump)
   nil nil nil
   :other-end :no-other-end))

;;;;; Dispatch Bounds

(oclosure-define (conn-over-argument
                  (:parent conn-read-args-argument)))

(defun conn--dispatch-bounds (bounds &optional subregions-p)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix (conn-bounds-arg bounds)
                   :prompt "Bounds of Dispatch"
                   :reference (list conn-dispatch-command-ref
                                    conn-dispatch-thing-ref))
      ((`(,thing ,thing-arg) (conn-thing-argument t))
       (transform (conn-dispatch-transform-argument))
       (repeat (conn-dispatch-repeat-argument subregions-p)))
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
                 (window pt thing thing-arg transform)
               (with-selected-window window
                 (pcase (conn-bounds-of-dispatch thing thing-arg pt)
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
                    (user-error "No %s found at point" thing)))))
             thing thing-arg transform
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

(cl-defmethod conn-bounds-of ((cmd (conn-thing dispatch)) arg)
  (conn-make-bounds
   cmd arg
   (lambda (bounds) (conn--dispatch-bounds bounds))
   :subregions (lambda (bounds) (conn--dispatch-bounds bounds t))))

(cl-defmethod conn-dispatch-bounds-between (bounds)
  (pcase bounds
    ((conn-dispatch-bounds `(,beg . ,end))
     (let (obeg oend)
       (conn-dispatch-setup
        (oclosure-lambda (conn-action
                          (action-no-history t)
                          (action-description "Bounds")
                          (action-window-predicate
                           (let ((win (selected-window)))
                             (lambda (window) (eq win window)))))
            (window pt thing thing-arg transform)
          (with-selected-window window
            (pcase (conn-bounds-of-dispatch thing thing-arg pt)
              ((conn-dispatch-bounds `(,beg . ,end) transform)
               (setq obeg beg
                     oend end))
              (_ (user-error "No %s found at point" thing)))))
        (conn-bounds-thing bounds)
        (conn-bounds-arg bounds)
        (when (conn-transformed-bounds-p bounds)
          (conn-transformed-bounds-transforms bounds))
        :other-end nil)
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

(cl-defmethod register-val-jump-to ((val conn-dispatch-register) arg)
  (let ((prev (conn-dispatch-register-dispatch val)))
    (conn-dispatch-setup-previous
     prev
     :repeat (xor arg (conn-previous-dispatch-repeat prev)))))

(cl-defmethod register-val-describe ((val conn-dispatch-register) _verbose)
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
  (conn-dispatch-all-defuns))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing visual-line))
                                      _arg)
  #'conn-dispatch-visual-lines)

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
  (conn-dispatch-all-things 'sentence))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing paragraph))
                                      _arg)
  (conn-dispatch-all-things 'paragraph))

(cl-defmethod conn-get-target-finder ((_cmd (eql forward-char))
                                      _arg)
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line))
                                      _arg)
  #'conn-dispatch-lines)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column))
                                      _arg)
  #'conn-dispatch-columns)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing outer-line))
                                      _arg)
  #'conn-dispatch-lines)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing inner-line))
                                      _arg)
  #'conn-dispatch-inner-lines)

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line))
                                      _arg)
  #'conn-dispatch-end-of-inner-lines)

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-forward-inner-line-dwim))
                                      _arg)
  #'conn-dispatch-end-of-inner-lines)

(provide 'conn-dispatch)
