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
  (require 'cl-lib))

(defvar outline-heading-end-regexp)
(defvar treesit-defun-type-regexp)

(declare-function face-remap-remove-relative "face-remap")
(declare-function conn--kmacro-display "conn-transient")
(declare-function kmacro--keys "kmacro")
(declare-function conn-posframe--dispatch-ring-display-subr "conn-posframe")
(declare-function treesit-induce-sparse-tree "treesit")
(declare-function treesit-defun-name "treesit")
(declare-function treesit-buffer-root-node "treesit")

(declare-function conn-scroll-up "conn-commands")
(declare-function conn-scroll-down "conn-commands")
(declare-function conn-append-region "conn-commands")
(declare-function conn-register-load "conn-commands")
(declare-function conn-end-of-inner-line "conn-commands")
(declare-function conn-beginning-of-inner-line "conn-commands")
(declare-function conn-dispatch-kapply-prefix "conn-transients")
(declare-function conn-kill-thing "conn-commands")
(declare-function color-hsl-to-rgb "color")
(declare-function color-rgb-to-hsl "color")

;;;; Labels

(defcustom conn-simple-label-characters
  (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e"
        "r" "t" "y" "u" "i" "c" "v" "b" "n" "m")
  "Chars to use for label overlays for the default labeling function."
  :group 'conn
  :type '(list integer))

(defcustom conn-disptach-stable-label-characters
  `(("j" "u" "m" "k" "i" "," "l" "o" "h" "y" "n" "p" ";")
    ("f" "r" "v" "d" "e" "c" "s" "w" "x" "g" "t" "b" "a"))
  "Chars to use for label overlays when recording a keyboard macro."
  :group 'conn
  :type '(list integer))

(defface conn-dispatch-action-pulse-face
  '((t (:inherit pulse-highlight-start-face)))
  "Face for highlight pulses after dispatch actions."
  :group 'conn-face)

(defvar conn-dispatch-undo-pulse-face nil)

(defface conn--dispatch-action-current-pulse-face
  '((t (:inherit conn-dispatch-undo-pulse-face)))
  "Face for current action pulse, do not customize."
  :group 'conn-face)

(defface conn-dispatch-label-face
  '((t (:inherit highlight :bold t)))
  "Face for group in dispatch lead overlay."
  :group 'conn-faces)

(defvar conn-dispatch-label-alt-face nil)

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
    (named-let rec ((curr 0))
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
              (rec (1+ curr)))
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

(defmacro conn-with-dispatch-event-handler (tag keymap message-fn handler &rest body)
  (declare (indent 4))
  (cl-once-only (keymap)
    (let* ((body `(let ((conn--dispatch-read-event-handlers
                         (cons ,handler conn--dispatch-read-event-handlers))
                        (conn--dispatch-read-event-message-prefixes
                         ,(if message-fn
                              `(cons ,message-fn conn--dispatch-read-event-message-prefixes)
                            'conn--dispatch-read-event-message-prefixes)))
                    ,@body)))
      (if (or (not tag) (eq tag '_))
          (if keymap
              `(let ((conn--dispatch-event-handler-maps
                      (cons ,keymap conn--dispatch-event-handler-maps)))
                 ,body)
            body)
        `(catch ,tag
           ,(if keymap
                `(let ((conn--dispatch-event-handler-maps
                        (cons ,keymap conn--dispatch-event-handler-maps)))
                   ,body)
              body))))))

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

(defun conn-label-select (candidates char-reader &optional prompt always-prompt)
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
                         (push l next)))
             conn--read-args-error-message nil)))))

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
(defun conn--get-windows (&optional window minibuffer all-frames dedicated predicate)
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
          (conn-with-dispatch-event-handler 'mouse-click
              nil nil
              (lambda (cmd)
                (when (or (and (eq cmd 'act)
                               (mouse-event-p last-input-event))
                          (eq 'dispatch-mouse-repeat
                              (event-basic-type last-input-event)))
                  (let* ((posn (event-start last-input-event))
                         (win (posn-window posn)))
                    (when (and (not (posn-area posn))
                               (funcall conn-target-window-predicate win))
                      (throw 'mouse-click win)))))
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
(defvar conn-dispatch-repeat-count nil)

(defvar conn-dispatch-other-end nil)
(defvar conn-dispatch-no-other-end nil)

(defvar conn-dispatch-override-target-finders nil)

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
  :mode-line-face 'conn-dispatch-mode-line-face
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(conn-define-state conn-dispatch-bounds-state (conn-dispatch-targets-state)
  :lighter "DISPATCH"
  :mode-line-face 'conn-dispatch-mode-line-face
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(conn-define-state conn-dispatch-state (conn-dispatch-targets-state)
  "State for reading a dispatch command."
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(conn-define-state conn-dispatch-thingatpt-state (conn-dispatch-state)
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(put 'repeat-dispatch :advertised-binding (key-parse "TAB"))

(cl-defgeneric conn-make-default-action (cmd)
  (declare (conn-anonymous-thing-property :default-action)
           (important-return-value t)))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing t)))
  (conn-make-action 'conn-dispatch-goto))

(defun conn-dispatch-setup-target-finder (cmd arg)
  (or (when conn-dispatch-override-target-finders
        (funcall conn-dispatch-override-target-finders cmd arg))
      (conn-get-target-finder cmd arg)))

(cl-defgeneric conn-get-target-finder (cmd arg)
  (declare (conn-anonymous-thing-property :target-finder)
           (important-return-value t)))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing t))
                                      _arg)
  (conn-dispatch-read-n-chars
   :string-length 2
   :hide-target-overlays t))

(defun conn--dispatch-restrict-windows (win)
  (declare (side-effect-free t))
  (eq win (selected-window)))

(defun conn--dispatch-command-affixation (command-names)
  (with-selected-window (or (minibuffer-selected-window) (selected-window))
    (cl-loop
     for command-name in command-names
     collect (let* ((fun (and (stringp command-name) (intern-soft command-name)))
                    (binding (where-is-internal fun nil t))
                    (binding (if (and binding (not (stringp binding)))
                                 (format " {%s}" (key-description binding))
                               ""))
                    (thing (format " (%s)" (or (conn-command-thing fun)
                                               "action"))))
               (put-text-property 0 (length binding)
                                  'face 'help-key-binding binding)
               (put-text-property 0 (length thing)
                                  'face 'completions-annotations thing)
               (list command-name "" (concat thing binding))))))

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
  "x" 'conn-bounds-trim
  "a" 'conn-bounds-after-point
  "b" 'conn-bounds-before-point
  "X" 'conn-transform-reset)

(defun conn-dispatch-transform-argument ()
  (declare (important-return-value t))
  (let ((conn-transform-map conn-dispatch-transform-map))
    (conn-transform-argument)))

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

(defvar conn-dispatch-thing-ref-extras
  (conn-reference-quote
    (("symbol" forward-symbol)
     ("line" forward-line)
     ("column" next-line)
     ("defun"
      (:eval (conn-quick-ref-find-remap
              conn-end-of-defun-remap
              (conn-get-state-map 'conn-dispatch-targets-state)))))))

(defvar conn-dispatch-thing-ref
  (conn-reference-page "Things"
    "Use a thing command to specify a region to operate on."
    "Dispatch state redefines some thing bindings:
"
    ((:keymap (list (conn-get-state-map 'conn-dispatch-targets-state)))
     (:splice (conn-quick-ref-to-cols conn-dispatch-thing-ref-extras)))))

(defvar conn-dispatch-action-ref
  (conn-reference-page "Actions"
    ((("copy from/replace"
       conn-dispatch-copy-from
       conn-dispatch-copy-from-replace)
      ("yank to/replace"
       conn-dispatch-yank-to
       conn-dispatch-yank-to-replace)
      ("yank read/replace"
       conn-dispatch-reading-yank-to
       conn-dispatch-reading-yank-to-replace)
      ("send/replace"
       conn-dispatch-send
       conn-dispatch-send-replace)
      ("take/replace"
       conn-dispatch-take
       conn-dispatch-take-replace))
     (("copy to"
       conn-dispatch-copy-to
       conn-dispatch-copy-replace-to)
      ("transpose" conn-dispatch-transpose)
      ("over" conn-dispatch-over)
      ("kapply" conn-dispatch-kapply)
      ("register load/replace"
       conn-dispatch-register-load
       conn-dispatch-register-load-replace)))))

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
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-action-argument ()
  (declare (important-return-value t))
  (setq conn--dispatch-thing-predicate #'always)
  (oclosure-lambda (conn-dispatch-action-argument)
      (self type)
    (if (not (conn-argument-predicate self type))
        self
      (conn-cancel-action value)
      (if-let* ((_(not (cl-typep value type)))
                (action (conn-make-action type)))
          (progn
            (setq conn--dispatch-thing-predicate
                  (or (conn-action--thing-predicate action)
                      #'always))
            (conn-set-argument self action))
        (conn-set-argument self nil)))))

(cl-defmethod conn-argument-cancel ((arg conn-dispatch-action-argument))
  (conn-cancel-action (conn-read-args-argument-value arg)))

(cl-defmethod conn-argument-value :before ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-read-args-argument-value arg)))
    (conn-accept-action action)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-action-argument)
                                       sym)
  (conn--action-type-p sym))

(cl-defmethod conn-argument-display ((arg conn-dispatch-action-argument))
  (when-let* ((action (conn-read-args-argument-value arg)))
    (propertize (conn-action-pretty-print action)
                'face 'eldoc-highlight-function-argument)))

;;;;;; Other End

(oclosure-define (conn-dispatch-other-end-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-other-end-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-other-end-argument
                    (value value))
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

(defvar conn-dispatch-autorepeat-actions (list 'conn-dispatch-kapply))

(oclosure-define (conn-dispatch-repeat-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-repeat-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-repeat-argument
                    (value value))
      (self cmd)
    (if (or (eq cmd 'repeat-dispatch)
            (memq cmd conn-dispatch-autorepeat-actions))
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-dispatch-repeat-argument)
                                       (_sym (eql dispatch-repeat)))
  t)

(cl-defmethod conn-argument-display ((arg conn-dispatch-repeat-argument))
  (concat "\\[repeat-dispatch] "
          (propertize "repeat"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

;;;;;; Restrict Windows

(oclosure-define (conn-dispatch-restrict-windows-argument
                  (:parent conn-read-args-argument)))

(defun conn-dispatch-restrict-windows-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-dispatch-restrict-windows-argument
                    (value value))
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
    (user-error "Dispatch ring empty")))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-cycle-ring-previous)))
  (condition-case _
      (progn
        (conn-dispatch-cycle-ring-previous)
        (if (bound-and-true-p conn-posframe-mode)
            (conn-posframe--dispatch-ring-display-subr)
          (conn-read-args-message "%s" (conn-describe-dispatch
                                        (conn-ring-head conn-dispatch-ring))))
        (conn-read-args-handle))
    (user-error "Dispatch ring empty")))

(cl-defmethod conn-dispatch-command-handler ((_ (eql conn-dispatch-ring-describe-head)))
  (conn-dispatch-ring-remove-stale)
  (if-let* ((head (conn-ring-head conn-dispatch-ring)))
      (if (bound-and-true-p conn-posframe-mode)
          (conn-posframe--dispatch-ring-display-subr)
        (conn-read-args-message "%s" (conn-describe-dispatch head)))
    (user-error "Dispatch ring empty"))
  (conn-read-args-handle))

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

(defvar conn--dispatch-prev-state nil)

(put 'conn-target-overlay 'conn-overlay t)
(put 'conn-target-overlay 'priority 2002)

(cl-defun conn-make-target-overlay ( pt length
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
                  (overlay-get o 'label-string) nil)
            (throw 'done nil))))
      (when thing (overlay-put ov 'thing thing))
      (cl-loop for (prop val) on properties by #'cddr
               do (overlay-put ov prop val))
      (push ov (alist-get window conn-targets))
      ov)))

(defun conn-make-string-target-overlays (string &optional predicate fixed-length)
  (when (length> string 0)
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-matches string predicate))
          (conn-make-target-overlay beg (or fixed-length (- end beg))))))))

(defun conn-make-re-target-overlays (regexp &optional predicate)
  (when (length> regexp 0)
    (dolist (win (conn--get-target-windows))
      (with-selected-window win
        (pcase-dolist (`(,beg . ,end)
                       (conn--visible-re-matches regexp predicate))
          (conn-make-target-overlay beg (- end beg)))))))

(defun conn--read-string-with-timeout (&optional predicate)
  (unwind-protect
      (conn-with-input-method
        (let* ((prompt (propertize "String" 'face 'minibuffer-prompt))
               (string (char-to-string (read-char prompt t))))
          (while-no-input
            (conn-make-string-target-overlays string predicate))
          (while-let ((next-char (read-char (format (concat prompt "%s ") string)
                                            t conn-read-string-timeout)))
            (setq string (concat string (char-to-string next-char)))
            (conn-delete-targets)
            (while-no-input
              (conn-make-string-target-overlays string predicate)))
          (message nil)
          string))
    (conn-delete-targets)))

;;;;; Dispatch Labels

(defvar conn-dispatch-target-finder nil)

(defvar conn-dispatch-label-function 'conn-dispatch-smart-labels
  "Function responsible for labeling all `conn-targets'.

A labeling function should take a single argument STATE and
return either a list of labels or a list of the form
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

(defun conn-disptach-label-target (target string)
  (declare (important-return-value t))
  (let ((window (overlay-get target 'window)))
    (when (<= (window-start window)
              (overlay-start target)
              (overlay-end target)
              (window-end window))
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
         :setup-function (if (conn-dispatch-pixelwise-label-p ov)
                             'conn--dispatch-setup-label-pixelwise
                           'conn--dispatch-setup-label-charwise)
         :padding-function (overlay-get target 'padding-function)
         :string str
         :prefix (when-let* ((pfx (overlay-get target 'label-prefix)))
                   (propertize pfx 'face face))
         :suffix (when-let* ((sfx (overlay-get target 'label-suffix)))
                   (propertize sfx 'face face))
         :narrowed-string str
         :overlay ov
         :target target)))))

(cl-defstruct (conn-simple-label-state)
  pool
  in-use
  size)

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
                             (* len 3))))
              pool (conn-simple-labels size))
        (dolist (str pool)
          (remhash str in-use)))
      (pcase-dolist (`(,win . ,targets) conn-targets)
        (when-let* ((ov (buffer-local-value 'conn--mark-cursor
                                            (window-buffer win))))
          (delete-overlay ov))
        (dolist (tar (if (eq win (selected-window))
                         (sort targets conn-target-sort-function)
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
                  (push (conn-disptach-label-target tar str) labels)))
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
                      (push (conn-disptach-label-target tar str) labels))))
      `(:state ,state ,@labels))))

(defun conn--stable-label-subr (window targets characters)
  (let ((group 0)
        (chars characters)
        (labels nil)
        (window-label
         (unless (eq window (selected-window))
           (upcase (window-parameter window 'conn-label-string)))))
    (dolist (tar targets labels)
      (when (null chars)
        (cl-incf group)
        (setq chars characters))
      (push (conn-disptach-label-target
             tar (concat window-label
                         (when (> group 0)
                           (number-to-string group))
                         (pop chars)))
            labels))))

(defun conn-dispatch-stable-labels (_state)
  (declare (important-return-value t))
  (conn--ensure-window-labels)
  (pcase-let* ((`(,bchars ,achars) conn-disptach-stable-label-characters)
               (labels nil))
    (pcase-dolist (`(,win . ,targets) conn-targets)
      (when-let* ((ov (buffer-local-value 'conn--mark-cursor
                                          (window-buffer win))))
        (delete-overlay ov))
      (let (before after)
        (dolist (tar (sort targets (lambda (a b)
                                     (< (abs (- (overlay-end a)
                                                (window-point win)))
                                        (abs (- (overlay-end b)
                                                (window-point win)))))))
          (if (> (window-point win) (overlay-start tar))
              (push tar before)
            (push tar after)))
        (push (conn--stable-label-subr win (nreverse after) achars) labels)
        (push (conn--stable-label-subr win (nreverse before) bchars) labels)))
    (apply #'nconc labels)))

(defun conn-dispatch-smart-labels (state)
  (declare (important-return-value t))
  (if (or executing-kbd-macro defining-kbd-macro)
      (conn-dispatch-stable-labels state)
    (conn-dispatch-simple-labels state)))

(defun conn--dispatch-read-event-prefix (keymap)
  (declare (important-return-value t))
  (when-let* ((prefix
               (flatten-tree
                (cl-loop for pfx in conn--dispatch-read-event-message-prefixes
                         for str = (pcase pfx
                                     ((pred functionp) (funcall pfx keymap))
                                     ((pred stringp) pfx))
                         if str collect str))))
    (concat " (" (string-join prefix "; ") ")")))

(defun conn-dispatch-read-event (&optional prompt
                                           inherit-input-method
                                           seconds
                                           prompt-suffix)
  (declare (important-return-value t))
  (let ((inhibit-message conn-read-args-inhibit-message)
        (message-log-max nil)
        (prompt-suffix (unless inhibit-message
                         (concat prompt-suffix " "
                                 (when conn--read-args-error-message
                                   (propertize conn--read-args-error-message
                                               'face 'error)))))
        (keymap (make-composed-keymap conn--dispatch-event-handler-maps
                                      conn-dispatch-read-event-map)))
    (catch 'return
      (if seconds
          (while-let ((ev (conn-with-input-method
                            (read-event (unless inhibit-message
                                          (concat prompt ": " prompt-suffix))
                                        inherit-input-method seconds))))
            (when (characterp ev)
              (throw 'return ev)))
        (while t
          (pcase (conn-with-overriding-map keymap
                   (thread-first
                     (unless inhibit-message
                       (concat prompt
                               (conn--dispatch-read-event-prefix keymap)
                               ":" prompt-suffix))
                     (read-key-sequence-vector)
                     (key-binding t)))
            ('restart (throw 'return ?\8))
            ('dispatch-character-event
             (setq conn--dispatch-must-prompt nil)
             (push `(no-record . ,last-input-event) unread-command-events)
             (throw 'return
                    (conn-with-input-method
                      (read-event
                       (unless inhibit-message
                         (concat prompt
                                 (conn--dispatch-read-event-prefix keymap)
                                 ":" prompt-suffix))
                       inherit-input-method))))
            (cmd
             (let ((unhandled nil))
               (unwind-protect
                   (catch 'dispatch-handle
                     (cl-loop for handler in conn--dispatch-read-event-handlers
                              do (funcall handler cmd))
                     (setq unhandled t))
                 (unless unhandled
                   (setf conn-read-args-last-command cmd)))
               (when (and unhandled (eq cmd 'keyboard-quit))
                 (keyboard-quit))))))))))

(defun conn-dispatch-prompt-p ()
  (or conn--dispatch-must-prompt
      conn--dispatch-action-always-prompt
      (> conn-dispatch-repeat-count 0)
      (conn-targets-prompt-p conn-dispatch-target-finder)))

(defun conn-select-target (&rest _args)
  (error "conn-select-target only allowed inside conn-perform-dispatch-loop"))

(defvar conn--previous-labels-cleanup nil)

(defun conn-cleanup-labels ()
  (when conn--previous-labels-cleanup
    (funcall conn--previous-labels-cleanup)))

(defmacro conn-with-dispatch-labels (labels &rest body)
  (declare (indent 1))
  `(let (,labels)
     (unwind-protect
         (progn ,@body)
       (conn-cleanup-labels)
       (letrec ((cleanup
                 (lambda (&rest _)
                   (unwind-protect
                       (mapc #'conn-label-delete ,(car labels))
                     (setq conn--previous-labels-cleanup nil)
                     (remove-hook 'pre-redisplay-functions cleanup)))))
         (add-hook 'pre-redisplay-functions cleanup)
         (setq conn--previous-labels-cleanup cleanup)))))

(cl-defgeneric conn-dispatch-select-target (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-dispatch-select-target :around (_target-finder)
  (let ((conn--dispatch-remap-cookies nil))
    (conn-with-dispatch-event-handler 'mouse-click
        nil nil
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
                (throw 'mouse-click (list pt win nil))))))
      (conn-dispatch-select-mode 1)
      (unwind-protect
          (let ((inhibit-message t))
            (cl-call-next-method))
        (conn-dispatch-select-mode -1)))))

(cl-defmethod conn-dispatch-select-target (target-finder)
  (conn-setup-targets target-finder)
  (conn-with-dispatch-labels
   (labels (pcase (funcall conn-dispatch-label-function
                           conn--dispatch-label-state)
             (`(:state ,state . ,labels)
              (setq conn--dispatch-label-state state)
              labels)
             (labels labels)))
   (conn-label-select labels
                      #'conn-dispatch-read-event
                      (cl-loop for (_ . c) in conn-target-count
                               sum c into count
                               finally return (format "Label [%s]" count))
                      (conn-dispatch-prompt-p))))

;;;;; Perform Dispatch Loop

(defvar conn-dispatch-in-progress nil)
(defvar conn--dispatch-undo-change-groups nil)
(defvar conn--prev-scroll-conservatively nil)

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

(cl-defun conn-dispatch-handle-and-redisplay (&key (prompt t))
  (redisplay)
  (setq conn--dispatch-must-prompt prompt)
  (throw 'dispatch-redisplay nil))

(defun conn-dispatch-handle ()
  (throw 'dispatch-handle t))

(defmacro conn-perform-dispatch-loop (repeat &rest body)
  (declare (indent 1))
  (setq body `(progn
                (push nil conn--dispatch-undo-change-groups)
                ,@body
                (cl-incf conn-dispatch-repeat-count)))
  (when repeat
    (setq body `(condition-case err
                    ,body
                  (user-error
                   (pcase-dolist (`(,_ . ,undo-fn)
                                  (pop conn--dispatch-undo-change-groups))
                     (funcall undo-fn :undo))
                   (setf conn--read-args-error-message
                         (error-message-string err))))))
  (cl-with-gensyms (rep display-always success)
    `(let* ((,rep nil)
            (,display-always nil)
            (,success nil)
            (conn--dispatch-label-state nil)
            (conn--prev-scroll-conservatively scroll-conservatively)
            (scroll-conservatively 100)
            (conn-dispatch-repeating (and ,repeat t))
            (conn--dispatch-undo-change-groups nil)
            (conn--read-args-error-message nil)
            (conn--dispatch-read-event-message-prefixes
             `(,(car conn--dispatch-read-event-message-prefixes)
               ,@(cdr conn--dispatch-read-event-message-prefixes))))
       (unwind-protect
           (cl-flet ((conn-select-target ()
                       (conn-dispatch-select-target
                        conn-dispatch-target-finder)))
             (catch 'dispatch-select-exit
               (let ((conn-dispatch-in-progress t))
                 (while (or (setq ,rep ,repeat)
                            (< conn-dispatch-repeat-count 1))
                   (catch 'dispatch-redisplay
                     ,body))))
             (setq ,success (not dispatch-quit-flag)))
         (dolist (undo conn--dispatch-undo-change-groups)
           (pcase-dolist (`(,_ . ,undo-fn) undo)
             (funcall undo-fn (if ,success :accept :cancel)))))
       (when dispatch-quit-flag (keyboard-quit)))))

(defmacro conn-with-dispatch-suspended (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (select-mode)
    `(progn
       (pcase-dolist (`(_ . ,targets) conn-targets)
         (dolist (target targets)
           (overlay-put target 'category 'conn-old-target)
           (overlay-put target 'face nil)))
       (conn-cleanup-labels)
       (clrhash conn--pixelwise-window-cache)
       (clrhash conn--dispatch-window-lines-cache)
       (setq conn-target-count nil)
       (conn-targets-suspend conn-dispatch-target-finder)
       (pcase-let ((`(,conn-target-window-predicate
                      ,conn-target-predicate
                      ,conn-target-sort-function)
                    conn--dispatch-prev-state)
                   (conn-targets nil)
                   (conn--dispatch-label-state nil)
                   (scroll-conservatively conn--prev-scroll-conservatively)
                   (conn-dispatch-target-finder nil)
                   (conn-dispatch-in-progress nil)
                   (conn--dispatch-undo-change-groups nil)
                   (inhibit-message nil)
                   (recenter-last-op nil)
                   (conn-dispatch-repeat-count nil)
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
  (conn-read-args (conn-dispatch-targets-state
                   :prompt "New Targets"
                   :reference (list conn-dispatch-thing-ref)
                   :around (lambda (cont)
                             (conn-with-dispatch-suspended
                               (funcall cont))))
      ((`(,thing ,thing-arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument)))
    (conn-dispatch-change-target thing thing-arg transform)))

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
  (conn-targets-retarget conn-dispatch-target-finder)
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

(defun conn-dispatch-change-target (thing thing-arg thing-transform)
  (conn-targets-cleanup conn-dispatch-target-finder)
  (setq conn-dispatch-target-finder (conn-get-target-finder thing thing-arg))
  (throw 'dispatch-change-target (list thing thing-arg thing-transform)))

;;;;; Dispatch Target Finders

(defface conn-dispatch-context-separator-face
  '((t (:inherit (shadow tooltip) :extend t)))
  "Face for context region separator."
  :group 'conn)

(defun conn-target-nearest-op (a b)
  (declare (side-effect-free t)
           (important-return-value t))
  (< (abs (- (overlay-end a) (point)))
     (abs (- (overlay-end b) (point)))))

(defun conn-delete-targets ()
  (pcase-dolist (`(_ . ,targets) conn-targets)
    (dolist (target targets)
      (delete-overlay target)))
  (clrhash conn--pixelwise-window-cache)
  (clrhash conn--dispatch-window-lines-cache)
  (setq conn-targets nil
        conn-target-count nil))

(cl-defgeneric conn-targets-update (target-finder))

(cl-defmethod conn-targets-update (target-finder)
  (when (functionp target-finder)
    (funcall target-finder)))

(defun conn-dispatch-label-alt-background ()
  (or conn-dispatch-label-alt-face
      (pcase (thread-last
               (face-background 'conn-dispatch-label-face)
               (color-name-to-rgb)
               (apply #'color-rgb-to-hsl))
        (`(,h ,s ,l)
         (apply #'color-rgb-to-hex (color-hsl-to-rgb h s (* l 1.1))))
        (_ 'conn-dispatch-label-face))))

(cl-defgeneric conn-targets-label-faces (target-finder))

(cl-defmethod conn-targets-label-faces (_target-finder)
  (let ((face1 'conn-dispatch-label-face)
        (face2 `( :inherit conn-dispatch-label-face
                  :background ,(conn-dispatch-label-alt-background))))
    (pcase-dolist (`(,_window . ,targets) conn-targets)
      (dolist (tar (sort targets :key #'overlay-start))
        (overlay-put tar 'label-face face1)
        (cl-rotatef face1 face2)))))

(cl-defgeneric conn-targets-cleanup (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-targets-suspend (target-finder)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-targets-other-end (target-finder)
  "Default value for :other-end parameter in `conn-perform-dispatch'."
  (declare (conn-anonymous-thing-property :other-end)
           (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-targets-save-state (target-finder)
  "Return a list of functions to restore TARGET-FINDER\\='s state.

Each function should take an uninitialized target finder of the same
type and initialize it to contain the same state as TARGET-FINDER."
  (declare (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-targets-keymaps (target-finder)
  (declare (important-return-value t))
  ( :method (_) nil))

(cl-defgeneric conn-targets-message-prefixes (target-finder)
  (declare (important-return-value t))
  (:method (_) nil))

(cl-defgeneric conn-targets-prompt-p (target-finder)
  ( :method (_target-finder) nil))

(defun conn-setup-targets (target-finder)
  (conn-cleanup-labels)
  (when conn--dispatch-always-retarget
    (conn-targets-retarget target-finder))
  (let ((old nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(_ . ,targets) conn-targets)
            (dolist (target targets)
              (overlay-put target 'category 'conn-old-target)
              (push target old)))
          (clrhash conn--pixelwise-window-cache)
          (clrhash conn--dispatch-window-lines-cache)
          (setq conn-targets nil
                conn-target-count nil)
          (conn-targets-update target-finder)
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
  (conn-targets-label-faces target-finder))

(defclass conn-dispatch-target-key-labels ()
  ()
  "Abstract type for target finders which use key-bindings as labels.

The `conn-targets-update' method for any class inheriting from
this class should set the \\='label-key overlay property of each target
to the key binding for that target."
  :abstract t)

(defun conn-dispatch-key-labels ()
  (declare (important-return-value t))
  (let ((labels nil))
    (pcase-dolist (`(,_window . ,targets) conn-targets)
      (dolist (tar targets)
        (push (conn-disptach-label-target
               tar (overlay-get tar 'label-key))
              labels)))
    labels))

(cl-defmethod conn-dispatch-select-target ((target-finder
                                            conn-dispatch-target-key-labels))
  (conn-setup-targets target-finder)
  (conn-with-dispatch-labels
   (labels (conn-dispatch-key-labels))
   (conn-with-dispatch-event-handler 'label
       (let ((map (make-sparse-keymap)))
         (cl-loop for label in labels
                  for key = (conn-dispatch-label-string label)
                  do (keymap-set map key label))
         (mapc #'conn-label-redisplay labels)
         map)
       nil
       (lambda (obj)
         (when (conn-dispatch-label-p obj)
           (throw 'label obj)))
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

(cl-defmethod conn-targets-update :before ((state conn-dispatch-target-window-predicate))
  (let ((pred (oref state window-predicate)))
    (unless (advice-function-member-p pred conn-target-window-predicate)
      (add-function :before-while conn-target-window-predicate pred)))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(cl-defmethod conn-targets-cleanup ((state conn-dispatch-target-window-predicate))
  (remove-function conn-target-window-predicate
                   (oref state window-predicate))
  (ignore-error cl-no-next-method
    (cl-call-next-method)))

(defclass conn-dispatch-retargetable-target () ()
  :abstract t)

(defvar-keymap conn-dispatch-retargetable-map
  "M-f" 'always-retarget
  "C-f" 'retarget)

(cl-defgeneric conn-targets-retarget (target-finder))

(cl-defgeneric conn-dispatch-has-targets-p (target-finder)
  (declare (important-return-value t)))

(cl-defmethod conn-targets-keymaps ((_ conn-dispatch-retargetable-target))
  conn-dispatch-retargetable-map)

(cl-defmethod conn-targets-message-prefixes ((state conn-dispatch-retargetable-target))
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

(cl-defmethod conn-targets-save-state ((target-finder conn-dispatch-string-targets))
  (cons (let ((str (oref target-finder string)))
          (lambda (tf) (setf (oref tf string) str)))
        (cl-call-next-method)))

(cl-defmethod conn-targets-retarget ((state conn-dispatch-string-targets))
  (setf (oref state string) nil))

(cl-defmethod conn-dispatch-has-targets-p ((state conn-dispatch-string-targets))
  (and (oref state string) t))

(defclass conn-dispatch-read-n-chars (conn-dispatch-string-targets)
  ((string-length :initform 1 :initarg :string-length)
   (predicate :initform nil :initarg :predicate)
   (hide-target-overlays :initform nil :initarg :hide-target-overlays)))

(cl-defmethod conn-targets-update ((state conn-dispatch-read-n-chars))
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
      (conn-with-input-method
        (while (length< string string-length)
          (when (length> string 0)
            (while-no-input
              (conn-make-string-target-overlays string predicate)))
          (catch 'dispatch-redisplay
            (conn-with-dispatch-event-handler 'backspace
                (define-keymap "<remap> <backward-delete-char>" 'backspace)
                nil
                (lambda (cmd)
                  (when (eq cmd 'backspace)
                    (when (length> string 0)
                      (cl-callf substring string 0 -1))
                    (throw 'backspace nil)))
              (cl-callf thread-last
                  string
                (conn-dispatch-read-event prompt t)
                (char-to-string)
                (concat string))))
          (conn-delete-targets)))
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
   (predicate :initform nil :initarg :predicate)))

(cl-defmethod conn-targets-update ((state conn-dispatch-read-with-timeout))
  (with-slots (string timeout predicate) state
    (if string
        (conn-make-string-target-overlays string predicate)
      (let* ((prompt (propertize "String" 'face 'minibuffer-prompt)))
        (setq string (char-to-string (conn-dispatch-read-event prompt t)))
        (while-no-input
          (conn-make-string-target-overlays string predicate))
        (while-let ((next-char (conn-dispatch-read-event
                                prompt t timeout string)))
          (conn-delete-targets)
          (setq string (concat string (char-to-string next-char)))
          (while-no-input
            (conn-make-string-target-overlays string predicate))))))
  (cl-call-next-method))

(defun conn-dispatch-read-string-with-timeout (&optional predicate)
  (declare (important-return-value t))
  (conn-dispatch-read-with-timeout
   :timeout conn-read-string-timeout
   :predicate predicate))

(defclass conn-dispatch-focus-targets ()
  ((hidden :initform nil)
   (context-lines :initform 0 :initarg :context-lines)
   (cursor-location :initform nil)
   (separator-p :initarg :separator))
  "Abstract type for target finders that hide buffer contents that do not
contain targets."
  :abstract t)

(cl-defmethod conn-targets-prompt-p ((_state conn-dispatch-focus-targets))
  t)

(cl-defmethod conn-targets-cleanup ((state conn-dispatch-focus-targets))
  (mapc #'delete-overlay (oref state hidden))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-targets-suspend ((state conn-dispatch-focus-targets))
  (mapc #'delete-overlay (oref state hidden))
  (setf (oref state hidden) nil)
  (cl-call-next-method))

(cl-defmethod conn-handle-dispatch-select-command ((_ (eql scroll-down))
                                                   &context (conn-dispatch-target-finder
                                                             conn-dispatch-focus-targets))
  (let ((col (current-column)))
    (goto-char (window-start))
    (move-to-column col))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_ (eql scroll-up))
                                                   &context (conn-dispatch-target-finder
                                                             conn-dispatch-focus-targets))
  (let ((col (current-column)))
    (goto-char (window-end))
    (move-to-column col))
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-handle-dispatch-select-command ((_cmd (eql recenter-top-bottom))
                                                   &context (conn-dispatch-target-finder
                                                             conn-dispatch-focus-targets))
  (cl-callf thread-first
      (alist-get (selected-window)
                 (oref conn-dispatch-target-finder cursor-location))
    (memq recenter-positions)
    (cadr)
    (or (car recenter-positions)))
  (pulse-momentary-highlight-one-line)
  (conn-dispatch-handle-and-redisplay))

(cl-defmethod conn-targets-update ((state conn-dispatch-focus-targets))
  (mapc #'delete-overlay (oref state hidden))
  (setf (oref state hidden) nil)
  (cl-call-next-method)
  (conn-protected-let* ((hidden (list (make-overlay (point-min) (point-min)))
                                (mapc #'delete-overlay hidden))
                        (context-lines (oref state context-lines))
                        (separator-p (if (slot-boundp state 'separator-p)
                                         (oref state separator-p)
                                       (> context-lines 0))))
    (pcase-dolist (`(,win . ,targets) conn-targets)
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
             (recenter nil))))))
    (setf (oref state hidden) hidden)
    (sit-for 0)))

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

(cl-defmethod conn-targets-update ((state conn-dispatch-focus-thing-at-point))
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

(cl-defmethod conn-targets-other-end ((_ conn-dispatch-mark-ring))
  :no-other-end)

(cl-defmethod conn-targets-update ((_state conn-dispatch-mark-ring))
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

(cl-defmethod conn-targets-other-end ((_ conn-dispatch-global-mark))
  :no-other-end)

(cl-defmethod conn-targets-update ((_state conn-dispatch-global-mark))
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

(cl-defmethod conn-targets-other-end ((_ conn-dispatch-mark-register))
  :no-other-end)

(cl-defmethod conn-targets-update ((_state conn-dispatch-mark-register))
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

(cl-defmethod conn-targets-other-end ((_ conn-dispatch-previous-emacs-state))
  :no-other-end)

(cl-defmethod conn-targets-update ((_state conn-dispatch-previous-emacs-state))
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

(cl-defmethod conn-targets-update ((_state conn-dispatch-headings))
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
    :initform (lambda (win) (eq win (selected-window))))))

(defvar-local conn-extract-defuns-function
  'conn--dispatch-extract-defuns-default)

(defun conn--dispatch-extract-defuns-default (win)
  (conn-for-each-visible (point-min) (point-max)
    (goto-char (point-max))
    (while (beginning-of-defun)
      (conn-make-target-overlay (point) 0 :window win))))

(defun conn--dispatch-extract-defuns-treesit (win)
  (treesit-induce-sparse-tree
   (treesit-buffer-root-node)
   (or treesit-defun-type-regexp 'defun)
   (lambda (node)
     (save-excursion
       (goto-char (treesit-node-start node))
       (conn-make-target-overlay
        (point) 0
        :window win
        :properties `(context
                      ,(cons (pos-bol)
                             (progn
                               (when-let* ((name (treesit-defun-name node)))
                                 (search-forward name))
                               (pos-bol 2)))))))))

(cl-defmethod conn-targets-update ((_state conn-dispatch-all-defuns))
  (dolist (win (conn--get-target-windows))
    (with-current-buffer (window-buffer win)
      (funcall conn-extract-defuns-function win)))
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
    (save-excursion
      (pcase-dolist (`(,vbeg . ,vend)
                     (conn--visible-regions (window-start) (window-end)))
        (goto-char vbeg)
        (unless (zerop goal-column)
          (move-to-column goal-column))
        (unless (and (bolp) (not (bobp))
                     (invisible-p (1- (point))))
          (conn-make-target-overlay
           (point) 0
           :padding-function padding-function))
        (while (< (point) vend)
          (forward-line)
          (unless (zerop goal-column)
            (move-to-column goal-column))
          (unless (and (bolp) (invisible-p (1- (point))))
            (conn-make-target-overlay
             (point) 0
             :padding-function padding-function)))))))

(cl-defmethod conn-targets-label-faces ((_ (eql conn-dispatch-columns)))
  nil)

(defun conn-dispatch-lines ()
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (let ((goal-column (window-hscroll)))
        (conn-dispatch-columns)))))

(cl-defmethod conn-targets-label-faces ((_ (eql conn-dispatch-lines)))
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

(cl-defmethod conn-targets-other-end ((_ (eql conn-dispatch-end-of-lines)))
  t)

(defun conn-dispatch-inner-lines ()
  (let ((thing (conn-anonymous-thing
                 'conn-forward-inner-line
                 :description ( :method (_self) "inner-line")
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
                 :description ( :method (_self) "end-of-inner-line")
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

(cl-defmethod conn-targets-other-end
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
         :thing 'char
         :padding-function (lambda (ov width _face)
                             (conn--right-justify-padding ov width nil)))
        (vertical-motion 1)
        (while (<= (point) (window-end))
          (if (= (point) (point-max))
              (conn-make-target-overlay
               (point) 0
               :thing 'char
               ;; hack to get the label displayed on its own line
               :properties `(after-string
                             ,(propertize " " 'display '(space :width 0))))
            (conn-make-target-overlay
             (point) 0
             :thing 'char
             :padding-function (lambda (ov width _face)
                                 (conn--right-justify-padding ov width nil))))
          (vertical-motion 1))))))

;;;;; Dispatch Actions

(oclosure-define (conn-action
                  (:predicate conn-action-p)
                  (:copier conn-action--copy))
  (no-history :mutable t :type boolean)
  (description :type (or string nil))
  (window-predicate :type function)
  (target-predicate :type function)
  (thing-predicate :type function)
  (always-retarget :type boolean)
  (always-prompt :type boolean))

(defvar conn-dispatch-amalgamate-undo nil)

(defmacro conn-dispatch-undo-case (depth &rest body)
  (declare (indent 1))
  (cl-assert (<= -100 depth 100))
  (cl-with-gensyms (do buf)
    `(progn
       (push (cons ,depth (let ((,buf (current-buffer)))
                            (lambda (,do)
                              (with-current-buffer ,buf
                                (pcase ,do ,@body)))))
             (car conn--dispatch-undo-change-groups))
       (conn--compat-callf sort (car conn--dispatch-undo-change-groups)
         :key #'car
         :in-place t))))

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

(defun conn-dispatch-action-pulse (beg end)
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (face-background 'pulse-highlight-start-face
                    nil
                    'default))
  (pulse-momentary-highlight-region
   beg end
   'conn--dispatch-action-current-pulse-face))

(defun conn-dispatch-undo-pulse (beg end)
  (require 'pulse)
  (set-face-background
   'conn--dispatch-action-current-pulse-face
   (or conn-dispatch-undo-pulse-face
       (pcase-let ((`(,h ,s ,l)
                    (apply #'color-rgb-to-hsl
                           (color-name-to-rgb
                            (face-background
                             'pulse-highlight-start-face
                             nil
                             'default)))))
         (apply #'color-rgb-to-hex
                (color-hsl-to-rgb
                 (+ h (* .5 float-pi))
                 s
                 (if (> l .5)
                     (- l .2)
                   (+ l .2)))))))
  (pulse-momentary-highlight-region
   beg end
   'conn--dispatch-action-current-pulse-face))

(defun conn--action-type-p (item)
  (declare (important-return-value t)
           (side-effect-free t))
  (when-let* ((class (and (symbolp item)
                          (cl--find-class item))))
    (and (oclosure--class-p class)
         (memq 'conn-action (oclosure--class-allparents class)))))

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
    (conn-action--description action)))

(cl-defgeneric conn-accept-action (action)
  (:method ((action conn-action)) action))

(cl-defgeneric conn-cancel-action (action)
  (:method (_) "Noop" nil))

(cl-defgeneric conn-make-action (type)
  (declare (important-return-value t))
  (:method (type) (error "Unknown action type %s" type))
  (:method :after (_type) (conn-read-args-consume-prefix-arg)))

(cl-defmethod conn-make-action :around (type)
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (or (cl-call-next-method)
            (error "Failed to construct %s" type))
      (set-window-configuration wconf))))

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
                    (description "Goto"))
      (window pt thing thing-arg transform)
    (select-window window)
    (conn-dispatch-loop-undo-boundary)
    (unless (and (= pt (point))
                 (region-active-p))
      (let ((forward (< (point) pt)))
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (when conn-dispatch-other-end
             (cl-rotatef beg end))
           (if (region-active-p)
               (goto-char (if forward end beg))
             (conn--push-ephemeral-mark end)
             (goto-char beg)))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-push-button
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-push-button)))
  (oclosure-lambda (conn-dispatch-push-button
                    (description "Push Button")
                    (no-history t))
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
      ((`(,thing ,arg) (conn-thing-argument-dwim))
       (transform (conn-transform-argument))
       (separator (conn-dispatch-separator-argument 'default)))
    (let ((str (pcase (conn-bounds-of thing arg)
                 ((conn-bounds `(,beg . ,end) transform)
                  (save-mark-and-excursion
                    (goto-char beg)
                    (conn--push-ephemeral-mark end)
                    (funcall region-extract-function nil))))))
      (oclosure-lambda (conn-dispatch-copy-to
                        (str str)
                        (separator
                         (if (eq separator 'default)
                             (cond ((eq thing 'region))
                                   ((seq-contains-p str ?\n #'eql)
                                    'newline)
                                   (t 'space))
                           separator))
                        (window-predicate
                         (lambda (win)
                           (not
                            (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
          (window pt thing thing-arg transform)
        (with-selected-window window
          (conn-dispatch-loop-undo-boundary)
          (save-mark-and-excursion
            (goto-char pt)
            (pcase (conn-bounds-of thing thing-arg)
              ((conn-bounds `(,beg . ,end) transform)
               (goto-char (if conn-dispatch-other-end end beg))
               (when (and separator conn-dispatch-other-end)
                 (conn-dispatch-insert-separator separator))
               (conn--push-ephemeral-mark)
               (insert-for-yank str)
               (when (and separator (not conn-dispatch-other-end))
                 (conn-dispatch-insert-separator separator))
               (unless executing-kbd-macro
                 (conn-dispatch-action-pulse
                  (- (point) (length str))
                  (point))))
              (_ (user-error "Cannot find thing at point")))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-copy-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-copy-to--separator action))))
      (format "Copy To <%s>" sep)
    "Copy To"))

(oclosure-define (conn-dispatch-copy-replace-to
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-replace-to)))
  (conn-read-args (conn-copy-state
                   :prompt "Copy Thing")
      ((`(,thing ,thing-arg) (conn-thing-argument-dwim))
       (transform (conn-transform-argument)))
    (let ((str (pcase (conn-bounds-of thing thing-arg)
                 ((conn-bounds `(,beg . ,end) transform)
                  (save-mark-and-excursion
                    (goto-char beg)
                    (conn--push-ephemeral-mark end)
                    (funcall region-extract-function nil)))
                 (_ (user-error "Cannot find thing at point")))))
      (oclosure-lambda (conn-dispatch-copy-replace-to
                        (description "Copy and Replace To")
                        (str str)
                        (window-predicate
                         (lambda (win)
                           (not
                            (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
          (window pt thing thing-arg transform)
        (with-selected-window window
          (conn-dispatch-loop-undo-boundary)
          (save-excursion
            (goto-char pt)
            (pcase (conn-bounds-of thing thing-arg)
              ((conn-bounds `(,beg . ,end) transform)
               (delete-region beg end)
               (insert-for-yank str)
               (unless executing-kbd-macro
                 (conn-dispatch-action-pulse
                  (- (point) (length str))
                  (point))))
              (_ (user-error "Cannot find thing at point")))))))))

(oclosure-define (conn-dispatch-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-yank-to-replace)))
  (oclosure-lambda (conn-dispatch-yank-to-replace
                    (description "Yank and Replace To")
                    (str (current-kill 0))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point))))
          (_ (user-error "Cannot find thing at point")))))))

(oclosure-define (conn-dispatch-reading-yank-to-replace
                  (:parent conn-action))
  (str :type string))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-reading-yank-to-replace)))
  (oclosure-lambda (conn-dispatch-reading-yank-to-replace
                    (description "Yank and Replace To")
                    (str (read-from-kill-ring "Yank: "))
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (insert-for-yank str)
           (unless executing-kbd-macro
             (conn-dispatch-action-pulse
              (- (point) (length str)) (point))))
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
                    (window-predicate
                     (lambda (win)
                       (not
                        (buffer-local-value 'buffer-read-only
                                            (window-buffer win))))))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (conn-dispatch-loop-undo-boundary)
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (goto-char (if conn-dispatch-other-end end beg))
           (when (and separator conn-dispatch-other-end)
             (conn-dispatch-insert-separator separator))
           (insert-for-yank str)
           (when (and separator (not conn-dispatch-other-end))
             (conn-dispatch-insert-separator separator))
           (unless executing-kbd-macro
             (conn-dispatch-action-pulse
              (- (point) (length str))
              (point))))
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
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char (if conn-dispatch-other-end end beg))
             (when (and separator conn-dispatch-other-end)
               (conn-dispatch-insert-separator separator))
             (insert-for-yank str)
             (when (and separator (not conn-dispatch-other-end))
               (conn-dispatch-insert-separator separator))
             (unless executing-kbd-macro
               (conn-dispatch-action-pulse
                (- (point) (length str))
                (point))))
            (_ (user-error "Cannot find thing at point")))
          (unless executing-kbd-macro
            (conn-dispatch-action-pulse
             (- (point) (length str))
             (point))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-reading-yank-to) &optional short)
  (if-let* ((sep (and (not short)
                      (conn-dispatch-reading-yank-to--separator action))))
      (format "Yank To <%s>" sep)
    "Yank To"))

(oclosure-define (conn-dispatch-send
                  (:parent conn-action))
  (str :type string)
  (separator :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send)))
  (conn-read-args (conn-kill-state
                   :prompt "Send Thing")
      ((`(,thing ,arg) (conn-thing-argument-dwim))
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
                        (description "Send")
                        (change-group cg)
                        (str str)
                        (separator
                         (if (eq separator 'default)
                             (cond ((eq thing 'region))
                                   ((seq-contains-p str ?\n #'eql) 'newline)
                                   (t 'space))
                           separator))
                        (window-predicate
                         (lambda (win)
                           (not
                            (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
          (window pt thing thing-arg transform)
        (with-selected-window window
          (conn-dispatch-loop-undo-boundary)
          (save-excursion
            (goto-char pt)
            (pcase (conn-bounds-of thing thing-arg)
              ((conn-bounds `(,beg . ,end) transform)
               (goto-char (if conn-dispatch-other-end end beg))
               (when conn-dispatch-other-end
                 (conn-dispatch-insert-separator separator))
               (insert-for-yank str)
               (when (not conn-dispatch-other-end)
                 (conn-dispatch-insert-separator separator))
               (unless executing-kbd-macro
                 (conn-dispatch-action-pulse
                  (- (point) (length str))
                  (point))))
              (_ (user-error "Cannot find thing at point")))
            (unless executing-kbd-macro
              (conn-dispatch-action-pulse
               (- (point) (length str))
               (point)))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send))
  (conn--action-accept-change-group (conn-dispatch-send--change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send))
  (conn--action-cancel-change-group (conn-dispatch-send--change-group action)))

(oclosure-define (conn-dispatch-send-replace
                  (:parent conn-action))
  (str :type string)
  (change-group))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-send-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (oclosure-lambda (conn-dispatch-send-replace
                      (description "Send and Replace")
                      (change-group cg)
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
                      (window-predicate
                       (lambda (win)
                         (not
                          (buffer-local-value 'buffer-read-only
                                              (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (delete-region beg end)
             (insert-for-yank str)
             (unless executing-kbd-macro
               (conn-dispatch-action-pulse
                (- (point) (length str)) (point))))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-accept-action ((action conn-dispatch-send-replace))
  (conn--action-accept-change-group (conn-dispatch-send-replace--change-group action))
  action)

(cl-defmethod conn-cancel-action ((action conn-dispatch-send-replace))
  (conn--action-cancel-change-group (conn-dispatch-send-replace--change-group action)))

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
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (goto-char (if conn-dispatch-other-end end beg))
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
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (delete-region beg end)
           (conn-register-load register))
          (_ (user-error "Cannot find thing at point")))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-register-load-replace) &optional short)
  (if short "Register Replace"
    (format "Register Replace <%c>" (conn-dispatch-register-load-replace--register action))))

(oclosure-define (conn-dispatch-copy-from
                  (:parent conn-action)
                  (:copier conn-dispatch-copy-from-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-copy-from))
  (thread-first
    (conn-dispatch-copy-from--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-copy-from))
  (set-marker (conn-dispatch-copy-from--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-copy-from))
  (conn-dispatch-copy-from-copy
   action
   (copy-marker (conn-dispatch-copy-from--opoint action) t)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-from)))
  (oclosure-lambda (conn-dispatch-copy-from
                    (description "Copy From")
                    (opoint (copy-marker (point) t)))
      (window pt thing thing-arg transform)
    (let (str)
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (conn-dispatch-action-pulse
              beg end)
             (setq str (filter-buffer-substring beg end)))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer (marker-buffer opoint)
        (conn-dispatch-loop-undo-boundary)
        (cond ((null str)
               (user-error "Cannot find thing at point"))
              ((/= (point) opoint)
               (save-excursion
                 (goto-char opoint)
                 (insert-for-yank str)))
              (t
               (goto-char opoint)
               (insert-for-yank str)))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-copy-from))
  (set-marker (conn-dispatch-copy-from--opoint action) nil))

(oclosure-define (conn-dispatch-copy-from-replace
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-copy-from-replace)))
  (oclosure-lambda (conn-dispatch-copy-from-replace
                    (description "Copy From and Replace"))
      (window pt thing thing-arg transform)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (pcase (conn-bounds-of thing thing-arg)
          ((conn-bounds `(,beg . ,end) transform)
           (conn-dispatch-action-pulse
            beg end)
           (copy-region-as-kill beg end))
          (_ (user-error "Cannot find thing at point")))))
    (conn-dispatch-loop-undo-boundary)
    (delete-region (region-beginning) (region-end))
    (yank)))

(oclosure-define (conn-dispatch-take-replace
                  (:parent conn-action)
                  (:copier conn-dispatch-take-replace-copy (opoint)))
  (opoint :type marker)
  (change-group))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take-replace))
  (thread-first
    (conn-dispatch-take-replace--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take-replace))
  (thread-first
    (conn-dispatch-take-replace--opoint action)
    (copy-marker t)
    (conn--flip-last conn-dispatch-take-replace-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take-replace)))
  (let ((cg (conn--action-buffer-change-group)))
    (delete-region (region-beginning) (region-end))
    (oclosure-lambda (conn-dispatch-take-replace
                      (description "Take From and Replace")
                      (change-group cg)
                      (opoint (copy-marker (point) t))
                      (window-predicate
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
      (with-current-buffer (marker-buffer opoint)
        (save-excursion
          (goto-char opoint)
          (yank))))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take-replace))
  (set-marker (conn-dispatch-take-replace--opoint action) nil)
  (conn--action-cancel-change-group
   (conn-dispatch-take-replace--change-group action)))

(cl-defmethod conn-accept-action ((action conn-dispatch-take-replace))
  (conn--action-accept-change-group
   (conn-dispatch-take-replace--change-group action)))

(oclosure-define (conn-dispatch-take
                  (:parent conn-action)
                  (:copier conn-dispatch-take-copy (opoint)))
  (opoint :type marker))

(cl-defmethod conn-action-stale-p ((action conn-dispatch-take))
  (thread-first
    (conn-dispatch-take--opoint action)
    marker-buffer buffer-live-p not))

(cl-defmethod conn-action-cleaup ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--opoint action) nil))

(cl-defmethod conn-action-copy ((action conn-dispatch-take))
  (thread-first
    (conn-dispatch-take--opoint action)
    (copy-marker t)
    (conn--flip-last conn-dispatch-take-copy action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-take)))
  (oclosure-lambda (conn-dispatch-take
                    (description "Take From")
                    (opoint (copy-marker (point) t))
                    (window-predicate
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
    (with-current-buffer (marker-buffer opoint)
      (yank))))

(cl-defmethod conn-cancel-action ((action conn-dispatch-take))
  (set-marker (conn-dispatch-take--opoint action) nil))

(oclosure-define (conn-dispatch-over
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-over)))
  (oclosure-lambda (conn-dispatch-over
                    (description "Over")
                    (window-predicate (let ((obuf (current-buffer)))
                                        (lambda (win)
                                          (eq (window-buffer win) obuf)))))
      (window pt thing thing-arg transform)
    (when (and (eq (window-buffer window) (current-buffer))
               (/= pt (point)))
      (unless (region-active-p)
        (push-mark nil t))
      (pcase (cons (or (conn-bounds
                        (conn-transform-bounds
                         (conn-bounds-of thing thing-arg)
                         transform))
                       (point))
                   (progn
                     (goto-char pt)
                     (conn-bounds
                      (conn-transform-bounds
                       (conn-bounds-of thing thing-arg)
                       transform))))
        ((and `((,beg1 . ,end1) . (,beg2 . ,end2))
              (or (guard (<= beg1 end1 beg2 end2))
                  (guard (>= end1 beg1 end2 beg2))
                  (guard (and (= beg1 beg2) (= end1 end2)))))
         (if (> beg2 end1)
             (progn
               (conn--push-ephemeral-mark beg1)
               (goto-char (if conn-dispatch-other-end beg2 end2)))
           (conn--push-ephemeral-mark end1)
           (goto-char (if conn-dispatch-other-end end2 beg2))))
        ((and `(,point . (,beg . ,end))
              (guard (integerp point)))
         (cond ((<= point beg end)
                (goto-char end))
               ((<= beg point end)
                (goto-char beg)
                (conn--push-ephemeral-mark end))
               ((<= beg end point)
                (goto-char beg))))))))

(oclosure-define (conn-dispatch-jump
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-jump)))
  (oclosure-lambda (conn-dispatch-jump
                    (description "Jump"))
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
  (letrec ((action nil)
           (setup (lambda ()
                    (conn-without-recursive-stack
                      (conn-dispatch-kapply-prefix
                       (lambda (kapply-action)
                         (setf action kapply-action))))
                    (remove-hook 'post-command-hook setup))))
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
                      (window-predicate
                       (lambda (win)
                         (not (buffer-local-value 'buffer-read-only
                                                  (window-buffer win))))))
        (window pt thing thing-arg transform)
      (with-selected-window window
        (conn-dispatch-loop-undo-boundary)
        (save-mark-and-excursion
          (goto-char pt)
          (pcase (conn-bounds-of thing thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (goto-char (if conn-dispatch-other-end end beg))
             (conn--push-ephemeral-mark (if conn-dispatch-other-end beg end))
             (eval command))
            (_ (user-error "Cannot find thing at point"))))))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-repeat-command) &optional short)
  (if short "Repeat Cmd"
    (format "Repeat <%s>" (car (oref action command)))))

(oclosure-define (conn-dispatch-transpose
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-transpose)))
  (oclosure-lambda (conn-dispatch-transpose
                    (description "Transpose")
                    (always-retarget t)
                    (window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
      (window1 pt1 thing1 window2 pt2 thing2 thing-arg transform)
    (conn-dispatch-loop-undo-boundary (window-buffer window1)
                                      (window-buffer window2))
    (conn--dispatch-transpose-subr
     (window-buffer window1) pt1 thing1
     (window-buffer window2) pt2 thing2
     thing-arg transform)))

(defun conn--dispatch-transpose-subr ( buffer1 pt1 thing1
                                       buffer2 pt2 thing2
                                       thing-arg &optional transform)
  (if (eq buffer1 buffer2)
      (with-current-buffer buffer1
        (save-excursion
          (pcase-let ((`(,beg1 . ,end1)
                       (progn
                         (goto-char pt1)
                         (or (conn-bounds (conn-transform-bounds
                                           (conn-bounds-of thing1 thing-arg)
                                           transform))
                             (user-error "Cannot find thing at point"))))
                      (`(,beg2 . ,end2)
                       (progn
                         (goto-char pt2)
                         (or (conn-bounds (conn-transform-bounds
                                           (conn-bounds-of thing2 thing-arg)
                                           transform))
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
                          (str1)
                          (str2))
      (activate-change-group cg)
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (pcase (conn-bounds-of thing1 thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (setq pt1 beg)
             (setq str1 (filter-buffer-substring beg end))
             (delete-region beg end))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer2
        (save-excursion
          (goto-char pt2)
          (pcase (conn-bounds-of thing2 thing-arg)
            ((conn-bounds `(,beg . ,end) transform)
             (setq str2 (filter-buffer-substring beg end))
             (delete-region beg end)
             (insert str1))
            (_ (user-error "Cannot find thing at point")))))
      (with-current-buffer buffer1
        (save-excursion
          (goto-char pt1)
          (insert str2)))
      (accept-change-group cg))))

(put 'conn-dispatch-upcase :advertised-binding (key-parse "M-u"))
(put 'conn-dispatch-downcase :advertised-binding (key-parse "M-l"))

;;;;; Dispatch Ring

(cl-defstruct (conn-previous-dispatch
               (:constructor
                conn-make-dispatch
                ( action thing thing-arg thing-transform keys
                  &aux
                  (restrict-windows (advice-function-member-p
                                     'conn--dispatch-restrict-windows
                                     conn-target-window-predicate))
                  (other-end (if conn-dispatch-no-other-end
                                 :no-other-end
                               conn-dispatch-other-end))
                  (always-retarget conn--dispatch-always-retarget)
                  (setup
                   (let ((fns (conn-targets-save-state conn-dispatch-target-finder)))
                     (lambda ()
                       (dolist (fn fns)
                         (funcall fn conn-dispatch-target-finder)))))))
               (:copier conn--copy-previous-dispatch))
  (action nil :type conn-action)
  (thing nil :type (or symbol conn-anonymous-thing))
  (thing-arg nil :type (or nil integer))
  (thing-transform nil :type list)
  (keys nil :type list)
  (other-end nil :type symbol)
  (always-retarget nil :type boolean)
  (repeat nil :type boolean)
  (restrict-windows nil :type boolean)
  (setup nil :type function))

(defvar conn-dispatch-ring-max 12)

(defvar conn-dispatch-ring
  (conn-make-ring conn-dispatch-ring-max
                  :cleanup 'conn-dispatch--cleanup))

(cl-defmethod conn-dispatch-command-handler ((_cmd (eql conn-repeat-last-dispatch)))
  (if-let* ((prev (conn-ring-head conn-dispatch-ring)))
      (if (conn-action-stale-p (conn-previous-dispatch-action prev))
          (progn
            (conn-dispatch-ring-remove-stale)
            (user-error "Last dispatch action stale"))
        (conn-ring-delq prev conn-dispatch-ring)
        (conn-read-args-handle)
        (conn-read-args-return
          (conn-call-previous-dispatch
           prev
           :repeat (xor (conn-read-args-consume-prefix-arg)
                        (conn-previous-dispatch-repeat prev)))))
    (user-error "Dispatch ring empty")))

(defun conn-dispatch-copy (dispatch)
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
          (conn-thing-print-pretty (conn-previous-dispatch-thing dispatch))
          (conn-previous-dispatch-thing-arg dispatch)
          (if-let* ((ts (conn-previous-dispatch-thing-transform dispatch)))
              (concat
               "; "
               (mapconcat (lambda (tf)
                            (or (get tf :conn-transform-description) ""))
                          ts " > "))
            "")))

(defun conn-dispatch-push-history (dispatch)
  (conn-dispatch-ring-remove-stale)
  (unless (conn-action--no-history (conn-previous-dispatch-action dispatch))
    (add-to-history 'command-history `(conn-call-previous-dispatch
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

(defun conn-call-previous-dispatch (dispatch &rest override-keys)
  (pcase-let (((cl-struct conn-previous-dispatch
                          thing
                          thing-arg
                          thing-transform
                          action
                          keys
                          other-end
                          always-retarget
                          repeat
                          restrict-windows
                          setup)
               dispatch))
    (apply #'conn-perform-dispatch
           `( ,action ,thing ,thing-arg ,thing-transform
              ,@override-keys
              :always-retarget ,always-retarget
              :repeat ,repeat
              :restrict-windows ,restrict-windows
              :other-end ,other-end
              :setup ,setup
              ,@keys))))

;;;;; Dispatch Commands

(cl-defgeneric conn-perform-dispatch (action
                                      thing
                                      thing-arg
                                      thing-transform
                                      &key
                                      repeat
                                      restrict-windows
                                      other-end
                                      &allow-other-keys))

(cl-defmethod conn-perform-dispatch ((_action (eql nil))
                                     thing thing-arg thing-transform
                                     &rest keys
                                     &key &allow-other-keys)
  (apply #'conn-perform-dispatch
         (conn-make-default-action thing)
         thing thing-arg thing-transform keys))

(cl-defmethod conn-perform-dispatch :around ((action conn-action)
                                             thing
                                             thing-arg
                                             thing-transform
                                             &rest keys
                                             &key
                                             restrict-windows
                                             other-end
                                             always-retarget
                                             setup
                                             &allow-other-keys)
  (let* ((dispatch-quit-flag nil)
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
         (conn--dispatch-action-always-prompt (conn-action--always-prompt action))
         (conn-dispatch-target-finder
          (conn-get-target-finder thing thing-arg))
         (conn-dispatch-repeat-count 0)
         (conn--dispatch-always-retarget
          (or always-retarget
              (conn-action--always-retarget action)))
         (target-other-end (conn-targets-other-end
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
               (conn-targets-message-prefixes
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
    (when-let* ((predicate (conn-action--window-predicate action)))
      (add-function :after-while conn-target-window-predicate predicate))
    (when-let* ((predicate (conn-action--target-predicate action)))
      (add-function :after-while conn-target-predicate predicate))
    (when restrict-windows
      (add-function :after-while conn-target-window-predicate
                    'conn--dispatch-restrict-windows))
    (when setup (funcall setup))
    (conn--unwind-protect-all
      (progn
        (while-let ((new-target
                     (catch 'dispatch-change-target
                       (let ((emulation-mode-map-alists
                              `(((conn-dispatch-select-mode
                                  . ,(make-composed-keymap
                                      (conn-targets-keymaps
                                       conn-dispatch-target-finder))))
                                ,@emulation-mode-map-alists)))
                         (apply #'cl-call-next-method
                                action
                                thing thing-arg thing-transform
                                keys))
                       nil)))
          (pcase-setq `(,thing ,thing-arg ,thing-transform) new-target))
        (conn-dispatch-push-history
         (conn-make-dispatch action thing thing-arg thing-transform keys)))
      (conn-targets-cleanup conn-dispatch-target-finder)
      (conn-cleanup-labels)
      (conn-delete-targets)
      (progn
        (with-current-buffer (marker-buffer opoint)
          (if dispatch-quit-flag
              (goto-char opoint)
            (unless (eql (point) (marker-position opoint))
              (conn--push-mark-ring opoint))))
        (set-marker opoint nil)
        (let ((inhibit-message conn-read-args-inhibit-message))
          (message nil))))))

(cl-defmethod conn-perform-dispatch ((action conn-action)
                                     thing
                                     thing-arg
                                     thing-transform
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let* ((`(,pt ,win ,thing-override)
                  (conn-select-target)))
      (funcall action win pt
               (or thing-override thing)
               thing-arg thing-transform))))

(cl-defmethod conn-perform-dispatch ((action conn-dispatch-transpose)
                                     thing
                                     thing-arg
                                     thing-transform
                                     &key
                                     repeat
                                     &allow-other-keys)
  (conn-perform-dispatch-loop repeat
    (pcase-let ((`(,pt1 ,win1 ,thing-override1)
                 (conn-select-target))
                (`(,pt2 ,win2 ,thing-override2)
                 (conn-select-target)))
      (funcall action
               win1 pt1 (or thing-override1 thing)
               win2 pt2 (or thing-override2 thing)
               thing-arg thing-transform))))

(cl-defmethod conn-perform-dispatch ((action conn-dispatch-kapply)
                                     thing thing-arg thing-transform
                                     &key repeat &allow-other-keys)
  (let ((conn-label-select-always-prompt t))
    (conn-perform-dispatch-loop repeat
      (pcase-let* ((`(,pt ,win ,thing-override)
                    (conn-select-target)))
        (while
            (condition-case err
                (progn
                  (funcall action win pt
                           (or thing-override thing)
                           thing-arg thing-transform)
                  nil)
              (user-error (message (cadr err)) t))))))
  (unless conn-kapply-suppress-message
    (message "Kapply completed successfully after %s iterations"
             conn-dispatch-repeat-count)))

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
      ((action (conn-dispatch-action-argument))
       (`(,thing ,thing-arg) (conn-dispatch-target-argument))
       (transform (conn-dispatch-transform-argument))
       (repeat (conn-dispatch-repeat-argument))
       (other-end (conn-dispatch-other-end-argument nil))
       (restrict-windows (conn-dispatch-restrict-windows-argument)))
    (conn-perform-dispatch
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
        ((action (conn-dispatch-action-argument))
         (`(,thing ,thing-arg) (conn-dispatch-target-argument))
         (transform (conn-dispatch-transform-argument))
         (repeat (conn-dispatch-repeat-argument))
         (other-end (conn-dispatch-other-end-argument nil))
         (restrict-windows (conn-dispatch-restrict-windows-argument)))
      (conn-perform-dispatch
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
  (let ((prev (conn-ring-head conn-dispatch-ring)))
    (conn-call-previous-dispatch
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
                  key-seq (conn-dispatch-copy (conn-ring-head conn-dispatch-ring)))
      (message "Dispatch bound to %s" (format-kbd-macro key-seq)))))

(defun conn-dispatch-on-buttons ()
  "Dispatch on buttons."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-push-button)
   (conn-anonymous-thing
     'button
     :description ( :method (_self) "all-buttons")
     :target-finder ( :method (_self _arg) 'conn-dispatch-all-buttons)
     :other-end ( :method (_self) :no-other-end))
   nil nil))

(defun conn-dispatch-isearch ()
  "Jump to an isearch match with dispatch labels."
  (interactive)
  (let ((targets (with-restriction (window-start) (window-end)
                   (conn--isearch-matches))))
    (unwind-protect ;In case this was a recursive isearch
        (isearch-exit)
      (conn-perform-dispatch
       (conn-make-action 'conn-dispatch-jump)
       (conn-anonymous-thing
         nil
         :target-finder ( :method (_self _arg)
                          (lambda ()
                            (cl-loop for (beg . end) in targets
                                     do (conn-make-target-overlay
                                         beg (- end beg))))))
       nil nil
       :restrict-windows t
       :other-end :no-other-end))))

(defun conn-goto-char-2 ()
  "Jump to point defined by two characters and maybe a label."
  (interactive)
  (conn-perform-dispatch
   (conn-make-action 'conn-dispatch-jump)
   nil nil
   :other-end :no-other-end))

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
            (conn-perform-dispatch
             (oclosure-lambda (conn-action
                               (no-history t)
                               (description "Bounds")
                               (window-predicate
                                (let ((win (selected-window)))
                                  (lambda (window) (eq win window)))))
                 (window pt thing thing-arg transform)
               (with-selected-window window
                 (pcase (conn-bounds-of-remote thing thing-arg pt)
                   ((and (conn-bounds `(,beg . ,end) transform)
                         bound)
                    (unless executing-kbd-macro
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

;;;;; Dispatch Registers

(cl-defstruct (conn-dispatch-register
               (:constructor conn--make-dispatch-register (dispatch-command)))
  (dispatch-command nil :type conn-dispatch-context))

(cl-defmethod register-val-jump-to ((val conn-dispatch-register) arg)
  (conn-call-previous-dispatch
   val
   :repeat (xor arg (conn-previous-dispatch-repeat val))))

(cl-defmethod register-val-describe ((_val conn-dispatch-register) _arg)
  (princ "Dispatch Register"))

(defun conn-last-dispatch-to-register (register)
  "Store last dispatch command in REGISTER."
  (interactive (list (register-read-with-preview "Dispatch to register: ")))
  (set-register register (conn--make-dispatch-register
                          (conn-ring-head conn-dispatch-ring))))

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
  (conn-dispatch-read-string-with-timeout))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing char)))
  (conn-make-action 'conn-dispatch-jump))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line))
                                      _arg)
  #'conn-dispatch-lines)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing line-column))
                                      _arg)
  #'conn-dispatch-columns)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing line-column)))
  (conn-make-action 'conn-dispatch-jump))

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
