;;; conn-mode.el --- Modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn-mode.el
;; Description: A modal keybinding mode and keyboard macro enhancement
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.4"))
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

(require 'compat)
(require 'pulse)
(require 'face-remap)
(require 'rect)
(require 'elec-pair)
(require 'isearch)
(require 'repeat)
(require 'hi-lock)
(require 'kmacro)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


;;;; Declarations

(declare-function conn--sorted-overlays "conn-mode")

(defvar conn-mode nil)
(defvar conn-local-mode)
(defvar view-state)
(defvar conn--mark-cursor-timer nil)
(defvar conn--aux-timer nil)
(defvar conn-modes)


;;;; Variables

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

(defcustom conn-aux-map-update-delay 0.5
  "Update delay for aux-map remappings."
  :type '(number :tag "seconds")
  :set (lambda (sym val)
         (set sym val)
         (when conn-mode
           (when conn--aux-timer
             (cancel-timer conn--aux-timer))
           (setq conn--aux-timer
                 (run-with-idle-timer
                  val t #'conn--aux-map-timer-func))))
  :group 'conn-states)

(defcustom conn-lighter " Conn"
  "Modeline lighter for conn-mode."
  :type '(choice string (const nil))
  :group 'conn-states)

(defcustom conn-state-buffer-colors nil
  "Indicate state using buffer faces."
  :type 'boolean
  :group 'conn-states)

(defcustom conn-default-state 'emacs-state
  "Default conn state for new buffers."
  :type 'symbol
  :group 'conn-states)

(defcustom conn-buffer-default-state-alist nil
  "Alist of the form ((REGEXP . STATE) ...).
Defines default STATE for buffers matching REGEXP."
  :type '(list (cons string symbol))
  :group 'conn-states)

(defface conn-dot-face
  '((t (:background "#ffcddc")))
  "Face for dots."
  :group 'conn-dots)

(defcustom conn-overlay-priority 1001
  "Priority of dot overlays."
  :type 'integer
  :group 'conn-dots)

(defcustom conn-last-dispatch-macro-register ?.
  "Register used for the last dot macro."
  :type 'character
  :group 'conn-dots)

(defface conn-mark-face
  '((default (:background "gray65")))
  "Face for mark."
  :group 'conn-marks)

(defcustom conn-ephemeral-mark-states
  nil
  "States in which ephemeral marks should be used."
  :type '(repeat symbol)
  :group 'conn-marks)

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

(defvar conn--state-maps nil)
(defvar-local conn--aux-maps nil)
(defvar-local conn--local-maps nil)
(defvar-local conn--major-mode-maps nil)
(defvar conn--mode-maps nil)
(defvar-local conn--local-mode-maps nil)
(defvar conn--transition-maps nil)

(defvar conn--prev-mark-even-if-inactive nil)

(defvar conn-isearch-recursive-edit-p nil)

;;;;; Command Histories

(defvar conn-thing-history nil)

(defvar conn--seperator-history nil
  "History var for `conn-set-register-seperator'.")

(defvar-local conn-mode-line-indicator "")


;;;; Utilities

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

(defun conn--derived-mode-property (property &optional buffer)
  "Check major mode in BUFFER and each `derived-mode-parent' for PROPERTY.
If BUFFER is nil check `current-buffer'."
  (let ((mode (buffer-local-value 'major-mode (or buffer (current-buffer))))
        result)
    (while (and mode (not result))
      (setq result (get mode property)
            mode (get mode 'derived-mode-parent)))
    result))

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

(defun conn--minimize (fn list)
  (let ((d (funcall fn (car list)))
        (result (car list)))
    (dolist (e (cdr list))
      (let ((d2 (funcall fn e)))
        (when (< d2 d)
          (setq d d2
                result e))))
    result))

(defun conn--beginning-of-region-or-restriction ()
  (if (use-region-p) (region-beginning) (point-min)))

(defun conn--end-of-region-or-restriction ()
  (if (use-region-p) (region-end) (point-max)))

(eval-and-compile
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

(defun conn--create-marker (pos &optional buffer)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    marker))

;; From meow
(defmacro conn--as-merged-undo (&rest body)
  "Execute BODY as a single undo unit."
  (declare (indent defun))
  (let ((handle (gensym "change-group-handle"))
        (success (gensym "change-group-success")))
    `(let ((,handle (prepare-change-group))
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (prog2
               (activate-change-group ,handle)
               ,(macroexp-progn body)
             (setq ,success t))
         (if (not ,success)
             (cancel-change-group ,handle)
           (accept-change-group ,handle)
           (undo-amalgamate-change-group ,handle))))))


;;;; Mark

(defvar-local conn--unpop-ring nil
  "Ring for marks popped with `pop-to-mark-command'.")

(defvar conn--global-unpop-ring nil
  "Ring for marks popped with `pop-global-mark'.")

(defvar-local conn--ephemeral-mark nil)

(defvar conn--mark-cursor-timer nil
  "`run-with-idle-timer' timer to update `mark' cursor.")

(defvar-local conn--mark-cursor nil
  "`mark' cursor overlay.")
(put 'conn--mark-cursor 'permanent-local t)

(defvar-local conn--handle-mark nil)

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
       (if (boundp ',name)
           (conn--thread needle
             (pcase-lambda  (`(,,ts . ,,ss))
               (let ((,thing ,ts))
                 (fset ,ss ,lambda-form)
                 (cons ,ss ,thing)))
             (mapcar needle ,name)
             (setf ,name needle))
         (defvar ,name nil))

       (defun ,name ,args
         ,docstring
         (or (alist-get ,thing ,name)
             (let ((,sym (make-symbol (conn--stringify ',name "-" ,thing))))
               (fset ,sym ,lambda-form)
               (setf (alist-get ,thing ,name) ,sym)))))))

(conn-define-thing-handler conn-continuous-thing-handler (thing)
  "Return a continuous mark handler for THING.
If one has already been created return it, otherwise create a new one.
Continuous handlers will mark all THINGs when moving over multiple THINGs
at once unless `region-active-p'."
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

(conn-define-thing-handler conn-discrete-thing-handler (thing)
  "Return a discrete mark handler for THING.
If one has already been created return it, otherwise create a new one.
Discrete handlers will only mark the last THING when moving over multiple
THINGs at once unless `region-active-p'."
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

(defun conn-add-mark-commands (commands handler)
  "Register a thing movement command for THING."
  (dolist (cmd (ensure-list commands))
    (put cmd :conn-mark-handler handler)))

(defvar conn--thing-mark-commands
  `(((forward-sexp backward-sexp)
     ,(conn-continuous-thing-handler 'sexp))
    ((beginning-of-buffer end-of-buffer)
     ,(conn-discrete-thing-handler 'buffer))
    ((move-end-of-line move-beginning-of-line)
     ,(conn-discrete-thing-handler 'outer-line))
    ((forward-word backward-word)
     ,(conn-continuous-thing-handler 'word))
    ((forward-line conn-backward-line)
     ,(conn-continuous-thing-handler 'line))
    ((beginning-of-defun end-of-defun)
     ,(conn-continuous-thing-handler 'defun))
    ((forward-paragraph backward-paragraph)
     ,(conn-continuous-thing-handler 'paragraph))
    ((forward-sentence backward-sentence)
     ,(conn-continuous-thing-handler 'sentence))
    ((forward-whitespace conn-backward-whitespace)
     ,(conn-continuous-thing-handler 'whitespace))
    ((conn-end-of-inner-line conn-beginning-of-inner-line)
     ,(conn-discrete-thing-handler 'inner-line))
    ((conn-next-dot conn-previous-dot)
     ,(conn-continuous-thing-handler 'dot))
    ((next-line
      previous-line
      conn-back-to-indentation-or-beginning
      conn-end-of-line-or-next
      conn-backward-whitespace
      forward-whitespace)
     conn-jump-handler))
  "Default conn mark commands and their handlers.")
(put 'outer-line 'beginning-op 'beginning-of-line)
(put 'outer-line 'end-op 'end-of-line)

(defun conn--mark-cursor-p (ov)
  (eq (overlay-get ov 'type) 'conn--mark-cursor))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.

For the meaning of MSG and ACTIVATE see `push-mark'."
  (let ((global-mark-ring nil))
    (push-mark location (not msg) activate))
  (setq conn--ephemeral-mark t))

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
        (overlay-put conn--mark-cursor 'conn-overlay t)
        (overlay-put conn--mark-cursor 'face 'conn-mark-face)
        (overlay-put conn--mark-cursor 'type 'conn--mark-cursor)
        (overlay-put conn--mark-cursor 'priority conn-overlay-priority))
       (t
        (move-overlay conn--mark-cursor (mark t) (1+ (mark t)))
        (overlay-put conn--mark-cursor 'after-string
                     (when (and (= (mark-marker) (point-max))
                                (/= (point) (mark-marker)))
                       (propertize " " 'face 'conn-mark-face))))))))

(defun conn--mark-cursor-timer-func ()
  (walk-windows #'conn--mark-cursor-timer-func-1 nil 'visible))

(defun conn-hide-mark-cursor (mode-or-state &optional predicate)
  "Hide mark cursor in buffers with in MODE-OR-STATE.
If PREDICATE is non-nil it is a function that will be called
to determine if mark cursor should be hidden in buffer."
  (put mode-or-state :conn-hide-mark (or predicate t)))

(defun conn-show-mark-cursor (mode-or-state)
  "Show mark cursor in MODE-OR-STATE."
  (put mode-or-state :conn-hide-mark nil))

(defun conn--mark-pre-command-hook ()
  (if-let ((_ (memq conn-current-state conn-ephemeral-mark-states))
           (handler (conn--command-property :conn-mark-handler)))
      (setq conn--handle-mark (list handler (point)))
    (setq conn--handle-mark nil)))
(put 'conn--mark-pre-command-hook 'permanent-local-hook t)

(defun conn--mark-post-command-hook ()
  (with-demoted-errors "error marking thing: %s"
    (if conn--handle-mark (apply conn--handle-mark))))
(put 'conn--mark-post-command-hook 'permanent-local-hook t)

(defun conn--buffer-bounds ()
  (cons (point-min) (point-max)))
(put 'buffer 'bounds-of-thing-at-point 'conn--buffer-bounds)

(defun conn-unpop-to-mark-command ()
  "`pop-mark' and and add it to the conn unpop ring."
  (interactive)
  (if (null conn--unpop-ring)
      (user-error "No markers to unpop")
    (when (= (point) (car conn--unpop-ring))
      (push-mark (point) t nil)
      (set-marker (pop conn--unpop-ring) nil))
    (goto-char (marker-position (car conn--unpop-ring)))
    (push-mark (point) t nil)
    (set-marker (pop conn--unpop-ring) nil)))

(defun conn--setup-mark ()
  (when conn--mark-cursor-timer
    (cancel-timer conn--mark-cursor-timer)
    (setq conn--mark-cursor-timer nil))
  (when conn-mode
    (pcase-dolist (`(,commands ,handler)
                   conn--thing-mark-commands)
      (conn-add-mark-commands commands handler))
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

(defvar conn-macro-dispatch-p nil
  "Non-nil during macro dispatch.

See `conn--dispatch-on-regions'.")

(cl-defstruct (conn-macro-dispatch-register)
  (macro nil :read-only t)
  (transition nil :read-only t))

(cl-defmethod register-val-jump-to ((val conn-macro-dispatch-register) _arg)
  (if conn-macro-dispatch-p
      (throw 'conn-dispatch-register val)
    (conn--dispatch-on-regions
     (region-bounds)
     :transition (conn-macro-dispatch-register-transition val)
     :macro (conn-macro-dispatch-register-macro val))))

(cl-defmethod register-val-describe ((val conn-macro-dispatch-register) _arg)
  (conn--thread needle
      (list (if-let ((state (conn-macro-dispatch-register-transition val)))
                (symbol-name state)
              "")
            (key-description (conn-macro-dispatch-register-macro val)))
    (apply 'format "%s dispatch macro:\n   %s" needle)
    (princ needle)))

(defun conn--canonicalize-regions (regions)
  "Transform REGIONS into canonical form for `conn--dispatch-on-regions'.

Transform a flat list of elements of the form (MARKER-OR-INT . MARKER-OR-INT)
into an alist with elements (BUFFER ((MARKER . MARKER) ...))."
  (conn--thread needle
      (pcase-lambda (`(,beg . ,end))
        (cons (if (integerp beg) (conn--create-marker beg) beg)
              (if (integerp end) (conn--create-marker end) end)))
    (mapcar needle regions)
    (seq-group-by (pcase-lambda (`(,beg . ,_))
                    (marker-buffer beg))
                  needle)
    (mapcar (pcase-lambda (`(,key . ,val))
              (cons key (sort val (lambda (a b) (> (car a) (car b))))))
            needle)
    (if-let ((curr (seq-find (lambda (c)
                               (eq (car c) (current-buffer)))
                             needle))
             (first (conn--minimize
                     (pcase-lambda (`(,beg . ,end))
                       (min (abs (- beg (point)))
                            (abs (- end (point)))))
                     (cdr curr)))
             (rest (remq first (cdr curr))))
        (cons (cons (car curr) (cons first rest))
              (seq-remove (lambda (c)
                            (eq (car c) (current-buffer)))
                          needle))
      needle)))

(defun conn--dispatch-on-regions (regions &rest rest)
  "Begin a macro dispatch on REGIONS.

REGIONS should be a list of elements of the form
(MARKER-OR-INT . MARKER-OR-INT).  Integers are treated as locations in
the current buffer.

All of the keyword arguments are options.

BEFORE should be a function that will be called with the markers bounding
the region to be acted upon before any other setup is performed.

TRANSITION is called interactively and should put the buffer in the
desired conn state.

AFTER should be a function that will be called with the markers bounding the
region after the keyboard macro has been run.  AFTER is guarenteed to be
called if BEFORE is called.

MACRO is a keyboard macro to use instead of recording a new macro on the
first iteration of dispatch.

\(fn REGIONS &key BEFORE AFTER TRANSITION MACRO)"
  (when conn-macro-dispatch-p
    (user-error "Recursive call to macro dispatch"))
  (let ((regions (conn--canonicalize-regions regions))
        (last-kbd-macro (plist-get rest :macro))
        (conn-macro-dispatch-p t)
        (wind (current-window-configuration))
        (undo-outer-limit nil)
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (register nil)
        (success nil)
        (handles nil))
    (unwind-protect
        (progn
          (unless regions (user-error "No regions for dispatch"))
          (setq register (catch 'conn-dispatch-register
                           (pcase-dolist (`(,buffer . ,rs) regions)
                             (push (prepare-change-group buffer) handles)
                             (apply 'conn--dispatch-in-buffer buffer rs rest))))
          (when (conn-macro-dispatch-register-p register)
            (mapc #'cancel-change-group handles)
            (setf handles nil
                  last-kbd-macro (conn-macro-dispatch-register-macro register)
                  (plist-get rest :transition) (conn-macro-dispatch-register-transition
                                                register))

            (pcase-dolist (`(,buffer . ,rs) regions)
              (push (prepare-change-group buffer) handles)
              (apply 'conn--dispatch-in-buffer buffer rs rest)))
          (setq success t)
          (if register
              (set-register conn-last-dispatch-macro-register register)
            (set-register conn-last-dispatch-macro-register
                          (make-conn-macro-dispatch-register
                           :macro last-kbd-macro
                           :transition (plist-get rest :transition)))))
      (if (not success)
          (mapc #'cancel-change-group handles)
        (dolist (handle handles)
          (accept-change-group handle)
          (undo-amalgamate-change-group handle)))
      (unless (plist-get rest :preserve-marks)
        (pcase-dolist (`(,_ . ,rs) regions)
          (pcase-dolist (`(,beg . ,end) rs)
            (set-marker beg nil)
            (set-marker end nil))))
      (set-window-configuration wind))))

(defun conn--dispatch-in-buffer (buffer regions &rest rest)
  "Begin a macro dispatch on REGIONS in BUFFER.

\(fn BUFFER REGIONS &key BEFORE AFTER TRANSITION)"
  (pop-to-buffer-same-window buffer)
  (conn-with-saved-state
    (save-mark-and-excursion
      (deactivate-mark t)
      (pcase-dolist (`(,beg . ,end) regions)
        (apply 'conn--macro-dispatch-1 beg end rest)))))

(cl-defgeneric conn--macro-dispatch-1 (beg end &key before after transition &allow-other-keys)
  "Dispatch on region from BEG to END.")

(cl-defmethod conn--macro-dispatch-1 (beg end
                                          &context (last-kbd-macro (eql nil))
                                          &key before after transition
                                          &allow-other-keys)
  "Perform first iteration of macro dispatch."
  (let ((mark-ring mark-ring))
    (unwind-protect
        (progn
          (when before (funcall before beg end))
          (goto-char beg)
          (conn--push-ephemeral-mark end)
          (when transition (call-interactively transition))
          (kmacro-start-macro nil)
          (pulse-momentary-highlight-region (region-beginning)
                                            (region-end)
                                            'conn-dot-face)
          (unwind-protect
              (recursive-edit)
            (kmacro-end-macro nil)))
      (when after (funcall after beg end)))
    (unless last-kbd-macro
      (user-error "A keyboard macro was not defined."))))

(cl-defmethod conn--macro-dispatch-1 (beg end &key before after transition &allow-other-keys)
  "Perform remaining iterations of macro dispatch."
  (let ((mark-ring mark-ring))
    (with-demoted-errors "Error in macro dispatch: %s"
      (unwind-protect
          (progn
            (when before (funcall before beg end))
            (goto-char beg)
            (conn--push-ephemeral-mark end)
            (when transition (call-interactively transition))
            (kmacro-call-macro nil nil nil last-kbd-macro))
        (when after (funcall after beg end))))))


;;;; Dots

;;;;; Variables

(defvar-local conn--dot-undoing nil)
(defvar-local conn--dot-undo-ring nil)
(defvar-local conn--dot-undone nil)
(defvar-local conn--dot-this-undo nil)

(defvar conn-dot-undo-ring-max 32
  "Maximum size of the dot undo ring.")

(defvar conn--repat-check-key-prev-val)

;;;;; Dot Registers

(cl-defstruct (conn-dot-register (:constructor %conn-make-dot-register (data)))
  (data nil :read-only t))

(defun conn-make-dot-register ()
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
    (%conn-make-dot-register dots)))

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
  (set-register register (conn-make-dot-register)))

;;;;; Dot Functions

(if (version<= "29" emacs-version)
    (cl-defun conn--sorted-overlays (typep &optional (predicate '<) start end)
      "Get all dots between START and END sorted by starting position."
      (let ((overlays (conn--all-overlays typep start end)))
        (pcase predicate
          ('< overlays)
          ('> (nreverse overlays))
          (_ (sort overlays predicate)))))

  (cl-defun conn--sorted-overlays (typep &optional (predicate '<) start end)
    "Get all dots between START and END sorted by starting position."
    (let ((overlays (conn--all-overlays typep start end)))
      (pcase predicate
        ('< (cl-sort overlays #'< :key #'overlay-start))
        ('> (cl-sort overlays #'> :key #'overlay-start))
        (_  (cl-sort overlays predicate))))))

(defun conn--clear-overlays ()
  "Delete all conn overlays."
  (dolist (ov (flatten-tree (overlay-lists)))
    (when (overlay-get ov 'conn-overlay)
      (delete-overlay ov))))

(defun conn--dot-post-command-hook ()
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
  (unless (or conn--dot-undoing
              conn-macro-dispatch-p)
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
        (overlay-put overlay 'conn-overlay t)
        (overlay-put overlay 'dot t)
        (overlay-put overlay 'priority conn-overlay-priority)
        (overlay-put overlay 'face 'conn-dot-face)
        (unless (or conn--dot-undoing
                    conn-macro-dispatch-p)
          (push `(create ,start . ,end) conn--dot-this-undo))))))

(defun conn--remove-dots (&optional start end)
  (mapc #'conn--delete-dot (conn--all-overlays #'conn-dotp start end)))

(defun conn--all-overlays (predicate &optional start end)
  "Get all overlays between START and END satisfying PREDICATE."
  (seq-filter predicate
              (overlays-in (or start (conn--beginning-of-region-or-restriction))
                           (or end (conn--end-of-region-or-restriction)))))

(defun conn-dotp (overlay)
  "Return t if OVERLAY is a dot."
  (overlay-get overlay 'dot))

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

(defmacro conn-with-saved-state (&rest body)
  "Execute BODY preserving current conn state and previous state values"
  (declare (indent defun))
  (let ((saved-state (gensym "saved-state"))
        (saved-previous-state (gensym "saved-previous-state")))
    `(let ((,saved-state conn-current-state)
           (,saved-previous-state conn-previous-state)
           (conn-previous-state conn-previous-state))
       (unwind-protect
           ,(macroexp-progn body)
         (funcall ,saved-state)
         (setq conn-previous-state ,saved-previous-state)))))

(cl-defun conn--dot-macro-dispatch (buffers &key transition before after)
  "Perform macro dispatch on all dots in BUFFERS."
  (let* ((ov)
         (before (lambda (beg end)
                   (setq ov (conn--dot-before-point end))
                   (overlay-put ov 'face nil)
                   (overlay-put ov 'evaporate nil)
                   (when before (funcall before beg end))))
         (after (lambda (beg end)
                  (when after (funcall after beg end))
                  (conn--move-dot ov (region-beginning) (region-end))
                  (overlay-put ov 'face 'conn-dot-face)
                  (overlay-put ov 'evaporate t)))
         regions)
    (dolist (buf buffers)
      (with-current-buffer buf
        (dolist (ov (conn--all-overlays #'conn-dotp))
          (push (cons (conn--create-marker (overlay-start ov))
                      (conn--create-marker (overlay-end ov)))
                regions))))
    (conn--dispatch-on-regions regions
                               :before before
                               :after after
                               :transition transition)))


;;;; Advice

(defun conn--pop-mark-ad ()
  (when mark-ring
    (set-marker (mark-marker) (car mark-ring))
    (set-marker (car mark-ring) nil)
    (pop mark-ring))
  (deactivate-mark))

(defun conn--pop-to-mark-command-ad (&rest _)
  (unless (null (mark t))
    (add-to-history 'conn--unpop-ring (copy-marker (mark-marker)) mark-ring-max))
  (setq conn--ephemeral-mark nil))

(defun conn--push-mark-ad (fn &rest args)
  (if (and conn--ephemeral-mark
           (not (use-region-p)))
      (let ((mark-ring nil))
        (apply fn args))
    (apply fn args))
  (setq conn--ephemeral-mark nil))

(defun conn--save-ephemeral-mark-ad (fn &rest _)
  (cons conn--ephemeral-mark (funcall fn)))

(defun conn--restore-ephemeral-mark-ad (fn saved-mark-info)
  (setq-local conn--ephemeral-mark (pop saved-mark-info))
  (funcall fn saved-mark-info))

(defun conn--copy-region-ad (beg end &optional region)
  "Pulse region when copying."
  (if region
      (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)
    (pulse-momentary-highlight-region beg end 'region)))

(defun conn--insert-register-ad (_register &optional _arg)
  "`delete-region' or `delete-rectangle' before `insert-register'
when `use-region-p' is non-nil."
  (when (use-region-p)
    (if (rectangle-mark-mode)
        (delete-rectangle (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))))

(defun conn--setup-advice ()
  (if conn-mode
      (progn
        (advice-add 'insert-register :before #'conn--insert-register-ad)
        (advice-add 'copy-region-as-kill :before #'conn--copy-region-ad)
        (advice-add 'push-mark :around #'conn--push-mark-ad)
        (advice-add 'pop-to-mark-command :before #'conn--pop-to-mark-command-ad)
        (advice-add 'pop-mark :override #'conn--pop-mark-ad)
        (advice-add 'save-mark-and-excursion--save
                    :around #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore
                    :around #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'insert-register #'conn--insert-register-ad)
    (advice-remove 'copy-region-as-kill #'conn--copy-region-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'pop-to-mark-command #'conn--pop-to-mark-command-ad)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'save-mark-and-excursion--save
                   #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore
                   #'conn--restore-ephemeral-mark-ad)))


;;;; State Functionality

(defvar conn--aux-bindings nil)

(defun conn-keymap-local-set (key state command)
  (interactive
   (list (key-description (read-key-sequence "Set key sequence locally: " nil t))
         (intern-soft (completing-read "In state: " (mapcar #'symbol-name conn-states) nil t))
         (read-command "To command: ")))
  (keymap-set (or (alist-get state conn--local-maps)
                  (setf (alist-get state conn--local-maps)
                        (make-sparse-keymap)))
              key command))

(defun conn--lookup-binding (binding)
  (let ((emulation-mode-map-alists (seq-difference
                                    emulation-mode-map-alists
                                    '(conn--transition-maps
                                      ;; Remappings probably should be able
                                      ;; to see what is bound in these maps.
                                      ;; conn--local-mode-maps
                                      ;; conn--major-mode-maps
                                      ;; conn--local-maps
                                      conn--aux-maps
                                      conn--state-maps))))
    (keymap-lookup nil binding t)))

(defun conn--aux-map-timer-func ()
  (conn--setup-aux-maps (window-buffer (selected-window))))

(defun conn--setup-aux-maps (&optional buffer)
  "Setup conn aux maps for state in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((aux-map (setf (alist-get conn-current-state conn--aux-maps)
                         (make-sparse-keymap))))
      (dolist (remapping conn--aux-bindings)
        (when-let ((to-keys (where-is-internal remapping))
                   (def (conn--lookup-binding (symbol-value remapping))))
          (dolist (key to-keys)
            (define-key aux-map key def)))))))

(defmacro conn-define-remapping-command (name from-keys)
  "Define a command NAME that remaps to FROM-KEYS.

Placing NAME in a keymap will cause conn to remap it to the
result of FROM-KEYS.  For example conn uses this to map C-c,
C-x, M-s and M-g into various state maps."
  `(progn
     (defcustom ,name
       ,from-keys
       ,(conn--stringify
         "Key sequence for `" name "' to remap.\n"
         "Set this variable to change `" name "''s remapping.\n"
         "The key sequence must satisfy `key-valid-p'.")
       :type 'string
       :group 'conn-key-remappings
       :set (lambda (sym val)
              (set sym val)
              (conn--setup-aux-maps)))

     (defun ,name ()
       ,(conn--stringify
         "Conn remapping command.\n"
         "Conn will remap this command to the value of `" name "'.\n"
         "If this function is called interactively it will `user-error'.\n"
         "If called from Emacs lisp it will `call-interactively'\n "
         "the binding of the key sequence in `" name "'.")
       (interactive)
       (pcase (keymap--menu-item-binding (conn--lookup-binding ,name))
         ((and (pred commandp) cmd)
          (call-interactively cmd))
         (_ (error "Key not bound to a command %s." ,name))))

     (cl-pushnew ',name conn--aux-bindings)))

(conn-define-remapping-command conn-C-x-keys             "C-x")
(conn-define-remapping-command conn-C-c-keys             "C-c")
(conn-define-remapping-command conn-M-s-keys             "M-s")
(conn-define-remapping-command conn-M-g-keys             "M-g")
(conn-define-remapping-command conn-C-x-t-keys           "C-x t")
(conn-define-remapping-command conn-delete-char-keys     "C-d")
(conn-define-remapping-command conn-yank-keys            "C-y")
(conn-define-remapping-command conn-kill-region-keys     "C-w")
(conn-define-remapping-command conn-backward-delete-keys "DEL")
(conn-define-remapping-command conn-delete-region-keys   "C-S-w")

(defun conn-previous-state ()
  "Transition to previous conn state."
  (interactive)
  (funcall conn-previous-state))

(defun conn--setup-major-mode-maps ()
  (setq conn--major-mode-maps nil)
  (pcase-dolist (`(,state . ,maps) conn--mode-maps)
    (when-let ((map (alist-get major-mode maps)))
      (push (cons state map) conn--major-mode-maps))))

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
(put 'conn--activate-input-method 'permanent-local-hook t)

(defun conn--deactivate-input-method ()
  "Disable input method in all states."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (setq conn--input-method nil)))
(put 'conn--deactivate-input-method 'permanent-local-hook t)

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

(defmacro define-conn-state (name doc &rest body)
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
only has an effect when `conn-state-buffer-colors' is non-nil.

BODY contains code to be executed each time the transition function is executed.

\(fn NAME DOC &key CURSOR LIGHTER-FACE SUPPRESS-INPUT-METHOD KEYMAP TRANSITIONS INDICATOR EPHEMERAL-MARKS BUFFER-FACE &rest BODY)"
  (declare (indent defun))
  (let* ((map-name (conn--symbolicate name "-map"))
         (transition-map-name (conn--symbolicate name "-transition-map"))
         (cursor-name (conn--symbolicate name "-cursor-type"))
         (lighter-face-name (conn--symbolicate name "-lighter-face"))
         (indicator-name (conn--symbolicate name "-indicator"))
         (buffer-face-name (conn--symbolicate name "-buffer-face"))
         keyw
         lighter-face
         suppress-input-method
         ephemeral-marks
         (keymap '(make-sparse-keymap))
         cursor
         (transitions '(make-sparse-keymap))
         (indicator "")
         buffer-face)
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:cursor (setq cursor (pop body)))
        (:lighter-face (setq lighter-face (pop body)))
        (:suppress-input-method (setq suppress-input-method (pop body)))
        (:keymap (setq keymap (pop body)))
        (:transitions
         (setq transitions
               `(let ((map (make-sparse-keymap)))
                  (pcase-dolist (`(,key . ,def) ',(pop body))
                    (keymap-set map key def))
                  map)))
        (:indicator (setq indicator (pop body)))
        (:ephemeral-marks (setq ephemeral-marks (pop body)))
        (:buffer-face (setq buffer-face (pop body)))
        (_ (pop body))))
    `(progn
       (defvar-local ,name nil
         ,(conn--stringify "Non-nil when `" name "' is active."))

       (defvar ,map-name ,keymap
         ,(conn--stringify "Keymap active in `" name "'."))

       (defvar ,transition-map-name ,transitions
         ,(conn--stringify
           "Keymap for commands that transition from `" name "' to other states."))

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

       (defface ,buffer-face-name
         ',buffer-face
         ,(conn--stringify "Face for `" name "' buffers.")
         :group 'conn-states)

       ,(when ephemeral-marks
          `(cl-pushnew ',name conn-ephemeral-mark-states))

       (put ',name :conn-suppress-input-method ,suppress-input-method)
       (put ',name :conn-cursor-type ',cursor-name)
       (put ',name :conn-indicator ',indicator-name)

       (cl-pushnew ',name conn-states)
       (push (cons ',name ,map-name) conn--state-maps)
       (push (cons ',name ,transition-map-name) conn--transition-maps)

       (defun ,name ()
         ,doc
         (interactive)
         (when conn-current-state
           (funcall (get conn-current-state :conn-transition-fn) t))
         (funcall (get ',name :conn-transition-fn)))

       (put ',name :conn-transition-fn
            (lambda (&optional exit)
              (unless (xor exit (eq conn-current-state ',name))
                (if exit
                    (progn
                      (setq ,name nil)
                      (setq conn-current-state nil)
                      (setq conn-previous-state ',name)
                      (when conn-state-buffer-colors
                        (buffer-face-set 'default)))
                  (setq conn-current-state ',name)
                  (setq ,name t)
                  (when conn-lighter
                    (setq-local conn-lighter
                                (propertize conn-lighter
                                            'face ',lighter-face-name)))
                  (when conn-state-buffer-colors
                    (buffer-face-set ',buffer-face-name))
                  (conn--activate-input-method)
                  (conn--setup-aux-maps)
                  (setq conn--local-mode-maps
                        (alist-get conn-current-state conn--mode-maps))
                  (conn--update-cursor)
                  (conn--update-mode-line-indicator))
                ,@body
                (run-hooks 'conn-transition-hook)
                (force-mode-line-update)))))))


;;;; State Definitions

(defvar conn-common-map (make-sparse-keymap))

(defvar-local view-state--start-marker nil)

(define-conn-state emacs-state
  "Activate `emacs-state' in the current buffer.
A `conn-mode' state for inserting text.  By default `emacs-state' does not
bind anything except transition commands.

See `emacs-state-transition-map' for keybindings to enter other states
from Emacs state.  See `emacs-state-map' for commands bound by Emacs state."
  :lighter-face ((t (:background "#cae1ff" :box (:line-width 2 :color "#355687"))))
  :indicator " E "
  :cursor box
  :buffer-face ((t :inherit default))
  :ephemeral-marks t
  :transitions (("<escape>" . view-state)
                ("<f7>"     . conn-pop-state)
                ("<f8>"     . conn-state)
                ("<f9>"     . dot-state)))

(define-conn-state view-state
  "Activate `view-state' in the current buffer.
A `conn-mode' state for viewing and navigating buffers.

See `view-state-transition-map' for keybindings to enter other states
from view state.  See `view-state-map' for commands bound by view state."
  :lighter-face ((t (:background "#f5c5ff" :box (:line-width 2 :color "#2d242f"))))
  :suppress-input-method t
  :cursor box
  :indicator " V "
  :buffer-face ((t :inherit default :background "#fff6ff"))
  :ephemeral-marks nil
  :keymap (define-keymap :suppress t)
  :transitions (("f"        . emacs-state)
                ("E"        . emacs-state-eol)
                ("A"        . emacs-state-eol)
                ("="        . dot-state)
                ("<f8>"     . conn-state)
                ("<f9>"     . dot-state)
                ("<escape>" . conn-pop-state)
                ("w"        . view-state-quit)
                ("c"        . conn-state))
  (if view-state
      (progn
        (setq-local view-state--start-marker (point-marker)))
    (set-marker view-state--start-marker nil)
    (setq-local view-state--start-marker nil)))
(put 'view-state :conn-hide-mark t)

(define-conn-state conn-state
  "Activate `conn-state' in the current buffer.
A `conn-mode' state for editing text.

See `conn-state-transition-map' for keybindings to enter other states
from conn state.  See `conn-state-map' for commands bound by conn state."
  :lighter-face ((t (:background "#f3bdbd" :box (:line-width 2 :color "#7a1a1a"))))
  :suppress-input-method t
  :indicator " C "
  :ephemeral-marks t
  :buffer-face ((t :inherit default :background "#f7eee1"))
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (("f"        . conn-emacs-state)
                ("<escape>" . view-state)
                ("t"        . conn-change)
                ("'"        . conn-quoted-insert-overwrite)
                ("E"        . emacs-state-eol)
                ("A"        . emacs-state-bol)
                ("<f7>"     . emacs-state)
                ("<f8>"     . conn-pop-state)
                ("<f9>"     . dot-state)
                ("="        . dot-state)))

(set-default-conn-state '(prog-mode
                          text-mode
                          conf-mode)
                        'conn-state)

(define-conn-state dot-state
  "Activate `dot-state' in the current buffer.
A `conn-mode' state for dispatching keyboard macros on buffer regions.

See `dot-state-transition-map' for keybindings to enter other states
from dot state.  See `dot-state-map' for commands bound by dot state."
  :lighter-face ((t (:background "#d1ead5" :box (:line-width 2 :color "#33553d"))))
  :suppress-input-method t
  :indicator " D "
  :ephemeral-marks t
  :buffer-face ((t :inherit default :background "#f6fff9"))
  :keymap (define-keymap :parent conn-common-map :suppress t)
  :transitions (("<escape>" . view-state)
                ("<f7>"     . emacs-state)
                ("<f8>"     . conn-state)
                ("<f9>"     . conn-pop-state))
  (if dot-state
      (progn
        (setq conn--dot-undo-ring nil)
        (conn--for-each-dot
         (lambda (dot)
           (push `(create ,(overlay-start dot) . ,(overlay-end dot))
                 conn--dot-this-undo)))
        (add-hook 'post-command-hook #'conn--dot-post-command-hook t t))
    (setq conn--dot-undo-ring nil)
    (remove-hook 'post-command-hook #'conn--dot-post-command-hook t)))

(define-conn-state org-tree-edit-state
  "Activate `org-tree-edit-state' in the current buffer.
A `conn-mode' state for structural editing of `org-mode' buffers.

See `org-tree-edit-state-transition-map' for keybindings to enter other states
from org-tree-edit state.  See `org-tree-edit-state-map' for commands bound by
org-tree-edit state."
  :lighter-face ((t :inherit conn-view-state-lighter-face))
  :suppress-input-method t
  :indicator (:propertize " T " face conn-org-tree-edit-state-lighter-face)
  :buffer-face ((t :inherit view-state-buffer-face))
  :keymap (define-keymap :suppress t)
  :transitions (("f"        . emacs-state)
                ("E"        . emacs-state-eol)
                ("A"        . emacs-state-eol)
                ("="        . dot-state)
                ("<f8>"     . conn-state)
                ("<f9>"     . dot-state)
                ("<escape>" . conn-pop-state)))
(put 'org-tree-edit-state :conn-hide-mark t)


;;;; Extensions

(defvar conn--extensions nil)

(defun conn--setup-extensions ()
  "Run when `conn-mode' is turned on or off to turn shims on or off."
  (run-hook-with-args 'conn--extensions conn-mode))

(defmacro conn-define-extension (name &rest body)
  "Define a conn conn extension.

\(fn NAME [DOCSTRING] &rest body)"
  (declare (indent 1))
  (let (doc)
    (when (stringp (car body))
      (setq doc (pop body)))
    `(progn
       (defvar ,name nil)

       (defun ,name (enable)
         ,(or doc "")
         (when (xor enable ,name)
           (let ((fn (get ',name :conn-feature-function)))
             (when (and conn-mode enable)
               (funcall fn enable))
             (if enable
                 (add-hook 'conn--extensions fn)
               (remove-hook 'conn--extensions fn)))))

       (when-let ((body-fn (get ',name :conn-feature-function)))
         (funcall body-fn nil)
         (remove-hook 'conn--extensions body-fn)
         (put ',name :conn-feature-function nil))

       (let ((body-sym (make-symbol ,(conn--stringify name "-body-fn"))))
         (fset body-sym (lambda (enable)
                          (message "Called")
                          (setq ,name enable)
                          ,@body))
         (put ',name :conn-feature-function body-sym))

       ',name)))


;;;; Commands

;;;;; Dot Commands

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
        (`(create ,beg . ,_end)
         (let ((dot (or (conn--dot-after-point beg)
                        (error "Dot undo ring corrupted"))))
           (conn--delete-dot dot)))
        (`(delete ,beg . ,end)
         (conn--create-dots (cons beg end)))
        (`(move (,to-beg . ,_to-end) . (,from-beg . ,from-end))
         (let ((dot (or (conn--dot-after-point to-beg)
                        (error "Dot undo ring corrupted"))))
           (conn--move-dot dot from-beg from-end)))))
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
         (conn--create-dots (cons beg end)))
        (`(delete ,beg . ,_end)
         (let ((dot (or (conn--dot-after-point beg)
                        (error "Dot undo ring corrupted"))))
           (conn--delete-dot dot)))
        (`(move (,to-beg . ,to-end) . (,from-beg . ,_from-end))
         (let ((dot (or (conn--dot-after-point from-beg)
                        (error "Dot undo ring corrupted"))))
           (conn--move-dot dot to-beg to-end)))))
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
      (while (and (> arg 0) dot)
        (conn--delete-dot dot)
        (setq dot (or (conn--dot-before-point (point))
                      (when (conn--previous-dot-1)
                        (conn--next-dot-1)))
              arg (1- arg)))
      (when dot
        (conn--push-ephemeral-mark (overlay-start dot))))))

(defun conn-remove-dot-forward (arg)
  "Remove nearest dot within the range `point' to `point-max'."
  (interactive "p")
  (let ((dot (or (conn--dot-after-point (point))
                 (when (conn--next-dot-1)
                   (conn--previous-dot-1)))))
    (while (and (> arg 0) dot)
      (conn--delete-dot dot)
      (setq dot (or (conn--dot-after-point (point))
                    (when (conn--next-dot-1)
                      (conn--previous-dot-1)))
            arg (1- arg)))
    (when dot
      (conn--push-ephemeral-mark (overlay-start dot))))
  (when (called-interactively-p 'interactive)
    (message "Region removed forward")))

(defun conn-dot-region (bounds)
  "Dot current region."
  (interactive (list (region-bounds)))
  (apply #'conn--create-dots bounds)
  (deactivate-mark))

(defun conn-dot-word-at-point ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'word)
    (`(,beg . ,end)
     (conn-add-dots-matching-regexp
      (concat "\\b" (regexp-quote (buffer-substring beg end)) "\\b")))))

(defun conn-dot-sexp-at-point ()
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

(defun conn-dot-thing (thing)
  "Transpose things at point and mark."
  (interactive (list (intern
                      (completing-read
                       (format "Thing: ")
                       (conn--things 'conn--defined-thing-p) nil nil nil
                       'conn-thing-history))))
  (conn--create-dots (bounds-of-thing-at-point thing)))

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
  (interactive)
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

(defun conn-dot-all-things-in-region (thing)
  "Dot all THINGs in region.

THING is something with a forward-op as defined by thingatpt."
  (interactive
   (let ((things (conn--things 'conn--movement-thing-p)))
     (list (intern (completing-read "Thing: " things nil t nil
                                    'conn-thing-history)))))
  (save-excursion
    (with-restriction
        (region-beginning) (region-end)
      (goto-char (point-min))
      (forward-thing thing)
      (conn--create-dots (bounds-of-thing-at-point thing))
      (while (and (/= (point) (point-max))
                  (/= (point) (progn
                                (forward-thing thing)
                                (point))))
        (conn--create-dots (bounds-of-thing-at-point thing))))))

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

(defun conn--isearch-dispatch (&rest keys)
  (let ((matches
         (if (or (not (boundp 'multi-isearch-buffer-list))
                 (not multi-isearch-buffer-list))
             (conn--isearch-matches-in-buffer (current-buffer))
           (mapcan 'conn--isearch-matches-in-buffer
                   multi-isearch-buffer-list))))
    (isearch-exit)
    (apply #'conn--dispatch-on-regions matches keys)))

(defun conn-isearch-in-dot-p (beg end)
  (when-let ((ov (conn--dot-after-point beg)))
    (>= (overlay-end ov) end)))

(defun conn-isearch-not-in-dot-p (beg end)
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
    (dot-state)
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
  (let (dots)
    (save-excursion
      (dolist (dot (conn--all-overlays #'conn-dotp))
        (goto-char (overlay-start dot))
        (unless refine (push (point) dots))
        (while (isearch-search-string isearch-string (overlay-end dot) t)
          (when (funcall isearch-filter-predicate
                         (match-beginning 0) (match-end 0))
            (push (match-beginning 0) dots)
            (push (match-end 0) dots)))
        (unless refine (push (overlay-end dot) dots))))
    (conn--remove-dots)
    (cl-loop for (beg end) on dots by #'cddr
             when (/= beg end)
             do (conn--create-dots (cons beg end)))))

(defun conn-isearch-refine-dots ()
  "Clear dots and add new dots at isearch matches within previous dots."
  (interactive)
  (conn-isearch-split-dots t))

;;;;; Editing Commands

(defun conn-last-macro-dispatch-to-register (register)
  "Set REGISTER to last dot macro."
  (interactive (list (register-read-with-preview "register: ")))
  (set-register register (get-register conn-last-dispatch-macro-register)))

(defun conn-macro-at-point-and-mark ()
  "Dispatch dot macro at point and mark.
With prefix arg of 0 dispatch the contents of
`conn-last-dispatch-macro-register'.  With any other prefix arg prompt
for a dot register to use for dispatch."
  (interactive)
  (when-let ((mark (mark t))
             (pt (point-marker)))
    (conn--dispatch-on-regions
     (list (cons pt pt)
           (cons mark mark))
     :before (lambda (beg end)
               (goto-char beg)
               (conn--push-ephemeral-mark end)
               (conn-state)))
    (set-marker pt nil)))

(defun conn-scroll-down ()
  "`scroll-down-command' leaving point at the same relative window position."
  (interactive)
  (if (pos-visible-in-window-p (point-min))
      (progn (beep) (message "Beginning of buffer"))
    (scroll-down)))

(defun conn-scroll-up ()
  "`scroll-up-command' leaving point at the same relative window position."
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (scroll-up)))

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

(defun conn-narrow-to-beginning-of-buffer ()
  "Narrow to the region between `point-min' and `point'."
  (interactive)
  (narrow-to-region (point-min) (point)))

(defvar-keymap conn-isearch-symbol-repeat-map
  :repeat t
  "m" 'isearch-repeat-forward
  "n" 'isearch-repeat-backward)

(defun conn-isearch-forward-symbol-at-point ()
  "Isearch forward for symbol at point."
  (interactive)
  (funcall-interactively #'isearch-forward-symbol-at-point 1))
(put 'conn-isearch-forward-symbol-at-point 'repeat-map 'conn-isearch-symbol-repeat-map)

(defun conn-isearch-backward-symbol-at-point ()
  "Isearch backward for symbol at point."
  (interactive)
  (funcall-interactively #'isearch-forward-symbol-at-point -1)
  (isearch-repeat-backward))
(put 'conn-isearch-backward-symbol-at-point 'repeat-map 'conn-isearch-symbol-repeat-map)

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

(defvar conn--goto-char-last-char nil)

(defvar-keymap conn-goto-char-backward-repeat-map
  "j" 'conn-goto-char-backward)

(defun conn-goto-char-backward (char arg)
  "Behaves like `backward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for CHAR and search backward for nearest
occurrence of CHAR.  Repeated calls will then repeatedly jump to occurrences
of CHAR up to `window-end'.

Not for use in lisp code."
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4)) (read-char "Char: ")))
                     (prefix-numeric-value current-prefix-arg)))
  (if (and (null char)
           (not (and (eq last-command 'conn-goto-char-backward)
                     conn--goto-char-last-char)))
      (progn
        (setq conn--goto-char-last-char nil)
        (put this-command 'repeat-map nil)
        (backward-char arg))
    (when char
      (setq conn--goto-char-last-char (if (stringp char) char (string char)))
      (put this-command 'repeat-map 'conn-goto-char-backward-repeat-map))
    (when-let ((pos (or (save-excursion
                          (backward-char)
                          (search-backward conn--goto-char-last-char (window-start) t))
                        (progn
                          (put this-command 'repeat-map nil)
                          (user-error "%s character not found."
                                      conn--goto-char-last-char)))))
      (goto-char pos))))

(defvar-keymap conn-goto-char-forward-repeat-map
  "l" 'conn-goto-char-forward)

(defun conn-goto-char-forward (char arg)
  "Behaves like `forward-char' except when `current-prefix-arg' is 1 or \\[universal-argument].
If `current-prefix-arg' is 1 prompt for CHAR and search forward for nearest
occurrence of CHAR.  Repeated calls will then repeatedly jump to occurrences
of CHAR up to `window-end'.

Not for use in lisp code."
  (interactive (list (pcase current-prefix-arg
                       ((or '1 '(4)) (read-char "Char: ")))
                     (prefix-numeric-value current-prefix-arg)))
  (if (and (null char)
           (not (and (eq last-command 'conn-goto-char-forward)
                     conn--goto-char-last-char)))
      (progn
        (setq conn--goto-char-last-char nil)
        (put this-command 'repeat-map nil)
        (forward-char arg))
    (when char
      (setq conn--goto-char-last-char (if (stringp char) char (string char)))
      (put this-command 'repeat-map conn-goto-char-forward-repeat-map))
    (when-let ((pos (or (save-excursion
                          (forward-char)
                          (search-forward conn--goto-char-last-char (window-end) t))
                        (progn
                          (put this-command 'repeat-map nil)
                          (user-error "%s character not found."
                                      conn--goto-char-last-char)))))
      (goto-char pos))))

(defun conn-pop-state ()
  "Return to previous state."
  (interactive)
  (when conn-previous-state
    (funcall conn-previous-state)))

(defun conn-toggle-minibuffer-focus ()
  "Toggle input focus between minibuffer and `other-window'."
  (interactive)
  (cond ((not (active-minibuffer-window))
         (user-error "Minibuffer is not active"))
        ((eq (selected-window) (active-minibuffer-window))
         (other-window 1)
         (setq this-command 'other-window)
         (message "Switched to %s" (current-buffer)))
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
            (funcall transform-func))
        (apply-on-rectangle
         (lambda (start-col end-col)
           (with-restriction
               (+ (point) start-col) (+ (point) end-col)
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
  (interactive (list (region-beginning)
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
  (interactive (list (intern
                      (completing-read
                       (format "Thing: ")
                       (conn--things 'conn--defined-thing-p) nil nil nil
                       'conn-thing-history))))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (goto-char (cdr bounds))
    (conn--push-ephemeral-mark (car bounds))
    (activate-mark t)))

(defun conn-narrow-to-thing (thing)
  "Narrow to THING at point."
  (interactive (list (intern
                      (completing-read
                       (format "Thing: ")
                       (conn--things 'conn--defined-thing-p) nil nil nil
                       'conn-thing-history))))
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (narrow-to-region (car bounds) (cdr bounds))))

(defun conn--read-pair ()
  (pcase (string-split (minibuffer-with-setup-hook
                           (lambda () (electric-pair-mode -1))
                         (read-string "Pair: " nil 'conn-pair-history))
                       "	")
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
  "Insert STRING at BEG and END.

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
  "Call `delete-pair' with ARG and then call `insert-pair' with STRING."
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
  (cond (arg (if (region-active-p)
                 (rectangle-mark-mode 'toggle)
               (activate-mark)
               (rectangle-mark-mode)))
        (mark-active (deactivate-mark))
        (t (activate-mark))))

(defun conn-set-mark-command (&optional arg)
  (interactive "P")
  (cond (arg
         (rectangle-mark-mode 'toggle))
        ((eq last-command 'conn-set-mark-command)
         (if (region-active-p)
             (progn
               (deactivate-mark)
               (message "Mark deactivated"))
           (activate-mark)
           (message "Mark activated")))
        (t
         (push-mark-command nil))))

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
  (conn-yank-keys)
  (if arg
      (kill-region start end)
    (delete-region start end)))

(defun conn-isearch-exit-and-mark ()
  "`isearch-exit' and set region to match."
  (interactive)
  (isearch-exit)
  (conn--push-ephemeral-mark isearch-other-end))

(defun conn-end-of-inner-line (&optional N)
  (interactive "p")
  (unless N (setq N 1))
  (let ((pt (point)))
    (end-of-line (if (< N 0) (1+ N) N))
    (while (and (not (eobp))
                (and (eolp) (bolp)))
      (end-of-line (if (< N 0) 0 2)))
    (when-let ((cs (and (conn--point-is-in-comment-p)
                        (save-excursion
                          (comment-search-backward
                           (line-beginning-position) t)))))
      (goto-char cs)
      (skip-chars-backward " \t" (line-beginning-position))
      (when (bolp)
        (skip-chars-forward " \t" (line-end-position))))
    (when (and (called-interactively-p 'any)
               (= pt (point)))
      (forward-line)
      (conn-end-of-inner-line))))
(put 'inner-line 'end-op 'conn-end-of-inner-line)
(put 'inner-line 'forward-op 'conn-end-of-inner-line)

(defun conn-beginning-of-inner-line (&optional N)
  "Go to first non-whitespace character in line."
  (interactive "p")
  (unless N (setq N 1))
  (let ((pt (point)))
    (back-to-indentation)
    (unless (and (called-interactively-p 'any)
                 (= pt (point)))
      (cl-decf N (cl-signum N)))
    (when (> N 0)
      (setq pt (point))
      (forward-line (- N))
      (back-to-indentation)
      (while (= (pos-eol) (pos-bol))
        (forward-line (- (cl-signum N)))
        (back-to-indentation))
      (back-to-indentation))))
(put 'inner-line 'beginning-op 'conn-beginning-of-inner-line)

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
  (condition-case err
      (jump-to-register reg arg)
    (user-error
     (unless (string-search "access aborted" (error-message-string err))
       (insert-register reg (not arg))))))

(defun conn-clear-register (register)
  "Reset REGISTER value."
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
  (pulse-momentary-highlight-region start end 'region))

(defun conn-kill-region (&optional arg)
  "Kill region between START and END.

If START and END are equal delete char backward.

If ARG is an ordinary prefix argument (\\[universal-argument]) delete
the region instead of killing it.

If ARG is a numeric prefix argument kill region to a register."
  (interactive (list current-prefix-arg))
  (cond ((= (point) (mark t))
         (conn-backward-delete-keys))
        ((consp arg)
         (conn-delete-region-keys))
        ((numberp arg)
         (thread-first
           (concat "Kill "
                   (if rectangle-mark-mode "Rectangle " " ")
                   "to register:")
           (register-read-with-preview)
           (copy-to-register nil nil t t)))
        (t (conn-kill-region-keys))))

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

(defun conn--uniquify-tab-name (name)
  (let ((tabs (seq-filter (lambda (tab-name)
                            (string-match (concat (regexp-quote name)
                                                  "\\( <[0-9]+>\\)?")
                                          tab-name))
                          (mapcar (lambda (tab-name)
                                    (alist-get 'name tab-name))
                                  (funcall tab-bar-tabs-function (selected-frame))))))
    (if tabs (concat name " <" (number-to-string (length tabs)) ">") name)))

(defun conn-tab-bar-new-named-tab (&optional name)
  (interactive
   (list (when current-prefix-arg
           (conn--uniquify-tab-name (read-from-minibuffer "Tab name: ")))))
  (tab-bar-new-tab-to)
  (when name (tab-rename name)))

(defun conn-tab-bar-duplicate-and-name-tab (&optional name)
  (interactive
   (list (when current-prefix-arg
           (conn--uniquify-tab-name (read-from-minibuffer "Tab name: ")))))
  (let ((tab-bar-new-tab-choice 'clone)
        (tab-bar-new-tab-group t))
    (tab-bar-new-tab-to)
    (when name (tab-rename name))))

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

(defun conn-duplicate-region (&optional arg)
  "Duplicates the current region ARG times."
  (interactive "p")
  (if (region-active-p)
      (duplicate-dwim)
    (let ((end (set-marker (make-marker) (region-end))))
      (unwind-protect
          (dotimes (_ arg)
            (conn--duplicate-region-1 (region-beginning)
                                      (region-end)))
        (goto-char end)
        (set-marker end nil)))))

(defun conn-duplicate-and-comment-region (beg end &optional arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive
   (list (region-beginning)
         (region-end)
         (prefix-numeric-value current-prefix-arg)))
  (let* ((origin (point))
         (oend end)
         (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg)))
    (comment-region beg oend)))

(cl-macrolet
    ((transpose-backward (&body fns)
       (macroexp-progn
        (seq-map
         (lambda (command)
           `(defun ,(conn--symbolicate "conn-" command "-backward") (arg)
              (interactive "*p")
              (funcall-interactively #',command (- arg))))
         fns))))
  (transpose-backward transpose-words
                      transpose-sexps
                      transpose-lines
                      transpose-paragraphs
                      transpose-chars))

;;;;; Transition Functions

(defun conn-state-isearch-matches ()
  (interactive)
  (conn--isearch-dispatch :transition 'conn-state))

(defun conn-emacs-state-isearch-matches ()
  (interactive)
  (conn--isearch-dispatch :transition 'emacs-state))

(defun conn-emacs-state-after-isearch-matches ()
  (interactive)
  (conn--isearch-dispatch :transition (lambda ()
                                        (interactive)
                                        (conn-exchange-mark-command)
                                        (emacs-state))))

(defun conn-change-isearch-matches ()
  (interactive)
  (conn--isearch-dispatch :transition 'conn-change))

(defun view-state-quit ()
  "Pop state and goto point where view-state was entered."
  (interactive)
  (when view-state--start-marker
    (goto-char view-state--start-marker))
  (conn-pop-state))

(defun conn-dot-quit ()
  "Pop state and clear all dots."
  (interactive)
  (conn--remove-dots)
  (conn-pop-state))

(defun conn-change-region ()
  (interactive)
  (conn--dispatch-on-regions (region-bounds) :transition 'conn-change))

(defun conn-emacs-state-after-region ()
  (interactive)
  (conn--dispatch-on-regions
   (region-bounds) :transition (lambda ()
                                 (interactive)
                                 (conn-exchange-mark-command)
                                 (emacs-state))))

(defun conn-emacs-state-region ()
  (interactive)
  (conn--dispatch-on-regions (region-bounds) :transition 'emacs-state))

(defun conn-state-region ()
  (interactive)
  (conn--dispatch-on-regions (region-bounds) :transition 'conn-state))

(defun conn-dots-command (buffers)
  "Begin recording dot macro for BUFFERS, initially in conn-state.

Interactively buffers defaults to current buffer.
With prefix argument \\[universal-argument] ask for a regexp and operate
on all buffers matching regexp.
With any other prefix argument select buffers with `completing-read-multiple'."
  (interactive
   (list (pcase current-prefix-arg
           ('(4) (conn-read-matching-dot-buffers))
           ('nil (list (current-buffer)))
           (_    (conn-read-dot-buffers)))))
  (conn--dot-macro-dispatch buffers :transition 'conn-state))

(defun conn-dots-change (buffers)
  "Begin recording dot macro for BUFFERS, beginning with `conn-change'.

Interactively buffers defaults to current buffer.
With prefix argument \\[universal-argument] ask for a regexp and operate
on all buffers matching regexp.
With any other prefix argument select buffers with `completing-read-multiple'."
  (interactive
   (list (pcase current-prefix-arg
           ('(4) (conn-read-matching-dot-buffers))
           ('nil (list (current-buffer)))
           (_    (conn-read-dot-buffers)))))
  (conn--dot-macro-dispatch buffers :transition 'conn-change))

(defun conn-dots-insert (buffers)
  "Begin recording dot macro for BUFFERS, beginning with `emacs-state'.

Interactively buffers defaults to current buffer.
With prefix argument \\[universal-argument] ask for a regexp and operate
on all buffers matching regexp.
With any other prefix argument select buffers with `completing-read-multiple'."
  (interactive
   (list (pcase current-prefix-arg
           ('(4) (conn-read-matching-dot-buffers))
           ('nil (list (current-buffer)))
           (_    (conn-read-dot-buffers)))))
  (conn--dot-macro-dispatch buffers :transition 'emacs-state))

(defun conn-dots-insert-after (buffers)
  "Begin recording dot macro for BUFFERS, beginning at the end of each dot
 and in `emacs-state'.

Interactively buffers defaults to current buffer.
With prefix argument \\[universal-argument] ask for a regexp and operate
on all buffers matching regexp.
With any other prefix argument select buffers with `completing-read-multiple'."
  (interactive
   (list (pcase current-prefix-arg
           ('(4) (conn-read-matching-dot-buffers))
           ('nil (list (current-buffer)))
           (_    (conn-read-dot-buffers)))))
  (conn--dot-macro-dispatch buffers :transition 'emacs-state))

(defun conn-quoted-insert-overwrite ()
  "Overwrite char after point using `quoted-insert'."
  (interactive)
  (save-excursion
    (let ((overwrite-mode 'overwrite-mode-binary))
      (message "%s" (propertize "Overwrite Char:"
                                'face 'minibuffer-prompt))
      (call-interactively #'quoted-insert))))

(defun conn-emacs-state (&optional arg)
  "Transition to Emacs-state.

If ARG is non-negative open a new line below point and enter insert state.

If ARG is negative open a new line above point and enter insert state."
  (interactive "P")
  (cond ((null arg))
        ((>= (prefix-numeric-value arg) 0)
         (move-end-of-line 1)
         (newline-and-indent))
        ((< (prefix-numeric-value arg) 0)
         (move-beginning-of-line 1)
         (open-line 1)
         (indent-according-to-mode)
         (save-excursion
           (forward-line 1)
           (indent-according-to-mode))))
  (emacs-state))

(defun conn-change (start end &optional kill)
  "Change region between START and END.

If KILL is non-nil add region to the `kill-ring'.
When in `rectangle-mark-mode' defer to `string-rectangle'."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (cond ((and rectangle-mark-mode kill)
         (copy-rectangle-as-kill start end)
         (call-interactively #'string-rectangle))
        (rectangle-mark-mode
         (call-interactively #'string-rectangle))
        (kill
         (conn-kill-region-keys)
         (emacs-state))
        (t
         (conn-delete-region-keys)
         (emacs-state))))

(defun emacs-state-eol (&optional N)
  "Move point to end of line and enter `emacs-state'."
  (interactive "P")
  (end-of-line N)
  (emacs-state))

(defun emacs-state-bol (&optional N)
  "Move point to beginning of line and enter `emacs-state'."
  (interactive "P")
  (beginning-of-line N)
  (back-to-indentation)
  (emacs-state))


;;;; Keymaps

(dolist (state conn-states)
  (define-keymap
    :keymap (conn-get-mode-map state 'conn-macro-dispatch-p)
    "<remap> <kmacro-end-macro>"                'exit-recursive-edit
    "<remap> <kmacro-end-or-call-macro>"        'exit-recursive-edit
    "<remap> <kmacro-end-and-call-macro>"       'exit-recursive-edit
    "<remap> <kmacro-end-or-call-macro-repeat>" 'exit-recursive-edit))

(dolist (state '(conn-state emacs-state dot-state))
  (keymap-set (conn-get-mode-map state 'occur-mode) "C-c e" 'occur-edit-mode))

(dolist (state '(conn-state emacs-state dot-state))
  (keymap-set (conn-get-mode-map state 'occur-edit-mode) "C-c e" 'occur-cease-edit))

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "`" 'other-window)

(defvar-keymap conn-region-map
  :prefix 'conn-region-map
  "b"   'conn-command-at-point-and-mark
  "m"   'conn-macro-at-point-and-mark
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule
  "c"   'conn-region-case-map
  "j"   'conn-join-lines
  "I"   'indent-rigidly
  "i"   'indent-region
  "w"   'delete-region
  "e"   'eval-region
  "d"   'conn-duplicate-region
  "D"   'conn-duplicate-and-comment-region
  "u"   'conn-insert-pair
  "DEL" 'conn-delete-pair
  "p"   'conn-change-pair
  "s"   'conn-isearch-region-forward
  "r"   'conn-isearch-region-backward
  "o"   'conn-occur-region
  "g"   'conn-rgrep-region
  ";"   'comment-or-uncomment-region
  "TAB" 'fill-region
  "n"   'narrow-to-region
  "< a" 'sort-pages
  "< c" 'sort-columns
  "< l" 'sort-lines
  "< n" 'sort-numeric-fields
  "< p" 'sort-paragraphs
  "< r" 'sort-regexp-fields
  "*"   'calc-grab-region
  "$"   'ispell-region
  "v"   'vc-region-history)

(defvar-keymap conn-c-x-4-map
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward
  "`" 'other-window-prefix
  "-" 'conn-window-resize-map)

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

(defvar-keymap conn-isearch-dot-map
  :repeat t
  "C-d" 'conn-isearch-add-dots
  "C-r" 'conn-isearch-refine-dots
  "C-w" 'conn-isearch-remove-dots
  "C-x" 'conn-isearch-split-dots
  "C-c" 'conn-dots-command
  "C-t" 'conn-dots-change
  "C-f" 'conn-dots-insert
  "C-e" 'conn-dots-insert-after)

(defvar-keymap conn-dot-this-map
  :prefix 'conn-dot-this-map
  :doc "Dot this map."
  "p" 'conn-dot-text-property
  "t" 'conn-dot-thing
  "o" 'conn-dot-word-at-point
  "m" 'conn-dot-sexp-at-point
  "w" 'conn-remove-dot-backward
  "d" 'conn-remove-dot-forward)

(defvar-keymap conn-dot-region-repeat-map
  :repeat t
  "j" 'conn-dot-region-backward
  "k" 'conn-dot-region-forward
  "u" 'conn-dot-skip-backward
  "i" 'conn-dot-skip-forward)

(defvar-keymap conn-dot-region-map
  :parent conn-region-map
  "k" 'conn-dot-region-forward
  "j" 'conn-dot-region-backward
  "e" 'conn-add-dots-matching-region
  "a" 'conn-dot-all-things-in-region)

(define-keymap
  :keymap dot-state-transition-map
  "Q"      'conn-dot-quit
  "T"      'conn-dots-change
  "F"      'conn-dots-insert
  "E"      'conn-dots-insert-after
  "C"      'conn-dots-command
  "S-<f8>" 'conn-dots-command
  "<f8>"   'conn-state)

(define-keymap
  :keymap isearch-mode-map
  "M-D c"       'conn-state-isearch-matches
  "M-D f"       'conn-emacs-state-isearch-matches
  "M-D t"       'conn-change-isearch-matches
  "M-D e"       'conn-emacs-state-after-isearch-matches
  "M-D o"       'conn-isearch-edit-each-match
  "M-D d"       conn-isearch-dot-map
  "M-<return>"  'conn-isearch-exit-and-mark)

(define-keymap
  :keymap (conn-get-mode-map 'conn-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

(defvar-keymap conn-search-map
  "n"     'conn-isearch-backward-symbol-at-point
  "m"     'conn-isearch-forward-symbol-at-point
  "TAB"   'conn-isearch-backward-symbol
  "M-TAB" 'conn-isearch-forward-symbol
  "0"     'xref-find-references
  "C-."   'conn-xref-definition-prompt
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
  "C" 'conn-state-region
  "F" 'conn-emacs-state-region
  "T" 'conn-change-region
  "E" 'conn-emacs-state-after-region
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "y" 'yank-rectangle)

(defvar-keymap conn-dispatch-map
  :prefix 'conn-dispatch-map
  "C" 'conn-state-region
  "F" 'conn-emacs-state-region
  "T" 'conn-change-region
  "E" 'conn-emacs-state-after-region
  "c" 'conn-state-region
  "f" 'conn-emacs-state-region
  "t" 'conn-change-region
  "e" 'conn-emacs-state-after-region)

(defvar-keymap conn-tab-bar-history-mode-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

(defvar-keymap conn-region-case-map
  :prefix 'conn-region-case-map
  "-" 'conn-kebab-case-region
  "c" 'capitalize-region
  "d" 'downcase-region
  "u" 'upcase-region
  "o" 'conn-capital-case-region
  "a" 'conn-camel-case-region
  "S" 'conn-capital-snake-case-region
  "s" 'conn-snake-case-region)

(defvar-keymap conn-misc-edit-map
  :prefix 'conn-misc-edit-map
  "y"   'yank-rectangle
  "DEL" 'kill-whole-line
  ";"   'comment-line
  "TAB" 'indent-rigidly
  "q"   'indent-for-tab-command
  "RET" 'whitespace-cleanup
  "F"   'fill-paragraph
  "N"   'conn-transpose-paragraphs-backward
  "M"   'transpose-paragraphs
  "r"   'query-replace-regexp
  "v"   'conn-mark-thing
  "d"   'duplicate-dwim
  "j"   'join-line
  "i"   'conn-transpose-lines-backward
  ","   'conn-transpose-chars-backward
  "k"   'transpose-lines
  "."   'transpose-chars
  "m"   'transpose-sexps
  "n"   'conn-transpose-sexps-backward
  "T"   'conn-narrow-to-thing
  "o"   'transpose-words
  "SPC" 'transpose-regions
  "u"   'conn-transpose-words-backward)

(defvar-keymap conn-ctl-x-r-map
  "\\" 'conn-set-register-seperator
  "."  'conn-last-macro-dispatch-to-register
  ","  'conn-dot-state-to-register
  "!"  'kmacro-to-register
  "W"  'conn-clear-register)

(define-keymap
  :keymap dot-state-map
  "$"                'conn-add-dots-matching-literal
  "d"                'conn-remove-dots-outside-region
  "("                'conn-add-dots-matching-regexp
  "<backspace>"      'conn-kill-to-dots
  "<return>"         'conn-dot-lines
  "<tab>"            'conn-remove-all-dots
  "TAB"              'conn-remove-all-dots
  "C-n"              'conn-next-dot
  "C-p"              'conn-previous-dot
  "M-<down-mouse-1>" 'conn-dot-at-click
  "M-?"              'conn-dot-redo
  "M-/"              'conn-dot-undo
  "["                'conn-remove-dots-before
  "\\"               'conn-split-dots-on-regexp
  "]"                'conn-remove-dots-after
  "_"                'conn-dot-trim-regexp
  "`"                'conn-dot-movement-mode
  "e"                'conn-dot-region
  "q"                'conn-dot-point
  "%"                'conn-query-remove-dots
  "t"                'conn-dot-this-map
  "w"                'conn-remove-dot
  "|"                'conn-split-region-on-regexp
  "{"                'conn-first-dot
  "}"                'conn-last-dot
  "r"                conn-dot-region-map)

(define-keymap
  :keymap conn-state-map
  "M-D"  'conn-dispatch-map
  "<f4>" 'save-buffer
  "\\"   'indent-region
  "r"    'conn-region-map
  "z"    'conn-exchange-mark-command
  "."    'other-window-prefix
  "$"    'ispell-word
  "|"    'shell-command-on-region
  "*"    'calc-dispatch
  "C-y"  'conn-yank-replace
  "M-y"  'conn-completing-yank-replace
  "Q"    'kill-buffer-and-window
  "Y"    'yank-from-kill-ring
  "["    'conn-kill-prepend-region
  "]"    'conn-kill-append-region
  "c"    'conn-C-c-keys
  "d"    'conn-delete-char-keys
  "q"    'conn-misc-edit-map
  "w"    'conn-kill-region
  "y"    'conn-yank-keys)

(define-keymap
  :keymap conn-common-map
  "W"   'widen
  "j"   'conn-goto-char-backward
  "l"   'conn-goto-char-forward
  "!"   'kmacro-start-macro-or-insert-counter
  "("   'conn-backward-whitespace
  ")"   'forward-whitespace
  "+"   'conn-set-register-seperator
  "<"   'beginning-of-defun
  ">"   'end-of-defun
  "@"   'kmacro-end-or-call-macro
  "I"   'backward-paragraph
  "J"   'conn-beginning-of-inner-line
  "K"   'forward-paragraph
  "L"   'conn-end-of-inner-line
  "M"   'forward-line
  "N"   'conn-backward-line
  "O"   'forward-sentence
  "SPC" 'conn-set-mark-command
  "U"   'backward-sentence
  ";"   'execute-extended-command
  "a"   'conn-copy-region
  "g"   'conn-M-g-keys
  "i"   'previous-line
  "u"   'backward-word
  "k"   'next-line
  "o"   'forward-word
  "m"   'forward-sexp
  "n"   'backward-sexp
  "b"   'switch-to-buffer
  "B"   'conn-C-x-t-keys
  "`"   'other-window
  "p"   'conn-register-load
  "s"   'conn-M-s-keys
  "v"   'conn-toggle-mark-command
  "x"   'conn-C-x-keys
  "c"   'conn-C-c-keys
  "/"   'undo-only
  "?"   'undo-redo)

(define-keymap
  :keymap view-state-map
  "SPC"     'conn-scroll-up
  "DEL"     'conn-scroll-down
  "a"       'execute-extended-command
  "q"       'quit-window
  "Q"       'kill-buffer-and-window
  "s"       'conn-M-s-keys
  "g"       'conn-M-g-keys
  "x"       'conn-C-x-keys
  "j"       'backward-page
  "l"       'forward-page
  "b"       'switch-to-buffer
  "B"       'conn-C-x-t-keys
  "i"       'conn-scroll-down
  "k"       'conn-scroll-up
  "."       'point-to-register
  "m"       'mark-page
  "p"       'conn-register-load
  "z"       'conn-exchange-mark-command
  "<up>"    'conn-scroll-down
  "<down>"  'conn-scroll-up
  "<right>" 'forward-page
  "<left>"  'backward-page)

(define-keymap
  :keymap org-tree-edit-state-map
  "SPC" 'conn-scroll-up
  "DEL" 'conn-scroll-down
  "u"   'outline-up-heading
  "a"   'execute-extended-command
  "z"   'conn-exchange-mark-command
  "Q"   'kill-buffer-and-window
  "s"   'conn-M-s-keys
  "g"   'conn-M-g-keys
  "x"   'conn-C-x-keys
  "c"   'conn-C-c-keys
  "w"   'org-refile
  "b"   'switch-to-buffer
  "B"   'conn-C-x-t-keys
  "p"   'conn-register-load
  "."   'point-to-register
  "j"   'org-previous-visible-heading
  "l"   'org-next-visible-heading
  "i"   'org-backward-heading-same-level
  "k"   'org-forward-heading-same-level
  "I"   'org-metaup
  "K"   'org-metadown
  "L"   'org-metaright
  "J"   'org-metaleft
  "U"   'org-previous-block
  "O"   'org-next-block
  ">"   'org-demote-subtree
  "<"   'org-promote-subtree
  "m"   'org-mark-subtree
  ";"   'org-toggle-comment
  "t"   'org-todo
  "n"   'conn-org-tree-edit-insert-heading
  "N"   'org-toggle-narrow-to-subtree
  "_"   'org-columns
  "^"   'org-sort
  "/"   'undo-only
  "?"   'undo-redo)

(defvar-keymap conn-mode-map
  "<remap> <scroll-up-command>" 'conn-scroll-up
  "<remap> <scroll-down-command>" 'conn-scroll-down
  "M-RET"   'conn-open-line-and-indent
  "C-x n t" 'conn-narrow-to-thing
  "C-x r"   conn-ctl-x-r-map
  "C-x 4"   conn-c-x-4-map
  "C-x t s" 'tab-switch
  "C-x n >" 'conn-narrow-to-end-of-buffer
  "C-x n <" 'conn-narrow-to-beginning-of-buffer
  "<pause>" 'conn-toggle-minibuffer-focus
  "C-S-w"   'delete-region
  "M-O"     'pop-to-mark-command
  "M-U"     'conn-unpop-to-mark-command)

(defun conn--setup-keymaps ()
  (if conn-mode
      (progn
        (setq conn--local-maps nil)
        (cl-pushnew 'conn--state-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--aux-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--major-mode-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--local-mode-maps emulation-mode-map-alists)
        (cl-pushnew 'conn--transition-maps emulation-mode-map-alists)
        (set-keymap-parent search-map conn-search-map)
        (set-keymap-parent goto-map conn-goto-map)
        (set-keymap-parent indent-rigidly-map conn-indent-rigidly-map)
        (define-keymap
          :keymap undo-repeat-map
          "u" `(menu-item "Undo" undo
                          :filter ,(lambda (_) (when emacs-state 'undo)))))
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
                          emulation-mode-map-alists #'eq))
    (define-keymap :keymap undo-repeat-map "u" 'undo))
  (when (timerp conn--aux-timer)
    (cancel-timer conn--aux-timer))
  (setq conn--aux-timer
        (when conn-mode
          (run-with-idle-timer
           conn-aux-map-update-delay t #'conn--aux-map-timer-func))))


;;;; Mode Definition

(defun conn--update-mode-line-indicator ()
  "Update conn mode-line indicator."
  (setq conn-mode-line-indicator
        (or (get conn-current-state :conn-indicator) "")))

(define-minor-mode conn-mode-line-indicator-mode
  "Display conn state indicator at the beginning of the mode line."
  :group 'conn-mode
  :global t)

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :keymap conn-mode-map
  :lighter (:eval conn-lighter)
  (if conn-local-mode
      (progn
        ;; Since eldoc clobbers mode-line-format structure we need to
        ;; flatten the mode-line-format tree before searching it.
        (unless (seq-contains-p (flatten-tree mode-line-format)
                                'conn-mode-line-indicator-mode
                                #'eq)
          (push '(conn-mode-line-indicator-mode (:eval conn-mode-line-indicator))
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
        (funcall (conn--default-state-for-buffer)))
    (without-restriction
      (conn--remove-dots))
    (when conn-current-state
      (funcall (get conn-current-state :conn-transition-fn) t))
    (setq conn-current-state nil)
    (conn--delete-mark-cursor)
    (setq-local mode-line-format
                (assq-delete-all
                 'conn-mode-line-indicator-mode
                 mode-line-format))
    (pcase-dolist (`(,_ . ,hooks) conn-input-method-overriding-modes)
      (dolist (hook hooks)
        (remove-hook hook 'conn--activate-input-method t)))
    (remove-hook 'pre-command-hook #'conn--mark-pre-command-hook t)
    (remove-hook 'post-command-hook #'conn--mark-post-command-hook t)
    (remove-hook 'change-major-mode-hook #'conn--clear-overlays t)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (remove-hook 'clone-indirect-buffer-hook 'conn--delete-mark-cursor t)))

(defun conn--initialize-buffer ()
  "Initialize conn STATE in BUFFER."
  (conn-local-mode 1))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn--initialize-buffer
  :group 'conn-mode
  :predicate '((not special-mode)
               (not minibuffer-mode)
               (not dired-mode)
               (not compilation-mode)
               (not slime-xref-mode)
               (not calc-mode)
               (not calc-trail-mode)
               eshell-mode
               grep-mode
               occur-mode
               t)
  (progn
    (conn--setup-keymaps)
    (conn--setup-mark)
    (conn--setup-advice)
    (conn--setup-extensions)
    (if conn-mode
        (progn
          (setq conn--prev-mark-even-if-inactive mark-even-if-inactive
                mark-even-if-inactive t)
          (add-hook 'window-configuration-change-hook #'conn--update-cursor))
      (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
      (remove-hook 'window-configuration-change-hook #'conn--update-cursor))))

(provide 'conn-mode)

;;; Load Extensions

(with-eval-after-load 'corfu
  (defun conn--exit-completion ()
    (completion-in-region-mode -1))
  (add-hook 'conn-transition-hook 'conn--exit-completion))

(with-eval-after-load 'repeat
  (require 'conn-repeat)
  (conn-repeatable-commands t)
  (conn-repeat-cursor t))

(with-eval-after-load 'isearch+
  (require 'conn-isearch+))

(with-eval-after-load 'avy
  (require 'conn-avy))

(with-eval-after-load 'expand-region
  (require 'conn-expand-region))

(with-eval-after-load 'embark
  (require 'conn-embark)
  (declare-function conn-complete-keys-prefix-help-command "conn-embark")
  (conn-complete-keys-prefix-help-command t))

(with-eval-after-load 'consult
  (require 'conn-consult))

(with-eval-after-load 'calc
  (require 'conn-calc)
  (conn-calc-shim t))

(with-eval-after-load 'org
  (defvar org-mode-map)
  (conn-add-mark-commands '(org-forward-paragraph
                            org-backward-paragraph)
                          (conn-continuous-thing-handler 'org-paragraph))
  (put 'org-paragraph 'forward-op 'org-forward-paragraph)

  (keymap-set org-mode-map "<remap> <view-state>" 'org-tree-edit-state)

  (dolist (state '(conn-state dot-state))
    (define-keymap
      :keymap (conn-get-mode-map state 'org-mode)
      "<remap> <forward-paragraph>" 'org-forward-paragraph
      "<remap> <backward-paragraph>" 'org-backward-paragraph
      "<remap> <forward-sentence>" 'org-forward-sentence
      "<remap> <backward-sentence>" 'org-backward-sentence)))

(with-eval-after-load 'polymode
  (defvar polymode-move-these-vars-from-old-buffer)
  (dolist (v '(conn--mark-cursor
               conn-current-state
               conn-state
               emacs-state
               dot-state))
    (add-to-list 'polymode-move-these-vars-from-old-buffer v)))

(with-eval-after-load 'corfu
  (defun conn-exit-completion ()
    (completion-in-region-mode -1))

  (add-hook 'conn-transition-hook 'conn-exit-completion))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-goto-char-backward
                     'conn-goto-char-forward
                     'paredit-forward
                     'paredit-forward-up
                     'paredit-backward
                     'paredit-backward-up))

(with-eval-after-load 'paredit
  (dolist (state '(conn-state dot-state))
    (define-keymap
      :keymap (conn-get-mode-map state 'paredit-mode)
      "<remap> <forward-sexp>" 'paredit-forward
      "<remap> <backward-sexp>" 'paredit-backward
      "O" 'paredit-forward-up
      "U" 'paredit-backward-up))

  (conn-add-mark-commands '(paredit-forward
                            paredit-backward
                            paredit-forward-up
                            paredit-backward-up)
                          (conn-continuous-thing-handler 'sexp)))

(with-eval-after-load 'zones
  (defvar zz-add-zone-anyway-p)
  ;; Make this command add narrowings to izone var
  (defun conn-narrow-to-thing (thing)
    "Narrow to THING at point."
    (interactive (list (intern
                        (completing-read
                         (format "Thing: ")
                         (conn--things 'conn--defined-thing-p) nil nil nil
                         'conn-thing-history))))
    (when-let ((bounds (bounds-of-thing-at-point thing)))
      (let ((zz-add-zone-anyway-p t))
        (narrow-to-region (car bounds) (cdr bounds))))))
