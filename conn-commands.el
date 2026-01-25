;;; conn-commands.el --- Commands -*- lexical-binding: t -*-
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

;; Editing commands

;;; Code

(require 'compat)
(require 'conn-jump-ring)
(require 'conn-vars)
(require 'conn-utils)
(require 'conn-things)
(require 'conn-states)
(require 'conn-dispatch)
(require 'conn-expand)
(eval-when-compile
  (require 'cl-lib))

(autoload 'multi-isearch-read-files "misearch")
(autoload 'multi-isearch-read-matching-files "misearch")
(autoload 'multi-isearch-read-buffers "misearch")
(autoload 'multi-isearch-read-matching-buffers "misearch")
(autoload 'kmacro-ring-head "kmacro")

(declare-function outline-insert-heading "outline")
(declare-function project-files "project")
(declare-function project-root "project")
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")
(declare-function fileloop-continue "fileloop")

;;;; Commands

(defun conn-bind-last-kmacro-to-key ()
  "Like `kmacro-bind-to-key' but binds in `conn-get-overriding-map'.

This binding will be inactive during keyboard macro definition and
execution."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (if defining-kbd-macro
          (message "Cannot save macro while defining it."))
    (unless last-kbd-macro
      (error "No keyboard macro defined"))
    (let* ((key-seq (read-key-sequence "Bind last macro to key: "))
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
                    key-seq
                    `(menu-item
                      "Keyboard Macro"
                      ,(let ((kmacro (kmacro-ring-head)))
                         (lambda (arg) (funcall kmacro arg)))
                      :filter ,(lambda (cmd)
                                 (unless (or executing-kbd-macro
                                             defining-kbd-macro)
                                   cmd))))
        (message "Keyboard macro bound to %s" (format-kbd-macro key-seq))))))

(defun conn-repeat (&optional arg)
  "Repeat the last conn operator."
  (interactive "P")
  (unless conn-command-history
    (user-error "No repeatable last command"))
  (let ((cmd (if arg
                 (let* ((print-level nil)
                        (cmds (cl-loop for cmd in conn-command-history
                                       collect (cons (prin1-to-string cmd) cmd))))
                   (alist-get (completing-read
                               "Command: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata (display-sort-function . ,#'identity))
                                   (complete-with-action action cmds string pred)))
                               nil t)
                              cmds nil nil #'equal))
               (car conn-command-history)))
        (conn-repeating-command t))
    (setq this-command (car cmd))
    (apply #'funcall-interactively cmd)))

;;;;; Movement

(defun conn-forward-up-list (arg)
  (interactive "p")
  (backward-up-list (- arg)))

(defun conn-backward-down-list (arg)
  (interactive "p")
  (down-list (- arg)))

(defun conn-forward-visual-line (arg)
  "Move forward ARG visual lines."
  (declare (conn-thing-command visual-line #'conn-continuous-thing-handler))
  (interactive "p")
  (let ((line-move-visual t))
    (vertical-motion 0)
    (line-move arg t)))

(defun conn-backward-visual-line (arg)
  "Move backward ARG visual lines."
  (declare (conn-thing-command visual-line #'conn-continuous-thing-handler))
  (interactive "p")
  (conn-forward-visual-line (- arg)))

(defun conn-goto-line (line)
  "Goto absolute line, 1 origin.

Respects the current restriction."
  (declare (conn-thing-command line #'conn-continuous-thing-handler))
  (interactive "p")
  (if (> 0 line)
      (progn
        (goto-char (point-max))
        (cl-incf line))
    (goto-char (point-min))
    (cl-decf line))
  (forward-line line))

(defun conn-backward-up-inner-list (arg)
  (declare (conn-thing-command inner-list #'conn-inner-list-handler))
  (interactive "p")
  (unless (= 0 arg)
    (conn-protected-let* ((pt (point) (goto-char pt))
                          (dir (cl-signum arg)))
      (backward-up-list dir)
      (save-excursion
        (down-list dir)
        (unless (= pt (point))
          (cl-decf arg dir)))
      (backward-up-list arg)
      (down-list dir))))

(defun conn-forward-up-inner-list (arg)
  (declare (conn-thing-command inner-list #'conn-inner-list-handler))
  (interactive "p")
  (conn-backward-up-inner-list (- arg)))

(defun conn-forward-defun (N)
  "Move forward by defuns.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (declare (conn-thing-command defun #'conn-continuous-thing-handler))
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(defun conn-backward-symbol (arg)
  "`forward-symbol' in reverse."
  (declare (conn-thing-command symbol #'conn-continuous-thing-handler))
  (interactive "p")
  (forward-symbol (- arg)))

(defun conn--scroll-jump-predicate (_start)
  (not (memq last-command '(conn-scroll-down conn-scroll-up))))

(defun conn-scroll-down (&optional arg)
  "`scroll-down-command' leaving point at the same relative window position.

Pulses line that was the first visible line before scrolling."
  (declare (conn-thing-command visible #'conn-discrete-thing-handler)
           (conn-jump #'conn--scroll-jump-predicate))
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (progn (beep) (message "Beginning of buffer"))
    (let ((start (window-start)))
      (scroll-down arg)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-one-line start)))))
(put 'conn-scroll-down 'scroll-command t)

(defun conn-scroll-up (&optional arg)
  "`scroll-up-command' leaving point at the same relative window position.

Pulses line that was the last visible line before scrolling."
  (declare (conn-thing-command visible #'conn-discrete-thing-handler)
           (conn-jump #'conn--scroll-jump-predicate))
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (let ((end (window-end)))
      (scroll-up arg)
      (unless executing-kbd-macro
        (pulse-momentary-highlight-one-line (1- end))))))
(put 'conn-scroll-up 'scroll-command t)

(defun conn-backward-line (N)
  "`forward-line' by N but backward."
  (declare (conn-thing-command line #'conn-continuous-thing-handler))
  (interactive "p")
  (forward-line (- N)))

(defun conn-backward-whitespace (N)
  "`forward-whitespace' by N but backward."
  (declare (conn-thing-command whitespace #'conn-discrete-thing-handler))
  (interactive "p")
  (forward-whitespace (- N)))

(defun conn--end-of-inner-line-1 ()
  (let ((end (goto-char (line-end-position))))
    (when-let* ((cs (and (conn--point-in-comment-p)
                         (save-excursion
                           (comment-search-backward
                            (line-beginning-position) t)))))
      (goto-char cs))
    (skip-chars-backward " \t" (line-beginning-position))
    (when (bolp) (goto-char end))))

(defun conn-forward-inner-line (N)
  "Move forward by inner lines.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-handler))
  (interactive "p")
  (cond ((> N 0)
         (let ((pt (point)))
           (conn--end-of-inner-line-1)
           (when (> (point) pt) (cl-decf N))
           (cl-loop until (or (<= N 0)
                              (= (point) (point-max)))
                    do (forward-line 1)
                    unless (eolp)
                    do (cl-decf N))
           (conn--end-of-inner-line-1)))
        ((< N 0)
         (cl-callf abs N)
         (let ((pt (point)))
           (back-to-indentation)
           (when (> pt (point)) (cl-decf N))
           (cl-loop until (or (<= N 0)
                              (= (point) (point-max)))
                    do (forward-line -1)
                    unless (eolp)
                    do (cl-decf N))
           (back-to-indentation)))))

(defun conn-backward-inner-line (N)
  "Inverse of `conn-forward-inner-line'."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-handler))
  (interactive "p")
  (conn-forward-inner-line (- N)))

(defun conn-forward-inner-line-dwim (N)
  (declare (conn-thing-command inner-line #'conn-continuous-thing-handler))
  (interactive "p")
  (cond ((> N 0)
         (let ((pt (point)))
           (conn--end-of-inner-line-1)
           (unless (= pt (point))
             (cl-decf N))
           (conn-forward-inner-line N)))
        ((< N 0)
         (conn-backward-inner-line-dwim (abs N)))))

(defun conn-backward-inner-line-dwim (N)
  (declare (conn-thing-command inner-line #'conn-continuous-thing-handler))
  (interactive "p")
  (cond ((> N 0)
         (let ((pt (point)))
           (back-to-indentation)
           (unless (= pt (point))
             (cl-decf N))
           (conn-backward-inner-line N)))
        ((< N 0)
         (conn-forward-inner-line-dwim (abs N)))))

(defun conn-forward-outer-line (&optional N)
  (declare (conn-thing-command outer-line #'conn-continuous-thing-handler))
  (interactive "p")
  (if (< N 0)
      (conn-backward-outer-line (abs N))
    (move-end-of-line (+ N (if (eolp) 1 0)))))

(defun conn-backward-outer-line (&optional N)
  (declare (conn-thing-command outer-line #'conn-continuous-thing-handler))
  (interactive "p")
  (if (< N 0)
      (conn-forward-outer-line (abs N))
    (move-beginning-of-line (- 2 (+ N (if (bolp) 1 0))))))

(defun conn-beginning-of-inner-line (&optional N)
  "Move point to the first non-whitespace character in line.

Immediately repeating this command goes to the point at beginning
of line proper."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-handler))
  (interactive "P")
  (if (null N)
      (let ((point (point))
            (mark (mark t)))
        (back-to-indentation)
        (when (and (= point (point))
                   (or (= mark (save-excursion
                                 (conn--end-of-inner-line-1)
                                 (point)))
                       (region-active-p)))
          (goto-char (line-beginning-position))))
    (forward-line (- N))))

(defun conn-end-of-inner-list ()
  "Move point to the end of the enclosing list."
  (declare (conn-thing-command inner-list #'conn-inner-list-handler))
  (interactive)
  (up-list 1 t t)
  (down-list -1 t))

(defun conn-beginning-of-inner-list ()
  "Move point to the beginning of the enclosing list."
  (declare (conn-thing-command inner-list #'conn-inner-list-handler))
  (interactive)
  (backward-up-list nil t t)
  (down-list 1 t))

;;;;; Command Registers

(cl-defstruct (conn-command-register)
  (command nil :read-only t))

(cl-defmethod register-val-jump-to ((val conn-command-register)
                                    _arg)
  (apply #'funcall-interactively (conn-command-register-command val)))

(cl-defmethod register-val-describe ((val conn-command-register)
                                     _arg)
  (princ (format "Command:  %s"
                 (car (conn-command-register-command val)))))

(defun conn-command-to-register (register)
  "Store a previous command in REGISTER.

The command to be stored is read from `command-history'."
  (interactive
   (list (register-read-with-preview "Command to register: ")))
  (set-register
   register
   (make-conn-command-register
    :command (let* ((print-level nil)
                    (cmds (cl-loop for cmd in conn-command-history
                                   collect (cons (prin1-to-string cmd) cmd))))
               (alist-get (completing-read
                           "Command: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata (display-sort-function . ,#'identity))
                               (complete-with-action action cmds string pred)))
                           nil t)
                          cmds nil nil #'equal)))))

;;;;; Tab Registers

(cl-defstruct (conn-tab-register
               (:constructor nil)
               ( :constructor conn--make-tab-register
                 (&aux
                  (cookie
                   (with-memoization
                       (alist-get 'conn-tab-cookie
                                  (thread-first
                                    (funcall tab-bar-tabs-function)
                                    tab-bar--current-tab-find
                                    cdr))
                     (gensym "conn-tab-cookie")))
                  (frame (selected-frame)))))
  (cookie nil :read-only t)
  (frame nil :read-only t))

(defun conn--get-tab-index-by-cookie (cookie)
  (declare (important-return-value t))
  (seq-position (funcall tab-bar-tabs-function)
                cookie
                (lambda (tab c)
                  (eq c (alist-get 'conn-tab-cookie tab)))))

(cl-defmethod register-val-jump-to ((val conn-tab-register)
                                    _arg)
  (when-let* ((frame (conn-tab-register-frame val))
              (index (and (frame-live-p frame)
                          (with-selected-frame (conn-tab-register-frame val)
                            (conn--get-tab-index-by-cookie
                             (conn-tab-register-cookie val))))))
    (select-frame-set-input-focus frame)
    (tab-bar-select-tab (1+ index))))

(cl-defmethod register-val-describe ((val conn-tab-register)
                                     _arg)
  (princ (format "Tab:  %s"
                 (if (eq (selected-frame) (conn-tab-register-frame val))
                     (when-let* ((index (conn--get-tab-index-by-cookie
                                         (conn-tab-register-cookie val)))
                                 (tab (nth index (funcall tab-bar-tabs-function))))
                       (if (eq (car tab) 'current-tab)
                           (propertize "*CURRENT TAB*" 'face 'error)
                         (alist-get 'name tab)))
                   "on another frame"))))

(defun conn-tab-to-register (register)
  "Store the current tab in REGISTER."
  (interactive (list (register-read-with-preview "Tab to register: ")))
  (set-register register (conn--make-tab-register)))

;;;;; Mark Commands

(defun conn-rectangle-mark ()
  "Toggle `rectangle-mark-mode'."
  (interactive)
  (if (region-active-p)
      (rectangle-mark-mode 'toggle)
    (activate-mark)
    (rectangle-mark-mode)
    (conn-push-state 'conn-mark-state)))

(defun conn-set-mark-command ()
  "Push a mark at point and activate it.

If the mark is already active then deactivate it instead."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (push-mark nil nil t)
    (conn-push-state 'conn-mark-state)))

(defun conn-previous-mark-command ()
  "Push, and mark the region from the previous, `conn-mark-state'."
  (interactive)
  (pcase (conn-ring-extract-head conn-mark-state-ring)
    (`(,pt ,mk ,rmm)
     (goto-char pt)
     (push-mark mk t t)
     (set-marker pt nil)
     (set-marker mk nil)
     (when rmm (rectangle-mark-mode 1))
     (conn-push-state 'conn-mark-state))
    (_ (user-error "No previous mark state"))))

(defun conn-mark-ring-previous (arg)
  (interactive "p")
  (unless (conn-ring-head conn-mark-state-ring)
    (user-error "No previous mark state"))
  (cond ((< arg 0)
         (conn-mark-ring-next (abs arg)))
        ((> arg 0)
         (when conn-mark-state
           (conn-push-mark-state-ring
            (list (point-marker)
                  (copy-marker (mark-marker))
                  (bound-and-true-p rectangle-mark-mode))))
         (unless conn-mark-state (cl-decf arg))
         (dotimes (_ arg)
           (conn-ring-rotate-forward conn-mark-state-ring))
         (conn-previous-mark-command))))

(defun conn-mark-ring-next (arg)
  (interactive "p")
  (unless (conn-ring-head conn-mark-state-ring)
    (user-error "No previous mark state"))
  (cond ((< arg 0)
         (conn-mark-ring-previous (abs arg)))
        ((> arg 0)
         (when conn-mark-state
           (conn-push-mark-state-ring
            (list (point-marker)
                  (copy-marker (mark-marker))
                  (bound-and-true-p rectangle-mark-mode))))
         (dotimes (_ arg)
           (conn-ring-rotate-backward conn-mark-state-ring))
         (conn-previous-mark-command))))

(defun conn-exchange-mark-command (&optional arg)
  "`exchange-mark-and-point' and push `conn-mark-state' if mark is activated."
  (interactive "P")
  (static-if (version<= emacs-version "31")
      (exchange-point-and-mark (xor arg (not (region-active-p))))
    (let ((exchange-point-and-mark-highlight-region t))
      (exchange-point-and-mark (xor arg (not (region-active-p))))))
  (when (and (region-active-p)
             (not conn-mark-state))
    (conn-push-state 'conn-mark-state)))

(defun conn-exchange-and-mark-command ()
  (interactive)
  (static-if (version<= emacs-version "31")
      (exchange-point-and-mark (region-active-p))
    (let ((exchange-point-and-mark-highlight-region t))
      (exchange-point-and-mark (region-active-p))))
  (when (and (region-active-p)
             (not conn-mark-state))
    (conn-push-state 'conn-mark-state)))

(defun conn-push-mark-command ()
  "Set mark at point and push old mark on mark ring."
  (interactive)
  (push-mark))

(defvar conn--unpoped-marks nil)
(defvar conn--popping-marks nil)

(defun conn--popping-marks-hook ()
  (if conn--popping-marks
      (setf conn--popping-marks nil)
    (when conn--unpoped-marks
      (conn-push-jump-ring (car (last conn--unpoped-marks))))
    (setq mark-ring (nconc mark-ring
                           (mapcar #'copy-marker
                                   (nreverse conn--unpoped-marks))))
    (setf conn--unpoped-marks nil)
    (remove-hook 'post-command-hook 'conn--popping-marks-hook)))

(defun conn-pop-mark-ring ()
  (interactive)
  (setf conn--popping-marks t)
  (cond ((null (mark t))
         (user-error "Mark ring empty"))
        ((/= (point) (mark t))
         (push (point) conn--unpoped-marks))
        (mark-ring
         (push (mark t) conn--unpoped-marks)
         (set-marker (mark-marker) (car mark-ring))
         (set-marker (pop mark-ring) nil))
        (t (user-error "Mark ring empty")))
  (goto-char (mark t))
  (add-hook 'post-command-hook #'conn--popping-marks-hook))

(defun conn-unpop-mark-ring ()
  (interactive)
  (setf conn--popping-marks t)
  (if (null conn--unpoped-marks)
      (user-error "No marks to unpop")
    (when conn--unpoped-marks
      (push-mark (pop conn--unpoped-marks))
      (goto-char (mark t)))))

;;;;; Line Commands

(defun conn-open-line (arg)
  "Open line below the current line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent))

(defun conn-open-line-above (arg)
  "Open line above the current line."
  (interactive "p")
  (forward-line (- (1- arg)))
  (move-beginning-of-line nil)
  (insert "\n")
  (forward-line -1)
  (indent-according-to-mode))

(defun conn-open-line-and-indent (N)
  "Insert a newline, leave point before it and indent the new line.
With arg N, insert N newlines."
  (interactive "p")
  (save-excursion (newline-and-indent N)))

;;;;; Register Setting and Loading

(defvar-keymap conn-register-argument-map
  "." 'register)

(defun conn-set-register-separator (string)
  "Set `register-separator' register to string STRING."
  (interactive
   (list (read-string "Separator: "
                      (let ((reg (get-register register-separator)))
                        (when (stringp reg) reg))
                      'conn-separator-history nil t)))
  (set-register register-separator string))

;; register-load from consult
(defun conn-register-load (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (list (register-read-with-preview "Load register: ")
         current-prefix-arg))
  (condition-case err
      (jump-to-register reg arg)
    (user-error
     (unless (string-search "access aborted" (error-message-string err))
       (insert-register reg (not arg))))))

(defun conn-register-load-and-replace (thing
                                       arg
                                       transform
                                       register)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :interactive 'conn-register-load-and-replace
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (register (conn-read-argument
                   "register"
                   'register
                   conn-register-argument-map
                   (lambda (_) (register-read-with-preview "Register:"))
                   :formatter #'conn-argument-format-register
                   :value (register-read-with-preview "Register:"))))
     (list thing arg transform register)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (atomic-change-group
       (if (and (eq thing 'regions)
                (bound-and-true-p rectangle-mark-mode))
           (delete-rectangle (region-beginning) (region-end))
         (delete-region beg end))
       (register-val-insert (get-register register))))))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))

;;;;; Yanking

(defvar-keymap conn-yank-pop-repeat-map
  "C-y" 'conn-yank-unpop
  "y" 'yank-pop
  "Y" 'conn-yank-with-completion)

;; Adapted from `yank-with-completion'
(defun conn-yank-with-completion (string)
  (interactive
   (let ((ov (make-overlay (region-beginning)
                           (region-end)
                           nil t)))
     (unwind-protect
         (progn
           (overlay-put ov 'invisible t)
           (list (read-from-kill-ring "Yank from kill-ring: ")))
       (delete-overlay ov))))
  (when string
    (let ((inhibit-read-only t)
          (before (< (point) (mark t))))
      (if before
          (funcall (or yank-undo-function 'delete-region) (point) (mark t))
        (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (insert-for-yank string)
      ;; Set the window start back where it was in the yank command,
      ;; if possible.
      (set-window-start (selected-window) yank-window-start t)
      (if before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer))))))))

(defun conn-yank-unpop (arg)
  "Like `yank-pop' but rotate the kill ring in the other direction."
  (interactive "p")
  (yank-pop (- arg)))

(defun conn-yank-replace-rectangle ()
  "Delete the current rectangle and `yank-rectangle'."
  (interactive)
  (save-mark-and-excursion
    (unless (>= (mark t) (point))
      (conn-exchange-mark-command))
    (delete-rectangle (region-beginning) (region-end))
    (yank-rectangle)))

;;;;; Misc Commands

(defun conn-outline-insert-heading ()
  (interactive)
  (conn-with-recursive-stack 'conn-emacs-state
    (save-mark-and-excursion
      (save-current-buffer
        (outline-insert-heading)
        (recursive-edit)))))

(defun conn-rgrep-thing (thing arg transform)
  "`rgrep' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-read-thing-state
                    :interactive 'conn-rgrep-thing
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (let ((search-string
            (read-string "Search for: "
                         (regexp-quote (buffer-substring-no-properties beg end))
                         'grep-regexp-history)))
       (rgrep search-string)))))

(defun conn-occur-thing (thing arg transform)
  "`occur' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-read-thing-state
                    :interactive 'conn-occur-thing
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (let ((search-string
            (read-string "Search for: "
                         (regexp-quote (buffer-substring-no-properties beg end))
                         'grep-regexp-history)))
       (occur search-string)))))

;;;;; Transition Functions

(defun conn-one-emacs-state ()
  "Execute one command in `conn-emacs-state'."
  (interactive)
  (conn-push-state 'conn-one-emacs-state))

(defun conn-one-command ()
  "Execute one command in `conn-command-state'."
  (interactive)
  (conn-push-state 'conn-one-command-state))

(defun conn-previous-emacs-state (arg)
  "Enter `conn-emacs-state' ARG positions back in `conn-emacs-state-ring'.

Interactively ARG is the prefix argument."
  (interactive "p")
  (unless (conn-ring-head conn-emacs-state-ring)
    (user-error "No previous emacs state"))
  (cond ((< arg 0)
         (conn-next-emacs-state (abs arg)))
        ((> arg 0)
         (unless (memq last-command '(conn-previous-emacs-state
                                      conn-next-emacs-state))
           (push-mark nil t))
         (dotimes (_ (1- arg))
           (conn-ring-rotate-forward conn-emacs-state-ring))
         (if (and conn-emacs-state
                  (conn-ring-head conn-emacs-state-ring)
                  (= (point) (conn-ring-head conn-emacs-state-ring)))
             (progn
               (conn-ring-rotate-forward conn-emacs-state-ring)
               (goto-char (conn-ring-head conn-emacs-state-ring)))
           (goto-char (conn-ring-head conn-emacs-state-ring))
           (conn-push-state 'conn-emacs-state)))))

(defun conn-next-emacs-state (arg)
  "Enter `conn-emacs-state' ARG positions forward in `conn-emacs-state-ring'.

Interactively ARG is the prefix argument."
  (interactive "p")
  (unless (conn-ring-head conn-emacs-state-ring)
    (user-error "No next emacs state"))
  (cond ((< arg 0)
         (conn-previous-emacs-state (abs arg)))
        ((> arg 0)
         (unless (memq last-command '(conn-previous-emacs-state
                                      conn-next-emacs-state))
           (push-mark nil t))
         (dotimes (_ arg)
           (conn-ring-rotate-backward conn-emacs-state-ring))
         (goto-char (conn-ring-head conn-emacs-state-ring))
         (conn-push-state 'conn-emacs-state))))

(defun conn-emacs-state ()
  "Enter `conn-emacs-state'."
  (interactive)
  (conn-push-state 'conn-emacs-state))

(defun conn-command-state ()
  "Enter `conn-command-state'."
  (interactive)
  (conn-push-state 'conn-command-state))

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
  (conn-push-state 'conn-emacs-state))

(defun conn-emacs-state-open-line (&optional arg)
  "Open line and enter `conn-emacs-state'.

If ARG is non-nil move down ARG lines before opening line."
  (interactive "p")
  (move-end-of-line arg)
  (newline-and-indent)
  (conn-push-state 'conn-emacs-state))

(defun conn-emacs-state-overwrite (&optional arg)
  "Enter emacs state in `overwrite-mode'.

`overwrite-mode' will be turned off when when emacs state is exited.
If ARG is non-nil enter emacs state in `binary-overwrite-mode' instead."
  (interactive "P")
  (conn-push-state 'conn-emacs-state)
  (conn-state-on-exit _exit-type
    (overwrite-mode -1))
  (if arg
      (binary-overwrite-mode 1)
    (overwrite-mode 1)))

(defun conn-emacs-state-overwrite-binary ()
  "Enter Emacs state in `binary-overwrite-mode'."
  (interactive)
  (conn-emacs-state-overwrite 1))

;;;;; Window Commands

(defun conn-other-buffer ()
  "Switch to the most recently selected buffer.

Repeated calls allow one to switch back and forth between two buffers."
  (interactive)
  (switch-to-buffer nil))

(defun conn-other-place-prefix ()
  "Display next buffer in another place.

Choose from among the following options:

Window: `other-window-prefix'
Frame: `other-frame-prefix'
Tab: `other-tab-prefix'
Prompt: `conn-other-window-prompt-prefix'
Current Window: `conn-this-window-prefix'"
  (interactive)
  (pcase (car (read-multiple-choice
               "Place:"
               '((?w "window")
                 (?f "frame")
                 (?t "tab")
                 (?c "current window")
                 (?g "prompt"))))
    (?w (other-window-prefix))
    (?f (other-frame-prefix))
    (?t (other-tab-prefix))
    (?c (conn-this-window-prefix))
    (?g (conn-other-window-prompt-prefix))))

(defun conn-other-window-prefix (&optional this-window)
  "Display next buffer in another window.

If there are more than 2 windows prompt with `conn-prompt-for-window' to
determine which window to display the buffer in."
  (interactive "P")
  (if this-window
      (conn-this-window-prefix)
    (let ((windows (conn--get-windows nil 'nomini)))
      (if (length< windows 3)
          (other-window-prefix)
        (display-buffer-override-next-command
         (lambda (_ _)
           (cons (conn-prompt-for-window
                  (conn--get-windows nil 'nomini nil nil
                                     (lambda (win)
                                       (not (eq win (selected-window))))))
                 'reuse)))
        (message "Display next command in selected buffer…")))))

(defun conn-other-window-prompt-prefix ()
  "Display next buffer in a window selected by `conn-prompt-for-window'."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (cons (conn-prompt-for-window (conn--get-windows nil 'nomini) t)
           'reuse))
   nil "[select]")
  (message "Display next command in selected buffer…"))

(defun conn-this-window-prefix ()
  "Display next buffer in the currently selected window."
  (interactive)
  (display-buffer-override-next-command
   'display-buffer-same-window
   nil "[current-window]")
  (message "Display next command buffer in current window…"))

(defun conn-transpose-window (window)
  "Prompt for window and swap current window and other window."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn--get-windows nil 'nomini 'visible)))))
  (unless (eq window (selected-window))
    (if window
        (window-swap-states nil window)
      (user-error "No other visible windows"))))

(defun conn-throw-buffer ()
  "Send current buffer to another window and `switch-to-prev-buffer'."
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-prev-buffer)
    (save-selected-window
      (display-buffer
       buf
       (lambda (_ _)
         (cons (conn-prompt-for-window
                (delq (selected-window)
                      (conn--get-windows nil 'nomini)))
               'reuse))))))

(defun conn-yank-window (window)
  "Swap selected window and another window.

Currently selected window remains selected afterwards."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn--get-windows nil 'nomini 'visible)))))
  (unless (eq window (selected-window))
    (if window
        (save-selected-window (window-swap-states nil window))
      (user-error "No other visible windows"))))

;;;;; Minibuffer Commands

(defun conn-yank-thing-to-minibuffer ()
  (interactive)
  (insert-for-yank
   (with-minibuffer-selected-window
     (conn-read-args (conn-read-thing-state
                      :prompt "Thing")
         ((`(,thing ,arg) (conn-thing-argument t))
          (transform (conn-transform-argument)))
       (pcase (conn-bounds-of thing arg)
         ((and (conn-bounds `(,beg . ,end) transform)
               bounds)
          (pcase (conn-bounds-get bounds :direction)
            (1 (goto-char end))
            (-1 (goto-char beg)))
          (filter-buffer-substring beg end))
         (_ (user-error "No last thing to yank")))))))

;;;; Thing Commands

;;;;; Mark

(conn-define-state conn-mark-thing-state (conn-read-thing-state)
  :lighter "MARK")

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-thing-state)
  "p" 'point)

(cl-defgeneric conn-mark-thing-do (thing arg transform)
  (declare (conn-anonymous-thing-property :mark-op)))

(cl-defmethod conn-mark-thing-do ((thing (conn-thing t))
                                  arg
                                  transform)
  (pcase (conn-bounds-of thing arg)
    ((and (conn-bounds `(,beg . ,end) transform)
          (conn-bounds-get :direction))
     (goto-char (if (eql direction 1) end beg))
     (push-mark (if (eql direction 1) beg end) t t)
     (conn-push-state 'conn-mark-state))))

(cl-defmethod conn-mark-thing-do ((thing (conn-thing region))
                                  arg
                                  transform)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (unless (>= (point) end)
       (goto-char beg))
     (push-mark (if (>= (point) end) beg end) t t)
     (conn-push-state 'conn-mark-state))))

(cl-defmethod conn-mark-thing-do ((_thing (conn-thing dispatch))
                                  arg
                                  transform)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix arg
                   :prompt "Thing")
      ((`(,thing ,arg) (conn-thing-argument t))
       (dtform (conn-dispatch-transform-argument))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map))
       (other-end
        (conn-boolean-argument "other-end"
                               'other-end
                               conn-other-end-argument-map))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map)))
    (conn-dispatch-setup
     (oclosure-lambda (conn-action
                       (action-description "Mark"))
         ()
       (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                     (conn-select-target)))
         (conn-protected-let* ((owin (selected-window)
                                     (select-window owin)))
           (select-window window)
           (pcase (conn-bounds-of-dispatch thing arg pt)
             ((and (conn-dispatch-bounds `(,beg . ,end)
                                         (nconc dtform transform))
                   (conn-bounds-get :direction))
              (goto-char (if (eql direction 1) end beg))
              (push-mark (if (eql direction 1) beg end) t t))
             (_ (user-error "No %s found" thing))))))
     thing arg dtform
     :repeat repeat
     :other-end other-end
     :restrict-windows restrict-windows))
  (conn-push-state 'conn-mark-state))

(defun conn-mark-thing (thing arg transform)
  "Mark the region defined by THING, ARG, and TRANSFORM"
  (interactive
   (conn-read-args (conn-mark-thing-state
                    :interactive 'conn-mark-thing
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-mark-thing-do thing arg transform))

;;;;; Yank Replace

(conn-define-state conn-yank-replace-state (conn-read-thing-state)
  :lighter "YANK-REPLACE")

(defun conn-yank-replace-subr (beg end)
  (let ((cg (prepare-change-group))
        (ad-sym (make-symbol "yank-advice")))
    (fset ad-sym (lambda (&rest app)
                   (cancel-change-group cg)
                   (setf cg (prepare-change-group))
                   (delete-region beg end)
                   (let ((yank-undo-function #'ignore))
                     (apply app))))
    (delete-region beg end)
    (yank)
    (advice-add 'yank-pop :around ad-sym)
    (advice-add 'conn-yank-unpop :around ad-sym)
    (advice-add 'conn-yank-with-completion :around ad-sym)
    (set-transient-map
     conn-yank-pop-repeat-map
     t
     (lambda ()
       (advice-remove 'yank-pop ad-sym)
       (advice-remove 'conn-yank-unpop ad-sym)
       (advice-remove 'conn-yank-with-completion ad-sym)
       (undo-amalgamate-change-group cg))
     (format "%s yank pop; %s yank unpop; %s yank with completion"
             (propertize
              (key-description
               (where-is-internal 'yank-pop
                                  (list conn-yank-pop-repeat-map)
                                  t))
              'face 'help-key-binding)
             (propertize
              (key-description
               (where-is-internal 'conn-yank-unpop
                                  (list conn-yank-pop-repeat-map)
                                  t))
              'face 'help-key-binding)
             (propertize
              (key-description
               (where-is-internal 'conn-yank-with-completion
                                  (list conn-yank-pop-repeat-map)
                                  t))
              'face 'help-key-binding)))))

(cl-defgeneric conn-yank-replace-do (thing
                                     arg
                                     transform
                                     &optional
                                     swap
                                     register
                                     check-bounds)
  (declare (conn-anonymous-thing-property :yank-replace-op)))

(cl-defmethod conn-yank-replace-do ((thing (conn-thing t))
                                    arg
                                    transform
                                    &optional
                                    swap
                                    register
                                    check-bounds)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end)
                  (append transform
                          (when check-bounds
                            (list 'conn-check-bounds))))
     (atomic-change-group
       (if register
           (let ((str (cond (swap (filter-buffer-substring beg end t))
                            ((and (eq thing 'region)
                                  (bound-and-true-p rectangle-mark-mode))
                             (delete-rectangle (region-beginning)
                                               (region-end)))
                            (t (delete-region beg end)))))
             (register-val-insert (get-register register))
             (when swap (set-register register str)))
         (goto-char beg)
         (if swap
             (let ((str (filter-buffer-substring beg end t)))
               (yank)
               (kill-new str))
           (conn-yank-replace-subr beg end))))
     (conn-push-command-history 'conn-yank-replace
                                thing
                                arg
                                transform
                                swap
                                register
                                check-bounds))))

(cl-defmethod conn-yank-replace-do ((_thing (conn-thing dispatch))
                                    arg
                                    transform
                                    &optional
                                    swap
                                    register
                                    check-bounds)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix arg
                   :prompt "Yank and Replace"
                   :reference (list conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-thing-argument t))
       (transform (conn-dispatch-transform-argument transform))
       (read-from-kill-ring
        (unless register
          (conn-read-argument
           "read from kill ring"
           'from-kill-ring
           (define-keymap "y" 'from-kill-ring)
           (lambda (_) (read-from-kill-ring "Yank")))))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map))
       (other-end (conn-boolean-argument
                   "stay"
                   'other-end
                   conn-other-end-argument-map))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map)))
    (let ((str (if register
                   (get-register register)
                 (or read-from-kill-ring
                     (current-kill 0)))))
      (cl-assert (stringp str))
      (conn-dispatch-setup
       (oclosure-lambda (conn-action
                         (action-description "Yank and Replace To")
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
           (unless (funcall conn-dispatch-other-end)
             (select-window window))
           (with-selected-window window
             (conn-dispatch-change-group)
             (pcase (conn-bounds-of-dispatch thing arg pt)
               ((conn-bounds (and bounds `(,beg . ,end)) transform)
                (when check-bounds
                  (conn-check-bounds bounds))
                (unless (funcall conn-dispatch-other-end)
                  (goto-char beg))
                (if swap
                    (let ((newstr (filter-buffer-substring beg end)))
                      (delete-region beg end)
                      (save-excursion
                        (goto-char beg)
                        (insert-for-yank str))
                      (conn-dispatch-action-pulse
                       beg (+ beg (length str)))
                      (setq str newstr))
                  (delete-region beg end)
                  (save-excursion
                    (goto-char beg)
                    (insert-for-yank str))
                  (conn-dispatch-action-pulse
                   beg (+ beg (length str)))))
               (_ (user-error "Cannot find thing at point"))))))
       thing arg transform
       :repeat repeat
       :other-end other-end
       :restrict-windows restrict-windows))))

(defvar-keymap conn-swap-argument-map
  "w" 'swap)

(defun conn-yank-replace (thing
                          arg
                          transform
                          &optional
                          swap
                          register
                          check-bounds)
  (interactive
   (conn-read-args (conn-yank-replace-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (swap (conn-boolean-argument "swap" 'swap conn-swap-argument-map))
        (register (conn-read-argument
                   "register"
                   'register
                   conn-register-argument-map
                   (lambda (_) (register-read-with-preview "Register:"))
                   :formatter #'conn-argument-format-register))
        (check-bounds (conn-check-bounds-argument)))
     (list thing arg transform swap register check-bounds)))
  (conn-yank-replace-do thing arg transform swap register check-bounds))

;;;;; Replace

(defvar conn-replace-special-ref
  (append
   (conn-reference-quote
     (("In project" project)
      ("In files" multi-file)))
   (static-if (<= 30 emacs-major-version)
       (conn-reference-quote
         (("As diff" as-diff)
          ("Multi file as diff" multi-file-as-diff))))))

(defvar conn-replace-reference
  (list (conn-reference-page
          "Replace instances of a pattern in a thing."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-replace-special-ref 2))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn--replace-bounds nil)

(conn-define-state conn-replace-state (conn-read-thing-state)
  :lighter "REPLACE")

(defvar-keymap conn-replace-thing-argument-map
  "p" 'project
  "/" 'multi-file
  "'" 'kapply
  "e" 'conn-emacs-state)

(cl-defstruct (conn-replace-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-replace-thing-argument
                 (&aux
                  (required t)
                  (keymap conn-replace-thing-argument-map)
                  (recursive-edit t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (set-flag
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode)))))))

(cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                       (_cmd (eql project)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                       (_cmd (eql multi-file)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                       (_cmd (eql kapply)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                       (_cmd (eql conn-emacs-state)))
  t)

(defvar-keymap conn-regexp-argument-map
  "q" 'regexp)

(defvar-keymap conn-delimited-argument-map
  "d" 'delimited)

(defvar-keymap conn-backward-argument-map
  "z" 'backward)

(defvar-keymap conn-replace-from-map
  "C-M-;" 'conn-replace-insert-separator)

(defun conn-replace-insert-separator ()
  "Insert `query-replace-from-to-separator'."
  (interactive)
  (when query-replace-from-to-separator
    (let ((separator-string
           (when query-replace-from-to-separator
             (if (char-displayable-p
                  (string-to-char (string-replace
                                   " " "" query-replace-from-to-separator)))
                 query-replace-from-to-separator
               " -> "))))
      (insert (propertize separator-string
                          'display separator-string
                          'face 'minibuffer-prompt
                          'separator t)))))

(defvar conn--replace-reading nil)

(defun conn--replace-read-from (prompt
                                regions
                                &optional
                                regexp-flag
                                delimited-flag)
  (minibuffer-with-setup-hook
      (minibuffer-lazy-highlight-setup
       :case-fold case-fold-search
       :filter (lambda (mb me)
                 (cl-loop for (beg . end) in regions
                          when (<= beg mb me end) return t))
       :highlight query-replace-lazy-highlight
       :regexp regexp-flag
       :regexp-function (or replace-regexp-function
                            delimited-flag
                            (and replace-char-fold
                                 (not regexp-flag)
                                 #'char-fold-to-regexp))
       :transform (lambda (string)
                    (let* ((split (query-replace--split-string string))
                           (from-string (if (consp split) (car split) split)))
                      (when (and case-fold-search search-upper-case)
                        (setq isearch-case-fold-search
                              (isearch-no-upper-case-p from-string regexp-flag)))
                      from-string)))
    (minibuffer-with-setup-hook
        (lambda () (setq-local conn--replace-reading t))
      (if regexp-flag
          (query-replace-read-from prompt regexp-flag)
        (query-replace-read-from prompt regexp-flag)))))

(defun conn--replace-read-args (&optional
                                regexp-flag
                                backward
                                regions
                                delimited
                                prompt)
  (let ((prompt (or prompt
                    (concat "Replace"
                            (when backward " backward")
                            (when delimited " word")))))
    (deactivate-mark)
    (conn-with-region-emphasis regions
      (let* ((from (conn--replace-read-from
                    prompt
                    (or regions
                        (list (cons (point-min) (point-max))))
                    regexp-flag
                    delimited))
             (to (if (consp from)
                     (prog1 (cdr from) (setq from (car from)))
                   (query-replace-read-to from prompt regexp-flag))))
        (cons from to)))))

(cl-defgeneric conn-replace-do (thing
                                arg
                                transform
                                &optional
                                delimited
                                backward
                                regexp-flag
                                subregions-p
                                from
                                to)
  (declare (conn-anonymous-thing-property :replace-op)))

(cl-defmethod conn-replace-do ((thing (conn-thing t))
                               arg
                               transform
                               &optional
                               delimited
                               backward
                               regexp-flag
                               subregions-p
                               from
                               to)
  (pcase-let* (((and (conn-bounds (and region `(,beg . ,end))
                                  transform)
                     bounds)
                (conn-bounds-of thing arg))
               (subregions
                (and-let* ((_ subregions-p)
                           (sr (conn-bounds-get bounds
                                                :subregions
                                                transform)))
                  (conn--merge-overlapping-regions
                   (mapcar #'conn-bounds sr)
                   t))))
    (when (or (null from) (null to))
      (pcase-setq `(,from . ,to)
                  (conn--replace-read-args regexp-flag
                                           backward
                                           (or subregions (list region))
                                           delimited)))
    (deactivate-mark)
    (save-excursion
      (if subregions
          (let* ((region-extract-function
                  (lambda (method)
                    (pcase method
                      ('nil
                       (cl-loop for (beg . end) in subregions
                                collect (buffer-substring beg end)))
                      ('delete-only
                       (cl-loop for (beg . end) in subregions
                                do (delete-region beg end)))
                      ('bounds subregions)
                      (_
                       (prog1
                           (cl-loop for (beg . end) in subregions
                                    collect (filter-buffer-substring
                                             beg end method))
                         (cl-loop for (beg . end) in subregions
                                  do (delete-region beg end))))))))
            (perform-replace from to t regexp-flag delimited
                             nil nil beg end backward t))
        (perform-replace from to t regexp-flag delimited
                         nil nil beg end backward))))
  (conn-push-command-history 'conn-replace
                             thing
                             arg
                             transform
                             delimited
                             backward
                             regexp-flag
                             subregions-p
                             from
                             to))

(cl-defmethod conn-replace-do ((thing (eql project))
                               arg
                               transform
                               &optional
                               delimited
                               backward
                               regexp-flag
                               subregions-p
                               from
                               to)
  (when (or (null from) (null to))
    (pcase-setq `(,from . ,to)
                (conn--replace-read-args regexp-flag
                                         backward
                                         nil
                                         delimited)))
  (let ((mstart (make-hash-table :test 'eq)))
    (fileloop-initialize
     (project-files (project-current t))
     (lambda ()
       (when (re-search-forward from nil t)
         (puthash (current-buffer) (match-beginning 0) mstart)))
     (lambda ()
       (perform-replace from to t regexp-flag delimited
                        nil multi-query-replace-map
                        (gethash (current-buffer) mstart (point-min))
                        (point-max)))))
  (fileloop-continue)
  (conn-push-command-history 'conn-replace
                             thing
                             arg
                             transform
                             delimited
                             backward
                             regexp-flag
                             subregions-p
                             from
                             to))

(cl-defmethod conn-replace-do ((thing (eql multi-file))
                               arg
                               transform
                               &optional
                               delimited
                               backward
                               regexp-flag
                               subregions-p
                               from
                               to)
  (let ((files (multi-isearch-read-files)))
    (unless (and (buffer-file-name)
                 (seq-find (lambda (f)
                             (file-equal-p f (buffer-file-name)))
                           files))
      (find-file (car files)))
    (when (or (null from) (null to))
      (pcase-setq `(,from . ,to)
                  (conn--replace-read-args regexp-flag
                                           backward
                                           nil
                                           delimited)))
    (let ((mstart (make-hash-table :test 'eq)))
      (fileloop-initialize
       files
       (lambda ()
         (when (re-search-forward from nil t)
           (puthash (current-buffer) (match-beginning 0) mstart)))
       (lambda ()
         (perform-replace from to t regexp-flag delimited
                          nil multi-query-replace-map
                          (gethash (current-buffer) mstart (point-min))
                          (point-max)))))
    (fileloop-continue)
    (conn-push-command-history 'conn-replace
                               thing
                               arg
                               transform
                               delimited
                               backward
                               regexp-flag
                               subregions-p
                               from
                               to)))

(cl-defmethod conn-replace-do ((_thing (eql widen))
                               &rest _)
  (without-restriction
    (cl-call-next-method)))

(defvar conn-change-reference)

(cl-defmethod conn-replace-do ((_thing (eql conn-emacs-state))
                               arg
                               transform
                               &rest _)
  (conn-read-args (conn-change-state
                   :interactive 'conn-change-thing
                   :prefix arg
                   :prompt "Thing"
                   :reference conn-change-reference)
      ((`(,thing ,arg) (conn-change-thing-argument))
       (transform (conn-transform-argument transform)))
    (conn-change-thing thing arg transform)))

(static-if (<= 30 emacs-major-version)
    (progn
      (cl-defmethod conn-replace-do ((thing (eql as-diff-in-project))
                                     arg
                                     transform
                                     &optional
                                     delimited
                                     backward
                                     regexp-flag
                                     subregions-p
                                     from
                                     to)
        (when (or (null from) (null to))
          (pcase-setq `(,from . ,to)
                      (conn--replace-read-args regexp-flag
                                               nil
                                               nil
                                               delimited)))
        (multi-file-replace-as-diff
         (project-files (project-current))
         (list (or buffer-file-name (current-buffer)))
         from to regexp-flag delimited)
        (conn-push-command-history 'conn-replace
                                   thing
                                   arg
                                   transform
                                   delimited
                                   backward
                                   regexp-flag
                                   subregions-p
                                   from
                                   to))

      (cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                             (_cmd (eql as-diff-in-project)))
        t)

      (cl-defmethod conn-replace-do ((thing (eql as-diff))
                                     arg
                                     transform
                                     &optional
                                     delimited
                                     backward
                                     regexp-flag
                                     subregions-p
                                     from
                                     to)
        (when (or (null from) (null to))
          (pcase-setq `(,from . ,to)
                      (conn--replace-read-args regexp-flag
                                               nil
                                               nil
                                               delimited)))
        (multi-file-replace-as-diff
         (list (or buffer-file-name (current-buffer)))
         from to regexp-flag delimited)
        (conn-push-command-history 'conn-replace
                                   thing
                                   arg
                                   transform
                                   delimited
                                   backward
                                   regexp-flag
                                   subregions-p
                                   from
                                   to))

      (cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                             (_cmd (eql as-diff)))
        t)

      (cl-defmethod conn-replace-do ((thing (eql multi-file-as-diff))
                                     arg
                                     transform
                                     &optional
                                     delimited
                                     backward
                                     regexp-flag
                                     subregions-p
                                     from
                                     to)
        (let ((files (multi-isearch-read-files)))
          (unless (and (buffer-file-name)
                       (seq-find (lambda (f)
                                   (file-equal-p f (buffer-file-name)))
                                 files))
            (find-file (car files)))
          (when (or (null from) (null to))
            (pcase-setq `(,from . ,to)
                        (conn--replace-read-args regexp-flag
                                                 nil
                                                 nil
                                                 delimited)))
          (multi-file-replace-as-diff files from to regexp-flag delimited)
          (conn-push-command-history 'conn-replace
                                     thing
                                     arg
                                     transform
                                     delimited
                                     backward
                                     regexp-flag
                                     subregions-p
                                     from
                                     to)))

      (cl-defmethod conn-argument-predicate ((_arg conn-replace-thing-argument)
                                             (_cmd (eql multi-file-as-diff)))
        t)

      (define-keymap
        :keymap conn-replace-thing-argument-map
        "D" 'as-diff
        "F" 'multi-file-as-diff
        "P" 'as-diff-in-project)))

(defun conn-replace (thing
                     arg
                     transform
                     &optional
                     delimited
                     backward
                     regexp-flag
                     subregions-p
                     from
                     to)
  "Replace FROM with TO in a region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If DELIMITED is non-nil then replace only matches surrounded by word
boundaries.

If BACKWARD is non-nil then replace matches from last to first.

If REGEXP-FLAG is non-nil then treat FROM as a regexp and TO as a regexp
replacement.

If SUBREGIONS-P is non-nil then perform the replacement within each
subregion of the region in turn.

For more information about how the replacement is carried out see
`query-replace' and `query-replace-regexp'."
  (interactive
   (conn-read-args (conn-replace-state
                    :reference conn-replace-reference
                    :prompt "Replace in Thing")
       ((`(,thing ,arg) (conn-replace-thing-argument))
        (transform (conn-transform-argument))
        (subregions-p (conn-subregions-argument (use-region-p)))
        (regexp-flag
         (conn-boolean-argument "regexp"
                                'regexp
                                conn-regexp-argument-map))
        (delimited
         (conn-boolean-argument "word delimited"
                                'delimited
                                conn-delimited-argument-map))
        (backward
         (conn-boolean-argument "backward"
                                'backward
                                conn-backward-argument-map)))
     (list thing
           arg
           transform
           delimited
           backward
           regexp-flag
           subregions-p)))
  (conn-replace-do thing
                   arg
                   transform
                   delimited
                   backward
                   regexp-flag
                   subregions-p
                   from
                   to))

;;;;; Isearch

(defvar conn-isearch-special-ref
  (conn-reference-quote
    (("In multiple file" multi-file)
     ("In multiple buffers" multi-buffer)
     ("In current project" project))))

(defvar conn-isearch-reference
  (list (conn-reference-page
          "Isearch within a thing."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-isearch-special-ref 2))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-isearch-state (conn-read-thing-state)
  :lighter "ISEARCH-IN")

(defvar-keymap conn-isearch-thing-map
  "/" 'multi-file
  "*" 'multi-buffer
  "p" 'project)

(cl-defstruct (conn-isearch-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-isearch-thing-argument
                 (&aux
                  (keymap conn-isearch-thing-map)
                  (required t)
                  (recursive-edit t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (set-flag
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode)))))))

(cl-defmethod conn-argument-predicate ((_arg conn-isearch-thing-argument)
                                       cmd)
  (or (memq cmd '(multi-file multi-buffer project))
      (cl-call-next-method)))

(defun conn-isearch-open-recursive-edit ()
  "Open a recursive edit from within an isearch.

Exiting the recursive edit will resume the isearch."
  (interactive)
  ;; Binding these to nil prevents `with-isearch-suspended' from
  ;; defaulting to the previous search if this is called before a
  ;; search string has been entered.
  (let (regexp-search-ring
        search-ring)
    (save-selected-window
      (with-isearch-suspended
       (atomic-change-group
         (recursive-edit))))))

(defun conn-isearch-restrict-to-thing-subr (thing arg transform subregions)
  (let* ((bounds (conn-bounds-of thing arg))
         (regions
          (if-let* ((sr (and subregions
                             (conn-bounds-get bounds :subregions transform))))
              (conn--merge-overlapping-regions
               (cl-loop for bound in sr collect (conn-bounds bound))
               t)
            (list (conn-bounds bounds transform))))
         (regions
          (cl-loop for (beg . end) in regions
                   collect (cons (conn--create-marker beg)
                                 (conn--create-marker end nil t))))
         (depth (recursion-depth))
         (in-regions-p
          (lambda (beg end)
            (or (/= depth (recursion-depth))
                (cl-loop for (nbeg . nend) in regions
                         thereis (<= nbeg beg end nend)))))
         (thing (upcase
                 (symbol-name
                  (or (conn-command-thing thing)
                      thing))))
         (prefix (concat "[in " thing "] ")))
    (let ((setup (make-symbol "setup"))
          (cleanup (make-symbol "cleanup")))
      (fset setup (lambda ()
                    (when (= depth (recursion-depth))
                      (add-function :after-while (local 'isearch-filter-predicate)
                                    in-regions-p `((isearch-message-prefix . ,prefix)))
                      (remove-hook 'isearch-mode-hook setup t))))
      (fset cleanup (lambda ()
                      (if (and (= depth (recursion-depth))
                               (not isearch-suspended))
                          (remove-hook 'isearch-mode-end-hook cleanup t)
                        (add-hook 'isearch-mode-hook setup nil t))
                      (remove-function (local 'isearch-filter-predicate)
                                       in-regions-p)))
      (add-hook 'isearch-mode-end-hook cleanup nil t))
    (add-function :after-while (local 'isearch-filter-predicate) in-regions-p
                  `((name . conn-isearch-restrict)
                    (isearch-message-prefix . ,prefix)))))

(defun conn-isearch-restrict-to-thing ()
  (interactive)
  ;; Binding these to nil prevents `with-isearch-suspended' from
  ;; defaulting to the previous search if this is called before a
  ;; search string has been entered.
  (let (regexp-search-ring
        search-ring)
    (with-isearch-suspended
     (conn-read-args (conn-isearch-state
                      :prompt "Isearch in Thing"
                      :reference conn-isearch-reference)
         ((`(,thing ,arg) (conn-isearch-thing-argument))
          (subregions (conn-subregions-argument (use-region-p)))
          (transform (conn-transform-argument)))
       (conn-isearch-restrict-to-thing-subr thing
                                            arg
                                            subregions
                                            transform)))))

(cl-defgeneric conn-isearch-in-thing-do (thing
                                         arg
                                         transform
                                         &key
                                         backward
                                         regexp
                                         subregions-p)
  (declare (conn-anonymous-thing-property :isearch-in-op)))

(cl-defmethod conn-isearch-in-thing-do ((thing (conn-thing t))
                                        arg
                                        transform
                                        &key
                                        backward
                                        regexp
                                        subregions-p)
  (conn-isearch-restrict-to-thing-subr thing
                                       arg
                                       transform
                                       subregions-p)
  (if backward
      (isearch-backward regexp t)
    (isearch-forward regexp t)))

(cl-defmethod conn-isearch-in-thing-do ((_thing (eql multi-buffer))
                                        arg
                                        _transform
                                        &key
                                        backward
                                        _regexp
                                        _subregions-p)
  (let ((isearch-forward (not backward)))
    (multi-isearch-buffers
     (if arg
         (multi-isearch-read-matching-buffers)
       (multi-isearch-read-buffers)))))

(cl-defmethod conn-isearch-in-thing-do ((_thing (eql multi-file))
                                        arg
                                        _transform
                                        &key
                                        backward
                                        _regexp
                                        _subregions-p)
  (let ((isearch-forward (not backward)))
    (multi-isearch-files
     (if arg
         (multi-isearch-read-matching-files)
       (multi-isearch-read-files)))))

(cl-defmethod conn-isearch-in-thing-do ((_thing (eql project))
                                        _arg
                                        _transform
                                        &key
                                        backward
                                        _regexp
                                        _subregions-p)
  (if-let* ((_ (fboundp 'project-root))
            (files (project-files (project-current))))
      (let ((isearch-forward (not backward)))
        (multi-isearch-files
         (if-let* ((n (seq-position files (buffer-file-name) 'file-equal-p)))
             (cons (buffer-file-name)
                   (seq-remove-at-position files n))
           files)))
    (user-error "Buffer does not have a project")))

(defun conn-isearch-forward (thing
                             arg
                             transform
                             &optional
                             regexp
                             subregions-p)
  "Isearch forward within the bounds of a thing."
  (interactive
   (conn-read-args (conn-isearch-state
                    :interactive 'conn-isearch-forward
                    :prompt "Isearch in Thing"
                    :reference conn-isearch-reference)
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument (use-region-p)))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument "regexp"
                                       'regexp
                                       conn-regexp-argument-map)))
     (list thing arg transform regexp subregions)))
  (conn-isearch-in-thing-do thing
                            arg
                            transform
                            :backward nil
                            :regexp regexp
                            :subregions-p subregions-p))

(defun conn-isearch-backward (thing
                              arg
                              transform
                              &optional
                              regexp
                              subregions-p)
  "Isearch backward within the bounds of a thing."
  (interactive
   (conn-read-args (conn-isearch-state
                    :interactive 'conn-isearch-backward
                    :prompt "Isearch in Thing"
                    :reference conn-isearch-reference)
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument (use-region-p)))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument
                 "regexp"
                 'regexp
                 conn-regexp-argument-map)))
     (list thing arg transform regexp subregions)))
  (conn-isearch-in-thing-do thing
                            arg
                            transform
                            :backward t
                            :regexp regexp
                            :subregions-p subregions-p))

(defun conn-isearch-exit-other-end ()
  "`isearch-exit' at the other end of the current match."
  (interactive)
  (if isearch-forward
      (isearch-repeat-backward)
    (isearch-repeat-forward))
  (isearch-done))

(defun conn-isearch-thing-to-search-string ()
  "Read a thing and yank it to the isearch string."
  (interactive)
  (with-isearch-suspended
   (pcase (conn-read-args (conn-read-thing-state
                           :prompt "Thing")
              ((`(,thing ,arg) (conn-thing-argument-dwim))
               (transform (conn-transform-argument)))
            (conn-transform-bounds
             (conn-bounds-of thing arg)
             transform))
     ((conn-bounds `(,beg . ,end))
      (let ((string (buffer-substring-no-properties beg end)))
        (if (and isearch-case-fold-search
                 (eq 'not-yanks search-upper-case))
            (setq string (downcase string)))
        (if isearch-regexp (setq string (regexp-quote string)))
        (setq isearch-yank-flag t)
        (setq isearch-new-string (concat isearch-string string)
              isearch-new-message (concat isearch-message
                                          (mapconcat 'isearch-text-char-description
                                                     string ""))))
      (goto-char (if isearch-new-forward beg end))))))

;;;;; Transpose

(defvar conn--recursive-edit-transpose nil)

(defvar conn-transpose-special-ref
  (conn-reference-quote
    (("line" conn-backward-line forward-line)
     ("symbol" forward-symbol)
     ("recursive-edit" recursive-edit))))

(defvar conn-transpose-reference
  (list (conn-reference-page
          "Transpose two things."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-transpose-special-ref 3)))))

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE")

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "i" 'conn-backward-line
  "u" 'forward-symbol
  "f" 'conn-dispatch)

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-bounds-state))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-transpose-state)
  "TAB" 'repeat-dispatch
  "C-w" 'restrict-windows
  "SPC" 'scroll-up-command
  "DEL" 'scroll-down-command
  "C-o" 'other-window)

(defvar conn-transpose-repeat-commands-ref
  (conn-reference-quote
    (("Repeat transposition" conn-transpose-repeat)
     ("Repeat transposition in opposite direction" conn-transpose-repeat-inverse)
     ("Recenter" recenter-top-bottom))))

(defvar conn-transpose-repeat-reference
  (list (conn-reference-page
          (:eval (conn-quick-ref-to-cols
                  conn-transpose-repeat-commands-ref 1))
          "Any other non-prefix command ends repeating.")))

(defun conn-transpose-repeat-help ()
  "Display `conn-duplicate-repeat-reference' help during dispatch repeat."
  (interactive)
  (conn-quick-reference conn-transpose-repeat-reference))

(defun conn-transpose-repeat ()
  (interactive)
  (user-error "Not repeating transpose"))

(defun conn-transpose-repeat-inverse ()
  (interactive)
  (user-error "Not repeating transpose"))

(defvar-keymap conn-transpose-repeat-map
  "?" 'conn-transpose-repeat-help
  "t" 'conn-transpose-repeat
  "T" 'conn-transpose-repeat-inverse)

(defun conn-transpose-setup-repeat-map (repeat repeat-inverse)
  (advice-add 'conn-transpose-repeat :override repeat)
  (advice-add 'conn-transpose-repeat-inverse :override repeat-inverse)
  (set-transient-map
   conn-transpose-repeat-map
   (lambda ()
     (pcase this-command
       ((or 'recenter-top-bottom 'reposition-window
            'universal-argument 'digit-argument 'negative-argument
            'undo 'undo-only 'undo-redo)
        t)
       ((let mc (lookup-key conn-transpose-repeat-map
                            (this-command-keys-vector)))
        (when (and mc (symbolp mc))
          (setq mc (or (command-remapping mc) mc)))
        (and mc (eq this-command mc)))))
   (lambda ()
     (advice-remove 'conn-transpose-repeat repeat)
     (advice-remove 'conn-transpose-repeat-inverse repeat-inverse))
   (concat
    (format "%s repeat"
            (propertize
             (key-description
              (where-is-internal 'conn-transpose-repeat
                                 (list conn-transpose-repeat-map)
                                 t))
             'face 'help-key-binding))
    (when-let* ((key (where-is-internal 'conn-transpose-repeat-inverse
                                        (list conn-transpose-repeat-map)
                                        t)))
      (format "; %s other direction"
              (propertize
               (key-description key)
               'face 'help-key-binding)))
    (when-let* ((key (where-is-internal 'conn-transpose-repeat-help
                                        (list conn-transpose-repeat-map)
                                        t)))
      (format "; %s help"
              (propertize
               (key-description key)
               'face 'help-key-binding))))))

(cl-defgeneric conn-transpose-things-do (cmd arg at-point-and-mark)
  (declare (conn-anonymous-thing-property :transpose-op)))

(cl-defmethod conn-transpose-things-do ((cmd (conn-thing t))
                                        arg
                                        at-point-and-mark)
  (pcase cmd
    ((guard at-point-and-mark)
     (deactivate-mark)
     (pcase-let* (((conn-bounds `(,beg1 . ,end1))
                   (conn-bounds-of cmd arg))
                  ((conn-bounds `(,beg2 . ,end2))
                   (save-excursion
                     (goto-char (mark t))
                     (conn-bounds-of cmd arg))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((guard (use-region-p))
     (deactivate-mark)
     (pcase-let ((beg1 (region-beginning))
                 (end1 (region-end))
                 ((conn-bounds `(,beg2 . ,end2))
                  (conn-bounds-of cmd arg)))
       (transpose-regions beg1 end1 beg2 end2)))
    ((and (let (and thing (pred identity))
            (or (conn-command-thing cmd)
                (and (symbolp cmd)
                     (get cmd 'forward-op)
                     cmd)))
          (let arg (prefix-numeric-value arg)))
     (deactivate-mark)
     (transpose-subr (lambda (N) (forward-thing thing N)) arg)
     (conn-transpose-setup-repeat-map
      (lambda (n)
        (interactive "P")
        (transpose-subr (lambda (N) (forward-thing thing N))
                        (or arg n)))
      (lambda (n)
        (interactive "P")
        (transpose-subr (lambda (N) (forward-thing thing N))
                        (- (or arg n))))))
    (_ (user-error "Invalid transpose thing"))))

(cl-defmethod conn-transpose-things-do ((_cmd (conn-thing expansion))
                                        _arg
                                        _at-point-and-mark)
  (user-error "Invalid transpose thing"))

(cl-defmethod conn-transpose-things-do ((cmd (conn-thing isearch))
                                        arg
                                        _at-point-and-mark)
  (pcase-let* ((bounds (conn-bounds-of cmd arg))
               ((conn-bounds `(,beg1 . ,end1))
                bounds)
               ((conn-bounds `(,beg2 . ,end2))
                (conn-bounds-of (conn-bounds-thing bounds)
                                (conn-bounds-arg bounds))))
    (transpose-regions beg1 end1 beg2 end2)))

(cl-defmethod conn-transpose-things-do ((_cmd (conn-thing region))
                                        _arg
                                        _at-point-and-mark)
  (deactivate-mark)
  (pulse-momentary-highlight-region (region-beginning) (region-end))
  (let ((bounds1 (cons (region-beginning) (region-end)))
        (buf (current-buffer))
        (conn--recursive-edit-transpose t))
    (conn-with-recursive-stack 'conn-command-state
      (unwind-protect
          (progn
            (conn-bounds-of-recursive-edit-mode 1)
            (recursive-edit))
        (conn-bounds-of-recursive-edit-mode -1)))
    (unless (region-active-p)
      (user-error "No region to transpose"))
    (conn--dispatch-transpose-subr
     buf
     (car bounds1)
     (conn-anonymous-thing
       'region
       :bounds-op ( :method (_self _arg)
                    (conn-make-bounds 'region nil bounds1)))
     nil nil
     (current-buffer)
     (point)
     (let ((bounds2 (conn-make-bounds
                     'region nil
                     (cons (region-beginning) (region-end)))))
       (conn-anonymous-thing
         'region
         :bounds-op ( :method (_self _arg) bounds2)))
     nil nil)))

(cl-defmethod conn-transpose-things-do ((_cmd (conn-thing dispatch))
                                        arg
                                        _at-point-and-mark)
  (let ((pt1 (point))
        (buf1 (current-buffer))
        (thing1 (when (use-region-p)
                  (conn-anonymous-thing
                    'region
                    :bounds-op (let ((bounds (conn-make-bounds
                                              'region nil
                                              (cons (region-beginning)
                                                    (region-end)))))
                                 (:method (_self _arg) bounds))))))
    (conn-read-args (conn-dispatch-transpose-state
                     :prompt "Transpose Dispatch"
                     :prefix arg)
        ((`(,thing ,arg) (conn-thing-argument t))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map)))
      (deactivate-mark)
      (conn-dispatch-setup
       (oclosure-lambda (conn-action
                         (action-description "Transpose")
                         (action-no-history t)
                         (action-window-predicate
                          (lambda (win)
                            (not (buffer-local-value 'buffer-read-only
                                                     (window-buffer win))))))
           ()
         (pcase-let* ((`(,pt2 ,window2 ,thing2 ,arg2 ,_transform)
                       (conn-select-target)))
           (conn--dispatch-transpose-subr
            (window-buffer window2) pt2 thing2 arg2 nil
            buf1 pt1 thing1 arg2 nil)))
       thing arg nil
       :other-end :no-other-end
       :restrict-windows restrict-windows))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-transpose-state 'conn--replace-reading)
  ";" (conn-anonymous-thing
        'sexp
        :transpose-op ( :method (_self _arg _at-point-and-mark)
                        (conn--query-replace-read-transpose-from-to))))

;; Coming in emacs 31
(defun conn--query-replace-read-transpose-from-to ()
  (let* ((from-beg (minibuffer-prompt-end))
         (from-end (next-single-property-change from-beg 'separator))
         (to-beg   (and from-end
                        (next-single-property-change from-end 'separator)))
         (to-end   (point-max))
         (beg      (use-region-beginning))
         (end      (use-region-end)))
    (cond
     ((or (not from-end) (not to-beg))
      (user-error "No query-replace separator to transpose around"))
     ((or (not beg) (not end))
      (transpose-regions from-beg from-end to-beg to-end))
     (t
      ;; Calculate intersection of FROM and TO with active region.
      (when (< from-beg beg from-end) (setq from-beg beg))
      (when (< from-beg end from-end) (setq from-end end))
      (when (< to-beg beg to-end)     (setq to-beg beg))
      (when (< to-beg end to-end)     (setq to-end end))
      (transpose-regions from-beg from-end to-beg to-end)))))

(defvar-keymap conn-transpose-thing-argument-map)

(cl-defstruct (conn-transpose-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-transpose-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (required t)
                  (keymap conn-transpose-thing-argument-map)))))

(defvar-keymap conn-transpose-point-and-mark-argument-map
  "z" 'transpose-at-point-and-mark)

(defun conn-transpose-things (thing arg at-point-and-mark)
  "Exchange the current THING and the previous, leaving point after both.

When ARG is non-nil, takes the previous THING and moves if past ARG
following things.

When AT-POINT-AND-MARK is non-nil exchange the regions defined by THING
and ARG at point and mark.

If THING is \\='recursive-edit then exchange the current region and the
region after a `recursive-edit'."
  (interactive
   (conn-read-args (conn-transpose-state
                    :interactive 'conn-transpose-things
                    :prompt "Transpose"
                    :prefix current-prefix-arg
                    :reference conn-transpose-reference)
       ((`(,thing ,arg) (conn-transpose-thing-argument t))
        (at-point-and-mark (conn-boolean-argument
                            "transpose at point and mark"
                            'transpose-at-point-and-mark
                            conn-transpose-point-and-mark-argument-map)))
     (list thing arg at-point-and-mark)))
  (when conn--recursive-edit-transpose
    (user-error "Recursive call to conn-transpose-things"))
  (conn-transpose-things-do thing arg at-point-and-mark))

;;;;; Kill

(defvar conn-kill-special-ref
  (conn-reference-quote
    (("copy filename" filename)
     ("kill matching lines" kill-matching-lines)
     ("keep matching lines" keep-lines)
     ("surround" conn-surround)
     ("outer line" move-end-of-line))))

(defvar conn-kill-reference
  (list (conn-reference-page
          "Kill some things.
If append is set to COLLECT then the first invocation sets the place
which is being killed to and further invocations with `conn-repeat'
append to that place."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-kill-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn-kill-reformat-default t)

(conn-define-state conn-kill-state (conn-read-thing-state)
  :lighter "KILL")

(define-keymap
  :keymap (conn-get-state-map 'conn-kill-state)
  "w" 'copy
  "/" 'filename
  "P" 'project-filename
  ">" 'kill-matching-lines
  "%" 'keep-lines
  "j" 'move-end-of-line)

(defvar-keymap conn-append-argument-map
  "p" 'append)

(defvar-keymap conn-delete-argument-map
  "d" 'delete)

(cl-defstruct (conn-kill-how-argument
               (:include conn-composite-argument)
               ( :constructor conn--kill-how-argument
                 (delete
                  append
                  register
                  separator
                  &aux
                  (value (list delete append register separator)))))
  (delete nil :type boolean)
  (append nil :type symbol)
  (register nil :type (or char nil))
  (separator nil :type (or string nil)))

(cl-defsubst conn-kill-how-argument (&key delete
                                          append
                                          register
                                          separator)
  (declare (important-return-value t)
           (side-effect-free t))
  (cl-assert (memq append '(nil append prepend)))
  (cl-assert (not (and delete (or append register))))
  (cl-assert (not (and separator (null append))))
  (conn--kill-how-argument
   (conn-boolean-argument "delete"
                          'delete
                          conn-delete-argument-map
                          delete)
   (conn-cycling-argument "append"
                          '(nil append prepend repeat)
                          'append
                          :keymap conn-append-argument-map)
   (conn-read-argument "register"
                       'register
                       conn-register-argument-map
                       (lambda (_) (register-read-with-preview "Register:"))
                       :formatter #'conn-argument-format-register)
   (conn-separator-argument separator)))

(cl-defmethod conn-argument-update ((arg conn-kill-how-argument)
                                    cmd
                                    _updater)
  (cl-symbol-macrolet ((delete (conn-argument-value
                                (conn-kill-how-argument-delete arg)))
                       (register (conn-argument-value
                                  (conn-kill-how-argument-register arg)))
                       (append (conn-argument-value
                                (conn-kill-how-argument-append arg)))
                       (separator (conn-argument-value
                                   (conn-kill-how-argument-separator arg))))
    (pcase cmd
      ('append
       (cl-call-next-method)
       (if append
           (setf delete nil)
         (setf separator nil)))
      ('delete
       (cl-call-next-method)
       (when delete
         (setf register nil
               append nil
               separator nil)))
      ('register
       (cl-call-next-method)
       (when register
         (setf delete nil)))
      ((and 'separator (guard append))
       (cl-call-next-method)))))

(cl-defmethod conn-argument-display ((arg conn-kill-how-argument))
  (cl-symbol-macrolet ((delete (conn-kill-how-argument-delete arg))
                       (register (conn-kill-how-argument-register arg))
                       (append (conn-kill-how-argument-append arg))
                       (separator (conn-kill-how-argument-separator arg)))
    (mapcar #'conn-argument-display
            (list append
                  (and (conn-argument-value append) separator)
                  register
                  delete))))

(cl-defmethod conn-argument-extract-value ((arg conn-kill-how-argument))
  (unless (conn-argument-value (conn-kill-how-argument-append arg))
    (setf (conn-argument-value (conn-kill-how-argument-separator arg))
          nil))
  (cl-call-next-method))

(cl-defstruct (conn-kill-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-kill-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (required t)))))

(cl-defmethod conn-argument-predicate ((_arg conn-kill-thing-argument)
                                       (_cmd (eql filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-kill-thing-argument)
                                       (_cmd (eql project-filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-kill-thing-argument)
                                       (_cmd (eql keep-lines)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-kill-thing-argument)
                                       (_cmd (eql kill-matching-lines)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-kill-thing-argument)
                                       (_cmd (eql copy)))
  t)

(cl-defstruct (conn-transform-and-fixup-argument
               (:include conn-composite-argument)
               ( :constructor conn-transform-and-fixup-argument
                 (&aux
                  (transform (conn-transform-argument))
                  (reformat (when conn-kill-reformat-function
                              (conn-reformat-argument)))
                  (value (list transform reformat))))
               ( :constructor conn-dispatch-transform-and-fixup-argument
                 (&optional
                  initial-reformat
                  &aux
                  (reformat (when conn-kill-reformat-function
                              (conn-reformat-argument
                               initial-reformat)))
                  (transform (conn-dispatch-transform-argument))
                  (value (list transform reformat)))))
  (transform nil :type list)
  (reformat nil :type boolean)
  (explicit nil :type boolean))

(cl-defmethod conn-argument-update ((arg conn-transform-and-fixup-argument)
                                    cmd
                                    updater)
  (cl-symbol-macrolet ((tform (conn-transform-and-fixup-argument-transform arg))
                       (fws (conn-transform-and-fixup-argument-reformat arg)))
    (let ((valid nil))
      (cond ((and (conn-argument-update tform cmd (lambda (newval)
                                                    (setq tform newval
                                                          valid t)))
                  valid)
             (unless (conn-transform-and-fixup-argument-explicit arg)
               (setf (conn-reformat-argument-value fws)
                     (not (conn-transform-argument-value tform))))
             (funcall updater arg))
            ((and (conn-argument-update fws cmd (lambda (newval)
                                                  (setq fws newval
                                                        valid t)))
                  valid)
             (setf (conn-transform-and-fixup-argument-explicit arg) t)
             (funcall updater arg))))))

(defun conn-kill-thing (cmd
                        arg
                        transform
                        &optional
                        append
                        delete
                        register
                        separator
                        reformat
                        check-bounds)
  "Kill a region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If REGISTER is non-nil then kill the region into REGISTER instead of the
kill ring.

If APPEND is non-nil then append the killed region to the previous kill.
If killing to a registers then append to the register.  If APPEND is
\\='prepend then prepend to the previous kill or register instead.  If
APPEND is \\='repeat then the first invocation sets the place which is
being killed to and further invocations with `conn-repeat' append to
that place.

If DELETE is non-nil then delete the region without modifying the kill
ring.  If DELETE is non-nil then an error is signaled if either APPEND
or REGISTER is non-nil.

If REFORMAT is non-nil then attempt to fix up the whitespace
around the kill by calling `conn-kill-reformat-function'.
Interactively REFORMAT defaults to the value of
`conn-kill-reformat-default'.

If CHECK-BOUNDS is non-nil then run the `conn-check-bounds-functions'
hook, which see."
  (interactive
   (conn-read-args (conn-kill-state
                    :interactive 'conn-kill-thing
                    :prompt "Thing"
                    :reference conn-kill-reference
                    :display-handler (conn-read-args-display-columns 5 3))
       ((`(,thing ,arg) (conn-kill-thing-argument t))
        (`(,transform ,fixup) (conn-transform-and-fixup-argument))
        (`(,delete ,append ,register ,separator) (conn-kill-how-argument))
        (check-bounds (conn-check-bounds-argument)))
     (list thing arg transform append delete
           register separator fixup check-bounds)))
  (cl-assert (not (and delete (or register append))))
  (cl-callf and reformat (null transform))
  (conn-kill-thing-do cmd
                      arg
                      transform
                      append
                      delete
                      register
                      separator
                      reformat
                      check-bounds)
  (setq this-command 'conn-kill-thing))

(cl-defgeneric conn-kill-reformat (bounds))

(cl-defmethod conn-kill-reformat :after (_bounds
                                         &context
                                         (major-mode (derived-mode prog-mode)))
  (let ((tab-always-indent t))
    (unless (save-excursion
              (beginning-of-line)
              (looking-at-p (rx eol)))
      (indent-for-tab-command))))

(cl-defmethod conn-kill-reformat ((_bounds (conn-thing region)))
  "Noop" nil)

(cl-defmethod conn-kill-reformat ((_bounds (conn-thing char)))
  "Noop" nil)

(cl-defmethod conn-kill-reformat (bounds
                                  &context
                                  (major-mode (derived-mode lisp-data-mode)))
  (cl-call-next-method)
  (cond ((conn-get-thing-property (conn-bounds-thing bounds) :linewise))
        ((save-excursion
           (beginning-of-line)
           (looking-at-p (rx (seq (* (syntax whitespace))
                                  (+ (syntax close-parenthesis))
                                  eol))))
         (unless (save-excursion
                   (move-end-of-line 0)
                   (conn--point-in-comment-p))
           (join-line)))
        ((and (looking-back (rx (seq (* (syntax whitespace))
                                     (syntax open-parenthesis)))
                            (pos-bol))
              (looking-at-p (rx (seq (* (syntax whitespace)) eol))))
         (forward-line)
         (join-line))
        ((save-excursion
           (beginning-of-line)
           (and (looking-at-p (rx (seq (* (syntax whitespace))
                                       eol)))
                (> (car (syntax-ppss)) 0)))
         (unless (save-excursion
                   (move-end-of-line 0)
                   (conn--point-in-comment-p))
           (let ((col (current-column)))
             (join-line)
             (forward-line)
             (move-to-column col))))))

(cl-defmethod conn-kill-reformat (bounds)
  (cl-flet ((empty-lines (&optional backward)
              (save-excursion
                (when backward (forward-line -1))
                (cl-loop for i from 0
                         while (and (looking-at-p (rx (seq (* (syntax whitespace)) eol)))
                                    (not (or (eobp) (bobp))))
                         do (forward-line (if backward -1 1))
                         finally return i))))
    (cond ((looking-back (rx (syntax whitespace)) 1)
           (fixup-whitespace)
           (when (looking-at "[ \t]")
             (forward-char 1)))
          ((looking-at (rx (syntax whitespace)))
           (fixup-whitespace)))
    (when (and (conn-get-thing-property bounds :linewise)
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p (rx eol))))
      (dotimes (_ (min (empty-lines) (empty-lines t)))
        (join-line)))))

(defun conn--kill-region (beg
                          end
                          &optional
                          delete-flag
                          append
                          register
                          separator)
  (when (eq append 'repeat)
    (setq append (and conn-repeating-command 'append)))
  (if register
      (pcase append
        ('nil
         (copy-to-register register beg end delete-flag))
        ('prepend
         (prepend-to-register register beg end delete-flag))
        (_
         (append-to-register register beg end delete-flag)))
    (when (or (and (eq append 'append)
                   (< end beg))
              (and (eq append 'prepend)
                   (< beg end)))
      (cl-rotatef beg end))
    (let ((last-command (if append 'kill-region last-command)))
      (when (and append separator)
        (kill-append (conn-kill-separator-for-region beg end separator)
                     (eq append 'prepend)))
      (if delete-flag
          (kill-region beg end)
        (copy-region-as-kill beg end)))))

(defun conn--kill-string (string &optional append register separator)
  (when (eq append 'repeat)
    (setq append (and conn-repeating-command 'append)))
  (if register
      (if append
          (let ((reg (get-register register))
                (sep (conn-kill-separator-for-strings
                      (list string) separator)))
            (set-register
             register
             (cond ((not reg) string)
                   ((stringp reg)
                    (if (eq append 'prepend)
                        (concat string sep reg)
                      (concat reg sep string)))
                   (t (user-error "Register does not contain text")))))
        (set-register register string))
    (if append
        (let ((sep (conn-kill-separator-for-strings
                    (list string) separator)))
          (when sep
            (kill-append sep (eq append 'prepend)))
          (kill-append string (eq append 'prepend)))
      (kill-new string))))

(cl-defgeneric conn-kill-thing-do (cmd
                                   arg
                                   transform
                                   &optional
                                   append
                                   delete
                                   register
                                   separator
                                   reformat
                                   check-bounds)
  (declare (conn-anonymous-thing-property :kill-op)))

(cl-defmethod conn-kill-thing-do ((_cmd (conn-thing expansion))
                                  &rest _)
  (cl-call-next-method))

(cl-defmethod conn-kill-thing-do ((_cmd (eql filename))
                                  _arg
                                  transform
                                  &optional
                                  append
                                  _delete
                                  register
                                  separator
                                  _reformat
                                  _check-bounds)
  (if-let* ((fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (cond ((memq 'conn-bounds-after-point transform)
                        (file-name-nondirectory fname))
                       ((memq 'conn-bounds-before-point transform)
                        (file-name-directory fname))
                       (t fname))))
      (progn
        (when (memq 'conn-bounds-trim transform)
          (setq str (file-name-sans-extension str)))
        (conn--kill-string str append register separator)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a file")))

(cl-defmethod conn-kill-thing-do ((_cmd (eql project-filename))
                                  _arg
                                  _transform
                                  &optional
                                  append
                                  _delete
                                  register
                                  separator
                                  _reformat
                                  _check-bounds)
  (if-let* ((_ (fboundp 'project-root))
            (fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (expand-file-name (project-root (project-current)))))
      (progn
        (conn--kill-string str append register separator)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a project")))

(cl-defmethod conn-kill-thing-do ((_cmd (eql kill-matching-lines))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  _register
                                  _separator
                                  _reformat
                                  _check-bounds)
  (conn-read-args (conn-read-thing-state
                   :prompt (if delete
                               "Delete Matching Lines In"
                             "Kill Matching Lines In")
                   :prefix arg)
      ((`(,thing ,targ) (conn-thing-argument t))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds (and region `(,beg . ,end)) transform)
       (if delete
           (flush-lines
            (conn-read-regexp "Delete lines containing match for regexp" region)
            beg end t)
         (when append (setq last-command 'kill-region))
         (kill-matching-lines
          (conn-read-regexp "Kill lines containing match for regexp" region)
          beg end t))))))

(cl-defmethod conn-kill-thing-do ((_cmd (eql keep-lines))
                                  arg
                                  transform
                                  &optional
                                  _append
                                  _delete
                                  _register
                                  _separator
                                  _reformat
                                  _check-bounds)
  (conn-read-args (conn-read-thing-state
                   :prompt "Keep Matching Lines In"
                   :prefix arg)
      ((`(,thing ,targ) (conn-thing-argument t))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds (and region `(,beg . ,end)) transform)
       (keep-lines
        (conn-read-regexp "Keep lines containing match for regexp" region)
        beg end t)))))

(defun conn-kill-separator-for-region (beg end separator)
  (pcase separator
    ('nil)
    ((pred stringp) separator)
    (_ (save-excursion
         (goto-char beg)
         (if (search-forward "\n" end t) "\n" " ")))))

(defun conn-kill-separator-for-strings (strings separator)
  (pcase separator
    ('nil)
    ((pred stringp) separator)
    (_ (catch 'sep
         (dolist (str (ensure-list strings))
           (when (string-match "\n" str nil t)
             (throw 'sep "\n")))
         " "))))

(defvar-keymap conn-kill-dispatch-append-map
  "C-p" 'append)

(cl-defmethod conn-kill-thing-do ((_cmd (conn-thing dispatch))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (let ((conn-dispatch-amalgamate-undo t)
        (result nil)
        (strings nil))
    (conn-read-args (conn-dispatch-bounds-state
                     :prefix arg
                     :prompt "Kill"
                     :reference (list conn-dispatch-thing-reference)
                     :display-handler (conn-read-args-display-columns 3 3))
        ((`(,thing ,arg) (conn-thing-argument t))
         (repeat
          (conn-boolean-argument "repeat"
                                 'repeat-dispatch
                                 conn-dispatch-repeat-argument-map))
         (`(,delete ,append ,register ,separator)
          (conn-kill-how-argument
           :append (if (eq append 'repeat) nil append)
           :delete delete
           :register register
           :separator separator))
         (other-end
          (conn-boolean-argument "stay"
                                 'other-end
                                 conn-other-end-argument-map))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map))
         (`(,dtform ,reformat)
          (conn-dispatch-transform-and-fixup-argument
           reformat)))
      (conn-with-dispatch-event-handlers
        ( :handler (cmd)
          (when (eq cmd 'append)
            (setq append (pcase append
                           ('nil 'append)
                           ('prepend nil)
                           (_ 'prepend)))
            (conn-dispatch-handle)))
        ( :keymap conn-kill-dispatch-append-map)
        ( :message 10 (keymap)
          (when-let* ((binding (where-is-internal 'append keymap t)))
            (concat
             (propertize (key-description binding)
                         'face 'help-key-binding)
             " "
             (pcase append
               ('nil "append")
               (val
                (concat
                 (propertize "(" 'face 'shadow)
                 (propertize (format "%s" val)
                             'face 'eldoc-highlight-function-argument)
                 (propertize "|" 'face 'shadow)
                 (propertize (truncate-string-ellipsis) 'face 'shadow)
                 (propertize ")" 'face 'shadow)))))))
        (conn-dispatch-setup
         (oclosure-lambda (conn-action
                           (action-description "Kill"))
             ()
           (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                         (conn-select-target)))
             (unless (funcall conn-dispatch-other-end)
               (select-window window))
             (with-selected-window window
               (conn-dispatch-change-group)
               (pcase (conn-bounds-of-dispatch thing arg pt)
                 ((and bounds
                       (conn-dispatch-bounds `(,beg . ,end)
                                             `(,@dtform
                                               ,@transform
                                               ,@(when check-bounds
                                                   (list 'conn-check-bounds)))))
                  (unless (funcall conn-dispatch-other-end)
                    (push-mark (conn-bounds-get bounds :origin))
                    (goto-char beg))
                  (unless delete
                    (push (cons append (filter-buffer-substring beg end))
                          strings))
                  (delete-region beg end)
                  (conn-dispatch-undo-case
                    :depth 90
                    (:undo
                     (pop strings)
                     (conn-dispatch-undo-pulse beg end))
                    (:cancel
                     (pop strings)))
                  (when reformat
                    (funcall conn-kill-reformat-function bounds)))
                 (_ (user-error "No %s found" thing))))))
         thing arg dtform
         :repeat repeat
         :other-end other-end
         :restrict-windows restrict-windows))
      (when strings
        (let ((sep (conn-kill-separator-for-strings (mapcar #'cdr strings)
                                                    separator)))
          (pcase-dolist (`(,append . ,string) (nreverse strings))
            (setq result
                  (if append
                      (concat result (and result sep) string)
                    (concat string (and result sep) result))))
          (conn--kill-string result append register sep))))))

(cl-defmethod conn-kill-thing-do (cmd
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (pcase (conn-bounds-of cmd arg)
    ((and (conn-bounds `(,beg . ,end)
                       `(,@transform
                         ,@(when check-bounds
                             (list 'conn-check-bounds))))
          bounds)
     (if delete
         (delete-region beg end)
       (conn--kill-region beg end t append register separator))
     (when reformat
       (goto-char beg)
       (funcall conn-kill-reformat-function bounds)))))

(cl-defmethod conn-kill-thing-do ((_cmd (conn-thing line))
                                  &rest _)
  (let ((col (current-column)))
    (cl-call-next-method)
    (move-to-column col)))

(cl-defmethod conn-kill-thing-do ((_cmd (eql copy))
                                  arg
                                  transform
                                  &optional
                                  append
                                  _delete
                                  register
                                  separator
                                  _reformat
                                  _check-bounds)
  (conn-read-args (conn-copy-state
                   :interactive 'conn-copy-thing
                   :prefix arg
                   :prompt "Copy Thing")
      ((`(,thing ,arg) (conn-copy-thing-argument))
       (transform (conn-transform-argument transform))
       (`(,append ,register ,separator)
        (conn-copy-how-argument
         :append append
         :register register
         :separator separator)))
    (conn-copy-thing-do thing
                        arg
                        transform
                        append
                        register
                        separator)))

(cl-defmethod conn-kill-thing-do :extra "rmm" ((_cmd (conn-thing region))
                                               _arg
                                               _transform
                                               &optional
                                               _append
                                               delete
                                               register
                                               _separator
                                               _reformat
                                               _check-bounds)
  (if (bound-and-true-p rectangle-mark-mode)
      (let ((beg (region-beginning))
            (end (region-end)))
        (cond (register (copy-rectangle-to-register register beg end t))
              (delete (delete-rectangle beg end))
              (t (kill-rectangle beg end))))
    (cl-call-next-method)))

;;;;; Copy

(defvar conn-copy-special-ref
  (conn-reference-quote
    (("copy filename" filename)
     ("kill matching lines" copy-matching-lines)
     ("surround" conn-surround))))

(defvar conn-copy-reference
  (list (conn-reference-page
          "Copy some things.
If append is set to COLLECT then the first invocation sets the place
which is being copied to and further invocations with `conn-repeat'
append to that place."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-copy-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-copy-state (conn-read-thing-state)
  :lighter "COPY")

(define-keymap
  :keymap (conn-get-state-map 'conn-copy-state)
  "j" 'move-end-of-line)

(cl-defstruct (conn-copy-how-argument
               (:include conn-composite-argument)
               ( :constructor conn--copy-how-argument
                 (append
                  register
                  separator
                  &aux
                  (value (list append register separator)))))
  (append nil :type symbol)
  (register nil :type (or char nil))
  (separator nil :type (or string nil)))

(cl-defsubst conn-copy-how-argument (&key append
                                          register
                                          separator)
  (declare (important-return-value t)
           (side-effect-free t))
  (cl-assert (memq append '(nil append prepend)))
  (cl-assert (not (and separator (null append))))
  (conn--copy-how-argument
   (conn-cycling-argument "append"
                          '(nil append prepend repeat)
                          'append
                          :keymap conn-append-argument-map
                          :value append)
   (conn-read-argument "register"
                       'register
                       conn-register-argument-map
                       (lambda (_) (register-read-with-preview "Register:"))
                       :formatter #'conn-argument-format-register
                       :value register)
   (conn-separator-argument separator)))

(cl-defmethod conn-argument-update ((arg conn-copy-how-argument)
                                    cmd
                                    _updater)
  (pcase cmd
    ((and 'separator
          (guard (conn-argument-value
                  (conn-copy-how-argument-append arg))))
     (cl-call-next-method))
    (_ (cl-call-next-method))))

(cl-defmethod conn-argument-display ((arg conn-copy-how-argument))
  (cl-symbol-macrolet ((separator (conn-copy-how-argument-separator arg))
                       (register (conn-copy-how-argument-register arg))
                       (append (conn-copy-how-argument-append arg)))
    (list (conn-argument-display append)
          (when (conn-argument-value append)
            (conn-argument-display separator))
          (conn-argument-display register))))

(cl-defmethod conn-argument-extract-value ((arg conn-copy-how-argument))
  (unless (conn-argument-value (conn-copy-how-argument-append arg))
    (setf (conn-argument-value (conn-copy-how-argument-separator arg))
          nil))
  (cl-call-next-method))

(defvar-keymap conn-copy-thing-argument-map
  "/" 'filename
  "P" 'project-filename
  ">" 'copy-matching-lines)

(cl-defstruct (conn-copy-thing-argument
               (:include conn-thing-argument)
               ( :constructor
                 conn-copy-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-copy-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql project-filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql copy-matching-lines)))
  t)

(defun conn-copy-thing (thing
                        arg
                        &optional
                        transform
                        append
                        register
                        separator)
  "Copy a region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If REGISTER is non-nil then copy the region into REGISTER instead of the
kill ring.

If APPEND is non-nil then append the copied region to the previous kill.
If copying to a registers then append to the register.  If APPEND is
\\='prepend then prepend to the previous kill or register instead.  If
APPEND is \\='repeat then the first invocation sets the place which is
being copied to and further invocations with `conn-repeat' append to
that place."
  (interactive
   (conn-read-args (conn-copy-state
                    :interactive 'conn-copy-thing
                    :prompt "Thing"
                    :reference conn-copy-reference)
       ((`(,thing ,arg) (conn-copy-thing-argument))
        (transform (conn-transform-argument))
        (`(,append ,register ,separator) (conn-copy-how-argument)))
     (list thing arg transform append register separator)))
  (conn-copy-thing-do thing
                      arg
                      transform
                      append
                      register
                      separator))

(cl-defgeneric conn-copy-thing-do (cmd
                                   arg
                                   &optional
                                   transform
                                   append
                                   register
                                   separator)
  (declare (conn-anonymous-thing-property :copy-op)))

(cl-defmethod conn-copy-thing-do (cmd
                                  arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn--kill-region beg end nil append register separator)
     (unless executing-kbd-macro
       (pulse-momentary-highlight-region beg end)))))

(cl-defmethod conn-copy-thing-do ((_cmd (eql copy-matching-lines))
                                  arg
                                  &optional
                                  transform
                                  append
                                  _register
                                  _separator)
  (conn-read-args (conn-read-thing-state
                   :prompt "Copy Matching Lines In"
                   :prefix arg)
      ((`(,thing ,targ) (conn-thing-argument t))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds (and region `(,beg . ,end)) transform)
       (when append (setq last-command 'kill-region))
       (copy-matching-lines
        (conn-read-regexp "Copy lines containing match for regexp" region)
        beg end t)))))

(cl-defmethod conn-copy-thing-do ((_cmd (eql filename))
                                  _arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (if-let* ((fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (cond ((memq 'conn-bounds-after-point transform)
                        (file-name-nondirectory fname))
                       ((memq 'conn-bounds-before-point transform)
                        (file-name-directory fname))
                       (t fname))))
      (progn
        (when (memq 'conn-bounds-trim transform)
          (setq str (file-name-sans-extension str)))
        (conn--kill-string str append register separator)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a file")))

(cl-defmethod conn-copy-thing-do ((_cmd (eql project-filename))
                                  _arg
                                  &optional
                                  _transform
                                  append
                                  register
                                  separator)
  (if-let* ((fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (expand-file-name (project-root (project-current)))))
      (progn
        (conn--kill-string str append register separator)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a project")))

(cl-defmethod conn-copy-thing-do ((_cmd (conn-thing expansion))
                                  &rest _)
  (cl-call-next-method))

(cl-defmethod conn-copy-thing-do ((_command (conn-thing dispatch))
                                  arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix arg
                   :prompt "Copy"
                   :reference (list conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-thing-argument t))
       (transform (conn-dispatch-transform-argument transform))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map))
       (`(,append ,register ,separator)
        (conn-copy-how-argument
         :append (if (eq append 'repeat) nil append)
         :register register
         :separator separator))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map)))
    (conn-with-dispatch-event-handlers
      ( :handler (cmd)
        (when (eq cmd 'other-end)
          (setq append (pcase append
                         ('nil 'append)
                         ('prepend nil)
                         (_ 'prepend)))
          (conn-dispatch-handle)))
      ( :message 10 (keymap)
        (when-let* ((binding
                     (where-is-internal 'other-end keymap t)))
          (concat
           (propertize (key-description binding)
                       'face 'help-key-binding)
           " "
           (pcase append
             ('nil "append")
             ('prepend
              (propertize
               "prepend"
               'face 'eldoc-highlight-function-argument))
             (_
              (propertize
               "append"
               'face 'eldoc-highlight-function-argument))))))
      (let ((result nil)
            (strings nil))
        (conn-dispatch-setup
         (oclosure-lambda (conn-action
                           (action-description "Copy"))
             ()
           (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                         (conn-select-target)))
             (with-selected-window window
               (conn-dispatch-change-group)
               (save-mark-and-excursion
                 (pcase (conn-bounds-of-dispatch thing arg pt)
                   ((conn-dispatch-bounds `(,beg . ,end) transform)
                    (push (cons append (filter-buffer-substring beg end))
                          strings)
                    (conn-dispatch-action-pulse beg end)
                    (conn-dispatch-undo-case
                      :depth 90
                      (:undo
                       (pop strings)
                       (conn-dispatch-undo-pulse beg end))
                      (:cancel
                       (pop strings))))
                   (_ (user-error "No %s found" thing)))))))
         thing arg transform
         :repeat repeat
         :other-end :no-other-end
         :restrict-windows restrict-windows)
        (when strings
          (let ((sep (conn-kill-separator-for-strings (mapcar #'cdr strings)
                                                      separator)))
            (pcase-dolist (`(,prepend . ,string) (nreverse strings))
              (setq result
                    (if prepend
                        (concat string (and result sep) result)
                      (concat result (and result sep) string))))
            (conn--kill-string result append register sep)))))))

;;;;; How Many

(defvar conn-how-many-special-ref nil)

(defvar conn-how-many-reference
  (list (conn-reference-page
          "Count the number of matches within a thing."
          (:splice (when conn-how-many-special-ref
                     (conn-reference-quote
                       ((:heading "Special Bindings")
                        (:eval (conn-quick-ref-to-cols
                                conn-how-many-special-ref 3))))))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-how-many-state (conn-read-thing-state)
  :lighter "HOW-MANY")

(defvar-keymap conn-how-many-in-thing-argument-map)

(cl-defstruct (conn-how-many-in-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-how-many-in-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-how-many-in-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(defun conn-how-many-in-thing (thing arg transform)
  "Print and return the number of matches of a regexp.

The matches are restricted to the region defined by THING, ARG, and
TRANSFORM.  For how they are used to define the region see
`conn-bounds-of' and `conn-transform-bounds'.

The regexp is read interactively."
  (interactive
   (conn-read-args (conn-how-many-state
                    :interactive 'conn-how-many-in-thing
                    :reference conn-how-many-reference
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-how-many-in-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-how-many-in-thing-do thing arg transform))

(cl-defgeneric conn-how-many-in-thing-do (thing arg transform)
  (declare (conn-anonymous-thing-property :how-many-op)))

(cl-defmethod conn-how-many-in-thing-do ((thing (conn-thing t))
                                         arg
                                         transform)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds (and region `(,beg . ,end)) transform)
     (how-many
      (conn-read-regexp "How many matches for regexp" region)
      beg end t))))

;;;;; Comment

(defvar conn-comment-special-ref nil)

(defvar conn-comment-reference
  (list (conn-reference-page
          "Comment a thing."
          (:splice (when conn-comment-special-ref
                     (conn-reference-quote
                       ((:heading "Special Bindings")
                        (:eval (conn-quick-ref-to-cols
                                conn-comment-special-ref 3))))))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-comment-state (conn-read-thing-state)
  :lighter "COMMENT")

(cl-defgeneric conn-comment-thing-do (thing arg transform)
  (declare (conn-anonymous-thing-property :comment-op)))

(cl-defmethod conn-comment-thing-do (thing arg transform)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (comment-or-uncomment-region beg end))))

(defvar-keymap conn-comment-thing-argument-map)

(cl-defstruct (conn-comment-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-comment-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-comment-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(defun conn-comment-thing (thing arg transform)
  "Comment the region defined by THING, ARG, and TRANSFORM.

For how they are used to define the region see `conn-bounds-of' and
`conn-transform-bounds'."
  (interactive
   (conn-read-args (conn-comment-state
                    :interactive 'conn-comment-thing
                    :reference conn-comment-reference
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-comment-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-comment-thing-do thing arg transform))

;;;;; Duplicate

(defvar conn-duplicate-special-ref nil)

(defvar conn-duplicate-reference
  (list (conn-reference-page
          "Duplicate a thing."
          (:splice (when conn-duplicate-special-ref
                     (conn-reference-quote
                       ((:heading "Special Bindings")
                        (:eval (conn-quick-ref-to-cols
                                conn-duplicate-special-ref 3))))))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn-duplicate-repeat-commands-ref
  (conn-reference-quote
    (("Repeat duplicate" conn-duplicate-repeat)
     ("Delete previous duplicate" conn-duplicate-delete-repeat)
     ("Indent each duplicate" conn-duplicate-indent-repeat)
     ("Toggle newline padding" conn-duplicate-repeat-toggle-padding)
     ("Comment or uncomment each duplicate" conn-duplicate-repeat-comment)
     ("Recenter" recenter-top-bottom))))

(defvar conn-duplicate-repeat-reference
  (list (conn-reference-page
          (:eval (conn-quick-ref-to-cols
                  conn-duplicate-repeat-commands-ref 1))
          "Any other non-prefix command ends repeating.")))

(defun conn-duplicate-repeat-help ()
  "Display `conn-duplicate-repeat-reference' help during dispatch repeat."
  (interactive)
  (conn-quick-reference conn-duplicate-repeat-reference))

(conn-define-state conn-duplicate-state (conn-read-thing-state)
  :lighter "DUPLICATE")

(define-keymap
  :keymap (conn-get-state-map 'conn-duplicate-state)
  "c" 'copy-from-above-command)

(defvar-keymap conn-duplicate-repeat-map
  "M-?" 'conn-duplicate-repeat-help
  "TAB" 'conn-duplicate-indent-repeat
  "<tab>" 'conn-duplicate-indent-repeat
  "DEL" 'conn-duplicate-delete-repeat
  "<backspace>" 'conn-duplicate-delete-repeat
  "q" 'conn-duplicate-repeat
  "RET" 'conn-duplicate-repeat-toggle-padding
  "<return>" 'conn-duplicate-repeat-toggle-padding
  "c" 'conn-duplicate-repeat-comment)

(defun conn-duplicate-repeat ()
  "Repeat the previous duplicate.

Only available during repeating duplicate."
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-toggle-padding ()
  "Toggle newline padding between duplicates.

Only available during repeating duplicate."
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-comment ()
  "Comment or uncomment each duplicate

Only available during repeating duplicate."
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-delete-repeat (arg)
  "Delete the previous ARG duplicates.

Only available during repeating duplicate."
  (interactive "p")
  (ignore arg)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-indent-repeat ()
  "Indent each duplicate.

Only available during repeating duplicate."
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-subr (beg end &optional repeat)
  "Duplicate the region from BEG to END REPEAT times."
  (unless repeat (setq repeat 1))
  (deactivate-mark)
  (conn-protected-let*
      ((regions (list (make-overlay beg end nil t))
                (mapc #'delete-overlay regions))
       (end-marker (copy-marker end)
                   (set-marker end-marker nil))
       (cg (prepare-change-group)
           (cancel-change-group cg))
       (m1 (make-marker)
           (set-marker m1 nil))
       (m2 (make-marker)
           (set-marker m2 nil))
       (offset (max 0 (- (point) beg)))
       (str (buffer-substring-no-properties beg end))
       (block (seq-contains-p str ?\n))
       (extra-newline nil)
       (padding (if block "\n" " "))
       (regexp (if block "\n" "[\t ]"))
       (commented nil))
    (set-marker-insertion-type m2 t)
    (cl-labels
        ((remove-all-advice (symbol)
           (setf (symbol-function symbol)
                 (advice--cd*r (symbol-function symbol))))
         (dup ()
           (save-excursion
             (goto-char end-marker)
             (unless (looking-back regexp 1)
               (insert padding))
             (let ((rbeg (point))
                   ov)
               (insert str)
               (push (make-overlay rbeg (point) nil t)
                     regions)
               (setq ov (car regions))
               (overlay-put ov 'face 'lazy-highlight)
               (when commented
                 (set-marker m1 (overlay-start ov))
                 (set-marker m2 (overlay-end ov))
                 (set-marker-insertion-type m2 t)
                 (comment-region m1 m2)
                 (move-overlay
                  ov m1 (min m2 (save-excursion
                                  (goto-char (overlay-end ov))
                                  (pos-eol))))
                 (set-mark (overlay-end ov))
                 (set-marker m1 nil)
                 (set-marker m2 nil)))
             (set-marker end-marker (point))))
         (cleanup ()
           (set-marker end-marker nil)
           (set-marker m1 nil)
           (set-marker m2 nil)
           (mapc #'delete-overlay regions)
           (undo-amalgamate-change-group cg)
           (remove-all-advice 'conn-duplicate-indent-repeat)
           (remove-all-advice 'conn-duplicate-repeat)
           (remove-all-advice 'conn-duplicate-delete-repeat)
           (remove-all-advice 'conn-duplicate-repeat-comment)
           (remove-all-advice 'conn-duplicate-repeat-toggle-padding))
         (indent ()
           (interactive)
           (indent-region (overlay-start (car (last regions)))
                          (overlay-end (car regions))))
         (repeat (n)
           (interactive "p")
           (atomic-change-group
             (dotimes (_ n) (dup))))
         (delete (n)
           (interactive "p")
           (when (> (length regions)
                    (+ n repeat))
             (let* ((delete (take n regions))
                    (keep (drop n regions)))
               (delete-region (overlay-end (car keep))
                              (overlay-end (car delete)))
               (mapc #'delete-overlay delete)
               (setq regions keep))))
         (comment ()
           (interactive)
           (atomic-change-group
             (unless (or block extra-newline)
               (conn-duplicate-repeat-toggle-padding))
             (dolist (ov (butlast regions))
               (set-marker m1 (overlay-start ov))
               (set-marker m2 (overlay-end ov))
               (comment-or-uncomment-region m1 m2)
               (move-overlay
                ov m1 (min m2 (save-excursion
                                (goto-char (overlay-end ov))
                                (pos-eol)))))
             (set-marker m1 nil)
             (set-marker m2 nil)
             (setq commented (not commented))))
         (block-padding ()
           (interactive)
           (atomic-change-group
             (dolist (ov (cdr regions))
               (save-excursion
                 (goto-char (overlay-end ov))
                 (if extra-newline
                     (progn
                       (forward-line)
                       (join-line))
                   (newline))))
             (cl-callf not extra-newline)))
         (non-block-padding ()
           (interactive)
           (let* ((extra-newline (not extra-newline))
                  (padding (if extra-newline "\n" " "))
                  (regexp (if extra-newline "\n" "[\t ]")))
             (atomic-change-group
               (when commented
                 (conn-duplicate-repeat-comment))
               (cl-loop for (ov1 ov2) on (reverse regions)
                        while ov2
                        for e1 = (overlay-end ov1)
                        for b2 = (overlay-start ov2)
                        do (save-excursion
                             (goto-char e1)
                             (delete-region e1 b2)
                             (unless (looking-back regexp 1)
                               (insert padding))))))
           (setq extra-newline (not extra-newline)
                 padding (if extra-newline "\n" " ")
                 regexp (if extra-newline "\n" "[\t ]"))))
      (when (> repeat 0) (push-mark nil nil))
      (undo-boundary)
      (dotimes (_ repeat) (dup))
      (goto-char (+ offset (overlay-start (car regions))))
      (advice-add 'conn-duplicate-indent-repeat :override #'indent)
      (advice-add 'conn-duplicate-repeat :override #'repeat)
      (advice-add 'conn-duplicate-delete-repeat :override #'delete)
      (advice-add 'conn-duplicate-repeat-comment :override #'comment)
      (advice-add 'conn-duplicate-repeat-toggle-padding :override
                  (if block #'block-padding #'non-block-padding))
      (set-transient-map
       conn-duplicate-repeat-map
       (lambda ()
         (pcase this-command
           ((or 'recenter-top-bottom 'reposition-window
                'universal-argument 'digit-argument 'negative-argument)
            t)
           ((let mc (lookup-key conn-duplicate-repeat-map
                                (this-command-keys-vector)))
            (when (and mc (symbolp mc))
              (setq mc (or (command-remapping mc) mc)))
            (and mc (eq this-command mc)))))
       #'cleanup
       (format "%s repeat; %s newline; %s indent; %s comment; %s delete; %s help"
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-repeat
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding)
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-repeat-toggle-padding
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding)
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-indent-repeat
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding)
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-repeat-comment
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding)
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-delete-repeat
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding)
               (propertize
                (key-description
                 (where-is-internal 'conn-duplicate-repeat-help
                                    (list conn-duplicate-repeat-map)
                                    t))
                'face 'help-key-binding))))))

(cl-defgeneric conn-duplicate-thing-do (cmd
                                        arg
                                        transform
                                        &optional
                                        repeat)
  (declare (conn-anonymous-thing-property :duplicate-op)))

(cl-defmethod conn-duplicate-thing-do (cmd
                                       arg
                                       transform
                                       &optional
                                       repeat)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn-duplicate-subr beg end repeat))))

(cl-defmethod conn-duplicate-thing-do ((_cmd (eql copy-from-above-command))
                                       arg
                                       _transform
                                       &optional
                                       _repeat)
  (copy-from-above-command arg))

(defvar-keymap conn-duplicate-thing-argument-map)

(cl-defstruct (conn-duplicate-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-duplicate-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-duplicate-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(cl-defmethod conn-argument-predicate ((_arg conn-duplicate-thing-argument)
                                       (_cmd (eql copy-from-above-command)))
  t)

(defun conn-duplicate-thing (thing arg transform &optional repeat)
  "Duplicate the region defined by THING, ARG, and TRANSFORM.

For how they are used to define the region see `conn-bounds-of' and
`conn-transform-bounds'.

If REPEAT is non-nil then duplicate the region REPEAT times.
Interactively REPEAT is given by the prefix argument."
  (interactive
   (conn-read-args (conn-duplicate-state
                    :interactive 'conn-duplicate-thing
                    :prompt "Thing"
                    :reference conn-duplicate-reference)
       ((`(,thing ,arg) (conn-duplicate-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform
           (prefix-numeric-value current-prefix-arg))))
  (conn-duplicate-thing-do thing arg transform repeat))

;;;;; Change

(defvar conn-change-special-ref
  (conn-reference-quote
    (("quoted-insert" quoted-insert)
     ("emacs-state-overwrite" conn-emacs-state-overwrite)
     ("emacs-state-binary-overwrite" conn-emacs-state-overwrite-binary)
     ("surround" conn-surround))))

(defvar conn-change-reference
  (list (conn-reference-page
          "Change some things."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-change-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-change-state (conn-kill-state)
  :lighter "CHANGE")

(define-keymap
  :keymap (conn-get-state-map 'conn-change-state)
  "y" 'yank
  "Y" 'yank-from-kill-ring
  "e" 'conn-emacs-state-overwrite
  "E" 'conn-emacs-state-overwrite-binary
  "j" conn-backward-char-remap
  "l" conn-forward-char-remap
  "h" 'conn-replace
  "q" 'conn-replace)

(defvar-keymap conn-change-thing-argument-map)

(cl-defstruct (conn-change-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-change-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-change-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(cl-defmethod conn-argument-predicate ((_arg conn-change-thing-argument)
                                       (_cmd (eql conn-emacs-state-overwrite-binary)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-change-thing-argument)
                                       (_cmd (eql conn-emacs-state-overwrite)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-change-thing-argument)
                                       (_cmd (eql conn-replace)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-change-thing-argument)
                                       (_cmd (eql yank-replace)))
  t)

(cl-defgeneric conn-change-thing-do (cmd arg transform)
  (declare (conn-anonymous-thing-property :change-op)))

(cl-defmethod conn-change-thing-do (cmd arg transform)
  (pcase-let (((conn-bounds `(,beg . ,end) transform)
               (conn-bounds-of cmd arg)))
    (goto-char beg)
    (delete-region beg end)
    (if (eq 'conn-emacs-state (conn-peek-state))
        (conn-pop-state)
      (conn-push-state 'conn-emacs-state))))

(cl-defmethod conn-change-thing-do ((_cmd (eql conn-emacs-state-overwrite))
                                    _arg
                                    _transform)
  (conn-emacs-state-overwrite))

(cl-defmethod conn-change-thing-do ((_cmd (eql conn-emacs-state-overwrite-binary))
                                    _arg
                                    _transform)
  (conn-emacs-state-overwrite-binary))

(cl-defmethod conn-change-thing-do :extra "rectangle" ((_cmd (conn-thing region))
                                                       _arg
                                                       _transform)
  (if (bound-and-true-p rectangle-mark-mode)
      (call-interactively #'string-rectangle)
    (cl-call-next-method)))

(cl-defmethod conn-change-thing-do ((cmd (conn-thing char))
                                    arg
                                    transform)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (goto-char beg)
     (delete-region beg end)
     (if (= (- end beg) 1)
         (conn-push-state 'conn-one-emacs-state)
       (conn-push-state 'conn-emacs-state)))))

(cl-defmethod conn-change-thing-do ((_cmd (eql conn-replace))
                                    arg
                                    transform)
  (conn-read-args (conn-replace-state
                   :prefix arg
                   :reference conn-replace-reference
                   :prompt "Replace in Thing")
      ((`(,thing ,arg) (conn-replace-thing-argument))
       (transform (conn-transform-argument transform))
       (subregions-p (conn-subregions-argument (use-region-p)))
       (regexp-flag
        (conn-boolean-argument "regexp"
                               'regexp
                               conn-regexp-argument-map))
       (delimited
        (conn-boolean-argument "word delimited"
                               'delimited
                               conn-delimited-argument-map))
       (backward
        (conn-boolean-argument "backward"
                               'backward
                               conn-backward-argument-map)))
    (conn-replace-do thing
                     arg
                     transform
                     delimited
                     backward
                     regexp-flag
                     subregions-p)))

(cl-defmethod conn-change-thing-do ((_thing (eql yank))
                                    arg
                                    transform)
  (conn-read-args (conn-yank-replace-state
                   :interactive 'conn-copy-thing
                   :prefix arg
                   :prompt "Yank Replace")
      ((`(,thing ,arg) (conn-thing-argument-dwim))
       (transform (conn-transform-argument transform))
       (swap (conn-boolean-argument "swap" 'swap conn-swap-argument-map))
       (register (conn-read-argument
                  "register"
                  'register
                  conn-register-argument-map
                  (lambda (_) (register-read-with-preview "Register:"))
                  :formatter #'conn-argument-format-register))
       (check-bounds (conn-check-bounds-argument)))
    (conn-yank-replace-do thing
                          arg
                          transform
                          swap
                          register
                          check-bounds)))

(defun conn-change-thing (cmd arg transform)
  "Change region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'."
  (interactive
   (conn-read-args (conn-change-state
                    :interactive 'conn-change-thing
                    :prompt "Thing"
                    :reference conn-change-reference)
       ((`(,thing ,arg) (conn-change-thing-argument))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-change-thing-do cmd arg transform))

;;;;; Indent

(defvar conn-indent-special-ref nil)

(defvar conn-indent-reference
  (list (conn-reference-page
          "Indent some thing."
          (:splice (when conn-indent-special-ref
                     (conn-reference-quote
                       ((:heading "Special Bindings")
                        (:eval (conn-quick-ref-to-cols
                                conn-indent-special-ref 3))))))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-indent-state (conn-read-thing-state)
  :lighter "INDENT")

(defvar-keymap conn-indent-cleanup-whitespace-map
  "w" 'cleanup-whitespace)

(defvar-keymap conn-indent-thing-argument-map)

(cl-defstruct (conn-indent-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-indent-thing-argument
                 (&aux
                  (recursive-edit t)
                  (keymap conn-indent-thing-argument-map)
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))))

(cl-defgeneric conn-indent-thing-do (cmd arg transform)
  (declare (conn-anonymous-thing-property :indent-op)))

(cl-defmethod conn-indent-thing-do (cmd arg transform)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (indent-region beg end))))

(cl-defmethod conn-indent-thing-do (cmd
                                    arg
                                    transform
                                    &optional
                                    cleanup-whitespace)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (let ((beg (set-marker (make-marker) beg))
           (end (set-marker (make-marker) end)))
       (unwind-protect
           (save-excursion
             (indent-region beg end)
             (when cleanup-whitespace
               (whitespace-cleanup-region beg end)))
         (set-marker beg nil)
         (set-marker end nil))))))

(defun conn-indent-thing (cmd arg transform &optional cleanup-whitespace)
  "Indent the region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If CLEANUP-WHITESPACE is non-nil then also run
`whitespace-cleanup-region' on the region."
  (interactive
   (conn-read-args (conn-indent-state
                    :interactive 'conn-indent-thing
                    :prompt "Thing"
                    :reference conn-indent-reference)
       ((`(,thing ,arg) (conn-indent-thing-argument))
        (transform (conn-transform-argument))
        (cleanup-whitespace
         (conn-boolean-argument "cleanup-whitespace"
                                'cleanup-whitespace
                                conn-indent-cleanup-whitespace-map)))
     (list thing arg transform cleanup-whitespace)))
  (conn-indent-thing-do cmd arg transform cleanup-whitespace))

(defun conn-indent-right ()
  (interactive)
  (user-error "Not currently indenting"))

(defun conn-indent-left ()
  (interactive)
  (user-error "Not currently indenting"))

(defun conn-indent-right-to-tab-stop ()
  (interactive)
  (user-error "Not currently indenting"))

(defun conn-indent-left-to-tab-stop ()
  (interactive)
  (user-error "Not currently indenting"))

(defvar-keymap conn-indent-thing-rigidly-map
  "l" #'conn-indent-right
  "j" #'conn-indent-left
  "L" #'conn-indent-right-to-tab-stop
  "J" #'conn-indent-left-to-tab-stop
  "?" #'conn-indent-rigidly-reference)

(defvar conn-indent-thing-rigidly-reference
  (conn-reference-page
    (:heading "Indent")
    ((:keymap conn-indent-thing-rigidly-map)
     (("left/to tab stop"
       conn-indent-left
       conn-indent-left-to-tab-stop))
     (("right/to tab stop"
       conn-indent-right
       conn-indent-right-to-tab-stop)))))

(defun conn-indent-rigidly-reference ()
  (interactive)
  (conn-quick-reference conn-indent-thing-rigidly-reference))

(defun conn-indent-thing-rigidly (thing arg transform)
  (interactive
   (conn-read-args (conn-indent-state
                    :interactive 'conn-indent-thing-rigidly
                    :prompt "Thing"
                    :reference conn-indent-reference)
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (setq end (copy-marker end))
     (cl-labels ((remove-all-advice (symbol)
                   (setf (symbol-function symbol)
                         (advice--cd*r (symbol-function symbol))))
                 (right ()
                   (interactive)
                   (save-excursion
                     (goto-char beg)
                     (indent-rigidly-right beg end)
                     (setq beg (point))))
                 (left ()
                   (interactive)
                   (save-excursion
                     (goto-char beg)
                     (indent-rigidly-left beg end)
                     (setq beg (point))))
                 (ltts ()
                   (interactive)
                   (save-excursion
                     (goto-char beg)
                     (indent-rigidly-left-to-tab-stop beg end)
                     (setq beg (point))))
                 (rtts ()
                   (interactive)
                   (save-excursion
                     (goto-char beg)
                     (indent-rigidly-right-to-tab-stop beg end)
                     (setq beg (point)))))
       (advice-add 'conn-indent-left :override #'left)
       (advice-add 'conn-indent-right :override #'right)
       (advice-add 'conn-indent-right-to-tab-stop :override #'rtts)
       (advice-add 'conn-indent-left-to-tab-stop :override #'ltts)
       (set-transient-map
        conn-indent-thing-rigidly-map
        t
        (lambda ()
          (set-marker end nil)
          (remove-all-advice 'conn-indent-left)
          (remove-all-advice 'conn-indent-right)
          (remove-all-advice 'conn-indent-right-to-tab-stop)
          (remove-all-advice 'conn-indent-left-to-tab-stop))
        "Type %k to indent region interactively")))))

;;;;; Narrowing Commands

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(defvar conn-narrow-ring-max 14)

(cl-defstruct (conn-narrowing
               (:constructor conn-narrowing
                             (start
                              end
                              &key
                              (point (make-marker)))))
  (start nil :type marker)
  (end nil :type marker)
  (point nil :type marker))

(defun conn-copy-narrowing (narrowing)
  (conn-narrowing
   (copy-marker (conn-narrowing-start narrowing))
   (copy-marker (conn-narrowing-end narrowing))
   :point (copy-marker (conn-narrowing-point narrowing))))

(defun conn-delete-narrowing (narrowing)
  (set-marker (conn-narrowing-start narrowing) nil)
  (set-marker (conn-narrowing-end narrowing) nil)
  (set-marker (conn-narrowing-point narrowing) nil))

(conn-define-state conn-narrow-state (conn-read-thing-state)
  :lighter "NARROW")

(cl-defmethod conn-bounds-of ((_cmd (conn-thing narrow-ring))
                              _arg)
  (cl-symbol-macrolet ((beg (conn-narrowing-start n))
                       (end (conn-narrowing-end n)))
    (cl-loop for n in (conn-ring-list conn-narrow-ring)
             minimize beg into narrow-beg
             maximize end into narrow-end
             collect (conn-make-bounds
                      'restriction nil
                      (cons beg end))
             into narrowings
             finally return (conn-make-bounds
                             'narrow-ring nil
                             (cons narrow-beg narrow-end)
                             :subregions narrowings))))

(defun conn-thing-to-narrow-ring (thing
                                  arg
                                  transform
                                  &optional
                                  subregions-p)
  "Push thing regions to narrow ring."
  (interactive
   (conn-read-args (conn-narrow-state
                    :interactive 'conn-thing-to-narrow-ring
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing arg transform subregions)))
  (pcase (conn-bounds-of thing arg)
    ((and (guard subregions-p)
          (conn-bounds-get :subregions
                           transform
                           (and subregions (pred identity))))
     (cl-loop for bound in (reverse subregions)
              for (b . e) = (conn-bounds bound)
              do (conn--narrow-ring-record b e)))
    ((conn-bounds `(,beg . ,end) transform)
     (conn--narrow-ring-record beg end))))

(defun conn--narrow-ring-record (beg end &optional point)
  (unless (conn-ring-p conn-narrow-ring)
    (setq conn-narrow-ring
          (conn-make-ring conn-narrow-ring-max
                          :cleanup #'conn-delete-narrowing
                          :copier #'conn-copy-narrowing)))
  (pcase-let (((or 'nil (cl-struct conn-narrowing (start bf) (end ef)))
               (conn-ring-head conn-narrow-ring))
              ((or 'nil (cl-struct conn-narrowing (start bb) (end eb)))
               (conn-ring-tail conn-narrow-ring)))
    (cond
     ((and bf (= beg bf) (= end ef)))
     ((and bb (= beg bb) (= end eb))
      (conn-ring-rotate-forward conn-narrow-ring))
     (t (conn-ring-insert-front
         conn-narrow-ring
         (conn-narrowing
          (conn--create-marker beg)
          (conn--create-marker end)
          :point (or (copy-marker point)
                     (make-marker))))))))

(defun conn-cycle-narrowings (arg)
  "Cycle to the ARGth region in `conn-narrow-ring'."
  (interactive "p")
  (unless (= arg 0)
    (pcase (conn-ring-head conn-narrow-ring)
      ((and head (cl-struct conn-narrowing start end))
       (if (and (= (point-min) start)
                (= (point-max) end))
           (set-marker (conn-narrowing-point head) (point))
         (cl-decf arg)))
      (_ (user-error "Narrow ring empty")))
    (cond ((> arg 0)
           (dotimes (_ arg)
             (conn-ring-rotate-forward conn-narrow-ring)))
          ((< arg 0)
           (dotimes (_ (abs arg))
             (conn-ring-rotate-backward conn-narrow-ring))))
    (pcase (conn-ring-head conn-narrow-ring)
      ((cl-struct conn-narrowing start end point)
       (widen)
       (unless (and (<= start (point) end)
                    (null (marker-position point)))
         (push-mark (point) t)
         (goto-char (or (marker-position point) start)))
       (narrow-to-region start end))
      (_ (user-error "Narrow ring empty")))))

(defun conn-widen ()
  "Widen and record the current position in `conn-narrow-ring'.

Records point in `conn-narrow-ring' if the current narrowing is the head
of `conn-narrow-ring'."
  (interactive)
  (pcase (conn-ring-head conn-narrow-ring)
    ((and head (cl-struct conn-narrowing start end))
     (when (and (= (point-min) start)
                (= (point-max) end))
       (set-marker (conn-narrowing-point head) (point)))))
  (widen))

(defun conn-merge-narrow-ring (&optional interactive)
  "Merge overlapping narrowings in `conn-narrow-ring'."
  (interactive (list t))
  (let* ((new (conn--merge-overlapping-regions
               (cl-loop for n in (conn-ring-list conn-narrow-ring)
                        collect (cons (conn-narrowing-start n)
                                      (conn-narrowing-end n)))))
         (new (cl-loop for (beg . end) in new
                       collect (conn-narrowing beg end))))
    (setf (conn-ring-list conn-narrow-ring) new
          (conn-ring-history conn-narrow-ring) (copy-sequence new)))
  (when (and interactive (not executing-kbd-macro))
    (message "Narrow ring merged into %s region"
             (length (conn-ring-list conn-narrow-ring)))))

(defun conn-clear-narrow-ring ()
  "Remove all narrowings from the `conn-narrow-ring'."
  (interactive)
  (mapc (conn-ring-cleanup conn-narrow-ring)
        (conn-ring-list conn-narrow-ring))
  (setf (conn-ring-list conn-narrow-ring) nil
        (conn-ring-history conn-narrow-ring) nil))

(defun conn-pop-narrow-ring ()
  "Pop `conn-narrow-ring'."
  (interactive)
  (pcase (conn-ring-head conn-narrow-ring)
    ('nil (widen))
    ((and (cl-struct conn-narrowing start end)
          (guard (= (point-min) start))
          (guard (= (point-max) end))
          narrowing)
     (conn-ring-delq narrowing conn-narrow-ring)
     (pcase (conn-ring-head conn-narrow-ring)
       ((cl-struct conn-narrowing start end point)
        (unless (<= start (point) end)
          (push-mark (point) t)
          (goto-char (or (marker-position point) start)))
        (narrow-to-region start end))
       (_ (widen))))))

(defun conn--narrow-to-region (beg end &optional record)
  (narrow-to-region beg end)
  (when record (conn--narrow-ring-record beg end)))

(defun conn--narrow-indirect-to-region (beg end &optional record)
  "Narrow from BEG to END in an indirect buffer."
  (let* ((line-beg (line-number-at-pos beg))
         (linenum (- (line-number-at-pos end) line-beg))
         (name (format "%s@%s+%s"
                       (buffer-name (current-buffer)) line-beg linenum)))
    (clone-indirect-buffer-other-window name t)
    (conn--narrow-to-region beg end record)
    (deactivate-mark)))

(defvar-keymap conn-indirect-map
  "d" 'indirect)

(defun conn-narrow-to-thing (thing arg transform &optional indirect)
  "Narrow to the region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If INDIRECT is non-nil then narrow to the region in an indirect buffer.

The region is added to `conn-narrow-ring'."
  (interactive
   (conn-read-args (conn-narrow-state
                    :interactive 'conn-narrow-to-thing
                    :prompt "Thing"
                    :prefix current-prefix-arg)
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (indirect (conn-boolean-argument "indirect"
                                         'indirect
                                         conn-indirect-map)))
     (list thing arg transform indirect)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (unless (and (mark t)
                  (<= beg (point) end)
                  (<= beg (mark t) end))
       (deactivate-mark))
     (if indirect
         (progn
           (conn--narrow-indirect-to-region beg end t)
           (when (called-interactively-p 'interactive)
             (message "Buffer narrowed indirect")))
       (conn--narrow-to-region beg end t)
       (when (called-interactively-p 'interactive)
         (message "Buffer narrowed"))))))

;;;;; Join Lines

(conn-define-state conn-join-lines-state (conn-read-thing-state)
  :lighter "JOIN")

(defun conn-join-lines (thing arg transform &optional subregions-p)
  "Join the lines in region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If SUBREGIONS-P is non-nil then join the lines in each individual
subregion."
  (interactive
   (conn-read-args (conn-join-lines-state
                    :interactive 'conn-join-lines
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing arg transform subregions)))
  (save-mark-and-excursion
    (pcase (conn-bounds-of thing arg)
      ((and (guard subregions-p)
            (conn-bounds-get :subregions transform))
       (pcase-dolist ((conn-bounds `(,beg . ,end))
                      (compat-call
                       sort subregions
                       :lessp (lambda (a b)
                                (> (car (conn-bounds a))
                                   (car (conn-bounds b))))))
         (delete-indentation nil beg end)
         (indent-according-to-mode)))
      ((conn-bounds `(,beg . ,end) transform)
       (delete-indentation nil beg end)
       (indent-according-to-mode)))))

;;;;; Shell Command

(defvar-keymap conn-shell-command-replace-map
  "w" 'replace)

(conn-define-state conn-join-lines-state (conn-read-thing-state)
  :lighter "SHELL")

(defun conn-shell-command-on-thing (thing
                                    arg
                                    transform
                                    &optional
                                    replace
                                    subregions)
  (interactive
   (conn-read-args (conn-join-lines-state
                    :interactive 'conn-shell-command-on-thing
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument
                     (and (use-region-p)
                          (region-noncontiguous-p))))
        (replace (conn-boolean-argument "replace"
                                        'replace
                                        conn-shell-command-replace-map)))
     (list thing arg transform replace subregions)))
  (conn-shell-command-on-thing-do thing arg transform replace subregions))

(cl-defgeneric conn-shell-command-on-thing-do (thing
                                               arg
                                               transform
                                               &optional
                                               replace
                                               subregions)
  (declare (conn-anonymous-thing-property :shell-command-op)))

(cl-defmethod conn-shell-command-on-thing-do (thing
                                              arg
                                              transform
                                              &optional
                                              replace
                                              _subregions)
  (pcase (conn-bounds-of thing arg)
    ;; TODO: handle subregions
    ;; ((and (guard subregions)
    ;;       (conn-bounds-get :subregions transform)
    ;;       (conn-bounds `(,beg . ,end))))
    ((conn-bounds `(,beg . ,end) transform)
     (shell-command-on-region
      beg end
      (read-shell-command "Shell command on region: ")
      replace
      replace
      shell-command-default-error-buffer
      t nil))))

(provide 'conn-commands)
