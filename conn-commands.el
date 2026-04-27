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
(require 'conn-read-args)
(require 'conn-dispatch)
(require 'conn-kapply)
(require 'conn-expand)
(eval-when-compile
  (require 'cl-lib))

(autoload 'multi-isearch-read-files "misearch")
(autoload 'multi-isearch-read-matching-files "misearch")
(autoload 'multi-isearch-read-buffers "misearch")
(autoload 'multi-isearch-read-matching-buffers "misearch")
(autoload 'kmacro-ring-head "kmacro")
(autoload 'pulse-momentary-highlight-overlay "pulse")
(autoload 'hi-lock-regexp-okay "hi-lock")

(declare-function outline-insert-heading "outline")
(declare-function ffap-file-at-point "ffap")
(declare-function project-files "project")
(declare-function project-root "project")
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")
(declare-function fileloop-continue "fileloop")
(declare-function hi-lock-read-face-name "hi-lock")

(defvar hi-lock-auto-select-face)
(defvar hi-lock-interactive-patterns)
(defvar hi-lock-interactive-lighters)

;;;; Commands

(defun conn-toggle-highlight-at-point (&optional read)
  (interactive "P")
  (require 'hi-lock)
  (let ((hi-lock-auto-select-face t)
        (regexps nil))
    (cond ((use-region-p)
           (let ((re (hi-lock-regexp-okay
                      (regexp-quote
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))))))
             (hi-lock-face-buffer
              (if (not read) re
                (read-regexp "Regexp" re 'regexp-history))
              (hi-lock-read-face-name)))
           (deactivate-mark))
          ((setq regexps (and (fboundp 'hi-lock--regexps-at-point)
                              (hi-lock--regexps-at-point)))
           (mapc #'hi-lock-unface-buffer regexps))
          (t
           (hi-lock-face-buffer
            (if (not read) (find-tag-default-as-symbol-regexp)
              (read-regexp "Regexp"
                           (find-tag-default-as-symbol-regexp)
                           'regexp-history))
            (hi-lock-read-face-name))))))

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
        (message "Keyboard macro bound to %s"
                 (format-kbd-macro key-seq))))))

(defun conn-repeat-try-next () nil)

(defun conn-repeat (&optional arg)
  "Repeat the last conn operator."
  (interactive "P")
  (unless conn-command-history
    (user-error "No repeatable last command"))
  (cl-letf ((cmd (if arg
                     (conn-read-from-command-history)
                   (car conn-command-history)))
            (conn-repeating-command t)
            ((symbol-function 'conn-repeat-try-next)
             (if arg
                 (lambda () (error "Cannot repeat command"))
               (lambda ()
                 (let ((conn-command-history (cdr conn-command-history)))
                   (conn-repeat arg))))))
    (setq this-command (car cmd))
    (apply #'funcall-interactively cmd)))

;;;;; Movement

(defun conn-to-char-forward (char count)
  (declare (conn-thing-command to-char))
  (interactive "cChar: \np")
  (if (< count 0)
      (conn-to-char-backward char (abs count))
    (conn-protected-let*
        ((case-fold-search (or (char-uppercase-p char)
                               case-fold-search))
         (str (char-to-string char))
         (beg (point) (goto-char beg)))
      (while (and (> count 0)
                  (search-forward str)
                  (conn--region-visible-p (1- (point)) (point)))
        (cl-decf count)))))

(defun conn-to-char-backward (char count)
  (declare (conn-thing-command to-char))
  (interactive "cChar: \np")
  (if (< count 0)
      (conn-to-char-forward char (abs count))
    (conn-protected-let*
        ((case-fold-search (or (char-uppercase-p char)
                               case-fold-search))
         (str (char-to-string char))
         (beg (point) (goto-char beg)))
      (while (and (> count 0)
                  (search-backward str)
                  (conn--region-visible-p (point) (1+ (point))))
        (cl-decf count)))))

(defun conn-forward-up-list (arg)
  (interactive "p")
  (backward-up-list (- arg)))

(defun conn-backward-down-list (arg)
  (interactive "p")
  (down-list (- arg)))

(defun conn-forward-visual-line (arg)
  "Move forward ARG visual lines."
  (declare (conn-thing-command visual-line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (let ((line-move-visual t))
    (vertical-motion 0)
    (line-move arg t)))

(defun conn-backward-visual-line (arg)
  "Move backward ARG visual lines."
  (declare (conn-thing-command visual-line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (conn-forward-visual-line (- arg)))

(defun conn-goto-line (line)
  "Goto absolute line, 1 origin.

Respects the current restriction."
  (declare (conn-thing-command line #'conn-continuous-thing-other-end-handler))
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
      (backward-up-list (* dir (1- (abs arg))) t t)
      (if (conn--point-in-comment-or-string-p)
          (progn
            (while (conn--point-in-comment-or-string-p)
              (backward-up-list dir t t))
            (if (> dir 0)
                (skip-syntax-forward (rx (syntax string-quote)))
              (skip-syntax-backward (rx (syntax string-quote)))))
        (backward-up-list dir t t)
        (down-list dir))
      (when (= (point) pt)
        (backward-up-list (1+ dir) t t)
        (down-list dir)))))

(defun conn-forward-up-inner-list (arg)
  (declare (conn-thing-command inner-list #'conn-inner-list-handler))
  (interactive "p")
  (conn-backward-up-inner-list (- arg)))

(defun conn-forward-defun (N)
  "Move forward by defuns.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (declare (conn-thing-command defun #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(defun conn-backward-symbol (arg)
  "`forward-symbol' in reverse."
  (declare (conn-thing-command symbol #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (forward-symbol (- arg)))

(defun conn-scroll-down (&optional arg)
  "`scroll-down-command' leaving point at the same relative window position.

Pulses line that was the first visible line before scrolling."
  (declare (conn-thing-command visible #'conn-discrete-thing-other-end-handler)
           (conn-jump #'conn-ignore-repeat-jump-handler))
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (progn (beep) (message "Beginning of buffer"))
    (let ((start (window-start)))
      (scroll-down arg)
      (unless executing-kbd-macro
        (let ((o (save-excursion
                   (goto-char start)
                   (make-overlay (pos-bol) (pos-bol 2)))))
          (overlay-put o 'pulse-delete t)
          (overlay-put o 'window (selected-window))
          (pulse-momentary-highlight-overlay o))))))
(put 'conn-scroll-down 'scroll-command t)

(defun conn-scroll-up (&optional arg)
  "`scroll-up-command' leaving point at the same relative window position.

Pulses line that was the last visible line before scrolling."
  (declare (conn-thing-command visible #'conn-discrete-thing-other-end-handler)
           (conn-jump #'conn-ignore-repeat-jump-handler))
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (let ((end (window-end)))
      (scroll-up arg)
      (unless executing-kbd-macro
        (let ((o (save-excursion
                   (goto-char end)
                   (make-overlay (pos-bol) (pos-bol 0)))))
          (overlay-put o 'pulse-delete t)
          (overlay-put o 'window (selected-window))
          (pulse-momentary-highlight-overlay o))))))
(put 'conn-scroll-up 'scroll-command t)

(defun conn-backward-line (N)
  "`forward-line' by N but backward."
  (declare (conn-thing-command line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (forward-line (- N)))

(defun conn-backward-whitespace (N)
  "`forward-whitespace' by N but backward."
  (declare (conn-thing-command whitespace #'conn-discrete-thing-other-end-handler))
  (interactive "p")
  (forward-whitespace (- N)))

(defun conn--end-of-inner-line-1 ()
  (goto-char (line-end-position))
  (when-let* ((cs (and comment-start-skip
                       (conn--point-in-comment-p)
                       (save-excursion
                         (comment-search-backward
                          (line-beginning-position) t)))))
    (goto-char cs))
  (skip-chars-backward " \t" (line-beginning-position)))

(defun conn-forward-inner-line (N)
  "Move to the last non-whitespace, non-comment character in current line.

If point is already at or after the last non-whitespace, non-comment
character in the current line then move to the end of the next inner
line.

Empty lines are skipped."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-other-end-handler))
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
  "Move to the first non-whitespace character in current line.

If point is already at or before the first non-whitespace character
in the current line then move to the beginning of the previous inner
line.

Repeat N times.

Empty lines are skipped."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (conn-forward-inner-line (- N)))

(defun conn-forward-inner-line-dwim (N)
  "Move to the first non-whitespace character in current line.

If the point is already at the first non-whitespace character in the
current line then move to the beginning of the previous inner line.

Repeat N times.

Empty lines are skipped."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-other-end-handler))
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
  "Move to the last non-whitespace, non-comment character in current line.

If the point is already at the last non-whitespace, non-comment
character in the current line then move to the end of the next inner
line.

Repeat N times.

Empty lines are skipped."
  (declare (conn-thing-command inner-line #'conn-continuous-thing-other-end-handler))
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
  "Move to the end of the current line.

If already at the end of the current line, move to the end of the next
line.

With argument N not nil or 1 move to the Nth line end position from
point.

Line beginning positions are determined by `move-end-of-line'."
  (declare (conn-thing-command outer-line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (if (< N 0)
      (conn-backward-outer-line (abs N))
    (move-end-of-line (+ N (if (eolp) 1 0)))))

(defun conn-backward-outer-line (&optional N)
  "Move to the beginning of the current line.

If already at the beginning of the current line, move to the beginning
of the next line.

With argument N not nil or 1 move to the Nth line beginning position
from point.

Line beginning positions are determined by `move-beginning-of-line'."
  (declare (conn-thing-command outer-line #'conn-continuous-thing-other-end-handler))
  (interactive "p")
  (if (< N 0)
      (conn-forward-outer-line (abs N))
    (move-beginning-of-line (- 2 (+ N (if (bolp) 1 0))))))

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
    (push-mark)
    (rectangle-mark-mode 1)
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
  (if conn-mark-state-ring
      (pcase (conn-ring-extract-head conn-mark-state-ring)
        (`(,pt ,mk ,rmm)
         (goto-char pt)
         (push-mark mk t t)
         (set-marker pt nil)
         (set-marker mk nil)
         (when rmm (rectangle-mark-mode 1))
         (conn-push-state 'conn-mark-state))
        (_ (user-error "No previous mark state")))
    (user-error "Mark state ring empty")))

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

(defvar-keymap conn-register-argument-map)

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
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (register (conn-read-argument
                   "register"
                   'register
                   conn-register-argument-map
                   (lambda (_) (register-read-with-preview "Register: "))
                   :formatter #'conn-argument-format-register
                   :value (register-read-with-preview "Register: "))))
     (list thing arg transform register)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (atomic-change-group
       (if (and (eq thing 'regions)
                (bound-and-true-p rectangle-mark-mode))
           (delete-rectangle (region-beginning) (region-end))
         (delete-region beg end))
       (register-val-insert (get-register register)))))
  (conn-push-command-history 'conn-register-load-and-replace
                             thing
                             arg
                             transform
                             register))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))

;;;;; Yanking

(defvar-keymap conn-yank-pop-repeat-map)

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
       (rgrep search-string))))
  (conn-push-command-history 'conn-rgrep-thing
                             thing
                             arg
                             transform))

(defun conn-occur-thing (thing arg transform)
  "`occur' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-read-thing-state
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
       (occur search-string))))
  (conn-push-command-history 'conn-occur-thing
                             thing
                             arg
                             transform))

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
  (conn-state-on-exit _transition
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
    (let ((windows (conn-get-windows nil 'nomini)))
      (if (length< windows 3)
          (other-window-prefix)
        (display-buffer-override-next-command
         (lambda (_ _)
           (cons (conn-prompt-for-window
                  (conn-get-windows nil 'nomini nil nil
                                    (lambda (win)
                                      (not (eq win (selected-window))))))
                 'reuse)))
        (message "Display next command in selected buffer…")))))

(defun conn-other-window-prompt-prefix ()
  "Display next buffer in a window selected by `conn-prompt-for-window'."
  (interactive)
  (display-buffer-override-next-command
   (lambda (_ _)
     (cons (conn-prompt-for-window (conn-get-windows nil 'nomini) t)
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
                (conn-get-windows nil 'nomini 'visible 'not-dedicated)))))
  (unless (eq window (selected-window))
    (if window
        (window-swap-states nil window)
      (user-error "No other visible windows"))))

(defun conn-delete-window (window)
  "Prompt for window and delete it."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn-get-windows nil 'nomini 'visible)))))
  (delete-window window))

(defun conn-throw-buffer (window)
  "Send current buffer to another window and `switch-to-prev-buffer'."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn-get-windows nil 'nomini 'visible 'not-dedicated)))))
  (let ((buf (current-buffer)))
    (with-selected-window window
      (switch-to-buffer buf))
    (switch-to-prev-buffer)))

(defun conn-yank-window (window)
  "Swap selected window and another window.

Currently selected window remains selected afterwards."
  (interactive
   (list (conn-prompt-for-window
          (delq (selected-window)
                (conn-get-windows nil 'nomini 'visible 'not-dedicated)))))
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

;;;;; Dynamic Commands

(defvar conn-dwim-at-point-hook nil)

(defun conn--dwim-at-point-filter (_orig)
  (pcase (run-hook-with-args-until-success 'conn-dwim-at-point-hook)
    (:punt nil)
    (cmd cmd)))

(defvar conn-dwim-at-point
  `(menu-item
    "DWIM at point"
    nil
    :filter ,#'conn--dwim-at-point-filter))

(defvar conn-alt-dwim-at-point-hook nil)

(defun conn--alt-dwim-at-point-filter (_orig)
  (pcase (run-hook-with-args-until-success 'conn-alt-dwim-at-point-hook)
    (:punt nil)
    (cmd cmd)))

(defvar conn-alt-dwim-at-point
  `(menu-item
    "Alt DWIM at point"
    nil
    :filter ,#'conn--alt-dwim-at-point-filter))

(defun conn-dwim-heading ()
  (when (and (bound-and-true-p outline-regexp)
             (bound-and-true-p outline-minor-mode))
    (let ((beg (line-beginning-position)))
      (when (save-excursion
              (goto-char beg)
              (and (bolp)
                   (looking-at outline-regexp)))
        'outline-cycle))))

(defun conn-dwim-alt-heading ()
  (when (and (bound-and-true-p outline-regexp)
             (bound-and-true-p outline-minor-mode))
    (let ((beg (line-beginning-position)))
      (when (save-excursion
              (goto-char beg)
              (and (bolp)
                   (looking-at outline-regexp)))
        'conn-outline-state))))

(defun conn-dwim-eval-sexp ()
  (when (and (derived-mode-p '(emacs-lisp-mode
                               lisp-interaction-mode))
             (eql (point)
                  (cdr (bounds-of-thing-at-point 'sexp))))
    'pp-eval-last-sexp))

(defun conn-dwim-describe-symbol ()
  (when (and (derived-mode-p '(emacs-lisp-mode
                               lisp-interaction-mode))
             (bounds-of-thing-at-point 'symbol))
    (when-let* ((sym (intern-soft (thing-at-point 'symbol))))
      (lambda ()
        (interactive)
        (describe-symbol sym)))))

(defun conn-dwim-xref-definitions ()
  (and-let* ((_ (derived-mode-p 'prog-mode))
             (bounds (bounds-of-thing-at-point 'symbol))
             (_ (<= (car bounds) (point) (cdr bounds))))
    'xref-find-definitions))

(defun conn-dwim-button ()
  (cond ((and (fboundp 'widget-apply)
              (ignore-errors
                (widget-apply (get-char-property (point) 'button)
                              :active)))
         'widget-button-press)))

;; From embark
(defun conn-dwim-file ()
  (require 'ffap)
  (or (and (derived-mode-p 'dired-mode)
           (fboundp 'dired-get-filename)
           (dired-get-filename t 'no-error-if-not-filep)
           'dired-find-file)
      (and (derived-mode-p 'image-dired-thumbnail-mode)
           (fboundp 'image-dired-original-file-name)
           (image-dired-original-file-name)
           'image-dired-display-this)
      (and (ffap-file-at-point)
           #'find-file-at-point)))

(defun conn-dwim-hs ()
  (when (bound-and-true-p hs-minor-mode)
    'hs-toggle-hiding))

;; From embark
(defun conn-dwim-alt-file ()
  (require 'ffap)
  (and (ffap-file-at-point)
       #'dired-at-point))

(add-hook 'conn-dwim-at-point-hook #'conn-dwim-button -70)
(add-hook 'conn-dwim-at-point-hook #'conn-dwim-file -20)
(add-hook 'conn-dwim-at-point-hook #'conn-dwim-xref-definitions -10)
(add-hook 'conn-dwim-at-point-hook #'conn-dwim-eval-sexp 0)
(add-hook 'conn-dwim-at-point-hook #'conn-dwim-heading 20)
(add-hook 'conn-dwim-at-point-hook #'conn-dwim-hs 80)

(add-hook 'conn-alt-dwim-at-point-hook #'conn-dwim-alt-file -20)
(add-hook 'conn-alt-dwim-at-point-hook #'conn-dwim-describe-symbol 0)
(add-hook 'conn-alt-dwim-at-point-hook #'conn-dwim-alt-heading 20)

(defvar conn-emacs-state-tab-hook nil)

(defun conn--emacs-state-tab-filter (_orig)
  (run-hook-with-args-until-success 'conn-emacs-state-tab-hook))

(defvar conn-emacs-state-tab
  `(menu-item
    "Smart tab"
    nil
    :filter ,#'conn--emacs-state-tab-filter))

;;;; Thing Commands

(defvar-keymap conn-stay-argument-map)
(defvar-keymap conn-dispatch-stay-map)

(defvar conn-stay-argument-documentation
  "Do not move the point after performing the dispatch action.")

(defvar conn-regexp-argument-documentation
  "Toggle matching by regular expressions.")

(defvar conn-word-delimited-argument-documentation
  "Toggle whether matches must be bounded by word delimiters.")

(defvar conn-backward-argument-documentation
  "Toggle whether matches should be operated on backward.")

;;;;; Mark

(conn-define-state conn-mark-thing-state (conn-read-thing-state)
  :lighter "MARK")

(cl-defstruct (conn-mark-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-mark-thing-argument
                 (&aux (required t)))))

(conn-define-argument-command ((arg conn-mark-thing-argument)
                               (cmd (eql conn-exchange-mark-command)))
  "Activate the mark and exhange the point and mark.")

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

(cl-defmethod conn-mark-thing-do ((_thing (eql conn-exchange-mark-command))
                                  _arg
                                  _transform)
  (conn-exchange-and-mark-command))

(cl-defmethod conn-mark-thing-do ((_thing (eql conn-rectangle-mark))
                                  _arg
                                  _transform)
  (cl-call-next-method)
  (rectangle-mark-mode 1))

(conn-define-state conn-mark-dispatch-state (conn-dispatch-bounds-state))

(cl-defmethod conn-mark-thing-do ((_thing (conn-thing dispatch))
                                  arg
                                  transform)
  (conn-read-args (conn-mark-dispatch-state
                   :prefix arg
                   :reference (list conn-dispatch-thing-reference)
                   :prompt "Thing")
      ((`(,thing ,arg) (conn-dispatch-thing-argument t))
       (dtform (conn-dispatch-transform-argument))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               :documentation conn-repeat-argument-documentation))
       (other-end
        (conn-boolean-argument "other-end"
                               'other-end
                               conn-other-end-argument-map
                               :documentation conn-dispatch-other-end-documentation))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map
                               :documentation conn-this-win-argument-documentation)))
    (conn-dispatch-setup
     (conn-action ()
       (:description "Mark")
       (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                     (conn-select-target)))
         (conn-protected-let* ((owin (selected-window)
                                     (select-window owin)))
           (select-window window)
           (pcase (conn-bounds-of-dispatch thing arg pt)
             ((conn-dispatch-bounds `(,beg . ,end)
                                    (nconc dtform transform))
              (conn-dispatch-goto-char beg)
              (push-mark end t t))
             (_ (user-error "No %s found"
                            (conn-thing-pretty-print thing)))))))
     thing arg dtform
     :repeat repeat
     :other-end other-end
     :restrict-windows restrict-windows))
  (conn-push-state 'conn-mark-state))

(cl-defmethod conn-mark-thing-do ((_thing (conn-thing expansion))
                                  arg
                                  transform)
  (conn-dispatch-setup
   (conn-action ()
     (:description "Mark")
     (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                   (conn-select-target)))
       (conn-protected-let* ((owin (selected-window)
                                   (select-window owin)))
         (select-window window)
         (pcase (conn-bounds-of-dispatch thing arg pt)
           ((conn-dispatch-bounds `(,beg . ,end)
                                  (nconc dtform transform))
            (conn-dispatch-goto-char beg)
            (push-mark end t t))
           (_ (user-error "No %s found"
                          (conn-thing-pretty-print thing)))))))
   (conn-anonymous-thing
     '(expansion)
     :pretty-print ( :method (_) "expansion")
     :target-finder (:method (_self &rest _) (conn-expansion-targets)))
   arg nil
   :other-end :no-other-end)
  (conn-push-state 'conn-mark-state))

(defun conn-mark-thing (thing arg transform)
  "Mark the region defined by THING, ARG, and TRANSFORM"
  (interactive
   (conn-read-args (conn-mark-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-mark-thing-argument))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-mark-thing-do thing arg transform))

;;;;; Yank Replace

(conn-define-state conn-yank-replace-state (conn-read-thing-state)
  :lighter "YANK-REPLACE")

(defvar-keymap conn-swap-argument-map)

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
       (let ((inhibit-quit t))
         (advice-remove 'yank-pop ad-sym)
         (advice-remove 'conn-yank-unpop ad-sym)
         (advice-remove 'conn-yank-with-completion ad-sym)
         (undo-amalgamate-change-group cg)))
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

(cl-defmethod conn-yank-replace-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history
             'conn-yank-replace-do
             args))))

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
           (conn-yank-replace-subr beg end)))))))

(cl-defmethod conn-yank-replace-do ((thing (conn-thing region))
                                    arg
                                    transform
                                    &optional
                                    swap
                                    register
                                    check-bounds)
  (if (bound-and-true-p rectangle-mark-mode)
      (pcase (conn-bounds-of thing arg)
        ((conn-bounds-get :subregions
                          (append transform
                                  (when check-bounds
                                    (list 'conn-check-bounds))))
         (atomic-change-group
           (pcase-dolist ((conn-bounds `(,beg . ,end))
                          (compat-call sort subregions
                                       :key (lambda (b) (car (conn-bounds b)))
                                       :reverse t))
             (if register
                 (let ((str (cond (swap (filter-buffer-substring beg end t))
                                  (t (delete-region beg end)))))
                   (register-val-insert (get-register register))
                   (when swap (set-register register str)))
               (goto-char beg)
               (delete-region beg end)
               (yank))))))
    (cl-call-next-method)))

(conn-define-state conn-yank-replace-dispatch-state (conn-dispatch-bounds-state))

(cl-defmethod conn-yank-replace-do ((_thing (conn-thing dispatch))
                                    arg
                                    transform
                                    &optional
                                    swap
                                    register
                                    check-bounds)
  (conn-read-args (conn-yank-replace-dispatch-state
                   :prefix arg
                   :prompt "Yank and Replace"
                   :reference (list conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-dispatch-thing-argument t))
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
                               conn-dispatch-repeat-argument-map
                               :documentation conn-repeat-argument-documentation))
       (other-end (conn-boolean-argument
                   "other-end"
                   'other-end
                   conn-other-end-argument-map
                   :documentation conn-dispatch-other-end-documentation))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map
                               :documentation conn-this-win-argument-documentation))
       (stay
        (conn-boolean-argument "stay"
                               'stay
                               conn-stay-argument-map
                               :value t
                               :documentation conn-stay-argument-documentation)))
    (let ((str (if register
                   (get-register register)
                 (or read-from-kill-ring
                     (current-kill 0)))))
      (cl-assert (stringp str))
      (conn-with-dispatch-handlers
        (:handler
         ( :keymap conn-dispatch-stay-map)
         ( :predicate (cmd) (eq cmd 'stay))
         ( :update (_cmd break)
           (cl-callf not stay)
           (funcall break))
         ( :display ()
           (concat "\\[stay] "
                   (propertize
                    "stay"
                    'face (when stay
                            'eldoc-highlight-function-argument)))))
        (conn-dispatch-setup
         (conn-action ()
           (:window-predicate
            (lambda (win)
              (not
               (buffer-local-value 'buffer-read-only
                                   (window-buffer win)))))
           (:reference
            "Yank the the last killed text from the kill ring and replace the region
selected by dispatch with it.")
           (:description "Yank and Replace To")
           (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                         (conn-select-target)))
             (unless stay
               (select-window window))
             (with-selected-window window
               (conn-dispatch-change-group)
               (pcase (conn-bounds-of-dispatch thing arg pt)
                 ((conn-bounds `(,beg . ,end)
                               `(,@transform
                                 ,@(when check-bounds
                                     (list 'conn-check-bounds))))
                  (cond ((not stay)
                         (conn-dispatch-goto-char beg))
                        ((<= beg (point) end)
                         (conn-dispatch-goto-char beg 'nopush)))
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
  (conn-push-command-history
   'conn-dispatch-setup-previous
   (conn-previous-dispatch-copy
    (conn-ring-head conn-dispatch-ring))))

(cl-defmethod conn-yank-replace-do ((thing (conn-thing expansion))
                                    arg
                                    transform
                                    &optional
                                    _swap
                                    register
                                    check-bounds)
  (let (beg end)
    (conn-dispatch-setup
     (conn-action ()
       (:description "Yank and Replace To")
       (:window-predicate
        (lambda (win)
          (not
           (buffer-local-value 'buffer-read-only
                               (window-buffer win)))))
       (:reference
        "Yank the the last killed text from the kill ring and replace the region
selected by dispatch with it.")
       (pcase-let* ((`(,pt ,_window ,thing ,arg ,_dtform)
                     (conn-select-target)))
         (conn-dispatch-change-group)
         (pcase (conn-bounds-of-dispatch thing arg pt)
           ((conn-bounds `(,b . ,e) `(,@transform
                                      ,@(when check-bounds
                                          (list 'conn-check-bounds))))
            (conn-dispatch-goto-char b 'nopush)
            (setq beg b
                  end e))
           (_ (user-error "Cannot find thing at point")))))
     thing arg nil
     :other-end :no-other-end)
    (if register
        (progn
          (delete-region beg end)
          (insert-for-yank (get-register register)))
      (conn-yank-replace-subr beg end))
    (conn-dispatch-action-pulse
     beg (point)))
  (conn-push-command-history
   'conn-dispatch-setup-previous
   (conn-previous-dispatch-copy
    (conn-ring-head conn-dispatch-ring))))

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
                   (lambda (_) (register-read-with-preview "Register: "))
                   :formatter #'conn-argument-format-register))
        (check-bounds (conn-check-bounds-argument)))
     (list thing arg transform swap register check-bounds)))
  (conn-yank-replace-do thing
                        arg
                        transform
                        swap
                        register
                        check-bounds))

;;;;; Replace

(defvar conn-replace-special-ref
  (append
   (conn-reference-quote
     (("In project" project)
      ("In files" multi-file)))
   (static-if (<= 30 emacs-major-version)
       (conn-reference-quote
         (("As diff" as-diff)
          ("Multi file as diff" multi-file-as-diff)
          ("As diff in project" as-diff-in-project))))))

(defvar conn-replace-reference
  (list (conn-reference-page
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-replace-special-ref 2))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn--replace-bounds nil)

(conn-define-state conn-replace-state (conn-read-thing-state)
  :lighter "REPLACE")

(defvar-keymap conn-replace-thing-argument-map)

(cl-defstruct (conn-replace-thing-argument
               (:include conn-thing-with-subregions-argument)
               ( :constructor conn-replace-thing-argument
                 (&optional
                  subregions-default
                  &aux
                  (required t)
                  (keymap conn-replace-thing-argument-map)
                  (recursive-edit t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (subregions
                   (or (and (use-region-p)
                            (bound-and-true-p rectangle-mark-mode))
                       subregions-default))
                  (subregions-explicit-flag subregions-default)
                  (set-flag
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode)))))))

(conn-define-argument-command ((arg conn-replace-thing-argument)
                               (cmd (eql kapply)))
  "Kapply on matches.")

(defvar-keymap conn-replace-from-map)

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

(cl-defmethod conn-replace-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-replace-do args))))

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
          (let ((region-extract-function
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
  (conn-push-command-history 'conn-replace-do
                             thing
                             arg
                             transform
                             delimited
                             backward
                             regexp-flag
                             subregions-p
                             from
                             to))

(conn-define-argument-command ((arg conn-replace-thing-argument)
                               (cmd (eql project)))
  "Replace matches within the current project.")

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
  (conn-push-command-history 'conn-replace-do
                             thing
                             arg
                             transform
                             delimited
                             backward
                             regexp-flag
                             subregions-p
                             from
                             to))

(conn-define-argument-command ((arg conn-replace-thing-argument)
                               (cmd (eql multi-file)))
  "Replace matches within multiple files.")

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
    (conn-push-command-history 'conn-replace-do
                               thing
                               arg
                               transform
                               delimited
                               backward
                               regexp-flag
                               subregions-p
                               from
                               to)))

(cl-defmethod conn-replace-do ((_thing (conn-thing widen))
                               &rest _)
  (without-restriction
    (cl-call-next-method)))

(defvar conn-change-reference)

(conn-define-argument-command ((arg conn-replace-thing-argument)
                               (cmd (eql conn-emacs-state)))
  "Call `conn-change-thing'.")

(cl-defmethod conn-replace-do ((_thing (eql conn-emacs-state))
                               arg
                               transform
                               &rest _)
  (conn-read-args (conn-change-state
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

      (conn-define-argument-command ((arg conn-replace-thing-argument)
                                     (cmd (eql as-diff-in-project)))
        "Replaces matches as a diff in the current project.")

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

      (conn-define-argument-command ((arg conn-replace-thing-argument)
                                     (cmd (eql as-diff)))
        "Replace matches as a diff.")

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

      (conn-define-argument-command ((arg conn-replace-thing-argument)
                                     (cmd (eql multi-file-as-diff)))
        "Replace matches in multiple files as a diff.")))

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
       ((`(,thing ,arg ,subregions-p) (conn-replace-thing-argument))
        (transform (conn-transform-argument))
        (regexp-flag
         (conn-boolean-argument "regexp"
                                'regexp
                                conn-regexp-argument-map
                                :documentation conn-regexp-argument-documentation))
        (delimited
         (conn-boolean-argument "word delimited"
                                'delimited
                                conn-delimited-argument-map
                                :documentation conn-word-delimited-argument-documentation))
        (backward
         (conn-boolean-argument "backward"
                                'backward
                                conn-backward-argument-map
                                :documentation conn-backward-argument-documentation)))
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
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-isearch-special-ref 2))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-isearch-state (conn-read-thing-state)
  :lighter "ISEARCH-IN")

(defvar-keymap conn-isearch-thing-map)

(cl-defstruct (conn-isearch-thing-argument
               (:include conn-thing-with-subregions-argument)
               ( :constructor conn-isearch-thing-argument
                 (&aux
                  (keymap conn-isearch-thing-map)
                  (required t)
                  (recursive-edit t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (subregions
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode)))
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
         (thing (upcase (symbol-name thing)))
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
         ((`(,thing ,arg ,subregions) (conn-isearch-thing-argument))
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
                    :prompt "Isearch in Thing"
                    :reference conn-isearch-reference)
       ((`(,thing ,arg ,subregions) (conn-isearch-thing-argument))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument "regexp"
                                       'regexp
                                       conn-regexp-argument-map
                                       :documentation conn-regexp-argument-documentation)))
     (list thing arg transform regexp subregions)))
  (conn-isearch-in-thing-do thing
                            arg
                            transform
                            :backward nil
                            :regexp regexp
                            :subregions-p subregions-p)
  (conn-push-command-history 'conn-isearch-forward
                             thing
                             arg
                             transform
                             regexp
                             subregions-p))

(defun conn-isearch-backward (thing
                              arg
                              transform
                              &optional
                              regexp
                              subregions-p)
  "Isearch backward within the bounds of a thing."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Isearch in Thing"
                    :reference conn-isearch-reference)
       ((`(,thing ,arg ,subregions) (conn-isearch-thing-argument))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument
                 "regexp"
                 'regexp
                 conn-regexp-argument-map
                 :documentation conn-regexp-argument-documentation)))
     (list thing arg transform regexp subregions)))
  (conn-isearch-in-thing-do thing
                            arg
                            transform
                            :backward t
                            :regexp regexp
                            :subregions-p subregions-p)
  (conn-push-command-history 'conn-isearch-backward
                             thing
                             arg
                             transform
                             regexp
                             subregions-p))

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
     ((and (conn-bounds `(,beg . ,end))
           (conn-bounds-thing thing))
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
      (goto-char (if isearch-new-forward beg end))
      (when (eql thing 'region)
        (deactivate-mark))))))

;;;;; Transpose

(defvar conn-transpose-special-ref
  (conn-reference-quote
    (("line" conn-backward-line forward-line)
     ("symbol" forward-symbol))))

(defvar conn-transpose-reference
  (list (conn-reference-page
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-transpose-special-ref 3)))))

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE")

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-bounds-state))

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

(defvar-keymap conn-transpose-repeat-map)

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
     (let ((inhibit-quit t))
       (advice-remove 'conn-transpose-repeat repeat)
       (advice-remove 'conn-transpose-repeat-inverse repeat-inverse)))
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

(cl-defgeneric conn-transpose-things-do (thing arg at-point-and-mark)
  (declare (conn-anonymous-thing-property :transpose-op)))

(cl-defmethod conn-transpose-things-do ((thing (conn-thing t))
                                        arg
                                        at-point-and-mark)
  (pcase thing
    ((guard at-point-and-mark)
     (deactivate-mark)
     (pcase-let* (((conn-bounds `(,beg1 . ,end1))
                   (conn-bounds-of thing arg))
                  ((conn-bounds `(,beg2 . ,end2))
                   (save-excursion
                     (goto-char (mark t))
                     (conn-bounds-of thing arg))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((guard (use-region-p))
     (deactivate-mark)
     (pcase-let ((beg1 (region-beginning))
                 (end1 (region-end))
                 ((conn-bounds `(,beg2 . ,end2))
                  (conn-bounds-of thing arg)))
       (transpose-regions beg1 end1 beg2 end2)))
    ((and (let (and thing (pred identity))
            (cl-loop for th in (conn-thing-all-parents thing)
                     when (conn-simple-thing-p th) return th))
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

(cl-defmethod conn-transpose-things-do ((_thing (conn-thing expansion))
                                        _arg
                                        _at-point-and-mark)
  (user-error "Invalid transpose thing"))

(cl-defmethod conn-transpose-things-do ((thing (conn-thing isearch))
                                        arg
                                        _at-point-and-mark)
  (pcase-let* ((bounds (conn-bounds-of thing arg))
               ((conn-bounds `(,beg1 . ,end1))
                bounds)
               ((conn-bounds `(,beg2 . ,end2))
                (conn-bounds-of (conn-bounds-thing bounds)
                                (conn-bounds-arg bounds))))
    (transpose-regions beg1 end1 beg2 end2)))

(cl-defmethod conn-transpose-things-do ((_thing (conn-thing region))
                                        _arg
                                        _at-point-and-mark)
  (deactivate-mark)
  (pulse-momentary-highlight-region (region-beginning) (region-end))
  (let* ((bounds1 (conn-bounds-of 'region nil))
         (bounds2 (conn-bounds-of 'recursive-edit nil)))
    (conn--dispatch-transpose-subr bounds1 bounds2)))

(conn-define-state conn-transpose-dispatch-state (conn-dispatch-bounds-state))

(cl-defmethod conn-transpose-things-do ((_thing (conn-thing dispatch))
                                        arg
                                        _at-point-and-mark)
  (let ((pt1 (point))
        (buf1 (current-buffer))
        (thing1 (when (use-region-p)
                  (conn-anonymous-thing
                    '(region)
                    :bounds-op (let ((bounds (conn-make-bounds
                                              'region nil
                                              (cons (region-beginning)
                                                    (region-end)))))
                                 (:method (_self _arg) bounds))))))
    (conn-read-args (conn-transpose-dispatch-state
                     :prompt "Transpose Dispatch"
                     :prefix arg
                     :reference (list conn-dispatch-thing-reference))
        ((`(,thing ,arg) (conn-dispatch-thing-argument t))
         (restrict-windows
          (conn-boolean-argument "this-win"
                                 'restrict-windows
                                 conn-restrict-windows-argument-map
                                 :documentation conn-this-win-argument-documentation)))
      (deactivate-mark)
      (save-excursion
        (conn-dispatch-setup
         (conn-action ()
           (:description "Transpose")
           (:no-history t)
           (:window-predicate
            (lambda (win)
              (not (buffer-local-value 'buffer-read-only
                                       (window-buffer win)))))
           (pcase-let* ((`(,pt2 ,window2 ,thing2 ,arg2 ,_transform)
                         (conn-select-target)))
             (conn--dispatch-transpose-subr
              (with-current-buffer (window-buffer window2)
                (conn-bounds-of-dispatch thing2 arg2 pt2))
              (with-current-buffer buf1
                (save-excursion
                  (goto-char pt1)
                  (conn-bounds-of (or thing1 thing2) arg2))))))
         thing arg nil
         :other-end :no-other-end
         :restrict-windows restrict-windows)))))

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

(defvar-keymap conn-transpose-point-and-mark-argument-map)

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
                    :prompt "Transpose"
                    :prefix current-prefix-arg
                    :reference conn-transpose-reference)
       ((`(,thing ,arg) (conn-transpose-thing-argument t))
        (at-point-and-mark (conn-boolean-argument
                            "transpose at point and mark"
                            'transpose-at-point-and-mark
                            conn-transpose-point-and-mark-argument-map
                            :documentation "Transpose the things at the point and mark.")))
     (list thing arg at-point-and-mark)))
  (conn-transpose-things-do thing arg at-point-and-mark)
  (conn-push-command-history 'conn-transpose-things
                             thing
                             arg
                             at-point-and-mark))

;;;;; Kill

(defvar conn-kill-special-ref
  (conn-reference-quote
    (("copy filename" buffer-filename)
     ("kill matching lines" kill-matching-lines)
     ("keep matching lines" keep-lines)
     ("outer line" move-end-of-line))))

(defvar conn-kill-reference
  (list (conn-reference-page
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-kill-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn-kill-reformat-default t)

(conn-define-state conn-kill-state (conn-read-thing-state)
  :lighter "KILL")

(defvar-keymap conn-kill-append-argument-map)

(cl-defstruct (conn-kill-append-argument
               (:include conn-cycling-argument)
               ( :constructor conn-kill-append-argument
                 (&key
                  value
                  (formatter #'conn-format-cycling-argument)
                  required
                  annotation
                  reference
                  display-prefix
                  &aux
                  (name "append")
                  (choices '(nil append prepend repeat))
                  (cycling-commands '(append append-on-repeat))
                  (keymap conn-kill-append-argument-map)))))

(cl-defmethod conn-argument-update ((arg conn-kill-append-argument)
                                    cmd
                                    break)
  (pcase cmd
    ('append-on-repeat
     (conn-<f (conn-cycling-argument-value arg)
       (eq 'repeat)
       not
       (and 'repeat))
     (funcall break))
    (_ (cl-call-next-method))))

(conn-define-argument-command ((arg conn-kill-append-argument)
                               (cmd (eql append-on-repeat)))
  "Append each kill after the first.")

(defvar-keymap conn-delete-argument-map)

(cl-defstruct (conn-kill-how-argument
               (:include conn-composite-argument)
               ( :constructor conn--kill-how-argument
                 (delete
                  append
                  register
                  separator
                  &aux
                  (value (list delete append register separator)))))
  (delete nil)
  (append nil)
  (register nil)
  (separator nil))

(cl-defsubst conn-kill-how-argument (&key delete
                                          append
                                          register
                                          separator)
  (declare (important-return-value t)
           (side-effect-free t))
  (cl-assert (memq append '(nil append prepend repeat)))
  (cl-assert (not (and delete (or append register))))
  (conn--kill-how-argument
   (conn-boolean-argument "delete"
                          'delete
                          conn-delete-argument-map
                          :value delete
                          :documentation "Delete the thing without added it the the kill-ring.")
   (conn-kill-append-argument :value append)
   (conn-read-argument "register"
                       'register
                       conn-register-argument-map
                       (lambda (_) (register-read-with-preview "Register: "))
                       :formatter #'conn-argument-format-register
                       :value register)
   (conn-separator-argument separator)))

(cl-defmethod conn-argument-update ((arg conn-kill-how-argument)
                                    cmd
                                    _break)
  (cl-symbol-macrolet ((delete (conn-argument-value
                                (conn-kill-how-argument-delete arg)))
                       (register (conn-argument-value
                                  (conn-kill-how-argument-register arg)))
                       (append (conn-argument-value
                                (conn-kill-how-argument-append arg)))
                       (separator (conn-argument-value
                                   (conn-kill-how-argument-separator arg))))
    (pcase cmd
      ((or 'append 'append-on-repeat)
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
      ('separator
       (cl-call-next-method)))))

(cl-defmethod conn-argument-display ((arg conn-kill-how-argument))
  (cl-symbol-macrolet ((delete (conn-kill-how-argument-delete arg))
                       (register (conn-kill-how-argument-register arg))
                       (append (conn-kill-how-argument-append arg))
                       (separator (conn-kill-how-argument-separator arg)))
    (mapcar #'conn-argument-display
            (list append
                  (when (or (conn-argument-value append)
                            (conn-argument-value separator))
                    separator)
                  register
                  delete))))

(cl-defstruct (conn-kill-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-kill-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (required t)))))

(conn-define-argument-command ((arg conn-kill-thing-argument)
                               (cmd (eql buffer-filename)))
  "Copy buffer filename.")

(conn-define-argument-command ((arg conn-kill-thing-argument)
                               (cmd (eql project-filename)))
  "Copy project filename.")

(conn-define-argument-command ((arg conn-kill-thing-argument)
                               (cmd (eql keep-lines)))
  "Call `keep-lines'.")

(conn-define-argument-command ((arg conn-kill-thing-argument)
                               (cmd (eql kill-matching-lines)))
  "Kill matching lines.")

(conn-define-argument-command ((arg conn-kill-thing-argument)
                               (cmd (eql copy)))
  "Call `conn-copy-thing'.")

(cl-defstruct (conn-transform-and-fixup-argument
               (:include conn-composite-argument)
               ( :constructor conn-transform-and-fixup-argument
                 (&optional
                  initial-reformat
                  &aux
                  (transform (conn-transform-argument))
                  (reformat (when conn-kill-reformat-function
                              (conn-reformat-argument initial-reformat)))
                  (value (list transform reformat))))
               ( :constructor conn-dispatch-transform-and-fixup-argument
                 (&optional
                  initial-reformat
                  &aux
                  (transform (conn-dispatch-transform-argument))
                  (reformat (when conn-kill-reformat-function
                              (conn-reformat-argument initial-reformat)))
                  (value (list transform reformat)))))
  (transform nil)
  (reformat nil)
  (explicit nil :type boolean))

(cl-defmethod conn-argument-update ((arg conn-transform-and-fixup-argument)
                                    cmd
                                    break)
  (cl-symbol-macrolet ((tform (conn-transform-and-fixup-argument-transform arg))
                       (fws (conn-transform-and-fixup-argument-reformat arg)))
    (let ((valid nil))
      (cond ((progn
               (conn-argument-update tform cmd (lambda () (setq valid t)))
               valid)
             (unless (conn-transform-and-fixup-argument-explicit arg)
               (setf (conn-reformat-argument-value fws)
                     (cl-loop for tf in (conn-transform-argument-value tform)
                              never (plist-get
                                     (function-get tf :conn-transform-properties)
                                     :no-reformat))))
             (funcall break))
            ((progn
               (conn-argument-update fws cmd (lambda () (setq valid t)))
               valid)
             (setf (conn-transform-and-fixup-argument-explicit arg) t)
             (funcall break))))))

(defun conn-kill-thing (thing
                        arg
                        transform
                        &optional
                        append
                        delete
                        register
                        separator
                        reformat
                        check-bounds
                        repeat-count)
  "Kill a region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
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
                    :prompt "Thing"
                    :reference conn-kill-reference
                    :display-handler (conn-read-args-display-columns 5 3))
       ((`(,thing ,arg) (conn-kill-thing-argument t))
        (`(,transform ,reformat) (conn-transform-and-fixup-argument))
        (`(,delete ,append ,register ,separator) (conn-kill-how-argument))
        (check-bounds (conn-check-bounds-argument)))
     (list thing arg transform append delete
           register separator reformat check-bounds
           current-prefix-arg)))
  (cl-callf prefix-numeric-value repeat-count)
  (cl-assert (not (and delete (or register append))))
  (let ((last-command last-command)
        (conn-repeating-command conn-repeating-command))
    (dotimes (_ (abs repeat-count))
      (conn-kill-thing-do thing
                          arg
                          transform
                          append
                          delete
                          register
                          separator
                          reformat
                          check-bounds)
      (setq this-command 'conn-kill-thing
            last-command 'conn-kill-thing
            conn-repeating-command t))))

(cl-defgeneric conn-kill-reformat (bounds)
  (declare (conn-anonymous-thing-property :kill-reformat)))

(cl-defmethod conn-kill-reformat :around (bounds)
  (unless (conn-thing-get (conn-bounds-thing bounds)
                          :no-reformat)
    (cl-call-next-method)))

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
  (cond ((conn-thing-get bounds :linewise))
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
    (cond ((and (conn-thing-get bounds :linewise)
                (save-excursion
                  (beginning-of-line)
                  (looking-at-p (rx eol))))
           (dotimes (_ (min (empty-lines) (empty-lines t)))
             (join-line)))
          ((and (bolp)
                (looking-at-p (rx (seq (* (syntax whitespace)) eol)))
                (not (or (eobp) (bobp))))
           (join-line)))))

(defun conn--kill-region (beg
                          end
                          &optional
                          delete-flag
                          append
                          register
                          separator)
  (pcase append
    ('repeat
     (setq append (and conn-repeating-command 'append)))
    ((and 'nil
          (guard (and conn-repeating-command
                      (eq last-command this-command))))
     (setq append (if (<= end (point)) 'prepend 'append)
           separator (or separator t))))
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
  (pcase append
    ('repeat
     (setq append (and conn-repeating-command 'append)))
    ((and 'nil
          (guard (and conn-repeating-command
                      (eq last-command this-command))))
     (setq append 'append
           separator (or separator t))))
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

(cl-defgeneric conn-kill-thing-do (thing
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

(cl-defmethod conn-kill-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-kill-thing args))))

(cl-defmethod conn-kill-thing-do ((thing (conn-thing conn-things-in-region))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (pcase (conn-bounds-of thing arg)
    ((and (conn-bounds-get :subregions
                           `(,@transform
                             ,@(when check-bounds
                                 (list 'conn-check-bounds))))
          (conn-bounds `(,beg . ,end))
          bounds)
     (unless delete
       (let ((strings nil))
         (pcase-dolist ((conn-bounds `(,beg . ,end)) subregions)
           (push (filter-buffer-substring beg end) strings))
         (conn--kill-string
          (string-join (nreverse strings)
                       (conn-kill-separator-for-strings
                        strings
                        (or separator 'default)))
          append
          register
          separator)))
     (delete-region beg end)
     (when reformat
       (goto-char beg)
       (funcall conn-kill-reformat-function bounds)))
    (_ (user-error "No thing found"))))

(cl-defmethod conn-kill-thing-do ((_thing (eql buffer-filename))
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

(cl-defmethod conn-kill-thing-do ((_thing (eql project-filename))
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

(cl-defmethod conn-kill-thing-do ((_thing (eql kill-matching-lines))
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

(cl-defmethod conn-kill-thing-do ((_thing (eql keep-lines))
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
        beg end t))
      (_ (user-error "No thing found")))))

(defvar-keymap conn-kill-dispatch-append-map)

(conn-define-state conn-kill-dispatch-state (conn-dispatch-bounds-state))

(cl-defmethod conn-kill-thing-do ((_thing (conn-thing dispatch))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (let ((result nil)
        (strings nil))
    (with-undo-amalgamate
      (conn-read-args (conn-kill-dispatch-state
                       :prefix arg
                       :prompt "Kill"
                       :reference (list conn-dispatch-thing-reference)
                       :display-handler (conn-read-args-display-columns 3 3))
          ((`(,thing ,arg) (conn-dispatch-thing-argument t))
           (repeat
            (conn-boolean-argument "repeat"
                                   'repeat-dispatch
                                   conn-dispatch-repeat-argument-map
                                   :documentation conn-repeat-argument-documentation))
           (`(,delete ,append ,register ,separator)
            (conn-kill-how-argument
             :append (if (eq append 'repeat) nil append)
             :delete delete
             :register register
             :separator separator))
           (other-end
            (conn-boolean-argument "other-end"
                                   'other-end
                                   conn-other-end-argument-map
                                   :documentation conn-dispatch-other-end-documentation))
           (restrict-windows
            (conn-boolean-argument "this-win"
                                   'restrict-windows
                                   conn-restrict-windows-argument-map
                                   :documentation conn-this-win-argument-documentation))
           (stay
            (conn-boolean-argument "stay"
                                   'stay
                                   conn-stay-argument-map
                                   :value t
                                   :documentation conn-stay-argument-documentation))
           (`(,dtform ,reformat)
            (conn-dispatch-transform-and-fixup-argument reformat)))
        (conn-with-dispatch-handlers
          (:handler
           (:predicate (cmd) (eq cmd 'append))
           ( :keymap conn-kill-dispatch-append-map)
           ( :update (_cmd break)
             (setq append (pcase append
                            ('nil 'append)
                            ('prepend nil)
                            (_ 'prepend)))
             (funcall break))
           ( :display ()
             (concat
              "\\[append] "
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
          (:handler
           (:predicate (cmd) (eq cmd 'stay))
           ( :keymap conn-dispatch-stay-map)
           ( :display ()
             (concat
              "\\[stay] "
              (propertize "stay"
                          'face (when stay
                                  'eldoc-highlight-function-argument))))
           ( :update (_cmd break)
             (cl-callf not stay)
             (funcall break)))
          (conn-dispatch-setup
           (conn-action ()
             (:description "Kill")
             (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                           (conn-select-target)))
               (unless stay
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
                    (cond ((not stay)
                           (push-mark (conn-bounds-get bounds :origin))
                           (conn-dispatch-goto-char beg))
                          ((<= beg (point) end)
                           (conn-dispatch-goto-char beg 'nopush)))
                    (unless delete
                      (push (cons append (filter-buffer-substring beg end))
                            strings))
                    (save-excursion
                      (goto-char beg)
                      (delete-region beg end)
                      (when reformat
                        (funcall conn-kill-reformat-function bounds)))
                    (conn-dispatch-undo-case
                      :depth 90
                      (:undo
                       (pop strings)
                       (conn-dispatch-undo-pulse beg end))
                      (:cancel
                       (pop strings))))
                   (_ (user-error "No %s found"
                                  (conn-thing-pretty-print thing)))))))
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
            (conn--kill-string result append register sep)))
        (conn-push-command-history
         'conn-dispatch-setup-previous
         (conn-previous-dispatch-copy
          (conn-ring-head conn-dispatch-ring)))))))

(cl-defmethod conn-kill-thing-do ((thing (conn-thing expansion))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (conn-with-dispatch-handlers
    (:handler
     (:predicate (cmd) (eq cmd 'append))
     ( :keymap conn-kill-dispatch-append-map)
     ( :update (_cmd break)
       (setq append (pcase append
                      ('nil 'append)
                      ('prepend nil)
                      (_ 'prepend)))
       (funcall break))
     ( :display ()
       (concat
        "\\[append] "
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
     (conn-action ()
       (:description "Kill")
       (:repeat :not-repeatable)
       (pcase-let* ((`(,pt ,_window ,thing ,arg ,_dtform)
                     (conn-select-target)))
         (conn-dispatch-change-group)
         (pcase (conn-bounds-of-dispatch thing arg pt)
           ((and bounds
                 (conn-dispatch-bounds `(,beg . ,end)
                                       `(,@transform
                                         ,@(when check-bounds
                                             (list 'conn-check-bounds)))))
            (conn-dispatch-goto-char beg 'nopush)
            (if delete
                (delete-region beg end)
              (conn--kill-region beg end t append register separator))
            (when reformat
              (funcall conn-kill-reformat-function bounds)))
           (_ (user-error "No %s found"
                          (conn-thing-pretty-print thing))))))
     thing arg nil
     :other-end :no-other-end)
    (conn-push-command-history
     'conn-dispatch-setup-previous
     (conn-previous-dispatch-copy
      (conn-ring-head conn-dispatch-ring)))))

(cl-defmethod conn-kill-thing-do (thing
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  separator
                                  reformat
                                  check-bounds)
  (pcase (conn-bounds-of thing arg)
    ((and (conn-bounds `(,beg . ,end)
                       `(,@transform
                         ,@(when check-bounds
                             (list 'conn-check-bounds))))
          bounds)
     (if delete
         (delete-region beg end)
       (conn--kill-region beg end t append register
                          (or separator (when transform 'no))))
     (goto-char beg)
     (when reformat
       (funcall conn-kill-reformat-function bounds)))
    (_ (user-error "No thing found"))))

(cl-defmethod conn-kill-thing-do ((_thing (conn-thing line))
                                  &rest _)
  (let ((col (current-column)))
    (cl-call-next-method)
    (move-to-column col)))

(cl-defmethod conn-kill-thing-do ((_thing (eql copy))
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

(cl-defmethod conn-kill-thing-do :extra "rmm" ((_thing (conn-thing region))
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
    (("copy filename" buffer-filename)
     ("kill matching lines" copy-matching-lines))))

(defvar conn-copy-reference
  (list (conn-reference-page
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-copy-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-copy-state (conn-read-thing-state)
  :lighter "COPY")

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
  (conn--copy-how-argument
   (conn-kill-append-argument :value append)
   (conn-read-argument "register"
                       'register
                       conn-register-argument-map
                       (lambda (_) (register-read-with-preview "Register: "))
                       :formatter #'conn-argument-format-register
                       :value register)
   (conn-separator-argument separator)))

(cl-defmethod conn-argument-display ((arg conn-copy-how-argument))
  (cl-symbol-macrolet ((separator (conn-copy-how-argument-separator arg))
                       (register (conn-copy-how-argument-register arg))
                       (append (conn-copy-how-argument-append arg)))
    (list (conn-argument-display append)
          (when (or (conn-argument-value append)
                    (conn-argument-value separator))
            (conn-argument-display separator))
          (conn-argument-display register))))

(defvar-keymap conn-copy-thing-argument-map)

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

(conn-define-argument-command ((arg conn-copy-thing-argument)
                               (cmd (eql buffer-filename)))
  "Copy buffer filename.")

(conn-define-argument-command ((arg conn-copy-thing-argument)
                               (cmd (eql project-filename)))
  "Copy project filename.")

(conn-define-argument-command ((arg conn-copy-thing-argument)
                               (cmd (eql copy-matching-lines)))
  "Copy matching lines.")

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

(cl-defgeneric conn-copy-thing-do (thing
                                   arg
                                   &optional
                                   transform
                                   append
                                   register
                                   separator)
  (declare (conn-anonymous-thing-property :copy-op)))

(cl-defmethod conn-copy-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-copy-thing args))))

(cl-defmethod conn-copy-thing-do (thing
                                  arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn--kill-region beg end nil append register separator)
     (unless executing-kbd-macro
       (pulse-momentary-highlight-region beg end)))
    (_ (user-error "No thing found"))))

(cl-defmethod conn-copy-thing-do ((thing (conn-thing conn-things-in-region))
                                  arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds-get :subregions transform)
     (let ((strings nil))
       (pcase-dolist ((conn-bounds `(,beg . ,end)) subregions)
         (push (filter-buffer-substring beg end) strings))
       (conn--kill-string
        (string-join (nreverse strings)
                     (conn-kill-separator-for-strings
                      strings
                      (or separator 'default)))
        append
        register
        separator)))
    (_ (user-error "No thing found"))))

(cl-defmethod conn-copy-thing-do ((_thing (eql copy-matching-lines))
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
        beg end t))
      (_ (user-error "No thing found")))))

(cl-defmethod conn-copy-thing-do ((_thing (eql buffer-filename))
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

(cl-defmethod conn-copy-thing-do ((_thing (eql project-filename))
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

(conn-define-state conn-copy-dispatch-state (conn-dispatch-bounds-state))

(cl-defmethod conn-copy-thing-do ((_thing (conn-thing dispatch))
                                  arg
                                  &optional
                                  transform
                                  append
                                  register
                                  separator)
  (conn-read-args (conn-copy-dispatch-state
                   :prefix arg
                   :prompt "Copy"
                   :reference (list conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-dispatch-thing-argument t))
       (transform (conn-dispatch-transform-argument transform))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               :documentation conn-repeat-argument-documentation))
       (`(,append ,register ,separator)
        (conn-copy-how-argument
         :append (if (eq append 'repeat) nil append)
         :register register
         :separator separator))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map
                               :documentation conn-this-win-argument-documentation)))
    (conn-with-dispatch-handlers
      (:handler
       (:keymap (define-keymap "<remap> <other-end>" 'copy-append))
       (:predicate (cmd) (eq cmd 'copy-append))
       ( :update (_cmd break)
         (setq append (pcase append
                        ('nil 'append)
                        ('prepend nil)
                        (_ 'prepend)))
         (funcall break))
       ( :display ()
         (concat "\\[copy-append] "
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
         (conn-action ()
           (:description "Copy")
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
                   (_ (user-error "No %s found"
                                  (conn-thing-pretty-print thing))))))))
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
            (conn--kill-string result append register sep)))
        (conn-push-command-history
         'conn-dispatch-setup-previous
         (conn-previous-dispatch-copy
          (conn-ring-head conn-dispatch-ring)))))))

;;;;; How Many

(defvar conn-how-many-special-ref nil)

(defvar conn-how-many-reference
  (list (conn-reference-page
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
                    :reference conn-how-many-reference
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-how-many-in-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-how-many-in-thing-do thing arg transform))

(cl-defgeneric conn-how-many-in-thing-do (thing arg transform)
  (declare (conn-anonymous-thing-property :how-many-op)))

(cl-defmethod conn-how-many-in-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-how-many-in-thing args))))

(cl-defmethod conn-how-many-in-thing-do ((thing (conn-thing t))
                                         arg
                                         transform)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds (and region `(,beg . ,end)) transform)
     (how-many
      (conn-read-regexp "How many matches for regexp" region)
      beg end t))
    (_ (user-error "No thing found"))))

;;;;; Comment

(defvar conn-comment-special-ref nil)

(defvar conn-comment-reference
  (list (conn-reference-page
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

(cl-defmethod conn-comment-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-comment-thing args))))

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
     ("Delete previous duplicate" conn-duplicate-repeat-delete)
     ("Indent each duplicate" conn-duplicate-repeat-indent)
     ("Toggle newline padding" conn-duplicate-repeat-toggle-padding)
     ("Comment or uncomment each duplicate" conn-duplicate-repeat-comment)
     ("Apply a keyboard macro at each duplicate.
With a prefix argument include the original."
      conn-duplicate-repeat-kapply)
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

(defvar-keymap conn-duplicate-repeat-map)
(defvar-keymap conn-duplicate-repeat-padding-map)

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

(defun conn-duplicate-repeat-delete (arg)
  "Delete the previous ARG duplicates.

Only available during repeating duplicate."
  (interactive "p")
  (ignore arg)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-indent ()
  "Indent each duplicate.

Only available during repeating duplicate."
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-kapply ()
  "Kapply on each duplicate"
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn--duplicate-message-string (keymap &rest desc-and-commands)
  (cl-macrolet ((key-desc (command)
                  `(propertize
                    (key-description
                     (where-is-internal ,command (list keymap) 'first-only))
                    'face 'help-key-binding)))
    (conn--with-work-buffer
      (insert (format "Repeat duplicate (%s reference):\n"
                      (key-desc 'conn-duplicate-repeat-help)))
      (conn-to-vtable
       (cl-loop for (d c) on desc-and-commands by #'cddr
                collect (format "%s %s" (key-desc c) d))
       4 (current-buffer)
       :separator-width 3
       :use-header-line nil)
      (buffer-substring (point-min) (1- (point-max))))))

(defun conn-duplicate-subr (beg end &optional repeat no-padding)
  "Duplicate the region from BEG to END REPEAT times."
  (unless repeat (setq repeat 1))
  (deactivate-mark)
  (conn-protected-let*
      ((keymap (if no-padding
                   conn-duplicate-repeat-map
                 conn-duplicate-repeat-padding-map))
       (regions (list (make-overlay beg end nil t))
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
       (commented nil)
       (exit-fn nil))
    (set-marker-insertion-type m2 t)
    (cl-labels
        ((dup ()
           (save-excursion
             (goto-char end-marker)
             (unless (or no-padding
                         (looking-back regexp 1))
               (insert padding))
             (when extra-newline
               (if block
                   (insert "\n")
                 (unless (looking-back "\n" 1)
                   (insert "\n"))))
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
           (let ((inhibit-quit t))
             (set-marker end-marker nil)
             (set-marker m1 nil)
             (set-marker m2 nil)
             (mapc #'delete-overlay regions)
             (conn--remove-all-advice 'conn-duplicate-repeat-indent
                                      'conn-duplicate-repeat-kapply
                                      'conn-duplicate-repeat
                                      'conn-duplicate-repeat-delete
                                      'conn-duplicate-repeat-comment
                                      'conn-duplicate-repeat-toggle-padding)
             (undo-amalgamate-change-group cg)))
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
                             (unless (or no-padding
                                         (looking-back regexp 1))
                               (insert padding))))))
           (setq extra-newline (not extra-newline)
                 padding (if extra-newline "\n" " ")
                 regexp (if extra-newline "\n" "[\t ]")))
         (kapply (all)
           (interactive "P")
           (let ((kapply-regions nil)
                 (offset 0))
             (dolist (ov (if all regions (butlast regions)))
               (when (<= (overlay-start ov) (point) (overlay-end ov))
                 (setq offset (- (point) (overlay-start ov))))
               (push (conn-kapply-make-region (overlay-start ov)
                                              (overlay-end ov))
                     kapply-regions))
             (funcall exit-fn)
             (conn-kapply-on-iterator
              (conn-kapply-region-iterator kapply-regions)
              :extra (lambda (it)
                       (conn-kapply-relocate-to-region it offset)))))
         (pred ()
           (pcase this-command
             ((or 'recenter-top-bottom 'reposition-window
                  'universal-argument 'digit-argument 'negative-argument)
              t)
             ((let mc (lookup-key keymap (this-command-keys-vector)))
              (when (and mc (symbolp mc))
                (setq mc (or (command-remapping mc) mc)))
              (and mc (eq this-command mc))))))
      (when (> repeat 0) (push-mark nil nil))
      (undo-boundary)
      (dotimes (_ repeat) (dup))
      (goto-char (+ offset (overlay-start (car regions))))
      (if (and conn-dispatch-in-progress
               conn-dispatch-repeating)
          (cleanup)
        (advice-add 'conn-duplicate-repeat :override #'repeat)
        (advice-add 'conn-duplicate-repeat-delete :override #'delete)
        (advice-add 'conn-duplicate-repeat-kapply :override #'kapply)
        (unless no-padding
          (advice-add 'conn-duplicate-repeat-indent :override #'indent)
          (advice-add 'conn-duplicate-repeat-comment :override #'comment)
          (advice-add 'conn-duplicate-repeat-toggle-padding :override
                      (if block #'block-padding #'non-block-padding)))
        (setq exit-fn
              (set-transient-map
               keymap
               #'pred
               #'cleanup
               (if no-padding
                   (conn--duplicate-message-string
                    keymap
                    "repeat" 'conn-duplicate-repeat
                    "delete" 'conn-duplicate-repeat-delete
                    "kapply" 'conn-duplicate-repeat-kapply)
                 (conn--duplicate-message-string
                  keymap
                  "repeat" 'conn-duplicate-repeat
                  "indent" 'conn-duplicate-repeat-indent
                  "newline" 'conn-duplicate-repeat-toggle-padding
                  "comment" 'conn-duplicate-repeat-comment
                  "delete" 'conn-duplicate-repeat-delete
                  "kapply" 'conn-duplicate-repeat-kapply))))))))

(cl-defgeneric conn-duplicate-thing-do (thing
                                        arg
                                        transform
                                        &optional
                                        repeat)
  (declare (conn-anonymous-thing-property :duplicate-op)))

(cl-defmethod conn-duplicate-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-duplicate-thing args))))

(cl-defmethod conn-duplicate-thing-do (thing
                                       arg
                                       transform
                                       &optional
                                       repeat)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn-duplicate-subr beg end repeat (conn-subthing-p thing 'region)))))

(cl-defmethod conn-duplicate-thing-do ((_thing (eql copy-from-above-command))
                                       arg
                                       _transform
                                       &optional
                                       _repeat)
  (copy-from-above-command arg))

(cl-defmethod conn-duplicate-thing-do ((_thing (conn-thing region))
                                       _arg
                                       _transform
                                       &optional
                                       repeat)
  (if (and (bound-and-true-p rectangle-mark-mode)
           (fboundp 'rectangle--duplicate-right))
      (let ((cgs (list (prepare-change-group)))
            (exit-fn nil))
        (cl-flet
            ((dup ()
               (interactive)
               (let ((inhibit-message t))
                 (push (prepare-change-group) cgs)
                 (rectangle--duplicate-right 1 0)))
             (delete ()
               (interactive)
               (when (cdr cgs)
                 (cancel-change-group (pop cgs))))
             (kapply ()
               (funcall exit-fn)
               (activate-mark)
               (rectangle-mark-mode 1)
               (conn-kapply-on-things-do 'region nil nil))
             (pred ()
               (pcase this-command
                 ((or 'recenter-top-bottom 'reposition-window
                      'universal-argument 'digit-argument 'negative-argument)
                  t)
                 ((let mc (lookup-key conn-duplicate-repeat-map
                                      (this-command-keys-vector)))
                  (when (and mc (symbolp mc))
                    (setq mc (or (command-remapping mc) mc)))
                  (and mc (eq this-command mc)))))
             (cleanup ()
               (let ((inhibit-quit t))
                 (conn--remove-all-advice 'conn-duplicate-repeat
                                          'conn-duplicate-repeat-delete
                                          'conn-duplicate-repeat-kapply)
                 (dolist (cg cgs)
                   (accept-change-group cg))
                 (undo-amalgamate-change-group (car (last cgs))))))
          (advice-add 'conn-duplicate-repeat :override #'dup)
          (advice-add 'conn-duplicate-repeat-delete :override #'delete)
          (advice-add 'conn-duplicate-repeat-kapply :override #'kapply)
          (undo-boundary)
          (rectangle--duplicate-right repeat 1)
          (setq exit-fn
                (set-transient-map
                 conn-duplicate-repeat-map
                 #'pred
                 #'cleanup
                 (conn--duplicate-message-string
                  conn-duplicate-repeat-map
                  "repeat" 'conn-duplicate-repeat
                  "indent" 'conn-duplicate-repeat-delete
                  "kapply" 'conn-duplicate-repeat-kapply)))))
    (cl-call-next-method)))

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

(conn-define-argument-command ((arg conn-duplicate-thing-argument)
                               (cmd (eql copy-from-above-command)))
  "Call `copy-from-above-command'.")

(defun conn-duplicate-thing (thing arg transform &optional repeat)
  "Duplicate the region defined by THING, ARG, and TRANSFORM.

For how they are used to define the region see `conn-bounds-of' and
`conn-transform-bounds'.

If REPEAT is non-nil then duplicate the region REPEAT times.
Interactively REPEAT is given by the prefix argument."
  (interactive
   (conn-read-args (conn-duplicate-state
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
     ("emacs-state-binary-overwrite" conn-emacs-state-overwrite-binary))))

(defvar conn-change-reference
  (list (conn-reference-page
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-change-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-change-state (conn-kill-state)
  :lighter "CHANGE")

(defvar-keymap conn-change-thing-argument-map)

(cl-defstruct (conn-change-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-change-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-change-thing-argument-map)
                  (required t)
                  (value (when (and (use-region-p)
                                    (not (or defining-kbd-macro
                                             executing-kbd-macro)))
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 (not (or defining-kbd-macro
                                          executing-kbd-macro))))))))

(conn-define-argument-command ((arg conn-change-thing-argument)
                               (cmd (eql conn-emacs-state-overwrite-binary)))
  "Enter emacs state and `overwrite-mode-binary'.")

(conn-define-argument-command ((arg conn-change-thing-argument)
                               (cmd (eql conn-emacs-state-overwrite)))
  "Enter emacs state and `overwrite-mode'.")

(conn-define-argument-command ((arg conn-change-thing-argument)
                               (cmd (eql conn-replace)))
  "Call `conn-replace-thing'.")

(conn-define-argument-command ((arg conn-change-thing-argument)
                               (cmd (eql yank-replace)))
  "Call `conn-yank-replace'.")

(conn-define-argument-command ((arg conn-change-thing-argument)
                               (cmd (eql conn-emacs-state-record-insert)))
  "Enter emacs state with a record region.")

(cl-defgeneric conn-change-thing-do (thing
                                     arg
                                     transform
                                     &optional
                                     check-bounds
                                     with
                                     kbd-macro-query)
  (declare (conn-anonymous-thing-property :change-op)))

(cl-defmethod conn-change-thing-do ((thing (conn-thing t))
                                    arg
                                    transform
                                    &optional
                                    check-bounds
                                    with
                                    kbd-macro-query)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end)
                  (if check-bounds
                      (append transform
                              (list 'conn-check-bounds))
                    transform))
     (let ((cg (prepare-change-group)))
       (atomic-change-group
         (goto-char beg)
         (delete-region beg end)
         (cl-macrolet ((push-hist (item)
                         `(conn-push-command-history
                           'conn-change-thing-do
                           thing
                           arg
                           transform
                           check-bounds
                           ,item)))
           (cond ((stringp with)
                  (insert with))
                 (kbd-macro-query
                  (let (executing-kbd-macro
                        defining-kbd-macro)
                    (push-hist
                     (if (and (= (abs (- end beg)) 1)
                              (or (conn-subthing-p thing 'char)
                                  (conn-subthing-p thing 'point)))
                         (conn-record-one-insertion)
                       (conn-record-insertion t cg)))))
                 (t
                  (if (and (= (abs (- end beg)) 1)
                           (or (conn-subthing-p thing 'char)
                               (conn-subthing-p thing 'point)))
                      (push-hist (conn-record-one-insertion))
                    (conn-record-insertion nil cg)
                    (conn-state-unwind clone
                      (when (not clone)
                        (when-let* ((text (conn-insertion-recording-text)))
                          (push-hist text)))))))))))
    (_ (error "No thing at point"))))

(cl-defmethod conn-change-thing-do ((_thing (eql conn-emacs-state-record-insert))
                                    _arg
                                    _transform
                                    &optional
                                    _check-bounds
                                    with
                                    kbd-macro-query)
  (conn-emacs-state-record-insert with)
  (when kbd-macro-query
    (let (executing-kbd-macro
          defining-kbd-macro)
      (recursive-edit))))

(cl-defmethod conn-change-thing-do ((thing (eql conn-emacs-state-overwrite))
                                    _arg
                                    _transform
                                    &optional
                                    _check-bounds
                                    _with
                                    kbd-macro-query)
  (conn-emacs-state-overwrite)
  (when kbd-macro-query
    (let (executing-kbd-macro
          defining-kbd-macro)
      (recursive-edit)))
  (conn-push-command-history
   'conn-change-thing-do
   thing nil nil nil nil kbd-macro-query))

(cl-defmethod conn-change-thing-do ((_thing (eql conn-emacs-state-overwrite-binary))
                                    _arg
                                    _transform
                                    &optional
                                    _check-bounds
                                    _with)
  (conn-emacs-state-overwrite-binary)
  (conn-push-command-history 'conn-emacs-state-overwrite-binary))

(cl-defmethod conn-change-thing-do :extra "rectangle" ((thing (conn-thing region))
                                                       _arg
                                                       _transform
                                                       &optional
                                                       _check-bounds
                                                       _with
                                                       kbd-macro-query)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (if kbd-macro-query
            (let (executing-kbd-macro
                  defining-kbd-macro)
              (call-interactively #'string-rectangle))
          (call-interactively #'string-rectangle))
        (conn-push-command-history
         'conn-change-thing-do
         thing nil nil nil nil kbd-macro-query))
    (cl-call-next-method)))

(conn-define-state conn-dispatch-change-state (conn-dispatch-bounds-state))

(cl-defmethod conn-change-thing-do ((_thing (conn-thing dispatch))
                                    arg
                                    transform
                                    &optional
                                    check-bounds
                                    with
                                    _kbd-macro-query)
  (conn-read-args (conn-dispatch-change-state
                   :prefix arg
                   :reference (list conn-dispatch-thing-reference)
                   :prompt "Thing")
      ((`(,dthing ,darg) (conn-dispatch-thing-argument t))
       (dtform (conn-dispatch-transform-argument))
       (other-end
        (conn-boolean-argument "other-end"
                               'other-end
                               conn-other-end-argument-map
                               :documentation conn-dispatch-other-end-documentation))
       (repeat
        (conn-boolean-argument "repeat"
                               'repeat-dispatch
                               conn-dispatch-repeat-argument-map
                               :documentation conn-repeat-argument-documentation))
       (stay
        (conn-boolean-argument "stay"
                               'stay
                               conn-stay-argument-map
                               :value t
                               :documentation conn-stay-argument-documentation)))
    (conn-with-dispatch-handlers
      (:handler
       (:predicate (cmd) (eq cmd 'stay))
       ( :keymap conn-dispatch-stay-map)
       ( :display ()
         (concat "\\[stay] "
                 (propertize
                  "stay"
                  'face (when stay
                          'eldoc-highlight-function-argument))))
       ( :update (_cmd break)
         (cl-callf not stay)
         (funcall break)))
      (conn-dispatch-setup
       (conn-action ()
         (:description "Change")
         (pcase-let ((`(,pt ,_window ,thing ,arg ,dtform)
                      (conn-select-target))
                     (end-pt nil))
           (conn-dispatch-change-group)
           (pcase (conn-bounds-of-dispatch thing arg pt)
             ((conn-bounds `(,beg . ,end)
                           (nconc dtform
                                  (if check-bounds
                                      (append transform
                                              (list 'conn-check-bounds))
                                    transform)))
              (save-excursion
                (conn-dispatch-goto-char beg 'nopush)
                (delete-region beg end)
                (if (stringp with)
                    (insert with)
                  (setq with
                        (if (and (= (abs (- end beg)) 1)
                                 (or (conn-subthing-p thing 'char)
                                     (conn-subthing-p thing 'point)))
                            (conn-record-one-insertion)
                          (conn-record-insertion t))))
                (setq end-pt (point)))
              (unless stay
                (conn-dispatch-goto-char end-pt))))))
       dthing darg dtform
       :other-end other-end
       :repeat repeat))
    (conn-push-command-history
     'conn-dispatch-setup-previous
     (conn-previous-dispatch-copy
      (conn-ring-head conn-dispatch-ring)))))

(cl-defmethod conn-change-thing-do ((thing (conn-thing expansion))
                                    arg
                                    transform
                                    &optional
                                    check-bounds
                                    with
                                    _kbd-macro-query)
  (conn-protected-let* ((cg (prepare-change-group)
                            (cancel-change-group cg)))
    (activate-change-group cg)
    (with-undo-amalgamate
      (conn-dispatch-setup
       (conn-action ()
         (:description "Change")
         (:repeat :not-repeatable)
         (pcase-let ((`(,pt ,_window ,thing ,arg ,_dtform)
                      (conn-select-target)))
           (conn-dispatch-change-group)
           (pcase (conn-bounds-of-dispatch thing arg pt)
             ((conn-bounds `(,beg . ,end)
                           (if check-bounds
                               (append transform
                                       (list 'conn-check-bounds))
                             transform))
              (conn-dispatch-goto-char beg 'nopush)
              (delete-region beg end)))))
       thing arg nil
       :other-end :no-other-end)
      (if with
          (progn
            (insert with)
            (accept-change-group cg))
        (conn-record-insertion nil cg)
        (conn-state-unwind clone
          (unless clone
            (setq with (conn-insertion-recording-text)))))))
  (conn-push-command-history
   (let ((prev (conn-previous-dispatch-copy
                (conn-ring-head conn-dispatch-ring))))
     (lambda ()
       (conn-protected-let* ((cg (prepare-change-group)
                                 (cancel-change-group cg)))
         (activate-change-group cg)
         (with-undo-amalgamate
           (conn-dispatch-setup-previous prev)
           (if with
               (progn
                 (insert with)
                 (accept-change-group cg))
             (conn-record-insertion nil cg)
             (conn-state-unwind clone
               (unless clone
                 (setq with (conn-insertion-recording-text)))))))))))

(cl-defmethod conn-change-thing-do ((_thing (eql conn-replace))
                                    arg
                                    transform
                                    &optional
                                    _check-bounds
                                    _with
                                    _kbd-macro-query)
  (conn-read-args (conn-replace-state
                   :prefix arg
                   :reference conn-replace-reference
                   :prompt "Replace in Thing")
      ((`(,thing ,arg ,subregions-p) (conn-replace-thing-argument))
       (transform (conn-transform-argument transform))
       (regexp-flag
        (conn-boolean-argument "regexp"
                               'regexp
                               conn-regexp-argument-map
                               :documentation conn-regexp-argument-documentation))
       (delimited
        (conn-boolean-argument "word delimited"
                               'delimited
                               conn-delimited-argument-map
                               :documentation conn-word-delimited-argument-documentation))
       (backward
        (conn-boolean-argument "backward"
                               'backward
                               conn-backward-argument-map
                               :documentation conn-backward-argument-documentation)))
    (conn-replace-do thing
                     arg
                     transform
                     delimited
                     backward
                     regexp-flag
                     subregions-p)))

(cl-defmethod conn-change-thing-do ((_thing (eql yank))
                                    arg
                                    transform
                                    &optional
                                    check-bounds
                                    _with
                                    _kbd-macro-query)
  (conn-read-args (conn-yank-replace-state
                   :prefix arg
                   :prompt "Yank Replace")
      ((`(,thing ,arg) (conn-thing-argument-dwim))
       (transform (conn-transform-argument transform))
       (swap (conn-boolean-argument "swap"
                                    'swap
                                    conn-swap-argument-map
                                    :documentation "Swap the head of the kill-ring with the thing."))
       (register (conn-read-argument
                  "register"
                  'register
                  conn-register-argument-map
                  (lambda (_) (register-read-with-preview "Register: "))
                  :formatter #'conn-argument-format-register))
       (check-bounds (conn-check-bounds-argument check-bounds)))
    (conn-yank-replace thing
                       arg
                       transform
                       swap
                       register
                       check-bounds)))

(defun conn-change-thing (thing
                          arg
                          transform
                          &optional
                          check-bounds
                          kbd-macro-query)
  "Change region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'."
  (interactive
   (conn-read-args (conn-change-state
                    :prompt "Thing"
                    :reference conn-change-reference)
       ((`(,thing ,arg) (conn-change-thing-argument))
        (transform (conn-transform-argument))
        (check-bounds (conn-check-bounds-argument))
        (kbd-macro-query
         (when (or defining-kbd-macro
                   executing-kbd-macro)
           (conn-boolean-argument
            "kbd-macro-query"
            'conn-kapply-kbd-macro-query
            nil
            :documentation "`kbd-macro-query' for this replacement."))))
     (list thing arg transform check-bounds kbd-macro-query)))
  (conn-change-thing-do thing
                        arg
                        transform
                        check-bounds
                        nil
                        kbd-macro-query))

;;;;;; Record Insertion

(conn-define-state conn-record-set-region-state (conn-read-thing-state)
  :lighter "SETREC")

(defun conn-record-set-region ()
  (interactive)
  (unless (conn-insertion-recording-p)
    (user-error "Not replacing"))
  (conn-read-args (conn-record-set-region-state
                   :prompt "Thing")
      ((`(,thing ,arg) (conn-thing-argument-dwim))
       (transform (conn-transform-argument)))
    (pcase (conn-bounds-of thing arg)
      ((conn-bounds `(,beg . ,end) transform)
       (goto-char
        (if (> (point) conn-insertion-recording-other-end) end beg))
       (set-marker
        conn-insertion-recording-other-end
        (if (> (point) conn-insertion-recording-other-end) beg end))
       (pulse-momentary-highlight-region beg end 'diff-added)
       (exit-recursive-edit)))))

(defun conn-record-exhange ()
  (interactive)
  (unless (conn-insertion-recording-p)
    (user-error "Not replacing"))
  (goto-char
   (prog1 conn-insertion-recording-other-end
     (set-marker conn-insertion-recording-other-end (point)))))

(defun conn-insertion-end-recording ()
  (interactive)
  (when conn-record-emacs-state
    (conn-pop-state)
    (unless conn-emacs-state
      (conn-push-state 'conn-emacs-state))))

(defun conn-insertion-abort-recording ()
  (interactive)
  (when conn-record-emacs-state
    (cancel-change-group (cl-shiftf conn--insertion-recording-change-group nil))
    (when (markerp conn-insertion-recording-other-end)
      (set-marker (cl-shiftf conn-insertion-recording-other-end nil)
                  nil))
    (conn-pop-state)))

(defun conn-insertion-insert-previous ()
  (interactive)
  (when conn-record-emacs-state
    (when (markerp conn-insertion-recording-other-end)
      (set-marker (cl-shiftf conn-insertion-recording-other-end nil)
                  (point)))
    (insert conn-insertion-recording-last-insertion)
    (conn-pop-state)))

;;;;; Indent

(defvar conn-indent-special-ref nil)

(defvar conn-indent-reference
  (list (conn-reference-page
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

(defvar-keymap conn-indent-cleanup-whitespace-map)

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

(cl-defgeneric conn-indent-thing-do (thing arg transform)
  (declare (conn-anonymous-thing-property :indent-op)))

(cl-defmethod conn-indent-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history 'conn-indent-thing-do args))))

(cl-defmethod conn-indent-thing-do (thing arg transform)
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (indent-region beg end))))

(cl-defmethod conn-indent-thing-do (thing
                                    arg
                                    transform
                                    &optional
                                    cleanup-whitespace)
  (pcase (conn-bounds-of thing arg)
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

(defun conn-indent-thing (thing arg transform &optional cleanup-whitespace)
  "Indent the region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If CLEANUP-WHITESPACE is non-nil then also run
`whitespace-cleanup-region' on the region."
  (interactive
   (conn-read-args (conn-indent-state
                    :prompt "Thing"
                    :reference conn-indent-reference)
       ((`(,thing ,arg) (conn-indent-thing-argument))
        (transform (conn-transform-argument))
        (cleanup-whitespace
         (conn-boolean-argument "cleanup-whitespace"
                                'cleanup-whitespace
                                conn-indent-cleanup-whitespace-map
                                :documentation "Cleanup whitespace")))
     (list thing arg transform cleanup-whitespace)))
  (conn-indent-thing-do thing arg transform cleanup-whitespace))

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

(defvar-keymap conn-indent-thing-rigidly-map)

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
                    :prompt "Thing"
                    :reference conn-indent-reference)
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (setq end (copy-marker end))
     (cl-labels ((right ()
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
          (let ((inhibit-quit t))
            (set-marker end nil)
            (conn--remove-all-advice 'conn-indent-left
                                     'conn-indent-right
                                     'conn-indent-right-to-tab-stop
                                     'conn-indent-left-to-tab-stop)))
        "Type %k to indent region interactively")
       (conn-push-command-history 'conn-indent-thing-rigidly
                                  thing
                                  arg
                                  transform)))))

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

(cl-defmethod conn-bounds-of ((_thing (conn-thing narrow-ring))
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
                    :prompt "Thing")
       ((`(,thing ,arg ,subregions)
         (conn-thing-with-subregions-argument-dwim))
        (transform (conn-transform-argument)))
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
     (conn--narrow-ring-record beg end))
    (_ (user-error "No thing at point")))
  (conn-push-command-history 'conn-thing-to-narrow-ring
                             thing
                             arg
                             transform
                             subregions-p))

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

(defvar-keymap conn-indirect-map)

(defun conn-narrow-to-thing (thing arg transform &optional indirect)
  "Narrow to the region defined by THING, ARG, and TRANSFORM.

For how the region is determined using THING, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If INDIRECT is non-nil then narrow to the region in an indirect buffer.

The region is added to `conn-narrow-ring'."
  (interactive
   (conn-read-args (conn-narrow-state
                    :prompt "Thing"
                    :prefix current-prefix-arg)
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (indirect (conn-boolean-argument "indirect"
                                         'indirect
                                         conn-indirect-map
                                         :documentation "Narrow to thing in an indirect buffer.")))
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
         (message "Buffer narrowed")))
     (conn-push-command-history 'conn-narrow-to-thing
                                thing
                                arg
                                transform
                                indirect))))

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
                    :prompt "Thing")
       ((`(,thing ,arg ,subregions)
         (conn-thing-with-subregions-argument-dwim t))
        (transform (conn-transform-argument)))
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
       (indent-according-to-mode))
      (_ (user-error "No thing found"))))
  (conn-push-command-history 'conn-join-lines
                             thing
                             arg
                             transform
                             subregions-p))

;;;;; Shell Command

(defvar-keymap conn-shell-command-replace-map)

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
                    :prompt "Thing")
       ((`(,thing ,arg ,subregions) (conn-thing-with-subregions-argument-dwim t))
        (transform (conn-transform-argument))
        (replace
         (conn-boolean-argument
          "replace"
          'replace
          conn-shell-command-replace-map
          :documentation "Replace the thing with the result of the shell command.")))
     (list thing arg transform replace subregions)))
  (conn-shell-command-on-thing-do thing arg transform replace subregions))

(cl-defgeneric conn-shell-command-on-thing-do (thing
                                               arg
                                               transform
                                               &optional
                                               replace
                                               subregions)
  (declare (conn-anonymous-thing-property :shell-command-op)))

(cl-defmethod conn-shell-command-on-thing-do :around (&rest args)
  (let ((hist conn-command-history))
    (cl-call-next-method)
    (when (eq hist conn-command-history)
      (apply #'conn-push-command-history
             'conn-shell-command-on-thing-do
             args))))

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
