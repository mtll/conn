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
(require 'conn-vars)
(require 'conn-utils)
(require 'conn-things)
(require 'conn-states)
(require 'conn-dispatch)
(require 'conn-expand)
(eval-when-compile
  (require 'cl-lib))

(declare-function outline-insert-heading "outline")
(declare-function project-files "project")
(declare-function project-root "project")
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")
(declare-function multi-isearch-read-matching-files "misearch")
(declare-function multi-isearch-read-files "misearch")
(declare-function multi-isearch-read-matching-buffers "misearch")
(declare-function multi-isearch-read-buffers "misearch")
(declare-function fileloop-continue "fileloop")

;;;; Commands

(autoload 'kmacro-ring-head "kmacro")

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

(defun conn-repeat-last-complex-command ()
  "Repeat the last complex command.

Complex commands are those that read arguments from terminal.  See
`command-history' for more."
  (interactive)
  (if-let* ((last-repeatable-command (caar command-history))
            (repeat-message-function 'ignore))
      (repeat nil)
    (user-error "No repeatable last command")))
(put 'conn-repeat-last-complex-command 'repeat-continue t)

;;;;; Movement

(defun conn-forward-visual-line (arg)
  "Move forward ARG visual lines."
  (interactive "p")
  (let ((line-move-visual t))
    (vertical-motion 0)
    (line-move arg t)))

(defun conn-backward-visual-line (arg)
  "Move backward ARG visual lines."
  (interactive "p")
  (conn-forward-visual-line (- arg)))

(defun conn-goto-line (line)
  "Goto absolute line, 1 origin.

Respects the current restriction."
  (interactive "p")
  (if (> 0 line)
      (progn
        (goto-char (point-max))
        (cl-incf line))
    (goto-char (point-min))
    (cl-decf line))
  (forward-line line))

(defun conn-backward-up-inner-list (arg)
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
  (interactive "p")
  (conn-backward-up-inner-list (- arg)))

(defun conn-forward-defun (N)
  "Move forward by defuns.

Behaves as `thingatpt' expects a \\='forward-op to behave."
  (interactive "p")
  (if (< N 0)
      (beginning-of-defun (abs N))
    (end-of-defun N)))

(defun conn-backward-symbol (arg)
  "`forward-symbol' in reverse."
  (interactive "p")
  (forward-symbol (- arg)))

(defun conn-scroll-down (&optional arg)
  "`scroll-down-command' leaving point at the same relative window position.

Pulses line that was the first visible line before scrolling."
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
  (interactive "p")
  (forward-line (- N)))

(defun conn-backward-whitespace (N)
  "`forward-whitespace' by N but backward."
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
  (interactive "p")
  (if (> N 0)
      (let ((pt (point)))
        (conn--end-of-inner-line-1)
        (when (> (point) pt) (cl-decf N))
        (cl-loop until (or (<= N 0)
                           (= (point) (point-max)))
                 do (forward-line 1)
                 unless (eolp)
                 do (cl-decf N))
        (conn--end-of-inner-line-1))
    (cl-callf abs N)
    (let ((pt (point)))
      (back-to-indentation)
      (when (> pt (point)) (cl-decf N))
      (cl-loop until (or (<= N 0)
                         (= (point) (point-max)))
               do (forward-line -1)
               unless (eolp)
               do (cl-decf N))
      (back-to-indentation))))

(defun conn-forward-inner-line-dwim (N)
  "Like `conn-forward-inner-line' but attempts to be more clever."
  (interactive "p")
  (cond ((= N 0))
        ((> 0 N)
         (conn-backward-inner-line-dwim (- N)))
        ((= (point)
            (progn
              (conn--end-of-inner-line-1)
              (point)))
         (conn-forward-inner-line N))
        ((= N 1))
        (t (conn-forward-inner-line N))))

(defun conn-backward-inner-line (N)
  "Inverse of `conn-forward-inner-line'."
  (interactive "p")
  (conn-forward-inner-line (- N)))

(defun conn-backward-inner-line-dwim (N)
  "Like `conn-backwad-inner-line' but attempts to be more clever."
  (interactive "p")
  (cond ((= N 0))
        ((> 0 N)
         (conn-backward-inner-line-dwim (- N)))
        ((= (point)
            (progn
              (back-to-indentation)
              (point)))
         (conn-forward-inner-line (- N)))
        ((= N 1))
        (t (conn-forward-inner-line (- N)))))

(defun conn-end-of-inner-line (&optional N)
  "Move point to after the last non-whitespace or comment character in line.

Immediately repeating this command goes to the point at end
of line proper."
  (interactive "P")
  (if (null N)
      (let ((point (point))
            (mark (mark t)))
        (conn--end-of-inner-line-1)
        (when (and (= point (point))
                   (or (= mark (save-excursion
                                 (back-to-indentation)
                                 (point)))
                       (region-active-p)))
          (goto-char (line-end-position))))
    (forward-line N)))

(defun conn-beginning-of-inner-line (&optional N)
  "Move point to the first non-whitespace character in line.

Immediately repeating this command goes to the point at beginning
of line proper."
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

(defun conn-end-of-list ()
  "Move point to the end of the enclosing list."
  (interactive)
  (up-list 1 t t)
  (down-list -1 t))

(defun conn-beginning-of-list ()
  "Move point to the beginning of the enclosing list."
  (interactive)
  (backward-up-list nil t t)
  (down-list 1 t))

;;;;; Command Registers

(cl-defstruct (conn-command-register)
  (command nil :read-only t))

(cl-defmethod register-val-jump-to ((val conn-command-register)
                                    _arg)
  (let ((cmd (conn-command-register-command val)))
    (apply #'funcall-interactively
           (car cmd)
           (mapcar (lambda (e) (eval e t)) (cdr cmd)))))

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
                    (cmds (cdr (mapcar #'prin1-to-string command-history))))
               (completing-read
                "Command: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function . ,#'identity))
                    (complete-with-action action cmds string pred)))
                nil t)))))

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
                                    tab-bar--current-tab-find cdr))
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

(defun conn-mark-last-command ()
  (interactive)
  (pcase (conn-bounds-of-last)
    ((conn-bounds `(,beg . ,end))
     (push-mark (if (= (point) beg) end beg) t t)
     (conn-push-state 'conn-mark-state))
    (_
     (push-mark nil t t)
     (conn-push-state 'conn-mark-state))))

(defun conn-set-mark-command ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (push-mark nil t t)
    (conn-push-state 'conn-mark-state)))

(defun conn-previous-mark-command ()
  "Push, and mark the region from the previous, `conn-mark-state'."
  (interactive)
  (unless conn--previous-mark-state
    (user-error "No previous mark state"))
  (goto-char (nth 0 conn--previous-mark-state))
  (push-mark (nth 1 conn--previous-mark-state) t t)
  (pcase (nth 2 conn--previous-mark-state)
    (`(,pc . ,mc)
     (rectangle-mark-mode 1)
     (rectangle--reset-crutches)
     (save-excursion
       (goto-char (mark))
       (rectangle--col-pos mc 'mark))
     (rectangle--col-pos pc 'point)))
  (conn-push-state 'conn-mark-state))

(defun conn-mark-thing (thing arg transform)
  "Mark the region defined by THING, ARG, and TRANSFORM"
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((and (conn-bounds `(,beg . ,end) transform)
          (conn-bounds-get :forward))
     (goto-char (if forward end beg))
     (push-mark (if forward beg end) t t)
     (conn-push-state 'conn-mark-state))))

(defun conn-exchange-mark-command (&optional arg)
  "`exchange-mark-and-point' avoiding activating the mark.

With a prefix ARG `push-mark' without activating it."
  (interactive "P")
  (cond (arg
         (push-mark (point) t nil)
         (message "Marker pushed"))
        (t
         (exchange-point-and-mark (not mark-active)))))

(defun conn-push-mark-command ()
  "Set mark at point and push old mark on mark ring."
  (interactive)
  (push-mark))

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
  (open-line N)
  (indent-according-to-mode)
  (save-excursion
    (dotimes (_ N)
      (forward-line 1)
      (indent-according-to-mode))))

;;;;; Register Setting and Loading

(defvar conn--separator-history nil
  "History var for `conn-set-register-separator'.")

(defvar-keymap conn-register-argument-map
  "<" 'register)

(defun conn-set-register-separator (string)
  "Set `register-separator' register to string STRING."
  (interactive
   (list (read-string "Separator: "
                      (let ((reg (get-register register-separator)))
                        (when (stringp reg) reg))
                      conn--separator-history nil t)))
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
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument))
        (register (conn-read-argument
                   "register"
                   'register
                   conn-register-argument-map
                   (lambda () (register-read-with-preview "Register"))
                   #'char-to-string
                   (register-read-with-preview "Register"))))
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

;;;;; Killing and Yanking

(defun conn-yank-replace (&optional kill)
  "`yank' replacing region between START and END.

If called interactively uses the region between point and mark.
If arg is non-nil, kill the region between START and END instead
of deleting it."
  (interactive "P")
  (pcase (if (region-active-p)
             (conn-bounds-of 'region nil)
           (conn-bounds-of-last))
    ((conn-bounds `(,beg . ,end)
                  (list 'conn-check-bounds))
     (goto-char beg)
     (atomic-change-group
       (conn--without-conn-maps
         (if kill
             (let ((str (filter-buffer-substring beg end t)))
               (yank)
               (kill-new str))
           (delete-region beg end)
           (yank))
         ;; yank changes this-command to 'yank, fix that
         (setq this-command 'conn-yank-replace))))))

;; (defun conn-yank-replace (thing
;;                           arg
;;                           transform
;;                           &optional
;;                           kill
;;                           check-bounds)
;;   "`yank' replacing region between START and END.
;; 
;; If called interactively uses the region between point and mark.
;; If arg is non-nil, kill the region between START and END instead
;; of deleting it."
;;   (interactive
;;    (conn-read-args (conn-read-thing-state
;;                     :prompt "Thing")
;;        ((`(,thing ,arg) (conn-thing-argument-dwim-always))
;;         (transform (conn-transform-argument))
;;         (kill (conn-boolean-argument
;;                'conn-kill-thing nil "kill"))
;;         (check-bounds (conn-check-bounds-argument)))
;;      (list thing arg transform kill check-bounds)))
;;   (pcase (conn-bounds-of thing arg)
;;     ((conn-bounds `(,beg . ,end)
;;                   (append transform
;;                           (when check-bounds
;;                             (list 'conn-check-bounds))))
;;      (goto-char beg)
;;      (atomic-change-group
;;        (conn--without-conn-maps
;;          (if kill
;;              (let ((str (filter-buffer-substring beg end t)))
;;                (yank)
;;                (kill-new str))
;;            (delete-region beg end)
;;            (yank))
;;          ;; yank changes this-command to 'yank, fix that
;;          (setq this-command 'conn-yank-replace))))))

(defun conn-completing-yank-replace (&optional kill)
  "Replace region with result of `yank-from-kill-ring'.

If ARG is non-nil `kill-region' instead of `delete-region'."
  (interactive "P")
  (pcase (if (region-active-p)
             (conn-bounds-of 'region nil)
           (conn-bounds-of-last))
    ((conn-bounds `(,beg . ,end)
                  (list 'conn-check-bounds))
     (goto-char end)
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'conn-overlay t)
       (unwind-protect
           (progn
             (overlay-put ov 'invisible t)
             (call-interactively (or (command-remapping 'yank-from-kill-ring)
                                     'yank-from-kill-ring))
             (if kill
                 (kill-region (overlay-start ov) (overlay-end ov))
               (delete-region (overlay-start ov) (overlay-end ov))))
         (delete-overlay ov))))))

;; (defun conn-completing-yank-replace (thing
;;                                      arg
;;                                      transform
;;                                      &optional
;;                                      kill
;;                                      check-bounds)
;;   "Replace region with result of `yank-from-kill-ring'.
;; 
;; If ARG is non-nil `kill-region' instead of `delete-region'."
;;   (interactive
;;    (conn-read-args (conn-read-thing-state
;;                     :prompt "Thing")
;;        ((`(,thing ,arg) (conn-thing-argument-dwim-always))
;;         (transform (conn-transform-argument))
;;         (kill (conn-boolean-argument
;;                'conn-kill-thing nil "kill"))
;;         (check-bounds (conn-check-bounds-argument)))
;;      (list thing arg transform kill check-bounds)))
;;   (pcase (conn-bounds-of thing arg)
;;     ((conn-bounds `(,beg . ,end)
;;                   (append transform
;;                           (when check-bounds
;;                             (list 'conn-check-bounds))))
;;      (goto-char end)
;;      (let ((ov (make-overlay beg end)))
;;        (overlay-put ov 'conn-overlay t)
;;        (unwind-protect
;;            (progn
;;              (overlay-put ov 'invisible t)
;;              (call-interactively (or (command-remapping 'yank-from-kill-ring)
;;                                      'yank-from-kill-ring))
;;              (if kill
;;                  (kill-region (overlay-start ov) (overlay-end ov))
;;                (delete-region (overlay-start ov) (overlay-end ov))))
;;          (delete-overlay ov))))))

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
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
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
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
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

(defvar conntext-state-hook nil)

(defun conntext-state ()
  (interactive)
  (run-hook-with-args-until-success 'conntext-state-hook))

(defun conn-one-emacs-state ()
  (interactive)
  (conn-push-state 'conn-one-emacs-state))

(defun conn-one-command ()
  (interactive)
  (conn-push-state 'conn-one-command-state))

(defun conn-previous-emacs-state (arg)
  (interactive "p")
  (cond ((< arg 0)
         (conn-next-emacs-state (abs arg)))
        ((> arg 0)
         (push-mark nil t)
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
  (interactive "p")
  (cond ((< arg 0)
         (conn-previous-emacs-state (abs arg)))
        ((> arg 0)
         (push-mark nil t)
         (dotimes (_ arg)
           (conn-ring-rotate-backward conn-emacs-state-ring))
         (goto-char (conn-ring-head conn-emacs-state-ring))
         (conn-push-state 'conn-emacs-state))))

(defun conn-emacs-state ()
  (interactive)
  (conn-push-state 'conn-emacs-state))

(defun conn-command-state ()
  (interactive)
  (conn-push-state 'conn-command-state))

(defun conn-emacs-state-at-mark ()
  "Exchange point and mark then enter `conn-emacs-state'."
  (interactive)
  (conn-exchange-mark-command)
  (conn-push-state 'conn-emacs-state))

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
  (conn-state-defer
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
          (delq (selected-window) (conn--get-windows nil 'nomini 'visible)))))
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

;;;; Thing Commands

;;;;; Replace

(defvar conn-replace-special-ref
  (conn-reference-quote
    (("In project" project))))

(defvar conn-replace-reference
  (list (conn-reference-page "Replace"
          "Replace instances of a pattern in a thing."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-replace-special-ref 2))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(conn-define-state conn-replace-state (conn-read-thing-state)
  :lighter "REPLACE")

(defvar-keymap conn-replace-thing-argument-map
  "p" 'project)

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

(defvar-keymap conn-regexp-argument-map
  "q" 'regexp)

(defvar-keymap conn-delimited-argument-map
  "d" 'delimited)

(defvar-keymap conn-backward-argument-map
  "z" 'backward)

(defvar conn-query-flag t
  "Default value for conn-query-flag.

If flag is t then `conn-replace' and `conn-regexp-replace'
will query before replacing from-string, otherwise just replace all
instances of from-string.")

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

(defun conn--replace-read-from ( prompt
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
        (lambda ()
          (setq-local conn--replace-reading t)
          (thread-last
            (current-local-map)
            (make-composed-keymap conn-replace-from-map)
            (use-local-map)))
      (if regexp-flag
          (query-replace-read-from prompt regexp-flag)
        (query-replace-read-from prompt regexp-flag)))))

(defun conn--replace-read-args (prompt
                                regexp-flag
                                &optional
                                regions
                                noerror
                                delimited-flag)
  (unless noerror
    (barf-if-buffer-read-only))
  (deactivate-mark)
  (conn-with-region-emphasis regions
    (let* ((from (conn--replace-read-from prompt
                                          regions
                                          regexp-flag
                                          delimited-flag))
           (to (if (consp from)
                   (prog1 (cdr from) (setq from (car from)))
                 (query-replace-read-to from prompt regexp-flag))))
      (list from to))))

(cl-defgeneric conn-replace-do (thing
                                arg
                                transform
                                from-string
                                to-string
                                &optional
                                delimited
                                backward
                                regexp-flag
                                subregions-p))

(cl-defmethod conn-replace-do ((thing (conn-thing t))
                               arg
                               transform
                               from-string
                               to-string
                               &optional
                               delimited
                               backward
                               regexp-flag
                               subregions-p)
  (pcase-let* (((and (conn-bounds `(,beg . ,end) transform)
                     bounds)
                (or (conn-bounds-of 'conn-bounds-of nil)
                    (conn-bounds-of thing arg))))
    (deactivate-mark)
    (save-excursion
      (if-let* ((subregions (and subregions-p
                                 (conn-bounds-get bounds :subregions transform))))
          (let* ((regions
                  (conn--merge-overlapping-regions
                   (cl-loop for bound in subregions
                            collect (conn-bounds bound))
                   t))
                 (region-extract-function
                  (lambda (method)
                    (pcase method
                      ('nil
                       (cl-loop for (beg . end) in regions
                                collect (buffer-substring beg end)))
                      ('delete-only
                       (cl-loop for (beg . end) in regions
                                do (delete-region beg end)))
                      ('bounds regions)
                      (_
                       (prog1
                           (cl-loop for (beg . end) in regions
                                    collect (filter-buffer-substring beg end method))
                         (cl-loop for (beg . end) in regions
                                  do (delete-region beg end))))))))
            (perform-replace from-string to-string t regexp-flag
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string t regexp-flag
                         delimited nil nil beg end backward)))))

(cl-defmethod conn-replace-do ((_thing (eql project))
                               _arg
                               _transform
                               from
                               to
                               &optional
                               delimited
                               _backward
                               regexp-flag
                               _subregions-p)
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
  (fileloop-continue))

(defun conn-replace (thing
                     arg
                     transform
                     from
                     to
                     &optional
                     delimited
                     backward
                     regexp-flag
                     subregions-p)
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
         (conn-boolean-argument 'regexp
                                conn-regexp-argument-map
                                "regexp"))
        (delimited-flag
         (conn-boolean-argument 'delimited
                                conn-delimited-argument-map
                                "word delimited"))
        (backward
         (conn-boolean-argument 'backward
                                conn-backward-argument-map
                                "backward")))
     (pcase-let* ((bounds
                   (ignore-errors
                     (conn-transform-bounds (conn-bounds-of thing arg)
                                            transform)))
                  (subregions (and subregions-p bounds
                                   (conn-bounds-get bounds :subregions)))
                  (`(,from ,to)
                   (conn--replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg
                                (if (eq current-prefix-arg '-) " backward" " word")
                              ""))
                    regexp-flag
                    (if (and subregions-p subregions)
                        (cl-loop for bound in subregions
                                 collect (conn-bounds bound))
                      (and bounds (list (conn-bounds bounds))))
                    nil
                    delimited-flag)))
       (list thing
             arg
             transform
             from
             to
             delimited-flag
             backward
             regexp-flag
             (and subregions-p subregions t)))))
  (conn-replace-do thing
                   arg
                   transform
                   from
                   to
                   delimited
                   backward
                   regexp-flag
                   subregions-p))

;;;;; Isearch

(defvar conn-isearch-special-ref
  (conn-reference-quote
    (("In multiple file" multi-file)
     ("In multiple buffers" multi-buffer)
     ("In current project" project))))

(defvar conn-isearch-reference
  (list (conn-reference-page "Isearch"
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
  "F" 'multi-file
  "B" 'multi-buffer
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
  (save-selected-window
    (with-isearch-suspended
     (atomic-change-group
       (recursive-edit)))))

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
  (let* ((bounds (conn-bounds-of thing arg))
         (regions
          (if-let* ((sr (and subregions-p
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
                  `((isearch-message-prefix . ,prefix)))
    (if backward
        (isearch-backward regexp t)
      (isearch-forward regexp t))))

(cl-defmethod conn-isearch-in-thing-do ((_thing (eql multi-buffer))
                                        arg
                                        _transform
                                        &key
                                        backward
                                        _regexp
                                        _subregions-p)
  (require 'misearch)
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
  (require 'misearch)
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
  (require 'project)
  (let ((files (project-files (project-current)))
        (isearch-forward (not backward)))
    (multi-isearch-files
     (if-let* ((n (seq-position files (buffer-file-name) 'file-equal-p)))
         (cons (buffer-file-name)
               (seq-remove-at-position files n))
       files))))

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
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument (use-region-p)))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument 'regexp
                                       conn-regexp-argument-map
                                       "regexp")))
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
                    :prompt "Isearch in Thing"
                    :reference conn-isearch-reference)
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument (use-region-p)))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument 'regexp
                                       conn-regexp-argument-map
                                       "regexp")))
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
              ((`(,thing ,arg) (conn-thing-argument-dwim-always))
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
                                                     string ""))))))))

;;;;; Transpose

(defvar conn--recursive-edit-transpose nil)

(defvar conn-transpose-special-ref
  (conn-reference-quote
    (("line" conn-backward-line forward-line)
     ("symbol" forward-symbol)
     ("recursive-edit" recursive-edit))))

(defvar conn-transpose-reference
  (list (conn-reference-page "Transpose"
          "Transpose two things."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-transpose-special-ref 3)))))

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE")

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-bounds-state))

(oclosure-define (conn-transpose-command
                  (:parent conn-dispatch-transpose))
  (buffer :type buffer)
  (point :type marker)
  (thing1 :type function))

(defvar conn-transpose-repeat-commands-ref
  (conn-reference-quote
    (("Repeat transposition" conn-transpose-repeat)
     ("Repeat transposition in opposite direction" conn-transpose-repeat-inverse)
     ("Recenter" recenter-top-bottom))))

(defvar conn-transpose-repeat-reference
  (list (conn-reference-page "Commands"
          (:eval (conn-quick-ref-to-cols
                  conn-transpose-repeat-commands-ref 1))
          ""
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
  "-" 'negative-argument
  "C-q" 'conn-transpose-repeat-help
  "C-l" 'recenter-top-bottom
  "q" 'conn-transpose-repeat
  "Q" 'conn-transpose-repeat-inverse)

(defun conn-transpose-setup-repeat-map (repeat repeat-inverse)
  (advice-add 'conn-transpose-repeat :override repeat)
  (advice-add 'conn-transpose-repeat-inverse :override repeat-inverse)
  (set-transient-map
   conn-transpose-repeat-map
   t
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

(cl-defmethod conn-transpose-things-do ((_cmd (conn-thing recursive-edit-thing))
                                        _arg
                                        _at-point-and-mark)
  (deactivate-mark)
  (let ((bounds1 (cons (region-beginning) (region-end)))
        (buf (current-buffer))
        (conn--recursive-edit-transpose t))
    (unwind-protect
        (conn-with-recursive-stack 'conn-command-state
          (conn-bounds-of-recursive-edit-mode 1)
          (recursive-edit))
      (conn-bounds-of-recursive-edit-mode -1))
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
     (let ((bounds2 (cons (region-beginning) (region-end))))
       (conn-anonymous-thing
         'region
         :bounds-op ( :method (_self _arg)
                      (conn-make-bounds 'recursive-edit nil bounds2))))
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
          (conn-boolean-argument 'restrict-windows
                                 conn-dispatch-restrict-windows-map
                                 "this-win")))
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
                    :prompt "Transpose"
                    :prefix current-prefix-arg
                    :reference conn-transpose-reference)
       ((`(,thing ,arg) (conn-transpose-thing-argument t))
        (at-point-and-mark (conn-boolean-argument
                            'transpose-at-point-and-mark
                            conn-transpose-point-and-mark-argument-map
                            "transpose at point and mark")))
     (list thing arg at-point-and-mark)))
  (when conn--recursive-edit-transpose
    (user-error "Recursive call to conn-transpose-things"))
  (conn-transpose-things-do thing arg at-point-and-mark))

;;;;; Kill

(defvar conn-check-bounds-default t)

(defvar conn-kill-special-ref
  (conn-reference-quote
    (("copy filename" filename)
     ("kill matching lines" kill-matching-lines)
     ("keep matching lines" keep-lines)
     ("surround" conn-surround)
     ("outer line" move-end-of-line))))

(defvar conn-kill-reference
  (list (conn-reference-page "Kill"
          "Kill some things."
          (:heading "Special Bindings")
          (:eval (conn-quick-ref-to-cols
                  conn-kill-special-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn-kill-fixup-whitespace-default t)

(conn-define-state conn-kill-state (conn-read-thing-state)
  :lighter "KILL")

(define-keymap
  :keymap (conn-get-state-map 'conn-kill-state)
  "j" 'move-end-of-line)

(defvar-keymap conn-kill-how-map
  "z" 'append-next-kill
  "c" 'copy
  "d" 'delete
  "<" 'register)

(cl-defstruct (conn-kill-how-argument
               (:include conn-argument))
  (delete nil :type boolean)
  (append nil :type symbol)
  (register nil :type (or char nil)))

(cl-defsubst conn-kill-how-argument (&key delete append register)
  (declare (important-return-value t)
           (side-effect-free t))
  (cl-assert (memq delete '(nil delete copy)))
  (cl-assert (memq append '(nil append prepend)))
  (cl-assert (not (and (eq delete 'delete)
                       (or append register))))
  (make-conn-kill-how-argument :delete delete
                               :append append
                               :register register
                               :keymap conn-kill-how-map))

(cl-defmethod conn-argument-update ((arg conn-kill-how-argument)
                                    cmd
                                    updater)
  (cl-symbol-macrolet ((delete (conn-kill-how-argument-delete arg))
                       (register (conn-kill-how-argument-register arg))
                       (append (conn-kill-how-argument-append arg)))
    (pcase cmd
      ('append-next-kill
       (setf delete (if (eq delete 'delete) nil delete)
             append (pcase append
                      ('nil 'append)
                      ('prepend nil)
                      (_ 'prepend)))
       (funcall updater arg))
      ('delete
       (setf delete (if (eq delete 'delete) nil 'delete)
             register nil
             append nil)
       (funcall updater arg))
      ('copy
       (setf delete (unless (eq delete 'copy) 'copy))
       (funcall updater arg))
      ('register
       (setf delete (if (eq delete 'delete) nil delete)
             register (if register
                          nil
                        (register-read-with-preview "Register:")))
       (funcall updater arg)))))

(cl-defmethod conn-argument-predicate ((_arg conn-kill-how-argument)
                                       sym)
  (memq sym '(append-next-kill delete register copy)))

(cl-defmethod conn-argument-display ((arg conn-kill-how-argument))
  (list
   (concat
    "\\[append-next-kill] "
    (propertize "(" 'face 'shadow)
    (propertize
     "append"
     'face (when (eq 'append (conn-kill-how-argument-append arg))
             'eldoc-highlight-function-argument))
    (propertize "|" 'face 'shadow)
    (propertize
     "prepend"
     'face (when (eq 'prepend (conn-kill-how-argument-append arg))
             'eldoc-highlight-function-argument))
    (propertize ")" 'face 'shadow))
   (concat
    "\\[register] "
    (if-let* ((ts (conn-kill-how-argument-register arg)))
        (propertize
         (format "register <%c>" ts)
         'face 'eldoc-highlight-function-argument)
      "register"))
   (concat
    "\\[delete] "
    (propertize
     "delete"
     'face (if (eq (conn-kill-how-argument-delete arg) 'delete)
               'eldoc-highlight-function-argument)))
   (when (eq (conn-kill-how-argument-delete arg) 'copy)
     (concat
      "\\[copy] "
      (propertize "copy" 'face 'conn-argument-active-face)))))

(cl-defmethod conn-argument-extract-value ((arg conn-kill-how-argument))
  (list (conn-kill-how-argument-delete arg)
        (conn-kill-how-argument-append arg)
        (conn-kill-how-argument-register arg)))

(defvar-keymap conn-kill-thing-argument-map
  "." 'filename
  "P" 'project-filename
  ">" 'kill-matching-lines
  "%" 'keep-lines)

(cl-defstruct (conn-kill-thing-argument
               (:include conn-thing-argument)
               ( :constructor conn-kill-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (keymap conn-kill-thing-argument-map)
                  (required t)
                  (value (when (and (use-region-p)
                                    conn-thing-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-thing-argument-region-dwim))))))

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

(cl-defstruct (conn-transform-and-fixup-argument
               (:include conn-composite-argument)
               ( :constructor conn-transform-and-fixup-argument
                 (&aux
                  (transform (conn-transform-argument))
                  (fixup-whitespace (conn-fixup-whitespace-argument
                                     (unless (region-active-p)
                                       conn-kill-fixup-whitespace-default)))
                  (value (list transform fixup-whitespace))))
               ( :constructor conn-dispatch-transform-and-fixup-argument
                 (&optional
                  initial-fixup-whitespace
                  &aux
                  (fixup-whitespace (conn-fixup-whitespace-argument
                                     initial-fixup-whitespace))
                  (transform (conn-dispatch-transform-argument))
                  (value (list transform fixup-whitespace)))))
  (transform nil :type list)
  (fixup-whitespace nil :type boolean)
  (explicit nil :type boolean))

(cl-defmethod conn-argument-update ((arg conn-transform-and-fixup-argument)
                                    cmd
                                    updater)
  (cl-symbol-macrolet ((tform (conn-transform-and-fixup-argument-transform arg))
                       (fws (conn-transform-and-fixup-argument-fixup-whitespace arg)))
    (let ((valid nil))
      (cond ((and (conn-argument-update tform cmd (lambda (newval)
                                                    (setq tform newval
                                                          valid t)))
                  valid)
             (unless (conn-transform-and-fixup-argument-explicit arg)
               (setf (conn-fixup-whitespace-argument-value fws)
                     (not (conn-transform-argument-value tform))))
             (funcall updater arg))
            ((and (conn-argument-update fws cmd (lambda (newval)
                                                  (setq fws newval
                                                        valid t)))
                  valid)
             (setf (conn-transform-and-fixup-argument-explicit arg) t)
             (funcall updater arg))))))

(defun conn-check-bounds-argument ()
  (when (cl-loop for h on conn-check-bounds-functions
                 thereis (pcase h
                           ('t (default-value 'conn-check-bounds-functions))
                           ('nil)
                           (_ t)))
    (conn-boolean-argument 'check-bounds
                           conn-check-bounds-argument-map
                           "check bounds"
                           conn-check-bounds-default)))

(defun conn-kill-thing (cmd
                        arg
                        transform
                        &optional
                        append
                        delete
                        register
                        fixup-whitespace
                        check-bounds)
  "Kill a region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If REGISTER is non-nil then kill the region into REGISTER instead of the
kill ring.

If APPEND is non-nil then append the killed region to the previous kill.
If killing to a registers then append to the register.  If APPEND is
\\='prepend then prepend to the previous kill or register instead.

If DELETE is non-nil then delete the region without modifying the kill
ring.  If DELETE is non-nil then an error is signaled if either APPEND
or REGISTER is non-nil.

If FIXUP-WHITESPACE is non-nil then attempt to fix up the whitespace
around the kill by calling `conn-kill-fixup-whitespace-function'.
Interactively FIXUP-WHITESPACE defaults to the value of
`conn-kill-fixup-whitespace-default'.

If CHECK-BOUNDS is non-nil then run the `conn-check-bounds-functions'
hook, which see."
  (interactive
   (conn-read-args (conn-kill-state
                    :prompt "Thing"
                    :reference conn-kill-reference
                    :command-handler
                    (lambda (cmd)
                      (when (eq cmd 'conn-set-register-separator)
                        (call-interactively #'conn-set-register-separator)
                        (conn-read-args-handle))))
       ((`(,thing ,arg) (conn-kill-thing-argument t))
        (`(,transform ,fixup) (conn-transform-and-fixup-argument))
        (`(,delete ,append ,register) (conn-kill-how-argument))
        (check-bounds (conn-check-bounds-argument)))
     (list thing arg transform append
           delete register fixup check-bounds)))
  (cl-assert (not (and delete (or register append))))
  (when (and (null append)
             (fboundp 'repeat-is-really-this-command)
             (repeat-is-really-this-command))
    (setq append 'repeat))
  (cl-callf and fixup-whitespace (null transform))
  (conn-kill-thing-do cmd arg transform
                      append delete register
                      fixup-whitespace check-bounds)
  (setq this-command 'conn-kill-thing))

(cl-defgeneric conn-kill-fixup-whitespace (bounds))

(cl-defmethod conn-kill-fixup-whitespace :after (_bounds
                                                 &context
                                                 (major-mode (derived-mode prog-mode)))
  (let ((tab-always-indent t))
    (unless (save-excursion
              (beginning-of-line)
              (looking-at-p (rx eol)))
      (indent-for-tab-command))))

(cl-defmethod conn-kill-fixup-whitespace ((_bounds (conn-thing region)))
  "Noop" nil)

(cl-defmethod conn-kill-fixup-whitespace ((_bounds (conn-thing char)))
  "Noop" nil)

(cl-defmethod conn-kill-fixup-whitespace (bounds
                                          &context
                                          (major-mode (derived-mode lisp-data-mode)))
  (cl-call-next-method)
  (cond ((conn-get-thing-property (conn-bounds-thing bounds) :linewise))
        ((save-excursion
           (beginning-of-line)
           (looking-at-p (rx (seq (* (syntax whitespace))
                                  (+ (syntax close-parenthesis))
                                  eol))))
         (join-line))
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
         (let ((col (current-column)))
           (join-line)
           (forward-line)
           (move-to-column col)))))

(cl-defmethod conn-kill-fixup-whitespace (bounds)
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
    (setq append (if (>= beg (point)) 'append 'prepend)))
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
        (kill-append (conn-kill-separator-for-region separator)
                     (eq append 'prepend)))
      (if delete-flag
          (kill-region beg end)
        (copy-region-as-kill beg end)))))

(defun conn--kill-string (string &optional append register separator)
  (if register
      (if append
          (let ((reg (get-register register)))
            (set-register
             register
             (cond ((not reg) string)
                   ((stringp reg)
                    (if (eq append 'prepend)
                        (concat string separator reg)
                      (concat reg separator string)))
                   (t (user-error "Register does not contain text")))))
        (set-register register string))
    (if append
        (progn
          (when separator
            (kill-append separator (eq append 'prepend)))
          (kill-append string (eq append 'prepend)))
      (kill-new string))))

(cl-defgeneric conn-kill-thing-do (cmd
                                   arg
                                   transform
                                   &optional
                                   append
                                   delete
                                   register
                                   fixup-whitespace
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
                                  _fixup-whitespace
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
        (conn--kill-string str append register)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a file")))

(cl-defmethod conn-kill-thing-do ((_cmd (eql project-filename))
                                  _arg
                                  _transform
                                  &optional
                                  append
                                  _delete
                                  register
                                  _fixup-whitespace
                                  _check-bounds)
  (require 'project)
  (if-let* ((fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (expand-file-name (project-root (project-current)))))
      (progn
        (conn--kill-string str append register)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a project")))

(cl-defmethod conn-kill-thing-do ((_cmd (eql kill-matching-lines))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  _register
                                  _fixup-whitespace
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
                                  _fixup-whitespace
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

(defvar-keymap conn-separator-argument-map
  "+" 'register-separator
  "_" 'separator)

(cl-defstruct (conn-separator-argument
               (:include conn-argument)
               ( :constructor conn-separator-argument
                 (value
                  &aux
                  (keymap conn-separator-argument-map)))))

(cl-defmethod conn-argument-update ((arg conn-separator-argument)
                                    cmd
                                    updater)
  (cl-symbol-macrolet ((value (conn-argument-value arg)))
    (pcase cmd
      ('separator
       (if (or (stringp value)
               (eq 'default value))
           (setf value nil)
         (setf value (if (conn-read-args-consume-prefix-arg)
                         (read-string "Separator: " nil nil nil t)
                       'default)))
       (funcall updater arg))
      ('register-separator
       (if (eq value 'register)
           (setf value nil)
         (setf value (get-register register-separator)))
       (funcall updater arg)))))

(cl-defmethod conn-argument-predicate ((_arg conn-separator-argument)
                                       sym)
  (or (eq sym 'separator)
      (eq sym 'register-separator)))

(cl-defmethod conn-argument-display ((arg conn-separator-argument))
  (concat "\\[separator]/\\[register-separator] separator"
          (when-let* ((sep (conn-argument-value arg)))
            (concat
             ": "
             (propertize (format "<%s>" sep)
                         'face 'eldoc-highlight-function-argument)))))

(defun conn-kill-separator-for-region (separator)
  (pcase separator
    ('nil)
    ((pred stringp) separator)
    (_ (let ((beg (region-beginning))
             (end (region-end)))
         (save-excursion
           (goto-char beg)
           (if (search-forward "\n" end t) "\n" " "))))))

(defun conn-kill-separator-for-strings (strings separator)
  (pcase separator
    ('nil)
    ((pred stringp) separator)
    (_ (catch 'sep
         (dolist (str (ensure-list strings))
           (when (string-match "\n" str nil t)
             (throw 'sep "\n")))
         " "))))

(oclosure-define (conn-kill-action
                  (:parent conn-action)))

(cl-defmethod conn-kill-thing-do ((_cmd (conn-thing dispatch))
                                  arg
                                  transform
                                  &optional
                                  append
                                  delete
                                  register
                                  fixup-whitespace
                                  check-bounds)
  (let ((conn-dispatch-amalgamate-undo t)
        (result nil)
        (strings nil))
    (conn-read-args (conn-dispatch-bounds-state
                     :prefix arg
                     :prompt "Kill"
                     :reference (list conn-dispatch-thing-reference))
        ((`(,thing ,arg) (conn-thing-argument t))
         (`(,dtform ,fixup-whitespace)
          (conn-dispatch-transform-and-fixup-argument
           fixup-whitespace))
         (`(,delete ,append ,register)
          (conn-kill-how-argument
           :append (if (eq append 'repeat)
                       'append
                     append)
           :delete delete
           :register register))
         (repeat
          (conn-boolean-argument 'repeat-dispatch
                                 conn-dispatch-repeat-arg-map
                                 "repeat"))
         (separator (when (not delete)
                      (conn-separator-argument 'default)))
         (restrict-windows
          (conn-boolean-argument 'restrict-windows
                                 conn-dispatch-restrict-windows-map
                                 "this-win")))
      (conn-with-dispatch-event-handlers
        ( :handler (cmd)
          (when (eq cmd 'dispatch-other-end)
            (setq append (pcase append
                           ('nil 'append)
                           ('prepend nil)
                           (_ 'prepend)))
            (conn-dispatch-handle)))
        ( :message 10 (keymap)
          (when-let* ((binding
                       (where-is-internal 'dispatch-other-end keymap t)))
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
        (conn-dispatch-setup
         (oclosure-lambda (conn-kill-action
                           (action-description "Kill"))
             ()
           (pcase-let* ((`(,pt ,window ,thing ,arg ,dtform)
                         (conn-select-target)))
             (with-selected-window window
               (conn-dispatch-change-group)
               (save-mark-and-excursion
                 (pcase (conn-bounds-of-dispatch thing arg pt)
                   ((and bounds
                         (conn-dispatch-bounds `(,beg . ,end)
                                               `(,@dtform
                                                 ,@transform
                                                 ,@(when check-bounds
                                                     (list 'conn-check-bounds)))))
                    (goto-char beg)
                    (push-mark end t)
                    (if delete
                        (delete-region beg end)
                      (push (cons append (funcall region-extract-function t))
                            strings)
                      (conn-dispatch-undo-case 90
                        (:undo
                         (pop strings)
                         (conn-dispatch-undo-pulse beg end))
                        (:cancel
                         (pop strings))))
                    (when fixup-whitespace
                      (funcall conn-kill-fixup-whitespace-function bounds)))
                   (_ (user-error "No %s found" thing)))))))
         thing arg dtform
         :repeat repeat
         :other-end :no-other-end
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
                                  fixup-whitespace
                                  check-bounds)
  (if (eq delete 'copy)
      (conn-copy-thing-do cmd arg transform append register)
    (pcase (conn-bounds-of cmd arg)
      ((and (conn-bounds `(,beg . ,end)
                         `(,@transform
                           ,@(when check-bounds
                               (list 'conn-check-bounds))))
            bounds)
       (if delete
           (delete-region beg end)
         (conn--kill-region beg end t append register))
       (when fixup-whitespace
         (goto-char beg)
         (funcall conn-kill-fixup-whitespace-function bounds))))))

(cl-defmethod conn-kill-thing-do ((_cmd (conn-thing line))
                                  &rest _)
  (let ((col (current-column)))
    (cl-call-next-method)
    (move-to-column col)))

(cl-defmethod conn-kill-thing-do :extra "rmm" ((_cmd (conn-thing region))
                                               _arg
                                               _transform
                                               &optional
                                               _append
                                               delete
                                               register
                                               _fixup-whitespace
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
  (list (conn-reference-page "Copy"
          "Copy some things."
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

(defvar-keymap conn-copy-how-map
  "z" 'append-next-kill
  "<" 'register)

(cl-defstruct (conn-copy-how-argument
               (:include conn-argument)
               (:constructor conn-copy-how-argument
                             (&key
                              append
                              register
                              &aux
                              (keymap conn-copy-how-map))))
  (append nil)
  (register nil))

(cl-defmethod conn-argument-update ((arg conn-copy-how-argument)
                                    cmd updater)
  (pcase cmd
    ('append-next-kill
     (setf (conn-copy-how-argument-append arg)
           (pcase (conn-copy-how-argument-append arg)
             ('nil 'append)
             ('prepend nil)
             (_ 'prepend)))
     (funcall updater arg))
    ('register
     (setf (conn-copy-how-argument-register arg)
           (if (conn-copy-how-argument-register arg)
               nil
             (register-read-with-preview "Register:")))
     (funcall updater arg))))

(cl-defmethod conn-argument-predicate ((_arg conn-copy-how-argument)
                                       sym)
  (memq sym '(append-next-kill register)))

(cl-defmethod conn-argument-display ((arg conn-copy-how-argument))
  (list
   (concat
    "\\[append-next-kill] "
    (propertize "(" 'face 'shadow)
    (propertize
     "append"
     'face (when (eq 'append (conn-copy-how-argument-append arg))
             'eldoc-highlight-function-argument))
    (propertize "|" 'face 'shadow)
    (propertize
     "prepend"
     'face (when (eq 'prepend (conn-copy-how-argument-append arg))
             'eldoc-highlight-function-argument))
    (propertize ")" 'face 'shadow))
   (concat
    "\\[register] "
    (if-let* ((ts (conn-copy-how-argument-register arg)))
        (propertize
         (format "register <%c>" ts)
         'face 'eldoc-highlight-function-argument)
      "register"))))

(cl-defmethod conn-argument-extract-value ((arg conn-copy-how-argument))
  (list (conn-copy-how-argument-append arg)
        (conn-copy-how-argument-register arg)))

(defvar-keymap conn-copy-thing-argument-map
  "." 'filename
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
                  (value (when (and (use-region-p)
                                    conn-thing-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-thing-argument-region-dwim))))))

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql project-filename)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-copy-thing-argument)
                                       (_cmd (eql copy-matching-lines)))
  t)

(defun conn-copy-thing (thing arg &optional transform append register)
  "Copy a region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'.

If REGISTER is non-nil then copy the region into REGISTER instead of the
kill ring.

If APPEND is non-nil then append the copied region to the previous kill.
If copying to a registers then append to the register.  If APPEND is
\\='prepend then prepend to the previous kill or register instead."
  (interactive
   (conn-read-args (conn-copy-state
                    :prompt "Thing"
                    :reference conn-copy-reference
                    :command-handler
                    (lambda (cmd)
                      (when (eq cmd 'conn-set-register-separator)
                        (call-interactively #'conn-set-register-separator)
                        (conn-read-args-handle))))
       ((`(,thing ,arg) (conn-copy-thing-argument))
        (transform (conn-transform-argument))
        (`(,append ,register)
         (conn-copy-how-argument
          :register (when current-prefix-arg
                      (register-read-with-preview "Register:")))))
     (list thing arg transform append register)))
  (conn-copy-thing-do thing arg transform append register))

(cl-defgeneric conn-copy-thing-do (cmd
                                   arg
                                   &optional
                                   transform
                                   append
                                   register)
  (declare (conn-anonymous-thing-property :copy-op)))

(cl-defmethod conn-copy-thing-do (cmd
                                  arg
                                  &optional
                                  transform
                                  append
                                  register)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn--kill-region beg end nil append register)
     (unless executing-kbd-macro
       (pulse-momentary-highlight-region beg end)))))

(cl-defmethod conn-copy-thing-do ((_cmd (eql copy-matching-lines))
                                  arg
                                  &optional
                                  transform
                                  append
                                  _register)
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
                                  register)
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
        (conn--kill-string str append register)
        (message "Yanked \"%s\"" str))
    (user-error "Buffer does not have a file")))

(cl-defmethod conn-copy-thing-do ((_cmd (eql project-filename))
                                  _arg
                                  &optional
                                  _transform
                                  append
                                  register)
  (if-let* ((fname (buffer-file-name
                    (if (minibuffer-window-active-p (selected-window))
                        (window-buffer (minibuffer-selected-window))
                      (current-buffer))))
            (str (expand-file-name (project-root (project-current)))))
      (progn
        (conn--kill-string str append register)
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
                                  register)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix arg
                   :prompt "Copy"
                   :reference (list conn-dispatch-thing-reference))
      ((`(,thing ,arg) (conn-thing-argument t))
       (transform (conn-dispatch-transform-argument transform))
       (repeat
        (conn-boolean-argument 'repeat-dispatch
                               conn-dispatch-repeat-arg-map
                               "repeat"))
       (append (conn-copy-how-argument :append append))
       (separator (conn-separator-argument 'default))
       (restrict-windows
        (conn-boolean-argument 'restrict-windows
                               conn-dispatch-restrict-windows-map
                               "this-win")))
    (conn-with-dispatch-event-handlers
      ( :handler (cmd)
        (when (eq cmd 'dispatch-other-end)
          (setq append (pcase append
                         ('nil 'append)
                         ('prepend nil)
                         (_ 'prepend)))
          (conn-dispatch-handle)))
      ( :message 10 (keymap)
        (when-let* ((binding
                     (where-is-internal 'dispatch-other-end keymap t)))
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
         (oclosure-lambda (conn-kill-action
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
                    (conn-dispatch-undo-case 90
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

(defun conn-last-thing ()
  "Copy the thing just moved over or the active region."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end) t)
    (pcase (conn-bounds-of-last)
      ((conn-bounds `(,beg . ,end))
       (pulse-momentary-highlight-region beg end)
       (copy-region-as-kill beg end)))))

;;;;; How Many

(defvar conn-how-many-special-ref nil)

(defvar conn-how-many-reference
  (list (conn-reference-page "How Many"
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
  (list (conn-reference-page "Comment"
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
                    :reference conn-comment-reference
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-comment-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-comment-thing-do thing arg transform))

;;;;; Duplicate

(defvar conn-duplicate-special-ref nil)

(defvar conn-duplicate-reference
  (list (conn-reference-page "Duplicate"
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
  (list (conn-reference-page "Commands"
          (:eval (conn-quick-ref-to-cols
                  conn-duplicate-repeat-commands-ref 1))
          ""
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

(define-minor-mode conn-duplicate-repeat-mode
  "After duplicating provide a set of keybindings for repeating.

The following commands are bound in `conn-duplicate-repeat-map'.
If the next command is one in `conn-duplicate-repeat-map' then the
map stays active.  The commands are only usable while the map is active.
\\<conn-duplicate-repeat-map>

`digit-argument' and `negative-argument'.

\\[conn-duplicate-indent-repeat] `conn-duplicate-indent-repeat':
 Indent the region containing all duplicates.

\\[conn-duplicate-delete-repeat] `conn-duplicate-delete-repeat':
 Delete the most recently inserted duplicate.  With a prefix argument
delete that many duplicates.

\\[conn-duplicate-repeat] `conn-duplicate-repeat':
 Insert another duplicate.  With a prefix argument insert that many
additional duplicates.

\\[conn-duplicate-repeat-toggle-padding] `conn-duplicate-repeat-toggle-padding':
 Toggles the insertion of an extra newline before each duplicate.

\\[conn-duplicate-repeat-comment] `conn-duplicate-repeat-comment':
 Comment or uncomment each duplicated region.

\\[recenter-top-bottom] `recenter-top-bottom':
 Recenter the window."
  :global t
  :lighter nil
  :group 'conn)

(defvar-keymap conn-duplicate-repeat-map
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
  "-" 'negative-argument
  "C-q" 'conn-duplicate-repeat-help
  "TAB" 'conn-duplicate-indent-repeat
  "<tab>" 'conn-duplicate-indent-repeat
  "DEL" 'conn-duplicate-delete-repeat
  "<backspace>" 'conn-duplicate-delete-repeat
  "D" 'conn-duplicate-repeat
  "M-RET" 'conn-duplicate-repeat-toggle-padding
  "M-<return>" 'conn-duplicate-repeat-toggle-padding
  ";" 'conn-duplicate-repeat-comment
  "C-l" 'recenter-top-bottom)

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
  "Duplicate the region from BEG to END REPEAT times.

When `conn-duplicate-repeat-mode' is non-nil activate
`conn-duplicate-repeat-map' after duplicating."
  (deactivate-mark)
  (conn-protected-let*
      ((regions (list (make-overlay beg end nil t))
                (mapc #'delete-overlay regions))
       (str (buffer-substring-no-properties beg end))
       (block (seq-contains-p str ?\n))
       (extra-newline nil)
       (padding (if block "\n" " "))
       (regexp (if block "\n" "[\t ]"))
       (commented nil))
    (cl-labels
        ((dup ()
           (unless (looking-back regexp 1)
             (insert padding))
           (let ((rbeg (point))
                 ov)
             (insert str)
             (push (make-overlay rbeg (point) nil t)
                   regions)
             (setq ov (car regions))
             (overlay-put ov 'face 'lazy-highlight)
             (let ((beg (make-marker))
                   (end (make-marker)))
               (set-marker beg (overlay-start ov))
               (set-marker end (overlay-end ov))
               (set-marker-insertion-type end t)
               (when commented
                 (comment-region beg end))
               (move-overlay
                ov beg (min end (save-excursion
                                  (goto-char (overlay-end ov))
                                  (pos-eol))))
               (set-mark (overlay-end ov))
               (set-marker beg nil)
               (set-marker end nil))))
         (cleanup ()
           (mapc #'delete-overlay regions)
           (advice-remove 'conn-duplicate-indent-repeat #'indent)
           (advice-remove 'conn-duplicate-repeat #'repeat)
           (advice-remove 'conn-duplicate-delete-repeat #'delete)
           (advice-remove 'conn-duplicate-repeat-comment #'comment)
           (advice-remove 'conn-duplicate-repeat-toggle-padding
                          (if block #'block-padding #'non-block-padding)))
         (indent ()
           (interactive)
           (indent-region (overlay-start (car (last regions)))
                          (overlay-end (car regions))))
         (repeat (n)
           (interactive "p")
           (atomic-change-group
             (save-excursion
               (goto-char (overlay-end (car regions)))
               (dotimes (_ n) (dup)))))
         (delete (n)
           (interactive "p")
           (when (> (length regions) 2)
             (let* ((n (min (abs n) (1- (length regions))))
                    (delete (take n regions))
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
             (let ((beg (make-marker))
                   (end (make-marker)))
               (set-marker-insertion-type end t)
               (dolist (ov (butlast regions))
                 (set-marker beg (overlay-start ov))
                 (set-marker end (overlay-end ov))
                 (comment-or-uncomment-region beg end)
                 (move-overlay
                  ov beg (min end (save-excursion
                                    (goto-char (overlay-end ov))
                                    (pos-eol)))))
               (set-marker beg nil)
               (set-marker end nil))
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
      (goto-char end)
      (save-excursion
        (when (> repeat 0) (push-mark nil nil))
        (dotimes (_ repeat) (dup)))
      (if (not conn-duplicate-repeat-mode)
          (cleanup)
        (advice-add 'conn-duplicate-indent-repeat :override #'indent)
        (advice-add 'conn-duplicate-repeat :override #'repeat)
        (advice-add 'conn-duplicate-delete-repeat :override #'delete)
        (advice-add 'conn-duplicate-repeat-comment :override #'comment)
        (advice-add 'conn-duplicate-repeat-toggle-padding :override
                    (if block #'block-padding #'non-block-padding))
        (set-transient-map
         conn-duplicate-repeat-map
         t
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
                  'face 'help-key-binding)))))))

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
                  (value (when (and (use-region-p)
                                    conn-thing-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-thing-argument-region-dwim))))))

(defun conn-duplicate-thing (thing arg transform &optional repeat)
  "Duplicate the region defined by THING, ARG, and TRANSFORM.

For how they are used to define the region see `conn-bounds-of' and
`conn-transform-bounds'.

If REPEAT is non-nil then duplicate the region REPEAT times.
Interactively REPEAT is given by the prefix argument.

When `conn-duplicate-repeat-mode', which see, is active the transient map
`conn-duplicate-repeat-map' is setup after duplicating."
  (interactive
   (conn-read-args (conn-duplicate-state
                    :prompt "Thing"
                    :reference conn-duplicate-reference)
       ((`(,thing ,arg) (conn-thing-argument-dwim-always t))
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
  (list (conn-reference-page "Change"
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
  "e" 'conn-emacs-state-overwrite
  "E" 'conn-emacs-state-overwrite-binary
  "j" conn-backward-char-remap
  "l" conn-forward-char-remap)

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

(defun conn-change-thing (cmd arg transform)
  "Change region defined by CMD, ARG, and TRANSFORM.

For how the region is determined using CMD, ARG, and TRANSFORM see
`conn-bounds-of' and `conn-transform-bounds'."
  (interactive
   (conn-read-args (conn-change-state
                    :prompt "Thing"
                    :reference conn-change-reference)
       ((`(,thing ,arg) (conn-change-thing-argument))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-change-thing-do cmd arg transform))

;;;;; Indent

(defvar conn-indent-special-ref nil)

(defvar conn-indent-reference
  (list (conn-reference-page "Indent"
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
                    :prompt "Thing"
                    :reference conn-indent-reference)
       ((`(,thing ,arg) (conn-indent-thing-argument))
        (transform (conn-transform-argument))
        (cleanup-whitespace
         (conn-boolean-argument 'cleanup-whitespace
                                conn-indent-cleanup-whitespace-map
                                "cleanup-whitespace")))
     (list thing arg transform cleanup-whitespace)))
  (conn-indent-thing-do cmd arg transform cleanup-whitespace))

;;;;; Narrowing Commands

(defvar-local conn-narrow-ring nil
  "Ring of recent narrowed regions.")

(defvar conn-narrow-ring-max 14)

(conn-define-state conn-narrow-state (conn-read-thing-state)
  :lighter "NARROW")

(cl-defmethod conn-bounds-of ((_cmd (conn-thing narrow-ring))
                              _arg)
  (when conn-narrow-ring
    (let ((subregions nil)
          (beg most-positive-fixnum)
          (end most-negative-fixnum))
      (pcase-dolist ((and bound `(,b . ,e))
                     (conn-ring-list conn-narrow-ring))
        (cl-callf max end e)
        (cl-callf min beg b)
        (push (conn-make-bounds 'region nil bound)
              subregions))
      (when subregions
        (conn-make-bounds
         'narrow-ring nil
         (cons beg end)
         :subregions subregions)))))

(defun conn-thing-to-narrow-ring (thing
                                  arg
                                  transform
                                  &optional
                                  subregions-p)
  "Push thing regions to narrow ring."
  (interactive
   (conn-read-args (conn-narrow-state
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

(defun conn--narrow-ring-record (beg end)
  (unless (conn-ring-p conn-narrow-ring)
    (setq conn-narrow-ring
          (conn-make-ring conn-narrow-ring-max
                          :cleanup (pcase-lambda (`(,b . ,e))
                                     (set-marker b nil)
                                     (set-marker e nil))
                          :copier (pcase-lambda (`(,b . ,e))
                                    (cons (conn--copy-mark b)
                                          (conn--copy-mark e))))))
  (pcase-let ((`(,bf . ,ef) (conn-ring-head conn-narrow-ring))
              (`(,bb . ,eb) (conn-ring-tail conn-narrow-ring)))
    (cond
     ((and bf (= beg bf) (= end ef)))
     ((and bb (= beg bb) (= end eb))
      (conn-ring-rotate-forward conn-narrow-ring))
     (t (conn-ring-insert-front conn-narrow-ring
                                (cons (conn--create-marker beg)
                                      (conn--create-marker end)))))))

(defun conn-cycle-narrowings (arg)
  "Cycle to the ARGth region in `conn-narrow-ring'."
  (interactive "p")
  (cond ((= arg 0)
         (conn-merge-narrow-ring))
        ((> arg 0)
         (pcase-let ((`(,beg . ,end)
                      (conn-ring-head conn-narrow-ring)))
           (unless (and (= (point-min) beg)
                        (= (point-max) end))
             (cl-decf arg)))
         (dotimes (_ arg)
           (conn-ring-rotate-forward conn-narrow-ring)))
        ((< arg 0)
         (dotimes (_ (abs arg))
           (conn-ring-rotate-backward conn-narrow-ring))))
  (pcase (conn-ring-head conn-narrow-ring)
    (`(,beg . ,end)
     (unless (<= beg (point) end)
       (push-mark (point) t)
       (goto-char beg))
     (narrow-to-region beg end))
    (_ (user-error "Narrow ring empty"))))

(defun conn-merge-narrow-ring (&optional interactive)
  "Merge overlapping narrowings in `conn-narrow-ring'."
  (interactive (list t))
  (let ((new (conn--merge-overlapping-regions
              (conn-ring-list conn-narrow-ring))))
    (setf (conn-ring-list conn-narrow-ring) new
          (conn-ring-history conn-narrow-ring) (copy-sequence new)))
  (when (and interactive (not executing-kbd-macro))
    (message "Narrow ring merged into %s region"
             (length (conn-ring-list conn-narrow-ring)))))

(defun conn-clear-narrow-ring ()
  "Remove all narrowings from the `conn-narrow-ring'."
  (interactive)
  (cl-loop for (beg . end) in (conn-ring-list conn-narrow-ring)
           do
           (set-marker beg nil)
           (set-marker end nil))
  (setf (conn-ring-list conn-narrow-ring) nil
        (conn-ring-history conn-narrow-ring) nil))

(defun conn-pop-narrow-ring ()
  "Pop `conn-narrow-ring'."
  (interactive)
  (pcase (conn-ring-head conn-narrow-ring)
    ('nil (widen))
    ((and `(,beg . ,end)
          (guard (= (point-min) beg))
          (guard (= (point-max) end))
          narrowing)
     (conn-ring-delq narrowing conn-narrow-ring)
     (pcase (conn-ring-head conn-narrow-ring)
       (`(,beg . ,end)
        (narrow-to-region beg end))
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
                    :prompt "Thing"
                    :prefix current-prefix-arg)
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (indirect (conn-boolean-argument 'indirect
                                         conn-indirect-map
                                         "indirect")))
     (list thing arg transform indirect)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (unless (and (<= beg (point) end)
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

;;;;;; Bounds of Narrow Ring

(conn-register-thing 'narrowing)

(cl-defmethod conn-bounds-of ((_cmd (conn-thing narrowing))
                              _arg)
  (cl-loop for (beg . end) in conn-narrow-ring
           minimize beg into narrow-beg
           maximize end into narrow-end
           collect (conn-make-bounds
                    'narrowing nil
                    (cons beg end))
           into narrowings
           finally return (conn-make-bounds
                           'narrowing nil
                           (cons narrow-beg narrow-end)
                           :subregions narrowings)))

(conn-register-thing-commands
 'narrowing nil
 'narrow-to-region 'widen
 'conn-narrow-to-thing
 'conn-narrow-ring-prefix)

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
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument
                     (and (use-region-p)
                          (region-noncontiguous-p))))
        (replace (conn-boolean-argument 'replace
                                        conn-shell-command-replace-map
                                        "replace")))
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
