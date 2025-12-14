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
(require 'conn-mark)
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

(defun conn-make-command-repeatable (&optional command)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector last-command-event)
                (or command
                    (when (eq this-command (caar command-history))
                      'conn-repeat-last-complex-command)
                    'repeat))
    (setq repeat-map map)))

(defun conn-disable-repeating ()
  (setq repeat-map nil))

(defun conn-repeat-last-complex-command ()
  (interactive)
  (if-let* ((last-repeatable-command (caar command-history))
            (repeat-message-function 'ignore))
      (repeat nil)
    (user-error "No repeatable last command")))
(put 'conn-repeat-last-complex-command 'repeat-continue t)

;;;;; Movement

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
    (conn-protected-let* ((pt (point) (goto-char pt)))
      (backward-up-list (cl-signum arg))
      (down-list (cl-signum arg))
      (if (= pt (point))
          (backward-up-list (cl-signum arg))
        (cl-decf arg (cl-signum arg)))
      (backward-up-list arg)
      (down-list (cl-signum arg)))))

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
          (goto-char (line-end-position))
          (setq conn-this-command-thing 'outer-line)))
    (forward-line N)
    (setq conn-this-command-handler (conn-command-mark-handler 'forward-line)
          conn-this-command-thing 'line)))

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
          (goto-char (line-beginning-position))
          (setq conn-this-command-thing 'outer-line)))
    (forward-line (- N))
    (setq conn-this-command-thing 'line
          conn-this-command-handler (conn-command-mark-handler 'forward-line))))

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

(defun conn-unpop-movement-ring (arg)
  "Rotate backward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-pop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-movement-ring (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-backward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

(defun conn-pop-movement-ring (arg)
  "Rotate forward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-unpop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-movement-ring (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-forward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

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
  "Store command in REGISTER."
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
  "Store tab in REGISTER."
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

(defun conn-toggle-mark-command (&optional arg)
  "Toggle `mark-active'.

With a prefix ARG activate `rectangle-mark-mode'."
  (interactive "P")
  (cond (arg
         (conn-rectangle-mark)
         (conn-push-state 'conn-mark-state))
        (mark-active (deactivate-mark))
        (t
         (activate-mark)
         (conn-push-state 'conn-mark-state))))

(defun conn-set-mark-command (&optional arg)
  "Toggle `mark-active' and push ephemeral mark at point.

With a prefix ARG activate `rectangle-mark-mode'.
Immediately repeating this command pushes a mark."
  (interactive "P")
  (cond (arg
         (rectangle-mark-mode 'toggle))
        ((eq last-command 'conn-set-mark-command)
         (setq conn-record-mark-state nil)
         (if (region-active-p)
             (progn
               (push-mark nil t)
               (deactivate-mark)
               (message "Mark pushed and deactivated"))
           (activate-mark)
           (message "Mark activated")))
        (t
         (conn--push-ephemeral-mark)
         (activate-mark)))
  (when (region-active-p)
    (conn-push-state 'conn-mark-state)))

(defun conn-previous-mark-command ()
  (interactive)
  (unless conn-previous-mark-state
    (user-error "No previous mark state"))
  (goto-char (nth 0 conn-previous-mark-state))
  (conn--push-ephemeral-mark (nth 1 conn-previous-mark-state)
                             nil t)
  (pcase (nth 2 conn-previous-mark-state)
    (`(,pc . ,mc)
     (rectangle-mark-mode 1)
     (rectangle--reset-crutches)
     (save-excursion
       (goto-char (mark))
       (rectangle--col-pos mc 'mark))
     (rectangle--col-pos pc 'point)))
  (conn-push-state 'conn-mark-state))

(defun conn-mark-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (goto-char beg)
     (push-mark end t t)
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
  (interactive)
  (push-mark))

;;;;;; Mark Ring

(defun conn-pop-mark-ring ()
  "Like `pop-to-mark-command' but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-forward conn-mark-ring)
    (goto-char (conn-ring-head conn-mark-ring)))
  (deactivate-mark))

(defun conn-unpop-mark-ring ()
  "Like `pop-to-mark-command' in reverse but uses `conn-mark-ring'."
  (interactive)
  (if (null conn-mark-ring)
      (user-error "Mark ring empty")
    (conn--push-ephemeral-mark (point))
    (conn--push-mark-ring (point))
    (conn-ring-rotate-backward conn-mark-ring)
    (goto-char (conn-ring-head conn-mark-ring)))
  (deactivate-mark))

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

(defun conn-join-lines-in-region (beg end)
  "`delete-indentation' in region from BEG and END."
  (interactive (list (region-beginning)
                     (region-end)))
  (delete-indentation nil beg end)
  (indent-according-to-mode))

(conn-define-state conn-join-lines-state (conn-read-thing-state)
  :lighter "JOIN")

(defun conn-join-lines (thing arg transform &optional subregions-p)
  "`delete-indentation' in region from START and END."
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
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
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

;;;;; Register Setting and Loading

(defvar conn--separator-history nil
  "History var for `conn-set-register-separator'.")

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

(defun conn-register-load-and-replace (reg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (register-read-with-preview
            "Insert register and replace: "
            #'register--insertable-p))))
  (atomic-change-group
    (if (bound-and-true-p rectangle-mark-mode)
        (delete-rectangle (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))
    (register-val-insert (get-register reg))))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))

;;;;; Killing and Yanking

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn
  :type 'symbol)

(defvar-local conn--minibuffer-initial-region nil)

(defun conn--yank-region-to-minibuffer-hook ()
  (setq conn--minibuffer-initial-region
        (with-minibuffer-selected-window
          (ignore-errors (cons (region-beginning) (region-end))))))

(defun conn-yank-region-to-minibuffer (&optional quote-function)
  "Yank region from `minibuffer-selected-window' into minibuffer."
  (interactive
   (list (if current-prefix-arg
             (if conn-completion-region-quote-function
                 (pcase (car (read-multiple-choice
                              "Quote:"
                              '((?r "regexp-quote")
                                (?c "conn-completion-region-quote-function"))))
                   (?r 'regexp-quote)
                   (?c conn-completion-region-quote-function))
               'regexp-quote)
           'identity)))
  (insert-for-yank
   (pcase conn--minibuffer-initial-region
     (`(,beg . ,end)
      (with-minibuffer-selected-window
        (funcall (or quote-function 'identity)
                 (filter-buffer-substring beg end))))
     (_ (user-error "No region in buffer")))))

(defun conn-yank-replace (&optional kill-region)
  "`yank' replacing region between START and END.

If called interactively uses the region between point and mark.
If arg is non-nil, kill the region between START and END instead
of deleting it."
  (interactive "P")
  (pcase (conn-bounds-of 'region nil)
    ((conn-bounds `(,beg . ,end) (list 'conn-check-bounds))
     (atomic-change-group
       (conn--without-conn-maps
         (if kill-region
             (let ((str (filter-buffer-substring beg end t)))
               (yank)
               (kill-new str))
           (delete-region beg end)
           (yank))
         ;; yank changes this-command to 'yank, fix that
         (setq this-command 'conn-yank-replace))))))

(defun conn-copy-region (start end &optional register)
  "Copy region between START and END as kill.

If REGISTER is given copy to REGISTER instead."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (register-read-with-preview "Copy to register: "))))
  (if register
      (if (bound-and-true-p rectangle-mark-mode)
          (copy-rectangle-to-register register start end)
        (copy-to-register register start end)
        (when (called-interactively-p 'interactive)
          (pulse-momentary-highlight-region start end)))
    (if (bound-and-true-p rectangle-mark-mode)
        (copy-rectangle-as-kill start end)
      (copy-region-as-kill start end)
      (when (called-interactively-p 'interactive)
        (pulse-momentary-highlight-region start end)))))

(defun conn-completing-yank-replace (&optional arg)
  "Replace region with result of `yank-from-kill-ring'.

If ARG is non-nil `kill-region' instead of `delete-region'."
  (interactive "P")
  (pcase (conn-bounds-of 'region nil)
    ((conn-bounds `(,beg . ,end) (list 'conn-check-bounds))
     (let ((ov (make-overlay beg end))
           exchange)
       (overlay-put ov 'conn-overlay t)
       (unwind-protect
           (progn
             (when (setq exchange (= (point) beg))
               (exchange-point-and-mark (not mark-active)))
             (overlay-put ov 'invisible t)
             (call-interactively (or (command-remapping 'yank-from-kill-ring)
                                     'yank-from-kill-ring))
             (if arg
                 (kill-region (overlay-start ov) (overlay-end ov))
               (delete-region (overlay-start ov) (overlay-end ov))))
         (when exchange
           (exchange-point-and-mark (not mark-active)))
         (delete-overlay ov))))))

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

(defun conn-shell-command-on-region (&optional arg)
  "Like `shell-command-on-region' but inverts the meaning of ARG."
  (interactive "P")
  (let ((current-prefix-arg (not arg)))
    (call-interactively 'shell-command-on-region)))

(defun conn-rgrep-region (beg end)
  "`rgrep' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (list (region-beginning)
         (region-end)))
  (let ((search-string
         (read-string "Search for: "
                      (regexp-quote (buffer-substring-no-properties beg end))
                      'grep-regexp-history)))
    (rgrep search-string)))

(defun conn-occur-region (beg end)
  "`occur' for the string contained in the region from BEG to END.
Interactively `region-beginning' and `region-end'."
  (interactive
   (list (region-beginning)
         (region-end)))
  (let ((search-string
         (read-string "Search for: "
                      (regexp-quote (buffer-substring-no-properties beg end))
                      'grep-regexp-history)))
    (occur search-string)))

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
      (if (length= windows 1)
          (other-window-prefix)
        (display-buffer-override-next-command
         (lambda (_ _)
           (cons (conn-prompt-for-window
                  (conn--get-windows nil 'nomini nil nil
                                     (lambda (win)
                                       (not (eq win (selected-window))))))
                 'reuse))
         nil "[select]")
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

(cl-defmethod conn-argument-display ((_arg conn-replace-thing-argument))
  (list (cl-call-next-method)
        "\\[project] project"))

(defvar-keymap conn-regexp-argument-map
  "q" 'regexp)

(defvar-keymap conn-delimited-argument-map
  "d" 'delimited)

(defvar conn-query-flag t
  "Default value for conn-query-flag.

If flag is t then `conn-replace' and `conn-regexp-replace'
will query before replacing from-string, otherwise just replace all
instances of from-string.")

(defvar-keymap conn-replace-query-map
  "C-RET" 'conn-replace-query-invert
  "C-<return>" 'conn-replace-query-invert)

(defvar-keymap conn-replace-from-map
  :parent conn-replace-query-map
  "C-M-;" 'conn-replace-insert-separator)

(defvar-keymap conn-replace-to-map
  :parent conn-replace-query-map)

(defun conn-replace-query-invert ()
  "Invert value of `conn-query-flag' and exit minibuffer."
  (interactive)
  (setq conn-query-flag (not conn-query-flag))
  (exit-minibuffer))

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

(defun conn-replace-read-default ()
  (let ((beg (region-beginning))
        (end (region-end)))
    (when (and (< (- end beg) 60)
               (<= end (save-excursion
                         (goto-char beg)
                         (pos-eol)))
               (not (use-region-p)))
      (buffer-substring-no-properties beg end))))

(defun conn-replace-read-regexp-default ()
  (when-let* ((default (conn-replace-read-default)))
    (regexp-quote default)))

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
          (thread-last
            (current-local-map)
            (make-composed-keymap conn-replace-from-map)
            (use-local-map)))
      (if regexp-flag
          (let ((query-replace-read-from-regexp-default
                 (if-let* ((def (conn-replace-read-regexp-default)))
                     def
                   query-replace-read-from-regexp-default)))
            (query-replace-read-from prompt regexp-flag))
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
  (conn--with-region-emphasis regions
    (let* ((conn-query-flag conn-query-flag)
           (from (conn--replace-read-from prompt
                                          regions
                                          regexp-flag
                                          delimited-flag))
           (to (if (consp from)
                   (prog1 (cdr from) (setq from (car from)))
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (thread-last
                         (current-local-map)
                         (make-composed-keymap conn-replace-to-map)
                         (use-local-map)))
                   (query-replace-read-to from prompt regexp-flag)))))
      (list from to
            (or delimited-flag
                (and (plist-member (text-properties-at 0 from)
                                   'isearch-regexp-function)
                     (get-text-property 0 'isearch-regexp-function from)))
            (and current-prefix-arg (eq current-prefix-arg '-))
            conn-query-flag))))

(cl-defgeneric conn-replace-do (thing
                                arg
                                transform
                                from-string
                                to-string
                                &optional
                                delimited
                                backward
                                query-flag
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
                               query-flag
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
            (perform-replace from-string to-string query-flag regexp-flag
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string query-flag regexp-flag
                         delimited nil nil beg end backward)))))

(cl-defmethod conn-replace-do ((_thing (eql project))
                               _arg
                               _transform
                               from
                               to
                               &optional
                               delimited
                               _backward
                               _query-flag
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
                     from-string
                     to-string
                     &optional
                     delimited
                     backward
                     query-flag
                     regexp-flag
                     subregions-p)
  "Perform a `replace-string' within the bounds of a thing."
  (interactive
   (conn-read-args (conn-replace-state
                    :prompt "Replace in Thing")
       ((`(,thing ,arg) (conn-replace-thing-argument))
        (transform (conn-transform-argument))
        (subregions-p (conn-subregions-argument (use-region-p)))
        (regexp-flag (conn-boolean-argument 'regexp
                                            conn-regexp-argument-map
                                            "regexp"))
        (delimited-flag (conn-boolean-argument 'delimited
                                               conn-delimited-argument-map
                                               "word delimited")))
     (let* ((bounds
             (ignore-errors
               (conn-transform-bounds (conn-bounds-of thing arg)
                                      transform)))
            (subregions (and subregions-p bounds
                             (conn-bounds-get bounds :subregions)))
            (common
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
              nil delimited-flag)))
       (append (list thing arg transform) common
               (list regexp-flag
                     (and subregions-p subregions t))))))
  (conn-replace-do thing
                   arg
                   transform
                   from-string
                   to-string
                   delimited
                   backward
                   query-flag
                   regexp-flag
                   subregions-p))

;;;;; Isearch

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

(cl-defmethod conn-argument-display ((_arg conn-isearch-thing-argument))
  (list (cl-call-next-method)
        "\\[multi-buffer] multi-buffer"
        "\\[multi-file] multi-file"
        "\\[project] project"))

(defun conn-isearch-dispatch-region ()
  (interactive)
  (isearch-done)
  (conn-dispatch))

(defun conn-isearch-yank-region ()
  "Yank the current region to isearch."
  (interactive)
  (isearch-yank-internal (lambda () (mark t))))

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
                                         subregions-p))

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
                    :prompt "Isearch in Thing")
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
                    :prompt "Isearch in Thing")
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

(defun conn-isearch-region-forward (thing
                                    arg
                                    transform
                                    &optional
                                    regexp
                                    subregions)
  "Isearch forward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument 'regexp
                                       conn-regexp-argument-map
                                       "regexp")))
     (list thing arg transform regexp subregions)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn-isearch-in-thing-do thing
                              arg
                              transform
                              :backward nil
                              :regexp regexp
                              :subregions-p subregions)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-isearch-region-backward (thing
                                     arg
                                     transform
                                     &optional
                                     regexp
                                     subregions)
  "Isearch backward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-isearch-thing-argument))
        (subregions (conn-subregions-argument))
        (transform (conn-transform-argument))
        (regexp (conn-boolean-argument 'regexp
                                       conn-regexp-argument-map
                                       "regexp")))
     (list thing arg transform regexp subregions)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn-isearch-in-thing-do thing
                              arg
                              transform
                              :backward t
                              :regexp regexp
                              :subregions-p subregions)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-isearch-exit-and-mark ()
  "`isearch-exit' and set region to match."
  (interactive)
  (isearch-done)
  (conn--push-ephemeral-mark isearch-other-end))

(defun conn-isearch-exit-other-end ()
  (interactive)
  (if isearch-forward
      (isearch-repeat-backward)
    (isearch-repeat-forward))
  (isearch-done))

;;;;; Transpose

(defvar conn--recursive-edit-transpose nil)

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE")

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-bounds-state))

(oclosure-define (conn-transpose-command
                  (:parent conn-dispatch-transpose))
  (buffer :type buffer)
  (point :type marker)
  (thing1 :type function))

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
  (conn-disable-repeating)
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

(defvar conn-transpose-reference
  (list
   (conn-reference-page "Transpose"
     "Transpose reads a THING command and transposes two of those THINGs. If
THING is `recursive-edit' then the current region and a region defined
within a recursive edit will be transposed.

Transpose defines some addition thing bindings:
"
     ((("line" conn-backward-line forward-line))
      (("symbol" forward-symbol))
      (("recursive-edit" recursive-edit))))))

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
  "Exchange regions defined by a thing command.

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
     ("surround" conn-surround))))

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
  (delete nil)
  (append nil)
  (register nil))

(cl-defsubst conn-kill-how-argument (&key delete append register)
  (declare (important-return-value t)
           (side-effect-free t))
  (cl-assert (memq delete '(nil delete copy)))
  (cl-assert (not (and (eq delete 'delete)
                       (or append register))))
  (make-conn-kill-how-argument :delete delete
                               :append append
                               :register register
                               :keymap conn-kill-how-map))

(cl-defmethod conn-argument-update ((arg conn-kill-how-argument)
                                    cmd
                                    update-fn)
  (cl-symbol-macrolet ((delete (conn-kill-how-argument-delete arg))
                       (register (conn-kill-how-argument-register arg))
                       (append (conn-kill-how-argument-append arg)))
    (pcase cmd
      ('append-next-kill
       (setf delete (if (eq delete 'delete) nil delete)
             append (pcase append
                      ('nil 'append)
                      ('append 'prepend)
                      ('prepend nil)))
       (funcall update-fn arg))
      ('delete
       (setf delete (if (eq delete 'delete) nil 'delete)
             register nil
             append nil)
       (funcall update-fn arg))
      ('copy
       (setf delete (unless (eq delete 'copy) 'copy))
       (funcall update-fn arg))
      ('register
       (setf delete (if (eq delete 'delete) nil delete)
             register (if register
                          nil
                        (register-read-with-preview "Register:")))
       (funcall update-fn arg)))))

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
                                    conn-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-argument-region-dwim))))))

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

(defun conn-kill-thing (cmd
                        arg
                        transform
                        &optional
                        append
                        delete
                        register
                        fixup-whitespace
                        check-bounds)
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
        (transform (conn-transform-argument))
        (`(,delete ,append ,register)
         (conn-kill-how-argument
          :append (and (eq last-command 'conn-kill-thing)
                       'prepend)
          :register (when current-prefix-arg
                      (register-read-with-preview "Register:"))))
        (fixup (conn-fixup-whitespace-argument
                (unless (region-active-p)
                  conn-kill-fixup-whitespace-default)))
        (check-bounds
         (conn-boolean-argument 'check-bounds
                                conn-check-bounds-argument-map
                                "check bounds"
                                conn-check-bounds-default)))
     (list thing arg transform append
           delete register fixup check-bounds)))
  (when (and (null append)
             (and (fboundp 'repeat-is-really-this-command)
                  (repeat-is-really-this-command))
             (eq last-command 'conn-kill-thing))
    (setq append 'append))
  (cl-callf and fixup-whitespace (null transform))
  (conn-kill-thing-do cmd arg transform append delete register fixup-whitespace check-bounds)
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
  (if register
      (pcase append
        ('nil
         (copy-to-register register beg end delete-flag t))
        ('prepend
         (prepend-to-register register beg end delete-flag))
        (_
         (append-to-register register beg end delete-flag)))
    (when (and append
               (xor (eq append 'prepend)
                    (< (point) (mark t))))
      (let ((omark (mark t)))
        (set-mark (point))
        (goto-char omark)))
    (let ((last-command (if append 'kill-region last-command)))
      (when (and append separator)
        (kill-append (conn-kill-separator-for-region separator)
                     (< (point) (mark t))))
      (if delete-flag
          (kill-region (mark t) (point) t)
        (copy-region-as-kill (mark t) (point) t)))))

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
  "TAB" 'separator)

(cl-defstruct (conn-separator-argument
               (:include conn-argument)
               ( :constructor conn-separator-argument
                 (value
                  &aux
                  (keymap conn-separator-argument-map)))))

(cl-defmethod conn-argument-update ((arg conn-separator-argument)
                                    cmd
                                    update-fn)
  (cl-symbol-macrolet ((value (conn-argument-value arg)))
    (pcase cmd
      ('separator
       (if (or (stringp value)
               (eq 'default value))
           (setf value nil)
         (setf value (if (conn-read-args-consume-prefix-arg)
                         (read-string "Separator: " nil nil nil t)
                       'default)))
       (funcall update-fn arg))
      ('register-separator
       (if (eq value 'register)
           (setf value nil)
         (setf value (get-register register-separator)))
       (funcall update-fn arg)))))

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
  (let ((conn-dispatch-amalgamate-undo t))
    (conn-read-args (conn-dispatch-bounds-state
                     :prefix arg
                     :prompt "Kill"
                     :reference (list conn-dispatch-thing-ref))
        ((`(,thing ,arg) (conn-thing-argument t))
         (`(,delete ,append ,register)
          (conn-kill-how-argument :append append
                                  :delete delete
                                  :register register))
         (transform (conn-dispatch-transform-argument transform))
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
                           ('append 'prepend)
                           ('prepend nil)))
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
               ('append
                (propertize
                 "append"
                 'face 'eldoc-highlight-function-argument))))))
        (let ((result nil)
              (strings nil))
          (conn-dispatch-setup
           (oclosure-lambda (conn-kill-action
                             (action-description "Kill"))
               ()
             (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                           (conn-select-target)))
               (with-selected-window window
                 (conn-dispatch-change-group)
                 (save-mark-and-excursion
                   (pcase (conn-bounds-of-dispatch thing arg pt)
                     ((and bounds
                           (conn-dispatch-bounds `(,beg . ,end)
                                                 `(,@transform
                                                   ,@(when check-bounds
                                                       (list 'conn-check-bounds)))))
                      (goto-char beg)
                      (conn--push-ephemeral-mark end)
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
           thing arg transform
           :repeat repeat
           :other-end :no-other-end
           :restrict-windows restrict-windows)
          (when strings
            (let ((sep (conn-kill-separator-for-strings (mapcar #'cdr strings)
                                                        separator)))
              (pcase-dolist (`(,append . ,string) (nreverse strings))
                (setq result
                      (if append
                          (concat result (and result sep) string)
                        (concat string (and result sep) result))))
              (conn--kill-string result append register sep))))))))

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
       (goto-char beg)
       (save-mark-and-excursion
         (conn--push-ephemeral-mark end)
         (if delete
             (delete-region beg end)
           (conn--kill-region beg end t append register)))
       (when fixup-whitespace
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
                                    cmd update-fn)
  (pcase cmd
    ('append-next-kill
     (setf (conn-copy-how-argument-append arg)
           (pcase (conn-copy-how-argument-append arg)
             ('nil 'append)
             ('append 'prepend)
             ('prepend nil)))
     (funcall update-fn arg))
    ('register
     (setf (conn-copy-how-argument-register arg)
           (if (conn-copy-how-argument-register arg)
               nil
             (register-read-with-preview "Register:")))
     (funcall update-fn arg))))

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
                                    conn-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-argument-region-dwim))))))

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
  "Copy THING at point."
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
     (save-mark-and-excursion
       (goto-char beg)
       (conn--push-ephemeral-mark end)
       (conn--kill-region beg end nil append register))
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
                   :reference (list conn-dispatch-thing-ref))
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
                         ('append 'prepend)
                         ('prepend nil)))
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
             ('append
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
                    (goto-char beg)
                    (conn--push-ephemeral-mark end)
                    (push (cons append (funcall region-extract-function nil))
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

;;;;; How Many

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
  (interactive
   (conn-read-args (conn-how-many-state
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
  (interactive
   (conn-read-args (conn-comment-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-comment-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-comment-thing-do thing arg transform))

;;;;; Duplicate

(conn-define-state conn-duplicate-state (conn-read-thing-state)
  :lighter "DUPLICATE")

(define-keymap
  :keymap (conn-get-state-map 'conn-duplicate-state)
  "c" 'copy-from-above-command)

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
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-toggle-padding ()
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-repeat-comment ()
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-delete-repeat ()
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn-duplicate-indent-repeat ()
  (interactive)
  (user-error "Not repeating duplicate"))

(defun conn--duplicate-subr (beg end &optional repeat)
  (deactivate-mark)
  (let* ((regions (list (make-overlay beg end nil t)))
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
      (save-excursion
        (goto-char end)
        (dotimes (_ repeat) (dup)))
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
       (format "%s repeat; %s newline; %s indent; %s comment; %s delete"
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
     (conn--duplicate-subr beg end repeat))))

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
                                    conn-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-argument-region-dwim))))))

(defun conn-duplicate-thing (thing arg transform &optional repeat)
  "Duplicate the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive
   (conn-read-args (conn-duplicate-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-duplicate-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing arg transform
           (prefix-numeric-value current-prefix-arg))))
  (conn-duplicate-thing-do thing arg transform repeat))

;;;;; Recenter

(defcustom conn-recenter-pulse t
  "Momentarily highlight region after `conn-recenter-on-region'."
  :group 'conn
  :type 'boolean)

(defvar conn-recenter-positions
  (list 'center 'top 'bottom)
  "Cycle order for `conn-recenter-on-region'.")

(defun conn-recenter-on-region ()
  "Recenter the screen on the current region.

Repeated invocations scroll the window according to the ordering
of `conn-recenter-positions'."
  (interactive)
  (if (eq this-command last-command)
      (put this-command :conn-positions
           (let ((ps (conn--command-property :conn-positions)))
             (append (cdr ps) (list (car ps)))))
    (put this-command :conn-positions conn-recenter-positions))
  (let ((beg (region-beginning))
        (end (region-end)))
    (pcase (car (conn--command-property :conn-positions))
      ('center
       (save-excursion
         (forward-line
          (if (> (point) (mark t))
              (- (/ (count-lines beg end) 2))
            (/ (count-lines beg end) 2)))
         (recenter))
       (when (not (pos-visible-in-window-p (point)))
         (if (> (point) (mark t))
             (recenter -1)
           (recenter 0))))
      ('top
       (save-excursion
         (goto-char beg)
         (recenter 0)))
      ('bottom
       (save-excursion
         (goto-char end)
         (recenter -1))))
    (when (and conn-recenter-pulse
               (not (region-active-p)))
      (pulse-momentary-highlight-region beg end))))

(defun conn-recenter-on-region-other-window ()
  "Recenter the current region in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (conn-recenter-on-region)))

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
  "Change region defined by CMD and ARG."
  (interactive
   (conn-read-args (conn-change-state
                    :prompt "Thing"
                    :reference conn-change-reference)
       ((`(,thing ,arg) (conn-change-thing-argument))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (conn-change-thing-do cmd arg transform))

(provide 'conn-commands)
