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
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")

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
      (pulse-momentary-highlight-one-line start))))
(put 'conn-scroll-down 'scroll-command t)

(defun conn-scroll-up (&optional arg)
  "`scroll-up-command' leaving point at the same relative window position.

Pulses line that was the last visible line before scrolling."
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (progn (beep) (message "End of buffer"))
    (let ((end (window-end)))
      (scroll-up arg)
      (pulse-momentary-highlight-one-line (1- end)))))
(put 'conn-scroll-up 'scroll-command t)

(defun conn-goto-string-backward (string)
  "Go to the first visible occurrence backward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (let ((conn-target-window-predicate nil)
               (pt (point)))
           (conn--read-string-with-timeout
            (lambda (beg _end) (< (window-start) beg pt))))))
  (let ((case-fold-search (conn--string-no-upper-case-p string)))
    (with-restriction (window-start) (window-end)
      (when-let* ((pos (or (save-excursion
                             (backward-char)
                             (cl-loop while (search-backward string nil t)
                                      when (conn--region-visible-p (match-beginning 0)
                                                                   (match-end 0))
                                      return (match-beginning 0)))
                           (user-error "\"%s\" not found." string))))
        (goto-char pos)))))

(defun conn-goto-string-forward (string)
  "Go to the first visible occurrence forward of STRING in buffer.

When called interactively reads STRING with timeout
`conn-read-string-timeout'."
  (interactive
   (list (let ((conn-target-window-predicate nil)
               (pt (point)))
           (conn--read-string-with-timeout
            (lambda (beg _end) (< pt beg (window-end)))))))
  (with-restriction (window-start) (window-end)
    (let ((case-fold-search (conn--string-no-upper-case-p string)))
      (when-let* ((pos (or (save-excursion
                             (forward-char)
                             (cl-loop while (search-forward string nil t)
                                      when (conn--region-visible-p (match-beginning 0)
                                                                   (match-end 0))
                                      return (match-beginning 0)))
                           (user-error "\"%s\" not found." string))))
        (goto-char pos)))))

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

;;;;; Replace

(conn-define-state conn-replace-state (conn-read-thing-state)
  :lighter "REPLACE")

(defvar conn-query-flag nil
  "Default value for conn-query-flag.

If flag is t then `conn-replace' and `conn-regexp-replace'
will query before replacing from-string, otherwise just replace all
instances of from-string.")

(defvar-keymap conn-replace-query-map
  "C-RET" 'conn-query-replace
  "C-<return>" 'conn-query-replace)

(defvar-keymap conn-replace-from-map
  :parent conn-replace-query-map
  "C-M-;" 'conn-replace-insert-separator)

(defvar-keymap conn-replace-to-map
  :parent conn-replace-query-map)

(defun conn-query-replace ()
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

(defun conn--replace-read-from ( prompt regions
                                 &optional regexp-flag delimited-flag)
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

(defun conn--replace-read-args ( prompt regexp-flag regions
                                 &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (conn--with-region-emphasis regions
    (save-mark-and-excursion
      (let* ((conn-query-flag conn-query-flag)
             (delimited-flag (and current-prefix-arg
                                  (not (eq current-prefix-arg '-))))
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
              conn-query-flag)))))

(defun conn-replace ( thing-mover arg transform from-string to-string
                      &optional delimited backward query-flag subregions-p)
  "Perform a `replace-string' within the bounds of a thing."
  (interactive
   (conn-read-args (conn-replace-state
                    :prompt "Replace in Thing")
       ((`(,thing-mover ,arg) (conn-thing-argument t))
        (transform (conn-transform-argument))
        (subregions-p (conn-subregions-argument (use-region-p))))
     (let* ((bounds (conn-transform-bounds (conn-bounds-of thing-mover arg)
                                           transform))
            (subregions (and subregions-p
                             (conn-bounds-get bounds :subregions)))
            (common
             (conn--replace-read-args
              (concat "Replace"
                      (if current-prefix-arg
                          (if (eq current-prefix-arg '-) " backward" " word")
                        ""))
              nil (if (and subregions-p subregions)
                      (cl-loop for bound in subregions
                               collect (conn-bounds bound))
                    (list (conn-bounds bounds))))))
       (append (list thing-mover arg transform) common
               (list (and subregions-p subregions t))))))
  (pcase-let* (((and bounds (conn-bounds `(,beg . ,end) transform))
                (or (conn-bounds-of 'conn-bounds-of nil)
                    (conn-bounds-of thing-mover arg))))
    (deactivate-mark t)
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
            (perform-replace from-string to-string query-flag nil
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string query-flag nil
                         delimited nil nil beg end backward)))))

(defun conn-regexp-replace ( thing-mover arg transform from-string to-string
                             &optional delimited backward query-flag subregions-p)
  "Perform a `regexp-replace' within the bounds of a thing."
  (interactive
   (conn-read-args (conn-replace-state
                    :prompt "Replace Regexp in Thing")
       ((`(,thing-mover ,arg) (conn-thing-argument t))
        (transform (conn-transform-argument))
        (subregions-p (conn-subregions-argument t)))
     (let* ((bounds (conn-transform-bounds (conn-bounds-of thing-mover arg)
                                           transform))
            (subregions (and subregions-p
                             (conn-bounds-get bounds :subregions)))
            (common
             (conn--replace-read-args
              (concat "Replace"
                      (if current-prefix-arg
                          (if (eq current-prefix-arg '-)
                              " backward"
                            " word")
                        ""))
              t (if (and subregions-p subregions)
                    (cl-loop for bound in subregions
                             collect (conn-bounds bound))
                  (list (conn-bounds bounds))))))
       (append (list thing-mover arg transform) common
               (list (and subregions-p subregions t))))))
  (pcase-let* (((and (conn-bounds `(,beg . ,end) transform)
                     bounds)
                (or (conn-bounds-of 'conn-bounds-of nil)
                    (conn-bounds-of thing-mover arg))))
    (deactivate-mark t)
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
            (perform-replace from-string to-string query-flag t
                             delimited nil nil beg end backward t))
        (perform-replace from-string to-string query-flag t
                         delimited nil nil beg end backward)))))

;;;;; Command Registers

(cl-defstruct (conn-command-register)
  (command nil :read-only t))

(cl-defmethod register-val-jump-to ((val conn-command-register) _arg)
  (let ((cmd (conn-command-register-command val)))
    (apply #'funcall-interactively
           (car cmd)
           (mapcar (lambda (e) (eval e t)) (cdr cmd)))))

(cl-defmethod register-val-describe ((val conn-command-register) _arg)
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

(cl-defmethod register-val-jump-to ((val conn-tab-register) _arg)
  (when-let* ((frame (conn-tab-register-frame val))
              (index (and (frame-live-p frame)
                          (with-selected-frame (conn-tab-register-frame val)
                            (conn--get-tab-index-by-cookie
                             (conn-tab-register-cookie val))))))
    (select-frame-set-input-focus frame)
    (tab-bar-select-tab (1+ index))))

(cl-defmethod register-val-describe ((val conn-tab-register) _arg)
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

;;;;; Isearch Commands

(conn-define-state conn-isearch-state (conn-read-thing-state)
  :lighter "ISEARCH-IN")

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

(cl-defun conn--isearch-in-thing (thing-cmd thing-arg &key backward regexp subregions-p)
  (pcase-let* ((bounds
                (conn-bounds-of thing-cmd thing-arg))
               (regions
                (mapcar (pcase-lambda (`(,beg . ,end))
                          (cons (conn--create-marker beg)
                                (conn--create-marker end nil t)))
                        (or (if (and subregions-p (conn-bounds-get bounds :subregions))
                                (conn--merge-overlapping-regions
                                 (cl-loop for bound in (conn-bounds-get bounds :subregions)
                                          collect (conn-bounds bound))
                                 t)
                              (list (conn-bounds bounds))))))
               (depth (recursion-depth))
               (in-regions-p (lambda (beg end)
                               (or (/= depth (recursion-depth))
                                   (cl-loop for (nbeg . nend) in regions
                                            thereis (<= nbeg beg end nend)))))
               (thing (upcase (symbol-name (or (conn-command-thing thing-cmd)
                                               thing-cmd))))
               (prefix (concat "[in " thing "] ")))
    (letrec ((setup
              (lambda ()
                (when (= depth (recursion-depth))
                  (add-function :after-while (local 'isearch-filter-predicate)
                                in-regions-p `((isearch-message-prefix . ,prefix)))
                  (remove-hook 'isearch-mode-hook setup t))))
             (cleanup
              (lambda ()
                (if (and (= depth (recursion-depth))
                         (not isearch-suspended))
                    (remove-hook 'isearch-mode-end-hook cleanup t)
                  (add-hook 'isearch-mode-hook setup nil t))
                (remove-function (local 'isearch-filter-predicate)
                                 in-regions-p))))
      (add-hook 'isearch-mode-end-hook cleanup nil t))
    (add-function :after-while (local 'isearch-filter-predicate) in-regions-p
                  `((isearch-message-prefix . ,prefix)))
    (if backward
        (isearch-backward regexp t)
      (isearch-forward regexp t))))

(defun conn-isearch-forward (thing-cmd thing-arg &optional regexp subregions-p)
  "Isearch forward within the bounds of a thing."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Isearch in Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing thing-arg current-prefix-arg subregions)))
  (conn--isearch-in-thing thing-cmd thing-arg
                          :backward nil
                          :regexp regexp
                          :subregions-p subregions-p))

(defun conn-isearch-backward (thing-cmd thing-arg &optional regexp subregions-p)
  "Isearch backward within the bounds of a thing."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Isearch in Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing thing-arg current-prefix-arg subregions)))
  (conn--isearch-in-thing thing-cmd thing-arg
                          :backward t
                          :regexp regexp
                          :subregions-p subregions-p))

(defun conn-isearch-region-forward (thing-cmd thing-arg &optional regexp)
  "Isearch forward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument t)))
     (list thing thing-arg current-prefix-arg)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg
                            :backward nil
                            :regexp regexp)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-isearch-region-backward (thing-cmd thing-arg &optional regexp)
  "Isearch backward for region from BEG to END.

Interactively `region-beginning' and `region-end'."
  (interactive
   (conn-read-args (conn-isearch-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument t)))
     (list thing thing-arg current-prefix-arg)))
  (let ((string (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (conn--isearch-in-thing thing-cmd thing-arg
                            :backward t
                            :regexp regexp)
    (with-isearch-suspended
     (setq isearch-new-string (if regexp (regexp-quote string) string)
           isearch-new-message (mapconcat #'isearch-text-char-description
                                          isearch-new-string "")))))

(defun conn-multi-isearch-project ()
  "Perform a `multi-isearch' within the files of a project."
  (interactive)
  (require 'project)
  (multi-isearch-files
   (seq-uniq (cons (buffer-file-name)
                   (project-files (project-current)))
             'file-equal-p)))

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
       ((`(,thing ,thing-arg) (conn-thing-argument t))
        (transform (conn-transform-argument)))
     (list thing thing-arg transform)))
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

;;;;; Transpose

(conn-define-state conn-transpose-state (conn-read-thing-state)
  :lighter "TRANSPOSE"
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-transpose-command)))

(conn-define-state conn-dispatch-transpose-state
    (conn-dispatch-bounds-state))

(oclosure-define (conn-transpose-argument
                  (:parent conn-thing-argument)))

(defun conn-transpose-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-transpose-argument
                    (required t)
                    (recursive-edit t))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(defun conn--transpose-recursive-message ()
  (message
   (substitute-command-keys
    (concat
     "Define region. "
     "Press \\[exit-recursive-edit] to end and use current region. "
     "Press \\[abort-recursive-edit] to abort."))))

(defvar conn--transpose-eldoc-prev-msg-fn)

(define-minor-mode conn-transpose-recursive-edit-mode
  "Find a region to transpose in a recursive edit."
  :global t
  :group 'conn
  (if conn-transpose-recursive-edit-mode
      (progn
        (setq conn--transpose-eldoc-prev-msg-fn eldoc-message-function
              eldoc-message-function #'ignore)
        (add-hook 'post-command-hook 'conn--transpose-recursive-message))
    (setq eldoc-message-function conn--transpose-eldoc-prev-msg-fn
          conn--transpose-eldoc-prev-msg-fn nil)
    (remove-hook 'post-command-hook 'conn--transpose-recursive-message)))

(oclosure-define (conn-transpose-command
                  (:parent conn-dispatch-transpose))
  (buffer :type buffer)
  (point :type marker)
  (thing1 :type function))

(cl-defmethod conn-perform-dispatch ((action conn-transpose-command)
                                     thing
                                     thing-arg
                                     &key &allow-other-keys)
  (conn-perform-dispatch-loop nil
    (pcase-let* ((`(,pt ,win ,thing-override)
                  (save-mark-and-excursion
                    (conn-dispatch-select-target))))
      (funcall action win pt
               (or thing-override thing)
               thing-arg))))

(cl-defgeneric conn-perform-transpose (cmd arg)
  (declare (conn-anonymous-thing-property :transpose-op)))

(cl-defmethod conn-perform-transpose :before (_cmd _arg)
  (conn-make-command-repeatable))

(cl-defmethod conn-perform-transpose (cmd arg)
  (pcase cmd
    ((guard (use-region-p))
     (deactivate-mark t)
     (pcase-let ((beg1 (region-beginning))
                 (end1 (region-end))
                 ((conn-bounds `(,beg2 . ,end2))
                  (conn-bounds-of cmd arg)))
       (transpose-regions beg1 end1 beg2 end2)))
    ((let 0 arg)
     (deactivate-mark t)
     (pcase-let* ((thing (conn-command-thing cmd))
                  (`(,beg1 . ,end1) (if (region-active-p)
                                        (cons (region-beginning) (region-end))
                                      (bounds-of-thing-at-point thing)))
                  (`(,beg2 . ,end2) (save-excursion
                                      (goto-char (mark t))
                                      (bounds-of-thing-at-point thing))))
       (transpose-regions beg1 end1 beg2 end2)))
    ((let (and thing (pred identity))
       (conn-command-thing cmd))
     (deactivate-mark t)
     (transpose-subr (lambda (N) (forward-thing thing N))
                     (prefix-numeric-value arg)))
    (_ (error "Invalid transpose mover"))))

(cl-defmethod conn-perform-transpose ((cmd (conn-thing isearch)) arg)
  (pcase-let* ((bounds (conn-bounds-of cmd arg))
               ((conn-bounds `(,beg1 . ,end1))
                bounds)
               ((conn-bounds `(,beg2 . ,end2))
                (conn-bounds-of (conn-bounds-thing bounds)
                                (conn-bounds-arg bounds))))
    (transpose-regions beg1 end1 beg2 end2)))

(cl-defmethod conn-perform-transpose ((_cmd (conn-thing recursive-edit-thing)) _arg)
  (deactivate-mark t)
  (let ((bounds1 (cons (region-beginning) (region-end)))
        (buf (current-buffer)))
    (conn-transpose-recursive-edit-mode 1)
    (unwind-protect
        (conn-with-recursive-stack 'conn-bounds-of-recursive-edit-state
          (recursive-edit))
      (conn-transpose-recursive-edit-mode -1))
    (conn--dispatch-transpose-subr
     buf (car bounds1) (conn-anonymous-thing
                         'region
                         :bounds-op ( :method (_)
                                      (conn-make-bounds 'region nil bounds1)))
     (current-buffer) (point) (let ((bounds2 (cons (region-beginning) (region-end))))
                                (conn-anonymous-thing
                                  'region
                                  :bounds-op ( :method (_)
                                               (conn-make-bounds 'region nil bounds2))))
     nil)))

(cl-defmethod conn-perform-transpose ((_cmd (conn-thing dispatch)) arg)
  (while
      (condition-case err
          (progn
            (conn-read-args (conn-dispatch-transpose-state
                             :prompt "Transpose Dispatch"
                             :prefix arg)
                ((`(,thing ,thing-arg) (conn-thing-argument t))
                 (restrict-windows (conn-dispatch-restrict-windows-argument)))
              (conn-perform-dispatch
               (oclosure-lambda
                   (conn-transpose-command
                    (description "Transpose")
                    (no-history t)
                    (buffer (current-buffer))
                    (point (point))
                    (thing1
                     (when (use-region-p)
                       (conn-anonymous-thing
                         'region
                         :bounds-op (let ((bounds (conn-make-bounds
                                                   'region nil
                                                   (cons (region-beginning)
                                                         (region-end)))))
                                      (:method (_self _arg) bounds)))))
                    (window-predicate
                     (lambda (win)
                       (not (buffer-local-value 'buffer-read-only
                                                (window-buffer win))))))
                   (window2 pt2 thing2 thing-arg)
                 (conn--dispatch-transpose-subr
                  buffer point (or thing1 thing2)
                  (window-buffer window2) pt2 thing2
                  thing-arg))
               thing thing-arg nil
               :other-end :no-other-end
               :restrict-windows restrict-windows))
            nil)
        ;; TODO: make this display somehow
        (user-error (message "%s" (cadr err)) t))))

(defvar conn-transpose-reference
  (list (conn-reference-page "Transpose"
          "Transpose reads a THING command and transposes two of those THINGs. If
THING is `recursive-edit' then the current region and a region defined
within a recursive edit will be transposed.

Transpose defines some addition thing bindings:
"
          ((("line" conn-backward-line forward-line))
           (("symbol" forward-symbol))
           (("defun" (:eval (conn-quick-ref-find-remap
                             conn-end-of-defun-remap
                             (conn-get-state-map 'conn-transpose-state)))))
           (("recursive-edit" recursive-edit))))))

(defun conn-transpose-things (mover arg)
  "Exchange regions defined by a thing command.

With argument ARG 0, exchange the things at point and mark.

If MOVER is \\='recursive-edit then exchange the current region and the
region after a `recursive-edit'."
  (interactive
   (conn-read-args (conn-transpose-state
                    :prompt "Transpose"
                    :prefix current-prefix-arg
                    :reference conn-transpose-reference)
       ((`(,thing ,thing-arg) (conn-transpose-argument)))
     (list thing thing-arg)))
  (when conn-transpose-recursive-edit-mode
    (user-error "Recursive call to conn-transpose-things"))
  (conn-perform-transpose mover arg))

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

(defun conn-join-lines (thing-mover thing-arg transform &optional subregions-p)
  "`delete-indentation' in region from START and END."
  (interactive
   (conn-read-args (conn-join-lines-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing thing-arg transform subregions)))
  (save-mark-and-excursion
    (pcase (conn-bounds-of thing-mover thing-arg)
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

(defun conn-thing-to-narrow-ring ( thing-cmd thing-arg thing-transform
                                   &optional subregions-p)
  "Push thing regions to narrow ring."
  (interactive
   (conn-read-args (conn-narrow-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (subregions (conn-subregions-argument (use-region-p))))
     (list thing thing-arg transform subregions)))
  (pcase (conn-bounds-of thing-cmd thing-arg)
    ((and (guard subregions-p)
          (conn-bounds-get :subregions
                           thing-transform
                           (and subregions (pred identity))))
     (cl-loop for bound in (reverse subregions)
              for (b . e) = (conn-bounds bound)
              do (conn--narrow-ring-record b e)))
    ((conn-bounds `(,beg . ,end) thing-transform)
     (conn--narrow-ring-record beg end))))

(defun conn--narrow-ring-record (beg end)
  (unless (conn-ring-p conn-narrow-ring)
    (setq conn-narrow-ring
          (conn-make-ring conn-narrow-ring-max
                          :cleanup (pcase-lambda (`(,b . ,e))
                                     (set-marker b nil)
                                     (set-marker e nil))
                          :copier (pcase-lambda (`(,b . ,e))
                                    (cons (copy-marker (marker-position b))
                                          (copy-marker (marker-position e)))))))
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
             (length conn-narrow-ring))))

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

(defun conn--narrow-to-region-1 (beg end &optional record)
  (narrow-to-region beg end)
  (when record (conn--narrow-ring-record beg end)))

(defun conn--narrow-indirect (beg end &optional record)
  "Narrow from BEG to END in an indirect buffer."
  (let* ((line-beg (line-number-at-pos beg))
         (linenum (- (line-number-at-pos end) line-beg))
         (name (format "%s@%s+%s - %s"
                       (buffer-name (current-buffer)) line-beg linenum
                       (thread-first
                         (buffer-substring-no-properties beg end)
                         (string-trim)
                         (substring 0 (min 20 (- end beg)))))))
    (clone-indirect-buffer-other-window name t)
    (conn--narrow-to-region-1 beg end record)
    (deactivate-mark)))

(defun conn-narrow-to-thing (thing-mover arg transform)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive
   (conn-read-args (conn-narrow-state
                    :prompt "Thing"
                    :prefix current-prefix-arg)
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument)))
     (list thing thing-arg transform)))
  (pcase (conn-bounds-of thing-mover arg)
    ((conn-bounds `(,beg . ,end) transform)
     (unless (and (<= beg (point) end)
                  (<= beg (mark t) end))
       (deactivate-mark))
     (conn--narrow-to-region-1 beg end t)
     (when (called-interactively-p 'interactive)
       (message "Buffer narrowed")))))

(defun conn-narrow-indirect (thing-mover arg transform)
  "Narrow to THING at point.

Interactively prompt for the keybinding of a command and use THING
associated with that command (see `conn-register-thing')."
  (interactive
   (conn-read-args (conn-narrow-state
                    :prompt "Thing"
                    :prefix current-prefix-arg)
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument)))
     (list thing thing-arg transform)))
  (pcase (conn-bounds-of thing-mover arg)
    ((conn-bounds `(,beg . ,end) transform)
     (conn--narrow-indirect beg end t)
     (when (called-interactively-p 'interactive)
       (message "Buffer narrowed indirect")))))

(defun conn-narrow-to-region (beg end &optional record)
  "Narrow to region from BEG to END and record it in `conn-narrow-ring'."
  (interactive (list (region-beginning) (region-end) (list t)))
  (conn--narrow-to-region-1 beg end record)
  (when (called-interactively-p 'interactive)
    (message "Buffer narrowed")))

;;;;;; Bounds of Narrow Ring

(defun conn--bounds-of-narrowings (_cmd _arg)
  (unless conn-narrow-ring
    (user-error "Narrow ring empty"))
  (cl-loop for (beg . end) in conn-narrow-ring
           minimize beg into narrow-beg
           maximize end into narrow-end
           collect (cons beg end) into narrowings
           finally return (cons (cons narrow-beg narrow-end)
                                narrowings)))

;; (conn-register-thing-commands
;;  'narrowing nil
;;  'narrow-to-region 'widen
;;  'conn-narrow-to-thing
;;  'conn-narrow-ring-prefix)

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

(defun conn-register-load-and-replace (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (list (register-read-with-preview "Load register: ")
         current-prefix-arg))
  (atomic-change-group
    (if (bound-and-true-p rectangle-mark-mode)
        (delete-rectangle (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))
    (conn-register-load reg arg)))

(defun conn-unset-register (register)
  "Unset REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil))

;;;;; Killing and Yanking Commands

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
  (interactive "p")
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

;;;;;; Kill Thing

(defvar conn-kill-option-ref
  (conn-reference-quote
    (("append" append-next-kill)
     ("delete" delete)
     ("register" register)
     ("fixup whitespace" fixup-whitespace)
     ("check bounds" check-bounds))))

(defvar conn-kill-reference
  (list (conn-reference-page "Kill"
          "Kill some things."
          (:heading "Options")
          (:eval (conn-quick-ref-to-cols
                  conn-kill-option-ref 3))
          (:heading "Transformations")
          (:eval (conn-quick-ref-to-cols
                  conn-transformations-quick-ref 3)))))

(defvar conn-kill-fixup-whitespace-default t)

(conn-define-state conn-kill-state (conn-read-thing-state)
  :lighter "KILL")

(define-keymap
  :keymap (conn-get-state-map 'conn-kill-state)
  "j" 'move-end-of-line)

(oclosure-define (conn-kill-argument
                  (:parent conn-thing-argument)))

(defun conn-kill-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-kill-argument
                    (required t)
                    (recursive-edit t)
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p)))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(oclosure-define (conn-kill-append-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-kill-append-map
  "@" 'append-next-kill)

(defun conn-kill-append-argument (&optional value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-kill-append-argument
                    (value value)
                    (keymap conn-kill-append-map))
      (self cmd)
    (pcase cmd
      ('append-next-kill
       (cond (value
              (conn-unset-argument self nil))
             ((> (prefix-numeric-value
                  (conn-read-args-consume-prefix-arg))
                 0)
              (conn-set-argument self t))
             (t
              (conn-set-argument self 'prepend))))
      ('delete
       (conn-unset-argument self nil))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-kill-append-argument)
                                       (_sym (eql append-next-kill)))
  t)

(cl-defmethod conn-argument-display ((arg conn-kill-append-argument))
  (substitute-command-keys
   (concat
    "\\[append-next-kill] "
    (pcase (conn-read-args-argument-value arg)
      ('nil "append")
      ('prepend
       (propertize
        "prepend"
        'face 'eldoc-highlight-function-argument))
      (_
       (propertize
        "append"
        'face 'eldoc-highlight-function-argument))))))

(oclosure-define (conn-delete-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-delete-argument-map
  "d" 'delete)

(defun conn-delete-argument (&optional value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-delete-argument
                    (value value)
                    (keymap conn-delete-argument-map))
      (self cmd)
    (pcase cmd
      ('delete (conn-set-argument self (null value)))
      ((or 'register 'append-next-kill)
       (conn-unset-argument self nil))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-delete-argument)
                                       (_sym (eql delete)))
  t)

(cl-defmethod conn-argument-display ((arg conn-delete-argument))
  (substitute-command-keys
   (concat
    "\\[delete]: "
    (if-let* ((ts (conn-read-args-argument-value arg)))
        (propertize
         "del"
         'face 'eldoc-highlight-function-argument)
      "del"))))

(oclosure-define (conn-register-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-register-argument-map
  "<" 'register)

(defun conn-register-argument (&optional value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-register-argument
                    (value value)
                    (keymap conn-register-argument-map))
      (self cmd)
    (pcase cmd
      ('register
       (if value
           (conn-unset-argument self nil)
         (conn-set-argument self (register-read-with-preview "Register:"))))
      ('delete
       (conn-unset-argument self nil))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-register-argument)
                                       (_sym (eql register)))
  t)

(cl-defmethod conn-argument-display ((arg conn-register-argument))
  (substitute-command-keys
   (concat
    "\\[register]: "
    (if-let* ((ts (conn-read-args-argument-value arg)))
        (propertize
         (format "reg <%c>" ts)
         'face 'eldoc-highlight-function-argument)
      "reg"))))

(defun conn-kill-thing ( cmd arg transform
                         &optional
                         append
                         delete
                         register
                         fixup-whitespace
                         check-bounds)
  (interactive
   (conn-read-args (conn-kill-state
                    :prompt "Thing"
                    :reference conn-kill-reference)
       ((`(,thing ,thing-arg) (conn-kill-argument))
        (transform (conn-transform-argument))
        (append (conn-kill-append-argument
                 (and (eq last-command 'conn-kill-thing)
                      'prepend)))
        (delete (conn-delete-argument))
        (register (conn-register-argument
                   (when (and current-prefix-arg
                              (not (equal '(4) current-prefix-arg)))
                     (register-read-with-preview "Register:"))))
        (fixup (conn-fixup-whitespace-argument
                (unless (region-active-p)
                  conn-kill-fixup-whitespace-default)))
        (check-bounds (conn-check-bounds-argument (listp current-prefix-arg))))
     (list thing thing-arg transform append
           delete register fixup check-bounds)))
  (when (and (null append)
             (and (fboundp 'repeat-is-really-this-command)
                  (repeat-is-really-this-command))
             (eq last-command 'conn-kill-thing))
    (setq append 'append))
  (cl-callf and fixup-whitespace (null transform))
  (when check-bounds (cl-callf append transform (list 'conn-check-bounds)))
  (conn-perform-kill cmd arg transform append delete register fixup-whitespace)
  (setq this-command 'conn-kill-thing))

(cl-defgeneric conn-kill-fixup-whitespace (bounds))

(cl-defmethod conn-kill-fixup-whitespace :after
  (_bounds &context (major-mode (derived-mode prog-mode)))
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
                                    (not (bobp)))
                         do (forward-line (if backward -1 1))
                         finally return i))))
    (when (or (looking-at (rx (syntax whitespace)))
              (looking-back (rx (syntax whitespace)) 1))
      (save-excursion
        (fixup-whitespace)))
    (when (and (conn-get-thing-property bounds :linewise)
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p (rx eol))))
      (dotimes (_ (min (empty-lines) (empty-lines t)))
        (join-line)))))

(defun conn--kill-region (beg end &optional delete-flag append register separator)
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

(cl-defgeneric conn-perform-kill ( cmd arg transform
                                   &optional
                                   append
                                   delete
                                   register
                                   fixup-whitespace)
  (declare (conn-anonymous-thing-property :kill-op)))

(cl-defmethod conn-perform-kill :before (&rest _)
  (conn-make-command-repeatable))

(cl-defmethod conn-perform-kill ((_cmd (conn-thing expansion)) &rest _)
  (conn-disable-repeating)
  (cl-call-next-method))

(oclosure-define (conn-prepend-argument
                  (:parent conn-read-args-argument)))

(defun conn-prepend-argument (&optional value)
  (oclosure-lambda (conn-prepend-argument
                    (value value))
      (self cmd)
    (if (eq cmd 'dispatch-other-end)
        (conn-set-argument self (not value))
      self)))

(cl-defmethod conn-argument-display ((arg conn-prepend-argument))
  (concat "\\[dispatch-other-end] "
          (propertize "prepend"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

(oclosure-define (conn-separator-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-separator-argument-keymap
  "+" 'register-separator
  "SPC" 'separator)

(defun conn-separator-argument (&optional initial-value)
  (oclosure-lambda (conn-separator-argument
                    (value initial-value)
                    (keymap conn-separator-argument-keymap))
      (self cmd)
    (pcase cmd
      ('separator
       (if (or (stringp value)
               (eq 'default value))
           (conn-unset-argument self nil)
         (conn-set-argument
          self
          (if (conn-read-args-consume-prefix-arg)
              (read-string "Separator: " nil nil nil t)
            'default))))
      ('register-separator
       (if (eq value 'register)
           (conn-unset-argument self nil)
         (conn-set-argument self (get-register register-separator))))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-separator-argument)
                                       sym)
  (or (eq sym 'separator)
      (eq sym 'register-separator)))

(cl-defmethod conn-argument-display ((arg conn-separator-argument))
  (concat "\\[separator]/\\[register-separator] separator"
          (when-let* ((sep (conn-read-args-argument-value arg)))
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

(cl-defmethod conn-perform-kill ((_cmd (conn-thing dispatch))
                                 arg transform
                                 &optional
                                 append
                                 delete
                                 register
                                 fixup-whitespace)
  (conn-disable-repeating)
  (let ((conn-dispatch-amalgamate-undo t))
    (conn-read-args (conn-dispatch-bounds-state
                     :prefix arg
                     :prompt "Kill"
                     :reference (list conn-dispatch-thing-ref))
        ((`(,thing ,thing-arg) (conn-thing-argument t))
         (repeat (conn-dispatch-repeat-argument))
         (prepend (conn-prepend-argument (eq append 'prepend)))
         (separator (when (not delete)
                      (conn-separator-argument 'default)))
         (restrict-windows (conn-dispatch-restrict-windows-argument t)))
      (conn-with-dispatch-event-handler _
          nil
          (lambda ()
            (when-let* ((binding
                         (where-is-internal 'dispatch-other-end
                                            conn-dispatch-read-event-map
                                            t)))
              (concat
               (propertize (key-description binding)
                           'face 'help-key-binding)
               " "
               (propertize
                "prepend"
                'face (when prepend
                        'eldoc-highlight-function-argument)))))
          (lambda (cmd)
            (when (eq cmd 'dispatch-other-end)
              (setq prepend (not prepend))
              (conn-dispatch-handle)))
        (let ((result nil)
              (strings nil))
          (conn-perform-dispatch
           (oclosure-lambda (conn-kill-action
                             (description "Kill"))
               (window pt thing thing-arg transform)
             (with-selected-window window
               (conn-dispatch-loop-undo-boundary)
               (save-mark-and-excursion
                 (goto-char pt)
                 (pcase (conn-bounds-of thing thing-arg)
                   ((and (conn-bounds `(,beg . ,end) transform)
                         bounds)
                    (goto-char (if conn-dispatch-other-end end beg))
                    (conn--push-ephemeral-mark (if conn-dispatch-other-end beg end))
                    (if delete
                        (delete-region beg end)
                      (push (cons prepend (funcall region-extract-function t))
                            strings)
                      (conn-dispatch-undo-case 90
                        (:undo
                         (pop strings)
                         (conn-dispatch-undo-pulse beg end))
                        (:cancel
                         (pop strings))))
                    (when fixup-whitespace
                      (funcall conn-kill-fixup-whitespace-function bounds)))))))
           thing thing-arg transform
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
              (conn--kill-string result append register sep))))))))

(cl-defmethod conn-perform-kill ( cmd arg transform
                                  &optional
                                  append
                                  delete
                                  register
                                  fixup-whitespace)
  (pcase (conn-transform-bounds (conn-bounds-of cmd arg) transform)
    ((and (conn-bounds `(,beg . ,end))
          bounds)
     (goto-char beg)
     (save-mark-and-excursion
       (conn--push-ephemeral-mark end)
       (if delete
           (delete-region beg end)
         (conn--kill-region beg end t append register)))
     (when fixup-whitespace
       (funcall conn-kill-fixup-whitespace-function bounds)))))

(cl-defmethod conn-perform-kill ((_cmd (conn-thing line)) &rest _)
  (let ((col (current-column)))
    (cl-call-next-method)
    (move-to-column col)))

(cl-defmethod conn-perform-kill ((cmd (conn-thing char))
                                 arg transform
                                 &optional
                                 append
                                 _delete
                                 register
                                 fixup-whitespace)
  (cl-call-next-method cmd arg transform append t register fixup-whitespace))

(cl-defmethod conn-perform-kill ( (cmd (conn-thing region))
                                  arg transform
                                  &optional
                                  _append
                                  delete
                                  register
                                  _fixup-whitespace)
  (conn-disable-repeating)
  (pcase (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :subregions nil
                           (and r (guard (length> r 1))))
          (conn-bounds `(,beg . ,end) transform))
     (cond (register (copy-rectangle-to-register register beg end t))
           (delete (delete-rectangle beg end))
           (t (kill-rectangle beg end))))
    (_ (cl-call-next-method))))

;;;;; Copy Thing

(conn-define-state conn-copy-state (conn-read-thing-state)
  :lighter "COPY")

(define-keymap
  :keymap (conn-get-state-map 'conn-copy-state)
  "j" 'move-end-of-line)

(defun conn-copy-thing (thing arg &optional transform append register)
  "Copy THING at point."
  (interactive
   (conn-read-args (conn-copy-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim))
        (transform (conn-transform-argument))
        (append (conn-kill-append-argument))
        (register (conn-register-argument
                   (when current-prefix-arg
                     (register-read-with-preview "Register:")))))
     (list thing thing-arg transform append register)))
  (conn-perform-copy thing arg transform append register))

(cl-defgeneric conn-perform-copy (cmd arg &optional transform append register)
  (declare (conn-anonymous-thing-property :copy-op)))

(cl-defmethod conn-perform-copy (cmd arg &optional transform append register)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (save-mark-and-excursion
       (goto-char beg)
       (conn--push-ephemeral-mark end)
       (conn--kill-region beg end nil append register))
     (unless executing-kbd-macro
       (pulse-momentary-highlight-region beg end)))))

(cl-defmethod conn-perform-copy ((_cmd (conn-thing expansion)) &rest _)
  (conn-disable-repeating)
  (cl-call-next-method))

(cl-defmethod conn-perform-copy ((_cmd (conn-thing dispatch)) arg
                                 &optional transform append register)
  (conn-disable-repeating)
  (conn-read-args (conn-dispatch-bounds-state
                   :prefix arg
                   :prompt "Copy"
                   :reference (list conn-dispatch-thing-ref))
      ((`(,thing ,thing-arg) (conn-thing-argument t))
       (repeat (conn-dispatch-repeat-argument))
       (prepend (conn-prepend-argument (eq append 'prepend)))
       (separator (conn-separator-argument 'default))
       (restrict-windows (conn-dispatch-restrict-windows-argument t)))
    (conn-with-dispatch-event-handler _
        nil
        (lambda ()
          (when-let* ((binding
                       (where-is-internal 'dispatch-other-end
                                          conn-dispatch-read-event-map
                                          t)))
            (concat
             (propertize (key-description binding)
                         'face 'help-key-binding)
             " "
             (propertize
              "prepend"
              'face (when prepend
                      'eldoc-highlight-function-argument)))))
        (lambda (cmd)
          (when (eq cmd 'dispatch-other-end)
            (setq prepend (not prepend))
            (conn-dispatch-handle)))
      (let ((result nil)
            (strings nil))
        (conn-perform-dispatch
         (oclosure-lambda (conn-kill-action
                           (description "Copy"))
             (window pt thing thing-arg transform)
           (with-selected-window window
             (conn-dispatch-loop-undo-boundary)
             (save-mark-and-excursion
               (goto-char pt)
               (pcase (conn-bounds-of thing thing-arg)
                 ((conn-bounds `(,beg . ,end) transform)
                  (goto-char (if conn-dispatch-other-end end beg))
                  (conn--push-ephemeral-mark (if conn-dispatch-other-end beg end))
                  (push (cons prepend (funcall region-extract-function nil))
                        strings)
                  (conn-dispatch-action-pulse beg end)
                  (conn-dispatch-undo-case 90
                    (:undo
                     (pop strings)
                     (conn-dispatch-undo-pulse beg end))
                    (:cancel
                     (pop strings))))))))
         thing thing-arg transform
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

;;;;; Comment Thing

(conn-define-state conn-comment-state (conn-read-thing-state)
  :lighter "COMMENT")

(cl-defgeneric conn-perform-comment (thing thing-arg transform)
  (declare (conn-anonymous-thing-property :comment-op)))

(cl-defmethod conn-perform-comment (thing thing-arg transform)
  (pcase (conn-bounds-of thing thing-arg)
    ((conn-bounds `(,beg . ,end) transform)
     (comment-or-uncomment-region beg end))))

(defun conn-comment-thing (thing thing-arg transform)
  (interactive
   (conn-read-args (conn-comment-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument)))
     (list thing thing-arg transform)))
  (conn-perform-comment thing thing-arg transform))

;;;;; Duplicate Thing

(conn-define-state conn-duplicate-state (conn-read-thing-state)
  :lighter "DUPLICATE")

(define-keymap
  :keymap (conn-get-state-map 'conn-duplicate-state)
  "c" 'copy-from-above-command)

(oclosure-define (conn-duplicate-thing
                  (:parent conn-thing-argument)))

(cl-defmethod conn-argument-predicate ((_arg conn-duplicate-thing)
                                       (_cmd (eql copy-from-above-command)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-duplicate-thing)
                                       (_cmd (conn-thing con-dispatch)))
  nil)

(defun conn-duplicate-thing-argument ()
  (oclosure-lambda (conn-duplicate-thing
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p))
                    (required t)
                    (recursive-edit t))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(cl-defgeneric conn-perform-duplicate (cmd arg transform &optional comment)
  (declare (conn-anonymous-thing-property :duplicate-op)))

(cl-defmethod conn-perform-duplicate (cmd arg transform &optional comment)
  (pcase (conn-bounds-of cmd arg)
    ((conn-bounds `(,beg . ,end) transform)
     (save-mark-and-excursion
       (let* ((region (buffer-substring-no-properties beg end))
              (multiline (seq-contains-p region ?\n))
              (padding (if multiline "\n" " "))
              (regexp (if multiline "\n" "[\t ]")))
         (goto-char beg)
         (insert-before-markers region)
         (unless (looking-back regexp 1)
           (insert-before-markers padding))
         (goto-char beg)))
     (when comment
       (comment-region beg end)))))

(cl-defmethod conn-perform-duplicate ((_cmd (eql copy-from-above-command))
                                      arg _transform _comment)
  (copy-from-above-command arg))

(oclosure-define (conn-duplicate-comment-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-duplicate-comment-argument-map
  "q" 'duplicate-comment)

(defun conn-duplicate-comment-argument (&optional value)
  (oclosure-lambda (conn-duplicate-comment-argument
                    (value value)
                    (keymap conn-duplicate-comment-argument-map))
      (self cmd)
    (pcase cmd
      ('duplicate-comment
       (conn-set-argument self (not value)))
      (_ self))))

(cl-defmethod conn-argument-predicate ((_arg conn-duplicate-comment-argument)
                                       (_cmd (eql duplicate-comment)))
  t)

(cl-defmethod conn-argument-display ((arg conn-duplicate-comment-argument))
  (concat
   "\\[duplicate-comment] "
   (propertize "comment"
               'face (when (conn-read-args-argument-value arg)
                       'eldoc-highlight-function-argument))))

(defun conn-duplicate (thing-mover thing-arg transform &optional comment)
  "Duplicate the region defined by a thing command.

With prefix arg N duplicate region N times."
  (interactive
   (conn-read-args (conn-duplicate-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
        (transform (conn-transform-argument))
        (comment (conn-duplicate-comment-argument)))
     (list thing thing-arg transform comment)))
  (conn-make-command-repeatable)
  (conn-perform-duplicate thing-mover thing-arg transform comment))

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

;;;;; Change

(conn-define-state conn-change-state (conn-kill-state)
  :lighter "CHANGE")

(define-keymap
  :keymap (conn-get-state-map 'conn-change-state)
  "w" 'quoted-insert
  "RET" 'conn-emacs-state-overwrite
  "M-RET" 'conn-emacs-state-overwrite-binary)

(cl-defgeneric conn-perform-change (cmd arg transform)
  (declare (conn-anonymous-thing-property :change-op)))

(cl-defmethod conn-perform-change (cmd arg transform)
  (pcase-let (((conn-bounds `(,beg . ,end) transform)
               (conn-bounds-of cmd arg)))
    (goto-char beg)
    (delete-region beg end)
    (if (eq 'conn-emacs-state (conn-peek-state))
        (conn-pop-state)
      (conn-push-state 'conn-emacs-state))))

(cl-defmethod conn-perform-change :extra "rectangle" ((_cmd (conn-thing region))
                                                      _arg _transform)
  (if (bound-and-true-p rectangle-mark-mode)
      (call-interactively #'string-rectangle)
    (cl-call-next-method)))

(cl-defmethod conn-perform-change ((_cmd (eql conn-emacs-state-overwrite))
                                   _arg _transform)
  (conn-emacs-state-overwrite))

(cl-defmethod conn-perform-change ((_cmd (eql quoted-insert))
                                   arg _transform)
  (atomic-change-group
    (delete-char 1)
    (conn-with-recursive-stack 'conn-emacs-state
      (quoted-insert (prefix-numeric-value arg)))))

(cl-defmethod conn-perform-change ((_cmd (eql conn-emacs-state-overwrite-binary))
                                   _arg _transform)
  (conn-emacs-state-overwrite-binary))

(oclosure-define (conn-change-argument
                  (:parent conn-thing-argument)))

(defun conn-change-argument ()
  (oclosure-lambda (conn-change-argument
                    (required t)
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p)))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-change-argument)
                                       sym)
  (or (eq sym 'conn-emacs-state-overwrite-binary)
      (eq sym 'conn-emacs-state-overwrite)
      (eq sym 'quoted-insert)
      (cl-call-next-method)))

(defun conn-change-thing (cmd arg transform)
  "Change region defined by CMD and ARG."
  (interactive
   (conn-read-args (conn-change-state
                    :prompt "Thing")
       ((`(,thing ,thing-arg) (conn-change-argument))
        (transform (conn-transform-argument 'conn-bounds-last)))
     (list thing thing-arg transform)))
  (conn-perform-change cmd arg transform))

;;;;; Transition Functions

(defvar conntext-state-hook nil)

(defun conntext-state ()
  (interactive)
  (run-hook-with-args-until-success 'conntext-state-hook))

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

(provide 'conn-commands)
