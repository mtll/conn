;;; conn-posframe.el --- Conn posframe extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") posframe conn)
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
;;; Code:

(require 'conn)
(require 'conn-quick-ref)
(require 'conn-things)
(require 'posframe)
(eval-when-compile
  (require 'cl-lib))

(defvar conn-read-pair-function)
(defvar kmacro-counter)
(defvar kmacro-initial-counter-value)
(defvar kmacro-ring)

(declare-function kmacro--keys "kmacro")
(declare-function conn--kmacro-display "conn-transient")

;;;; Posframe command displays

(defgroup conn-posframe nil
  "Conn posframes."
  :prefix "conn-posframe-"
  :group 'conn)

(defface conn-posframe-highlight
  '((t ( :inherit completions-highlight
         :extend t)))
  "Face for selection in Conn posframes."
  :group 'conn-posframe)

(defface conn-posframe-header
  '((t ( :inverse-video nil :extend t :bold t
         :box nil :underline (:style line :position t)
         :inherit header-line)))
  "Face for selection in Conn posframes."
  :group 'conn-posframe)

(defcustom conn-posframe-border-width 3
  "Border width for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-width 55
  "Minimum width for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defface conn-posframe-border
  '((((background light)) (:background "#000000"))
    (((background dark)) (:background "#999999")))
  "Border color for conn posframes."
  :group 'conn-posframe)

(defcustom conn-posframe-timeout nil
  "Timeout for conn posframes."
  :type (or 'integer 'nil)
  :group 'conn-posframe)

(defcustom conn-posframe-buffer-context-lines 5
  "Number of context lines for buffer cycling posframe."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-tab-poshandler
  'posframe-poshandler-frame-center
  "Timeout for conn posframes."
  :type 'symbol
  :group 'conn-posframe)

(defcustom conn-posframe-buffer-poshandler
  'posframe-poshandler-frame-center
  "Timeout for conn posframes."
  :type 'symbol
  :group 'conn-posframe)

;; From nerd-icons-corfu
(defconst conn-posframe--padding
  (if (display-graphic-p)
      (propertize " " 'display '(space :width 0.5))
    " ")
  "Padding for posframe.")

(defun conn-posframe--hide-pre ()
  (unless (advice-member-p 'conn-list-posframe this-command)
    (posframe-hide " *conn-list-posframe*")
    (remove-hook 'pre-command-hook 'conn-posframe--hide-pre)))

;; Adapted from window.el
(defun conn-posframe--next-buffers (&optional window)
  (let* ((window (window-normalize-window window t))
         (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
         (old-buffer (window-buffer window))
         (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond ((or (functionp switch-to-prev-buffer-skip)
                     (memq switch-to-prev-buffer-skip '(t visible 0)))
                 switch-to-prev-buffer-skip)
                (switch-to-prev-buffer-skip
                 frame)))
         found-buffers new-buffer killed-buffers skipped)
    (catch 'found
      (dolist (buffer next-buffers)
        (when (and (or (buffer-live-p buffer)
                       (not (setq killed-buffers
                                  (cons buffer killed-buffers))))
                   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
                   (assq buffer (window-prev-buffers window)))
          (if (switch-to-prev-buffer-skip-p skip window buffer)
              (setq skipped buffer)
            (setq new-buffer buffer)
            (cl-pushnew new-buffer found-buffers)
            (when (length= found-buffers conn-posframe-buffer-context-lines)
              (throw 'found t)))))
      (unless window-side
        (dolist (buffer (buffer-list frame))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
                     ;; Skip buffers whose names start with a space.
                     (not (eq (aref (buffer-name buffer) 0) ?\s))
                     ;; Skip buffers shown in a side window before.
                     (not (buffer-local-value 'window--sides-shown buffer))
                     (not (assq buffer (window-prev-buffers window))))
            (if (switch-to-prev-buffer-skip-p skip window buffer)
                (setq skipped (or skipped buffer))
              (setq new-buffer buffer)
              (cl-pushnew new-buffer found-buffers)
              (when (length= found-buffers conn-posframe-buffer-context-lines)
                (throw 'found t))))))
      (dolist (entry (reverse (window-prev-buffers window)))
        (when (and (not (eq new-buffer (car entry)))
                   (not (eq old-buffer (car entry)))
                   (setq new-buffer (car entry))
                   (or (buffer-live-p new-buffer)
                       (not (setq killed-buffers
                                  (cons new-buffer killed-buffers))))
                   (or (null pred) (funcall pred new-buffer)))
          (if (switch-to-prev-buffer-skip-p skip window new-buffer)
              (setq skipped (or skipped new-buffer))
            (cl-pushnew new-buffer found-buffers)
            (when (length= found-buffers conn-posframe-buffer-context-lines)
              (throw 'found t)))))
      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        (setq new-buffer skipped)
        (cl-pushnew new-buffer found-buffers)))
    (mapconcat
     (lambda (buf)
       (concat (if (fboundp 'nerd-icons-icon-for-buffer)
                   (concat conn-posframe--padding
                           (with-current-buffer buf
                             (nerd-icons-icon-for-buffer))
                           conn-posframe--padding)
                 conn-posframe--padding)
               (buffer-name buf)))
     (reverse found-buffers)
     "\n")))

;; Adapted from window.el
(defun conn-posframe--previous-buffers (&optional window)
  (let* ((window (window-normalize-window window t))
         (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
         (old-buffer (window-buffer window))
         (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond ((or (functionp switch-to-prev-buffer-skip)
                     (memq switch-to-prev-buffer-skip '(t visible 0)))
                 switch-to-prev-buffer-skip)
                (switch-to-prev-buffer-skip
                 frame)))
         found-buffers new-buffer killed-buffers skipped)
    (catch 'found
      (dolist (entry (window-prev-buffers window))
        (when (and (not (eq (car entry) old-buffer))
                   (setq new-buffer (car entry))
                   (or (buffer-live-p new-buffer)
                       (not (setq killed-buffers
                                  (cons new-buffer killed-buffers))))
                   (or (null pred) (funcall pred new-buffer))
                   (not (memq new-buffer next-buffers)))
          (if (switch-to-prev-buffer-skip-p skip window new-buffer)
              (setq skipped new-buffer)
            (cl-pushnew new-buffer found-buffers)
            (when (length= found-buffers conn-posframe-buffer-context-lines)
              (throw 'found t)))))
      (unless window-side
        (dolist (buffer (nreverse (buffer-list frame)))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
                     (not (eq (aref (buffer-name buffer) 0) ?\s))
                     (not (buffer-local-value 'window--sides-shown buffer))
                     (not (memq buffer next-buffers)))
            (if (switch-to-prev-buffer-skip-p skip window buffer)
                (setq skipped (or skipped buffer))
              (setq new-buffer buffer)
              (cl-pushnew new-buffer found-buffers)
              (when (length= found-buffers conn-posframe-buffer-context-lines)
                (throw 'found t))))))
      (dolist (buffer (reverse next-buffers))
        (when (and (or (buffer-live-p buffer)
                       (not (setq killed-buffers
                                  (cons buffer killed-buffers))))
                   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
                   (assq buffer (window-prev-buffers window)))
          (if (switch-to-prev-buffer-skip-p skip window buffer)
              (setq skipped (or skipped buffer))
            (setq new-buffer buffer)
            (cl-pushnew new-buffer found-buffers)
            (when (length= found-buffers conn-posframe-buffer-context-lines)
              (throw 'found t)))))
      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        (setq new-buffer skipped)
        (cl-pushnew new-buffer found-buffers)))
    (mapconcat
     (lambda (buf)
       (concat (if (fboundp 'nerd-icons-icon-for-buffer)
                   (concat conn-posframe--padding
                           (with-current-buffer buf
                             (nerd-icons-icon-for-buffer))
                           conn-posframe--padding)
                 conn-posframe--padding)
               (buffer-name buf)))
     found-buffers
     "\n")))

(defun conn-posframe--switch-buffer-display (&rest _)
  (when (and (not executing-kbd-macro)
             (when (symbolp this-command)
               (cl-loop for fn in (cons this-command (function-alias-p this-command))
                        thereis (advice-member-p 'conn-list-posframe fn))))
    (let* ((header (conn--with-work-buffer
                     (insert conn-posframe--padding
                             (if (fboundp 'nerd-icons-faicon)
                                 (concat (nerd-icons-faicon "nf-fa-buffer")
                                         conn-posframe--padding))
                             "Buffers\n")
                     (add-face-text-property (point-min) (point-max)
                                             'conn-posframe-header 'append)
                     (buffer-string)))
           (current (concat
                     (when (fboundp 'nerd-icons-icon-for-buffer)
                       (concat conn-posframe--padding
                               (nerd-icons-icon-for-buffer)))
                     conn-posframe--padding
                     (buffer-name (current-buffer))
                     "\n"))
           (prev (conn-posframe--previous-buffers))
           (next (conn-posframe--next-buffers)))
      (add-face-text-property 0 (length current)
                              'conn-posframe-highlight
                              'append current)
      (posframe-show
       " *conn-list-posframe*"
       :string (concat header prev "\n" current next)
       :left-fringe 0
       :right-fringe 0
       :width conn-posframe-width
       :poshandler conn-posframe-buffer-poshandler
       :timeout conn-posframe-timeout
       :border-width conn-posframe-border-width
       :border-color (face-background 'conn-posframe-border)
       :lines-truncate t))
    (add-hook 'pre-command-hook 'conn-posframe--hide-pre)))

(defun conn-posframe--switch-tab-display (&rest _)
  (when (and (not executing-kbd-macro)
             (cl-loop for fn in (cons this-command (function-alias-p this-command))
                      thereis (advice-member-p 'conn-list-posframe fn)))
    (posframe-show
     " *conn-list-posframe*"
     :string (concat
              (conn--with-work-buffer
                (insert (when (fboundp 'nerd-icons-mdicon)
                          (concat conn-posframe--padding
                                  (nerd-icons-mdicon "nf-md-tab")
                                  conn-posframe--padding))
                        "Tabs\n")
                (add-face-text-property (point-min) (point-max)
                                        'conn-posframe-header 'append)
                (buffer-string))
              (mapconcat
               (lambda (tab)
                 (concat
                  (if (eq (car tab) 'current-tab)
                      (propertize
                       (concat (alist-get 'name (cdr tab)) "\n")
                       'face 'conn-posframe-highlight)
                    (concat (alist-get 'name (cdr tab)) "\n"))))
               (reverse (funcall tab-bar-tabs-function))))
     :left-fringe 0
     :right-fringe 0
     :poshandler conn-posframe-tab-poshandler
     :timeout conn-posframe-timeout
     :border-width conn-posframe-border-width
     :border-color (face-background 'conn-posframe-border))
    (add-hook 'pre-command-hook 'conn-posframe--hide-pre)))

(defun conn-posframe--kmacro-ring-display-subr ()
  (require 'kmacro)
  (posframe-show
   " *conn-list-posframe*"
   :string (concat
            (propertize (format " %s Kmacro Ring\n"
                                (or (if defining-kbd-macro
                                        kmacro-counter
                                      kmacro-initial-counter-value)
                                    (format "[%s]" kmacro-counter)))
                        'face 'conn-posframe-header)
            (mapconcat
             (lambda (km)
               (conn--kmacro-display (kmacro--keys km)))
             (reverse (take (min 4 (floor (length kmacro-ring) 2))
                            (reverse kmacro-ring)))
             "\n")
            (when (length> kmacro-ring 1) "\n")
            (propertize (concat (conn--kmacro-display last-kbd-macro)
                                "\n")
                        'face 'conn-posframe-highlight)
            (mapconcat
             (lambda (km)
               (conn--kmacro-display (kmacro--keys km)))
             (take (min 4 (ceiling (length kmacro-ring) 2))
                   kmacro-ring)
             "\n")
            (when (length> kmacro-ring 1) "\n"))
   :left-fringe 0
   :right-fringe 0
   :poshandler conn-posframe-tab-poshandler
   :timeout conn-posframe-timeout
   :border-width conn-posframe-border-width
   :border-color (face-background 'conn-posframe-border)))

(defun conn-posframe--kmacro-ring-display (&rest _)
  (when (and (not executing-kbd-macro)
             (when (symbolp this-command)
               (cl-loop for fn in (cons this-command (function-alias-p this-command))
                        thereis (advice-member-p 'conn-list-posframe fn))))
    (conn-posframe--kmacro-ring-display-subr)
    (add-hook 'pre-command-hook #'conn-posframe--hide-pre)))

(defun conn-posframe--dispatch-ring-display-subr ()
  (let ((ring (conn-ring-list conn-dispatch-ring)))
    (cl-flet ((describe (d)
                (concat conn-posframe--padding
                        (conn-describe-dispatch d)
                        conn-posframe--padding)))
      (posframe-show
       " *conn-list-posframe*"
       :string (concat
                (conn--with-work-buffer
                  (insert conn-posframe--padding
                          (when (fboundp 'nerd-icons-faicon)
                            (concat (nerd-icons-faicon "nf-fa-buffer")
                                    conn-posframe--padding))
                          "Dispatch Ring\n")
                  (add-face-text-property (point-min) (point-max)
                                          'conn-posframe-header 'append)
                  (buffer-string))
                (mapconcat #'describe
                           (reverse (take (min 4 (floor (length (cdr ring)) 2))
                                          (cdr ring)))
                           "\n")
                (when (length> ring 2) "\n")
                (propertize (concat (describe (car ring)) "\n")
                            'face 'conn-posframe-highlight)
                (mapconcat #'describe
                           (take (min 4 (ceiling (length (cdr ring)) 2))
                                 (reverse (cdr ring)))
                           "\n"))
       :left-fringe 0
       :right-fringe 0
       :poshandler conn-posframe-tab-poshandler
       :timeout conn-posframe-timeout
       :border-width conn-posframe-border-width
       :border-color (face-background 'conn-posframe-border)))))

(defun conn-posframe--dispatch-ring-display (&rest _)
  (when (and (not executing-kbd-macro)
             (when (symbolp this-command)
               (cl-loop for fn in (cons this-command (function-alias-p this-command))
                        thereis (advice-member-p 'conn-list-posframe fn)))
             conn-dispatch-ring)
    (conn-posframe--dispatch-ring-display-subr)
    (add-hook 'pre-command-hook 'conn-posframe--hide-pre)))

;;;###autoload
(define-minor-mode conn-posframe-mode
  "Posframes for Conn."
  :global t
  :lighter ""
  (if conn-posframe-mode
      (progn
        (advice-add 'kmacro-cycle-ring-next :after
                    'conn-posframe--kmacro-ring-display
                    '((name . conn-list-posframe)))
        (advice-add 'kmacro-cycle-ring-previous :after
                    'conn-posframe--kmacro-ring-display
                    '((name . conn-list-posframe)))
        (advice-add 'previous-buffer :after
                    'conn-posframe--switch-buffer-display
                    '((name . conn-list-posframe)))
        (advice-add 'next-buffer :after
                    'conn-posframe--switch-buffer-display
                    '((name . conn-list-posframe)))
        (advice-add 'bury-buffer :after
                    'conn-posframe--switch-buffer-display
                    '((name . conn-list-posframe)))
        (advice-add 'unbury-buffer :after
                    'conn-posframe--switch-buffer-display
                    '((name . conn-list-posframe)))
        (advice-add 'conn-kill-this-buffer :after
                    'conn-posframe--switch-buffer-display
                    '((name . conn-list-posframe)))
        (advice-add 'tab-bar-new-tab :after
                    'conn-posframe--switch-tab-display
                    '((name . conn-list-posframe)))
        (advice-add 'tab-bar-switch-to-next-tab :after
                    'conn-posframe--switch-tab-display
                    '((name . conn-list-posframe)))
        (advice-add 'tab-bar-switch-to-prev-tab :after
                    'conn-posframe--switch-tab-display
                    '((name . conn-list-posframe)))
        (advice-add 'tab-bar-close-tab :after
                    'conn-posframe--switch-tab-display
                    '((name . conn-list-posframe)))
        (advice-add 'tab-bar-duplicate-tab :after
                    'conn-posframe--switch-tab-display
                    '((name . conn-list-posframe)))
        (advice-add 'conn-dispatch-cycle-ring-previous :after
                    'conn-posframe--dispatch-ring-display
                    '((name . conn-list-posframe)))
        (advice-add 'conn-dispatch-cycle-ring-next :after
                    'conn-posframe--dispatch-ring-display
                    '((name . conn-list-posframe)))
        (defvar conn-quick-ref-display-function)
        (setq conn-quick-ref-display-function 'conn--quick-ref-posframe))
    (advice-remove 'kmacro-cycle-ring-next 'conn-list-posframe)
    (advice-remove 'kmacro-cycle-ring-previous 'conn-list-posframe)
    (advice-remove 'previous-buffer 'conn-list-posframe)
    (advice-remove 'next-buffer 'conn-list-posframe)
    (advice-remove 'bury-buffer 'conn-list-posframe)
    (advice-remove 'unbury-buffer 'conn-list-posframe)
    (advice-remove 'conn-kill-this-buffer 'conn-list-posframe)
    (advice-remove 'tab-bar-new-tab 'conn-list-posframe)
    (advice-remove 'tab-bar-switch-to-next-tab 'conn-list-posframe)
    (advice-remove 'tab-bar-switch-to-prev-tab 'conn-list-posframe)
    (advice-remove 'tab-bar-duplicate-tab 'conn-list-posframe)
    (advice-remove 'tab-bar-close-tab 'conn-list-posframe)
    (defvar conn-quick-ref-display-function)
    (setq conn-quick-ref-display-function 'conn--quick-ref-minibuffer)))

;;;; Posframe window labels

(defface conn-posframe-window-label-face
  '((t (:inherit help-key-binding :height 4.5)))
  "Face for conn window label posframe."
  :group 'conn-faces)

(defvar conn-posframe-window-label-poshandler
  'posframe-poshandler-window-center)

(cl-defstruct (conn-posframe-window-label
               ( :constructor conn--make-posframe-window-label
                 (string window bufname overlay)))
  "Store the state for a window label."
  string window bufname overlay)

(defun conn--setup-posframe-window-label (window string)
  (let ((bufname (format " *conn-label-posfame-%s*" string)))
    (with-selected-window window
      (posframe-show
       bufname
       :string (propertize string 'face 'conn-posframe-window-label-face)
       :poshandler conn-posframe-window-label-poshandler)
      (let ((overlay (make-overlay (point-min) (point-max) (current-buffer))))
        (overlay-put overlay 'window window)
        (overlay-put overlay 'face 'shadow)
        (conn--make-posframe-window-label string window bufname overlay)))))

;;;###autoload
(defun conn-posframe-window-labels (windows)
  (conn--simple-window-labels)
  (cl-loop for win in windows
           collect (conn--setup-posframe-window-label
                    win (window-parameter win 'conn-label-string))))

(cl-defmethod conn-label-delete ((label conn-posframe-window-label))
  (delete-overlay (conn-posframe-window-label-overlay label))
  (posframe-hide (conn-posframe-window-label-bufname label)))

(cl-defmethod conn-label-reset ((label conn-posframe-window-label))
  (pcase-let (((cl-struct conn-posframe-window-label window string bufname overlay)
               label))
    (overlay-put overlay 'face 'shadow)
    (with-selected-window window
      (posframe-show
       bufname
       :string (propertize string 'face 'conn-posframe-window-label-face)
       :poshandler conn-posframe-window-label-poshandler))))

(cl-defmethod conn-label-payload ((label conn-posframe-window-label))
  (conn-posframe-window-label-window label))

(cl-defmethod conn-label-partial-p ((label conn-posframe-window-label))
  (pcase-let (((cl-struct conn-posframe-window-label bufname string)
               label))
    (with-current-buffer bufname
      (not (or (= 0 (buffer-size))
               (= (length string) (buffer-size)))))))

(cl-defmethod conn-label-narrow ((label conn-posframe-window-label) prefix-char)
  (pcase-let (((cl-struct conn-posframe-window-label bufname overlay)
               label))
    (with-current-buffer bufname
      (goto-char 0)
      (if (eql (char-after) prefix-char)
          (progn
            (delete-char 1)
            label)
        (posframe-hide bufname)
        (overlay-put overlay 'face nil)
        nil))))

;;;; Quick Ref

(defun conn--quick-ref-posframe (buffer &optional _state teardown)
  (if teardown
      (posframe-hide buffer)
    (posframe-show buffer
                   :left-fringe 0
                   :right-fringe 0
                   :border-width conn-posframe-border-width
                   :border-color (face-background 'conn-posframe-border)
                   :poshandler conn-posframe-buffer-poshandler)))

(provide 'conn-posframe)
