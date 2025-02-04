;;; conn-posframe.el --- Conn posframe extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.0.0") posframe conn)
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
(require 'posframe)

(defgroup conn-posframe nil
  "Conn posframes."
  :prefix "conn-posframe-"
  :group 'conn)

(defface conn-posframe-highlight
  '((t (:inherit eldoc-highlight-function-argument)))
  "Face for selection in Conn posframes."
  :group 'conn-posframe)

(defface conn-posframe-header
  '((t ( :inherit diff-hunk-header :extend t
         :underline (:style line :position t))))
  "Face for selection in Conn posframes."
  :group 'conn-posframe)

(defcustom conn-posframe-border-width 2
  "Border width for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-min-width 50
  "Minimum width for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-border-color
  "#000000"
  "Border color for conn posframes."
  :type 'string
  :group 'conn-posframe)

(defcustom conn-posframe-timeout nil
  "Timeout for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-poshandler
  'posframe-poshandler-frame-center
  "Timeout for conn posframes."
  :type 'symbol
  :group 'conn-posframe)

(defun conn-posframe--hide ()
  (posframe-hide " *conn-list-posframe*")
  (remove-hook 'pre-command-hook 'conn-posframe--hide))

;; Implementation from window.el
(defun conn-posframe--next-buffers (&optional window)
  (let* ((window (window-normalize-window window t))
         (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
         (old-buffer (window-buffer window))
         (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond
           ((or (functionp switch-to-prev-buffer-skip)
                (memq switch-to-prev-buffer-skip '(t visible 0)))
            switch-to-prev-buffer-skip)
           ((or switch-to-prev-buffer-skip
                (not switch-to-visible-buffer))
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
            (when (length= found-buffers 5)
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
              (when (length= found-buffers 5)
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
            (when (length= found-buffers 5)
              (throw 'found t)))))

      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        (setq new-buffer skipped)
        (cl-pushnew new-buffer found-buffers)))

    found-buffers))

;; Implementation from window.el
(defun conn-posframe--previous-buffers (&optional window)
  (let* ((window (window-normalize-window window t))
         (frame (window-frame window))
         (window-side (window-parameter window 'window-side))
         (old-buffer (window-buffer window))
         (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
         (skip
          (cond
           ((or (functionp switch-to-prev-buffer-skip)
                (memq switch-to-prev-buffer-skip '(t visible 0)))
            switch-to-prev-buffer-skip)
           ((or switch-to-prev-buffer-skip
                (not switch-to-visible-buffer))
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
            (when (length= found-buffers 5)
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
              (when (length= found-buffers 5)
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
            (when (length= found-buffers 5)
              (throw 'found t)))))

      (when (and skipped (not (functionp switch-to-prev-buffer-skip)))
        (setq new-buffer skipped)
        (cl-pushnew new-buffer found-buffers)))

    (nreverse found-buffers)))

(defun conn-posframe--switch-buffer-display ()
  (posframe-show
   " *conn-list-posframe*"
   :string (concat
            (propertize " Buffers:\n"
                        'face 'conn-posframe-header)
            (mapconcat
             (lambda (buf)
               (if (eq (current-buffer) buf)
                   (propertize (buffer-name buf)
                               'face 'conn-posframe-highlight)
                 (buffer-name buf)))
             (append (conn-posframe--next-buffers)
                     (list (current-buffer))
                     (conn-posframe--previous-buffers))
             "\n"))
   :min-width conn-posframe-min-width
   :poshandler conn-posframe-poshandler
   :timeout conn-posframe-timeout
   :border-width conn-posframe-border-width
   :border-color conn-posframe-border-color)
  (add-hook 'pre-command-hook 'conn-posframe--hide))

(defun conn-posframe--switch-tab-display (&rest _)
  (posframe-show
   " *conn-list-posframe*"
   :string (concat
            (propertize " Tabs:\n"
                        'face 'conn-posframe-header)
            (mapconcat
             (lambda (tab)
               (if (eq (car tab) 'current-tab)
                   (propertize (alist-get 'name (cdr tab))
                               'face 'conn-posframe-highlight)
                 (alist-get 'name (cdr tab))))
             (reverse (funcall tab-bar-tabs-function))
             "\n"))
   :poshandler conn-posframe-poshandler
   :timeout conn-posframe-timeout
   :border-width conn-posframe-border-width
   :border-color conn-posframe-border-color)
  (add-hook 'pre-command-hook 'conn-posframe--hide))

(defun conn-prev-buffer (&optional arg)
  (interactive "P")
  (previous-buffer arg t)
  (conn-posframe--switch-buffer-display))

(defun conn-next-buffer (&optional arg)
  (interactive "P")
  (next-buffer arg t)
  (conn-posframe--switch-buffer-display))

(defun conn-next-tab (&optional arg)
  (interactive "P")
  (tab-bar-switch-to-next-tab arg)
  (conn-posframe--switch-tab-display))

(defun conn-prev-tab (&optional arg)
  (interactive "P")
  (tab-bar-switch-to-prev-tab arg)
  (conn-posframe--switch-tab-display))

(defun conn-tab-new (&optional arg)
  (interactive "P")
  (tab-bar-new-tab arg)
  (conn-posframe--switch-tab-display))

(defun conn-tab-close (&optional arg)
  (interactive "P")
  (tab-bar-close-tab arg)
  (conn-posframe--switch-tab-display))

;;;###autoload
(define-minor-mode conn-posframe-mode
  "Posframes for Conn."
  :global t
  :lighter ""
  :keymap (define-keymap
            "<remap> <tab-new>" 'conn-tab-new
            "<remap> <tab-close>" 'conn-tab-close
            "<remap> <tab-next>" 'conn-next-tab
            "<remap> <tab-previous>" 'conn-prev-tab
            "<remap> <previous-buffer>" 'conn-prev-buffer
            "<remap> <next-buffer>" 'conn-next-buffer))

(provide 'conn-posframe)

;; Local Variables:
;; outline-regexp: ";;;;* [^    \n]"
;; End:
;;; conn-posframe.el ends here
