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

(defcustom conn-posframe-border-width 2
  "Border width for conn posframes."
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

(defun conn-posframe--switch-tab-display (&rest _)
  (posframe-show
   " *conn-list-posframe*"
   :string (mapconcat
            (lambda (tab)
              (if (eq (car tab) 'current-tab)
                  (propertize (alist-get 'name (cdr tab))
                              'face 'conn-posframe-highlight)
                (alist-get 'name (cdr tab))))
            (reverse (funcall tab-bar-tabs-function))
            "\n")
   :poshandler conn-posframe-poshandler
   :timeout conn-posframe-timeout
   :border-width conn-posframe-border-width
   :border-color conn-posframe-border-color)
  (add-hook 'pre-command-hook 'conn-posframe--hide))

(defun conn-posframe--switch-buffer-display ()
  (posframe-show
   " *conn-list-posframe*"
   :string (mapconcat
            (lambda (buf)
              (if (eq (current-buffer) buf)
                  (propertize (buffer-name buf)
                              'face 'conn-posframe-highlight)
                (buffer-name buf)))
            (let* ((save-blist (window-prev-buffers))
                   (save-nlist (window-next-buffers))
                   (prev (prog1
                             (save-window-excursion
                               (cl-loop repeat 5
                                        until (memq (current-buffer) bufs)
                                        collect (switch-to-prev-buffer) into bufs
                                        finally return bufs))
                           (set-window-prev-buffers (selected-window) save-blist)
                           (set-window-next-buffers (selected-window) save-nlist)))
                   (next (prog1
                             (save-window-excursion
                               (cl-loop repeat 5
                                        until (memq (current-buffer) bufs)
                                        collect (switch-to-next-buffer) into bufs
                                        finally return bufs))
                           (set-window-prev-buffers (selected-window) save-blist)
                           (set-window-next-buffers (selected-window) save-nlist))))
              (append (reverse next) (list (current-buffer)) prev))
            "\n")
   :min-width 60
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
;; outline-regexp: ";;;;* [^ 	\n]"
;; End:
;;; conn-posframe.el ends here
