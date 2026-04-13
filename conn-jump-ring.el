;;; conn-jump-ring.el --- Conn Jump Ring -*- lexical-binding: t -*-
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

;;; Commentary:

;;; Code:

(require 'compat)
(require 'conn-vars)
(require 'conn-utils)
(eval-when-compile
  (require 'subr-x))

(defvar-local conn-jump-ring nil
  "Ring of previous jump positions in a buffer.")

(defun conn--jump-pre-command-hook ()
  (set-marker conn-this-command-start (point) (current-buffer)))

(defun conn--jump-post-command-hook ()
  (when-let* ((handler (and (symbolp this-command)
                            (get this-command :conn-jump-command))))
    (when (and (markerp conn-this-command-start)
               (marker-position conn-this-command-start)
               (eq (marker-buffer conn-this-command-start)
                   (current-buffer))
               (/= (point) conn-this-command-start))
      (if (eq t handler)
          (conn-push-jump-ring conn-this-command-start)
        (ignore-errors
          (funcall handler (marker-position conn-this-command-start)))))))

;;;###autoload
(defun conn-set-jump-command (command &optional handler)
  "Register COMMAND as a jump command.

If optional argument HANDLER is nil then COMMAND will unconditionally
push to the jump ring.  If handler is non-nil it should be a function
that will be called from the `post-command-hook' with the position at
which command was first called and is responsible for pushing a position
to the jump ring."
  (dolist (cmd (ensure-list command))
    (function-put cmd :conn-jump-command (or handler t))))

(defun conn--isearch-jump-handler ()
  (when (/= (point) isearch-opoint)
    (or mark-active
        (conn-push-jump-ring isearch-opoint))))

(defun conn--xref-push-markers-ad (buf pt _win)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless (= pt (point))
        (conn-push-jump-ring pt)))))

(defun conn-pop-jump-ring ()
  (interactive)
  (conn-push-jump-ring (point-marker))
  (conn-ring-rotate-forward conn-jump-ring)
  (goto-char (conn-ring-head conn-jump-ring)))

(defun conn-unpop-jump-ring ()
  (interactive)
  (conn-push-jump-ring (point-marker))
  (conn-ring-rotate-backward conn-jump-ring)
  (goto-char (conn-ring-head conn-jump-ring)))

(defvar-keymap conn-pop-to-mark-command-repeat-map
  :repeat t
  "," 'conn-pop-jump-ring
  "." 'conn-unpop-jump-ring)

;;;###autoload
(define-minor-mode conn-jump-ring-mode
  "Enable the conn jump ring."
  :global t
  :group 'conn
  :keymap (define-keymap
            "<conn-goto-map> ," 'conn-pop-jump-ring
            "<conn-goto-map> ." 'conn-unpop-jump-ring)
  (if conn-jump-ring-mode
      (progn
        (advice-add 'xref--push-markers :after #'conn--xref-push-markers-ad)
        (add-hook 'isearch-mode-end-hook #'conn--isearch-jump-handler)
        (add-hook 'pre-command-hook #'conn--jump-pre-command-hook)
        (add-hook 'post-command-hook #'conn--jump-post-command-hook 90))
    (advice-remove 'xref--push-markers #'conn--xref-push-markers-ad)
    (remove-hook 'isearch-mode-end-hook #'conn--isearch-jump-handler)
    (remove-hook 'pre-command-hook #'conn--jump-pre-command-hook)
    (remove-hook 'post-command-hook #'conn--jump-post-command-hook)))

(defvar conn-push-jump-functions nil)

(defun conn-push-jump-ring (location &optional back msg)
  "Push LOCATION to the jump ring.

If BACK is non-nil then push LOCATION to the back of the jump ring."
  (interactive (list (point) nil t))
  (when conn-jump-ring-mode
    (when (not conn-jump-ring)
      (setq conn-jump-ring
            (conn-make-ring 40
                            :cleanup (lambda (mk) (set-marker mk nil))
                            :copier #'conn--copy-mark)))
    (pcase-let ((ptb (conn-ring-tail conn-jump-ring))
                (ptf (conn-ring-head conn-jump-ring)))
      (cond
       ((and ptf (= location ptf))
        (when back (conn-ring-rotate-forward conn-jump-ring)))
       ((and ptb (= location ptb))
        (unless back (conn-ring-rotate-backward conn-jump-ring)))
       (t
        (if back
            (conn-ring-insert-back conn-jump-ring
                                   (conn--create-marker location))
          (conn-ring-insert-front conn-jump-ring
                                  (conn--create-marker location))))))
    (run-hook-wrapped 'conn-push-jump-functions
                      (lambda (fn location back)
                        (ignore-errors
                          (ignore (funcall fn location back)))))
    (when msg (message "Jump ring pushed"))))

(setf (alist-get 'conn-jump defun-declarations-alist)
      (list #'conn--set-jump-property))

(defun conn--set-jump-property (f _args &optional handler)
  `(progn
     :autoload-end
     (conn-set-jump-command ',f ,handler)))

(defvar-local conn-jump-repeating nil)

(defun conn--repeat-push-jump-hook (&rest _)
  (remove-hook 'conn-push-jump-functions 'conn--repeat-jump-hook 'local)
  (remove-hook 'after-change-functions 'conn--repeat-push-jump-hook 'local)
  (setq conn-jump-repeating nil))

(defun conn--repeat-change-hook (&rest _)
  (unless (eql (buffer-chars-modified-tick)
               conn-jump-repeating)
    (remove-hook 'conn-push-jump-functions 'conn--repeat-jump-hook 'local)
    (remove-hook 'post-command-hook 'conn--repeat-change-hook 'local)
    (setq conn-jump-repeating nil)))

(defun conn-ignore-repeat-jump-handler (pos)
  (unless conn-jump-repeating
    (let (conn-jump-repeating)
      (conn-push-jump-ring pos))
    (setq conn-jump-repeating (buffer-chars-modified-tick))
    (add-hook 'conn-push-jump-functions 'conn--repeat-push-jump-hook nil 'local)
    (add-hook 'post-command-hook 'conn--repeat-change-hook nil 'local)))

(conn-set-jump-command 'scroll-up-command #'conn-ignore-repeat-jump-handler)
(conn-set-jump-command 'scroll-down-command #'conn-ignore-repeat-jump-handler)

(conn-set-jump-command 'beginning-of-buffer #'conn-ignore-repeat-jump-handler)
(conn-set-jump-command 'end-of-buffer #'conn-ignore-repeat-jump-handler)

(conn-set-jump-command 'beginning-of-defun #'conn-ignore-repeat-jump-handler)
(conn-set-jump-command 'end-of-defun #'conn-ignore-repeat-jump-handler)

(conn-set-jump-command 'forward-paragraph #'conn-ignore-repeat-jump-handler)
(conn-set-jump-command 'backward-paragraph #'conn-ignore-repeat-jump-handler)

(provide 'conn-jump-ring)
