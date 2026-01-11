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
  (require 'subr-x)
  (require 'cl-lib))

(defvar-local conn-jump-ring nil
  "Ring of previous jump positions in a buffer.")

(defun conn--jump-pre-command-hook ()
  (set-marker conn-this-command-start (point) (current-buffer)))

(defun conn--jump-post-command-hook ()
  (when-let*
      ((_ (marker-position conn-this-command-start))
       (pred (function-get this-command :conn-jump-command))
       (_ (and (eq (marker-buffer conn-this-command-start)
                   (current-buffer))
               (/= (point) conn-this-command-start)
               (or (eq t pred)
                   (ignore-errors
                     (funcall pred (marker-position conn-this-command-start)))))))
    (conn-push-jump-ring conn-this-command-start)))

;;;###autoload
(defun conn-set-jump-command (command &optional predicate)
  "Register COMMAND as a jump command.

If optional argument PREDICATE is nil then COMMAND will unconditionally
push to the jump ring.  If predicate is non-nil it should be a function
that will be called from the `post-command-hook' with the position at
which command was first called and is responsible for pushing a position
to the jump ring."
  (dolist (cmd (ensure-list command))
    (function-put cmd :conn-jump-command (or predicate t))))

(defun conn--isearch-jump-predicate ()
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
        (add-hook 'isearch-mode-end-hook #'conn--isearch-jump-predicate)
        (add-hook 'pre-command-hook #'conn--jump-pre-command-hook)
        (add-hook 'post-command-hook #'conn--jump-post-command-hook 90))
    (advice-remove 'xref--push-markers #'conn--xref-push-markers-ad)
    (remove-hook 'isearch-mode-end-hook #'conn--isearch-jump-predicate)
    (remove-hook 'pre-command-hook #'conn--jump-pre-command-hook)
    (remove-hook 'post-command-hook #'conn--jump-post-command-hook)))

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
    (when msg (message "Jump ring pushed"))))

;;;###autoload
(setf (alist-get 'conn-jump defun-declarations-alist)
      (list #'conn--set-jump-property))

;;;###autoload
(defun conn--set-jump-property (f _args &optional predicate)
  `(progn
     :autoload-end
     (conn-set-jump-command ',f ,predicate)))

(provide 'conn-jump-ring)
