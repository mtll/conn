;;; conn-avy.el --- Conn avy extension -*- lexical-binding: t -*-
;;
;; Filename: conn-avy.el
;; Description: Avy extensions for Conn Mode
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") avy conn)
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
;;; Code:

(require 'avy)
(require 'conn)

;;;###autoload
(defun conn-avy-goto-dot ()
  (interactive)
  (let ((avy-all-windows t))
    (avy-process
     (let ((dots))
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (dolist (dot (conn--all-overlays #'conn-dotp
                                             (window-start win)
                                             (window-end)))
              (push (cons (cons (overlay-start dot)
                                (overlay-end dot))
                          win)
                    dots))))
        'no-minibuffer)
       dots))))

(defun conn-avy-action-dot (pt)
  "Kill sexp at PT and move there."
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (beg (point)))
    (save-excursion
      (goto-char pt)
      (conn-dot-thing-at-point thing)))
  (point))

(defun conn-avy-action-transpose (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (r1 (bounds-of-thing-at-point thing))
             (r2 (progn
                   (goto-char pt)
                   (bounds-of-thing-at-point thing))))
    (transpose-regions (car r1) (cdr r1)
                       (car r2) (cdr r2)))
  (point))

(defun conn-avy-action-kill-move (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (region (progn
                       (goto-char pt)
                       (bounds-of-thing-at-point thing))))
    (kill-region (car region) (cdr region))
    (message "Killed: %s" (current-kill 0))
    (point)))

(defun conn-avy-action-kill-stay (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (region (save-excursion
                       (goto-char pt)
                       (bounds-of-thing-at-point thing))))
    (kill-region (car region) (cdr region))
    (message "Killed: %s" (current-kill 0))
    (point)))

(defun conn-avy-action-teleport (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (region (save-excursion
                       (goto-char pt)
                       (bounds-of-thing-at-point thing)))
             (string (buffer-substring (car region) (cdr region))))
    (kill-region (car region) (cdr region))
    (insert string)
    (point)))

(defun conn-avy-action-yank (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (region (save-excursion
                       (goto-char pt)
                       (bounds-of-thing-at-point thing))))
    (kill-new (buffer-substring (car region) (cdr region)))
    (yank)
    (point)))

(defun conn-avy-action-copy (pt)
  (when-let ((thing (or (conn--get-this-command-thing)
                        (conn--read-thing-command)))
             (region (save-excursion
                       (goto-char pt)
                       (bounds-of-thing-at-point thing)))
             (string (buffer-substring (car region) (cdr region))))
    (kill-new string)
    (message "Copied: %s" string)
    (point)))

;;;###autoload
(defun conn-avy-dispatch (thing)
  (interactive (list (conn--read-thing-command)))
  (if (eq thing 'char)
      (avy-goto-char-timer)
    (setq conn-this-command-thing thing
          conn-this-command-handler
          (lambda (start)
            (unless (= start (point))
              (pcase (bounds-of-thing-at-point thing)
                (`(,beg . ,end)
                 (goto-char beg)
                 (conn--push-ephemeral-mark end))))))
    (avy-goto-char-timer)))

(conn-register-thing-commands
 'sexp (conn-individual-thing-handler 'sexp)
 'avy-goto-symbol-1
 'avy-goto-symbol-1-above
 'avy-goto-symbol-1-below)

(conn-register-thing-commands
 'word (conn-individual-thing-handler 'word)
 'avy-goto-word-0
 'avy-goto-word-1
 'avy-goto-word-or-subword-1
 'avy-goto-word-1-above
 'avy-goto-word-1-below)

(conn-register-thing-commands
 'line (conn-individual-thing-handler 'line)
 'avy-goto-line
 'avy-goto-line-above
 'avy-goto-line-below
 'avy-goto-end-of-line)

(provide 'conn-avy)
;;; conn-avy.el ends here
