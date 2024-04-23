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

(provide 'conn-avy)
;;; conn-avy.el ends here
