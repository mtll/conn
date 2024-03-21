;;; conn-expand-region.el --- Conn expand-region extension -*- lexical-binding: t -*-
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

(require 'conn-mode)
(require 'expand-region)

;;;###autoload
(defun conn-expand-region (arg)
  (interactive "p")
  (setq this-command 'er/expand-region)
  (activate-mark)
  (er/expand-region arg))

;;;###autoload
(defun conn-expand-dots (arg)
  "Expand each dot using `er/expand-region'."
  (interactive "p")
  (conn--move-each-dot
   (lambda (d)
     (goto-char (overlay-start d))
     (push-mark (overlay-end d) t t)
     (let ((expand-region-fast-keys-enabled nil))
       (er/expand-region arg)))))

;;;###autoload
(defun conn-contract-dots (arg)
  "Contract each dot using `er/expand-region'."
  (interactive "p")
  (conn-expand-dots (- arg)))

(provide 'conn-expand-region)
