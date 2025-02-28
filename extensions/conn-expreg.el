;;; conn-expreg.el --- Conn expreg extension -*- lexical-binding: t -*-
;;
;; Filename: conn-expreg.el
;; Description: expreg extension for conn
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") expreg conn)
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
;;; Commentary
;;
;; Use expreg to proved expansions for conn-expand.
;;
;;; Code:

(require 'conn)
(require 'expreg)

;;;###autoload
(defun conn-expreg-expansions ()
  (save-mark-and-excursion
    (activate-mark)
    (mapcan (lambda (fn) (save-excursion
                           (mapcar #'cdr (funcall fn))))
            expreg-functions)))

(provide 'conn-expreg)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-expreg.el ends here
