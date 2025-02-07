;;; conn-embark.el --- Conn embark extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.0.0") (embark "1.0") conn)
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
(require 'embark)
(require 'outline)

(conn-define-dispatch-action conn-embark-dwim (window pt _thing)
  :description "Embark-DWIM"
  :key "<tab>"
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (embark-dwim))))

(conn-define-dispatch-action conn-embark-act (window pt _thing)
  :description "Embark"
  :key ","
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (embark-act))))

(defun conn-embark-conn-bindings ()
  (interactive)
  (embark-bindings-in-keymap (alist-get conn-current-state conn--state-maps)))

(setf (alist-get 'conn-replace-region-substring embark-target-injection-hooks)
      (list #'embark--ignore-target))

(add-to-list 'embark-target-injection-hooks
             '(conn-insert-pair embark--ignore-target))
(add-to-list 'embark-target-injection-hooks
             '(conn-change-pair embark--ignore-target))

(provide 'conn-embark)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; End:
;;; conn-embark.el ends here
