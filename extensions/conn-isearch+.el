;;; conn-isearch+.el --- Conn isearch+ extension -*- lexical-binding: t -*-
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
(require 'isearch+)

(cl-pushnew '("~[dot]" conn-isearch-not-in-dot-p "~[DOT]")
            isearchp-current-filter-preds-alist
            :test #'equal)

(cl-pushnew '( "[dot]" conn-isearch-in-dot-p      "[DOT]")
            isearchp-current-filter-preds-alist
            :test #'equal)

(defun conn-isearch-in-dot-toggle ()
  "Toggle restricting search to dots."
  (interactive)
  (if (advice-function-member-p #'conn-isearch-in-dot-p isearch-filter-predicate)
      (isearchp-remove-filter-predicate "conn-isearch-in-dot-p")
    (isearchp-add-filter-predicate '("[dot]" conn-isearch-in-dot-p "[DOT]"))))

(defun conn--isearch-in-dots-hook ()
  (isearchp-add-filter-predicate '("[dot]" conn-isearch-in-dot-p "[DOT]")))

(define-keymap
  :keymap isearch-mode-map
  "C-y m" 'isearchp-yank-sexp-symbol-or-char
  "C-y o" 'isearchp-yank-word-or-char-forward
  "C-y u" 'isearchp-yank-word-or-char-backward
  "C-y i" 'isearchp-yank-line-backward
  "C-y k" 'isearchp-yank-line-forward
  "C-y l" 'isearchp-yank-char
  "M-o"   isearchp-filter-map
  "C-z"   'conn-isearch-dispatch)

;; (keymap-set isearchp-filter-map "f" 'isearchp-add-filter-predicate)
;; (keymap-set isearchp-filter-map "r" 'isearchp-add-regexp-filter-predicate)

(provide 'conn-isearch+)
