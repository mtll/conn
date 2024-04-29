;;; conn-isearch+.el --- Conn isearch+ extension -*- lexical-binding: t -*-
;;
;; Filename: conn-isearch+.el
;; Description: Isearch+ extensions for Conn Mode
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") isearch+ conn)
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

(require 'conn)
(require 'isearch+)

(cl-pushnew '("~[dot]" conn-isearch-not-in-dot-p "~[DOT]")
            isearchp-current-filter-preds-alist
            :test #'equal)

(cl-pushnew '( "[dot]" conn-isearch-in-dot-p      "[DOT]")
            isearchp-current-filter-preds-alist
            :test #'equal)

;;;###autoload
(defun conn-isearch-in-dot-toggle ()
  "Toggle restricting search to dots."
  (interactive)
  (if (advice-function-member-p #'conn-isearch-in-dot-p isearch-filter-predicate)
      (isearchp-remove-filter-predicate "conn-isearch-in-dot-p")
    (isearchp-add-filter-predicate '("[dot]" conn-isearch-in-dot-p "[DOT]"))))

(defun conn--isearch-in-dots-hook ()
  (isearchp-add-filter-predicate '("[dot]" conn-isearch-in-dot-p "[DOT]")))

;;;###autoload
(defun conn-isearch-forward-in-dots ()
  (interactive)
  (conn-dot-state)
  (isearch-mode t)
  (conn-isearch-in-dot-toggle))

;;;###autoload
(defun conn-isearch-backward-in-dots ()
  (interactive)
  (conn-dot-state)
  (isearch-mode nil)
  (conn-isearch-in-dot-toggle))

(define-keymap
  :keymap isearch-mode-map
  "C-y m" 'isearchp-yank-sexp-symbol-or-char
  "C-y o" 'isearchp-yank-word-or-char-forward
  "C-y u" 'isearchp-yank-word-or-char-backward
  "C-y i" 'isearchp-yank-line-backward
  "C-y k" 'isearchp-yank-line-forward
  "C-y l" 'isearchp-yank-char
  "M-o"   isearchp-filter-map
  "M-."   'conn-isearch-in-dot-toggle)

;; isearchp-highlight-lighter forces redisplay, this
;; is not desirable during keyboard macros.  I don't
;; know if this is reasonable to apply by default.
;;
;; (defun conn--executing-kbd-macro-p () executing-kbd-macro)
;; (advice-add 'isearchp-highlight-lighter :before-until
;;             'conn--executing-kbd-macro-p)

(provide 'conn-isearch+)
;;; conn-isearch+.el ends here
