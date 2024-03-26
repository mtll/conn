;;; conn-evil-treesit-obj.el --- Conn evil treesit extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") evil-textobj-tree-sitter conn-mode)
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

(require 'conn-mode)
(require 'evil-textobj-tree-sitter)

(put 'inner-function 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "function.inner")))

(put 'inner-loop 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "loop.inner")))

(put 'inner-conditional 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "conditional.inner")))

(put 'inner-assignment 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "assignment.inner")))

(put 'inner-class 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "class.inner")))

(put 'inner-comment 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "comment.inner")))

(put 'inner-parameter 'bounds-of-thing-at-point
     (lambda ()
       (evil-textobj-tree-sitter--thing-at-point-bounds "parameter.inner")))

(defun conn-mark-inner-parameter ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-parameter)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-function ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-function)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-loop ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-loop)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-conditional ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-conditional)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-assignment ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-assignment)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-class ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-class)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-inner-comment ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'inner-comment)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-parameter ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'parameter)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-function ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'function)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-loop ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'loop)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-conditional ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'conditional)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-assignment ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'assignment)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-class ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'class)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defun conn-mark-comment ()
  (interactive)
  (pcase (bounds-of-thing-at-point 'comment)
    (`(,beg . ,end)
     (goto-char beg)
     (conn--push-ephemeral-mark end))))

(defvar-keymap conn-mark-map
  "e p" 'conn-mark-parameter
  "e f" 'conn-mark-function
  "e l" 'conn-mark-loop
  "e i" 'conn-mark-conditional
  "e a" 'conn-mark-assignment
  "e c" 'conn-mark-class
  "e ;" 'conn-mark-comment
  "w p" 'conn-mark-inner-parameter
  "w f" 'conn-mark-inner-function
  "w l" 'conn-mark-inner-loop
  "w i" 'conn-mark-inner-conditional
  "w a" 'conn-mark-inner-assignment
  "w c" 'conn-mark-inner-class
  "w ;" 'conn-mark-inner-comment)

(dolist (major-mode '(c++-ts-mode
                      rustic-mode
                      c-ts-mode
                      csharp-ts-mode
                      elixir-ts-mode
                      elm-ts-mode
                      go-ts-mode
                      haskell-ts-mode
                      html-ts-mode
                      java-ts-mode
                      javascript-ts-mode
                      js-ts-mode
                      julia-ts-mode
                      php-ts-mode
                      prisma-ts-mode
                      python-ts-mode
                      ruby-ts-mode
                      rust-ts-mode
                      bash-ts-mode
                      typescript-ts-mode))
  (dolist (state '(conn-state conn-dot-state))
    (define-keymap
      :keymap (conn-get-mode-map state major-mode)
      "b" conn-mark-map)))

(provide 'conn-evil-treesit-obj)
