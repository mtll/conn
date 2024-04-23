;;; conn-evil-treesit-obj.el --- Conn evil treesit extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") evil-textobj-tree-sitter conn)
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
(require 'evil-textobj-tree-sitter)

(defvar conn--ts-modes '(c++-ts-mode
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

(conn-register-thing inner-function
  :modes conn--ts-modes
  :mark-key "f"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "function.inner")))

(conn-register-thing inner-loop
  :modes conn--ts-modes
  :mark-key "p"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "loop.inner")))

(conn-register-thing inner-conditional
  :modes conn--ts-modes
  :mark-key "i"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "conditional.inner")))

(conn-register-thing inner-assignment
  :modes conn--ts-modes
  :mark-key "a"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "assignment.inner")))

(conn-register-thing inner-class
  :modes conn--ts-modes
  :mark-key "c"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "class.inner")))

(conn-register-thing inner-comment
  :modes conn--ts-modes
  :mark-key ";"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "comment.inner")))

(conn-register-thing inner-parameter
  :modes conn--ts-modes
  :mark-key "p"
  :bounds-op (lambda ()
               (evil-textobj-tree-sitter--thing-at-point-bounds "parameter.inner")))

(conn-register-thing function    :modes conn--ts-modes :mark-key "F")
(conn-register-thing loop        :modes conn--ts-modes :mark-key "P")
(conn-register-thing conditional :modes conn--ts-modes :mark-key "I")
(conn-register-thing assignment  :modes conn--ts-modes :mark-key "A")
(conn-register-thing class       :modes conn--ts-modes :mark-key "C")
(conn-register-thing comment     :modes conn--ts-modes :mark-key ":")
(conn-register-thing parameter   :modes conn--ts-modes :mark-key "P")

(provide 'conn-evil-treesit-obj)
