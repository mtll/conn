;;; conn-treesit.el --- Treesit things for Conn -*- lexical-binding: t -*-
;;
;; Filename: conn-treesit.el
;; Description: Treesit things for Conn
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.1
;; Package-Requires: ((emacs "30") (seq "2.24") conn)
;; Homepage: https://github.com/mtll/conn
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
;; Treesit things for Conn
;;
;;; Code:

(require 'conn)
(require 'treesit)

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

(defun conn--dispatch-all-ts-things (thing)
  (conn--thread -->
      (cl-loop for n in (with-restriction (window-start) (window-end)
                          (thread-first
                            (treesit-buffer-root-node)
                            (treesit-induce-sparse-tree thing)
                            (flatten-tree)))
               for beg = (treesit-node-start n)
               for end = (treesit-node-end n)
               when (and (conn--region-visible-p beg end)
                         (>= beg (window-start))
                         (<= beg (window-end)))
               collect beg)
    (seq-sort '< -->)
    (cl-loop for pt in --> collect (conn--make-preview-overlay pt 1 thing))))

(dolist (mode conn--ts-modes)
  (add-hook (conn--symbolicate mode "-hook")
            (lambda ()
              (setq-local conn-dispatch-all-things-collector 'conn--dispatch-all-ts-things))))
