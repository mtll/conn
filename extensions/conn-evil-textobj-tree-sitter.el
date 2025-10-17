;;; conn-evil-textobj-tree-sitter.el --- Evil Treesit Text Objects -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") (evil-textobj-tree-sitter "0.5") conn)
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
(require 'evil-textobj-tree-sitter)

(cl-defstruct (conn-etts-thing)
  (group nil :type list)
  (query nil :type list))

(define-inline conn-make-etts-thing (groups &optional query)
  "See `evil-textobj-tree-sitter-get-textobj'."
  (inline-quote
   (make-conn-etts-thing :group (mapcar #'intern (ensure-list ,groups))
                         :query ,query)))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-etts-thing)) arg)
  (cl-loop with thing = (get cmd :conn-etts-thing)
           with nodes = (evil-textobj-tree-sitter--get-within-and-after
                         (conn-etts-thing-group thing)
                         (prefix-numeric-value arg)
                         (conn-etts-thing-query thing))
           for n in nodes
           for (_ beg end) = n
           minimize beg into min
           maximize end into max
           collect (cdr n) into subregions
           finally return (conn-make-bounds cmd arg
                                            (cons min max)
                                            :subregions subregions)))

(conn-register-thing 'conn-etts-thing)

(defmacro conn-etts-define-thing (name group &optional query)
  "See `evil-textobj-tree-sitter-get-textobj'."
  (declare (indent defun))
  `(progn
     (put ',name
          :conn-etts-thing (conn-make-etts-thing ,(macroexp-quote group)
                                                 ,(macroexp-quote query)))
     (conn-register-thing ',name
                          :parent 'conn-etts-thing)))

(conn-etts-define-thing conn-etts-assignment-inner "assignment.inner")
(conn-etts-define-thing conn-etts-assignment-lhs "assignment.lhs")
(conn-etts-define-thing conn-etts-assignment-outer "assignment.outer")
(conn-etts-define-thing conn-etts-assignment-rhs "assignment.rhs")
(conn-etts-define-thing conn-etts-attribute-inner "attribute.inner")
(conn-etts-define-thing conn-etts-attribute-outer "attribute.outer")
(conn-etts-define-thing conn-etts-block-inner "block.inner")
(conn-etts-define-thing conn-etts-block-outer "block.outer")
(conn-etts-define-thing conn-etts-call-inner "call.inner")
(conn-etts-define-thing conn-etts-call-outer "call.outer")
(conn-etts-define-thing conn-etts-class-inner "class.inner")
(conn-etts-define-thing conn-etts-class-outer "class.outer")
(conn-etts-define-thing conn-etts-comment-inner "comment.inner")
(conn-etts-define-thing conn-etts-comment-outer "comment.outer")
(conn-etts-define-thing conn-etts-conditional-inner "conditional.inner")
(conn-etts-define-thing conn-etts-conditional-outer "conditional.outer")
(conn-etts-define-thing conn-etts-frame-inner "frame.inner")
(conn-etts-define-thing conn-etts-frame-outer "frame.outer")
(conn-etts-define-thing conn-etts-function-inner "function.inner")
(conn-etts-define-thing conn-etts-function-outer "function.outer")
(conn-etts-define-thing conn-etts-loop-inner "loop.inner")
(conn-etts-define-thing conn-etts-loop-outer "loop.outer")
(conn-etts-define-thing conn-etts-number "number.inner")
(conn-etts-define-thing conn-etts-parameter-inner "parameter.inner")
(conn-etts-define-thing conn-etts-parameter-outer "parameter.outer")
(conn-etts-define-thing conn-etts-regex-inner "regex.inner")
(conn-etts-define-thing conn-etts-regex-outer "regex.outer")
(conn-etts-define-thing conn-etts-return-inner "return.inner")
(conn-etts-define-thing conn-etts-return-outer "return.outer")
(conn-etts-define-thing conn-etts-scopename "scopename.inner")
(conn-etts-define-thing conn-etts-statement "statement.outer")

(defvar-keymap conn-etts-things-mode-map
  "<conn-thing-map> = i" 'conn-etts-assignment-inner
  "<conn-thing-map> = l" 'conn-etts-assignment-lhs
  "<conn-thing-map> = o" 'conn-etts-assignment-outer
  "<conn-thing-map> = r" 'conn-etts-assignment-rhs
  "<conn-thing-map> @ i" 'conn-etts-attribute-inner
  "<conn-thing-map> @ o" 'conn-etts-attribute-outer
  "<conn-thing-map> b i" 'conn-etts-block-inner
  "<conn-thing-map> b o" 'conn-etts-block-outer
  "<conn-thing-map> . i" 'conn-etts-call-inner
  "<conn-thing-map> . o" 'conn-etts-call-outer
  "<conn-thing-map> c i" 'conn-etts-class-inner
  "<conn-thing-map> c o" 'conn-etts-class-outer
  "<conn-thing-map> ; i" 'conn-etts-comment-inner
  "<conn-thing-map> ; o" 'conn-etts-comment-outer
  "<conn-thing-map> i i" 'conn-etts-conditional-inner
  "<conn-thing-map> i o" 'conn-etts-conditional-outer
  "<conn-thing-map> [ i" 'conn-etts-frame-inner
  "<conn-thing-map> [ o" 'conn-etts-frame-outer
  "<conn-thing-map> f i" 'conn-etts-function-inner
  "<conn-thing-map> f o" 'conn-etts-function-outer
  "<conn-thing-map> l i" 'conn-etts-loop-inner
  "<conn-thing-map> l o" 'conn-etts-loop-outer
  "<conn-thing-map> #" 'conn-etts-number
  "<conn-thing-map> p i" 'conn-etts-parameter-inner
  "<conn-thing-map> p o" 'conn-etts-parameter-outer
  "<conn-thing-map> x i" 'conn-etts-regex-inner
  "<conn-thing-map> x o" 'conn-etts-regex-outer
  "<conn-thing-map> r i" 'conn-etts-return-inner
  "<conn-thing-map> r o" 'conn-etts-return-outer
  "<conn-thing-map> S" 'conn-etts-scopename
  ;; "<conn-thing-map> t" 'conn-etts-statement
  )

(define-minor-mode conn-etts-things-mode
  "Minor mode for conn-etts things")

(provide 'conn-evil-textobj-tree-sitter)
