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
(require 'conn-things)
(require 'evil-textobj-tree-sitter)

(cl-defstruct (conn--etts-thing
               (:constructor conn--make-etts-thing))
  (group nil :type list)
  (query nil :type list))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-etts-thing)) arg)
  (cl-loop with thing = (get (or (get cmd :conn-command-thing) cmd)
                             :conn-etts-thing)
           with nodes = (evil-textobj-tree-sitter--get-within-and-after
                         (conn--etts-thing-group thing)
                         (prefix-numeric-value arg)
                         (conn--etts-thing-query thing))
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
  (declare (indent defun))
  (let ((mark-cmd (intern (format "%s-mark" name))))
    `(progn
       (put ',name
            :conn-etts-thing (conn--make-etts-thing
                              :group (mapcar #'intern
                                             (ensure-list ,(macroexp-quote group)))
                              :query ,(macroexp-quote query)))
       (conn-register-thing ',name
                            :parent 'conn-etts-thing)
       (defun ,mark-cmd (&optional arg)
         (interactive "p")
         (pcase (conn-bounds-of ',name arg)
           ((conn-bounds `(,beg . ,end))
            (cond ((not (region-active-p))
                   (goto-char beg)
                   (conn--push-ephemeral-mark end))
                  ((= (point) (mark))
                   (pcase (car (read-multiple-choice
                                "Mark"
                                '((?a "after point")
                                  (?b "before point"))))
                     (?e (goto-char end))
                     (?b (goto-char beg))))
                  ((> (point) (mark)) (goto-char end))
                  (t (goto-char beg))))))
       (conn-register-thing-commands ',name 'ignore ',mark-cmd))))

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
  "<conn-thing-map> w i" 'conn-etts-assignment-inner-mark
  "<conn-thing-map> w l" 'conn-etts-assignment-lhs-mark
  "<conn-thing-map> w o" 'conn-etts-assignment-outer-mark
  "<conn-thing-map> w r" 'conn-etts-assignment-rhs-mark
  "<conn-thing-map> @ i" 'conn-etts-attribute-inner-mark
  "<conn-thing-map> @ o" 'conn-etts-attribute-outer-mark
  "<conn-thing-map> b i" 'conn-etts-block-inner-mark
  "<conn-thing-map> b o" 'conn-etts-block-outer-mark
  "<conn-thing-map> . i" 'conn-etts-call-inner-mark
  "<conn-thing-map> . o" 'conn-etts-call-outer-mark
  "<conn-thing-map> C i" 'conn-etts-class-inner-mark
  "<conn-thing-map> C o" 'conn-etts-class-outer-mark
  "<conn-thing-map> c i" 'conn-etts-comment-inner-mark
  "<conn-thing-map> c o" 'conn-etts-comment-outer-mark
  "<conn-thing-map> q i" 'conn-etts-conditional-inner-mark
  "<conn-thing-map> q o" 'conn-etts-conditional-outer-mark
  "<conn-thing-map> [ i" 'conn-etts-frame-inner-mark
  "<conn-thing-map> [ o" 'conn-etts-frame-outer-mark
  "<conn-thing-map> f i" 'conn-etts-function-inner-mark
  "<conn-thing-map> f o" 'conn-etts-function-outer-mark
  "<conn-thing-map> r i" 'conn-etts-loop-inner-mark
  "<conn-thing-map> r o" 'conn-etts-loop-outer-mark
  "<conn-thing-map> n" 'conn-etts-number-mark
  "<conn-thing-map> d i" 'conn-etts-parameter-inner-mark
  "<conn-thing-map> d o" 'conn-etts-parameter-outer-mark
  "<conn-thing-map> x i" 'conn-etts-regex-inner-mark
  "<conn-thing-map> x o" 'conn-etts-regex-outer-mark
  "<conn-thing-map> t i" 'conn-etts-return-inner-mark
  "<conn-thing-map> t o" 'conn-etts-return-outer-mark
  "<conn-thing-map> S" 'conn-etts-scopename-mark
  ;; "<conn-thing-map> t" 'conn-etts-statement-mark
  )

(define-minor-mode conn-etts-things-mode
  "Minor mode for conn-etts things")

(provide 'conn-evil-textobj-tree-sitter)
