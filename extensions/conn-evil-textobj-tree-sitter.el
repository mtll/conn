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
  (type nil :type (or cons symbol))
  (query nil :type list)
  (bounds nil :type (or cons symbol)))

(defconst conn-etts--query-cache
  (make-hash-table :test 'eq))

(defun conn-etts--get-query ()
  (with-memoization (gethash (treesit-language-at (point)) conn-etts--query-cache)
    (treesit-query-compile
     (treesit-language-at (point))
     (evil-textobj-tree-sitter--get-query-from-dir
      (symbol-name (treesit-language-at (point)))
      (file-name-as-directory (concat evil-textobj-tree-sitter--dir "queries"))
      t))))

(defun conn-etts--get-captures (thing before)
  (let ((f-query (or (alist-get major-mode (conn--etts-thing-query thing))
                     (conn-etts--get-query))))
    (if before
        (nreverse
         (treesit-query-capture (treesit-buffer-root-node)
                                f-query
                                (point-min) (point)
                                nil t))
      (treesit-query-capture (treesit-buffer-root-node)
                             f-query
                             (point) (point-max)
                             nil t))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-etts-thing)) arg)
  (pcase-let* ((thing (get (or (get cmd :conn-command-thing) cmd)
                           :conn-etts-thing))
               (type (conn--etts-thing-type thing))
               (`(,tbeg . ,tend)
                (conn--etts-thing-bounds thing))
               (captures (conn-etts--get-captures thing (< arg 0)))
               (count 0)
               (nodes nil)
               (max most-negative-fixnum)
               (min most-positive-fixnum))
    (unless (= 0 arg)
      (catch 'return
        (cl-flet ((push-node (beg end)
                    (when-let* ((_(if (< arg 0)
                                      (< beg (point))
                                    (> end (point))))
                                (n (cons beg end))
                                (_(not (member n nodes))))
                      (cl-callf max max end)
                      (cl-callf min min beg)
                      (push (conn-make-bounds cmd 1 n) nodes)
                      (when (= (cl-incf count) (abs arg))
                        (throw 'return nil)))))
          (dolist (capture captures)
            (if-let* ((beg (alist-get tbeg capture)))
                (when-let* ((end (alist-get tend capture)))
                  (push-node (treesit-node-start beg)
                             (treesit-node-end end)))
              (when-let* ((node (alist-get type capture)))
                (push-node (treesit-node-start node)
                           (treesit-node-end node)))))))
      (when nodes
        (conn-make-bounds
         thing arg
         (cons min max)
         :subregions (nreverse nodes))))))

(conn-register-thing 'conn-etts-thing)

(defmacro conn-etts-define-thing (name type &optional query)
  (declare (indent defun))
  (let ((forward-cmd (intern (format "%s-forward" name)))
        (backward-cmd (intern (format "%s-backward" name))))
    `(progn
       (put ',name
            :conn-etts-thing (conn--make-etts-thing
                              :type ',(intern type)
                              :query ,(macroexp-quote query)
                              :bounds (cons ',(intern (format "%s._start" type))
                                            ',(intern (format "%s._end" type)))))
       (conn-register-thing ',name :parent 'conn-etts-thing)

       (defun ,forward-cmd (&optional arg)
         (interactive "p")
         (pcase (conn-bounds-of ',name arg)
           ((conn-bounds `(,beg . ,end))
            (if (> arg 0)
                (progn
                  (goto-char end)
                  (when (not (region-active-p))
                    (conn--push-ephemeral-mark beg)))
              (goto-char beg)
              (when (not (region-active-p))
                (conn--push-ephemeral-mark end))))
           (_ (signal 'scan-error
                      (list (format-message "No more %S to move across" ',name)
                            (point) (point))))))

       (defun ,backward-cmd (&optional arg)
         (interactive "p")
         (,forward-cmd (- arg)))

       (conn-register-thing-commands
        ',name 'ignore
        ',forward-cmd
        ',backward-cmd))))

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

(defvar-keymap conn-etts-assignment-inner-repeat-map
  :repeat t
  "l" 'conn-etts-assignment-inner-forward
  "j" 'conn-etts-assignment-inner-backward)

(defvar-keymap conn-etts-assignment-lhs-repeat-map
  :repeat t
  "l"  'conn-etts-assignment-lhs-forward
  "j"  'conn-etts-assignment-lhs-backward)

(defvar-keymap conn-etts-assignment-outer-repeat-map
  :repeat t
  "l"  'conn-etts-assignment-outer-forward
  "j"  'conn-etts-assignment-outer-backward)

(defvar-keymap conn-etts-assignment-rhs-repeat-map
  :repeat t
  "l"  'conn-etts-assignment-rhs-forward
  "j"  'conn-etts-assignment-rhs-backward)

(defvar-keymap conn-etts-attribute-inner-repeat-map
  :repeat t
  "l"  'conn-etts-attribute-inner-forward
  "j"  'conn-etts-attribute-inner-backward)

(defvar-keymap conn-etts-attribute-outer-repeat-map
  :repeat t
  "l"  'conn-etts-attribute-outer-forward
  "j"  'conn-etts-attribute-outer-backward)

(defvar-keymap conn-etts-block-inner-repeat-map
  :repeat t
  "l"  'conn-etts-block-inner-forward
  "j"  'conn-etts-block-inner-backward)

(defvar-keymap conn-etts-block-outer-repeat-map
  :repeat t
  "l"  'conn-etts-block-outer-forward
  "j"  'conn-etts-block-outer-backward)

(defvar-keymap conn-etts-call-inner-repeat-map
  :repeat t
  "l"  'conn-etts-call-inner-forward
  "j"  'conn-etts-call-inner-backward)

(defvar-keymap conn-etts-call-outer-repeat-map
  :repeat t
  "l"  'conn-etts-call-outer-forward
  "j"  'conn-etts-call-outer-backward)

(defvar-keymap conn-etts-class-inner-repeat-map
  :repeat t
  "l"  'conn-etts-class-inner-forward
  "j"  'conn-etts-class-inner-backward)

(defvar-keymap conn-etts-class-outer-repeat-map
  :repeat t
  "l"  'conn-etts-class-outer-forward
  "j"  'conn-etts-class-outer-backward)

(defvar-keymap conn-etts-comment-inner-repeat-map
  :repeat t
  "l"  'conn-etts-comment-inner-forward
  "j"  'conn-etts-comment-inner-backward)

(defvar-keymap conn-etts-comment-outer-repeat-map
  :repeat t
  "l"  'conn-etts-comment-outer-forward
  "j"  'conn-etts-comment-outer-backward)

(defvar-keymap conn-etts-conditional-inner-repeat-map
  :repeat t
  "l"  'conn-etts-conditional-inner-forward
  "j"  'conn-etts-conditional-inner-backward)

(defvar-keymap conn-etts-conditional-outer-repeat-map
  :repeat t
  "l"  'conn-etts-conditional-outer-forward
  "j"  'conn-etts-conditional-outer-backward)

(defvar-keymap conn-etts-frame-inner-repeat-map
  :repeat t
  "l"  'conn-etts-frame-inner-forward
  "j"  'conn-etts-frame-inner-backward)

(defvar-keymap conn-etts-frame-outer-repeat-map
  :repeat t
  "l"  'conn-etts-frame-outer-forward
  "j"  'conn-etts-frame-outer-backward)

(defvar-keymap conn-etts-function-inner-repeat-map
  :repeat t
  "l"  'conn-etts-function-inner-forward
  "j"  'conn-etts-function-inner-backward)

(defvar-keymap conn-etts-function-outer-repeat-map
  :repeat t
  "l"  'conn-etts-function-outer-forward
  "j"  'conn-etts-function-outer-backward)

(defvar-keymap conn-etts-loop-inner-repeat-map
  :repeat t
  "l"  'conn-etts-loop-inner-forward
  "j"  'conn-etts-loop-inner-backward)

(defvar-keymap conn-etts-loop-outer-repeat-map
  :repeat t
  "l"  'conn-etts-loop-outer-forward
  "j"  'conn-etts-loop-outer-backward)

(defvar-keymap conn-etts-number-repeat-map
  :repeat t
  "l"  'conn-etts-number-forward
  "j"  'conn-etts-number-backward)

(defvar-keymap conn-etts-parameter-inner-repeat-map
  :repeat t
  "l"  'conn-etts-parameter-inner-forward
  "j"  'conn-etts-parameter-inner-backward)

(defvar-keymap conn-etts-parameter-outer-repeat-map
  :repeat t
  "l"  'conn-etts-parameter-outer-forward
  "j"  'conn-etts-parameter-outer-backward)

(defvar-keymap conn-etts-regex-inner-repeat-map
  :repeat t
  "l"  'conn-etts-regex-inner-forward
  "j"  'conn-etts-regex-inner-backward)

(defvar-keymap conn-etts-regex-outer-repeat-map
  :repeat t
  "l"  'conn-etts-regex-outer-forward
  "j"  'conn-etts-regex-outer-backward)

(defvar-keymap conn-etts-return-inner-repeat-map
  :repeat t
  "l"  'conn-etts-return-inner-forward
  "j"  'conn-etts-return-inner-backward)

(defvar-keymap conn-etts-return-outer-repeat-map
  :repeat t
  "l"  'conn-etts-return-outer-forward
  "j"  'conn-etts-return-outer-backward)

(defvar-keymap conn-etts-scopename-repeat-map
  :repeat t
  "l"  'conn-etts-scopename-forward
  "j"  'conn-etts-scopename-backward)

(defvar-keymap conn-etts-things-mode-map
  "<conn-thing-map> w i l" 'conn-etts-assignment-inner-forward
  "<conn-thing-map> w i j" 'conn-etts-assignment-inner-backward
  "<conn-thing-map> w l l" 'conn-etts-assignment-lhs-forward
  "<conn-thing-map> w l j" 'conn-etts-assignment-lhs-backward
  "<conn-thing-map> w o l" 'conn-etts-assignment-outer-forward
  "<conn-thing-map> w o j" 'conn-etts-assignment-outer-backward
  "<conn-thing-map> w r l" 'conn-etts-assignment-rhs-forward
  "<conn-thing-map> w r j" 'conn-etts-assignment-rhs-backward
  "<conn-thing-map> @ i l" 'conn-etts-attribute-inner-forward
  "<conn-thing-map> @ i j" 'conn-etts-attribute-inner-backward
  "<conn-thing-map> @ o l" 'conn-etts-attribute-outer-forward
  "<conn-thing-map> @ o j" 'conn-etts-attribute-outer-backward
  "<conn-thing-map> b i l" 'conn-etts-block-inner-forward
  "<conn-thing-map> b i j" 'conn-etts-block-inner-backward
  "<conn-thing-map> b o l" 'conn-etts-block-outer-forward
  "<conn-thing-map> b o j" 'conn-etts-block-outer-backward
  "<conn-thing-map> . i l" 'conn-etts-call-inner-forward
  "<conn-thing-map> . i j" 'conn-etts-call-inner-backward
  "<conn-thing-map> . o l" 'conn-etts-call-outer-forward
  "<conn-thing-map> . o j" 'conn-etts-call-outer-backward
  "<conn-thing-map> C i l" 'conn-etts-class-inner-forward
  "<conn-thing-map> C i j" 'conn-etts-class-inner-backward
  "<conn-thing-map> C o l" 'conn-etts-class-outer-forward
  "<conn-thing-map> C o j" 'conn-etts-class-outer-backward
  "<conn-thing-map> c i l" 'conn-etts-comment-inner-forward
  "<conn-thing-map> c i j" 'conn-etts-comment-inner-backward
  "<conn-thing-map> c o l" 'conn-etts-comment-outer-forward
  "<conn-thing-map> c o j" 'conn-etts-comment-outer-backward
  "<conn-thing-map> q i l" 'conn-etts-conditional-inner-forward
  "<conn-thing-map> q i j" 'conn-etts-conditional-inner-backward
  "<conn-thing-map> q o l" 'conn-etts-conditional-outer-forward
  "<conn-thing-map> q o j" 'conn-etts-conditional-outer-backward
  "<conn-thing-map> [ i l" 'conn-etts-frame-inner-forward
  "<conn-thing-map> [ i j" 'conn-etts-frame-inner-backward
  "<conn-thing-map> [ o l" 'conn-etts-frame-outer-forward
  "<conn-thing-map> [ o j" 'conn-etts-frame-outer-backward
  "<conn-thing-map> f i l" 'conn-etts-function-inner-forward
  "<conn-thing-map> f i j" 'conn-etts-function-inner-backward
  "<conn-thing-map> f o l" 'conn-etts-function-outer-forward
  "<conn-thing-map> f o j" 'conn-etts-function-outer-backward
  "<conn-thing-map> r i l" 'conn-etts-loop-inner-forward
  "<conn-thing-map> r i j" 'conn-etts-loop-inner-backward
  "<conn-thing-map> r o l" 'conn-etts-loop-outer-forward
  "<conn-thing-map> r o j" 'conn-etts-loop-outer-backward
  "<conn-thing-map> n l" 'conn-etts-number-forward
  "<conn-thing-map> n j" 'conn-etts-number-backward
  "<conn-thing-map> d i l" 'conn-etts-parameter-inner-forward
  "<conn-thing-map> d i j" 'conn-etts-parameter-inner-backward
  "<conn-thing-map> d o l" 'conn-etts-parameter-outer-forward
  "<conn-thing-map> d o j" 'conn-etts-parameter-outer-backward
  "<conn-thing-map> x i l" 'conn-etts-regex-inner-forward
  "<conn-thing-map> x i j" 'conn-etts-regex-inner-backward
  "<conn-thing-map> x o l" 'conn-etts-regex-outer-forward
  "<conn-thing-map> x o j" 'conn-etts-regex-outer-backward
  "<conn-thing-map> t i l" 'conn-etts-return-inner-forward
  "<conn-thing-map> t i j" 'conn-etts-return-inner-backward
  "<conn-thing-map> t o l" 'conn-etts-return-outer-forward
  "<conn-thing-map> t o j" 'conn-etts-return-outer-backward
  "<conn-thing-map> S l" 'conn-etts-scopename-forward
  "<conn-thing-map> S j" 'conn-etts-scopename-backward)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-read-thing-state 'conn-etts-things-mode-map)
  "<conn-thing-map> w i" 'conn-etts-assignment-inner-forward
  "<conn-thing-map> w l" 'conn-etts-assignment-lhs-forward
  "<conn-thing-map> w o" 'conn-etts-assignment-outer-forward
  "<conn-thing-map> w r" 'conn-etts-assignment-rhs-forward
  "<conn-thing-map> @ i" 'conn-etts-attribute-inner-forward
  "<conn-thing-map> @ o" 'conn-etts-attribute-outer-forward
  "<conn-thing-map> b i" 'conn-etts-block-inner-forward
  "<conn-thing-map> b o" 'conn-etts-block-outer-forward
  "<conn-thing-map> . i" 'conn-etts-call-inner-forward
  "<conn-thing-map> . o" 'conn-etts-call-outer-forward
  "<conn-thing-map> C i" 'conn-etts-class-inner-forward
  "<conn-thing-map> C o" 'conn-etts-class-outer-forward
  "<conn-thing-map> c i" 'conn-etts-comment-inner-forward
  "<conn-thing-map> c o" 'conn-etts-comment-outer-forward
  "<conn-thing-map> q i" 'conn-etts-conditional-inner-forward
  "<conn-thing-map> q o" 'conn-etts-conditional-outer-forward
  "<conn-thing-map> [ i" 'conn-etts-frame-inner-forward
  "<conn-thing-map> [ o" 'conn-etts-frame-outer-forward
  "<conn-thing-map> f i" 'conn-etts-function-inner-forward
  "<conn-thing-map> f o" 'conn-etts-function-outer-forward
  "<conn-thing-map> r i" 'conn-etts-loop-inner-forward
  "<conn-thing-map> r o" 'conn-etts-loop-outer-forward
  "<conn-thing-map> n" 'conn-etts-number-forward
  "<conn-thing-map> d i" 'conn-etts-parameter-inner-forward
  "<conn-thing-map> d o" 'conn-etts-parameter-outer-forward
  "<conn-thing-map> x i" 'conn-etts-regex-inner-forward
  "<conn-thing-map> x o" 'conn-etts-regex-outer-forward
  "<conn-thing-map> t i" 'conn-etts-return-inner-forward
  "<conn-thing-map> t o" 'conn-etts-return-outer-forward
  "<conn-thing-map> S" 'conn-etts-scopename-forward)

(define-minor-mode conn-etts-things-mode
  "Minor mode for conn-etts things")

(provide 'conn-evil-textobj-tree-sitter)
