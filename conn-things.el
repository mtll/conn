;;; conn-things.el --- Things -*- lexical-binding: t -*-
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

;;; Commentary:

;;; Code:

(require 'conn-utils)
(require 'conn-mark)
(require 'conn-states)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn--end-of-inner-line-1 "conn-commands")
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")

;;;; Thing Types

(cl-defstruct (conn-bounds
               (:constructor conn--make-bounds)
               (:conc-name conn-bounds--))
  (thing nil :type symbol :read-only t)
  (arg nil :type (or nil integer) :read-only t)
  (whole nil :type cons)
  (properties nil :type list))

(defalias 'conn-bounds-thing 'conn-bounds--thing)
(defalias 'conn-bounds-arg 'conn-bounds--arg)

(cl-deftype conn-thing-function () '(satisfies conn-command-thing))

(cl-deftype conn-thing () '(satisfies conn-thing-p))

(define-inline conn-command-thing (cmd)
  (declare (side-effect-free t)
           (gv-setter conn-set-command-thing))
  (inline-letevals (cmd)
    (inline-quote
     (and (symbolp ,cmd)
          (get ,cmd :conn-command-thing)))))

(define-inline conn-thing-command-p (cmd)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (cmd)
    (inline-quote
     (and (symbolp ,cmd)
          (get ,cmd :conn-command-thing)
          t))))

(defun conn-set-command-thing (cmd thing)
  (put cmd :conn-command-thing thing))

(define-inline conn-thing-p (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (thing)
    (inline-quote
     (and (symbolp ,thing)
          (or (get ,thing :conn-thing)
              (get ,thing 'forward-op)
              (intern-soft (format "forward-%s" ,thing))
              (get ,thing 'end-op)
              (get ,thing 'bounds-of-thing-at-point))
          ,thing))))

(defconst conn--thing-all-parents-cache (make-hash-table :test 'eq))

(defun conn-thing-all-parents (thing)
  (declare (important-return-value t))
  (pcase thing
    ((let (and command-thing (pred identity))
       (conn-command-thing thing))
     (cons thing (conn-thing-all-parents command-thing)))
    ((pred conn-bounds-p)
     (conn-thing-all-parents (conn-bounds-thing thing)))
    ((pred conn-anonymous-thing-p)
     (conn-thing-all-parents (conn-anonymous-thing-parent thing)))
    ((pred conn-thing-p)
     (cl-loop for p = thing then (conn-get-thing-parent p)
              while p collect p))))

(defun conn-set-thing-parent (thing parent)
  (put thing :conn--thing-parent parent))

(define-inline conn-get-thing-parent (thing)
  (declare (gv-setter conn-set-thing-parent)
           (important-return-value t)
           (side-effect-free t))
  (inline-quote (get ,thing :conn--thing-parent)))

(defconst conn--thing-property-table (make-hash-table :test 'equal))

(defun conn-set-thing-property (thing property val)
  (setf (gethash (cons thing property) conn--thing-property-table)
        val))

(defun conn-unset-thing-property (thing property)
  (remhash (cons thing property) conn--thing-property-table))

(defun conn-get-thing-property (thing property)
  (declare (gv-setter conn-set-thing-property))
  (cl-loop for thing in (conn-thing-all-parents thing)
           for val = (gethash (cons thing property)
                              conn--thing-property-table
                              conn--key-missing)
           unless (eq conn--key-missing val) return val))

(cl-defun conn-register-thing (thing &key parent properties forward-op beg-op end-op bounds-op)
  (put thing :conn-thing t)
  (when parent
    (setf (conn-get-thing-parent thing) parent))
  (when forward-op
    (put thing 'forward-op forward-op))
  (when (or beg-op end-op)
    (cl-assert (and beg-op end-op)
               nil "If either beg-op or end-op is specified then both must be")
    (put thing 'beginning-op beg-op)
    (put thing 'end-op end-op))
  (when bounds-op
    (put thing 'bounds-of-thing-at-point bounds-op))
  (cl-loop for (prop val) on properties by #'cddr
           do (conn-set-thing-property thing prop val)))

(cl-defstruct (conn--anonymous-thing
               (:constructor nil)
               (:constructor conn--make-anonymous-thing)
               ;; This would be nice but cl-defsubst does not handle
               ;; &rest arguments properly and as a result PROPERTIES
               ;; gets evaluated twice in the expansion.
               ;; (:constructor conn-anonymous-thing (parent &rest properties))
               )
  (parent nil :read-only t)
  (methods nil :read-only t)
  (properties nil))

(defalias 'conn-anonymous-thing-parent 'conn--anonymous-thing-parent)
(defalias 'conn-anonymous-thing-p 'conn--anonymous-thing-p)

(defmacro conn-anonymous-thing (parent &rest properties)
  "Make an anonymous thing."
  (declare (indent 0))
  (cl-assert (plistp properties))
  (cl-loop with known = (get 'conn-anonymous-thing :known-properties)
           with seen = nil
           for (key var) on properties by #'cddr
           if (when-let* ((fn (alist-get key known)))
                (cl-assert (not (memq fn seen)) nil
                           "Duplicate method in anonymous thing definition")
                (push fn seen))
           nconc (let ((method-expander
                        (lambda (args &rest body)
                          (pcase (macroexpand `(cl-function (lambda ,args ,@body)))
                            (`#'(lambda ,args . ,body)
                             (let ((parsed-body (macroexp-parse-body body))
                                   (cnm (gensym "thing--cnm")))
                               `#'(lambda ,(cons cnm args)
                                    ,@(car parsed-body)
                                    ,(macroexpand-all
                                      `(cl-flet ((cl-call-next-method ,cnm))
                                         ,@(cdr parsed-body))))))
                            (result (error "Unexpected macroexpansion result :%S"
                                           result))))))
                   `(',(car seen)
                     ,(macroexpand-all
                       var
                       (cons (cons :method method-expander)
                             macroexpand-all-environment))))
           into methods
           else nconc (list key var) into props
           finally return `(conn--make-anonymous-thing
                            :parent ,parent
                            :methods ,(cons 'list methods)
                            :properties ,(cons 'list props))))

(define-inline conn-anonymous-thing-property (object property)
  (declare (side-effect-free t)
           (gv-setter
            (lambda (val)
              `(setf (plist-get (conn--anonymous-thing-properties ,object) ,property)
                     ,val))))
  (inline-quote
   (plist-get (conn--anonymous-thing-properties ,object) ,property)))

(define-inline conn--anonymous-thing-method (object method)
  (declare (side-effect-free t))
  (inline-quote
   (plist-get (conn--anonymous-thing-methods ,object) ,method)))

;; from cl--generic-make-defmethod-docstring/pcase--make-docstring
(defun conn--make-anonymous-thing-docstring ()
  (let* ((main (documentation (symbol-function 'conn-anonymous-thing) 'raw))
         (ud (help-split-fundoc main 'conn-anonymous-thing)))
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main ""))
      (insert "\n\n\tCurrently supported properties for anonymous things:\n\n")
      (pcase-dolist (`(,fn . ,props)
                     (seq-group-by #'cdr (get 'conn-anonymous-thing :known-properties)))
        (insert (format "`%s' - %s" fn (mapcar #'car props)))
        (insert "\n\n"))
      (let ((combined-doc (buffer-string)))
        (if ud
            (help-add-fundoc-usage combined-doc (car ud))
          combined-doc)))))

(put 'conn-anonymous-thing 'function-documentation
     '(conn--make-anonymous-thing-docstring))

(eval-and-compile
  (defun conn--set-anonymous-thing-property (f args &rest properties)
    `(progn
       (eval-and-compile
         (let ((props ',(cons (intern (concat ":" (symbol-name f)))
                              properties)))
           (dolist (prop props)
             (when-let* ((gfn (alist-get prop (get 'conn-anonymous-thing :known-properties)))
                         (_ (not (eq gfn ',f))))
               (error "%s already an anonymous thing property for %s" prop gfn)))
           (dolist (prop props)
             (setf (alist-get prop (get 'conn-anonymous-thing :known-properties))
                   ',f))))
       :autoload-end
       (cl-defmethod ,f ((,(car args) (conn-thing internal--anonymous-thing-method))
                         &rest rest)
         (if-let* ((op (conn--anonymous-thing-method ,(car args) ',f)))
             (apply op #'cl-call-next-method ,(car args) rest)
           (cl-call-next-method)))))
  (setf (alist-get 'conn-anonymous-thing-property defun-declarations-alist)
        (list #'conn--set-anonymous-thing-property)))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER.

HANDLER will be run from the `post-command-hook' and should be a
function of one argument, the location of `point' before the command
ran.  HANDLER is responsible for calling `conn--push-ephemeral-mark' in
order to mark the region that should be defined by any of COMMANDS."
  (dolist (cmd commands)
    (setf (conn-command-thing cmd) thing
          (conn-command-mark-handler cmd) handler)))

(defmacro conn-define-mark-command (name thing &optional ignore-mark-active)
  `(progn
     (defun ,name ()
       (interactive)
       (pcase (ignore-errors (bounds-of-thing-at-point ',thing))
         (`(,beg . ,end)
          (cond ((or ,ignore-mark-active
                     (not (region-active-p)))
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
     (conn-register-thing-commands ',thing 'ignore ',name)))

(defun conn-get-thing (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (pcase thing
    ((pred conn-bounds-p)
     (conn-bounds-thing thing))
    ((pred conn-anonymous-thing-p)
     (conn-anonymous-thing-parent thing))
    ((or (pred conn-thing-p)
         (pred conn-thing-command-p))
     thing)))

;;;; Specializers

(cl-generic-define-generalizer conn--thing-generalizer
  70 (lambda (thing &rest _)
       `(or (and (or (conn-thing-p ,thing)
                     (conn-thing-command-p ,thing))
                 ,thing)
            (and (conn-bounds-p ,thing)
                 (conn-bounds-thing ,thing))))
  (lambda (thing &rest _)
    (when thing
      `(,@(cl-loop for parent in (conn-thing-all-parents thing)
                   collect `(conn-thing ,parent))
        (conn-thing t)))))

(defconst conn--anonymous-thing-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--anonymous-thing-generalizer
  70 (lambda (thing &rest _)
       `(and (conn-anonymous-thing-p ,thing)
             (with-memoization
                 (gethash (conn-anonymous-thing-parent ,thing)
                          conn--anonymous-thing-tag-cache)
               (cons 'anonymous-thing
                     (conn-anonymous-thing-parent ,thing)))))
  (lambda (thing &rest _)
    (when thing
      `((conn-thing internal--anonymous-thing-method)
        ,@(cl-loop for parent in (conn-thing-all-parents (cdr thing))
                   collect `(conn-thing ,parent))
        (conn-thing t)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-thing)))
  "Support for (conn-thing THING) specializers."
  (list conn--anonymous-thing-generalizer
        conn--thing-generalizer))

;;;; Read Things

(cl-defgeneric conn-thing-print-pretty (thing)
  (declare (conn-anonymous-thing-property :description)
           (side-effect-free t))
  (:method (thing) (copy-sequence (symbol-name thing))))

;;;;; Thing Args

(oclosure-define (conn-thing-argument
                  (:parent conn-read-args-argument))
  (recursive-edit :type boolean))

(defun conn-thing-argument (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(defun conn-thing-argument-dwim (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p))
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-read-args-consume-prefix-arg)))
      self)))

(cl-defmethod conn-argument-value ((arg conn-thing-argument))
  (conn-read-args-argument-value arg))

(cl-defmethod conn-argument-predicate ((_arg conn-thing-argument)
                                       (_sym (conn-thing t)))
  t)

(cl-defmethod conn-argument-predicate ((arg conn-thing-argument)
                                       (_sym (eql recursive-edit)))
  (conn-thing-argument--recursive-edit arg))

;;;;;; Subregions

(defvar-keymap conn-subregions-map
  "z" 'toggle-subregions)

(oclosure-define (conn-subregions-argument
                  (:parent conn-read-args-argument)))

(defun conn-subregions-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-subregions-argument
                    (value value)
                    (keymap conn-subregions-map))
      (self cmd)
    (if (eq cmd 'toggle-subregions)
        (conn-set-argument
         self (not (conn-read-args-argument-value self)))
      (conn-subregions-default-value cmd self))))

(cl-defgeneric conn-subregions-default-value (cmd arg)
  ( :method (_ arg) arg))

(cl-defmethod conn-subregions-default-value ((_cmd (eql conn-things-in-region))
                                             arg)
  (conn-set-argument arg t))

(cl-defmethod conn-subregions-default-value ((_cmd (conn-thing region))
                                             arg)
  (conn-set-argument arg t))

(cl-defmethod conn-subregions-default-value ((_cmd (conn-thing recursive-edit-thing))
                                             arg)
  (conn-set-argument arg t))

(cl-defmethod conn-argument-predicate ((_arg conn-subregions-argument)
                                       (_sym (eql toggle-subregions)))
  t)

(cl-defmethod conn-argument-display ((arg conn-subregions-argument))
  (concat "\\[toggle-subregions] "
          (propertize "subregions"
                      'face (when (conn-read-args-argument-value arg)
                              'eldoc-highlight-function-argument))))

(defvar conn-subregions-argument-reference
  (conn-reference-page "Subregions"
    "If this argument is non-nil then operate on the subregions defined by
the thing command. By default the subregions of a thing command are the
individual things that are moved over. For example the subregions of
`forward-word' with a prefix argument of 3 are the 3 regions containing
the 3 individual words, as opposed to the single region containing all 3
words."))

;;;;;; Fixup Whitespace Argument

(oclosure-define (conn-fixup-whitespace-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-fixup-whitespace-argument-map
  "q" 'fixup-whitespace)

(defun conn-fixup-whitespace-argument (&optional value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-fixup-whitespace-argument
                    (value value)
                    (keymap conn-fixup-whitespace-argument-map))
      (self cmd)
    (if (eq cmd 'fixup-whitespace)
        (conn-set-argument self (null value))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-fixup-whitespace-argument)
                                       (_sym (eql fixup-whitespace)))
  t)

(cl-defmethod conn-argument-display ((arg conn-fixup-whitespace-argument))
  (substitute-command-keys
   (concat
    "\\[fixup-whitespace]: "
    (if-let* ((ts (conn-read-args-argument-value arg)))
        (propertize
         "fixup"
         'face 'eldoc-highlight-function-argument)
      "fixup"))))

;;;;;; Check Bounds Argument

(oclosure-define (conn-check-bounds-argument
                  (:parent conn-read-args-argument)))

(defvar-keymap conn-check-bounds-argument-map
  "!" 'check-bounds)

(defun conn-check-bounds-argument (&optional value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-check-bounds-argument
                    (value value)
                    (keymap conn-check-bounds-argument-map))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument self (null value))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-check-bounds-argument)
                                       (_sym (eql check-bounds)))
  t)

(cl-defmethod conn-argument-display ((arg conn-check-bounds-argument))
  (substitute-command-keys
   (concat
    "\\[check-bounds]: "
    (if-let* ((ts (conn-read-args-argument-value arg)))
        (propertize
         "check region"
         'face 'eldoc-highlight-function-argument)
      "check region"))))

;;;;;; Thing Transform Argument

(defvar conn-transformations-quick-ref
  (conn-reference-quote
    (("trim" conn-bounds-trim)
     ("after point/exclusive"
      conn-bounds-after-point
      conn-bounds-after-point-exclusive)
     ("before point/exclusive"
      conn-bounds-before-point
      conn-bounds-before-point-exclusive)
     ("last" conn-bounds-last)
     ("reset" conn-transform-reset))))

(defvar-keymap conn-transform-map
  "x" 'conn-bounds-trim
  "a" 'conn-bounds-after-point
  "A" 'conn-bounds-after-point-exclusive
  "b" 'conn-bounds-before-point
  "B" 'conn-bounds-before-point-exclusive
  "SPC" 'conn-bounds-last
  "X" 'conn-transform-reset)

(oclosure-define (conn-transform-argument
                  (:parent conn-read-args-wrapper)))

(cl-defmethod conn-argument-required-p ((_arg conn-transform-argument)) nil)

(defun conn-transform-argument (&rest initial)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-transform-argument
                    (name "transforms")
                    (separator "∘")
                    (contents initial)
                    (keymap conn-transform-map))
      (self cmd)
    (let ((next
           (cl-loop for tform in (conn-read-args-wrapper-contents self)
                    for update = (conn-argument-update tform cmd)
                    when update collect update)))
      (pcase cmd
        ('conn-transform-reset
         (conn-set-wrapper self nil))
        ((and (guard (symbolp cmd))
              (let (and handler (pred identity))
                (get cmd :conn-bounds-transform)))
         (cond ((memq cmd next)
                (conn-set-wrapper self (remq cmd next)))
               ((functionp handler)
                (funcall handler cmd self))
               (t (conn-set-wrapper self (cons cmd next)))))
        (_
         (if (equal next contents)
             self
           (conn-set-wrapper self next)))))))

(cl-defmethod conn-argument-predicate ((_arg conn-transform-argument)
                                       sym)
  (get sym :conn-bounds-transform))

(cl-defmethod conn-argument-display ((arg conn-transform-argument))
  (when-let* ((ts (conn-read-args-wrapper-contents arg)))
    (concat
     "transforms: "
     (propertize
      (mapconcat (lambda (tf)
                   (or (get tf :conn-transform-description) ""))
                 ts "∘")
      'face 'eldoc-highlight-function-argument))))

(cl-defmethod conn-argument-value ((_arg conn-transform-argument))
  (nreverse (cl-call-next-method)))

;;;;; Read Mover State

(conn-define-state conn-read-thing-state (conn-read-thing-common-state)
  "A state for reading things."
  :lighter "THG"
  :loop-completion-metadata `((affixation-function
                               . conn--dispatch-command-affixation)
                              (category
                               . conn-dispatch-command)))

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-q" 'help
  "," conn-thing-remap
  "'" 'recursive-edit
  "c" 'conn-things-in-region)

(put 'reset-arg :advertised-binding (key-parse "M-DEL"))

;;;; Bounds of Thing

(defun conn-bounds (bounds &optional transform)
  (declare (gv-setter
            (lambda (val)
              (ignore transform)
              `(setf (conn-bounds--whole ,bounds) ,val)))
           (compiler-macro
            (lambda (_exp)
              `(let* ((bounds ,bounds)
                      (w (conn-bounds--whole
                          ,(if transform
                               `(conn-transform-bounds bounds ,transform)
                             'bounds))))
                 (if (functionp w) (funcall w bounds) w))))
           (important-return-value t))
  (let ((w (conn-bounds--whole
            (if transform
                (conn-transform-bounds bounds transform)
              bounds))))
    (if (functionp w) (funcall w bounds) w)))

(defun conn-make-bounds (thing arg whole &rest properties)
  (declare (compiler-macro
            (lambda (_exp)
              `(conn--make-bounds
                :thing ,thing
                :arg ,arg
                :whole ,whole
                :properties (list ,@properties)))))
  (conn--make-bounds
   :thing thing
   :arg arg
   :whole whole
   :properties properties))

(cl-defstruct (conn-bounds-transform
               (:include conn-bounds)
               (:constructor conn--make-bounds-transform))
  (parent nil :type conn-bounds :read-only t))

(defun conn-make-bounds-transform (from to &rest properties)
  (declare (compiler-macro
            (lambda (_exp)
              `(conn--make-bounds-transform
                :thing (conn-bounds-thing ,from)
                :arg (conn-bounds-arg ,from)
                :whole ,to
                :parent ,from
                :properties (list ,@properties)))))
  (conn--make-bounds-transform
   :thing (conn-bounds-thing from)
   :arg (conn-bounds-arg from)
   :whole to
   :parent from
   :properties properties))

(defun conn-bounds-set (bounds prop val)
  (setf (plist-get (conn-bounds--properties bounds) prop) val))

(defun conn-bounds-get (bounds prop &optional transform)
  (declare (gv-setter conn-bounds-set))
  (when bounds
    (let ((p (plist-get (conn-bounds--properties bounds) prop)))
      (conn-transform-bounds
       (if (functionp p) (funcall p bounds) p)
       transform))))

(pcase-defmacro conn-bounds-get (property &optional transform pat)
  (static-if (< emacs-major-version 30)
      `(and (pred conn-bounds-p)
            (app (lambda (v) (conn-bounds-get ,property v ,transform))
                 ,(or pat (if (keywordp property)
                              (intern (substring (symbol-name property) 1))
                            property))))
    `(and (pred conn-bounds-p)
          (app (conn-bounds-get _ ,property ,transform)
               ,(or pat (if (keywordp property)
                            (intern (substring (symbol-name property) 1))
                          property))))))

(pcase-defmacro conn-bounds (pattern &optional transform)
  `(and (pred conn-bounds-p)
        (app ,(static-if (< emacs-major-version 30)
                  `(pcase--flip conn-bounds ,transform)
                `(conn-bounds _ ,transform))
             ,pattern)))

(defun conn-transform-bounds (bounds transforms)
  (catch 'break
    (cond ((null transforms)
           bounds)
          ((consp bounds)
           (delq nil
                 (seq-reduce (lambda (bounds transform)
                               (unless bounds (throw 'break nil))
                               (mapcar transform bounds))
                             transforms bounds)))
          (bounds
           (seq-reduce (lambda (bounds transform)
                         (unless bounds (throw 'break nil))
                         (funcall transform bounds))
                       transforms bounds)))))

(pcase-defmacro conn-transform-bounds (transform pat)
  (static-if (< emacs-major-version 30)
      `(app (pcase--flip conn-transform-bounds ,transform) ,pat)
    `(app (conn-transform-bounds _ ,transform) ,pat)))

(defvar conn--bounds-of-in-progress nil)

(defun conn--get-boundable-thing (thing)
  (catch 'boundable
    (while (pcase thing
             ((or (and (pred conn-thing-command-p)
                       (pred conn-command-mark-handler))
                  (pred conn-thing-p))
              (throw 'boundable thing))
             ((pred conn-anonymous-thing-p)
              (setq thing (conn-anonymous-thing-parent thing)))
             ((pred conn-bounds-p)
              (setq thing (conn-bounds-thing thing)))))))

(cl-defgeneric conn-bounds-of (cmd arg &key &allow-other-keys)
  (declare (conn-anonymous-thing-property :bounds-op)
           (important-return-value t)))

(cl-defmethod conn-bounds-of :around (&rest _)
  (if conn--bounds-of-in-progress
      (cl-call-next-method)
    (setf (alist-get (recursion-depth) conn--last-perform-bounds)
          (let ((conn--bounds-of-in-progress t))
            (save-mark-and-excursion
              (cl-call-next-method))))))

(cl-defmethod conn-bounds-of ((thing (conn-thing t)) arg)
  (pcase (conn--get-boundable-thing thing)
    ((and thing (pred conn-thing-p))
     (conn-make-bounds
      thing arg
      (bounds-of-thing-at-point thing)))
    ((and thing
          (let (and conn-this-command-handler
                    (pred identity))
            (conn-command-mark-handler thing)))
     (let (conn--last-perform-bounds)
       (deactivate-mark t)
       (pcase (prefix-numeric-value arg)
         (0 nil)
         ((and n (or 1 -1))
          (let ((pt (point))
                (mk (mark))
                (current-prefix-arg n)
                (conn-this-command-thing (conn-command-thing thing))
                (conn-this-command-start (point-marker))
                (this-command thing))
            (unwind-protect
                (progn
                  (ignore-errors
                    (call-interactively thing)
                    (funcall conn-this-command-handler conn-this-command-start))
                  (unless (and (eql pt (point))
                               (eql mk (mark)))
                    (conn-make-bounds
                     thing 1
                     (cons (region-beginning)
                           (region-end)))))
              (set-marker conn-this-command-start nil))))
         (n
          (let (subregions)
            (catch 'break
              (dotimes (_ (abs n))
                (if-let* ((bound (conn-bounds-of thing (cl-signum arg))))
                    (push bound subregions)
                  (throw 'break nil))))
            (conn-make-bounds
             thing arg
             (cl-loop for bound in subregions
                      for (b . e) = (conn-bounds bound)
                      minimize b into beg
                      maximize e into end
                      finally return (cons beg end))
             :subregions (nreverse subregions)))))))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing region)) arg)
  (conn-make-bounds
   cmd arg
   (cons (region-beginning) (region-end))
   :subregions (cl-loop for r in (region-bounds)
                        collect (conn-make-bounds cmd arg r))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing buffer)) arg)
  (conn-make-bounds cmd arg (cons (point-min) (point-max))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing visible)) arg)
  (conn-make-bounds cmd arg (cons (window-start) (window-end))))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-bounds-of)) _arg)
  (alist-get (recursion-depth) conn--last-perform-bounds))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-previous-mark-command))
                              _arg)
  (save-mark-and-excursion
    (goto-char (nth 0 conn-previous-mark-state))
    (conn--push-ephemeral-mark (nth 1 conn-previous-mark-state) nil t)
    (when (nth 2 conn-previous-mark-state)
      (rectangle-mark-mode 1))
    (cl-call-next-method)))

(defvar conn--bounds-last-kbd-macro nil)

(cl-defmethod conn-bounds-of ((cmd (conn-thing kbd-macro))
                              arg)
  (save-mark-and-excursion
    (pcase cmd
      ((and (or 'start-kbd-macro
                'kmacro-start-macro
                'kmacro-start-macro-or-insert-counter)
            (guard (not (and (fboundp 'repeat-is-really-this-command)
                             (repeat-is-really-this-command)))))
       (let ((buffer-read-only t)
             (last-kbd-macro conn--bounds-last-kbd-macro))
         (start-kbd-macro arg)
         (unwind-protect
             (conn-with-recursive-stack 'conn-command-state
               (recursive-edit))
           (if defining-kbd-macro
               (end-kbd-macro)
             (error "Not defining kbd macro"))
           (setq conn--bounds-last-kbd-macro last-kbd-macro))))
      (_
       (conn-with-recursive-stack 'conn-command-state
         (execute-kbd-macro conn--bounds-last-kbd-macro))))
    (conn-bounds-of 'region nil)))

;;;;; Bounds Transformations

;;;;;; Last Bounds

(put 'conn-bounds-last :conn-bounds-transform t)
(put 'conn-bounds-last :conn-transform-description "last")

(cl-defmethod conn-argument-update ((arg (eql 'conn-bounds-last)) form)
  (unless (memq form '(toggle-subregions
                       conn-bounds-after-point
                       conn-bounds-before-point))
    arg))

(cl-defgeneric conn-bounds-last (bounds)
  ( :method ((bounds (conn-thing dispatch)))
    (ignore (conn-bounds bounds))
    (or (car (conn-bounds-get bounds :subregions))
        bounds))
  ( :method (bounds)
    (or (car (last (conn-bounds-get bounds :subregions)))
        bounds)))

;;;;;; Trim Bounds

(defvar conn-bounds-trim-chars " \t\r\n")

(put 'conn-bounds-trim :conn-bounds-transform t)
(put 'conn-bounds-trim :conn-transform-description "trim")

(cl-defgeneric conn-bounds-trim (bounds))

(cl-defmethod conn-bounds-trim (bounds)
  (pcase-let* (((conn-bounds `(,beg . ,end)) bounds)
               (tb (save-excursion
                     (goto-char beg)
                     (skip-chars-forward conn-bounds-trim-chars end)
                     (point)))
               (te (save-excursion
                     (goto-char end)
                     (skip-chars-backward conn-bounds-trim-chars beg)
                     (point))))
    (unless (> tb te)
      (conn-make-bounds-transform bounds (cons tb te)))))

;;;;;; Bounds Before/After

(put 'conn-bounds-after-point :conn-bounds-transform t)
(put 'conn-bounds-after-point :conn-transform-description "after")

(cl-defmethod conn-argument-update ((arg (eql 'conn-bounds-after-point)) form)
  (unless (or (eq form 'conn-bounds-last)
              (eq form 'conn-bounds-before-point)
              (eq form 'conn-bounds-before-point-exclusive)
              (eq form 'conn-bounds-after-point)
              (eq form 'conn-bounds-after-point-exclusive))
    arg))

(cl-defgeneric conn-bounds-after-point (bounds &optional exclusive))

(cl-defmethod conn-bounds-after-point (bounds &optional exclusive)
  (pcase-let (((conn-bounds `(,beg . ,end)) bounds))
    (if (<= (point) end)
        (conn-make-bounds-transform
         bounds (cons (point) (if exclusive beg end)))
      (error "Invalid bounds"))))

(put 'conn-bounds-after-point-exclusive :conn-bounds-transform t)
(put 'conn-bounds-after-point-exclusive :conn-transform-description "after exclusive")

(cl-defmethod conn-argument-update ((arg (eql 'conn-bounds-after-point-exclusive))
                                    form)
  (unless (or (eq form 'conn-bounds-last)
              (eq form 'conn-bounds-before-point)
              (eq form 'conn-bounds-before-point-exclusive)
              (eq form 'conn-bounds-after-point)
              (eq form 'conn-bounds-after-point-exclusive))
    arg))

(defun conn-bounds-after-point-exclusive (bounds)
  (conn-bounds-after-point bounds t))

(put 'conn-bounds-before-point :conn-bounds-transform t)
(put 'conn-bounds-before-point :conn-transform-description "before")

(cl-defmethod conn-argument-update ((arg (eql 'conn-bounds-before-point)) form)
  (unless (or (eq form 'conn-bounds-last)
              (eq form 'conn-bounds-before-point)
              (eq form 'conn-bounds-before-point-exclusive)
              (eq form 'conn-bounds-after-point)
              (eq form 'conn-bounds-after-point-exclusive))
    arg))

(cl-defgeneric conn-bounds-before-point (bounds &optional exclusive))

(cl-defmethod conn-bounds-before-point (bounds &optional exclusive)
  (pcase-let (((conn-bounds `(,beg . ,end)) bounds))
    (if (>= (point) beg)
        (conn-make-bounds-transform
         bounds (cons (if exclusive end beg) (point)))
      (error "Invalid bounds"))))

(put 'conn-bounds-before-point-exclusive :conn-bounds-transform t)
(put 'conn-bounds-before-point-exclusive :conn-transform-description "before exclusive")

(cl-defmethod conn-argument-update ((arg (eql 'conn-bounds-before-point-exclusive))
                                    form)
  (unless (or (eq form 'conn-bounds-last)
              (eq form 'conn-bounds-before-point)
              (eq form 'conn-bounds-before-point-exclusive)
              (eq form 'conn-bounds-after-point)
              (eq form 'conn-bounds-after-point-exclusive))
    arg))

(defun conn-bounds-before-point-exclusive (bounds)
  (conn-bounds-before-point bounds t))

;;;;;; Check Bounds

(defvar-local conn-check-bounds-functions nil)

(defun conn-check-bounds (bounds)
  (cl-loop for fn in conn-check-bounds-functions
           do (funcall fn bounds)
           finally return bounds))

;;;;; Perform Bounds

(defvar conn--bounds-of-recursive-edit nil)

(cl-defmethod conn-bounds-of ((_cmd (conn-thing recursive-edit-thing)) _arg)
  (let* ((eldoc-message-function 'ignore)
         (conn--bounds-of-recursive-edit t)
         (pre (lambda ()
                (message
                 (substitute-command-keys
                  (concat
                   "Recursive Edit: Press \\[exit-recursive-edit] to exit, "
                   "\\[abort-recursive-edit] to abort"))))))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook pre)
          (conn-with-recursive-stack 'conn-command-state
            (funcall pre)
            (recursive-edit))
          (conn-bounds-of 'region nil))
      (remove-hook 'pre-command-hook pre))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing emacs-state)) arg)
  (setq arg (prefix-numeric-value arg))
  (when (> arg 0) (cl-decf arg))
  (when (eq cmd 'conn-next-emacs-state)
    (setq arg (- arg)))
  (let* ((ring (conn-ring-list conn-emacs-state-ring))
         (mk (nth (mod arg (length ring)) ring))
         (pt (point)))
    (conn-make-bounds cmd arg (cons (min pt mk) (max pt mk)))))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-previous-mark-command)) _arg)
  (unless conn-previous-mark-state
    (user-error "No previous mark state"))
  (save-mark-and-excursion
    (goto-char (nth 0 conn-previous-mark-state))
    (conn--push-ephemeral-mark (nth 1 conn-previous-mark-state)
                               nil t)
    (pcase (nth 2 conn-previous-mark-state)
      (`(,pc . ,mc)
       (rectangle-mark-mode 1)
       (rectangle--reset-crutches)
       (save-excursion
         (goto-char (mark))
         (rectangle--col-pos mc 'mark))
       (rectangle--col-pos pc 'point)))
    (conn-bounds-of 'region nil)))

(cl-defmethod conn-bounds-of ((_cmd (conn-thing narrow-ring)) _arg)
  (when conn-narrow-ring
    (let ((subregions nil)
          (beg most-positive-fixnum)
          (end most-negative-fixnum))
      (pcase-dolist ((and bound `(,b . ,e))
                     (conn-ring-list conn-narrow-ring))
        (cl-callf max end e)
        (cl-callf min beg b)
        (push (conn-make-bounds 'region nil bound)
              subregions))
      (when subregions
        (conn-make-bounds
         'narrow-ring nil
         (cons beg end)
         :subregions subregions)))))

;;;;; Bounds of Remote Thing

(cl-defgeneric conn-bounds-of-remote (cmd arg pt)
  (declare (conn-anonymous-thing-property :bounds-op-remote)
           (important-return-value t))
  ( :method (cmd arg pt)
    (save-excursion
      (goto-char pt)
      (conn-bounds-of cmd arg))))

(cl-defmethod conn-bounds-of-remote ((_cmd (conn-thing region))
                                     arg pt)
  (conn-make-bounds
   'region arg
   (cons (min (point) pt)
         (max (point) pt))))

(cl-defmethod conn-bounds-of-remote ((_cmd (conn-thing char))
                                     arg pt)
  (conn-make-bounds
   'region arg
   (cons (min (point) pt)
         (max (point) pt))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing isearch)) _arg)
  (conn-read-args (conn-read-thing-state
                   :prompt "Thing")
      ((`(,thing ,thing-arg) (conn-thing-argument)))
    (let* ((name (symbol-name cmd))
           (at nil)
           (quit (lambda ()
                   (when (or isearch-mode-end-hook-quit
                             (null isearch-other-end))
                     (abort-recursive-edit))
                   (setq at (min (point) isearch-other-end)))))
      (unwind-protect
          (save-mark-and-excursion
            (add-hook 'isearch-mode-end-hook quit)
            (isearch-mode (not (string-match-p "backward" name))
                          (string-match-p "regexp" name)
                          nil t))
        (remove-hook 'isearch-mode-end-hook quit))
      (conn-bounds-of-remote thing thing-arg at))))

;;;; Bounds of Things in Region

(cl-defgeneric conn-get-things-in-region (thing beg end)
  (declare (conn-anonymous-thing-property :things-in-region)
           (important-return-value t)))

(cl-defmethod conn-get-things-in-region ((thing (conn-thing t))
                                         beg end)
  (setf thing (conn--get-boundable-thing thing))
  (when (or (conn-thing-p thing)
            (conn-thing-p (setf thing (conn-command-thing thing))))
    (save-excursion
      (goto-char beg)
      (forward-thing thing 1)
      (cl-loop for bd = (cons (save-excursion
                                (forward-thing thing -1)
                                (point))
                              (point))
               while (and bd (< (car bd) end))
               collect (conn-make-bounds thing nil bd) into sr
               minimize (car bd) into b
               maximize (cdr bd) into e
               while (and (< (point) end)
                          (ignore-errors
                            (forward-thing thing 1)
                            t))
               finally return (conn-make-bounds
                               thing nil
                               (cons b e)
                               :subregions sr)))))

(conn-register-thing 'conn-things-in-region)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-things-in-region)) _arg)
  (conn-read-args (conn-read-thing-state
                   :prompt "Things in Region")
      ((thing (car (conn-thing-argument-dwim))))
    (thread-first
      (cl-loop for parent in (conn-thing-all-parents thing)
               when (conn-thing-p parent) return parent
               finally (user-error "Not a valid things in region thing"))
      (conn-get-things-in-region (region-beginning) (region-end)))))

;;;; Thing Definitions

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)
(conn-define-mark-command conn-mark-comment comment)

(conn-register-thing 'kbd-macro)

(conn-register-thing-commands
 'kbd-macro nil
 'kmacro-start-macro
 'kmacro-start-macro-or-insert-counter
 'kmacro-end-and-call-macro
 'kmacro-end-macro
 'kmacro-call-macro
 'start-kbd-macro
 'end-kbd-macro)

(conn-register-thing 'narrow-ring)

(conn-register-thing-commands
 'narrow-ring nil
 'conn-cycle-narrowings
 'conn-narrow-ring-prefix)

(conn-register-thing
 'comment
 :bounds-op (lambda ()
              (if (conn--point-in-comment-p)
                  (cons (save-excursion
                          (while (and (conn--point-in-comment-p)
                                      (not (eobp)))
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r"))
                          (skip-chars-backward " \t\n\r")
                          (point))
                        (save-excursion
                          (while (conn--point-in-comment-p)
                            (forward-char -1)
                            (skip-chars-backward " \t\n\r"))
                          (skip-chars-forward " \t\n\r")
                          (unless (conn--point-in-comment-p)
                            (forward-char 1))
                          (point)))
                (error "Point not in comment"))))

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :properties '(:linewise t))

(conn-register-thing
 'visual-line
 :forward-op (lambda (&optional N)
               (let ((line-move-visual t))
                 (vertical-motion 0)
                 (line-move N t))))

(conn-define-mark-command conn-mark-visual-line visual-line)

(conn-register-thing
 'region
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing-commands
 'region nil
 'conn-previous-mark-command
 'conn-toggle-mark-command
 'conn-set-mark-command)

(conn-register-thing
 'visible
 :bounds-op (lambda () (cons (window-start) (window-end)))
 :properties '(:linewise t))

(conn-register-thing-commands
 'visible nil
 'conn-scroll-up 'conn-scroll-down
 'scroll-up-command 'scroll-down-command
 'conn-mark-visible)

(conn-define-mark-command conn-mark-visible visible)

(conn-register-thing
 'recursive-edit-thing
 :parent 'region)

(conn-register-thing-commands
 'recursive-edit-thing nil
 'recursive-edit 'exit-recursive-edit)

(conn-register-thing 'isearch)

(conn-register-thing-commands
 'isearch nil
 'isearch-forward
 'isearch-backward
 'isearch-forward-regexp
 'isearch-backward-regexp
 'isearch-forward-symbol
 'isearch-forward-word
 'isearch-forward-symbol-at-point
 'isearch-forward-thing-at-point)

(conn-register-thing 'symbol :forward-op 'forward-symbol)

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing
 'page
 :forward-op 'forward-page
 :properties '(:linewise t))

(conn-register-thing-commands
 'page 'conn-discrete-thing-handler
 'forward-page 'backward-page)

(defun conn-char-mark-handler (beg)
  (when current-prefix-arg
    (conn--push-ephemeral-mark beg)))

(conn-register-thing-commands
 'char 'conn-char-mark-handler
 'forward-char 'backward-char)

(conn-register-thing-commands
 'word 'conn-continuous-thing-handler
 'forward-word 'backward-word
 'upcase-word 'downcase-word 'capitalize-word
 'upcase-dwim 'downcase-dwim 'capitalize-dwim)

(conn-register-thing 'sexp :forward-op 'forward-sexp)

(conn-register-thing-commands
 'sexp 'conn-continuous-thing-handler
 'forward-sexp 'backward-sexp)

(conn-register-thing 'list :forward-op 'forward-list)

(conn-register-thing-commands
 'list 'conn-continuous-thing-handler
 'forward-list 'backward-list)

(defun conn--up-list-mark-handler (beg)
  (condition-case _err
      (cond ((> (point) beg)
             (save-excursion
               (forward-thing 'list -1)
               (conn--push-ephemeral-mark (point))))
            ((< (point) beg)
             (save-excursion
               (forward-thing 'list 1)
               (conn--push-ephemeral-mark (point)))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--up-list-mark-handler
 'up-list 'backward-up-list)

(defun conn--down-list-mark-handler (_beg)
  (condition-case _err
      (cond ((= (point) (save-excursion
                          (up-list 1 t t)
                          (down-list -1 t)
                          (point)))
             (conn--push-ephemeral-mark (save-excursion
                                          (up-list -1 t t)
                                          (down-list 1 t)
                                          (point))))
            ((= (point) (save-excursion
                          (up-list -1 t t)
                          (down-list 1 t)
                          (point)))
             (conn--push-ephemeral-mark (save-excursion
                                          (up-list 1 t t)
                                          (down-list -1 t)
                                          (point)))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'down-list)

(conn-register-thing 'whitespace :forward-op 'forward-whitespace)

(conn-register-thing-commands
 'whitespace 'conn-discrete-thing-handler
 'forward-whitespace 'conn-backward-whitespace)

(conn-register-thing 'sentence :forward-op 'forward-sentence)

(conn-register-thing-commands
 'sentence 'conn-continuous-thing-handler
 'forward-sentence 'backward-sentence)

(conn-register-thing
 'paragraph
 :forward-op 'forward-paragraph
 :properties '(:linewise t))

(conn-register-thing-commands
 'paragraph 'conn-continuous-thing-handler
 'forward-paragraph 'backward-paragraph)

(conn-register-thing-commands
 'defun 'conn-continuous-thing-handler
 'end-of-defun 'beginning-of-defun
 'conn-forward-defun)

(conn-register-thing-commands
 'buffer 'conn-discrete-thing-handler
 'end-of-buffer 'beginning-of-buffer)

(conn-register-thing
 'line
 :properties '(:linewise t))

(conn-register-thing-commands
 'line 'conn-continuous-thing-handler
 'forward-line 'conn-backward-line
 'conn-line-forward-op
 'conn-goto-line)

(conn-register-thing 'line-column :forward-op 'next-line)

(conn-register-thing-commands
 'line-column 'conn-jump-handler
 'next-line 'previous-line
 'rectangle-next-line 'rectangle-previous-line)

(conn-register-thing-commands 'line nil 'comment-line)

(conn-register-thing
 'outer-line
 :beg-op (lambda () (move-beginning-of-line nil))
 :end-op (lambda () (move-end-of-line nil)))

(conn-register-thing-commands
 'outer-line 'conn-discrete-thing-handler
 'move-beginning-of-line 'move-end-of-line
 'org-beginning-of-line 'org-end-of-line)

(defun conn--bounds-of-inner-line ()
  (cons
   (save-excursion
     (back-to-indentation)
     (point))
   (save-excursion
     (conn--end-of-inner-line-1)
     (point))))

(conn-register-thing
 'inner-line
 :bounds-op 'conn--bounds-of-inner-line
 :forward-op 'conn-forward-inner-line)

(conn-register-thing-commands
 'inner-line 'conn-continuous-thing-handler
 'conn-forward-inner-line-dwim
 'conn-backward-inner-line-dwim
 'back-to-indentation
 'conn-forward-inner-line
 'conn-backward-inner-line
 'conn-beginning-of-inner-line
 'conn-end-of-inner-line
 'comment-line)

(conn-register-thing-commands
 'org-link 'conn-discrete-thing-handler
 'org-next-link 'org-previous-link)

(conn-register-thing-commands
 'org-link nil
 'org-insert-link-global 'org-store-link 'org-insert-link)

(conn-register-thing-commands
 'expansion nil
 'conn-expand 'conn-contract)

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'conn-beginning-of-list
 'conn-end-of-list)

(provide 'conn-things)
