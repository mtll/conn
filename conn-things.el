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
(require 'conn-states)
(require 'thingatpt)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn--end-of-inner-line-1 "conn-commands")
(declare-function conn-exchange-mark-command "conn-command")
(declare-function rectangle--reset-crutches "rect")
(declare-function rectangle--col-pos "rect")

;;;; Mark Handlers

(defvar-local conn-mark-handler-overrides-alist nil
  "Buffer local overrides for command mark handlers.

Is an alist of the form ((CMD . MARK-HANDLER) ...).

For the meaning of MARK-HANDLER see `conn-command-mark-handler'.")

(define-inline conn-command-mark-handler (command)
  "Return the mark handler for COMMAND."
  (declare (important-return-value t)
           (side-effect-free t)
           (gv-setter conn-set-command-mark-handler))
  (inline-letevals (command)
    (inline-quote
     (and (symbolp ,command)
          (or (alist-get ,command conn-mark-handler-overrides-alist)
              (function-get ,command :conn-mark-handler t))))))

(defun conn-set-command-mark-handler (command handler)
  (function-put command :conn-mark-handler handler))

(defun conn-continuous-thing-handler (thing beg)
  "Mark the things which have been moved over."
  (ignore-errors
    (cond ((= 0 (abs (prefix-numeric-value current-prefix-arg))))
          ((= (point) beg)
           (pcase (bounds-of-thing-at-point thing)
             (`(,beg . ,end)
              (cond ((= (point) beg) end)
                    ((= (point) end) beg)))))
          ((let ((dir (pcase (- (point) beg)
                        (0 0)
                        ((pred (< 0)) 1)
                        ((pred (> 0)) -1))))
             (save-excursion
               (goto-char beg)
               (forward-thing thing dir)
               (forward-thing thing (- dir))
               (point)))))))

(defun conn-discrete-thing-handler (thing _beg)
  "Mark the thing at point."
  (pcase (ignore-errors (bounds-of-thing-at-point thing))
    (`(,beg . ,end)
     (if (= (point) end) beg end))))

(defun conn-jump-handler (_thing beg)
  "Place a mark where point used to be."
  (unless (= beg (point)) beg))

(defun conn--thing-pre-command-hook ()
  (set-marker conn-this-command-start (point) (current-buffer)))

(defun conn--thing-post-command-hook ()
  (unless conn--last-thing-command-pos
    (setf conn--last-thing-command-pos (make-marker)))
  (cond (conn--last-thing-override
         (pcase-let ((`(,mk . ,thing+arg) conn--last-thing-override))
           (setf conn--last-thing-command thing+arg)
           (set-marker conn--last-thing-command-pos
                       (and mk (marker-position mk)))
           (when mk (set-marker mk nil)))
         (setf conn--last-thing-override nil))
        ((and (conn-thing-command-p this-command)
              (eq (current-buffer)
                  (marker-buffer conn-this-command-start)))
         (set-marker conn--last-thing-command-pos
                     (marker-position conn-this-command-start))
         (setf conn--last-thing-command
               (cons this-command current-prefix-arg)))))

;;;; Thing Types

(cl-defstruct (conn-bounds
               (:constructor conn--make-bounds)
               (:conc-name conn-bounds--))
  (thing nil :type symbol :read-only t)
  (arg nil :type (or nil integer) :read-only t)
  (whole nil :type cons)
  (properties nil :type list))

(oclosure-define (conn-bounds-delay
                  (:predicate conn-bounds-delay-p)))

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
     (and (function-get ,cmd :conn-command-thing)
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

(cl-defun conn-register-thing (thing
                               &key
                               parent
                               properties
                               forward-op
                               beg-op
                               end-op
                               bounds-op)
  "Register a THING."
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
  (parent nil)
  (methods nil :read-only t)
  (properties nil))

(defalias 'conn-anonymous-thing-parent 'conn--anonymous-thing-parent)
(defalias 'conn-anonymous-thing-p 'conn--anonymous-thing-p)

(defun conn--anonymous-thing-parse-properties (properties)
  (cl-loop
   with known = (get 'conn-anonymous-thing :known-properties)
   with seen = nil
   for (key val) on properties by #'cddr
   if (when-let* ((fn (alist-get key known)))
        (cl-assert (not (memq fn seen)) nil
                   "Duplicate method in anonymous thing definition")
        (push fn seen))
   collect (let ((method-expander
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
             `(cons ',(car seen)
                    ,(macroexpand-all
                      val
                      (cons (cons :method method-expander)
                            macroexpand-all-environment))))
   into methods
   else collect `(cons ,key ,val) into props
   finally return (cons (cons 'list methods)
                        (cons 'list props))))

(defmacro conn-anonymous-thing (parent &rest properties)
  "Make an anonymous thing inheriting from PARENT."
  (declare (indent 0)
           (debug (sexp [&rest symbolp form])))
  (cl-assert (plistp properties))
  (pcase-let ((`(,methods . ,props)
               (conn--anonymous-thing-parse-properties properties)))
    `(conn--make-anonymous-thing
      :parent ,parent
      :methods ,methods
      :properties ,props)))

(define-inline conn-anonymous-thing-property (object property)
  (declare (side-effect-free t)
           (gv-setter
            (lambda (val)
              `(setf (alist-get ,property (conn--anonymous-thing-properties ,object))
                     ,val))))
  (inline-quote
   (alist-get ,property (conn--anonymous-thing-properties ,object))))

(define-inline conn--anonymous-thing-method (object method)
  (declare (side-effect-free t))
  (inline-quote
   (alist-get ,method (conn--anonymous-thing-methods ,object))))

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
             (when-let* ((gfn (alist-get prop
                                         (get 'conn-anonymous-thing
                                              :known-properties)))
                         (_ (not (eq gfn ',f))))
               (error "%s already an anonymous thing property for %s" prop gfn)))
           (dolist (prop props)
             (setf (alist-get prop (get 'conn-anonymous-thing
                                        :known-properties))
                   ',f))))
       :autoload-end
       (cl-defmethod ,f ((,(car args) (conn-thing internal--anonymous-thing-method))
                         &rest rest)
         (if-let* ((thing (conn-get-thing ,(car args)))
                   (op (conn--anonymous-thing-method thing ',f)))
             (apply op #'cl-call-next-method thing rest)
           (cl-call-next-method)))))
  (setf (alist-get 'conn-anonymous-thing-property defun-declarations-alist)
        (list #'conn--set-anonymous-thing-property)))

(defun conn-register-thing-commands (thing handler &rest commands)
  "Associate COMMANDS with a THING and a HANDLER."
  (unless (conn-thing-p thing)
    (error "%s is not a known thing" thing))
  (dolist (cmd commands)
    (setf (conn-command-thing cmd) thing
          (conn-command-mark-handler cmd) handler)))

(eval-and-compile
  (defun conn--mark-for-mark-command (region ignore-mark-active)
    (pcase region
      (`(,beg . ,end)
       (cond ((or ignore-mark-active
                  (not (region-active-p)))
              (goto-char beg)
              (push-mark end t))
             ((= (point) (mark))
              (pcase (car (read-multiple-choice
                           "Mark"
                           '((?a "after point")
                             (?b "before point"))))
                (?e (goto-char end))
                (?b (goto-char beg))))
             ((> (point) (mark)) (goto-char end))
             (t (goto-char beg))))
      (_ (user-error "Invalid region")))))

(defmacro conn-define-mark-command (name thing &optional ignore-mark-active)
  (declare (autoload-macro expand))
  `(progn
     (defun ,name ()
       (interactive)
       (conn--mark-for-mark-command
        (bounds-of-thing-at-point ',thing)
        ,ignore-mark-active))
     (conn-register-thing-commands ',thing 'ignore ',name)))

(define-inline conn-get-thing (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (thing)
    (inline-quote
     (pcase ,thing
       ((pred conn-bounds-p)
        (conn-bounds-thing ,thing))
       ((or (pred conn-thing-p)
            (pred conn-thing-command-p)
            (pred conn-anonymous-thing-p))
        ,thing)))))

;;;; Specializers

(cl-generic-define-generalizer conn--thing-generalizer
  70 (lambda (thing &rest _)
       `(let ((th (conn-get-thing ,thing)))
          (and (or (conn-thing-p th)
                   (conn-thing-command-p th))
               th)))
  (lambda (thing &rest _)
    (when thing
      `(,@(cl-loop for parent in (conn-thing-all-parents thing)
                   collect `(conn-thing ,parent))
        (conn-thing t)))))

(defconst conn--anonymous-thing-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--anonymous-thing-generalizer
  70 (lambda (thing &rest _)
       `(let ((th (conn-get-thing ,thing)))
          (and (conn-anonymous-thing-p th)
               (with-memoization
                   (gethash (conn-anonymous-thing-parent th)
                            conn--anonymous-thing-tag-cache)
                 (cons 'anonymous-thing
                       (conn-anonymous-thing-parent th))))))
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

(cl-defgeneric conn-thing-pretty-print (thing)
  (declare (conn-anonymous-thing-property :pretty-print)
           (side-effect-free t))
  (:method (thing) (format "%s" thing))
  (:method ((thing symbol)) (copy-sequence (symbol-name thing)))
  ( :method ((thing conn--anonymous-thing))
    (format "<anonymous %s %s>"
            (conn-anonymous-thing-parent thing)
            (substring (secure-hash 'sha1 (prin1-to-string thing)) 0 8))))

;;;;; Thing Args

(defcustom conn-thing-argument-region-dwim t
  "When non-nil makes thing commands always use the region when active.

This causes all thing commands to skip reading any arguments when the
region is active. If you would prefer to thing commands to read
arguments even when the region is active then set this variable to nil."
  :group 'conn
  :type 'boolean)

(defvar-keymap conn-recursive-edit-thing-map
  "`" 'recursive-edit)

(cl-defstruct (conn-thing-argument
               (:include conn-argument)
               ( :constructor conn-thing-argument
                 (&optional
                  recursive-edit
                  &aux
                  (required t)))
               ( :constructor conn-thing-argument-dwim
                 (&optional
                  recursive-edit
                  &aux
                  (required t)
                  (value (when (and (use-region-p)
                                    conn-thing-argument-region-dwim)
                           (list 'region nil)))
                  (set-flag (and (use-region-p)
                                 conn-thing-argument-region-dwim))))
               ( :constructor conn-thing-argument-dwim-always
                 (&optional
                  recursive-edit
                  &aux
                  (required t)
                  (value (when (use-region-p)
                           (list 'region nil)))
                  (set-flag (use-region-p))))
               ( :constructor conn-thing-argument-dwim-rectangle
                 (&optional
                  recursive-edit
                  &aux
                  (required t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (set-flag
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode))))))
  "Thing argument for thing commands."
  (recursive-edit nil))

(cl-defmethod conn-argument-compose-keymap ((arg conn-thing-argument))
  (if (conn-thing-argument-recursive-edit arg)
      (make-composed-keymap conn-recursive-edit-thing-map
                            (conn-thing-argument-keymap arg))
    (conn-thing-argument-keymap arg)))

(cl-defmethod conn-argument-update ((arg conn-thing-argument)
                                    cmd updater)
  (when (conn-argument-predicate arg cmd)
    (setf (conn-argument-set-flag arg) t
          (conn-argument-value arg)
          (list cmd (conn-read-args-consume-prefix-arg)))
    (funcall updater arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-thing-argument)
                                       (_sym (conn-thing t)))
  t)

(cl-defmethod conn-argument-predicate ((_arg conn-thing-argument)
                                       (_sym (conn-thing dispatch)))
  (not (or defining-kbd-macro executing-kbd-macro)))

(cl-defmethod conn-argument-predicate ((arg conn-thing-argument)
                                       (_sym (eql recursive-edit)))
  (conn-thing-argument-recursive-edit arg))

(cl-defmethod conn-argument-completion-annotation ((arg conn-thing-argument)
                                                   sym)
  (when (conn-argument-predicate arg sym)
    (pcase sym
      ((and (pred conn-anonymous-thing-p)
            (let thing (conn-anonymous-thing-parent sym)))
       (format " (<anonymous %s>)" thing))
      ((let (and thing (pred identity))
         (or (conn-command-thing sym)
             (and (conn-thing-p sym) sym)))
       (format " (%s)" thing))
      (_ " (<thing arg>)"))))

;;;;;; Subregions

(defvar-keymap conn-subregions-map
  "/" 'toggle-subregions)

(cl-defstruct (conn-subregions-argument
               (:include conn-argument)
               ( :constructor conn-subregions-argument
                 (&optional
                  value
                  &aux
                  (keymap conn-subregions-map)))))

(cl-defmethod conn-argument-update ((arg conn-subregions-argument)
                                    cmd updater)
  (if (eq cmd 'toggle-subregions)
      (progn
        (cl-callf not (conn-argument-value arg))
        (funcall updater arg))
    (conn-subregions-default-value cmd arg)))

(cl-defgeneric conn-subregions-default-value (cmd arg)
  ( :method (_ _) nil))

(cl-defmethod conn-subregions-default-value ((_cmd (eql conn-things-in-region))
                                             arg)
  (setf (conn-argument-value arg) t))

(cl-defmethod conn-subregions-default-value ((_cmd (conn-thing region))
                                             arg)
  (setf (conn-argument-value arg) t))

(cl-defmethod conn-subregions-default-value ((_cmd (conn-thing recursive-edit-thing))
                                             arg)
  (setf (conn-argument-value arg) t))

(cl-defmethod conn-argument-predicate ((_arg conn-subregions-argument)
                                       (_sym (eql toggle-subregions)))
  t)

(cl-defmethod conn-argument-display ((arg conn-subregions-argument))
  (concat "\\[toggle-subregions] "
          (propertize "subregions"
                      'face (when (conn-argument-value arg)
                              'conn-argument-active-face))))

(defvar conn-subregions-argument-reference
  (conn-reference-page "Subregions"
    "If this argument is non-nil then operate on the subregions defined by
the thing command. By default the subregions of a thing command are the
individual things that are moved over. For example the subregions of
`forward-word' with a prefix argument of 3 are the 3 regions containing
the 3 individual words, as opposed to the single region containing all 3
words."))

;;;;;; Fixup Whitespace Argument

(defvar-keymap conn-fixup-whitespace-argument-map
  "q" 'fixup-whitespace)

(cl-defstruct (conn-fixup-whitespace-argument
               (:include conn-argument)
               ( :constructor conn-fixup-whitespace-argument
                 (&optional
                  value
                  &aux
                  (keymap conn-fixup-whitespace-argument-map)))))

(cl-defmethod conn-argument-update ((arg conn-fixup-whitespace-argument)
                                    cmd updater)
  (when (eq cmd 'fixup-whitespace)
    (cl-callf null (conn-argument-value arg))
    (funcall updater arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-fixup-whitespace-argument)
                                       (_sym (eql fixup-whitespace)))
  t)

(cl-defmethod conn-argument-display ((arg conn-fixup-whitespace-argument))
  (substitute-command-keys
   (concat
    "\\[fixup-whitespace] "
    (if-let* ((ts (conn-argument-value arg)))
        (propertize
         "fixup"
         'face 'eldoc-highlight-function-argument)
      "fixup"))))

;;;;;; Check Bounds Argument

(defvar-keymap conn-check-bounds-argument-map
  "!" 'check-bounds)

;;;;;; Thing Transform Argument

(defvar conn-transformations-quick-ref
  (conn-reference-quote
    (("upto" conn-bounds-upto)
     ("after point/exclusive"
      conn-bounds-after-point
      conn-bounds-after-point-exclusive)
     ("trim" conn-bounds-trim)
     ("last" conn-bounds-last)
     ("before point/exclusive"
      conn-bounds-before-point
      conn-bounds-before-point-exclusive)
     ("reset" conn-transform-reset))))

(defvar-keymap conn-transform-map
  "V" 'conn-dispatch-bounds-between
  "x" 'conn-bounds-trim
  "a" 'conn-bounds-after-point
  "A" 'conn-bounds-after-point-exclusive
  "b" 'conn-bounds-before-point
  "B" 'conn-bounds-before-point-exclusive
  "SPC" 'conn-bounds-last
  "X" 'conn-transform-reset
  "t" 'conn-bounds-upto)

(cl-defstruct (conn-transform-argument
               (:include conn-argument)
               ( :constructor conn-transform-argument
                 (&optional
                  value
                  &key
                  (keymap conn-transform-map)
                  (annotation "transform")))))

(defun conn--transforms-get-references (transforms)
  (let ((doc-strings (get 'conn-transform-bounds :known-transformations)))
    (conn--with-work-buffer
      (dolist (tform transforms)
        (when-let* ((doc (alist-get tform doc-strings)))
          (let ((pt (point)))
            (insert (propertize
                     (concat (get tform :conn-transform-description) ":\n")
                     'face 'conn-quick-ref-heading-face))
            (capitalize-region pt (point)))
          (insert doc "\n")))
      (when (buffer-modified-p)
        (substring (buffer-string) 0 -1)))))

(cl-defmethod conn-argument-get-reference ((arg conn-transform-argument))
  (when-let* ((tforms (conn-argument-value arg))
              (ref (conn--transforms-get-references tforms)))
    (conn-reference-page "Active Transforms"
      :depth -50
      (:eval ref)
      (:heading "Transform Bindings")
      (:eval (conn-quick-ref-to-cols
              conn-transformations-quick-ref 3)))))

(cl-defmethod conn-argument-update ((arg conn-transform-argument)
                                    cmd updater)
  (cl-symbol-macrolet ((transforms (conn-argument-value arg)))
    (cl-labels ((update ()
                  (if (and (symbolp cmd)
                           (get cmd :conn-bounds-transformation))
                      (if (memq cmd transforms)
                          (remq cmd transforms)
                        (cons cmd transforms))
                    transforms)))
      (pcase cmd
        ('conn-transform-reset
         (setf transforms nil)
         (funcall updater arg))
        ((and (let ts (update))
              (guard (not (eq ts transforms))))
         (setf transforms ts)
         (funcall updater arg))))))

(cl-defmethod conn-argument-predicate ((_arg conn-transform-argument)
                                       sym)
  (and (symbolp sym)
       (get sym :conn-bounds-transformation)))

(cl-defmethod conn-argument-display ((arg conn-transform-argument))
  (when-let* ((ts (conn-argument-value arg)))
    (concat
     "T: "
     (propertize
      (mapconcat (lambda (tf)
                   (or (get tf :conn-transform-description) ""))
                 ts "∘")
      'face 'eldoc-highlight-function-argument))))

(cl-defmethod conn-argument-extract-value ((_arg conn-transform-argument))
  (nreverse (cl-call-next-method)))

;;;;; Read Mover State

(conn-define-state conn-read-thing-state (conn-read-thing-common-state)
  "A state for reading things."
  :lighter "THG")

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "?" 'reference
  "C-h" 'help
  "," conn-thing-remap
  "h" 'conn-things-in-region)

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
                 (if (conn-bounds-delay-p w) (funcall w bounds) w))))
           (important-return-value t))
  (let ((w (conn-bounds--whole
            (if transform
                (conn-transform-bounds bounds transform)
              bounds))))
    (if (conn-bounds-delay-p w) (funcall w bounds) w)))

(defun conn-make-bounds (thing arg whole &rest properties)
  (declare (compiler-macro
            (lambda (_exp)
              (macroexp-let2 nil thing thing
                `(conn--make-bounds
                  :thing (or (conn-get-thing ,thing)
                             (error "Not a valid thing: %s" ,thing))
                  :arg ,arg
                  :whole ,whole
                  :properties (list ,@properties))))))
  (conn--make-bounds
   :thing (or (conn-get-thing thing)
              (error "Not a valid thing: %s" thing))
   :arg arg
   :whole whole
   :properties properties))

(cl-defstruct (conn-transformed-bounds
               (:include conn-bounds)
               (:constructor conn--make-bounds-transform))
  (transforms nil :type list))

(defun conn-make-transformed-bounds (transform from to)
  (declare (compiler-macro
            (lambda (_exp)
              `(conn--make-bounds-transform
                :thing (conn-bounds-thing ,from)
                :arg (conn-bounds-arg ,from)
                :whole ,to
                :properties (conn-bounds--properties ,from)
                :transforms (append (when (conn-transformed-bounds-p ,from)
                                      (conn-transformed-bounds-transforms ,from))
                                    (list ,transform))))))
  (conn--make-bounds-transform
   :thing (conn-bounds-thing from)
   :arg (conn-bounds-arg from)
   :whole to
   :properties (conn-bounds--properties from)
   :transforms (append (when (conn-transformed-bounds-p from)
                         (conn-transformed-bounds-transforms from))
                       (list transform))))

(defun conn-bounds-set (bounds prop val)
  (setf (plist-get (conn-bounds--properties bounds) prop) val))

(defun conn-bounds-get (bounds prop &optional transform)
  (declare (gv-setter conn-bounds-set))
  (when bounds
    (let ((p (plist-get (conn-bounds--properties bounds) prop)))
      (conn-transform-bounds
       (if (conn-bounds-delay-p p) (funcall p bounds) p)
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
  "Transform BOUNDS with TRANSFORMS.

TRANSFORMS must be a list of transforms or nil.

BOUNDS may be a single `conn-bounds' struct or a list of `conn-bounds'
structs.  If BOUNDS is a list then each element is transformed using
TRANSFORM."
  (catch 'break
    (cond ((null transforms)
           bounds)
          ((consp bounds)
           (seq-reduce (lambda (bounds transform)
                         (unless bounds (throw 'break nil))
                         (delq nil (mapcar transform bounds)))
                       transforms bounds))
          (bounds
           (seq-reduce (lambda (bounds transform)
                         (unless bounds (throw 'break nil))
                         (funcall transform bounds))
                       transforms bounds)))))

(pcase-defmacro conn-transform-bounds (transform pat)
  (static-if (< emacs-major-version 30)
      `(app (pcase--flip conn-transform-bounds ,transform) ,pat)
    `(app (conn-transform-bounds _ ,transform) ,pat)))

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
  "Return the bounds of CMD as if called with prefix arg ARG.

Returns a `conn-bounds' struct."
  (declare (conn-anonymous-thing-property :bounds-op)
           (important-return-value t)))

(cl-defmethod conn-bounds-of ((_thing (conn-thing point))
                              _arg)
  (conn-make-bounds
   'point nil
   (cons (point) (point))))

(defun conn--bounds-of-thing (bounds)
  (let ((thing (conn-bounds-thing bounds))
        (arg (conn-bounds-arg bounds)))
    (pcase (conn--get-boundable-thing thing)
      ((and thing (pred conn-thing-p))
       (setf (conn-bounds bounds)
             (bounds-of-thing-at-point thing)))
      ((and thing
            (let (and handler (pred identity))
              (conn-command-mark-handler thing)))
       (deactivate-mark)
       (pcase (prefix-numeric-value arg)
         (n
          (let ((current-prefix-arg n)
                (start (point-marker)))
            (unwind-protect
                (condition-case _
                    (progn
                      (call-interactively thing)
                      (setf (conn-bounds-get bounds :direction)
                            (cond ((> (point) start) 1)
                                  ((> start (point)) -1)))
                      (let ((mk (funcall handler
                                         (conn-command-thing thing)
                                         start)))
                        (setf (conn-bounds bounds)
                              (cons (min mk (point))
                                    (max mk (point))))))
                  (error
                   (setf (conn-bounds bounds) nil
                         (conn-bounds-get bounds :subregions) nil)))
              (set-marker start nil)))))))
    (conn-bounds bounds)))

(defun conn--bounds-of-thing-subregions (bounds)
  (let ((thing (conn-bounds-thing bounds))
        (arg (conn-bounds-arg bounds)))
    (pcase (conn--get-boundable-thing thing)
      ((pred conn-thing-p) nil)
      ((and thing
            (let (and handler (pred identity))
              (conn-command-mark-handler thing)))
       (deactivate-mark)
       (pcase (prefix-numeric-value arg)
         (0
          (conn--bounds-of-thing bounds)
          (setf (conn-bounds-get bounds :subregions)
                (conn-bounds bounds)))
         (n
          (condition-case _
              (let ((start (make-marker))
                    subregions)
                (unwind-protect
                    (catch 'break
                      (dotimes (_ (abs n))
                        (set-marker start (point))
                        (let ((current-prefix-arg 1))
                          (call-interactively thing)
                          (let ((mk (funcall handler
                                             (conn-command-thing thing)
                                             start)))
                            (pcase (car subregions)
                              ((or 'nil
                                   (conn-bounds
                                    (and `(,beg . ,end)
                                         (guard (or (/= beg (min (point) mk))
                                                    (/= end (max (point) mk)))))))
                               (push (conn-make-bounds
                                      thing 1
                                      (cons (min (point) mk)
                                            (max (point) mk)))
                                     subregions))
                              (_ (throw 'break nil)))))))
                  (set-marker start nil))
                (when (conn-bounds-delay-p (conn-bounds bounds))
                  (setf (conn-bounds bounds)
                        (cl-loop for bound in subregions
                                 for (b . e) = (conn-bounds bound)
                                 minimize b into beg
                                 maximize e into end
                                 finally return (cons beg end))))
                (setf (conn-bounds-get bounds :subregions)
                      (nreverse subregions)))
            (error
             (setf (conn-bounds bounds) nil
                   (conn-bounds-get bounds :subregions) nil)))))))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing t))
                              arg)
  (let ((pt (point))
        (buf (current-buffer)))
    (when (conn--get-boundable-thing cmd)
      (conn-make-bounds
       cmd arg
       (oclosure-lambda (conn-bounds-delay)
           (bounds)
         (with-current-buffer buf
           (save-mark-and-excursion
             (goto-char pt)
             (conn--bounds-of-thing bounds))))
       :subregions (oclosure-lambda (conn-bounds-delay)
                       (bounds)
                     (with-current-buffer buf
                       (save-mark-and-excursion
                         (goto-char pt)
                         (conn--bounds-of-thing-subregions bounds))))))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing region))
                              arg)
  (conn-make-bounds
   cmd arg
   (cons (region-beginning) (region-end))
   :subregions (cl-loop for r in (region-bounds)
                        collect (conn-make-bounds cmd arg r))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing buffer))
                              arg)
  (conn-make-bounds cmd arg (cons (point-min) (point-max))))

(cl-defmethod conn-bounds-of ((cmd (conn-thing visible))
                              arg)
  (conn-make-bounds cmd arg (cons (window-start) (window-end))))

(defvar conn--bounds-last-kbd-macro nil)

(cl-defmethod conn-bounds-of ((cmd (conn-thing kbd-macro))
                              _arg)
  (let ((buf (current-buffer)))
    (save-mark-and-excursion
      (pcase cmd
        ((and (or 'start-kbd-macro
                  'kmacro-start-macro
                  'kmacro-start-macro-or-insert-counter)
              (guard (not (and (fboundp 'repeat-is-really-this-command)
                               (repeat-is-really-this-command)))))
         (let ((buffer-read-only t)
               (last-kbd-macro conn--bounds-last-kbd-macro))
           (start-kbd-macro nil)
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
      (if (eq buf (current-buffer))
          (conn-make-bounds
           cmd nil
           (cons (region-beginning) (region-end))
           :subregions (cl-loop for r in (region-bounds)
                                collect (conn-make-bounds cmd nil r)))
        (error "Buffer change during keyboard macro")))))

;;;;; Bounds Transformations

(defun conn--make-transform-bounds-docstring ()
  (let* ((main (documentation (symbol-function 'conn-transform-bounds) 'raw))
         (ud (help-split-fundoc main 'conn-transform-bounds)))
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main ""))
      (insert "\n\n\tCurrently defined transformations for bounds:\n\n")
      (pcase-dolist (`(,fn . ,tform-doc)
                     (reverse (get 'conn-transform-bounds :known-transformations)))
        (insert (format "`%s':\n %s" fn tform-doc))
        (insert "\n\n"))
      (let ((combined-doc (buffer-string)))
        (if ud
            (help-add-fundoc-usage combined-doc (car ud))
          combined-doc)))))

(put 'conn-transform-bounds 'function-documentation
     '(conn--make-transform-bounds-docstring))

(eval-and-compile
  (defun conn--set-bounds-transform-property (f _args short-name doc-string)
    `(eval-and-compile
       (setf (alist-get ',f (get 'conn-transform-bounds
                                 :known-transformations))
             ,doc-string)
       (function-put ',f :conn-bounds-transformation t)
       (function-put ',f :conn-transform-description ,short-name)))
  (setf (alist-get 'conn-bounds-transformation defun-declarations-alist)
        (list #'conn--set-bounds-transform-property)))

;;;;;; Upto Bounds

(cl-defgeneric conn-bounds-upto (bounds)
  (declare (conn-bounds-transformation
            "upto"
            "Bounds from point up to the nearest bound of the final subregion.  If
the point is within the region then the entire region is returned.")))

(cl-defmethod conn-bounds-upto (bounds)
  (pcase (or (car (last (conn-bounds-get bounds :subregions)))
             bounds)
    ((conn-bounds (and `(,beg . ,end) last))
     (conn-make-transformed-bounds
      'conn-bounds-upto
      bounds
      (cond ((< (point) beg)
             (cons (point) beg))
            ((> (point) end)
             (cons end (point)))
            (t last))))))

;;;;;; Last Bounds

(cl-defgeneric conn-bounds-last (bounds)
  (declare (conn-bounds-transformation
            "last"
            "Only return the bounds of the last thing.")))

(cl-defmethod conn-bounds-last (bounds)
  (conn-bounds bounds)
  (pcase (car (last (conn-bounds-get bounds :subregions)))
    ((conn-bounds last)
     (conn-make-transformed-bounds
      'conn-bounds-trim
      bounds last))
    (_
     (conn-make-transformed-bounds
      'conn-bounds-trim
      bounds bounds))))

;;;;;; Trim Bounds

(defvar conn-bounds-trim-chars " \t\r\n")

(cl-defgeneric conn-bounds-trim (bounds)
  (declare (conn-bounds-transformation
            "trim"
            "Trim `conn-bounds-trim-chars' from either end of bounds.")))

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
      (conn-make-transformed-bounds
       'conn-bounds-trim
       bounds (cons tb te)))))

;;;;;; Bounds Before/After

(cl-defgeneric conn-bounds-after-point (bounds &optional exclusive)
  (declare (conn-bounds-transformation
            "after"
            "Make bounds begin at point.")))

(cl-defmethod conn-bounds-after-point (bounds &optional exclusive)
  (pcase-let (((conn-bounds `(,beg . ,end)) bounds))
    (if (<= (point) end)
        (conn-make-transformed-bounds
         'conn-bounds-after-point
         bounds (cons (point) (if exclusive beg end)))
      (error "Invalid bounds"))))

(defun conn-bounds-after-point-exclusive (bounds)
  (declare (conn-bounds-transformation
            "after exclusive"
            "Make bounds begin at point and end at the start of bounds."))
  (conn-bounds-after-point bounds t))

(cl-defgeneric conn-bounds-before-point (bounds &optional exclusive)
  (declare (conn-bounds-transformation
            "before"
            "Make bounds end at point")))

(cl-defmethod conn-bounds-before-point (bounds &optional exclusive)
  (pcase-let (((conn-bounds `(,beg . ,end)) bounds))
    (if (>= (point) beg)
        (conn-make-transformed-bounds
         'conn-bounds-before-point
         bounds (cons (if exclusive end beg) (point)))
      (error "Invalid bounds"))))

(defun conn-bounds-before-point-exclusive (bounds)
  (declare (conn-bounds-transformation
            "before exclusive"
            "Make bounds end at point and begin at end of bounds."))
  (conn-bounds-before-point bounds t))

;;;;;; Check Bounds

(defvar conn-check-bounds-functions nil
  "Abnormal hook to check the bounds of a region before deleting it.

Each function in the hook is called with a single argument, a
`conn-bounds' struct, and should signal an error if the region should
not be delete.  The the value returned by each function is ignored.")

(defun conn-check-bounds (bounds)
  "Run `conn-check-bounds-functions' with BOUNDS."
  (run-hook-with-args 'conn-check-bounds-functions bounds)
  bounds)

;;;;; Bounds Of

(defvar conn--eldoc-prev-msg-fn nil)

(defun conn--bounds-of-recursive-edit-message ()
  (message
   (substitute-command-keys
    (concat
     "Recursive Edit: Press \\[exit-recursive-edit] to exit, "
     "\\[abort-recursive-edit] to abort"))))

(define-minor-mode conn-bounds-of-recursive-edit-mode
  "Mode for `conn-bounds-of' `recursive-edit'."
  :group 'conn
  :global t
  (if conn-bounds-of-recursive-edit-mode
      (progn
        (setq conn--eldoc-prev-msg-fn eldoc-message-function
              eldoc-message-function #'ignore)
        (conn--bounds-of-recursive-edit-message)
        (add-hook 'pre-command-hook #'conn--bounds-of-recursive-edit-message))
    (remove-hook 'pre-command-hook #'conn--bounds-of-recursive-edit-message)
    (setq eldoc-message-function conn--eldoc-prev-msg-fn
          conn--eldoc-prev-msg-fn nil)
    (message nil)))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state
                                   'conn-bounds-of-recursive-edit-mode)
  "e" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(cl-defmethod conn-bounds-of ((_cmd (conn-thing recursive-edit-thing))
                              _arg)
  (unwind-protect
      (progn
        (conn-bounds-of-recursive-edit-mode 1)
        (conn-with-recursive-stack 'conn-command-state
          (recursive-edit))
        (conn-bounds-of 'region nil))
    (conn-bounds-of-recursive-edit-mode -1)))

(cl-defmethod conn-bounds-of ((cmd (conn-thing emacs-state))
                              arg)
  (setq arg (prefix-numeric-value arg))
  (when (> arg 0) (cl-decf arg))
  (when (eq cmd 'conn-next-emacs-state)
    (setq arg (- arg)))
  (let* ((ring (conn-ring-list conn-emacs-state-ring))
         (mk (nth (mod arg (length ring)) ring))
         (pt (point)))
    (conn-make-bounds cmd arg (cons (min pt mk) (max pt mk)))))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-mark-last-command))
                              _arg)
  (if (region-active-p)
      (cl-call-next-method)
    (conn-bounds-of-last)))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-previous-mark-command))
                              _arg)
  (unless conn--previous-mark-state
    (user-error "No previous mark state"))
  (save-mark-and-excursion
    (goto-char (nth 0 conn--previous-mark-state))
    (push-mark (nth 1 conn--previous-mark-state) t t)
    (pcase (nth 2 conn--previous-mark-state)
      (`(,pc . ,mc)
       (rectangle-mark-mode 1)
       (rectangle--reset-crutches)
       (save-excursion
         (goto-char (mark))
         (rectangle--col-pos mc 'mark))
       (rectangle--col-pos pc 'point)))
    (cl-call-next-method)))

(cl-defmethod conn-bounds-of ((cmd (conn-thing isearch))
                              arg)
  (let* ((bounds nil)
         (quit (make-symbol "quit")))
    (fset quit (lambda ()
                 (unless isearch-suspended
                   (when (or isearch-mode-end-hook-quit
                             (null isearch-other-end))
                     (abort-recursive-edit))
                   (setq bounds (conn-make-bounds
                                 cmd arg
                                 (cons (min (point) isearch-other-end)
                                       (max (point) isearch-other-end)))))))
    (unwind-protect
        (save-mark-and-excursion
          (add-hook 'isearch-mode-end-hook quit)
          (if (conn-thing-command-p cmd)
              (progn
                (call-interactively cmd)
                (when isearch-mode
                  (let ((isearch-recursive-edit t))
                    (recursive-edit))))
            (isearch-forward)))
      (remove-hook 'isearch-mode-end-hook quit))
    bounds))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-thing-at-isearch))
                              arg)
  (conn-read-args (conn-read-thing-state
                   :prompt "Thing"
                   :prefix arg)
      ((`(,thing ,arg) (conn-thing-argument)))
    (let ((bounds nil)
          (quit (make-symbol "quit-hook")))
      (fset quit (lambda ()
                   (unless isearch-suspended
                     (when (or isearch-mode-end-hook-quit
                               (null isearch-other-end))
                       (abort-recursive-edit))
                     (setq bounds (conn-bounds-of thing arg)))))
      (unwind-protect
          (save-mark-and-excursion
            (add-hook 'isearch-mode-end-hook quit)
            (if (conn-thing-command-p cmd)
                (progn
                  (call-interactively cmd)
                  (when isearch-mode
                    (let ((isearch-recursive-edit t))
                      (recursive-edit))))
              (isearch-forward)))
        (remove-hook 'isearch-mode-end-hook quit))
      bounds)))

(conn-register-thing 'conn-thing-at-isearch)

;;;;; Bounds of Last

(defvar-local conn--bounds-of-last-cache nil)

(defun conn-bounds-of-last ()
  (if (region-active-p)
      (progn
        (setf conn--bounds-of-last-cache nil)
        (conn-bounds-of 'region nil))
    (cdr
     (if (and conn--bounds-of-last-cache
              (eq conn--last-thing-command
                  (caar conn--bounds-of-last-cache))
              (eql (buffer-chars-modified-tick)
                   (cdar conn--bounds-of-last-cache)))
         conn--bounds-of-last-cache
       (pcase conn--last-thing-command
         (`(,command . ,arg)
          (setf conn--bounds-of-last-cache
                (cons (cons conn--last-thing-command
                            (buffer-chars-modified-tick))
                      (conn-bounds-of-last-do
                       command arg
                       (marker-position conn--last-thing-command-pos))))))))))

(defun conn-last-command-thing ()
  (if conn--bounds-of-last-cache
      (cons (caaar conn--bounds-of-last-cache)
            (cdaar conn--bounds-of-last-cache))
    (cons (car conn--last-thing-command)
          (cdr conn--last-thing-command))))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-mark-last-command))
                              _arg)
  (pcase (conn-bounds-of-last)
    ((conn-bounds bds)
     (conn-make-bounds 'region nil bds))))

(cl-defgeneric conn-bounds-of-last-do (cmd arg point)
  (declare (conn-anonymous-thing-property :bounds-of-last-op)))

(cl-defmethod conn-bounds-of-last-do (cmd arg point)
  (save-excursion
    (goto-char point)
    (conn-bounds-of cmd arg)))

(cl-defmethod conn-bounds-of-last-do ((cmd (conn-thing isearch))
                                      _arg
                                      point)
  (conn-make-bounds
   cmd nil
   (cons point (point))))

(cl-defmethod conn-bounds-of-last-do ((cmd (conn-thing region))
                                      _arg
                                      _point)
  (conn-make-bounds
   cmd nil
   (cons (min (point) (mark t))
         (max (point) (mark t)))))

(cl-defmethod conn-bounds-of-last-do ((_cmd (eql kapply))
                                      _arg
                                      point)
  (conn-make-bounds
   'region nil
   (cons (point) point)))

;;;; Bounds of Things in Region

(cl-defgeneric conn-get-things-in-region (thing arg transforms beg end)
  (declare (conn-anonymous-thing-property :things-in-region)
           (important-return-value t)))

(cl-defmethod conn-get-things-in-region ((thing (conn-thing t))
                                         arg
                                         transforms
                                         beg
                                         end)
  (save-excursion
    (goto-char beg)
    (when-let* ((thing (seq-find #'conn-thing-p
                                 (conn-thing-all-parents thing)))
                (_ (ignore-errors
                     (forward-thing thing 1)
                     t)))
      (let ((subregions nil)
            (things-beg most-positive-fixnum)
            (things-end most-negative-fixnum))
        (catch 'end
          (while (< things-end end)
            (let ((sub-beg most-positive-fixnum)
                  (sub-end most-negative-fixnum)
                  (ssr nil))
              (cl-flet ((push-bound ()
                          (push (conn-transform-bounds
                                 (conn-make-bounds
                                  thing arg
                                  (cons sub-beg sub-end)
                                  :subregions ssr)
                                 transforms)
                                subregions)))
                (dotimes (_ (prefix-numeric-value arg))
                  (pcase-let (((and bounds (conn-bounds `(,b . ,e)))
                               (conn-make-bounds
                                thing arg
                                (cons (save-excursion
                                        (forward-thing thing -1)
                                        (point))
                                      (point))
                                :subregions ssr)))
                    (unless (< b end)
                      (when ssr (push-bound))
                      (throw 'end nil))
                    (cl-callf min sub-beg b)
                    (cl-callf min things-beg b)
                    (cl-callf max sub-end e)
                    (cl-callf max things-end e)
                    (push bounds ssr)
                    (unless (and (< (point) end)
                                 (ignore-errors
                                   (forward-thing thing 1)
                                   t))
                      (push-bound)
                      (throw 'end nil))))
                (push-bound)))))
        (conn-make-bounds
         (conn-anonymous-thing
           thing
           :bounds-op ( :method (_self _arg)
                        (conn-make-bounds
                         thing nil
                         (bounds-of-thing-at-point thing)
                         :direction 1)))
         nil
         (cons things-beg things-end)
         :subregions (nreverse subregions))))))

(conn-register-thing 'conn-things-in-region)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-things-in-region))
                              arg)
  (conn-read-args (conn-read-thing-state
                   :prompt "Things in Region"
                   :prefix arg)
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument)))
    (conn-get-things-in-region
     thing arg transform
     (region-beginning) (region-end))))

;;;; Multi Things

(conn-define-state conn-multi-thing-select-state (conn-mode-line-face-state)
  "State for selecting a tree sit node."
  :lighter "THING"
  :mode-line-face 'conn-read-thing-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-multi-thing-select-state)
  "z" 'conn-exchange-mark-command
  "f" 'conn-expand
  "l" 'conn-expand
  "s" 'conn-contract
  "j" 'conn-contract
  "e" 'select
  "r" 'select-other-end
  "a" 'abort
  "<escape>" 'abort)

(defface conn-multi-thing-selected-face
  '((t (:inherit cursor)))
  "Face for selected pip in multi thing select state.

Only the background color is used."
  :group 'conn-faces)

(defun conn--multi-thing-pip-strings ()
  (let* ((asciip (not (and (char-displayable-p ?⬤)
                           (char-displayable-p ?◯))))
         (selected (if asciip
                       (propertize
                        "@"
                        'face 'eldoc-highlight-function-argument)
                     (propertize
                      "⬤"
                      'face `(:foreground
                              ,(face-background
                                'conn-multi-thing-selected-face nil t)))))
         (unselected (if asciip "." "◯")))
    (cons selected unselected)))

(defun conn-multi-thing-select (things &optional always-prompt)
  (let* ((bounds (compat-call sort things
                              :key #'conn-bounds--whole
                              :lessp (lambda (a b)
                                       (if (= (car a) (car b))
                                           (< (cdr a) (cdr b))
                                         (> (car a) (car b))))))
         (curr 0)
         (size (length bounds))
         (pips (conn--multi-thing-pip-strings))
         (display-handler
          (lambda (prompt _args)
            (message
             (substitute-command-keys
              (concat
               (propertize prompt 'face 'minibuffer-prompt)
               (if (> size 4)
                   (propertize (format " [%s/%s]" curr size)
                               'face 'minibuffer-prompt)
                 (cl-loop for i below size
                          concat " "
                          if (= i curr) concat (car pips)
                          else concat (cdr pips)))
               " ("
               (let* ((desc (conn-thing-pretty-print
                             (conn-bounds-thing (nth curr bounds)))))
                 (propertize (if (length> desc 40)
                                 (truncate-string-to-width desc 40 nil nil t)
                               desc)
                             'face 'eldoc-highlight-function-argument))
               ")"
               (when-let* ((msg (conn--read-args-display-message)))
                 (concat ": " msg))
               "\n\\[select] select; "
               "\\[select-other-end] select other end; "
               "\\[abort] abort"
               (when (> size 1)
                 (concat
                  "; \\[conn-expand] next; "
                  "\\[conn-contract] prev")))))))
         (command-handler
          (lambda (command)
            (pcase command
              ('recenter-top-bottom
               (let ((this-command 'recenter-top-bottom)
                     (last-command conn-read-args-last-command))
                 (recenter-top-bottom (conn-read-args-prefix-arg)))
               (conn-read-args-handle))
              ('conn-exchange-mark-command
               (conn-exchange-mark-command)
               (conn-read-args-handle))
              ('conn-contract
               (setq curr (mod (1- curr) size))
               (pcase (nth curr bounds)
                 ((conn-bounds `(,beg . ,end))
                  (goto-char (if (< (point) (mark)) beg end))
                  (push-mark (if (< (point) (mark)) end beg) t)))
               (conn-read-args-handle))
              ('conn-expand
               (setq curr (mod (1+ curr) size))
               (pcase (nth curr bounds)
                 ((conn-bounds `(,beg . ,end))
                  (goto-char (if (< (point) (mark)) beg end))
                  (push-mark (if (< (point) (mark)) end beg))))
               (conn-read-args-handle))
              ('abort
               (user-error "Aborted"))))))
    (pcase bounds
      ('nil (user-error "No things found at point"))
      ((and `(,bound . nil)
            (guard (not always-prompt)))
       bound)
      (`(,(conn-bounds `(,beg . ,end)) . ,_)
       (save-mark-and-excursion
         (goto-char end)
         (push-mark beg t t)
         (activate-mark)
         (conn-read-args (conn-multi-thing-select-state
                          :prompt "Thing"
                          :display-handler display-handler
                          :command-handler command-handler)
             ((bound
               (oclosure-lambda (conn-anonymous-argument
                                 (required t))
                   (_self command updater)
                 (let ((bounds (nth curr bounds)))
                   (pcase command
                     ('select
                      (setf (conn-bounds-get bounds :direction) -1)
                      (funcall updater (conn-argument bounds)))
                     ('select-other-end
                      (setf (conn-bounds-get bounds :direction) 1)
                      (funcall updater (conn-argument bounds))))))))
           bound))))))

;;;; Thing Definitions

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)
(conn-define-mark-command conn-mark-comment comment)

(conn-register-thing 'kbd-macro)

(conn-register-thing 'point)

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
                          (while (conn--point-in-comment-p)
                            (forward-char -1)
                            (skip-chars-backward " \t\n\r"))
                          (skip-chars-forward " \t\n\r")
                          (unless (conn--point-in-comment-p)
                            (forward-char 1))
                          (point))
                        (save-excursion
                          (while (and (conn--point-in-comment-p)
                                      (not (eobp)))
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r"))
                          (skip-chars-backward " \t\n\r")
                          (point)))
                (error "Point not in comment"))))

(conn-register-thing
 'defun
 :forward-op 'conn-forward-defun
 :properties '(:linewise t))

(conn-register-thing
 'visual-line
 :forward-op 'conn-forward-visual-line
 :properties '(:linewise t))

(conn-register-thing-commands
 'visual-line 'conn-continuous-thing-handler
 'conn-forward-visual-line
 'conn-backward-visual-line)

(conn-define-mark-command conn-mark-visual-line visual-line)

(conn-register-thing
 'region
 :bounds-op (lambda () (cons (region-beginning) (region-end))))

(conn-register-thing-commands
 'region nil
 'conn-mark-last-command
 'conn-exchange-mark-command
 'conn-mark-thing
 'conn-previous-mark-command
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
 'conn-isearch-region-forward
 'conn-isearch-region-backward
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

(defun conn-char-mark-handler (_thing beg)
  (when current-prefix-arg beg))

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

(defun conn--up-list-mark-handler (thing beg)
  (condition-case _err
      (cond ((> (point) beg)
             (save-excursion
               (forward-thing thing -1)
               (point)))
            ((< (point) beg)
             (save-excursion
               (forward-thing thing 1)
               (point))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--up-list-mark-handler
 'up-list 'backward-up-list)

(defun conn--down-list-mark-handler (_thing _beg)
  (condition-case _err
      (cond ((= (point) (save-excursion
                          (up-list 1 t t)
                          (down-list -1 t)
                          (point)))
             (save-excursion
               (up-list -1 t t)
               (down-list 1 t)
               (point)))
            ((= (point) (save-excursion
                          (up-list -1 t t)
                          (down-list 1 t)
                          (point)))
             (save-excursion
               (up-list 1 t t)
               (down-list -1 t)
               (point))))
    (scan-error nil)))

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'down-list
 'conn-backward-up-inner-list
 'conn-forward-up-inner-list)

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

(conn-register-thing 'expansion)

(conn-register-thing-commands
 'expansion nil
 'conn-expand 'conn-contract)

(conn-register-thing-commands
 'list 'conn--down-list-mark-handler
 'conn-beginning-of-list
 'conn-end-of-list)

(provide 'conn-things)
