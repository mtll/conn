;;; conn-states.el --- States -*- lexical-binding: t -*-
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

;;; Commentary

;; Implementation of conn states.

;;; Code

(require 'conn-vars)
(require 'conn-utils)
(require 'conn-mark)
(require 'conn-quick-ref)
(eval-when-compile
  (require 'cl-lib))

(defvar conn-local-mode)

(declare-function face-remap-remove-relative "face-remap")
(declare-function conn-local-mode "conn")

;;;; States

(defcustom conn-null-state-modes
  (list 'calc-mode
        'calc-trail-mode
        'calc-keypad-mode
        'image-mode
        'doc-view-mode
        'pdf-view-mode)
  "Major modes in which `conn-null-state' should be active."
  :group 'conn
  :type '(list symbol))

(defcustom conn-command-state-modes
  (list 'prog-mode
        'text-mode
        'conf-mode
        'fundamental-mode)
  "Major modes in which `conn-command-state' should be the base state."
  :group 'conn
  :type '(list symbol))

(defvar conn-setup-state-hook nil
  "Hook responsible for setting up the base state in a new buffer.

The hook is run when `conn-local-mode' is turned on. Each function is
called in turn until one returns non-nil.  A function returning non-nil
should set the base state for the current buffer by pushing it to the
stack with `conn-push-state'.  The function may setup any other
necessary state as well.")

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar-local conn-next-state nil)
(defvar-local conn-previous-state nil)

(defvar-local conn--state-stack nil
  "Previous conn states in buffer.")

(defvar conn--minor-mode-maps-sort-tick 0)

(cl-defstruct (conn-state
               (:constructor nil)
               ( :constructor conn--make-state
                 ( name docstring parents
                   &aux
                   (properties (make-hash-table :test 'eq))
                   (minor-mode-depths (make-hash-table :test 'eq))
                   (minor-mode-sort-tick conn--minor-mode-maps-sort-tick)
                   (minor-mode-maps (list :conn-minor-mode-map-alist))))
               (:conc-name conn-state--)
               (:copier nil))
  (name nil :type symbol :read-only t)
  (docstring nil :type string :read-only t)
  (parents nil :type (list-of symbol))
  (children nil :type (list-of symbol))
  (properties nil :type hash-table :read-only t)
  (keymap nil :type (or nil keymap))
  (minor-mode-depths nil :type hash-table :read-only t)
  (minor-mode-sort-tick nil :type (or nil integer))
  (minor-mode-maps nil :type alist :read-only t))

(defmacro conn--find-state (state)
  `(get ,state :conn--state))

(define-inline conn-state-minor-mode-maps-alist (state)
  "Return the minor mode maps alist for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (conn-state--minor-mode-maps
    (conn--find-state ,state))))

(define-inline conn-state-name-p (state)
  "Return non-nil if STATE is a conn-state."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (eq 'conn-state (type-of (conn--find-state ,state)))))

(define-inline conn-state-parents (state)
  "Return only the immediate parents for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (conn-state--parents (conn--find-state ,state))))

(defconst conn--state-all-parents-cache (make-hash-table :test 'eq))

(defun conn-state-all-parents (state)
  "Return all parents for STATE."
  (declare (important-return-value t))
  (with-memoization (gethash state conn--state-all-parents-cache)
    (cons state (merge-ordered-lists
                 (mapcar 'conn-state-all-parents
                         (conn-state--parents (conn--find-state state)))))))

(defun conn-state-all-keymap-parents (state)
  "Return all parents for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (cons state
        (merge-ordered-lists
         (cl-loop with no-inherit = (conn-state-get state :no-inherit-keymaps)
                  for parent in (conn-state--parents (conn--find-state state))
                  unless (memq parent no-inherit)
                  collect (conn-state-all-keymap-parents parent)))))

(defun conn-state-all-children (state)
  "Return all children for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (cl-labels ((all-children (state)
                (let ((children (conn-state--children
                                 (conn--find-state state))))
                  (append children (mapcan #'all-children children)))))
    (delete-dups (all-children state))))

(define-inline conn-substate-p (state parent)
  "Return non-nil if STATE is a substate of PARENT."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (memq ,parent (conn-state-all-parents ,state))))

;;;;; Properties

(eval-and-compile
  (defun conn-declare-property-static (property)
    "Declare a state PROPERTY static.

Static state properties can only be changed by redefining a state and
are not inherited."
    (setf (get property :conn-static-property) t))

  (define-inline conn-property-static-p (property)
    "Return non-nil if PROPERTY is static."
    (declare (side-effect-free t)
             (important-return-value t))
    (inline-quote
     (and (get ,property :conn-static-property) t)))

  (conn-declare-property-static :no-keymap)
  (conn-declare-property-static :no-inherit-keymaps)
  (conn-declare-property-static :abstract)

  (defun conn-state-get--cmacro ( exp state property
                                  &optional
                                  no-inherit default)
    (let ((no-inherit (macroexpand-all no-inherit macroexpand-all-environment))
          (prop (macroexpand-all property macroexpand-all-environment)))
      (if (or (and (macroexp-const-p no-inherit)
                   (if (consp no-inherit) (cadr no-inherit) no-inherit))
              (and (symbolp prop)
                   (conn-property-static-p prop)))
          `(gethash ,property
                    (conn-state--properties (conn--find-state ,state))
                    ,default)
        exp))))

(defun conn-state-get (state property &optional no-inherit default)
  "Return the value of PROPERTY for STATE.

If PROPERTY is not set for STATE then check all of STATE's parents for
PROPERTY.  If no parent has that property either than nil is returned."
  (declare (compiler-macro conn-state-get--cmacro)
           (side-effect-free t)
           (important-return-value t)
           (gv-setter conn-state-set))
  (if (or no-inherit (conn-property-static-p property))
      (gethash property
               (conn-state--properties (conn--find-state state))
               default)
    (cl-loop for parent in (conn-state-all-parents state)
             for table = (conn-state--properties (conn--find-state parent))
             for prop = (gethash property table conn--key-missing)
             unless (eq prop conn--key-missing) return prop
             finally return default)))

(define-inline conn-state-set (state property value)
  "Set the value of PROPERTY in STATE to VALUE.

Returns VALUE."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (thread-last
         (conn--find-state ,state)
         conn-state--properties (puthash ,property ,value))))))

(define-inline conn-state-unset (state property)
  "Make PROPERTY unset in STATE.

If a property is unset in a state it will inherit the value of that
property from its parents."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (remhash (conn-state--properties (conn--find-state ,state))
                ,property)))))

(define-inline conn-state-has-property-p (state property)
  "Return t if PROPERTY is set for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (property)
    (inline-quote
     (thread-first
       (gethash ,property
                (conn-state--properties (conn--find-state ,state))
                conn--key-missing)
       (eq conn--key-missing) not))))

;;;;; Keymaps

(defvar-local conn--state-map nil)
(defvar-local conn--minor-mode-maps nil)

(defconst conn--composed-state-maps (make-hash-table :test 'eq))

(defun conn--rebuild-state-keymaps (state)
  "Rebuild all composed keymaps for STATE.

Called when the inheritance hierarchy for STATE changes."
  (unless (conn-state-get state :no-keymap)
    (let ((parents (conn-state-all-keymap-parents state))
          (state-obj (conn--find-state state)))
      (when-let* ((state-map (gethash state conn--composed-state-maps)))
        (setf (cdr state-map)
              (cl-loop for pstate in parents
                       for pmap = (conn-get-state-map pstate t)
                       when pmap collect pmap)))
      (let (to-remove)
        (pcase-dolist ((and `(,mode . ,map) cons)
                       (cdr (conn-state--minor-mode-maps state-obj)))
          (setf (cdr map)
                (cl-loop for parent in parents
                         for pmap = (conn-get-minor-mode-map parent mode t)
                         when pmap collect pmap))
          (unless (cdr map)
            (push cons to-remove)))
        (cl-callf seq-difference (cdr (conn-state--minor-mode-maps state-obj))
          to-remove #'eq)))))

;;;;;; State Maps

(defun conn--compose-state-map ()
  "Return composed state map for STATE.

The composed keymap is of the form:

(keymap
 (keymap . bindings)  ;; state map for STATE
 (keymap . bindings)  ;; state map for STATE parent
 ...)"
  (declare (important-return-value t))
  (with-memoization (gethash conn-current-state conn--composed-state-maps)
    (cl-assert (not (conn-state-get conn-current-state :no-keymap))
               nil "%s :no-keymap property is non-nil" conn-current-state)
    (make-composed-keymap
     (cl-loop for pstate in (conn-state-all-keymap-parents conn-current-state)
              for pmap = (conn-state--keymap (conn--find-state pstate))
              when pmap collect pmap))))

(defun conn-set-state-map (state map)
  (cl-assert (keymapp map))
  (setf (conn-state--keymap (conn--find-state state)) map)
  (dolist (child (cons state (conn-state-all-children state)) map)
    (when-let* ((map (gethash child conn--composed-state-maps)))
      (cl-loop for parent in (conn-state-all-keymap-parents child)
               for pmap = (conn-get-state-map parent t)
               when pmap collect pmap into pmaps
               finally (setf (cdr map) pmaps)))))

(gv-define-simple-setter conn-get-state-map conn-set-state-map)

(defun conn-get-state-map (state &optional dont-create)
  "Return the state keymap for STATE."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (conn-state--keymap (conn--find-state state))
        (unless dont-create
          (setf (conn-get-state-map state) (make-sparse-keymap))))))

;;;;;; Minor Mode Maps

(define-inline conn--mode-maps-sorted-p (state)
  (declare (side-effect-free t)
           (important-return-value t)
           (gv-setter
            (lambda (value)
              (cl-labels ((set (v)
                            `(setf (conn-state--minor-mode-sort-tick
                                    (conn--find-state ,state))
                                   ,v)))
                (pcase value
                  ('nil (set nil))
                  ((pred macroexp-const-p)
                   (set 'conn--minor-mode-maps-sort-tick))
                  (_ (macroexp-let2 nil value value
                       `(progn
                          ,(set `(when ,value conn--minor-mode-maps-sort-tick))
                          ,value))))))))
  (inline-quote
   (eql conn--minor-mode-maps-sort-tick
        (conn-state--minor-mode-sort-tick
         (conn--find-state ,state)))))

(defun conn--sort-mode-maps (state)
  (cl-check-type (conn--find-state state) conn-state)
  (unless (conn-state-get state :no-keymap)
    (let* ((parents (conn-state-all-keymap-parents state))
           (tables (mapcar (lambda (s)
                             (conn-state--minor-mode-depths
                              (conn--find-state s)))
                           parents)))
      (conn--compat-callf sort (cdr (conn-state-minor-mode-maps-alist state))
        :key (lambda (cons)
               (or (catch 'break
                     (dolist (table tables)
                       (when-let* ((v (gethash (car cons) table)))
                         (throw 'break v))))
                   (get (car cons) :conn-mode-depth)
                   0))
        :in-place t)
      (setf (conn--mode-maps-sorted-p state) t))))

(defun conn-set-mode-map-depth (mode depth &optional state)
  (cl-check-type mode symbol)
  (cl-assert (<= -100 depth 100) nil "Depth must be between -100 and 100")
  (if (null state)
      (progn
        (setf (get mode :conn-mode-depth) depth)
        (cl-incf conn--minor-mode-maps-sort-tick))
    (let* ((state-obj (conn--find-state state))
           (mmode-maps (cdr (conn-state--minor-mode-maps state-obj)))
           (table (conn-state--minor-mode-depths state-obj)))
      (cl-assert (not (conn-state-get state :no-keymap))
                 nil "%s :no-keymap property is non-nil" state)
      (unless (eql depth (gethash mode table))
        (setf (gethash mode table) depth)
        (dolist (child (conn-state-all-children state))
          (setf (conn--mode-maps-sorted-p child) nil))
        (when (alist-get mode mmode-maps)
          (conn--sort-mode-maps state)))))
  nil)

;; setup special mode maps
(conn-set-mode-map-depth :override -80)
(conn-set-mode-map-depth :bind-last -90)

(defconst conn--minor-mode-maps-cache (make-hash-table :test 'equal))

(defun conn-set-minor-mode-map (state mode map)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--minor-mode-maps-cache))
                (get-composed-map (state)
                  `(alist-get mode (cdr (conn-state-minor-mode-maps-alist ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (cl-assert (keymapp map))
    (cl-check-type (conn--find-state state) conn-state)
    (cl-check-type mode symbol)
    (setf (get-map state) map)
    (setf (get-composed-map state)
          (make-composed-keymap (parent-maps state)))
    (conn--sort-mode-maps state)
    (dolist (child (conn-state-all-children state) map)
      (unless (conn-state-get child :no-keymap)
        (if-let* ((map (get-composed-map child)))
            (setf (cdr map) (parent-maps child))
          (conn--ensure-minor-mode-map child mode))))))

(gv-define-simple-setter conn-get-minor-mode-map conn-set-minor-mode-map)

(defun conn-get-minor-mode-map (state mode &optional dont-create)
  "Return keymap for MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (gethash (cons state mode) conn--minor-mode-maps-cache)
        (unless dont-create
          (setf (conn-get-minor-mode-map state mode)
                (make-sparse-keymap))))))

(defun conn--ensure-minor-mode-map (state mode)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--minor-mode-maps-cache))
                (get-composed-map (state)
                  `(alist-get mode (cdr (conn-state-minor-mode-maps-alist ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (unless (conn-state-get state :no-keymap)
      (setf (get-composed-map state)
            (make-composed-keymap (parent-maps state)))
      (unless (get-map state)
        (setf (conn--mode-maps-sorted-p state) nil)))))

;;;;; Input Methods

(defvar-local conn--input-method nil
  "Input method for current buffer.")
(put 'conn--input-method 'permanent-local t)

(defvar-local conn--input-method-title nil
  "Title string of the current input method shown in mode line.")
(put 'conn--input-method-title 'permanent-local t)

(defvar-local conn--prev-mode-line-mule-info nil)
(put 'conn--prev-mode-line-mule-info 'risky-local-variable t)

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property."
  (when conn-local-mode
    (let (input-method-activate-hook
          input-method-deactivate-hook)
      (pcase (conn-state-get conn-current-state :suppress-input-method)
        ((and 'nil (guard current-input-method))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title))
        ((and 'nil (guard conn--input-method))
         (activate-input-method conn--input-method))
        ((guard (and current-input-method
                     conn--input-method
                     deactivate-current-input-method-function))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title)
         (deactivate-input-method))
        ((guard (and current-input-method
                     deactivate-current-input-method-function))
         (setq conn--input-method current-input-method
               conn--input-method-title current-input-method-title)
         (deactivate-input-method))))))
(put 'conn--activate-input-method 'permanent-local-hook t)

(defun conn--deactivate-input-method ()
  (setq conn--input-method nil
        conn--input-method-title nil))
(put 'conn--deactivate-input-method 'permanent-local-hook t)

(defun conn--isearch-input-method ()
  "Ensure input method is enabled in isearch-mode."
  (when (and conn--input-method isearch-mode)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (let ((overriding-terminal-local-map nil))
      (activate-input-method conn--input-method))
    (setq isearch-input-method-function input-method-function)
    (setq-local input-method-function nil)
    (isearch-update)
    (add-hook 'isearch-mode-end-hook
              (conn-anaphoricate hook
                (lambda ()
                  (conn--activate-input-method)
                  (add-hook 'input-method-activate-hook
                            #'conn--activate-input-method
                            nil t)
                  (add-hook 'input-method-deactivate-hook
                            #'conn--deactivate-input-method
                            nil t)
                  (remove-hook 'isearch-mode-end-hook hook))))))
(put 'conn--isearch-input-method 'permanent-local-hook t)

(defun conn--input-method-mode-line ()
  "Display the mode line information for conn--input-method.

This ensures that the mode line information for the current input method
is shown even if the input method is deactivated because a state is
suppressing it."
  (cond
   (conn-local-mode
    (setq conn--prev-mode-line-mule-info mode-line-mule-info
          mode-line-mule-info
          `(""
            (conn--input-method
             ( :propertize ("" conn--input-method-title)
               help-echo (concat
                          "Current input method: "
                          conn--input-method
                          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
               local-map ,mode-line-input-method-map
               mouse-face mode-line-highlight))
            ,( propertize "%z"
               'help-echo 'mode-line-mule-info-help-echo
               'mouse-face 'mode-line-highlight
               'local-map mode-line-coding-system-map)
            (:eval (mode-line-eol-desc)))))
   (conn--prev-mode-line-mule-info
    (setq mode-line-mule-info conn--prev-mode-line-mule-info))))

(defmacro conn-with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (input-method-p)
    `(let ((,input-method-p conn--input-method))
       (unwind-protect
           (progn
             (when ,input-method-p
               (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
               (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
               (activate-input-method conn--input-method))
             ,@body)
         (when ,input-method-p
           (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
           (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
           (conn--activate-input-method))))))

;;;;; Macros

;; Adapted from map pattern, we can't just use the map pattern on the
;; property table because of inheritance.
(pcase-defmacro conn-state-props (&rest properties)
  "Build a `pcase' pattern matching state properties.

PROPERTIES is a list of elements to be matched in the state.

Each element of PROPERTIES is of the form (KEY PAT), which looks up the
property KEY in the state and matches to associated value against
`pcase' pattern PAT.

Each element can also be a SYMBOL, which is an abbreviation of a (KEY
PAT) tuple of the form (\\='SYMBOL SYMBOL).  When SYMBOL is a keyword,
it is an abbreviation of the form (:SYMBOL SYMBOL)."
  `(and (pred conn-state-name-p)
        ,@(mapcar
           (static-if (< emacs-major-version 30)
               (lambda (prop)
                 (cond ((consp elt)
                        (cl-with-gensyms (state)
                          `(app (lambda (,state)
                                  (conn-state-get ,state ,(car prop) ,(caddr prop)))
                                ,(cadr prop))))
                       ((keywordp prop)
                        (let ((var (intern (substring (symbol-name prop) 1))))
                          `(app (pcase--flip conn-state-get ,prop) ,var)))
                       (t `(app (pcase--flip conn-state-get ',prop) ,prop))))
             (lambda (prop)
               (cond ((consp prop)
                      `(app (conn-state-get _ ,(car prop) ,(caddr prop)) ,(cadr prop)))
                     ((keywordp prop)
                      (let ((var (intern (substring (symbol-name prop) 1))))
                        `(app (conn-state-get _ ,prop) ,var)))
                     (t `(app (conn-state-get _ ',prop) ,prop)))))
           properties)))

(pcase-defmacro conn-substate (parent)
  "Matches if EXPVAL is a substate of PARENT."
  `(and (pred conn-state-name-p)
        (pred ,(static-if (< emacs-major-version 30)
                   `(pcase--flip conn-substate-p ',parent)
                 `(conn-substate-p _ ',parent)))))

(defmacro conn-with-recursive-stack (state &rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms (buffer stack)
    `(progn
       (conn-enter-recursive-stack ,state)
       (let ((,stack (cdr (memq nil conn--state-stack)))
             (,buffer (current-buffer)))
         (unwind-protect
             ,(macroexp-progn body)
           (with-current-buffer ,buffer
             (setq conn--state-stack ,stack)
             (conn-enter-state (car ,stack))))))))

(defmacro conn-without-recursive-stack (&rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (stack buffer)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (conn-exit-recursive-stack)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer ,buffer
           (setq conn--state-stack ,stack)
           (conn-enter-state (car ,stack)))))))

;;;;; Cl-Generic Specializers

(cl-generic-define-generalizer conn--substate-generalizer
  90 (lambda (state &rest _)
       `(and (conn-state-name-p ,state)
             (conn-state-all-parents ,state)))
  (lambda (tag &rest _)
    (when tag
      `(,@(mapcar (lambda (state) `(conn-substate ,state)) tag)
        (conn-substate t)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-substate)))
  "Support for (conn-substate STATE) specializers.
These match if the argument is a substate of STATE."
  (list conn--substate-generalizer))

;;;;; Enter/Exit Functions

(defvar conn-state-entry-functions nil
  "Hook run when a state is entered.

When this hook is run `conn-previous-state' will be bound to the state
that has just been exited.")

(defvar-local conn--state-defered (list 'conn--state-defered-default)
  "Code to be run when the current state is exited.")

(defvar-local conn--state-defered-ids nil)

(defun conn--state-defered-default (_)
  (when conn-current-state
    (set (cl-shiftf conn-current-state nil) nil)))

(defun conn--run-defered ()
  (unwind-protect
      (funcall (car conn--state-defered) (cdr conn--state-defered))
    (setq conn--state-defered (list 'conn--state-defered-default)
          conn--state-defered-ids nil)))

(defmacro conn-state-defer (&rest body)
  "Defer evaluation of BODY until the current state is exited.

Note that if a `conn-state-defer' form is evaluated multiple times in
one state then BODY will evaluated that many times when the state is
exited.  If you want to ensure that BODY will be evaluated only once
when the current state exits then use `conn-state-defer-once'.

When BODY is evaluated `conn-next-state' will be bound to the state
that is being entered after the current state has exited."
  (declare (indent 0))
  (cl-with-gensyms (rest)
    `(push (lambda (,rest)
             (unwind-protect
                 ,(macroexp-progn body)
               (funcall (car ,rest) (cdr ,rest))))
           conn--state-defered)))

(defmacro conn-state-defer-once (&rest body)
  "Like `conn-state-defer' but BODY will be evaluated only once per state.

For more information see `conn-state-defer'."
  (declare (indent 0))
  (cl-with-gensyms (rest id)
    `(unless (memq ',id conn--state-defered-ids)
       (push ',id conn--state-defered-ids)
       (push (lambda (,rest)
               (unwind-protect
                   ,(macroexp-progn body)
                 (funcall (car ,rest) (cdr ,rest))))
             conn--state-defered))))

(defvar conn-state-lighter-separator "→"
  "Separator string for state lighters in `conn-lighter'.")

(defconst conn--lighter-cache
  (make-hash-table :test 'equal :weakness 'value))

(defun conn--get-lighter ()
  (with-memoization (buffer-local-value 'conn-lighter (current-buffer))
    (with-memoization (gethash conn--state-stack conn--lighter-cache)
      (let ((lighter (conn-state-get conn-current-state :lighter)))
        (dolist (elem (cdr conn--state-stack))
          (setq lighter
                (if elem
                    (concat (conn-state-get elem :lighter)
                            conn-state-lighter-separator
                            lighter)
                  (concat "[" lighter "]"))))
        (concat " [" lighter "]")))))

(defun conn-update-lighter (&optional buffer)
  "Force the mode-line lighter to be updated in BUFFER.

If BUFFER is nil then use the current buffer."
  (setf (buffer-local-value 'conn-lighter (or buffer (current-buffer))) nil)
  (force-mode-line-update))

(defun conn--setup-state-keymaps ()
  (if (conn-state-get conn-current-state :no-keymap)
      (setf conn--state-map nil
            conn--minor-mode-maps nil)
    (setf conn--state-map `((conn-local-mode . ,(conn--compose-state-map)))
          conn--minor-mode-maps (conn-state-minor-mode-maps-alist conn-current-state))))

(defun conn--setup-state-properties ()
  (setf conn--disable-mark-cursor (or (when-let* ((hide (conn-get-buffer-property
                                                         :disable-mark-cursor)))
                                        (if (eq hide t) t
                                          (alist-get conn-current-state hide)))
                                      (when-let* ((hide (conn-get-mode-property
                                                         major-mode :disable-mark-cursor)))
                                        (if (eq hide t) t
                                          (alist-get conn-current-state hide)))
                                      (conn-state-get conn-current-state :disable-mark-cursor))
        cursor-type (let ((c (conn-state-get conn-current-state :cursor nil t)))
                      (if (functionp c) (funcall c) c))))

(cl-defgeneric conn-enter-state (state)
  "Enter STATE.

Code that should be run when a state is entered should be added as
methods to this function.  The (conn-substate STATE) method specializer is
provided so that code can be run for every state inheriting from some
state.

To execute code when a state is exiting use `conn-state-defer'."
  (:method ((_state (eql 'nil))) "Noop" nil)
  (:method ((_state (conn-substate t))) "Noop" nil)
  (:method (state) (error "Attempting to enter unknown state: %s" state)))

(cl-defmethod conn-enter-state :around ((state (conn-substate t)))
  (unless (symbol-value state)
    (when (conn-state-get state :abstract)
      (error "Attempting to enter abstract state %s" state))
    (let (conn-previous-state)
      (unwind-protect
          (progn
            (let ((conn-next-state state))
              (conn--run-defered))
            (cl-shiftf conn-previous-state conn-current-state state)
            (conn--setup-state-properties)
            (conn--setup-state-keymaps)
            (conn--activate-input-method)
            (unless (conn--mode-maps-sorted-p state)
              (conn--sort-mode-maps state))
            (cl-call-next-method)
            (conn-update-lighter)
            (set state t))
        (unless (symbol-value state)
          (conn-local-mode -1)
          (message "Error entering state %s." state)))
      (run-hook-wrapped
       'conn-state-entry-functions
       (lambda (fn)
         (condition-case err
             (funcall fn)
           (error
            (remove-hook 'conn-state-entry-functions fn)
            (message "Error in conn-state-entry-functions: %s" (car err)))))))))

(defun conn-push-state (state)
  "Push STATE to the state stack."
  (unless (symbol-value state)
    (conn-enter-state state)
    (push state conn--state-stack)))

(defun conn-pop-state ()
  "Pop to the previous state in the state stack."
  (interactive)
  (if-let* ((state (cadr conn--state-stack)))
      (progn
        (pop conn--state-stack)
        (conn-enter-state state))
    (conn-push-state
     (conn-state-get conn-current-state :pop-alternate
                     t 'conn-command-state))))

(defun conn-peek-state ()
  "Peek the state stack.

Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (cadr conn--state-stack))

(defun conn-enter-recursive-stack (state)
  "Enter a recursive state stack."
  (conn-enter-state state)
  ;; Ensure the lighter gets updates even if we haven't changed state
  (conn-update-lighter)
  (push nil conn--state-stack)
  (push state conn--state-stack))

(defun conn-exit-recursive-stack ()
  "Exit the current recursive state stack.

If there is not recursive stack an error is signaled."
  (interactive)
  (if-let* ((tail (memq nil conn--state-stack)))
      (progn
        (conn-enter-state (cadr tail))
        (setq conn--state-stack (cdr tail))
        ;; Ensure the lighter gets updates
        ;; even if we haven't changed state
        (conn-update-lighter))
    (error "Not in a recursive state")))

;;;;; Definitions

(defun conn--define-state (name docstring parents properties no-inherit-keymaps)
  (cl-labels ((setup-properties (table)
                (cl-loop with kvs = properties
                         for (k v) on kvs by #'cddr
                         do (puthash k v table))
                (cl-callf seq-union (gethash :no-inherit-keymaps table)
                  no-inherit-keymaps)))
    (if-let* ((state-obj (conn--find-state name)))
        (let ((prev-parents (conn-state--parents state-obj)))
          (remhash name conn--state-all-parents-cache)
          (clrhash (conn-state--properties state-obj))
          (setup-properties (conn-state--properties state-obj))
          (setf (conn-state--parents state-obj) parents)
          (dolist (former (seq-difference prev-parents parents))
            (cl-callf2 delq name (conn-state--children
                                  (conn--find-state former))))
          (let (new-mode-maps)
            (dolist (parent (seq-difference parents prev-parents))
              (pcase-dolist (`(,mode . ,_)
                             (cdr (conn-state-minor-mode-maps-alist parent)))
                (unless (gethash (cons name mode) conn--minor-mode-maps-cache)
                  (conn--ensure-minor-mode-map name mode)
                  (push mode new-mode-maps))))
            (conn--rebuild-state-keymaps name)
            (dolist (child (conn-state-all-children name))
              (remhash child conn--state-all-parents-cache)
              (dolist (mode new-mode-maps)
                (conn--ensure-minor-mode-map child mode))
              (conn--rebuild-state-keymaps child))))
      (let* ((state-obj (conn--make-state name docstring parents)))
        (setf (conn--find-state name) state-obj)
        (setup-properties (conn-state--properties state-obj))
        (dolist (parent parents)
          (cl-pushnew name (conn-state--children
                            (conn--find-state parent))))
        (unless (conn-state-get name :no-keymap)
          (dolist (parent parents)
            (pcase-dolist (`(,mode . ,_)
                           (cdr (conn-state-minor-mode-maps-alist parent)))
              (conn--ensure-minor-mode-map name mode))))))))

(defmacro conn-define-state (name parents &rest properties)
  "Define a conn state NAME.

Defines a transition function and variable NAME.  NAME is non-nil when
the state is active.

PARENTS is a list of states from which NAME should inherit properties
and keymaps.

PROPERTIES is a property list defining the state properties for NAME.  A
state may have any number of properties and a state will inherit the
value for a property from its parents if it does not set the value
explicitly.

The following properties have a special meaning:

:LIGHTER is the mode-line lighter text for NAME.

:DISABLE-MARK-CURSOR if non-nil will hide the mark cursor in NAME.

:SUPPRESS-INPUT-METHOD if non-nil suppresses current input method in
NAME.

:CURSOR is the `cursor-type' in NAME.

:ABSTRACT if non-nil indicates that NAME is only for other states to
inherit from and should never be entered directly.

\(fn NAME PARENTS &optional DOCSTRING [KEY VALUE ...])"
  (declare (debug ( name form string-or-null-p
                    [&rest keywordp sexp]))
           (indent 2))
  (let ((docstring (or (and (stringp (car properties))
                            (pop properties))
                       (format "Non-nil when `%s' is active." name))))
    (cl-assert (plistp properties))
    `(progn
       (dolist (parent ',parents)
         (cl-check-type (conn--find-state parent) conn-state))
       (cl-assert
        (cl-loop for parent in ',parents
                 never (memq ',name (conn-state-all-parents parent)))
        nil "Cycle detected in %s inheritance hierarchy" ',name)
       (conn--define-state
        ',name
        ,docstring
        (list ,@(mapcar (lambda (p) `',(or (car-safe p) p)) parents))
        (list ,@(cl-loop for (key value) on properties by #'cddr
                         nconc (pcase key
                                 ((pred keywordp) (list key value))
                                 ((pred symbolp) `(',key ,value))
                                 (_ (error "State property name must be a symbol")))))
        (list ,@(cl-loop for p in parents
                         when (and (consp p)
                                   (eq (cadr p) :no-inherit-keymap))
                         collect `',(car p))))
       (defvar-local ,name nil ,docstring)
       ',name)))

(conn-define-state conn-null-state ()
  "An empty state.

For use in buffers that should not have any other state."
  :no-keymap t
  :lighter "Ø"
  :disable-mark-cursor t
  :cursor '(bar . 4))

(conn-define-state conn-command-state ()
  "A `conn-mode' state for editing test."
  :pop-alternate 'conn-emacs-state
  :lighter "C"
  :suppress-input-method t
  :cursor 'box)

(conn-define-state conn-outline-state ()
  "State for editing outline sections."
  :cursor '(hbar . 10)
  :lighter "*"
  :suppress-input-method t)

(conn-define-state conn-org-state (conn-outline-state)
  "A `conn-mode' state for structural editing of `org-mode' buffers.")

(conn-define-state conn-emacs-state ()
  "A `conn-mode' state for inserting text.

By default `conn-emacs-state' does not bind anything."
  :lighter "E"
  :cursor '(bar . 4))

(conn-define-state conn-mode-line-face-state ()
  "An abstract state for adding a mode-line face to a state.

Causes the mode-line face to be remapped to the face specified by the
:mode-line-face state property when the state is current."
  :abstract t
  :no-keymap t)

(cl-defmethod conn-enter-state ((state (conn-substate conn-mode-line-face-state)))
  (when-let* ((face (conn-state-get state :mode-line-face))
              (cookie (face-remap-add-relative 'mode-line face)))
    (conn-state-defer
      (face-remap-remove-relative cookie)))
  (cl-call-next-method))

;;;;; Read Thing State

(defface conn-read-thing-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(defun conn-read-thing-cursor ()
  `(hbar . ,(floor (default-line-height) 2.5)))

(conn-define-state conn-read-thing-common-state (conn-command-state
                                                 conn-mode-line-face-state)
  "Common elements of thing reading states."
  :cursor #'conn-read-thing-cursor
  :suppress-input-method t
  :mode-line-face 'conn-read-thing-mode-line-face
  :abstract t)

(defvar conn-read-thing-reference
  (list (conn-reference-page "Thing"
          "The region to operate on will be defined by a thing command. A prefix
argument may be supplied for the thing command.")))

;;;;; Emacs State

(defvar conn-emacs-state-register nil
  "If non-nil specifies a register to contain the last `conn-emacs-state' position.")

(defvar-local conn-emacs-state-ring nil
  "Ring of previous positions where `conn-emacs-state' was exited.")

(defvar conn-emacs-state-preserve-prefix-commands
  '(conn-pop-state
    conn-emacs-state-at-mark
    conn-emacs-state))

(cl-defmethod conn-enter-state ((_state (eql conn-emacs-state)))
  (when (memq this-command conn-emacs-state-preserve-prefix-commands)
    (run-hooks 'prefix-command-preserve-state-hook)
    (prefix-command-update))
  (cl-call-next-method))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-emacs-state)))
  (conn-state-defer
    (conn-ring-remove (point) conn-emacs-state-ring #'=)
    (let ((pt (conn--create-marker (point) nil t)))
      (conn-ring-insert-front conn-emacs-state-ring pt)
      (when conn-emacs-state-register
        (when-let* ((marker (get-register conn-emacs-state-register))
                    ((markerp marker)))
          (set-marker marker (point) (current-buffer)))
        (set-register conn-emacs-state-register (copy-marker pt)))))
  (cl-call-next-method))

;;;;; Autopop State

(conn-define-state conn-autopop-state ()
  "Abstract state that automatically pops the state after executing a command.

The :pop-predicate state property is called at the end of
`post-command-hook' and should return non-nil if the state should be
popped and nil if the state should stay active.  The default value of
:pop-predicate is `always'.  Note that `pop-predicate' is not called and
the state stays active if the previous command was a prefix command."
  :abstract t
  :no-keymap t
  :pop-predicate #'always)

(cl-defmethod conn-enter-state ((state (conn-substate conn-autopop-state)))
  (letrec ((prefix-command nil)
           (preserve-state
            (lambda ()
              (setq prefix-command t)))
           (msg-fn (conn-state-get state :message-function))
           (pop-pred
            (let ((pred (conn-state-get state :pop-predicate)))
              (cl-check-type pred function)
              (lambda ()
                (unless (or (cl-shiftf prefix-command nil)
                            (not (funcall pred))
                            (not (eq conn-current-state state)))
                  (conn-enter-state (conn-peek-state))))))
           (setup
            (lambda ()
              (remove-hook 'post-command-hook setup t)
              (add-hook 'prefix-command-preserve-state-hook preserve-state)
              (when msg-fn (add-hook 'post-command-hook msg-fn 91 t))
              (add-hook 'post-command-hook pop-pred 90 t))))
    (conn-state-defer
      (cl-callf2 remq state conn--state-stack)
      (when msg-fn (remove-hook 'post-command-hook msg-fn t))
      (remove-hook 'post-command-hook setup t)
      (remove-hook 'post-command-hook pop-pred t)
      (remove-hook 'prefix-command-preserve-state-hook preserve-state))
    (add-hook 'post-command-hook setup 99 t)
    (cl-call-next-method)))

(conn-define-state conn-one-command-state (conn-command-state
                                           conn-autopop-state)
  "Execute one command in `conn-command-state'."
  :lighter "1C")

;;;;; Mark State

(defvar-local conn-previous-mark-state nil)

(defvar conn--mark-state-rmm nil)

(defvar-local conn-record-mark-state t)

(conn-define-state conn-mark-state (conn-command-state
                                    conn-autopop-state)
  :lighter "M"
  :pop-predicate (lambda ()
                   (or (not (region-active-p))
                       deactivate-mark
                       (progn
                         (setf conn--mark-state-rmm
                               (and (bound-and-true-p rectangle-mark-mode)
                                    (fboundp 'rectangle--pos-cols)
                                    (rectangle--pos-cols (point) (mark))))
                         nil))))

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-state)
  "TAB" 'indent-rigidly
  "Y" 'conn-completing-yank-replace
  "y" 'conn-yank-replace
  "*" 'calc-grab-region
  "C-j" 'conn-join-lines-in-region
  "v" 'rectangle-mark-mode
  "V" 'undefined
  "g" 'conn-surround
  "RET" 'conn-duplicate
  "S-<return>" 'conn-duplicate-and-comment-region
  "SPC" 'conn-push-mark-command)

(cl-defmethod conn-enter-state ((_state (conn-substate conn-mark-state)))
  (setf conn--mark-state-rmm (and (bound-and-true-p rectangle-mark-mode)
                                  (fboundp 'rectangle--pos-cols)
                                  (rectangle--pos-cols (point) (mark)))
        conn-record-mark-state t)
  (conn-state-defer
    (setq deactivate-mark t)
    (unless (or (null conn-record-mark-state)
                (eq this-command 'keyboard-quit))
      (unless conn-previous-mark-state
        (setq conn-previous-mark-state (list (make-marker) (make-marker) nil)))
      (set-marker (nth 0 conn-previous-mark-state) (point))
      (set-marker (nth 1 conn-previous-mark-state) (mark t))
      (setf (nth 2 conn-previous-mark-state) conn--mark-state-rmm)))
  (cl-call-next-method))

;;;;; Buffer State Setup

(defun conn-setup-commit-state ()
  "Set the base state to `conn-emacs-state' in commit message buffers."
  (when (buffer-match-p "COMMIT_EDITMSG" (current-buffer))
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-commit-state -80)

(defun conn-setup-edmacro-state ()
  "Set the base state to `conn-command-state' in edit macro buffers."
  (when (buffer-match-p "\\*Edit Macro\\*" (current-buffer))
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-edmacro-state -80)

(defun conn-setup-dired-state ()
  "Set the base state to `conn-emacs-state' in dired buffers."
  (when (derived-mode-p 'dired-mode)
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-dired-state -50)

(defun conn-setup-null-state ()
  "Set the base state to `conn-null-state' in `conn-null-state-modes' buffers."
  (when (derived-mode-p conn-null-state-modes)
    (conn-push-state 'conn-null-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-null-state -90)

(defun conn-setup-command-state ()
  "Set base state to `conn-command-state' in `conn-command-state-modes' buffers."
  (when (derived-mode-p conn-command-state-modes)
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-command-state)

(defun conn-setup-minibuffer-state ()
  "Setup `minibuffer-mode' buffer state."
  (when (eq major-mode 'minibuffer-mode)
    (setf (alist-get 'conn-emacs-state
                     (conn-get-buffer-property :disable-mark-cursor))
          t)
    (conn-push-state 'conn-emacs-state)
    (add-hook 'minibuffer-setup-hook
              (conn-anaphoricate hook
                (lambda ()
                  (conn--push-ephemeral-mark)
                  (remove-hook 'minibuffer-setup-hook hook))))
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-minibuffer-state -95)

;;;; Read Args

(defvar conn-read-args-last-command nil
  "Last command read by `conn-read-args'.")

(defvar conn-read-args-inhibit-message nil
  "Value for `inhibit-message' in `conn-read-args' message functions.")

(defvar conn--read-args-prefix-mag nil)
(defvar conn--read-args-prefix-sign nil)
(defvar conn--read-args-error-message nil)
(defvar conn--read-args-message nil)
(defvar conn--read-args-message-timeout nil)
(defvar conn--read-args-exiting nil)

(defun conn-read-args-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond (conn--read-args-prefix-mag
         (* (if conn--read-args-prefix-sign -1 1) conn--read-args-prefix-mag))
        (conn--read-args-prefix-sign -1)))

(defun conn-read-args-consume-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'.

Resets the current prefix argument."
  (prog1 (conn-read-args-prefix-arg)
    (setf conn--read-args-prefix-mag nil
          conn--read-args-prefix-sign nil)))

(defun conn-read-args-handle ()
  "Handle the current command.

This function should be called from any function passed as the
:command-handler argument to `conn-read-args' when the function
chooses to handle a command."
  (setf conn--read-args-error-message ""))

(defun conn-read-args-message (format-string &rest args)
  (let ((inhibit-message conn-read-args-inhibit-message)
        (message-log-max nil))
    (setq conn--read-args-message (apply #'format format-string args)
          conn--read-args-message-timeout (time-add nil minibuffer-message-timeout))))


(defun conn--read-args-display-message ()
  (concat
   (when conn--read-args-message (format "[%s] " conn--read-args-message))
   (propertize conn--read-args-error-message 'face 'error)))

(defun conn--read-args-prompt (prompt arguments)
  (message
   (substitute-command-keys
    (concat
     (propertize prompt 'face 'minibuffer-prompt)
     " (arg: "
     (propertize
      (cond (conn--read-args-prefix-mag
             (number-to-string
              (* (if conn--read-args-prefix-sign -1 1)
                 conn--read-args-prefix-mag)))
            (conn--read-args-prefix-sign "[-1]")
            (t "[1]"))
      'face 'read-multiple-choice-face)
     ", \\[reset-arg] reset"
     (when-let* ((args (flatten-tree (mapcar #'conn-display-argument arguments))))
       (string-join (cons nil args) "; "))
     "): "
     (conn--read-args-display-message)))))

;; From embark
(defun conn--all-bindings (keymap)
  (let (bindings)
    (map-keymap
     (lambda (_key def)
       (pcase (keymap--menu-item-binding def)
         ((and (pred keymapp) keymap)
          (setq bindings (nconc (conn--all-bindings keymap) bindings)))
         ((and (pred symbolp) sym)
          (push sym bindings))))
     (keymap-canonicalize keymap))
    bindings))

(defun conn--read-args-completing-read (state args)
  (when-let* ((metadata (conn-state-get state :loop-completion-metadata))
              (table
               (cl-loop for sym in (mapcan #'conn--all-bindings
                                           (current-active-maps))
                        when (cl-loop for arg in args
                                      thereis (conn-argument-predicate arg sym))
                        collect sym)))
    (condition-case _
        (intern
         (completing-read
          "Command: "
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata ,@metadata)
              (complete-with-action action table string pred)))
          nil t))
      (quit nil))))

(cl-defun conn--read-args ( state arglist callback
                            &key
                            command-handler
                            update-handler
                            (display-handler #'conn--read-args-prompt)
                            around
                            overriding-map
                            prompt
                            prefix
                            pre
                            post
                            reference)
  (let ((arguments arglist)
        (prefix (when prefix (prefix-numeric-value prefix)))
        (prompt (or prompt (symbol-name state)))
        (local-exit nil))
    (cl-labels
        ((continue-p ()
           (cl-loop for arg in arguments
                    thereis (conn-argument-required-p arg)))
         (display-message ()
           (when (and conn--read-args-message-timeout
                      (time-less-p conn--read-args-message-timeout nil))
             (setq conn--read-args-message nil
                   conn--read-args-message-timeout nil))
           (let ((inhibit-message conn-read-args-inhibit-message)
                 (message-log-max nil))
             (funcall display-handler prompt arguments))
           (setf conn--read-args-error-message ""))
         (update-args (cmd)
           (setf conn--read-args-error-message "Invalid Command")
           (when command-handler
             (funcall command-handler cmd))
           (let ((next (if update-handler
                           (funcall update-handler cmd arguments)
                         (cl-loop for arg in arguments
                                  collect (conn-update-argument arg cmd)))))
             (unless (equal arguments next)
               (setq conn--read-args-error-message "")
               (setq arguments next))))
         (read-command ()
           (let ((cmd (key-binding (read-key-sequence nil) t)))
             (while (arrayp cmd) ; keyboard macro
               (setq cmd (key-binding cmd t)))
             (when cmd
               (when pre (funcall pre cmd))
               (pcase cmd
                 ('help
                  (when reference
                    (conn-quick-reference reference)))
                 ('digit-argument
                  (let* ((char (if (integerp last-input-event)
                                   last-input-event
                                 (get last-input-event 'ascii-character)))
                         (digit (- (logand char ?\177) ?0)))
                    (setf conn--read-args-prefix-mag
                          (if (integerp conn--read-args-prefix-mag)
                              (+ (* 10 conn--read-args-prefix-mag) digit)
                            (when (/= 0 digit) digit)))))
                 ('backward-delete-arg
                  (when conn--read-args-prefix-mag
                    (cl-callf floor conn--read-args-prefix-mag 10)))
                 ('reset-arg
                  (setf conn--read-args-prefix-mag nil))
                 ('negative-argument
                  (cl-callf not conn--read-args-prefix-sign))
                 ((or 'keyboard-quit 'quit)
                  (keyboard-quit))
                 ('execute-extended-command
                  (when-let* ((cmd (conn--read-args-completing-read
                                    state arguments)))
                    (update-args cmd)))
                 (_ (update-args cmd)))
               (setq conn-read-args-last-command cmd)
               (when post (funcall post cmd)))))
         (cont ()
           (let ((conn--read-args-prefix-mag (when prefix (abs prefix)))
                 (conn--read-args-prefix-sign (when prefix (> 0 prefix)))
                 (conn--read-args-error-message "")
                 (conn--read-args-message nil)
                 (conn--read-args-message-timeout nil)
                 (conn--read-args-exiting nil)
                 (inhibit-message t)
                 (emulation-mode-map-alists
                  `(((,state . ,(thread-last
                                  (mapcar #'conn-argument-keymaps arguments)
                                  (cons overriding-map)
                                  (delq nil)
                                  (make-composed-keymap))))
                    ,@emulation-mode-map-alists)))
             (conn-with-recursive-stack state
               (while (continue-p)
                 (display-message)
                 (read-command)))
             (setq local-exit t))))
      (apply
       (catch 'conn-read-args-return
         (unwind-protect
             (if around (funcall around #'cont) (cont))
           (unless local-exit
             (mapc #'conn-cancel-argument arguments))
           (message nil))
         (cons callback (mapcar #'conn-eval-argument arguments)))))))

(defmacro conn-read-args-return (&rest body)
  (declare (indent 0))
  `(throw 'conn-read-args-return
          (list (lambda () ,@body))))

(defmacro conn-read-args (state-and-keys varlist &rest body)
  "Eval BODY with value in VARLIST read in STATE.

VARLIST bindings should be patterns accepted by `pcase-let'.'

\(fn (STATE KEYS) &rest BODY)"
  (declare (indent 2))
  (pcase-let* (((or `(,state . ,keys)
                    state)
                state-and-keys)
               (patterns nil)
               (values nil))
    (pcase-dolist (`(,pat ,val) varlist)
      (push pat patterns)
      (push val values))
    `(conn--read-args ',state
                      (list ,@(nreverse values))
                      (pcase-lambda ,(nreverse patterns) ,@body)
                      ,@keys)))

;;;;; Loop Arguments

(oclosure-define (conn-read-args-argument
                  ;; (:predicate conn-read-args-argument-p)
                  (:copier conn-set-argument (value &aux (set-flag t)))
                  (:copier conn-unset-argument (value &aux (set-flag nil))))
  (value :type t)
  (set-flag :type boolean)
  (required :type boolean)
  (name :type (or nil string function))
  (reference :type function)
  (keymap :type keymap))

(oclosure-define (conn-read-args-argument-wrapper)
  (wrapped :type list))

(defalias 'conn-read-args-argument-name
  'conn-read-args-argument--name)

(defalias 'conn-read-args-argument-value
  'conn-read-args-argument--value)

(defalias 'conn-read-args-argument-keymap
  'conn-read-args-argument--keymap)

(cl-defgeneric conn-cancel-argument (argument)
  ( :method (arg) arg)
  ( :method ((arg conn-read-args-argument-wrapper))
    (mapc #'conn-cancel-argument
          (conn-read-args-argument-wrapper--wrapped arg))))

(cl-defgeneric conn-argument-required-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-read-args-argument))
    (and (conn-read-args-argument--required arg)
         (not (conn-read-args-argument--set-flag arg))))
  ( :method ((arg conn-read-args-argument-wrapper))
    (and (seq-find #'conn-argument-required-p
                   (conn-read-args-argument-wrapper--wrapped arg))
         t)))

(cl-defgeneric conn-update-argument (argument form)
  ( :method (arg _form) arg)
  ( :method ((arg conn-read-args-argument) form)
    (funcall arg arg form))
  ( :method ((arg conn-read-args-argument-wrapper) form)
    (funcall arg arg form)))

(cl-defgeneric conn-eval-argument (argument)
  (declare (important-return-value t))
  ( :method (arg) arg)
  ( :method ((arg conn-read-args-argument))
    (conn-read-args-argument-value arg))
  ( :method ((arg conn-read-args-argument-wrapper))
    (mapcar #'conn-read-args-argument-value
            (conn-read-args-argument-wrapper--wrapped arg))))

(cl-defgeneric conn-display-argument (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg string)) arg)
  ( :method ((arg conn-read-args-argument))
    (pcase (conn-read-args-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and (pred functionp) fn)
       (and-let* ((str (funcall fn arg))
                  ((stringp str)))
         str))))
  ( :method ((arg conn-read-args-argument-wrapper))
    (thread-last
      (conn-read-args-argument-wrapper--wrapped arg)
      (mapcar #'conn-display-argument)
      (delq nil))))

(cl-defgeneric conn-argument-predicate (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg _val) nil)
  ( :method ((arg conn-read-args-argument-wrapper) val)
    (cl-loop for arg in (conn-read-args-argument-wrapper--wrapped arg)
             thereis (conn-argument-predicate arg val))))

(cl-defgeneric conn-argument-keymaps (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg cons))
    (conn-argument-keymaps (cdr arg)))
  ( :method ((arg conn-read-args-argument))
    (conn-read-args-argument-keymap arg))
  ( :method ((arg conn-read-args-argument-wrapper))
    (when-let* ((maps (thread-last
                        (conn-read-args-argument-wrapper--wrapped arg)
                        (mapcar #'conn-argument-keymaps)
                        (delq nil))))
      (make-composed-keymap maps))))

(provide 'conn-states)
