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

(require 'mule-util)
(require 'conn-vars)
(require 'conn-utils)
(require 'conn-quick-ref)
(eval-when-compile
  (require 'cl-lib))

(defvar conn-local-mode)
(declare-function conn-local-mode "conn")
(declare-function conn-thing-pretty-print "conn-things")
(declare-function face-remap-remove-relative "face-remap")
(declare-function conn--thing-post-command-hook "conn-things")

;;;; States

(defcustom conn-null-state-modes
  (list 'calc-mode
        'calc-trail-mode
        'calc-keypad-mode
        'image-mode
        'doc-view-mode
        'vterm-mode)
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
  "Hook responsible for setting up the base state in a buffer.

The hook is run when `conn-local-mode' is turned on in a buffer or when
a buffer is cloned. Each function is called in turn until one returns
non-nil.  A function returning non-nil should set the base state for the
current buffer by pushing it to the stack with `conn-push-state'.  The
function may setup any other necessary state as well.")

(defvar-local conn-current-state nil
  "Current conn state in buffer.")

(defvar conn-next-state nil
  "Bound during `conn-state-on-exit' forms to the next state to be entered.")

(defvar conn-previous-state nil
  "Bound during `conn-enter-state' to the state being exited.")

(defvar conn-entering-recursive-stack nil
  "Non-nil during `conn-enter-state' when entering a recursive stack.")

(defvar-local conn--state-stack nil
  ;; Be careful when modifying this variable.
  ;; `conn--state-re-entry-functions' is keyed on the conses of the
  ;; stack so copying the stack will result in re-entry functions not
  ;; being run.
  "Previous conn states in buffer.")

(defvar conn--minor-mode-maps-sort-tick 0)

(cl-defstruct (conn-state
               (:constructor nil)
               ( :constructor conn--make-state
                 (name
                  docstring
                  parents
                  &aux
                  (properties (make-hash-table :test 'eq))
                  (minor-mode-depths (make-hash-table :test 'eq))
                  (minor-mode-sort-tick conn--minor-mode-maps-sort-tick)
                  (minor-mode-maps (list :conn-minor-mode-map-alist))
                  (major-mode-maps (make-hash-table :test 'eq))))
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
  (minor-mode-maps nil :type alist :read-only t)
  (major-mode-maps nil :type hash-table :read-only t))

(define-inline conn--find-state (state)
  (declare (important-return-value t)
           (gv-setter
            (lambda (val)
              (macroexp-let2 nil val val
                `(progn
                   (cl-check-type ,val conn-state)
                   (setf (get ,state :conn--state) ,val))))))
  (inline-letevals (state)
    (inline-quote
     (or (get ,state :conn--state)
         (error "%s is not a state" ,state)))))

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
  (inline-quote (get ,state :conn--state)))

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
                 (mapcar #'conn-state-all-parents
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

(defun conn--state-all-children-subr (state)
  (declare (side-effect-free t)
           (important-return-value t))
  (let ((children (conn-state--children (conn--find-state state))))
    (append children (mapcan #'conn--state-all-children-subr children))))

(defun conn-state-all-children (state)
  "Return all children for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (delete-dups (conn--state-all-children-subr state)))

(define-inline conn-substate-p (state parent)
  "Return non-nil if STATE is a substate of PARENT."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (memq ,parent (conn-state-all-parents ,state))))

;;;;; Properties

(eval-and-compile
  (defun conn-declare-state-property (property doc-string &optional static)
    "Declare a state property PROPERTY.

DOC-STRING is displayed in the `conn-define-state' doc-string when
non-nil.

If STATIC is non-nil then the property is declared static.  Static state
properties can only be changed by redefining a state and are not
inherited."
    (setf (get property :conn-static-property) static)
    (when doc-string
      (setf (alist-get property
                       (get 'conn-define-state :known-properties))
            doc-string)))

  (define-inline conn-property-static-p (property)
    "Return non-nil if PROPERTY is static.

Static state properties can only be changed by redefining a state and
are not inherited.

See also `conn-declare-state-property'."
    (declare (side-effect-free t)
             (important-return-value t))
    (inline-quote
     (and (get ,property :conn-static-property) t)))

  (defun conn-state-get--cmacro (exp
                                 state
                                 property
                                 &optional
                                 no-inherit
                                 default)
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
PROPERTY.  If no parent has that property either than nil is returned.

If NO-INHERIT is non-nil or if PROPERTY is a static property then
STATE's parents are not checked.

DEFAULT is a value to return if PROPERTY is not found."
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
         conn-state--properties
         (puthash ,property ,value))))))

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
  "Return t if STATE has an explicit value set for PROPERTY."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (thread-first
     (gethash ,property
              (conn-state--properties (conn--find-state ,state))
              conn--key-missing)
     (eq conn--key-missing)
     not)))

;;;;; Keymaps

(defvar-local conn--state-map nil)
(defvar-local conn--major-mode-map nil)
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
          to-remove #'eq))
      (maphash
       (lambda (mode map)
         (setf (cdr map)
               (cl-loop for pstate in parents
                        for pmap = (conn-get-major-mode-map pstate mode t)
                        when pmap collect pmap)))
       (conn-state--major-mode-maps state-obj)))))

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
  "Set STATE's keymap to MAP."
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
  "Return the state keymap for STATE.

If dont-create is non-nil then return nil if STATE does not have a
keymap."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (conn-state--keymap (conn--find-state state))
        (unless dont-create
          (setf (conn-get-state-map state)
                (if (conn-state-get state :full-keymap)
                    (make-keymap)
                  (make-sparse-keymap)))))))

;;;;;; Major Mode Maps

(defvar-local conn--active-major-mode-maps nil)

(defconst conn--composed-major-mode-maps-cache
  (make-hash-table :test 'equal))

(defun conn--compose-major-mode-map (state mode)
  (declare (important-return-value t))
  (cl-assert (not (conn-state-get state :no-keymap)) nil
             "%s :no-keymap property non-nil" state)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--composed-major-mode-maps-cache))
                (get-composed-map (state)
                  `(gethash mode (conn-state--major-mode-maps
                                  (conn--find-state ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (or (get-composed-map state)
        (setf (get-composed-map state)
              (make-composed-keymap (parent-maps state))))))

(defun conn-set-major-mode-map (state mode map)
  "Set the major MODE map for STATE to MAP."
  (cl-assert (keymapp map))
  (cl-check-type mode symbol)
  (cl-assert (not (conn-state-get state :no-keymap)) nil
             "%s :no-keymap property non-nil" state)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--composed-major-mode-maps-cache))
                (get-composed-map (state)
                  `(gethash mode (conn-state--major-mode-maps
                                  (conn--find-state ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
    (setf (get-map state) map)
    (setf (get-composed-map state)
          (make-composed-keymap (parent-maps state)))
    (dolist (child (conn-state-all-children state) map)
      (if-let* ((map (get-composed-map child)))
          (setf (cdr map) (parent-maps child))
        (unless (conn-state-get child :no-keymap)
          (setf (get-composed-map child)
                (make-composed-keymap (parent-maps child))))))))

(gv-define-simple-setter conn-get-major-mode-map conn-set-major-mode-map)

(defun conn-get-major-mode-map (state mode &optional dont-create)
  "Return keymap for major MODE in STATE.

If one does not exists create a new sparse keymap for MODE in STATE and
return it."
  (declare (important-return-value t))
  (if (conn-state-get state :no-keymap)
      (unless dont-create
        (error "%s has non-nil :no-keymap property" state))
    (or (gethash (cons state mode) conn--composed-major-mode-maps-cache)
        (unless dont-create
          (setf (conn-get-major-mode-map state mode)
                (make-sparse-keymap))))))

;;;;;; Minor Mode Maps

(define-inline conn--mode-maps-sorted-p (state)
  (declare (side-effect-free t)
           (important-return-value t)
           (gv-setter
            (lambda (value)
              (cl-labels ((setter (v)
                            `(setf (conn-state--minor-mode-sort-tick
                                    (conn--find-state ,state))
                                   ,v)))
                (pcase value
                  ('nil (setter nil))
                  ((pred macroexp-const-p)
                   (setter 'conn--minor-mode-maps-sort-tick))
                  (_ (macroexp-let2 nil value value
                       `(progn
                          ,(setter `(when ,value conn--minor-mode-maps-sort-tick))
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
  "Set the DEPTH of minor MODE's map in STATE.

The map depth controls the sorting order for minor mode maps.  Minor
mode maps with a lesser depth value are sorted before minor mode maps
with a greater depth value.  This means that minor mode maps with a
lesser depth value take precedence over minor mode maps with a greater
depth value.  Depth should be an integer between -100 and 100."
  (cl-check-type mode symbol)
  (cl-assert (<= -100 depth 100) nil "Depth must be between -100 and 100")
  (if (null state)
      (progn
        (setf (get mode :conn-mode-depth) depth)
        (cl-incf conn--minor-mode-maps-sort-tick))
    (cl-assert (not (conn-state-get state :no-keymap)) nil
               "%s :no-keymap property non-nil" state)
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
  "Set minor MODE map to MAP in STATE."
  (cl-assert (keymapp map))
  (cl-assert (not (conn-state-get state :no-keymap)) nil
             "%s :no-keymap property non-nil" state)
  (cl-check-type mode symbol)
  (cl-macrolet ((get-map (state)
                  `(gethash (cons ,state mode) conn--minor-mode-maps-cache))
                (get-composed-map (state)
                  `(alist-get mode (cdr (conn-state-minor-mode-maps-alist ,state))))
                (parent-maps (state)
                  `(cl-loop for parent in (conn-state-all-keymap-parents ,state)
                            for pmap = (get-map parent)
                            when pmap collect pmap)))
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
return it.  If DONT-CREATE is non-nil and a map does not already exist
then an error is signaled."
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

(defvar-local conn-disable-input-method-hooks nil
  "When non-nil conn input method hooks and advice do not run.")

(defvar-local conn--input-method nil
  "Input method for current buffer.")
(put 'conn--input-method 'permanent-local t)

(defvar-local conn--input-method-title nil
  "Title string of the current input method shown in mode line.")
(put 'conn--input-method-title 'permanent-local t)

(defvar-local conn--prev-mode-line-mule-info nil)

(defun conn--toggle-input-method-ad (&rest app)
  (if (and conn-local-mode
           (not isearch-mode)
           (not conn-disable-input-method-hooks)
           (conn-state-get conn-current-state :suppress-input-method)
           conn--input-method)
      (unwind-protect
          (progn
            (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
            (activate-input-method conn--input-method)
            (deactivate-input-method))
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t))
    (apply app)))

(defun conn--activate-input-method ()
  "Enable input method in states with nil :conn-suppress-input-method property."
  (when (and conn-local-mode
             (not conn-disable-input-method-hooks))
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
  (unless conn-disable-input-method-hooks
    (setq conn--input-method nil
          conn--input-method-title nil)))
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
    (let ((hook (make-symbol "hook")))
      (fset hook (lambda ()
                   (conn--activate-input-method)
                   (add-hook 'input-method-activate-hook
                             #'conn--activate-input-method
                             50 t)
                   (add-hook 'input-method-deactivate-hook
                             #'conn--deactivate-input-method
                             50 t)
                   (remove-hook 'isearch-mode-end-hook hook)))
      (add-hook 'isearch-mode-end-hook hook))))
(put 'conn--isearch-input-method 'permanent-local-hook t)

(defun conn--input-method-mode-line ()
  "Display the mode line information for conn--input-method.

This ensures that the mode line information for the current input method
is shown even if the input method is deactivated because a state is
suppressing it."
  (cond
   (conn-local-mode
    (setq conn--prev-mode-line-mule-info
          (buffer-local-set-state
           mode-line-mule-info `(""
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
                                 (:eval (mode-line-eol-desc))))))
   (conn--prev-mode-line-mule-info
    (buffer-local-restore-state
     (cl-shiftf conn--prev-mode-line-mule-info nil)))))

(defmacro conn-with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  `(let ((conn-disable-input-method-hooks t))
     (activate-input-method conn--input-method)
     ,@body))

;;;;; Macros

;; Adapted from map pattern, we can't just use the map pattern on the
;; property table because of inheritance.
(pcase-defmacro conn-state-properties (&rest properties)
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
  "Execute BODY in a recursive stack with STATE as the base state."
  (declare (debug (form body))
           (indent 1))
  (cl-with-gensyms (buffer stack)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (unwind-protect
           (progn
             (ignore (conn-enter-recursive-stack ,state))
             ,@body)
         (with-current-buffer ,buffer
           (conn-enter-state
            (car ,stack)
            (conn-stack-transition exit-recursive
              (setq conn--state-stack ,stack)
              (conn-call-re-entry-fns)))
           (conn-update-lighter))))))

(defmacro conn-without-recursive-stack (&rest body)
  "Execute BODY with the most recent recursive stack temporarily exited."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (stack buffer)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (unwind-protect
           (progn
             (if-let* ((tail (memq nil conn--state-stack)))
                 (progn
                   (conn-enter-state
                    (cadr tail)
                    (conn-stack-transition exit-recursive
                      (setq conn--state-stack (cdr tail))
                      (conn-call-re-entry-fns)))
                   (conn-update-lighter))
               (error "Not in a recursive state"))
             ,@body)
         (with-current-buffer ,buffer
           (conn-enter-state
            (car ,stack)
            (conn-stack-transition enter-recursive
              (setq conn--state-stack ,stack)))
           (conn-update-lighter))))))

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

(defvar conn-state-entry-hook nil
  "Hook run when a state is entered.

When this hook is run `conn-previous-state' will be bound to the state
that has just been exited.")

(defvar-local conn-pop-alternate-default 'conn-emacs-state
  "Default state to enter if the state is popped in the base state.")

(defvar-local conn--state-exit-functions (list 'conn--state-exit-default)
  "Code to be run when the current state is exited.")

(defvar-local conn--state-exit-functions-ids nil)

(defconst conn--state-transition-table
  (make-hash-table :test 'eq))

(cl-defstruct (conn-state-transition
                  (:constructor conn-state-transition)
                (:conc-name conn-state-transition--))
  (type nil :type symbol :read-only t)
  (data nil :type list :read-only t)
  (body #'ignore :type function :read-only t))

(defmacro conn-define-state-transition (name &optional parent)
  `(setf (gethash ',name conn--state-transition-table)
         ',(or parent t)))

(conn-define-state-transition pop)
(conn-define-state-transition exit-recursive pop)
(conn-define-state-transition push)
(conn-define-state-transition enter-recursive push)
(conn-define-state-transition exit)
(conn-define-state-transition clone exit)

(define-inline conn-state-transition-get (op propname)
  (inline-quote
   (plist-get (conn-state-transition--data ,op) ,propname)))

(define-inline conn-state-subtransition-p (name val)
  (inline-quote
   (cl-loop for type = (conn-state-transition--type ,val)
            then (gethash type conn--state-transition-table)
            while type thereis (eq type ,name))))

(pcase-defmacro conn-state-transition (name &rest properties)
  (if properties
      `(and (pred (conn-state-subtransition-p ',name))
            (cl-struct
             conn-state-transition
             (data (map ,@properties))))
    `(pred (conn-state-subtransition-p ',name))))

(defmacro conn-call-re-entry-fns ()
  "Call re-entry functions for the current stack state.
Can only be used within the body of `conn-stack-transition'."
  (error "conn-call-re-entry-fns only allowed inside conn-stack-transition"))

(defmacro conn-stack-transition (name-and-properties &rest body)
  (declare (indent 1))
  (pcase name-and-properties
    ((or (and name (pred symbolp) (let properties nil))
         `(,name . ,properties))
     (cl-with-gensyms (transition)
       (let* ((body (if body
                        `(lambda (,transition)
                           (cl-macrolet ((conn-call-re-entry-fns ()
                                           `(conn--run-re-entry-fns ,',transition)))
                             ,@body))
                      `#'ignore))
              (expr `(conn-state-transition
                         :type ',name
                         :data ,(if properties `(list ,@properties))
                         :body ,body)))
         (if (gethash name conn--state-transition-table)
             expr
           (macroexp-warn-and-return
            (format "Undefined stack op %s" name)
            expr nil nil name-and-properties)))))))

(define-inline conn--perform-transition (transition)
  (inline-letevals (transition)
    (inline-quote
     (funcall (conn-state-transition--body ,transition) ,transition))))

(defun conn--state-exit-default (_type _)
  (when conn-current-state
    (set (cl-shiftf conn-current-state nil) nil)))

(defun conn--run-exit-fns (type)
  (let ((fns conn--state-exit-functions))
    (setq conn--state-exit-functions (list 'conn--state-exit-default)
          conn--state-exit-functions-ids nil)
    (funcall (car fns) type (cdr fns))))

(defmacro conn-state-on-exit (type &rest body)
  "Defer evaluation of BODY until the current state is exited.

Note that if a `conn-state-on-exit' form is evaluated multiple times in
one state then BODY will evaluated that many times when the state is
exited.  If you want to ensure that BODY will be evaluated only once
when the current state exits then use `conn-state-on-exit-once'.

BODY may be be run more than once if a buffer is cloned during the
current state.  See also `conn--clone-buffer-setup'.

When BODY is evaluated `conn-next-state' will be bound to the state that
is being entered after the current state has exited or nil if
`conn-local-mode' is being exited or a cloned buffer is being setup."
  (declare (indent 1)
           (debug (def-body)))
  (when (eql ?_ (string-to-char (symbol-name type)))
    (setq type (gensym)))
  (cl-with-gensyms (rest)
    `(push (lambda (,type ,rest)
             (unwind-protect
                 ,(macroexp-progn body)
               (funcall (car ,rest) ,type (cdr ,rest))))
           conn--state-exit-functions)))

(defmacro conn-state-on-exit-once (name type &rest body)
  "Like `conn-state-on-exit' but BODY will be evaluated only once per state.

For more information see `conn-state-on-exit'."
  (declare (indent 2)
           (debug (def-body)))
  (when (eql ?_ (string-to-char (symbol-name type)))
    (setq type (gensym)))
  (cl-with-gensyms (rest)
    `(unless (memq ',name conn--state-exit-functions-ids)
       (push ',name conn--state-exit-functions-ids)
       (push (lambda (,type ,rest)
               (unwind-protect
                   ,(macroexp-progn body)
                 (funcall (car ,rest) ,type (cdr ,rest))))
             conn--state-exit-functions))))

(defconst conn--state-re-entry-functions
  (make-hash-table :test 'eq
                   :weakness 'key))

(defmacro conn-state-on-re-entry (type &rest body)
  "Defer evaluation of BODY until the current state is re-entered.

BODY will never be evaluated if the state is not re-entered."
  (declare (indent 1))
  (when (eql ?_ (string-to-char (symbol-name type)))
    (setq type (gensym)))
  (cl-symbol-macrolet ((place '(gethash conn--state-stack conn--state-re-entry-functions)))
    (cl-with-gensyms (rest fns)
      `(let ((,fns (or ,place (setf ,place (cons nil nil)))))
         (push (lambda (,type ,rest)
                 (unwind-protect
                     ,(macroexp-progn body)
                   (when ,rest (funcall (car ,rest) ,type (cdr ,rest)))))
               (cdr ,fns))))))

(defmacro conn-state-on-re-entry-once (name type &rest body)
  "Defer evaluation of BODY until the current state is re-entered.

BODY will never be evaluated if the state is not re-entered."
  (declare (indent 1))
  (when (eql ?_ (string-to-char (symbol-name type)))
    (setq type (gensym)))
  (cl-symbol-macrolet ((place '(gethash conn--state-stack conn--state-re-entry-functions)))
    (cl-with-gensyms (rest fns)
      `(let ((,fns (or ,place (setf ,place (cons nil nil)))))
         (unless (memq ',name (car ,fns))
           (push ',name (car ,place))
           (push (lambda (,type ,rest)
                   (unwind-protect
                       ,(macroexp-progn body)
                     (when ,rest (funcall (car ,rest) ,type (cdr ,rest)))))
                 (cdr ,fns)))))))

(define-inline conn--run-re-entry-fns (transition)
  (inline-quote
   (when-let* ((fns (cdr (gethash conn--state-stack
                                  conn--state-re-entry-functions))))
     (remhash conn--state-stack conn--state-re-entry-functions)
     (funcall (car fns) ,transition (cdr fns)))))

(defvar conn-state-lighter-separator
  (if (char-displayable-p ?→) "→" ">")
  "Separator string for state lighters in `conn-lighter'.")

(defun conn--get-lighter ()
  (with-memoization (buffer-local-value 'conn-lighter (current-buffer))
    (let ((lighter (conn-state-get conn-current-state :lighter)))
      (dolist (elem (cdr conn--state-stack))
        (setq lighter
              (if elem
                  (concat (conn-state-get elem :lighter)
                          conn-state-lighter-separator
                          lighter)
                (concat "[" lighter "]"))))
      (concat " [" lighter "]"))))

(defun conn-update-lighter (&optional buffer)
  "Force the mode-line lighter to be updated in BUFFER.

If BUFFER is nil then use the current buffer."
  (setf (buffer-local-value 'conn-lighter (or buffer (current-buffer))) nil)
  (force-mode-line-update))

(defun conn-set-major-mode-maps (maps)
  "Set buffers activate conn major mode maps to MAPS."
  (setf conn--active-major-mode-maps maps
        conn--major-mode-map `((conn-local-mode
                                . ,(make-composed-keymap
                                    (mapcar (lambda (mode)
                                              (conn--compose-major-mode-map
                                               conn-current-state
                                               mode))
                                            conn--active-major-mode-maps))))))

(defun conn-get-major-mode-maps ()
  "Return a list of the currently active conn major mode maps."
  (declare (gv-setter conn-set-major-mode-maps))
  (copy-sequence conn--active-major-mode-maps))

(defun conn--setup-state-keymaps ()
  (if (conn-state-get conn-current-state :no-keymap)
      (setf conn--state-map nil
            conn--major-mode-map nil
            conn--minor-mode-maps nil)
    (setf conn--state-map `((conn-local-mode . ,(conn--compose-state-map)))
          conn--major-mode-map `((conn-local-mode
                                  . ,(make-composed-keymap
                                      (mapcar (lambda (mode)
                                                (conn--compose-major-mode-map
                                                 conn-current-state
                                                 mode))
                                              conn--active-major-mode-maps))))
          conn--minor-mode-maps (conn-state-minor-mode-maps-alist
                                 conn-current-state))))

(defun conn--setup-state-properties ()
  (setf cursor-type
        (let ((c (conn-state-get conn-current-state :cursor nil t)))
          (if (functionp c) (funcall c) c))))

(cl-defgeneric conn-enter-state (state transition)
  "Enter STATE.

Code that is run when a state is entered should be added as methods to
this function.  The (conn-substate STATE) specializer is provided so
that code can be run for every state inheriting from some parent state.

To execute code when a state is exiting use `conn-state-on-exit'."
  ( :method ((_state (conn-substate t)) _transition)
    "Noop" nil)
  ( :method (state _transition)
    (error "Attempting to enter unknown state: %s" state)))

(cl-defmethod conn-enter-state :around ((state (conn-substate t))
                                        transition)
  (unless (symbol-value state)
    (when (conn-state-get state :abstract)
      (error "Attempting to enter abstract state %s" state))
    (let (conn-previous-state)
      (unwind-protect
          (progn
            (let ((conn-next-state state))
              (conn--run-exit-fns transition))
            (cl-shiftf conn-previous-state conn-current-state state)
            (conn--setup-state-properties)
            (conn--setup-state-keymaps)
            (conn--activate-input-method)
            (unless (conn--mode-maps-sorted-p state)
              (conn--sort-mode-maps state))
            (cl-call-next-method)
            (conn-update-lighter)
            (set state t)
            (conn--perform-transition transition))
        (unless (symbol-value state)
          (conn-local-mode -1)
          (message "Error entering state %s." state)))
      (run-hook-wrapped
       'conn-state-entry-hook
       (lambda (fn)
         (condition-case err
             (funcall fn)
           (error
            (remove-hook 'conn-state-entry-hook fn)
            (message "Error in conn-state-entry-hook: %s" (car err)))))))))

(defun conn-push-state (state)
  "Enter STATE and push it to the state stack."
  (unless (symbol-value state)
    (conn-enter-state
     state
     (conn-stack-transition push
       (push state conn--state-stack)))))

(defun conn-pop-state ()
  "Pop to the previous state in the state stack.

If the current state is the base state for the stack then push the
state given by the current state's :pop-alternate property.  If the
current state does not have a :pop-alternate property then push
`conn-command-state'."
  (interactive)
  (if-let* ((state (cadr conn--state-stack)))
      (conn-enter-state
       state
       (conn-stack-transition pop
         (pop conn--state-stack)
         (conn-call-re-entry-fns)))
    (conn-push-state
     (conn-state-get conn-current-state :pop-alternate
                     t conn-pop-alternate-default))))

(defun conn-peek-state ()
  "Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (cadr conn--state-stack))

(defun conn-buffer-base-state (&optional buffer)
  "Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (conn-thread<-
    'conn--state-stack
    (buffer-local-value (or buffer (current-buffer)))
    last
    car))

(defun conn-enter-recursive-stack (state)
  "Enter a recursive stack with STATE as the base state."
  (declare (important-return-value t))
  (prog1 conn--state-stack
    (unwind-protect
        (progn
          (conn-enter-state
           state
           (conn-stack-transition enter-recursive
             (push nil conn--state-stack)
             (push state conn--state-stack)))
          ;; Ensure the lighter gets updates even if we haven't changed state
          (conn-update-lighter))
      (unless (car conn--state-stack)
        (pop conn--state-stack)))))

(defun conn-exit-recursive-stack (cookie)
  "Exit the recursive state stack associated with COOKIE.

COOKIE should be a cookie returned by `conn-enter-recursive-stack'."
  (if (cl-loop for cons on conn--state-stack
               thereis (eq cons cookie))
      (progn
        (conn-enter-state
         (car conn--state-stack)
         (conn-stack-transition enter-recursive
           (setq conn--state-stack cookie)))
        ;; Ensure the lighter gets updates
        ;; even if we haven't changed state
        (conn-update-lighter))
    (error "Invalid recursive stack cookie")))

;;;;; Definitions

(defun conn--define-state (name docstring parents properties no-inherit-keymaps)
  (cl-labels ((setup-properties (table)
                (cl-loop with kvs = properties
                         for (k v) on kvs by #'cddr
                         do (puthash k v table))
                (cl-callf seq-union
                    (gethash :no-inherit-keymaps table)
                  no-inherit-keymaps)))
    (if-let* ((state-obj (conn-state-name-p name)))
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
      (let ((state-obj (conn--make-state name docstring parents)))
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

(defun conn--make-state-docstring (state docstring)
  (put state 'variable-documentation
       (list
        (lambda ()
          (concat
           docstring
           "\n\n\tParent states:\n"
           (cl-loop for parent in (cdr (conn-state-all-parents state))
                    concat (format "`%s'\n" parent) into ps
                    finally return (if (string-empty-p ps)
                                       "No parent states"
                                     ps))
           "\n\n\tChild states:\n"
           (cl-loop for child in (conn-state-all-children state)
                    concat (format "`%s'\n" child) into cs
                    finally return (if (string-empty-p cs)
                                       "No child states"
                                     cs)))))))

(defmacro conn-define-state (name parents &rest properties)
  "Define a conn state named NAME.

Defines a variable NAME which is non-nil when the state is active.

PARENTS is a list of states from which NAME should inherit properties
and keymaps.

PROPERTIES is a property list defining the state properties for NAME.  A
state may have any number of properties.  A state will inherit the value
for unspecified properties from its parents.  Static state properties
can only be changed by redefining a state and are not inherited.

\(fn NAME PARENTS &optional DOCSTRING [KEY VALUE ...])"
  (declare (debug ( name form
                    [&optional stringp]
                    [&rest symbolp form]))
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
       (defvar-local ,name nil)
       (conn--make-state-docstring ',name ,docstring)
       ',name)))

(defun conn--make-define-state-docstring ()
  (let* ((main (documentation (symbol-function 'conn-define-state) 'raw))
         (ud (help-split-fundoc main 'conn-define-state)))
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main ""))
      (insert "\n\n\tKnown properties for states:\n\n")
      (pcase-dolist (`(,property . ,doc-string)
                     (reverse (get 'conn-define-state :known-properties)))
        (insert (format "`%s' (static: %s)\n %s"
                        (upcase (symbol-name property))
                        (conn-property-static-p property)
                        doc-string))
        (insert "\n\n"))
      (let ((combined-doc (buffer-string)))
        (if ud
            (help-add-fundoc-usage combined-doc (car ud))
          combined-doc)))))

(put 'conn-define-state 'function-documentation
     '(conn--make-define-state-docstring))

(conn-declare-state-property
 :lighter
 "State lighter to display in the mode line state stack display.")

(conn-declare-state-property
 :suppress-input-method
 "If non-nil suppress the current input method while the state is active.")

(conn-declare-state-property
 :cursor
 "The `cursor-type' to use while in the state.")

(conn-declare-state-property
 :pop-alternate
 "The state to enter if the defined state is the base state and the state
stack is popped."
 t)

(eval-and-compile
  (conn-declare-state-property
   :no-keymap
   "Do not allow a keymap to be created for this state"
   t)

  (conn-declare-state-property
   :full-keymap
   "Create a chartable alist keymap for this state.  See also `make-keymap'."
   t)

  (conn-declare-state-property
   :no-inherit-keymaps
   "Parent states from which this state should not inherit keymaps."
   t)

  (conn-declare-state-property
   :abstract
   "If non-nil indicates that a state is only for other states to inherit
from and should not be entered.  `conn-enter-state' will signal an error
if it is called with an abstract state."
   t))

(conn-define-state conn-null-state ()
  "An empty state.

For use in buffers that should not have any other state."
  :no-keymap t
  :lighter "Ø")

(conn-define-state conn-command-state ()
  "A state for editing commands."
  :lighter "C"
  :suppress-input-method t
  :cursor 'box
  :full-keymap t)

(conn-define-state conn-outline-state ()
  "A state for editing outline sections."
  :cursor '(hbar . 10)
  :lighter "*"
  :suppress-input-method t
  :full-keymap t)

(conn-define-state conn-org-state (conn-outline-state)
  "A state for structural editing of `org-mode' buffers.")

(conn-define-state conn-emacs-state ()
  "A state for inserting text.

By default `conn-emacs-state' does not bind anything except
`conn-pop-state'."
  :pop-alternate 'conn-command-state
  :lighter "E"
  :cursor '(bar . 4))

(conn-define-state conn-mode-line-face-state ()
  "An abstract state for adding a mode-line face to a state.

Causes the mode-line face to be remapped to the face specified by the
:mode-line-face state property when the state is current."
  :abstract t
  :no-keymap t)

(conn-declare-state-property
 :mode-line-face
 "Used by `conn-mode-line-face-state'.  Face for the mode line in the
state.")

(cl-defmethod conn-enter-state ((state (conn-substate conn-mode-line-face-state))
                                _transition)
  (when-let* ((face (conn-state-get state :mode-line-face))
              (cookie (face-remap-add-relative 'mode-line face)))
    (conn-state-on-exit _type
      (face-remap-remove-relative cookie)))
  (cl-call-next-method))

;;;;; Read Thing State

(defface conn-read-thing-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(defun conn-read-thing-cursor ()
  "Returns the cursor to be used in `conn-read-thing-common-state'."
  `(hbar . ,(floor (default-line-height) 2.5)))

(conn-define-state conn-read-thing-common-state (conn-command-state
                                                 conn-mode-line-face-state)
  "Common elements of thing reading states."
  :cursor #'conn-read-thing-cursor
  :suppress-input-method t
  :mode-line-face 'conn-read-thing-mode-line-face
  :abstract t)

;;;;; Emacs State

(defvar-local conn-emacs-state-ring nil
  "Ring of previous positions where `conn-emacs-state' was exited.")

(defvar conn-emacs-state-preserve-prefix-commands
  '(conn-pop-state
    conn-emacs-state-at-mark
    conn-emacs-state))

(cl-defmethod conn-enter-state ((_state (eql conn-emacs-state))
                                _transition)
  (when (memq this-command conn-emacs-state-preserve-prefix-commands)
    (run-hooks 'prefix-command-preserve-state-hook)
    (prefix-command-update))
  (cl-call-next-method))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-emacs-state))
                                _transition)
  (conn-state-on-exit _exit-type
    (conn-ring-delete (point) conn-emacs-state-ring #'=)
    (let ((pt (conn--create-marker (point) nil t)))
      (conn-ring-insert-front conn-emacs-state-ring pt)))
  (cl-call-next-method))

;;;;; Autopop State

(conn-define-state-transition autopop pop)

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

(conn-declare-state-property
 :pop-predicate
 "Used by `conn-autopop-state'.  A function which is called at the end of
`post-command-hook' and should return non-nil if the state should be
popped and nil otherwise.  The default value is `always'.  Note that the
function is not called and the state stays active if the previous
command was a prefix command.")

(cl-defmethod conn-enter-state ((state (conn-substate conn-autopop-state))
                                transition)
  (when (or (conn-state-subtransition-p 'enter-recursive transition)
            (null conn--state-stack))
    (error "%s cannot be the base state" state))
  (let ((prefix-command nil)
        (msg-fn (make-symbol "msg"))
        (preserve-state (make-symbol "preserve-state"))
        (post (make-symbol "post"))
        (pre (make-symbol "pre")))
    (when-let* ((fn (conn-state-get state :message-function)))
      (fset msg-fn
            (lambda ()
              (funcall fn)
              (remove-hook 'post-command-hook msg-fn t))))
    (fset preserve-state (lambda () (setq prefix-command t)))
    (fset pre (lambda ()
                (remove-hook 'pre-command-hook pre t)
                (add-hook 'prefix-command-preserve-state-hook preserve-state)
                (add-hook 'post-command-hook post 90 t)
                (when (symbol-function msg-fn)
                  (add-hook 'post-command-hook msg-fn 91 t))))
    (fset post (let ((pred (conn-state-get state :pop-predicate)))
                 (cl-check-type pred function)
                 (lambda ()
                   (remove-hook 'post-command-hook post t)
                   (when (eq conn-current-state state)
                     (if (or (cl-shiftf prefix-command nil)
                             (not (funcall pred)))
                         (add-hook 'pre-command-hook pre 99 t)
                       (remove-hook 'prefix-command-preserve-state-hook
                                    preserve-state)
                       (conn-enter-state
                        (conn-peek-state)
                        (conn-stack-transition autopop
                          (pop conn--state-stack)
                          (conn-call-re-entry-fns))))))))
    (conn-state-on-exit exit-type
      (pcase exit-type
        ((conn-state-transition enter-recursive))
        ((conn-state-transition push)
         (pop conn--state-stack)))
      (remove-hook 'pre-command-hook pre t))
    (add-hook 'pre-command-hook pre 99 t)
    (cl-call-next-method)))

(conn-define-state conn-one-command-state (conn-command-state
                                           conn-autopop-state)
  "Execute one command in `conn-command-state'."
  :lighter "1C")

(conn-define-state conn-one-emacs-state (conn-emacs-state
                                         conn-autopop-state)
  "Execute one command in `conn-emacs-state'."
  :lighter "1E")

;;;;; Mark State

(defvar-local conn-mark-state-ring nil)

(defvar conn-mark-state-ring-max 8)

(defvar conn--mark-state-rmm nil)

(defvar-local conn-record-mark-state t
  "Record the region in mark state history when mark state exits.

`conn-mark-state' sets this to `t' when entering.  To prevent the region
in the current mark state from being recorded set this to nil after
entering mark state.")

(conn-define-state conn-mark-state (conn-command-state
                                    conn-autopop-state)
  :lighter "M"
  :pop-predicate (lambda ()
                   (or (not (region-active-p))
                       deactivate-mark
                       (progn
                         (setf conn--mark-state-rmm
                               (bound-and-true-p rectangle-mark-mode))
                         nil))))

(defun conn-pop-mark-state ()
  "Pop `conn-mark-state'."
  (interactive)
  (setq deactivate-mark t))

(defun conn-push-mark-state-ring (state)
  (unless conn-mark-state-ring
    (setq conn-mark-state-ring
          (conn-make-ring conn-mark-state-ring-max
                          :cleanup (lambda (elem)
                                     (pcase elem
                                       (`(,pt ,mk ,_rmm)
                                        (set-marker pt nil)
                                        (set-marker mk nil))))
                          :copier (lambda (elem)
                                    (pcase-exhaustive elem
                                      (`(,pt ,mk ,rmm)
                                       (list (copy-marker pt)
                                             (copy-marker mk)
                                             rmm)))))))
  (conn-ring-insert-front conn-mark-state-ring state))

(defun conn-mark-state-keep-mark-active-p (transition)
  "When non-nil keep the mark active when exiting `conn-mark-state'."
  (or (conn-substate-p conn-next-state 'conn-emacs-state)
      (conn-state-subtransition-p 'enter-recursive transition)))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-mark-state))
                                _transition)
  (setf conn--mark-state-rmm (bound-and-true-p rectangle-mark-mode)
        conn-record-mark-state t)
  (conn-state-on-exit exit-type
    (when (bound-and-true-p rectangle-mark-mode)
      (conn-state-on-re-entry _type
        (activate-mark)
        (rectangle-mark-mode 1)))
    (unless (conn-mark-state-keep-mark-active-p exit-type)
      (deactivate-mark)
      (setq conn-record-mark-state nil))
    (unless (or (null conn-record-mark-state)
                (eq this-command 'keyboard-quit)
                (= (point) (mark t)))
      (conn-push-mark-state-ring
       (list (point-marker)
             (copy-marker (mark-marker))
             conn--mark-state-rmm))))
  (cl-call-next-method))

;;;;; Buffer State Setup

(defun conn-setup-state-for-buffer (&optional no-major-mode-maps)
  (when conn--state-stack
    (let (conn-next-state)
      (conn--run-exit-fns (conn-stack-transition exit)))
    (setq conn--state-stack nil))
  (unless no-major-mode-maps
    (setq conn--active-major-mode-maps
          (conn--derived-mode-all-parents major-mode)))
  (or (run-hook-with-args-until-success 'conn-setup-state-hook)
      (conn-push-state 'conn-emacs-state)))

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
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-minibuffer-state -95)

;;;; Read Args

(defvar conn-read-args-last-command nil
  "Last command read by `conn-read-args'.")

(defvar conn-read-args-inhibit-message nil
  "Value for `inhibit-message' in `conn-read-args' message functions.")

(defvar conn-reading-args nil
  "Non-nil during `conn-read-args'.")

(defvar conn-read-args-last-prefix nil)

(defvar conn--read-args-prefix-mag nil)
(defvar conn--read-args-prefix-sign nil)

(defvar conn--read-args-error-message nil)
(defvar conn--read-args-error-flag nil)

(defvar conn--read-args-message nil)
(defvar conn--read-args-message-timeout nil)

(defvar-keymap conn-read-args-map
  "0" 'digit-argument
  "1" 'digit-argument
  "2" 'digit-argument
  "3" 'digit-argument
  "4" 'digit-argument
  "5" 'digit-argument
  "6" 'digit-argument
  "7" 'digit-argument
  "8" 'digit-argument
  "9" 'digit-argument
  "?" 'reference
  "M-?" 'reference
  "C-h" 'help
  "<escape>" 'keyboard-quit
  "C-g" 'keyboard-quit
  "DEL" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(defvar conn-read-args-ref-bindings
  (conn-reference-quote
    ((:keymap conn-read-args-map)
     ("backward delete arg" backward-delete-arg)
     ("reset arg" reset-arg)
     ("completing-read available commands" help)
     (:keymap conn-quick-ref-map)
     ("close quick reference" close)
     ("next/previous page" next previous))))

(defvar conn-read-args-reference-page
  (conn-reference-page
    :depth 50
    (:heading "Read Args")
    "Interactively reading arguments for a command.
"
    (:eval (conn-quick-ref-to-cols
            conn-read-args-ref-bindings 1))))

(defun conn-read-args-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond (conn--read-args-prefix-mag
         (* (if conn--read-args-prefix-sign -1 1)
            conn--read-args-prefix-mag))
        (conn--read-args-prefix-sign -1)))

(defun conn-read-args-consume-prefix-arg ()
  "Return the value of the current prefix argument during `conn-read-args'.

Resets the current prefix argument."
  (prog1 (conn-read-args-prefix-arg)
    (setf conn--read-args-prefix-mag nil
          conn--read-args-prefix-sign nil)))

(defun conn-read-args-handle (&optional new-command)
  "Handle the current command.

This function should be called from any function passed as the
:command-handler argument to `conn-read-args' when the function
chooses to handle a command.

If NEW-COMMAND is non-nil it will replace the current command and
command handlers and argument update functions will be called with the
new command."
  (if new-command
      (throw 'conn-read-args-new-command new-command)
    (throw 'conn-read-args-handle nil)))

(defun conn-read-args-message (format-string &rest args)
  "Print FORMAT-STRING with ARGS during `conn-read-args'.

The duration of the message display is controlled by
`minibuffer-message-timeout'."
  (let ((inhibit-message conn-read-args-inhibit-message)
        (message-log-max nil))
    (setq conn--read-args-message (apply #'format format-string args)
          conn--read-args-message-timeout (time-add nil minibuffer-message-timeout))))

(defun conn-read-args-error (format-string &rest args)
  "Print an error message of FORMAT-STRING with ARGS."
  (setq conn--read-args-error-message (apply #'format format-string args)
        conn--read-args-error-flag t)
  (throw 'conn-read-args-handle nil))

(defun conn--read-args-display-prefix-arg ()
  (let ((msg (concat
              (when conn--read-args-message
                (format "[%s] " conn--read-args-message))
              (propertize conn--read-args-error-message
                          'face 'error))))
    (unless (equal msg "") msg)))

(defun conn-read-args-prompt-line (prompt)
  (substitute-command-keys
   (concat
    "\\<conn-read-args-map>"
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
    "; \\[reference] reference"
    "; \\[help] help"
    ")"
    (when-let* ((msg (conn--read-args-display-prefix-arg)))
      (concat ": " msg)))))

(defun conn--read-args-prompt (prompt arguments)
  (concat
   (conn-read-args-prompt-line prompt)
   (when-let* ((args (flatten-tree
                      (mapcar #'conn-argument-display arguments))))
     (concat "\n" (string-join args "   ")))))

(defun conn--read-args-display-prompt (prompt
                                       arguments
                                       &optional
                                       _state
                                       teardown)
  (if teardown
      (message nil)
    (message "%s" (conn--read-args-prompt prompt arguments))))

(defun conn-read-args-display-columns (column-count separator-width)
  (lambda (prompt arguments &optional _state teardown)
    (if teardown
        (message nil)
      (message
       "%s"
       (let ((to-display
              (flatten-tree
               (cl-loop for arg in arguments
                        collect (conn-argument-display arg))))
             (prompt-line (conn-read-args-prompt-line prompt)))
         (if (length> to-display column-count)
             (with-work-buffer
               (insert prompt-line "\n")
               (conn-to-vtable to-display
                               column-count
                               (current-buffer)
                               :separator-width separator-width
                               :use-header-line nil)
               (buffer-substring (point-min) (1- (point-max))))
           (concat prompt-line "\n"
                   (string-join to-display
                                (make-string separator-width ?\ )))))))))

;; From embark
(defun conn--read-args-bindings (args &optional keymap)
  (let ((result nil))
    (cl-labels ((predicate (item)
                  (cl-loop for arg in args
                           thereis (conn-argument-predicate arg item)))
                (bindings (keymap)
                  (map-keymap
                   (lambda (_key def)
                     (pcase (keymap--menu-item-binding def)
                       ((and (pred keymapp) keymap)
                        (bindings keymap))
                       ((and (pred symbolp)
                             (pred predicate)
                             sym)
                        (push (cons (symbol-name sym) sym) result))
                       (`(,(and desc (pred stringp))
                          . ,(and item (pred predicate)))
                        (push (cons desc item) result))
                       ((and item (pred predicate))
                        (push (cons (propertize
                                     (conn-thing-pretty-print item)
                                     'command item)
                                    item)
                              result))))
                   (keymap-canonicalize keymap))))
      (if keymap
          (bindings keymap)
        (mapc #'bindings (current-active-maps)))
      result)))

(defun conn--read-args-affixation-function (args &optional keymap)
  (lambda (command-names)
    (with-selected-window (or (minibuffer-selected-window) (selected-window))
      (cl-loop
       for command-name in command-names
       collect
       (let* ((fun (or (get-char-property 0 'command command-name)
                       (and (stringp command-name) (intern-soft command-name))))
              (binding (where-is-internal fun keymap t))
              (binding (if (and binding (not (stringp binding)))
                           (format " {%s}" (key-description binding))
                         ""))
              (annotation
               (or (cl-loop for arg in args
                            thereis (conn-argument-completion-annotation arg fun))
                   "")))
         (put-text-property 0 (length binding)
                            'face 'help-key-binding binding)
         (when annotation
           (put-text-property 0 (length annotation)
                              'face 'completions-annotations annotation))
         (list command-name "" (concat annotation binding)))))))

(defun conn--read-args-completing-read (state args &optional keymap)
  (let ((inhibit-message nil))
    (message nil))
  (and-let* ((metadata
              `((affixation-function . ,(conn--read-args-affixation-function
                                         args keymap))
                ,@(conn-state-get state :loop-completion-metadata)))
             (table (conn--read-args-bindings args keymap)))
    (conn--where-is-with-remaps
      (condition-case _
          (alist-get (completing-read
                      "Command: "
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            `(metadata ,@metadata)
                          (complete-with-action action table string pred)))
                      nil t)
                     table nil nil #'equal)
        (quit nil)))))

(defmacro conn-read-args-return (&rest body)
  "Evaluate body and return the result from the current `conn-read-args'.

This skips executing the body of the `conn-read-args' form entirely."
  (declare (indent 0)
           (debug (def-body)))
  `(throw 'conn-read-args-return
          (list (lambda () ,@body))))

(cl-defgeneric conn-read-args-command-handler (cmd)
  (:method (_) "Noop" nil))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql recenter-top-bottom)))
  (let ((this-command 'recenter-top-bottom)
        (last-command conn-read-args-last-command))
    (recenter-top-bottom (conn-read-args-consume-prefix-arg))
    (conn-read-args-handle)))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql digit-argument)))
  (let* ((char (if (integerp last-input-event)
                   last-input-event
                 (get last-input-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setf conn--read-args-prefix-mag
          (if (integerp conn--read-args-prefix-mag)
              (+ (* 10 conn--read-args-prefix-mag) digit)
            digit)))
  (conn-read-args-handle))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql backward-delete-arg)))
  (when conn--read-args-prefix-mag
    (cl-callf floor conn--read-args-prefix-mag 10))
  (conn-read-args-handle))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql reset-arg)))
  (setf conn--read-args-prefix-mag nil)
  (conn-read-args-handle))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql negative-argument)))
  (cl-callf not conn--read-args-prefix-sign)
  (conn-read-args-handle))

(cl-defmethod conn-read-args-command-handler ((_cmd (eql keyboard-quit)))
  (keyboard-quit))

(cl-defun conn--read-args (state
                           arglist
                           callback
                           &key
                           interactive
                           command-handler
                           (display-handler #'conn--read-args-display-prompt)
                           around
                           overriding-map
                           prompt
                           prefix
                           pre
                           post
                           reference)
  (let ((arguments arglist)
        (interactive (and (eq this-command interactive)
                          interactive))
        (prefix (when prefix (prefix-numeric-value prefix)))
        (prompt (or prompt (symbol-name state)))
        (keymap (thread-last
                  (mapcar #'conn-argument-compose-keymap arglist)
                  (cons overriding-map)
                  (delq nil)
                  make-composed-keymap))
        (display-state nil)
        (quit-event (car (last (current-input-mode)))))
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
                 (message-log-max nil)
                 (scroll-conservatively 100))
             (conn-threadf-> display-state
               (funcall display-handler
                        prompt
                        (cons command-handler arguments))))
           (setf conn--read-args-error-message ""))
         (call-handlers (cmd)
           (catch 'conn-read-args-new-command
             (when command-handler
               (funcall command-handler cmd))
             (conn-read-args-command-handler cmd)
             nil))
         (update-args (cmd)
           (catch 'conn-read-args-handle
             (if-let* ((newcmd (call-handlers cmd)))
                 (progn
                   (when post (funcall post cmd))
                   ;; (setq conn-read-args-last-command cmd)
                   (update-args newcmd))
               (let (valid)
                 (cl-loop for as on arguments
                          do (conn-argument-update (car as)
                                                   cmd
                                                   (lambda (newval)
                                                     (setf (car as) newval
                                                           valid t))))
                 (unless valid
                   (setf conn--read-args-error-message
                         (format "Invalid Command <%s>" cmd)))))))
         (read-command ()
           (let (keyseq cmd partial-keymap)
             (conn-with-overriding-map conn-read-args-map
               (setq keyseq (read-key-sequence-vector nil)
                     cmd (key-binding keyseq t))
               (while (arrayp cmd) ; keyboard macro
                 (setq cmd (key-binding cmd t)))
               (when (and (null cmd)
                          (eql help-char (aref keyseq (1- (length keyseq)))))
                 (setq cmd 'execute-extended-command
                       partial-keymap (key-binding (seq-subseq keyseq 0 -1)))))
             (when (eql (aref keyseq 0) quit-event)
               (setq cmd 'keyboard-quit))
             (when cmd
               (when pre (funcall pre cmd))
               (pcase cmd
                 ('keyboard-quit
                  (keyboard-quit))
                 ('reference
                  (apply #'conn-quick-reference
                         reference
                         conn-read-args-reference-page
                         (mapcar #'conn-argument-get-reference
                                 arguments)))
                 ((or 'execute-extended-command 'help)
                  (when-let* ((cmd (conn--read-args-completing-read
                                    state
                                    (if command-handler
                                        `(,command-handler
                                          conn-read-args-command-handler
                                          ,@arguments)
                                      `(conn-read-args-command-handler
                                        ,@arguments))
                                    partial-keymap)))
                    (update-args cmd)))
                 (_ (update-args cmd)))
               (when post (funcall post cmd))
               (setq conn-read-args-last-command cmd))))
         (cont ()
           (conn-with-recursive-stack state
             (let ((conn--read-args-prefix-mag (when prefix (abs prefix)))
                   (conn--read-args-prefix-sign (when prefix (> 0 prefix)))
                   (conn--read-args-error-message "")
                   (conn--read-args-error-flag nil)
                   (conn--read-args-message nil)
                   (conn--read-args-message-timeout nil)
                   (conn-reading-args t)
                   (emulation-mode-map-alists (cons `((,state . ,keymap))
                                                    emulation-mode-map-alists))
                   (inhibit-message t)
                   (minibuffer-message-clear-timeout nil))
               (while (continue-p)
                 (unless executing-kbd-macro
                   (display-message))
                 (read-command))
               (setq conn-read-args-last-prefix (conn-read-args-prefix-arg))))))
      (let* ((local-exit nil)
             (vals nil)
             (ret (apply
                   (catch 'conn-read-args-return
                     (conn--unwind-protect-all
                       (let ((conn-read-args-last-prefix nil))
                         (if around (funcall around #'cont) (cont))
                         (setq vals (mapcar #'conn-argument-extract-value
                                            arguments))
                         (setq local-exit t))
                       (unless local-exit
                         (mapc #'conn-argument-cancel arguments))
                       (unless executing-kbd-macro
                         (funcall display-handler nil nil display-state t)))
                     (mapc #'conn-argument-accept arguments)
                     (cons callback vals)))))
        (when interactive
          (add-to-history 'conn-command-history
                          (cons interactive ret)
                          conn-command-history-max
                          t))
        ret))))

(defmacro conn-read-args (state-and-keys varlist &rest body)
  "Eval BODY with value in VARLIST read in STATE.

VARLIST is a list of the form ((PATTERN ARGUMENT) ...) where PATTERN is
a pattern accepted by `pcase-let'.

The execution of a `conn-read-args' form proceeds as follows:

First each ARGUMENT is evaluated.  Then if an AROUND function has been
specified it is called with a continuation which the AROUND function
should call to continue to the read args loop after doing whatever setup
is desired.

The arg reading loop continues while `conn-argument-required-p' returns
non-nil for at least one argument.

The loop then prompts the user for a command via
`read-key-sequence-vector'.  If a PRE function was given then it is
called with the command that has been read.

Then the default command handler and the COMMAND-HANDLER function, if
provided, are called with the current command.  If the command handler
chooses to handle the command and call `conn-read-args-handle' then
POST is called with the current command, and the current iteration of
the loop ends.

If the current command is not handled by a command handler then
`conn-argument-update' is called for each ARGUMENT with ARGUMENT, the
command, and an updater function.  The updater function is a function of
one argument which when called updates the value of ARGUMENT to the
supplied value.  Once updater has been called `conn-argument-update' is
not called with any more ARGUMENTs, POST is called with the current
command and the current iteration of the loop end.

If no command handler handles the current command and no argument
calls an updater then an invalid command message is printed.

Once the loop ends `conn-argument-extract-value' is called on each
argument and the result is bound to the corresponding pattern form by
`pcase-let' and BODY then runs.

INTERACTIVE if non-nil should be a command and means that
`conn-read-args' is being called to produce a list for the interactive
form that command.  This causes the command, along with the arguments
read, to be added to `conn-command-history' when the command is run.

OVERRIDING-MAP if non-nil should be a keymap which will be active during
the read args loop and take precedence over the ARGUMENT keymaps.  Note
that `conn-read-args-map' will still take precedence over
OVERRIDING-MAP.

REFERENCE if non-nil is a `conn-reference-page's or a list of pages to be
displayed as interactive help.

PREFIX is the initial value of the prefix argument.

DISPLAY-HANLDER if non-nil should be a function which will be called
each iteration with a prompt and a list of all ARGUMENTs and display the
echo area help message.

\(fn (STATE &key COMMAND-HANDLER DISPLAY-HANDLER AROUND OVERRIDING-MAP PROMPT PREFIX PRE POST REFERENCE) VARLIST &rest BODY)"
  (declare (indent 2)
           (debug (([sexp &rest keywordp form])
                   ([&rest sexp form])
                   def-body)))
  (pcase-let (((or `(,state . ,keys) state) state-and-keys))
    `(conn--read-args ',state
                      (list ,@(mapcar #'cadr varlist))
                      (pcase-lambda ,(mapcar #'car varlist) ,@body)
                      ,@keys)))

;;;;; Argument Types

(cl-defstruct (conn-argument
               ( :constructor conn-argument
                 (value &aux (required nil) (set-flag nil))))
  (value nil)
  (set-flag nil :type boolean)
  (required nil :type boolean :read-only t)
  (name nil :type (or string function nil) :read-only t)
  (annotation nil :type (or nil string function) :read-only t)
  (keymap nil :type keymap :read-only t)
  (reference nil :type (or list conn--reference-page)))

(cl-defgeneric conn-argument-cancel (argument)
  ( :method (_arg) nil))

(cl-defgeneric conn-argument-accept (argument)
  ( :method (_arg) nil))

(cl-defgeneric conn-argument-required-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (and (conn-argument-required arg)
         (not (conn-argument-set-flag arg)))))

(cl-defgeneric conn-argument-update (argument form updater)
  ( :method (_arg _form _updater) nil))

(cl-defgeneric conn-argument-extract-value (argument)
  "Extract ARGUMENT's value."
  (declare (important-return-value t))
  ( :method (arg) arg)
  ( :method ((arg conn-argument))
    (conn-argument-value arg)))

(cl-defgeneric conn-argument-display (argument)
  "Display string in `conn-read-args-message' for ARGUMENT.

Return value should be a string or a list of strings, each of which will
be displayed in the echo area during `conn-read-args'."
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (pcase (conn-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       str))))

(cl-defgeneric conn-argument-compose-keymap (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-argument))
    (conn-argument-keymap arg)))

(cl-defgeneric conn-argument-predicate (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg _val) nil))

(cl-defgeneric conn-argument-completion-annotation (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (&rest _) nil)
  ( :method ((arg conn-argument) value)
    (when-let* ((ann (conn-argument-annotation arg))
                (_ (conn-argument-predicate arg value)))
      (pcase ann
        ((and (pred stringp) str)
         (concat " (" str ")"))
        ((and fn (pred functionp)
              (let (and str (pred stringp))
                (funcall fn arg)))
         (concat " (" str ")"))))))

(cl-defgeneric conn-argument-get-reference (arg)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (_arg) nil)
  ( :method ((arg conn-argument))
    (conn-argument-reference arg)))

;;;;;; Read Args Command Handler

(cl-defmethod conn-argument-completion-annotation ((arg (eql conn-read-args-command-handler))
                                                   value)
  (when (conn-argument-predicate arg value)
    " (command)"))

(cl-defmethod conn-argument-predicate ((_arg (eql conn-read-args-command-handler))
                                       cmd)
  (memq cmd '(backward-delete-arg
              reset-arg
              negative-argument
              keyboard-quit
              execute-extended-command
              help)))

;;;;;; Anonymous Argument

(oclosure-define (conn-anonymous-argument
                  ;; (:predicate conn-anonymous-argument-p)
                  (:copier conn-set-argument (value &optional (set-flag t)))
                  (:copier conn-unset-argument ( &optional value
                                                 &aux (set-flag nil))))
  (predicate :type (or nil function))
  (value :type t)
  (set-flag :type boolean)
  (required :type boolean)
  (name :type (or nil string function))
  (annotation :type (or nil string function))
  (keymap :type keymap)
  (reference :type (or list conn--reference-page)))

(defalias 'conn-anonymous-argument-name
  'conn-anonymous-argument--name)

(defalias 'conn-anonymous-argument-required
  'conn-anonymous-argument--required)

(defalias 'conn-anonymous-argument-set-flag
  'conn-anonymous-argument--set-flag)

(defalias 'conn-anonymous-argument-value
  'conn-anonymous-argument--value)

(defalias 'conn-anonymous-argument-keymap
  'conn-anonymous-argument--keymap)

(defalias 'conn-anonymous-argument-reference
  'conn-anonymous-argument--reference)

(cl-defmethod conn-argument-required-p ((arg conn-anonymous-argument))
  (and (conn-anonymous-argument-required arg)
       (not (conn-anonymous-argument-set-flag arg))))

(cl-defmethod conn-argument-update ((arg conn-anonymous-argument)
                                    form
                                    updater)
  (funcall arg arg form updater))

(cl-defmethod conn-argument-extract-value ((arg conn-anonymous-argument))
  (conn-anonymous-argument-value arg))

(cl-defmethod conn-argument-display ((arg conn-anonymous-argument))
  (pcase (conn-anonymous-argument-name arg)
    ((and (pred stringp) str)
     str)
    ((and fn (pred functionp)
          (let (and str (pred stringp))
            (funcall fn arg)))
     str)))

(cl-defmethod conn-argument-compose-keymap ((arg conn-anonymous-argument))
  (conn-anonymous-argument-keymap arg))

(cl-defmethod conn-argument-predicate ((arg conn-anonymous-argument)
                                       cmd)
  (when-let* ((pred (conn-anonymous-argument--predicate arg)))
    (funcall pred cmd)))

(cl-defmethod conn-argument-completion-annotation ((arg conn-anonymous-argument)
                                                   value)
  (when-let* ((ann (conn-anonymous-argument--annotation arg))
              (_ (conn-anonymous-argument--predicate arg value)))
    (pcase ann
      ((and (pred stringp) str)
       (concat " (" str ")"))
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       (concat " (" str ")")))))

(cl-defmethod conn-argument-get-reference ((arg conn-anonymous-argument))
  (conn-anonymous-argument-reference arg))

;;;;;; Composite Argument

(cl-defstruct (conn-composite-argument
               (:include conn-argument)
               (:constructor conn-composite-argument (value))))

(cl-defmethod conn-argument-required-p ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           always (conn-argument-required-p a)))

(cl-defmethod conn-argument-update ((arg conn-composite-argument)
                                    form
                                    updater)
  (cl-loop with done = nil
           for as on (conn-argument-value arg)
           until done
           do (conn-argument-update (car as)
                                    form
                                    (lambda (a)
                                      (setf (car as) a
                                            done t)
                                      (funcall updater arg)))))

(cl-defmethod conn-argument-extract-value ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           collect (conn-argument-extract-value a)))

(cl-defmethod conn-argument-display ((arg conn-composite-argument))
  (cl-loop for a in (conn-composite-argument-value arg)
           collect (conn-argument-display a)))

(cl-defmethod conn-argument-compose-keymap ((arg conn-composite-argument))
  (make-composed-keymap
   (cl-loop for a in (conn-composite-argument-value arg)
            collect (conn-argument-compose-keymap a))))

(cl-defmethod conn-argument-predicate ((arg conn-composite-argument)
                                       cmd)
  (cl-loop for a in (conn-composite-argument-value arg)
           thereis (conn-argument-predicate a cmd)))

(cl-defmethod conn-argument-completion-annotation ((arg conn-composite-argument)
                                                   value)
  (cl-loop for a in (conn-composite-argument-value arg)
           thereis (conn-argument-completion-annotation a value)))

(cl-defmethod conn-argument-get-reference ((arg conn-composite-argument))
  (mapcar #'conn-argument-get-reference
          (conn-composite-argument-value arg)))

;;;;;; Boolean Argument

(cl-defstruct (conn-boolean-argument
               (:include conn-argument)
               ( :constructor conn-boolean-argument
                 (name
                  toggle-command
                  keymap
                  &optional
                  value
                  annotation
                  reference)))
  (toggle-command nil :read-only t))

(cl-defmethod conn-argument-update ((arg conn-boolean-argument)
                                    cmd updater)
  (when (eq cmd (conn-boolean-argument-toggle-command arg))
    (cl-callf not (conn-boolean-argument-value arg))
    (funcall updater arg)))

(cl-defmethod conn-argument-predicate ((arg conn-boolean-argument)
                                       cmd)
  (eq cmd (conn-boolean-argument-toggle-command arg)))

(cl-defmethod conn-argument-display ((arg conn-boolean-argument))
  (concat
   (substitute-command-keys
    (format "\\[%s]" (conn-boolean-argument-toggle-command arg)))
   " "
   (propertize (conn-boolean-argument-name arg)
               'face (when (conn-argument-value arg)
                       'conn-argument-active-face))))

;;;;;; Cycling Argument

(defun conn-format-cycling-argument (choice)
  (format "%s" (or (car-safe choice) choice)))

(cl-defstruct (conn-cycling-argument
               (:include conn-argument)
               ( :constructor conn-cycling-argument
                 (name
                  choices
                  cycling-command
                  &key
                  keymap
                  (formatter #'conn-format-cycling-argument)
                  required
                  annotation
                  reference
                  display-prefix
                  (value (car choices)))))
  (display-prefix nil :type (or nil string))
  (choices nil :type list :read-only t)
  (cycling-command nil :type symbol :read-only t)
  (formatter #'conn-format-cycling-argument
             :type function :read-only t))

(cl-defmethod conn-argument-update ((arg conn-cycling-argument)
                                    cmd
                                    updater)
  (when (eq cmd (conn-cycling-argument-cycling-command arg))
    (pcase (memq (conn-cycling-argument-value arg)
                 (conn-cycling-argument-choices arg))
      (`(,_ ,next . ,_)
       (setf (conn-cycling-argument-value arg) next)
       (funcall updater arg))
      (`(,_ . nil)
       (setf (conn-cycling-argument-value arg)
             (car (conn-cycling-argument-choices arg)))
       (funcall updater arg)))))

(cl-defmethod conn-argument-extract-value ((arg conn-cycling-argument))
  (let ((val (conn-argument-value arg)))
    (or (cdr-safe val) val)))

(cl-defmethod conn-argument-predicate ((arg conn-cycling-argument)
                                       sym)
  (eq sym (conn-cycling-argument-cycling-command arg)))

(cl-defmethod conn-argument-display ((arg conn-cycling-argument))
  (let ((choices (conn-cycling-argument-choices arg))
        (name (conn-cycling-argument-name arg))
        (formatter (conn-cycling-argument-formatter arg))
        (value (if (consp (conn-cycling-argument-value arg))
                   (car (conn-cycling-argument-value arg))
                 (conn-cycling-argument-value arg))))
    (concat
     (substitute-command-keys
      (format "\\[%s] " (conn-cycling-argument-cycling-command arg)))
     (cond
      ((>= (seq-count #'identity choices) 3)
       (if value
           (concat (propertize "(" 'face 'shadow)
                   (propertize (funcall formatter value)
                               'face 'conn-argument-active-face)
                   (propertize (concat "|"
                                       (truncate-string-ellipsis)
                                       ")")
                               'face 'shadow))
         name))
      (value
       (concat
        (conn-cycling-argument-display-prefix arg)
        (propertize "(" 'face 'shadow)
        (let ((cs choices)
              result)
          (cl-loop
           (when-let* ((choice (pop cs))
                       (str (funcall formatter choice)))
             (when (eq choice value)
               (cl-callf propertize str 'face 'conn-argument-active-face))
             (cl-callf concat result str)
             (when (car cs)
               (cl-callf concat result (propertize "|" 'face 'shadow))))
           (unless cs (cl-return result))))
        (propertize ")" 'face 'shadow)))
      (t name)))))

;;;;;; Read Argument

(cl-defstruct (conn-read-argument
               (:include conn-argument)
               ( :constructor conn-read-argument
                 (name
                  toggle-command
                  keymap
                  reader
                  &key
                  formatter
                  value
                  reference
                  annotation)))
  (reader nil :type function :read-only t)
  (formatter nil :type function :read-only t)
  (toggle-command nil :type symbol :read-only t))

(cl-defmethod conn-argument-update ((arg conn-read-argument)
                                    cmd updater)
  (condition-case err
      (when (eq cmd (conn-read-argument-toggle-command arg))
        (setf (conn-argument-value arg)
              (unless (conn-argument-value arg)
                (funcall (conn-read-argument-reader arg)
                         (conn-argument-value arg))))
        (funcall updater arg))
    (quit (conn-read-args-error "Quit"))
    (error (conn-read-args-error (error-message-string err)))))

(cl-defmethod conn-argument-predicate ((arg conn-read-argument)
                                       sym)
  (eq sym (conn-read-argument-toggle-command arg)))

(cl-defmethod conn-argument-display ((arg conn-read-argument))
  (concat
   (substitute-command-keys
    (format "\\[%s] " (conn-read-argument-toggle-command arg)))
   (or (and-let* ((fn (conn-read-argument-formatter arg))
                  (str (funcall fn
                                (conn-read-argument-name arg)
                                (conn-argument-value arg)))
                  (_ (not (string-empty-p str))))
         str)
       (propertize (conn-read-argument-name arg)
                   'face (when (conn-argument-value arg)
                           'conn-argument-active-face)))))

;;;;; Protected Argument

(cl-defstruct (conn-protected-argument
               (:constructor conn--protected-argument (value cleanup)))
  (value nil :type t :read-only t)
  (cleanup #'ignore :type function :read-only t))

(defmacro conn-protect-argument (value &rest cleanup-body)
  (declare (indent 1))
  `(conn--protected-argument ,value (lambda () ,@cleanup-body)))

(cl-defmethod conn-argument-cancel ((arg conn-protected-argument))
  (funcall (conn-protected-argument-cleanup arg)))

(provide 'conn-states)
