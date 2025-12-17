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
(declare-function conn-local-mode "conn")

(declare-function conn-thing-pretty-print "conn-things")
(declare-function face-remap-remove-relative "face-remap")

;;;; States

(defcustom conn-null-state-modes
  (list 'calc-mode
        'calc-trail-mode
        'calc-keypad-mode
        'image-mode
        'doc-view-mode)
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
  (cl-assert (keymapp map))
  (cl-check-type (conn--find-state state) conn-state)
  (cl-check-type mode symbol)
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
  (cl-assert (keymapp map))
  (cl-check-type (conn--find-state state) conn-state)
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

(defvar-local conn--without-input-method-hooks nil)

(defmacro conn-without-input-method-hooks (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (remove buffer)
    `(let ((,remove (not conn--without-input-method-hooks))
           (,buffer (current-buffer)))
       (unwind-protect
           (progn
             (when ,remove
               (setq conn--without-input-method-hooks t)
               (remove-hook 'input-method-activate-hook
                            #'conn--activate-input-method
                            t)
               (remove-hook 'input-method-deactivate-hook
                            #'conn--deactivate-input-method
                            t))
             ,@body)
         (when ,remove
           (with-current-buffer ,buffer
             (setq conn--without-input-method-hooks nil)
             (add-hook 'input-method-activate-hook
                       #'conn--activate-input-method
                       50 t)
             (add-hook 'input-method-deactivate-hook
                       #'conn--deactivate-input-method
                       50 t)
             (conn--activate-input-method)))))))

(defmacro conn-with-input-method (&rest body)
  "Run BODY ensuring `conn--input-method' is active."
  (declare (debug (body))
           (indent 0))
  `(conn-without-input-method-hooks
     (activate-input-method conn--input-method)
     ,@body))

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
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (unwind-protect
           (progn
             (conn-enter-recursive-stack ,state)
             ,@body)
         (with-current-buffer ,buffer
           (setq conn--state-stack ,stack)
           (conn-enter-state (car ,stack))
           (conn-update-lighter))))))

(defmacro conn-without-recursive-stack (&rest body)
  "Call TRANSITION-FN and run BODY preserving state variables."
  (declare (debug (body))
           (indent 0))
  (cl-with-gensyms (stack buffer)
    `(let ((,stack conn--state-stack)
           (,buffer (current-buffer)))
       (unwind-protect
           (progn
             (conn-exit-recursive-stack)
             ,@body)
         (with-current-buffer ,buffer
           (setq conn--state-stack ,stack)
           (conn-enter-state (car ,stack))
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

(defvar conn-state-entry-functions nil
  "Hook run when a state is entered.

When this hook is run `conn-previous-state' will be bound to the state
that has just been exited.")

(defvar-local conn--state-deferred (list 'conn--state-deferred-default)
  "Code to be run when the current state is exited.")

(defvar-local conn--state-deferred-ids nil)

(defun conn--state-deferred-default (_)
  (when conn-current-state
    (set (cl-shiftf conn-current-state nil) nil)))

(defun conn--run-deferred ()
  (let ((deferred conn--state-deferred))
    (setq conn--state-deferred (list 'conn--state-deferred-default)
          conn--state-deferred-ids nil)
    (funcall (car deferred) (cdr deferred))))

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
           conn--state-deferred)))

(defmacro conn-state-defer-once (&rest body)
  "Like `conn-state-defer' but BODY will be evaluated only once per state.

For more information see `conn-state-defer'."
  (declare (indent 0))
  (cl-with-gensyms (rest id)
    `(unless (memq ',id conn--state-deferred-ids)
       (push ',id conn--state-deferred-ids)
       (push (lambda (,rest)
               (unwind-protect
                   ,(macroexp-progn body)
                 (funcall (car ,rest) (cdr ,rest))))
             conn--state-deferred))))

(defvar conn-state-lighter-separator "→"
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
  (setf conn--active-major-mode-maps maps
        conn--major-mode-map `((conn-local-mode
                                . ,(make-composed-keymap
                                    (mapcar (lambda (mode)
                                              (conn--compose-major-mode-map
                                               conn-current-state
                                               mode))
                                            conn--active-major-mode-maps))))))

(defun conn-get-major-mode-maps ()
  (declare (gv-setter conn-set-major-mode-maps))
  conn--active-major-mode-maps)

(defun conn--setup-state-keymaps ()
  (if (conn-state-get conn-current-state :no-keymap)
      (setf conn--state-map nil
            conn--major-mode-map nil
            conn--minor-mode-maps nil)
    (setf conn--state-map `((conn-local-mode . ,(conn--compose-state-map)))
          conn--active-major-mode-maps (conn--derived-mode-all-parents major-mode)
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
  (setf conn--disable-mark-cursor
        (or (when-let* ((hide (conn-get-buffer-property
                               :disable-mark-cursor)))
              (if (eq hide t) t
                (alist-get conn-current-state hide)))
            (when-let* ((hide (conn-get-mode-property
                               major-mode :disable-mark-cursor)))
              (if (eq hide t) t
                (alist-get conn-current-state hide)))
            (conn-state-get conn-current-state :disable-mark-cursor)))
  (setf cursor-type
        (let ((c (conn-state-get conn-current-state :cursor nil t)))
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
              (conn--run-deferred))
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
    (push state conn--state-stack)
    (conn-enter-state state)))

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
  (push nil conn--state-stack)
  (push state conn--state-stack)
  (conn-enter-state state)
  ;; Ensure the lighter gets updates even if we haven't changed state
  (conn-update-lighter))

(defun conn-exit-recursive-stack ()
  "Exit the current recursive state stack.

If there is not recursive stack an error is signaled."
  (interactive)
  (if-let* ((tail (memq nil conn--state-stack)))
      (progn
        (setq conn--state-stack (cdr tail))
        (conn-enter-state (cadr tail))
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
                (cl-callf seq-union
                    (gethash :no-inherit-keymaps table)
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
    (conn-ring-delete (point) conn-emacs-state-ring #'=)
    (let ((pt (conn--create-marker (point) nil t)))
      (conn-ring-insert-front conn-emacs-state-ring pt)
      (when conn-emacs-state-register
        (if-let* ((marker (get-register conn-emacs-state-register))
                  ((markerp marker)))
            (set-marker marker (point) (current-buffer))
          (set-register conn-emacs-state-register (copy-marker pt))))))
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
                       (remove-hook 'prefix-command-preserve-state-hook preserve-state)
                       (conn-enter-state (conn-peek-state)))))))
    (conn-state-defer
      (setq conn--state-stack
            (append (remq state (seq-take-while #'identity conn--state-stack))
                    (seq-drop-while #'identity conn--state-stack)))
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

(defun conn-pop-mark-state ()
  (interactive)
  (setq deactivate-mark t))

(defun conn-mark-state-keep-mark-active-p ()
  (or (conn-substate-p conn-next-state 'conn-emacs-state)
      (and (null (nth 1 conn--state-stack))
           (eq (nth 2 conn--state-stack)
               conn-current-state))))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-mark-state)))
  (setf conn--mark-state-rmm (and (bound-and-true-p rectangle-mark-mode)
                                  (fboundp 'rectangle--pos-cols)
                                  (rectangle--pos-cols (point) (mark)))
        conn-record-mark-state t)
  (conn-state-defer
    (unless (or (null conn-record-mark-state)
                (eq this-command 'keyboard-quit))
      (unless conn-previous-mark-state
        (setq conn-previous-mark-state (list (make-marker) (make-marker) nil)))
      (set-marker (nth 0 conn-previous-mark-state) (point))
      (set-marker (nth 1 conn-previous-mark-state) (mark t))
      (setf (nth 2 conn-previous-mark-state) conn--mark-state-rmm))
    (unless (conn-mark-state-keep-mark-active-p)
      (deactivate-mark)))
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
  (let ((fn (make-symbol "hook")))
    (fset fn (lambda ()
               (conn--push-ephemeral-mark)
               (remove-hook 'minibuffer-setup-hook fn)))
    (when (eq major-mode 'minibuffer-mode)
      (setf (alist-get 'conn-emacs-state
                       (conn-get-buffer-property :disable-mark-cursor))
            t)
      (conn-push-state 'conn-emacs-state)
      (add-hook 'minibuffer-setup-hook fn)
      t)))
(add-hook 'conn-setup-state-hook 'conn-setup-minibuffer-state -95)

;;;; Read Args

(defvar conn-read-args-last-command nil
  "Last command read by `conn-read-args'.")

(defvar conn-read-args-inhibit-message nil
  "Value for `inhibit-message' in `conn-read-args' message functions.")

(defvar conn-reading-args nil)

(defvar conn--read-args-prefix-mag nil)
(defvar conn--read-args-prefix-sign nil)

(defvar conn--read-args-error-message nil)
(defvar conn--read-args-error-flag nil)

(defvar conn--read-args-message nil)
(defvar conn--read-args-message-timeout nil)

(defvar conn--read-args-exiting nil)

(defvar-keymap conn-read-args-map
  "C-q" 'reference
  "C-h" 'help
  "<escape>" 'keyboard-quit
  "C-g" 'keyboard-quit
  "DEL" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

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

(defun conn-read-args-handle (&optional new-command)
  "Handle the current command.

This function should be called from any function passed as the
:command-handler argument to `conn-read-args' when the function
chooses to handle a command."
  (if new-command
      (throw 'conn-read-args-new-command new-command)
    (throw 'conn-read-args-handle nil)))

(defun conn-read-args-message (format-string &rest args)
  (let ((inhibit-message conn-read-args-inhibit-message)
        (message-log-max nil))
    (setq conn--read-args-message (apply #'format format-string args)
          conn--read-args-message-timeout (time-add nil minibuffer-message-timeout))))

(defun conn-read-args-error (format-string &rest args)
  (setq conn--read-args-error-message (apply #'format format-string args)
        conn--read-args-error-flag t)
  (throw 'conn-read-args-handle nil))

(defun conn--read-args-display-message ()
  (let ((msg (concat
              (when conn--read-args-message
                (format "[%s] " conn--read-args-message))
              (propertize conn--read-args-error-message
                          'face 'error))))
    (unless (equal msg "") msg)))

(defun conn--read-args-prompt (prompt arguments)
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
    ", \\[reset-arg] reset)"
    (when-let* ((msg (conn--read-args-display-message)))
      (concat ": " msg))
    (when-let* ((args (flatten-tree
                       (mapcar #'conn-argument-display arguments))))
      (concat "\n" (string-join args "; "))))))

(defun conn--read-args-display (prompt arguments)
  (message (conn--read-args-prompt prompt arguments)))

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
  (declare (indent 0))
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
                           command-handler
                           (display-handler #'conn--read-args-display)
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
                 (message-log-max nil)
                 (scroll-conservatively 100))
             (funcall display-handler prompt arguments))
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
           (let* ((keyseq (read-key-sequence nil))
                  (cmd (key-binding keyseq t))
                  keymap)
             (while (arrayp cmd) ; keyboard macro
               (setq cmd (key-binding cmd t)))
             (when (and (null cmd)
                        (eql help-char (aref keyseq (1- (length keyseq)))))
               (setq cmd 'execute-extended-command
                     keymap (key-binding (seq-subseq keyseq 0 -1))))
             (when cmd
               (when pre (funcall pre cmd))
               (pcase cmd
                 ('reference
                  (when reference
                    (conn-quick-reference reference)))
                 ((or 'execute-extended-command
                      'help)
                  (when-let* ((cmd (conn--read-args-completing-read
                                    state
                                    (if command-handler
                                        `(,command-handler
                                          conn-read-args-command-handler
                                          ,@arguments)
                                      `(conn-read-args-command-handler
                                        ,@arguments))
                                    keymap)))
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
                   (conn--read-args-exiting nil)
                   (conn-reading-args t)
                   (inhibit-message t)
                   (emulation-mode-map-alists
                    `(((,state . ,(thread-last
                                    (mapcar #'conn-argument-compose-keymap arguments)
                                    (cons conn-read-args-map)
                                    (cons overriding-map)
                                    (cons (define-keymap
                                            "C-h" 'execute-extended-command))
                                    (delq nil)
                                    make-composed-keymap)))
                      ,@emulation-mode-map-alists)))
               (while (continue-p)
                 (unless executing-kbd-macro
                   (display-message))
                 (read-command))))
           (setq local-exit t)))
      (apply
       (catch 'conn-read-args-return
         (unwind-protect
             (if around (funcall around #'cont) (cont))
           (unless local-exit
             (mapc #'conn-argument-cancel arguments))
           (unless executing-kbd-macro
             (message nil)))
         (cons callback (mapcar #'conn-argument-extract-value arguments)))))))

(defmacro conn-read-args (state-and-keys varlist &rest body)
  "Eval BODY with value in VARLIST read in STATE.

VARLIST bindings should be patterns accepted by `pcase-let'.'

\(fn (STATE KEYS) &rest BODY)"
  (declare (indent 2))
  (pcase-let (((or `(,state . ,keys) state) state-and-keys))
    `(conn--read-args ',state
                      (list ,@(mapcar #'cadr varlist))
                      (pcase-lambda ,(mapcar #'car varlist) ,@body)
                      ,@keys)))

;;;;; Loop Arguments

(cl-defstruct (conn-argument
               ( :constructor conn-argument
                 (value &aux (required nil) (set-flag nil))))
  (value nil)
  (set-flag nil :type boolean)
  (required nil :type boolean :read-only t)
  (name nil :type (or string function nil) :read-only t)
  (annotation nil :type (or nil string function) :read-only t)
  (keymap nil :type keymap :read-only t))

(cl-defstruct (conn-composite-argument
               (:include conn-argument)
               (:constructor conn-composite-argument (value))))

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
  (keymap :type keymap))

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

(cl-defgeneric conn-argument-cancel (argument)
  ( :method (_arg) nil))

(cl-defgeneric conn-argument-required-p (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-anonymous-argument))
    (and (conn-anonymous-argument-required arg)
         (not (conn-anonymous-argument-set-flag arg))))
  ( :method ((arg conn-argument))
    (and (conn-argument-required arg)
         (not (conn-argument-set-flag arg))))
  ( :method ((arg conn-composite-argument))
    (cl-loop for a in (conn-composite-argument-value arg)
             always (conn-argument-required-p a))))

(cl-defgeneric conn-argument-update (argument form update-fn)
  ( :method (_arg _form _update-fn) nil)
  ( :method ((arg conn-anonymous-argument) form update-fn)
    (funcall arg arg form update-fn)))

(cl-defgeneric conn-argument-extract-value (argument)
  (declare (important-return-value t))
  ( :method (arg) arg)
  ( :method ((arg conn-anonymous-argument))
    (conn-anonymous-argument-value arg))
  ( :method ((arg conn-argument))
    (conn-argument-value arg))
  ( :method ((arg conn-composite-argument))
    (cl-loop for a in (conn-composite-argument-value arg)
             collect (conn-argument-extract-value a))))

(cl-defgeneric conn-argument-display (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-anonymous-argument))
    (pcase (conn-anonymous-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       str)))
  ( :method ((arg conn-argument))
    (pcase (conn-argument-name arg)
      ((and (pred stringp) str)
       str)
      ((and fn (pred functionp)
            (let (and str (pred stringp))
              (funcall fn arg)))
       str)))
  ( :method ((arg conn-composite-argument))
    (cl-loop for a in (conn-composite-argument-value arg)
             collect (conn-argument-display a))))

(cl-defgeneric conn-argument-predicate (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg _val) nil)
  ( :method ((arg conn-anonymous-argument) cmd)
    (when-let* ((pred (conn-anonymous-argument--predicate arg)))
      (funcall pred cmd)))
  ( :method ((arg conn-composite-argument))
    (cl-loop for a in (conn-composite-argument-value arg)
             thereis (conn-argument-predicate a))))

(cl-defmethod conn-argument-predicate ((_arg (eql conn-read-args-command-handler))
                                       cmd)
  (memq cmd '(backward-delete-arg
              reset-arg
              negative-argument
              keyboard-quit
              execute-extended-command
              help)))

(cl-defgeneric conn-argument-completion-annotation (argument value)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (&rest _) nil)
  ( :method ((arg (eql conn-read-args-command-handler))
             value)
    (when (conn-argument-predicate arg value)
      " (command)"))
  ( :method ((arg conn-anonymous-argument) value)
    (when-let* ((ann (conn-anonymous-argument--annotation arg))
                (_ (conn-anonymous-argument--predicate arg value)))
      (pcase ann
        ((and (pred stringp) str)
         (concat " (" str ")"))
        ((and fn (pred functionp)
              (let (and str (pred stringp))
                (funcall fn arg)))
         (concat " (" str ")")))))
  ( :method ((arg conn-argument) value)
    (when-let* ((ann (conn-argument-annotation arg))
                (_ (conn-argument-predicate arg value)))
      (pcase ann
        ((and (pred stringp) str)
         (concat " (" str ")"))
        ((and fn (pred functionp)
              (let (and str (pred stringp))
                (funcall fn arg)))
         (concat " (" str ")")))))
  ( :method ((arg conn-composite-argument))
    (cl-loop for a in (conn-composite-argument-value arg)
             thereis (conn-argument-completion-annotation a))))

(cl-defgeneric conn-argument-compose-keymap (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg conn-anonymous-argument))
    (conn-anonymous-argument-keymap arg))
  ( :method ((arg conn-argument))
    (conn-argument-keymap arg))
  ( :method ((arg conn-composite-argument))
    (make-composed-keymap
     (cl-loop for a in (conn-composite-argument-value arg)
              collect (conn-argument-compose-keymap a)))))

;;;;; Boolean Argument

(defface conn-argument-active-face
  '((t (:inherit eldoc-highlight-function-argument)))
  "Face for active arguments"
  :group 'conn-faces)

(cl-defstruct (conn-boolean-argument
               (:include conn-argument)
               (:constructor conn-boolean-argument
                             (toggle-command
                              keymap
                              name
                              &optional
                              value
                              annotation)))
  (toggle-command nil :read-only t))

(cl-defmethod conn-argument-update ((arg conn-boolean-argument)
                                    cmd update-fn)
  (when (eq cmd (conn-boolean-argument-toggle-command arg))
    (cl-callf not (conn-boolean-argument-value arg))
    (funcall update-fn arg)))

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
  (format "%s" choice))

(cl-defstruct (conn-cycling-argument
               (:include conn-argument)
               ( :constructor conn-cycling-argument
                 (choices
                  cycling-command
                  &key
                  keymap
                  (format-function #'conn-format-cycling-argument)
                  required
                  name
                  annotation
                  &aux
                  (value (car choices)))))
  (choices nil :type list :read-only t)
  (cycling-command nil :type symbol :read-only t)
  (format-function #'identity :type function :read-only t))

(cl-defmethod conn-argument-update ((arg conn-cycling-argument)
                                    _cmd update-fn)
  (pcase (memq (conn-cycling-argument-value arg)
               (conn-cycling-argument-choices arg))
    (`(,_ ,next . ,_)
     (setf (conn-cycling-argument-value arg) next)
     (funcall update-fn arg))
    (`(,_ . nil)
     (setf (conn-cycling-argument-value arg)
           (car (conn-cycling-argument-choices arg)))
     (funcall update-fn arg))))

(cl-defmethod conn-argument-predicate ((arg conn-cycling-argument)
                                       sym)
  (eq sym (conn-cycling-argument-cycling-command arg)))

(cl-defmethod conn-argument-display ((arg conn-cycling-argument))
  (concat
   (format "\\[%s] " (conn-cycling-argument-cycling-command arg))
   (propertize "(" 'face 'shadow)
   (let ((choices (conn-cycling-argument-choices arg))
         (format (conn-cycling-argument-format-function arg))
         result)
     (cl-loop
      (let* ((choice (pop choices))
             (str (funcall format choice)))
        (when (eq choice (conn-cycling-argument-value arg))
          (cl-callf propertize str 'face 'conn-argument-active-face))
        (cl-callf concat result str)
        (if choices
            (cl-callf concat result (propertize "|" 'face 'shadow))
          (cl-return)))))
   (propertize ")" 'face 'shadow)))

;;;;;; Read Argument

(cl-defstruct (conn-read-argument
               (:include conn-argument)
               (:constructor conn-read-argument
                             (name
                              toggle-command
                              keymap
                              reader-function
                              format-function
                              &optional
                              value
                              annotation)))
  (reader-function nil :type function :read-only t)
  (format-function nil :type function :read-only t)
  (toggle-command nil :type symbol :read-only t))

(cl-defmethod conn-argument-update ((arg conn-read-argument)
                                    cmd update-fn)
  (when (eq cmd (conn-read-argument-toggle-command arg))
    (setf (conn-argument-value arg)
          (funcall (conn-read-argument-reader-function arg)
                   (conn-argument-value arg)))
    (funcall update-fn arg)))

(cl-defmethod conn-argument-predicate ((arg conn-read-argument)
                                       sym)
  (eq sym (conn-read-argument-toggle-command arg)))

(cl-defmethod conn-argument-display ((arg conn-read-argument))
  (concat
   (format "\\[%s] " (conn-read-argument-toggle-command arg))
   (conn-read-argument-name arg)
   (when-let* ((v (funcall (conn-read-argument-format-function arg)
                           (conn-argument-value arg)))
               (_ (not (string-empty-p v))))
     (concat ": " v))))

(provide 'conn-states)
