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
(eval-when-compile
  (require 'cl-lib))

(defvar conn-local-mode)
(defvar conn-wincontrol-mode)
(defvar conn-wincontrol-one-command-mode)
(declare-function conn-local-mode "conn")
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
        'text-mode)
  "Major modes in which `conn-command-state' should be the base state."
  :group 'conn
  :type '(list symbol))

(defvar conn-setup-state-functions nil
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
  "Previous conn states in buffer.")

(defvar conn--minor-mode-maps-sort-tick 0)

(cl-defstruct (conn-state
               (:constructor nil)
               ( :constructor conn--make-state
                 (name
                  parents
                  &aux
                  (minor-mode-depths (make-hash-table :test 'eq))
                  (minor-mode-sort-tick conn--minor-mode-maps-sort-tick)
                  (minor-mode-maps (list :conn-minor-mode-map-alist))
                  (major-mode-maps (make-hash-table :test 'eq))))
               (:conc-name conn-state--)
               (:copier nil))
  (name nil :type symbol :read-only t)
  (parents nil :type (list-of symbol))
  (children nil :type (list-of symbol))
  (properties nil :type alist)
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
                   (put ,state :conn-state ,val))))))
  (inline-quote
   (get ,state :conn-state)))

(define-inline conn-state-name-p (state)
  "Return non-nil if STATE is a conn-state."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (conn-state-p (get ,state :conn-state))))

(define-inline conn-state-minor-mode-maps-alist (state)
  "Return the minor mode maps alist for STATE."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (conn-state--minor-mode-maps
    (conn--find-state ,state))))

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
    (inline-quote (get ,property :conn-static-property)))

  (defun conn-state-get--cmacro (exp
                                 state
                                 property
                                 &optional
                                 no-inherit
                                 default)
    (if (or (and-let* ((ni (macroexpand-all no-inherit macroexpand-all-environment))
                       (_ (macroexp-const-p ni)))
              (if (consp ni) (cadr ni) ni))
            (and-let* ((prop (macroexpand-all property macroexpand-all-environment))
                       (_ (macroexp-const-p prop))
                       (prop (cond ((symbolp prop) prop)
                                   ((eq 'quote (car-safe prop))
                                    (let ((prop (car-safe (cdr prop))))
                                      (and (symbolp prop) prop))))))
              (conn-property-static-p prop)))
        `(alist-get ,property
                    (conn-state--properties (conn--find-state ,state))
                    ,default)
      exp)))

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
           (gv-setter
            (lambda (val)
              (ignore no-inherit default)
              `(conn-state-set ,state ,property ,val))))
  (if (or no-inherit (conn-property-static-p property))
      (alist-get property
                 (conn-state--properties (conn--find-state state))
                 default)
    (cl-loop for parent in (conn-state-all-parents state)
             for table = (conn-state--properties (conn--find-state parent))
             for prop = (assq property table)
             when prop return (cdr prop)
             finally return default)))

(define-inline conn-state-set (state property value)
  "Set the value of PROPERTY in STATE to VALUE.

Returns VALUE."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (setf (alist-get ,property (conn-state--properties
                                   (conn--find-state ,state)))
             ,value)))))

(define-inline conn-state-unset (state property)
  "Make PROPERTY unset in STATE.

If a property is unset in a state it will inherit the value of that
property from its parents."
  (inline-letevals (property)
    (inline-quote
     (progn
       (cl-assert (not (conn-property-static-p ,property))
                  t "%s is a static property")
       (cl-callf2 assq-delete-all
           ,property
           (conn-state--properties (conn--find-state ,state)))))))

(define-inline conn-state-has-property-p (state property)
  "Return t if STATE has an explicit value set for PROPERTY."
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-quote
   (and (assq ,property (conn-state--properties (conn--find-state ,state)))
        t)))

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
      (cl-check-type state-obj conn-state)
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
  "Return composed state map for STATE."
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
keymap.  Otherwise a keymap will be created if one does not already
exist."
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
  (cl-with-gensyms (handle)
    `(let ((,handle (conn-enter-recursive-stack ,state)))
       (unwind-protect
           ,(macroexp-progn body)
         (conn-exit-recursive-stack ,handle)))))

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

(oclosure-define (conn-transition))

(oclosure-define (conn-stack-pop
                  (:parent conn-transition)))
(oclosure-define (conn-stack-exit-recursive
                  (:parent conn-stack-pop)))

(oclosure-define (conn-stack-push
                  (:parent conn-transition)))
(oclosure-define (conn-stack-enter-recursive
                  (:parent conn-stack-push)))

(oclosure-define (conn-stack-exit
                  (:parent conn-transition)))
(oclosure-define (conn-stack-clone
                  (:parent conn-stack-exit)))

(defmacro conn-stack-transition (name &rest body)
  (declare (indent 1))
  (let ((exp `(oclosure-lambda (,name) () ,@body)))
    (if (ignore-errors
          (memq 'conn-transition
                (oclosure--class-allparents (cl--find-class name))))
        exp
      (macroexp-warn-and-return
       (format "Unknown state transition type %s" name)
       exp))))

(defun conn--state-exit-default (_type _)
  (when conn-current-state
    (set (cl-shiftf conn-current-state nil) nil)))

(defun conn--run-exit-fns (type)
  (let ((fns conn--state-exit-functions))
    (unless (cl-typep type 'conn-stack-clone)
      (setq conn--state-exit-functions (list 'conn--state-exit-default)
            conn--state-exit-functions-ids nil))
    (funcall (car fns) type (cdr fns))))

(defmacro conn-state-on-exit (transition &rest body)
  "Defer evaluation of BODY until the current state is exited.

Note that if a `conn-state-on-exit' form is evaluated multiple times in
one state then BODY will evaluated that many times when the state is
exited.  If you want to ensure that BODY will be evaluated only once
when the current state exits then use `conn-state-on-exit-once'.

BODY may be be run more than once if a buffer is cloned during the
current state.  See also `conn--clone-buffer-setup'.

When BODY is evaluated `conn-next-state' will be bound to the state that
is being entered after the current state has exited or nil if
`conn-local-mode' is being exited or a cloned buffer is being setup.

\(fn [:label LABEL] BODY)"
  (declare (indent 1)
           (debug (def-body)))
  (when (eql ?_ (string-to-char (symbol-name transition)))
    (setq transition (gensym)))
  (cl-with-gensyms (rest)
    (if (eq (car body) :label)
        `(let ((label ,(cadr body)))
           (unless (memq label conn--state-exit-functions-ids)
             (push label conn--state-exit-functions-ids)
             (push (lambda (,transition ,rest)
                     (unwind-protect
                         ,(macroexp-progn (cddr body))
                       (funcall (car ,rest) ,transition (cdr ,rest))))
                   conn--state-exit-functions)))
      `(push (lambda (,transition ,rest)
               (unwind-protect
                   ,(macroexp-progn body)
                 (funcall (car ,rest) ,transition (cdr ,rest))))
             conn--state-exit-functions))))

(defconst conn--state-unwind-functions
  (make-hash-table :test 'eq
                   :weakness 'key))

(defvar conn-state-unwind-clone nil)

(defmacro conn-state-unwind (clone &rest body)
  "Run BODY when the state stack is unwinding past the current point.

If the stack is being unwound because the buffer has been cloned then
CLONE will be non-nil, otherwise CLONE will nil."
  (declare (indent 1))
  (when (eql ?_ (string-to-char (symbol-name clone)))
    (setq clone (gensym)))
  (cl-with-gensyms (rest)
    `(push (lambda (,clone ,rest)
             (unwind-protect
                 ,(macroexp-progn body)
               (when ,rest (funcall (car ,rest) ,clone (cdr ,rest)))))
           (gethash conn--state-stack
                    conn--state-unwind-functions))))

(defun conn--run-state-unwind-functions (clone)
  (let ((fns (gethash conn--state-stack
                      conn--state-unwind-functions)))
    (unless clone
      (remhash conn--state-stack conn--state-unwind-functions))
    (when fns (funcall (car fns) clone (cdr fns)))))

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
  ( :method ((state (conn-substate t)) transition)
    (set state t)
    (funcall transition)
    (conn-update-lighter))
  ( :method (state _transition)
    (error "Attempting to enter unknown state: %s" state)))

(cl-defmethod conn-enter-state :around ((state (conn-substate t))
                                        transition)
  (when (conn-state-get state :abstract)
    (error "Attempting to enter abstract state %s" state))
  (let (conn-previous-state)
    (unwind-protect
        (progn
          (let ((conn-next-state state))
            (conn--run-exit-fns transition))
          (cl-shiftf conn-previous-state conn-current-state state)
          (unless (eq state conn-previous-state)
            (conn--setup-state-properties)
            (conn--setup-state-keymaps)
            (conn--activate-input-method)
            (unless (conn--mode-maps-sorted-p state)
              (conn--sort-mode-maps state)))
          (cl-call-next-method))
      (unless (symbol-value state)
        (conn-local-mode -1)
        (error "State not active after conn-enter-state %s." state)))
    (run-hook-wrapped
     'conn-state-entry-hook
     (lambda (fn)
       (ignore
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
     (conn-stack-transition conn-stack-push
       (push state conn--state-stack)))))

(defun conn--pop-state-1 (&optional clone)
  (conn--run-state-unwind-functions clone)
  (pop conn--state-stack))

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
       (conn-stack-transition conn-stack-pop
         (conn--pop-state-1)))
    (conn-push-state
     (conn-state-get conn-current-state :pop-alternate
                     t conn-pop-alternate-default))))

(defun conn-unwind-stack ()
  (interactive)
  (while (conn-peek-state)
    (conn-pop-state)))

(defun conn-peek-state ()
  "Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (cadr conn--state-stack))

(defun conn-buffer-base-state (&optional buffer)
  "Returns the next state in the state stack."
  (declare (side-effect-free t)
           (important-return-value t))
  (conn-<
    'conn--state-stack
    (buffer-local-value (or buffer (current-buffer)))
    last
    car))

(defun conn--state-stack-handle ()
  (cons (current-buffer) conn--state-stack))

(defun conn-enter-recursive-stack (state)
  "Enter a recursive stack with STATE as the base state."
  (declare (important-return-value t))
  (prog1 (conn--state-stack-handle)
    (conn-enter-state
     state
     (conn-stack-transition conn-stack-enter-recursive
       (push nil conn--state-stack)
       (push state conn--state-stack)))))

(defun conn-exit-recursive-stack (handle)
  "Exit the recursive state stack associated with HANDLE.

HANDLE should be a handle returned by `conn-enter-recursive-stack'."
  (pcase handle
    (`(,buffer . ,stack)
     (with-current-buffer buffer
       (unless (eq conn--state-stack stack)
         (if (cl-loop for cons on conn--state-stack
                      thereis (eq cons stack))
             (conn-enter-state
              (car stack)
              (conn-stack-transition conn-stack-exit-recursive
                (while (not (eq conn--state-stack stack))
                  (conn--pop-state-1))))
           (error "Invalid recursive stack handle")))))
    (_ (error "Invalid recursive stack handle"))))

;;;;; Definitions

(defun conn--define-state (name parents properties)
  (let ((props (cl-loop with kvs = properties
                        for (k v) on kvs by #'cddr
                        collect (cons k v))))
    (if-let* ((state-obj (conn--find-state name)))
        (let ((prev-parents (conn-state--parents state-obj)))
          (remhash name conn--state-all-parents-cache)
          (setf (conn-state--properties state-obj) props
                (conn-state--parents state-obj) parents)
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
      (let ((state-obj (conn--make-state name parents)))
        (setf (conn--find-state name) state-obj
              (conn-state--properties state-obj) props)
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
       (unless (cl-loop for parent in ',parents
                        never (memq ',name (conn-state-all-parents parent)))
         (error "Cycle detected in %s inheritance hierarchy" ',name))
       (conn--define-state
        ',name
        (list ,@(mapcar (lambda (p) `',(or (car-safe p) p)) parents))
        (list ,@(cl-loop for (key value) on properties by #'cddr
                         nconc (pcase key
                                 ((pred symbolp)
                                  `(,(macroexp-quote key) ,value))
                                 (`(quote ,(pred symbolp))
                                  `(,key ,value))
                                 (_ (error "State property name must be a symbol"))))))
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

;;;;; Read Thing State

(defun conn-read-thing-cursor ()
  "Returns the cursor to be used in `conn-read-thing-common-state'."
  `(hbar . ,(floor (default-line-height) 2.5)))

(conn-define-state conn-read-thing-common-state (conn-command-state)
  "Common elements of thing reading states."
  :cursor #'conn-read-thing-cursor
  :suppress-input-method t
  :abstract t)

;;;;; Autopop State

(oclosure-define (conn-stack-autopop
                  (:parent conn-stack-pop)))

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

(cl-defmethod conn-enter-state :around ((state (conn-substate conn-autopop-state))
                                        transition)
  (when (or (eq 'conn-stack-enter-recursive
                (oclosure-type transition))
            (null conn--state-stack))
    (error "%s cannot be the base state" state))
  (cl-check-type (conn-state-get state :pop-predicate) function)
  (cl-call-next-method))

(cl-defmethod conn-enter-state ((state (conn-substate conn-autopop-state))
                                _transition)
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
                        (conn-stack-transition conn-stack-autopop
                          (conn--pop-state-1))))))))
    (add-hook 'pre-command-hook pre 99 t)
    (conn-state-on-exit transition
      (remove-hook 'pre-command-hook pre t)
      (pcase transition
        ((cl-type conn-stack-enter-recursive))
        ((cl-type conn-stack-push)
         (conn--pop-state-1))))
    (cl-call-next-method)))

(conn-define-state conn-one-command-state (conn-command-state
                                           conn-autopop-state)
  "Execute one command in `conn-command-state'."
  :lighter "1C")

(conn-define-state conn-one-emacs-state (conn-emacs-state
                                         conn-autopop-state)
  "Execute one command in `conn-emacs-state'."
  :lighter "1E")

;;;;; Emacs State

(defvar-local conn-emacs-state-ring nil
  "Ring of previous positions where `conn-emacs-state' was exited.")

(defvar conn-emacs-state-preserve-prefix-commands
  '(conn-pop-state
    conn-emacs-state-at-mark
    conn-emacs-state
    conn-one-emacs-state))

(cl-defmethod conn-enter-state ((_state (eql conn-emacs-state))
                                _transition)
  (when (memq this-command conn-emacs-state-preserve-prefix-commands)
    (run-hooks 'prefix-command-preserve-state-hook)
    (prefix-command-update))
  (cl-call-next-method))

(cl-defmethod conn-enter-state ((_state (eql conn-one-emacs-state))
                                _transition)
  (when (memq this-command conn-emacs-state-preserve-prefix-commands)
    (run-hooks 'prefix-command-preserve-state-hook)
    (prefix-command-update))
  (cl-call-next-method))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-emacs-state))
                                _transition)
  (conn-state-on-exit _transition
    (conn-ring-delete (point) conn-emacs-state-ring #'=)
    (let ((pt (conn--create-marker (point) nil t)))
      (conn-ring-insert-front conn-emacs-state-ring pt)))
  (cl-call-next-method))

;;;;;; Record Emacs State

(conn-define-state conn-record-emacs-state (conn-emacs-state)
  :lighter "REC")

(conn-define-state conn-record-emacs-recursive-state (conn-record-emacs-state))

(defvar-local conn-insertion-recording-other-end nil)
(defvar conn-insertion-recording-last-insertion nil)
(defvar-local conn--insertion-recording-overlay nil)
(defvar-local conn--insertion-recording-change-group nil)

(defun conn--update-record-insertion-region (window)
  (when (eq window (selected-window))
    (with-current-buffer (window-buffer window)
      (when (and (overlayp conn--insertion-recording-overlay)
                 conn-insertion-recording-other-end)
        (move-overlay conn--insertion-recording-overlay
                      (min (point) conn-insertion-recording-other-end)
                      (max (point) conn-insertion-recording-other-end))
        (overlay-put conn--insertion-recording-overlay 'window window)))))

(defun conn-insertion-recording-p ()
  (and conn--insertion-recording-overlay t))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-record-emacs-state))
                                _transition)
  (cl-call-next-method)
  (unless (conn-insertion-recording-p)
    (require 'diff-mode)
    (setq conn--insertion-recording-overlay (make-overlay (point) (point))
          conn-insertion-recording-other-end (point-marker))
    (unless conn--insertion-recording-change-group
      (setq conn--insertion-recording-change-group (prepare-change-group)))
    (overlay-put conn--insertion-recording-overlay 'face 'diff-added)
    (overlay-put conn--insertion-recording-overlay 'category 'conn-recording-region)
    (add-hook 'pre-redisplay-functions
              #'conn--update-record-insertion-region
              nil 'local)
    (conn-state-unwind clone
      (remove-hook 'pre-redisplay-functions
                   #'conn--update-record-insertion-region
                   'local)
      (when conn--insertion-recording-change-group
        (unless clone
          (accept-change-group conn--insertion-recording-change-group)
          (undo-amalgamate-change-group conn--insertion-recording-change-group))
        (setq conn--insertion-recording-change-group nil))
      (when conn-insertion-recording-other-end
        (unless clone
          (setq conn-insertion-recording-last-insertion
                (conn-insertion-recording-text)))
        (setq conn-insertion-recording-other-end nil))
      (when (overlayp conn--insertion-recording-overlay)
        (if clone
            (without-restriction
              (mapc #'delete-overlay
                    (conn--overlays-in-of-type (point-min) (point-max)
                                               'conn-recording-region)))
          (delete-overlay conn--insertion-recording-overlay))
        (setq conn--insertion-recording-overlay nil)))))

(defun conn-insertion-recording-text ()
  (when (markerp conn-insertion-recording-other-end)
    (filter-buffer-substring
     (min (point) conn-insertion-recording-other-end)
     (max (point) conn-insertion-recording-other-end))))

(defvar-keymap conn-record-insertion-transient-map)
(defvar-keymap conn-record-insertion-recursive-transient-map)

(defun conn-record-insertion (&optional recursive-edit change-group)
  (require 'diff-mode)
  (when (conn-insertion-recording-p)
    (error "Already recording"))
  (when change-group
    (setq conn--insertion-recording-change-group change-group))
  (if (not recursive-edit)
      (progn
        (set-transient-map conn-record-insertion-transient-map
                           nil nil
                           (substitute-command-keys
                            (concat
                             (propertize
                              "Change Transient Map: "
                              'face 'minibuffer-prompt)
                             "\\<conn-record-insertion-transient-map>"
                             "\\[conn-insertion-abort-recording] abort, "
                             "\\[conn-insertion-insert-previous] insert previous")))
        (conn-push-state 'conn-record-emacs-state))
    (set-transient-map conn-record-insertion-recursive-transient-map
                       nil nil
                       (substitute-command-keys
                        (concat
                         (propertize
                          "Change Transient Map: "
                          'face 'minibuffer-prompt)
                         "\\<conn-record-insertion-recursive-transient-map>"
                         "\\[abort-recursive-edit] abort, "
                         "\\[conn-insertion-insert-previous] insert previous")))
    (conn-with-recursive-stack 'conn-record-emacs-recursive-state
      (atomic-change-group
        (save-current-buffer
          (recursive-edit))))
    conn-insertion-recording-last-insertion))

(defun conn-record-one-insertion ()
  (conn-with-recursive-stack 'conn-emacs-state
    (let ((char (char-to-string (read-char "Char:" t))))
      (insert char)
      char)))

(defun conn-emacs-state-record-insert (&optional with)
  (interactive)
  (if with (insert with)
    (conn-record-insertion)
    (conn-state-unwind clone
      (when (and (not clone)
                 conn-insertion-recording-other-end)
        (conn-push-command-history
         'conn-emacs-state-record-insert
         (conn-insertion-recording-text))))))

;;;;; Mark State

(defvar-local conn-mark-state-ring nil)

(defvar conn-mark-state-ring-max 8)

(defvar conn--mark-state-rmm nil)

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
      (eq (oclosure-type transition) 'conn-stack-enter-recursive)))

(cl-defmethod conn-enter-state ((_state (conn-substate conn-mark-state))
                                _transition)
  (cl-call-next-method)
  (unless (region-active-p)
    (activate-mark))
  (setf conn--mark-state-rmm (bound-and-true-p rectangle-mark-mode))
  (conn-state-unwind clone
    (when (and (not clone)
               (use-region-p))
      (conn-push-mark-state-ring
       (list (point-marker)
             (copy-marker (mark-marker))
             conn--mark-state-rmm)))
    (deactivate-mark)))

;;;;; Buffer State Setup

(defun conn-setup-state-for-buffer (&optional no-major-mode-maps)
  (when conn--state-stack
    (let (conn-next-state)
      (conn--run-exit-fns (conn-stack-transition conn-stack-exit)))
    (setq conn--state-stack nil))
  (unless no-major-mode-maps
    (setq conn--active-major-mode-maps
          (conn--derived-mode-all-parents major-mode)))
  (or (run-hook-with-args-until-success 'conn-setup-state-functions)
      (conn-push-state 'conn-emacs-state)))

(defun conn-setup-commit-state ()
  "Set the base state to `conn-emacs-state' in commit message buffers."
  (when (buffer-match-p "COMMIT_EDITMSG" (current-buffer))
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-functions 'conn-setup-commit-state)

(defun conn-setup-edmacro-state ()
  "Set the base state to `conn-command-state' in edit macro buffers."
  (when (buffer-match-p "\\*Edit Macro\\*" (current-buffer))
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-functions 'conn-setup-edmacro-state)

(defun conn-setup-null-state ()
  "Set the base state to `conn-null-state' in `conn-null-state-modes' buffers."
  (when (derived-mode-p conn-null-state-modes)
    (conn-push-state 'conn-null-state)
    t))
(add-hook 'conn-setup-state-functions 'conn-setup-null-state -90)

(defun conn-setup-command-state ()
  "Set base state to `conn-command-state' in `conn-command-state-modes' buffers."
  (when (derived-mode-p conn-command-state-modes)
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-functions 'conn-setup-command-state 50)

(defun conn-setup-minibuffer-state ()
  "Setup `minibuffer-mode' buffer state."
  (when (eq major-mode 'minibuffer-mode)
    (conn-push-state 'conn-emacs-state)
    t))
(add-hook 'conn-setup-state-functions 'conn-setup-minibuffer-state -80)

(provide 'conn-states)
