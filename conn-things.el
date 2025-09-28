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

;;;; Thing Types

(cl-deftype conn-thing-function () '(satisfies conn-command-thing))

(cl-deftype conn-thing () '(satisfies conn-thing-p))

(define-inline conn-command-thing (cmd)
  (declare (side-effect-free t)
           (gv-setter conn-set-command-thing))
  (inline-letevals (cmd)
    (inline-quote
     (and (symbolp ,cmd)
          (get ,cmd :conn-command-thing)))))

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
  (with-memoization (gethash thing conn--thing-all-parents-cache)
    (cons thing (merge-ordered-lists
                 (mapcar #'conn-thing-all-parents
                         (get thing :conn--thing-parents))))))

(defun conn-anonymous-thing-all-parents (thing)
  (declare (important-return-value t))
  (conn-thing-all-parents (conn-anonymous-thing-parent thing)))

(cl-defun conn-register-thing (thing &key parents forward-op beg-op end-op bounds-op)
  (put thing :conn-thing t)
  (when parents
    (put thing :conn--thing-parents parents))
  (when forward-op
    (put thing 'forward-op forward-op))
  (when (or beg-op end-op)
    (cl-assert (and beg-op end-op)
               nil "If either beg-op or end-op is specified then both must be")
    (put thing 'beginning-op beg-op)
    (put thing 'end-op end-op))
  (when bounds-op
    (put thing 'bounds-of-thing-at-point bounds-op)))

(cl-defstruct (conn-anonymous-thing
               (:constructor nil)
               (:constructor conn--make-anonymous-thing)
               ;; This would be nice but cl-defsubst does not handle
               ;; &rest arguments properly and as a result PROPERTIES
               ;; gets evaluated twice in the expansion.
               ;; (:constructor conn-anonymous-thing (parent &rest properties))
               )
  (parent nil :read-only t)
  (properties nil :read-only t))

(defun conn-anonymous-thing (parent &rest properties)
  "Make an anonymous thing."
  (declare (important-return-value t)
           (compiler-macro
            (lambda (_exp)
              `(conn--make-anonymous-thing
                :parent ,parent
                :properties (list ,@properties)))))
  (conn--make-anonymous-thing
   :parent parent
   :properties properties))

(define-inline conn-get-thing (thing)
  (declare (side-effect-free t)
           (important-return-value t))
  (inline-letevals (thing)
    (inline-quote
     (or (when (conn-anonymous-thing-p ,thing)
           (conn-anonymous-thing-parent ,thing))
         (conn-command-thing ,thing)
         ,thing))))

;; from cl--generic-make-defmethod-docstring/pcase--make-docstring
(defun conn--make-anonymous-thing-docstring ()
  (let* ((main (documentation (symbol-function 'conn-anonymous-thing) 'raw))
         (ud (help-split-fundoc main 'conn-anonymous-thing)))
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main ""))
      (insert "\n\n\tCurrently supported properties for anonymous things:\n\n")
      (pcase-dolist (`(,prop . ,fn) (get 'conn-anonymous-thing :known-properties))
        (if (stringp fn)
            (insert (format "%s - %s" prop fn))
          (insert (format "%s - `%s' method for this thing" prop fn)))
        (insert "\n\n"))
      (let ((combined-doc (buffer-string)))
        (if ud
            (help-add-fundoc-usage combined-doc (car ud))
          combined-doc)))))

(put 'conn-anonymous-thing 'function-documentation
     '(conn--make-anonymous-thing-docstring))

(eval-and-compile
  (defun conn--set-anonymous-thing-property (f _args property &optional description)
    `(setf (alist-get ,property (get 'conn-anonymous-thing :known-properties))
           (or ,description ',f)))
  (setf (alist-get 'conn-anonymous-thing-property defun-declarations-alist)
        (list #'conn--set-anonymous-thing-property)))

(define-inline conn-anonymous-thing-property (object property)
  (declare (side-effect-free t))
  (inline-quote
   (plist-get (conn-anonymous-thing-properties ,object) ,property)))

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
          (if ,(unless ignore-mark-active '(region-active-p))
              (pcase (car (read-multiple-choice
                           "Mark"
                           '((?a "after point")
                             (?b "before point"))))
                (?e (goto-char end))
                (?b (goto-char beg)))
            (goto-char beg)
            (conn--push-ephemeral-mark end)))))
     (conn-register-thing-commands ',thing 'ignore ',name)))

;;;; Specializers

(defconst conn--thing-cmd-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--thing-generalizer
  70 (lambda (thing &rest _)
       `(and (conn-thing-p ,thing)
             (conn-thing-all-parents ,thing)))
  (lambda (things &rest _)
    (when things
      `(,@(cl-loop for thing in things
                   collect `(conn-thing ,thing))
        (conn-thing thing)
        (conn-thing t)))))

(defconst conn--thing-command-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--thing-command-generalizer
  70 (lambda (cmd &rest _)
       `(when-let* ((thing (conn-command-thing ,cmd)))
          (with-memoization
              (gethash (conn-thing-all-parents thing)
                       conn--thing-command-tag-cache)
            (cons 'thing-command (conn-thing-all-parents thing)))))
  (lambda (things &rest _)
    (when things
      `(,@(cl-loop for thing in (cdr things)
                   collect `(conn-thing ,thing))
        (conn-thing command)
        (conn-thing t)))))

(defconst conn--thing-bounds-tag-cache (make-hash-table :test 'eq))

(cl-generic-define-generalizer conn--thing-bounds-generalizer
  70 (lambda (bounds &rest _)
       `(when-let* ((thing (and (conn-bounds-p ,bounds)
                                (conn-bounds-thing ,bounds))))
          (with-memoization
              (gethash (conn-thing-all-parents thing)
                       conn--thing-bounds-tag-cache)
            (cons 'thing-bounds (conn-thing-all-parents thing)))))
  (lambda (things &rest _)
    (when things
      `((conn-thing bounds)
        ,@(cl-loop for thing in (cdr things)
                   collect `(conn-thing ,thing))
        (conn-thing t)))))

(cl-generic-define-generalizer conn--anonymous-thing-generalizer
  70 (lambda (thing &rest _)
       `(when (conn-anonymous-thing-p ,thing)
          (conn-anonymous-thing-all-parents ,thing)))
  (lambda (things &rest _)
    (when things
      `((conn-thing anonymous-thing)
        (cl-loop for thing in things
                 collect `(conn-thing ,thing))
        (conn-thing t)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head conn-thing)))
  "Support for (conn-thing THING) specializers."
  (list conn--anonymous-thing-generalizer
        conn--thing-bounds-generalizer
        conn--thing-command-generalizer
        conn--thing-generalizer))

;;;; Read Things

;;;;; Thing Args

(oclosure-define (conn-thing-argument
                  (:parent conn-state-eval-argument))
  (recursive-edit :type boolean))

(defun conn-thing-argument (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (conn-handle-thing-argument cmd self)))

(defun conn-thing-argument-dwim (&optional recursive-edit)
  (declare (important-return-value t))
  (oclosure-lambda (conn-thing-argument
                    (value (when (use-region-p)
                             (list 'region nil)))
                    (set-flag (use-region-p))
                    (required t)
                    (recursive-edit recursive-edit))
      (self cmd)
    (conn-handle-thing-argument cmd self)))

(cl-defgeneric conn-handle-thing-argument (cmd arg)
  ( :method (_ arg) arg))

(cl-defmethod conn-handle-thing-argument ((cmd (conn-thing t)) arg)
  (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg))))

(cl-defmethod conn-handle-thing-argument ((cmd (conn-thing recursive-edit)) arg)
  (when (conn-thing-argument--recursive-edit arg)
    (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg)))))

(cl-defmethod conn-eval-argument ((arg conn-thing-argument))
  (conn-state-eval-argument-value arg))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-thing-argument) sym)
  (or (conn-thing-p sym)
      (conn-command-thing sym)
      (cl-call-next-method)))

;;;;;; Subregions

(defvar-keymap conn-subregions-map
  "z" 'toggle-subregions)

(oclosure-define (conn-subregions-argument
                  (:parent conn-state-eval-argument)))

(defun conn-subregions-argument (&optional value)
  (declare (important-return-value t))
  (oclosure-lambda (conn-subregions-argument
                    (value value)
                    (keymap conn-subregions-map))
      (self cmd)
    (conn-handle-subregions-argument cmd self)))

(cl-defgeneric conn-handle-subregions-argument (cmd arg)
  ( :method (_ arg) arg))

(cl-defmethod conn-handle-subregions-argument ((_cmd (eql toggle-subregions))
                                               arg)
  (conn-set-argument arg (not (conn-state-eval-argument-value arg))))

(cl-defmethod conn-handle-subregions-argument ((_cmd (eql conn-things-in-region))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-handle-subregions-argument ((_cmd (conn-thing region))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-handle-subregions-argument ((_cmd (conn-thing recursive-edit))
                                               arg)
  (conn-set-argument arg t))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-subregions-argument)
                                                  sym)
  (or (eq sym 'toggle-subregions)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-subregions-argument))
  (concat "\\[toggle-subregions] "
          (propertize "subregions"
                      'face (when (conn-state-eval-argument-value arg)
                              'eldoc-highlight-function-argument))))

(defvar conn-subregions-argument-reference
  (conn-reference-page "Subregions"
    "If this argument is non-nil then operate on the subregions defined by
the thing command. By default the subregions of a thing command are the
individual things that are moved over. For example the subregions of
`forward-word' with a prefix argument of 3 are the 3 regions containing
the 3 individual words, as opposed to the single region containing all 3
words."))

(cl-defmethod conn-argument-reference ((_arg conn-subregions-argument))
  (list conn-subregions-argument-reference))

;;;;;; Thing Transform Argument

(defvar-keymap conn-transform-map
  "x" 'conn-bounds-trim
  "a" 'conn-bounds-after-point
  "b" 'conn-bounds-before-point
  "SPC" 'conn-bounds-last
  "X" 'conn-transform-reset)

(oclosure-define (conn-transform-argument
                  (:parent conn-state-eval-argument)))

(defun conn-transform-argument (&rest value)
  (declare (important-return-value t)
           (side-effect-free t))
  (oclosure-lambda (conn-transform-argument
                    (value value)
                    (keymap conn-transform-map))
      (self cmd)
    (conn-handle-transform-argument cmd self)))

(cl-defgeneric conn-handle-transform-argument (cmd arg))

(cl-defmethod conn-handle-transform-argument (cmd arg)
  (let ((val (conn-update-argument (conn-state-eval-argument-value arg) cmd)))
    (pcase cmd
      ('conn-transform-reset
       (conn-set-argument arg nil))
      ((and (guard (symbolp cmd))
            (let (and handler (pred identity))
              (get cmd :conn-bounds-transform)))
       (if-let* ((_(memq cmd val)))
           (conn-set-argument arg (remq cmd val))
         (if (functionp handler)
             (funcall handler cmd arg)
           (conn-set-argument arg (cons cmd val)))))
      (_
       (if (equal val (conn-state-eval-argument-value arg))
           arg
         (conn-set-argument arg val))))))

(cl-defmethod conn-argument-completion-predicate ((_arg conn-transform-argument)
                                                  sym)
  (or (get sym :conn-bounds-transform)
      (cl-call-next-method)))

(cl-defmethod conn-display-argument ((arg conn-transform-argument))
  (when-let* ((ts (conn-state-eval-argument-value arg)))
    (concat
     "Transforms: "
     (propertize
      (mapconcat (lambda (tf)
                   (or (get tf :conn-transform-description) ""))
                 ts "∘")
      'face 'eldoc-highlight-function-argument))))

(cl-defmethod conn-eval-argument ((arg conn-transform-argument))
  (nreverse (conn-state-eval-argument-value arg)))

;; (defvar conn-transform-argument-reference
;;   (conn-reference-page "Transform"
;;     "Trim excess characters at either end of the region. By default trims
;; whitespace characters. With a non-nil prefix argument this will prompt
;; for a custom trim regex."))
;;
;; (cl-defmethod conn-argument-reference ((_arg conn-transform-argument))
;;   (list conn-transform-argument-reference))

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
  "[" 'recursive-edit
  "'" 'recursive-edit
  "c" 'conn-things-in-region)

(put 'reset-arg :advertised-binding (key-parse "M-DEL"))

;;;; Bounds of Thing

(cl-defstruct (conn-bounds
               (:constructor conn--make-bounds)
               (:conc-name conn-bounds--))
  (thing nil :type symbol :read-only t)
  (arg nil :type (or nil integer) :read-only t)
  (whole nil :type cons)
  (properties nil :type list))

(defalias 'conn-bounds-thing 'conn-bounds--thing)
(defalias 'conn-bounds-arg 'conn-bounds--arg)

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
      `(app (lambda (v) (conn-bounds-get ,property v ,transform))
            ,(or pat (if (keywordp property)
                         (intern (substring (symbol-name property) 1))
                       property)))
    `(app (conn-bounds-get _ ,property ,transform)
          ,(or pat (if (keywordp property)
                       (intern (substring (symbol-name property) 1))
                     property)))))

(pcase-defmacro conn-bounds (pattern &optional transform)
  `(app ,(static-if (< emacs-major-version 30)
             `(pcase--flip conn-bounds ,transform)
           `(conn-bounds _ ,transform))
        ,pattern))

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

(cl-defgeneric conn-bounds-of (cmd arg)
  (declare (conn-anonymous-thing-property :bounds-op)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) arg)
    (if-let* ((bounds-op (conn-anonymous-thing-property cmd :bounds-op)))
        (funcall bounds-op arg)
      (cl-call-next-method (conn-anonymous-thing-parent cmd) arg))))

(cl-defmethod conn-bounds-of :around (cmd arg)
  (if conn--bounds-of-in-progress
      (cl-call-next-method cmd arg)
    (setf (alist-get (recursion-depth) conn--last-perform-bounds)
          (let ((conn--bounds-of-in-progress t))
            (save-mark-and-excursion
              (cl-call-next-method cmd arg))))))

(cl-defmethod conn-bounds-of ((thing (conn-thing command)) arg)
  (let (conn--last-perform-bounds)
    (deactivate-mark t)
    (pcase (prefix-numeric-value arg)
      (0 nil)
      ((and n (or 1 -1))
       (let ((pt (point))
             (mk (mark))
             (current-prefix-arg n)
             (conn-this-command-handler (conn-command-mark-handler thing))
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
          :subregions (nreverse subregions)))))))

(cl-defmethod conn-bounds-of ((thing (conn-thing thing)) arg)
  (conn-make-bounds thing arg (bounds-of-thing-at-point thing)))

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

(cl-defmethod conn-bounds-of ((cmd (eql conn-composite-thing)) arg)
  (if conn--last-composite-thing
      (conn-make-bounds
       cmd arg
       (cons (point)
             (save-mark-and-excursion
               (conn-with-recursive-stack 'conn-mark-state
                 (execute-kbd-macro conn--last-composite-thing))
               (point))))
    (error "No last composite thing")))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-bounds-of)) _arg)
  (alist-get (recursion-depth) conn--last-perform-bounds))

;;;;; Bounds Transformations

;;;;;; Last Bounds

(put 'conn-bounds-last :conn-bounds-transform t)
(put 'conn-bounds-last :conn-transform-description "last")

(cl-defmethod conn-update-argument ((arg (eql 'conn-bounds-last)) form)
  (if (memq form '(toggle-subregions
                   conn-bounds-after-point
                   conn-bounds-before-point))
      (conn-argument-remove)
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

(cl-defmethod conn-update-argument ((arg (eql 'conn-bounds-after-point)) form)
  (if (or (eq form 'conn-bounds-last)
          (eq form 'conn-bounds-before-point))
      (conn-argument-remove)
    arg))

(cl-defgeneric conn-bounds-after-point (bounds))

(cl-defmethod conn-bounds-after-point (bounds)
  (pcase-let (((conn-bounds `(,_beg . ,end)) bounds))
    (if (<= (point) end)
        (conn-make-bounds-transform bounds (cons (point) end))
      (error "Invalid bounds"))))

(put 'conn-bounds-before-point :conn-bounds-transform t)
(put 'conn-bounds-before-point :conn-transform-description "before")

(cl-defmethod conn-update-argument ((arg (eql 'conn-bounds-before-point)) form)
  (if (or (eq form 'conn-bounds-last)
          (eq form 'conn-bounds-after-point))
      (conn-argument-remove)
    arg))

(cl-defgeneric conn-bounds-before-point (bounds))

(cl-defmethod conn-bounds-before-point (bounds)
  (pcase-let (((conn-bounds `(,beg . ,_end)) bounds))
    (if (>= (point) beg)
        (conn-make-bounds-transform bounds (cons beg (point)))
      (error "Invalid bounds"))))

;;;;;; Check Region

(put 'conn-check-bounds :conn-bounds-transform t)
(put 'conn-check-bounds :conn-transform-description "check")

(defvar-local conn-check-bounds-functions nil)

(defun conn-check-bounds (bounds)
  (cl-loop for fn in conn-check-bounds-functions
           do (funcall fn bounds)
           finally return bounds))

;;;;; Perform Bounds

(conn-define-state conn-bounds-of-recursive-edit-state (conn-command-state)
  :lighter "R")

(cl-defmethod conn-enter-state ((_ (conn-substate conn-bounds-of-recursive-edit-state)))
  (setq buffer-read-only t)
  (conn-state-defer
    (setq buffer-read-only nil))
  (cl-call-next-method))

(define-keymap
  :keymap (conn-get-state-map 'conn-bounds-of-recursive-edit-state)
  "<escape>" 'exit-recursive-edit
  "e" 'exit-recursive-edit
  "C-]" 'abort-recursive-edit
  "q" 'abort-recursive-edit)

(cl-defmethod conn-bounds-of ((_cmd (conn-thing recursive-edit)) _arg)
  (let* ((eldoc-message-function 'ignore)
         (pre (lambda ()
                (message
                 (substitute-command-keys
                  (concat
                   "Recursive Edit: Press \\[exit-recursive-edit] to exit, "
                   "\\[abort-recursive-edit] to abort"))))))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook pre)
          (conn-with-recursive-stack 'conn-bounds-of-recursive-edit-state
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

;;;;; Bounds of Remote Thing

(cl-defgeneric conn-bounds-of-remote (cmd arg pt)
  (declare (conn-anonymous-thing-property :bounds-op-remote)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) arg)
    (if-let* ((remote (conn-anonymous-thing-property cmd :bounds-op-remote)))
        (funcall remote arg)
      (cl-call-next-method (conn-anonymous-thing-parent cmd) arg)))
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
  (pcase-let* ((name (symbol-name cmd))
               (at nil)
               (quit (lambda ()
                       (setq at (min (point) isearch-other-end))
                       (when isearch-mode-end-hook-quit
                         (abort-recursive-edit))))
               (`(,thing ,arg)
                (conn-eval-with-state 'conn-read-thing-state
                    (list && (conn-thing-argument)))))
    (unwind-protect
        (save-mark-and-excursion
          (add-hook 'isearch-mode-end-hook quit)
          (isearch-mode (not (string-match-p "backward" name))
                        (string-match-p "regexp" name)
                        nil t))
      (remove-hook 'isearch-mode-end-hook quit))
    (conn-bounds-of-remote thing arg at)))

;;;; Bounds of Things in Region

(cl-defgeneric conn-get-things-in-region (thing beg end)
  (declare (conn-anonymous-thing-property :things-in-region)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing)) beg end)
    (if-let* ((op (conn-anonymous-thing-property cmd :things-in-region)))
        (funcall op beg end)
      (cl-call-next-method (conn-anonymous-thing-parent cmd) beg end))))

(cl-defmethod conn-get-things-in-region ((thing (conn-thing thing))
                                         beg end)
  (save-excursion
    (let ((thing (conn-get-thing thing)))
      (ignore-errors
        (goto-char beg)
        (forward-thing thing 1)
        (cl-loop for bd = (cons (save-excursion
                                  (forward-thing thing -1)
                                  (point))
                                (point))
                 while (and bd (< (car bd) end))
                 collect (conn-make-bounds thing nil bd)
                 while (and (< (point) end)
                            (ignore-errors
                              (forward-thing thing 1)
                              t)))))))

(conn-register-thing 'conn-things-in-region)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-things-in-region)) _arg)
  (let* ((thing (conn-eval-with-state 'conn-read-thing-state
                    (conn-get-thing (car & (conn-thing-argument-dwim)))
                  :prompt "Things in Region"))
         (beg (save-excursion
                (goto-char (region-beginning))
                (forward-thing thing 1)
                (forward-thing thing -1)
                (point)))
         (end (save-excursion
                (let ((end (region-end)))
                  (goto-char end)
                  (forward-thing thing -1)
                  (ignore-errors
                    (while (< (point) end)
                      (forward-thing thing 1))))
                (point))))
    (conn-make-bounds
     thing nil
     (cons beg end)
     :subregions (lambda (bounds)
                   (pcase-let ((`(,beg . ,end)
                                (conn-bounds bounds)))
                     (setf (conn-bounds-get bounds :subregions)
                           (conn-get-things-in-region
                            (conn-bounds-thing bounds) beg end)))))))

;;;; Thing Definitions

(conn-define-mark-command conn-mark-email email)
(conn-define-mark-command conn-mark-uuid uuid)
(conn-define-mark-command conn-mark-string string)
(conn-define-mark-command conn-mark-filename filename)
(conn-define-mark-command conn-mark-comment comment)

(put 'conn-composite-thing :conn-thing t)

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

(conn-register-thing 'defun :forward-op 'conn-forward-defun)

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

(conn-register-thing
 'buffer-after-point
 :bounds-op (lambda () (cons (point) (point-max))))

(conn-register-thing
 'buffer-before-point
 :bounds-op (lambda () (cons (point-min) (point))))

(conn-register-thing
 'visible
 :bounds-op (lambda () (cons (window-start) (window-end))))

(conn-define-mark-command conn-mark-visible visible)

(conn-register-thing-commands
 'recursive-edit nil
 'recursive-edit 'exit-recursive-edit)

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

(conn-register-thing-commands
 'visible nil
 'conn-scroll-up 'conn-scroll-down
 'scroll-up-command 'scroll-down-command
 'conn-mark-visible)

(conn-register-thing-commands
 'region nil
 'conn-toggle-mark-command
 'conn-set-mark-command)

(conn-register-thing 'symbol :forward-op 'forward-symbol)

(conn-register-thing-commands
 'symbol 'conn-continuous-thing-handler
 'forward-symbol 'conn-backward-symbol)

(conn-register-thing 'page :forward-op 'forward-page)

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
 'word 'conn-symbol-handler
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

(conn-register-thing 'paragraph :forward-op 'forward-paragraph)

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
