;;; conn-transients.el --- Transients for Conn -*- lexical-binding: t -*-
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
;;; Commentary:

;; Transient commands for Conn.

;;; Code:

(require 'conn)
(require 'conn-states)
(require 'conn-commands)
(require 'kmacro)
(require 'transient)
(require 'text-property-search)
(eval-when-compile
  (require 'map))

;;;; Declarations

(defvar sort-fold-case)

;;;; Utils

(defmacro conn-transient-mode-suffix (name arglist description mode &rest properties)
  "Define a `transient' suffix for a minor mode.

\(fn NAME DESCRIPTION MODE &rest [KEYWORD VALUE]... [BODY...])"
  (declare (indent defun))
  (pcase-let* (((or `(,mode-command ,mode-var)
                    (and mode-command mode-var))
                mode)
               (body `((interactive)
                       (,mode-command 'toggle)))
               (properties (cl-loop for (k . rest) on properties by #'cddr
                                    if (keywordp k)
                                    nconc (list k (car rest)) into props
                                    else
                                    do (setq body (cons k rest))
                                    and return props)))
    `(transient-define-suffix ,name ,arglist
       :description (lambda ()
                      (concat ,description " "
                              (if (bound-and-true-p ,mode-var)
                                  (propertize "(*)" 'face 'transient-value)
                                (propertize "( )" 'face 'transient-inactive-value))))
       :transient t
       ,@properties
       ,@body)))

;;;;; Kmacro Utils

;;;###autoload
(defun conn--kmacro-display (macro &optional trunc)
  (pcase macro
    ((or 'nil '[] "") "nil")
    (_ (let* ((m (format-kbd-macro macro))
              (l (length m))
              (z (and trunc (> l trunc))))
         (format "%s%s"
                 (if z (substring m 0 (1- trunc)) m)
                 (if z "…" ""))))))

(defun conn--kmacro-ring-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Ring: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value)
     " - "
     (propertize
      (conn--kmacro-display last-kbd-macro 35)
      'face 'transient-value))))

(defun conn--kmacro-counter-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Counter: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value))))

(defun conn--in-kbd-macro-p ()
  (or defining-kbd-macro executing-kbd-macro))

;;;; Transient Classes

;;;;; Lisp Values

(defclass conn-transient-lisp-value (transient-infix)
  ((keyword :initarg :keyword))
  "Abstract super class for lisp values."
  :abstract t)

(cl-defmethod transient-infix-value ((obj conn-transient-lisp-value))
  (cons (if (slot-boundp obj 'keyword)
            (oref obj keyword)
          (oref obj description))
        (oref obj value)))

;;;;;; Switch

(defclass conn-transient-lisp-bool (conn-transient-lisp-value)
  nil)

(cl-defmethod transient-init-value ((_obj conn-transient-lisp-bool))
  "Noop" nil)

(cl-defmethod transient-infix-read ((obj conn-transient-lisp-bool))
  (not (oref obj value)))

(cl-defmethod transient-infix-set ((obj conn-transient-lisp-bool) newval)
  (oset obj value newval))

(cl-defmethod transient-format-value ((obj conn-transient-lisp-bool))
  (propertize (downcase (oref obj description))
              'face (if (oref obj value)
                        'transient-argument
                      'transient-inactive-value)))

;;;;;; Choices

(defclass conn-transient-lisp-choices (conn-transient-lisp-value)
  ((choices :initarg :choices :initform nil)))

(cl-defmethod transient-init-value ((obj conn-transient-lisp-choices))
  (with-slots (value choices) obj
    (setf value (car choices))))

(cl-defmethod transient-infix-read ((obj conn-transient-lisp-choices))
  (with-slots (choices value) obj
    (thread-first
      (1+ (seq-position choices value #'eq))
      (mod (length choices))
      (nth choices))))

(cl-defmethod transient-infix-set ((obj conn-transient-lisp-choices) newval)
  (setf (oref obj value) newval))

(cl-defmethod transient-format-value ((obj conn-transient-lisp-choices))
  (with-slots (value choices) obj
    (format
     (propertize "%s" 'face 'transient-delimiter)
     (mapconcat
      (pcase-lambda ((and `(,description . ,_) choice))
        (propertize description
                    'face (if (eq choice value)
                              'transient-argument
                            'transient-inactive-value)))
      (seq-filter #'car choices)
      (propertize "|" 'face 'transient-delimiter)))))

(cl-defmethod transient-infix-value ((obj conn-transient-lisp-choices))
  (cons (if (slot-boundp obj 'keyword)
            (oref obj keyword)
          (oref obj description))
        (cdr (oref obj value))))

;;;;; Kapply Pipeline Infix

(defclass conn-transient-kapply-pipeline (conn-transient-lisp-choices)
  ((keyword :initform :pipeline)))

;;;; Kapply Transients

(defun conn--transient-kapply-pipeline-args (arglist)
  (cl-loop for arg in arglist
           when (eq (car-safe arg) :pipeline)
           collect (cdr arg)))

(defun conn-recursive-edit-kmacro (arg)
  "Edit last keyboard macro inside a recursive edit.

Press \\[exit-recursive-edit] to exit the recursive edit and abort the
edit in the macro."
  (interactive "P")
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-macro (not arg))
      (when-let* ((buffer (get-buffer "*Edit Macro*")))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
              (goto-char (match-beginning 1))
              (delete-region (match-beginning 1) (match-end 1))
              (insert (substitute-command-keys "\\[exit-recursive-edit]"))))
          (delete-other-windows)
          (conn-local-mode 1)
          (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
          (unwind-protect
              (recursive-edit)
            (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
            (kill-buffer buffer)))))))

(defun conn-recursive-edit-lossage ()
  "Edit lossage macro inside a recursive edit.

Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive)
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-lossage)
      (when-let* ((buffer (get-buffer "*Edit Macro*")))
        (with-current-buffer buffer
          (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
            (goto-char (match-beginning 1))
            (delete-region (match-beginning 1) (match-end 1))
            (insert (substitute-command-keys "\\[exit-recursive-edit]")))
          (delete-other-windows)
          (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
          (unwind-protect
              (recursive-edit)
            (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
            (kill-buffer buffer)))))))

(defun conn--push-macro-ring (macro)
  (interactive
   (list (get-register (register-read-with-preview "Kmacro: "))))
  (unless (or (stringp macro)
              (vectorp macro)
              (kmacro-p macro))
    (user-error "Invalid keyboard macro"))
  (kmacro-push-ring macro)
  (kmacro-swap-ring))

;;;;; Kapply infixes

(transient-define-argument conn--kapply-macro-infix ()
  "Dispatch `last-kbd-macro'.

  APPLY simply executes the macro at each region.  APPEND executes
  the macro and records additional keys on the first iteration.
  STEP-EDIT uses `kmacro-step-edit-macro' to edit the macro before
  dispatch."
  :class 'conn-transient-lisp-choices
  :description "Last Kmacro"
  :key "k"
  :keyword :kmacro
  :choices `((nil . conn--kmacro-apply)
             ("apply" . ,(lambda (it)
                           (conn--kmacro-apply it 0 last-kbd-macro)))
             ("step-edit" . conn--kmacro-apply-step-edit)
             ("append" . conn--kmacro-apply-append)))

(transient-define-argument conn--kapply-query-infix ()
  :class 'conn-transient-kapply-pipeline
  :description "Query"
  :key "q"
  :choices `(nil
             ("query" . conn--kapply-query)))

(transient-define-argument conn--kapply-sort-infix ()
  :class 'conn-transient-lisp-choices
  :description "Order"
  :key "o"
  :keyword :order
  :choices `((nil . nil)
             ("forward" . identity)
             ("reverse" . nreverse)))

(transient-define-argument conn--kapply-empty-infix ()
  :class 'conn-transient-kapply-pipeline
  :description "Empty"
  :key "0"
  :choices `(nil
             ("skip" . conn--kapply-skip-empty)))

(transient-define-argument conn--kapply-ibuffer-infix ()
  "Display buffers touched in an `ibuffer' buffer."
  :class 'conn-transient-kapply-pipeline
  :description "Ibuffer"
  :key "b"
  :choices `(("auto" . conn--kapply-ibuffer-overview)
             nil))

(transient-define-argument conn--kapply-matches-infix ()
  "Restrict dispatch to only some isearch matches.

AFTER means only those matchs after, and including, the current match.
BEFORE means only those matches before, and including, the current match."
  :class 'conn-transient-lisp-choices
  :description "Restrict Matches Inclusive"
  :if-not (lambda ()
            (or (bound-and-true-p multi-isearch-buffer-list)
                (bound-and-true-p multi-isearch-file-list)))
  :key "j"
  :keyword :matches
  :choices '(nil
             ("after" . after)
             ("before" . before)))

(transient-define-argument conn--kapply-state-infix ()
  "Dispatch in a specific state."
  :class 'conn-transient-kapply-pipeline
  :description "State"
  :key "w"
  :choices `((nil . ,(lambda (it)
                       (conn--kapply-with-state it conn-current-state)))
             ("change" . conn--kapply-change-region)
             ("command" . ,(lambda (it)
                             (conn--kapply-with-state it 'conn-command-state)))
             ("emacs" . ,(lambda (it)
                           (conn--kapply-with-state it 'conn-emacs-state)))))

(transient-define-argument conn--kapply-region-infix ()
  "How to dispatch on each region.

START means place the point at the start of the region before
each iteration.  END means place the point at the end of the
region before each iteration.  CHANGE means delete the region
before each iteration."
  :class 'conn-transient-kapply-pipeline
  :key "t"
  :description "Regions"
  :choices '(("start" . nil)
             ("end" . conn--kapply-at-end)))

(transient-define-argument conn--kapply-read-hl-patterns ()
  "Dispatch on regions from last to first."
  :class 'conn-transient-lisp-bool
  :key "r"
  :description "Read Patterns"
  :keyword :read-patterns)

(transient-define-argument conn--kapply-highlights-in-thing ()
  "Dispatch on regions from last to first."
  :class 'conn-transient-lisp-choices
  :key "i"
  :description "In"
  :keyword :in-thing
  :choices '(("buffer" . nil)
             ("thing" . t)))

(transient-define-argument conn--kapply-save-excursion-infix ()
  "Save the point and mark in each buffer during dispatch."
  :class 'conn-transient-kapply-pipeline
  :key "x"
  :description "Excursions"
  :choices '(("save" . conn--kapply-save-excursion)
             nil))

(transient-define-argument conn--kapply-save-restriction-infix ()
  "Save and restore the current restriction in each buffer during dispatch."
  :class 'conn-transient-kapply-pipeline
  :key "N"
  :description "Restrictions"
  :choices '(("save" . conn--kapply-save-restriction)
             nil))

(transient-define-argument conn--kapply-merge-undo-infix ()
  "Merge all macro iterations into a single undo in each buffer."
  :class 'conn-transient-kapply-pipeline
  :key "/"
  :description "Merge Undo Per"
  :choices '(("buffer atomic" . conn--kapply-per-buffer-atomic-undo)
             ("buffer" . conn--kapply-per-buffer-undo)
             ("iteration" . conn--kapply-per-iteration-undo)
             nil))

(transient-define-argument conn--kapply-save-windows-infix ()
  "Save and restore current window configuration during dispatch."
  :class 'conn-transient-kapply-pipeline
  :key "4"
  :description "Window Conf"
  :choices '(nil
             ("save" . conn--kapply-save-windows)))

;;;;; Kapply suffixes

(transient-define-suffix conn--kapply-replace-rectangle-suffix (args)
  "Apply keyboard macro to a rectangle replacing each line."
  :transient 'transient--do-exit
  :key "m"
  :description "Replace"
  (interactive (list (transient-args transient-current-command)))
  (let ((regions (region-bounds)))
    (deactivate-mark)
    (unless (or (length= regions 1)
                (< (point) (car (nth 1 regions))))
      (setq regions (nreverse regions)))
    (conn--kapply-macro
     (alist-get :kmacro args)
     (conn-kapply-region-iterator regions)
     `(conn--kapply-relocate-to-region
       conn--kapply-skip-invisible-regions
       conn--kapply-pulse-region
       ,@(conn--transient-kapply-pipeline-args args)
       conn--kapply-change-region))))

(transient-define-suffix conn--kapply-emacs-rectangle-suffix (args)
  "Apply keyboard macro in `conn-emacs-state' to a rectangle."
  :transient 'transient--do-exit
  :key "e"
  :description "Emacs"
  (interactive (list (transient-args transient-current-command)))
  (let ((regions (region-bounds)))
    (unless (or (length= regions 1)
                (< (point) (car (nth 1 regions))))
      (setq regions (nreverse regions)))
    (deactivate-mark)
    (conn--kapply-macro
     (alist-get :kmacro args)
     (conn-kapply-region-iterator regions)
     `(conn--kapply-relocate-to-region
       conn--kapply-skip-invisible-regions
       ,(when (> (point) (caar regions))
          'conn--kapply-at-end)
       conn--kapply-pulse-region
       ,@(conn--transient-kapply-pipeline-args args)
       ,(lambda (iterator)
          (conn--kapply-with-state iterator 'conn-emacs-state))))))

(transient-define-suffix conn--kapply-command-rectangle-suffix (args)
  "Apply keyboard macro in `conn-command-state' to a rectangle."
  :transient 'transient--do-exit
  :key "c"
  :description "Command"
  (interactive (list (transient-args transient-current-command)))
  (let ((regions (region-bounds)))
    (unless (or (length= regions 1)
                (< (point) (car (nth 1 regions))))
      (setq regions (nreverse regions)))
    (deactivate-mark)
    (conn--kapply-macro
     (alist-get :kmacro args)
     (conn-kapply-region-iterator regions)
     `(conn--kapply-relocate-to-region
       conn--kapply-skip-invisible-regions
       ,(when (> (point) (caar regions))
          'conn--kapply-at-end)
       conn--kapply-pulse-region
       ,@(conn--transient-kapply-pipeline-args args)
       ,(lambda (iterator)
          (conn--kapply-with-state iterator 'conn-command-state))))))

(transient-define-suffix conn--kapply-match-suffix (args)
  "Apply keyboard macro to every occurrence of a string within a region."
  :transient 'transient--do-exit
  :key "m"
  :description "Match"
  (interactive (list (transient-args transient-current-command)))
  (conn-read-args (conn-read-thing-state
                   :prompt "Thing")
      ((`(,thing ,arg) (conn-replace-thing-argument))
       (transform (conn-transform-argument))
       (subregions (conn-subregions-argument (use-region-p)))
       (regexp (conn-boolean-argument 'regexp
                                      conn-regexp-argument-map
                                      "regexp")))
    (let ((delimited (oref transient-current-prefix scope)))
      (conn--kapply-macro
       (alist-get :kmacro args)
       (conn-kapply-match-iterator thing
                                   arg
                                   transform
                                   subregions
                                   regexp
                                   delimited
                                   (or (alist-get :order args)
                                       'conn--nnearest-first))
       `(conn--kapply-relocate-to-region
         ,(if (eq search-invisible 'open)
              'conn--kapply-open-invisible
            'conn--kapply-skip-invisible-regions)
         conn--kapply-pulse-region
         ,@(conn--transient-kapply-pipeline-args args)
         conn--kapply-query)))))

(transient-define-suffix conn--kapply-things-suffix (args)
  "Apply keyboard macro on the current region.

If the region is discontinuous (e.g. a rectangular region) then
apply to each contiguous component of the region."
  :if (lambda () conn-local-mode)
  :transient 'transient--do-exit
  :key "f"
  :description "Things"
  (interactive (list (transient-args transient-current-command)))
  (conn--kapply-macro
   (alist-get :kmacro args)
   (conn-kapply-region-iterator
    (conn-read-args (conn-read-thing-state
                     :prompt "Thing")
        ((`(,thing ,arg)
          (conn-thing-argument-dwim-rectangle t))
         (transform (conn-transform-argument)))
      (mapcar #'conn-bounds (conn-bounds-get
                             (conn-bounds-of thing arg)
                             :subregions transform))))
   `(conn--kapply-relocate-to-region
     conn--kapply-skip-invisible-points
     conn--kapply-pulse-region
     ,@(conn--transient-kapply-pipeline-args args))))

(transient-define-suffix conn--kapply-iterate-suffix (args)
  "Apply keyboard macro a specified number of times.

A zero means repeat until error."
  :transient 'transient--do-exit
  :key "i"
  :description "Iterate"
  (interactive (list (transient-args transient-current-command)))
  (conn--kapply-macro
   (lambda (iterator)
     (funcall (alist-get :kmacro args)
              iterator
              (read-number "Iterations: " 0)))
   (conn-kapply-infinite-iterator)
   (conn--transient-kapply-pipeline-args args)))

(transient-define-suffix conn--kapply-regions-suffix (iterator args)
  "Apply keyboard macro on regions."
  :transient 'transient--do-exit
  :key "v"
  :description "Regions"
  (interactive (list (oref transient-current-prefix scope)
                     (transient-args transient-current-command)))
  (conn--kapply-macro
   (alist-get :kmacro args)
   iterator
   `(conn--kapply-relocate-to-region
     conn--kapply-open-invisible
     conn--kapply-pulse-region
     ,@(conn--transient-kapply-pipeline-args args))))

(transient-define-suffix conn--kapply-dispatch-suffix (callback args)
  "Apply keyboard macro on dispatch targets."
  :transient 'transient--do-exit
  :key "f"
  :description "Resume"
  (interactive (list (oref transient-current-prefix scope)
                     (transient-args transient-current-command)))
  (let ((applier (alist-get :kmacro args))
        (pipeline (conn--transient-kapply-pipeline-args args)))
    (when (and (eq applier 'conn--kmacro-apply-step-edit)
               (length= last-kbd-macro 0))
      (error "No keyboard macro to edit"))
    (funcall callback applier pipeline)))

(transient-define-suffix conn--kapply-isearch-suffix (args)
  "Apply keyboard macro on current isearch matches."
  :transient 'transient--do-exit
  :key "m"
  :description "Matches"
  (interactive (list (transient-args transient-current-command)))
  (let* ((matches
          (cond ((bound-and-true-p multi-isearch-file-list)
                 (mapcan 'conn--isearch-matches
                         (nconc
                          (delq (current-buffer)
                                (mapcar 'find-file-noselect
                                        multi-isearch-file-list))
                          (list (current-buffer)))))
                ((bound-and-true-p multi-isearch-buffer-list)
                 (mapcan 'conn--isearch-matches
                         (nconc
                          (remq (current-buffer) multi-isearch-buffer-list)
                          (list (current-buffer)))))
                (t
                 (conn--isearch-matches
                  (current-buffer)
                  (alist-get :matches args))))))
    (unwind-protect
        (isearch-done)
      (conn--kapply-macro
       (alist-get :kmacro args)
       (conn-kapply-region-iterator
        matches
        (or (alist-get :order args)
            'conn--nnearest-first))
       `(conn--kapply-relocate-to-region
         conn--kapply-open-invisible
         conn--kapply-pulse-region
         ,@(conn--transient-kapply-pipeline-args args))))))

(transient-define-suffix conn--kapply-highlights (args)
  "Apply keyboard macro on regions of text with a specified text property."
  :transient 'transient--do-exit
  :key "h"
  :description "Highlights"
  (interactive (list (transient-args transient-current-command)))
  (conn-read-args (conn-read-thing-state
                   :prompt "Thing")
      ((`(,thing ,arg) (conn-thing-argument-dwim)))
    (pcase (conn-bounds-of thing arg)
      ((conn-bounds `(,beg . ,end))
       (conn--kapply-macro
        (alist-get :kmacro args)
        (conn-kapply-highlight-iterator
         (or beg (point-min))
         (or end (point-max))
         (or (alist-get :order args)
             'conn--nnearest-first)
         (alist-get :read-patterns args))
        `(conn--kapply-relocate-to-region
          conn--kapply-open-invisible
          conn--kapply-pulse-region
          ,@(conn--transient-kapply-pipeline-args args)))))))

(transient-define-suffix conn--kapply-occur (args)
  "Apply keyboard macro on regions of text with a specified text property."
  :transient 'transient--do-exit
  :key "l"
  :description "Occur Matches"
  :if (lambda () (eq major-mode 'occur-mode))
  (interactive (list (transient-args transient-current-command)))
  (conn--kapply-macro
   (alist-get :kmacro args)
   (conn-kapply-region-iterator
    (save-excursion
      (goto-char (point-min))
      (cl-loop for match = (text-property-search-forward 'occur-target)
               while match
               append (pcase (prop-match-value match)
                        ((and pt (guard (markerp pt)))
                         (list (cons pt (marker-position pt))))
                        (reg reg))))
    (or (alist-get :order args)
        'conn--nnearest-first))
   `(conn--kapply-relocate-to-region
     conn--kapply-skip-invisible-regions
     conn--kapply-pulse-region
     ,@(conn--transient-kapply-pipeline-args args))))

(transient-define-suffix conn--kapply-text-property-suffix (prop value args)
  "Apply keyboard macro on regions of text with a specified text property."
  :transient 'transient--do-exit
  :key "T"
  :description "Text Prop"
  (interactive
   (let* ((prop (intern (completing-read
                         "Property: "
                         (cl-loop for prop in (text-properties-at (point))
                                  by #'cddr collect prop)
                         nil t)))
          (vals (mapcar (lambda (s) (cons (format "%s" s) s))
                        (ensure-list (get-text-property (point) prop))))
          (val (alist-get (completing-read "Value: " vals) vals
                          nil nil #'string=)))
     (list prop val (transient-args transient-current-command))))
  (conn--kapply-macro
   (alist-get :kmacro args)
   (conn-kapply-region-iterator
    (save-excursion
      (goto-char (point-min))
      (let (regions)
        (while-let ((match (text-property-search-forward prop value t)))
          (push (cons (prop-match-beginning match)
                      (prop-match-end match))
                regions))
        regions))
    (or (alist-get :order args)
        'conn--nnearest-first))
   `(conn--kapply-advance-region
     conn--kapply-open-invisible
     conn--kapply-pulse-region
     ,@(conn--transient-kapply-pipeline-args args))))

;;;;; Kapply prefixes

;;;###autoload (autoload 'conn-kapply-prefix "conn-transients" nil t)
(transient-define-prefix conn-kapply-prefix (arg)
  "Transient menu for keyboard macro application on regions."
  [ :description conn--kmacro-ring-display
    [ ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("M" "Display"
       (lambda ()
         (interactive)
         (kmacro-display last-kbd-macro t))
       :transient t)]
    [ ("c" "Set Counter" kmacro-set-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)
      ("g" "Push Register" conn--push-macro-ring :transient t)]
    [ ("E" "Edit Macro"
       (lambda (arg)
         (interactive "P")
         (conn-recursive-edit-kmacro arg)
         (transient-resume))
       :transient transient--do-suspend)
      ("L" "Edit Lossage"
       (lambda ()
         (interactive)
         (conn-recursive-edit-lossage)
         (transient-resume))
       :transient transient--do-suspend)]]
  [ :if (lambda () (bound-and-true-p rectangle-mark-mode))
    :description "Options:"
    [ (conn--kapply-state-infix)
      (conn--kapply-macro-infix)
      (conn--kapply-empty-infix)]]
  [ :if-not (lambda () (bound-and-true-p rectangle-mark-mode))
    :description "Options:"
    [ (conn--kapply-region-infix)
      (conn--kapply-state-infix)
      (conn--kapply-sort-infix)
      (conn--kapply-empty-infix)]
    [ (conn--kapply-macro-infix)
      (conn--kapply-ibuffer-infix)
      (conn--kapply-query-infix)]]
  [ [ :if (lambda () (bound-and-true-p rectangle-mark-mode))
      :description "With State:"
      (conn--kapply-replace-rectangle-suffix)
      (conn--kapply-emacs-rectangle-suffix)
      (conn--kapply-command-rectangle-suffix)]
    [ :if-not (lambda () (bound-and-true-p rectangle-mark-mode))
      :description "Apply Kmacro On:"
      (conn--kapply-occur)
      (conn--kapply-match-suffix)
      (conn--kapply-things-suffix)
      (conn--kapply-text-property-suffix)
      (conn--kapply-iterate-suffix)]
    [ :description "Save State:"
      (conn--kapply-merge-undo-infix)
      (conn--kapply-save-windows-infix)
      (conn--kapply-save-restriction-infix)
      (conn--kapply-save-excursion-infix)]]
  (interactive "P")
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-kapply-prefix nil nil :scope arg))

;;;###autoload (autoload 'conn-kapply-hightlight-prefix "conn-transients" nil t)
(transient-define-prefix conn-kapply-hightlight-prefix ()
  "Transient menu for keyboard macro application on highlights."
  [ :description conn--kmacro-ring-display
    [ ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("M" "Display"
       (lambda ()
         (interactive)
         (kmacro-display last-kbd-macro t))
       :transient t)]
    [ ("c" "Set Counter" kmacro-set-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)
      ("g" "Push Register" conn--push-macro-ring :transient t)]
    [ ("E" "Edit Macro"
       (lambda (arg)
         (interactive "P")
         (conn-recursive-edit-kmacro arg)
         (transient-resume))
       :transient transient--do-suspend)
      ("L" "Edit Lossage"
       (lambda ()
         (interactive)
         (conn-recursive-edit-lossage)
         (transient-resume))
       :transient transient--do-suspend)]]
  [ :description "Options:"
    [ (conn--kapply-region-infix)
      (conn--kapply-state-infix)
      (conn--kapply-sort-infix)
      (conn--kapply-highlights-in-thing)]
    [ (conn--kapply-macro-infix)
      (conn--kapply-ibuffer-infix)
      (conn--kapply-read-hl-patterns)
      (conn--kapply-query-infix)]]
  [ [ :description "Apply Kmacro On:"
      (conn--kapply-highlights)]
    [ :description "Save State:"
      (conn--kapply-merge-undo-infix)
      (conn--kapply-save-windows-infix)
      (conn--kapply-save-restriction-infix)
      (conn--kapply-save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-kapply-hightlight-prefix))

;;;###autoload (autoload 'conn-isearch-kapply-prefix "conn-transients" nil t)
(transient-define-prefix conn-isearch-kapply-prefix ()
  "Transient menu for keyboard macro application on isearch matches."
  [ :description conn--kmacro-ring-display
    [ ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("M" "Display"
       (lambda ()
         (interactive)
         (kmacro-display last-kbd-macro t))
       :transient t)]
    [ ("c" "Set Counter" kmacro-set-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)
      ("g" "Push Register" conn--push-macro-ring :transient t)]
    [ ("E" "Edit Macro"
       (lambda (arg)
         (interactive "P")
         (conn-recursive-edit-kmacro arg)
         (transient-resume))
       :transient transient--do-suspend)
      ("L" "Edit Lossage"
       (lambda ()
         (interactive)
         (conn-recursive-edit-lossage)
         (transient-resume))
       :transient transient--do-suspend)]]
  [ :description "Options:"
    [ (conn--kapply-region-infix)
      (conn--kapply-state-infix)
      (conn--kapply-sort-infix)
      (conn--kapply-query-infix)]
    [ (conn--kapply-matches-infix)
      (conn--kapply-macro-infix)
      (conn--kapply-ibuffer-infix)]]
  [ [ :description "Apply Kmacro On:"
      (conn--kapply-isearch-suffix)]
    [ :description "Save State:"
      (conn--kapply-merge-undo-infix)
      (conn--kapply-save-windows-infix)
      (conn--kapply-save-restriction-infix)
      (conn--kapply-save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-isearch-kapply-prefix))

;;;###autoload (autoload 'conn-regions-kapply-prefix "conn-transients" nil t)
(transient-define-prefix conn-regions-kapply-prefix (iterator)
  "Transient menu for keyboard macro application on regions."
  [ :description conn--kmacro-ring-display
    [ ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("M" "Display"
       (lambda ()
         (interactive)
         (kmacro-display last-kbd-macro t))
       :transient t)]
    [ ("c" "Set Counter" kmacro-set-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)
      ("g" "Push Register" conn--push-macro-ring :transient t)]
    [ ("E" "Edit Macro"
       (lambda (arg)
         (interactive "P")
         (conn-recursive-edit-kmacro arg)
         (transient-resume))
       :transient transient--do-suspend)
      ("L" "Edit Lossage"
       (lambda ()
         (interactive)
         (conn-recursive-edit-lossage)
         (transient-resume))
       :transient transient--do-suspend)]]
  [ :description "Options:"
    [ (conn--kapply-region-infix)
      (conn--kapply-state-infix)]
    [ (conn--kapply-query-infix)
      (conn--kapply-macro-infix)
      (conn--kapply-ibuffer-infix)]]
  [ [ :description "Apply Kmacro On:"
      (conn--kapply-regions-suffix)]
    [ :description "Save State:"
      (conn--kapply-merge-undo-infix)
      (conn--kapply-save-windows-infix)
      (conn--kapply-save-restriction-infix)
      (conn--kapply-save-excursion-infix)]]
  (interactive (list nil))
  (unless iterator (user-error "No regions"))
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-regions-kapply-prefix nil nil :scope iterator))

;;;###autoload (autoload 'conn-dispatch-kapply-prefix "conn-transients" nil t)
(transient-define-prefix conn-dispatch-kapply-prefix (callback)
  "Transient menu for keyboard macro application on regions."
  [ :description conn--kmacro-ring-display
    [ ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("M" "Display"
       (lambda ()
         (interactive)
         (kmacro-display last-kbd-macro t))
       :transient t)]
    [ ("c" "Set Counter" kmacro-set-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)
      ("g" "Push Register" conn--push-macro-ring :transient t)]
    [ ("E" "Edit Macro"
       (lambda (arg)
         (interactive "P")
         (conn-recursive-edit-kmacro arg)
         (transient-resume))
       :transient transient--do-suspend)
      ("L" "Edit Lossage"
       (lambda ()
         (interactive)
         (conn-recursive-edit-lossage)
         (transient-resume))
       :transient transient--do-suspend)]]
  [ [ :description "Options:"
      (conn--kapply-macro-infix)
      (conn--kapply-state-infix)
      (conn--kapply-region-infix)]
    [ :description "Save State:"
      (conn--kapply-save-restriction-infix)
      (conn--kapply-save-excursion-infix)
      (conn--kapply-save-windows-infix
       :init-value (lambda (obj)
                     (oset obj value (assoc "save" (oref obj choices)))))]]
  [ [ :description "Dispatch:"
      (conn--kapply-dispatch-suffix)]]
  (interactive (list nil))
  (kmacro-display last-kbd-macro t)
  (transient-setup 'conn-dispatch-kapply-prefix nil nil :scope callback))

;;;; Kmacro Prefix

(transient-define-infix conn--set-counter-format-infix ()
  "Set `kmacro-counter-format'."
  :class 'transient-lisp-variable
  :set-value (lambda (_ format) (kmacro-set-format format))
  :variable 'kmacro-counter-format
  :reader (lambda (&rest _)
            (read-string "Macro Counter Format: ")))

;;;###autoload (autoload 'conn-kmacro-prefix "conn-transients" nil t)
(transient-define-prefix conn-kmacro-prefix ()
  "Transient menu for kmacro functions."
  [ :description conn--kmacro-ring-display
    :if-not conn--in-kbd-macro-p
    [ ("l" "List Macros" list-keyboard-macros
       :if (lambda () (version<= "30" emacs-version)))
      ("n" "Next" kmacro-cycle-ring-previous :transient t)
      ("p" "Previous" kmacro-cycle-ring-next :transient t)
      ("w" "Swap" kmacro-swap-ring :transient t)
      ("o" "Pop" kmacro-delete-ring-head :transient t)]
    [ ("i" "Insert Counter" kmacro-insert-counter)
      ("c" "Set Counter" kmacro-set-counter :transient t)
      ("+" "Add to Counter" kmacro-add-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)]
    [ :if (lambda () (version<= "30" emacs-version))
      ("q<" "Quit Counter Less" kmacro-quit-counter-less)
      ("q>" "Quit Counter Greater" kmacro-quit-counter-greater)
      ("q=" "Quit Counter Equal" kmacro-quit-counter-equal)]]
  [ :if (lambda () (version<= "30" emacs-version))
    :description "Counter Registers"
    [ ("rs" "Save Counter Register" kmacro-reg-save-counter)
      ("rl" "Load Counter Register" kmacro-reg-load-counter)]
    [ ("r<" "Register Add Counter <" kmacro-reg-add-counter-less)
      ("r>" "Register Add Counter >" kmacro-reg-add-counter-greater)
      ("r=" "Register Add Counter =" kmacro-reg-add-counter-equal)]]
  [ "Commands"
    :if-not conn--in-kbd-macro-p
    [ ("m" "Record Macro" kmacro-start-macro)
      ("k" "Call Macro" kmacro-call-macro)
      ("a" "Append to Macro" (lambda ()
                               (interactive)
                               (kmacro-start-macro '(4))))
      ("A" "Append w/o Executing" (lambda ()
                                    (interactive)
                                    (kmacro-start-macro '(16))))]
    [ ("E" "Edit Macro" kmacro-edit-macro)
      ("L" "Edit Lossage" kmacro-edit-lossage)
      ("s" "Register Save" kmacro-to-register)
      ("c" "Apply Macro on Lines" apply-macro-to-region-lines)]
    [ ("b" "Bind Last Macro" conn-bind-last-kmacro-to-key)
      ("d" "Name Last Macro" kmacro-name-last-macro)
      ("S" "Step Edit Macro" kmacro-step-edit-macro)]]
  [ :if conn--in-kbd-macro-p
    [ "Commands"
      ("q" "Query" conn-kapply-kbd-macro-query)
      ("d" "Redisplay" kmacro-redisplay)]
    [ :description conn--kmacro-counter-display
      ("i" "Insert Counter" kmacro-insert-counter)
      ("c" "Set Counter" kmacro-set-counter :transient t)
      ("+" "Add to Counter" kmacro-add-counter :transient t)
      ("F" "Set Format" conn--set-counter-format-infix)]])

;;;; Narrow Ring Prefix

(defun conn--narrow-ring-save-state ()
  (list (point) (save-mark-and-excursion--save)
        (point-min) (point-max)
        (and-let* ((_(conn-ring-p conn-narrow-ring))
                   (ring (conn--copy-ring conn-narrow-ring)))
          (setf (conn-ring-copier ring) (pcase-lambda (`(,b . ,e))
                                          (cons (marker-position b)
                                                (marker-position e)))
                ring (conn-copy-ring ring)
                (conn-ring-copier ring) (pcase-lambda (`(,b . ,e))
                                          (cons (copy-marker b)
                                                (copy-marker e))))
          ring)))

(defun conn--narrow-ring-restore-state (state)
  (widen)
  (pcase state
    (`(,point ,mark ,min ,max ,ring)
     (narrow-to-region min max)
     (goto-char point)
     (save-mark-and-excursion--restore mark)
     (conn-clear-narrow-ring)
     (setq conn-narrow-ring (conn-copy-ring ring)))))

(defun conn--format-narrowing (narrowing)
  (if (long-line-optimizations-p)
      (pcase-let ((`(,beg . ,end) narrowing))
        (format "(%s . %s)"
                (marker-position beg)
                (marker-position end)))
    (save-restriction
      (widen)
      (pcase-let ((`(,beg . ,end) narrowing))
        (format "%s+%s"
                (line-number-at-pos (marker-position beg) t)
                (count-lines (marker-position beg)
                             (marker-position end)))))))

(defun conn--narrow-ring-display ()
  (let ((len (length (when (conn-ring-p conn-narrow-ring)
                       (conn-ring-list conn-narrow-ring)))))
    (ignore-errors
      (concat
       (propertize "Narrow Ring: " 'face 'transient-heading)
       (propertize (format "[%s]" len)
                   'face 'transient-value)
       " - "
       (when (> len 2)
         (format "%s, " (conn--format-narrowing
                         (conn-ring-tail conn-narrow-ring))))
       (when (> len 0)
         (pcase (conn-ring-head conn-narrow-ring)
           ('nil (propertize "nil" 'face 'transient-value))
           ((and reg `(,beg . ,end)
                 (guard (and (= (point-min) beg)
                             (= (point-max) end))))
            (propertize (conn--format-narrowing reg)
                        'face 'transient-value))
           (reg
            (propertize (conn--format-narrowing reg)
                        'face 'bold))))
       (when (> len 1)
         (format ", %s" (conn--format-narrowing
                         (cadr (conn-ring-list conn-narrow-ring)))))))))

;;;###autoload (autoload 'conn-narrow-ring-prefix "conn-transients" nil t)
(transient-define-prefix conn-narrow-ring-prefix ()
  "Transient menu for narrow ring function."
  [ :description conn--narrow-ring-display
    [ ("m" "Merge" conn-merge-narrow-ring :transient t)
      ("w" "Widen"
       (lambda ()
         (interactive)
         (widen)
         (conn-recenter-on-region)))
      ("c" "Clear" conn-clear-narrow-ring)
      ("j" "Add Region" conn-thing-to-narrow-ring)]
    [ ("n" "Cycle Next" conn-cycle-narrowings :transient t)
      ("p" "Cycle Previous"
       (lambda (arg)
         (interactive "p")
         (conn-cycle-narrowings (- arg)))
       :transient t)
      ("d" "Pop" conn-pop-narrow-ring :transient t)
      ("a" "Abort Cycling"
       (lambda ()
         (interactive)
         (conn--narrow-ring-restore-state (oref transient-current-prefix scope))))]
    [ ("N" "Narrow Indirect"
       (lambda ()
         (interactive)
         (let ((beg (point-min))
               (end (point-max))
               (buf (current-buffer))
               (win (selected-window)))
           (widen)
           (conn--narrow-indirect-to-region beg end)
           (with-current-buffer buf
             (if (eq (window-buffer win) buf)
                 (with-selected-window win
                   (conn--narrow-ring-restore-state (oref transient-current-prefix scope)))
               (conn--narrow-ring-restore-state (oref transient-current-prefix scope)))))))]]
  (interactive)
  (transient-setup 'conn-narrow-ring-prefix nil nil
                   :scope (conn--narrow-ring-save-state)))

;;;; Register Prefix

;;;###autoload (autoload 'conn-register-prefix "conn-transients" nil t)
(transient-define-prefix conn-register-prefix ()
  "Transient menu for register functions."
  [[ :description "Register"
     ("e" "Load" conn-register-load)
     ("u" "Unset" conn-unset-register)
     ("+" "Set Separator" conn-set-register-separator)
     ("i" "Increment" increment-register)
     ("L" "List" list-registers)]
   [ :description "Register Store"
     (">" "Point" point-to-register)
     ("r" "Rectangle" copy-rectangle-to-register)
     ("a" "Command" conn-command-to-register)
     ("b" "Buffer" buffer-to-register :if (lambda () (<= 31 emacs-major-version)))
     ("o" "File" file-to-register :if (lambda () (<= 31 emacs-major-version)))]
   [ ""
     ("f" "Dispatch" conn-last-dispatch-to-register)
     ("k" "Keyboard Macro" kmacro-to-register)
     ("t" "Tab" conn-tab-to-register)
     ("4" "Window Configuration" window-configuration-to-register)
     ("5" "Frameset" frameset-to-register)]
   [ "Bookmarks"
     ("l" "List" (lambda ()
                   (interactive)
                   ;; Do this so that called-interactively will
                   ;; return t in bookmark-bmenu-list.
                   (call-interactively #'bookmark-bmenu-list)))
     ("m" "Set" bookmark-set)
     ("M" "Push" (lambda ()
                   (interactive)
                   (bookmark-set-no-overwrite nil t)))
     ("j" "Jump" bookmark-jump)]])

;;;; Fill Prefix

(transient-define-infix conn--set-fill-column-infix ()
  "Set `fill-column'."
  :class 'transient-lisp-variable
  :variable 'fill-column
  :set-value (lambda (_ val) (set-fill-column val))
  :reader (lambda (&rest _)
            (read-number (format "Change fill-column from %s to: " fill-column)
                         (current-column))))

(transient-define-infix conn--set-fill-prefix-infix ()
  "Toggle `fill-prefix'."
  :class 'transient-lisp-variable
  :set-value #'ignore
  :variable 'fill-prefix
  :reader (lambda (&rest _)
            (if fill-prefix
                (progn
                  (set-fill-prefix t)
                  nil)
              (set-fill-prefix)
              (substring-no-properties fill-prefix))))

(conn-transient-mode-suffix conn-auto-fill-suffix ()
  "Auto Fill"
  (auto-fill-mode auto-fill-function))

;;;###autoload (autoload 'conn-fill-prefix "conn-transients" nil t)
(transient-define-prefix conn-fill-prefix ()
  "Transient menu for fill functions."
  [ [ "Fill:"
      ("r" "Region" fill-region)
      ("i" "Paragraph" fill-paragraph)
      ("k" "Region as Paragraph" fill-region-as-paragraph)]
    [ "Options:"
      ("c" "Column" conn--set-fill-column-infix)
      ("p" "Prefix" conn--set-fill-prefix-infix)
      ("a" conn-auto-fill-suffix)]])

;;;; Ibuffer

(declare-function ibuffer-format-qualifier "ibuf-ext")
(defvar ibuffer-filtering-qualifiers)

;;;###autoload (autoload 'conn-ibuffer-filter-prefix "conn-transients" nil t)
(transient-define-prefix conn-ibuffer-filter-prefix ()
  "Ibuffer filter prefix"
  [:description
   (lambda ()
     (require 'ibuffer)
     (require 'ibuf-ext)
     (concat
      " "
      (mapconcat
       (lambda (q)
         (propertize (substring (ibuffer-format-qualifier q) 1)
                     'face 'transient-value))
       ibuffer-filtering-qualifiers
       " ")))
   ["Filter"
    ("s" "Save" ibuffer-save-filters :transient t)
    ("x" "Delete Saved" ibuffer-delete-saved-filters :transient t)
    ("/" "Disable" ibuffer-filter-disable :transient t)
    ("r" "Switch To" ibuffer-switch-to-saved-filters :transient t)
    ("p" "Pop" ibuffer-pop-filter :transient t)]
   ["Ops"
    ("!" "Negate" ibuffer-negate-filter :transient t)
    ("+" "And" ibuffer-and-filter :transient t)
    ("*" "Or" ibuffer-or-filter :transient t)
    ("D" "Decompose" ibuffer-decompose-filter :transient t)
    ("t" "Exchange" ibuffer-exchange-filters :transient t)]
   ["Groups"
    ("S" "Save" ibuffer-delete-saved-filter-groups :transient t)
    ("X" "Delete Saved" ibuffer-delete-saved-filter-groups :transient t)
    ("g" "Group" ibuffer-filters-to-filter-group :transient t)
    ("P" "Pop" ibuffer-pop-filter-group :transient t)
    ("R" "Switch To" ibuffer-switch-to-saved-filter-groups :transient t)]]
  ["Filter By"
   [("i" "Modified" ibuffer-filter-by-modified :transient t)
    ("m" "Mode" ibuffer-filter-by-mode :transient t)
    ("M" "Derived Mode" ibuffer-filter-by-derived-mode :transient t)
    ("." "Extension" ibuffer-filter-by-file-extension :transient t)
    ("*" "Starred Name" ibuffer-filter-by-starred-name :transient t)]
   [("c" "Content" ibuffer-filter-by-content :transient t)
    ("f" "Filename" ibuffer-filter-by-filename :transient t)
    ("F" "Directory" ibuffer-filter-by-directory :transient t)
    ("n" "Name" ibuffer-filter-by-name :transient t)
    ("v" "Visiting" ibuffer-filter-by-visiting-file :transient t)]
   [("<" "Size" ibuffer-filter-by-size-lt :transient t)
    (">" "Size" ibuffer-filter-by-size-gt :transient t)
    ("e" "Predicate" ibuffer-filter-by-predicate :transient t)
    ("b" "Basename" ibuffer-filter-by-basename :transient t)
    ("E" "Process" ibuffer-filter-by-process :transient t)]
   [("q" "quit" ignore)]])

(provide 'conn-transients)

(with-eval-after-load 'compile
  (declare-function compilation--message->loc "compile")

  (transient-define-suffix conn--kapply-compilation (args)
    "Apply keyboard macro on regions of text with a specified text property."
    :transient 'transient--do-exit
    :key "l"
    :description "Compilation Matches"
    :if (lambda () (derived-mode-p 'compilation-mode))
    (interactive (list (transient-args transient-current-command)))
    (conn--kapply-macro
     (alist-get :kmacro args)
     (conn-kapply-region-iterator
      (save-excursion
        (goto-char (point-min))
        (cl-loop for match = (text-property-search-forward 'compilation-message)
                 while match
                 collect (pcase (compilation--message->loc (prop-match-value match))
                           (`(,col ,line (,file . ,_) . ,_)
                            (with-current-buffer
                                (let ((name (apply #'expand-file-name file)))
                                  (or (get-file-buffer name)
                                      (find-file-noselect name)))
                              (goto-char (point-min))
                              (save-excursion
                                (forward-line (1- line))
                                (forward-char (1- col))
                                (cons (point-marker) (line-end-position))))))))
      'conn--nnearest-first)
     `(conn--kapply-relocate-to-region
       conn--kapply-skip-invisible-regions
       conn--kapply-pulse-region
       ,@(conn--transient-kapply-pipeline-args args))))

  (transient-append-suffix
    'conn-kapply-prefix
    'conn--kapply-match-suffix
    '(conn--kapply-compilation)
    t))

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-transients.el ends here
