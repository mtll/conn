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
(require 'conn-kapply)
(require 'kmacro)
(require 'transient)
(require 'text-property-search)

;;;; Utils

(defmacro conn-transient-mode-suffix (name arglist description mode &rest properties)
  "Define a `transient' suffix for a minor mode.

\(fn NAME ARGLIST DESCRIPTION MODE &rest [KEYWORD VALUE...] BODY)"
  (declare (indent defun))
  (pcase mode
    ((or `(,mode-command ,mode-var)
         (and mode-command mode-var (pred symbolp)))
     (let ((body `((interactive)
                   (,mode-command 'toggle)))
           (props nil))
       (cl-loop for (k . rest) on properties by #'cddr
                do (pcase k
                     ((pred keywordp)
                      (cl-callf2 nconc (list k (car rest)) props))
                     (`(interactive)
                      (cl-return (setq body (cons k rest))))
                     (_ (error "Malformed body"))))
       `(transient-define-suffix ,name ,arglist
          :description (lambda ()
                         (concat ,description " "
                                 (if (bound-and-true-p ,mode-var)
                                     (propertize "(*)" 'face 'transient-value)
                                   (propertize "( )" 'face 'transient-inactive-value))))
          :transient t
          ,@props
          ,@body)))
    (_ (error "Malformed mode"))))

;;;; Kmacro Prefix

(transient-define-infix conn-set-counter-format-infix ()
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
      ("F" "Set Format" conn-set-counter-format-infix)]
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
      ("." "Register Save" kmacro-to-register)
      ("c" "Apply Macro on Lines" apply-macro-to-region-lines)]
    [ ("b" "Bind Last Macro" conn-bind-last-kmacro-to-key)
      ("d" "Name Last Macro" kmacro-name-last-macro)
      ("s" "Step Edit Macro" kmacro-step-edit-macro)]]
  [ :if conn--in-kbd-macro-p
    [ "Commands"
      ("q" "Query" conn-kapply-kbd-macro-query)
      ("d" "Redisplay" kmacro-redisplay)]
    [ :description conn--kmacro-counter-display
      ("i" "Insert Counter" kmacro-insert-counter)
      ("c" "Set Counter" kmacro-set-counter :transient t)
      ("+" "Add to Counter" kmacro-add-counter :transient t)
      ("F" "Set Format" conn-set-counter-format-infix)]])

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
     ("<" "Point" point-to-register)
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

;;;; Narrow Ring Prefix

(defun conn--narrow-ring-save-state ()
  (list (point) (save-mark-and-excursion--save)
        (point-min) (point-max)
        (and-let* ((_(conn-ring-p conn-narrow-ring))
                   (ring (conn-copy-ring conn-narrow-ring)))
          (setf (conn-ring-copier ring) (pcase-lambda ((cl-struct conn-narrowing
                                                                  start
                                                                  end
                                                                  point))
                                          (conn-narrowing
                                           (marker-position start)
                                           (marker-position end)
                                           :point (marker-position point)))
                ring (conn-copy-ring ring)
                (conn-ring-copier ring) #'conn-copy-narrowing)
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
    [ ("e" "End" ignore)
      ("a" "Abort"
       (lambda ()
         (interactive)
         (conn--narrow-ring-restore-state (oref transient-current-prefix scope))))
      ("w" "Widen" conn-widen)
      ("c" "Clear" conn-clear-narrow-ring)]
    [ ("n" "Cycle Next" conn-cycle-narrowings :transient t)
      ("p" "Cycle Previous"
       (lambda (arg)
         (interactive "p")
         (conn-cycle-narrowings (- arg)))
       :transient t)
      ("d" "Pop" conn-pop-narrow-ring :transient t)
      ("m" "Merge" conn-merge-narrow-ring :transient t)]
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
               (conn--narrow-ring-restore-state (oref transient-current-prefix scope)))))))
      ("j" "Add Region" conn-thing-to-narrow-ring)]]
  (interactive)
  (transient-setup 'conn-narrow-ring-prefix nil nil
                   :scope (conn--narrow-ring-save-state)))

;;;; Fill Prefix

(transient-define-infix conn-set-fill-column-infix ()
  "Set `fill-column'."
  :class 'transient-lisp-variable
  :variable 'fill-column
  :set-value (lambda (_ val) (set-fill-column val))
  :reader (lambda (&rest _)
            (read-number (format "Change fill-column from %s to: " fill-column)
                         (current-column))))

(transient-define-infix conn-set-fill-prefix-infix ()
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
      ("c" "Column" conn-set-fill-column-infix)
      ("p" "Prefix" conn-set-fill-prefix-infix)
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
    ("S" "Save" ibuffer-save-filter-groups :transient t)
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
