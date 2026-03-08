;;; conn-expand.el -*- lexical-binding: t -*-
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

(require 'compat)
(require 'conn-things)
(require 'conn-dispatch)

(declare-function conn-exchange-mark-command "conn-commands")

;;;; Expand Region

(defvar conn-expansion-functions nil
  "Functions which provide expansions for `conn-expand'.

Functions should return a list of (BEGIN . END) pairs representing
potential expansions.  Functions may return invalid expansions
(e.g. nil, invalid regions, etc.) and they will be filtered out.")

(defvar-local conn--current-expansions nil)
(defvar-local conn--current-expansions-tick nil)

(conn-register-thing 'expansion)

(defun conn--expand-filter-regions (regions)
  (declare (important-return-value t))
  (let (result)
    (pcase-dolist ((and reg `(,beg . ,end))
                   (delete-dups regions))
      (when (and beg end
                 (/= beg end)
                 (<= beg (point) end))
        (push reg result)))
    result))

(defun conn--valid-expansions-p ()
  (declare (important-return-value t)
           (side-effect-free t))
  (and (eql conn--current-expansions-tick (buffer-chars-modified-tick))
       conn--current-expansions
       (region-active-p)
       (member (cons (region-beginning) (region-end))
               conn--current-expansions)))

(defun conn--expand-create-expansions ()
  (unless (conn--valid-expansions-p)
    (prog1
        (setq conn--current-expansions
              (compat-call sort
                           (thread-first
                             (mapcan #'funcall conn-expansion-functions)
                             (conn--expand-filter-regions))
                           :lessp (lambda (a b)
                                    (or (> (car a) (car b))
                                        (< (cdr a) (cdr b))))
                           :in-place t))
      (setq conn--current-expansions-tick (buffer-chars-modified-tick)))))

(defun conn-expand-subr (arg)
  (conn--expand-create-expansions)
  (if (< arg 0)
      (conn-contract (- arg))
    (dotimes (_ arg)
      (cl-loop for (beg . end) in conn--current-expansions
               when (> (abs (- end beg))
                       (abs (- (region-end) (region-beginning))))
               return (progn
                        (goto-char (if (= (point) (region-beginning)) beg end))
                        (set-mark (if (= (point) (region-beginning)) end beg))
                        (unless (region-active-p) (activate-mark)))
               finally (user-error "No more expansions")))))

(defun conn-expand (arg)
  "Expend region by semantic units.

If the region is active only the `point' is moved.
Expansions are provided by functions in `conn-expansion-functions'."
  (interactive "p")
  (unless (and (region-active-p)
               (conn--valid-expansions-p))
    (push-mark nil t t))
  (conn-expand-subr arg)
  (unless conn-mark-state
    (conn-push-state 'conn-mark-state)))

(defun conn-contract-subr (arg)
  (unless (conn--valid-expansions-p)
    (user-error "No expansion in progress"))
  (if (< arg 0)
      (conn-expand (- arg))
    (dotimes (_ arg)
      (cl-loop for (beg . end) in (reverse conn--current-expansions)
               when (or (> beg (region-beginning))
                        (< end (region-end)))
               return (progn
                        (goto-char (if (= (point) (region-beginning)) beg end))
                        (set-mark (if (= (point) (region-end)) beg end))
                        (unless (region-active-p) (activate-mark)))
               finally (user-error "No more contractions")))))

(defun conn-contract (arg)
  "Contract region by semantic units.

If the region is active only the `point' is moved.
Expansions and contractions are provided by functions in
`conn-expansion-functions'."
  (interactive "p")
  (conn-contract-subr arg)
  (unless conn-mark-state
    (conn-push-state 'conn-mark-state)))

;;;;; Bounds of expansion

(conn-define-state conn-expand-state (conn-mode-line-face-state)
  "State for expanding."
  :lighter "EXPAND"
  :mode-line-face 'conn-read-thing-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-expand-state)
  "M-DEL" 'reset-arg
  "z" 'exchange-point-and-mark
  "j" 'conn-contract
  "l" 'conn-expand
  "e" 'end
  "<mouse-3>" 'end
  "<mouse-1>" 'conn-expand
  "S-<mouse-1>" 'conn-contract
  "<escape>" 'end)

(cl-defmethod conn-bounds-of ((_cmd (conn-thing expansion))
                              _arg)
  (conn--expand-create-expansions)
  (let ((bounds))
    (conn-dispatch-setup
     (oclosure-lambda (conn-action
                       (action-description "Bounds")
                       (action-not-repeatable t))
         ()
       (pcase-let ((`(,pt ,window ,thing ,_arg ,_transform)
                    (conn-select-target)))
         (unless (eq window (selected-window))
           (error "Window not selected window"))
         (save-excursion
           (goto-char pt)
           (setf bounds (conn-bounds-of thing nil)))))
     (conn-anonymous-thing
       '(expansion)
       :pretty-print ( :method (_) "expansion")
       :target-finder (:method (_self _arg) (conn-expansion-targets)))
     nil nil
     :other-end :no-other-end)
    bounds))

(conn-define-target-finder conn-expansion-targets
    (conn-dispatch-focus-mixin)
    ((expansions :initform nil)
     (context-lines
      :initform 1
      :initarg :context-lines)
     (window-predicate
      :initform (lambda (win) (eq win (selected-window)))))
  ( :default-update-handler (state &optional len)
    (let* ((expansions (oref state expansions))
           (bounds nil)
           (thing
            (conn-anonymous-thing
              '(region)
              :multi-thing-p (lambda (_self target)
                               (length> (alist-get (overlay-start target) bounds)
                                        1))
              :pretty-print ( :method (self) "Expansion")
              :bounds-op ( :method (_ _)
                           (conn-multi-thing-select
                            (alist-get (point) bounds))))))
      (unless expansions
        (setf expansions (conn--expand-create-expansions)
              (oref state expansions) expansions))
      (pcase-dolist ((and cons `(,beg . ,end)) expansions)
        (push (conn-make-bounds 'region nil cons)
              (alist-get beg bounds))
        (conn-make-target-overlay beg 0 :thing thing)))))

(provide 'conn-expand)
