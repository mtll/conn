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
(eval-when-compile
  (require 'cl-lib))

(declare-function conn-exchange-mark-command "conn-commands")

;;;; Expand Region

(defvar conn-expansion-functions nil
  "Functions which provide expansions for `conn-expand'.

Functions should return a list of (BEGIN . END) pairs representing
potential expansions.  Functions may return invalid expansions
(e.g. nil, invalid regions, etc.) and they will be filtered out.")

(defvar-local conn--current-expansions nil)
(defvar-local conn--current-expansions-tick nil)

(defvar-keymap conn-expand-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "z" 'exchange-point-and-mark
  "j" 'conn-contract
  "h" 'conn-expand
  "l" 'conn-expand)

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
                        (set-mark (if (= (point) (region-end)) beg end))
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
  "h" 'conn-expand
  "e" 'end
  "<mouse-3>" 'end
  "<mouse-1>" 'conn-expand
  "S-<mouse-1>" 'conn-contract
  "<escape>" 'end)

(cl-defmethod conn-bounds-of ((cmd (conn-thing expansion))
                              arg)
  (cl-flet ((command-handler (command)
              (condition-case err
                  (pcase command
                    ('exchange-point-and-mark
                     (exchange-point-and-mark)
                     (conn-read-args-handle))
                    ('conn-contract
                     (conn-contract-subr
                      (prefix-numeric-value
                       (conn-read-args-consume-prefix-arg)))
                     (conn-read-args-handle))
                    ('conn-expand
                     (conn-expand-subr
                      (prefix-numeric-value
                       (conn-read-args-consume-prefix-arg)))
                     (conn-read-args-handle)))
                (user-error
                 (conn-read-args-error (error-message-string err)))))
            (display (prompt args &optional teardown)
              (if teardown
                  (unless executing-kbd-macro
                    (message nil))
                (message
                 (substitute-command-keys
                  (concat
                   (conn--read-args-prompt prompt args)
                   "\n"
                   "\\[conn-expand] expand; "
                   "\\[conn-contract] contract; "
                   "\\[end] finish"))))))
    (save-excursion
      (push-mark nil t)
      (conn-expand-subr (prefix-numeric-value arg))
      (conn-read-args (conn-expand-state
                       :prompt "Expansion"
                       :prefix arg
                       :display-handler #'display
                       :command-handler #'command-handler)
          ((bounds
            (oclosure-lambda (conn-anonymous-argument
                              (required t))
                (_self command updater)
              (pcase command
                ((or 'end 'exit-recursive-edit)
                 (funcall updater
                          (conn-argument (cons (region-beginning)
                                               (region-end)))))))))
        (deactivate-mark)
        (conn-make-bounds cmd arg bounds)))))

(provide 'conn-expand)
