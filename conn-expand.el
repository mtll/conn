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

(defcustom conn-expand-pulse-region t
  "Pulse region on expansion when mark is not active"
  :group 'conn
  :type 'boolean)

(defvar conn-expansion-functions nil
  "Functions which provide expansions for `conn-expand'.

Functions should return a list of (BEGIN . END) pairs representing
potential expansions.  Functions may return invalid expansions
(e.g. nil, invalid regions, etc.) and they will be filtered out.")

(defvar-local conn--current-expansions nil)
(defvar-local conn--current-expansions-tick nil)

(defvar-keymap conn-expand-repeat-map
  :repeat t
  "z" 'conn-expand-exchange
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
       (or (and conn--current-expansions
                (region-active-p)
                (cl-loop for (beg . end) in conn--current-expansions
                         when (or (= beg (region-beginning))
                                  (= end (region-end)))
                         return t))
           (member (cons (region-beginning) (region-end))
                   conn--current-expansions))))

(defun conn--expand-create-expansions ()
  (unless (conn--valid-expansions-p)
    (setq conn--current-expansions-tick (buffer-chars-modified-tick)
          conn--current-expansions
          (compat-call sort
                       (thread-last
                         (mapcan #'funcall conn-expansion-functions)
                         (conn--expand-filter-regions))
                       :lessp (lambda (a b)
                                (or (> (car a) (car b))
                                    (< (cdr a) (cdr b))))
                       :in-place t))))

(defun conn-expand-exchange ()
  "Move point to the other end of the current expansion."
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)
    (conn-exchange-mark-command)))

(defun conn-expand (arg)
  "Expend region by semantic units.

If the region is active only the `point' is moved.
Expansions are provided by functions in `conn-expansion-functions'."
  (interactive "P")
  (if (consp arg)
      (progn
        (conn--push-ephemeral-mark)
        (setq arg (log (prefix-numeric-value arg) 4)))
    (setq arg (prefix-numeric-value arg)))
  (conn--expand-create-expansions)
  (if (< arg 0)
      (conn-contract (- arg))
    (dotimes (_ arg)
      (cond ((and (region-active-p)
                  (= (point) (region-beginning)))
             (cl-loop for (beg . _end) in conn--current-expansions
                      when (< beg (point)) return (goto-char beg)
                      finally (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (cl-loop for (_beg . end) in conn--current-expansions
                      when (> end (point)) return (goto-char end)
                      finally (user-error "No more expansions")))
            ((cl-loop for (beg . end) in conn--current-expansions
                      when (or (and (< beg (region-beginning))
                                    (>= end (region-end)))
                               (and (<= beg (region-beginning))
                                    (> end (region-end))))
                      return (progn
                               (goto-char (if (= (point) (region-beginning)) beg end))
                               (conn--push-ephemeral-mark
                                (if (= (point) (region-beginning)) end beg)))
                      finally (user-error "No more expansions"))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))

(defun conn-contract (arg)
  "Contract region by semantic units.

If the region is active only the `point' is moved.
Expansions and contractions are provided by functions in
`conn-expansion-functions'."
  (interactive "P")
  (if (consp arg)
      (setq arg (log (prefix-numeric-value arg) 4))
    (setq arg (prefix-numeric-value arg)))
  (conn--expand-create-expansions)
  (if (< arg 0)
      (conn-expand (- arg))
    (dotimes (_ arg)
      (cond ((and (region-active-p)
                  (= (point) (region-beginning)))
             (cl-loop for (beg . _end) in (reverse conn--current-expansions)
                      when (> beg (point)) return (goto-char beg)
                      finally (user-error "No more expansions")))
            ((and (region-active-p)
                  (= (point) (region-end)))
             (cl-loop for (_beg . end) in (reverse conn--current-expansions)
                      when (< end (point)) return (goto-char end)
                      finally (user-error "No more expansions")))
            ((cl-loop for (beg . end) in (reverse conn--current-expansions)
                      when (or (> beg (region-beginning))
                               (< end (region-end)))
                      return (progn
                               (goto-char (if (= (point) (region-beginning)) beg end))
                               (conn--push-ephemeral-mark (if (= (point) (region-end)) beg end)))
                      finally (user-error "No more contractions"))))))
  (unless (or (region-active-p)
              (not conn-expand-pulse-region)
              executing-kbd-macro)
    (pulse-momentary-highlight-region (region-beginning) (region-end) 'region)))

;;;;; Bounds of expansion

(conn-define-state conn-expand-state (conn-mode-line-face-state)
  "State for expanding."
  :lighter "↔"
  :mode-line-face 'conn-read-thing-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-expand-state)
  "z" 'conn-expand-exchange
  "j" 'conn-contract
  "l" 'conn-expand
  "v" 'conn-toggle-mark-command
  "e" 'end
  "<mouse-3>" 'end
  "<mouse-1>" 'conn-expand
  "S-<mouse-1>" 'conn-contract
  "<escape>" 'end)

(defun conn--read-expand-message (_arg)
  (substitute-command-keys
   (concat
    "\\[conn-expand] expand; "
    "\\[conn-contract] contract; "
    "\\[conn-toggle-mark-command] toggle mark; "
    "\\[end] finish")))

(cl-defmethod conn-bounds-of-subr ((cmd (conn-thing expansion)) arg)
  (call-interactively cmd)
  (conn-eval-with-state 'conn-expand-state
      (conn-make-bounds
       & cmd & arg
       & (oclosure-lambda (conn-state-eval-argument
                           (required t)
                           (name 'conn--read-expand-message))
             (self command)
           (pcase command
             ('conn-expand-exchange
              (conn-expand-exchange)
              self)
             ('conn-contract
              (conn-contract (conn-state-eval-consume-prefix-arg))
              self)
             ('conn-expand
              (conn-expand (conn-state-eval-consume-prefix-arg))
              self)
             ('conn-toggle-mark-command
              (if mark-active
                  (deactivate-mark)
                (activate-mark))
              self)
             ((or 'end 'exit-recursive-edit)
              (conn-set-argument self (cons (region-beginning)
                                            (region-end)))))))
    :prompt "Expansion"
    :prefix arg))

(provide 'conn-expand)
