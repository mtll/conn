;;; conn-expreg.el -*- lexical-binding: t -*-
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
;;; Code:

(require 'conn-mode)
(require 'expreg)

(defcustom conn-expreg-always-use-region-states
  '(conn-state dot-state)
  "States in which to expand from the current point and mark.")

(defcustom conn-expreg-leave-region-active
  nil
  "Whether to leave the region active after expreg commands when the
current state is in `conn-expreg-always-use-region-states'.")

(define-keymap
  :keymap conn-common-map
  "b" 'expreg-expand)

(defvar-keymap conn-expreg-repeat-map
  "B" 'expreg-contract)

(defun conn--expreg-advice (fn &rest args)
  (let ((always-use-region
         (seq-find #'identity conn-expreg-always-use-region-states)))
    (when always-use-region (activate-mark t))
    (unwind-protect
        (apply fn args)
      (when (and always-use-region conn-expreg-leave-region-active)
        (deactivate-mark)))))

(conn-define-extension conn-expreg-always-use-region
  (if conn-expreg-always-use-region
      (progn
        (advice-add 'expreg-expand :around 'conn--expreg-advice)
        (advice-add 'expreg-contract :around 'conn--expreg-advice)
        (put 'expreg-contract 'repeat-map 'conn-expreg-repeat-map)
        (put 'expreg-expand 'repeat-map 'conn-expreg-repeat-map)
        (put 'expreg-expand 'repeat-check-key 'no))
    (advice-remove 'expreg-expand 'conn--expreg-advice)
    (advice-remove 'expreg-contract 'conn--expreg-advice)
    (put 'expreg-contract 'repeat-map nil)
    (put 'expreg-expand 'repeat-map nil)
    (put 'expreg-expand 'repeat-check-key nil)))

(provide 'conn-expreg)
