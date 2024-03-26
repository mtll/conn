;;; conn-expreg.el --- Expreg extension for Conn Mode -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") expreg conn-mode)
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

(defgroup conn-expreg nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn-mode)

(defcustom conn-expreg-always-use-region-states
  '(conn-state dot-state)
  "States in which to expand from the current point and mark."
  :type '(list symbol)
  :group 'conn-expreg)

(defcustom conn-expreg-leave-region-active
  t
  "Whether to leave the region active after expreg commands when the
current state is in `conn-expreg-always-use-region-states'."
  :type 'boolean
  :group 'conn-expreg)

(defun conn--expreg-advice (fn &rest args)
  (let ((always-use-region
         (seq-find #'identity conn-expreg-always-use-region-states))
        (active (region-active-p)))
    (when always-use-region (activate-mark t))
    (unwind-protect
        (apply fn args)
      (when (and (not (or active conn-expreg-leave-region-active))
                 always-use-region)
        (deactivate-mark)))))

;;;###autoload (autoload 'conn-expreg-always-use-region "conn-expreg" nil t)
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
;;; conn-expreg.el ends here
