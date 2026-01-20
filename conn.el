;;; conn.el --- A modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") (transient "0.8.7"))
;; Homepage: https://github.com/mtll/conn
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

;; A modal keybinding mode.

;;; Code:

;;;; Requires

(require 'conn-utils)
(require 'conn-things)
(require 'conn-states)
(require 'conn-kapply)
(require 'conn-expand)
(require 'conn-wincontrol)
(require 'conn-dispatch)
(require 'conn-commands)
(require 'conn-keymaps)
(require 'conn-surround)
(eval-when-compile (require 'cl-lib))

;;;; Mode Definition

(defun conn--clone-buffer-setup ()
  (setq conn-narrow-ring (conn-copy-ring conn-narrow-ring)
        conn-jump-ring (conn-copy-ring conn-jump-ring)
        conn-emacs-state-ring (conn-copy-ring conn-emacs-state-ring)
        conn-mark-state-ring (conn-copy-ring conn-mark-state-ring))
  (cl-loop for e in conn--previous-mark-state
           if (markerp e)
           collect (copy-marker (marker-position e)) into mstate
           else collect e into mstate
           finally do (setq conn--previous-mark-state mstate))
  (setq conn--state-stack nil)
  (let (conn-next-state)
    (conn--run-exit-fns (conn-stack-signal clone)))
  (or (run-hook-with-args-until-success 'conn-setup-state-hook)
      (conn-push-state 'conn-emacs-state)))

(defun conn--setup-keymaps ()
  (if conn-mode
      (progn
        (cl-pushnew 'conn--state-map emulation-mode-map-alists)
        (cl-pushnew 'conn--major-mode-map emulation-mode-map-alists)
        (cl-pushnew 'conn--minor-mode-maps emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (seq-difference '(conn--state-map
                            conn--major-mode-map
                            conn--minor-mode-maps)
                          emulation-mode-map-alists #'eq))))

(define-minor-mode conn-local-mode
  "Minor mode for setting up conn in a buffer."
  :init-value nil
  :lighter (:eval (conn--get-lighter))
  :group 'conn
  :keymap conn-local-mode-map
  (conn--input-method-mode-line)
  (if conn-local-mode
      (progn
        (make-local-variable 'conn-lighter)
        (setq-local conn--state-map (list (list 'conn-local-mode))
                    conn--major-mode-map (list (list 'conn-local-mode)))
        (unless conn-emacs-state-ring
          (setq conn-emacs-state-ring
                (conn-make-ring 8
                                :cleanup (lambda (mk) (set-marker mk nil))
                                :copier #'conn--copy-mark)))
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'isearch-mode-hook 'conn--isearch-input-method nil t)
        (setq conn--input-method current-input-method)
        (conn-setup-state-for-buffer))
    (let (conn-next-state)
      (conn--run-exit-fns (conn-stack-signal exit)))
    (setq conn--state-stack nil)
    (kill-local-variable 'conn-lighter)
    (setq cursor-type t)
    (remove-hook 'input-method-activate-hook #'conn--activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'conn--deactivate-input-method t)
    (remove-hook 'isearch-mode-hook 'conn--isearch-input-method t)
    (when (and conn--input-method (not current-input-method))
      (activate-input-method conn--input-method))))

(defun conn--initialize-buffer ()
  (conn-local-mode 1))

;;;###autoload
(define-globalized-minor-mode conn-mode
  conn-local-mode conn--initialize-buffer
  :group 'conn
  (progn
    (conn--setup-keymaps)
    (if conn-mode
        (progn
          (advice-add 'toggle-input-method :around #'conn--toggle-input-method-ad)
          (add-hook 'clone-buffer-hook #'conn--clone-buffer-setup)
          (add-hook 'clone-indirect-buffer-hook #'conn--clone-buffer-setup))
      (advice-remove 'toggle-input-method #'conn--toggle-input-method-ad)
      (remove-hook 'isearch-mode-end-hook #'conn--isearch-jump-predicate)
      (remove-hook 'clone-buffer-hook #'conn--clone-buffer-setup)
      (remove-hook 'clone-indirect-buffer-hook #'conn--clone-buffer-setup))))

(provide 'conn)

;;; Misc

(cl-pushnew (list nil (concat "^\\s-*("
                              (eval-when-compile
                                (regexp-opt
                                 '("conn-define-state"
                                   "conn-define-mark-command")
                                 t))
                              "\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                  2)
            lisp-imenu-generic-expression :test #'equal)
