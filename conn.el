;;; conn.el --- A modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: conn.el
;; Description: A modal keybinding mode
;; Author: David Feller
;; Keywords: convenience, editing
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") (transient "0.8.7") (seq "2.23"))
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
(require 'conn-mark)
(require 'conn-states)
(require 'conn-kapply)
(require 'conn-expand)
(require 'conn-wincontrol)
(require 'conn-dispatch)
(require 'conn-commands)
(require 'conn-keymaps)
(eval-when-compile (require 'cl-lib))

;;;; Advice

(defun conn--toggle-input-method-ad (&rest app)
  (if (and conn-local-mode
           (not isearch-mode)
           (conn-state-get conn-current-state :suppress-input-method)
           conn--input-method)
      (unwind-protect
          (progn
            (remove-hook 'input-method-activate-hook 'conn--activate-input-method t)
            (activate-input-method conn--input-method)
            (deactivate-input-method))
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t))
    (apply app)))

(defun conn--read-from-suggestions-ad (&rest app)
  (if (and (mark t)
           (not (use-region-p))
           ;; Maybe not when its extremely large
           (> 10000 (abs (- (point) (mark t)))))
      (cons (buffer-substring-no-properties
             (region-beginning) (region-end))
            (apply app))
    (apply app)))

(defun conn--push-mark-ad (&rest _)
  (unless (or conn--ephemeral-mark
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring (mark-marker)))
  (setq conn--ephemeral-mark nil))

(defun conn--pop-mark-ad (&rest _)
  (unless (or conn--ephemeral-mark
              (null (marker-position (mark-marker))))
    (conn--push-mark-ring (mark-marker) t))
  (setq conn--ephemeral-mark t))

(defun conn--set-mark-ad (&rest _)
  (setq conn--ephemeral-mark nil))

(defvar conn--saved-ephemeral-marks nil)

(defun conn--save-ephemeral-mark-ad (&rest _)
  (push conn--ephemeral-mark conn--saved-ephemeral-marks))

(defun conn--restore-ephemeral-mark-ad (&rest _)
  (setq conn--ephemeral-mark (pop conn--saved-ephemeral-marks)))

(defun conn--setup-advice ()
  (if conn-mode
      (progn
        (advice-add 'toggle-input-method :around #'conn--toggle-input-method-ad)
        (advice-add 'query-replace-read-from-suggestions :around
                    #'conn--read-from-suggestions-ad)
        (advice-add 'read-regexp-suggestions :around
                    #'conn--read-from-suggestions-ad)
        (advice-add 'push-mark :before #'conn--push-mark-ad)
        (advice-add 'pop-mark :before #'conn--pop-mark-ad)
        (advice-add 'set-mark :after #'conn--set-mark-ad)
        (advice-add 'save-mark-and-excursion--save :before
                    #'conn--save-ephemeral-mark-ad)
        (advice-add 'save-mark-and-excursion--restore :after
                    #'conn--restore-ephemeral-mark-ad))
    (advice-remove 'toggle-input-method #'conn--toggle-input-method-ad)
    (advice-remove 'query-replace-read-from-suggestions
                   #'conn--read-from-suggestions-ad)
    (advice-remove 'read-regexp-suggestions
                   #'conn--read-from-suggestions-ad)
    (advice-remove 'set-mark #'conn--set-mark-ad)
    (advice-remove 'pop-mark #'conn--pop-mark-ad)
    (advice-remove 'push-mark #'conn--push-mark-ad)
    (advice-remove 'save-mark-and-excursion--save #'conn--save-ephemeral-mark-ad)
    (advice-remove 'save-mark-and-excursion--restore #'conn--restore-ephemeral-mark-ad)))


;;;; Mode Definition

(defun conn--clone-buffer-setup ()
  (setq conn--mark-cursor nil)
  (dolist (ov (when (mark t)
                (conn--overlays-in-of-type (mark t) (1+ (mark t))
                                           'conn--mark-cursor)))
    (delete-overlay ov))
  (setq conn-narrow-ring (conn-copy-ring conn-narrow-ring)
        conn-movement-ring (conn-copy-ring conn-movement-ring)
        conn-mark-ring (conn-copy-ring conn-mark-ring)
        conn-emacs-state-ring (conn-copy-ring conn-emacs-state-ring)
        conn--state-stack (copy-sequence conn--state-stack)))

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
        (setq conn-current-state nil)
        (kill-local-variable 'conn--state-stack)
        (make-local-variable 'conn-lighter)
        (setq-local conn--state-map (list (list 'conn-local-mode))
                    conn--major-mode-map (list (list 'conn-local-mode))
                    conn-emacs-state-ring
                    (conn-make-ring 8
                                    :cleanup (lambda (mk) (set-marker mk nil))
                                    :copier (lambda (mk)
                                              (copy-marker (marker-position mk)))))
        ;; We would like to be able to do the same to
        ;; query-replace-read-from-regexp-default but it must be
        ;; either nil, a string, a list of strings, or a symbol with a
        ;; function definition.
        (if query-replace-read-from-default
            (add-function :around
                          (local 'query-replace-read-from-default)
                          (lambda (fn &rest args)
                            (or (conn-replace-read-default)
                                (when fn (apply fn args))))
                          `((name . conn-replace-default)))
          (setq query-replace-read-from-default 'conn-replace-read-default))
        (unless (mark t)
          (conn--push-ephemeral-mark (point) t nil))
        (add-hook 'change-major-mode-hook #'conn--clear-overlays nil t)
        (add-hook 'input-method-activate-hook #'conn--activate-input-method nil t)
        (add-hook 'input-method-deactivate-hook #'conn--deactivate-input-method nil t)
        (add-hook 'isearch-mode-hook 'conn--isearch-input-method nil t)
        (setq conn--input-method current-input-method)
        (or (run-hook-with-args-until-success 'conn-setup-state-hook)
            (conn-push-state 'conn-emacs-state)))
    (conn--run-defered)
    (conn--clear-overlays)
    (setq cursor-type t)
    (if (eq 'conn-replace-read-default query-replace-read-from-default)
        (setq query-replace-read-from-default 'conn-replace-read-default)
      (remove-function (local 'query-replace-read-from-default)
                       'conn-replace-default))
    (remove-hook 'change-major-mode-hook #'conn--clear-overlays t)
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
    (conn--setup-mark)
    (conn--setup-advice)
    (if conn-mode
        (progn
          ;; TODO: don't do this unconditionally
          (keymap-set minibuffer-mode-map "M-Y" 'conn-yank-region-to-minibuffer)
          (add-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook -50)
          (add-hook 'clone-buffer-hook 'conn--clone-buffer-setup)
          (add-hook 'clone-indirect-buffer-hook 'conn--clone-buffer-setup))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook)
      (remove-hook 'clone-buffer-hook 'conn--clone-buffer-setup)
      (remove-hook 'clone-indirect-buffer-hook 'conn--clone-buffer-setup)
      (when (eq (keymap-lookup minibuffer-mode-map "M-Y")
                'conn-yank-region-to-minibuffer)
        (keymap-unset minibuffer-mode-map "M-Y"))
      (remove-hook 'minibuffer-setup-hook 'conn--yank-region-to-minibuffer-hook))))

(define-minor-mode conn-emacs-state-operators-mode
  "Bind conn operators in conn-emacs-state."
  :global t
  :group 'conn)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-emacs-state 'conn-emacs-state-operators-mode)
  "C-w" 'conn-kill-thing
  "M-w" 'conn-copy-thing
  ;; "C-." 'conn-change-thing
  ;; "C-," 'conn-dispatch
  "C-t" 'conn-transpose-things)

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
