;;; conn-embark.el --- Conn embark extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") (embark "1.0") conn)
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
;;
;;; Code:

(require 'conn)
(require 'embark)
(require 'outline)

(defgroup conn-embark nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn)

(defcustom conn-embark-alt-default-action-overrides
  '((symbol . describe-symbol)
    (defun . comment-defun)
    (identifier . xref-find-references))
  "`embark-default-action-overrides' for alternate actions."
  :type '(alist :key-type (choice (symbol :tag "Type")
                                  (cons (symbol :tag "Type")
                                        (symbol :tag "Command")))
                :value-type (function :tag "Default action"))
  :group 'conn-embark)

(defcustom conn-embark-alt-key "M-RET"
  "Key for embark-alt-dwim."
  :type 'string
  :group 'conn-embark)

(defun conn-embark-alt--default-action (type)
  "`embark--default-action' for alt actions"
  (or (alist-get (cons type embark--command) conn-embark-alt-default-action-overrides
                 nil nil #'equal)
      (alist-get type conn-embark-alt-default-action-overrides)
      (alist-get t conn-embark-alt-default-action-overrides)
      (keymap-lookup (embark--raw-action-keymap type) conn-embark-alt-key)))

;;;###autoload
(defun conn-embark-alt-dwim (&optional arg)
  "alternate `embark-dwim'."
  (interactive "P")
  (if-let ((targets (embark--targets)))
      (let* ((target
              (or (nth
                   (if (or (null arg) (minibufferp))
                       0
                     (mod (prefix-numeric-value arg) (length targets)))
                   targets)))
             (type (plist-get target :type))
             (default-action (conn-embark-alt--default-action type))
             (action (or (command-remapping default-action) default-action)))
        (unless action
          (user-error "No alt action for %s targets" type))
        (when (and arg (minibufferp)) (setq embark--toggle-quit t))
        (embark--act action
                     (if (and (eq default-action embark--command)
                              (not (memq default-action
                                         embark-multitarget-actions)))
                         (embark--orig-target target)
                       target)
                     (embark--quit-p action)))
    (user-error "No target found.")))

;;;###autoload
(defun conn-embark-dwim-either (&optional arg)
  (interactive "P")
  (if arg (conn-embark-alt-dwim) (embark-dwim)))

(defun conn-narrow-indirect-to-heading ()
  (interactive)
  (outline-mark-subtree)
  (conn--narrow-indirect (region-beginning)
                         (region-end))
  (outline-show-subtree))

(defun conn-embark-conn-bindings ()
  (interactive)
  (embark-bindings-in-keymap (alist-get conn-current-state conn--state-maps)))

(setf (alist-get 'conn-replace-region-substring embark-target-injection-hooks)
      (list #'embark--ignore-target))

(add-to-list 'embark-target-injection-hooks
             '(conn-insert-pair embark--ignore-target))
(add-to-list 'embark-target-injection-hooks
             '(conn-change-pair embark--ignore-target))

(provide 'conn-embark)
;;; conn-embark.el ends here
