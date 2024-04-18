;;; conn-embark.el --- Conn embark extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") (embark "1.0") conn-mode)
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

(require 'conn-mode)
(require 'embark)

(defgroup conn-embark nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn-mode)

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

(defcustom conn-complete-keys-toggle-display-keys
  "M-j"
  "Keys bound in `conn-complete-keys' to toggle between tree and flat display.
Value must satisfy `key-valid-p'."
  :type 'string
  :group 'conn-embark)

(defun conn-complete-keys--get-bindings (prefix map)
  (let ((prefix-map (if (= 0 (seq-length prefix))
                        map
                      (keymap-lookup map (key-description prefix))))
        binds)
    (cond
     ((or (null prefix-map) (numberp prefix-map)))
     ((keymapp prefix-map)
      (map-keymap
       (lambda (key def)
         (cond
          ((and (numberp key)
                (= key 27)
                (keymapp def))
           (map-keymap
            (lambda (key2 def2)
              (unless (memq def (list 'undefined 'self-insert-command 'digit-argument
                                      'negative-argument 'embark-keymap-help nil))
                (push (cons (vconcat (vector key key2)) def2) binds)))
            def))
          (t (push (cons (vector key) def) binds))))
       (keymap-canonicalize prefix-map))))
    (nreverse binds)))

;; `embark--formatted-bindings' almost
(defun conn-complete-keys--formatted-bindings (map)
  "Return the formatted keybinding of KEYMAP.
The keybindings are returned in their order of appearance.
If NESTED is non-nil subkeymaps are not flattened."
  (let* ((commands
          (cl-loop for (key . def) in map
                   for name = (embark--command-name def)
                   for cmd = (keymap--menu-item-binding def)
                   unless (memq cmd '(nil embark-keymap-help
                                          negative-argument
                                          digit-argument
                                          self-insert-command
                                          undefined))
                   collect (list name cmd key
                                 (concat
                                  (if (eq (car-safe def) 'menu-item)
                                      "menu-item"
                                    (key-description key))))))
         (width (cl-loop for (_name _cmd _key desc) in commands
                         maximize (length desc)))
         (candidates
          (cl-loop for item in commands
                   for (name cmd _key desc) = item
                   for desc-rep =
                   (concat
                    (propertize desc 'face 'embark-keybinding)
                    (and (embark--action-repeatable-p cmd)
                         embark-keybinding-repeat))
                   for formatted =
                   (propertize
                    (concat desc-rep
                            (make-string (- width (length desc-rep) -1) ?\s)
                            name)
                    'embark-command cmd)
                   ;; when (equal key [13]) do (setq default formatted)
                   collect (cons formatted item))))
    candidates))

(defun conn-complete-keys--up ()
  (interactive)
  (delete-minibuffer-contents)
  (exit-minibuffer))

(defun conn--active-maps (maps)
  (pcase (car maps)
    ('keymap maps)
    ((pred consp)
     (seq-mapcat #'conn--active-maps maps))
    ((and (pred boundp) (pred symbol-value))
     (conn--active-maps (cdr maps)))))

;;;###autoload
(defun conn-complete-keys (prefix map)
  "Complete key sequence beginning with current keys using
`completing-read'.  When called via \\[conn-complete-keys] and with a
prefix argument restrict completion to key bindings defined by
`conn-mode'. `conn-complete-keys-toggle-display-keys' toggles between
a tree view and the embark flat view. In the default tree view DEL
will navigate up out of a keymap."
  (interactive
   (let* ((prefix (seq-subseq (this-command-keys-vector) 0 -1)))
     (list prefix
           (if current-prefix-arg
               (make-composed-keymap
                (conn--active-maps (list conn--transition-maps
                                         conn--local-mode-maps
                                         conn--major-mode-maps
                                         conn--local-maps
                                         conn--aux-maps
                                         conn--state-maps
                                         conn-global-map)))
             (make-composed-keymap (current-active-maps t))))))
  (let* ((tree (lambda ()
                 (interactive)
                 (embark--quit-and-run
                  (lambda ()
                    (conn-complete-keys prefix map)))))
         (flat (lambda ()
                 (interactive)
                 (embark--quit-and-run
                  (lambda ()
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (use-local-map
                           (define-keymap
                             :parent (current-local-map)
                             conn-complete-keys-toggle-display-keys tree)))
                      (embark-bindings-in-keymap
                       (if (seq-empty-p prefix)
                           map
                         (keymap-lookup map (key-description prefix)))))))))
         prompt choice cand return)
    (while (not return)
      (setq cand (conn-complete-keys--formatted-bindings
                  (conn-complete-keys--get-bindings prefix map))
            prompt (if (> (length prefix) 0)
                       (concat "Command: " (key-description prefix) "- ")
                     "Command: ")
            choice (minibuffer-with-setup-hook
                       (:append
                        (lambda ()
                          (use-local-map
                           (define-keymap
                             :parent (current-local-map)
                             "M-<backspace>" 'conn-complete-keys--up
                             "M-DEL" 'conn-complete-keys--up
                             conn-complete-keys-toggle-display-keys flat))))
                     (completing-read
                      prompt
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            `(metadata (display-sort-function . ,(lambda (c) (sort c 'string<)))
                                       (category . embark-keybinding))
                          (complete-with-action action cand string predicate)))
                      nil nil)))
      (pcase-exhaustive (assoc choice cand)
        ((and 'nil (guard (string= choice "")))
         (setq prefix (ignore-errors (seq-subseq prefix 0 -1)))
         (when (and (> (length prefix) 0)
                    (= 27 (elt prefix (1- (length prefix)))))
           ;; This was a meta bind so we need to
           ;; remove the ESC key as well
           (setq prefix (ignore-errors (seq-subseq prefix 0 -1)))))
        ('nil (setq return t))
        ((and `(,_ ,_ ,cmd . ,_)
              (guard (commandp (keymap--menu-item-binding cmd))))
         (setq return t)
         (call-interactively cmd))
        ((and `(,_ ,_ ,cmd ,key . ,_)
              (guard (keymapp cmd)))
         (setq prefix (vconcat prefix key)))))))

;;;###autoload
(defun conn-complete-conn-keys ()
  (interactive)
  (conn-complete-keys
   "" (make-composed-keymap
       (conn--active-maps (list conn--transition-maps
                                conn--local-mode-maps
                                conn--major-mode-maps
                                conn--local-maps
                                conn--aux-maps
                                conn--state-maps
                                conn-global-map)))))

(defvar conn-complete-keys--prefix-cmd-backup nil)

;;;###autoload (autoload 'conn-complete-keys-prefix-help-command "conn-embark" nil t)
(conn-define-extension conn-complete-keys-prefix-help-command
  (if conn-complete-keys-prefix-help-command
      (progn
        (setq conn-complete-keys--prefix-cmd-backup prefix-help-command)
        (setq prefix-help-command 'conn-complete-keys))
    (when (eq prefix-help-command 'conn-complete-keys)
      (setq prefix-help-command conn-complete-keys--prefix-cmd-backup))))

(defun conn-repeat-prefix-help-command (prefix map global)
  (interactive
   (let* ((prefix (seq-subseq (this-command-keys-vector) 0 -1)))
     (list prefix
           (key-binding prefix 'accept-default)
           (make-composed-keymap (current-active-maps t)))))
  (let ((quit-fn #'ignore)
        (transient-map (make-composed-keymap (list map))))
    (keymap-set transient-map
                "C-h" (lambda ()
                        (interactive)
                        (funcall quit-fn)
                        (conn-complete-keys prefix global)
                        (conn-repeat-prefix-help-command prefix map global)))
    (setq quit-fn (set-transient-map transient-map t nil t))))

;;;###autoload (autoload 'conn-repeat-keys-prefix-help-command "conn-embark" nil t)
(conn-define-extension conn-repeat-keys-prefix-help-command
  (if conn-repeat-keys-prefix-help-command
      (progn
        (setq conn-complete-keys--prefix-cmd-backup prefix-help-command)
        (setq prefix-help-command 'conn-repeat-prefix-help-command))
    (when (eq prefix-help-command 'conn-repeat-prefix-help-command)
      (setq prefix-help-command conn-complete-keys--prefix-cmd-backup))))

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

(keymap-set conn-global-map "M-S-<iso-lefttab>"   'conn-complete-keys)
(keymap-set conn-global-map "C-M-S-<iso-lefttab>" 'conn-complete-conn-keys)

(setf (alist-get 'conn-replace-region-substring embark-target-injection-hooks)
      (list #'embark--ignore-target))

(add-to-list 'embark-target-injection-hooks
             '(conn-insert-pair embark--ignore-target))
(add-to-list 'embark-target-injection-hooks
             '(conn-change-pair embark--ignore-target))

(provide 'conn-embark)
;;; conn-embark.el ends here
