;;; conn-smartparens.el --- Smartparens Integration for Conn -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") smartparens conn)
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

(require 'conn)
(require 'smartparens)
(require 'thingatpt)

(defun conn-sp-forward-sexp-op (backward)
  (pcase-let* ((next (sp-get-thing))
               (prev (sp-get-thing t)))
    (cond ((if backward
               (< (sp-get prev :end) (sp-get next :end))
             (> (sp-get next :beg) (sp-get prev :beg)))
           (goto-char (if backward
                          (sp-get prev :beg)
                        (sp-get next :end))))
          ((and (eql (sp-get prev :end) (sp-get next :end))
                (eql (sp-get next :beg) (sp-get prev :beg)))
           (if backward
               (goto-char (sp-get prev :beg))
             (goto-char (sp-get prev :end)))))))

(defun conn-sp-bounds-of-sexp ()
  (pcase-let* ((next (sp-get-thing))
               (prev (sp-get-thing t)))
    (when (or next prev)
      (if (eql (point) (plist-get prev :end))
          (cons (plist-get prev :beg)
                (plist-get prev :end))
        (cons (plist-get next :beg)
              (plist-get next :end))))))

(defun conn-sp-bounds-providers ()
  (if smartparens-mode
      (setq-local bounds-of-thing-at-point-provider-alist
                  (cons (cons 'sexp 'conn-sp-bounds-of-sexp)
                        bounds-of-thing-at-point-provider-alist)
                  forward-thing-provider-alist
                  (cons (cons 'sexp 'conn-sp-forward-sexp-op)
                        forward-thing-provider-alist))
    (setq-local bounds-of-thing-at-point-provider-alist
                (remove '(sexp . conn-sp-bounds-of-sexp)
                        bounds-of-thing-at-point-provider-alist)
                forward-thing-provider-alist
                (remove '(sexp . conn-sp-forward-sexp-op)
                        bounds-of-thing-at-point-provider-alist))))
(add-hook 'smartparens-mode-hook 'conn-sp-bounds-providers)

(define-keymap
  :keymap (conn-get-mode-map 'conn-movement-state 'smartparens-mode)
  "]" 'sp-down-sexp
  "[" 'sp-backward-down-sexp
  ")" 'sp-up-sexp
  "(" 'sp-backward-up-sexp
  "U" 'sp-backward-symbol
  "O" 'sp-forward-symbol)

(conn-register-thing-commands
 'symbol 'conn-symbol-handler
 'sp-forward-symbol 'sp-backward-symbol)

(defun conn-sp-list-handler (beg)
  (cond ((> (point) beg)
         (save-excursion
           (sp-backward-sexp)
           (conn--push-ephemeral-mark (point))))
        ((< (point) beg)
         (save-excursion
           (sp-forward-sexp)
           (conn--push-ephemeral-mark (point))))))

(conn-register-thing-commands
 'list 'conn-sp-list-handler
 'sp-up-sexp 'sp-backward-up-sexp)

(defun conn-sp-down-list-handler (beg)
  (cond ((> (point) beg)
         (let ((pt (point)))
           (save-excursion
             (sp-end-of-sexp)
             (when (= pt (point))
               (sp-beginning-of-sexp))
             (conn--push-ephemeral-mark (point)))))
        ((< (point) beg)
         (let ((pt (point)))
           (save-excursion
             (sp-beginning-of-sexp)
             (when (= pt (point))
               (sp-end-of-sexp))
             (conn--push-ephemeral-mark (point)))))
        ((= (point) beg)
         (when-let ((enc (sp-get-enclosing-sexp)))
           (if (eql (point) (sp-get enc :beg-in))
               (conn--push-ephemeral-mark (sp-get enc :end-in))
             (conn--push-ephemeral-mark (sp-get enc :beg-in)))))))

(conn-register-thing-commands
 'list 'conn-sp-down-list-handler
 'sp-down-sexp 'sp-backward-down-sexp
 'sp-beginning-of-sexp 'sp-end-of-sexp)

(defun conn-sp-sexp-handler (beg)
  (unless (= (point) beg)
    (save-excursion
      (cond ((< (point) beg)
             (let ((beg-sexp (save-excursion
                               (goto-char beg)
                               (min (point) (plist-get (sp-get-thing t) :end)))))
               (while (and (< (point) beg-sexp) (sp-forward-sexp)))))
            ((> (point) beg)
             (let ((beg-sexp (save-excursion
                               (goto-char beg)
                               (max (point) (plist-get (sp-get-thing) :beg)))))
               (while (and (> (point) beg-sexp) (sp-backward-sexp))))))
      (conn--push-ephemeral-mark))))

(conn-register-thing-commands
 'sexp 'conn-sp-sexp-handler
 'sp-forward-sexp 'sp-backward-sexp
 'sp-forward-parallel-sexp 'sp-backward-parallel-sexp)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'sp-down-sexp
                     'sp-backward-down-sexp
                     'sp-up-sexp
                     'sp-backward-up-sexp
                     'sp-backward-sexp
                     'sp-forward-sexp))

(provide 'conn-smartparens)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-smartparens.el ends here
