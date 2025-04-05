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

(defun conn-sp-forward-sexp-op (arg)
  (let ((backward (< arg 0))
        (N (abs arg)))
    (dotimes (_ N)
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
                 (goto-char (sp-get prev :end))))
              (t (signal 'scan-error "No more sexp")))))))

(defun conn-sp-bounds-of-sexp ()
  (pcase-let* ((next (sp-get-thing))
               (prev (sp-get-thing t)))
    (when (or next prev)
      (if (eql (point) (plist-get prev :end))
          (cons (plist-get prev :beg)
                (plist-get prev :end))
        (cons (plist-get next :beg)
              (plist-get next :end))))))

(define-keymap
  :keymap (conn-get-mode-map 'conn-movement-state 'smartparens-mode)
  "]" 'sp-down-sexp
  "[" 'sp-backward-down-sexp
  ")" 'sp-up-sexp
  "(" 'sp-backward-up-sexp
  "U" 'sp-backward-symbol
  "O" 'sp-forward-symbol)

(define-keymap
  :keymap (conn-get-mode-map 'conn-read-thing-common-state 'smartparens-mode)
  "m" 'forward-sexp
  "n" 'backward-sexp)

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
         (when-let* ((enc (sp-get-enclosing-sexp)))
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

(conn-register-thing
 'sp-sexp
 :forward-op 'conn-sp-forward-sexp-op
 :bounds-op 'conn-sp-bounds-of-sexp
 :dispatch-target-finder (lambda () (conn--dispatch-things-with-prefix 'sp-sexp 1 t))
 :default-action 'conn-dispatch-goto)

(conn-register-thing-commands
 'sp-sexp 'conn-sp-sexp-handler
 'sp-forward-sexp 'sp-backward-sexp
 'sp-forward-parallel-sexp 'sp-backward-parallel-sexp)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'sp-down-sexp
                     'sp-backward-down-sexp
                     'sp-up-sexp
                     'sp-backward-up-sexp
                     'sp-backward-sexp
                     'sp-forward-sexp))

(conn-define-state conn-paren-state ()
  "State for editing parens."
  :cursor '(hbar . 5)
  :suppress-input-method t
  :lighter " ()")

(define-keymap
  :keymap (conn-get-state-map 'conn-paren-state)
  :suppress t
  "o" 'sp-forward-symbol
  "u" 'sp-backward-symbol
  "O" 'forward-word
  "U" 'backward-word
  "e" 'conn-previous-state
  "i" 'sp-backward-up-sexp
  "j" 'sp-backward-down-sexp
  "k" 'sp-down-sexp
  "I" 'sp-beginning-of-sexp
  "K" 'sp-end-of-sexp
  ")" 'sp-splice-sexp-killing-forward
  "(" 'sp-splice-sexp-killing-backward
  ";" 'comment-region
  "DEL" 'backward-kill-sexp
  "d" 'kill-sexp
  "p" 'sp-splice-sexp-killing-around
  "l" 'sp-up-sexp
  "r" 'sp-raise-sexp
  "s" 'sp-splice-sexp
  "c" 'sp-convolute-sexp
  "f" 'conn-dispatch-on-things
  "," 'sp-backward-slurp-sexp
  "." 'sp-forward-slurp-sexp
  "y" 'sp-forward-barf-sexp
  "t" 'sp-backward-barf-sexp
  "q" 'transpose-sexps
  "h" 'sp-join-sexp
  "n" 'sp-backward-sexp
  "m" 'sp-forward-sexp)

(defun conn-paren-state ()
  (interactive)
  (conn-enter-state 'conn-paren-state))

(define-minor-mode conntext-smartparens-mode
  "Minor mode for contextual bindings in outline-mode.")

(define-keymap
  :keymap (conn-get-mode-map 'conn-command-state 'conntext-smartparens-mode)
  "c" (conntext-define conntext-smartparens-map
        "Context smartparens map."
        (when (or (looking-at-p "(")
                  (looking-back ")"))
          'conn-paren-state)))

(conn-set-mode-map-depth 'conn-command-state 'conntext-smartparens-mode -90)

(provide 'conn-smartparens)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-smartparens.el ends here
