;;; conn-org.el --- org mode integration for conn -*- lexical-binding: t -*-
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

;; org mode integration for conn

;;; Code:

(require 'conn)
(require 'org)
(require 'org-element)
(require 'org-agenda)

(define-keymap
  :keymap (conn-get-state-map 'conn-org-state)
  :suppress t
  "_" 'org-down-element
  "*" 'conn-org-insert-heading
  "<" 'org-drag-element-backward
  ">" 'org-drag-element-forward
  "C" 'org-toggle-comment
  "I" 'org-metaup
  "J" 'org-metaleft
  "K" 'org-metadown
  "L" 'org-metaright
  "M" 'org-mark-subtree
  "N" 'org-toggle-narrow-to-subtree
  "O" 'org-next-block
  "T" 'org-todo
  "U" 'org-previous-block
  "b" (conn-remap-key "C-c C-v")
  "d" (conn-remap-key "C-c C-x")
  "i" 'org-previous-visible-heading
  "j" 'org-backward-heading-same-level
  "k" 'org-next-visible-heading
  "l" 'org-forward-heading-same-level
  "m" 'org-forward-element
  "n" 'org-backward-element
  "t" 'org-sparse-tree
  "u" 'org-up-element
  "w" 'org-refile
  "y" 'org-show-all)

;;;###autoload
(defun conn-org-heading-state ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive)
  (conn-push-state 'conn-org-state))

;;;###autoload
(defun conn-org-state-prev-heading ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive)
  (unless (progn
            (goto-char (pos-bol))
            (looking-at-p outline-regexp))
    (org-previous-visible-heading 1))
  (conn-push-state 'conn-org-state))

(defun conn-org-insert-heading ()
  "Insert org heading."
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content)
  (conn-with-recursive-state 'conn-emacs-state
    (recursive-edit)))

(put 'org-inner-math 'bounds-of-thing-at-point
     (lambda ()
       (let ((node (org-element-context)))
         (save-excursion
           (pcase (org-element-type node)
             ('latex-environment
              (cons (progn
                      (goto-char (org-element-begin node))
                      (re-search-forward "\\\\begin{[^}]*}")
                      (skip-chars-forward "\n\t ")
                      (point))
                    (progn
                      (goto-char (org-element-end node))
                      (re-search-backward "\\\\end")
                      (skip-chars-backward "\n\t ")
                      (point))))
             ('latex-fragment
              (cons (progn
                      (goto-char (org-element-begin node))
                      (re-search-forward (regexp-opt '("\\(" "\\[")))
                      (point))
                    (progn
                      (goto-char (org-element-end node))
                      (re-search-backward (regexp-opt '("\\)" "\\]")))
                      (point))))
             (_ (error "no math at point")))))))

(conn-define-mark-command conn-mark-org-inner-math org-inner-math)

(put 'org-math 'bounds-of-thing-at-point
     (lambda ()
       (let ((node (org-element-context)))
         (save-excursion
           (pcase (org-element-type node)
             ('latex-environment
              (cons (org-element-begin node)
                    (- (org-element-end node)
                       (org-element-post-blank node))))
             ('latex-fragment
              (cons (org-element-begin node)
                    (- (org-element-end node)
                       (org-element-post-blank node))))
             (_ (error "no math at point")))))))

(conn-define-mark-command conn-mark-org-math org-math)

(defun conn--org-window-p (win)
  (eq 'org-mode (buffer-local-value 'major-mode (window-buffer win))))

(put 'org-link 'bounds-of-thing-at-point
     (lambda () (org-in-regexp org-link-any-re)))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing org-link)))
  (add-function :before-while
                conn-target-window-predicate
                'conn--org-window-p)
  (conn-dispatch-re-matches org-link-any-re))

(put 'org-paragraph 'forward-op 'org-forward-paragraph)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing org-paragraph)))
  (add-function :before-while
                conn-target-window-predicate
                'conn--org-window-p)
  (conn-dispatch-all-things 'org-paragraph))

(conn-register-thing-commands
 'org-paragraph 'conn-continuous-thing-handler
 'org-forward-paragraph 'org-backward-paragraph)

(oclosure-define (conn-open-org-link
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-open-org-link)))
  (oclosure-lambda (conn-open-org-link
                    (description "Open Link"))
      (window pt _thing _thing-arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (org-open-at-point-global)))))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing org-link)))
  (conn-make-action 'conn-open-org-link))

(defun conn-org-sentence-forward (arg)
  (interactive "p")
  (if (>= arg 0)
      (org-forward-sentence arg)
    (org-backward-sentence (abs arg))))

(put 'org-sentence 'forward-op 'conn-org-sentence-forward)

(conn-register-thing-commands
 'org-sentence 'conn-continuous-thing-handler
 'conn-org-sentence-forward
 'org-forward-sentence 'org-backward-sentence)

(put 'org-element 'bounds-of-thing-at-point
     (lambda ()
       (save-mark-and-excursion
         (org-mark-element)
         (cons (region-beginning) (region-end)))))
(put 'org-element 'beginning-op 'org-backward-element)
(put 'org-element 'end-op 'org-forward-element)

;; FIXME: org-element all broken
(conn-register-thing-commands
 'org-element
 (lambda (_beg)
   (cond ((eobp))
         ((org-with-limited-levels (org-at-heading-p))
          (conn--push-ephemeral-mark
           (save-excursion (org-end-of-subtree nil t))))
         (t
          (let* ((elem (org-element-at-point))
                 (end (org-element-end elem))
                 (parent (org-element-parent elem)))
            (cond ((and parent (= (org-element-contents-end parent) end))
                   (conn--push-ephemeral-mark (org-element-end parent)))
                  ((integer-or-marker-p end)
                   (conn--push-ephemeral-mark end)))))))
 'org-forward-element
 'org-backward-element
 'org-next-visible-heading
 'org-previous-visible-heading
 'org-forward-heading-same-level
 'org-backward-heading-same-level
 'org-up-element
 'org-up-heading)

(put 'org-heading 'bounds-of-thing-at-point
     (lambda () (bounds-of-thing-at-point 'org-element)))
(put 'org-heading 'forward-op 'org-next-visible-heading)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing org-heading)))
  (let ((conn-target-window-predicate conn-target-window-predicate))
    (add-function :before-while
                  conn-target-window-predicate
                  'conn--org-window-p)
    (conn-dispatch-all-things 'org-heading)))

(conn-register-thing-commands
 'org-heading 'conn-continuous-thing-handler
 'conn-org-heading-state-prev-heading
 'org-next-visible-heading
 'org-previous-visible-heading)

(conn-register-thing-commands
 'org-element 'conn-discrete-thing-handler
 'org-forward-element
 'org-backward-element
 'org-next-visible-heading
 'org-previous-visible-heading
 'org-forward-heading-same-level
 'org-backward-heading-same-level
 'org-up-element
 'org-up-heading)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-movement-state 'org-mode)
  "^" 'org-up-element
  ")" 'org-next-visible-heading
  "(" 'org-previous-visible-heading
  "N" 'org-backward-element
  "M" 'org-forward-element
  "I" 'org-backward-paragraph
  "K" 'org-forward-paragraph)

(defun conn-setup-org-capture-state ()
  (when (bound-and-true-p org-capture-mode)
    (conn-push-state 'conn-command-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-commit-state -50)

(defun conntext-edit-special ()
  (when (or (org-babel-where-is-src-block-head)
            (org-inside-LaTeX-fragment-p (org-element-context)))
    (org-edit-special)
    t))

(defun conntext-org-state ()
  (conn-org-heading-state)
  t)

(defun conntext-org-hook ()
  (add-hook 'conntext-state-hook 'conntext-edit-special -20 t)
  (add-hook 'conntext-state-hook 'conntext-org-state 90 t))

(add-hook 'org-mode-hook 'conntext-org-hook)

;;;; Surround

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-surround-with-state 'org-mode)
  "q" 'org-quote
  "c" 'org-center
  "C" 'org-comment
  "s" 'org-src
  "v" 'org-verse
  "a" 'org-export-ascii
  "h" 'org-export-html
  "l" 'org-export-latex
  "E" 'org-export
  "e" 'org-example)

(cl-defmethod conn-handle-surround-with-argument :extra "conn-org" (cmd arg)
  (if (memq cmd '(org-quote
                  org-center
                  org-comment
                  org-src
                  org-verse
                  org-export-ascii
                  org-export-html
                  org-export-latex
                  org-export
                  org-example))
      (conn-set-argument arg (list cmd (conn-state-eval-consume-prefix-arg)))
    (cl-call-next-method)))

(cl-defmethod conn-perform-surround ((_cmd (eql org-quote)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "quote"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-comment)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "comment"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-center)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "center"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-src)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "src")
  (undo-boundary)
  (conn-with-recursive-state 'conn-emacs-state
    (recursive-edit)))

(cl-defmethod conn-perform-surround ((_cmd (eql org-verse)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "verse"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-export-ascii)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "export ascii"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-export-html)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "export html"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-export-latex)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "export latex"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-example)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "example"))

(cl-defmethod conn-perform-surround ((_cmd (eql org-export)) _arg
                                     &key &allow-other-keys)
  (org-insert-structure-template "export")
  (undo-boundary)
  (conn-with-recursive-state 'conn-emacs-state
    (recursive-edit)))

(provide 'conn-org)
