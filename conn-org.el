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
(require 'conn-states)
(require 'conn-commands)
(require 'org)
(require 'org-element)
(require 'org-agenda)

;;;###autoload
(defun conn-dispatch-org-button-handler (pt)
  (and-let* ((link (save-excursion
                     (goto-char pt)
                     (org-element-link-parser))))
    (org-link-open link)
    t))

;;;###autoload
(add-hook 'conn-dispatch-button-functions 'conn-dispatch-org-button-handler)

(cl-defmethod conn-transpose-things-do ((_cmd (conn-thing org-element))
                                        _arg)
  (org-transpose-element))

(define-keymap
  :keymap (conn-get-state-map 'conn-org-state)
  :suppress t
  "`" 'conn-wincontrol-mru-window
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
(defun conn-setup-capture-state ()
  (when (buffer-match-p "CAPTURE-.*" (current-buffer))
    (conn-push-state 'conn-emacs-state)
    t))
;;;###autoload
(add-hook 'conn-setup-state-hook 'conn-setup-capture-state -20)

;;;###autoload
(defun conn-org-heading-state ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive)
  (conn-push-state 'conn-org-state))

;;;###autoload
(defun conn-org-state-up-heading (arg)
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive "p")
  (outline-up-heading (1- arg))
  (conn-push-state 'conn-org-state))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-org-state-up-heading))
                              arg)
  (conn-bounds-of 'org-up-heading (1- arg)))

(defun conn-org-insert-heading ()
  "Insert org heading."
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content)
  (conn-with-recursive-stack 'conn-emacs-state
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

(defun conn--org-window-p (win)
  (eq 'org-mode (buffer-local-value 'major-mode (window-buffer win))))

(conn-register-thing
 'org-link
 :bounds-op (lambda () (org-in-regexp org-link-any-re)))

(conn-register-thing-commands
 'org-link 'conn-discrete-thing-handler
 'org-next-link 'org-previous-link)

(conn-register-thing-commands
 'org-link nil
 'org-insert-link-global 'org-store-link 'org-insert-link)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing org-link))
                                      _arg)
  (conn-dispatch-regexp-targets
   :regexp org-link-any-re
   :fixed-length 0
   :window-predicate #'conn--org-window-p
   :reference (conn-reference-quote
                ((:heading "Org Link Targets")
                 "Dispatch on org links."))))

(put 'org-paragraph 'forward-op 'org-forward-paragraph)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing org-paragraph))
                                      _arg)
  (conn-all-things-targets
   :thing 'org-paragraph
   :window-predicate #'conn--org-window-p
   :reference (conn-reference-quote
                ((:heading "Org Paragraph Targets")))))

(conn-register-thing-commands
 'org-paragraph 'conn-continuous-thing-handler
 'org-forward-paragraph 'org-backward-paragraph)

(defun conn-open-org-link ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Open Link"))
      (window pt _thing _arg)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (org-open-at-point-global)))))

(cl-defmethod conn-make-default-action ((_cmd (conn-thing org-link)))
  (conn-open-org-link))

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
          (save-excursion (org-end-of-subtree nil t)))
         (t
          (let* ((elem (org-element-at-point))
                 (end (org-element-end elem))
                 (parent (org-element-parent elem)))
            (cond ((and parent (= (org-element-contents-end parent) end))
                   (org-element-end parent))
                  ((integer-or-marker-p end)
                   end))))))
 'org-forward-element
 'org-backward-element
 'org-next-visible-heading
 'org-previous-visible-heading
 'org-forward-heading-same-level
 'org-backward-heading-same-level
 'org-up-element
 'org-up-heading)

(conn-register-thing
 'org-heading
 :parent 'org-element
 :bounds-op (lambda () (bounds-of-thing-at-point 'org-element))
 :forward-op 'org-next-visible-heading)

(defun conn-update-org-heading-targets (state)
  (cl-symbol-macrolet ((cache (oref state cache)))
    (when (or (derived-mode-p (list 'outline-mode))
              (bound-and-true-p outline-minor-mode))
      (unless (and-let* ((cached (alist-get (current-buffer) cache)))
                (= (car cached) (buffer-chars-modified-tick)))
        (let ((pts nil))
          (setf (alist-get (current-buffer) cache)
                (cons (buffer-chars-modified-tick)
                      (progn
                        (save-excursion
                          (catch 'break
                            (while t
                              (when (= (point)
                                       (progn
                                         (org-next-visible-heading -1)
                                         (point)))
                                (throw 'break nil))
                              (push (point) pts))))
                        (save-excursion
                          (catch 'break
                            (while t
                              (when (= (org-next-visible-heading 1)
                                       (point-max))
                                (throw 'break nil))
                              (push (point) pts))))
                        pts)))))
      (dolist (pt (cdr (alist-get (current-buffer) cache)))
        (conn-make-target-overlay
         pt 0
         :properties '(no-hide t))))))

(conn-add-update-handler
 'conn-dispatch-headings
 (lambda (trynext)
   (if (provided-mode-derived-p major-mode 'org-mode)
       #'conn-update-org-heading-targets
     (funcall trynext))))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-dispatch-targets-state 'org-mode)
  "h" 'heading)

(conn-register-thing-commands
 'org-heading 'conn-continuous-thing-handler
 'conn-org-heading-state-up-heading
 'org-next-visible-heading
 'org-previous-visible-heading
 'org-forward-heading-same-level
 'org-backward-heading-same-level)

(conn-register-thing-commands
 'org-element 'conn-discrete-thing-handler
 'org-forward-element
 'org-backward-element
 'org-up-element
 'org-up-heading)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-command-state 'org-mode)
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

(cl-defmethod conn-argument-predicate :extra "conn-org" ((_arg conn-surround-with-argument)
                                                         sym)
  (or (memq sym '(org-quote
                  org-center
                  org-comment
                  org-src
                  org-verse
                  org-export-ascii
                  org-export-html
                  org-export-latex
                  org-export
                  org-example))
      (cl-call-next-method)))

(cl-defmethod conn-surround-do ((_cmd (eql org-quote))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "quote"))

(cl-defmethod conn-surround-do ((_cmd (eql org-comment))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "comment"))

(cl-defmethod conn-surround-do ((_cmd (eql org-center))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "center"))

(cl-defmethod conn-surround-do ((_cmd (eql org-src))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "src")
  (undo-boundary)
  (conn-with-recursive-stack 'conn-emacs-state
    (recursive-edit)))

(cl-defmethod conn-surround-do ((_cmd (eql org-verse))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "verse"))

(cl-defmethod conn-surround-do ((_cmd (eql org-export-ascii))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "export ascii"))

(cl-defmethod conn-surround-do ((_cmd (eql org-export-html))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "export html"))

(cl-defmethod conn-surround-do ((_cmd (eql org-export-latex))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "export latex"))

(cl-defmethod conn-surround-do ((_cmd (eql org-example))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "example"))

(cl-defmethod conn-surround-do ((_cmd (eql org-export))
                                _arg
                                &key &allow-other-keys)
  (org-insert-structure-template "export")
  (undo-boundary)
  (conn-with-recursive-stack 'conn-emacs-state
    (recursive-edit)))

(provide 'conn-org)
