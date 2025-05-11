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
  :keymap (conn-get-state-map 'conn-org-edit-state)
  :suppress t
  "e" 'conn-previous-state
  "<escape>" 'conn-previous-state
  "SPC" 'conn-scroll-up
  "<backspace>" 'conn-scroll-down
  "DEL" 'conn-scroll-down
  "." 'point-to-register
  "/" (conn-remap-key conn-undo-keys t)
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "*" 'conn-org-edit-insert-heading
  "<" 'org-drag-element-backward
  ">" 'org-drag-element-forward
  "?" (conn-remap-key conn-undo-redo-keys t)
  "f" 'conn-dispatch-state
  "C" 'org-toggle-comment
  "b" (conn-remap-key "C-c C-v")
  "c" (conn-remap-key "C-c")
  "r" (conn-remap-key "C-c C-x")
  "d" 'org-down-element
  "g" (conn-remap-keymap "M-g" t)
  "i" 'org-backward-heading-same-level
  "I" 'org-metaup
  "J" 'org-metaleft
  "j" 'org-previous-visible-heading
  "k" 'org-forward-heading-same-level
  "K" 'org-metadown
  "L" 'org-metaright
  "l" 'org-next-visible-heading
  "M" 'org-mark-subtree
  "m" 'org-forward-element
  "n" 'org-backward-element
  "N" 'org-toggle-narrow-to-subtree
  "O" 'org-next-block
  "p" 'conn-register-load
  "s" (conn-remap-keymap "M-s" t)
  "T" 'org-todo
  "t" 'org-sparse-tree
  "U" 'org-previous-block
  "u" 'org-up-element
  "W" 'widen
  "w" 'org-refile
  "x" (conn-remap-key "C-x" t)
  "z" 'conn-exchange-mark-command)

;;;###autoload
(defun conn-org-edit-state ()
  "A `conn-mode' state for structural editing of `org-mode' buffers."
  (interactive)
  (conn-enter-state 'conn-org-edit-state))

;;;###autoload
(define-minor-mode conntext-org-mode
  "Conntext keys for org mode."
  :global t
  :group 'conn)

(defun conn-org-edit-insert-heading ()
  "Insert org heading."
  (interactive)
  (forward-char 1)
  (call-interactively 'org-insert-heading-respect-content))

(conn-register-thing
 'org-inner-math
 :bounds-op (lambda ()
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

(conn-register-thing
 'org-math
 :bounds-op (lambda ()
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

(conn-register-thing
 'org-link
 :dispatch-target-finder (lambda ()
                           (let ((conn-target-window-predicate conn-target-window-predicate))
                             (add-function :before-while
                                           conn-target-window-predicate
                                           'conn--org-window-p)
                             (conn-dispatch-re-matches org-link-any-re)))
 :bounds-op (lambda () (org-in-regexp org-link-any-re)))

(conn-register-thing
 'org-paragraph
 :dispatch-target-finder (lambda ()
                           (let ((conn-target-window-predicate conn-target-window-predicate))
                             (add-function :before-while
                                           conn-target-window-predicate
                                           'conn--org-window-p)
                             (conn-dispatch-all-things 'org-paragraph)))
 :forward-op 'org-forward-paragraph)

(conn-register-thing-commands
 'org-paragraph 'conn-continuous-thing-handler
 'org-forward-paragraph 'org-backward-paragraph)

(conn-define-dispatch-action conn-open-org-link
    (window pt _thing-cmd _thing-arg)
  :description "Open Link"
  (with-selected-window window
    (save-excursion
      (goto-char pt)
      (org-open-at-point-global))))

(setf (alist-get 'org-link conn-dispatch-default-action-alist)
      'conn-open-org-link)

(defun conn-org-sentence-forward (arg)
  (interactive "p")
  (if (>= arg 0)
      (org-forward-sentence arg)
    (org-backward-sentence (abs arg))))

(conn-register-thing
 'org-sentence
 :forward-op 'conn-org-sentence-forward)

(conn-register-thing-commands
 'org-sentence 'conn-continuous-thing-handler
 'conn-org-sentence-forward
 'org-forward-sentence 'org-backward-sentence)

(conn-register-thing
 'org-element
 :bounds-op (lambda ()
              (save-mark-and-excursion
                (org-mark-element)
                (cons (region-beginning) (region-end))))
 :beg-op 'org-backward-element
 :end-op 'org-forward-element)

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

(conn-register-thing
 'org-heading
 :bounds-op (lambda () (bounds-of-thing-at-point 'org-element))
 :dispatch-target-finder (lambda ()
                           (let ((conn-target-window-predicate conn-target-window-predicate))
                             (add-function :before-while
                                           conn-target-window-predicate
                                           'conn--org-window-p)
                             (conn-dispatch-all-things 'org-heading)))
 :forward-op 'org-next-visible-heading)

(conn-register-thing-commands
 'org-heading 'conn-continuous-thing-handler
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

(defun conn-org-speed-next-heading ()
  (interactive)
  (org-speed-move-safe 'org-next-visible-heading))

(defun conn-org-speed-previous-heading ()
  (interactive)
  (org-speed-move-safe 'org-previous-visible-heading))

(defun conn-org-speed-forward-heading ()
  (interactive)
  (org-speed-move-safe 'org-forward-heading-same-level))

(defun conn-org-speed-backward-heading ()
  (interactive)
  (org-speed-move-safe 'org-backward-heading-same-level))

(defun conn-org-speed-up-heading ()
  (interactive)
  (org-speed-move-safe 'outline-up-heading))

(conn-register-thing-commands
 'org-element 'conn-discrete-thing-handler
 'conn-org-speed-up-heading
 'conn-org-speed-next-heading
 'conn-org-speed-previous-heading
 'conn-org-speed-forward-heading
 'conn-org-speed-backward-heading
 'conn-org-speed-next-block
 'conn-org-speed-previous-block)

(defun conntext-edit-special ()
  (when (or (org-babel-where-is-src-block-head)
            (org-inside-LaTeX-fragment-p (org-element-context)))
    (org-edit-special)
    t))

(defun conntext-org-edit-state ()
  (conn-org-edit-state)
  t)

(defun conntext-org-hook ()
  (add-hook 'conntext-state-hook 'conntext-edit-special -20 t)
  (add-hook 'conntext-state-hook 'conntext-org-edit-state 90 t))

(add-hook 'org-mode-hook 'conntext-org-hook)

(define-keymap
  :keymap (conn-get-mode-map 'conn-emacs-state 'conntext-org-mode)
  "k" (conntext-define conntext-org-next-visible-heading
        (when (and (bolp) (looking-at org-outline-regexp))
          'conn-org-speed-next-heading))
  "i" (conntext-define conntext-org-prev-visible-heading
        (when (and (bolp) (looking-at org-outline-regexp))
          'conn-org-speed-previous-heading))
  "l" (conntext-define conntext-org-forward-heading
        (when (and (bolp) (looking-at org-outline-regexp))
          'conn-org-speed-forward-heading))
  "j" (conntext-define conntext-org-backward-heading
        (when (and (bolp) (looking-at org-outline-regexp))
          'conn-org-speed-backward-heading))
  "F" (conntext-define conntext-org-next-block
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-next-block))
  "B" (conntext-define conntext-org-prev-block
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-previous-block))
  "u" (conntext-define conntext-org-up-heading
        (when (and (bolp) (looking-at org-outline-regexp))
          'conn-org-speed-up-heading))
  "g" (conntext-define conntext-org-refile-goto
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-refile '(4)))))
  "C" (conntext-define conntext-org-shifttab
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-shifttab))
  "SPC" (conntext-define conntext-org-outline-path
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-display-outline-path))
  "s" (conntext-define conntext-org-narrow-subtree
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-toggle-narrow-to-subtree))
  "w" (conntext-define conntext-org-cut-subtree
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-cut-subtree))
  "=" (conntext-define conntext-org-columns
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-columns))
  "I" (conntext-define conntext-org-metaup
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-metaup))
  "K" (conntext-define conntext-org-metadown
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-metadown))
  "L" (conntext-define conntext-org-metaright
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-metaright))
  "J" (conntext-define conntext-org-metaleft
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-metaleft))
  "M-L" (conntext-define conntext-org-shiftmetaright
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-shiftmetaright))
  "M-J" (conntext-define conntext-org-shiftmetaleft
          (when (and (bolp) (looking-at org-outline-regexp))
            'org-shiftmetaleft))
  "RET" (conntext-define conntext-org-insert-heading
          (when (and (bolp) (looking-at org-outline-regexp))
            (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content))))
  "^" (conntext-define conntext-org-sort
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-sort))
  "r" (conntext-define conntext-org-refile
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-refile))
  "a" (conntext-define conntext-org-archive-subtree
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-archive-subtree-default-with-confirmation))
  "@" (conntext-define conntext-org-mark-subtree
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-mark-subtree))
  "#" (conntext-define conntext-org-comment
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-toggle-comment))
  "p" (conntext-define conntext-org-clock-map
        (when (and (bolp) (looking-at org-outline-regexp))
          (define-keymap
            "i" 'org-clock-in
            "o" 'org-clock-out)))
  "t" (conntext-define conntext-org-todo
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-todo))
  "," (conntext-define conntext-org-priority
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-priority))))
  "0" (conntext-define conntext-org-priority-0
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-priority ?\ ))))
  "1" (conntext-define conntext-org-priority-1
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-priority ?A))))
  "2" (conntext-define conntext-org-priority-2
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-priority ?B))))
  "3" (conntext-define conntext-org-priority-3
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-priority ?C))))
  ":" (conntext-define conntext-org-set-tags
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-set-tags-command))
  "e" (conntext-define conntext-org-set-effort
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-set-effort))
  "E" (conntext-define conntext-org-inc-effor
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-inc-effort))
  "W" (conntext-define conntext-org-appt-warning
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda (m)
            (interactive "sMinutes before warning: ")
            (org-entry-put (point) "APPT_WARNTIME" m))))
  "v" (conntext-define conntext-org-agenda
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-agenda))
  "/" (conntext-define conntext-org-sparse-tree
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-sparse-tree))
  "o" (conntext-define conntext-org-open-at-point
        (when (and (bolp) (looking-at org-outline-regexp))
          'org-open-at-point))
  "<" (conntext-define conntext-org-agenda-lock
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-agenda-set-restriction-lock 'subtree))))
  ">" (conntext-define conntext-org-agenda-unlock
        (when (and (bolp) (looking-at org-outline-regexp))
          (lambda ()
            (interactive)
            (org-agenda-remove-restriction-lock)))))
(conn-set-mode-map-depth 'conn-emacs-state 'conntext-org-mode -81)

(provide 'conn-org)
