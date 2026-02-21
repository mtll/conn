;;; conn-extras.el -*- lexical-binding: t -*-
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

(require 'conn-extras)

(define-keymap
  :keymap (conn-get-state-map 'conn-special-state)
  :suppress t
  "SPC" 'conn-one-emacs-state
  "<escape>" 'conn-pop-state
  "M-j" 'conn-command-state
  "w" 'conn-wincontrol-one-command
  ";" 'conn-wincontrol
  "`" 'conn-wincontrol-mru-window)

(define-keymap
  :keymap (conn-get-state-map 'conn-outline-state)
  :suppress t
  "TAB" 'outline-cycle
  "<backstab>" 'outline-cycle-buffer
  "*" 'conn-outline-insert-heading
  "<backspace>" 'conn-scroll-down
  ";" 'conn-wincontrol
  "/" (conn-remap-key conn-undo-keys t)
  "?" (conn-remap-key conn-undo-redo-keys t)
  "W" 'widen
  "<escape>" 'conn-pop-state
  "J" 'outline-promote
  "L" 'outline-demote
  "O" 'outline-move-subtree-down
  "C-SPC" 'conn-set-mark-command
  "U" 'outline-move-subtree-up
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "B" 'outline-show-branches
  "c" (conn-remap-key "C-c" t)
  "d" 'conn-kill-thing
  "h h" 'outline-hide-by-heading-regexp
  "h s" 'outline-show-by-heading-regexp
  "e" 'conn-pop-state
  "f" 'conn-dispatch
  "g" (conn-remap-key "M-g" t)
  "w" 'conn-wincontrol-one-command
  "i" 'outline-previous-visible-heading
  "j" 'outline-backward-same-level
  "k" 'outline-next-visible-heading
  "l" 'outline-forward-same-level
  "m" 'outline-show-subtree
  "n" 'outline-hide-leaves
  "o" 'outline-hide-other
  "q" 'conn-transpose-things
  "s" conn-search-remap
  "t" 'outline-hide-body
  "u" 'outline-up-heading
  "x" (conn-remap-key "C-x" t)
  "y" 'outline-show-all
  "z" 'conn-exchange-mark-command)

;;;; Dired

(define-keymap
  :keymap (conn-get-state-map 'conn-dired-dispatch-state)
  "f" 'conn-dispatch-dired-mark
  "w" 'conn-dispatch-dired-kill-line
  "d" 'conn-dispatch-dired-kill-subdir)

(conn-define-remap-keymap conn-dired-regexp-remap
    "Conn Dired Regexp Map"
  "%")

(conn-define-remap-keymap conn-dired-mark-remap
    "Conn Dired Mark Map"
  "*")

(conn-define-remap-keymap conn-dired-search-remap
    "Conn Dired Search Map"
  [conn-dired-search-map]
  [conn-search-map]
  "M-s")

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'dired-mode)
  "M-?" 'conn-dired-quick-ref
  "<conn-dired-search-map> s" 'dired-do-isearch
  "<conn-dired-search-map> c" 'dired-do-isearch-regexp
  "<conn-dired-search-map> q" 'dired-do-find-regexp
  "<conn-dired-search-map> r" 'dired-do-find-regexp-and-replace
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  ;; "A" 'dired-find-alternate-file
  "D" 'dired-do-flagged-delete
  "b" 'dired-up-directory
  "k" 'dired-next-line
  "i" 'dired-previous-line
  "/" 'dired-undo
  "I" 'dired-tree-up
  "l" 'dired-next-dirline
  "j" 'dired-prev-dirline
  "m" 'dired-next-subdir
  "n" 'dired-prev-subdir
  ">" 'dired-next-marked-file
  "<" 'dired-prev-marked-file
  "K" 'dired-tree-down
  "F" 'dired-create-empty-file
  "TAB" 'dired-maybe-insert-subdir
  "M-TAB" 'dired-kill-subdir
  "<backtab>" 'dired-hide-all
  "C-<tab>" 'dired-hide-subdir
  "h" 'dired-do-kill-lines
  "s" conn-dired-search-remap
  "r" conn-dired-regexp-remap
  "," conn-dired-mark-remap
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t)
  "f" 'conn-dired-dispatch-state
  "M-SPC" 'dired-toggle-marks
  "C-M-l" 'dired-do-redisplay
  "z" 'dired-goto-file
  ";" 'conn-wincontrol
  "`" 'conn-wincontrol-mru-window
  "v" 'dired-mark
  "V" 'dired-toggle-marks
  "C-d" 'dired-unmark
  "M-DEL" 'dired-unmark-all-marks
  "C-M-<backspace>" 'dired-unmark-all-files
  "u" 'dired-do-delete
  "M-w" 'dired-copy-filename-as-kill
  "RET" 'dired-find-file
  "o" 'dired-find-file-other-window
  "* p" 'dired-sort-toggle-or-edit
  "* e" 'dired-mark-executables
  "* l" 'dired-mark-symlinks
  "* d" 'dired-mark-directories
  "* r" 'dired-mark-files-regexp
  "% c" 'dired-do-copy-regexp
  "% h" 'dired-do-hardlink-regexp
  "% s" 'dired-do-symlink-regexp
  "% y" 'dired-do-relsymlink-regexp
  "% t" 'dired-flag-garbage-files)

;;;; Diff

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'diff-mode)
  ";" 'conn-wincontrol
  "w" 'conn-wincontrol-one-command
  "M-?" 'conn-diff-quick-ref
  "q" 'quit-window
  "/" 'diff-undo
  "DEL" 'diff-hunk-kill
  "M-DEL" 'diff-file-kill
  "i" 'diff-hunk-prev
  "I" 'conn-scroll-down
  "K" 'conn-scroll-up
  "k" 'diff-hunk-next
  "C" 'diff-kill-ring-save
  "TAB" 'diff-goto-source
  "l" 'diff-file-next
  "j" 'diff-file-prev
  "p" 'diff-ediff-patch
  "n" 'diff-restrict-view
  "R" 'diff-reverse-direction
  "d" 'diff-revert-and-kill-hunk
  "u" 'diff-context->unified
  "U" 'diff-unified->context
  "h" 'diff-ignore-whitespace-hunk
  "m" 'next-error-follow-minor-mode
  "f" 'diff-refine-hunk
  "g" 'diff-refresh-hunk
  "a" 'diff-apply-hunk
  "A" 'diff-apply-buffer
  "t" 'diff-test-hunk
  "s" 'diff-split-hunk
  "X" 'diff-delete-other-hunks
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Magit

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'magit-section-mode)
  "M-?" 'conn-magit-quick-ref
  "w" 'conn-wincontrol-one-command
  "," 'magit-dispatch
  "i" 'magit-section-backward
  "k" 'magit-section-forward
  "d" 'magit-delete-thing
  "h" 'magit-diff
  "p" 'magit-reset-quickly
  "n" 'magit-gitignore
  "`" 'conn-wincontrol-mru-window
  "@" 'magit-am
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Ibuffer

(keymap-set (conn-get-state-map 'conn-ibuffer-dispatch-state)
            "f" 'conn-dispatch-ibuffer-mark)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'ibuffer-mode)
  "C-M-<backspace>" 'ibuffer-unmark-all
  "M-DEL" 'ibuffer-unmark-all-marks
  "f" 'conn-ibuffer-dispatch-state
  "M-?" 'conn-ibuffer-quick-ref
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  ";" 'conn-wincontrol
  "/" 'ibuffer-do-revert
  "`" 'conn-wincontrol-mru-window
  "y" 'ibuffer-yank
  "z" 'ibuffer-jump-to-buffer
  "r" (conn-remap-key "%")
  "," (conn-remap-key "*")
  "l" 'ibuffer-forward-filter-group
  "j" 'ibuffer-backward-filter-group
  "m" 'ibuffer-jump-to-filter-group
  "n" 'conn-ibuffer-filter-prefix
  "k" 'ibuffer-forward-line
  "i" 'ibuffer-backward-line
  "h" 'ibuffer-do-kill-lines
  "u" 'ibuffer-do-kill-on-deletion-marks
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t)
  "s" conn-search-remap
  "t a" 'ibuffer-do-sort-by-alphabetic
  "t f" 'ibuffer-do-sort-by-filename/process
  "t i" 'ibuffer-invert-sorting
  "t m" 'ibuffer-do-sort-by-major-mode
  "t s" 'ibuffer-do-sort-by-size
  "t v" 'ibuffer-do-sort-by-recency
  "M-s i r" 'ibuffer-do-isearch-regexp
  "M-s i s" 'ibuffer-do-isearch
  "M-s i o" 'ibuffer-do-occur
  "M-w" 'ibuffer-copy-filename-as-kill
  "<" 'ibuffer-forward-next-marked
  ">" 'ibuffer-backwards-next-marked
  "M-SPC" 'ibuffer-toggle-marks
  "C-M-l" 'ibuffer-redisplay
  "v" 'ibuffer-mark-forward
  "V" 'ibuffer-toggle-marks
  "C-d" 'ibuffer-unmark-forward
  "D" 'ibuffer-mark-for-delete-backwards
  "o" 'ibuffer-visit-buffer-other-window
  "RET" 'ibuffer-visit-buffer)

;;;; Bookmark Bmenu

(define-keymap
  :keymap (conn-get-state-map 'conn-bmenu-dispatch-state)
  "f" 'conn-dispatch-bmenu-mark
  "k" (conn-anonymous-thing
        '(line)
        :target-finder (:method (_self _arg) #'conn-bmenu-target-finder)))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'bookmark-bmenu-mode)
  "M-?" 'conn-bookmark-bmenu-quick-ref
  "x" (conn-remap-key "C-x" t)
  "c" (conn-remap-key "C-c" t)
  "`" 'conn-wincontrol-mru-window
  "e" 'bookmark-bmenu-edit-annotation
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "n" 'bookmark-bmenu-show-annotation
  "N" 'bookmark-bmenu-show-all-annotations
  "v" 'bookmark-bmenu-mark
  "V" 'bookmark-bmenu-mark-all
  "K" 'scroll-up-command
  "I" 'scroll-down-command
  "i" 'previous-line
  "k" 'next-line
  "w" 'conn-wincontrol-one-command
  ";" 'conn-wincontrol
  "j" 'bookmark-bmenu-select
  "f" 'conn-bmenu-dispatch-state
  "h" 'bookmark-bmenu-this-window
  "D" 'bookmark-bmenu-delete-backwards
  "C-d" 'bookmark-bmenu-unmark
  "M-DEL" 'bookmark-bmenu-unmark-all
  "L" 'bookmark-locate
  "u" 'bookmark-bmenu-execute-deletions
  "y" 'tabulated-list-sort)

;;;; Help

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'help-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down-command
  "k" 'scroll-up-command
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'helpful-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down-command
  "k" 'scroll-up-command
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Info

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'Info-mode)
  "M-?" 'conn-info-quick-ref
  "w" 'conn-wincontrol-one-command
  "o" 'Info-history-back
  "u" 'Info-history-forward
  "L" 'Info-next
  "J" 'Info-prev
  "k" 'Info-scroll-up
  "i" 'Info-scroll-down
  "l" 'Info-next-reference
  "j" 'Info-prev-reference
  "m" 'Info-forward-node
  "n" 'Info-backward-node
  "r" 'Info-up
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "p" 'Info-menu
  "z" 'Info-toc
  "f" 'dispatch-on-info-refs
  "v" 'Info-index
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Treemacs

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'treemacs-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "`" 'treemacs-select-window
  "i" 'treemacs-previous-line
  "k" 'treemacs-next-line
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Messages

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'messages-buffer-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "`" 'conn-wincontrol-mru-window
  "i" 'scroll-down-command
  "k" 'scroll-up-command
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Debugger mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'debugger-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "`" 'conn-wincontrol-mru-window
  "i" 'backtrace-backward-frame
  "k" 'backtrace-forward-frame
  "I" 'scroll-down-command
  "K" 'scroll-up-command
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Occur mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'occur-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; Compile mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'compilation-mode)
  "w" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "." 'conn-register-load
  "x" (conn-remap-key "C-x" t))

;;;; pdf-tools

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'pdf-view-mode)
  "x" (conn-remap-key "C-x" t)
  "`" 'conn-wincontrol-mru-window
  "w" 'conn-wincontrol-one-command
  ";" 'conn-wincontrol
  "i" 'pdf-view-scroll-down-or-previous-page
  "k" 'pdf-view-scroll-up-or-next-page
  "l" 'pdf-view-next-page-command
  "j" 'pdf-view-previous-page-command
  "m" 'pdf-history-forward
  "n" 'pdf-history-backward
  ">" 'pdf-view-position-to-register
  "." 'conn-register-load
  "y" image-map
  "<escape>" 'undefined
  "M-j" 'undefined)

(provide 'conn-extras-qwerty)
