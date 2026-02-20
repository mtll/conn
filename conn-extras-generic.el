;;; conn-extras-generic.el -*- lexical-binding: t -*-
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

;;; Special State

(define-keymap
  :keymap (conn-get-state-map 'conn-special-state)
  :suppress t
  "h" 'conn-one-command
  "SPC" 'conn-one-emacs-state
  "<escape>" 'conn-pop-state
  "M-j" 'conn-command-state
  "w" 'conn-wincontrol-one-command
  ";" 'conn-wincontrol
  "`" 'conn-wincontrol-mru-window)

;;; Load Extensions

;;;; Outline

(define-keymap
  :keymap (conn-get-state-map 'conn-outline-state)
  :suppress t
  "TAB" 'outline-cycle
  "<backstab>" 'outline-cycle-buffer
  "*" 'conn-outline-insert-heading
  "<backspace>" 'conn-scroll-down
  "/" (conn-remap-key conn-undo-keys t)
  "?" (conn-remap-key conn-undo-redo-keys t)
  "W" 'widen
  "<escape>" 'conn-pop-state
  "F" 'outline-promote
  "B" 'outline-demote
  "P" 'outline-move-subtree-down
  "C-SPC" 'conn-set-mark-command
  "U" 'outline-move-subtree-up
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "B" 'outline-show-branches
  "c" (conn-remap-key "C-c" t)
  "k" 'conn-kill-thing
  "h h" 'outline-hide-by-heading-regexp
  "h s" 'outline-show-by-heading-regexp
  "e" 'conn-pop-state
  "d" 'conn-dispatch
  "g" (conn-remap-key "M-g" t)
  "p" 'outline-previous-visible-heading
  "b" 'outline-backward-same-level
  "n" 'outline-next-visible-heading
  "f" 'outline-forward-same-level
  "m" 'outline-show-subtree
  "l" 'outline-hide-leaves
  "o" 'outline-hide-other
  "t" 'conn-transpose-things
  "s" conn-search-remap
  "z" 'outline-hide-body
  "u" 'outline-up-heading
  "x" (conn-remap-key "C-x" t)
  "y" 'outline-show-all
  "Z" 'conn-exchange-mark-command)

;;;; Dired

(define-keymap
  :keymap (conn-get-state-map 'conn-dired-dispatch-state)
  "d" 'conn-dispatch-dired-mark
  "k" 'conn-dispatch-dired-kill-line
  "/" 'conn-dispatch-dired-kill-subdir)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'dired-mode)
  "M-?" 'conn-dired-quick-ref
  "M-w" 'dired-copy-filename-as-kill
  "/" 'dired-undo
  "C-." 'conn-register-load)

;;;; Diff

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'diff-mode)
  "M-w" 'diff-kill-ring-save
  "M-?" 'conn-diff-quick-ref
  "q" 'quit-window
  "/" 'diff-undo
  "TAB" 'diff-goto-source
  "<tab>" 'diff-goto-source)

;;;; Magit

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'magit-section-mode)
  "M-?" 'conn-magit-quick-ref
  "@" 'magit-am)

;;;; Ibuffer

(keymap-set (conn-get-state-map 'conn-ibuffer-dispatch-state)
            "d" 'conn-dispatch-ibuffer-mark)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'ibuffer-mode)
  "M-?" 'conn-ibuffer-quick-ref
  "M-w" 'ibuffer-copy-filename-as-kill
  "y" 'ibuffer-yank)

;;;; Bookmark Bmenu

(define-keymap
  :keymap (conn-get-state-map 'conn-bmenu-dispatch-state)
  "d" 'conn-dispatch-bmenu-mark
  "k" (conn-anonymous-thing
        '(line)
        :target-finder (:method (_self _arg) #'conn-bmenu-target-finder)))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'bookmark-bmenu-mode)
  "M-?" 'conn-bookmark-bmenu-quick-ref)

;;;; Help

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'help-mode)
  "d" 'conn-dispatch-on-buttons)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'helpful-mode)
  "d" 'conn-dispatch-on-buttons)

;;;; Info

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'Info-mode)
  "M-?" 'conn-info-quick-ref
  "d" 'dispatch-on-info-refs)

;;;; Treemacs

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'treemacs-mode)
  "d" 'conn-dispatch)

;;;; Messages

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'messages-buffer-mode)
  "d" 'conn-dispatch)

;;;; Debugger mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'debugger-mode)
  "d" 'conn-dispatch)

;;;; Occur mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'occur-mode)
  "d" 'conn-dispatch)

;;;; Compile mode

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'compilation-mode)
  "d" 'conn-dispatch)

;;;; pdf-tools

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-special-state 'pdf-view-mode)
  ">" 'pdf-view-position-to-register
  "." 'pdf-view-jump-to-register
  "<escape>" 'undefined
  "M-j" 'undefined)

(provide 'conn-extras-generic)
