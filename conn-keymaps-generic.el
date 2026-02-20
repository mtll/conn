;;; conn-keymaps.el --- Keymaps -*- lexical-binding: t -*-
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

;;; Commentary

;;; Code

(require 'compat)
(require 'conn)

(setq conn-keymaps-defined 'generic)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "s" 'conn-surround
  "h" 'end-of-buffer
  "C-e" 'conn-forward-outer-line
  "C-a" 'conn-backward-outer-line
  ";" 'conn-things-in-region
  "@" 'kmacro-start-macro
  "#" 'kmacro-call-macro)

(define-keymap
  :keymap conn-other-end-argument-map
  "z" 'other-end)

;;;; Repeat Map

(defvar-keymap conn-mru-window-repeat-map
  :repeat t
  "`" 'conn-wincontrol-mru-window)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

;;;; Mode Keymaps

(defvar-keymap conn-error-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "p" 'previous-error
  "n" 'next-error)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state 'rectangle-mark-mode)
  "z" 'rectangle-exchange-point-and-mark
  "C-y" 'conn-yank-replace-rectangle
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "M-DEL" 'clear-rectangle
  "<backspace>" 'clear-rectangle
  "<conn-edit-map> d" 'delete-whitespace-rectangle
  "<conn-edit-map> o" 'open-rectangle
  "#" 'rectangle-number-lines)
(conn-set-mode-map-depth 'rectangle-mark-mode -90 'conn-command-state)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-command-state 'occur-mode)
  "<conn-edit-map> '" 'conn-kapply-on-occur)

(defun conn-setup-isearch-map ()
  (define-keymap
    :keymap isearch-mode-map
    "C-w" 'conn-isearch-restrict-to-thing
    "C-t" 'conn-isearch-thing-to-search-string
    "C-z" 'conn-isearch-exit-other-end
    "M-'" 'conn-kapply-on-isearch
    "C-," 'conn-dispatch-isearch
    "C-." 'conn-dispatch-isearch-with-action
    "C-'" 'conn-isearch-open-recursive-edit))

;;;; Top-level Command State Maps

(defvar-keymap conn-indent-relative-repeat-map
  :repeat t
  "=" 'indent-relative
  "+" 'indent-relative-first-indent-point)

(define-keymap
  :keymap conn-default-edit-map
  "," 'conn-dispatch-on-buttons
  "r" 'conn-register-prefix
  "F" #'conn-bind-last-dispatch-to-key
  "b" 'conn-command-to-register
  "#" 'conn-how-many-in-thing
  "'" 'conn-kapply-count-iterator
  "+" 'indent-relative-first-indent-point
  "c" 'conn-comment-thing
  "_" 'indent-relative
  "L" 'clone-indirect-buffer
  "SPC" 'whitespace-cleanup
  "q" 'conn-indent-thing
  "l" 'conn-indent-thing-rigidly
  "R" 'indent-rigidly
  "V" 'vc-region-history
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule
  "TAB" 'conn-fill-prefix
  "k" 'conn-kmacro-prefix
  "g" 'conn-rgrep-thing
  "e" 'conn-emacs-state-open-line-above
  "j" 'conn-join-lines
  "d" 'conn-emacs-state-open-line
  "n" 'conn-narrow-to-thing
  "o" 'conn-occur-thing
  "Y" 'yank-rectangle
  "s" 'conn-sort-things
  "v" 'diff-buffer-with-file
  "y" 'yank-in-context)

(define-keymap
  :keymap conn-search-map
  "d" 'conn-dispatch-thing-at-point
  "h '" 'conn-kapply-on-highlights
  "s" 'conn-isearch-forward
  "r" 'conn-isearch-backward
  "o" 'occur
  "l" 'locate)

;; (defvar-keymap conn-global-mark-repeat-map
;;   :repeat t
;;   "n" 'pop-global-mark)

(defvar-keymap conn-xref-repeat-map
  :repeat t
  "<" 'xref-go-back
  ">" 'xref-go-forward)

(defvar-keymap conn-mark-ring-repeat-map
  :repeat t
  "o" 'conn-pop-mark-ring
  "i" 'conn-unpop-mark-ring)

(define-keymap
  :keymap conn-goto-map
  "v" 'conn-previous-mark-command
  "e" 'conn-previous-emacs-state
  "E" 'conn-next-emacs-state
  "O" 'conn-push-jump-ring
  "o" 'conn-pop-mark-ring
  "i" 'conn-unpop-mark-ring
  ;; "n" 'pop-global-mark
  "m" 'imenu
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "<" 'xref-go-back
  ">" 'xref-go-forward)

;;;; Global Bindings

(defvar-keymap conn-last-emacs-state-repeat-map
  :repeat t
  "e" 'conn-previous-emacs-state
  "E" 'conn-next-emacs-state)

(put 'conn-next-emacs-state 'repeat-check-key 'no)
(put 'conn-previous-emacs-state 'repeat-check-key 'no)

(defvar-keymap conn-local-mode-map
  "C-<escape>" 'exit-recursive-edit)

(defvar-keymap conn-whitespace-repeat-map
  :repeat t
  "w" 'forward-whitespace
  "W" 'conn-backward-whitespace)

(define-keymap
  :keymap conn-default-thing-map
  "," conn-thing-inner-remap
  "l" 'forward-line
  ")" 'forward-list
  "SPC" 'forward-whitespace
  "v" 'conn-forward-visual-line
  "h" 'outline-previous-visible-heading)

(keymap-set
 (with-memoization (alist-get 'conn-kmacro-applying-p minor-mode-map-alist)
   (make-sparse-keymap))
 "<remap> <kbd-macro-query>" 'conn-kapply-kbd-macro-query)

;;;; Minibuffer

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'minibuffer-mode)
  "C-t" 'conn-yank-thing-to-minibuffer)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-command-state 'minibuffer-mode)
  "C-t" 'conn-yank-thing-to-minibuffer)

;;;; State Keymaps

;;;;; Mark State

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-state)
  "z" 'conn-exchange-mark-command
  "e" 'conn-change-thing
  "r" 'conn-replace
  "M-n" 'conn-mark-ring-next
  "M-p" 'conn-mark-ring-previous
  "DEL" 'kill-region
  ;; "e" 'conn-emacs-state
  "TAB" 'indent-rigidly
  "Y" 'conn-completing-yank-replace
  "y" 'conn-yank-replace
  "^" 'ispell-region
  "*" 'calc-grab-region
  "C-j" 'conn-join-lines
  "v" 'rectangle-mark-mode
  "V" 'conn-mark-thing)

;;;;; Emacs State

(keymap-set (conn-get-state-map 'conn-emacs-state)
            "<escape>" 'conn-pop-state)

;;;;; Read Thing State

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-common-state)
  "<conn-thing-map> x" 'sexp
  "<conn-thing-map> /" 'filename
  "<conn-thing-map> U" 'uuid
  "<conn-thing-map> \"" 'string
  "<conn-thing-map> @" 'email
  "<conn-thing-map> (" 'list
  "<conn-thing-map> )" 'forward-list
  "<conn-inner-thing-map> (" 'inner-list
  "n" 'forward-line
  "p" 'conn-backward-line
  ;; "e" 'conn-expand
  "F" 'conn-forward-inner-line
  "B" 'conn-backward-inner-line
  "S" 'conn-thing-at-isearch
  "C-s" 'isearch-forward
  "C-r" 'isearch-backward
  "C-M-s" 'isearch-forward-regexp
  "C-M-r" 'isearch-backward-regexp
  "c" 'comment
  "i" conn-thing-inner-remap)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-read-thing-common-state 'outline-minor-mode)
  "M-h" 'outline-up-heading)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-read-thing-common-state 'outline-mode)
  "M-h" 'outline-up-heading)

;;;;; Command State

(define-keymap
  :keymap (conn-get-state-map 'conn-command-state)
  :suppress t
  "o" conn-forward-word-remap
  "O" conn-backward-word-remap
  "u" conn-end-of-defun-remap
  "U" conn-beginning-of-defun-remap
  "j" 'forward-symbol
  "J" 'conn-backward-symbol
  "m" conn-forward-sexp-remap
  "M" conn-backward-sexp-remap
  "i" 'conn-duplicate-thing
  "," conn-thing-remap
  "@" 'kmacro-start-macro
  "#" 'kmacro-call-macro
  "%" 'eshell
  "$" 'project-eshell
  "S-<return>" 'conn-open-line-and-indent
  "{" 'conn-backward-up-inner-list
  "}" 'conn-forward-up-inner-list
  "(" 'backward-up-list
  ")" 'conn-forward-up-list
  "[" 'conn-backward-down-list
  "]" 'down-list
  "G" 'conn-adjust-surround
  "C" 'conn-surround-raise
  "P" conn-backward-paragraph-remap
  "p" conn-previous-line-remap
  "B" 'conn-backward-inner-line-dwim
  "b" conn-backward-char-remap
  "N" conn-forward-paragraph-remap
  "n" conn-next-line-remap
  "F" 'conn-forward-inner-line-dwim
  "f" conn-forward-char-remap
  "s" conn-search-remap
  "g" conn-goto-remap
  "c" (conn-remap-key "C-c" t)
  "x" (conn-remap-key "C-x" t)
  "C-4" (conn-remap-key "C-x 4" t)
  "C-5" (conn-remap-key "C-x 5" t)
  "S" 'conn-surround
  "<escape>" 'conn-pop-state
  "q" 'conn-yank-replace
  "E" 'conn-expand
  "l" 'conn-repeat
  "e" 'conn-pop-state
  "`" 'conn-wincontrol-mru-window
  "|" 'conn-shell-command-on-thing
  "/" (conn-remap-key conn-undo-keys t)
  ";" 'conn-wincontrol
  "'" 'conn-kapply-on-things
  "?" (conn-remap-key conn-undo-redo-keys t)
  "_" 'repeat-complex-command
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "r" 'conn-change-thing
  "d" 'conn-dispatch
  "w" 'conn-wincontrol-one-command
  "." 'conn-register-load
  "<" 'point-to-register
  "t" 'conn-transpose-things
  "v" 'conn-mark-thing
  "h" conn-edit-remap
  "k" 'conn-kill-thing
  "W" 'conn-widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" (conn-remap-key "C-y" t)
  "z" 'conn-other-window-prefix
  "Z" 'conn-exchange-mark-command)

;;;;; Dispatch State

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-targets-state)
  "TAB" 'repeat-dispatch
  "n" 'forward-line
  "p" 'next-line
  "<conn-thing-map> b" (conn-anonymous-thing
                         '(visual-line)
                         :target-finder ( :method (_self _arg)
                                          (conn-dispatch-end-of-visual-line-targets))
                         :bounds-op ( :method (_self _arg)
                                      (save-excursion
                                        (goto-char (pos-bol))
                                        (cl-call-next-method)))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-targets-state 'outline-minor-mode)
  "<conn-thing-map> h" 'heading)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-bounds-state)
  "O" (conn-anonymous-thing
        '(forward-word)
        :pretty-print (:method (_self) "all-words")
        :target-finder ( :method (_self _arg)
                         (conn-all-things-targets :thing 'word)))
  "U" (conn-anonymous-thing
        '(forward-symbol)
        :pretty-print (:method (_self) "all-symbols")
        :target-finder ( :method (_self _arg)
                         (conn-all-things-targets :thing 'symbol))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-targets-state :override)
  "<remap> <conn-expand>" (conn-anonymous-thing
                            '(expansion)
                            :pretty-print ( :method (_) "conn-expand")
                            :bounds-op ( :method (_self arg)
                                         (conn-bounds-of 'conn-expand arg)))
  ")" (conn-anonymous-thing
        '(sexp)
        :pretty-print (:method (_) "outer-list-or-string")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-things-with-re-prefix-targets
                          :thing 'sexp
                          :prefix-regexp (rx (or (syntax open-parenthesis)
                                                 (syntax string-quote))))))
  "]" (conn-anonymous-thing
        '(inner-list)
        :pretty-print (:method (_) "inner-list-or-string")
        :bounds-op ( :method (_self arg)
                     (pcase (conn-bounds-of 'inner-string arg)
                       ((and bounds (conn-bounds (pred identity)))
                        bounds)
                       (_ (cl-call-next-method))))
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-things-with-re-prefix-targets
                          :thing 'sexp
                          :skip-prefix t
                          :prefix-regexp (rx (or (syntax open-parenthesis)
                                                 (syntax string-quote)))))))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-state)
  "'" 'conn-dispatch-kapply
  "w" 'conn-dispatch-copy-to
  "r" 'conn-dispatch-copy-to-replace
  "q" 'conn-dispatch-copy-from-replace
  "y" 'conn-dispatch-yank-to
  "Y" 'conn-dispatch-reading-yank-to
  "d" 'conn-dispatch-copy-from
  "x" 'conn-dispatch-grab
  "X" 'conn-dispatch-grab-replace
  "k" 'conn-dispatch-send
  "K" 'conn-dispatch-send-replace
  "t" 'conn-dispatch-transpose
  "." 'conn-dispatch-register-load
  ">" 'conn-dispatch-register-load-replace
  "l" 'conn-dispatch-repeat-command
  "RET" 'conn-repeat-last-dispatch
  "<return>" 'conn-repeat-last-dispatch
  "M-n" 'conn-dispatch-cycle-ring-next
  "M-p" 'conn-dispatch-cycle-ring-previous
  "M-f" 'conn-dispatch-ring-describe-head
  "g e" (conn-anonymous-thing
          '(point)
          :pretty-print ( :method (_) "prev-emacs-state")
          :default-action ( :method (_self)
                            (let ((jump (conn-dispatch-jump)))
                              (oclosure-lambda (conn-action
                                                (action-description "Previous Emacs State")
                                                (action-no-history t))
                                  ()
                                (funcall jump)
                                (conn-push-state 'conn-emacs-state))))
          :target-finder (:method (_self _arg) (conn-dispatch-previous-emacs-state)))
  "b" 'point
  "S-SPC" (conn-anonymous-thing
            '(point)
            :pretty-print ( :method (_) "global-mark-ring")
            :target-finder (:method (_self _arg) (conn-dispatch-global-mark)))
  "<" (conn-anonymous-thing
        '(point)
        :pretty-print ( :method (_) "position-registers")
        :target-finder (:method (_self _arg) (conn-dispatch-mark-register))))

;;;;; Other States

(define-keymap
  :keymap (conn-get-state-map 'conn-change-state)
  "y" 'yank
  "Y" 'yank-from-kill-ring
  "e" 'conn-emacs-state-overwrite
  "E" 'conn-emacs-state-overwrite-binary
  "b" conn-backward-char-remap
  "f" conn-forward-char-remap
  "q" 'conn-replace)

(define-keymap
  :keymap (conn-get-state-map 'conn-kill-state)
  "w" 'copy
  "/" 'buffer-filename
  "$" 'project-filename
  ">" 'kill-matching-lines
  "%" 'keep-lines
  "b" 'move-end-of-line)

(define-keymap
  :keymap (conn-get-state-map 'conn-copy-state)
  "b" 'move-end-of-line)

(define-keymap
  :keymap (conn-get-state-map 'conn-duplicate-state)
  "c" 'copy-from-above-command)

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-thing-state)
  "z" 'conn-exchange-mark-command)

(static-if (<= 30 emacs-major-version)
    (define-keymap
      :keymap conn-replace-thing-argument-map
      "D" 'as-diff
      "/" 'multi-file-as-diff
      "$" 'as-diff-in-project))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-transpose-state)
  "TAB" 'repeat-dispatch
  "C-w" 'restrict-windows
  "SPC" 'scroll-up-command
  "DEL" 'scroll-down-command
  "C-o" 'other-window)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-transpose-state 'conn--replace-reading)
  ";" (conn-anonymous-thing
        '(sexp)
        :transpose-op ( :method (_self _arg _at-point-and-mark)
                        (conn--query-replace-read-transpose-from-to))))

;;;; Argument Keymaps

(define-keymap
  :keymap conn-register-argument-map
  "." 'register)

(define-keymap
  :keymap conn-swap-argument-map
  "~" 'swap)

(define-keymap
  :keymap conn-replace-thing-argument-map
  "$" 'project
  "/" 'multi-file
  "'" 'kapply
  "e" 'conn-emacs-state)

(define-keymap
  :keymap conn-regexp-argument-map
  "q" 'regexp)

(define-keymap
  :keymap conn-delimited-argument-map
  "l" 'delimited)

(define-keymap
  :keymap conn-backward-argument-map
  "z" 'backward)

(define-keymap
  :keymap conn-transpose-point-and-mark-argument-map
  "z" 'transpose-at-point-and-mark)

(define-keymap
  :keymap conn-delete-argument-map
  "k" 'delete)

(define-keymap
  :keymap conn-kill-append-argument-map
  "x" 'append
  "X" 'append-on-repeat)

(define-keymap
  :keymap conn-copy-thing-argument-map
  "/" 'buffer-filename
  "$" 'project-filename
  ">" 'copy-matching-lines)

(define-keymap
  :keymap conn-sort-reverse-argument-map
  "r" 'reverse)

(define-keymap
  :keymap conn-sort-fold-case-argument-map
  "g" 'sort-fold-case)

(define-keymap
  :keymap conn-kapply-state-argument-map
  "s" 'kapply-state)

(define-keymap
  :keymap conn-kapply-order-argument-map
  "o" 'kapply-order)

(define-keymap
  :keymap conn-kapply-empty-argument-map
  "," 'kapply-empty)

(define-keymap
  :keymap conn-kapply-ibuffer-argument-map
  "b" 'kapply-ibuffer)

(define-keymap
  :keymap conn-kapply-undo-argument-map
  "u" 'kapply-undo)

(define-keymap
  :keymap conn-kapply-excursions-argument-map
  "x" 'save-excursions)

(define-keymap
  :keymap conn-kapply-restrictions-argument-map
  "n" 'save-restrictions)

(define-keymap
  :keymap conn-kapply-window-conf-argument-map
  "w" 'save-window-conf)

(define-keymap
  :keymap conn-kapply-query-argument-map
  "q" 'query)

(define-keymap
  :keymap conn-restrict-argument-map
  "v" 'restrict)

(define-keymap
  :keymap conn-read-pattern-map
  "p" 'read-pattern)

(define-keymap
  :keymap conn-yank-pop-repeat-map
  "C-y" 'conn-yank-unpop
  "y" 'yank-pop
  "Y" 'conn-yank-with-completion)

(define-keymap
  :keymap conn-replace-from-map
  "C-M-;" 'conn-replace-insert-separator)

(define-keymap
  :keymap conn-isearch-thing-map
  "/" 'multi-file
  "*" 'multi-buffer
  "$" 'project)

(define-keymap
  :keymap conn-transpose-repeat-map
  "?" 'conn-transpose-repeat-help
  "t" 'conn-transpose-repeat
  "T" 'conn-transpose-repeat-inverse)

(define-keymap
  :keymap conn-kill-dispatch-append-map
  "C-x" 'append)

(define-keymap
  :keymap conn-duplicate-repeat-map
  "M-?" 'conn-duplicate-repeat-help
  "TAB" 'conn-duplicate-indent-repeat
  "<tab>" 'conn-duplicate-indent-repeat
  "DEL" 'conn-duplicate-delete-repeat
  "<backspace>" 'conn-duplicate-delete-repeat
  "i" 'conn-duplicate-repeat
  "RET" 'conn-duplicate-repeat-toggle-padding
  "<return>" 'conn-duplicate-repeat-toggle-padding
  "c" 'conn-duplicate-repeat-comment)

(define-keymap
  :keymap conn-indent-cleanup-whitespace-map
  "w" 'cleanup-whitespace)

(define-keymap
  :keymap conn-indent-thing-rigidly-map
  "f" #'conn-indent-right
  "b" #'conn-indent-left
  "F" #'conn-indent-right-to-tab-stop
  "B" #'conn-indent-left-to-tab-stop
  "?" #'conn-indent-rigidly-reference)

(define-keymap
  :keymap conn-indirect-map
  "i" 'indirect)

(define-keymap
  :keymap conn-shell-command-replace-map
  "w" 'replace)

(define-keymap
  :keymap conn-surround-padding-argument-map
  "TAB" 'conn-padding-flag)

(define-keymap
  :keymap conn-surround-trim-argument-map
  "q" 'trim)

;;;; Expand

(define-keymap
  :keymap conn-expand-repeat-map
  "e" 'ignore
  "z" 'exchange-point-and-mark
  "j" 'conn-contract
  "l" 'conn-expand)

;;;; Dispatch

(define-keymap
  :keymap conn-dispatch-transform-argument-map
  "a" 'conn-dispatch-bounds-anchored
  "B" 'conn-dispatch-bounds-between
  "<" 'conn-bounds-trim
  "c" 'conn-dispatch-bounds-over
  "T" 'conn-transform-reset)

;;;; Things

(define-keymap
  :keymap conn-recursive-edit-thing-map
  "R" 'recursive-edit
  "r" 'recursive-edit-mark)

(define-keymap
  :keymap conn-subregions-argument-map
  "~" 'toggle-subregions)

(define-keymap
  :keymap conn-reformat-argument-map
  "TAB" 'reformat)

(define-keymap
  :keymap conn-check-bounds-argument-map
  "q" 'check-bounds)

(define-keymap
  :keymap conn-transform-map
  "<" 'conn-bounds-trim
  "a" 'conn-bounds-after-point
  "A" 'conn-bounds-after-point-exclusive
  "e" 'conn-bounds-before-point
  "E" 'conn-bounds-before-point-exclusive
  "t" 'conn-bounds-last
  "T" 'conn-bounds-upto)

;;;; Wincontrol

(defvar-keymap conn-window-resize-map
  "f" 'toggle-frame-fullscreen
  "v" 'conn-wincontrol-maximize-vertically
  "r" 'conn-wincontrol-maximize-horizontally
  "m" 'maximize-window
  "b" 'balance-windows
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-window-resize-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "o" 'other-window)

(defvar-keymap conn-other-window-tab-repeat-map
  :repeat t
  "TAB" 'other-window
  "<tab>" 'other-window)

(defvar-keymap conn-wincontrol-next-window-repeat-map
  :repeat t
  "o" 'conn-wincontrol-next-window)

(defvar-keymap conn-wincontrol-scroll-repeat-map
  :repeat t
  "n" 'conn-wincontrol-scroll-down
  "m" 'conn-wincontrol-scroll-up
  "N" 'conn-wincontrol-other-window-scroll-up
  "M" 'conn-wincontrol-other-window-scroll-down)

(defvar-keymap conn-wincontrol-text-scale-repeat-map
  :repeat t
  "z" 'text-scale-decrease
  "Z" 'text-scale-increase)

(defvar-keymap conn-wincontrol-tab-repeat-map
  :repeat t
  "O" 'tab-bar-duplicate-tab
  "E" 'tab-new
  "o" 'tab-next
  "U" 'tab-close
  "u" 'tab-previous
  "G" 'tab-bar-move-window-to-tab
  "D" 'tab-bar-detach-tab
  "B" 'tab-bar-switch-to-tab)

(defvar-keymap conn-buffer-one-command-repeat-map
  :repeat t
  "J" 'bury-buffer
  "L" 'unbury-buffer
  "l" 'next-buffer
  "j" 'previous-buffer)

(defvar-keymap conn-buffer-one-command-repeat-map
  :repeat t
  "P" 'bury-buffer
  "N" 'unbury-buffer
  "n" 'next-buffer
  "p" 'previous-buffer)

(define-keymap
  :keymap conn-wincontrol-map
  "M-?" 'conn-wincontrol-quick-ref
  "C-l" 'recenter-top-bottom
  "-" 'conn-wincontrol-invert-argument
  "0" 'conn-wincontrol-digit-argument
  "1" 'conn-wincontrol-digit-argument
  "2" 'conn-wincontrol-digit-argument
  "3" 'conn-wincontrol-digit-argument
  "4" 'conn-wincontrol-digit-argument
  "5" 'conn-wincontrol-digit-argument
  "6" 'conn-wincontrol-digit-argument
  "7" 'conn-wincontrol-digit-argument
  "8" 'conn-wincontrol-digit-argument
  "9" 'conn-wincontrol-digit-argument
  ";" 'conn-wincontrol-exit-to-initial-win
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward
  "C-M-d" 'delete-other-frames
  "M-r" 'move-to-window-line-top-bottom
  "C-]" 'conn-wincontrol-abort
  "C-u" 'conn-wincontrol-universal-arg
  "DEL" 'conn-wincontrol-backward-delete-arg
  "<backspace>" 'conn-wincontrol-backward-delete-arg
  "M-/" 'undelete-frame
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-DEL" 'conn-wincontrol-digit-argument-reset
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "M-o" 'other-frame
  "<escape>" 'conn-wincontrol-exit
  "<down>" 'windmove-down
  "<left>" 'windmove-left
  "<right>" 'windmove-right
  "<up>" 'windmove-up
  "<next>" 'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "w" 'conn-other-place-prefix
  "j" 'other-window
  "G" 'tab-bar-move-window-to-tab
  "I" 'tab-bar-switch-to-tab
  "F" 'tab-bar-duplicate-tab
  "J" 'tab-bar-detach-tab
  "H" 'conn-kill-this-buffer
  "d" 'windmove-down
  "l" 'windmove-left
  "r" 'windmove-right
  "u" 'windmove-up
  "D" 'windmove-swap-states-down
  "L" 'windmove-swap-states-left
  "R" 'windmove-swap-states-right
  "U" 'windmove-swap-states-up
  "P" 'conn-wincontrol-other-window-scroll-down
  "N" 'conn-wincontrol-other-window-scroll-up
  "E" 'tab-new
  "R" 'conn-wincontrol-isearch-other-window-backward
  "S" 'conn-wincontrol-isearch-other-window
  "B" 'tab-close
  "Z" 'text-scale-increase
  "T" 'tear-off-window
  "[" 'conn-previous-buffer
  "]" 'conn-next-buffer
  "{" 'conn-bury-buffer
  "}" 'conn-unbury-buffer
  "`" 'conn-wincontrol-mru-window
  "c" 'delete-window
  "k" 'conn-delete-window
  "e" 'conn-wincontrol-exit
  "K" 'delete-other-windows
  "<tab>" 'conn-goto-window
  "TAB" 'conn-goto-window
  "g" (conn-remap-key "M-g" t t)
  "x" (conn-remap-key "C-x" t t)
  "C" 'kill-buffer-and-window
  "P" 'conn-wincontrol-scroll-down
  "N" 'conn-wincontrol-scroll-up
  "f" 'tab-next
  "," 'conn-register-prefix
  "." 'conn-register-load
  "q" 'quit-window
  "s" 'conn-wincontrol-split-right
  "i" conn-window-resize-map
  "t" 'conn-transpose-window
  "b" 'tab-previous
  "v" 'conn-wincontrol-split-vertically
  "h" 'conn-throw-buffer
  "y" 'conn-yank-window
  "z" 'text-scale-decrease)

(define-keymap
  :keymap conn-wincontrol-one-command-map
  "d" 'kill-buffer-and-window
  "u" 'conn-kill-this-buffer
  "U" 'undefined
  "D" 'undefined
  "p" 'previous-buffer
  "n" 'next-buffer
  "P" 'bury-buffer
  "N" 'unbury-buffer
  "e" 'delete-other-windows)

(defvar-keymap conn-kill-buffer-repeat-map
  :repeat t
  "u" 'conn-kill-this-buffer
  "P" 'conn-bury-buffer
  "N" 'conn-unbury-buffer
  "n" 'conn-next-buffer
  "p" 'conn-previous-buffer)

(static-if (<= 31 emacs-major-version)
    (progn
      (define-keymap
        :keymap conn-wincontrol-map
        "\\" 'window-layout-transpose
        "|" 'window-layout-flip-leftright
        "_" 'window-layout-flip-topdown
        "<" 'rotate-windows-back
        ">" 'rotate-windows)

      (define-keymap
        :keymap conn-window-resize-map
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise)

      (defvar-keymap conn-window-rotate-repeat-map
        :repeat t
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise)))

(provide 'conn-keymaps-generic)
