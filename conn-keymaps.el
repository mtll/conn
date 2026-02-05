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

;; The primary conn keymaps

;;; Code

(require 'compat)
(require 'conn-states)
(require 'conn-things)
(require 'conn-commands)
(require 'conn-dispatch)

;;;; Keymaps

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

;;;;; Repeat Map

(defvar-keymap conn-mru-window-repeat-map
  :repeat t
  "`" 'conn-wincontrol-mru-window)

(defvar-keymap conn-tab-bar-history-repeat-map
  :repeat t
  "/" 'tab-bar-history-back
  "?" 'tab-bar-history-forward)

;;;;; Mode Keymaps

(defvar-keymap conn-error-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "i" 'previous-error
  "k" 'next-error)

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

;;;;; Top-level Command State Maps

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
  "F" 'conn-dispatch-thing-at-point
  "h '" 'conn-kapply-on-highlights
  "s" 'conn-isearch-forward
  "r" 'conn-isearch-backward
  "o" 'occur
  "l" 'locate)

(defvar-keymap conn-global-mark-repeat-map
  :repeat t
  "n" 'pop-global-mark)

(defvar-keymap conn-xref-repeat-map
  :repeat t
  "<" 'xref-go-back
  ">" 'xref-go-forward)

(defvar-keymap conn-mark-ring-repeat-map
  :repeat t
  "o" 'conn-pop-mark-ring
  "u" 'conn-unpop-mark-ring)

(define-keymap
  :keymap conn-goto-map
  "v" 'conn-previous-mark-command
  "e" 'conn-previous-emacs-state
  "E" 'conn-next-emacs-state
  "O" 'conn-push-jump-ring
  "o" 'conn-pop-mark-ring
  "u" 'conn-unpop-mark-ring
  "i" 'previous-error
  "k" 'next-error
  "n" 'pop-global-mark
  "m" 'imenu
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "<" 'xref-go-back
  ">" 'xref-go-forward)

;;;;; Global Bindings

(defvar-keymap conn-dispatch-cycle-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "l" 'conn-dispatch-cycle-ring-next
  "j" 'conn-dispatch-cycle-ring-previous)

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

(defvar-keymap conn-line-repeat-map
  :repeat t
  "k" 'forward-line
  "i" 'conn-backward-line)

(defvar-keymap conn-list-repeat-map
  :repeat t
  ")" 'forward-list
  "(" 'backward-list)

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

;;;;; Minibuffer

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-emacs-state 'minibuffer-mode)
  "C-t" 'conn-yank-thing-to-minibuffer)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-command-state 'minibuffer-mode)
  "C-t" 'conn-yank-thing-to-minibuffer)

;;;;; State Keymaps

;;;;;; Mark State

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

;;;;;; Emacs State

(keymap-set (conn-get-state-map 'conn-emacs-state)
            "<escape>" 'conn-pop-state)

;;;;;; Read Thing State

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
  "k" 'forward-line
  "i" 'conn-backward-line
  "e" 'conn-expand
  "L" 'conn-forward-inner-line
  "J" 'conn-backward-inner-line
  "S" 'conn-thing-at-isearch
  "C-s" 'isearch-forward
  "C-r" 'isearch-backward
  "C-M-s" 'isearch-forward-regexp
  "C-M-r" 'isearch-backward-regexp
  "c" 'comment
  "q" conn-thing-inner-remap)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-read-thing-common-state 'outline-minor-mode)
  "M-h" 'outline-up-heading)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-read-thing-common-state 'outline-mode)
  "M-h" 'outline-up-heading)

;;;;;; Command State

(define-keymap
  :keymap (conn-get-state-map 'conn-command-state)
  :suppress t
  "," conn-thing-remap
  "#" 'eshell
  "$" 'project-eshell
  "S-<return>" 'conn-open-line-and-indent
  "p" 'conn-other-window-prefix
  "o" conn-forward-word-remap
  "O" 'forward-symbol
  "U" 'conn-backward-symbol
  "u" conn-backward-word-remap
  "{" 'conn-backward-up-inner-list
  "}" 'conn-forward-up-inner-list
  "(" 'backward-up-list
  ")" 'conn-forward-up-list
  "[" 'conn-backward-down-list
  "]" 'down-list
  "G" 'conn-adjust-surround
  "C" 'conn-surround-raise
  "I" conn-backward-paragraph-remap
  "i" conn-previous-line-remap
  "J" 'conn-backward-inner-line-dwim
  "j" conn-backward-char-remap
  "K" conn-forward-paragraph-remap
  "k" conn-next-line-remap
  "L" 'conn-forward-inner-line-dwim
  "l" conn-forward-char-remap
  "M" conn-end-of-defun-remap
  "m" conn-forward-sexp-remap
  "N" conn-beginning-of-defun-remap
  "n" conn-backward-sexp-remap
  "s" conn-search-remap
  "g" conn-goto-remap
  "c" (conn-remap-key "C-c" t)
  "x" (conn-remap-key "C-x" t)
  "C-4" (conn-remap-key "C-x 4" t)
  "C-5" (conn-remap-key "C-x 5" t)
  "S" 'conn-surround
  "<escape>" 'conn-pop-state
  "q" 'conn-duplicate-thing
  "+" 'conn-set-register-separator
  "E" 'conn-expand
  "b" 'conn-repeat
  "&" 'conn-other-buffer
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
  "f" 'conn-dispatch
  "w" 'conn-wincontrol-one-command
  "." 'conn-register-load
  "<" 'point-to-register
  "t" 'conn-transpose-things
  "v" 'conn-mark-thing
  "h" conn-edit-remap
  "d" 'conn-kill-thing
  "W" 'conn-widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" (conn-remap-key "C-y" t)
  "z" 'conn-yank-replace
  "Z" 'conn-exchange-mark-command)

;;;;;; Dispatch State

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-targets-state)
  "TAB" 'repeat-dispatch
  "u" 'forward-symbol
  "i" 'forward-line
  "k" 'next-line
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
  :keymap (conn-get-state-map 'conn-dispatch-thingatpt-state)
  "u" conn-backward-word-remap
  "n" conn-backward-sexp-remap)

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
  "O" (conn-anonymous-thing
        '(word)
        :pretty-print (:method (_self) "all-words")
        :target-finder ( :method (_self _arg)
                         (conn-all-things-targets :thing 'word)))
  "U" (conn-anonymous-thing
        '(symbol)
        :pretty-print (:method (_self) "all-symbols")
        :target-finder ( :method (_self _arg)
                         (conn-all-things-targets :thing 'symbol)))
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
  "f" 'conn-dispatch-copy-from
  "x" 'conn-dispatch-grab
  "X" 'conn-dispatch-grab-replace
  "d" 'conn-dispatch-send
  "D" 'conn-dispatch-send-replace
  "t" 'conn-dispatch-transpose
  "." 'conn-dispatch-register-load
  ">" 'conn-dispatch-register-load-replace
  "B" 'conn-dispatch-repeat-command
  "=" 'conn-dispatch-repeat-command
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
  "j" 'point
  "S-SPC" (conn-anonymous-thing
            '(point)
            :pretty-print ( :method (_) "global-mark-ring")
            :target-finder (:method (_self _arg) (conn-dispatch-global-mark)))
  "<" (conn-anonymous-thing
        '(point)
        :pretty-print ( :method (_) "position-registers")
        :target-finder (:method (_self _arg) (conn-dispatch-mark-register))))

(provide 'conn-keymaps)
