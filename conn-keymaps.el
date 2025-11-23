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

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-state)
  "p" conn-thing-inner-remap
  "@" 'append-next-kill
  "TAB" 'indent-rigidly
  "Y" 'conn-completing-yank-replace
  "y" 'conn-yank-replace
  "*" 'calc-grab-region
  "v" 'rectangle-mark-mode
  "V" 'undefined
  "g" 'conn-surround
  "RET" 'conn-duplicate-thing
  "SPC" 'conn-push-mark-command)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "," conn-thing-remap
  "'" 'recursive-edit
  "c" 'conn-things-in-region)

;;;; Keymaps

;;;;; Repeat Map

(defvar-keymap conn-pop-mark-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "u" 'conn-pop-mark-ring
  "o" 'conn-unpop-mark-ring)

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
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "<backspace>" 'clear-rectangle
  "SPC d" 'delete-whitespace-rectangle
  "SPC o" 'open-rectangle
  "#" 'rectangle-number-lines)
(conn-set-mode-map-depth 'rectangle-mark-mode -90 'conn-command-state)

(defvar-keymap conn-isearch-map
  "M-Y" 'conn-isearch-yank-region
  "M-<return>" 'conn-isearch-exit-and-mark
  "C-<return>" 'conn-isearch-exit-other-end
  "M-RET" 'conn-isearch-exit-and-mark
  "M-'" 'conn-isearch-kapply-prefix
  "C-," 'conn-dispatch-isearch
  "C-'" 'conn-isearch-open-recursive-edit)

;;;;; Top-level Command State Maps

(define-keymap
  :keymap conn-default-region-map
  "m" 'conn-replace
  "u" 'conn-regexp-replace
  "'" 'conn-kapply-on-thing-prefix
  "TAB" 'indent-rigidly
  "$" 'ispell-region
  "*" 'calc-grab-region
  ";" 'conn-comment-thing
  "g" 'conn-rgrep-region
  "k" 'delete-region
  "j" 'conn-join-lines
  "o" 'conn-occur-region
  "V" 'vc-region-history
  "s" 'conn-isearch-region-forward
  "r" 'conn-isearch-region-backward
  "n" 'conn-narrow-to-thing)

(define-keymap
  :keymap conn-default-edit-map
  "v" 'diff-buffer-with-file
  "SPC" 'whitespace-cleanup
  "f" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "L" 'clone-indirect-buffer
  "i" 'conn-emacs-state-open-line-above
  "k" 'conn-emacs-state-open-line
  "d" 'conn-duplicate-thing
  "y" 'yank-in-context
  "s" 'conn-sort-prefix
  "r" 'yank-rectangle
  "DEL" 'clear-rectangle
  "a c" 'align-current
  "a e" 'align-entire
  "a h" 'align-highlight-rule
  "a n" 'align-newline-and-indent
  "a r" 'align-regexp
  "a u" 'align-unhighlight-rule)

(define-keymap
  :keymap conn-search-map
  "f" 'conn-dispatch-thing-at-point
  "h '" 'conn-kapply-hightlight-prefix
  "s" 'conn-isearch-forward
  "r" 'conn-isearch-backward
  "o" 'occur
  "l" 'locate)

(defvar-keymap conn-global-mark-repeat-map
  :repeat t
  "n" 'pop-global-mark)

(define-keymap
  :keymap conn-goto-map
  "i" 'previous-error
  "k" 'next-error
  "n" 'pop-global-mark
  "m" 'imenu
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward
  "J" 'conn-pop-movement-ring
  "L" 'conn-unpop-movement-ring)

(defvar-keymap conn-movement-ring-repeat-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "J" 'conn-pop-movement-ring
  "L" 'conn-unpop-movement-ring
  "j" 'conn-pop-movement-ring
  "l" 'conn-unpop-movement-ring)

;;;;; Misc Maps

(defvar-keymap conn-indent-rigidly-map
  "l" 'indent-rigidly-right
  "j" 'indent-rigidly-left
  "L" 'indent-rigidly-right-to-tab-stop
  "J" 'indent-rigidly-left-to-tab-stop)

;;;;; Global Bindings

(defvar-keymap conn-dispatch-cycle-map
  :repeat (:exit (ignore))
  "e" 'ignore
  "l" 'conn-dispatch-cycle-ring-next
  "j" 'conn-dispatch-cycle-ring-previous)

(defvar-keymap conn-last-emacs-state-repeat-map
  :repeat t
  "M-p" 'conn-previous-emacs-state
  "M-n" 'conn-next-emacs-state)

(put 'conn-next-emacs-state 'repeat-check-key 'no)
(put 'conn-previous-emacs-state 'repeat-check-key 'no)

(defvar-keymap conn-local-mode-map
  "C-<escape>" 'exit-recursive-edit
  "C-x y" conn-dispatch-cycle-map
  "M-g u" 'conn-pop-mark-ring
  "M-g o" 'conn-unpop-mark-ring
  "M-g e" 'conn-previous-emacs-state
  "M-g E" 'conn-next-emacs-state
  "C-S-w" 'delete-region
  "C-," 'conn-dispatch
  "C->" 'conn-dispatch-on-buttons
  "C-x /" 'tab-bar-history-back
  "C-x 4 /" 'tab-bar-history-back
  "C-x 4 ?" 'tab-bar-history-forward
  "C-x 4 -" 'conn-window-resize-map
  "C-x ?" 'tab-bar-history-forward
  "C-x t s" 'tab-switch
  "C-x t a" 'conn-tab-to-register
  "C-`" 'other-window
  "C-x m" 'conn-kmacro-prefix
  "M-H" 'conn-wincontrol-maximize-horizontally
  "M-V" 'conn-wincontrol-maximize-vertically)

(defvar-keymap conn-whitespace-repeat-map
  :repeat t
  "w" 'forward-whitespace
  "W" 'conn-backward-whitespace)

(defvar-keymap conn-line-repeat-map
  :repeat t
  ">" 'forward-line
  "<" 'conn-backward-line)

(defvar-keymap conn-list-repeat-map
  :repeat t
  ")" 'forward-list
  "(" 'backward-list)

(define-keymap
  :keymap conn-default-thing-map
  "p" conn-thing-inner-remap
  "," conn-thing-inner-remap
  "w" 'forward-whitespace
  "W" 'conn-backward-whitespace
  ">" 'forward-line
  "<" 'conn-backward-line
  ";" 'conn-mark-comment
  "v" 'conn-mark-visual-line
  "V" 'conn-mark-visible
  "g" 'conn-goto-line
  "/" 'conn-mark-filename
  "U" 'conn-mark-uuid
  "s" 'conn-mark-string
  "@" 'conn-mark-email
  ")" 'forward-list
  "(" 'backward-list
  "a" 'beginning-of-buffer
  "e" 'end-of-buffer
  "h" 'outline-previous-visible-heading)

(static-if (<= 30 emacs-major-version)
    (progn
      (keymap-global-set "<conn-edit-map> W" 'replace-regexp-as-diff)
      (keymap-global-set "<conn-edit-map> Q" 'multi-file-replace-regexp-as-diff)))

(keymap-set
 (with-memoization (alist-get 'conn-kmacro-applying-p minor-mode-map-alist)
   (make-sparse-keymap))
 "<remap> <kbd-macro-query>" 'conn-kapply-kbd-macro-query)

;;;;; State Keymaps

;;;;;; Mark State

(define-keymap
  :keymap (conn-get-state-map 'conn-mark-state)
  "<remap> <conn-pop-state>" 'conn-pop-mark-state
  "e" 'conn-emacs-state
  "TAB" 'indent-rigidly
  "Y" 'conn-completing-yank-replace
  "y" 'conn-yank-replace
  "*" 'calc-grab-region
  "C-j" 'conn-join-lines-in-region
  "v" 'rectangle-mark-mode
  "V" 'undefined
  "g" 'conn-surround
  "RET" 'conn-duplicate-thing
  "SPC" 'conn-push-mark-command)

;;;;;; Emacs State

(keymap-set (conn-get-state-map 'conn-emacs-state)
            "<escape>" 'conn-pop-state)

;;;;;; Read Thing State

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-common-state)
  "M-h" 'conn-mark-heading
  "C-s" 'isearch-forward
  "s" 'isearch-forward
  "r" 'isearch-backward
  "C-r" 'isearch-backward
  "C-M-s" 'isearch-forward-regexp
  "C-M-r" 'isearch-backward-regexp
  ";" 'comment
  "d" 'conn-forward-defun
  "i" 'conn-backward-line
  "k" 'forward-line
  "h" 'conn-expand
  "," conn-thing-remap
  "p" conn-thing-inner-remap
  "e" 'end-of-buffer)

;;;;;; Command State

(define-keymap
  :keymap (conn-get-state-map 'conn-command-state)
  :suppress t
  "F" 'conn-bind-last-dispatch-to-key
  "=" 'conn-repeat-last-complex-command
  "S-<return>" 'conn-open-line-and-indent
  "p" 'conn-other-window-prefix
  "o" conn-forward-word-remap
  "O" 'forward-symbol
  "U" 'conn-backward-symbol
  "u" conn-backward-word-remap
  "(" conn-backward-list-remap
  ")" conn-forward-list-remap
  "[" conn-backward-up-list-remap
  "]" conn-down-list-remap
  "{" conn-backward-sentence-remap
  "}" conn-forward-sentence-remap
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
  "G" 'conn-surround
  "<escape>" 'conn-pop-state
  "D" 'conn-duplicate-thing
  "+" 'conn-set-register-separator
  "H" 'conn-expand
  "SPC" conn-edit-remap
  "Z" 'pop-to-mark-command
  "&" 'conn-other-buffer
  "e" 'conn-pop-state
  "E" 'conn-emacs-state-at-mark
  "`" 'conn-wincontrol-mru-window
  "|" 'conn-shell-command-on-region
  "\\" 'conntext-state
  "/" (conn-remap-key conn-undo-keys t)
  ";" 'conn-wincontrol
  "'" 'conn-kapply-prefix
  "?" (conn-remap-key conn-undo-redo-keys t)
  "_" 'repeat-complex-command
  "M-y" 'conn-completing-yank-replace
  "C-M-l" 'conn-recenter-on-region
  "C-M-S-l" 'conn-recenter-on-region-other-window
  "C-y" 'conn-yank-replace
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "C" 'conn-copy-region
  "t" 'conn-copy-thing
  "w" 'conn-change-thing
  "f" 'conn-dispatch
  "h" 'conn-wincontrol-one-command
  "," conn-thing-remap
  "." 'conn-register-load
  ">" 'conn-register-load-and-replace
  "<" 'conn-register-prefix
  "q" 'conn-transpose-things
  "r" conn-region-remap
  "v" 'conn-toggle-mark-command
  "V" 'conn-previous-mark-command
  "b" 'conn-set-mark-command
  "d" 'conn-kill-thing
  "W" 'widen
  "X" 'conn-narrow-ring-prefix
  "Y" 'yank-from-kill-ring
  "y" (conn-remap-key "C-y" t)
  "z" 'conn-exchange-mark-command)

;;;;;; Dispatch State

(define-keymap
  :keymap conn-dispatch-read-char-map
  "C-\\" 'toggle-input-method
  "C-M-\\" 'set-input-method
  "C-z" 'dispatch-other-end
  "DEL" 'restart
  "<backspace>" 'restart
  "C-/" 'undo
  "C-'" 'recursive-edit
  "<mouse-1>" 'act
  "<mouse-3>" 'undo
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  ;; "C-f" 'retarget
  ;; "M-f" 'always-retarget
  "C-t" 'change-target-finder
  "<escape>" 'finish
  "C-o" 'conn-goto-window
  "C-s" 'isearch-forward
  "C-M-s" 'isearch-regexp-forward
  "C-M-r" 'isearch-regexp-backward
  "C-v" 'scroll-up
  "M-v" 'scroll-down)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-targets-state)
  "TAB" 'repeat-dispatch
  "u" 'forward-symbol
  "i" 'forward-line
  "k" 'next-line
  "," conn-thing-remap)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-thingatpt-state)
  "u" conn-backward-word-remap
  "n" conn-backward-sexp-remap)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-bounds-state)
  "O" (conn-anonymous-thing
        'forward-word
        :pretty-print (:method (_self) "all-words")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'word)))
  "U" (conn-anonymous-thing
        'forward-symbol
        :pretty-print (:method (_self) "all-symbols")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'symbol))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-targets-state :override)
  "<remap> <conn-expand>" (conn-anonymous-thing
                            'expansion
                            :pretty-print ( :method (_) "conn-expand")
                            :bounds-op ( :method (_self arg)
                                         (conn--push-ephemeral-mark)
                                         (conn-bounds-of 'conn-expand arg)))
  "<conn-thing-map> e" 'move-end-of-line
  "<conn-thing-map> a" 'move-beginning-of-line
  "O" (conn-anonymous-thing
        'word
        :pretty-print (:method (_self) "all-words")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'word)))
  "U" (conn-anonymous-thing
        'symbol
        :pretty-print (:method (_self) "all-symbols")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'symbol)))
  "b" 'conn-dispatch-buttons)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-targets-state)
  ")" (conn-anonymous-thing
        'forward-sexp
        :pretty-print (:method (_self) "list")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-things-with-re-prefix-targets
                          :thing 'sexp
                          :prefix-regexp (rx (syntax open-parenthesis)))))
  "]" (conn-anonymous-thing
        'sexp
        :pretty-print (:method (_self) "inner-list")
        :bounds-op ( :method (_self arg)
                     (conn-bounds-of 'down-list arg))
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-things-with-re-prefix-targets
                          :thing 'sexp
                          :prefix-regexp (rx (syntax open-parenthesis))))))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-state)
  "'" 'conn-dispatch-kapply
  "t" 'conn-dispatch-copy-to
  "T" 'conn-dispatch-copy-to-replace
  "C-y" 'conn-dispatch-yank-to-replace
  "M-y" 'conn-dispatch-reading-yank-to-replace
  "y" 'conn-dispatch-yank-to
  "Y" 'conn-dispatch-reading-yank-to
  "f" 'conn-dispatch-copy-from
  "F" 'conn-dispatch-copy-from-replace
  "s" 'conn-dispatch-send
  "S" 'conn-dispatch-send-replace
  "d" 'conn-dispatch-take
  "D" 'conn-dispatch-take-replace
  "q" 'conn-dispatch-transpose
  "." 'conn-dispatch-register-load
  ">" 'conn-dispatch-register-load-replace
  "=" 'conn-dispatch-repeat-command
  "RET" 'conn-repeat-last-dispatch
  "<return>" 'conn-repeat-last-dispatch
  "M-n" 'conn-dispatch-cycle-ring-next
  "M-p" 'conn-dispatch-cycle-ring-previous
  "M-f" 'conn-dispatch-ring-describe-head
  "e" (conn-anonymous-thing
        'point
        :pretty-print ( :method (_) "prev-emacs-state")
        :default-action ( :method (_self)
                          (let ((goto (conn-make-action 'conn-dispatch-goto)))
                            (oclosure-lambda (conn-action
                                              (action-description "Previous Emacs State")
                                              (action-no-history t))
                                ()
                              (funcall goto)
                              (conn-push-state 'conn-emacs-state))))
        :target-finder (:method (_self _arg) (conn-dispatch-previous-emacs-state)))
  "j" 'point
  "g y" (conn-anonymous-thing
          'point
          :pretty-print ( :method (_) "global-mark-ring")
          :target-finder (:method (_self _arg) (conn-dispatch-global-mark)))
  "<" (conn-anonymous-thing
        'point
        :pretty-print ( :method (_) "position-registers")
        :target-finder (:method (_self _arg) (conn-dispatch-mark-register)))
  "<remap> <conn-pop-mark-ring>"
  (conn-anonymous-thing
    'point
    :pretty-print ( :method (_) "mark-ring")
    :target-finder (:method (_self _arg) (conn-dispatch-mark-ring))))

;;;;;; Transpose State

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "i" 'conn-backward-line
  "u" 'forward-symbol
  "f" 'conn-dispatch)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-transpose-state)
  "TAB" 'repeat-dispatch
  "C-w" 'restrict-windows
  "SPC" 'scroll-up
  "DEL" 'scroll-down
  "C-o" 'other-window
  "o" (conn-anonymous-thing
        'word
        :pretty-print (:method (_self) "all-words")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'word)))
  "u" (conn-anonymous-thing
        'symbol
        :pretty-print (:method (_self) "all-symbols")
        :target-finder ( :method (_self _arg)
                         (conn-dispatch-all-things 'symbol))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state
                                   'conn-transpose-recursive-edit-mode)
  "d" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(provide 'conn-keymaps)
