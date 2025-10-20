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
  "@" 'append-next-kill
  "TAB" 'indent-rigidly
  "Y" 'conn-completing-yank-replace
  "y" 'conn-yank-replace
  "*" 'calc-grab-region
  "v" 'rectangle-mark-mode
  "V" 'undefined
  "g" 'conn-surround
  "RET" 'conn-duplicate
  "S-<return>" 'conn-duplicate-and-comment-region
  "SPC" 'conn-push-mark-command)

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-state)
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-q" 'help
  "," conn-thing-remap
  "'" 'recursive-edit
  "c" 'conn-things-in-region)

;;;; Keymaps

;;;;; Repeat Map

(defvar-keymap conn-pop-mark-repeat-map
  :repeat t
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

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-mode)
              "C-c e" 'occur-edit-mode))

(dolist (state '(conn-command-state conn-emacs-state))
  (keymap-set (conn-get-major-mode-map state 'occur-edit-mode)
              "C-c e" 'occur-cease-edit))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-command-state 'compilation-mode)
  "<" 'previous-error-no-select
  ">" 'next-error-no-select)

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state 'rectangle-mark-mode)
  "<conn-region-map> '" 'conn-kapply-on-rectangle-prefix
  "z" 'rectangle-exchange-point-and-mark
  "C-y" 'conn-yank-replace-rectangle
  "*" 'calc-grab-rectangle
  "+" 'calc-grab-sum-down
  "_" 'calc-grab-sum-across
  "y" 'yank-rectangle
  "DEL" 'clear-rectangle
  "<backspace>" 'clear-rectangle
  "d" 'open-rectangle
  "C-d" 'delete-whitespace-rectangle
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

(defvar-keymap conn-default-region-map
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
  "N" 'conn-narrow-indirect
  "n" 'conn-narrow-to-thing)

(defvar-keymap conn-default-edit-map
  "v" 'diff-buffer-with-file
  "SPC" 'whitespace-cleanup
  "f" 'conn-fill-prefix
  "TAB" 'indent-for-tab-command
  "L" 'clone-indirect-buffer
  "i" 'conn-emacs-state-open-line-above
  "k" 'conn-emacs-state-open-line
  "d" 'conn-duplicate
  "D" 'conn-duplicate-and-comment
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

(defvar-keymap conn-search-map
  "f" 'conn-dispatch-thing-at-point
  "h '" 'conn-kapply-hightlight-prefix
  "s" 'conn-isearch-forward
  "r" 'conn-isearch-backward
  "o" 'occur
  "l" 'locate
  "m B" 'multi-isearch-buffers-regexp
  "m F" 'multi-isearch-files-regexp
  "m b" 'multi-isearch-buffers
  "m p" 'conn-multi-isearch-project
  "m f" 'multi-isearch-files)

(defvar-keymap conn-goto-map
  "r" 'xref-find-references
  "d" 'xref-find-definitions
  "s" 'xref-find-apropos
  "," 'xref-go-back
  "." 'xref-go-forward
  "J" 'conn-pop-movement-ring
  "L" 'conn-unpop-movement-ring)

(defvar-keymap conn-movement-ring-repeat-map
  :repeat t
  "J" 'conn-pop-movement-ring
  "L" 'conn-unpop-movement-ring
  "j" 'conn-pop-movement-ring
  "l" 'conn-unpop-movement-ring)

(defvar-keymap conn-global-mark-repeat-map
  :repeat t
  "p" 'pop-global-mark)

(defvar-keymap conn-error-repeat-map
  :repeat t
  "l" 'previous-error
  "j" 'next-error)

;;;;; Misc Maps

(defvar-keymap conn-indent-rigidly-map
  "l" 'indent-rigidly-right
  "j" 'indent-rigidly-left
  "L" 'indent-rigidly-right-to-tab-stop
  "J" 'indent-rigidly-left-to-tab-stop)

;;;;; Global Bindings

(defvar-keymap conn-dispatch-cycle-map
  :repeat t
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
  "C-." 'conn-dispatch
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

(define-keymap
  :keymap global-map
  "<conn-region-map>" conn-default-region-map
  "<conn-edit-map>" conn-default-edit-map
  "<conn-thing-map> >" 'forward-line
  "<conn-thing-map> <" 'conn-backward-line
  "<conn-thing-map> ;" 'conn-mark-comment
  "<conn-thing-map> V" 'conn-mark-visual-line
  "<conn-thing-map> ," 'conn-goto-line
  "<conn-thing-map> /" 'conn-mark-filename
  "<conn-thing-map> U" 'conn-mark-uuid
  "<conn-thing-map> s" 'conn-mark-string
  "<conn-thing-map> @" 'conn-mark-email
  "<conn-thing-map> v" 'conn-mark-visible
  "<conn-thing-map> L" 'forward-line
  "<conn-thing-map> )" 'forward-list
  "<conn-thing-map> (" 'backward-list
  "<conn-thing-map> a" 'beginning-of-buffer
  "<conn-thing-map> e" 'end-of-buffer
  "<conn-thing-map> h" 'outline-previous-visible-heading)

(static-if (<= 30 emacs-major-version)
    (progn
      (keymap-global-set "<conn-edit-map> W" 'replace-regexp-as-diff)
      (keymap-global-set "<conn-edit-map> Q" 'multi-file-replace-regexp-as-diff)))

(keymap-set
 (with-memoization (alist-get 'conn-kmacro-applying-p minor-mode-map-alist)
   (make-sparse-keymap))
 "<remap> <kbd-macro-query>" 'conn-kapply-kbd-macro-query)

;;;;; State Keymaps

;;;;;; Emacs State

(keymap-set (conn-get-state-map 'conn-emacs-state)
            "<escape>" 'conn-pop-state)

;;;;;; Read Thing State

(define-keymap
  :keymap (conn-get-state-map 'conn-read-thing-common-state)
  "<escape>" 'keyboard-quit
  "C-g" 'keyboard-quit
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
  "p" 'forward-paragraph
  "," conn-thing-remap
  "." conn-thing-inner-remap
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
  "R" 'conn-emacs-state-overwrite
  "D" 'conn-duplicate
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

(defvar-keymap conn-dispatch-common-map
  "C-z" 'dispatch-other-end
  "C-\\" 'toggle-input-method
  "C-M-\\" 'set-input-method)

(define-keymap
  :keymap conn-dispatch-read-event-map
  :parent conn-dispatch-common-map
  "C-q" 'help
  "C-/" 'undo
  "C-'" 'recursive-edit
  "<mouse-1>" 'act
  "<mouse-3>" 'undo
  "DEL" 'backward-delete-char
  "<backspace>" 'backward-delete-char
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "C-f" 'retarget
  "M-f" 'always-retarget
  "C-t" 'change-target-finder
  "<escape>" 'finish
  "C-o" 'conn-goto-window
  "C-s" 'isearch-forward
  "C-M-s" 'isearch-regexp-forward
  "C-M-r" 'isearch-regexp-backward
  "C-v" 'scroll-up
  "M-v" 'scroll-down
  "C-n" 'restrict-windows)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-mover-state)
  :parent conn-dispatch-common-map
  "z" 'dispatch-other-end
  "<escape>" 'finish
  "C-q" 'help
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg
  "TAB" 'repeat-dispatch
  "C-n" 'restrict-windows
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "u" 'forward-symbol
  "i" 'forward-line
  "k" 'next-line
  "n" conn-end-of-defun-remap
  "," conn-thing-remap
  "<remap> <conn-bounds-after-point>" 'undefined
  "<remap> <conn-bounds-before-point>" 'undefined)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-thingatpt-state)
  "u" conn-backward-word-remap
  "n" conn-backward-sexp-remap
  "k" 'forward-line)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-bounds-state)
  "O" (conn-anonymous-thing
       'forward-word
       :description "all-words"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'word)))
  "U" (conn-anonymous-thing
       'forward-symbol
       :description "all-symbols"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'symbol))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-mover-state :override)
  "<remap> <conn-expand>" (conn-anonymous-thing
                           'expansion
                           :bounds-op (lambda (arg)
                                        (conn--push-ephemeral-mark)
                                        (conn-bounds-of 'conn-expand arg)))
  "m" 'forward-sexp
  ";" 'conn-forward-inner-line
  "<conn-thing-map> e" 'move-end-of-line
  "<conn-thing-map> a" 'move-beginning-of-line
  "O" (conn-anonymous-thing
       'word
       :description "all-words"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'word)))
  "U" (conn-anonymous-thing
       'symbol
       :description "all-symbols"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'symbol)))
  "b" 'conn-dispatch-buttons)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-mover-state)
  ")" (conn-anonymous-thing
       'forward-sexp
       :description "list"
       :target-finder (lambda (_arg)
                        (conn-dispatch-things-with-re-prefix
                         'sexp (rx (syntax open-parenthesis)))))
  "]" (conn-anonymous-thing
       'sexp
       :description "inner-list"
       :bounds-op (lambda (arg)
                    (conn-bounds-of 'down-list arg))
       :target-finder (lambda (_arg)
                        (conn-dispatch-things-with-re-prefix
                         'sexp (rx (syntax open-parenthesis))))))

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-state)
  "=" 'conn-dispatch-repeat-command
  "'" 'conn-dispatch-kapply
  "RET" 'conn-repeat-last-dispatch
  "t" 'conn-dispatch-copy
  "<return>" 'conn-repeat-last-dispatch
  "M-n" 'conn-dispatch-cycle-ring-next
  "M-p" 'conn-dispatch-cycle-ring-previous
  "M-f" 'conn-dispatch-ring-describe-head
  "w" 'conn-dispatch-copy-to
  "W" 'conn-dispatch-copy-replace-to
  "v" 'conn-dispatch-over
  "x" 'conn-dispatch-yank-replace
  "X" 'conn-dispatch-yank-read-replace
  "C-y" 'conn-dispatch-yank-replace
  "M-y" 'conn-dispatch-yank-read-replace
  "y" 'conn-dispatch-yank
  "Y" 'conn-dispatch-reading-yank-to
  "c" 'conn-dispatch-copy-from
  "C" 'conn-dispatch-copy-from-replace
  "s" 'conn-dispatch-send
  "S" 'conn-dispatch-send-replace
  "F" 'conn-dispatch-grab-replace
  "f" 'conn-dispatch-grab
  "d" 'conn-dispatch-kill
  "q" 'conn-dispatch-transpose
  "SPC" 'conn-dispatch-jump
  "." 'conn-dispatch-register-load
  ">" 'conn-dispatch-register-replace
  "<remap> <downcase-word>" 'conn-dispatch-downcase
  "<remap> <downcase-region>" 'conn-dispatch-downcase
  "<remap> <downcase-dwim>" 'conn-dispatch-downcase
  "<remap> <upcase-word>" 'conn-dispatch-upcase
  "<remap> <upcase-region>" 'conn-dispatch-upcase
  "<remap> <upcase-dwim>" 'conn-dispatch-upcase
  "<remap> <capitalize-word>" 'conn-dispatch-capitalize
  "<remap> <capitalize-region>" 'conn-dispatch-capitalize
  "<remap> <capitalize-dwim>" 'conn-dispatch-capitalize
  "D" 'conn-dispatch-kill-append
  "C-d" 'conn-dispatch-kill-prepend
  "T" 'conn-dispatch-copy-append
  "C-t" 'conn-dispatch-copy-prepend
  "e" (conn-anonymous-thing
       'char
       :default-action (lambda ()
                         (let ((jump (conn-make-action 'conn-dispatch-jump)))
                           (oclosure-lambda (conn-action
                                             (description "Previous Emacs State")
                                             (no-history t))
                               (&rest args)
                             (apply jump args)
                             (conn-push-state 'conn-emacs-state))))
       :target-finder (lambda (_arg) (conn-dispatch-previous-emacs-state)))
  "g y" (conn-anonymous-thing
         'char
         :default-action (lambda () (conn-make-action 'conn-dispatch-jump))
         :target-finder (lambda (_arg) (conn-dispatch-global-mark)))
  "<" (conn-anonymous-thing
       'char
       :default-action (lambda () (conn-make-action 'conn-dispatch-jump))
       :target-finder (lambda (_arg) (conn-dispatch-mark-register)))
  "<remap> <conn-pop-mark-ring>"
  (conn-anonymous-thing
   'char
   :default-action (lambda () (conn-make-action 'conn-dispatch-jump))
   :target-finder (lambda (_arg) (conn-dispatch-mark-ring))))

;;;;;; Transpose State

(define-keymap
  :keymap (conn-get-state-map 'conn-transpose-state)
  "i" 'conn-backward-line
  "k" 'forward-line
  "u" 'forward-symbol
  "f" 'conn-dispatch)

(define-keymap
  :keymap (conn-get-state-map 'conn-dispatch-transpose-state)
  "TAB" 'repeat-dispatch
  "C-n" 'restrict-windows
  "SPC" 'scroll-up
  "DEL" 'scroll-down
  "C-o" 'other-window
  "o" (conn-anonymous-thing
       'word
       :description "all-words"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'word)))
  "u" (conn-anonymous-thing
       'symbol
       :description "all-symbols"
       :target-finder (lambda (_arg)
                        (conn-dispatch-all-things 'symbol))))

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-command-state
                                   'conn-transpose-recursive-edit-mode)
  "d" 'exit-recursive-edit
  "q" 'abort-recursive-edit)

(provide 'conn-keymaps)
