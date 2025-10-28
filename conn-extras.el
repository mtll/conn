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

(require 'conn)
(require 'conn-quick-ref)
(require 'conn-states)

(declare-function outline-previous-visible-heading "outline")

;;; State

(defvar conn-integration-modes nil)

(conn-define-state conn-integration-state ()
  :lighter "I"
  :pop-alternate 'conn-emacs-state)

(conn-define-state conn-one-emacs-state (conn-emacs-state
                                         conn-autopop-state)
  "Execute one command in `conn-emacs-state'."
  :lighter "1E")

(defun conn-one-emacs-state ()
  (interactive)
  (conn-push-state 'conn-one-emacs-state))

(define-keymap
  :keymap (conn-get-state-map 'conn-integration-state)
  :suppress t
  "SPC" 'conn-one-emacs-state
  "<escape>" 'conn-pop-state
  "M-j" 'conn-command-state)

(defun conn-setup-integration-state ()
  (when (derived-mode-p conn-integration-modes)
    (conn-push-state 'conn-integration-state)
    t))
(add-hook 'conn-setup-state-hook 'conn-setup-integration-state -20)

;;; Load Extensions

;;;; Calc

(with-eval-after-load 'calc
  (declare-function calc-dispatch "calc")

  (defun conn--calc-dispatch-ad (&rest app)
    (conn-with-recursive-stack 'conn-null-state
      (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))

;;;; Completion

(defun conn--exit-completion ()
  (conn-state-defer-once
    (completion-in-region-mode -1)))
(add-hook 'completion-in-region-mode-hook 'conn--exit-completion)

;;;; Eldoc

(with-eval-after-load 'eldoc
  (eldoc-add-command 'conn-end-of-inner-line
                     'conn-beginning-of-inner-line
                     'conn-backward-char
                     'conn-goto-char-backward
                     'conn-forward-char
                     'conn-goto-char-forward
                     'conn-dispatch))

;;;; Edebug

(defun conn--edebug-toggle-emacs-state ()
  (if (bound-and-true-p edebug-mode)
      (conn-push-state 'conn-emacs-state)
    (conn-pop-state)))
(add-hook 'edebug-mode-hook 'conn--edebug-toggle-emacs-state)

;;;; Outline

(declare-function outline-mark-subtree "outline")
(declare-function outline-next-heading "outline")
(declare-function outline-previous-heading "outline")
(declare-function outline-on-heading-p "outline")
(declare-function outline-up-heading "outline")

(conn-register-thing
 'heading
 :bounds-op (lambda ()
              (save-mark-and-excursion
                (outline-mark-subtree)
                (cons (region-beginning) (region-end))))
 :forward-op 'outline-next-visible-heading)

(conn-define-mark-command conn-mark-heading heading)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing heading))
                                      _arg)
  (conn-dispatch-headings))

(conn-register-thing-commands
 'heading 'conn-discrete-thing-handler
 'conn-outline-state-prev-heading
 'outline-up-heading
 'outline-next-heading
 'outline-next-visible-heading
 'outline-previous-visible-heading
 'outline-previous-heading
 'outline-forward-same-level
 'outline-backward-same-level)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-outline-state-prev-heading)) arg)
  (conn-bounds-of 'outline-previous-visible-heading arg))

;;;###autoload
(define-minor-mode conntext-outline-mode
  "Minor mode for contextual bindings in outline-mode."
  :global t
  :group 'conn
  (if conntext-outline-mode
      (add-hook 'conntext-state-hook 'conntext-outline-state -80)
    (remove-hook 'conntext-state-hook 'conntext-outline-state)))

(defun conn-outline-state ()
  (interactive)
  (conn-push-state 'conn-outline-state))

(defun conn-outline-state-prev-heading ()
  (interactive)
  (unless (progn
            (goto-char (pos-bol))
            (looking-at-p outline-regexp))
    (outline-previous-visible-heading 1))
  (conn-push-state 'conn-outline-state))

(defun conntext-outline-state ()
  (when (and (bound-and-true-p outline-minor-mode)
             (save-excursion
               (goto-char (pos-bol))
               (looking-at-p outline-regexp)))
    (goto-char (pos-bol))
    (conn-push-state 'conn-outline-state)
    t))

(define-keymap
  :keymap (conn-get-state-map 'conn-outline-state)
  :suppress t
  "*" 'conn-outline-insert-heading
  "<backspace>" 'conn-scroll-down
  ";" 'conn-wincontrol
  "." 'point-to-register
  "/" (conn-remap-key conn-undo-keys t)
  "?" (conn-remap-key conn-undo-redo-keys t)
  "DEL" 'conn-scroll-down
  "SPC" 'conn-scroll-up
  "W" 'widen
  "<escape>" 'conn-pop-state
  "J" 'outline-promote
  "L" 'outline-demote
  "O" 'outline-move-subtree-down
  "C-SPC" 'conn-set-mark-command
  "U" 'outline-move-subtree-up
  "a" 'execute-extended-command
  "A" 'execute-extended-command-for-buffer
  "b" 'outline-show-branches
  "c" (conn-remap-key "C-c" t)
  "d" 'conn-kill-thing
  "w h" 'outline-hide-by-heading-regexp
  "w s" 'outline-show-by-heading-regexp
  "e" 'conn-pop-state
  "f" 'conn-dispatch
  "g" (conn-remap-key "M-g" t)
  "h" 'conn-wincontrol-one-command
  "i" 'outline-previous-visible-heading
  "j" 'outline-backward-same-level
  "k" 'outline-next-visible-heading
  "l" 'outline-forward-same-level
  "m" 'outline-show-subtree
  "n" 'outline-hide-leaves
  "o" 'outline-hide-other
  "p" 'conn-register-prefix
  "q" 'conn-transpose-things
  "r" conn-region-remap
  "s" (conn-remap-key "M-s" t)
  "t" 'outline-hide-body
  "u" 'outline-up-heading
  "v" 'conn-toggle-mark-command
  "x" (conn-remap-key "C-x" t)
  "y" 'outline-show-all
  "z" 'conn-exchange-mark-command)

;;;; Dired

(cl-pushnew 'dired-mode conn-integration-modes)

(conn-define-state conn-dired-dispatch-state (conn-dispatch-state)
  "State for dispatch in `dired-mode'."
  :cursor 'box
  :suppress-input-method t)

(defun conn-dired-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-dired-dispatch-state
                   :prefix initial-arg
                   :prompt "Dired Dispatch")
      ((action (conn-dispatch-action-argument))
       (`(,thing ,thing-arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (repeat (conn-dispatch-repeat-argument))
       (restrict-windows (conn-dispatch-restrict-windows-argument)))
    (conn-perform-dispatch
     action thing thing-arg transform
     :repeat repeat
     :restrict-windows restrict-windows
     :other-end :no-other-end)))

(define-keymap
  :keymap (conn-get-state-map 'conn-dired-dispatch-state)
  "f" 'conn-dispatch-dired-mark
  "w" 'conn-dispatch-dired-kill-line
  "d" 'conn-dispatch-dired-kill-subdir)

(defvar conn-dired-ref-1
  (conn-reference-page "Dired"
    ((("next/prev" dired-next-line dired-previous-line)
      ("next/prev dirline" dired-next-dirline dired-prev-dirline)
      ("next/prev subdir" dired-next-subdir dired-prev-subdir)
      ("next/prev marked" dired-next-marked-file dired-prev-marked-file))
     (("dir up" dired-up-directory)
      ("tree up/down" dired-tree-up dired-tree-down)
      ("undo" dired-undo)
      ("find alt" dired-find-alternate-file))
     (("mark/unmark" dired-mark dired-unmark)
      ("delete" dired-do-delete)
      ("copy" dired-do-copy)
      ("isearch/regexp" dired-do-isearch dired-do-isearch-regexp)
      ("find/replace regexp" dired-do-find-regexp dired-do-find-regexp-and-replace)))))

(defun conn-dired-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-dired-ref-1)))

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
  :keymap (conn-get-major-mode-map 'conn-integration-state 'dired-mode)
  "C-q" 'conn-dired-quick-ref
  "<conn-dired-search-map> s" 'dired-do-isearch
  "<conn-dired-search-map> c" 'dired-do-isearch-regexp
  "<conn-dired-search-map> q" 'dired-do-find-regexp
  "<conn-dired-search-map> r" 'dired-do-find-regexp-and-replace
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "A" 'dired-find-alternate-file
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
  "w" 'dired-do-kill-lines
  "s" conn-dired-search-remap
  "r" conn-dired-regexp-remap
  "," conn-dired-mark-remap
  "x" (conn-remap-key "C-x" t)
  "f" 'conn-dired-dispatch-state
  "M-SPC" 'dired-toggle-marks
  "C-M-l" 'dired-do-redisplay
  "z" 'dired-goto-file
  ";" 'conn-wincontrol
  "`" 'conn-wincontrol-mru-window
  "v" 'dired-mark
  "c" 'dired-unmark
  "u" 'dired-do-delete
  "M-w" 'dired-copy-filename-as-kill
  "RET" 'dired-find-file
  "o" 'dired-find-file-other-window
  "M-u" 'conn-pop-mark-ring
  "M-o" 'conn-unpop-mark-ring
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

(defvar dired-subdir-alist)
(defvar dired-movement-style)
(defvar dired-mode-map)

(declare-function dired-mark "dired")
(declare-function dired-unmark "dired")
(declare-function dired-next-line "dired")
(declare-function dired-next-dirline "dired")
(declare-function dired-marker-regexp "dired")
(declare-function dired-kill-subdir "dired-aux")
(declare-function dired-kill-line "dired-aux")

(conn-set-mode-property 'dired-mode :disable-mark-cursor t)

(defun conn--dispatch-dired-lines ()
  (let ((dired-movement-style 'bounded))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-min))
        (while (/= (point)
                   (progn
                     (dired-next-line 1)
                     (point)))
          (conn-make-target-overlay (point) 0))))))

(defun conn--dispatch-dired-dirline ()
  (save-excursion
    (with-restriction (window-start) (window-end)
      (goto-char (point-min))
      (while (/= (point)
                 (progn
                   (dired-next-dirline 1)
                   (point)))
        (conn-make-target-overlay (point) 0)))))

(defun conn--dispatch-dired-subdir ()
  (let ((start (window-start))
        (end (window-end)))
    (save-excursion
      (pcase-dolist (`(,_ . ,marker) dired-subdir-alist)
        (when (<= start marker end)
          (goto-char marker)
          (conn-make-target-overlay
           (+ 2 marker) (- (line-end-position) marker 2)))))))

(conn-register-thing-commands
 'dired-line nil
 'dired-previous-line 'dired-next-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-line))
                                      _arg)
  #'conn--dispatch-dired-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-line)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'dired-subdir nil
 'dired-next-subdir 'dired-prev-subdir
 'dired-tree-up 'dired-tree-down)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-subdir))
                                      _arg)
  #'conn--dispatch-dired-subdir)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-subdir)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'dired-dirline nil
 'dired-next-dirline 'dired-prev-dirline)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-dirline))
                                      _arg)
  #'conn--dispatch-dired-dirline)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-dirline)))
  (conn-make-action 'conn-dispatch-jump))

(oclosure-define (conn-dispatch-dired-mark
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-mark)))
  (oclosure-lambda (conn-dispatch-dired-mark
                    (description "Mark")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg _transform)
    (with-selected-window window
      (save-excursion
        (let ((regexp (dired-marker-regexp)))
          (goto-char pt)
          (goto-char (line-beginning-position))
          (if (looking-at regexp)
              (dired-unmark 1)
            (dired-mark 1)))))))

(oclosure-define (conn-dispatch-dired-kill-line
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-kill-line)))
  (oclosure-lambda (conn-dispatch-dired-kill-line
                    (description "Kill Line")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg _transform)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-line)))))

(oclosure-define (conn-dispatch-dired-kill-subdir
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-dired-kill-subdir)))
  (oclosure-lambda (conn-dispatch-dired-kill-subdir
                    (description "Kill Subdir")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      (window pt _thing _thing-arg _transform)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (dired-kill-subdir)))))

;;;; Magit

(cl-pushnew 'magit-section-mode conn-integration-modes)

(conn-set-mode-property 'magit-section-mode :disable-mark-cursor t)

(defvar conn-magit-ref
  (conn-reference-page "Magit"
    ((("section forward/back" magit-section-forward magit-section-backward)
      ("delete thing" magit-delete-thing)
      ("dispatch" magit-dispatch))
     (("reset quickly" magit-reset-quickly)
      ("gitignore" magit-gitignore)
      ("apply mail" magit-am)))))

(defun conn-magit-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-magit-ref)))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'magit-section-mode)
  "C-q" 'conn-magit-quick-ref
  "h" 'conn-wincontrol-one-command
  "," 'magit-dispatch
  "i" 'magit-section-backward
  "k" 'magit-section-forward
  "w" 'magit-delete-thing
  "p" 'magit-reset-quickly
  "n" 'magit-gitignore
  "`" 'conn-wincontrol-mru-window
  "@" 'magit-am
  "x" (conn-remap-key "C-x" t))

;;;; Ibuffer

(cl-pushnew 'ibuffer-mode conn-integration-modes)

(conn-define-state conn-ibuffer-dispatch-state (conn-dispatch-targets-state)
  "State for dispatch in `ibuffer-mode'."
  :cursor '(bar . 4)
  :disable-mark-cursor t
  :suppress-input-method t)

(defun conn-ibuffer-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-ibuffer-dispatch-state
                   :prefix initial-arg
                   :prompt "Ibuffer Dispatch")
      ((action (conn-dispatch-action-argument))
       (`(,thing ,thing-arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (repeat (conn-dispatch-repeat-argument))
       (restrict-windows (conn-dispatch-restrict-windows-argument)))
    (conn-perform-dispatch
     action thing thing-arg transform
     :repeat repeat
     :restrict-windows restrict-windows
     :other-end :no-other-end)))

(conn-set-mode-property 'ibuffer-mode :disable-mark-cursor t)

(defvar ibuffer-movement-cycle)
(defvar ibuffer-marked-char)

(declare-function ibuffer-backward-line "ibuffer")
(declare-function ibuffer-unmark-forward "ibuffer")
(declare-function ibuffer-mark-forward "ibuffer")
(declare-function ibuffer-current-mark "ibuffer")
(declare-function ibuffer-backward-filter-group "ibuffer")

(defun conn--dispatch-ibuffer-lines ()
  (let ((ibuffer-movement-cycle nil))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-max))
        (while (/= (point)
                   (progn
                     (ibuffer-backward-line)
                     (point)))
          (unless (get-text-property (point) 'ibuffer-filter-group-name)
            (conn-make-target-overlay (point) 0)))))))

(defun conn--dispatch-ibuffer-filter-group ()
  (let ((ibuffer-movement-cycle nil))
    (save-excursion
      (with-restriction (window-start) (window-end)
        (goto-char (point-max))
        (while (/= (point)
                   (progn
                     (ibuffer-backward-filter-group)
                     (point)))
          (conn-make-target-overlay (point) 0))))))

(conn-register-thing-commands
 'ibuffer-line nil
 'ibuffer-backward-line 'ibuffer-forward-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-line))
                                      _arg)
  #'conn--dispatch-ibuffer-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-line)))
  (conn-make-action 'conn-dispatch-jump))

(conn-register-thing-commands
 'ibuffer-filter-group nil
 'ibuffer-forward-filter-group
 'ibuffer-backward-filter-group)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-filter-group))
                                      _arg)
  #'conn--dispatch-ibuffer-filter-group)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-filter-group)))
  (conn-make-action 'conn-dispatch-jump))

(oclosure-define (conn-dispatch-ibuffer-mark
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-ibuffer-mark)))
  (oclosure-lambda (conn-dispatch-ibuffer-mark
                    (description "Mark")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'ibuffer-mode))))
      (window pt _thing _thing-arg _transform)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (if (or (null (ibuffer-current-mark))
                (= (ibuffer-current-mark) ? ))
            (ibuffer-mark-forward nil nil 1)
          (ibuffer-unmark-forward nil nil 1))))))

(defvar conn-ibuffer-ref
  (conn-reference-page "Ibuffer"
    ((("next/prev line" ibuffer-forward-line ibuffer-backward-line)
      ("next/prev group" ibuffer-forward-filter-group ibuffer-backward-filter-group)
      ("jump to group" ibuffer-jump-to-filter-group)
      ("jump to buffer" ibuffer-jump-to-buffer)
      ("yank" ibuffer-yank)
      ("copy filename" ibuffer-copy-filename-as-kill)
      ("visit/other win" ibuffer-visit-buffer ibuffer-visit-buffer-other-window)
      ("redisplay" ibuffer-redisplay))
     ((:heading "Sort:")
      ("invert" ibuffer-invert-sorting)
      ("alphabetic" ibuffer-do-sort-by-alphabetic)
      ("path" ibuffer-do-sort-by-filename/process)
      ("mode" ibuffer-do-sort-by-major-mode)
      ("size" ibuffer-do-sort-by-size)
      ("recency" ibuffer-do-sort-by-recency)))))

(defvar conn-ibuffer-mark-ref
  (conn-reference-page "Ibuffer Mark"
    ((("mark" ibuffer-mark-forward)
      ("unmark next/prev" ibuffer-unmark-forward ibuffer-unmark-backward)
      ("unmark all" ibuffer-unmark-all)
      ("toggle" ibuffer-toggle-marks)
      ("next/prev" ibuffer-forward-next-marked ibuffer-backwards-next-marked))
     (("revert" ibuffer-do-revert)
      ("hide" ibuffer-do-kill-lines)
      ("kill" ibuffer-do-kill-on-deletion-marks)
      ("isearch/regexp" ibuffer-do-isearch ibuffer-do-isearch-regexp)
      ("occur" ibuffer-do-occur)))))

(defun conn-ibuffer-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-ibuffer-ref conn-ibuffer-mark-ref)))

(keymap-set (conn-get-state-map 'conn-ibuffer-dispatch-state)
            "f" 'conn-dispatch-ibuffer-mark)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'ibuffer-mode)
  "f" 'conn-ibuffer-dispatch-state
  "C-q" 'conn-ibuffer-quick-ref
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
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
  "f" 'conn-dispatch
  "k" 'ibuffer-forward-line
  "i" 'ibuffer-backward-line
  "w" 'ibuffer-do-kill-lines
  "u" 'ibuffer-do-kill-on-deletion-marks
  "x" (conn-remap-key "C-x" t)
  "s" (conn-remap-key "M-s" t)
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
  "c" 'ibuffer-unmark-forward
  "C" 'ibuffer-unmark-backward
  "o" 'ibuffer-visit-buffer-other-window
  "RET" 'ibuffer-visit-buffer)

;;;; Bookmark Bmenu

(cl-pushnew 'bookmark-bmenu-mode conn-integration-modes)

(declare-function bookmark-bmenu-unmark "bookmark")
(declare-function bookmark-bmenu-mark "bookmark")

(conn-define-state conn-bmenu-dispatch-state (conn-dispatch-targets-state)
  "State for dispatch in `bookmark-bmenu-mode'."
  :disable-mark-cursor t
  :suppress-input-method t)

(defun conn-bmenu-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-bmenu-dispatch-state
                   :prefix initial-arg
                   :prompt "Bookmark Dispatch")
      ((action (conn-dispatch-action-argument))
       (`(,thing ,thing-arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (repeat (conn-dispatch-repeat-argument)))
    (conn-perform-dispatch
     action thing thing-arg transform
     :repeat repeat
     :restrict-windows t
     :other-end :no-other-end)))

(oclosure-define (conn-dispatch-bmenu-mark
                  (:parent conn-action)))

(cl-defmethod conn-make-action ((_type (eql conn-dispatch-bmenu-mark)))
  (oclosure-lambda (conn-dispatch-bmenu-mark
                    (description "Mark")
                    (window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'bookmark-bmenu-mode))))
      (window pt _thing _thing-arg _transform)
    (with-selected-window window
      (save-excursion
        (goto-char pt)
        (beginning-of-line)
        (when (tabulated-list-get-entry)
          (if (search-forward ">" (+ (point) tabulated-list-padding) t)
              (bookmark-bmenu-unmark)
            (bookmark-bmenu-mark)))))))

(defun conn-bmenu-target-finder ()
  (save-excursion
    (with-restriction (window-start) (window-end)
      (goto-char (point-min))
      (while (progn
               (when (tabulated-list-get-entry)
                 (conn-make-target-overlay
                  (+ (point) tabulated-list-padding) 0))
               (forward-line)
               (not (eobp)))))))

(define-keymap
  :keymap (conn-get-state-map 'conn-bmenu-dispatch-state)
  "f" 'conn-dispatch-bmenu-mark
  "k" (conn-anonymous-thing
        'line
        :target-finder (:method (_self _arg) #'conn-bmenu-target-finder)))

(conn-set-mode-property 'bookmark-bmenu-mode :disable-mark-cursor t)

(defvar conn-bookmark-bmenu-ref
  (conn-reference-page "Bookmark Menu"
    ((("mark/all" bookmark-bmenu-mark bookmark-bmenu-mark-all)
      ("unmark/all" bookmark-bmenu-unmark bookmark-bmenu-unmark-all)
      ("annotations/all"
       bookmark-bmenu-show-annotation
       bookmark-bmenu-show-all-annotations)
      ("edit annotation" bookmark-bmenu-edit-annotation)
      ("this window" bookmark-bmenu-this-window))
     (("select" bookmark-bmenu-select)
      ("mark for deletion" bookmark-bmenu-delete)
      ("execute delete" bookmark-bmenu-execute-deletions)
      ("save" bookmark-bmenu-save)
      ("jump" bookmark-jump))
     (("scroll up/down"
       scroll-up-command
       scroll-down-command)
      ("line next/prev" next-line previous-line)
      ("revert buffer" revert-buffer)
      ("locate" bookmark-locate)))))

(defun conn-bookmark-bmenu-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-bookmark-bmenu-ref)))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'bookmark-bmenu-mode)
  "C-q" 'conn-bookmark-bmenu-quick-ref
  "e" 'bookmark-bmenu-edit-annotation
  "a" 'execute-extended-command
  "n" 'bookmark-bmenu-show-annotation
  "N" 'bookmark-bmenu-show-all-annotations
  "v" 'bookmark-bmenu-mark
  "V" 'bookmark-bmenu-mark-all
  "K" 'scroll-up-command
  "I" 'scroll-down-command
  "i" 'previous-line
  "k" 'next-line
  "h" 'conn-wincontrol-one-command
  ";" 'conn-wincontrol
  "j" 'bookmark-bmenu-select
  "f" 'conn-bmenu-dispatch-state
  "w" 'bookmark-bmenu-this-window
  "b" 'bookmark-bmenu-unmark
  "B" 'bookmark-bmenu-unmark-all
  "L" 'bookmark-locate
  "u" 'bookmark-bmenu-execute-deletions
  "y" 'tabulated-list-sort)

;;;; Markdown

(conn-register-thing
 'md-paragraph
 :forward-op 'markdown-forward-paragraph)

(conn-register-thing-commands
 'md-paragraph 'conn-continuous-thing-handler
 'markdown-forward-paragraph
 'markdown-backward-paragraph)

;; TODO: other markdown things

;;;; Treesit

(static-if (<= 30 emacs-major-version)
    (conn-register-thing-commands
     'defun 'conn-continuous-thing-handler
     'treesit-end-of-defun
     'treesit-beginning-of-defun))

;;;; Help

(cl-pushnew 'help-mode conn-integration-modes)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'help-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

(cl-pushnew 'helpful-mode conn-integration-modes)

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'helpful-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "j" 'backward-button
  "l" 'forward-button
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch-on-buttons
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;;; Info

(cl-pushnew 'Info-mode conn-integration-modes)

(declare-function Info-prev-reference "info")
(declare-function Info-follow-nearest-node "info")

(oclosure-define (conn-action-info-ref
                  (:parent conn-action)))

(defun conn-dispatch-on-info-refs ()
  (interactive)
  (conn-perform-dispatch
   (oclosure-lambda (conn-action-info-ref
                     (description "Info Refs")
                     (window-predicate
                      (lambda (win)
                        (eq 'Info-mode
                            (buffer-local-value 'major-mode
                                                (window-buffer win))))))
       (win pt _thing _thing-arg)
     (select-window win)
     (goto-char pt)
     (Info-follow-nearest-node))
   (lambda ()
     (dolist (win (conn--get-target-windows))
       (with-selected-window win
         (save-excursion
           (let ((last-pt (goto-char (window-end))))
             (while (and (> last-pt (progn
                                      (Info-prev-reference)
                                      (setq last-pt (point))))
                         (<= (window-start) (point) (window-end)))
               (conn-make-target-overlay (point) 0)))))))
   nil nil
   :other-end :no-other-end))

(defvar conn-info-ref
  (conn-reference-page "Info"
    ((("history forward/back" Info-history-forward Info-history-back)
      ("next/prev" Info-next Info-prev)
      ("scroll up/down" Info-scroll-up Info-scroll-down))
     (("node forward/back" Info-forward-node Info-backward-node)
      ("node up" Info-up)
      ("menu" Info-menu))
     (("toc" Info-toc)
      ("index" Info-index)))))

(defun conn-info-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-info-ref)))

(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'Info-mode)
  "C-q" 'conn-info-quick-ref
  "h" 'conn-wincontrol-one-command
  "o" 'Info-history-back
  "u" 'Info-history-forward
  "m" 'Info-next
  "n" 'Info-prev
  "k" 'Info-scroll-up
  "i" 'Info-scroll-down
  "l" 'Info-forward-node
  "j" 'Info-backward-node
  "r" 'Info-up
  "a" 'execute-extended-command
  "p" 'Info-menu
  "z" 'Info-toc
  "f" 'dispatch-on-info-refs
  "v" 'Info-index
  "`" 'conn-wincontrol-mru-window
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;;; Treemacs

(cl-pushnew 'treemacs-mode conn-integration-modes)

(conn-set-mode-property 'treemacs-mode :disable-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'treemacs-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'treemacs-select-window
  "i" 'treemacs-previous-line
  "k" 'treemacs-next-line
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;;; Messages

(cl-pushnew 'messages-buffer-mode conn-integration-modes)

(conn-set-mode-property 'messages-buffer-mode :disable-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'messages-buffer-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "b" 'beginning-of-buffer
  "e" 'end-of-buffer
  "`" 'conn-wincontrol-mru-window
  "i" 'scroll-down
  "k" 'scroll-up
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;;; Debugger mode

(cl-pushnew 'debugger-mode conn-integration-modes)

(conn-set-mode-property 'debugger-mode :disable-mark-cursor t)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'debugger-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "i" 'backtrace-backward-frame
  "k" 'backtrace-forward-frame
  "I" 'scroll-down
  "K" 'scroll-up
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;;; Occur mode

(cl-pushnew 'occur-mode conn-integration-modes)

(conn-set-mode-property 'occur-mode :disable-mark-cursor t)
(conn-set-mode-property 'occur-edit-mode :disable-mark-cursor nil)
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'occur-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

(defun conn-occur-edit-map-setup ()
  (setf (conn-get-major-mode-maps) '(occur-edit-mode)))
(add-hook 'occur-edit-mode-hook 'conn-occur-edit-map-setup)

;;;; Compile mode

(cl-pushnew 'compilation-mode conn-integration-modes)

(conn-set-mode-property 'compilation-mode :disable-mark-cursor t)
(static-if (<= 31 emacs-major-version)
    (conn-set-mode-property 'grep-edit-mode :disable-mark-cursor nil))
(define-keymap
  :keymap (conn-get-major-mode-map 'conn-integration-state 'compilation-mode)
  "h" 'conn-wincontrol-one-command
  "a" 'execute-extended-command
  "`" 'conn-wincontrol-mru-window
  "k" 'next-error-no-select
  "i" 'previous-error-no-select
  "f" 'conn-dispatch
  ";" 'conn-wincontrol
  "x" (conn-remap-key "C-x" t))

;;; Footer
;; Local Variables:
;; outline-regexp: "^;;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn.el ends here

(provide 'conn-extras)
