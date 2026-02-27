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

;;; State

(defvar conn-special-state-modes
  (list 'special-mode
        'dired-mode))

(conn-define-state conn-special-state ()
  :lighter "S")

(defun conn-setup-special-state ()
  (when (derived-mode-p conn-special-state-modes)
    (conn-push-state 'conn-special-state)
    t))

;;;###autoload
(define-minor-mode conn-special-state-mode
  "Minor mode for `conn-special-state' integration."
  :global t
  :group 'conn
  (if conn-special-state-mode
      (add-hook 'conn-setup-state-hook #'conn-setup-special-state -20)
    (remove-hook 'conn-setup-state-hook #'conn-setup-special-state)))

;;; Load Extensions

;;;; Grep

(defvar-local conn--grep-edit-stack-cookie nil)

(with-eval-after-load 'wgrep
  (defun conn--wgrep-cleanup ()
    (when-let* ((cookie (cl-shiftf conn--grep-edit-stack-cookie nil)))
      (conn-set-major-mode-maps
       (conn--derived-mode-all-parents major-mode))
      (conn-exit-recursive-stack cookie)))
  (advice-add 'wgrep-to-original-mode :after 'conn--wgrep-cleanup)

  (defun conn--wgrep-setup ()
    (conn-set-major-mode-maps (list 'wgrep-mode))
    (setq conn--grep-edit-stack-cookie
          (conn-enter-recursive-stack 'conn-command-state)))
  (advice-add 'wgrep-change-to-wgrep-mode :after 'conn--wgrep-setup))

(with-eval-after-load 'grep
  (defun conn--exit-grep-edit-mode (&rest _)
    (conn-setup-state-for-buffer))
  (advice-add 'grep-edit-save-changes :after 'conn--exit-grep-edit-mode))

;;;; Calc

(with-eval-after-load 'calc
  (declare-function calc-dispatch "calc")

  (defun conn--calc-dispatch-ad (&rest app)
    (conn-with-recursive-stack 'conn-null-state
      (apply app)))
  (advice-add 'calc-dispatch :around 'conn--calc-dispatch-ad))

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

(cl-defmethod conn-bounds-of ((cmd (eql outline-up-heading))
                              arg)
  (cl-callf prefix-numeric-value arg)
  (cl-call-next-method cmd
                       (if (looking-at-p outline-regexp)
                           arg
                         (1- arg))))

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing heading))
                                      _arg)
  (conn-dispatch-headings
   :reference (conn-reference-quote
                ((:heading "Heading Targets")
                 "Hides buffer regions outside heading lines."))))

(eval-and-compile
  (conn-register-thing
   'heading
   :bounds-op (lambda ()
                (save-mark-and-excursion
                  (outline-mark-subtree)
                  (cons (region-beginning) (region-end))))
   :forward-op 'outline-next-visible-heading))

(conn-register-thing-commands
 '(heading) 'conn-discrete-thing-handler
 'conn-outline-state-up-heading
 'outline-up-heading
 'outline-next-heading
 'outline-next-visible-heading
 'outline-previous-visible-heading
 'outline-previous-heading
 'outline-forward-same-level
 'outline-backward-same-level)

(cl-defmethod conn-bounds-of ((_cmd (eql conn-outline-state-up-heading))
                              arg)
  (cl-callf prefix-numeric-value arg)
  (cl-call-next-method 'outline-up-heading
                       (if (looking-at-p outline-regexp)
                           arg
                         (1- arg))))

(defun conn-outline-state ()
  (interactive)
  (conn-push-state 'conn-outline-state))

(defun conn-outline-state-up-heading (arg)
  (interactive "p")
  (outline-up-heading (1- arg))
  (conn-push-state 'conn-outline-state))

;;;; Dired

(conn-define-state conn-dired-dispatch-state (conn-dispatch-state)
  "State for dispatch in `dired-mode'."
  :cursor 'box
  :suppress-input-method t)

(defun conn-dired-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-dired-dispatch-state
                   :prefix initial-arg
                   :prompt "Dired Dispatch")
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map))
       (`(,action ,repeat) (conn-dispatch-action-argument)))
    (conn-dispatch-setup
     action thing arg transform
     :repeat repeat
     :restrict-windows restrict-windows
     :other-end :no-other-end)))

(defvar conn-dired-ref-1
  (conn-reference-page
    (:heading "Dired")
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

(conn-register-thing 'dired-line :parents '(line))

(conn-register-thing-commands
 '(dired-line) nil
 'dired-previous-line 'dired-next-line)

(defun conn--dispatch-dired-lines (try-next)
  (if (derived-mode-p '(dired-mode))
      (lambda (_state)
        (save-excursion
          (goto-char (window-start))
          (vertical-motion (cons 1 0))
          (when (< (point) (window-end))
            (conn-make-target-overlay
             (point) 0
             :padding-function #'conn--flush-left-padding))
          (while (progn
                   (vertical-motion (cons 1 1))
                   (< (point) (window-end)))
            (conn-make-target-overlay
             (point) 0
             :padding-function #'conn--flush-left-padding))))
    (funcall try-next)))

(conn-add-update-handler 'conn-dispatch-line-targets
                         #'conn--dispatch-dired-lines)

(conn-add-update-handler 'conn-dispatch-column-targets
                         #'conn--dispatch-dired-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-line)))
  (conn-dispatch-jump))

(conn-register-thing 'dired-subdir)

(conn-register-thing-commands
 '(dired-subdir) nil
 'dired-next-subdir 'dired-prev-subdir
 'dired-tree-up 'dired-tree-down)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-subdir))
                                      _arg)
  #'conn--dispatch-dired-subdir)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-subdir)))
  (conn-dispatch-jump))

(conn-register-thing 'dired-dirline)

(conn-register-thing-commands
 '(dired-dirline) nil
 'dired-next-dirline 'dired-prev-dirline)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing dired-dirline))
                                      _arg)
  #'conn--dispatch-dired-dirline)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing dired-dirline)))
  (conn-dispatch-jump))

(defun conn-dispatch-dired-mark ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Mark")
                    (action-window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (with-selected-window window
        (save-excursion
          (let ((regexp (dired-marker-regexp)))
            (goto-char pt)
            (goto-char (line-beginning-position))
            (if (looking-at regexp)
                (dired-unmark 1)
              (dired-mark 1))))))))

(defun conn-dispatch-dired-kill-line ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Kill Line")
                    (action-window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (dired-kill-line))))))

(defun conn-dispatch-dired-kill-subdir ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Kill Subdir")
                    (action-window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'dired-mode))))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (dired-kill-subdir))))))

(defvar-local conn--wdired-stack-cookie nil)

(with-eval-after-load 'wdired
  (defun conn--wdired-cleanup ()
    (conn-set-major-mode-maps
     (conn--derived-mode-all-parents major-mode))
    (conn-exit-recursive-stack conn--wdired-stack-cookie))
  (advice-add 'wdired-change-to-dired-mode :after 'conn--wdired-cleanup)

  (defun conn--wdired-setup ()
    (conn-set-major-mode-maps (list 'wdired-mode))
    (setq conn--wdired-stack-cookie
          (conn-enter-recursive-stack 'conn-command-state)))
  (add-hook 'wdired-mode-hook 'conn--wdired-setup))

;;;; Diff

(defvar conn-diff-ref
  (conn-reference-page
    (:heading "Diff")
    ((("hunk next/prev" diff-hunk-next diff-hunk-prev)
      ("file next/prev" diff-file-next diff-file-prev)
      ("apply hunk/buffer" diff-apply-hunk diff-apply-buffer)
      ("revert hunk" diff-revert-and-kill-hunk)
      ("next" next-error-follow-minor-mode)
      ("scroll up/down" conn-scroll-down conn-scroll-up))
     (("kill ring save" diff-kill-ring-save)
      ("kill hunk/file" diff-hunk-kill diff-file-kill)
      ("delete other hunks" diff-delete-other-hunks)
      ("split hunk" diff-split-hunk)
      ("test hunk" diff-test-hunk)
      ("reverse direction" diff-reverse-direction))
     (("goto source" diff-goto-source)
      ("narrow/widen" diff-restrict-view widen)
      ("context->unified" diff-context->unified)
      ("unified->context" diff-unified->context)
      ("ignore whitespace" diff-ignore-whitespace-hunk)))))

(defun conn-diff-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-diff-ref)))

;;;; Magit

(defvar conn-magit-ref
  (conn-reference-page
    (:heading "Magit")
    ((("section forward/back" magit-section-forward magit-section-backward)
      ("delete thing" magit-delete-thing)
      ("dispatch" magit-dispatch))
     (("reset quickly" magit-reset-quickly)
      ("gitignore" magit-gitignore)
      ("apply mail" magit-am)))))

(defun conn-magit-quick-ref ()
  (interactive)
  (conn-quick-reference (list conn-magit-ref)))

;;;; Ibuffer

(conn-define-state conn-ibuffer-dispatch-state (conn-dispatch-targets-state)
  "State for dispatch in `ibuffer-mode'."
  :cursor '(bar . 4)
  :suppress-input-method t)

(defun conn-ibuffer-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-ibuffer-dispatch-state
                   :prefix initial-arg
                   :prompt "Ibuffer Dispatch")
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (restrict-windows
        (conn-boolean-argument "this-win"
                               'restrict-windows
                               conn-restrict-windows-argument-map))
       (`(,action ,repeat) (conn-dispatch-action-argument)))
    (conn-dispatch-setup
     action thing arg transform
     :repeat repeat
     :restrict-windows restrict-windows
     :other-end :no-other-end)))

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

(conn-register-thing 'ibuffer-line)

(conn-register-thing-commands
 '(ibuffer-line) nil
 'ibuffer-backward-line 'ibuffer-forward-line)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-line))
                                      _arg)
  #'conn--dispatch-ibuffer-lines)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-line)))
  (conn-dispatch-jump))

(conn-register-thing 'ibuffer-filter-group)

(conn-register-thing-commands
 '(ibuffer-filter-group) nil
 'ibuffer-forward-filter-group
 'ibuffer-backward-filter-group)

(cl-defmethod conn-get-target-finder ((_cmd (conn-thing ibuffer-filter-group))
                                      _arg)
  #'conn--dispatch-ibuffer-filter-group)

(cl-defmethod conn-make-default-action ((_cmd (conn-thing ibuffer-filter-group)))
  (conn-dispatch-jump))

(defun conn-dispatch-ibuffer-mark ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Mark")
                    (action-window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'ibuffer-mode))))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (if (or (null (ibuffer-current-mark))
                  (= (ibuffer-current-mark) ? ))
              (ibuffer-mark-forward nil nil 1)
            (ibuffer-unmark-forward nil nil 1)))))))

(defvar conn-ibuffer-ref
  (conn-reference-page
    (:heading "Ibuffer")
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
  (conn-reference-page
    (:heading "Ibuffer Mark")
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

;;;; Bookmark Bmenu

(declare-function bookmark-bmenu-unmark "bookmark")
(declare-function bookmark-bmenu-mark "bookmark")

(conn-define-state conn-bmenu-dispatch-state (conn-dispatch-targets-state)
  "State for dispatch in `bookmark-bmenu-mode'."
  :suppress-input-method t)

(defun conn-bmenu-dispatch-state (&optional initial-arg)
  (interactive "P")
  (conn-read-args (conn-bmenu-dispatch-state
                   :prefix initial-arg
                   :prompt "Bookmark Dispatch")
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (`(,action ,repeat) (conn-dispatch-action-argument)))
    (conn-dispatch-setup
     action thing arg transform
     :repeat repeat
     :restrict-windows t
     :other-end :no-other-end)))

(defun conn-dispatch-bmenu-mark ()
  (declare (conn-dispatch-action))
  (oclosure-lambda (conn-action
                    (action-description "Mark")
                    (action-window-predicate
                     (lambda (win)
                       (eq (buffer-local-value 'major-mode
                                               (window-buffer win))
                           'bookmark-bmenu-mode))))
      ()
    (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                  (conn-select-target)))
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (beginning-of-line)
          (when (tabulated-list-get-entry)
            (if (search-forward ">" (+ (point) tabulated-list-padding) t)
                (bookmark-bmenu-unmark)
              (bookmark-bmenu-mark))))))))

(defvar conn-bookmark-bmenu-ref
  (conn-reference-page
    (:heading "Bookmark Menu")
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

;;;; Markdown

(conn-register-thing
 'md-paragraph
 :forward-op 'markdown-forward-paragraph)

(conn-register-thing-commands
 '(md-paragraph) 'conn-continuous-thing-handler
 'markdown-forward-paragraph
 'markdown-backward-paragraph)

;; TODO: other markdown things

;;;; Treesit

(static-if (<= 30 emacs-major-version)
    (conn-register-thing-commands
     '(defun) 'conn-continuous-thing-handler
     'treesit-end-of-defun
     'treesit-beginning-of-defun))

;;;; Info

(declare-function Info-prev-reference "info")
(declare-function Info-follow-nearest-node "info")

(defun conn-dispatch-on-info-refs ()
  (interactive)
  (conn-dispatch-setup
   (oclosure-lambda (conn-action
                     (action-description "Info Refs")
                     (action-window-predicate
                      (lambda (win)
                        (eq 'Info-mode
                            (buffer-local-value 'major-mode
                                                (window-buffer win))))))
       ()
     (pcase-let* ((`(,pt ,window ,_thing ,_arg ,_transform)
                   (conn-select-target)))
       (select-window window)
       (goto-char pt)
       (Info-follow-nearest-node)))
   (conn-anonymous-thing
     '(point)
     :target-finder ( :method (_self _arg)
                      (save-excursion
                        (let ((last-pt (goto-char (window-end))))
                          (while (and (> last-pt (progn
                                                   (Info-prev-reference)
                                                   (setq last-pt (point))))
                                      (<= (window-start) (point) (window-end)))
                            (conn-make-target-overlay (point) 0))))))
   nil nil
   :other-end :no-other-end
   :restrict-windows t))

(defvar conn-info-ref
  (conn-reference-page
    (:heading "Info")
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

;;;; Occur mode

(defun conn-occur-edit-map-setup ()
  (setf (conn-get-major-mode-maps) '(occur-edit-mode)))
(add-hook 'occur-edit-mode-hook 'conn-occur-edit-map-setup)

(provide 'conn-extras)
