;;; conn-wincontrol.el --- WinControl -*- lexical-binding: t -*-
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

;; A simple version of hyperbole's hycontrol-windows

;;; Code

(require 'conn-utils)
(require 'conn-dispatch)
(require 'conn-commands)
(require 'repeat)

;;;; WinControl

(defgroup conn-wincontrol-mode nil
  "Conn-mode WinControl."
  :prefix "conn-wincontrol-"
  :group 'conn)

(defvar conn--wincontrol-help-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl " 'face 'minibuffer-prompt)
   "(arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) ", "
   "\\[conn-wincontrol-digit-argument-reset] reset arg; "
   "\\[conn-wincontrol-exit] exit; "
   "\\[conn-wincontrol-quick-ref] help):"))

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--wincontrol-preserve-arg nil)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)

(function-put 'keyboard-quit :conn-wincontrol-preserve-arg t)

(defalias 'conn-bury-buffer #'bury-buffer)
(defalias 'conn-unbury-buffer #'unbury-buffer)
(defalias 'conn-previous-buffer #'previous-buffer)
(defalias 'conn-next-buffer #'next-buffer)

;;;;; Wincontrol Internals

(defconst conn--wincontrol-maps
  `((conn-wincontrol-one-command-mode . ,conn-wincontrol-one-command-map)
    (conn-wincontrol-mode . ,conn-wincontrol-map)))

(put 'conn-wincontrol-digit-argument-reset :advertised-binding (key-parse "M-DEL"))

;;;###autoload
(define-minor-mode conn-wincontrol-mode
  "Global minor mode for wincontrol."
  :global t
  :lighter " WinC"
  :group 'conn-wincontrol-mode
  (if conn-wincontrol-mode
      (conn--wincontrol-setup)
    (conn--wincontrol-exit)))

(defun conn--wincontrol-pre-command ()
  (when (or conn--wincontrol-arg (< conn--wincontrol-arg-sign 0))
    (setq prefix-arg (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))
  (if this-command
      (when (function-get this-command :conn-wincontrol-preserve-arg)
        (setq conn--wincontrol-preserve-arg t))
    (setq conn--wincontrol-preserve-arg t)))

(eval-and-compile
  (defun conn--set-wincontrol-arg-property (f _args)
    `(progn
       :autoload-end
       (function-put ',f :conn-wincontrol-preserve-arg t)))
  (setf (alist-get 'conn-wincontrol-preserve-arg defun-declarations-alist)
        (list #'conn--set-wincontrol-arg-property)))

(defun conn--wincontrol-post-command ()
  (unless conn--wincontrol-preserve-arg
    (setq conn--wincontrol-arg nil
          conn--wincontrol-arg-sign 1))
  (setq conn--wincontrol-preserve-arg nil)
  (setq emulation-mode-map-alists
        `(conn--wincontrol-maps
          ,@(delq 'conn--wincontrol-maps emulation-mode-map-alists)))
  (let ((curr (current-message))
        (message-log-max nil))
    (cond ((minibuffer-window-active-p (selected-window)))
          ((null curr)
           (message "%s" (conn--wincontrol-message)))
          ((not (text-property-any 0 (length curr)
                                   'conn-wincontrol-string
                                   t curr))
           (message "%s%s" (conn--wincontrol-message) curr)))))

(defun conn--wincontrol-new-frame (frame)
  (set-face-inverse-video 'mode-line t frame)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active t frame))

(defalias 'conn--wincontrol-ignore 'ignore)

(defvar conn--wincontrol-message-newline t)

(defun conn--wincontrol-message ()
  (propertize
   (format (substitute-command-keys
            (if conn--wincontrol-message-newline
                (concat conn--wincontrol-help-format "\n")
              (concat conn--wincontrol-help-format " ")))
           (format (if conn--wincontrol-arg "%s%s" "[%s1]")
                   (if (= conn--wincontrol-arg-sign -1) "-" "")
                   conn--wincontrol-arg))
   'conn-wincontrol-string t))

(defun conn-wincontrol-message-function (string)
  (when conn-wincontrol-mode
    (if (text-property-any 0 (length string)
                           'conn-wincontrol-string
                           t string)
        string
      (concat (conn--wincontrol-message) string))))

(defun conn--wincontrol-minibuffer-exit ()
  (unless (> (minibuffer-depth) 1)
    (let ((message-log-max nil))
      (message "%s" (conn--wincontrol-message)))
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)))

(defun conn--wincontrol-minibuffer-setup ()
  (setq-local conn-wincontrol-mode nil
              conn-wincontrol-one-command-mode nil)
  (add-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit))

(defun conn--wincontrol-setup (&optional preserve-state)
  (when (zerop (minibuffer-depth))
    (let ((message-log-max nil))
      (message "%s" (conn--wincontrol-message))))
  (setq emulation-mode-map-alists
        `(conn--wincontrol-maps
          ,@(delq 'conn--wincontrol-maps emulation-mode-map-alists)))
  (add-hook 'set-message-functions #'conn-wincontrol-message-function)
  ;; Must be before 'repeat-post-hook
  (add-hook 'post-command-hook 'conn--wincontrol-post-command -98)
  (add-hook 'pre-command-hook 'conn--wincontrol-pre-command 98)
  (add-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
  (add-hook 'minibuffer-setup-hook 'conn--wincontrol-minibuffer-setup)
  (add-function :override eldoc-message-function 'conn--wincontrol-ignore)
  (unless preserve-state
    (setq conn--wincontrol-arg (when current-prefix-arg
                                 (prefix-numeric-value current-prefix-arg))
          conn--wincontrol-arg-sign 1
          conn--wincontrol-initial-window (selected-window)
          conn--wincontrol-initial-winconf (current-window-configuration)))
  (set-face-inverse-video 'mode-line t)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active t))

(defun conn--wincontrol-exit ()
  (setq conn--wincontrol-message-newline t)
  (remove-hook 'set-message-functions #'conn-wincontrol-message-function)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
  (remove-hook 'minibuffer-setup-hook 'conn--wincontrol-minibuffer-setup)
  (remove-function eldoc-message-function 'conn--wincontrol-ignore)
  (set-face-inverse-video 'mode-line nil)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active nil))

(defvar conn-wincontrol-one-command-stay-command
  (list 'conn-wincontrol-backward-delete-arg
        'conn-wincontrol-digit-argument-reset
        'conn-wincontrol-invert-argument
        'conn-wincontrol-digit-argument
        'conn-wincontrol-universal-arg
        'conn-wincontrol-quick-ref))

(defun conn-wincontrol-one-command-stay-p ()
  (memq this-command conn-wincontrol-one-command-stay-command))

(defun conn--wincontrol-one-command-hook ()
  (when (and conn-wincontrol-mode
             (not (conn-wincontrol-one-command-stay-p)))
    (remove-hook 'pre-command-hook 'conn--wincontrol-one-command-hook)
    (conn-wincontrol-one-command-mode -1)))

;;;###autoload
(define-minor-mode conn-wincontrol-one-command-mode
  "Global minor mode for wincontrol one command."
  :global t
  :group 'conn-wincontrol-mode
  (if conn-wincontrol-one-command-mode
      (progn
        (conn-wincontrol-mode 1)
        (add-hook 'pre-command-hook 'conn--wincontrol-one-command-hook)
        (setq conn--wincontrol-message-newline nil))
    (remove-hook 'pre-command-hook 'conn--wincontrol-one-command-hook)
    (conn-wincontrol-mode -1)))

;;;;; Wincontrol Quick Ref

(defvar conn-wincontrol-windows-1
  (conn-reference-page
    (:heading "Windows")
    ((("switch window" conn-goto-window)
      ("quit win" quit-window)
      ("delete win" delete-window)
      ("delete other" delete-other-windows)
      ("undo/redo" tab-bar-history-back tab-bar-history-forward)
      ("zoom in/out" text-scale-increase text-scale-decrease))
     (("kill buffer and win" kill-buffer-and-window)
      ("split win right/vert"
       conn-wincontrol-split-right
       conn-wincontrol-split-vertically)
      (:heading "Scroll:")
      ("up/down" conn-wincontrol-scroll-up conn-wincontrol-scroll-down)
      ("other win up/down"
       conn-wincontrol-other-window-scroll-up
       conn-wincontrol-other-window-scroll-down)
      ("register prefix" conn-register-prefix))
     ((:heading "Buffer:")
      ("switch" switch-to-buffer)
      ("beg/end" beginning-of-buffer end-of-buffer)
      ("prev/next" conn-previous-buffer conn-next-buffer)
      ("bury/unbury" conn-bury-buffer conn-unbury-buffer)
      ("kill buffer" conn-kill-this-buffer)))
    (((:keymap conn-window-resize-map)
      (:eval (concat
              (propertize "Resize Map:"
                          'face 'conn-quick-ref-heading-face)
              " "
              (propertize (key-description
                           (where-is-internal conn-window-resize-map
                                              conn-wincontrol-map t))
                          'face 'help-key-binding)))
      ("widen/narrow/heighten/shorten"
       conn-wincontrol-widen-window
       conn-wincontrol-narrow-window
       conn-wincontrol-heighten-window
       conn-wincontrol-shorten-window)
      ("maximize" maximize-window)
      ("max vert/horiz"
       conn-wincontrol-maximize-vertically
       conn-wincontrol-maximize-horizontally)
      ("balance" balance-windows))
     (("mru win" conn-wincontrol-mru-window)
      ("yank win" conn-yank-window)
      ("transpose win" conn-transpose-window)
      ("throw buffer" conn-throw-buffer)
      ("fit win" shrink-window-if-larger-than-buffer)))))

(defvar conn-wincontrol-windows-2
  (conn-reference-page
    (((:heading "Windmove")
      ("up/down/left/right"
       windmove-up
       windmove-down
       windmove-left
       windmove-right)
      ("swap states up/down/left/right"
       windmove-swap-states-up
       windmove-swap-states-down
       windmove-swap-states-left
       windmove-swap-states-right)))
    (((:heading "Misc:")
      ("tear off window" tear-off-window)
      ("isearch other window forward/back"
       conn-wincontrol-isearch-other-window
       conn-wincontrol-isearch-other-window-backward)))))

(defvar conn-wincontrol-tabs-and-frames
  (conn-reference-page
    (((:heading "Tabs:")))
    ((("next/prev" tab-next tab-previous)
      ("new" tab-new)
      ("close" tab-close))
     (("win to tab" tab-bar-move-window-to-tab)
      ("duplicate" tab-bar-duplicate-tab)
      ("detach" tab-bar-detach-tab)))
    (((:heading "Frames:")))
    ((("delete/other" delete-frame delete-other-frames)
      ("undelete" undelete-frame)
      ("clone" clone-frame))
     (("next/prev" tab-next tab-previous)
      ("other frame" other-frame)
      ("fullscreen" toggle-frame-fullscreen)))))

(defun conn-wincontrol-quick-ref (&optional page)
  (interactive "P")
  (let* ((pages (list conn-wincontrol-windows-1
                      conn-wincontrol-windows-2
                      conn-wincontrol-tabs-and-frames))
         (page (mod (or page 0) (length pages))))
    (conn-quick-reference
     (append (drop page pages)
             (reverse (take page pages))))))

;;;;; Wincontrol Prefix Arg

(defun conn-wincontrol-universal-arg ()
  "Multiply wincontrol prefix arg by 4."
  (declare (conn-wincontrol-preserve-arg))
  (interactive)
  (setq conn--wincontrol-arg (* 4 (or conn--wincontrol-arg 1))))

(defun conn-wincontrol-digit-argument ()
  (declare (conn-wincontrol-preserve-arg))
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (if conn--wincontrol-arg
        (setq conn--wincontrol-arg
              (+ (if (>= (or conn--wincontrol-arg 1) 0) digit (- digit))
                 (* 10 (or conn--wincontrol-arg 1))))
      (setq conn--wincontrol-arg digit)))
  (setq this-command 'conn-wincontrol-digit-argument))

(defun conn-wincontrol-invert-argument ()
  "Invert sign of wincontrol prefix arg."
  (declare (conn-wincontrol-preserve-arg))
  (interactive)
  (setq conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to nil and sign to +."
  (interactive))

(defun conn-wincontrol-backward-delete-arg ()
  "Delete least significant digit of prefix arg."
  (declare (conn-wincontrol-preserve-arg))
  (interactive)
  (setq conn--wincontrol-arg (floor conn--wincontrol-arg 10)))

(defun conn-wincontrol-prefix-arg-and-keep ()
  (setq conn--wincontrol-preserve-arg t)
  (when conn--wincontrol-arg
    (* conn--wincontrol-arg-sign conn--wincontrol-arg)))

(defun conn-wincontrol-consume-prefix-arg ()
  (when conn--wincontrol-arg
    (* conn--wincontrol-arg-sign conn--wincontrol-arg)))

;;;;; Wincontrol Quiting

(defun conn-wincontrol-exit ()
  "Exit `conn-wincontrol-mode'."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)))

(defun conn-wincontrol-abort ()
  "Exit `conn-wincontrol-mode'."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)
    (when conn--wincontrol-initial-winconf
      (set-window-configuration conn--wincontrol-initial-winconf))))

(defun conn-wincontrol-exit-to-initial-win ()
  "Exit `conn-wincontrol-mode' and select initial window."
  (interactive)
  (when conn-wincontrol-mode
    (conn-wincontrol-mode -1)
    (when (window-live-p conn--wincontrol-initial-window)
      (select-window conn--wincontrol-initial-window))))

;;;;; Wincontrol Isearch

(defun conn-wincontrol-isearch-other-window (arg)
  "`isearch-forward' in `other-window-for-scrolling'."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (isearch-forward arg)))

(defun conn-wincontrol-isearch-other-window-backward (arg)
  "`isearch-backward' in `other-window-for-scrolling'."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (isearch-backward arg)))

;;;;; Window Selection

(defun conn-wincontrol-next-window ()
  "`other-window' in cyclic order."
  (interactive)
  (other-window 1))

(defun conn-wincontrol-previous-window ()
  "`other-window' in reverse cyclic order."
  (interactive)
  (other-window -1))

(defalias 'conn-other-window 'other-window)

;;;###autoload
(defun conn-goto-window ()
  "Prompt for a window and then select it."
  (interactive)
  (let ((windows (delq (selected-window)
                       (conn--get-windows
                        nil 'nomini
                        (if current-prefix-arg 'visible)))))
    (if-let* ((window (conn-prompt-for-window windows)))
        (select-window window)
      (user-error "No other windows available to select"))))

;;;###autoload
(defun conn-wincontrol-mru-window ()
  "Select most recently used window."
  (interactive)
  (let* ((windows (sort (conn--get-windows)
                        :key #'window-use-time
                        :reverse t))
         (len (length windows))
         (idx 1))
    (when (> len 1)
      (let ((map (make-sparse-keymap))
            (msg-sym (make-symbol "msg-fn"))
            (key (vector last-command-event)))
        (define-key map key (lambda ()
                              (interactive)
                              (conn-<f idx 1+ (mod len))
                              (select-window (nth idx windows))))
        (select-window (nth idx windows))
        (set-transient-map
         map t
         (lambda ()
           (remove-hook 'post-command-hook msg-sym)
           (mapc #'window-bump-use-time (nreverse windows))))
        (fset msg-sym (lambda () (funcall repeat-echo-function map)))
        ;; Should be after `repeat-post-hook'
        (add-hook 'post-command-hook msg-sym 10)))))

;;;###autoload
(defun conn-wincontrol-quit-other-window-for-scrolling ()
  "`quit-window' in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))

(defun conn-window-label-mode-line ()
  (let ((win (selected-window)))
    (propertize
     (or (window-parameter win 'conn-label-string)
         (if (conn--dispatch-window-predicate (selected-window) t)
             (progn
               (conn--simple-window-labels)
               (window-parameter win 'conn-label-string))
           ""))
     'face 'bold)))

(define-minor-mode conn-wincontrol-label-mode-line-local-mode
  "Local mode for `conn-dispatch-window-mode-line-mode'."
  :lighter ""
  (if conn-wincontrol-label-mode-line-local-mode
      (setq mode-line-format
            `((conn-wincontrol-label-mode-line-local-mode
               (:eval (conn-window-label-mode-line)))
              ,@(assq-delete-all
                 'conn-wincontrol-label-mode-line-local-mode
                 mode-line-format)))
    (setq mode-line-format
          (assq-delete-all
           'conn-wincontrol-label-mode-line-local-mode
           mode-line-format))))

(defun conn--turn-on-label-mode-line-local-mode ()
  (conn-wincontrol-label-mode-line-local-mode 1))

;;;###autoload
(define-globalized-minor-mode conn-wincontrol-label-mode-line-mode
  conn-wincontrol-label-mode-line-local-mode
  conn--turn-on-label-mode-line-local-mode
  :group 'conn)

;;;;; Window Scroll Commands

;;;###autoload
(defun conn-wincontrol-other-window-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines
           (or (conn-wincontrol-prefix-arg-and-keep)
               next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-down-command)
                   (command-remapping #'conn-scroll-down)
                   #'conn-scroll-down)))))

;;;###autoload
(defun conn-wincontrol-other-window-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines
           (or (conn-wincontrol-prefix-arg-and-keep)
               next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-up-command)
                   (command-remapping #'conn-scroll-up)
                   #'conn-scroll-up)))))

;;;###autoload
(defun conn-wincontrol-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (let ((next-screen-context-lines
         (or (conn-wincontrol-prefix-arg-and-keep)
             next-screen-context-lines)))
    (conn-scroll-down)))

;;;###autoload
(defun conn-wincontrol-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (let ((next-screen-context-lines
         (or (conn-wincontrol-prefix-arg-and-keep)
             next-screen-context-lines)))
    (conn-scroll-up)))

;;;;; Window Configuration Commands

(defun conn-wincontrol-zoom-in ()
  (interactive)
  (conn->
    (conn-wincontrol-consume-prefix-arg)
    (prefix-numeric-value)
    (text-scale-increase)))

(defun conn-wincontrol-zoom-out ()
  (interactive)
  (conn->
    (conn-wincontrol-consume-prefix-arg)
    (prefix-numeric-value)
    (text-scale-decrease)))

(defun conn-wincontrol-reset-zoom ()
  (interactive)
  (text-scale-set 0))

(defun conn-wincontrol-widen-window ()
  (interactive)
  (enlarge-window-horizontally
   (or (conn-wincontrol-prefix-arg-and-keep) 1)))

(defun conn-wincontrol-narrow-window ()
  (interactive)
  (shrink-window-horizontally
   (or (conn-wincontrol-prefix-arg-and-keep) 1)))

(defun conn-wincontrol-heighten-window ()
  (interactive)
  (enlarge-window (or (conn-wincontrol-prefix-arg-and-keep) 1)))

(defun conn-wincontrol-shorten-window ()
  (interactive)
  (shrink-window (or (conn-wincontrol-prefix-arg-and-keep) 1)))

(defun conn-wincontrol-split-vertically ()
  "Split window vertically.
Uses `split-window-vertically'."
  (interactive)
  (select-window (split-window-vertically)))

(defun conn-wincontrol-split-right ()
  "Split window vertically.
Uses `split-window-right'."
  (interactive)
  (select-window (split-window-right)))

(defun conn-wincontrol-maximize-horizontally ()
  "Delete all adjacent windows horizontally.

Operates with the selected windows parent window."
  (interactive)
  (conn-wincontrol-maximize-vertically t))

(defun conn-wincontrol-maximize-vertically (&optional horizontal)
  "Delete all adjacent windows vertically.

Operates with the selected windows parent window."
  (interactive)
  (when (and (not (window-minibuffer-p))
             (not (window-parameter (selected-window) 'window-side))
             (window-combined-p (selected-window) horizontal))
    (cl-loop for sub = (thread-first
                         (selected-window)
                         window-parent
                         window-child)
             then (window-right sub)
             while sub
             unless (or (window-parameter sub 'no-delete-other-windows)
                        (eq sub (selected-window)))
             collect sub into to-delete
             finally (mapc #'delete-window to-delete))))

;;;###autoload
(defvar-keymap conn-kill-buffer-repeat-map)

;;;###autoload
(defun conn-kill-this-buffer ()
  (interactive)
  (kill-buffer)
  (set-transient-map conn-kill-buffer-repeat-map t nil t))

(provide 'conn-wincontrol-mode)
