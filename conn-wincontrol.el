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
(eval-when-compile
  (require 'cl-lib))

;;;; WinControl

(defgroup conn-wincontrol nil
  "Conn-mode WinControl."
  :prefix "conn-wincontrol-"
  :group 'conn)

(defvar conn--wincontrol-help-format
  (concat
   "\\<conn-wincontrol-map>"
   (propertize "WinControl " 'face 'minibuffer-prompt)
   "(arg: "
   (propertize "%s" 'face 'read-multiple-choice-face) ", "
   "\\[conn-wincontrol-digit-argument-reset]: reset arg; "
   "\\[conn-wincontrol-exit]: exit):"
   "%s"))

(defvar conn--wincontrol-arg nil)
(defvar conn--wincontrol-arg-sign 1)
(defvar conn--wincontrol-preserve-arg nil)
(defvar conn--wincontrol-initial-window nil)
(defvar conn--wincontrol-initial-winconf nil)
(defvar conn--wincontrol-error-message nil)
(defvar conn--wincontrol-prev-eldoc-msg-fn)

;;;;; Wincontrol Internals

(defvar-keymap conn-window-resize-map
  "v" 'conn-wincontrol-maximize-vertically
  "r" 'conn-wincontrol-maximize-horizontally
  "m" 'maximize-window
  "b" 'balance-windows
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-window-resize-repeat-map
  :repeat t
  "n" 'conn-wincontrol-narrow-window
  "s" 'conn-wincontrol-shorten-window
  "h" 'conn-wincontrol-heighten-window
  "w" 'conn-wincontrol-widen-window)

(defvar-keymap conn-other-window-repeat-map
  :repeat t
  "o" 'other-window)

(defvar-keymap conn-wincontrol-next-window-repeat-map
  :repeat t
  "o" 'conn-wincontrol-next-window)

(defvar-keymap conn-wincontrol-scroll-repeat-map
  :repeat t
  "i" 'conn-wincontrol-scroll-down
  "k" 'conn-wincontrol-scroll-up
  "K" 'conn-wincontrol-other-window-scroll-up
  "I" 'conn-wincontrol-other-window-scroll-down)

(defvar-keymap conn-wincontrol-text-scale-repeat-map
  :repeat t
  "z" 'text-scale-decrease
  "Z" 'text-scale-increase)

(defvar-keymap conn-wincontrol-tab-repeat-map
  :repeat t
  "C" 'tab-bar-duplicate-tab
  "O" 'tab-new
  "o" 'tab-next
  "U" 'tab-close
  "u" 'tab-previous
  "B" 'tab-bar-move-window-to-tab
  "D" 'tab-bar-detach-tab)

(defvar-keymap conn-windmove-repeat-map
  :repeat t
  "M-<down>" 'windmove-swap-states-down
  "M-<left>" 'windmove-swap-states-left
  "M-<right>" 'windmove-swap-states-right
  "M-<up>" 'windmove-swap-states-up
  "<down>" 'conn-wincontrol-windmove-down
  "<left>" 'conn-wincontrol-windmove-left
  "<right>" 'conn-wincontrol-windmove-right
  "<up>" 'conn-wincontrol-windmove-up
  "M-I" 'windmove-swap-states-up
  "M-J" 'windmove-swap-states-left
  "M-K" 'windmove-swap-states-down
  "M-L" 'windmove-swap-states-right
  "M-i" 'conn-wincontrol-windmove-up
  "M-j" 'conn-wincontrol-windmove-left
  "M-k" 'conn-wincontrol-windmove-down
  "M-l" 'conn-wincontrol-windmove-right)

(defalias 'conn-next-buffer 'next-buffer)
(defalias 'conn-previous-buffer 'previous-buffer)

(defvar-keymap conn-buffer-repeat-map
  :repeat t
  "l" 'conn-next-buffer
  "j" 'conn-previous-buffer)

(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits
  "C-q" 'conn-wincontrol-quick-ref
  "C-l" 'recenter-top-bottom
  "," conn-thing-remap
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
  "C-S-l" 'move-to-window-line-top-bottom
  "C-]" 'conn-wincontrol-abort
  "C-r" 'conn-wincontrol-isearch-backward
  "C-s" 'conn-wincontrol-isearch
  "C-u" 'conn-wincontrol-universal-arg
  "DEL" 'conn-wincontrol-backward-delete-arg
  "M-/" 'undelete-frame
  "M-<backspace>" 'conn-wincontrol-digit-argument-reset
  "M-<down>" 'windmove-swap-states-down
  "M-<left>" 'windmove-swap-states-left
  "M-<right>" 'windmove-swap-states-right
  "M-<up>" 'windmove-swap-states-up
  "M-DEL" 'conn-wincontrol-digit-argument-reset
  "M-I" 'windmove-swap-states-up
  "M-J" 'windmove-swap-states-left
  "M-K" 'windmove-swap-states-down
  "M-L" 'windmove-swap-states-right
  "M-c" 'clone-frame
  "M-d" 'delete-frame
  "M-i" 'conn-wincontrol-windmove-up
  "M-j" 'conn-wincontrol-windmove-left
  "M-k" 'conn-wincontrol-windmove-down
  "M-l" 'conn-wincontrol-windmove-right
  "M-o" 'other-frame
  "<escape>" 'conn-wincontrol-exit
  "<down>" 'conn-wincontrol-windmove-down
  "<left>" 'conn-wincontrol-windmove-left
  "<right>" 'conn-wincontrol-windmove-right
  "<up>" 'conn-wincontrol-windmove-up
  "<next>" 'conn-wincontrol-scroll-up
  "<prior>" 'conn-wincontrol-scroll-down
  "<return>" 'conn-other-place-prefix
  "<tab>" 'other-window
  "TAB" 'other-window
  "B" 'tab-bar-move-window-to-tab
  "C" 'tab-bar-duplicate-tab
  "D" 'tab-bar-detach-tab
  "F" 'toggle-frame-fullscreen
  "H" 'conn-kill-this-buffer
  "I" 'conn-wincontrol-other-window-scroll-down
  "J" 'bury-buffer
  "K" 'conn-wincontrol-other-window-scroll-up
  "L" 'unbury-buffer
  "O" 'tab-new
  "R" 'conn-wincontrol-isearch-other-window-backward
  "S" 'conn-wincontrol-isearch-other-window
  "U" 'tab-close
  "Z" 'text-scale-increase
  "T" 'tear-off-window
  "{" 'shrink-window-if-larger-than-buffer
  "`" 'conn-wincontrol-mru-window
  "b" 'switch-to-buffer
  "c" (conn-remap-key "C-c" nil t)
  "d" 'delete-window
  "e" 'conn-wincontrol-exit
  "f" 'conn-goto-window
  "g" (conn-remap-key "M-g" t t)
  "h" 'kill-buffer-and-window
  "i" 'conn-wincontrol-scroll-down
  "j" 'conn-previous-buffer
  "k" 'conn-wincontrol-scroll-up
  "l" 'conn-next-buffer
  "m" 'end-of-buffer
  "n" 'beginning-of-buffer
  "o" 'tab-next
  ">" 'conn-register-prefix
  "." 'conn-register-load
  "q" 'quit-window
  "s" 'conn-wincontrol-split-right
  "r" conn-window-resize-map
  "t" 'conn-transpose-window
  "u" 'tab-previous
  "v" 'conn-wincontrol-split-vertically
  "w" 'conn-throw-buffer
  "x" 'delete-other-windows
  "y" 'conn-yank-window
  "z" 'text-scale-decrease
  "C-c" nil
  "M-g" nil
  "C-x" nil
  "C-h" nil
  "C-g" 'keyboard-quit
  "<t>" 'conn-wincontrol-ignore)

(put 'conn-wincontrol-digit-argument-reset :advertised-binding (key-parse "M-DEL"))

(define-minor-mode conn-wincontrol-mode
  "Global minor mode for window control."
  :global t
  :lighter " WinC"
  :interactive nil
  :group 'conn-wincontrol
  (if conn-wincontrol-mode
      (conn--wincontrol-setup)
    (conn--wincontrol-exit)))

(defun conn-wincontrol ()
  "Enable `conn-wincontrol-mode'."
  (interactive)
  (if (= (minibuffer-depth) 0)
      (conn-wincontrol-mode 1)
    (user-error "Cannot activate wincontrol while minibuffer is active.")))

;; From transient
(defun conn--wincontrol-wrap-this-command ()
  (letrec ((command
            (when (symbolp this-command)
              (when-let* ((fn (symbol-function this-command))
                          ((autoloadp fn)))
                (autoload-do-load fn))
              this-command))
           (exit-and-debug
            (lambda (&rest args)
              (conn-wincontrol-mode -1)
              (apply #'debug args)))
           (advice
            (lambda (fn &rest args)
              (interactive
               (lambda (spec)
                 (let ((abort t))
                   (unwind-protect
                       (let ((debugger exit-and-debug))
                         (prog1
                             (advice-eval-interactive-spec spec)
                           (setq abort nil)))
                     (when (and abort command)
                       (remove-function (symbol-function command) advice))))))
              (unwind-protect
                  (let ((debugger exit-and-debug))
                    (apply fn args))
                (when command
                  (remove-function (symbol-function command) advice))))))
    (add-function :around (if command command this-command)
                  advice '((depth . -99)))))

(defun conn--wincontrol-pre-command ()
  (when (or conn--wincontrol-arg (< conn--wincontrol-arg-sign 0))
    (setq prefix-arg (* conn--wincontrol-arg-sign (or conn--wincontrol-arg 1))))
  (conn--wincontrol-wrap-this-command)
  (setq conn--wincontrol-error-message nil)
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message nil)))

(defun conn--wincontrol-post-command ()
  (unless conn--wincontrol-preserve-arg
    (setq conn--wincontrol-arg nil
          conn--wincontrol-arg-sign 1))
  (setq conn--wincontrol-preserve-arg nil)
  (cond
   ((not (eq conn-wincontrol-map (cadr overriding-terminal-local-map)))
    ;; Something else is using overriding-terminal-local-map,
    ;; e.g. isearch or transient, turn wincontrol off.
    (conn-wincontrol-mode -1))
   ((not (zerop (minibuffer-depth)))
    (conn--wincontrol-exit)
    (add-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit))
   (t (conn--wincontrol-message))))

(defun conn--wincontrol-new-frame (frame)
  (set-face-inverse-video 'mode-line t frame)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active t frame))

(defun conn--wincontrol-message ()
  (let ((message-log-max nil)
        (resize-mini-windows t))
    (message (substitute-command-keys conn--wincontrol-help-format)
             (format (if conn--wincontrol-arg "%s%s" "[%s1]")
                     (if (= conn--wincontrol-arg-sign -1) "-" "")
                     conn--wincontrol-arg)
             (concat
              (if conn--wincontrol-error-message " ")
              conn--wincontrol-error-message))))

(defalias 'conn--wincontrol-ignore 'ignore)

(defun conn--wincontrol-setup (&optional preserve-state)
  (internal-push-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  ;; Must be before 'repeat-post-hook
  (add-hook 'post-command-hook 'conn--wincontrol-post-command -98)
  (add-hook 'pre-command-hook 'conn--wincontrol-pre-command 98)
  (add-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
  (add-function :override eldoc-message-function 'conn--wincontrol-ignore)
  (unless preserve-state
    (setq conn--wincontrol-arg (when current-prefix-arg
                                 (prefix-numeric-value current-prefix-arg))
          conn--wincontrol-arg-sign 1
          conn--wincontrol-initial-window (selected-window)
          conn--wincontrol-initial-winconf (current-window-configuration)))
  (set-face-inverse-video 'mode-line t)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active t)
  (conn--wincontrol-message))

(defun conn--wincontrol-exit ()
  (internal-pop-keymap conn-wincontrol-map 'overriding-terminal-local-map)
  (remove-hook 'post-command-hook 'conn--wincontrol-post-command)
  (remove-hook 'pre-command-hook 'conn--wincontrol-pre-command)
  (remove-hook 'after-make-frame-functions 'conn--wincontrol-new-frame)
  (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
  (remove-function eldoc-message-function 'conn--wincontrol-ignore)
  (set-face-inverse-video 'mode-line nil)
  ;; Modus themes no longer have 'mode-line-active inherit from 'mode-line
  (set-face-inverse-video 'mode-line-active nil))

(defun conn--wincontrol-minibuffer-exit ()
  (when (= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'conn--wincontrol-minibuffer-exit)
    (conn--wincontrol-setup t)))

(defun conn-wincontrol-one-command ()
  "Execute one command in `conn-wincontrol-mode'."
  (interactive)
  (add-hook 'pre-command-hook
            (conn-anaphoricate hook
              (lambda ()
                (unless (memq this-command
                              '(conn-wincontrol-backward-delete-arg
                                conn-wincontrol-digit-argument-reset
                                conn-wincontrol-invert-argument
                                conn-wincontrol-digit-argument
                                conn-wincontrol-universal-arg
                                conn-wincontrol-quick-ref))
                  (remove-hook 'pre-command-hook hook)
                  (conn-wincontrol-exit))))
            99)
  (conn-wincontrol))

(defun conn-wincontrol-ignore ()
  (interactive)
  (setq conn--wincontrol-error-message (propertize "Invalid Command" 'face 'error)
        conn--wincontrol-preserve-arg t))

;;;;; Wincontrol Quick Ref

(defvar conn-wincontrol-windows-1
  (conn-reference-page "Windows"
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
      ("bury/unbury" bury-buffer unbury-buffer)
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
  (conn-reference-page "Windows"
    (((:heading "Windmove")
      ("up/down/left/right"
       conn-wincontrol-windmove-up
       conn-wincontrol-windmove-down
       conn-wincontrol-windmove-left
       conn-wincontrol-windmove-right)
      ("swap states up/down/left/right"
       windmove-swap-states-up
       windmove-swap-states-down
       windmove-swap-states-left
       windmove-swap-states-right)))
    (((:heading "Isearch:")
      ("this window forward/back"
       conn-wincontrol-isearch
       conn-wincontrol-isearch-backward)
      ("other window forward/back"
       conn-wincontrol-isearch-other-window
       conn-wincontrol-isearch-other-window-backward)))
    (((:heading "Misc:")
      ("tear off window" tear-off-window)))))

(defvar conn-wincontrol-tabs-and-frames
  (conn-reference-page "Tabs and Frames"
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
  (interactive)
  (setq conn--wincontrol-arg (* 4 (or conn--wincontrol-arg 1))
        conn--wincontrol-preserve-arg t))

(defun conn-wincontrol-digit-argument ()
  (interactive)
  (setq conn--wincontrol-preserve-arg t)
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
  (interactive)
  (setq conn--wincontrol-preserve-arg t
        conn--wincontrol-arg-sign (- conn--wincontrol-arg-sign)))

(defun conn-wincontrol-digit-argument-reset ()
  "Reset wincontrol prefix arg to nil and sign to +."
  (interactive)
  (setq conn--wincontrol-arg-sign 1
        conn--wincontrol-arg nil))

(defun conn-wincontrol-backward-delete-arg ()
  "Delete least significant digit of prefix arg."
  (interactive)
  (setq conn--wincontrol-preserve-arg t
        conn--wincontrol-arg (floor conn--wincontrol-arg 10)))

(defun conn-wincontrol-prefix-arg ()
  (when conn--wincontrol-arg
    (setq conn--wincontrol-preserve-arg t)
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

(defun conn-wincontrol-isearch (arg)
  "`isearch-forward', resuming `conn-wincontrol-mode' afterward."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-forward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-backward (arg)
  "`isearch-backward', resuming `conn-wincontrol-mode' afterward."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (isearch-backward arg)
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window (arg)
  "`isearch-forward' in `other-window-for-scrolling'."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-forward arg))
      (conn--wincontrol-setup t))))

(defun conn-wincontrol-isearch-other-window-backward (arg)
  "`isearch-backward' in `other-window-for-scrolling'."
  (interactive "P")
  (when conn-wincontrol-mode
    (conn--wincontrol-exit)
    (unwind-protect
        (with-selected-window (other-window-for-scrolling)
          (isearch-backward arg))
      (conn--wincontrol-setup t))))

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

(defvar conn-goto-window-cycle-limit 3)

(defun conn-goto-window (&optional arg)
  "Prompt for a window and then select it."
  (interactive "p")
  (let ((windows (delq (selected-window)
                       (conn--get-windows
                        nil 'nomini
                        (if current-prefix-arg 'visible)))))
    (if (length< windows conn-goto-window-cycle-limit)
        (progn
          (setq this-command 'conn-other-window)
          (put 'conn-other-window 'repeat-map
               (let ((map (make-sparse-keymap)))
                 (define-key map
                             (vector last-command-event)
                             'conn-other-window)
                 map))
          (other-window arg))
      (if-let* ((window (conn-prompt-for-window windows)))
          (select-window window)
        (user-error "No other windows available to select")))))

(defun conn-wincontrol-mru-window ()
  "Select most recently used window."
  (interactive)
  (when-let* ((mru (get-mru-window 0 nil t t)))
    (select-window mru)))

;;;;; Windmove

(defun conn-wincontrol-windmove-up ()
  "`windmove-up'."
  (interactive)
  (windmove-up))

(defun conn-wincontrol-windmove-down ()
  "`windmove-down'."
  (interactive)
  (windmove-down))

(defun conn-wincontrol-windmove-right ()
  "`windmove-right'."
  (interactive)
  (windmove-right))

(defun conn-wincontrol-windmove-left ()
  "`windmove-left'."
  (interactive)
  (windmove-left))

(defun conn-wincontrol-quit-other-window-for-scrolling ()
  "`quit-window' in `other-window-for-scrolling'."
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))

;;;;; Window Scroll Commands

(defun conn-wincontrol-other-window-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                         next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-down-command)
                   (command-remapping #'conn-scroll-down)
                   #'conn-scroll-down)))))

(defun conn-wincontrol-other-window-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (with-selected-window (other-window-for-scrolling)
    (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                         next-screen-context-lines)))
      (funcall (or (command-remapping #'scroll-up-command)
                   (command-remapping #'conn-scroll-up)
                   #'conn-scroll-up)))))

(defun conn-wincontrol-scroll-down ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-down)
  (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-down)))

(defun conn-wincontrol-scroll-up ()
  "Scroll down with ARG `next-screen-context-lines'."
  (interactive)
  (setq this-command 'conn-scroll-up)
  (let ((next-screen-context-lines (or (conn-wincontrol-prefix-arg)
                                       next-screen-context-lines)))
    (conn-scroll-up)))

;;;;; Window Configuration Commands

(defun conn-wincontrol-widen-window ()
  (interactive)
  (enlarge-window-horizontally (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-narrow-window ()
  (interactive)
  (shrink-window-horizontally (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-heighten-window ()
  (interactive)
  (enlarge-window (or (conn-wincontrol-prefix-arg) 1)))

(defun conn-wincontrol-shorten-window ()
  (interactive)
  (shrink-window (or (conn-wincontrol-prefix-arg) 1)))

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
  (when (and (not (window-minibuffer-p (selected-window)))
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

(static-if (<= 31 emacs-major-version)
    (progn
      (define-keymap
        :keymap conn-wincontrol-map
        "\\" 'window-layout-transpose
        "|" 'window-layout-flip-leftright
        "_" 'window-layout-flip-topdown)

      (define-keymap
        :keymap conn-window-resize-map
        "u" 'rotate-windows-back
        "o" 'rotate-windows
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise)

      (defvar-keymap conn-window-rotate-repeat-map
        :repeat t
        "<" 'window-layout-rotate-anticlockwise
        ">" 'window-layout-rotate-clockwise
        "u" 'rotate-windows-back
        "o" 'rotate-windows)))

(defun conn-kill-this-buffer ()
  (interactive)
  (kill-buffer))

(provide 'conn-wincontrol)
