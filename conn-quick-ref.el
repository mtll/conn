;;; conn-quick-ref.el --- Reference Pages -*- lexical-binding: t -*-
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

;;; Code:

;;;; Requires

(require 'conn)
(eval-when-compile
  (require 'cl-lib))

(autoload 'make-vtable "vtable")


;;;; Faces

(defgroup conn-quick-ref nil
  "Conn posframes."
  :prefix "conn-quick-ref-"
  :group 'conn)

(defface conn-quick-ref-heading-face
  '((t (:bold t :underline t)))
  "Face for column headings in quick ref buffer."
  :group 'conn-quick-ref)

(defface conn-quick-ref-error-face
  '((t (:inherit error)))
  "Face for key not found errors."
  :group 'conn-quick-ref)

(defface conn-quick-ref-page-header
  '((t ( :inverse-video nil :extend t
         :box nil :underline (:style line :position t)
         :inherit header-line)))
  "Face for selection in Conn posframes."
  :group 'conn-quick-ref)


;;;; Page Impl

(cl-defstruct (conn-reference-page
                  (:constructor conn--make-reference-page (title definition)))
  (title nil :type string :read-only t)
  (definition nil :type list :read-only t))

;;;###autoload
(defmacro conn-reference-page (title &rest definition)
  (declare (indent 1))
  (cl-labels ((process-definition (def)
                (pcase def
                  (`(,(and (or :eval :splice :keymap) type) . ,form)
                   (cons type (list '\, (cons 'lambda (cons nil form)))))
                  ((pred consp)
                   (mapcar #'process-definition def))
                  (_ def))))
    `(conn--make-reference-page
      ,title
      (list ,@(cl-loop for row in definition
                       if (listp row)
                       collect (list '\` (process-definition row))
                       else
                       collect row)))))

(defvar-keymap conn-quick-ref-map
  "C-h" 'next
  "M-h" 'previous
  "<escape>" 'close)

(defvar conn-quick-ref-display-function 'conn--quick-ref-minibuffer)

(defvar conn-quick-ref-text-scale 0.95)

(defvar conn--quick-ref-unbound
  (propertize "Ø" 'face 'conn-quick-ref-error-face))

(defun conn-quick-ref-find-remap (remap &optional keymap)
  (let (result)
    (cl-labels ((find-keys (keymap remap prefix)
                  (map-keymap
                   (lambda (key def)
                     (let ((all-keys (vconcat prefix (vector key))))
                       (pcase def
                         ((and (pred keymapp) sub-keymap)
                          (find-keys sub-keymap remap all-keys))
                         ((guard (and (equal def remap)
                                      (eq (keymap--menu-item-binding remap)
                                          (lookup-key keymap all-keys))))
                          (push all-keys result)))))
                   keymap)))
      (find-keys (pcase keymap
                   ('nil (make-composed-keymap (current-active-maps)))
                   ((pred keymapp) keymap)
                   (_ (make-composed-keymap keymap)))
                 remap []))
    (if-let* ((keys (car (sort result :key 'length))))
        (propertize (key-description keys)
                    'face 'help-key-binding)
      conn--quick-ref-unbound)))

(defun conn--format-ref-page (definition keymap-buffer)
  (cl-labels ((transpose (columns)
                (let ((curr columns)
                      rows)
                  (while (seq-find 'identity curr)
                    (cl-loop for col in curr
                             collect (or (car col) "-") into row
                             collect (cdr col) into next
                             finally do (progn
                                          (push row rows)
                                          (setq curr next))))
                  (nreverse rows)))
              (check-advertised (bind keymap)
                (when-let* ((_(symbolp bind))
                            (adv (get bind :advertised-binding))
                            (desc (key-description adv))
                            (_(eq bind (key-binding adv))))
                  adv))
              (get-key (bind keymap)
                (if-let* ((key (if keymap
                                   (where-is-internal bind keymap t)
                                 (or (check-advertised bind keymap)
                                     (when overriding-terminal-local-map
                                       (where-is-internal
                                        bind
                                        (list overriding-terminal-local-map)
                                        t))
                                     (where-is-internal bind nil t)))))
                    (propertize (key-description key) 'face 'help-key-binding)
                  conn--quick-ref-unbound))
              (process-bindings (description bindings keymap)
                (let (keys)
                  (while bindings
                    (pcase (pop bindings)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) bindings))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) bindings))
                      ((and str (pred stringp))
                       (push str keys))
                      (bind (push (get-key bind keymap) keys))))
                  (concat (string-join (nreverse keys) ", ")
                          ": " description)))
              (process-col (col keymap)
                (let ((result nil))
                  (while col
                    (pcase (pop col)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) col))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) col))
                      (`(:heading ,str)
                       (push (propertize str 'face 'conn-quick-ref-heading-face)
                             result))
                      ((and str (pred stringp)) (push str result))
                      (`(,desc . ,bindings)
                       (push (process-bindings desc bindings keymap)
                             result))))
                  (nreverse result)))
              (process-row (row)
                (let (keymap
                      result)
                  (while row
                    (pcase (pop row)
                      ('nil)
                      (`(:keymap . ,fn) (setf keymap (funcall fn)))
                      (`(:eval . ,fn) (push (funcall fn) row))
                      (`(:splice . ,fn) (cl-callf2 append (funcall fn) row))
                      ((and (pred consp) col)
                       (push (process-col col keymap) result))))
                  (nreverse result))))
    (conn--where-is-with-remaps
      (while definition
        (goto-char (point-max))
        (pcase (pop definition)
          ((and (pred stringp) row)
           (let ((beg (point)))
             (insert (with-current-buffer keymap-buffer
                       (substitute-command-keys row))
                     "\n")
             (goto-char beg)
             (while (search-forward "\n" nil 'move-to-end)
               (replace-match " \n"))))
          (`(:eval . ,fn)
           (with-current-buffer keymap-buffer
             (push (funcall fn) definition)))
          (`(:splice . ,fn)
           (with-current-buffer keymap-buffer
             (cl-callf2 append (funcall fn) definition)))
          ((and row (pred consp))
           (make-vtable
            :face `( :inherit default
                     :height ,conn-quick-ref-text-scale)
            :divider-width 2
            :use-header-line nil
            :objects (with-current-buffer keymap-buffer
                       (transpose (process-row row))))))))))

(defun conn-quick-ref-insert-page (page buffer)
  (pcase-let (((cl-struct conn-reference-page
                          title
                          definition)
               page)
              (keymap-buffer (current-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (let (buffer-read-only
            header-pos)
        (delete-region (point-min) (point-max))
        (insert (substitute-command-keys
                 (concat "\\<conn-quick-ref-map> "
                         (propertize title 'face 'bold)
                         " — \\[next]: Next; \\[previous]: Previous; \\[close]: Close \n")))
        (setq header-pos (point))
        (conn--format-ref-page definition keymap-buffer)
        (indent-region header-pos (point-max) 1)
        (add-face-text-property
         (point-min) (point-max)
         `(:height ,conn-quick-ref-text-scale)
         t)
        (add-face-text-property
         (point-min) header-pos
         'conn-quick-ref-page-header t)))))

;;;###autoload
(defun conn-quick-reference (pages)
  (let ((buf (get-buffer-create " *conn-quick-ref*"))
        (display-function conn-quick-ref-display-function)
        (pages (copy-sequence pages))
        (inhibit-message t))
    (conn-quick-ref-insert-page (car pages) buf)
    (funcall display-function buf nil)
    (unwind-protect
        (catch 'break
          (conn-with-overriding-map conn-quick-ref-map
            (while t
              (let ((keys (read-key-sequence-vector nil)))
                (pcase (key-binding keys)
                  ('close
                   (throw 'break nil))
                  ('next
                   (setq pages (nconc (cdr pages) (list (car pages))))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall display-function buf nil))
                  ('previous
                   (setq pages (nconc (last pages) (butlast pages)))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall display-function buf nil))
                  ((or 'quit 'keyboard-quit)
                   (keyboard-quit))
                  (_ (setq unread-command-events
                           (mapcar (lambda (key)
                                     (cons 'no-record key))
                                   (listify-key-sequence keys)))
                     (throw 'break nil)))))))
      (funcall display-function buf t))))

(defun conn--quick-ref-minibuffer (buffer hide-p)
  (let (inhibit-message message-log-max)
    (if hide-p
        (message nil)
      (with-current-buffer buffer
        (message (buffer-string))))))

;;;; Pages

(defvar conn-dispatch-thing-ref
  (conn-reference-page "Things"
    "Use a thing command to specify a region to operate on."
    "Dispatch state redefines some thing bindings:
"
    ((:keymap (list (conn-get-state-map 'conn-dispatch-mover-state)))
     (("symbol" forward-symbol))
     (("line" forward-line))
     (("column" next-line))
     (("defun"
       (:eval (conn-quick-ref-find-remap
               conn-end-of-defun-remap
               (conn-get-state-map 'conn-dispatch-mover-state))))))))

(defvar conn-dispatch-action-ref
  (conn-reference-page "Actions"
    ((("yank from/replace"
       conn-dispatch-yank-from
       conn-dispatch-yank-from-replace)
      ("yank to/replace"
       conn-dispatch-yank-to
       conn-dispatch-yank-replace-to)
      ("yank read/replace"
       conn-dispatch-reading-yank-to
       conn-dispatch-yank-read-replace-to)
      ("send/replace"
       conn-dispatch-send
       conn-dispatch-send-replace)
      ("take/replace"
       conn-dispatch-take
       conn-dispatch-take-replace))
     (("copy to"
       conn-dispatch-copy-to
       conn-dispatch-copy-replace-to)
      ("transpose" conn-dispatch-transpose)
      ("goto/over" conn-dispatch-over-or-goto)
      ("kapply" conn-dispatch-kapply))
     (("kill/append/prepend"
       conn-dispatch-kill
       conn-dispatch-kill-append
       conn-dispatch-kill-prepend)
      ("copy/append/prepend"
       conn-dispatch-copy
       conn-dispatch-copy-append
       conn-dispatch-copy-prepend)
      ("register"
       conn-dispatch-register-load
       conn-dispatch-register-replace)
      ("up/down/capital case"
       conn-dispatch-upcase
       conn-dispatch-downcase
       conn-dispatch-capitalize)))))

(defvar conn-dispatch-command-ref
  (conn-reference-page "Misc Commands"
    (((:heading "History:")
      ("next/prev"
       conn-dispatch-cycle-ring-next
       conn-dispatch-cycle-ring-previous))
     ((:heading "Last Dispatch:")
      ("repeat" conn-repeat-last-dispatch)
      ("describe" conn-dispatch-ring-describe-head)))
    (((:heading "Other Args")
      ("dispatch repeatedly" repeat-dispatch)
      ("exchange point and mark after selecting THING"
       dispatch-other-end)
      ("restrict matches to the selected window"
       restrict-windows)))))

(defvar conn-dispatch-select-ref
  (conn-reference-page "Selection Commands"
    (((:heading "Targeting Commands")
      ("retarget" retarget)
      ("always retarget" always-retarget)
      ("change target finder" change-target-finder))
     ((:heading "Window Commands")
      ("goto window" conn-goto-window)
      ("scroll up" scroll-up)
      ("scroll down" scroll-down)
      ("restrict" restrict-windows)))
    (((:heading "Misc")
      ("dispatch at click" act)
      ("undo" undo)
      ("recursive edit" recursive-edit))
     (""
      ("isearch forward" isearch-regexp-forward)
      ("isearch backward" isearch-regexp-backward)))))

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

(defvar conn-transpose-reference
  (conn-reference-page "Transpose"
    (:eval
     (string-join
      '("Transpose reads a THING command and transposes two of those THINGs. If
THING is `recursive-edit' then the current region and a region defined
within a recursive edit will be transposed."
        ""
        "Transpose defines some addition thing bindings:"
        "")
      "\n"))
    ((("line" conn-backward-line forward-line))
     (("symbol" forward-symbol))
     (("defun" (:eval (conn-quick-ref-find-remap
                       conn-end-of-defun-remap
                       (conn-get-state-map 'conn-transpose-state)))))
     (("recursive-edit" recursive-edit)))))

(defvar conn-read-thing-reference
  (conn-reference-page "Thing"
    "The region to operate on will be defined by a thing command. A prefix
argument may be supplied for the thing command."))

(defvar conn-trim-argument-reference
  (conn-reference-page "Trim"
    "Trim excess characters at either end of the region. By default trims
whitespace characters. With a non-nil prefix argument this will prompt
for a custom trim regex."))

(defvar conn-subregions-argument-reference
  (conn-reference-page "Subregions"
    "If this argument is non-nil then operate on the subregions defined by
the thing command. By default the subregions of a thing command are the
individual things that are moved over. For example the subregions of
`forward-word' with a prefix argument of 3 are the 3 regions containing
the 3 individual words, as opposed to the single region containing all 3
words."))

;;;; Wincontrol Reference

;;;###autoload
(defun conn-wincontrol-quick-ref (&optional page)
  (interactive "P")
  (let* ((pages (list conn-wincontrol-windows-1
                      conn-wincontrol-windows-2
                      conn-wincontrol-tabs-and-frames))
         (page (mod (or page 0) (length pages))))
    (conn-quick-reference
     (append (drop page pages)
             (reverse (take page pages))))))

;;;; Argument Reference

;;;###autoload
(cl-defgeneric conn-argument-reference (arg)
  ( :method (_) nil)
  ( :method ((args cons))
    (list (mapcan 'conn-argument-reference args)))
  ( :method :around ((arg conn-state-eval-argument))
    (if-let* ((ref (conn-state-eval-argument--reference arg)))
        (funcall ref)
      (cl-call-next-method))))

(cl-defmethod conn-argument-reference ((_arg conn-trim-argument))
  (list conn-trim-argument-reference))

(cl-defmethod conn-argument-reference ((_arg conn-subregions-argument))
  (list conn-subregions-argument-reference))

;;;; State Reference

;;;###autoload
(cl-defgeneric conn-state-reference (state &optional args)
  (declare (important-return-value t)
           (side-effect-free t))
  (:method (_state &optional _) nil))

(cl-defmethod conn-state-reference ((_state (eql conn-dispatch-state))
                                    &optional _args)
  (list conn-dispatch-action-ref
        conn-dispatch-command-ref
        conn-dispatch-thing-ref))

(cl-defmethod conn-state-reference ((_state (eql conn-transpose-state))
                                    &optional _args)
  (list conn-transpose-reference))

(cl-defmethod conn-state-reference ((_state (eql conn-read-thing-state))
                                    &optional args)
  (append (list conn-read-thing-reference)
          (thread-first
            (conn-argument-reference args)
            flatten-tree delete-dups)))

(provide 'conn-quick-ref)
