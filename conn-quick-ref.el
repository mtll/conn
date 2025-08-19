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
  '((t (:inherit error :bold t)))
  "Face for key not found errors."
  :group 'conn-quick-ref)

(defface conn-quick-ref-page-header
  '((t ( :inverse-video nil :extend t
         :box nil :underline (:style line :position t)
         :inherit header-line)))
  "Face for selection in Conn posframes."
  :group 'conn-quick-ref)


;;;; Page Impl

(defvar-keymap conn-quick-ref-map
  "C-h" 'next
  "M-h" 'previous
  "C-M-h" 'index)

(defvar conn-quick-ref-display-function 'conn--quick-ref-minibuffer)

(defvar conn-quick-ref-text-scale 0.95)

(defvar conn-quick-ref-pre-insert-hook nil)
(defvar conn-quick-ref-post-insert-hook nil)

;;;###autoload
(defmacro conn-define-ref-page (name description &rest rows)
  (declare (indent 2))
  `(progn
     (put ',name :conn-quick-ref-layout (list ,@rows))
     (put ',name :conn-quick-ref-description ,description)))

(defun conn--quick-ref-unbound ()
  (propertize "🚫" 'face 'conn-quick-ref-error-face))

(defun conn--format-ref-page (name keymap-buffer)
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
              (process-bindings (description bindings keymap)
                (let (keys)
                  (dolist (bind bindings)
                    (if-let* ((key (or (and (symbolp bind)
                                            (get bind :advertised-binding))
                                       (where-is-internal bind
                                                          (and keymap (list keymap))
                                                          t))))
                        (push (propertize (key-description key)
                                          'face 'help-key-binding)
                              keys)
                      (push (conn--quick-ref-unbound) keys)))
                  (concat (string-join (nreverse keys) ", ")
                          ": " description)))
              (process-col (col keymap)
                (let ((result nil))
                  (while col
                    (pcase (pop col)
                      (:keymap (setq keymap (eval (pop col) t)))
                      ((and fn (pred functionp))
                       (let ((str (funcall fn)))
                         (add-face-text-property
                          0 (length str)
                          'conn-quick-ref-heading-face t str)
                         (push str result)))
                      ((and str (pred stringp))
                       (push (propertize str 'face 'conn-quick-ref-heading-face)
                             result))
                      (`(:keymap ,keymap ,desc . ,bindings)
                       (push (process-bindings desc bindings keymap)
                             result))
                      (`(,desc . ,bindings)
                       (push (process-bindings desc bindings keymap)
                             result))))
                  (nreverse result)))
              (process-row (row keymap)
                (let ((result nil))
                  (while row
                    (pcase (pop row)
                      (:keymap (setq keymap (eval (pop row) t)))
                      ((and (pred consp) col)
                       (push (process-col col keymap) result))))
                  (nreverse result))))
    (let ((rows (get name :conn-quick-ref-layout))
          (keymap nil))
      (while rows
        (pcase (pop rows)
          (:keymap
           (setq keymap (eval (pop rows) t)))
          ((and row (pred stringp))
           (insert (with-current-buffer keymap-buffer
                     (substitute-command-keys row))
                   "\n"))
          ((and row (pred consp))
           (make-vtable
            :face `( :inherit default
                     :height ,conn-quick-ref-text-scale)
            :divider-width 2
            :use-header-line nil
            :objects (with-current-buffer keymap-buffer
                       (transpose (process-row row keymap))))))))))

(defun conn-quick-ref-insert-header (page)
  (let ((header
         (concat " "
                 (propertize (get page :conn-quick-ref-description)
                             'face 'bold)
                 ": "
                 (propertize "C-h" 'face 'help-key-binding)
                 " Next; "
                 (propertize "M-h" 'face 'help-key-binding)
                 " Previous; "
                 (propertize "C-M-h" 'face 'help-key-binding)
                 " Index \n")))
    (insert header)))

(defun conn-quick-ref-insert-page (page buffer)
  (let ((keymap-buffer (current-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (let (buffer-read-only
            header-pos)
        (delete-region (point-min) (point-max))
        (run-hooks 'conn-quick-ref-pre-insert-hook)
        (conn-quick-ref-insert-header page)
        (setq header-pos (point))
        (conn--format-ref-page page keymap-buffer)
        (run-hooks 'conn-quick-ref-post-insert-hook)
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
        (pages (copy-sequence pages))
        (inhibit-message t))
    (conn-quick-ref-insert-page (car pages) buf)
    (funcall conn-quick-ref-display-function buf nil)
    (unwind-protect
        (catch 'break
          (conn-with-overriding-map conn-quick-ref-map
            (while t
              (let ((keys (read-key-sequence-vector nil)))
                (pcase (key-binding keys)
                  ('next
                   (setq pages (nconc (cdr pages) (list (car pages))))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall conn-quick-ref-display-function buf nil))
                  ('previous
                   (setq pages (nconc (last pages) (butlast pages)))
                   (conn-quick-ref-insert-page (car pages) buf)
                   (funcall conn-quick-ref-display-function buf nil))
                  ('index)
                  ('quit (throw 'break nil))
                  (_ (setq unread-command-events
                           (mapcar (lambda (key)
                                     (cons 'no-record key))
                                   (listify-key-sequence keys)))
                     (throw 'break nil)))))))
      (funcall conn-quick-ref-display-function buf t))))

;;;###autoload
(defun conn-quick-ref-state ()
  (interactive)
  (when-let* ((manual (conn-state-get conn-current-state :quick-ref-manual)))
    (conn-quick-reference manual)))

(defun conn--quick-ref-minibuffer (buffer hide-p)
  (let (inhibit-message
        message-log-max)
    (if hide-p
        (message nil)
      (with-current-buffer buffer
        (message (buffer-string))))))

;;;; Eval With State Ref

(cl-defmethod conn-get-argument-ref-pages ((_arg conn-thing-argument))
  'conn-read-thing-ref)

(cl-defmethod conn-get-argument-ref-pages ((_arg conn-dispatch-action-argument))
  (list 'conn-dispatch-action-ref
        'conn-dispatch-command-ref))

;;;; Pages

(conn-define-ref-page conn-resize-ref "Window Page 1"
  "A Test Page"
  `( :keymap (conn-get-state-map 'conn-dispatch-state)
     (,(lambda ()
         (let ((binding
                (where-is-internal conn-window-resize-map
                                   conn-wincontrol-map
                                   t)))
           (concat (propertize
                    (if binding
                        (key-description binding)
                      (conn--quick-ref-unbound))
                    'face 'help-key-binding)
                   " Resize Map:")))
      ("Max" maximize-window)
      ("Max Vert/Horiz"
       conn-wincontrol-maximize-vertically
       conn-wincontrol-maximize-horizontally)
      ("Balance" balance-windows)
      ("heighten/shorten"
       conn-wincontrol-heighten-window
       conn-wincontrol-shorten-window)
      ("widen/narrow"
       conn-wincontrol-widen-window
       conn-wincontrol-narrow-window)
      ("error" conn-wincontrol-not-a-command))))

(conn-define-ref-page conn-read-thing-ref "Things"
  "Use a thing command to specify a region to operate on.")

(conn-define-ref-page conn-dispatch-action-ref "Actions"
  `((("yank from"
      conn-dispatch-yank-from
      conn-dispatch-yank-from-replace)
     ("yank to"
      conn-dispatch-yank-to
      conn-dispatch-yank-replace-to)
     ("yank read"
      conn-dispatch-reading-yank-to
      conn-dispatch-yank-read-replace-to)
     ("kapply" conn-dispatch-kapply))
    (("copy to"
      conn-dispatch-copy-to
      conn-dispatch-copy-replace-to)
     ("goto/over" conn-dispatch-over-or-goto)
     ("send"
      conn-dispatch-send
      conn-dispatch-send-replace)
     ("take"
      conn-dispatch-take
      conn-dispatch-take-replace))
    (("kill/append/prepend"
      conn-dispatch-kill
      conn-dispatch-kill-append
      conn-dispatch-kill-prepend)
     ("copy/append/prepend"
      conn-dispatch-copy
      conn-dispatch-copy-append
      conn-dispatch-copy-prepend)
     ("transpose" conn-dispatch-transpose)
     ("register"
      conn-dispatch-register-load
      conn-dispatch-register-replace)
     ("up/down/capital case"
      conn-dispatch-upcase
      conn-dispatch-downcase
      conn-dispatch-capitalize))))

(conn-define-ref-page conn-dispatch-command-ref "Dispatch Commands"
  `(("History:"
     ("next/prev"
      conn-dispatch-cycle-ring-next
      conn-dispatch-cycle-ring-previous))
    ("Last Dispatch:"
     ("repeat" conn-repeat-last-dispatch)
     ("describe" conn-dispatch-ring-describe-head))))

(provide 'conn-quick-ref)

