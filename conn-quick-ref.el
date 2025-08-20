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

(cl-defstruct (conn-reference-page
               (:constructor conn--make-reference-page))
  (title nil :type string :read-only t)
  (definition nil :type list :read-only t))

(defun conn-reference-page (title &rest definition)
  (declare (compiler-macro
            (lambda (_exp)
              `(conn--make-reference-page
                :title ,title
                :definition (list ,@definition)))))
  (conn--make-reference-page
   :title title
   :definition definition))

(defvar-keymap conn-quick-ref-map
  "C-h" 'next
  "M-h" 'previous
  "C-M-h" 'index)

(defvar conn-quick-ref-display-function 'conn--quick-ref-minibuffer)

(defvar conn-quick-ref-text-scale 0.95)

(defvar conn-quick-ref-pre-insert-hook nil)
(defvar conn-quick-ref-post-insert-hook nil)

(defun conn--quick-ref-unbound ()
  (propertize "🚫" 'face 'conn-quick-ref-error-face))

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
    (let ((keymap nil))
      (while definition
        (pcase (pop definition)
          (:keymap
           (setq keymap (eval (pop definition) t)))
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

(defun conn--quick-ref-insert-header (title)
  (let ((header
         (concat " "
                 (propertize title 'face 'bold)
                 ": "
                 (propertize "C-h" 'face 'help-key-binding)
                 " Next; "
                 (propertize "M-h" 'face 'help-key-binding)
                 " Previous; "
                 (propertize "C-M-h" 'face 'help-key-binding)
                 " Index \n")))
    (insert header)))

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
        (run-hooks 'conn-quick-ref-pre-insert-hook)
        (conn--quick-ref-insert-header title)
        (setq header-pos (point))
        (conn--format-ref-page definition keymap-buffer)
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

(defun conn--quick-ref-minibuffer (buffer hide-p)
  (let (inhibit-message
        message-log-max)
    (if hide-p
        (message nil)
      (with-current-buffer buffer
        (message (buffer-string))))))

(defun conn-get-reference (symbol)
  (declare (side-effect-free t)
           (important-return-value t)
           (gv-setter (lambda (val)
                        `(setf (get ,symbol :conn-quick-reference)
                               ,val))))
  (get symbol :conn-quick-reference))

;;;; Eval With State Args Reference

;;;###autoload
(cl-defgeneric conn-argument-get-reference (argument)
  (declare (important-return-value t)
           (side-effect-free t))
  ( :method (_arg) nil)
  ( :method ((arg cons))
    (append (ensure-list (conn-argument-get-reference (car arg)))
            (ensure-list (conn-argument-get-reference (cdr arg))))))

(cl-defmethod conn-argument-get-reference ((_arg conn-thing-argument))
  (conn-get-reference 'conn-read-thing-ref))

(cl-defmethod conn-argument-get-reference ((_arg conn-dispatch-action-argument))
  (conn-get-reference 'conn-dispatch-action-ref))

;;;; State Reference

;;;###autoload
(cl-defgeneric conn-state-get-reference (state)
  (declare (important-return-value t)
           (side-effect-free t)))

(cl-defmethod conn-state-get-reference ((state (conn-substate t))
                                        &optional argument-pages)
  (append (conn-state-get state :quick-reference t)
          argument-pages))

(cl-defmethod conn-state-get-reference ((state (conn-substate conn-dispatch-state))
                                        &optional argument-pages)
  (append argument-pages
          (conn-state-get state :quick-reference t)))

(setf (conn-state-get 'conn-dispatch-state :quick-reference)
      (list (conn-reference-page
             "Dispatch Commands"
             `(("History:"
                ("next/prev"
                 conn-dispatch-cycle-ring-next
                 conn-dispatch-cycle-ring-previous))
               ("Last Dispatch:"
                ("repeat" conn-repeat-last-dispatch)
                ("describe" conn-dispatch-ring-describe-head))))))

;;;; Pages

;; (setf (conn-get-reference 'conn-resize-ref)
;;       (conn-reference-page
;;        "Window Page 1" "A Test Page"
;;        `(:keymap
;;          (conn-get-state-map 'conn-dispatch-state)
;;          (,(lambda nil
;;              (let
;;                  ((binding
;;                    (where-is-internal
;;                     conn-window-resize-map
;;                     conn-wincontrol-map t)))
;;                (concat
;;                 (propertize
;;                  (if binding
;;                      (key-description binding)
;;                    (conn--quick-ref-unbound))
;;                  'face 'help-key-binding)
;;                 " Resize Map:")))
;;           ("Max" maximize-window)
;;           ("Max Vert/Horiz"
;;            conn-wincontrol-maximize-vertically
;;            conn-wincontrol-maximize-horizontally)
;;           ("Balance" balance-windows)
;;           ("heighten/shorten"
;;            conn-wincontrol-heighten-window
;;            conn-wincontrol-shorten-window)
;;           ("widen/narrow"
;;            conn-wincontrol-widen-window
;;            conn-wincontrol-narrow-window)
;;           ("error" conn-wincontrol-not-a-command)))))

(setf (conn-get-reference 'conn-read-thing-ref)
      (list
       (conn-reference-page
        "Things"
        "Use a thing command to specify a region to operate on.")))

(setf (conn-get-reference 'conn-dispatch-action-ref)
      (list
       (conn-reference-page
        "Actions"
        `((("yank from/replace"
            conn-dispatch-yank-from
            conn-dispatch-yank-from-replace)
           ("yank to/replace" conn-dispatch-yank-to
            conn-dispatch-yank-replace-to)
           ("yank read/replace"
            conn-dispatch-reading-yank-to
            conn-dispatch-yank-read-replace-to)
           ("send/replace" conn-dispatch-send
            conn-dispatch-send-replace)
           ("take/replace" conn-dispatch-take
            conn-dispatch-take-replace))
          (("copy to" conn-dispatch-copy-to
            conn-dispatch-copy-replace-to)
           ("transpose" conn-dispatch-transpose)
           ("goto/over" conn-dispatch-over-or-goto)
           ("kapply" conn-dispatch-kapply))
          (("kill/append/prepend" conn-dispatch-kill
            conn-dispatch-kill-append
            conn-dispatch-kill-prepend)
           ("copy/append/prepend" conn-dispatch-copy
            conn-dispatch-copy-append
            conn-dispatch-copy-prepend)
           ("register" conn-dispatch-register-load
            conn-dispatch-register-replace)
           ("up/down/capital case"
            conn-dispatch-upcase
            conn-dispatch-downcase
            conn-dispatch-capitalize))))))

(setf (conn-get-reference 'conn-dispatch-command-ref)
      (list
       (conn-reference-page
        "Dispatch Commands"
        `(("History:"
           ("next/prev"
            conn-dispatch-cycle-ring-next
            conn-dispatch-cycle-ring-previous))
          ("Last Dispatch:"
           ("repeat" conn-repeat-last-dispatch)
           ("describe"
            conn-dispatch-ring-describe-head))))))

(provide 'conn-quick-ref)

