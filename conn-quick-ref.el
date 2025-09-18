;;; conn-quick-ref.el --- Conn Utilities -*- lexical-binding: t -*-
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

;;; Code

(eval-when-compile
  (require 'cl-lib))

;;;;; Quick Reference

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

(cl-defstruct (conn-reference-page
                  (:constructor conn--make-reference-page (title definition)))
  (title nil :type string :read-only t)
  (definition nil :type list :read-only t))

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
  "C-q" 'next
  "M-q" 'previous
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
              (check-advertised (bind)
                (when-let* ((_(symbolp bind))
                            (adv (get bind :advertised-binding))
                            (desc (key-description adv))
                            (_(eq bind (key-binding adv))))
                  adv))
              (get-key (bind keymap)
                (if-let* ((key (if keymap
                                   (where-is-internal bind keymap t)
                                 (or (check-advertised bind)
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
    (let ((ref-buffer (current-buffer)))
      (with-current-buffer keymap-buffer
        (conn--where-is-with-remaps
          (while definition
            (pcase (pop definition)
              ((and (pred stringp) row)
               (let ((str (substitute-command-keys row))
                     beg)
                 (with-current-buffer ref-buffer
                   (setq beg (point))
                   (insert str "\n")
                   (goto-char beg)
                   (while (search-forward "\n" nil 'move-to-end)
                     (replace-match " \n"))
                   (goto-char (point-max)))))
              (`(:eval . ,fn)
               (push (funcall fn) definition))
              (`(:splice . ,fn)
               (cl-callf2 append (funcall fn) definition))
              ((and row (pred consp))
               (let ((objs (transpose (process-row row))))
                 (with-current-buffer ref-buffer
                   (make-vtable
                    :face `( :inherit default
                             :height ,conn-quick-ref-text-scale)
                    :divider-width 2
                    :use-header-line nil
                    :objects objs)
                   (goto-char (point-max))))))))))))

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

(provide 'conn-quick-ref)
