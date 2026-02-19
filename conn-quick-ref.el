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

(require 'conn-utils)

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

(defface conn-quick-ref-page-header-face
  '((t ( :extend t
         :box nil
         :inherit header-line
         :underline (:style line :position t :color "black"))))
  "Face for quick ref header."
  :group 'conn-quick-ref)

(defvar conn-quick-ref-display-function 'conn--quick-ref-minibuffer)

(defvar conn-quick-ref-text-scale 0.95
  "Text scale to use for text in the quick ref buffer.")

(defvar conn--quick-ref-unbound
  (propertize "Ø" 'face 'conn-quick-ref-error-face))

(defvar-keymap conn-quick-ref-map
  "?" 'next
  "M-?" 'previous
  "<escape>" 'close)

(cl-defstruct (conn--reference-page
               ( :constructor conn--make-reference-page
                 (depth definition)))
  (depth 0 :type integer :read-only t)
  (definition nil :type list :read-only t))

(defmacro conn-reference-quote (form)
  (declare (indent 0))
  (cl-labels ((process-definition (def)
                (pcase def
                  (`(,(and (or :eval :splice :keymap :heading)
                           type)
                     . ,form)
                   (cons type (list '\, (cons 'lambda (cons nil form)))))
                  ((pred consp)
                   (mapcar #'process-definition def))
                  (_ def))))
    (list '\`
          (cl-loop for elem in form
                   if (listp elem)
                   collect (process-definition elem)
                   else collect elem))))

(defmacro conn-reference-page (&rest definition)
  (declare (indent 0))
  (let ((depth (or (when (eq :depth (car definition))
                     (pop definition)
                     (pop definition))
                   0)))
    (cl-labels ((process-definition (def)
                  (pcase def
                    (`(,(and (or :eval :splice :keymap :heading)
                             type)
                       . ,form)
                     (cons type (list '\, (cons 'lambda (cons nil form)))))
                    ((pred consp)
                     (mapcar #'process-definition def))
                    (_ def))))
      `(conn--make-reference-page
        ,depth
        (list ,@(cl-loop for row in definition
                         if (listp row)
                         collect (list '\` (process-definition row))
                         else
                         collect row))))))

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
  (let ((ref-buffer (current-buffer)))
    (cl-labels ((transpose (columns)
                  (let ((curr columns)
                        rows)
                    (while (seq-find 'identity curr)
                      (cl-loop for col in curr
                               collect (or (car col) "") into row
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
                        (`(:heading . ,fn)
                         (when-let* ((str (funcall fn)))
                           (push (propertize str 'face 'conn-quick-ref-heading-face)
                                 result)))
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
                        (`(:heading . ,fn)
                         (when-let* ((str (funcall fn)))
                           (push (list (propertize str 'face 'conn-quick-ref-heading-face))
                                 result)))
                        (`(:keymap . ,fn) (setf keymap (funcall fn)))
                        (`(:eval . ,fn) (push (funcall fn) row))
                        (`(:splice . ,fn) (cl-callf2 append (funcall fn) row))
                        ((and (pred consp) col)
                         (push (process-col col keymap) result))))
                    (nreverse result)))
                (insert-ref-string (str)
                  (let (beg)
                    (with-current-buffer ref-buffer
                      (with-silent-modifications
                        (setq beg (point))
                        (insert str "\n")
                        (add-text-properties beg (point) '(line-prefix " "))
                        (save-excursion
                          (goto-char beg)
                          (while (search-forward "\n" nil 'move-to-end)
                            (replace-match " \n"))))))))
      (with-current-buffer keymap-buffer
        (conn--where-is-with-remaps
          (while definition
            (pcase (pop definition)
              ((and (pred stringp) row)
               (insert-ref-string
                (substitute-command-keys row)))
              (`(:heading . ,fn)
               (when-let* ((str (funcall fn)))
                 (insert-ref-string
                  (propertize str 'face 'conn-quick-ref-heading-face))))
              (`(:eval . ,fn)
               (push (funcall fn) definition))
              (`(:splice . ,fn)
               (cl-callf2 append (funcall fn) definition))
              ((and row (pred consp))
               (let ((objs (or (transpose (process-row row))
                               (list "-"))))
                 (with-current-buffer ref-buffer
                   (let ((beg (point)))
                     (make-vtable
                      :face `( :inherit default
                               :height ,conn-quick-ref-text-scale)
                      :divider-width 2
                      :use-header-line nil
                      :objects objs)
                     (goto-char (point-max))
                     (add-text-properties beg (point)
                                          '(line-prefix " ")))))))))))))

(defun conn-quick-ref-insert-pages (pages buffer header)
  (let ((page-count 1)
        (keymap-buffer (current-buffer))
        (page-lines (max 10 (ceiling (frame-height) 3))))
    (with-current-buffer buffer
      (with-silent-modifications
        (cl-loop with beg = (point)
                 for page in pages do
                 (let ((prev (point)))
                   (conn--format-ref-page
                    (conn--reference-page-definition page)
                    keymap-buffer)
                   (when (> (count-lines beg (point)) page-lines)
                     (cl-incf page-count)
                     (save-excursion
                       (goto-char prev)
                       (insert " "))
                     (setq beg prev))))
        (goto-char (point-min))
        (let ((current-page 1))
          (insert (format header current-page page-count))
          (while (search-forward "" nil t)
            (save-excursion
              (forward-line -1)
              (when (looking-at-p "^\n")
                (delete-char -1)))
            (cl-incf current-page)
            (insert (format header current-page page-count))))
        (add-face-text-property (point-min)
                                (point-max)
                                `(:height ,conn-quick-ref-text-scale)
                                t)
        (goto-char (point-min))
        (narrow-to-page)))))

(defun conn-quick-ref-next-page (buffer)
  (with-current-buffer buffer
    (widen)
    (forward-page)
    (when (= (point) (point-max))
      (goto-char (point-min)))
    (narrow-to-page)))

(defun conn-quick-ref-previous-page (buffer)
  (with-current-buffer buffer
    (widen)
    (if (= (point) (point-min))
        (progn
          (goto-char (point-max))
          (forward-page -1))
      (forward-page -1))
    (narrow-to-page)))

(defun conn--quick-ref-parse-pages (pages)
  (cl-loop for p in pages
           append (pcase p
                    ((pred listp) p)
                    ((pred conn--reference-page-p) (list p))
                    ((pred functionp) (ensure-list (funcall p))))
           into result
           finally return (compat-call
                           sort result
                           :key #'conn--reference-page-depth)))

(defun conn-quick-reference (&rest pages)
  (when-let* ((pages (conn--quick-ref-parse-pages
                      (flatten-tree pages))))
    (let* ((buf (get-buffer-create " *conn-quick-ref*"))
           (display-function conn-quick-ref-display-function)
           (inhibit-message t)
           (state nil)
           (header
            (substitute-command-keys
             (concat " \\<conn-quick-ref-map>"
                     (propertize "[%s/%s] " 'face 'minibuffer-prompt)
                     (propertize "Quick Reference" 'face 'bold)
                     " — \\[next] Next; \\[previous] Previous; \\[close] Close "
                     "\n"))))
      (add-face-text-property 0 (length header)
                              'conn-quick-ref-page-header-face
                              t header)
      (with-current-buffer buf
        (widen)
        (special-mode)
        (setq-local page-delimiter "")
        (with-silent-modifications
          (delete-region (point-min) (point-max))))
      (conn-quick-ref-insert-pages pages buf header)
      (conn-threadf-> state (funcall display-function buf))
      (unwind-protect
          (conn-with-overriding-map conn-quick-ref-map
            (cl-loop
             (let ((keys (read-key-sequence-vector nil)))
               (pcase (key-binding keys)
                 ('close (cl-return))
                 ('next
                  (conn-quick-ref-next-page buf)
                  (conn-threadf-> state (funcall display-function buf)))
                 ('previous
                  (conn-quick-ref-previous-page buf)
                  (conn-threadf-> state (funcall display-function buf)))
                 ((or 'quit 'keyboard-quit)
                  (keyboard-quit))
                 (_ (conn-add-unread-events (this-single-command-raw-keys))
                    (cl-return))))))
        (funcall display-function buf state t)))))

(defun conn-quick-ref-to-cols (list col-count)
  (cl-loop with cols = (cl-loop repeat col-count collect nil)
           for elem in list
           for i from 0
           do (push elem (nth (mod i col-count) cols))
           finally return (mapcar #'nreverse cols)))

(defun conn--quick-ref-minibuffer (buffer &optional state teardown)
  (let (inhibit-message
        message-log-max)
    (if teardown
        (when state (funcall state))
      (prog1
          (or state
              (let ((mh max-mini-window-height)
                    (rs resize-mini-windows))
                (setq max-mini-window-height 1.0
                      resize-mini-windows t)
                (lambda ()
                  (setq max-mini-window-height mh
                        resize-mini-windows rs)
                  (message nil))))
        (with-current-buffer buffer
          (message "%s" (buffer-substring (point-min)
                                          (1- (point-max)))))))))

(provide 'conn-quick-ref)
