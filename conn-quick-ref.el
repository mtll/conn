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

(defvar conn-quick-ref-display-function 'conn--quick-ref-buffer)

(defvar conn--quick-ref-unbound
  (propertize "Ø" 'face 'conn-quick-ref-error-face))

(defvar-keymap conn-quick-ref-map
  "?" 'next
  "M-?" 'previous
  "<escape>" 'close)

(cl-defstruct (conn--reference-page
               ( :constructor conn--make-reference-page
                 (name depth definition)))
  (name nil :type symbol :read-only t)
  (depth 0 :type integer :read-only t)
  (definition nil :type list :read-only t))

(defmacro conn-reference-quote (form)
  (declare (indent 0))
  (cl-labels ((process-definition (def)
                (pcase def
                  (`(,(and (or :eval :splice :keymap :heading)
                           type)
                     . ,form)
                   `(cons ,type (lambda () ,@form)))
                  ((pred consp)
                   `(list ,@(mapcar #'process-definition def)))
                  (_ `(quote ,def)))))
    `(list ,@(cl-loop for elem in form
                      if (listp elem)
                      collect (process-definition elem)
                      else collect elem))))

(defmacro conn-reference-page (&rest definition)
  (declare (indent 0))
  (let (depth name)
    (while (pcase definition
             (`(:depth ,n . ,_def)
              (cl-callf2 drop 2 definition)
              (setq depth n)
              t)
             (`(:name ,n . ,_def)
              (cl-callf2 drop 2 definition)
              (setq name n)
              t)))
    (unless depth (setq depth 0))
    (cl-labels ((process-definition (def)
                  (pcase def
                    (`(,(and (or :eval :splice :keymap :heading)
                             type)
                       . ,form)
                     `(cons ,type (lambda () ,@form)))
                    ((pred consp)
                     `(list ,@(mapcar #'process-definition def)))
                    (_ `(quote ,def)))))
      `(conn--make-reference-page
        ',(or name (make-symbol "anonymous-page"))
        ,depth
        (list ,@(cl-loop for row in definition
                         if (listp row)
                         collect (process-definition row)
                         else collect row))))))

(defun conn--format-ref-page (definition keymap-buffer)
  (let ((ref-buffer (current-buffer)))
    (cl-labels ((transpose (columns)
                  (let ((curr columns)
                        rows)
                    (while (seq-find 'identity curr)
                      (cl-loop for col in curr
                               collect (or (car col) "") into row
                               collect (cdr col) into next
                               finally do
                               (push row rows)
                               (setq curr next)))
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
                    (conn--with-work-buffer
                      (dolist (key (nreverse keys))
                        (insert key ", "))
                      (delete-char -2)
                      (insert " ")
                      (let ((col (current-column)))
                        (save-excursion
                          (insert description))
                        (while (= 0 (forward-line))
                          (indent-to col))
                        (buffer-string)))))
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
                  (let (keymap result)
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
                  (with-current-buffer ref-buffer
                    (with-silent-modifications
                      (insert str "\n")))))
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
                   (make-vtable
                    :divider-width 2
                    :use-header-line nil
                    :objects objs)
                   (goto-char (point-max))))))))))))

(defun conn-quick-ref-insert-pages (pages buffer header)
  (let ((page-count 1)
        (keymap-buffer (current-buffer))
        (page-lines (max 10 (ceiling (frame-height) 3.5)))
        (face `(:background ,(face-foreground 'default) :extend t)))
    (with-current-buffer buffer
      (with-silent-modifications
        (let ((beg (point)))
          (dolist (page pages)
            (let ((prev (point)))
              (conn--format-ref-page
               (conn--reference-page-definition page)
               keymap-buffer)
              (when (and (> (count-lines beg (point)) page-lines)
                         (/= prev beg))
                (cl-incf page-count)
                (save-excursion
                  (goto-char prev)
                  (insert
                   (propertize "__" 'face face 'display '(space :height (1)))
                   (propertize "\n" 'face face 'line-height t)
                   ""))
                (setq beg prev)))))
        (goto-char (point-min))
        (let ((current-page 1))
          (insert (format header current-page page-count))
          (while (search-forward "" nil t)
            (cl-incf current-page)
            (insert (format header current-page page-count))))
        (goto-char (point-max))
        (insert
         (propertize "__" 'face face 'display '(space :height (1)))
         (propertize "\n" 'face face 'line-height t))
        (goto-char (point-min))
        (narrow-to-page)))))

(defun conn-quick-ref-next-page (buffer)
  (with-current-buffer buffer
    (widen)
    (forward-page 1)
    (when (= (point) (point-max))
      (goto-char (point-min)))
    (narrow-to-page)))

(defun conn-quick-ref-previous-page (buffer)
  (with-current-buffer buffer
    (widen)
    (when (= (point) (point-min))
      (goto-char (point-max)))
    (backward-page 1)
    (narrow-to-page)))

(defun conn-get-quick-ref-pages (&optional keymap)
  (let ((result nil))
    (cl-labels ((bindings (keymap)
                  (map-keymap
                   (lambda (key def)
                     (pcase (keymap--menu-item-binding def)
                       ((and def (guard (eq key 'conn-quick-ref)))
                        (push def result))
                       ((and def (pred keymapp))
                        (bindings def))))
                   keymap)))
      (if keymap
          (bindings keymap)
        (mapc #'bindings (current-active-maps t)))
      (flatten-tree (nreverse result)))))

(defun conn--quick-ref-loop (pages loop-fn)
  (let* ((buf (get-buffer-create " *conn-quick-ref*"))
         (display-function conn-quick-ref-display-function)
         (inhibit-message t)
         (state nil)
         (header
          (substitute-command-keys
           (concat "\\<conn-quick-ref-map>"
                   (propertize "[%s/%s] " 'face 'minibuffer-prompt)
                   (propertize "Quick Reference" 'face 'bold)
                   " — \\[next] Next; \\[previous] Previous; \\[close] Close "
                   "\n"))))
    (add-face-text-property 0 (length header)
                            'conn-quick-ref-page-header-face
                            t header)
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (delete-all-overlays))
      (setq-local page-delimiter ""
                  cursor-type nil
                  mode-line-format nil))
    (conn-quick-ref-insert-pages pages buf header)
    (when loop-fn (funcall loop-fn))
    (conn->f state (funcall display-function buf))
    (unwind-protect
        (conn-with-overriding-map conn-quick-ref-map
          (cl-loop
           (let ((keys (let ((inhibit-quit t))
                         (read-key-sequence nil))))
             (when (eql (aref keys 0) (car (last (current-input-mode))))
               (signal 'quit nil))
             (pcase (key-binding keys)
               ('close (cl-return))
               ('next
                (conn-quick-ref-next-page buf)
                (when loop-fn (funcall loop-fn))
                (conn->f state (funcall display-function buf)))
               ('previous
                (conn-quick-ref-previous-page buf)
                (when loop-fn (funcall loop-fn))
                (conn->f state (funcall display-function buf)))
               ((or 'quit 'keyboard-quit)
                (signal 'quit nil))
               (_ (conn-add-unread-events
                   (this-single-command-raw-keys))
                  (cl-return))))))
      (funcall display-function buf state t))))

(defun conn-quick-reference (pages &optional loop-fn)
  (interactive (list (conn-get-quick-ref-pages)))
  (if-let* ((pages (compat-call
                    sort
                    (seq-uniq (flatten-tree pages)
                              (lambda (a b)
                                (eq (conn--reference-page-name a)
                                    (conn--reference-page-name b))))
                    :key #'conn--reference-page-depth)))
      (conn--quick-ref-loop pages loop-fn)
    (user-error "No quick reference pages")))

(defun conn-add-keymap-reference (keymap references)
  (let* ((pages (flatten-tree (ensure-list references)))
         (names (mapcar #'conn--reference-page-name pages)))
    (define-key keymap
                [conn-quick-ref]
                (append pages
                        (seq-remove
                         (lambda (page)
                           (memq (conn--reference-page-name page) names))
                         (lookup-key keymap [conn-quick-ref]))))))

(defun conn-remove-keymap-reference (keymap reference)
  (let ((name (if (conn--reference-page-p reference)
                  (conn--reference-page-name reference)
                reference)))
    (define-key keymap
                [conn-quick-ref]
                (seq-remove
                 (lambda (page)
                   (eq name (conn--reference-page-name page)))
                 (lookup-key keymap [conn-quick-ref])))))

(defun conn-quick-ref-to-cols (list col-count)
  (cl-loop with cols = (make-list col-count nil)
           for elem in list
           for i from 0
           do (push elem (nth (mod i col-count) cols))
           finally return (mapcar #'nreverse cols)))

(defun conn--quick-ref-buffer (buffer &optional state teardown)
  (if teardown
      (when state (set-window-configuration state))
    (prog1 (or state (current-window-configuration))
      (unless (get-buffer-window buffer)
        (display-buffer buffer '((display-buffer-in-side-window (side . bottom))
                                 (window-height . fit-window-to-buffer))))
      (when-let* ((win (get-buffer-window buffer)))
        (with-current-buffer buffer
          (set-window-point win (point)))
        (fit-window-to-buffer win)))))

(provide 'conn-quick-ref)
