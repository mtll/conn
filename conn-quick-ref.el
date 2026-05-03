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

;;;; Quick Reference

(defgroup conn-quick-ref nil
  "Conn posframes."
  :prefix "conn-quick-ref-"
  :group 'conn)

(defface conn-quick-ref-heading-face
  '((t (:bold t :underline t)))
  "Face for column headings in quick ref buffer."
  :group 'conn-quick-ref)

(defface conn-quick-ref-unbound-face
  '((t (:inherit error)))
  "Face for key not found errors."
  :group 'conn-quick-ref)

(define-derived-mode conn-quick-ref-mode special-mode "Quick-Ref"
  "Mode for conn quick reference buffers.")

(defvar conn-quick-ref-display-function 'conn-quick-ref-buffer)

(defvar conn--quick-ref-unbound
  (propertize "Ø" 'face 'conn-quick-ref-unbound-face))

(defvar conn-quick-reference-header-key-description
  '(:eval (substitute-command-keys
           "\\<conn-quick-ref-map>\\[next] Next; \\[previous] Previous; \\[close] Close"))
  "Keybinding hints for `conn-quick-reference-header-line-format'.")

(defvar conn-quick-reference-header-line-format
  `(" "
    (:eval (format "[%s/%s] "
                   conn--quick-ref-current-page
                   conn--quick-ref-page-count))
    ,(propertize "Quick Reference"
                 'face '(:weight bold))
    "   ("
    ,conn-quick-reference-header-key-description
    ") ")
  "Template for `header-line-format' in quick reference buffers.")

(defvar conn--quick-ref-current-page nil)
(defvar conn--quick-ref-page-count nil)

(defvar-keymap conn-quick-ref-map
  "?" 'next
  "M-?" 'previous
  "<escape>" 'close)

(cl-defstruct (conn--reference-page
               ( :constructor conn--make-reference-page
                 (name depth definition)))
  (name nil :type symbol :read-only t)
  (depth 0 :type integer)
  (definition nil :type list :read-only t))

(defmacro conn-reference-quote (form)
  (declare (indent 0))
  (cl-labels ((process-definition (def)
                (pcase def
                  (`(,(and (or :eval :splice :keymap :heading)
                           type)
                     . ,form)
                   `(cons ,type (lambda () ,@form)))
                  (`(,'\, ,def) def)
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
                    (`(,'\, ,exp) exp)
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
                (get-key (bind keymap &rest options)
                  (let ((noindirect (memq :noindirect options))
                        (no-remap (memq :no-remap options)))
                    (if-let* ((key (if keymap
                                       (where-is-internal bind keymap t noindirect no-remap)
                                     (or (check-advertised bind)
                                         (when overriding-terminal-local-map
                                           (where-is-internal
                                            bind
                                            (list overriding-terminal-local-map)
                                            t noindirect no-remap))
                                         (where-is-internal bind nil t noindirect no-remap)))))
                        (propertize (key-description key) 'face 'help-key-binding)
                      conn--quick-ref-unbound)))
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
                        ((or `(,bind . ,options)
                             bind)
                         (push (apply #'get-key bind keymap options) keys))))
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
                   (make-vtable :divider-width 2
                                :use-header-line nil
                                :objects objs)
                   (goto-char (point-max))))))))))))

(defun conn-quick-ref-insert-pages (pages buffer)
  (let ((keymap-buffer (current-buffer))
        (page-lines (max 10 (ceiling (frame-height) 3.5))))
    (with-current-buffer buffer
      (setq-local conn--quick-ref-page-count 1
                  conn--quick-ref-current-page 1
                  page-delimiter "")
      (with-silent-modifications
        (let ((beg (point)))
          (dolist (page pages)
            (let ((prev (point)))
              (conn--format-ref-page
               (conn--reference-page-definition page)
               keymap-buffer)
              (when (and (> (count-lines beg (point)) page-lines)
                         (/= prev beg))
                (cl-incf conn--quick-ref-page-count)
                (save-excursion
                  (goto-char prev)
                  (insert ""))
                (setq beg prev)))))
        (goto-char (point-min))
        (narrow-to-page)))))

(defun conn-quick-ref-next-page (buffer)
  (with-current-buffer buffer
    (widen)
    (forward-page 1)
    (cl-incf conn--quick-ref-current-page)
    (when (= (point) (point-max))
      (setq-local conn--quick-ref-current-page 1)
      (goto-char (point-min)))
    (narrow-to-page)))

(defun conn-quick-ref-previous-page (buffer)
  (with-current-buffer buffer
    (widen)
    (cl-decf conn--quick-ref-current-page)
    (when (= (point) (point-min))
      (setq-local conn--quick-ref-current-page conn--quick-ref-page-count)
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
  (let* ((inhibit-message t)
         (state nil)
         (buf (generate-new-buffer " *conn-quick-ref*")))
    (conn--unwind-protect-all
      (progn
        (with-current-buffer buf
          (conn-quick-ref-mode)
          (setq-local cursor-type nil
                      header-line-format conn-quick-reference-header-line-format
                      mode-line-format nil
                      face-remapping-alist `((header-line-inactive header-line)
                                             (mode-line-inactive mode-line)
                                             ,@face-remapping-alist)))
        (conn-quick-ref-insert-pages pages buf)
        (when loop-fn (funcall loop-fn))
        (conn->f state
          (funcall conn-quick-ref-display-function buf))
        (conn-with-overriding-map conn-quick-ref-map
          (cl-loop
           (let ((keys (let ((inhibit-quit t))
                         (read-key-sequence nil))))
             (when (eql (aref keys 0) (car (last (current-input-mode))))
               (signal 'quit nil))
             (pcase (key-binding keys 'accept-default)
               ('close (cl-return))
               ('next
                (conn-quick-ref-next-page buf)
                (when loop-fn (funcall loop-fn))
                (conn->f state
                  (funcall conn-quick-ref-display-function buf)))
               ('previous
                (conn-quick-ref-previous-page buf)
                (when loop-fn (funcall loop-fn))
                (conn->f state
                  (funcall conn-quick-ref-display-function buf)))
               ((or 'quit 'keyboard-quit)
                (signal 'quit nil))
               (binding
                (cl-block continue
                  (when loop-fn
                    (funcall loop-fn binding
                             (lambda () (cl-return-from continue))))
                  (conn-add-unread-events
                   (this-single-command-raw-keys))
                  (cl-return))))))))
      (funcall conn-quick-ref-display-function buf state t)
      (kill-buffer buf))))

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

(defun conn-quick-reference-keymap ()
  (interactive)
  (if-let* ((keys (this-command-keys-vector))
            (prefix (seq-take keys (1- (length keys))))
            (keymap (key-binding prefix 'accept-default))
            (pages (compat-call
                    sort
                    (seq-uniq
                     (flatten-tree (conn-get-quick-ref-pages keymap))
                     (lambda (a b)
                       (eq (conn--reference-page-name a)
                           (conn--reference-page-name b))))
                    :key #'conn--reference-page-depth)))
      (funcall
       (catch 'end-loop
         (conn-with-overriding-map (make-composed-keymap
                                    (list (define-keymap "C-h" prefix-help-command)
                                          keymap
                                          (define-keymap "<t>" #'undefined)))
           (conn--quick-ref-loop
            pages
            (lambda (&optional binding continue)
              (let (inhibit-message message-log-max)
                (message "References for keymaps under %s"
                         (key-description prefix)))
              (pcase binding
                ('nil)
                ('undefined
                 (let (inhibit-message)
                   (message "%s not bound in keymaps"
                            (key-description (this-command-keys-vector))))
                 (funcall continue))
                (_
                 (let (inhibit-message) (message nil))
                 (throw 'end-loop
                        (lambda ()
                          (conn->
                            (this-command-keys-vector)
                            (vconcat prefix)
                            (command-execute binding nil)))))))))
         #'ignore))
    (command-execute prefix-help-command nil
                     (vconcat prefix (key-parse "C-h")))))

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

;;;; Buffer Display Function

(defcustom conn-quick-reference-buffer-separator 'line
  "Separator for quick reference buffer display."
  :type '(choice
          (const :tag "No separator" nil)
          (const :tag "Substitute thin line" line)
          (const :tag "Display help information in mode-line instead of header-line"
                 mode-line)))

(defun conn-quick-ref-buffer (buffer &optional initialized teardown)
  (if teardown
      (when-let* ((win (get-buffer-window buffer)))
        (delete-window win))
    (unless (get-buffer-window buffer)
      (display-buffer buffer '((display-buffer-in-side-window (side . bottom))
                               (window-height . fit-window-to-buffer))))
    (unless initialized
      (pcase conn-quick-reference-buffer-separator
        ('line
         (with-current-buffer buffer
           (let* ((face `(:background ,(face-foreground 'default) :extend t))
                  (line (propertize "__" 'face face 'display '(space :height (1))))
                  (newline (propertize "\n" 'face face 'line-height t)))
             (with-silent-modifications
               (without-restriction
                 (save-excursion
                   (goto-char (point-min))
                   (while (search-forward "" nil t)
                     (backward-char 1)
                     (insert line newline)
                     (forward-char 1))
                   (goto-char (point-max))
                   (insert line newline)))))))
        ('mode-line
         (ignore
          (cl-shiftf (buffer-local-value 'mode-line-format buffer)
                     (buffer-local-value 'header-line-format buffer)
                     nil)))))
    (when-let* ((win (get-buffer-window buffer)))
      (with-current-buffer buffer
        (set-window-point win (point)))
      (let ((window-min-height 2))
        (fit-window-to-buffer win)))
    t))

(provide 'conn-quick-ref)
