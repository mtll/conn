;;; conn-mark.el -*- lexical-binding: t -*-
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

;;; Code:

(require 'conn-utils)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn-command-thing "conn-things")

;;;;; Mark Variables

(defvar conn-this-command-handler nil
  "Mark handler for current command.

Commands may set this variable if they need to change their handler
dynamically.")

(defvar conn-this-command-thing nil
  "`this-command'\\='s thing.")

(defvar conn-this-command-start (make-marker)
  "Start position for current mark movement command.")

;;;; Mark Handlers

(defvar-local conn-mark-handler-overrides-alist nil
  "Buffer local overrides for command mark handlers.

Is an alist of the form ((CMD . MARK-HANDLER) ...).

For the meaning of MARK-HANDLER see `conn-command-mark-handler'.")

(define-inline conn-command-mark-handler (command)
  "Return the mark handler for COMMAND."
  (declare (important-return-value t)
           (side-effect-free t)
           (gv-setter conn-set-command-mark-handler))
  (inline-letevals (command)
    (inline-quote
     (and (symbolp ,command)
          (or (alist-get ,command conn-mark-handler-overrides-alist)
              (get ,command :conn-mark-handler))))))

(defun conn-set-command-mark-handler (command handler)
  (put command :conn-mark-handler handler))

(defun conn-symbol-handler (beg)
  "Mark handler for symbols."
  (let ((list (ignore-errors (bounds-of-thing-at-point 'list))))
    (cond ((not (derived-mode-p 'prog-mode))
           (conn-continuous-thing-handler beg))
          ((and (conn--point-in-comment-or-string-p)
                (save-excursion
                  (goto-char beg)
                  (conn--point-in-comment-or-string-p)))
           (conn-continuous-thing-handler beg))
          ((or (conn--point-in-comment-or-string-p)
               (save-excursion
                 (goto-char beg)
                 (conn--point-in-comment-or-string-p)))
           (conn-discrete-thing-handler beg))
          ((equal list (save-excursion
                         (goto-char beg)
                         (bounds-of-thing-at-point 'list)))
           (conn-continuous-thing-handler beg))
          ((conn-discrete-thing-handler beg)))))

(defun conn-continuous-thing-handler (beg)
  "Mark the things which have been moved over."
  (ignore-errors
    (cond ((= 0 (abs (prefix-numeric-value current-prefix-arg))))
          ((= (point) beg)
           (pcase (bounds-of-thing-at-point conn-this-command-thing)
             (`(,beg . ,end)
              (cond ((= (point) beg) (conn--push-ephemeral-mark end))
                    ((= (point) end) (conn--push-ephemeral-mark beg))))))
          ((let ((dir (pcase (- (point) beg)
                        (0 0)
                        ((pred (< 0)) 1)
                        ((pred (> 0)) -1))))
             (save-excursion
               (goto-char beg)
               (forward-thing conn-this-command-thing dir)
               (forward-thing conn-this-command-thing (- dir))
               (conn--push-ephemeral-mark)))))))

(defun conn-discrete-thing-handler (_beg)
  "Mark the thing at point."
  (pcase (ignore-errors (bounds-of-thing-at-point conn-this-command-thing))
    (`(,beg . ,end)
     (conn--push-ephemeral-mark (if (= (point) end) beg end)))))

(defun conn-jump-handler (beg)
  "Place a mark where point used to be."
  (unless (= beg (point))
    (conn--push-ephemeral-mark beg)))

;;;; Mark Cursor

(defcustom conn-mark-overlay-priority 2000
  "Priority of mark overlay."
  :type 'integer
  :set (lambda (sym val)
         (set sym val)
         (put 'conn--mark-cursor 'priority val))
  :group 'conn)

(defface conn-mark-face
  '((t (:inherit region :extend nil)))
  "Face for conn mark cursor."
  :group 'conn-faces)

(defvar-local conn--mark-cursor nil)
(put 'conn--mark-cursor 'permanent-local t)

(defvar conn--prev-mark-even-if-inactive nil
  "Previous value of `mark-even-if-inactive'.

Used to restore previous value when `conn-mode' is disabled.")

(defvar-local conn--disable-mark-cursor nil)

(defvar-local conn--ephemeral-mark nil)

(put 'conn--mark-cursor 'face 'conn-mark-face)
(put 'conn--mark-cursor 'priority conn-mark-overlay-priority)
(put 'conn--mark-cursor 'conn-overlay t)
(put 'conn--mark-cursor 'overlay-after-string
     (propertize " " 'face 'conn-mark-face))

(conn-set-mode-property 'special-mode :disable-mark-cursor t)

(defun conn--mark-cursor-redisplay (win)
  (if (or (not conn-local-mode)
          conn--disable-mark-cursor
          (null (mark t))
          (and (window-minibuffer-p win)
               (not (eq win (active-minibuffer-window)))))
      (progn
        (when conn--mark-cursor
          (delete-overlay conn--mark-cursor))
        (setf conn--mark-cursor nil))
    (unless conn--mark-cursor
      (setf conn--mark-cursor (make-overlay (mark t) (1+ (mark t))))
      (overlay-put conn--mark-cursor 'category 'conn--mark-cursor))
    (cond ((or (use-region-p)
               (= (point-max) (mark t) (point)))
           (when (overlay-get conn--mark-cursor 'after-string)
             (overlay-put conn--mark-cursor 'after-string nil))
           (unless (eql (overlay-start conn--mark-cursor)
                        (overlay-end conn--mark-cursor))
             (move-overlay conn--mark-cursor (point-max) (point-max))))
          ((and (eql (overlay-start conn--mark-cursor) (mark t))
                (or (eql (overlay-end conn--mark-cursor) (1+ (mark t)))
                    (and (eql (overlay-start conn--mark-cursor) (point-max))
                         (overlay-get conn--mark-cursor 'after-string)))))
          ((= (mark t) (point-max))
           (move-overlay conn--mark-cursor (point-max) (point-max))
           (overlay-put conn--mark-cursor 'after-string
                        (get 'conn--mark-cursor 'overlay-after-string)))
          ((and (eql (char-after (mark t)) ?\t)
                (< 1 (save-excursion
                       (goto-char (mark t))
                       (let ((col (current-column)))
                         (- (indent-next-tab-stop col) col)))))
           (move-overlay conn--mark-cursor (mark t) (mark t))
           (overlay-put conn--mark-cursor 'after-string
                        (get 'conn--mark-cursor 'overlay-after-string)))
          (t
           (move-overlay conn--mark-cursor (mark t) (1+ (mark t)))
           (overlay-put conn--mark-cursor 'after-string nil)))))

(defun conn--push-ephemeral-mark (&optional location msg activate)
  "Push a mark at LOCATION that will not be added to `mark-ring'.

For the meaning of MSG and ACTIVATE see `push-mark'."
  (if (not conn-local-mode)
      (push-mark location (not msg) activate)
    (push-mark location (not msg) activate)
    (setq conn--ephemeral-mark t)
    nil))

(defvar conn--movement-ring-rotating nil)
(defvar conn--movement-tick nil)
(defvar conn--movement-mark nil)

(defun conn--mark-pre-command-hook ()
  (unless conn--disable-mark-cursor
    (set-marker conn-this-command-start (point))
    (setq conn--movement-tick (buffer-chars-modified-tick)
          conn--movement-mark (mark t)
          conn--movement-ring-rotating nil
          conn-this-command-thing nil
          conn-this-command-handler nil)))

(defun conn--mark-post-command-hook ()
  (unless conn--disable-mark-cursor
    (cl-callf2 assq-delete-all (recursion-depth) conn--last-perform-bounds)
    (unless conn-this-command-thing
      (setq conn-this-command-thing (or (conn-command-thing this-command)
                                        (conn-command-thing real-this-command))))
    (when (and conn-local-mode
               (marker-position conn-this-command-start)
               (eq (current-buffer) (marker-buffer conn-this-command-start)))
      (when-let* (((not (region-active-p)))
                  (handler
                   (or (conn-command-mark-handler this-command)
                       (conn-command-mark-handler real-this-command))))
        (with-demoted-errors "Error in Mark Handler: %s"
          (funcall handler conn-this-command-start)))
      (unless (or conn--movement-ring-rotating
                  mark-active
                  (null conn--movement-mark)
                  (not (eql conn--movement-tick (buffer-chars-modified-tick)))
                  (eql (mark t) conn--movement-mark))
        (with-demoted-errors "Error in Movement Ring: %s"
          (conn-push-movement-ring (point) (mark t)))))))

(defun conn--setup-mark ()
  (if conn-mode
      (progn
        (setq conn--prev-mark-even-if-inactive mark-even-if-inactive
              mark-even-if-inactive t)
        (add-hook 'pre-redisplay-functions 'conn--mark-cursor-redisplay 91)
        (add-hook 'pre-command-hook #'conn--mark-pre-command-hook)
        (add-hook 'post-command-hook #'conn--mark-post-command-hook))
    (setq mark-even-if-inactive conn--prev-mark-even-if-inactive)
    (remove-hook 'pre-redisplay-functions 'conn--mark-cursor-redisplay)
    (remove-hook 'pre-command-hook #'conn--mark-pre-command-hook)
    (remove-hook 'post-command-hook #'conn--mark-post-command-hook)))

;;;;; Mark Ring

(defvar-local conn-mark-ring nil
  "List of interesting former marks of the current buffer, most recent first.

Conn adds many uninteresting marks to the `mark-ring' and so to
ameliorate the problem implements this alternative mark ring which
filters out the uninteresting marks.  See also `conn-pop-mark-ring' and
`conn-unpop-mark-ring'.")

(defvar conn-mark-ring-max 40
  "Maximum length of `conn-mark-ring'.")

(defun conn-delete-mark-ring ()
  (when (conn-ring-p conn-mark-ring)
    (mapc (conn-ring-cleanup conn-mark-ring)
          (conn-ring-list conn-mark-ring))
    (setf conn-mark-ring nil)))

(defun conn--push-mark-ring (location &optional back)
  (when (not conn-mark-ring)
    (setq conn-mark-ring
          (conn-make-ring conn-mark-ring-max
                          :cleanup (lambda (mk) (set-marker mk nil))
                          :copier (lambda (mk)
                                    (copy-marker (marker-position mk))))))
  (pcase-let ((ptb (conn-ring-tail conn-mark-ring))
              (ptf (conn-ring-head conn-mark-ring)))
    (cond
     ((and ptf (= location ptf))
      (when back (conn-ring-rotate-forward conn-mark-ring)))
     ((and ptb (= location ptb))
      (unless back (conn-ring-rotate-backward conn-mark-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-mark-ring
                                 (conn--create-marker location))
        (conn-ring-insert-front conn-mark-ring
                                (conn--create-marker location)))))))

;;;;; Movement Ring

(defvar-local conn-movement-ring nil
  "List of previous regions, most recent first.

See also `conn-pop-movement-ring' and `conn-unpop-movement-ring'.")

(defvar conn-movement-ring-max 10
  "Maximum length of `conn-movement-ring'.")

(defun conn-push-movement-ring (point mark &optional back)
  (unless (conn-ring-p conn-movement-ring)
    (setq conn-movement-ring
          (conn-make-ring conn-movement-ring-max
                          :cleanup (pcase-lambda (`(,pt . ,mk))
                                     (set-marker pt nil)
                                     (set-marker mk nil))
                          :copier (pcase-lambda (`(,pt . ,mk))
                                    (cons (copy-marker (marker-position pt))
                                          (copy-marker (marker-position mk)))))))
  (pcase-let ((`(,ptf . ,mkf) (conn-ring-head conn-movement-ring))
              (`(,ptb . ,mkb) (conn-ring-tail conn-movement-ring)))
    (cond
     ((and ptf (= point ptf) (= mark mkf))
      (when back (conn-ring-rotate-backward conn-movement-ring)))
     ((and ptb (= point ptb) (= mark mkb))
      (unless back (conn-ring-rotate-forward conn-movement-ring)))
     (t
      (if back
          (conn-ring-insert-back conn-movement-ring
                                 ;; TODO: Think through the marker
                                 ;; insertion type
                                 (cons (conn--create-marker point nil t)
                                       (conn--create-marker mark)))
        (conn-ring-insert-front conn-movement-ring
                                (cons (conn--create-marker point nil t)
                                      (conn--create-marker mark))))))))

;;;###autoload
(defun conn-unpop-movement-ring (arg)
  "Rotate backward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-pop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-movement-ring (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-backward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

;;;###autoload
(defun conn-pop-movement-ring (arg)
  "Rotate forward through `conn-movement-ring'."
  (interactive "p")
  (setq conn--movement-ring-rotating t)
  (cond ((< arg 0)
         (conn-unpop-movement-ring (abs arg)))
        ((null conn-movement-ring)
         (message "Movement ring empty"))
        (t
         (conn-push-movement-ring (point) (mark t))
         (dotimes (_ (mod arg (conn-ring-capacity conn-movement-ring)))
           (conn-ring-rotate-forward conn-movement-ring))
         (pcase (conn-ring-head conn-movement-ring)
           (`(,pt . ,mk)
            (goto-char pt)
            (conn--push-ephemeral-mark mk))))))

(provide 'conn-mark)
