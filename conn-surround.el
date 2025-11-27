;;; conn-surround.el -*- lexical-binding: t -*-
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

;; A system similar to vim's surround

;;; Code

(require 'compat)
(require 'conn-things)
(require 'conn-states)
(require 'conn-dispatch)
(require 'conn-mark)
(require 'conn-expand)
(eval-when-compile
  (require 'cl-lib))

;;;;; Bounds

(cl-defstruct (conn-self-insert-event
               (:constructor conn--self-insert (id)))
  id)

(keymap-set (conn-get-state-map 'conn-read-thing-state)
            "g" 'conn-surround)

(put 'conn-surround :conn-thing t)
(put 'surround-self-insert :conn-thing t)

(conn-define-state conn-surround-with-state (conn-mode-line-face-state)
  :lighter "WITH"
  :mode-line-face 'conn-read-surround-with-mode-line-face)

(conn-define-state conn-surrounding-state (conn-surround-with-state)
  :lighter "SURROUNDING")

(cl-defstruct (conn-surround-property-argument
               (:include conn-argument)
               (:constructor
                conn-surround-property-argument
                ( &optional
                  value
                  &aux
                  (keymap (define-keymap
                            "w" :whole
                            "e" :inner))))))

(cl-defmethod conn-argument-update ((arg conn-surround-property-argument)
                                    cmd update-fn)
  (when (memq cmd '(:whole :inner))
    (setf (conn-argument-value arg) cmd)
    (funcall update-fn arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-property-argument)
                                       sym)
  (or (eq sym :whole)
      (eq sym :inner)))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-surround)) arg)
  (conn-read-args (conn-surround-with-state
                   :prompt "Surround"
                   :prefix arg)
      ((`(,thing ,thing-arg) (conn-surround-with-argument))
       (property (conn-surround-property-argument)))
    (let ((bounds (conn-bounds-of thing thing-arg)))
      (if (eq property :whole)
          bounds
        (conn-bounds-get bounds property)))))

(cl-defmethod conn-bounds-of ((cmd conn-self-insert-event) arg)
  (catch 'return
    (save-mark-and-excursion
      (pcase-let* (((cl-struct conn-self-insert-event id)
                    cmd)
                   ((or `(,_cmd ,open ,close)
                        `(,open ,close))
                    (or (assoc id insert-pair-alist)
                        (list id id)))
                   (n (prefix-numeric-value arg)))
        (conn--push-ephemeral-mark)
        (pcase-dolist (`(,beg . ,end)
                       (seq-drop-while (pcase-lambda (`(,beg . ,end))
                                         (or (>= beg (region-beginning))
                                             (<= end (region-end))))
                                       (conn--expand-create-expansions)))
          (when (and (eql open (char-after beg))
                     (eql close (char-before end))
                     (> (- end beg) 1)
                     (>= 0 (cl-decf n)))
            (throw 'return
                   (conn-make-bounds
                    'conn-surround arg (cons beg end)
                    :open (conn-make-bounds 'region nil (cons beg (1+ beg)))
                    :close (conn-make-bounds 'region nil (cons (1- end) end))
                    :inner (conn-make-bounds 'region nil (cons (1+ beg) (1- end)))))))))
    (signal 'conn-no-surround nil)))

;;;;; Delete

(cl-defgeneric conn-delete-surround (cmd arg))

(cl-defmethod conn-delete-surround (cmd arg)
  (pcase-exhaustive (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :open nil (conn-bounds `(,obeg . ,oend)))
          (conn-bounds-get :close nil (conn-bounds `(,cbeg . ,cend))))
     (delete-region cbeg cend)
     (delete-region obeg oend))))

(cl-defmethod conn-kill-thing-do ((_cmd (eql conn-surround))
                                  arg
                                  _transform
                                  &optional
                                  _append
                                  _delete
                                  _register
                                  _fixup-whitespace
                                  _check-bounds)
  (conn-read-args (conn-surround-with-state
                   :prompt "Surrounding"
                   :prefix arg)
      ((`(,thing ,thing-arg) (conn-surround-with-argument)))
    (conn-delete-surround thing thing-arg)))

;;;;; Surround

(defface conn-read-surround-with-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(conn-define-state conn-surround-thing-state (conn-read-thing-state)
  :lighter "SURROUND")

(define-keymap
  :keymap (conn-get-state-map 'conn-surround-with-state)
  :suppress t
  "r" 'conn-read-pair
  "c" 'surround-comment
  ";" 'surround-comment
  "C" 'surround-uncomment
  "<remap> <self-insert-command>" 'surround-self-insert
  "SPC <t>" 'surround-self-insert
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(put 'conn-surround-overlay 'face 'region)
(put 'conn-surround-overlay 'priority (1+ conn-mark-overlay-priority))
(put 'conn-surround-overlay 'conn-overlay t)

(defun conn-surround-create-regions (regions)
  (cl-loop for (beg . end) in regions
           for ov = (make-overlay beg end)
           do (overlay-put ov 'category 'conn-surround-overlay)
           collect ov))

;;;;;; Surround With arg

(cl-defstruct (conn-surround-with-argument
               (:include conn-argument)
               (:constructor
                conn-surround-with-argument
                (&aux
                 (required t)
                 (annotation "surround with")))))

(cl-defmethod conn-argument-update ((arg conn-surround-with-argument)
                                    cmd update-fn)
  (when (conn-argument-predicate arg cmd)
    (setf (conn-argument-set-flag arg) t
          (conn-argument-value arg)
          (list (conn-handle-surround-with-argument cmd)
                (conn-read-args-consume-prefix-arg)))
    (funcall update-fn arg)))

(cl-defgeneric conn-handle-surround-with-argument (cmd)
  ( :method (cmd) cmd))

(cl-defmethod conn-handle-surround-with-argument ((_cmd (eql surround-self-insert)))
  (conn--self-insert last-input-event))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-with-argument)
                                       sym)
  (memq sym '(surround-self-insert surround-command)))

(cl-defmethod conn-argument-extract-value ((arg conn-surround-with-argument))
  (conn-argument-value arg))

;;;;;; Padding Arg

(defvar-keymap conn-surround-padding-map
  "TAB" 'conn-padding-flag)

(defun conn-surround-padding-argument ()
  (conn-read-argument
   "padding"
   'conn-padding-flag
   conn-surround-padding-map
   (lambda (val)
     (unless val
       (if (conn-read-args-consume-prefix-arg)
           (read-string "Padding: ")
         " ")))
   (lambda (val)
     (when val
       (propertize (format "<%s>" val)
                   'face 'conn-argument-active-face)))))

;;;;;; Perform Surround

(cl-defgeneric conn-surround-do (with arg &key &allow-other-keys))

(cl-defmethod conn-surround-do :around (_with _arg &key regions &allow-other-keys)
  (dolist (ov regions)
    (goto-char (overlay-start ov))
    (conn--push-ephemeral-mark (overlay-end ov) nil t)
    (cl-call-next-method)))

(cl-defmethod conn-surround-do :before (_with _arg &key &allow-other-keys)
  ;; Normalize point and mark
  (unless (= (point) (region-beginning))
    (exchange-point-and-mark)))

(cl-defmethod conn-surround-do ((_with (eql surround-comment)) arg
                                &key &allow-other-keys)
  (comment-or-uncomment-region (region-beginning) (region-end) arg))

(cl-defmethod conn-surround-do ((_with (eql surround-uncomment)) arg
                                &key &allow-other-keys)
  (uncomment-region (region-beginning) (region-end) arg))

(defun conn--perform-surround-with-pair-subr (pair padding arg)
  (cl-labels ((ins-pair (open &optional close)
                (insert open)
                (exchange-point-and-mark)
                (save-excursion (insert (or close open)))
                (exchange-point-and-mark)))
    (pcase pair
      ('nil
       (dotimes (_ (prefix-numeric-value arg))
         (ins-pair last-input-event)))
      ((or `(,_cmd ,open ,close)
           `(,open ,close))
       (dotimes (_ (prefix-numeric-value arg))
         (ins-pair open close))))
    (when padding (ins-pair padding))))

(cl-defmethod conn-surround-do ((with conn-self-insert-event) arg
                                &key
                                padding
                                &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (assoc (conn-self-insert-event-id with) insert-pair-alist)
   padding arg))

(defun conn--make-surround-region (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'category 'conn-surround-overlay)
    ov))

(cl-defgeneric conn-prepare-surround (cmd arg transform &key &allow-other-keys)
  (declare (conn-anonymous-thing-property :prepare-surround-op)
           (important-return-value t))
  ( :method ((_ (eql nil)) _ _ &rest _ &key &allow-other-keys) nil))

(cl-defmethod conn-prepare-surround (cmd arg transform &key subregions &allow-other-keys)
  (let (regions)
    (pcase (conn-bounds-of cmd arg)
      ((and (guard subregions)
            (conn-bounds-get :subregions
                             transform
                             (and sr (pred identity))))
       (pcase-dolist ((conn-bounds `(,beg . ,end)) sr)
         (push (conn--make-surround-region beg end) regions)))
      ((conn-bounds `(,beg . ,end) transform)
       (push (conn--make-surround-region beg end) regions)))
    (list (nreverse regions))))

(defun conn-surround (&optional arg)
  (interactive "P")
  (conn-read-args (conn-surround-thing-state
                   :prompt "Surround"
                   :prefix arg)
      ((`(,thing ,thing-arg) (conn-thing-argument-dwim t))
       (transform (conn-transform-argument))
       (subregions (conn-subregions-argument
                    (use-region-p))))
    (atomic-change-group
      (save-mark-and-excursion
        (pcase-let* ((`(,regions . ,prep-keys)
                      (conn-prepare-surround thing thing-arg transform
                                             :subregions subregions))
                     (cleanup (plist-get prep-keys :cleanup))
                     (success nil))
          (unwind-protect
              (progn
                (when regions
                  (goto-char (overlay-start (car regions))))
                (conn-read-args (conn-surround-with-state
                                 :prompt "Surround With"
                                 :overriding-map (plist-get prep-keys :keymap))
                    ((`(,with ,with-arg) (conn-surround-with-argument))
                     (padding (conn-surround-padding-argument)))
                  (apply #'conn-surround-do
                         `( ,with ,with-arg
                            :regions ,regions
                            ,@prep-keys
                            :padding ,padding))
                  (add-to-history
                   'command-history
                   `(conn-previous-surround
                     ',(cons (list thing thing-arg transform
                                   :subregions subregions)
                             (list with with-arg
                                   :padding padding))))
                  (setq success t)))
            (mapc #'delete-overlay regions)
            (when cleanup
              (funcall cleanup (if success :accept :cancel)))))))))

(defun conn-previous-surround (data)
  (pcase-let* ((`(,prep-args . (,with ,with-arg . ,with-keys))
                data)
               (`(,regions . ,prep-keys)
                (apply #'conn-prepare-surround prep-args))
               (cleanup (plist-get prep-keys :cleanup))
               (success nil))
    (when regions
      (goto-char (overlay-start (car regions))))
    (unwind-protect
        (progn
          (apply #'conn-surround-do
                 `(,with ,with-arg :regions ,regions ,@prep-keys ,@with-keys))
          (setq success t))
      (mapc #'delete-overlay regions)
      (when cleanup
        (funcall cleanup (if success :accept :cancel))))))

;;;;;; Surround Read Pair

(cl-defstruct (conn-surround-pair
               (:constructor conn--make-surround-pair (id)))
  id)

(defvar conn-read-pair-function 'conn-progressive-read-pair)

(defun conn-progressive-read-pair (collection)
  (let* ((collection (compat-call
                      sort (seq-uniq (mapcar #'copy-sequence collection))
                      :lessp (lambda (x y)
                               (or (< (length x) (length y))
                                   (and (= (length x) (length y))
                                        (string< x y))))
                      :in-place t))
         (narrowed collection)
         (prompt (propertize "Pair" 'face 'minibuffer-prompt))
         (so-far ""))
    (unwind-protect
        (while (not (length= narrowed 1))
          (with-current-buffer-window
              "*Conn Pairs*"
              `((display-buffer--maybe-same-window
                 display-buffer-reuse-window
                 display-buffer-below-selected)
                (window-height . completions--fit-window-to-buffer)
                ,(when temp-buffer-resize-mode
                   '(preserve-size . (nil . t)))
                (body-function
                 . ,#'(lambda (_window)
                        (insert (cl-loop for i from 0 below 10
                                         for item in narrowed
                                         concat (concat item "\n"))))))
              nil)
          (conn-with-dispatch-event-handlers
            ( :handler (cmd)
              (when (eq cmd 'backspace)
                (when (length> so-far 0)
                  (cl-callf substring so-far 0 -1)
                  (setq narrowed collection)
                  (:return))))
            (:keymap (define-keymap
                       "<remap> <backward-delete-char>" 'backspace))
            (cl-callf thread-last
                so-far
              (conn-dispatch-read-char prompt t nil)
              (char-to-string)
              (concat so-far)))
          (cl-loop for item in narrowed
                   when (string-prefix-p so-far item)
                   do
                   (remove-text-properties 0 (1- (length item)) '(face) item)
                   (add-text-properties 0 (length so-far)
                                        '(face completions-highlight)
                                        item)
                   and collect item into next
                   finally do (if (null next)
                                  (setq so-far (substring so-far 0 -1))
                                (setq narrowed next))))
      (delete-window (get-buffer-window "*Conn Pairs*" 0)))
    (let ((result (car narrowed)))
      (remove-text-properties 0 (1- (length result)) '(face) result)
      result)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-with-argument)
                                       (_sym (eql conn-read-pair)))
  t)

(cl-defmethod conn-handle-surround-with-argument ((_cmd (eql conn-read-pair)))
  (conn--make-surround-pair
   (funcall conn-read-pair-function
            (cl-loop for (open . _) in insert-pair-alist
                     collect (pcase open
                               ((pred stringp) open)
                               (_ (string open)))))))

(cl-defmethod conn-surround-do ((with conn-surround-pair) arg
                                &key
                                padding
                                &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (let ((open (conn-surround-pair-id with)))
     (or (assoc open insert-pair-alist)
         (assq (aref open 0) insert-pair-alist)))
   padding
   arg))

;;;;;; Change Surround

(define-error 'conn-no-surround "No surround at point" 'user-error)

(conn-define-state conn-change-surround-state (conn-surround-with-state)
  :lighter "CHG-SURROUND")

(keymap-set (conn-get-state-map 'conn-change-state) "g" 'conn-surround)

(cl-defmethod conn-handle-change-argument ((cmd (eql conn-surround))
                                           arg)
  (conn-set-argument
   arg (list cmd (conn-read-args-consume-prefix-arg))))

(cl-defstruct (conn-change-surround-argument
               (:include conn-argument)
               (:constructor
                conn-change-surround-argument
                (&aux
                 (required t)
                 (annotation "change surround")))))

(cl-defmethod conn-argument-update ((arg conn-change-surround-argument)
                                    cmd update-fn)
  (when (conn-argument-predicate arg cmd)
    (setf (conn-argument-set-flag arg) t
          (conn-argument-value arg)
          (list (conn--self-insert last-input-event)
                (conn-read-args-consume-prefix-arg)))
    (funcall update-fn arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-change-surround-argument)
                                       (_sym (eql surround-self-insert)))
  t)

(cl-defgeneric conn-prepare-change-surround (cmd arg)
  (declare (conn-anonymous-thing-property :prepare-change-surround-op)
           (important-return-value t)))

(cl-defmethod conn-prepare-change-surround (cmd arg)
  (pcase (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :open nil (conn-bounds `(,obeg . ,oend)))
          (conn-bounds-get :close nil (conn-bounds `(,cbeg . ,cend)))
          (conn-bounds-get :inner nil (conn-bounds `(,ibeg . ,iend))))
     (prog1 (list (conn--make-surround-region ibeg iend))
       (delete-region cbeg cend)
       (delete-region obeg oend)))))

(cl-defmethod conn-change-thing-do ((_cmd (eql conn-surround))
                                    _arg
                                    _transform
                                    &optional
                                    _kill)
  (atomic-change-group
    (save-mark-and-excursion
      (conn-read-args (conn-change-surround-state
                       :prompt "Change Surrounding")
          ((`(,thing ,thing-arg) (conn-change-surround-argument)))
        (pcase-let* ((`(,ov . ,prep-keys)
                      (conn-prepare-change-surround thing thing-arg))
                     (cleanup (plist-get prep-keys :cleanup))
                     (success nil))
          (unwind-protect
              (conn-read-args (conn-surround-with-state
                               :prompt "Surround With"
                               :overriding-map (plist-get prep-keys :keymap))
                  ((`(,with ,with-arg) (conn-surround-with-argument))
                   (padding (conn-surround-padding-argument)))
                (apply #'conn-surround-do
                       `(,with ,with-arg
                               :regions ,(list ov)
                               ,@prep-keys
                               :padding ,padding))
                (add-to-history
                 'command-history
                 `(conn-previous-change-surround
                   ',(cons (list thing thing-arg)
                           (list with with-arg :padding padding))))
                (setq success t))
            (delete-overlay ov)
            (when cleanup
              (funcall cleanup (if success :accept :cancel)))))))))

(defun conn-previous-change-surround (data)
  (save-mark-and-excursion
    (atomic-change-group
      (pcase-let* ((`(,prev-change . (,with ,with-arg . ,with-keys))
                    data)
                   (`(,ov . ,prep-keys)
                    (apply #'conn-prepare-change-surround prev-change))
                   (cleanup (plist-get prep-keys :cleanup))
                   (success nil))
        (unwind-protect
            (progn
              (apply #'conn-surround-do
                     `(,with ,with-arg :regions ,(list ov) ,@prep-keys ,@with-keys))
              (setq success t))
          (delete-overlay ov)
          (when cleanup
            (funcall cleanup (if success :accept :cancel))))))))

(provide 'conn-surround)
