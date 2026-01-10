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

;;; Code

(require 'compat)
(require 'conn-things)
(require 'conn-states)
(require 'conn-dispatch)
(require 'conn-expand)
(require 'conn-commands)
(eval-when-compile
  (require 'cl-lib))

;;;;; Bounds

(cl-defstruct (conn-self-insert-event
               (:constructor conn--self-insert (id)))
  (id -1 :type integer))

(keymap-set (conn-get-state-map 'conn-read-thing-state)
            "g" 'conn-surround)

(put 'conn-surround :conn-thing t)
(put 'surround-self-insert :conn-thing t)

(conn-define-state conn-surround-with-state (conn-mode-line-face-state)
  :lighter "WITH"
  :mode-line-face 'conn-read-surround-with-mode-line-face
  :full-keymap t)

(let ((map (conn-get-state-map 'conn-surround-with-state)))
  (set-char-table-range (nth 1 map)
                        (cons #x100 (max-char))
                        'surround-self-insert)
  (cl-loop for i from ?! below 256
           do (define-key map (vector i) 'surround-self-insert)))

(define-keymap
  :keymap (conn-get-state-map 'conn-surround-with-state)
  :suppress t
  "SPC <t>" 'surround-self-insert
  "DEL" 'backward-delete-arg
  "<backspace>" 'backward-delete-arg
  "M-DEL" 'reset-arg
  "M-<backspace>" 'reset-arg)

(conn-define-state conn-surrounding-state (conn-surround-with-state)
  :lighter "SURROUNDING")

(cl-defstruct (conn-surround-property-argument
               (:include conn-argument)
               ( :constructor conn-surround-property-argument
                 (&optional
                  value
                  &aux
                  (keymap (define-keymap
                            "w" :whole
                            "e" :inner))))))

(cl-defmethod conn-argument-update ((arg conn-surround-property-argument)
                                    cmd updater)
  (when (memq cmd '(:whole :inner))
    (setf (conn-argument-value arg) cmd)
    (funcall updater arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-property-argument)
                                       sym)
  (or (eq sym :whole)
      (eq sym :inner)))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-surround))
                              arg)
  (conn-read-args (conn-surround-with-state
                   :prompt "Surround"
                   :prefix arg)
      ((`(,thing ,arg) (conn-surround-with-argument))
       (property (conn-surround-property-argument)))
    (let ((bounds (conn-bounds-of thing arg)))
      (if (eq property :whole)
          bounds
        (conn-bounds-get bounds property)))))

(cl-defmethod conn-bounds-of ((cmd conn-self-insert-event)
                              arg)
  (catch 'return
    (save-mark-and-excursion
      (pcase-let* (((cl-struct conn-self-insert-event id)
                    cmd)
                   ((or `(,_cmd ,open ,close)
                        `(,open ,close))
                    (or (assoc id insert-pair-alist)
                        (list id id)))
                   (n (prefix-numeric-value arg)))
        (push-mark nil t t)
        (condition-case _err
            (while t
              (conn-expand-subr 1)
              (let ((beg (region-beginning))
                    (end (region-end)))
                (when (and (eql open (char-after beg))
                           (eql close (char-before end))
                           (> (- end beg) 1)
                           (>= 0 (cl-decf n)))
                  (throw 'return
                         (conn-make-bounds
                          'conn-surround arg (cons beg end)
                          :open (conn-make-bounds
                                 'region nil
                                 (cons beg (1+ beg)))
                          :close (conn-make-bounds
                                  'region nil
                                  (cons (1- end) end))
                          :inner (conn-make-bounds
                                  'region nil
                                  (cons (1+ beg) (1- end))))))))
          (user-error nil))))))

;;;;; Delete

(cl-defgeneric conn-delete-surround (cmd arg transform))

(cl-defmethod conn-delete-surround (cmd arg transform)
  (pcase-exhaustive (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :open nil (conn-bounds `(,obeg . ,_oend)))
          (conn-bounds-get :close nil (conn-bounds `(,_cbeg . ,cend)))
          (conn-bounds-get :inner nil (conn-bounds `(,ibeg . ,iend) transform)))
     (delete-region iend cend)
     (delete-region obeg ibeg))))

(cl-defmethod conn-kill-thing-do ((_cmd (eql conn-surround))
                                  arg
                                  transform
                                  &optional
                                  _append
                                  _delete
                                  _register
                                  _separator
                                  _reformat
                                  _check-bounds)
  (conn-read-args (conn-surround-with-state
                   :prompt "Surrounding"
                   :prefix arg)
      ((`(,thing ,arg) (conn-surround-with-argument))
       (transform (conn-transform-argument transform)))
    (conn-delete-surround thing arg transform)))

;;;;; Surround

(defface conn-read-surround-with-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(conn-define-state conn-surround-thing-state (conn-read-thing-state)
  :lighter "SURROUND")

(put 'conn-surround-overlay 'face 'region)
(put 'conn-surround-overlay 'priority 2000)
(put 'conn-surround-overlay 'conn-overlay t)

(defun conn-surround-create-regions (regions)
  (cl-loop for (beg . end) in regions
           for ov = (make-overlay beg end)
           do (overlay-put ov 'category 'conn-surround-overlay)
           collect ov))

;;;;;; Surround With arg

(cl-defstruct (conn-surround-with-argument
               (:include conn-argument)
               ( :constructor conn-surround-with-argument
                 (&optional
                  adjustable
                  &aux
                  (required t)
                  (annotation "surround with"))))
  (adjustable nil :type boolean :read-only t))

(cl-defmethod conn-argument-update ((arg conn-surround-with-argument)
                                    cmd
                                    updater)
  (when (conn-argument-predicate arg cmd)
    (setf (conn-argument-set-flag arg) t
          (conn-argument-value arg)
          (list (conn-handle-surround-with-argument cmd)
                (conn-read-args-consume-prefix-arg)))
    (funcall updater arg)))

(cl-defgeneric conn-handle-surround-with-argument (cmd)
  ( :method (cmd) cmd))

(cl-defmethod conn-handle-surround-with-argument ((_cmd (eql surround-self-insert)))
  (conn--self-insert last-input-event))

(cl-defmethod conn-argument-predicate ((arg conn-surround-with-argument)
                                       sym)
  (or (memq sym '(surround-self-insert))
      (and (conn-surround-with-argument-adjustable arg)
           (memq sym '(conn-adjust-surround conn-adjust-surround-other-end)))))

(cl-defmethod conn-argument-extract-value ((arg conn-surround-with-argument))
  (conn-argument-value arg))

;;;;;; Padding Arg

(defvar-keymap conn-surround-padding-argument-map
  "TAB" 'conn-padding-flag)

(defun conn-surround-padding-argument ()
  (conn-read-argument
   "padding"
   'conn-padding-flag
   conn-surround-padding-argument-map
   (lambda (val)
     (unless val
       (if (conn-read-args-consume-prefix-arg)
           (read-string "Padding: ")
         " ")))
   :formatter (lambda (name val)
                (concat
                 name
                 (when val
                   (concat
                    " "
                    (propertize (format "<%s>" val)
                                'face 'conn-argument-active-face)))))))

;;;;;; Perform Surround

(cl-defgeneric conn-surround-do (with arg &key &allow-other-keys))

(cl-defmethod conn-surround-do :around (_with _arg &key regions &allow-other-keys)
  (dolist (ov regions)
    (goto-char (overlay-start ov))
    (push-mark (overlay-end ov) t t)
    (cl-call-next-method)))

(cl-defmethod conn-surround-do :before (&rest _)
  ;; Normalize point and mark
  (unless (= (point) (region-beginning))
    (exchange-point-and-mark)))

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

(cl-defmethod conn-surround-do ((with conn-self-insert-event)
                                arg
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
  (:method ((_ (eql nil)) &rest _) nil))

(cl-defmethod conn-prepare-surround (cmd
                                     arg
                                     transform
                                     &key
                                     subregions
                                     &allow-other-keys)
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
      ((`(,thing ,arg) (if (use-region-p)
                           (list 'region nil)
                         (conn-thing-argument t)))
       (transform (conn-transform-argument '(conn-bounds-trim)))
       (subregions (conn-subregions-argument
                    (use-region-p))))
    (atomic-change-group
      (save-mark-and-excursion
        (pcase-let* ((`(,regions . ,prep-keys)
                      (conn-prepare-surround thing arg transform
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
                     ',(cons (list thing arg transform
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

(defun conn--adjust-surround-edit-message ()
  (message
   (substitute-command-keys
    (concat
     "Adjusting: Press \\[conn-pop-state] to finish, "
     "\\[conn-change-thing] to change surround"))))

(define-minor-mode conn-adjust-surround-mode
  "Mode for adjusting surround."
  :group 'conn
  :global t
  (if conn-adjust-surround-mode
      (progn
        (setq conn--eldoc-prev-msg-fn eldoc-message-function
              eldoc-message-function #'ignore)
        (conn--adjust-surround-edit-message)
        (add-hook 'pre-command-hook #'conn--adjust-surround-edit-message))
    (remove-hook 'pre-command-hook #'conn--adjust-surround-edit-message)
    (setq eldoc-message-function conn--eldoc-prev-msg-fn
          conn--eldoc-prev-msg-fn nil)
    (message nil)))

(keymap-set (conn-get-minor-mode-map 'conn-mark-state 'conn-adjust-surround-mode)
            "e" 'exit-recursive-edit)

(cl-defun conn--adjust-surround-subr (other-end
                                      &rest
                                      keys
                                      &key
                                      regions
                                      keymap
                                      trim
                                      open
                                      close
                                      &allow-other-keys)
  (mapc #'delete-overlay regions)
  (save-mark-and-excursion
    (conn-with-recursive-stack (conn-buffer-base-state)
      (conn-push-state 'conn-mark-state)
      (when other-end (conn-exchange-mark-command))
      (let ((adjust t)
            with arg)
        (conn-with-overriding-map
            (define-keymap
              "<remap> <conn-change-thing>"
              (lambda ()
                (interactive)
                (conn-read-args (conn-surround-with-state
                                 :prompt "Surround With"
                                 :overriding-map keymap
                                 :prefix arg)
                    ((next-with (conn-surround-with-argument)))
                  (pcase-setq `(,with ,arg) next-with
                              adjust nil)
                  (exit-recursive-edit)))
              "<remap> <conn-pop-state>" #'exit-recursive-edit)
          (let ((buffer-read-only t))
            (unwind-protect
                (progn
                  (conn-adjust-surround-mode 1)
                  (recursive-edit))
              (conn-adjust-surround-mode -1)))
          (if adjust
              (let ((beg (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (when trim (skip-chars-backward trim))
                (insert close)
                (goto-char beg)
                (when trim (skip-chars-forward trim))
                (insert open))
            (let ((ov (make-overlay (region-beginning) (region-end))))
              (unwind-protect
                  (apply #'conn-surround-do
                         `( ,with ,arg
                            :regions ,(list ov)
                            ,@keys))
                (delete-overlay ov)))))))))

(cl-defmethod conn-surround-do ((_with (eql conn-adjust-surround))
                                _arg
                                &rest
                                keys)
  (apply #'conn--adjust-surround-subr nil keys))

(cl-defmethod conn-surround-do ((_with (eql conn-adjust-surround-other-end))
                                _arg
                                &rest
                                keys)
  (apply #'conn--adjust-surround-subr t keys))

;;;;;; Change Surround

(conn-define-state conn-change-surround-state (conn-surround-with-state)
  :lighter "CHG-SURROUND")

(define-keymap
  :keymap (conn-get-state-map 'conn-change-state)
  "g" 'conn-surround)

(cl-defmethod conn-handle-change-argument ((cmd (eql conn-surround))
                                           arg)
  (conn-set-argument
   arg (list cmd (conn-read-args-consume-prefix-arg))))

(cl-defstruct (conn-change-surround-argument
               (:include conn-argument)
               ( :constructor conn-change-surround-argument
                 (&aux
                  (required t)
                  (annotation "change surround")))))

(cl-defmethod conn-argument-update ((arg conn-change-surround-argument)
                                    cmd updater)
  (when (conn-argument-predicate arg cmd)
    (setf (conn-argument-set-flag arg) t
          (conn-argument-value arg)
          (list (conn--self-insert last-input-event)
                (conn-read-args-consume-prefix-arg)))
    (funcall updater arg)))

(cl-defmethod conn-argument-predicate ((_arg conn-change-surround-argument)
                                       (_sym (eql surround-self-insert)))
  t)

(cl-defgeneric conn-prepare-change-surround (cmd arg transform)
  (declare (conn-anonymous-thing-property :prepare-change-surround-op)
           (important-return-value t)))

(cl-defmethod conn-prepare-change-surround (cmd arg transform)
  (pcase (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :open nil (conn-bounds `(,obeg . ,oend)))
          (conn-bounds-get :close nil (conn-bounds `(,cbeg . ,cend)))
          (conn-bounds-get :inner nil (conn-bounds `(,ibeg . ,iend) transform)))
     (prog1 (list (conn--make-surround-region ibeg iend)
                  :open (buffer-substring-no-properties obeg oend)
                  :close (buffer-substring-no-properties cbeg cend))
       (delete-region iend cend)
       (delete-region obeg ibeg)))))

(cl-defmethod conn-change-thing-do ((_cmd (eql conn-surround))
                                    arg
                                    transform
                                    &optional
                                    _kill)
  (with-undo-amalgamate
    (atomic-change-group
      (save-mark-and-excursion
        (conn-read-args (conn-change-surround-state
                         :prompt "Change Surrounding"
                         :prefix arg)
            ((`(,thing ,arg) (conn-change-surround-argument))
             (transform (conn-transform-argument transform)))
          (pcase-let* ((`(,ov . ,prep-keys)
                        (conn-prepare-change-surround thing arg transform))
                       (cleanup (plist-get prep-keys :cleanup))
                       (success nil))
            (unwind-protect
                (conn-read-args (conn-surround-with-state
                                 :prompt "Surround With"
                                 :overriding-map (plist-get prep-keys :keymap))
                    ((`(,with ,with-arg) (conn-surround-with-argument t))
                     (padding (conn-surround-padding-argument)))
                  (apply #'conn-surround-do
                         `( ,with ,with-arg
                            :regions ,(list ov)
                            ,@prep-keys
                            :padding ,padding))
                  (add-to-history
                   'command-history
                   `(conn-previous-change-surround
                     ',(cons (list thing arg)
                             (list with with-arg :padding padding))))
                  (setq success t))
              (delete-overlay ov)
              (when cleanup
                (funcall cleanup (if success :accept :cancel))))))))))

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

;;;; Adjust Surround

(defvar-keymap conn-surround-trim-argument-map
  "q" 'trim)

(defun conn-adjust-surround ()
  (interactive)
  (conn-read-args (conn-change-surround-state
                   :prompt "Adjust Surrounding")
      ((`(,thing ,arg) (conn-change-surround-argument))
       (transform (conn-transform-argument))
       (at-end (conn-boolean-argument "At End"
                                      'other-end
                                      conn-other-end-argument-map
                                      t))
       (trim (conn-read-argument
              "trim whitespace"
              'trim
              conn-surround-trim-argument-map
              (lambda (_) (read-string "Whitespace Chars: "))
              :value "\n\t ")))
    (with-undo-amalgamate
      (atomic-change-group
        (pcase (conn-prepare-change-surround thing arg transform)
          (`(,ov . ,prep-keys)
           (let ((cleanup (plist-get prep-keys :cleanup))
                 (success nil))
             (unwind-protect
                 (progn
                   (apply #'conn-surround-do
                          `(,(if at-end
                                 'conn-adjust-surround-other-end
                               'conn-adjust-surround)
                            nil
                            :regions ,(list ov)
                            :trim ,trim
                            ,@prep-keys)))
               (delete-overlay ov)
               (when cleanup
                 (funcall cleanup (if success :accept :cancel))))))
          (_ (user-error "No surround found")))))))

(provide 'conn-surround)
