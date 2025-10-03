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

(keymap-set (conn-get-state-map 'conn-read-thing-state)
            "g" 'conn-surround)

(put 'conn-surround :conn-thing t)
(put 'surround-self-insert :conn-thing t)

(conn-define-state conn-surround-with-state (conn-mode-line-face-state)
  :lighter "WITH"
  :mode-line-face 'conn-read-surround-with-mode-line-face)

(conn-define-state conn-surrounding-state (conn-surround-with-state)
  :lighter "SURROUNDING")

(oclosure-define (conn-surround-property-argument
                  (:parent conn-state-eval-argument)))

(defun conn-surround-property-argument (&optional value)
  (oclosure-lambda (conn-surround-property-argument
                    (value (or value :whole))
                    (keymap (define-keymap
                              "w" :whole
                              "e" :inner)))
      (self cmd)
    (if (memq cmd '(:whole :inner))
        (conn-set-argument self cmd)
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-property-argument)
                                       sym)
  (or (eq sym :whole)
      (eq sym :inner)))

(cl-defmethod conn-bounds-of ((_cmd (eql conn-surround)) arg)
  (pcase-let* ((`(,bounds ,property)
                (conn-eval-with-state 'conn-surround-with-state
                    (list (conn-bounds-of && (conn-surround-with-argument))
                          & (conn-surround-property-argument))
                  :prompt "Surround"
                  :prefix arg)))
    (if (eq property :whole)
        bounds
      (conn-bounds-get bounds property))))

(cl-defmethod conn-bounds-of ((_cmd (eql surround-self-insert)) arg)
  (catch 'return
    (save-mark-and-excursion
      (pcase-let* (((or `(,_cmd ,open ,close)
                        `(,open ,close))
                    (or (assoc last-input-event insert-pair-alist)
                        (list last-input-event last-input-event)))
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

(cl-defmethod conn-perform-kill ((_cmd (eql conn-surround))
                                 arg _transform &optional _append _delete _register)
  (conn-eval-with-state 'conn-surround-with-state
      (conn-delete-surround && (conn-surround-with-argument))
    :prompt "Surrounding"
    :prefix arg))

;;;;; Surround

(defface conn-read-surround-with-mode-line-face
  '((t (:inherit mode-line :inverse-video t)))
  "Face for mode-line in a read-thing state."
  :group 'conn-faces)

(conn-define-state conn-surround-thing-state (conn-read-thing-state)
  :lighter "SURROUND")

(define-keymap
  :keymap (conn-get-state-map 'conn-surround-with-state)
  "0" 'digit-argument
  "1" 'digit-argument
  "2" 'digit-argument
  "3" 'digit-argument
  "4" 'digit-argument
  "5" 'digit-argument
  "6" 'digit-argument
  "7" 'digit-argument
  "8" 'digit-argument
  "9" 'digit-argument
  "r" 'conn-read-pair
  "c" 'surround-comment
  ";" 'surround-comment
  "C" 'surround-uncomment
  "<remap> <self-insert-command>" 'surround-self-insert
  "SPC <t>" 'surround-self-insert
  "RET" 'conn-padding-flag
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

(oclosure-define (conn-surround-with-argument
                  (:parent conn-state-eval-argument)))

(defun conn-surround-with-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-surround-with-argument
                    (required t))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (conn-state-eval-consume-prefix-arg))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-with-argument)
                                       sym)
  (memq sym '(surround-self-insert surround-command)))

(cl-defmethod conn-eval-argument ((arg conn-surround-with-argument))
  (conn-state-eval-argument-value arg))

;;;;;; Padding Arg

(oclosure-define (conn-surround-padding-argument
                  (:parent conn-state-eval-argument)))

(defun conn-surround-padding-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-surround-padding-argument)
      (self cmd)
    (if (eq cmd 'conn-padding-flag)
        (conn-set-argument
         self (unless value
                (if (conn-state-eval-consume-prefix-arg)
                    (read-string "Padding: ")
                  " ")))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-surround-padding-argument)
                                       (_sym (eql conn-padding-flag)))
  t)

(cl-defmethod conn-display-argument ((arg conn-surround-padding-argument))
  (concat "\\[conn-padding-flag] "
          (if-let* ((p (conn-state-eval-argument-value arg)))
              (propertize (format "padding <%s>" p)
                          'face 'eldoc-highlight-function-argument)
            "padding")))

;;;;;; Perform Surround

(cl-defgeneric conn-perform-surround (with arg &key &allow-other-keys))

(cl-defmethod conn-perform-surround :around (_with _arg &key regions &allow-other-keys)
  (dolist (ov regions)
    (goto-char (overlay-start ov))
    (conn--push-ephemeral-mark (overlay-end ov) nil t)
    (cl-call-next-method)))

(cl-defmethod conn-perform-surround :before (_with _arg &key &allow-other-keys)
  ;; Normalize point and mark
  (unless (= (point) (region-beginning))
    (exchange-point-and-mark)))

(cl-defmethod conn-perform-surround ((_with (eql surround-comment)) arg
                                     &key &allow-other-keys)
  (comment-or-uncomment-region (region-beginning) (region-end) arg))

(cl-defmethod conn-perform-surround ((_with (eql surround-uncomment)) arg
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

(cl-defmethod conn-perform-surround ((_with (eql surround-self-insert)) arg
                                     &key padding &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (assoc last-input-event insert-pair-alist)
   padding arg))

(defun conn--make-surround-region (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'category 'conn-surround-overlay)
    ov))

(cl-defgeneric conn-prepare-surround (cmd arg transform &key &allow-other-keys)
  (declare (conn-anonymous-thing-property :prepare-surround-op)
           (important-return-value t))
  ( :method ((_ (eql nil)) _ _ &rest _ &key &allow-other-keys) nil)
  ( :method ((cmd (conn-thing anonymous-thing-override))
             arg transform &rest keys &key &allow-other-keys)
    (if-let* ((op (conn-anonymous-thing-property cmd :prepare-surround-op)))
        (apply op arg transform keys)
      (cl-call-next-method))))

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

;;;###autoload
(defun conn-surround (&optional arg)
  (interactive "P")
  (atomic-change-group
    (save-mark-and-excursion
      (pcase-let* ((`(,regions . ,prep-keys)
                    (conn-eval-with-state 'conn-surround-thing-state
                        (conn-prepare-surround
                         && (conn-thing-argument-dwim t)
                         & (conn-transform-argument)
                         :subregions & (conn-subregions-argument
                                        (use-region-p)))
                      :prompt "Surround"
                      :prefix arg))
                   (cleanup (plist-get prep-keys :cleanup))
                   (success nil))
        (when regions
          (goto-char (overlay-start (car regions))))
        (unwind-protect
            (pcase-let ((`(,with ,with-arg . ,with-keys)
                         (conn-with-overriding-map (plist-get prep-keys :keymap)
                           (conn-eval-with-state 'conn-surround-with-state
                               (list && (conn-surround-with-argument)
                                     :padding & (conn-surround-padding-argument))
                             :prompt "Surround With"))))
              (apply #'conn-perform-surround
                     `(,with ,with-arg :regions ,regions ,@prep-keys ,@with-keys))
              (setq success t))
          (mapc #'delete-overlay regions)
          (when cleanup
            (funcall cleanup (if success :accept :cancel))))))))

;;;;;; Surround Read Pair

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
          (conn-with-dispatch-event-handler 'backspace
              (define-keymap
                "<remap> <backward-delete-char>" 'backspace)
              (lambda (cmd)
                (when (eq cmd 'backspace)
                  (when (length> so-far 0)
                    (cl-callf substring so-far 0 -1)
                    (setq narrowed collection)
                    (throw 'backspace nil))))
            (cl-callf thread-last
                so-far
              (conn-dispatch-read-event prompt t nil)
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

(defvar conn--surround-current-pair nil)

(cl-defmethod conn-perform-surround ((_with (eql conn-read-pair)) arg
                                     &key padding &allow-other-keys)
  (conn--perform-surround-with-pair-subr
   (with-memoization conn--surround-current-pair
     (let ((open (funcall conn-read-pair-function
                          (cl-loop for (open . _) in insert-pair-alist
                                   collect (pcase open
                                             ((pred stringp) open)
                                             (_ (string open)))))))
       (or (assoc open insert-pair-alist)
           (assq (aref open 0) insert-pair-alist))))
   padding
   arg))

(cl-defmethod conn-perform-surround :around ((_with (eql conn-read-pair))
                                             _arg &keys &allow-other-keys)
  (let ((conn--surround-current-pair nil))
    (cl-call-next-method)))

;;;;;; Change Surround

(define-error 'conn-no-surround "No surround at point" 'user-error)

(conn-define-state conn-change-surround-state (conn-surround-with-state)
  :lighter "CHG-SURROUND")

(keymap-set (conn-get-state-map 'conn-change-state) "g" 'conn-surround)

(cl-defmethod conn-handle-change-argument ((cmd (eql conn-surround))
                                           arg)
  (conn-set-argument
   arg (list cmd (conn-state-eval-consume-prefix-arg))))

(oclosure-define (conn-change-surround-argument
                  (:parent conn-state-eval-argument)))

(defun conn-change-surround-argument ()
  (declare (important-return-value t))
  (oclosure-lambda (conn-change-surround-argument
                    (required t))
      (self cmd)
    (if (conn-argument-predicate self cmd)
        (conn-set-argument
         self (list cmd (conn-state-eval-consume-prefix-arg)))
      self)))

(cl-defmethod conn-argument-predicate ((_arg conn-change-surround-argument)
                                       (_sym (eql surround-self-insert)))
  t)

(cl-defgeneric conn-prepare-change-surround (cmd arg)
  (declare (conn-anonymous-thing-property :prepare-change-surround-op)
           (important-return-value t))
  ( :method ((cmd (conn-thing anonymous-thing-override)) arg)
    (if-let* ((op (conn-anonymous-thing-property cmd :prepare-change-surround-op)))
        (funcall op arg)
      (cl-call-next-method))))

(cl-defmethod conn-prepare-change-surround (cmd arg)
  (pcase (conn-bounds-of cmd arg)
    ((and (conn-bounds-get :open nil (conn-bounds `(,obeg . ,oend)))
          (conn-bounds-get :close nil (conn-bounds `(,cbeg . ,cend)))
          (conn-bounds-get :inner nil (conn-bounds `(,ibeg . ,iend))))
     (prog1 (list (conn--make-surround-region ibeg iend))
       (delete-region cbeg cend)
       (delete-region obeg oend)))))

(cl-defmethod conn-perform-change ((_cmd (eql conn-surround))
                                   _arg _transform
                                   &optional _kill)
  (save-mark-and-excursion
    (atomic-change-group
      (pcase-let* ((`(,ov . ,prep-keys)
                    (conn-eval-with-state 'conn-change-surround-state
                        (conn-prepare-change-surround
                         && (conn-change-surround-argument))
                      :prompt "Change Surrounding"))
                   (cleanup (plist-get prep-keys :cleanup))
                   (success nil))
        (unwind-protect
            (pcase-let ((`(,with ,with-arg . ,with-keys)
                         (conn-with-overriding-map (plist-get prep-keys :keymap)
                           (conn-eval-with-state 'conn-surround-with-state
                               (list && (conn-surround-with-argument)
                                     :padding & (conn-surround-padding-argument))
                             :prompt "Surround With"))))
              (apply #'conn-perform-surround
                     `(,with ,with-arg :regions ,(list ov) ,@prep-keys ,@with-keys))
              (setq success t))
          (delete-overlay ov)
          (when cleanup
            (funcall cleanup (if success :accept :cancel))))))))

(provide 'conn-surround)
