;;; conn-kapply.el --- Keyboard macro system -*- lexical-binding: t -*-
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

(require 'conn-vars)
(require 'conn-utils)
(require 'conn-states)
(require 'conn-things)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn-exchange-mark-command "conn-commands")
(declare-function kmacro-step-edit-macro "kmacro")
(declare-function kmacro-p "kmacro")

;;;; Kapply

(defvar kmacro-step-edit-replace nil)

(defvar conn-kmacro-applying-p nil
  "Non-nil during kmacro application.")

(defvar conn-kmacro-apply-error nil
  "If non-nil contains the error encountered during macro application.")

(defvar conn-kmacro-apply-start-hook nil
  "Hook run before macro application begins.")

(defvar conn-kmacro-apply-end-hook nil
  "Hook run after macro application has completed.")

;;;;; Kapply Query

(defvar conn--kapply-automatic-flag nil)

(defun conn-kapply-kbd-macro-query (flag)
  "Query user during kbd macro execution.

With prefix argument FLAG, enter recursive edit, reading
keyboard commands even within a kbd macro.  You can give
different commands each time the macro executes.

Without prefix argument, ask whether to continue running the
macro.

Your options are: \\<query-replace-map>

\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that.
\\[automatic]   Apply keyboard macro to rest."
  (interactive "P")
  (or executing-kbd-macro
      defining-kbd-macro
      (user-error "Not defining or executing kbd macro"))
  (let ((msg (substitute-command-keys
              "Proceed with macro?\\<query-replace-map>\
 (\\[act] act, \\[skip] skip, \\[exit] exit, \\[recenter] recenter, \\[edit] edit, \\[automatic] auto)")))
    (cond
     (flag
      (let (executing-kbd-macro defining-kbd-macro)
        (recursive-edit)))
     ((not executing-kbd-macro))
     ((not conn--kapply-automatic-flag)
      (conn-named-loop _
        (pcase (let ((executing-kbd-macro nil)
                     (defining-kbd-macro nil))
                 (message "%s" msg)
                 (lookup-key query-replace-map (vector (read-event))))
          ('act (:return))
          ('skip
           (setq executing-kbd-macro "")
           (:return))
          ('exit
           (setq executing-kbd-macro t)
           (:return))
          ('recenter
           (recenter nil))
          ('edit
           (let (executing-kbd-macro defining-kbd-macro)
             (recursive-edit)))
          ('quit
           (setq quit-flag t)
           (:return))
          ('automatic
           (setq conn--kapply-automatic-flag t)
           (:return))
          ('help
           (with-output-to-temp-buffer "*Help*"
             (princ
              (substitute-command-keys
               "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that.
\\[automatic]   Apply keyboard macro to rest."))
             (with-current-buffer standard-output
               (help-mode))))
          (_ (ding t))))))))

;;;;; Iterators

(defun conn--kapply-macro (applier iterator pipeline)
  (funcall applier
           (seq-reduce (lambda (it ctor) (funcall ctor it))
                       (remq nil pipeline)
                       iterator)))

(defun conn--kapply-infinite-iterator ()
  (declare (important-return-value t)
           (side-effect-free t))
  (lambda (_state) (cons (point) (point))))

(defun conn--kapply-highlight-iterator (beg end &optional sort-function read-patterns)
  (declare (important-return-value t)
           (side-effect-free t))
  (let ((patterns
         (when (and (boundp 'hi-lock-interactive-patterns)
                    (boundp 'hi-lock-interactive-lighters))
           (if read-patterns
               (mapcar (lambda (regexp)
                         (alist-get regexp hi-lock-interactive-lighters
                                    nil nil #'equal))
                       (completing-read-multiple
                        "Regexps for kapply: "
                        (mapcar (lambda (pattern)
                                  (thread-first
                                    (rassq pattern hi-lock-interactive-lighters)
                                    car (or (car pattern)) (cons pattern)))
                                hi-lock-interactive-patterns)
                        nil t nil nil))
             hi-lock-interactive-patterns)))
        matches)
    (save-excursion
      (with-restriction beg end
        (pcase-dolist (`(,fn (,subexp . ,_)) patterns)
          (goto-char (point-min))
          (while-let ((match (funcall fn (point-max))))
            (push (cons (conn--create-marker (match-beginning subexp) nil t)
                        (conn--create-marker (match-end subexp)))
                  matches)))))
    (unless matches
      (user-error "No highlights for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp matches)
           (pcase-dolist (`(,beg . ,end) matches)
             (set-marker beg nil)
             (set-marker end nil))))
        ((or :record :next)
         (pop matches))))))

(defun conn--kapply-region-iterator (regions &optional sort-function)
  (declare (important-return-value t))
  (unless regions
    (user-error "No regions for kapply."))
  (pcase-dolist ((and reg `(,beg . ,end))
                 (setq regions (funcall (or sort-function #'identity)
                                        regions)))
    (if (markerp beg)
        (set-marker-insertion-type beg t)
      (setcar reg (conn--create-marker beg nil t)))
    (if (markerp end)
        (set-marker-insertion-type end nil)
      (setcdr reg (conn--create-marker end (marker-buffer (car reg))))))
  (lambda (state)
    (pcase state
      (:cleanup
       (when (consp regions)
         (pcase-dolist (`(,beg . ,end) regions)
           (set-marker beg nil)
           (set-marker end nil))))
      ((or :record :next)
       (pop regions))
      (_ state))))

(defun conn--kapply-point-iterator (points &optional sort-function)
  (declare (important-return-value t))
  (unless points
    (user-error "No points for kapply."))
  (let ((points
         (cl-loop for pt in (funcall (or sort-function #'identity) points)
                  collect (if (markerp pt) pt (conn--create-marker pt)))))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp points)
           (dolist (pt points)
             (set-marker pt nil))))
        ((or :record :next)
         (when-let* ((pt (pop points)))
           (cons pt pt)))))))

(defun conn--kapply-match-iterator ( string regions
                                     &optional
                                     sort-function
                                     regexp-flag
                                     delimited-flag)
  (declare (important-return-value t))
  (let (matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end) regions)
        (goto-char beg)
        (cl-loop
         while (replace-search string end regexp-flag
                               delimited-flag case-fold-search)
         for (mb me . _) = (match-data t)
         do (push (cons (conn--create-marker mb nil t)
                        (conn--create-marker me))
                  matches))))
    (unless matches
      (user-error "No matches for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (when (consp matches)
           (mapc (pcase-lambda (`(,beg . ,end))
                   (set-marker beg nil)
                   (set-marker end nil))
                 matches)))
        ((or :next :record)
         (pop matches))))))

;;;;; Pipeline Functions

(defvar conn--kapply-pipeline-depths
  '((kapply-skip-empty . 95)
    (kapply-relocate . 85)
    (kapply-invisible . 75)
    (kapply-nth . 70)
    (kapply-query-record . 50)
    (kapply-query-per-iteration . 49)
    (kapply-ibuffer . 40)
    (kapply-restrictions . 10)
    (kapply-excursions . 0)
    (kapply-undo . -10)
    (kapply-region . -20)
    (kapply-state . -50)
    (kapply-wconf . -70)
    (kapply-pulse . -90)))

(defun conn--kapply-query (iterator &optional each-iteration)
  (declare (important-return-value t)
           (side-effect-free t))
  (when each-iteration
    (add-function
     :around (var iterator)
     (let ((hl (make-overlay (point-min) (point-min))))
       (overlay-put hl 'priority 2000)
       (overlay-put hl 'face 'query-replace)
       (overlay-put hl 'conn-overlay t)
       (lambda (iterator state)
         (let ((msg (substitute-command-keys
                     "Proceed with macro?\\<query-replace-map>\
 (\\[act] act, \\[skip] skip, \\[exit] exit, \\[recenter] recenter, \\[edit] edit, \\[automatic] auto)")))
           (pcase state
             (:next
              (let ((res (funcall iterator state)))
                (if conn--kapply-automatic-flag
                    res
                  (conn-named-loop _
                    (move-overlay hl (region-beginning) (region-end) (current-buffer))
                    (pcase (let ((executing-kbd-macro nil)
                                 (defining-kbd-macro nil))
                             (message "%s" msg)
                             (lookup-key query-replace-map (vector (read-event))))
                      ('act (:return res))
                      ('skip (setq res (funcall iterator state)))
                      ('exit (:return))
                      ('recenter (recenter nil))
                      ('quit (signal 'quit nil))
                      ('automatic
                       (setq conn--kapply-automatic-flag t)
                       (:return res))
                      ('help
                       (with-output-to-temp-buffer "*Help*"
                         (princ
                          (substitute-command-keys
                           "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that.
\\[automatic]   Apply keyboard macro to rest."))
                         (with-current-buffer standard-output
                           (help-mode))))
                      (_ (ding t)))))))
             (:cleanup
              (delete-overlay hl)
              (funcall iterator state))
             (_ (funcall iterator state))))))
     `((depth . ,(alist-get 'kapply-query-per-iteration
                            conn--kapply-pipeline-depths))
       (name . kapply-query-per-iteration))))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       (:record
        (let ((hl (make-overlay (point) (point))))
          (overlay-put hl 'priority 2000)
          (overlay-put hl 'face 'query-replace)
          (overlay-put hl 'conn-overlay t)
          (unwind-protect
              (cl-loop
               for val = (funcall iterator state)
               until (or (null val)
                         (progn
                           (recenter nil)
                           (move-overlay hl
                                         (region-beginning)
                                         (region-end)
                                         (current-buffer))
                           (y-or-n-p (format "Record here?"))))
               finally return val)
            (delete-overlay hl))))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-query-record
                          conn--kapply-pipeline-depths))
     (name . kapply-query-record))))

(defun conn--kapply-skip-empty (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (catch 'non-empty
          (while-let ((region (funcall iterator state)))
            (pcase region
              ((and `(,beg . ,end)
                    (guard (/= beg end))
                    region)
               (throw 'non-empty region))
              (`(,beg . ,end)
               (when (markerp beg) (set-marker beg nil))
               (when (markerp end) (set-marker end nil)))))))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-skip-empty
                          conn--kapply-pipeline-depths))
     (name . kapply-skip-empty))))

(defun conn--kapply-every-nth (iterator N)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (dotimes (_ (1- N))
          (pcase (funcall iterator state)
            (`(,beg . ,end)
             (when (markerp beg) (set-marker beg nil))
             (when (markerp end) (set-marker end nil)))))
        (funcall iterator state))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-nth conn--kapply-pipeline-depths))
     (name . kapply-nth))))

(defun conn--kapply-skip-invisible-points (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (cl-loop for ret = (funcall iterator state)
                 until (or (null ret)
                           (not (invisible-p (car ret))))
                 when (markerp ret) do (set-marker ret nil)
                 finally return ret))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-skip-invisible-regions (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (cl-loop for ret = (funcall iterator state)
                 until (or (null ret)
                           (conn--region-visible-p (car ret) (cdr ret)))
                 do (pcase-let ((`(,beg . ,end) ret))
                      (when (markerp beg) (set-marker beg nil))
                      (when (markerp end) (set-marker end nil)))
                 finally return ret))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-open-invisible (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (restore)
     (lambda (iterator state)
       (pcase state
         ((or :next :record)
          (cl-loop for next = (funcall iterator state)
                   for res = (or (null next)
                                 (conn--open-invisible (car next) (cdr next)))
                   until res
                   do (pcase-let ((`(,beg . ,end) next))
                        (when (markerp beg) (set-marker beg nil))
                        (when (markerp end) (set-marker end nil)))
                   finally return (prog1 next
                                    (when (consp res)
                                      (setq restore (nconc res restore))))))
         (:cleanup
          (funcall iterator state)
          (mapc #'funcall restore))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn--kapply-relocate-to-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (let ((region (funcall iterator state)))
       (pcase state
         ((or :next :record)
          (pcase region
            ((and (pred identity)
                  `(,beg . ,end))
             (when-let* ((buffer (and (markerp beg) (marker-buffer beg)))
                         ((not (eq buffer (current-buffer)))))
               (pop-to-buffer-same-window buffer)
               (deactivate-mark t)
               (unless (eq buffer (window-buffer (selected-window)))
                 (error "Could not pop to buffer %s" buffer)))
             (goto-char beg)
             (conn--push-ephemeral-mark end)))))
       region))
   `((depth . ,(alist-get 'kapply-relocate conn--kapply-pipeline-depths))
     (name . kapply-relocate))))

(defun conn--kapply-per-buffer-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (undo-handles)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(_ . ,handle) undo-handles)
            (accept-change-group handle)
            (undo-amalgamate-change-group handle)))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (unless (or (alist-get (current-buffer) undo-handles)
                        (eq buffer-undo-list t))
              (activate-change-group
               (setf (alist-get (current-buffer) undo-handles)
                     (prepare-change-group))))))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-per-buffer-atomic-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (undo-handles)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(_ . ,handle) undo-handles)
            (if conn-kmacro-apply-error
                (cancel-change-group handle)
              (accept-change-group handle)
              (undo-amalgamate-change-group handle))))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (unless (or (alist-get (current-buffer) undo-handles)
                        (eq buffer-undo-list t))
              (activate-change-group
               (setf (alist-get (current-buffer) undo-handles)
                     (prepare-change-group))))))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-per-iteration-undo (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (handle)
     (lambda (iterator state)
       (pcase state
         (:record
          (prog1
              (funcall iterator state)
            (unless (eq buffer-undo-list t)
              (setq handle (prepare-change-group))
              (activate-change-group handle))))
         (:cleanup
          (when handle
            (accept-change-group handle)
            (undo-amalgamate-change-group handle))
          (funcall iterator state))
         (:next
          (when handle
            (accept-change-group handle)
            (undo-amalgamate-change-group handle))
          (prog1
              (funcall iterator state)
            (if (eq buffer-undo-list t)
                (setq handle nil)
              (undo-boundary)
              (setq handle (prepare-change-group))
              (activate-change-group handle))))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-undo conn--kapply-pipeline-depths))
     (name . kapply-undo))))

(defun conn--kapply-ibuffer-overview (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let ((msg (substitute-command-keys
               "\\<query-replace-map>Buffer is modified, save before continuing?\
 \\[act], \\[skip], \\[quit], \\[edit], \\[automatic], \\[help]"))
         buffers automatic)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (when (and (not conn-kmacro-apply-error)
                     (length> buffers 1))
            (ibuffer t "*Kapply Ibuffer*"
                     `((predicate . (memq (current-buffer) ',buffers))))))
         ((or :record :next)
          (prog1 (funcall iterator state)
            (unless (or automatic
                        (memq (current-buffer) buffers))
              (push (current-buffer) buffers)
              (when (and (buffer-modified-p)
                         buffer-file-name)
                (redisplay)
                (conn-named-loop _
                  (ding t)
                  (pcase (let ((executing-kbd-macro nil)
                               (defining-kbd-macro nil))
                           (message "%s" msg)
                           (lookup-key query-replace-map (vector (read-event))))
                    ('act (:return (save-buffer '(16))))
                    ('skip (:return))
                    ('quit (setq quit-flag t))
                    ('edit
                     (let (executing-kbd-macro defining-kbd-macro)
                       (recursive-edit))
                     (:return))
                    ('automatic
                     (setq automatic t)
                     (:return))
                    ('help
                     (with-output-to-temp-buffer "*Help*"
                       (princ
                        (substitute-command-keys
                         "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Save file and continue iteration.
\\[skip]	Don't save file and continue iteration.
\\[quit]	Stop the macro entirely right now.
\\[edit]	Enter recursive edit; resume executing the keyboard macro afterwards.
\\[automatic]	Continue iteration and don't ask to save again."))
                       (with-current-buffer standard-output
                         (help-mode)))))))))))))
   `((depth . ,(alist-get 'kapply-ibuffer conn--kapply-pipeline-depths))
     (name . kapply-ibuffer))))

(defun conn--kapply-save-excursion (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (saved-excursions)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(,buffer ,pt . ,saved) saved-excursions)
            (with-current-buffer buffer
              (goto-char pt)
              (set-marker pt nil)
              (save-mark-and-excursion--restore saved))))
         (:record
          (setf (alist-get (current-buffer) saved-excursions)
                (let ((pt (point-marker)))
                  (set-marker-insertion-type pt t)
                  (cons pt (save-mark-and-excursion--save))))
          (funcall iterator state))
         (:next
          (if saved-excursions
              (prog1 (funcall iterator state)
                (unless (alist-get (current-buffer) saved-excursions)
                  (setf (alist-get (current-buffer) saved-excursions)
                        (cons (point-marker) (save-mark-and-excursion--save)))))
            (setf (alist-get (current-buffer) saved-excursions)
                  (cons (point-marker) (save-mark-and-excursion--save)))
            (funcall iterator state)))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-excursions conn--kapply-pipeline-depths))
     (name . kapply-excursions))))

(defun conn--kapply-save-restriction (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (kapply-saved-restrictions)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (pcase-dolist (`(,buffer ,beg . ,end) kapply-saved-restrictions)
            (with-current-buffer buffer
              (widen)
              (narrow-to-region (or beg (point-min))
                                (or end (point-max))))))
         ((or :record :next)
          (prog1
              (funcall iterator state)
            (pcase (alist-get (current-buffer) kapply-saved-restrictions)
              ('nil
               (let ((beg (point-min-marker))
                     (end (point-max-marker)))
                 (without-restriction
                   (setf (alist-get (current-buffer) kapply-saved-restrictions)
                         (cons (when (/= beg (point-min)) beg)
                               (when (/= end (point-max)) end))))))
              (`(nil . nil) (widen))
              (`(,beg . ,end)
               (widen)
               (narrow-to-region (or beg (point-min))
                                 (or end (point-max)))))))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-restrictions conn--kapply-pipeline-depths))
     (name . kapply-restrictions))))

(defun conn--kapply-change-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (let ((ret (funcall iterator state)))
       (when (and ret (or (eq state :next)
                          (eq state :record)))
         (delete-region (region-beginning) (region-end)))
       ret))
   `((depth . ,(alist-get 'kapply-region conn--kapply-pipeline-depths))
     (name . kapply-region)))
  (conn--kapply-with-state iterator 'conn-emacs-state))

(defun conn--kapply-at-end (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (unless (eq state :cleanup)
       (conn-exchange-mark-command)))
   `((depth . ,(alist-get 'kapply-region conn--kapply-pipeline-depths))
     (name . kapply-region))))

(defun conn--kapply-with-state (iterator conn-state)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (buffer-stacks)
     (lambda (iterator state)
       (let ((ret (funcall iterator state)))
         (pcase state
           (:cleanup
            (pcase-dolist (`(,buf . ,stack) buffer-stacks)
              (with-current-buffer buf
                (setq conn--state-stack stack
                      conn-lighter nil)
                (conn-enter-state (car stack)))))
           ((and (or :record :next)
                 (guard ret))
            (when conn-local-mode
              (if-let* ((stack (alist-get (current-buffer) buffer-stacks)))
                  (setf conn--state-stack stack)
                (setf (alist-get (current-buffer) buffer-stacks)
                      conn--state-stack))
              (conn-enter-recursive-stack conn-state))))
         ret)))
   `((depth . ,(alist-get 'kapply-state conn--kapply-pipeline-depths))
     (name . kapply-state))))

(defun conn--kapply-pulse-region (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (when (eq state :record)
       (pulse-momentary-highlight-region (region-beginning)
                                         (region-end)
                                         'query-replace)))
   `((depth . ,(alist-get 'kapply-pulse conn--kapply-pipeline-depths))
     (name . kapply-pulse))))

(defun conn--kapply-save-windows (iterator)
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (wconf)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (funcall iterator state)
          (set-window-configuration wconf))
         ((or :record :next)
          (unless wconf (setq wconf (current-window-configuration)))
          (funcall iterator state))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-wconf conn--kapply-pipeline-depths))
     (name . kapply-wconf))))

;;;;; Applier Definitions

(defvar conn-kapply-suppress-message nil)

(defun conn--perform-kapply (iterator body)
  (require 'kmacro)
  (let* ((undo-outer-limit nil)
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (conn-kmacro-applying-p t)
         (conn--kapply-automatic-flag nil)
         (iterations 0)
         (success nil)
         (iterator (lambda (&optional state)
                     (when-let* ((ret (funcall iterator (or state :next))))
                       (pcase ret
                         (`(,beg . ,end)
                          (when (markerp beg) (set-marker beg nil))
                          (when (markerp end) (set-marker end nil))))
                       (cl-incf iterations)))))
    (deactivate-mark)
    (unwind-protect
        (cl-letf (((symbol-function 'kmacro-loop-setup-function)))
          (advice-add 'kmacro-loop-setup-function :before-while iterator)
          (run-hooks 'conn-kmacro-apply-start-hook)
          (funcall body iterator)
          (setq success t)
          (unless conn-kapply-suppress-message
            (message "Kapply completed successfully after %s iterations"
                     iterations)))
      (let ((conn-kmacro-apply-error (not success)))
        (funcall iterator :cleanup)
        (run-hooks 'conn-kmacro-apply-end-hook)))))

(defmacro conn--define-kapplier (name arglist &rest body)
  "Define a macro application function.

The iterator must be the first argument in ARGLIST.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug ( name lambda-list
                    [&optional lambda-doc]
                    def-body))
           (doc-string 3)
           (indent 2))
  (pcase-let ((`(,decls . ,exps) (macroexp-parse-body body)))
    `(defun ,name ,arglist
       ,@decls
       (conn--perform-kapply ,(car arglist)
                             (lambda (,(car arglist)) ,@exps)))))

(conn--define-kapplier conn--kmacro-apply (iterator &optional count macro)
  (pcase-exhaustive macro
    ((pred kmacro-p)
     (funcall macro (or count 0)))
    ((or (pred stringp) (pred vectorp))
     (kmacro-call-macro (or count 0) nil nil macro))
    ('nil
     (when (funcall iterator :record)
       (let ((last-macro last-kbd-macro))
         (kmacro-start-macro nil)
         (unwind-protect
             (progn
               (recursive-edit)
               (when (not defining-kbd-macro)
                 (user-error "Not defining keyboard macro")))
           (when defining-kbd-macro (kmacro-end-macro nil)))
         (when (eq last-macro last-kbd-macro)
           (user-error "New keyboard macro not defined"))
         (kmacro-call-macro (or count 0)))))))

(conn--define-kapplier conn--kmacro-apply-append (iterator &optional count skip-exec)
  (when (funcall iterator :record)
    (kmacro-start-macro (if skip-exec '(16) '(4)))
    (unwind-protect
        (progn
          (recursive-edit)
          (when (not defining-kbd-macro)
            (user-error "Not defining keyboard macro")))
      (when defining-kbd-macro (kmacro-end-macro nil)))
    (kmacro-call-macro (or count 0))))

(conn--define-kapplier conn--kmacro-apply-step-edit (iterator &optional count)
  (when (funcall iterator :record)
    (let* ((apply nil)
           (hook (lambda () (setq apply kmacro-step-edit-replace))))
      (add-hook 'kbd-macro-termination-hook hook)
      (unwind-protect
          (kmacro-step-edit-macro)
        (remove-hook 'kbd-macro-termination-hook hook))
      (unless apply
        (user-error "Keyboard macro edit aborted")))
    (kmacro-call-macro (or count 0))))

(provide 'conn-kapply)
