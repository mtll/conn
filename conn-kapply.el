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

(require 'kmacro)
(require 'conn-vars)
(require 'conn-utils)
(require 'conn-states)
(require 'conn-things)
(require 'conn-commands)
(eval-when-compile
  (require 'cl-lib))

(declare-function conn--kmacro-display "conn-transient")
(declare-function conn-dispatch-kapply-prefix "conn-transients")
(declare-function project-files "project")

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
      (let ((wconf (current-window-configuration)))
        (cl-loop
         (pcase (let ((executing-kbd-macro nil)
                      (defining-kbd-macro nil))
                  (message "%s" msg)
                  (lookup-key query-replace-map (vector (read-event))))
           ('act
            (set-window-configuration wconf)
            (cl-return))
           ('skip
            (setq executing-kbd-macro "")
            (cl-return))
           ('exit
            (setq executing-kbd-macro t)
            (cl-return))
           ('recenter
            (recenter nil))
           ('edit
            (let (executing-kbd-macro defining-kbd-macro)
              (recursive-edit)))
           ('quit
            (setq quit-flag t)
            (cl-return))
           ('automatic
            (set-window-configuration wconf)
            (setq conn--kapply-automatic-flag t)
            (cl-return))
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
           (_ (ding t)))))))))

;;;;; Iterators

(defun conn-kapply-make-region (beg end &optional buffer)
  "Create a region from BEG to END in BUFFER for a kapply iterator.

See also `conn-kapply-consume-region'."
  (make-overlay beg end buffer t))

(defun conn-kapply-consume-region (ov)
  "Consume overlay OV and return a region for kapply pipeline functions.

The return value is of the form ((BEG . END) . BUFFER).

This deletes the overlay OV.

See also `conn-kapply-make-region'."
  (when ov
    (prog1 (cons (cons (overlay-start ov)
                       (overlay-end ov))
                 (overlay-buffer ov))
      (delete-overlay ov))))

(defun conn-kapply-macro (applier iterator pipeline)
  "Apply a keyboard macro on a set of regions.

APPLIER is a function that will be called with one argument, an
iterator, and should apply a keyboard on regions returned by the
iterator.  See also `conn-define-kapplier'.

ITERATOR is a function that will be called with one argument, the state
of the current iteration.  If the state is :cleanup then ITERATOR should
cleanup any remaining regions and the return value will be discarded.
If the state is anything else ITERATOR should return the next region as
a pair ((BEG . END) . BUFFER) or nil if there is no next region.

PIPELINE is a list of pipeline functions.  Each function should take one
argument, an iterator, and return another iterator."
  (funcall applier
           (seq-reduce (lambda (it ctor) (funcall ctor it))
                       (remq nil pipeline)
                       iterator)))

(defun conn-kapply-infinite-iterator ()
  (declare (important-return-value t)
           (side-effect-free t))
  (lambda (_state)
    (cons (cons (point) (point))
          (current-buffer))))

(defun conn-kapply-highlight-iterator (beg
                                       end
                                       &optional
                                       sort-function
                                       read-patterns)
  "Create an iterator over all highlights in the region from BEG to END.

SORT-FUNCTION if non-nil is a function to sort the regions before
iterating over them.  SORT-FUNCTION should take a list of overlays.

READ-PATTERNS if non-nil prompts the user for the specific highlight
patterns to iterate over instead of iterating over all highlights in the
region."
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
            (push (conn-kapply-make-region (match-beginning subexp)
                                           (match-end subexp))
                  matches)))))
    (unless matches
      (user-error "No highlights for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (mapc #'delete-overlay matches))
        ((or :record :next)
         (conn-kapply-consume-region (pop matches)))))))

(defun conn-kapply-region-iterator (regions &optional sort-function)
  "Create an iterator over REGIONS.

REGIONS must be a list of overlays.  See `conn-kapply-make-region'.

SORT-FUNCTION if non-nil is a function to sort the regions before
iterating over them.  SORT-FUNCTION should take a list of overlays."
  (declare (important-return-value t))
  (unless regions
    (user-error "No regions for kapply."))
  (when sort-function
    (setq regions (funcall sort-function regions)))
  (lambda (state)
    (pcase state
      (:cleanup
       (mapc #'delete-overlay regions))
      ((or :record :next)
       (conn-kapply-consume-region (pop regions))))))

(defun conn-kapply-point-iterator (points &optional sort-function)
  "Create an iterator over a list of POINTS.

POINTS are taken to be in the current buffer.

SORT-FUNCTION if non-nil is a function to sort the regions before
iterating over them.  SORT-FUNCTION should take a list of overlays."
  (declare (important-return-value t))
  (unless points
    (user-error "No points for kapply."))
  (let ((points
         (cl-loop for pt in points
                  collect (conn-kapply-make-region pt pt))))
    (when sort-function
      (funcall sort-function points))
    (lambda (state)
      (pcase state
        (:cleanup
         (mapc #'delete-overlay points))
        ((or :record :next)
         (conn-kapply-consume-region (pop points)))))))

(defun conn--kapply-read-from-with-preview (prompt bounds &optional regexp-flag)
  "Read a from string with `minibuffer-lazy-highlight-setup' previews.

PROMPT is used as the minibuffer prompt when reading.

BOUNDS is a list of the form returned by `region-bounds' and defines the
limits of the highlighting.

REGEXP-FLAG means to treat the from string as a regexp for the purpose
of highlighting."
  (let ((default (conn-replace-read-default)))
    (conn-with-region-emphasis bounds
      (minibuffer-with-setup-hook
          (minibuffer-lazy-highlight-setup
           :case-fold case-fold-search
           :filter (lambda (mb me)
                     (cl-loop for (beg . end) in bounds
                              when (<= beg mb me end) return t))
           :highlight query-replace-lazy-highlight
           :regexp regexp-flag
           :regexp-function (or replace-regexp-function
                                (and replace-char-fold
                                     (not regexp-flag)
                                     #'char-fold-to-regexp))
           :transform (lambda (string)
                        (when (and case-fold-search search-upper-case)
                          (setq isearch-case-fold-search
                                (isearch-no-upper-case-p string regexp-flag)))
                        string))
        (if regexp-flag
            (read-regexp (format-prompt prompt default)
                         (when default (regexp-quote default))
                         'minibuffer-history)
          (let ((from (read-string
                       (format-prompt prompt default)
                       nil nil
                       (if default
                           (delete-dups
                            (cons default (query-replace-read-from-suggestions)))
                         (query-replace-read-from-suggestions))
                       t)))
            (or (and (length= from 0) default)
                from)))))))

(cl-defgeneric conn-kapply-match-iterator (thing
                                           arg
                                           transform
                                           &optional
                                           subregions
                                           regexp-flag
                                           delimited-flag
                                           sort-function)
  "Create an iterator over matches for a string in a region.

The region is defined by THING, ARG, and TRANSFORM.  For how they are
used to define the region see `conn-bounds-of' and
`conn-transform-bounds'.

If SUBREGIONS is non-nil then restrict matching to the subregions of the
region.

The string to match is read interactively.

If REGEXP-FLAG is non-nil then the string is taken to be a regexp.

If DELIMITED-FLAG is non-nil then only match regions surrounded by word
boundaries.

SORT-FUNCTION if non-nil is a function to sort the regions before
iterating over them.  SORT-FUNCTION should take a list of overlays.")

(cl-defmethod conn-kapply-match-iterator ((_thing (eql project))
                                          _arg
                                          _transform
                                          &optional
                                          _subregions
                                          regexp-flag
                                          delimited-flag
                                          sort-function)
  (require 'project)
  (let ((files (or (project-files (project-current t))
                   (user-error "No files for kapply.")))
        (string (conn--kapply-read-from-with-preview
                 (if regexp-flag "Regexp" "String")
                 (list (cons (point-min) (point-max)))
                 regexp-flag))
        matches)
    (cl-labels
        ((collect-matches (buffer)
           (with-current-buffer buffer
             (save-excursion
               (save-match-data
                 (goto-char (point-min))
                 (while (replace-search string (point-max) regexp-flag
                                        delimited-flag case-fold-search)
                   (pcase (match-data t)
                     (`(,mb ,me . ,_)
                      (push (conn-kapply-make-region mb me)
                            matches)))))))
           (setq matches (nreverse matches))
           (when sort-function
             (setq matches (funcall sort-function matches))))
         (check-buffer (buffer)
           (with-current-buffer buffer
             (save-excursion
               (goto-char (point-min))
               (replace-search string (point-max) regexp-flag
                               delimited-flag case-fold-search))))
         (next ()
           (when-let* ((next (pop files)))
             (let ((buffer (get-file-buffer next)))
               (cond (buffer
                      (when (check-buffer buffer)
                        (collect-matches buffer)))
                     ((with-work-buffer
                        (condition-case err
                            (insert-file-contents next nil)
                          (file-missing nil)
                          (file-error
                           (let ((msg (error-message-string err)))
                             (unless (string-search next msg)
                               (setq msg (format "%s: %s" next msg)))
                             (delay-warning 'file-error msg :error))
                           nil))
                        (check-buffer (current-buffer)))
                      (collect-matches (find-file-noselect next)))
                     (t (next)))))))
      (lambda (state)
        (pcase state
          (:cleanup
           (mapc #'delete-overlay matches))
          ((or :next :record)
           (unless matches (next))
           (conn-kapply-consume-region (pop matches))))))))

(cl-defmethod conn-kapply-match-iterator ((thing (conn-thing t))
                                          arg
                                          transform
                                          &optional
                                          subregions
                                          regexp-flag
                                          delimited-flag
                                          sort-function)
  (let* ((regions
          (prog1
              (pcase (conn-bounds-of thing arg)
                ((and (guard subregions)
                      (conn-bounds-get :subregions
                                       transform
                                       (and sr (pred identity))))
                 (cl-loop for reg in sr collect (conn-bounds reg)))
                ((conn-bounds whole transform)
                 (list whole)))
            (deactivate-mark)))
         (string (conn--kapply-read-from-with-preview
                  (if regexp-flag "Regexp" "String")
                  regions
                  regexp-flag))
         matches)
    (save-excursion
      (pcase-dolist (`(,beg . ,end) regions)
        (goto-char beg)
        (while (replace-search string end regexp-flag
                               delimited-flag case-fold-search)
          (pcase (match-data t)
            (`(,mb ,me . ,_)
             (push (conn-kapply-make-region mb me)
                   matches))))))
    (unless matches
      (user-error "No matches for kapply."))
    (setq matches (nreverse matches))
    (when sort-function
      (setq matches (funcall sort-function matches)))
    (lambda (state)
      (pcase state
        (:cleanup
         (mapc #'delete-overlay matches))
        ((or :next :record)
         (conn-kapply-consume-region (pop matches)))))))

;;;;; Pipeline Functions

(defconst conn--kapply-pipeline-depths
  '((kapply-skip-empty . 95)
    (kapply-relocate . 85)
    (kapply-invisible . 75)
    (kapply-nth . 70)
    (kapply-query . 50)
    (kapply-ibuffer . 40)
    (kapply-restrictions . 10)
    (kapply-excursions . 0)
    (kapply-undo . -10)
    (kapply-region . -20)
    (kapply-state . -50)
    (kapply-wconf . -70)
    (kapply-pulse . -90))
  "Alist of depth values for kapply pipeline functions.

The alist should not be modified.")

(defun conn-kapply-query (iterator)
  "Query user before each iteration of the keyboard macro.

The options provided are: \\<query-replace-map>

\\[act]	Proceed with this iteration normally and continue to the next.
\\[skip]	Skip this iteration and got to the next.
\\[exit]	End this kapply normally.
\\[quit]	End this kapply by signaling a quit.
\\[recenter]	Redisplay the screen, then ask again.
\\[automatic]	Apply keyboard macro to rest."
  (declare (important-return-value t)
           (side-effect-free t))
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
           ;; TODO: add more options to recording query
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
           (:next
            (let ((res (funcall iterator state)))
              (if conn--kapply-automatic-flag
                  res
                (save-window-excursion
                  (cl-loop
                   (move-overlay hl (region-beginning) (region-end) (current-buffer))
                   (pcase (let ((executing-kbd-macro nil)
                                (defining-kbd-macro nil))
                            (message "%s" msg)
                            (lookup-key query-replace-map (vector (read-event))))
                     ('act (cl-return res))
                     ('skip (setq res (funcall iterator state)))
                     ('exit (cl-return))
                     ('recenter (recenter nil))
                     ('quit (signal 'quit nil))
                     ('automatic
                      (setq conn--kapply-automatic-flag t)
                      (cl-return res))
                     ('help
                      (with-output-to-temp-buffer "*Help*"
                        (princ
                         (substitute-command-keys
                          "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Proceed with this iteration normally and continue to the next.
\\[skip]	Skip this iteration and got to the next.
\\[exit]	End this kapply normally.
\\[quit]	End this kapply by signaling a quit.
\\[recenter]	Redisplay the screen, then ask again.
\\[automatic]	Apply keyboard macro to rest."))
                        (with-current-buffer standard-output
                          (help-mode))))
                     (_ (ding t))))))))
           (:cleanup
            (delete-overlay hl)
            (funcall iterator state))
           (_ (funcall iterator state))))))
   `((depth . ,(alist-get 'kapply-query conn--kapply-pipeline-depths))
     (name . kapply-query))))

(defun conn-kapply-skip-empty (iterator)
  "Skip empty regions returned by ITERATOR.

Empty regions are those with a length of zero."
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
              ((and `(,beg ,end . ,_)
                    (guard (/= beg end))
                    region)
               (throw 'non-empty region))))))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-skip-empty
                          conn--kapply-pipeline-depths))
     (name . kapply-skip-empty))))

(defun conn-kapply-every-nth (iterator N)
  "Only apply keyboard macro to every Nth region returned by ITERATOR."
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (dotimes (_ (1- N))
          (funcall iterator state))
        (funcall iterator state))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-nth conn--kapply-pipeline-depths))
     (name . kapply-nth))))

(defun conn-kapply-skip-invisible-points (iterator)
  "Skip regions with a non-nil invisibility spec at region beginning."
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
                 finally return ret))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn-kapply-skip-invisible-regions (iterator)
  "Skip regions with a non-nil invisibility spec.

Regions are only skipped if the entire region is not visible."
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (lambda (iterator state)
     (pcase state
       ((or :next :record)
        (cl-loop for ret = (funcall iterator state)
                 until (or (null ret)
                           (conn--region-visible-p (caar ret) (cdar ret)))
                 finally return ret))
       (_ (funcall iterator state))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn-kapply-open-invisible (iterator)
  "Open invisible regions returned by ITERATOR.

If the region is invisible and cannot be opened then skip it."
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
                                 (conn--open-invisible (caar next)
                                                       (cdar next)))
                   until res
                   finally return (prog1 next
                                    (when (consp res)
                                      (setq restore (nconc res restore))))))
         (:cleanup
          (funcall iterator state)
          (mapc #'funcall restore))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-invisible conn--kapply-pipeline-depths))
     (name . kapply-invisible))))

(defun conn-kapply-relocate-to-region (iterator)
  "Relocate point and mark the bounds of region return by ITERATOR.

This also handles switching to the regions buffer if it is not the
current buffer."
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
                  `((,beg . ,end) . ,buffer))
             (unless (eq buffer (current-buffer))
               (switch-to-buffer buffer t)
               (deactivate-mark)
               (unless (eq buffer (window-buffer (selected-window)))
                 (error "Could not pop to buffer %s" buffer)))
             (goto-char beg)
             (conn--push-ephemeral-mark end))
            ('nil)
            (_ (error "Invalid region %s" region)))))
       region))
   `((depth . ,(alist-get 'kapply-relocate conn--kapply-pipeline-depths))
     (name . kapply-relocate))))

(defun conn-kapply-per-buffer-undo (iterator)
  "Amalgamate undo in each buffer returned by ITERATOR.

Changes will not be undone if an error is signaled during macro
application."
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

(defun conn-kapply-per-buffer-atomic-undo (iterator)
  "Amalgamate undo in each buffer returned by ITERATOR.

Changes will be undone if an error is signaled during macro application."
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

(defun conn-kapply-per-iteration-undo (iterator)
  "Amalgamate undo after each iteration."
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

(defun conn-kapply-ibuffer-overview (iterator)
  "Display an ibuffer window if multiple buffers are visited by ITERATOR."
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
                (let ((wconf (current-window-configuration)))
                  (cl-loop
                   (ding t)
                   (pcase (let ((executing-kbd-macro nil)
                                (defining-kbd-macro nil))
                            (message "%s" msg)
                            (lookup-key query-replace-map (vector (read-event))))
                     ('act
                      (set-window-configuration wconf)
                      (cl-return (save-buffer '(16))))
                     ('skip (cl-return))
                     ('quit (setq quit-flag t))
                     ('edit
                      (let (executing-kbd-macro defining-kbd-macro)
                        (recursive-edit))
                      (cl-return))
                     ('automatic
                      (set-window-configuration wconf)
                      (setq automatic t)
                      (cl-return))
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
                          (help-mode))))))))))))))
   `((depth . ,(alist-get 'kapply-ibuffer conn--kapply-pipeline-depths))
     (name . kapply-ibuffer))))

(defun conn-kapply-save-excursion (iterator)
  "Save excursion in each buffer visited by ITERATOR.

When kapply finishes restore the previous point in each buffer."
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

(defun conn-kapply-save-restriction (iterator)
  "Save restrictions in each buffer visited by ITERATOR.

When kapply finishes restore the restrictions in each buffer."
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

(defun conn-kapply-change-region (iterator)
  "Delete the region returned by ITERATOR before each macro iteration."
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
  (conn-kapply-with-state iterator 'conn-emacs-state))

(defun conn-kapply-at-end (iterator)
  "Exchange point and mark before each macro iteration."
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :after (var iterator)
   (lambda (state)
     (unless (eq state :cleanup)
       (conn-exchange-mark-command)))
   `((depth . ,(alist-get 'kapply-region conn--kapply-pipeline-depths))
     (name . kapply-region))))

(defun conn-kapply-with-state (iterator conn-state)
  "Begin each macro iteration in a recursive stack containing CONN-STATE."
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
                (conn-enter-state (car stack))
                (conn-update-lighter))))
           ((and (or :record :next)
                 (guard ret))
            (when conn-local-mode
              (if-let* ((stack (alist-get (current-buffer) buffer-stacks)))
                  (setf conn--state-stack stack)
                (setf (alist-get (current-buffer) buffer-stacks)
                      conn--state-stack))
              (ignore (conn-enter-recursive-stack conn-state)))))
         ret)))
   `((depth . ,(alist-get 'kapply-state conn--kapply-pipeline-depths))
     (name . kapply-state))))

(defun conn-kapply-pulse-region (iterator)
  "Pulse the region returned by ITERATOR when recording a macro."
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

(defun conn-kapply-save-windows (iterator)
  "Save the window configuration during kapply.

After kapply has finished restore the previous window configuration."
  (declare (important-return-value t)
           (side-effect-free t))
  (add-function
   :around (var iterator)
   (let (wconf)
     (lambda (iterator state)
       (pcase state
         (:cleanup
          (set-window-configuration wconf)
          (funcall iterator state))
         ((or :record :next)
          (unless wconf (setq wconf (current-window-configuration)))
          (funcall iterator state))
         (_ (funcall iterator state)))))
   `((depth . ,(alist-get 'kapply-wconf conn--kapply-pipeline-depths))
     (name . kapply-wconf))))

;;;;; Applier Definitions

(defvar conn-kapply-suppress-message nil
  "Suppress message displayed after finishing a kapply.")

(defun conn--perform-kapply (iterator body)
  (let* ((undo-outer-limit nil)
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (conn-kmacro-applying-p t)
         (conn--kapply-automatic-flag nil)
         (iterations 0)
         (success nil)
         (iterator (lambda (&optional state)
                     (and (funcall iterator (or state :next))
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

(defmacro conn-define-kapplier (name arglist &rest body)
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

(conn-define-kapplier conn-kmacro-apply (iterator &optional count macro)
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

(conn-define-kapplier conn-kmacro-apply-append (iterator &optional count skip-exec)
  (when (funcall iterator :record)
    (kmacro-start-macro (if skip-exec '(16) '(4)))
    (unwind-protect
        (progn
          (recursive-edit)
          (when (not defining-kbd-macro)
            (user-error "Not defining keyboard macro")))
      (when defining-kbd-macro (kmacro-end-macro nil)))
    (kmacro-call-macro (or count 0))))

(conn-define-kapplier conn-kmacro-apply-step-edit (iterator &optional count)
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

;;;; Dispatch

(oclosure-define (conn-dispatch-kapply
                  (:parent conn-action))
  (macro :mutable t))

(defun conn-dispatch-kapply ()
  (declare (conn-dispatch-action t))
  (let ((setup (make-symbol "setup-dispatch-kapply"))
        (applier nil)
        (pipeline nil))
    (fset setup (lambda ()
                  (conn-without-recursive-stack
                    (conn-dispatch-kapply-prefix
                     (lambda (a p)
                       (setq applier a
                             pipeline p))))
                  (remove-hook 'post-command-hook setup)))
    (unwind-protect
        (progn
          (add-hook 'post-command-hook setup -99)
          (add-hook 'transient-post-exit-hook 'exit-recursive-edit)
          (recursive-edit))
      (remove-hook 'post-command-hook setup)
      (remove-hook 'transient-post-exit-hook 'exit-recursive-edit))
    (oclosure-lambda (conn-dispatch-kapply
                      (macro nil)
                      (action-auto-repeat t))
        ()
      (pcase-let* ((`(,pt ,window ,thing ,arg ,transform)
                    (conn-select-target))
                   (counter (if macro
                                (kmacro--counter macro)
                              kmacro-counter)))
        (with-selected-window window
          (conn-dispatch-change-group)
          (pcase (conn-bounds-of-dispatch thing arg pt)
            ((conn-bounds `(,beg . ,end) transform)
             (conn-dispatch-undo-case 50
               (:undo (conn-dispatch-undo-pulse beg end)))
             (with-undo-amalgamate
               (conn-with-dispatch-suspended
                 (let ((conn-kapply-suppress-message t))
                   (conn-kapply-macro
                    (pcase applier
                      ((or 'conn-kmacro-apply
                           (guard macro))
                       (lambda (iterator)
                         (conn-kmacro-apply iterator nil macro)))
                      (_ applier))
                    (conn-kapply-region-iterator
                     (list (conn-kapply-make-region beg end)))
                    `(conn-kapply-relocate-to-region
                      conn-kapply-pulse-region
                      ,@pipeline))))))
            (_ (user-error "Cannot find thing at point"))))
        (unless macro (setq macro (kmacro-ring-head)))
        (conn-dispatch-undo-case 0
          ((or :undo :cancel)
           (setf (kmacro--counter macro) counter)))))))

(cl-defmethod conn-dispatch-perform-action ((_action conn-dispatch-kapply)
                                            _repeat)
  (let ((conn-label-select-always-prompt t))
    (cl-call-next-method)
    (unless conn-kapply-suppress-message
      (message "Kapply completed successfully after %s iterations"
               conn-dispatch-iteration-count))))

(cl-defmethod conn-action-pretty-print ((action conn-dispatch-kapply)
                                        &optional
                                        short)
  (if short "Kapply"
    (concat "Kapply"
            (when-let* ((macro (oref action macro)))
              (concat " <"
                      (conn--kmacro-display (kmacro--keys macro))
                      ">")))))

(provide 'conn-kapply)
