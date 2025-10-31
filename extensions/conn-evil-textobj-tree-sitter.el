;;; conn-evil-textobj-tree-sitter.el --- Evil Treesit Text Objects -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (compat "30.0.2.0") (evil-textobj-tree-sitter "0.5") conn)
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
;;
;;; Code:

(require 'conn)
(require 'conn-things)
(require 'conn-dispatch)
(require 'evil-textobj-tree-sitter)
(require 'mule-util)

(cl-defstruct (conn--etts-thing
               (:constructor conn--make-etts-thing))
  (groups nil :type (or cons symbol) :read-only t)
  (query nil :type list :read-only t))

(defvar conn-etts--chunk-size 10000)

(defconst conn-etts--query-cache
  (make-hash-table :test 'eq))

(defun conn-etts--get-query ()
  (with-memoization (gethash (treesit-language-at (point)) conn-etts--query-cache)
    (treesit-query-compile
     (treesit-language-at (point))
     (evil-textobj-tree-sitter--get-query-from-dir
      (symbol-name (treesit-language-at (point)))
      (file-name-as-directory (concat evil-textobj-tree-sitter--dir "queries"))
      t))))

(defun conn-etts--get-captures (thing start end)
  (let ((f-query (or (alist-get major-mode (conn--etts-thing-query thing))
                     (conn-etts--get-query))))
    (if (< start end)
        (treesit-query-capture (treesit-buffer-root-node)
                               f-query start end nil t)
      (nreverse
       (treesit-query-capture (treesit-buffer-root-node)
                              f-query end start nil t)))))

(defun conn-etts--filter-captures (groups captures)
  (let (regions)
    (dolist (capture captures)
      (cl-callf nreverse capture)
      (pcase-dolist (`(,group ,tbeg . ,tend) groups)
        (if-let* ((beg (alist-get tbeg capture)))
            (when-let* ((end (alist-get tend capture)))
              (push (cons (treesit-node-start beg)
                          (treesit-node-end end))
                    regions))
          (when-let* ((node (alist-get group capture)))
            (push (cons (treesit-node-start node)
                        (treesit-node-end node))
                  regions)))))
    regions))

(cl-defmethod conn-bounds-of ((cmd (conn-thing conn-etts-thing)) arg
                              &key flat)
  (setq arg (prefix-numeric-value arg))
  (unless (= 0 arg)
    (let* ((seen (when (> arg 100)
                   (make-hash-table :test 'equal :size arg)))
           (thing (get (or (get cmd :conn-command-thing) cmd)
                       :conn-etts-thing))
           (groups (conn--etts-thing-groups thing))
           (at (point))
           (beg (point))
           (end nil)
           (captures nil)
           (count 0)
           (nodes nil)
           (max most-negative-fixnum)
           (min most-positive-fixnum))
      (catch 'done
        (while (if (< arg 0)
                   (> beg (point-min))
                 (< beg (point-max)))
          (setq end (if (< arg 0)
                        (max (point-min) (- beg conn-etts--chunk-size))
                      (min (point-max) (+ beg conn-etts--chunk-size)))
                captures (conn-etts--get-captures thing beg end)
                beg end)
          (let ((pending (conn-etts--filter-captures groups captures)))
            (if flat
                (progn
                  (cl-callf sort pending
                    :key #'car
                    :reverse (< arg 0)
                    :in-place t)
                  (dolist (node pending)
                    (when (and (if (< arg 0)
                                   (< (car node) at)
                                 (> (car node) at))
                               (if seen
                                   (or (not (gethash node seen))
                                       (setf (gethash node seen) t))
                                 (not (member node nodes))))
                      (cl-callf max max (cdr node))
                      (cl-callf min min (car node))
                      (push (conn-make-bounds cmd 1 node) nodes)
                      (setq at (car node))
                      (when (= (cl-incf count) (abs arg))
                        (throw 'done nil)))))
              (cl-callf sort pending
                :key (if (< arg 0)
                         (lambda (n)
                           (if (>= at (cdr n)) (cdr n) (car n)))
                       (lambda (n)
                         (if (<= at (car n)) (car n) (cdr n))))
                :reverse (< arg 0)
                :in-place t)
              (dolist (node pending)
                (when (and (if (< arg 0)
                               (< (car node) at)
                             (> (cdr node) at))
                           (if seen
                               (or (not (gethash node seen))
                                   (setf (gethash node seen) t))
                             (not (member node nodes))))
                  (cl-callf max max (cdr node))
                  (cl-callf min min (car node))
                  (push (conn-make-bounds cmd 1 node) nodes)
                  (setq at (if (< arg 0) (car node) (cdr node)))
                  (when (= (cl-incf count) (abs arg))
                    (throw 'done nil))))))))
      (when nodes
        (conn-make-bounds
         thing arg
         (cons min max)
         :subregions (nreverse nodes))))))

(conn-register-thing 'conn-etts-thing)

(conn-define-state conn-etts-expand-state (conn-mode-line-face-state)
  "State for selecting a tree sit node."
  :lighter "NODE"
  :mode-line-face 'conn-read-thing-mode-line-face)

(define-keymap
  :keymap (conn-get-state-map 'conn-etts-expand-state)
  "l" 'conn-expand
  "j" 'conn-contract
  "e" 'end)

(defun conn-etts-select-node (nodes)
  (pcase nodes
    ('nil)
    (`((,_ ,beg . ,end) . nil)
     (conn-make-bounds
      'conn-etts-thing nil
      (cons beg end)))
    (_
     (save-mark-and-excursion
       (cl-callf sort nodes
         :key (pcase-lambda (`(,_ ,beg . ,end))
                (- end beg)))
       (pcase-exhaustive (car nodes)
         (`(,_ ,beg . ,end)
          (goto-char end)
          (conn--push-ephemeral-mark beg)
          (activate-mark)))
       (cl-flet ((display-handler (prompt _args)
                   (message
                    (substitute-command-keys
                     (concat
                      (propertize prompt 'face 'minibuffer-prompt)
                      " ("
                      (propertize (format "%s" (caar nodes))
                                  'face 'eldoc-highlight-function-argument)
                      "; "
                      (concat
                       "\\[conn-expand] expand; "
                       "\\[conn-contract] contract; "
                       "\\[end] finish")
                      "): "
                      (conn--read-args-display-message))))))
         (conn-read-args (conn-etts-expand-state
                          :prompt "Node"
                          :display-handler #'display-handler
                          :around (lambda (cont)
                                    (conn-with-dispatch-suspended
                                      (funcall cont))))
             ((bounds
               (oclosure-lambda (conn-read-args-argument
                                 (required t))
                   (self command)
                 (pcase command
                   ('conn-contract
                    (setq nodes (nconc (last nodes) (butlast nodes)))
                    (pcase (car nodes)
                      (`(,_ ,beg . ,end)
                       (goto-char end)
                       (conn--push-ephemeral-mark beg)))
                    (conn-read-args-handle)
                    self)
                   ('conn-expand
                    (setq nodes (nconc (cdr nodes) (list (car nodes))))
                    (pcase (car nodes)
                      (`(,_ ,beg . ,end)
                       (goto-char end)
                       (conn--push-ephemeral-mark beg)))
                    (conn-read-args-handle)
                    self)
                   ((or 'end 'exit-recursive-edit)
                    (conn-set-argument
                     self
                     (cons (region-beginning) (region-end))))
                   (_ self)))))
           (conn-make-bounds 'conn-etts-thing nil bounds)))))))

(defvar conn-etts-parent-things
  `(conn-etts-assignment-inner
    conn-etts-assignment-outer
    conn-etts-assignment-side
    conn-etts-attribute-inner
    conn-etts-attribute-outer
    conn-etts-block-inner
    conn-etts-block-outer
    conn-etts-call-inner
    conn-etts-call-outer
    conn-etts-class-inner
    conn-etts-class-outer
    conn-etts-comment-inner
    conn-etts-comment-outer
    conn-etts-conditional-inner
    conn-etts-conditional-outer
    conn-etts-frame-inner
    conn-etts-frame-outer
    conn-etts-function-inner
    conn-etts-function-outer
    conn-etts-loop-inner
    conn-etts-loop-outer
    conn-etts-number
    conn-etts-parameter-inner
    conn-etts-parameter-outer
    conn-etts-regex-inner
    conn-etts-regex-outer
    conn-etts-return-inner
    conn-etts-return-outer
    conn-etts-scopename))

(defvar conn-etts-all-things conn-etts-parent-things)

(defclass conn-etts-parents-targets (conn-dispatch-target-window-predicate)
  ((things :initarg :things)
   (window-predicate
    :initform (lambda (win) (eq win (selected-window))))))

(cl-defmethod conn-dispatch-update-targets ((state conn-etts-parents-targets))
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (pcase-dolist (`(,vbeg . ,vend)
                     (conn--visible-regions (window-start)
                                            (window-end)
                                            t))
        (let ((truncate-string-ellipsis nil)
              (captures
               (treesit-query-capture (treesit-buffer-root-node)
                                      (conn-etts--get-query)
                                      vbeg vend nil t))
              (groups
               (cl-loop for thing in (oref state things)
                        append (conn--etts-thing-groups
                                (get thing :conn-etts-thing))))
              (beg nil)
              (end nil))
          (dolist (capture captures)
            (cl-callf nreverse capture)
            (pcase-dolist (`(,group ,tbeg . ,tend) groups)
              (if-let* ((nbeg (alist-get tbeg capture)))
                  (when-let* ((nend (alist-get tend capture))
                              (_(and (<= (treesit-node-start nbeg) (point))
                                     (<= (point) (treesit-node-end nend)))))
                    (setq beg (treesit-node-start nbeg)
                          end (treesit-node-end nend)))
                (when-let* ((n (alist-get group capture))
                            (_(and (<= (treesit-node-start n) (point))
                                   (<= (point) (treesit-node-end n)))))
                  (setq beg (treesit-node-start n)
                        end (treesit-node-end n))))
              (when (and beg
                         (<= (window-start) beg (window-end))
                         (conn--region-visible-p beg (1+ beg)))
                (if-let* ((ov (car (conn--overlays-in-of-type
                                    beg (1+ beg) 'conn-target-overlay))))
                    (when (length= (cl-pushnew (cons group (cons beg end))
                                               (conn-anonymous-thing-property
                                                (overlay-get ov 'thing)
                                                :nodes)
                                               :key #'cdr
                                               :test #'equal)
                                   2)
                      (overlay-put ov 'label-suffix (truncate-string-ellipsis)))
                  (overlay-put
                   (conn-make-target-overlay beg 0)
                   'thing (conn-anonymous-thing
                            'conn-etts-thing
                            :nodes (list (cons group (cons beg end)))
                            :bounds-op ( :method (self _arg)
                                         (thread-first
                                           self
                                           (conn-anonymous-thing-property :nodes)
                                           (conn-etts-select-node)))))))))))))
  (cl-call-next-method))

(defclass conn-etts-node-targets (conn-dispatch-target-window-predicate)
  ((things :initarg :things)
   (window-predicate
    :initform (lambda (win) (eq win (selected-window))))))

(cl-defmethod conn-dispatch-update-targets ((state conn-etts-node-targets))
  (dolist (win (conn--get-target-windows))
    (with-selected-window win
      (pcase-dolist (`(,vbeg . ,vend)
                     (conn--visible-regions (window-start)
                                            (window-end)
                                            t))
        (let ((truncate-string-ellipsis nil)
              (captures
               (treesit-query-capture (treesit-buffer-root-node)
                                      (conn-etts--get-query)
                                      vbeg vend nil t))
              (groups
               (cl-loop for thing in (oref state things)
                        append (conn--etts-thing-groups
                                (get thing :conn-etts-thing))))
              (beg nil)
              (end nil))
          (dolist (capture captures)
            (cl-callf nreverse capture)
            (pcase-dolist (`(,group ,tbeg . ,tend) groups)
              (if-let* ((nbeg (alist-get tbeg capture)))
                  (when-let* ((nend (alist-get tend capture)))
                    (setq beg (treesit-node-start nbeg)
                          end (treesit-node-end nend)))
                (when-let* ((n (alist-get group capture)))
                  (setq beg (treesit-node-start n)
                        end (treesit-node-end n))))
              (when (and beg
                         (<= (window-start) beg (window-end))
                         (conn--region-visible-p beg (1+ beg)))
                (if-let* ((ov (car (conn--overlays-in-of-type
                                    beg (1+ beg) 'conn-target-overlay))))
                    (when (length= (cl-pushnew (cons group (cons beg end))
                                               (conn-anonymous-thing-property
                                                (overlay-get ov 'thing)
                                                :nodes)
                                               :key #'cdr
                                               :test #'equal)
                                   2)
                      (overlay-put ov 'label-suffix (truncate-string-ellipsis)))
                  (overlay-put
                   (conn-make-target-overlay beg 0)
                   'thing (conn-anonymous-thing
                            'conn-etts-thing
                            :nodes (list (cons group (cons beg end)))
                            :bounds-op ( :method (self _arg)
                                         (thread-first
                                           self
                                           (conn-anonymous-thing-property :nodes)
                                           (conn-etts-select-node)))))))))))))
  (cl-call-next-method))

(cl-defmethod conn-get-target-finder ((_cmd (eql conn-etts-all-nodes))
                                      _arg)
  (conn-etts-node-targets
   :things conn-etts-all-things))

(cl-defmethod conn-get-target-finder ((cmd (conn-thing conn-etts-thing))
                                      _arg)
  (conn-etts-node-targets
   :things (list (or (get cmd :conn-command-thing) cmd))))

(defmacro conn-etts-define-thing (name group &optional query)
  (declare (indent defun))
  (let ((forward-cmd (intern (format "%s-forward" name)))
        (forward-flat-cmd (intern (format "%s-forward-flat" name)))
        (backward-cmd (intern (format "%s-backward" name)))
        (backward-flat-cmd (intern (format "%s-backward-flat" name)))
        (groups (cl-loop
                 for group in (ensure-list group)
                 collect (cons (intern group)
                               (cons (intern (format "%s._start" group))
                                     (intern (format "%s._end" group)))))))
    `(progn
       (put ',name
            :conn-etts-thing (conn--make-etts-thing
                              :groups ',groups
                              :query ,(macroexp-quote query)))

       (conn-register-thing ',name
                            :parent 'conn-etts-thing
                            :forward-op ',forward-cmd)

       (defun ,forward-cmd (&optional arg)
         (interactive "p")
         (pcase (conn-bounds-of ',name arg)
           ((conn-bounds `(,beg . ,end))
            (goto-char (if (> arg 0) end beg))
            (unless (region-active-p)
              (conn--push-ephemeral-mark (if (> arg 0) beg end))))
           (_ (signal 'scan-error
                      (list (format-message "No more %S to move across" ',name)
                            (point) (point))))))
       (put ',forward-cmd 'repeat-check-key 'no)

       (defun ,backward-cmd (&optional arg)
         (interactive "p")
         (,forward-cmd (- arg)))
       (put ',backward-cmd 'repeat-check-key 'no)

       (defun ,forward-flat-cmd (&optional arg)
         (interactive "p")
         (pcase (thread-first
                  (conn-bounds-of ',name arg :flat t)
                  (conn-bounds-get :subregions)
                  last car)
           ((conn-bounds `(,beg . ,end))
            (goto-char beg)
            (conn--push-ephemeral-mark end))
           (_ (signal 'scan-error
                      (list (format-message "No more %S to move across" ',name)
                            (point) (point))))))
       (put ',forward-flat-cmd 'repeat-check-key 'no)

       (defun ,backward-flat-cmd (&optional arg)
         (interactive "p")
         (,forward-flat-cmd (- arg)))
       (put ',backward-flat-cmd 'repeat-check-key 'no)

       (conn-register-thing-commands
        ',name 'ignore
        ',forward-cmd
        ',backward-cmd
        ',forward-flat-cmd
        ',backward-flat-cmd))))

(conn-etts-define-thing conn-etts-assignment-inner "assignment.inner")
(conn-etts-define-thing conn-etts-assignment-outer "assignment.outer")
(conn-etts-define-thing conn-etts-assignment-side ("assignment.lhs" "assignment.rhs"))
(conn-etts-define-thing conn-etts-attribute-inner "attribute.inner")
(conn-etts-define-thing conn-etts-attribute-outer "attribute.outer")
(conn-etts-define-thing conn-etts-block-inner "block.inner")
(conn-etts-define-thing conn-etts-block-outer "block.outer")
(conn-etts-define-thing conn-etts-call-inner "call.inner")
(conn-etts-define-thing conn-etts-call-outer "call.outer")
(conn-etts-define-thing conn-etts-class-inner "class.inner")
(conn-etts-define-thing conn-etts-class-outer "class.outer")
(conn-etts-define-thing conn-etts-comment-inner "comment.inner")
(conn-etts-define-thing conn-etts-comment-outer "comment.outer")
(conn-etts-define-thing conn-etts-conditional-inner "conditional.inner")
(conn-etts-define-thing conn-etts-conditional-outer "conditional.outer")
(conn-etts-define-thing conn-etts-frame-inner "frame.inner")
(conn-etts-define-thing conn-etts-frame-outer "frame.outer")
(conn-etts-define-thing conn-etts-function-inner "function.inner")
(conn-etts-define-thing conn-etts-function-outer "function.outer")
(conn-etts-define-thing conn-etts-loop-inner "loop.inner")
(conn-etts-define-thing conn-etts-loop-outer "loop.outer")
(conn-etts-define-thing conn-etts-number "number.inner")
(conn-etts-define-thing conn-etts-parameter-inner "parameter.inner")
(conn-etts-define-thing conn-etts-parameter-outer "parameter.outer")
(conn-etts-define-thing conn-etts-regex-inner "regex.inner")
(conn-etts-define-thing conn-etts-regex-outer "regex.outer")
(conn-etts-define-thing conn-etts-return-inner "return.inner")
(conn-etts-define-thing conn-etts-return-outer "return.outer")
(conn-etts-define-thing conn-etts-scopename "scopename.inner")
;; (conn-etts-define-thing conn-etts-statement "statement.outer")

(defvar-keymap conn-etts-assignment-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "m" 'conn-etts-assignment-side-forward
  "n" 'conn-etts-assignment-side-backward
  "M" 'conn-etts-assignment-side-forward-flat
  "N" 'conn-etts-assignment-side-backward-flat
  "l" 'conn-etts-assignment-inner-forward
  "j" 'conn-etts-assignment-inner-backward
  "k" 'conn-etts-assignment-inner-forward-flat
  "i" 'conn-etts-assignment-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-assignment-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "m" 'conn-etts-assignment-side-forward
  "n" 'conn-etts-assignment-side-backward
  "M" 'conn-etts-assignment-side-forward-flat
  "N" 'conn-etts-assignment-side-backward-flat
  "l" 'conn-etts-assignment-outer-forward
  "j" 'conn-etts-assignment-outer-backward
  "k" 'conn-etts-assignment-outer-forward-flat
  "i" 'conn-etts-assignment-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-attribute-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-attribute-inner-forward
  "j" 'conn-etts-attribute-inner-backward
  "k" 'conn-etts-attribute-inner-forward-flat
  "i" 'conn-etts-attribute-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-attribute-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-attribute-outer-forward
  "j" 'conn-etts-attribute-outer-backward
  "k" 'conn-etts-attribute-outer-forward-flat
  "i" 'conn-etts-attribute-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-block-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-block-inner-forward
  "j" 'conn-etts-block-inner-backward
  "k" 'conn-etts-block-inner-forward-flat
  "i" 'conn-etts-block-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-block-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-block-outer-forward
  "j" 'conn-etts-block-outer-backward
  "k" 'conn-etts-block-outer-forward-flat
  "i" 'conn-etts-block-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-call-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-call-inner-forward
  "j" 'conn-etts-call-inner-backward
  "k" 'conn-etts-call-inner-forward-flat
  "i" 'conn-etts-call-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-call-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-call-outer-forward
  "j" 'conn-etts-call-outer-backward
  "k" 'conn-etts-call-outer-forward-flat
  "i" 'conn-etts-call-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-class-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-class-inner-forward
  "j" 'conn-etts-class-inner-backward
  "k" 'conn-etts-class-inner-forward-flat
  "i" 'conn-etts-class-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-class-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-class-outer-forward
  "j" 'conn-etts-class-outer-backward
  "k" 'conn-etts-class-outer-forward-flat
  "i" 'conn-etts-class-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-comment-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-comment-inner-forward
  "j" 'conn-etts-comment-inner-backward
  "k" 'conn-etts-comment-inner-forward-flat
  "i" 'conn-etts-comment-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-comment-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-comment-outer-forward
  "j" 'conn-etts-comment-outer-backward
  "k" 'conn-etts-comment-outer-forward-flat
  "i" 'conn-etts-comment-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-conditional-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-conditional-inner-forward
  "j" 'conn-etts-conditional-inner-backward
  "k" 'conn-etts-conditional-inner-forward-flat
  "i" 'conn-etts-conditional-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-conditional-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-conditional-outer-forward
  "j" 'conn-etts-conditional-outer-backward
  "k" 'conn-etts-conditional-outer-forward-flat
  "i" 'conn-etts-conditional-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-frame-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-frame-inner-forward
  "j" 'conn-etts-frame-inner-backward
  "k" 'conn-etts-frame-inner-forward-flat
  "i" 'conn-etts-frame-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-frame-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-frame-outer-forward
  "j" 'conn-etts-frame-outer-backward
  "k" 'conn-etts-frame-outer-forward-flat
  "i" 'conn-etts-frame-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-function-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-function-inner-forward
  "j" 'conn-etts-function-inner-backward
  "k" 'conn-etts-function-inner-forward-flat
  "i" 'conn-etts-function-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-function-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-function-outer-forward
  "j" 'conn-etts-function-outer-backward
  "k" 'conn-etts-function-outer-forward-flat
  "i" 'conn-etts-function-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-loop-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-loop-inner-forward
  "j" 'conn-etts-loop-inner-backward
  "k" 'conn-etts-loop-inner-forward-flat
  "i" 'conn-etts-loop-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-loop-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-loop-outer-forward
  "j" 'conn-etts-loop-outer-backward
  "k" 'conn-etts-loop-outer-forward-flat
  "i" 'conn-etts-loop-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-number-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-number-forward
  "j" 'conn-etts-number-backward
  "k" 'conn-etts-number-forward-flat
  "i" 'conn-etts-number-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-parameter-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-parameter-inner-forward
  "j" 'conn-etts-parameter-inner-backward
  "k" 'conn-etts-parameter-inner-forward-flat
  "i" 'conn-etts-parameter-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-parameter-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-parameter-outer-forward
  "j" 'conn-etts-parameter-outer-backward
  "k" 'conn-etts-parameter-outer-forward-flat
  "i" 'conn-etts-parameter-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-regex-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-regex-inner-forward
  "j" 'conn-etts-regex-inner-backward
  "k" 'conn-etts-regex-inner-forward-flat
  "i" 'conn-etts-regex-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-regex-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-regex-outer-forward
  "j" 'conn-etts-regex-outer-backward
  "k" 'conn-etts-regex-outer-forward-flat
  "i" 'conn-etts-regex-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-return-inner-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-return-inner-forward
  "j" 'conn-etts-return-inner-backward
  "k" 'conn-etts-return-inner-forward-flat
  "i" 'conn-etts-return-inner-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-return-outer-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-return-outer-forward
  "j" 'conn-etts-return-outer-backward
  "k" 'conn-etts-return-outer-forward-flat
  "i" 'conn-etts-return-outer-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-scopename-repeat-map
  :repeat ( :continue (conn-toggle-mark-command
                       conn-set-mark-command
                       conn-exchange-mark-command)
            :exit (ignore
                   conn-toggle-mark-command
                   conn-set-mark-command
                   conn-exchange-mark-command))
  "l" 'conn-etts-scopename-forward
  "j" 'conn-etts-scopename-backward
  "k" 'conn-etts-scopename-forward-flat
  "i" 'conn-etts-scopename-backward-flat
  "v" 'conn-toggle-mark-command
  "b" 'conn-set-mark-command
  "z" 'conn-exchange-mark-command
  "e" 'ignore)

(defvar-keymap conn-etts-things-mode-map
  "<conn-thing-map> i w l" 'conn-etts-assignment-inner-forward
  "<conn-thing-map> i w j" 'conn-etts-assignment-inner-backward
  "<conn-thing-map> i w k" 'conn-etts-assignment-inner-forward-flat
  "<conn-thing-map> i w i" 'conn-etts-assignment-inner-backward-flat
  "<conn-thing-map> w m" 'conn-etts-assignment-side-forward
  "<conn-thing-map> w n" 'conn-etts-assignment-side-backward
  "<conn-thing-map> w l" 'conn-etts-assignment-outer-forward
  "<conn-thing-map> w j" 'conn-etts-assignment-outer-backward
  "<conn-thing-map> w k" 'conn-etts-assignment-outer-forward-flat
  "<conn-thing-map> w i" 'conn-etts-assignment-outer-backward-flat
  "<conn-thing-map> i @ l" 'conn-etts-attribute-inner-forward
  "<conn-thing-map> i @ j" 'conn-etts-attribute-inner-backward
  "<conn-thing-map> i @ k" 'conn-etts-attribute-inner-forward-flat
  "<conn-thing-map> i @ i" 'conn-etts-attribute-inner-backward-flat
  "<conn-thing-map> @ l" 'conn-etts-attribute-outer-forward
  "<conn-thing-map> @ j" 'conn-etts-attribute-outer-backward
  "<conn-thing-map> @ k" 'conn-etts-attribute-outer-forward-flat
  "<conn-thing-map> @ i" 'conn-etts-attribute-outer-backward-flat
  "<conn-thing-map> i b l" 'conn-etts-block-inner-forward
  "<conn-thing-map> i b j" 'conn-etts-block-inner-backward
  "<conn-thing-map> i b k" 'conn-etts-block-inner-forward-flat
  "<conn-thing-map> i b i" 'conn-etts-block-inner-backward-flat
  "<conn-thing-map> b l" 'conn-etts-block-outer-forward
  "<conn-thing-map> b j" 'conn-etts-block-outer-backward
  "<conn-thing-map> b k" 'conn-etts-block-outer-forward-flat
  "<conn-thing-map> b i" 'conn-etts-block-outer-backward-flat
  "<conn-thing-map> i . l" 'conn-etts-call-inner-forward
  "<conn-thing-map> i . j" 'conn-etts-call-inner-backward
  "<conn-thing-map> i . k" 'conn-etts-call-inner-forward-flat
  "<conn-thing-map> i . i" 'conn-etts-call-inner-backward-flat
  "<conn-thing-map> . l" 'conn-etts-call-outer-forward
  "<conn-thing-map> . j" 'conn-etts-call-outer-backward
  "<conn-thing-map> . k" 'conn-etts-call-outer-forward-flat
  "<conn-thing-map> . i" 'conn-etts-call-outer-backward-flat
  "<conn-thing-map> i C l" 'conn-etts-class-inner-forward
  "<conn-thing-map> i C j" 'conn-etts-class-inner-backward
  "<conn-thing-map> i C k" 'conn-etts-class-inner-forward-flat
  "<conn-thing-map> i C i" 'conn-etts-class-inner-backward-flat
  "<conn-thing-map> C l" 'conn-etts-class-outer-forward
  "<conn-thing-map> C j" 'conn-etts-class-outer-backward
  "<conn-thing-map> C k" 'conn-etts-class-outer-forward-flat
  "<conn-thing-map> C i" 'conn-etts-class-outer-backward-flat
  "<conn-thing-map> i c l" 'conn-etts-comment-inner-forward
  "<conn-thing-map> i c j" 'conn-etts-comment-inner-backward
  "<conn-thing-map> i c k" 'conn-etts-comment-inner-forward-flat
  "<conn-thing-map> i c i" 'conn-etts-comment-inner-backward-flat
  "<conn-thing-map> c l" 'conn-etts-comment-outer-forward
  "<conn-thing-map> c j" 'conn-etts-comment-outer-backward
  "<conn-thing-map> c k" 'conn-etts-comment-outer-forward-flat
  "<conn-thing-map> c i" 'conn-etts-comment-outer-backward-flat
  "<conn-thing-map> i q l" 'conn-etts-conditional-inner-forward
  "<conn-thing-map> i q j" 'conn-etts-conditional-inner-backward
  "<conn-thing-map> i q k" 'conn-etts-conditional-inner-forward-flat
  "<conn-thing-map> i q i" 'conn-etts-conditional-inner-backward-flat
  "<conn-thing-map> q l" 'conn-etts-conditional-outer-forward
  "<conn-thing-map> q j" 'conn-etts-conditional-outer-backward
  "<conn-thing-map> q k" 'conn-etts-conditional-outer-forward-flat
  "<conn-thing-map> q i" 'conn-etts-conditional-outer-backward-flat
  "<conn-thing-map> i [ l" 'conn-etts-frame-inner-forward
  "<conn-thing-map> i [ j" 'conn-etts-frame-inner-backward
  "<conn-thing-map> i [ k" 'conn-etts-frame-inner-forward-flat
  "<conn-thing-map> i [ i" 'conn-etts-frame-inner-backward-flat
  "<conn-thing-map> [ l" 'conn-etts-frame-outer-forward
  "<conn-thing-map> [ j" 'conn-etts-frame-outer-backward
  "<conn-thing-map> [ k" 'conn-etts-frame-outer-forward-flat
  "<conn-thing-map> [ i" 'conn-etts-frame-outer-backward-flat
  "<conn-thing-map> i f l" 'conn-etts-function-inner-forward
  "<conn-thing-map> i f j" 'conn-etts-function-inner-backward
  "<conn-thing-map> i f k" 'conn-etts-function-inner-forward-flat
  "<conn-thing-map> i f i" 'conn-etts-function-inner-backward-flat
  "<conn-thing-map> f l" 'conn-etts-function-outer-forward
  "<conn-thing-map> f j" 'conn-etts-function-outer-backward
  "<conn-thing-map> f k" 'conn-etts-function-outer-forward-flat
  "<conn-thing-map> f i" 'conn-etts-function-outer-backward-flat
  "<conn-thing-map> i r l" 'conn-etts-loop-inner-forward
  "<conn-thing-map> i r j" 'conn-etts-loop-inner-backward
  "<conn-thing-map> i r k" 'conn-etts-loop-inner-forward-flat
  "<conn-thing-map> i r i" 'conn-etts-loop-inner-backward-flat
  "<conn-thing-map> r l" 'conn-etts-loop-outer-forward
  "<conn-thing-map> r j" 'conn-etts-loop-outer-backward
  "<conn-thing-map> r k" 'conn-etts-loop-outer-forward-flat
  "<conn-thing-map> r i" 'conn-etts-loop-outer-backward-flat
  "<conn-thing-map> n l" 'conn-etts-number-forward
  "<conn-thing-map> n j" 'conn-etts-number-backward
  "<conn-thing-map> i d l" 'conn-etts-parameter-inner-forward
  "<conn-thing-map> i d j" 'conn-etts-parameter-inner-backward
  "<conn-thing-map> i d k" 'conn-etts-parameter-inner-forward-flat
  "<conn-thing-map> i d i" 'conn-etts-parameter-inner-backward-flat
  "<conn-thing-map> d l" 'conn-etts-parameter-outer-forward
  "<conn-thing-map> d j" 'conn-etts-parameter-outer-backward
  "<conn-thing-map> d k" 'conn-etts-parameter-outer-forward-flat
  "<conn-thing-map> d i" 'conn-etts-parameter-outer-backward-flat
  "<conn-thing-map> i x l" 'conn-etts-regex-inner-forward
  "<conn-thing-map> i x j" 'conn-etts-regex-inner-backward
  "<conn-thing-map> i x k" 'conn-etts-regex-inner-forward-flat
  "<conn-thing-map> i x i" 'conn-etts-regex-inner-backward-flat
  "<conn-thing-map> x l" 'conn-etts-regex-outer-forward
  "<conn-thing-map> x j" 'conn-etts-regex-outer-backward
  "<conn-thing-map> x k" 'conn-etts-regex-outer-forward-flat
  "<conn-thing-map> x i" 'conn-etts-regex-outer-backward-flat
  "<conn-thing-map> i t l" 'conn-etts-return-inner-forward
  "<conn-thing-map> i t j" 'conn-etts-return-inner-backward
  "<conn-thing-map> i t k" 'conn-etts-return-inner-forward-flat
  "<conn-thing-map> i t i" 'conn-etts-return-inner-backward-flat
  "<conn-thing-map> t l" 'conn-etts-return-outer-forward
  "<conn-thing-map> t j" 'conn-etts-return-outer-backward
  "<conn-thing-map> t k" 'conn-etts-return-outer-forward-flat
  "<conn-thing-map> t i" 'conn-etts-return-outer-backward-flat
  "<conn-thing-map> S l" 'conn-etts-scopename-forward
  "<conn-thing-map> S j" 'conn-etts-scopename-backward)

(define-minor-mode conn-etts-things-mode
  "Minor mode for conn-etts things")

(define-keymap
  :keymap (conn-get-minor-mode-map 'conn-dispatch-targets-state 'conn-etts-things-mode)
  "w" (conn-anonymous-thing
        'conn-etts-thing
        :target-finder ( :method (_self _arg)
                         (conn-etts-node-targets
                          :things conn-etts-all-things)))
  "h" (conn-anonymous-thing
        'conn-etts-thing
        :target-finder ( :method (_self _arg)
                         (conn-etts-parents-targets
                          :things conn-etts-parent-things))))

;; fix etts comment thing overriding ours
(conn-register-thing
 'comment
 :bounds-op (lambda ()
              (if (conn--point-in-comment-p)
                  (cons (save-excursion
                          (while (and (conn--point-in-comment-p)
                                      (not (eobp)))
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r"))
                          (skip-chars-backward " \t\n\r")
                          (point))
                        (save-excursion
                          (while (conn--point-in-comment-p)
                            (forward-char -1)
                            (skip-chars-backward " \t\n\r"))
                          (skip-chars-forward " \t\n\r")
                          (unless (conn--point-in-comment-p)
                            (forward-char 1))
                          (point)))
                (error "Point not in comment"))))

(provide 'conn-evil-textobj-tree-sitter)
