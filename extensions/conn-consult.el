;;; conn-consult.el --- Conn consult extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") consult conn)
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
;;; Commentary:
;;
;;; Code:

(require 'conn)
(require 'consult)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defmacro conn--each-thing (thing beg end &rest body)
  "Iterate over each THING in buffer.

THING BEG and END are bound in BODY."
  (declare (indent 3))
  (cl-with-gensyms (max bounds)
    `(save-excursion
       (let ((,max (point-max)))
         (goto-char (point-min))
         (unless (bounds-of-thing-at-point ,thing)
           (forward-thing ,thing 1))
         (while (< (point) ,max)
           (let* ((,bounds (bounds-of-thing-at-point ,thing))
                  (,beg (car ,bounds))
                  (,end (cdr ,bounds)))
             ,@body
             (forward-thing ,thing 1)))))))

(defun conn--thing-candidates (thing)
  "Return list of thing candidates."
  (consult--forbid-minibuffer)
  (consult--fontify-all)
  (let* ((buffer (current-buffer))
         default-cand candidates)
    (conn--each-thing thing beg end
      (let ((line (line-number-at-pos)))
        (push (consult--location-candidate
               (consult--buffer-substring beg end)
               (cons buffer beg) line line)
              candidates))
      (when (not default-cand)
        (setq default-cand candidates)))
    (unless candidates
      (user-error "No lines"))
    (nreverse candidates)))

;;;###autoload
(defun conn-consult-ripgrep-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-ripgrep nil (funcall conn-completion-region-quote-function
                                (buffer-substring-no-properties beg end))))

;;;###autoload
(defun conn-consult-line-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-line (funcall conn-completion-region-quote-function
                         (buffer-substring-no-properties beg end))))

;;;###autoload
(defun conn-consult-line-multi-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-line-multi nil (funcall conn-completion-region-quote-function
                                   (buffer-substring-no-properties beg end))))

;;;###autoload
(defun conn-consult-locate-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-locate (funcall conn-completion-region-quote-function
                           (buffer-substring-no-properties beg end))))

;;;###autoload
(defun conn-consult-git-grep-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-git-grep nil (funcall conn-completion-region-quote-function
                                 (buffer-substring-no-properties beg end))))

;;;###autoload
(defun conn-consult-find-region (beg end)
  (interactive (list (region-beginning)
                     (region-end)))
  (consult-find nil (funcall conn-completion-region-quote-function
                             (buffer-substring-no-properties beg end))))

(defun conn-consult--isearch-matches (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (if isearch-forward (point-min) (point-max)))
      (cl-loop with bound = (if isearch-forward (point-max) (point-min))
               with case-fold-search = isearch-case-fold-search
               with cand-idx = 0
               while (isearch-search-string isearch-string bound t)
               when (funcall isearch-filter-predicate
                             (match-beginning 0) (match-end 0))
               collect (let ((line (line-number-at-pos (match-beginning 0) consult-line-numbers-widen)))
                         (save-excursion
                           (goto-char (match-beginning 0))
                           (consult--location-candidate
                            (consult--buffer-substring
                             (line-beginning-position)
                             (line-end-position)
                             'fontify)
                            (cons (current-buffer) (match-beginning 0))
                            line (prog1 cand-idx (cl-incf cand-idx)))))
               when (and (= (match-beginning 0) (match-end 0))
                         (not (if isearch-forward (eobp) (bobp))))
               do (forward-char (if isearch-forward 1 -1))))))

;;;###autoload
(defun conn-consult-isearch-matches ()
  (interactive)
  (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
         (candidates (consult--slow-operation "Collecting matches..."
                       (if (bound-and-true-p multi-isearch-buffer-list)
                           (mapcan 'conn-consult--isearch-matches multi-isearch-buffer-list)
                         (conn-consult--isearch-matches)))))
    (isearch-done)
    (consult--read
     candidates
     :prompt "Go to line: "
     :annotate (consult--line-prefix curr-line)
     :category 'consult-location
     :sort nil
     :require-match t
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-multi-history)
     :lookup #'consult--line-match
     :default (car candidates)
     :state (consult--location-state candidates)
     :group #'consult--line-multi-group)))

;;;###autoload
(defvar-keymap conn-consult-region-search-map
  :prefix 'conn-consult-region-search-map
  "o" 'conn-consult-line-region
  "O" 'conn-consult-line-multi-region
  "l" 'conn-consult-locate-region
  "i" 'conn-consult-git-grep-region
  "f" 'conn-consult-find-region
  "g" 'conn-consult-ripgrep-region)

(keymap-set conn-region-map "o" 'conn-consult-line-region)
(keymap-set conn-region-map "O" 'conn-consult-line-multi-region)
(keymap-set conn-region-map "g" 'conn-consult-ripgrep-region)
(keymap-set conn-region-map "h" 'conn-consult-region-search-map)
(keymap-set isearch-mode-map "M-s o" 'conn-consult-isearch-matches)
(keymap-global-set "M-s T" 'conn-consult-thing)

(provide 'conn-consult)

(with-eval-after-load 'embark
  (defvar embark-general-map)

  (defun conn-clone-indirect-buffer-location-candidate (cand)
    ;; Embark runs with inside a with-selected-window form
    ;; so we use a timer to get around it.
    (run-with-timer
     0 nil (lambda (marker)
             (clone-indirect-buffer nil t)
             (goto-char marker))
     (car (consult--get-location cand))))

  (defun conn-dot-consult-location-candidate (cand)
    (let ((marker (car (consult--get-location cand))))
      (goto-char marker)
      (unless (bolp) (beginning-of-line))
      (conn--create-dots (cons (point) (progn (end-of-line) (point))))))

  (defun conn-dot-consult-grep-candidate (cand)
    (let ((marker (car (consult--grep-position cand))))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (unless (bolp) (beginning-of-line))
        (conn--create-dots (cons (point) (progn (end-of-line) (point)))))))

  (defvar embark-multitarget-actions)
  (defvar embark-keymap-alist)

  (defun conn-kapply-grep-candidates (cands)
    (thread-last
      (mapcar (lambda (cand)
                (pcase-let ((`(,line-pos (,beg . ,end) . _)
                             (consult--grep-position cand)))
                  (cons (+ line-pos beg) (+ line-pos end))))
              cands)
      (apply-partially 'conn--kapply-region-iterator)
      (funcall-interactively 'conn-regions-kapply-prefix)))
  (add-to-list 'embark-multitarget-actions 'conn-kapply-grep-candidates)

  (defun conn-kapply-location-candidates (cands)
    (funcall-interactively
     'conn-regions-kapply-prefix
     (let ((lines (mapcar (lambda (cand)
                            (car (consult--get-location cand)))
                          cands)))
       (lambda (reverse)
         (when reverse (setq lines (nreverse lines)))
         (lambda (&optional state)
           (if (eq state :finalize)
               (dolist (line lines)
                 (set-marker line nil))
             (when-let ((line (pop lines)))
               (cons line (save-excursion
                            (goto-char line)
                            (conn--create-marker (line-end-position)))))))))))
  (add-to-list 'embark-multitarget-actions 'conn-kapply-location-candidates)

  (defvar-keymap conn-embark-consult-location-map
    :parent embark-general-map
    "\\" 'conn-kapply-location-candidates
    "D"   'conn-dot-consult-location-candidate
    "c"   '("clone-indirect-buffer" .
            conn-clone-indirect-buffer-location-candidate))
  (cl-pushnew 'conn-embark-consult-location-map
              (alist-get 'consult-location embark-keymap-alist))

  (defvar-keymap conn-embark-consult-grep-map
    :parent embark-general-map
    "\\" 'conn-kapply-grep-candidates
    "D"   'conn-dot-consult-grep-candidate)
  (cl-pushnew 'conn-embark-consult-grep-map
              (alist-get 'consult-grep embark-keymap-alist)))
;;; conn-consult.el ends here
