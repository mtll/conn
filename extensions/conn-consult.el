;;; conn-consult.el --- Conn consult extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") consult conn-mode)
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

(require 'conn-mode)
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
(defun conn-consult-thing (&optional initial thing)
  "Search for a matching top-level THING."
  (interactive (list nil
                     (intern
                      (completing-read
                       (format "Thing: ")
                       (conn--things 'conn--defined-thing-p) nil nil nil
                       'conn-thing-history))
                     (not (not current-prefix-arg))))
  (let* ((candidates (consult--slow-operation "Collecting things..."
                       (conn--thing-candidates thing))))
    (consult--read
     candidates
     :prompt "Goto thing: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last isearch string to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     ;; :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add isearch-string as initial input if starting from isearch
     :initial (or initial
                  (and isearch-mode
                       (prog1 isearch-string (isearch-done))))
     :state (consult--location-state candidates))))

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
(keymap-set conn-region-map "g" 'conn-consult-ripgrep-region)
(keymap-set conn-region-map "h" 'conn-consult-region-search-map)
(keymap-set conn-global-map "M-s T" 'conn-consult-thing)

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

  (defun conn-dispatch-grep-candidates (cands)
    (conn-regions-dispatch-menu
     (mapcar (lambda (cand)
               (pcase-let ((`(,line-pos (,beg . ,end) . _)
                            (consult--grep-position cand)))
                 (cons (+ line-pos beg) (+ line-pos end))))
             cands)))
  (add-to-list 'embark-multitarget-actions 'conn-dispatch-grep-candidates)

  (defun conn-dispatch-location-candidates (cands)
    (conn-regions-dispatch-menu
     (mapcar (lambda (cand)
               (let ((marker (car (consult--get-location cand))))
                 (save-excursion
                   (goto-char marker)
                   (cons marker (line-end-position)))))
             cands)))
  (add-to-list 'embark-multitarget-actions 'conn-dispatch-location-candidates)

  (defvar-keymap conn-embark-consult-location-map
    :parent embark-general-map
    "\\" 'conn-dispatch-location-candidates
    "D"   'conn-dot-consult-location-candidate
    "c"   '("clone-indirect-buffer" .
            conn-clone-indirect-buffer-location-candidate))
  (cl-pushnew 'conn-embark-consult-location-map
              (alist-get 'consult-location embark-keymap-alist))

  (defvar-keymap conn-embark-consult-grep-map
    :parent embark-general-map
    "\\" 'conn-dispatch-grep-candidates
    "D"   'conn-dot-consult-grep-candidate)
  (cl-pushnew 'conn-embark-consult-grep-map
              (alist-get 'consult-grep embark-keymap-alist)))
;;; conn-consult.el ends here
