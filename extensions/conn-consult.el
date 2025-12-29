;;; conn-consult.el --- Conn consult extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (compat "30.0.2.0") consult conn)
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
(require 'conn-things)
(require 'consult)
(require 'xref)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(declare-function conn-regions-kapply-prefix "conn-transient")
(declare-function consult-imenu "consult-imenu")

(defgroup conn-consult nil
  "Consult integration for conn"
  :prefix "conn-"
  :group 'conn)

(defcustom conn-completion-region-quote-function 'regexp-quote
  "Function used to quote region strings for consult search functions."
  :group 'conn-consult
  :type 'symbol)

(cl-defmethod conn-dispatch-common-commands ((_command (eql consult-line)))
  (let ((inhibit-message nil))
    (consult-line)))

(cl-defmethod conn-dispatch-common-commands ((_command (eql consult-imenu)))
  (let ((inhibit-message nil))
    (consult-imenu)))

(defun conn--consult-jump-advice (&rest _)
  (conn-push-jump-ring (mark-marker)))

(defun conn--consult-setup-jump-advice ()
  (if conn-mode
      (progn
        (advice-add 'consult-ripgrep :after #'conn--consult-jump-advice)
        (advice-add 'consult-line :after #'conn--consult-jump-advice)
        (advice-add 'consult-line-multi :after #'conn--consult-jump-advice)
        (advice-add 'consult-find :after #'conn--consult-jump-advice)
        (advice-add 'consult-grep :after #'conn--consult-jump-advice)
        (advice-add 'consult-mark :after #'conn--consult-jump-advice)
        (advice-add 'consult-imenu :after #'conn--consult-jump-advice))
    (progn
      (advice-remove 'consult-ripgrep #'conn--consult-jump-advice)
      (advice-remove 'consult-line #'conn--consult-jump-advice)
      (advice-remove 'consult-line-multi #'conn--consult-jump-advice)
      (advice-remove 'consult-find #'conn--consult-jump-advice)
      (advice-remove 'consult-grep #'conn--consult-jump-advice)
      (advice-remove 'consult-mark #'conn--consult-jump-advice)
      (advice-remove 'consult-imenu #'conn--consult-jump-advice))))
(conn--consult-setup-jump-advice)
(add-hook 'conn-mode-hook #'conn--consult-setup-jump-advice)

;;;###autoload
(defun conn-consult-ripgrep-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-ripgrep
      nil
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

;;;###autoload
(defun conn-consult-line-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-line
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

;;;###autoload
(defun conn-consult-line-multi-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-line-multi
      nil
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

;;;###autoload
(defun conn-consult-locate-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-locate
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

;;;###autoload
(defun conn-consult-git-grep-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-git-grep
      nil
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

;;;###autoload
(defun conn-consult-find-thing (thing arg transform)
  (interactive
   (conn-read-args (conn-read-thing-state
                    :prompt "Thing")
       ((`(,thing ,arg) (conn-thing-argument-dwim-always))
        (transform (conn-transform-argument)))
     (list thing arg transform)))
  (pcase (conn-bounds-of thing arg)
    ((conn-bounds `(,beg . ,end) transform)
     (deactivate-mark)
     (consult-find
      nil
      (when (and (< (- end beg) 1000)
                 (= (count-lines beg end) 1))
        (funcall conn-completion-region-quote-function
                 (buffer-substring-no-properties beg end))))
     (conn-push-jump-ring (mark-marker)))))

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

  (defvar embark-multitarget-actions)
  (defvar embark-keymap-alist)

  (defun conn-kapply-xref-candidates (cands)
    (let ((regions
           (mapcar
            (lambda (cand)
              (when-let* ((xref (get-text-property 0 'consult-xref cand))
                          (marker (xref-location-marker (xref-item-location xref)))
                          (pt (marker-position marker)))
                (prog1
                    (conn-kapply-make-region pt pt (marker-buffer marker))
                  (set-marker marker nil))))
            cands)))
      (conn-regions-kapply-prefix
       (conn-kapply-region-iterator regions))))
  (add-to-list 'embark-multitarget-actions 'conn-kapply-xref-candidates)

  (defun conn-kapply-grep-candidates (cands)
    (conn-regions-kapply-prefix
     (conn-kapply-region-iterator
      (mapcar (lambda (cand)
                (pcase-let ((`(,line-marker (,beg . ,end) . _)
                             (consult--grep-position cand 'find-file-noselect)))
                  (prog1
                      (conn-kapply-make-region (+ line-marker beg)
                                               (+ line-marker end)
                                               (marker-buffer line-marker))
                    (set-marker line-marker nil))))
              cands))))
  (add-to-list 'embark-multitarget-actions 'conn-kapply-grep-candidates)

  (defun conn-kapply-location-candidates (cands)
    (conn-regions-kapply-prefix
     (conn-kapply-region-iterator
      (mapcar (lambda (cand)
                (pcase (car (get-text-property 0 'consult-location cand))
                  ((and mk (pred markerp))
                   (conn-kapply-make-region mk mk (marker-buffer mk)))
                  (`(,buf . ,pt)
                   (conn-kapply-make-region pt pt buf))))
              cands))))
  (add-to-list 'embark-multitarget-actions 'conn-kapply-location-candidates)

  (defvar-keymap conn-embark-consult-xref-map
    :parent embark-general-map
    "'" 'conn-kapply-xref-candidates)
  (cl-pushnew 'conn-embark-consult-xref-map
              (alist-get 'consult-xref embark-keymap-alist))

  (defvar-keymap conn-embark-consult-location-map
    :parent embark-general-map
    "'" 'conn-kapply-location-candidates
    "c" '("clone-indirect-buffer" .
          conn-clone-indirect-buffer-location-candidate))
  (cl-pushnew 'conn-embark-consult-location-map
              (alist-get 'consult-location embark-keymap-alist))

  (defvar-keymap conn-embark-consult-grep-map
    :parent embark-general-map
    "'" 'conn-kapply-grep-candidates)
  (cl-pushnew 'conn-embark-consult-grep-map
              (alist-get 'consult-grep embark-keymap-alist)))

;; Local Variables:
;; outline-regexp: ";;;;* [^    \n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-consult.el ends here
