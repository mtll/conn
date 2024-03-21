;;; conn-helm.el --- Conn isearch+ extension -*- lexical-binding: t -*-
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

(require 'helm-ring)
(require 'conn-mode)

(defun conn-helm-kill-ring-action-yank-replace (_str)
  "Insert concatenated marked candidates in current-buffer.

When two prefix args are given prompt to choose separator, otherwise
use `helm-kill-ring-separator' as default."
  (with-helm-current-buffer  
    (delete-region (region-beginning) (region-end)))
  (let ((marked (helm-marked-candidates))
        (sep (if (equal helm-current-prefix-arg '(16))
                 (read-string "Separator: ")
               helm-kill-ring-separator)))
    (helm-kill-ring-action-yank-1
     (cl-loop for c in (butlast marked)
              concat (concat c sep) into str
              finally return (concat str (car (last marked)))))))

(defun helm-replace-args (regexp)
  "Create arguments of `query-replace-regexp' action in `helm-regexp'."
  (let ((region-only (helm-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Replace %sregexp %s"
                                    (if helm-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     helm-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defun helm-replace-regexp (_candidate)
  "Replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e. don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp helm-input))
    (apply 'replace-regexp (helm-replace-args regexp))))

(defun helm-dot-regexp (_candidate)
  (conn-add-dots-matching-regexp helm-input))

(setq helm-source-regexp
      (helm-build-in-buffer-source "Regexp Builder"
        :init (lambda ()
                (helm-init-candidates-in-buffer
                    'global (with-temp-buffer
                              (insert-buffer-substring helm-current-buffer)
                              (buffer-string))))
        :get-line #'helm-regexp-get-line
        :persistent-action #'helm-regexp-persistent-action
        :persistent-help "Show this line"
        :multiline t
        :multimatch nil
        :requires-pattern 2
        :group 'helm-regexp
        :mode-line "Press TAB to select action."
        :action '(("Kill Regexp" . helm-kill-regexp)
                  ("Query Replace Regexp (C-u Not inside word.)"
                   . helm-query-replace-regexp)
                  ("Replace Regexp (C-u Not inside word.)" . helm-replace-regexp)
                  ("Kill Regexp as sexp" . helm-kill-regexp-as-sexp)
                  ("Dot Regexp" . helm-dot-regexp))))

(defun helm-dot-occur-lines (_)
  (with-current-buffer helm-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let (buf-name)
      (while (not (eobp))
        (if (helm-pos-header-line-p)
            (let ((beg (point-at-bol))
                  (end (point-at-eol)))
              (set-text-properties beg (1+ end) nil)
              (delete-region (1- beg) end))
          (helm-aif (setq buf-name (assoc-default
                                    'buffer-name
                                    (get-text-property (point) 'helm-cur-source)))
              (let ((val (get-text-property (point) 'helm-realvalue)))
                (with-current-buffer buf-name
                  (save-excursion
                    (helm-goto-line val)
                    (conn--create-dots (cons (line-beginning-position)
                                             (line-end-position))))))))
        (forward-line 1)))))

(provide 'conn-helm)
