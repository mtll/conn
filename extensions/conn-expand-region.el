;;; conn-expand-region.el --- Conn expreg extension -*- lexical-binding: t -*-
;;
;; Filename: conn-expand-region.el
;; Description: expand-region extensions for conn
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") expand-region conn)
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
;;; Commentary
;;
;; Use expand-region to proved expansions for conn-expand.
;;
;;; Code:

(require 'conn)
(require 'expand-region)

;;;###autoload
(defun conn-er-expansions ()
  (save-mark-and-excursion
    (cl-loop
     with expansions = (list (cons (region-beginning) (region-end))) do
     (let ((start (region-beginning))
           (end (region-end))
           (best-start (point-min))
           (best-end (point-max)))
       (when (and expand-region-skip-whitespace
                  (er--point-is-surrounded-by-white-space)
                  (= (region-beginning) (region-end)))
         (skip-chars-forward er--space-str))

       (dolist (try er/try-expand-list)
         (er--save-excursion
          (ignore-errors
            (funcall try)
            (when (and (region-active-p)
                       (er--this-expansion-is-better
                        start end best-start best-end))
              (setq best-start (region-beginning))
              (setq best-end (region-end))))))

       (goto-char best-start)
       (set-mark best-end)
       (push (cons (region-beginning) (region-end)) expansions)

       (when (and (= best-start (point-min))
                  (= best-end (point-max)))
         (cl-return expansions))))))

(provide 'conn-expand-region)
;;; conn-expand-region.el ends here

