;;; conn-posframe.el --- Conn posframe extension -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.0.0") posframe conn)
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
(require 'posframe)

(defgroup conn-posframe nil
  "Conn posframes."
  :prefix "conn-posframe-")

(defcustom conn-posframe-border-width 2
  "Border width for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-border-color
  "#000000"
  "Border color for conn posframes."
  :type 'string
  :group 'conn-posframe)

(defcustom conn-posframe-timeout 1.5
  "Timeout for conn posframes."
  :type 'integer
  :group 'conn-posframe)

(defcustom conn-posframe-poshandler
  'posframe-poshandler-frame-center
  "Timeout for conn posframes."
  :type 'symbol
  :group 'conn-posframe)

(defun conn-posframe-switch-tab-ad (&rest _)
  (posframe-show
   " *conn-list-posframe*"
   :string (mapconcat (lambda (tab)
                        (if (eq (car tab) 'current-tab)
                            (propertize (alist-get 'name (cdr tab))
                                        'face 'transient-value)
                          (alist-get 'name (cdr tab))))
                      (reverse (funcall tab-bar-tabs-function))
                      "\n")
   :poshandler conn-posframe-poshandler
   :timeout conn-posframe-timeout
   :border-width conn-posframe-border-width
   :border-color conn-posframe-border-color))

;;;###autoload
(define-minor-mode conn-posframe-mode
  "Posframes for Conn."
  :global t
  :lighter ""
  (if conn-posframe-mode
      (progn
        (advice-add 'tab-bar-switch-to-next-tab :after 'conn-posframe-switch-tab-ad)
        (advice-add 'tab-bar-switch-to-prev-tab :after 'conn-posframe-switch-tab-ad)
        (advice-add 'tab-bar-new-tab :after 'conn-posframe-switch-tab-ad)
        (advice-add 'tab-bar-close :after 'conn-posframe-switch-tab-ad))
    (advice-remove 'tab-bar-switch-to-next-tab 'conn-posframe-switch-tab-ad)
    (advice-remove 'tab-bar-switch-to-prev-tab 'conn-posframe-switch-tab-ad)
    (advice-remove 'tab-bar-new-tab 'conn-posframe-switch-tab-ad)
    (advice-remove 'tab-bar-close 'conn-posframe-switch-tab-ad)))

(provide 'conn-posframe)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; End:
;;; conn-posframe.el ends here
