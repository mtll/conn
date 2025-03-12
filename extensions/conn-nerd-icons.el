;;; conn-nerd-icons.el --- Nerd Icons for Conn State Lighters -*- lexical-binding: t -*-
;;
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "30.0.2.0") nerd-icons conn)
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
(require 'nerd-icons)

;;;###autoload
(defun conn-enable-nerd-icon-lighters ()
  (setf (conn-state-get 'conn-command-state :lighter)
        (concat (nerd-icons-codicon "nf-cod-blank")
                (nerd-icons-mdicon "nf-md-ship_wheel"))
        (conn-state-get 'conn-emacs-state :lighter)
        (concat (nerd-icons-codicon "nf-cod-blank")
                (nerd-icons-devicon "nf-dev-emacs"))
        (conn-state-get 'conn-org-edit-state :lighter)
        (concat (nerd-icons-codicon "nf-cod-blank")
                (nerd-icons-sucicon "nf-custom-orgmode"))
        (conn-state-get 'conn-read-mover-state :lighter)
        (list (concat (nerd-icons-codicon "nf-cod-blank")
                      (nerd-icons-mdicon "nf-md-truck_cargo_container")))
        (conn-state-get 'conn-read-dispatch-state :lighter)
        (list (concat (nerd-icons-codicon "nf-cod-blank")
                      (nerd-icons-octicon "nf-oct-container")))
        (alist-get 'conn-wincontrol-mode minor-mode-alist)
        (list (concat (nerd-icons-codicon "nf-cod-blank")
                      (nerd-icons-codicon "nf-cod-window")))))

(provide 'conn-nerd-icons)

;; Local Variables:
;; outline-regexp: ";;;;* [^ 	\n]"
;; indent-tabs-mode: nil
;; End:
;;; conn-nerd-icons.el ends here
