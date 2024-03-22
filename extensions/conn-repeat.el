;;; conn-repeat.el --- Conn repeat extension -*- lexical-binding: t -*-
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

(require 'conn-mode)
(require 'repeat)

(defcustom conn-repeating-cursor-color
  "#a60000"
  "Cursor color while repeat map is active."
  :type 'color
  :group 'conn-mode)

(defun conn--repeat-get-map-ad ()
  (when-let (repeat-mode
             (prop (repeat--command-property :conn-repeat-command))
             (m (or (eq prop t)
                    (eq prop conn-current-state))))
    (define-keymap (single-key-description last-command-event) this-command)))

(defun conn-set-repeat-command (command &optional state)
  "Make COMMAND repeatable in STATE with whatever key called it.

If STATE is nil make COMMAND always repeat."
  (put command :conn-repeat-command (or state t)))

(defun conn-unset-repeat-command (command)
  "Remove repeat property from COMMAND."
  (put command :conn-repeat-command nil))

(mapc #'conn-set-repeat-command
      '(transpose-words
        transpose-sexps
        transpose-chars
        transpose-lines
        transpose-paragraphs
        conn-transpose-words-backward
        conn-transpose-sexps-backward
        conn-transpose-chars-backward
        conn-transpose-lines-backward
        conn-transpose-paragraphs-backward
        conn-set-window-dedicated
        previous-error
        next-error
        pop-global-mark
        conn-region-case-dwim
        conn-remove-dot-backward
        conn-remove-dot-forward
        duplicate-line
        duplicate-dwim
        conn-duplicate-region
        conn-delete-pair
        bury-buffer))

(conn-define-extension conn-repeatable-commands
  (if conn-repeatable-commands
      (advice-add 'repeat-get-map :after-until 'conn--repeat-get-map-ad)
    (advice-remove 'repeat-get-map 'conn--repeat-get-map-ad)))

(let (original-cursor-color)
  (defun conn--repeat-cursor-message-ad (map)
    (when (and map (not original-cursor-color))
      (setq original-cursor-color (face-background 'cursor)))
    (modify-all-frames-parameters
     `((cursor-color . ,(if map
                            conn-repeating-cursor-color
                          original-cursor-color))))
    (unless map (setq original-cursor-color nil))))

(conn-define-extension conn-repeat-cursor
  "Change the cursor color when a repeat map is active."
  (if conn-repeat-cursor
      (add-function :after repeat-echo-function 'conn--repeat-cursor-message-ad)
    (remove-function repeat-echo-function 'conn--repeat-cursor-message-ad)))

(provide 'conn-repeat)
