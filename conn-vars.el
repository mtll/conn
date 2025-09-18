;;; conn-vars.el --- Variables -*- lexical-binding: t -*-
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

;;; Code

(eval-when-compile (require 'cl-lib))

;;;; Declerations

(defvar conn-mode nil)
(defvar conn-local-mode nil)
(defvar conn-lighter " Conn")
(defvar conn--last-perform-bounds nil)

;;;; Custom Variables

(defgroup conn nil
  "A modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-marks nil
  "Conn-mode marks."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-faces nil
  "Conn-mode faces."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-states nil
  "Conn-mode states."
  :prefix "conn-"
  :group 'conn)

(defgroup conn-key-remapping nil
  "Conn-mode key remapping."
  :prefix "conn-"
  :group 'conn)

;;;;; Key Remapping

(defcustom conn-undo-keys (key-parse "C-/")
  "`undo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-undo-redo-keys (key-parse "C-?")
  "`undo-redo' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-yank-keys (key-parse "C-y")
  "`yank' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-region-keys (key-parse "C-w")
  "`kill-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-region-keys (key-parse "C-S-w")
  "`delete-region' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sexp-keys (key-parse "C-M-f")
  "`forward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sexp-keys (key-parse "C-M-b")
  "`backward-sexp' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-paragraph-keys (key-parse "M-{")
  "`backward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-paragraph-keys (key-parse "M-}")
  "`forward-paragraph' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-beginning-of-defun-keys (key-parse "C-M-a")
  "`beginning-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-end-of-defun-keys (key-parse "C-M-e")
  "`end-of-defun' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-next-line-keys (key-parse "C-n")
  "`next-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-previous-line-keys (key-parse "C-p")
  "`previous-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-char-keys (key-parse "C-f")
  "`forward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-char-keys (key-parse "C-b")
  "`backward-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-word-keys (key-parse "M-f")
  "`forward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-word-keys (key-parse "M-b")
  "`backward-word' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-sentence-keys (key-parse "M-a")
  "`backward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-sentence-keys (key-parse "M-e")
  "`forward-sentence' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-delete-char-keys (key-parse "DEL")
  "`backward-delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-delete-char-keys (key-parse "C-d")
  "`delete-char' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-up-list-keys (key-parse "C-M-u")
  "`backward-up-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-down-list-keys (key-parse "C-M-d")
  "`down-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-forward-list-keys (key-parse "C-M-n")
  "`forward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-backward-list-keys (key-parse "C-M-p")
  "`backward-list' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

(defcustom conn-kill-line-keys (key-parse "C-k")
  "`kill-line' key binding."
  :group 'conn-key-remapping
  :type '(vector integer))

;;;;; Remaps

(defmacro conn--without-conn-maps (&rest body)
  "Run BODY without any state, mode, or local maps active."
  (declare (debug (body))
           (indent 0))
  `(let ((emulation-mode-map-alists
          (seq-difference emulation-mode-map-alists
                          '(conn--minor-mode-maps
                            conn--major-mode-map
                            conn--state-map)
                          #'eq)))
     ,(macroexp-progn body)))

(defvar conn-demap-key
  `(menu-item
    "Demap key"
    nil
    :filter ,(lambda (_real-binding)
               (conn--without-conn-maps
                 (key-binding (vector last-input-event) t)))))

(defmacro conn-remap-key (from-keys &optional without-conn-maps no-accept-default)
  "Map to whatever is bound at FROM-KEYS.

This allows for transparently binding keys to commands which may be
conceptually the same but vary in implementation by mode, for example
paredit or smartparens commands.  Also see `conn-remap-key'."
  (let ((accept-default (if (macroexp-const-p no-accept-default)
                            (and no-accept-default t)
                          `(not ,no-accept-default)))
        (from-keys (if (stringp from-keys)
                       (key-parse from-keys)
                     from-keys)))
    `(list 'menu-item
           (format "Remap %s" ,from-keys)
           nil
           :filter (lambda (_real-binding)
                     ,(if (macroexp-const-p without-conn-maps)
                          (if without-conn-maps
                              `(conn--without-conn-maps
                                 (key-binding ,from-keys ,accept-default))
                            `(key-binding ,from-keys ,accept-default))
                        `(if ,without-conn-maps
                             (conn--without-conn-maps
                               (key-binding ,from-keys ,accept-default))
                           (key-binding ,from-keys ,accept-default)))))))

(defvar conn--remap-keymaps nil)

(eval-and-compile
  (defun conn--define-remap-keymap (description keys)
    (let ((keys (cl-loop for key in keys
                         if (stringp key) collect (key-parse key)
                         else collect key)))
      `(list 'menu-item
             ',description
             nil
             :filter (lambda (_real-binding)
                       ,(let (maps)
                          (dolist (key keys)
                            (pcase key
                              (`(:without-conn-maps ,key)
                               (if (stringp key) (setq key (key-parse key)))
                               (push `(conn--without-conn-maps
                                        (key-binding ,key t))
                                     maps))
                              (key
                               (if (stringp key) (setq key (key-parse key)))
                               (push `(key-binding ,key t) maps))))
                          `(make-composed-keymap
                            (delq nil (list ,@(nreverse maps))))))))))

(defmacro conn-define-remap-keymap (name description &rest keys)
  (declare (indent 2))
  `(progn
     (cl-pushnew ',name conn--remap-keymaps)
     (defvar ,name
       ,(conn--define-remap-keymap description keys)
       ,description)))

;; A hack but it allows the binding display in quick ref looks nice
(defmacro conn--where-is-with-remaps (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn
         (cl-loop for remap in conn--remap-keymaps
                  for val = (symbol-value remap)
                  do (setf (nth 2 val) (keymap--menu-item-binding val)))
         ,@body)
     (cl-loop for remap in conn--remap-keymaps
              do (setf (nth 2 (symbol-value remap)) nil))))

(conn-define-remap-keymap conn-search-remap
    "Conn Search Map"
  [conn-search-map]
  (:without-conn-maps "M-s"))

(conn-define-remap-keymap conn-goto-remap
    "Conn Search Map"
  [conn-goto-map]
  (:without-conn-maps "M-g"))

(conn-define-remap-keymap conn-thing-remap
    "Conn Search Map"
  [conn-thing-map])

(conn-define-remap-keymap conn-region-remap
    "Conn Search Map"
  [conn-region-map])

(conn-define-remap-keymap conn-edit-remap
    "Conn Search Map"
  [conn-edit-map])

(defvar conn-forward-word-remap (conn-remap-key conn-forward-word-keys t))
(defvar conn-forward-sexp-remap (conn-remap-key conn-forward-sexp-keys t))
(defvar conn-previous-line-remap (conn-remap-key conn-previous-line-keys t))
(defvar conn-backward-paragraph-remap (conn-remap-key conn-backward-paragraph-keys t))
(defvar conn-forward-sentence-remap (conn-remap-key conn-forward-sentence-keys t))
(defvar conn-backward-sentence-remap (conn-remap-key conn-backward-sentence-keys t))
(defvar conn-down-list-remap (conn-remap-key conn-down-list-keys t))
(defvar conn-backward-up-list-remap (conn-remap-key conn-backward-up-list-keys t))
(defvar conn-forward-list-remap (conn-remap-key conn-forward-list-keys t))
(defvar conn-backward-list-remap (conn-remap-key conn-backward-list-keys t))
(defvar conn-backward-word-remap (conn-remap-key conn-backward-word-keys t))
(defvar conn-backward-char-remap (conn-remap-key conn-backward-char-keys t))
(defvar conn-forward-paragraph-remap (conn-remap-key conn-forward-paragraph-keys t))
(defvar conn-next-line-remap (conn-remap-key conn-next-line-keys t))
(defvar conn-forward-char-remap (conn-remap-key conn-forward-char-keys t))
(defvar conn-end-of-defun-remap (conn-remap-key conn-end-of-defun-keys t))
(defvar conn-beginning-of-defun-remap (conn-remap-key conn-beginning-of-defun-keys t))
(defvar conn-backward-sexp-remap (conn-remap-key conn-backward-sexp-keys t))

(provide 'conn-vars)
