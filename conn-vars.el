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

(eval-when-compile
  (require 'cl-lib))

;;;; Declerations

(defvar conn-mode nil)
(defvar conn-local-mode nil)
(defvar conn-lighter " Conn")

(defvar conn-keymaps-defined nil)

(defvar conn-kill-reformat-function 'conn-kill-reformat
  "Function to reformat the buffer after killing a region.

The function will be called with a single argument, the `conn-bounds' of
the region that has just been killed, and with the point at the position
where the region was.

This variable may also be nil in which case fixing up whitespace is
disabled.")

(defvar-keymap conn-goto-map)
(defvar-keymap conn-default-thing-map)
(defvar-keymap conn-default-inner-thing-map)
(defvar-keymap conn-default-edit-map)
(defvar-keymap conn-search-map)
(defvar-keymap conn-other-end-argument-map)
(defvar-keymap conn-sort-reverse-argument-map)
(defvar-keymap conn-sort-fold-case-argument-map)
(defvar-keymap conn-window-resize-map)
(defvar-keymap conn-wincontrol-one-command-map)
(defvar-keymap conn-wincontrol-map
  :doc "Map active in `conn-wincontrol-mode'."
  :suppress 'nodigits)

;;;; Custom Variables

(defgroup conn nil
  "A modal keybinding mode."
  :prefix "conn-"
  :group 'editing)

(defgroup conn-faces nil
  "Conn-mode faces."
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
  "`undo-redo' key binding."
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

;;;;; Faces

(defface conn-argument-active-face
  '((t (:inherit eldoc-highlight-function-argument)))
  "Face for active arguments"
  :group 'conn-faces)

;;;;; History Variables

(defvar conn-separator-history nil)

(defvar conn-read-string-target-history nil)

;;;;; Mark Variables

(defvar conn-this-command-start (make-marker)
  "Start position for current command.")

(defvar-local conn--last-thing-command-pos nil)
(defvar-local conn--last-thing-command nil)

(defvar conn--last-thing-override nil)

(defvar conn-command-history nil
  "History of commands that read arguments with `conn-read-args'.

See also `conn-repeat'.")

(defvar conn-command-history-max 32
  "Maximum number of elements to keep in `conn-command-history'.")

(defvar conn-repeating-command nil)

;;;;; Remaps

(defmacro conn--without-conn-maps (&rest body)
  "Run BODY without any state, mode, or local maps active."
  (declare (debug (body))
           (indent 0))
  `(let ((emulation-mode-map-alists
          (seq-difference emulation-mode-map-alists
                          '(conn--wincontrol-map-alist
                            conn--minor-mode-maps
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
  "Map to whatever is bound at FROM-KEYS."
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

(defvar conn--key-remaps nil)

(defmacro conn-define-key-remap (name
                                 from-keys
                                 &optional
                                 without-conn-maps
                                 no-accept-default)
  (declare (indent 1))
  `(progn
     (defvar ,name
       (conn-remap-key ,from-keys ,without-conn-maps ,no-accept-default))
     (cl-pushnew ',name conn--key-remaps)))

(conn-define-key-remap conn-forward-word-remap conn-forward-word-keys t)
(conn-define-key-remap conn-forward-sexp-remap conn-forward-sexp-keys t)
(conn-define-key-remap conn-previous-line-remap conn-previous-line-keys t)
(conn-define-key-remap conn-backward-paragraph-remap conn-backward-paragraph-keys t)
(conn-define-key-remap conn-forward-sentence-remap conn-forward-sentence-keys t)
(conn-define-key-remap conn-backward-sentence-remap conn-backward-sentence-keys t)
(conn-define-key-remap conn-down-list-remap conn-down-list-keys t)
(conn-define-key-remap conn-backward-up-list-remap conn-backward-up-list-keys t)
(conn-define-key-remap conn-forward-list-remap conn-forward-list-keys t)
(conn-define-key-remap conn-backward-list-remap conn-backward-list-keys t)
(conn-define-key-remap conn-backward-word-remap conn-backward-word-keys t)
(conn-define-key-remap conn-backward-char-remap conn-backward-char-keys t)
(conn-define-key-remap conn-forward-paragraph-remap conn-forward-paragraph-keys t)
(conn-define-key-remap conn-next-line-remap conn-next-line-keys t)
(conn-define-key-remap conn-forward-char-remap conn-forward-char-keys t)
(conn-define-key-remap conn-end-of-defun-remap conn-end-of-defun-keys t)
(conn-define-key-remap conn-beginning-of-defun-remap conn-beginning-of-defun-keys t)
(conn-define-key-remap conn-backward-sexp-remap conn-backward-sexp-keys t)

(defvar conn--remap-keymaps nil)

(eval-and-compile
  (defun conn--define-remap-keymap (description keys)
    (let (maps default-maps)
      (dolist (key keys)
        (pcase key
          (`(:without-conn-maps ,key)
           (if (stringp key) (setq key (key-parse key)))
           (push `(conn--without-conn-maps
                    (key-binding ,key t))
                 maps))
          ((and key (pred stringp))
           (if (stringp key) (setq key (key-parse key)))
           (push `(key-binding ,key t) maps))
          ((and key (pred vectorp))
           (push `(key-binding ,key t) maps))
          (map
           (push map maps)
           (push map default-maps))))
      `(list 'menu-item
             ,description
             ,(when default-maps
                `(make-composed-keymap
                  (list ,@default-maps)))
             :filter (lambda (_real-binding)
                       (make-composed-keymap
                        (delq nil (list ,@(nreverse maps)))))))))

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
         (cl-loop for remap in conn--key-remaps
                  for val = (symbol-value remap)
                  do (setf (nth 2 val) (keymap--menu-item-binding val)))
         ,@body)
     (cl-loop for remap in conn--remap-keymaps
              do (setf (nth 2 (symbol-value remap)) nil))
     (cl-loop for remap in conn--key-remaps
              do (setf (nth 2 (symbol-value remap)) nil))))

(conn-define-remap-keymap conn-search-remap
    "Conn Search Map"
  [conn-search-map]
  conn-search-map
  (:without-conn-maps "M-s"))

(conn-define-remap-keymap conn-goto-remap
    "Conn Goto Map"
  [conn-goto-map]
  conn-goto-map
  (:without-conn-maps "M-g"))

(conn-define-remap-keymap conn-thing-remap
    "Conn Thing Map"
  [conn-thing-map]
  conn-default-thing-map)

(conn-define-remap-keymap conn-thing-inner-remap
    "Conn Inner Thing Map"
  [conn-inner-thing-map]
  conn-default-inner-thing-map)

(conn-define-remap-keymap conn-edit-remap
    "Conn Edit Map"
  [conn-edit-map]
  conn-default-edit-map)

(provide 'conn-vars)
