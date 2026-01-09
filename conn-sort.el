;;; conn-sort.el --- Sort -*- lexical-binding: t -*-
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

;;; Commentary

;; Sort command

;;; Code

(require 'sort)
(require 'conn)

(defvar conn-sort-special-bindings-ref
  (conn-reference-quote
    (("Columns" sort-columns)
     ("Regexp fields" sort-regexp-fields)
     ("Numeric fields" sort-numeric-fields)
     ("Fields" sort-fields))))

(defvar conn-sort-bindings-ref
  (list (conn-reference-page "Sort"
          "Special bindings"
          (:eval (conn-quick-ref-to-cols
                  conn-sort-special-bindings-ref 2)))))

(conn-define-state conn-sort-state (conn-read-thing-state)
  :lighter "SORT")

(define-keymap
  :keymap (conn-get-state-map 'conn-sort-state)
  "*" 'sort-regexp-fields
  "#" 'sort-numeric-fields
  "TAB" 'sort-fields
  "|" 'sort-columns)

(cl-defstruct (conn-sort-things-argument
               (:include conn-thing-argument)
               ( :constructor conn-sort-things-argument
                 (&aux
                  (required t)
                  (value
                   (when (and (use-region-p)
                              (bound-and-true-p rectangle-mark-mode))
                     (list 'region nil)))
                  (set-flag
                   (and (use-region-p)
                        (bound-and-true-p rectangle-mark-mode)))))))

(cl-defmethod conn-argument-predicate ((_arg conn-sort-things-argument)
                                       cmd)
  (or (memq cmd '(sort-numeric-fields
                  sort-fields
                  sort-regexp-fields
                  sort-columns))
      (cl-call-next-method)))

(defvar-keymap conn-sort-reverse-map
  "r" 'reverse)

(defvar-keymap conn-sort-fold-case-map
  "g" 'sort-fold-case)

(defun conn--sort-in-bounds (bounds
                             transform
                             reverse
                             fold-case
                             &optional
                             predicate)
  (pcase bounds
    ((and (conn-bounds-get :subregions nil
                           (and regions (pred identity)))
          (conn-bounds `(,beg . ,end)))
     (conn--compat-callf sort regions
       :lessp (pcase-lambda ((conn-bounds `(,b1 . ,e1))
                             (conn-bounds `(,b2 . ,e2)))
                (cond ((<= b1 e1 b2 e2) t)
                      ((>= e1 b1 e2 b2) nil)
                      (t (error "Overlapping regions")))))
     (save-excursion
       (with-restriction beg end
         (let* ((sort-lists nil)
                (messages (> (- (point-max) (point-min)) 50000)))
           (pcase-dolist ((conn-bounds cons transform)
                          regions)
             (push (cons cons cons) sort-lists))
           (let ((old (reverse sort-lists))
                 (case-fold-search fold-case))
             (when sort-lists
               (or reverse (setq sort-lists (nreverse sort-lists)))
               (if messages (message "Sorting records..."))
               (setq sort-lists
                     (sort sort-lists
                           (if predicate
                               (lambda (a b)
                                 (funcall predicate (car a) (car b)))
                             (lambda (a b)
                               (> 0 (compare-buffer-substrings
                                     nil (car (car a)) (cdr (car a))
                                     nil (car (car b)) (cdr (car b))))))))
               (if reverse (setq sort-lists (nreverse sort-lists)))
               (if messages (message "Reordering buffer..."))
               (with-buffer-unmodified-if-unchanged
                 (sort-reorder-buffer sort-lists old))))))))
    (_ (user-error "No regions to sort"))))

(cl-defgeneric conn-sort-things-do (thing
                                    arg
                                    transform
                                    &optional
                                    reverse
                                    fold-case))

(cl-defmethod conn-sort-things-do ((thing (conn-thing t))
                                   arg
                                   transform
                                   &optional
                                   reverse
                                   fold-case)
  (conn--sort-in-bounds (conn-bounds-of thing arg)
                        transform
                        reverse
                        fold-case))

(cl-defmethod conn-sort-things-do ((_thing (conn-thing conn-things-in-region))
                                   arg
                                   _transform
                                   &optional
                                   _reverse
                                   _fold-case)
  (conn-read-args (conn-read-thing-state
                   :prompt "Things in Region"
                   :prefix arg)
      ((`(,thing ,arg) (conn-thing-argument))
       (transform (conn-transform-argument))
       (reverse
        (conn-boolean-argument "reverse"
                               'reverse
                               conn-sort-reverse-map))
       (fold-case
        (conn-boolean-argument "fold case"
                               'sort-fold-case
                               conn-sort-fold-case-map
                               (bound-and-true-p sort-fold-case))))
    (conn--sort-in-bounds (conn-get-things-in-region
                           thing arg transform
                           (region-beginning) (region-end))
                          nil
                          reverse
                          fold-case)))

(cl-defmethod conn-sort-things-do ((_thing (eql sort-numeric-fields))
                                   arg
                                   transform
                                   &optional
                                   reverse
                                   _fold-case)
  (conn-read-args (conn-read-thing-state
                   :prompt "Sort Inside")
      ((`(,thing ,targ) (conn-thing-argument))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds `(,beg . ,end) transform)
       (sort-numeric-fields (prefix-numeric-value arg) beg end)
       (when reverse (reverse-region beg end))))))

(cl-defmethod conn-sort-things-do ((_thing (eql sort-fields))
                                   arg
                                   transform
                                   &optional
                                   reverse
                                   fold-case)
  (conn-read-args (conn-read-thing-state
                   :prompt "Sort Inside")
      ((`(,thing ,targ) (conn-thing-argument))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds `(,beg . ,end) transform)
       (let ((sort-fold-case fold-case))
         (sort-fields (prefix-numeric-value arg) beg end))
       (when reverse (reverse-region beg end))))))

(cl-defmethod conn-sort-things-do ((_thing (eql sort-regexp-fields))
                                   arg
                                   transform
                                   &optional
                                   reverse
                                   fold-case)
  (conn-read-args (conn-read-thing-state
                   :prompt "Sort Inside"
                   :prefix arg)
      ((`(,thing ,targ) (conn-thing-argument-dwim t))
       (transform (conn-transform-argument transform)))
    (pcase (conn-bounds-of thing targ)
      ((conn-bounds (and region `(,beg . ,end)) transform)
       (let ((sort-fold-case fold-case))
         (sort-regexp-fields
          reverse
          (conn-read-regexp "Regexp specifying records to sort: " region)
          (conn-read-regexp "Regexp specifying key within record: " region)
          beg end))))))

(cl-defmethod conn-sort-things-do ((_thing (eql sort-columns))
                                   _arg
                                   _transform
                                   &optional
                                   reverse
                                   fold-case)
  (let ((sort-fold-case fold-case))
    (sort-columns reverse
                  (region-beginning)
                  (region-end))))

;;;###autoload
(defun conn-sort-things (thing
                         arg
                         transform
                         &optional
                         reverse
                         fold-case)
  "Sort alphabetically subregions defined by THING, ARG, and TRANSFORM.

If REVERSE is non-nil then sort subregions in reverse order.

FOLD-CASE is the value of `sort-fold-case' to use for the sorting.
Interactively defaults to the current value of `sort-fold-case'."
  (interactive
   (conn-read-args (conn-sort-state
                    :prompt (if (region-active-p)
                                "Things in Regions"
                              "Thing")
                    :reference conn-sort-bindings-ref
                    :prefix current-prefix-arg)
       ((`(,thing ,arg) (if (region-active-p)
                            (list 'conn-things-in-region)
                          (conn-sort-things-argument)))
        (transform (conn-transform-argument))
        (reverse
         (conn-boolean-argument "reverse"
                                'reverse
                                conn-sort-reverse-map))
        (fold-case
         (conn-boolean-argument "fold case"
                                'sort-fold-case
                                conn-sort-fold-case-map
                                (bound-and-true-p sort-fold-case))))
     (list thing arg transform reverse fold-case)))
  (conn-sort-things-do thing arg transform reverse fold-case))

(provide 'conn-sort)
