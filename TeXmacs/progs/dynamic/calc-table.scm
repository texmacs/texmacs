
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-table.scm
;; DESCRIPTION : routines for spread sheets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-table)
  (:use (dynamic calc-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get standard name of table cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cell-row cell)
  (and (tree-is? cell 'cell)
       (tree-is? cell :up 'row)
       (+ (tree-index (tree-up cell)) 1)))

(tm-define (cell-column cell)
  (and (tree-is? cell 'cell)
       (+ (tree-index cell) 1)))

(tm-define (cell-name cell)
  (and-with r (cell-row cell)
    (and-with c (cell-column cell)
      (string-append (number->row r) (number->string c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard (re)naming of cells inside a table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define calc-rename-table (make-ahash-table))

(tm-define (calc-rename-formulas t)
  (cond ((tree-atomic? t) (noop))
        ((calc-ref-context? t)
         (with var (texmacs->string (tree-ref t 0))
           (with new (ahash-ref calc-rename-table var)
             (when new
               (tree-set t 0 new)))))
        (else (for-each calc-rename-formulas (tree-children t)))))

(tm-define (calc-table-rename-cell cell)
  ;;(display* "Renaming " cell "\n")
  (with s (cell-name cell)
    (with body (tree-ref cell 0)
      (if (tree-func? body 'document 1)
          (set! body (tree-ref body 0)))
      (when (calc-data-context? body)
        (with id (tree->string (tree-ref body 0))
          (if (!= s id) (ahash-set! calc-rename-table id s))
          (tree-set body 0 s))))))

(tm-define (calc-table-update-cell cell)
  ;;(display* "Updating " cell "\n")
  (with s (cell-name cell)
    (with body (tree-ref cell 0)
      (if (tree-func? body 'document 1)
          (set! body (tree-ref body 0)))
      (when (not (calc-data-context? body))
        (tree-insert-node! body 1 `(cell-input ,s ""))))))

(tm-define (calc-table-update-table t)
  (let* ((tid (tree->string (tree-ref t 0)))
	 (cells (select t '(1 :* table :* row :* cell))))
    ;;(display* "Cells: " cells "\n")
    (set! calc-rename-table (make-ahash-table))
    (for-each calc-table-rename-cell cells)
    ;;(display* "Renaming formulas\n")
    (calc-rename-formulas t)
    (set! calc-rename-table (make-ahash-table))
    ;;(display* "Updating cells\n")
    (for-each calc-table-update-cell cells)))

(tm-define (calc-table-update)
  (with-innermost t 'calc-table
    (calc-table-update-table t)))

(tm-define (make-calc-table tag)
  (insert-go-to `(calc-table ,(create-unique-id) "") '(1 0))
  (make tag)
  (calc-table-update))

(tm-define (table-resize-notify t)
  (:require (calc-table-context? t))
  (calc-table-update-table (calc-table-search t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell input rewriting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (calc-eat-cell-name cs num?)
  (cond ((null? cs) (and num? 0))
        ((and (not num?) (char>=? (car cs) #\a) (char<=? (car cs) #\z))
         (with i (calc-eat-cell-name (cdr cs) #f)
           (and i (+ i 1))))
        ((and (char>=? (car cs) #\0) (char<=? (car cs) #\9))
         (with i (calc-eat-cell-name (cdr cs) #t)
           (and i (+ i 1))))
        (else (and num? 0))))

(tm-define (calc-input-encode-sub cs)
  (cond ((null? cs) cs)
        ((and (char>=? (car cs) #\a) (char<=? (car cs) #\z))
         (with i (calc-eat-cell-name cs #f)
           (if (not i)
               (cons (list->string (list (car cs)))
                     (calc-input-encode-sub (cdr cs)))
               (cons `(cell-ref ,(list->string (sublist cs 0 i)))
                     (calc-input-encode-sub (sublist cs i (length cs)))))))
        (else
          (cons (list->string (list (car cs)))
                (calc-input-encode-sub (cdr cs))))))

(tm-define (cell-input-encode t)
  (cond ((tree-atomic? t)
         (with l (calc-input-encode-sub (string->list (tree->string t)))
           (tm->tree (apply tmconcat l))))
        ((tree-is? t 'concat)
         (with l (map cell-input-encode (tree-children t))
           (tm->tree (apply tmconcat l))))
        ((tree-is? t 'cell-ref) t)
        (else (tree-map-accessible-children cell-input-encode t))))

(tm-define (cell-input-decode t)
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'concat)
         (with l (map cell-input-decode (tree-children t))
           (tm->tree (apply tmconcat l))))
        ((tree-is? t 'cell-ref) (tree-ref t 0))
        (else (tree-map-accessible-children cell-input-decode t))))

(tm-define (calc-get-input t)
  (:require (tree-in? t '(cell-input cell-output)))
  (cell-input-encode (tree-ref t 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-output))
  (tree-assign-node t 'cell-input)
  (tree-go-to t 1 :end))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-input))
  (tree-assign-node t 'cell-output)
  (tree-go-to t 2 :end)
  (calc))
