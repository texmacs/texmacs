
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
;; Spreadsheets in tables
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

(tm-define (cell-row cell)
  (and (tree-is? cell 'cell)
       (tree-is? cell :up 'row)
       (+ (tree-index (tree-up cell)) 1)))

(tm-define (cell-column cell)
  (and (tree-is? cell 'cell)
       (+ (tree-index cell) 1)))

(tm-define (number->row r)
  (if (< r 27)
      (list->string (list (integer->char (+ r 96))))
      (string-append (number->row (quotient r 26))
                     (number->row (+ (modulo (- r 1) 26) 1)))))

(tm-define (cell-name cell)
  (and-with r (cell-row cell)
    (and-with c (cell-column cell)
      (string-append (number->row r) (number->string c)))))

(tm-define (calc-table-rename-cell cell)
  ;;(display* "Renaming " cell "\n")
  (with s (cell-name cell)
    (with body (tree-ref cell 0)
      (when (calc-data-context? body)
        (with id (tree->string (tree-ref body 0))
          (if (!= s id) (ahash-set! calc-rename-table id s))
          (tree-set body 0 s))))))

(tm-define (calc-table-update-cell cell)
  ;;(display* "Updating " cell "\n")
  (with s (cell-name cell)
    (with body (tree-ref cell 0)
      (when (not (calc-data-context? body))
        (tree-insert-node! body 1 `(cell-input ,s ""))))))

(tm-define (calc-table-update-table t)
  (let* ((tid (tree->string (tree-ref t 0)))
	 (cells (select t '(1 :* table :* row :* cell))))
    ;;(display* "Cells: " cells "\n")
    (set! calc-rename-table (make-ahash-table))
    (for-each calc-table-rename-cell cells)
    ;;(display* "Renaming formulas\n")
    (calc-rename-formulas (buffer-tree))
    (set! calc-rename-table (make-ahash-table))
    ;;(display* "Updating cells\n")
    (for-each calc-table-update-cell cells)))

(tm-define (calc-table-update)
  (with-innermost t 'calc-table
    (calc-table-update-table t)))

(tm-define (make-calc-table)
  (insert-go-to '(calc-table "" "") '(1 0))
  (make 'block)
  (calc-table-update))

(define (calc-table-search t)
  (or (and (tree-is? t 'calc-table) t)
      (and (tree-ref t :up)
	   (not (tree-is? t :up 'table))
	   (calc-table-search (tree-up t)))))

(tm-define (calc-table-context? t)
  (and (tree-is? t 'table)
       (nnot (calc-table-search (tree-up t)))))

(tm-define (table-resize-notify t)
  (:require (calc-table-context? t))
  (calc-table-update-table (calc-table-search t)))
