
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
;; Subroutines for naming cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (number->row r)
  (if (< r 27)
      (list->string (list (integer->char (+ r 96))))
      (string-append (number->row (quotient r 26))
                     (number->row (+ (modulo (- r 1) 26) 1)))))

(tm-define (row->number s)
  (with n (string-length s)
    (if (== n 1)
	(- (char->integer (car (string->list s))) 96)
	(+ (* 26 (row->number (substring s 0 (- n 1))))
	   (row->number (substring s (- n 1) n))))))

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

(tm-define (cell-ref-encode p)
  (with (r c) p
    (with s (string-append (number->row r) (number->string c))
      (tm->tree `(cell-ref ,s)))))

(tm-define (cell-ref-decode s)
  (if (string? s)
      (and-with i (list-find-index
		    (string->list s)
		    (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
	(list (row->number (substring s 0 i))
	      (string->number (substring s i (string-length s)))))
      (and (tree-is? s 'cell-ref)
	   (cell-ref-decode (texmacs->string (tree-ref s 0))))))

(define ((cell-ref-range-sub c1 c2) r)
  (map (lambda (c) (cell-ref-encode (list r c)))
       (.. c1 (+ c2 1))))

(tm-define (cell-ref-range x1 x2)
  (with (r1 c1) (cell-ref-decode x1)
    (with (r2 c2) (cell-ref-decode x2)
      (with rows (map (cell-ref-range-sub c1 c2) (.. r1 (+ r2 1)))
	(apply append rows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard (re)naming of cells inside a table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define calc-rename-table (make-ahash-table))

(tm-define (calc-rename-formulas t)
  (cond ((tree-atomic? t) t)
        ((tree-in? t '(cell-input cell-output))
         (let* ((enc (cell-input-encode (tree-ref t 1)))
                (ren (calc-rename-formulas enc))
                (dec (cell-input-decode ren)))
           (tm->tree `(,(tree-label t) ,(tree-ref t 0) ,dec ,(tree-ref t 2)))))
        ((tree-is? t 'cell-ref)
         (with var (texmacs->string (tree-ref t 0))
           (with new (ahash-ref calc-rename-table var)
             (if new (tm->tree `(cell-ref ,new)) t))))
        (else (tree-map-accessible-children calc-rename-formulas t))))

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
  (with s (cell-name cell)
    (with body (tree-ref cell 0)
      (if (tree-func? body 'document 1)
          (set! body (tree-ref body 0)))
      (when (not (calc-data-context? body))
        (tree-insert-node! body 1 `(cell-inert ,s))))))

(define (tree-replace-cells t r)
  (cond ((== t r) (noop))
        ((or (tree-atomic? t) (tree-atomic? r)
             (!= (tree-label t) (tree-label r))
             (!= (tree-arity t) (tree-arity r)))
         (tree-set t r))
        (else (for-each tree-replace-cells
                        (tree-children t)
                        (tree-children r)))))

(tm-define (calc-table-update-table t)
  (let* ((tid (tree->string (tree-ref t 0)))
	 (cells (select t '(1 :* table :* row :* cell))))
    ;;(display* "Cells: " cells "\n")
    (set! calc-rename-table (make-ahash-table))
    (for-each calc-table-rename-cell cells)
    ;;(display* "Renaming formulas\n")
    (tree-replace-cells t (calc-rename-formulas t))
    (set! calc-rename-table (make-ahash-table))
    ;;(display* "Updating cells\n")
    (set! cells (select t '(1 :* table :* row :* cell)))
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
;; Determine the kind of cells (inert or input)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (starts-with-equal? t)
  (cond ((tree-atomic? t) (string-starts? (tree->string t) "="))
        ((tree-in? t '(math concat document))
         (starts-with-equal? (tree-ref t 0)))
        ((tree-in? t '(with))
         (starts-with-equal? (tree-ref t :last)))
        ((tree-in? t '(cell-inert cell-input cell-output))
         (starts-with-equal? (tree-ref t 1)))
        (else #f)))

(define (remove-equal t)
  (if (tree-atomic? t)
      (if (not (string-starts? (tree->string t) "=")) t
          (string->tree (string-drop (tree->string t) 1)))
      (let* ((lab (tree-label t))
             (l (tree-children t)))
        (cond ((tree-in? t '(math concat document))
               (tm->tree `(,lab ,(remove-equal (car l)) ,@(cdr l))))
              ((tree-in? t '(with))
               (tm->tree `(,lab ,@(cDr l) ,(remove-equal (cAr l)))))
              ((tree-in? t '(cell-inert cell-input cell-output))
               (tm->tree `(,lab ,(car l) ,(remove-equal (cadr l)) ,@(cddr l))))
              (else t)))))

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

(tm-define (cell-input-expand t)
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'concat)
         (with l (map cell-input-expand (tree-children t))
           (tm->tree (apply tmconcat l))))
        ((tree-is? t 'cell-range)
         (let* ((c1 (tree-ref t 0))
                (c2 (tree-ref t 1))
                (l (cell-ref-range c1 c2))
                (cc `(concat ,@(list-intersperse l ","))))
           (tm->tree cc)))
        (else (tree-map-accessible-children cell-input-expand t))))

(tm-define (calc-get-input t)
  (:require (tree-in? t '(cell-input cell-output)))
  (cell-input-expand (cell-input-encode (remove-equal (tree-ref t 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-output))
  (tree-assign-node t 'cell-input)
  (tree-go-to t 1 :end))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-input))
  (if (starts-with-equal? t)
      (begin
        (tree-assign-node t 'cell-output)
        (tree-go-to t 2 :end)
        (calc))
      (begin
        (tree-assign-node! t 'cell-inert)
        (tree-remove! t 2 1)
        (calc))))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'cell-inert))
  (if (starts-with-equal? t)
      (begin
        (tree-assign-node t 'cell-output)
        (tree-insert! t 2 '(""))
        (tree-go-to t 2 :end)
        (calc))
      (begin
        (calc))))
