
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
;; Renumbering cells of table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (calc-table-collect t r c dic)
  ;;(display* "Collecting " r ", " c ": " t "\n")
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'table)
         (with fun
             (lambda (i)
               (calc-table-collect (tree-ref t i) (+ r i) c dic))
           (tm->tree `(table ,@(map fun (.. 0 (tree-arity t)))))))
        ((tree-is? t 'row)
         (with fun
             (lambda (i)
               (calc-table-collect (tree-ref t i) r (+ c i) dic))
           (tm->tree `(row ,@(map fun (.. 0 (tree-arity t)))))))
        ((tree-is? t 'cell)
         (let* ((body (tree-ref t 0))
                (block? (tree-func? (tree-ref t 0) 'document 1))
                (new (string-append (number->row r) (number->string c))))
           (if block? (set! body (tree-ref t 0)))
           (set! body
                 (if (tree-in? body '(cell-inert cell-input cell-output))
                     (let* ((old (tree->string (tree-ref body 0)))
                            (l (tree-children body)))
                       (if (!= new old) (ahash-set! dic old new))
                       `(,(tree-label body) ,new ,@(cdr l)))
                     `(cell-inert ,new ,body)))
           (if block? (set! body `(document ,body)))
           (tm->tree `(cell ,body))))
        (else
          (with fun (lambda (u) (calc-table-collect u r c dic))
            (tree-map-children fun t)))))

(tm-define (calc-table-renumber-sub t dic)
  ;;(display* "Renumbering: " t "\n")
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'calc-table) t)
        ((tree-is? t 'cell-ref)
         (let* ((old (texmacs->string (tree-ref t 0)))
                (new (ahash-ref dic old)))
           (if new (tm->tree `(cell-ref ,new)) t)))
        ((tree-in? t '(cell-input cell-output))
         (let* ((enc (cell-input-encode (tree-ref t 1)))
                (ren (calc-table-renumber-sub enc dic))
                (dec (cell-input-decode ren)))
           (tm->tree `(,(tree-label t) ,(tree-ref t 0) ,dec ,(tree-ref t 2)))))
        (else
          (with fun (cut calc-table-renumber-sub <> dic)
            (tree-map-children fun t)))))

(tm-define (calc-table-renumber t x y)
  (let* ((dic (make-ahash-table))
         (u (calc-table-collect t x y dic)))
    (if (tree-is? u 'calc-table)
        (tree-map-children (cut calc-table-renumber-sub <> dic) u)
        (calc-table-renumber-sub u dic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating a table (e.g. after creation or a resize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-replace-cells t r)
  (cond ((== t r) (noop))
        ((or (tree-atomic? t) (tree-atomic? r)
             (!= (tree-label t) (tree-label r))
             (!= (tree-arity t) (tree-arity r)))
         ;;(display* "<<< " t "\n")
         ;;(display* ">>> " r "\n")
         (tree-set t r))
        (else (for-each tree-replace-cells
                        (tree-children t)
                        (tree-children r)))))

(tm-define (calc-table-update-table t)
  (with r (calc-table-renumber t 1 1)
    (tree-replace-cells t r)))

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
