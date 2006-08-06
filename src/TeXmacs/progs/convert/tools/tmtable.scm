
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtable.scm
;; DESCRIPTION : tools for converting tables from and to other formats
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmtable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalization of table removes spurious document tags and
;; merges nested tformat tags for the outmost level.
;; All other routines in this file assume that the table is in normal form.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtable-normalize t)
  (:synopsis "Normalization of the table @t")
  (cond ((func? t 'tformat)
	 (with u (tmtable-normalize (cAr t))
	   (if (func? u 'tformat)
	       (append (cDr t) (cDdr u) (list (cAr u)))
	       (rcons (cDr t) u))))
	((func? t 'document 1) (tmtable-normalize (cadr t)))
	((func? t 'table) t)
	(else (texmacs-error "tmtable-normalize" "~S is not a table" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for getting table and row dimensions and
;; completing missing elements in table rows with empty strings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmrow-cols r)
  (:synopsis "Return the number of columns of the row @r")
  (cond ((func? r 'tformat) (tmrow-cols (cAr r)))
	((func? r 'row) (- (length r) 1))
	(else (texmacs-error "tmrow-cols" "~S is not a row" r))))

(tm-define (tmtable-cols t)
  (:synopsis "Return the number of columns of the table @t")
  (cond ((func? t 'tformat) (tmtable-cols (cAr t)))
	((== t '(table)) 0)
	((func? t 'table) (apply max (map tmrow-cols (cdr t))))
	(else (texmacs-error "tmtable-cols" "~S is not a table" t))))

(tm-define (tmtable-rows t)
  (:synopsis "Return the number of rows of the table @t")
  (cond ((func? t 'tformat) (tmtable-rows (cAr t)))
	((func? t 'table) (- (length t) 1))
	(else (texmacs-error "tmtable-rows" "~S is not a table" t))))

(define (tmrow-complete r n)
  "Complete the row @r with empty strings to become at least @n columns wide"
  (cond ((func? r 'tformat) (rcons (cDr r) (tmrow-complete (cAr r) n)))
	((== r '(row)) (cons 'row (make-list (max n 0) '(cell ""))))
	((func? r 'row)
	 (with next (tmrow-complete (cons 'row (cddr r)) (- n 1))
	   (cons* 'row (cadr r) (cdr next))))
	(else (texmacs-error "tmrow-complete" "~S is not a row" r))))

(tm-define (tmtable-complete t)
  (:synopsis "Completes missing cells on rows of @t with empty strings")
  (cond ((func? t 'tformat) (rcons (cDr t) (tmtable-complete (cAr t))))
	((func? t 'table)
	 (with cols (apply max (map tmrow-cols (cdr t)))
	   (cons 'table (map (cut tmrow-complete <> cols) (cdr t)))))
	(else (texmacs-error "tmtable-complete" "~S is not a table" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for raising tformat tags in a table to the outermost level.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmcell-format-up c)
  (cond ((func? c 'tformat) c)
	((func? c 'cell) `(tformat ,c))
	(else (texmacs-error "tmcell-format-up" "~S is not a cell" c))))

(define (tmrow-append r1 r2)
  `(tformat ,@(cDdr r1) ,@(cDdr r2) (row ,@(cdAr r1) ,@(cdAr r2))))

(define (tmrow-format-up-sub l nr)
  (if (null? l) `(tmformat (row))
      (let* ((cell (tmcell-format-up (car l)))
	     (snr (number->string nr))
	     (fun (lambda (x) `(cwith ,snr ,snr ,@(cdr x))))
	     (head `(tformat ,@(map fun (cDdr cell)) (row ,(cAr cell)))))
	(tmrow-append head (tmrow-format-up-sub (cdr l) (+ nr 1))))))

(define (tmrow-format-up r)
  "Raise tformat tags in cells and cells of @r to the upmost level"
  (cond ((func? r 'tformat)
	 (with s (tmrow-raise-format (cAr r))
	   (append (cDr r) (cdr s))))
	((func? r 'row) (tmrow-format-up-sub (cdr r) 1))
	(else (texmacs-error "tmrow-format-up" "~S is not a row" r))))

(define (tmtable-append t1 t2)
  `(tformat ,@(cDdr t1) ,@(cDdr t2) (table ,@(cdAr t1) ,@(cdAr t2))))

(define (tmtable-format-up-sub l nr)
  (if (null? l) `(tmformat (table))
      (let* ((row (tmrow-format-up (car l)))
	     (snr (number->string nr))
	     (fun (lambda (x) `(cwith ,snr ,snr ,@(cdr x))))
	     (head `(tformat ,@(map fun (cDdr row)) (table ,(cAr row)))))
	(tmtable-append head (tmtable-format-up-sub (cdr l) (+ nr 1))))))

(tm-define (tmtable-format-up t)
  (:synopsis "Raise tformat tags in rows and cells of @t to the upmost level")
  (cond ((func? t 'tformat)
	 (with u (tmtable-format-up (cAr t))
	   (append (cDr t) (cdr u))))
	((func? t 'table) (tmtable-format-up-sub (cdr t) 1))
	(else (texmacs-error "tmtable-format-up" "~S is not a table" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform negative indices in table format to positive ones.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmindex-get x last)
  (with n (string->number x)
    (if (< n 0) (+ last 1 n) n)))

(define (tmindex-positive x last)
  (number->string (tmindex-get x last)))

(define (tmtformat-positive l nrrows nrcols)
  (cond ((null? l) l)
	((not (func? l 'cwith 6))
	 (cons (car l) (tmtformat-positive (cdr l) nrrows nrcols)))
	(else (with c (car l)
		(cons (cons* 'cwith
			     (tmindex-positive (second c) nrrows)
			     (tmindex-positive (third c) nrrows)
			     (tmindex-positive (fourth c) nrcols)
			     (tmindex-positive (fifth c) nrcols)
			     (cddddr (cdr c)))
		      (tmtformat-positive (cdr l) nrrows nrcols))))))

(tm-define (tmtable-positive t)
  (:synopsis "Transform negative indices in table format into positive ones")
  ;; NOTE: assumes format of t to be raised upwards
  (cond ((func? t 'tformat)
	 (let* ((rows (tmtable-rows t))
		(cols (tmtable-cols t)))
	   (rcons (tmformat-positive (cDr t) rows cols)
		  (tmtable-positive (cAr t)))))
	((func? t 'table) t)
	(else (texmacs-error "tmtable-positive" "~S is not a table" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract table-, column-, row- and cell- properties from general format.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmindex-first? x last)
  (or (== x "1") (== x (number->string (- last)))))

(define (tmindex-last? x last)
  (or (== x "-1") (== x (number->string last))))

(define (tmtformat-split l pred?)
  ;; FIXME: we should also check whether the matching cwith items
  ;; commute with the preceeding ones in the list l
  (list-partition l pred?))

(define (tmtformat-formats l nrrows nrcols)
  (define (table-cwith? x)
    (or (func? x 'twith 2)
	(and (func? x 'cwith 6)
	     (tmindex-first? (second x) nrrows)
	     (tmindex-last? (third x) nrrows)
	     (tmindex-first? (fourth x) nrcols)
	     (tmindex-last? (fifth x) nrcols))))
  (define (column-cwith? x)
    (and (func? x 'cwith 6)
	 (tmindex-first? (second x) nrrows)
	 (tmindex-last? (third x) nrrows)))
  (define (row-cwith? x)
    (and (func? x 'cwith 6)
	 (tmindex-first? (fourth x) nrcols)
	 (tmindex-last? (fifth x) nrcols)))
  (receive (table-l table-r) (tmtformat-split l table-cwith?)
    (receive (column-l column-r) (tmtformat-split table-r column-cwith?)
      (receive (row-l row-r) (tmtformat-split column-r row-cwith?)
	(values table-l column-l row-l row-r)))))

(tm-define (tmtable-formats t)
  (:synopsis "Find table-, column-, row- and cell- format lists of @t")
  ;; NOTE: assumes format of t to be raised upwards
  (cond ((func? t 'tformat)
	 (let* ((rows (tmtable-rows t))
		(cols (tmtable-cols t)))
	   (tmtformat-formats (cDdr t) rows cols)))
	((func? t 'table) (values '() '() '() '()))
	(else (texmacs-error "tmtable-formats" "~S is not a table" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract property lists for tables and individual rows, columns and cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtformat-table-prop x)
  (cond ((func? x 'twith 2) (cdr x))
	((func? x 'cwith 6) (cddddr (cdr x)))
	(else #f)))

(define (tmtformat-table-props l)
  "Extract table property list from table format list"
  ;; The table format list is the first result of a call to tmtformat-formats
  (list-filter (map tmtformat-table-prop l) identity))

(define (tmtformat-prop x start end)
  (and (func? x 'cwith)
       (>= (length x) 5)
       (>= start (tmindex-get (cadr x) end))
       (<= start (tmindex-get (caddr x) end))
       (cdddr x)))

(define (tmtformat-props l start end)
  (list-filter (map (cut tmtformat-prop <> start end) l) identity))

(define (tmtformat-props-list l start end)
  (if (> start end) '()
      (cons (tmtformat-props l start end)
	    (tmtformat-props-list l (+ start 1) end))))

(define (tmtformat-column-props l nr-cols)
  "Extract list of column property lists from column format list"
  ;; The column format list is the second result of a call to tmtformat-formats
  (with r (map (lambda (x) (and (func? x 'cwith 6) (cons 'cwith (cdddr x)))) l)
    (tmtformat-props-list r 1 nr-cols)))

(define (tmtformat-row-props l nr-rows)
  "Extract list of row property lists from row format list"
  ;; The row format list is the third result of a call to tmtformat-formats
  (map (lambda (l) (map cddr l)) (tmtformat-props-list l 1 nr-rows)))

(define (tmtformat-cell-props-sub l nr-cols)
  (with r (map (lambda (x) (cons 'cwith x)) l)
    (tmtformat-props-list r 1 nr-cols)))

(define (tmtformat-cell-props l nr-rows nr-cols)
  "Extract list of lists of cell property lists from cell format list"
  ;; The cell format list is the fourth result of a call to tmtformat-formats
  (map (cut tmtformat-cell-props-sub <> nr-cols)
       (tmtformat-props-list l 1 nr-rows)))

(tm-define (tmtable-properties t)
  (:synopsis "Find table-, column-, row- and cell- properties of @t")
  ;; NOTE: assumes format of t to be raised upwards
  (receive (tablef colf rowf cellf) (tmtable-formats t)
    (let* ((rows (tmtable-rows t))
	   (cols (tmtable-cols t)))
      (values (tmtformat-table-props tablef)
	      (tmtformat-column-props colf cols)
	      (tmtformat-row-props rowf rows)
	      (tmtformat-cell-props cellf rows cols)))))

(tm-define (tmtable-properties* t)
  (:synopsis "Find table-, column-, row- and cell- properties of @t")
  ;; variant of tmtable-properties: use only cell properties
  (receive (tablef colf rowf cellf) (tmtable-formats t)
    (receive (t1 t2) (list-partition tablef (lambda (x) (func? x 'twith)))
      (set! tablef t1)
      (set! cellf (append t2 colf rowf cellf))
      (set! colf '())
      (set! rowf '())
      (let* ((rows (tmtable-rows t))
	     (cols (tmtable-cols t)))
	(values (tmtformat-table-props tablef)
		(tmtformat-column-props colf cols)
		(tmtformat-row-props rowf rows)
		(tmtformat-cell-props cellf rows cols))))))

(define (tmtformat-halign? prop)
  (and (func? prop 'cwith 6) (== (sixth prop) "cell-halign")))

(tm-define (tmtable-properties** t)
  (:synopsis "Find table-, column-, row- and cell- properties of @t")
  ;; variant of tmtable-properties for TeXmacs -> MathML converter
  (receive (tablef colf rowf cellf) (tmtable-formats t)
    (set! colf (append (list-filter tablef tmtformat-halign?) colf))
    (set! tablef (list-filter tablef (non tmtformat-halign?)))
    (let* ((rows (tmtable-rows t))
	   (cols (tmtable-cols t)))
      (values (tmtformat-table-props tablef)
	      (tmtformat-column-props colf cols)
	      (tmtformat-row-props rowf rows)
	      (tmtformat-cell-props cellf rows cols)))))
