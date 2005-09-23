
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtable.scm
;; DESCRIPTION : tools for converting tables from and to other formats
;; COPYRIGHT   : (C) 2002  David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmtable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for raising tformat tags in a table to the outermost level
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
;; Routines for getting table and row dimensions and
;; completing missing elements in table rows with empty strings
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
;; Transform negative indices in table format to positive ones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtformat-positive-indices l nrrows nrcols)
  (define (transform x max)
    (with n (string->number x)
      (if (< n 0) (set! n (+ max 1 n)))
      (number->string n)))
  (cond ((null? l) l)
	((not (func? l 'cwith 7))
	 (cons (car l) (tmtformat-positive-indices (cdr l) nrrows nrcols)))
	(else (with c (car l)
		(cons (cons* 'cwith
			     (transform (second c) nrrows)
			     (transform (third c) nrrows)
			     (transform (fourth c) nrcols)
			     (transform (fifth c) nrcols)
			     (cddddr (cdr c)))
		      (tmtformat-positive-indices (cdr l) nrrows nrcols))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract table-, column-, and row- properties from general format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (tmtformat-split l pred?)
;  (if (null? l) (values l l)
;      (receive (match rem) (tmtformat-split (cdr l) pred?)
;	(if (list-find match (cut tmcwith<=? (car l) <>))
;	    (values match rem)
;	    (if (pred? (car l))
;		(values (cons (car l) match) rem)
;		(values match (cons (car l) rem)))))))

(define (tmtformat-split l pred?)
  (list-partition l pred?))

(define (tmtformat-divide l nrrows nrcols)
  (define (table-cwith? x)
    (and (func? x 'cwith 7)
	 (== (second x) "1") (== (third x) (number->string nrrows))
	 (== (fourth x) "1") (== (fifth x) (number->string nrcols))))
  (define (column-cwith? x)
    (and (func? x 'cwith 7)
	 (== (second x) "1") (== (third x) (number->string nrrows))))
  (define (row-cwith? x)
    (and (func? x 'cwith 7)
	 (== (fourth x) "1") (== (fifth x) (number->string nrcols))))
  (receive (table-l table-r) (tmtformat-split l table-cwith?)
    (receive (column-l column-r) (tmtformat-split table-r column-cwith?)
      (receive (row-l row-r) (tmtformat-split column-r row-cwith?)
	(values table-l column-l row-l row-r)))))
