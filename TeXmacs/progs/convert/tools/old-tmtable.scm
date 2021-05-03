
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : old-tmtable.scm
;; DESCRIPTION : tools for converting tables from and to other formats
;; COPYRIGHT   : (C) 2002  David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools old-tmtable)
  (:use (convert tools tmlength) (convert tools tmcolor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-max l)
  ;; WARNING: not portable for long lists
  (apply max l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic table abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Foundation

(define tmtable-type (make-record-type "tmtable" '(nrows ncols cells formats)))
(define tmtable-record (record-constructor tmtable-type))
(tm-define tmtable? (record-predicate tmtable-type))
(tm-define tmtable-nrows (record-accessor tmtable-type 'nrows))
(tm-define tmtable-ncols (record-accessor tmtable-type 'ncols))
(tm-define tmtable-cells (record-accessor tmtable-type 'cells))
(define tmtable-formats (record-accessor tmtable-type 'formats))

(tm-define (tmtable formats cells)
  ;; Public tmtable constructor
  (tmtable-record (length cells)
		  (list-max (map length cells))
		  cells
		  formats))

(define (tmtable-cell t i j)
  ;; Content of a given cell
  (list-ref (list-ref (tmtable-cells t) i) j))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table format

(tm-define (tmformat-frame name value)
  `(twith ,name ,value))

;; NOTE: if this list gets too long, we might define region arithmetic.
;; This might also be needed for format simplification.
(tm-define (tmformat-table name value)
  `(cwith 1 -1 1 -1 ,name ,value))
(tm-define (tmformat-table-but-top name value)
  `(cwith 2 -1 1 -1 ,name ,value))
(tm-define (tmformat-table-but-bottom name value)
  `(cwith 1 -2 1 -1 ,name ,value))
(tm-define (tmformat-table-but-left name value)
  `(cwith 1 -1 2 -1 ,name ,value))
(tm-define (tmformat-table-but-right name value)
  `(cwith 1 -1 1 -2 ,name ,value))
(tm-define (tmformat-cell i j name value)
  `(cwith ,i ,i ,j ,j ,name ,value))

(define (tmformat-frame? f) (func? f 'twith))
(define (tmformat-cell? f) (func? f 'cwith))
(define (tmformat-cell-name f) (sixth f))
(define (tmformat-cell-value f) (if (seventh f) (seventh f) '()))

(define (tmtable-format-on-cell? t f i j)
  (and (tmformat-cell? f)
       (with (sym I1 I2 J1 J2 var val) f
	 (let ((i1 (decode-row t I1))
	       (i2 (decode-row t I2))
	       (j1 (decode-column t J1))
	       (j2 (decode-column t J2)))
	   (and (>= i i1) (<= i i2)
		(>= j j2) (<= j j2))))))

(define (tmtable-format-on-row? t f i)
  (with nc (tmtable-ncols t)
    (and (tmformat-cell? f)
         (with (sym I1 I2 J1 J2 var val) f
           (and (== 1 J1) (or (== -1 J2) (== nc J2))
                (let ((i1 (decode-row t I1))
                      (i2 (decode-row t I2)))
                  (and (>= i i1) (<= i i2))))))))

(define (tmtable-format-on-column? t f j)
  (with nr (tmtable-nrows t)
    (and (tmformat-cell? f)
         (with (sym I1 I2 J1 J2 var val) f
           (and (== 1 I1) (or (== -1 I2) (== nr I2))
                (let ((j1 (decode-column t J1))
                      (j2 (decode-column t J2)))
                  (and (>= j j1) (<= j j2))))))))

(define (decode i n) (cond ((< i 0) (+ i n)) ((> i 0) (1- i)) (else 0)))
(define (decode-row t i) (decode i (tmtable-nrows t)))
(define (decode-column t j) (decode j (tmtable-ncols t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table format partitioning

(define (tmtable-format-partition-by-column t fs)
  (map (lambda (j)
	 (list-filter
	  fs (cut tmtable-format-on-column? t <> j)))
       (iota (tmtable-ncols t))))

(define (tmtable-format-partition-by-row t fs)
  (map (lambda (j)
	 (list-filter
	  fs (cut tmtable-format-on-row? t <> j)))
       (iota (tmtable-nrows t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: simplification of formats.
;;   remove ignored formats
;;   collect formats which apply to whole lines/rows/table
;;   collect formats which apply to ranges of cells/lines/rows
;;     (giving precedence to extensible boundaries in case of conflict)

(define (tmtable-simplify t) (noop t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs table parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TeXmacs table is a TABLE primitive or a TFORMAT containing a number of
;; properties item and terminated by a TABLE or a TFORMAT.
;;
;; The TFORMAT properties apply to arbitrary contiguous ranges of the table.
;;
;; The properties defined in inner TFORMAT take precedence, so the TFORMAT must
;; be read in preorder. In a simple implementation, the TABLE would then be
;; read last.

(define (stm-table-cell-content x)
  ;; assert (func? x 'cell)
  (if (func? x 'cell) (second x) x))

(define (stm-table-row->list x)
  ;; assert (func? x 'row)
  (map stm-table-cell-content (cdr x)))

(define (stm-table-cells x)
  ;; @x must be a TFORMAT or TABLE element
  ;; Return a list of list of cell contents.
  ;; Each inner list correspond to a table row.
  (map stm-table-row->list
       (do ((x x (last x)))
	   ((or (func? x 'table) (func? x '!arg))
            (if (func? x 'table) (cdr x) `((row (cell ""))))))))

(define (stm-table-formats x)
    (cond ((func? x 'table) '())
	  ((func? x 'tformat)
	   (append (map (lambda (f)
			  (with (sym i1 i2 j1 j2 name value) f
			    (list sym
				  (string->number i1)
				  (string->number i2)
				  (string->number j1)
				  (string->number j2)
				  name
				  (stm-table-decode-format name value))))
			(list-filter (cDdr x)
                                     (lambda (l)
                                        (and (list? l) (= (length l) 7)))))
		   (stm-table-formats (cAr x))))
         (else '())))

(define (stm-table-length-name? name)
  (in? name
       '("cell-width" "cell-height"
	 "cell-lsep" "cell-rsep" "cell-bsep" "cell-tsep"
	 "cell-lborder" "cell-rborder" "cell-bborder" "cell-tborder"
	 "table-width" "table-height"
	 "table-lsep" "table-rsep" "table-bsep" "table-tsep"
	 "table-lborder" "table-rborder" "table-bborder" "table-tborder")))

(define (stm-table-number-name? name)
  (in? name
       '("cell-row-span" "cell-col-span"
	 "table-row-origin" "table-col-origin"
	 "table-min-rows" "table-min-cols"
	 "table-max-rows" "table-max-cols")))

(define (stm-table-color-name? name)
  (string=? name "cell-background"))

(define (stm-table-decode-format name value)
  ((cond ((stm-table-length-name? name)
	  string->tmlength)
	 ((stm-table-number-name? name)
	  string->number)
	 ((stm-table-color-name? name)
	  stm->tmcolor)
	 (else noop))
   (force-string value)))

(define (stm->tmtable x)
  (tmtable (reverse (stm-table-formats x))
	   (stm-table-cells x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs table output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stm-table-encode-format name value)
  ((cond ((stm-table-length-name? name)
	  tmlength->string)
	 ((stm-table-number-name? name)
	  number->string)
	 ((stm-table-color-name? name)
	  tmcolor->stm)
	 (else noop))
   value))

(tm-define (tmtable->stm t)
  (receive (centered formats)
      (list-partition (tmtable-formats t)
		      (cute == <> (tmformat-table "cell-halign" "c")))
    `(,(if (null? centered) 'tabular 'tabular*)
      (tformat
       ,@(reverse!
	  (map (lambda (f)
		 (cond ((tmformat-cell? f)
			(with (sym i1 i2 j1 j2 name value) f
			  (list sym
				(number->string i1) (number->string i2)
				(number->string j1) (number->string j2)
				name
				(stm-table-encode-format name value))))
		       ((tmformat-frame? f)
			(with (sym name value) f
			  (list sym name
				(stm-table-encode-format name value))))))
	       formats))
       ,(tmtable-cells->stm t)))))

(define (make-block-cell x)
  ;; NOTE: since TeXmacs 1.0.6.2, hyphenated => block structure
  (if (func? x 'document)
      `(cell ,x)
      `(cell (document ,x))))

(define (tmtable-cells->stm t)
  (cons 'table
	(map (lambda (r) (cons 'row (map make-block-cell r)))
	     (tmtable-cells t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Legacy table parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtable-parser x)
  ;; Internal state
  (let ((this (stm->tmtable x)))

    ;; Public getters
    (define (global name)
      (cond ((eq? name 'border) (any-border?))
	    ((eq? name 'nrcols) (tmtable-ncols this))))
    (define (cols name)
      (cond ((eq? name 'halign) (format-by :column "cell-halign" "l"))
	    ((eq? name 'tborder) (length-non-zero-by :column "cell-tborder"))
	    ((eq? name 'bborder) (length-non-zero-by :column "cell-bborder"))
	    ((eq? name 'lborder) (length-non-zero-by :column "cell-lborder"))
	    ((eq? name 'rborder) (length-non-zero-by :column "cell-rborder"))))
    (define (rows name)
      (cond ((eq? name 'content) (tmtable-cells this))
	    ((eq? name 'tborder) (length-non-zero-by :row "cell-tborder"))
	    ((eq? name 'bborder) (length-non-zero-by :row "cell-bborder"))
      	    ((eq? name 'lborder) (length-non-zero-by :row "cell-lborder"))
	    ((eq? name 'rborder) (length-non-zero-by :row "cell-rborder"))))

    ;; Public dispatcher
    (define (table-parser/dispatch scope . args)
      (apply (cond ((eq? scope 'global) global)
		   ((eq? scope 'cols) cols)
		   ((eq? scope 'rows) rows)) args))

    ;; Private
    (define (any-border?)
      (list-any noop
		(append (append-map cols '(tborder bborder lborder rborder))
			(append-map rows '(tborder bborder lborder rborder)))))

    (define (format-by axis name default)
      (map (lambda (fs)
             (if (or (null? fs) (null? (last fs)) (null? (last (car fs))))
                 default
                 (tmformat-cell-value (first fs))))
	   ((cond ((eq? axis :row) tmtable-format-partition-by-row)
		  ((eq? axis :column) tmtable-format-partition-by-column))
	    this
	    (list-filter (tmtable-formats this)
			 (lambda (f)
			   (and (tmformat-cell? f)
				(== name (tmformat-cell-name f))))))))

    (define (length-non-zero-by axis name)
      (map (lambda (len) (!= 0 (tmlength-value len)))
	   (format-by axis name (tmlength))))

    ;; evaluate to dispatcher closure
    table-parser/dispatch))

(tm-define (tmtable-block-borders x)
  (if x '((cwith "1" "-1" "1" "1" "cell-lborder" "1ln")
	  (cwith "1" "1" "1" "-1" "cell-tborder" "1ln")
	  (cwith "1" "-1" "1" "-1" "cell-bborder" "1ln")
	  (cwith "1" "-1" "1" "-1" "cell-rborder" "1ln"))
      '()))

(tm-define (tmtable-cell-halign x)
  `((cwith "1" "-1" "1" "-1" "cell-halign" ,x)))
