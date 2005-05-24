
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : sxhtml.scm
;; DESCRIPTION : Utilities for HTML in SXML format
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools sxhtml)
  (:use (convert tools sxml)))

;; TODO: rewrite the predicates using DRD
;; TODO: consolidate with htmltm dispatch DRD

;; Is the node x a HTML element whose name is a given set?
(tm-define sxhtml-heading? (ntype-names?? '(h:h1 h:h2 h:h3 h:h4 h:h5 h:h6)))
(tm-define sxhtml-list? (ntype-names?? '(h:ul h:ol h:dl)))
(tm-define sxhtml-table? (ntype-names?? '(h:table)))

(tm-define (sxhtml-label? x)
  ;; Is the node x a h:a element with an id attribute?
  (and (sxml-element? x)
       (eq? 'h:a (sxml-name x))
       (sxml-attr x 'id)))

(tm-define (sxhtml-glue-label x label)
  ;; Set the id attribute of element x from the id of element label.
  (sxml-set-attr x (list 'id (sxml-attr label 'id))))

(define table-kid?
  (ntype-names?? '(h:tr h:td h:th h:col h:colgroup h:tbody h:thead h:tfoot)))
(define row-group-kid? (ntype-names?? '(h:tr h:td h:th)))
(define row-group? (ntype-names?? '(h:tbody h:thead h:tfoot)))
(define tfoot? (ntype-names?? '(h:tfoot)))
(define col-data? (ntype-names?? '(h:col h:colgroup)))
(define row? (ntype-names?? '(h:tr)))
(define cell? (ntype-names?? '(h:td h:th)))

(define (shtml-attr-number as name)
  (and-let* ((x (shtml-attr-non-null as name)))
    (string->number x)))

(define (shtml-attr-positive-integer as name)
  (and-let* ((n (shtml-attr-number as name)))
    (if (< n 0) #f (inexact->exact n))))

(tm-define (shtml-decode-span as name)
  ;; FIXME: zero spans (until end of group) are not supported
  (let ((n (shtml-attr-positive-integer as name)))
    (cond ((not n) 1) ((zero? n) 1) (else n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correct invalid element nesting in HTML tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A correct TABLE contains only COL, COLGROUP, THEAD, TFOOT and TBODY
;; elements. THEAD, TFOOT and TBODY (row groups) contains only TR elements. TR
;; elements contain only TD elements.
;;
;; Correcting a table is inferring TBODY and TR elements when they are missing
;; and filtering illegal nodes out.
;;
;; TODO: filter contents of COLGROUP elements.

(tm-define (sxhtml-correct-table x)
  ;; Correct all TABLEs in postorder in the sxml tree @x.
  (let sub ((x x))
    (cond ((sxhtml-table? x)
	   (correct-table (sxml-set-content x (map sub (sxml-content x)))))
	  ((sxml-element? x)
	   (sxml-set-content x (map sub (sxml-content x))))
	  (else x))))

(define (correct-table e)
  ;; @e must be a h:table element (ns-prefixes must be normalized)
  ;; Make TBODY elements explicit.
  ;; Collect lone TD elements in TR.
  ;; Drop table data not in TD.
  (correct-table-content e table-kid?
			 (lambda (tr tbody) (cons `(h:tr ,@tr) tbody))
			 (lambda (tbody kdr) (cons `(h:tbody ,@tbody) kdr))))

(define (correct-row-group e)
  ;; Collect lone TD elements in TR inside of @e.
  ;; Drop table data not in TD.
  (correct-table-content e row-group-kid?
			 (lambda (tr tbody) (cons `(h:tr ,@tr) tbody))
			 (lambda (tbody kdr) tbody)))

(define (correct-row e)
  ;; Drop everything but TD elements.
  (sxml-set-content e (list-filter (sxml-content e) cell? )))

(define (correct-table-content e accept? make-tr make-tbody)
  (sxml-set-content
   e ((cut <> #f) (list-fold-right
		   (lambda (kar kdr) (kdr kar))
		   (cut correct-table-content/step
			accept? make-tr make-tbody <> '() '() '())
		   (sxml-content e)))))

(define (correct-table-content/step accept? cons-tr cons-tbody
				    kar tr tbody kdr)
  (define (state kar tr tbody kdr)
    (correct-table-content/step accept? cons-tr cons-tbody kar tr tbody kdr))
  (define (flush-tr curry-tbody-kdr)
    ;; curry-tbody-kdr: (proc tbody -> state)
    ;; curries @proc like (cut proc <> tbody kdr) with an updated @tbody.
    (curry-tbody-kdr (cut state <> '() <> <>)
		     (if (null? tr) tbody (cons-tr tr tbody))))
  (define (flush-tbody curry-kdr)
    ;; curry-kdr: (proc kdr -> state)
    ;; curries @proc like (cut proc <> kdr) with an updated @kdr.
    (flush-tr
     (lambda (proc tbody)
       (curry-kdr (cut proc <> '() <>)
		  (if (null? tbody) kdr (cons-tbody tbody kdr))))))
  (cond ((not kar) (flush-tbody (lambda (proc kdr) kdr)))
	((not (accept? kar)) (cut state <> tr tbody kdr))
	((cell? kar) (cut state <> (cons kar tr) tbody kdr))
	((row? kar)
	 (flush-tr (lambda (proc tbody)
		     (cut proc <> (cons (correct-row kar) tbody) kdr))))
	((row-group? kar)
	 (flush-tbody (lambda (proc kdr)
			(cut proc <> (cons (correct-row-group kar) kdr)))))
	((col-data? kar)
	 (flush-tbody (lambda (proc kdr) (cut proc <> (cons kar kdr)))))
	;; no "else" clause needed (assuming @accept is correct)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Table iterator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Iterating over cells of a table while maintaining the coordinates of the
;; current cell is a complex operation because table positions which are
;; covered by spanned cells are not represented by cell elements in HTML.
;; Also, the TFOOT element is present before TBODY element but is logically
;; located at the end of the table.

;;;; MISSING FEATURE ;;;;
;; HTML-4.01 says that ROWSPAN=0 or COLSPAN=0 spans the cell to the end of its
;; containing group (colgroup, tbody, thead, tfoot). However, neither
;; Mozilla-1.4 nor Konqueror 3.1.3 implement this part of the specification.
;; Since this feature also requires a significant added complexity, it is left
;; out.

(tm-define (sxhtml-table-fold kons knil table)
  ;; Fundamental HTML table content iterator.
  ;; knil: T
  ;; kons: symbol (int @i) (int @j) sxml T -> T
  ;; table: sxml
  ;; Fold @kons over elements of @table, with (@i @j) the cell position.
  ;;
  ;; @kons is applied in the following modes:
  ;;   Entering a row-group  : (kons :in-row-group i #f row-group kdr)
  ;;   Entering a row        : (kons :in-row i #f row kdr)
  ;;   (possible extension: entering a colgroup and iterating over a col)
  ;;   Leaving a row-group   : (kons :out-row-group nrows #f #f kdr)
  ;;   Leaving a row         : (kons :out-row #f ncols #f kdr)
  ;;   Iterating over a cell : (kons :cell i j cell kdr)
  ((cut <> #f) (list-fold (lambda (kar kdr) (kdr kar))
			  (cut table-fold/table <> 0 '() '() kons knil)
			  (sxml-content table))))

(define (table-fold/table kar i rowspans footers kons kdr)
  ;; @kar is a child element of the TABLE element.
  (cond ((not kar)
	 (if (not footers) kdr
	     ((cut <> #f)
	      (list-fold (lambda (kar kdr) (kdr kar))
			 (cut table-fold/table <> i rowspans #f kons kdr)
			 (reverse! footers)))))
	((and footers (tfoot? kar))
	 (cut table-fold/table <> i rowspans (cons kar footers) kons kdr))
	((row-group? kar)
	 ((cut <> #f)
	  (list-fold (lambda (kar kdr) (kdr kar))
		     (cut table-fold/group <> i i rowspans footers kons
			  (kons :in-row-group i #f kar kdr))
		     (sxml-content kar))))
	;; ELSE clause for col-data elements.
	;; NOTE: could be extended to support parsing of col-data
	(else (cut table-fold/table <> i rowspans footers kons kdr))))

(define (table-fold/group kar i0 i rowspans footers kons kdr)
  ;; @kar is a child element of a THEAD, TBODY or TFOOT element.
  (cond ((not kar) (cut table-fold/table <> i rowspans footers kons
			(kons :out-row-group (- i i0) #f #f kdr)))
	((row? kar)
	 ((cut <> #f)
	  (list-fold (lambda (kar kdr) (kdr kar))
		     (cut table-fold/row <> i0 i 0 rowspans footers kons
			  (kons :in-row i #f kar kdr))
		     (sxml-content kar))))
	;; ELSE clause should never be reached (the table is corrected).
	(else (cut table-fold/group <> i0 i rowspans footers kons kdr))))

(define (table-fold/row kar i0 i j rowspans footers kons kdr)
  ;; @kar is a child element of a TR element.
  (cond ((not kar)
	 (cut table-fold/group <> i0 (1+ i) (next-rowspans rowspans) footers
	      kons (kons :out-row #f (skip-spanned-cols j rowspans) #f kdr)))
	((cell? kar)
	 (let ((j (skip-spanned-cols j rowspans))
	       (a (sxml-attr-list kar)))
	   (let ((rspan (shtml-decode-span a 'rowspan))
		 (cspan (shtml-decode-span a 'colspan)))
	 (cut table-fold/row <> i0 i (+ j cspan)
	      (if (= 1 rspan) rowspans
		  (add-rowspan rowspans j rspan cspan))
	      footers kons (kons :cell i j kar kdr)))))
	;; ELSE clause should never be reached (the table is corrected).
	(else (cut table-fold/row <> i0 i j rowspans footers kons kdr))))

;(set-trace-level! sxhtml-table-fold
;		  table-fold/table table-fold/group table-fold/row)

;; Columns on which a cell is spanned are remember in a sorted ROWSPANS list.
;; Items of ROWSPANS are lists (J SPAN) where:
;;   J    : column number (zero-based). ROWSPANS is sorted by ascending J.
;;   SPAN : count of additional rows (incl. the current row) where this column
;;          is occupied by a spanned cell.

(define (skip-spanned-cols j rowspans)
  (let next ((j j) (rowspans rowspans))
    (cond ((null? rowspans) j)
	  ((< j (first (car rowspans))) j)
	  ((= j (first (car rowspans))) (next (1+ j) (cdr rowspans)))
	  (else (next j (cdr rowspans))))))

(define (add-rowspan rowspans j rspan cspan)
  (let next ((rowspans rowspans) (j j) (cspan cspan))
    (cond ((zero? cspan) rowspans)
	  ((null? rowspans)
	   (cons (list j rspan) (next '() (1+ j) (1- cspan))))
	  ((< j (first (car rowspans)))
	   (cons (list j rspan) (next rowspans (1+ j) (1- cspan))))
	  ((= j (first (car rowspans)))
	   ;; This can only happen with some very vicious incorrect HTML.
	   (cons (list j (max rspan (second (car rowspans))))
		 (next (cdr rowspans) (1+ j) (1- cspan))))
	  (else (cons (car rowspans) (next (cdr rowspans)))))))

(define (next-rowspans rowspans)
  (reverse! (list-fold (lambda (kar kdr)
			 (with (col old-span) kar
			   (let ((span (1- old-span)))
			     (if (zero? span)
				 kdr
				 (cons (list col span) kdr)))))
		       '() rowspans)))

;(set-trace-level! skip-spanned-cols add-rowspan next-rowspans)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Table iterator applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (sxhtml-table-dimension table)
  (sxhtml-table-fold
   (lambda (msg i j kar kdr)
     (with (nrows ncols) kdr
       (list (if (eq? msg :out-row-group) (+ nrows i) nrows)
	     (if (eq? msg :out-row) (max ncols j) ncols))))
   '(0 0) table))
