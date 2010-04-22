
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-compare.scm
;; DESCRIPTION : comparing two files
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-compare)
  (:use (version version-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines for document comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (denormalize-string s)
  (with d (string-index s #\space)
    (if (or (not d) (== s " ")) (list s)
	(let ((h (substring s 0 d))
	      (t (denormalize-string (substring s (+ 1 d) (string-length s)))))
	  (cond ((== d 0) (cons " " t))
		((== d (- (string-length s) 1)) (list h " "))
		(else (cons h (cons " " t))))))))

(define (denormalize-concat l)
  (cond ((null? l) l)
	((string? (car l))
	 (append (denormalize-string (car l))
		 (denormalize-concat (cdr l))))
	((tm-is? (car l) 'concat)
	 (denormalize-concat (append (cdar l) (cdr l))))
	(else (cons (car l) (denormalize-concat (cdr l))))))

(define (denormalize t)
  (cond ((string? t)
	 `(concat ,@(denormalize-concat (list t))))
	((tm-is? t 'concat)
	 `(concat ,@(denormalize-concat (cdr t))))
	((tm-is? t 'document)
	 `(document ,@(map denormalize (cdr t))))
	(else t)))

(define (normalize-concat l)
  (cond ((null? l) l)
	((tm-is? (car l) 'concat)
	 (normalize-concat (append (cdar l) (cdr l))))
	(else (cons (car l) (normalize-concat (cdr l))))))

(define (normalize-concat-string l)
  (with i (list-find-index l nstring?)
    (if (not i) (set! i (length l)))
    (if (== i 0)
	(if (null? l) l (cons (car l) (normalize-concat-string (cdr l))))
	(cons (apply string-append (sublist l 0 i))
	      (normalize-concat-string (list-tail l i))))))

(define (normalize t)
  (cond ((string? t) t)
	((tm-is? t 'concat)
	 (with l (normalize-concat-string (normalize-concat (cdr t)))
	   (cond ((null? l) "")
		 ((null? (cdr l)) (car l))
		 (else `(concat ,@l)))))
	((tm-is? t 'document)
	 (cons 'document (map normalize (cdr t))))
	(else t)))

(define (diff t1 t2)
  ;;(display* "diff " t1 ", " t2 "\n")
  (cond ((and (or (string? t1) (!= (car t1) 'document))
	      (tree-multi-paragraph? (tm->tree t2)))
	 (diff `(document ,t1) t2))
	((and (or (string? t2) (!= (car t2) 'document))
	      (tree-multi-paragraph? (tm->tree t1)))
	 (diff t1 `(document ,t2)))
	(else `(version-both ,t1 ,t2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding a long common subsequence where to break
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (common l1 l2)
  (if (or (null? l1) (null? l2) (!= (car l1) (car l2))) 0
      (+ (common (cdr l1) (cdr l2)) 1)))

(define (longest-common l1 l2)
  ;;(display* "longest-common " l1 ", " l2 "\n")
  (cond ((or (null? l1) (null? l2))
	 (values 0 0 0))
	((and (in? (car l1) l2) (!= (car l1) " "))
	 ;; NOTE: don't consider breaks at spaces
	 (let* ((i (list-find-index l2 (lambda (x) (== x (car l1)))))
		(r2 (list-tail l2 i))
		(n (common l1 r2)))
	   (if (>= n 25) (values 0 i n)
	       ;; NOTE: truncate for efficiency reasons
	       (receive (i1 i2 nn) (longest-common l1 (cdr r2))
		 (if (>= n nn)
		     (values 0 i n)
		     (values i1 (+ i2 i 1) nn))))))
	(else
	  (receive (i1 i2 n) (longest-common (cdr l1) l2)
	    (values (+ i1 1) i2 n)))))

(define (skeleton t)
  (cond ((string? t) t)
	((tm-is? t 'concat)
	 (with f (list-find (cdr t) (lambda (x) (!= x " ")))
	   (if f (skeleton f) '(concat 0))))
	(else (list (car t) (- (length t) 1)))))

(define (var-longest-common l1 l2)
  (receive (i1 i2 n) (longest-common l1 l2)
    (if (> n 0) (values i1 i2 n)
	(longest-common (map skeleton l1) (map skeleton l2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show old and new versions into a single document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compare-versions-list tag l1 l2)
  ;;(display* "compare-versions-list " tag ", " l1 ", " l2 "\n\n")
  (cond ((and (null? l1) (null? l2)) '())
	((null? l1) (list (diff "" (normalize `(,tag ,@l2)))))
	((null? l2) (list (diff (normalize `(,tag ,@l1)) "")))
	((== (car l1) (car l2))
	 (cons (car l1) (compare-versions-list tag (cdr l1) (cdr l2))))
	(else
	  (receive (i1 i2 n) (var-longest-common l1 l2)
	    ;;(display* "  break at " i1 ", " i2 ", " n "\n\n")
	    (cond ((== n 0)
		   (list (diff (normalize `(,tag ,@l1))
			       (normalize `(,tag ,@l2)))))
		  ((and (== n (length l1)) (== n (length l2)))
		   (map compare-versions l1 l2))
		  (else
		    (let* ((ll1 (sublist l1 0 i1))
			   (ll2 (sublist l2 0 i2))
			   (mm1 (sublist l1 i1 (+ i1 n)))
			   (mm2 (sublist l2 i2 (+ i2 n)))
			   (rr1 (sublist l1 (+ i1 n) (length l1)))
			   (rr2 (sublist l2 (+ i2 n) (length l2))))
		      (append (compare-versions-list tag ll1 ll2)
			      (compare-versions-list tag mm1 mm2)
			      (compare-versions-list tag rr1 rr2)))))))))

(tm-define (compare-versions t1 t2)
  ;;(display* "compare-versions " t1 ", " t2 "\n\n")
  (cond ((== t1 t2) t1)
	((and (string? t1) (string? t2))
	 (compare-versions `(concat ,t1) `(concat ,t2)))
	((and (not (tm-is? t1 'concat)) (tm-is? t2 'concat))
	 (compare-versions `(concat ,t1) t2))
	((and (tm-is? t1 'concat) (not (tm-is? t2 'concat)))
	 (compare-versions t1 `(concat ,t2)))
	((and (list? t1) (list? t2) (== (car t1) (car t2)))
	 (cond ((tm-in? t1 '(document concat))
		(let* ((l1 (cdr (denormalize t1)))
		       (l2 (cdr (denormalize t2)))
		       (m (compare-versions-list (car t1) l1 l2)))
		  (normalize (cons (car t1) m))))
	       ((!= (length t1) (length t2))
		(diff t1 t2))
	       ((in? (car t1) '(graphics))
		(diff t1 t2))
	       (else
		 (let* ((tt1 (tm->tree t1))
			(tt2 (tm->tree t2)))
		   (with wrong?
		       (lambda (i)
			 (and (!= (tree-ref tt1 i) (tree-ref tt2 i))
			      (not (tree-accessible-child? tt1 i))
			      (not (tree-accessible-child? tt2 i))))
		     (if (list-or (map wrong? (.. 0 (tree-arity tt1))))
			 (diff t1 t2)
			 (cons (car t1)
			       (map compare-versions (cdr t1) (cdr t2)))))))))
	(else (diff t1 t2))))

(tm-define (compare-file old)
  (let* ((t1 (tree-load-inclusion old))
	 (t2 (buffer-tree))
	 (u1 (tree->stree t1))
	 (u2 (tree->stree t2))
	 (x1 (if (tm-is? u1 'with) (cAr u1) u1))
	 (mv (compare-versions x1 u2))
	 (rt (stree->tree mv)))
    ;;(display* "rt= " rt "\n")
    (tree-set (buffer-tree) rt)
    (version-first-difference)))
