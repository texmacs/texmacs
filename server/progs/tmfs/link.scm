
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link.scm
;; DESCRIPTION : links between loci
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tmfs link))
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools list)
	     (server request) (tmfs locus))

(define link-serial 0)
(define link-table (make-ahash-table))
(define link-by-type-table (make-ahash-table))

(request-handler (link-new . l)
  (set! link-serial (+ link-serial 1))
  (ahash-set! link-table link-serial l)
  (ahash-set! link-by-type-table (car l)
	      (cons (cdr l) (ahash-ref* link-by-type-table (car l) '())))
  link-serial)

(define (link-match lk pat t)
  (cond ((== lk pat) t)
	((or (null? lk) (null? pat)) #f)
	((keyword? (car pat))
	 (with val (assoc-ref t (car pat))
	   (if val (link-match lk (cons val (cdr pat)) t)
	       (link-match (cdr lk) (cdr pat) (cons (cons (car pat) val) t)))))
	((string? (car pat))
	 (with locus (locus-inverse-ref (car pat))
	   (and locus (link-match lk (cons locus (cdr pat)) t))))
	((== (car lk) (car pat))
	 (link-match (cdr lk) (cdr pat) t))
	(else #f)))

(define (link-query-one q t)
  (if (npair? q) '()
      (with l (ahash-ref link-by-type-table (car q))
	(if (not l) '()
	    (list-filter (map (lambda (x) (link-match x (cdr q) t)) l)
			 (lambda (x) x))))))

(define (link-query-sub qs lt)
  (cond ((null? qs) lt)
	((null? lt) lt)
	((nnull? (cdr lt))
	 (append (link-query-sub qs (list (car lt)))
		 (link-query-sub qs (cdr lt))))
	(else (with rt (link-query-one (car qs) (car lt))
		(link-query-sub (cdr qs) rt)))))

(request-handler (link-query . qs)
  (link-query-sub qs (list '())))
