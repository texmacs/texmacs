
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : variants.scm
;; DESCRIPTION : circulate between variants of environments
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit variants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new structured variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define structured-variants-table (make-ahash-table))
(define structured-variants-list '())

(define (set-structured-variant-sub l)
  (if (nnull? (cddr l))
      (begin
	;(display* (cadr l) " -> " (car l) ", " (caddr l) "\n")
	(ahash-set! structured-variants-table
		    (cadr l) (list (car l) (caddr l)))
	(set! structured-variants-list
	      (cons (cadr l) structured-variants-list))
	(set-structured-variant-sub (cdr l)))))

(define (set-structured-variant l)
  (set-structured-variant-sub (rcons (cons (cAr l) l) (car l))))

(tm-define-macro (set-structured-variants . l)
  (map-in-order set-structured-variant l)
  (display* ""); prevents strange error
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic structured variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-structured-variants
  (chapter appendix section subsection subsubsection
   paragraph subparagraph)
  (chapter* section* subsection* subsubsection*)
  (itemize enumerate)
  (itemize-minus itemize-dot itemize-arrow)
  (enumerate-numeric enumerate-roman enumerate-Roman
   enumerate-alpha enumerate-Alpha)
  (description
   description-compact description-aligned
   description-dash description-long)
  (equation equation*)
  (eqnarray eqnarray*)
  (leqnarray leqnarray*)
  (matrix det)
  (theorem proposition lemma corollary conjecture)
  (definition axiom notation)
  (remark note example convention warning)
  (exercise problem)
  (tabular tabular* block block*)
  (strong em dfn)
  (name person cite*)
  (verbatim kbd code* var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions on structured variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (variant-replace which by)
  (let* ((p (search-upwards which))
	 (t (tm-subtree p)))
    (tm-assign-node p by)))

(tm-define (variant-circulate forward?)
  (let ((which (inside-which structured-variants-list)))
    (if which
	(let* ((val (ahash-ref structured-variants-table which))
	       (new (if forward? (cadr val) (car val))))
	  (variant-replace which new)))))
