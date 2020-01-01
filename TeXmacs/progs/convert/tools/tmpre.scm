
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmpre.scm
;; DESCRIPTION : preprocessing of TeXmacs tree before conversions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmpre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group tmpre-inline-env%
  verbatim code center indent description itemize itemize-minus
  itemize-dot itemize-arrow enumerate enumerate-numeric
  enumerate-roman enumerate-Roman enumerate-alpha enumerate-Alpha
  equation equation* eqnarray eqnarray* leqnarray leqnarray*
  elsequation elsequation*)

(logic-group tmpre-sectional%
  part chapter appendix section subsection subsubsection
  paragraph subparagraph
  part* chapter* appendix* section* subsection* subsubsection*
  paragraph* subparagraph*)

(logic-group tmpre-theorem-env%
  theorem proposition lemma corollary axiom definition
  notation conjecture remark note example
  warning convention acknowledgments exercise
  theorem* proposition* lemma* corollary* axiom* definition*
  notation* conjecture* remark* note* example*
  warning* convention* acknowledgments* exercise*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmpre-non-isolated? l)
  (not (and (list? l)
	    (= (length l) 2)
	    (logic-in? (car l) tmpre-sectional%))))

(define (tmpre-glueable? l)
  (or (func? l 'assign 2)
      (and (list? l)
	   (= (length l) 2)
	   (logic-in? (car l) tmpre-inline-env%)
	   (pair? (cadr l))
	   (in? (caadr l) '(document tformat table)))
      (and (func? l 'mtm 2)
           (tmpre-glueable? (caddr l)))))

(define (tmpre-para x l)
  (cond ((func? (car l) 'para)
	 (cons (cons* 'para x (cdar l)) (cdr l)))
	(else (cons (list 'para x (car l)) (cdr l)))))

(define (tmpre-document l)
  (if (null? l) l
      (let ((h (tmpre (car l)))
	    (r (tmpre-document (cdr l))))
	(cond ((null? (cdr l)) (list h))
	      ((and (tmpre-non-isolated? (car l)) (tmpre-glueable? (cadr l)))
	       (tmpre-para h r))
	      ((and (tmpre-glueable? (car l)) (tmpre-non-isolated? (cadr l)))
	       (tmpre-para h r))
	      (else (cons h r))))))

(define (tmpre-empty? x)
  (cond ((== x "") #t)
	((nlist? x) #f)
	((func? x 'label 1) #t)
	((func? x 'concat) (list-and (map-in-order tmpre-empty? (cdr x))))
	(else #f)))

(define (tmpre-var-document l)
  (if (and (tmpre-empty? (car l)) (tmpre-glueable? (cadr l)))
      (cons (list 'concat (tmpre (car l)) (list 'tmdummy))
	    (tmpre-document (cdr l)))
      (tmpre-document l)))

(define (tmpre l)
  (cond ((nlist? l) l)
	((and (= (length l) 2)
	      (logic-in? (car l) tmpre-theorem-env%)
	      (func? (cadr l) 'document)
	      (>= (length (cadr l)) 3))
	 (list (car l) (cons 'document (tmpre-var-document (cdadr l)))))
	((and (func? l 'document 1)
	      (or (func? (cadr l) 'tformat) (func? (cadr l) 'table)))
	 (tmpre (cadr l)))
	((func? l 'document) (cons 'document (tmpre-document (cdr l))))
	(else (cons (car l) (map-in-order tmpre (cdr l))))))

(tm-define (tmpre-produce l)
  (tmpre l))
