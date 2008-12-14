
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-eqns.scm
;; DESCRIPTION : conversion eqnumber <-> nonumber
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-eqns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These routines should be moved to base.scm once
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-contains? t u)
  (cond ((== t u) #t)
	((nlist? t) #f)
	((null? t) #f)
	(else (or (stree-contains? (car t) u) (stree-contains? (cdr t) u)))))

(define (stree-replace t what by)
  (cond ((== t what) by)
	((nlist? t) t)
	((null? t) t)
	(else (cons (stree-replace (car t) what by)
		    (stree-replace (cdr t) what by)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual rewriting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-find-eqlabel t)
  (cond ((func? t 'label) t)
	((nlist? t) #f)
	((null? t) #f)
	(else (let ((lab (tmtm-find-eqlabel (car t))))
		(if lab lab (tmtm-find-eqlabel (cdr t)))))))

(define (tmtm-add-eqnonumber t new)
  (cond ((func? t 'row)
	 (let ((lab (tmtm-find-eqlabel t)))
	   (if (and lab (== new '(eq-number)))
	       (let ((u (stree-replace t lab "")))
		 (rcons (cDr u)
			(tmtm-add-eqnonumber (cAr u) (list 'concat new lab))))
	       (rcons (cDr t) (tmtm-add-eqnonumber (cAr t) new)))))
	((func? t 'cell 1)
	 (list 'cell (tmtm-add-eqnonumber (cadr t) new)))
	(else (list 'concat t new))))

(define (tmtm-eqnumber<->nonumber-sub t old new)
  (cond ((or (func? t 'document) (func? t 'tformat) (func? t 'table))
	 (cons (car t)
	       (map (lambda (x) (tmtm-eqnumber<->nonumber-sub x old new))
		    (cdr t))))
	((func? t 'row)
	 (if (stree-contains? t old)
	     (stree-replace t old "")
	     (tmtm-add-eqnonumber t new)))
	(else t)))

(define (tmtm-eqnumber<->nonumber t old new)
  (cond ((nlist? t) t)
	((null? t) t)
	((and (func? t 'eqnarray* 1) (not (stree-contains? t old))) t)
	((or (func? t 'eqnarray 1) (func? t 'eqnarray* 1))
	 (list 'eqnarray (tmtm-eqnumber<->nonumber-sub (cadr t) old new)))
	(else (cons (car t)
		    (map-in-order (lambda (x)
				 (tmtm-eqnumber<->nonumber x old new))
			       (cdr t))))))

;; ATTENTION: output may not be concat-simplified
(tm-define (tmtm-eqnumber->nonumber t)
  (tmtm-eqnumber<->nonumber t '(eq-number) '(no-number)))

;; ATTENTION: output may not be concat-simplified
(tm-define (tmtm-nonumber->eqnumber t)
  (tmtm-eqnumber<->nonumber t '(no-number) '(eq-number)))
