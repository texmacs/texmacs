
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-eqns.scm
;; DESCRIPTION : conversion eqnumber <-> nonumber
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-eqns)
  (:export tmtm-eqnumber->nonumber tmtm-nonumber->eqnumber))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These routines should be moved to base.scm once
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (object-contains? t u)
  (cond ((== t u) #t)
	((not (list? t)) #f)
	((null? t) #f)
	(else (or (object-contains? (car t) u) (object-contains? (cdr t) u)))))

(define (object-replace t what by)
  (cond ((== t what) by)
	((not (list? t)) t)
	((null? t) t)
	(else (cons (object-replace (car t) what by)
		    (object-replace (cdr t) what by)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual rewriting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-find-eqlabel t)
  (cond ((func? t 'label) t)
	((not (list? t)) #f)
	((null? t) #f)
	(else (let ((lab (tmtm-find-eqlabel (car t))))
		(if lab lab (tmtm-find-eqlabel (cdr t)))))))

(define (tmtm-add-eqnonumber t new)
  (cond ((func? t 'row)
	 (let ((lab (tmtm-find-eqlabel t)))
	   (if (and lab (== new '(eqnumber)))
	       (let ((u (object-replace t lab "")))
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
	 (if (object-contains? t old)
	     (object-replace t old "")
	     (tmtm-add-eqnonumber t new)))
	(else t)))

(define (tmtm-eqnumber<->nonumber t old new)
  (cond ((not (list? t)) t)
	((null? t) t)
	((and (func? t 'eqnarray* 1) (not (object-contains? t old))) t)
	((or (func? t 'eqnarray 1) (func? t 'eqnarray* 1))
	 (list 'eqnarray (tmtm-eqnumber<->nonumber-sub (cadr t) old new)))
	(else (cons (car t)
		    (map-in-order (lambda (x)
				 (tmtm-eqnumber<->nonumber x old new))
			       (cdr t))))))

;; ATTENTION: output may not be concat-simplified
(define (tmtm-eqnumber->nonumber t)
  (tmtm-eqnumber<->nonumber t '(eqnumber) '(nonumber)))

;; ATTENTION: output may not be concat-simplified
(define (tmtm-nonumber->eqnumber t)
  (tmtm-eqnumber<->nonumber t
			    (if (tmp-use-apply?)
				'(apply "nonumber")
				'(nonumber))
			    '(eqnumber)))
