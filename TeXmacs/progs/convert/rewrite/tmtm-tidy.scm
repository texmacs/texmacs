
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-tidy.scm
;; DESCRIPTION : commodity routines for improving a document
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-tidy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some useful subroutines
;; FIXME: concat- and document- correction should go elsewhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-concat l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(define (tmtm-document-sub l)
  (cond ((null? l) l)
	((func? (car l) 'document)
	 (append (tmtm-document-sub (cdar l))
		 (tmtm-document-sub (cdr l))))
	(else (cons (car l) (tmtm-document-sub (cdr l))))))

(define (tmtm-document l)
  (with r (tmtm-document-sub l)
    (if (null? r) "" (cons 'document r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glue concats with document items in it to yield a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-document? x) (func? x 'document))

(define (tmtm-concat-document-sub l)
  (if (null? (cdr l))
      (cdar l)
      (append (cdar l) (list (tmtm-concat (cdr l))))))

(tm-define (tmtm-concat-document-correct l)
  ;; FIXME: might go into tmtm-concat constructor
  (if (nlist? l) l
      (with r (cons (car l) (map tmtm-concat-document-correct (cdr l)))
	(if (and (func? r 'concat) (list-find (cdr r) tmtm-document?))
	    (let* ((ll (list-scatter (cdr r) tmtm-document? #t))
		   (head (tmtm-concat (car ll)))
		   (aux (map tmtm-concat-document-sub (cdr ll)))
		   (tail (apply append aux)))
	      (if (== head "")
		  (tmtm-document tail)
		  (tmtm-document (cons head tail))))
	    r))))
