
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
;; Transform formatting newlines into documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-new-line? x) (== x '(new-line)))
(define (tmtm-document? x) (func? x 'document))

(tm-define (tmtm-modernize-newlines l)
  (if (nlist? l) l
      (with r (cons (car l) (map tmtm-modernize-newlines (cdr l)))
	(cond ((func? r 'concat)
	       (with ll (list-scatter (cdr r) tmtm-new-line? #f)
		 (if (< (length ll) 2) r
		     (tmtm-document (map tmtm-concat ll)))))
	      ((func? r 'document) (tmtm-document (cdr r)))
	      ((and (list-find (cdr r) tmtm-document?)
		    (not (list-find (cdr l) tmtm-document?)))
	       (list 'document r))
	      (else r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove spaces before (and possibly after) control markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-control? l)
  (and (list? l) (in? (car l) '(label index))))

(define (tmtm-really-eat? l)
  (or (null? l)
      (and (string? (car l)) (string-starts? (car l) " "))
      (and (tmtm-control? (car l)) (tmtm-really-eat? (cdr l)))))

(define (tmtm-eat-before? l first?)
  ;; eat space before if possible and return #f otherwise
  (cond ((< (length l) 2) #f)
	((not (and (string? (car l)) (string-ends? (car l) " "))) #f)
	((not (tmtm-control? (cadr l))) #f)
	((and (== (car l) " ") first?) (cdr l))
	((tmtm-really-eat? (cdr l))
	 (if (== (car l) " ")
	     (cdr l)
	     (cons (string-drop-right (car l) 1) (cdr l))))
	(else #f)))

(define (tmtm-eat-after? l first?)
  ;; eat space after if possible and return #f otherwise
  (cond ((or (null? l) (== l '(" "))) '())
	((tmtm-control? (car l))
	 (with r (tmtm-eat-after? (cdr l) first?)
	   (if r (cons (car l) r) #f)))
	((and (string? (car l)) (string-starts? (car l) " ") first?)
	 (if (== (car l) " ")
	     (cdr l)
	     (cons (string-drop (car l) 1) (cdr l))))
	(else #f)))

(define (tmtm-eat-around l first?)
  (with r (tmtm-eat-before? l first?)
    (cond (r (tmtm-eat-around r #f))
	  ((null? l) l)
	  ((tmtm-control? (car l))
	   (with r (tmtm-eat-after? (cdr l) first?)
	     (cons (car l) (tmtm-eat-around (if r r (cdr l)) #f))))
	  (else (cons (car l) (tmtm-eat-around (cdr l) #f))))))

(tm-define (tmtm-eat-space-around-control l)
  (if (nlist? l) l
      (with r (map tmtm-eat-space-around-control (cdr l))
	(if (func? l 'concat)
	    (tmtm-concat (tmtm-eat-around r #t))
	    (cons (car l) r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove superfluous newlines (i.e. remove empty paragraphs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-preserve-space? l)
  (and (list? l) (in? (car l) '(verbatim code))))

(tm-define (tmtm-remove-superfluous-newlines l)
  (cond ((tmtm-preserve-space? l) l)
	((func? l 'document)
	 (with r (map tmtm-remove-superfluous-newlines (cdr l))
	   (with f (list-filter r (lambda (x) (!= x "")))
	     (if (null? f) '(document "") (cons 'document f)))))
        ((pair? l)
	 (cons (car l) (map tmtm-remove-superfluous-newlines (cdr l))))
	(else l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glue concats with document items in it to yield a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
