
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : htmlout.scm
;; DESCRIPTION : generation of Html from scheme expressions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html htmlout)
  (:use (convert tools output))
  (:export serialize-html))

(define preformatted? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group htmlout-lang-big%
  html head style body p table tr ul ol li dl dt dd blockquote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmlout-big? op)
  (drd-in? op htmlout-lang-big%))

(define (htmlout-indent s plus) (htmlout-indent* s plus #f))
(define (htmlout-indent-close s plus) (htmlout-indent* s plus #t))

(define (htmlout-indent* s plus close?)
  (if (not preformatted?)
      (cond ((htmlout-big? s)
	     (output-indent plus)
	     (output-lf))
	    ((== s 'pre)
	     (if (not close?) (output-lf-verbatim))))))

(define (htmlout-text . ss)
  (if preformatted?
      (apply output-verbatim ss)
      (apply output-text ss)))

(define (htmlout-open s)
  (htmlout-text "<" (symbol->string s) ">")
  (htmlout-indent s 2))

(define (htmlout-tag x)
  (output-text " " (symbol->string (car x)) "=")
  (output-verbatim "\"" (cadr x) "\""))

(define (htmlout-open-tags s l)
  (htmlout-text "<" (symbol->string s))
  (for-each htmlout-tag l)
  (htmlout-text ">")
  (htmlout-indent s 2))

(define (htmlout-close s)
  (htmlout-indent-close s -2)
  (htmlout-text "</" (symbol->string s) ">"))

(define (htmlout-args l big?)
  (if (not (null? l))
      (begin
	(htmlout (car l))
	(if (and big? (not (null? (cdr l)))) (output-lf))
	(htmlout-args (cdr l) big?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmlout x)
  (cond ((string? x) (htmlout-text x))
	((null? x) (noop))
	((or (func? x '!concat) (func? x '*TOP*))
	 (for-each htmlout (cdr x)))
	((and (func? x 'p) (> (length x) 2) (not (func? (cadr x) '@)))
	 (htmlout `(p (!concat ,@(cdr x)))))
	((and (func? x 'p) (> (length x) 3) (func? (cadr x) '@))
	 (htmlout `(p ,(cadr x) (!concat ,@(cddr x)))))
  	((func? x '*PI*)
	 (output-lf-verbatim "<?" (symbol->string (cadr x)) " " (caddr x) "?>")
	 (output-lf))
	((null? (cdr x))
	 (htmlout-open (car x))
	 (htmlout-close (car x)))
	((not (func? (cadr x) '@))
	 (htmlout-open (car x))
	 (htmlout-args (cdr x) (htmlout-big? (car x)))
	 (htmlout-close (car x)))
	(else
	 (htmlout-open-tags (car x) (cdadr x))
	 (update-preformatted
	  (cdadr x)
	  (cut htmlout-args (cddr x) (htmlout-big? (car x))))
	 (htmlout-close (car x)))))

(define (update-preformatted atts thunk)
  (let ((saved-preformatted preformatted?)
	(new-preformatted
	 (cond ((assoc 'xml:space atts) =>
		(lambda (att)
		  (cond ((== (second att) "preserve") #t)
			((== (second att) "default") #f)
			(else preformatted?))))
	       (else preformatted?))))
    (if (== new-preformatted saved-preformatted)
	(thunk)
	(dynamic-wind
	    (lambda () (set! preformatted? new-preformatted))
	    thunk
	    (lambda () (set! preformatted? saved-preformatted))))))

(define (serialize-html x)
  (htmlout x)
  (output-produce))
