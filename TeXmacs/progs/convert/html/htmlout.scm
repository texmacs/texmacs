
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
  (:use (convert tools output)))

(define preformatted? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group htmlout-big%
  ;; Both the tag and the children are displayed in multi-line format.
  html head style body table tr ul ol dl)

(drd-group htmlout-big-tag%
  ;; The tag is displayed in multi-line format.
  p li dt dd center blockquote)

(drd-rule (htmlout-big-tag% 'x) (htmlout-big% 'x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmlout-big? op)
  (drd-in? op htmlout-big%))

(define (htmlout-big-tag? op)
  (drd-in? op htmlout-big-tag%))

(define (htmlout-indent* s plus close?)
  (if (not preformatted?)
      (cond ((htmlout-big-tag? s)
	     (output-indent plus)
	     (output-lf))
	    ((== s 'pre)
	     (if (not close?) (output-lf-verbatim))))))

(define (htmlout-indent s plus) (htmlout-indent* s plus #f))
(define (htmlout-indent-close s plus) (htmlout-indent* s plus #t))

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
  (if (nnull? l)
      (begin
	(htmlout (car l))
	(if (and big? (nnull? (cdr l))) (output-lf))
	(htmlout-args (cdr l) big?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmlout-doctype l)
  (define (helper x)
    (if (string? x) (string-append "\"" x "\"") (symbol->string x)))
  (let* ((l1 (map (lambda (x) (list " " (helper x))) l))
	 (l2 (apply append l1))
	 (l3 (append '("<!DOCTYPE") l2 '(">"))))
    (apply output-lf-verbatim l3)
    (output-lf)))

(define (htmlout x)
  (cond ((string? x) (htmlout-text x))
	((null? x) (noop))
	((or (func? x '!concat) (func? x '*TOP*))
	 (for-each htmlout (cdr x)))
	((and (func? x 'p 1) (pair? (cadr x)) (htmlout-big-tag? (caadr x)))
	 (htmlout (cadr x)))
  	((func? x '*PI*)
	 (output-lf-verbatim "<?" (symbol->string (cadr x)) " " (caddr x) "?>")
	 (output-lf))
	((func? x '*DOCTYPE*)
	 (htmlout-doctype (cdr x)))
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

(tm-define (serialize-html x)
  (htmlout x)
  (output-produce))
