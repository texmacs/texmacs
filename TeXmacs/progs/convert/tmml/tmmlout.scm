
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmlout.scm
;; DESCRIPTION : generation of Xml from scheme expressions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tmml tmmlout)
  (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining output layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmlout-big? doc)
  (cond ((npair? doc) #f)
	((func? doc '!document) #t)
	((func? doc 'tformat) #t)
	((func? doc 'table) #t)
	((func? doc 'collection) #t)
	((func? doc 'associate) #t)
	((func? doc 'tm-par) #t)
	(else (list-or (map tmmlout-big? (cdr doc)))))
  ;#t
  )

(define (tmmlout-preserve-one? x first? last?)
  (cond ((func? x '!concat) (tmmlout-preserve? (cdr x) first? last?))
	((nstring? x) #f)
	((and first? (string-starts? x " ")) #t)
	((and last? (string-ends? x " ")) #t)
	(else (>= (string-search-forwards "  " 0 x) 0))))

(define (tmmlout-preserve? l first? last?)
  (if (null? l) #f
      (or (tmmlout-preserve-one? (car l) first? (and last? (null? (cdr l))))
	  (tmmlout-preserve? (cdr l) #f last?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmlout-indent plus big? preserve?)
  (cond (preserve? (noop))
	(big? (output-indent plus) (output-lf))
	(else (noop))))

(define (tmmlout-attr x)
  ;(display-err* "[tmmlout-attr] " x "\n")
  (output-text " " (symbol->string (car x)) "=")
  (output-verbatim "\"" (string-replace (cadr x) "\"" "\\\"") "\""))

(define (tmmlout-stacked-args l)
  (if (nnull? l)
      (begin
	(tmmlout (car l))
	(if (nnull? (cdr l))
	    (begin
	      (output-lf)
	      (output-lf)))
	(tmmlout-stacked-args (cdr l)))))

(define (tmmlout-args l big? preserve?)
  ;(display-err* "[tmmlout-args] " l ", " big? ", " preserve? "\n")
  (cond ((null? l) (noop))
	((string? (car l))
	 (if preserve?
	     (output-verbatim (car l))
	     (output-text (car l)))
	 (tmmlout-args (cdr l) big? preserve?))
	((func? (car l) '!concat)
	 (tmmlout-args (cdar l) #f preserve?)
	 (tmmlout-args (cdr l) big? preserve?))
	((func? (car l) '!document)
	 (tmmlout-args (cdar l) big? preserve?)
	 (tmmlout-args (cdr l) big? preserve?))
	((func? (car l) '!stacked)
	 (tmmlout-stacked-args (cdar l))
	 (tmmlout-args (cdr l) big? preserve?))
	(else
	 (tmmlout (car l))
	 (if (and big?
		  (pair? (cdr l))
		  (nstring? (car l))
		  (nstring? (cadr l)))
	     (begin
	       (output-lf)
	       (if (func? (cadr l) 'tm-par) (output-lf))))
	 (tmmlout-args (cdr l) big? preserve?))))

(define (tmmlout-remove-duplicates l)
  (if (null? l) l
      (with r (tmmlout-remove-duplicates (cdr l))
	(if (in? (caar l) (map car r)) r (cons (car l) r)))))

(define (tmmlout-tag tag attrs args)
  ;(display-err* "[tmmlout-tag] " tag ", " attrs ", " args "\n")
  (let* ((big? (tmmlout-big? (cons tag args)))
	 (preserve? (tmmlout-preserve? args #t #t)))
    (if preserve? (set! attrs `((xml:space "preserve") ,@attrs)))
    (output-text "<")
    (output-text (symbol->string tag))
    (set! attrs (tmmlout-remove-duplicates attrs))
    (for-each tmmlout-attr attrs)
    (if (null? args) (output-text "/"))
    (output-text ">")
    (if (nnull? args)
	(begin
	  (tmmlout-indent 2 big? preserve?)
	  (tmmlout-args args big? preserve?)
	  (tmmlout-indent -2 big? preserve?)
	  (output-text "</" (symbol->string tag) ">")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmlout x)
  ;(display-err* "[tmmlout] " x "\n")
  (cond ((string? x) (output-text x))
	((null? x) (noop))
	((func? x '*PI*)
	 (output-text "<?" (symbol->string (cadr x)) " " (caddr x) "?>"))
	((and (pair? (cdr x)) (func? (cadr x) '@))
	 (tmmlout-tag (car x) (cdadr x) (cddr x)))
	((func? x '!concat) (tmmlout-args (cdr x) #f #t))
	((func? x '!document) (tmmlout-args (cdr x) #t #f))
	((func? x '!stacked) (tmmlout-stacked-args (cdr x)))
	((func? x '*TOP*) (tmmlout-stacked-args (cdr x)))
	(else (tmmlout-tag (car x) '() (cdr x)))))

(tm-define (serialize-tmml x)
  (tmmlout x)
  (output-produce))
