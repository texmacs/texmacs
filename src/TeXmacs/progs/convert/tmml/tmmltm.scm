
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmltm.scm
;; DESCRIPTION : conversion of Xml trees to TeXmacs trees
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tmml tmmltm)
  (:export parse-tmml tmml->texmacs tmmltm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the inverse functionality of tmmlout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xmlin-make tag attrs args impl)
  (if (not (null? attrs)) (set! attrs (list (cons '@ attrs))))
  (if (and (not (null? args)) (func? (car args) 'tm-attr))
      (begin
	(set! attrs (append attrs (list (car args))))
	(set! args (cdr args))))
  (if (and impl (not (and (== impl '!concat) (<= (length args) 1))))
      (set! args (list (cons impl args))))
  `(,tag ,@attrs ,@args))

(define (xmlin-special tag attrs l)
  (with args (map xmlin (list-filter l (lambda (x) (not (string? x)))))
    (with doc? (list-or (map (lambda (x) (func? x 'tm-par)) args))
      (xmlin-make tag attrs args (if doc? '!document #f)))))

(define (xmlin-unspace x first? last? preserve?)
  (cond ((and (string? x) preserve?) x)
	((string? x) (xml-unspace x first? last?))
	(else (xmlin x))))

(define (xmlin-unspace-args l first? preserve?)
  (cond ((null? l) l)
	((null? (cdr l)) (list (xmlin-unspace (car l) first? #t preserve?)))
	(else (cons (xmlin-unspace (car l) first? #f preserve?)
		    (xmlin-unspace-args (cdr l) #f preserve?)))))

(define (xmlin-regular tag attrs* args*)
  (let* ((search '(xml:space "preserve"))
	 (preserve? (in? search attrs*))
	 (attrs (list-filter attrs* (lambda (x) (not (== x search)))))
	 (args (xmlin-unspace-args args* #t preserve?)))
    (set! args (list-filter args (lambda (x) (not (== x "")))))
    (if (and (null? args) (in? tag '(tm-arg tm-par))) (set! args '("")))
    (xmlin-make tag attrs args '!concat)))

(define (xmlin x)
  ;(display* "[xmlin] " x "\n")
  (if (not (pair? x)) x
      (let* ((tag (car x))
	     (attrs? (and (pair? (cdr x)) (func? (cadr x) '@)))
	     (attrs (if attrs? (cdadr x) '()))
	     (args (if attrs? (cddr x) (cdr x))))
	(cond ((== tag '*TOP*)
	       (with r (xmlin-special tag attrs args)
		 (if (match? r '(*TOP* (*PI* xml :*) (TeXmacs (@ :*) :*)))
		     `(*TOP*
		       ,(cadr r)
		       (TeXmacs ,(cadr (caddr r))
				(!stacked ,@(cddr (caddr r)))))
		     (xmlin-regular tag attrs args))))
	      ((== tag '*PI*) x)
	      ((== tag 'TeXmacs)
	       (xmlin-special tag attrs args))
	      ((list-or (map (lambda (x) (func? x 'tm-arg)) args))
	       (xmlin-special tag attrs args))
	      ((list-or (map (lambda (x) (func? x 'tm-par)) args))
	       (xmlin-special tag attrs args))
	      (else (xmlin-regular tag attrs args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the inverse functionality of tmxml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmltm-file version args)
  `(!file (document (TeXmacs ,version) ,@(map tmmltm args))))

(define (tmmltm-document l)
  (cons 'document (map (lambda (x) (tmmltm (cadr x))) l)))

(define (tmmltm-concat l)
  (cons 'concat (map tmmltm l)))

(define (tmmltm-with x)
  (with (tag attr arg) (tmmltm-regular (car x) (cdr x))
    `(with ,@(cdr attr) ,arg)))

(define (tmmltm-ungroup-attrs attrs)
  (if (null? attrs) attrs
      (cons* (xml-name->tm (symbol->string (caar attrs)))
	     (cadar attrs)
	     (tmmltm-ungroup-attrs (cdr attrs)))))

(define (tmmltm-attributed tag attrs args)
  (cond ((null? attrs) (cons tag args))
	((== (car attrs) "tm-dyn")
	 (tmmltm-attributed (string->symbol (xml-name->tm (cadr attrs)))
			   (cddr attrs)
			   (cons (symbol->string tag) args)))
	(else `(,tag (attr ,@attrs) ,@args))))

(define (tmmltm-arg x)
  ;(display* "x= " x "\n")
  ;(display* "test? " (func? x 'tm-arg 1) "\n")
  (if (func? x 'tm-arg 1)
      (with y (cadr x)
	;(display* "y= " y "\n")
	(with r (tmmltm y)
	  ;(display* "r= " r "\n")
	  r))
      (tmmltm x)))

(define (tmmltm-args l)
  (map tmmltm-arg l))

(define (tmmltm-regular tag* args)
  (with tag (string->symbol (xml-name->tm (symbol->string tag*)))
    (cond ((and (pair? args) (func? (car args) '@))
	   (tmmltm-attributed tag
			     (tmmltm-ungroup-attrs (cdar args))
			     (tmmltm-args (cdr args))))
	  ((and (pair? args) (func? (car args) 'tm-attr))
	   (tmmltm-attributed tag
			     (tmmltm-args (cdar args))
			     (tmmltm-args (cdr args))))
	  (else (cons tag (tmmltm-args args))))))

(define (tmmltm x)
  ;(display* "[tmmltm] ") (write x) (display* "\n")
  (cond ((string? x) (xml-cdata->tm x))
	((and (func? x '*TOP*) (>= (length x) 3) (func? (caddr x) 'TeXmacs 2))
	 (tmmltm (caddr x)))
	((func? x '*TOP*) (tmmltm-concat (cdr x)))
	((func? x '*PI*) x)
	((func? x 'TeXmacs 2) (tmmltm-file (cadadr (cadr x)) (cdr (caddr x))))
	((func? x '!document) (tmmltm-document (cdr x)))
	((func? x '!concat) (tmmltm-concat (cdr x)))
	((func? x 'with) (tmmltm-with x))
	(else (tmmltm-regular (car x) (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parse-tmml s)
  (:type (string -> object))
  (:synopsis "Parse a TeXmacs XML document @s.")
  (with raw-xml (parse-xml s)
    ;(display* "raw= " raw-xml "\n")
    (xmlin raw-xml)))

(tm-define (tmml->texmacs tmml)
  (:type (object -> object))
  (:synopsis "Convert an TeXmacs XML object @s into TeXmacs.")
  (with doc (tmmltm tmml)
    (if (func? doc '!file 1)
	(tree->object (upgrade-tmml (cadr doc)))
	doc)))
