
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtmml.scm
;; DESCRIPTION : conversion of TeXmacs trees into Xml trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tmml tmtmml)
  (:use (convert tmml tmmlout)))

(define (tmtmml-file x)
  (define (tmtmml-keep? x)
    (not (func? x 'TeXmacs)))
  `(*TOP*
    (*PI* xml "version=\"1.0\"")
    (TeXmacs
     (@ (version ,(texmacs-version)))
     (!stacked ,@(map tmtmml (list-filter (cdr x) tmtmml-keep?))))))

(define (tmtmml-document l)
  (cons '!document (map (lambda (x) (list 'tm-par (tmtmml x))) l)))

(define (tmtmml-concat l)
  (cons '!concat (map tmtmml l)))

(define (tmtmml-with l)
  (tmtmml-regular 'with (list (cons 'attr (cDr l)) (cAr l))))

(define (tmtmml-args l)
  (cond ((null? l) l)
	((and (null? (cdr l)) (!= (car l) "")) (list (tmtmml (car l))))
	(else (map (lambda (x) (list 'tm-arg (tmtmml x))) l))))

(define (tmtmml-apply dyn tag l)
  (cond ((and (pair? l) (func? (car l) 'attr))
	 (tmtmml (cons* tag (cons* 'attr "tm-dyn" dyn (cdar l)) (cdr l))))
	(else (tmtmml (cons* tag (list 'attr "tm-dyn" dyn) l)))))

(define (tmtmml-group-attrs attrs)
  (if (< (length attrs) 2) '()
      (cons (list (string->symbol (tm->xml-name (car attrs))) (cadr attrs))
	    (tmtmml-group-attrs (cddr attrs)))))

(define (tmtmml-attrs l)
  (if (list-and (map string? l))
      (cons '@ (tmtmml-group-attrs l))
      (cons 'tm-attr (map (lambda (x) (list 'tm-arg (tmtmml x))) l))))

(define (tmtmml-regular tag* l)
  (with tag (string->symbol (tm->xml-name (symbol->string tag*)))
    (cond ((and (pair? l) (func? (car l) 'attr))
	   (cons* tag (tmtmml-attrs (cdar l)) (tmtmml-args (cdr l))))
	  (else (cons tag (tmtmml-args l))))))

(tm-define (tmtmml x)
  (cond ((string? x) (tm->xml-cdata x))
	((func? x '!file) (tmtmml-file (cadr x)))
	((func? x 'document) (tmtmml-document (cdr x)))
	((func? x 'concat) (tmtmml-concat (cdr x)))
	((func? x 'with) (tmtmml-with (cdr x)))
	((and (func? x 'compound) (string? (cadr x)))
	 (tmtmml-apply (symbol->string (car x))
		       (string->symbol (cadr x))
		       (cddr x)))
	(else (tmtmml-regular (car x) (cdr x)))))

(define (tmtmml-simplify x)
  (cond ((npair? x) x)
	((and (func? x 'quote) (string? (cadr x))) (cadr x))
	(else (map tmtmml-simplify x))))

(define (tmtmml-consistency-check-1 x)
  (with y (tmmltm (tmtmml x))
    (if (== x y)
	(display "ok\n")
	(write-diff x y))))

(define (tmtmml-consistency-check nr orig new)
  (display* "Consistency check " nr ": ")
  (if (== orig new)
      (display "ok\n")
      (begin
	(display "failed\n")
	(write-diff orig new))))

(tm-define (texmacs->tmml x)
  (if (tmfile? x)
      (texmacs->tmml (list '!file x))
      (with simplified (tree->stree (tree-simplify (stree->tree x)))
	(with xml-tree (tmtmml simplified)
          ;(tmtmml-consistency-check 1 simplified (tmmltm xml-tree))
          ;(tmtmml-consistency-check 2 simplified (tmmltm (parse-tmml (serialize-tmml xml-tree))))
	  xml-tree))))
