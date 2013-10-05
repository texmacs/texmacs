
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-drd.scm
;; DESCRIPTION : generic drd properties
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-drd)
  (:use (utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting environment variables which are parameters for a tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-parameters l)
  #f)

(tm-define (tree-parameter? t)
  (tree-label-parameter? (tree-label t)))

(define (get-with-vars l)
  (cond ((or (null? l) (null? (cdr l))) '())
	((tree-atomic? (car l))
	 (cons (tree->string (car l)) (get-with-vars (cddr l))))
	(else (get-with-vars (cddr l)))))

(define (collect-parameters-sub def v t)
  ;;(display* "Collect sub " def "\n")
  (cond ((tree-atomic? def) (noop))
	((and (tree-in? def '(value quote-value))
	      (tree-atomic? (tree-ref def 0)))
	 (ahash-set! v (tree->string (tree-ref def 0)) #t))
	((tree-is? def 'with)
	 (let* ((v* (make-ahash-table))
		(t* (make-ahash-table))
		(vs (get-with-vars (cDr (tree-children def)))))
	   (for-each (cut collect-parameters-sub <> v* t) (tree-children def))
	   (for (x vs) (ahash-remove! v* x))
	   (for (x (ahash-set->list v*)) (ahash-set! v x #t))))
	((and (tree-is? def 'compound) (tree-atomic? (tree-ref def 0)))
         (let* ((c (tree-children def))
                (l (string->symbol (tree->string (car c))))
                (u (tm->tree (cons l (cdr c)))))
           (collect-parameters-sub u v t)))
	(else
          (when (tree-parameter? def)
            (ahash-set! v (symbol->string (tree-label def)) #t))
	  (collect-parameters (symbol->string (tree-label def)) v t)
	  (for-each (cut collect-parameters-sub <> v t)
		    (tree-children def)))))

(define (collect-parameters l v t)
  (when (not (ahash-ref t l))
    ;;(display* "Collect " l "\n")
    (ahash-set! t l #t)
    (with std (standard-parameters l)
      (if std
	  (for (x std)
	    (ahash-set! v x #t))
	  (with def (get-init-tree l)
	    (cond ((tree-is? def 'uninit) (noop))
		  ((tree-in? def '(macro xmacro))
		   (collect-parameters-sub def v t))
		  (else (ahash-set! v l #t))))))))

(tm-define (search-parameters l)
  (if (symbol? l) (set! l (symbol->string l)))
  (let* ((v (make-ahash-table))
	 (t (make-ahash-table)))
    (collect-parameters l v t)
    (sort (ahash-set->list v) string<=?)))

(tm-define (search-tag-parameters t)
  (search-parameters (tree-label t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard environment variables for primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-parameters l)
  (:require (== l "action"))
  (list "locus-color"))

(tm-define (standard-parameters l)
  (:require (== l "locus"))
  (list "locus-color" "visited-color"))

(tm-define (standard-parameters l)
  (:require (== l "ornament"))
  (list "ornament-shape" "ornament-border"
	"ornament-hpadding" "ornament-vpadding"
	"ornament-color" "ornament-extra-color"
	"ornament-sunny-color" "ornament-shadow-color"))

(tm-define (standard-parameters l)
  (:require (in? l '("reference" "pageref" "label" "tag" "hlink")))
  (list))

(tm-define (search-parameters l)
  (:require (in? (if (string? l) l (symbol->string l))
                 '("reference" "pageref" "hlink")))
  (standard-parameters "locus"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Possible choices for parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-choice-list l) #f)

(tm-define (parameter-choice-list l)
  (:require (== l "ornament-shape"))
  (list "classic" "rounded"))

(tm-define (parameter-show-in-menu? l) #t)

(tm-define (parameter-show-in-menu? l)
  (:require (in? l (list "the-label" "auto-nr" "current-part" "language"
                         "page-nr" "page-the-page" "prog-language")))
  #f)
