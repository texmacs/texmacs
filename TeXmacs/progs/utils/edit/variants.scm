
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : variants.scm
;; DESCRIPTION : circulate between variants of environments
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit variants)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of tag groups (could be done using drds in the future)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define group-table (make-ahash-table))
(tm-define group-resolve-table (make-ahash-table))

(define (group-resolve-one x)
  (if (pair? x) (group-resolve (car x)) (list x)))

(tm-define (group-resolve which)
  (if (not (ahash-ref group-resolve-table which))
      (with l (ahash-ref group-table which)
	(ahash-set! group-resolve-table which
		    (if l (append-map group-resolve-one l) '()))))
  (ahash-ref group-resolve-table which))

(tm-define-macro (define-group group . l)
  (set! group-resolve-table (make-ahash-table))
  (with old (ahash-ref group-table group)
    (if old
	`(ahash-set! group-table ',group (append ',old ',l))
	`(begin
	   (ahash-set! group-table ',group ',l)
	   (tm-define (,(symbol-append group '-list))
	     (group-resolve ',group))
	   (tm-define (,(symbol-append group '?) lab)
	     (in? lab (group-resolve ',group)))
	   (tm-define (,(symbol-append 'inside- group '?))
	     (not (not (inside-which (group-resolve ',group)))))))))

(tm-define (group-find which group)
  (:synopsis "Find subgroup of @group which contains @which")
  (with l (ahash-ref group-table group)
    (cond ((not l) #f)
	  ((in? which l) group)
	  (else (with f (map car (list-filter l (lambda (x) (pair? x))))
		  (list-any (lambda (x) (group-find which x)) f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group numbered-tag)

(tm-define (symbol-numbered? s)
  (in? s (numbered-tag-list)))

(tm-define (symbol-unnumbered? s)
  (and (symbol-ends? s '*)
       (in? (symbol-drop-right s 1) (numbered-tag-list))))

(tm-define (symbol-toggle-number s)
  (if (symbol-ends? s '*)
      (symbol-drop-right s 1)
      (symbol-append s '*)))

(tm-define (numbered-tag-list*)
  (map (lambda (x) (symbol-append x '*)) (numbered-tag-list)))

(tm-define (numbered-unnumbered-append l)
  (append l (map (lambda (x) (symbol-append x '*)) l)))

(tm-define (numbered-unnumbered-complete l)
  (let* ((nl (numbered-tag-list))
	 (bl (list-intersection l nl)))
    (append l (map (lambda (x) (symbol-append x '*)) bl))))

(tm-define (numbered-standard-context? t)
  (or (tree-in? t (numbered-tag-list))
      (tree-in? t (numbered-tag-list*))))

(tm-define (numbered-context? t)
  #f)

(tm-define (numbered-context? t)
  (:require (numbered-standard-context? t))
  #t)

(tm-define (numbered-numbered? t)
  #f)

(tm-define (numbered-numbered? t)
  (:require (numbered-standard-context? t))
  (not (symbol-ends? (tree-label t) '*)))

(tm-define (numbered-unnumbered? t)
  (and (numbered-context? t) (not (numbered-numbered? t))))

(tm-define (numbered-toggle t)
  (and-with p (tree-outer t)
    (numbered-toggle p)))

(tm-define (numbered-toggle t)
  (:require (numbered-standard-context? t))
  (let* ((old (tree-label t))
         (new (symbol-toggle-number old)))
    (variant-set t new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alternate between two possibilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group alternate-tag (alternate-first-tag) (alternate-second-tag))
(define-group alternate-first-tag)
(define-group alternate-second-tag)

(tm-define (alternate-standard-context? t)
  (tree-in? t (alternate-tag-list)))
(tm-define (alternate-standard-first? t)
  (tree-in? t (alternate-first-tag-list)))
(tm-define (alternate-standard-second? t)
  (tree-in? t (alternate-second-tag-list)))

(tm-define (alternate-context? t) #f)
(tm-define (alternate-context? t)
  (:require (alternate-standard-context? t))
  #t)

(tm-define (alternate-first? t) #f)
(tm-define (alternate-first? t)
  (:require (alternate-standard-first? t))
  #t)

(tm-define (alternate-second? t) #f)
(tm-define (alternate-second? t)
  (:require (alternate-standard-second? t))
  #t)

(tm-define (pure-alternate-context? t)
  (alternate-context? t))

(tm-define alternate-table (make-ahash-table))

(tm-define-macro (define-alternate first second)
  `(begin
     (define-group alternate-first-tag ,first)
     (define-group alternate-second-tag ,second)
     (ahash-set! alternate-table ',first ',second)
     (ahash-set! alternate-table ',second ',first)))

(tm-define (alternate-reverse? t)
  #f)

(tm-define (alternate-first-name t)
  (alternate-second-name t))

(tm-define (alternate-first-icon t)
  "tm_alternate_first.xpm")

(tm-define (alternate-second-name t)
  "Expand")

(tm-define (alternate-second-icon t)
  "tm_alternate_second.xpm")

(tm-define (alternate-toggle t)
  (and-with p (tree-outer t)
    (alternate-toggle p)))

(tm-define (symbol-toggle-alternate l)
  (ahash-ref alternate-table l))

(tm-define (alternate-toggle t)
  (:require (alternate-standard-context? t))
  (variant-set t (symbol-toggle-alternate (tree-label t))))

(tm-define (alternate-fold t)
  (and-with p (tree-outer t)
    (alternate-fold p)))

(tm-define (alternate-fold t)
  (:require (alternate-standard-second? t))
  (alternate-toggle t))

(tm-define (alternate-unfold t)
  (and-with p (tree-outer t)
    (alternate-unfold p)))

(tm-define (alternate-unfold t)
  (:require (alternate-standard-first? t))
  (alternate-toggle t))

(tm-define (fold)
  (:type (-> void))
  (:synopsis "Fold at the current focus position")
  (alternate-fold (focus-tree)))

(tm-define (unfold)
  (:type (-> void))
  (:synopsis "Unold at the current focus position")
  (alternate-unfold (focus-tree)))

(tm-define (mouse-fold t)
  (:type (-> void))
  (:synopsis "Fold using the mouse")
  (:secure #t)
  (when (tree->path t)
    (tree-go-to t :start)
    (when (tree-up t)
      (alternate-fold (tree-up t)))))

(tm-define (mouse-unfold t)
  (:type (-> void))
  (:synopsis "Unfold using the mouse")
  (:secure #t)
  (when (tree->path t)
    (tree-go-to t :start)
    (when (tree-up t)
      (alternate-unfold (tree-up t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag)

(tm-define (focus-tree-modified t)
  (noop))

(tm-define (variant-set t by)
  (with-focus-after t
    (with i (tree-down-index t)
      (tree-assign-node! t by)
      (focus-tree-modified t)
      (when (and i (not (tree-accessible-child? t i)))
        (with ac (tree-accessible-children t)
          (when (nnull? ac)
            (tree-go-to (car ac) :start)))))))

(tm-define (variant-set-keep-numbering t v)
  (if (and (symbol-numbered? v) (symbol-unnumbered? (tree-label t)))
      (variant-set t (symbol-append v '*))
      (variant-set t v)))

(define (variants-of-sub lab type nv?)
  (with numbered? (or (in? lab (numbered-tag-list))
		      (in? lab (numbered-tag-list*)))
    (cond ((and numbered? (symbol-ends? lab '*))
	   (with l (variants-of-sub (symbol-drop-right lab 1) type nv?)
	     (if nv? l (map (lambda (x) (symbol-append x '*)) l))))
	  ((and numbered? nv?)
	   (numbered-unnumbered-append (variants-of-sub lab type #f)))
	  (else (with vg (group-find lab type)
		  (if (not vg) (list lab)
		      (group-resolve vg)))))))

(tm-define (variants-of lab)
  (:synopsis "Retrieve list of variants of @lab")
  (variants-of-sub lab 'variant-tag #f))

(tm-define (similar-to lab)
  (:synopsis "Retrieve list of tags similar to @lab")
  (variants-of-sub lab 'similar-tag #t))

(tm-define (variant-standard-context? t)
  (tree-in? t (numbered-unnumbered-complete (variant-tag-list))))

(tm-define (variant-context? t)
  #f)

(tm-define (variant-context? t)
  (:require (variant-standard-context? t))
  #t)

(tm-define (variant-circulate t forward?)
  (and-with p (tree-outer t)
    (variant-circulate p forward?)))

(tm-define (list-search-rotate which search)
  (receive (l r) (list-break which (lambda (x) (== x search)))
    (append r l)))

(tm-define (variant-circulate-in t l forward?)
  (let* ((old (tree-label t))
         (rot (list-search-rotate l old))
         (new (if (and forward? (nnull? rot)) (cadr rot) (cAr rot))))
    (variant-set t new)))

(tm-define (variant-circulate t forward?)
  (:require (variant-standard-context? t))
  (variant-circulate-in t (variants-of (tree-label t)) forward?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding-unfolding variants of tags with hidden arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (hidden-context? t)
  (tree-in? t (hidden-tag-list)))

(tm-define (tree-show-hidden t)
  (noop))

(tm-define (tree-show-hidden t)
  (:require (hidden-context? t))
  (tree-assign-node! t 'shown))

(tm-define (cursor-show-hidden)
  (with t (buffer-tree)
    (while (and t (!= t (cursor-tree)))
      (tree-show-hidden t)
      (set! t (tree-ref t :down)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag
  (argument-tag) (value-tag) (quote-tag)
  (binary-operation-tag) (binary-compare-tag))

(define-group similar-tag
  (label-tag) (reference-tag) (citation-tag))

(define-group hidden-tag
  hidden hidden*)

(define-group argument-tag
  arg quote-arg)

(define-group value-tag
  value quote-value)

(define-group quote-tag
  quote quasi quasiquote)

(define-group binary-operation-tag
  plus minus times over minimum maximum or and)

(define-group binary-compare-tag
  equal unequal less lesseq greater greatereq)

(define-group label-tag
  label)

(define-group reference-tag
  reference pageref eqref)

(define-group citation-tag
  cite nocite cite-TeXmacs cite-detail)

(define-group mini-flow-tag
  table graphics ornament ornamented tree)
