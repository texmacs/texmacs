
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : variants.scm
;; DESCRIPTION : circulate between variants of environments
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit variants)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of tag groups (could be done using drds in the future)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define group-table (make-ahash-table))

(define (group-resolve-one x)
  (if (pair? x) (group-resolve (car x)) (list x)))

(tm-define (group-resolve which)
  (with l (ahash-ref group-table which)
    (if l (append-map group-resolve-one l) '())))

(tm-define-macro (define-group group . l)
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

(define-group variant-tag)
(define-group numbered-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (numbered-unnumbered l)
  (append l (map (lambda (x) (symbol-append x '*)) l)))

(tm-define (numbered-unnumbered-complete l)
  (let* ((nl (numbered-tag-list))
	 (bl (list-intersection l nl)))
    (append l (map (lambda (x) (symbol-append x '*)) bl))))

(define (numbered-tag-list*)
  (numbered-unnumbered (numbered-tag-list)))

(tm-define (numbered-context? t)
  (tree-in? t (numbered-tag-list*)))

(tm-define (symbol-toggle-number s)
  (if (symbol-ends? s '*)
      (symbol-drop-right s 1)
      (symbol-append s '*)))

(tm-define (numbered?) #f)
(tm-define (toggle-number) (noop))

(tm-define (numbered?)
  (:context numbered-context?)
  (with-innermost t numbered-context?
    (not (symbol-ends? (tree-label t) '*))))

(tm-define (toggle-number)
  (:context numbered-context?)
  (with-innermost t numbered-context?
    (let* ((old (tree-label t))
	   (new (symbol-toggle-number old)))
      (variant-replace old new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions on structured variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (variant-replace which by)
  (with-innermost t which
    (tree-assign-node t by)))

(define (variants-of-sub lab type nv?)
  (with numbered? (in? lab (numbered-tag-list*))
    (cond ((and numbered? (symbol-ends? lab '*))
	   (with l (variants-of-sub (symbol-drop-right lab 1) type nv?)
	     (if nv? l (map (lambda (x) (symbol-append x '*)) l))))
	  ((and numbered? nv?)
	   (numbered-unnumbered (variants-of-sub lab type #f)))
	  (else (with vg (group-find lab type)
		  (if (not vg) (list lab)
		      (group-resolve vg)))))))

(tm-define (variants-of lab)
  (:synopsis "Retrieve list of variants of @lab")
  (variants-of-sub lab 'variant-tag #f))

(tm-define (similar-to lab)
  (:synopsis "Retrieve list of tags similar to @lab")
  (variants-of-sub lab 'similar-tag #t))

(tm-define (variant-context? t)
  (tree-in? t (numbered-unnumbered-complete (variant-tag-list))))

(tm-define (variant-circulate forward?)
  (noop))

(tm-define (list-search-rotate which search)
  (receive (l r) (list-break which (lambda (x) (== x search)))
    (append r l)))

(tm-define (variant-circulate forward?)
  (:context variant-context?)
  (with-innermost t variant-context?
    (let* ((old (tree-label t))
	   (val (variants-of old))
	   (rot (list-search-rotate val old))
	   (new (if (and forward? (nnull? rot)) (cadr rot) (cAr rot))))
      (variant-replace old new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding-unfolding variants of tags with hidden arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (hidden-variant)
  (noop))
