
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-search.scm
;; DESCRIPTION : searching properties of macros
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-search)
  (:use (utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for subsequent customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-options l) #f)

(tm-define (standard-parameters l) #f)

(tm-define (tree-parameter? t)
  (tree-label-parameter? (tree-label t)))

(tm-define (parameter-choice-list l) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting style options for a tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-options-sub def t)
  ;;(display* "Collect sub " def "\n")
  (cond ((tree-atomic? def) (list))
	((and (tree-is? def 'compound) (tree-atomic? (tree-ref def 0)))
         (let* ((c (tree-children def))
                (l (string->symbol (tree->string (car c))))
                (u (tm->tree (cons l (cdr c)))))
           (collect-options-sub u t)))
	(else
          (let* ((head (collect-options (symbol->string (tree-label def)) t))
                 (tail (map (cut collect-options-sub <> t)
                            (tree-children def))))
            (list-remove-duplicates (apply append (cons head tail)))))))

(define (append-options l1 l2)
  (cond ((not l1) l2)
	((and (pair? l1) (== (car l1) :recurse)) (append (cdr l1) l2))
	(else l1)))

(define (collect-options l t)
  (when (not (ahash-ref t l))
    ;;(display* "Collect " l "\n")
    (ahash-set! t l '())
    (ahash-set! t l
      (with std (standard-options (string->symbol l))
	(append-options std
			(with def (get-init-tree l)
			  (if (tree-in? def '(macro xmacro))
			      (collect-options-sub def t)
			      (list)))))))
  (ahash-ref t l))

(tm-define (search-options l)
  (if (symbol? l) (set! l (symbol->string l)))
  (with t (make-ahash-table)
    (collect-options l t)
    (ahash-ref t l)))

(tm-define (search-tag-options t)
  (search-options (tree-label t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting environment variables which are parameters for a tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (begin
            ;;(display* "Std= " std "\n")
            (for (x std)
              (ahash-set! v x #t)))
	  (with def (get-init-tree l)
            ;;(display* "  Def= " def "\n")
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
