
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : content.scm
;; DESCRIPTION : important subroutines for manipulating content
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicate for scheme trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (stree? x)
  (or (string? x)
      (and (pair? x) (symbol? (car x))
           (list? (cdr x)) (forall? stree? (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-atomic? x)
  (or (string? x)
      (and (tree? x) (tree-atomic? x))))

(define-public (tm-compound? x)
  (or (pair? x)
      (and (tree? x) (tree-compound? x))))

(define-public (tm-equal? x y)
  (cond ((tree? x)
	 (if (tree? y)
	     (== x y)
	     (tm-equal? (tree-explode x) y)))
	((tree? y) (tm-equal? x (tree-explode y)))
	((and (pair? x) (pair? y))
	 (and (tm-equal? (car x) (car y))
	      (tm-equal? (cdr x) (cdr y))))
	(else (== x y))))

(define-public (tm-length x)
  (cond ((string? x) (string-length x))
	((list? x) (- (length x) 1))
	((tree-atomic? x) (string-length (tree->string x)))
	(else (tree-arity x))))

(define-public (tm-arity x)
  (cond ((list? x) (- (length x) 1))
	((string? x) 0)
	(else (tree-arity x))))

(define-public (tm->string x)
  (if (string? x) x (tree->string x)))

(define-public (tm->list x)
  (if (list? x) x (tree->list x)))

(define-public (tm->stree t)
  (tree->stree (tm->tree t)))

(define-public (tm-label x)
  (if (pair? x) (car x) (tree-label x)))

(define-public (tm-car x)
  (if (pair? x) (car x) (tree-label x)))

(define-public (tm-cdr x)
  (cdr (if (pair? x) x (tree->list x))))

(define-public (tm-children x)
  (if (pair? x) (cdr x) (tree-children x)))

(define-public (tm-range x from to)
  (cond ((string? x) (substring x from to))
	((list? x) (cons (car x) (sublist (cdr x) from to)))
	((tree-atomic? x) (substring (tm->string x) from to))
	(else (cons (tm-car x) (sublist (tm-cdr x) from to)))))

(define-public (tm-func? x . args)
  (or (and (list? x) (apply func? (cons x args)))
      (and (compound-tree? x) (apply func? (cons (tree->list x) args)))))

(define-public (tm-is? x lab)
  (or (and (pair? x) (== (car x) lab))
      (and (compound-tree? x) (== (tree-label x) lab))))

(define-public (tm-in? x l)
  (or (and (pair? x) (in? (car x) l))
      (and (compound-tree? x) (in? (tree-label x) l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching for certain subtrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-find t pred?)
  "Find first subtree which matches predicate."
  (cond ((pred? t) t)
        ((tm-atomic? t) #f)
        (else (list-find (tm-children t) (cut tm-find <> pred?)))))

(define-public (tm-search t pred?)
  "Search list of subtrees which match a predicate."
  (cond ((pred? t) (list t))
        ((tm-atomic? t) (list))
        (else (append-map (cut tm-search <> pred?) (tm-children t)))))

(define-public (tm-find-tag t tag)
  (tm-find t (cut tm-is? <> tag)))

(define-public (tm-search-tag t tag)
  (tm-search t (cut tm-is? <> tag)))

(define (tm-replace-sub t what? by)
  (cond ((what? t) (by t))
        ((tm-atomic? t) t)
        (else `(,(tm-car t)
                ,@(map (cut tm-replace-sub <> what? by) (tm-cdr t))))))

(define-public (tm-replace t what by)
  (cond ((not (procedure? what))
         (tm-replace t (lambda (x) (tm-equal? x what)) by))
        ((not (procedure? by))
         (tm-replace t what (lambda (x) by)))
        (else (tm-replace-sub t what by))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs lengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-length-unit-search s pos)
  (cond ((nstring? s) #f)
	((>= pos (string-length s)) #f)
	((char-alphabetic? (string-ref s pos)) pos)
	((== (string-ref s pos) #\%) pos)
	(else (tm-length-unit-search s (+ pos 1)))))

(define-public (tm-make-length val unit)
  (string-append (number->string val) unit))

(define-public (tm-length? s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length? (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(let ((s1 (substring s 0 pos))
	      (s2 (substring s pos (string-length s))))
	  (and (string-number? s1)
	       (or (string-locase-alpha? s2) (== s2 "%")))))))

(define-public (tm-length-value s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length-value (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(string->number (substring s 0 pos)))))

(define-public (tm-length-unit s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length-unit (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(substring s pos (string-length s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (associate->binding t)
  (and (tm-func? t 'associate 2)
       (cons (cadr t) (caddr t))))

(define-public (collection->assoc col)
  (and (tm-func? col 'collection)
       (with c (tm-children (tm->stree col))
         (list-filter (map associate->binding c) identity))))

(define-public (collection-ref col key)
  (and-with l (collection->assoc col)
    (assoc-ref l key)))

(define-public (collection-set col key val)
  (and-with l (collection->assoc col)
    (set! l (assoc-set! l key val))
    (assoc->collection l)))

(define-public (binding->associate b)
  `(associate ,(car b) ,(cdr b)))

(define-public (assoc->collection l)
  `(collection ,@(map binding->associate l)))

(define-public (collection-append c1 c2)
  (let* ((a1 (collection->assoc c1))
         (a2 (collection->assoc c2))
         (a  (and a1 a2 (assoc-remove-duplicates* (append a1 a2)))))
    (and a1 a2 (assoc->collection a))))

(define-public (collection-delta c1 c2)
  (let* ((a1 (collection->assoc c1))
         (a2 (collection->assoc c2))
         (a  (and a1 a2 (assoc-delta a1 a2))))
    (and a1 a2 (assoc->collection a))))

(define-public (collection-exclude c l)
  (let* ((a (collection->assoc c))
         (b (and a (assoc-exclude a l))))
    (and a (assoc->collection b))))
