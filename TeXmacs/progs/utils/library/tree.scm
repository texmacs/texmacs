
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tree.scm
;; DESCRIPTION : routines for trees and for modifying documents
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In place versions of fundamental modification routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (tree-assign! ref t)
  `(begin
     (set! ,ref (tree-assign ,ref ,t))
     ,ref))

(tm-define (tree-insert t pos x)
  (cond ((string? x) (tree-var-insert t pos x))
	((list? x) (tree-var-insert t pos (cons 'tuple x)))
	(else (texmacs-error "tree-insert" "~S is not a string or a list" x))))

(tm-define tree-insert! tree-insert)
(tm-define tree-remove! tree-remove)
(tm-define tree-split! tree-split)
(tm-define tree-join! tree-join)
(tm-define tree-assign-node! tree-assign-node)

(tm-define-macro (tree-insert-node! ref pos t)
  `(begin
     (set! ,ref (tree-insert-node ,ref ,pos ,t))
     ,ref))

(tm-define-macro (tree-remove-node! ref pos)
  `(begin
     (set! ,ref (tree-remove-node ,ref ,pos))
     ,ref))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use fundamental modification routines in an intelligent way
;; via a unique assignment routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-common-left t1 t2)
  (cond ((and (tm-compound? t1) (tm-compound? t2))
	 (list-common-left (cdr (tm->list t1)) (cdr (tm->list t2))))
	((and (tm-atomic? t1) (tm-atomic? t2))
	 (list-common-left (string->list (tm->string t1))
			   (string->list (tm->string t2))))
	(else 0)))

(define (tree-common-right t1 t2)
  (cond ((and (tm-compound? t1) (tm-compound? t2))
	 (list-common-right (cdr (tm->list t1)) (cdr (tm->list t2))))
	((and (tm-atomic? t1) (tm-atomic? t2))
	 (list-common-right (string->list (tm->string t1))
			    (string->list (tm->string t2))))
	(else 0)))

(define (tree-focus-index ref l)
  (cond ((null? l) #f)
	((tree-inside? (car l) ref) 0)
	((and (list? (car l)) (tree-focus-index ref (car l))) 0)
	(else
	 (with r (tree-focus-index ref (cdr l))
	   (if r (+ r 1) #f)))))

(define (tree-get-focus-index ref t l)
  (if (or (== t ref) (not (tree-inside? t ref)))
      (tree-focus-index ref l)
      (with r (tree-focus-index t l)
	(if r r (tree-get-focus-index ref (tree-up t) l)))))

(tm-define (tree-set-diff ref t)
  (:type (-> tree content void))
  (:synopsis "Assign @ref with @t.")
  (let* ((p (tree->path ref))
	 (l (tree-common-left ref t))
	 (r (tree-common-right (tm-range ref l (tm-length ref))
			       (tm-range t l (tm-length t)))))
    (cond ((not p)
	   (texmacs-error "tree-set-diff" "~S isn't part of a document" ref))
	  ((tm-equal? ref t) ref)
	  ((tree-inside? t ref)
	   (with q (tree->path t)
	     (tree-remove-node! ref (list-ref q (length p)))
	     (tree-set-diff ref t)))
	  ((and (tm-atomic? ref) (tm-atomic? t)
		(= (+ l r) (tm-length ref)) (< (tm-length ref) (tm-length t)))
	   (tree-insert! ref l
			 (substring (tm->string t) l (- (tm-length t) r))))
	  ((and (tm-atomic? ref) (tm-atomic? t)
		(= (+ l r) (tm-length t)) (> (tm-length ref) (tm-length t)))
	   (tree-remove! ref l (- (- (tm-length ref) r) l)))
	  ((not (tm-compound? t)) (tree-assign! ref t))
	  ((and (tm-compound? ref) (= l (tm-arity ref)) (= l (tm-arity t)))
	   (tree-assign-node! ref (tm-car t)))
	  ((and (tm-compound? ref)
		(= (+ l r) (tm-arity ref)) (< (tm-arity ref) (tm-arity t)))
	   (tree-insert! ref l (sublist (tm-cdr t) l (- (tm-arity t) r)))
	   (if (== (tm-car ref) (tm-car t)) ref
	       (tree-assign-node! ref (tm-car t))))
	  ((and (tm-compound? ref)
		(= (+ l r) (tm-arity t)) (> (tm-arity ref) (tm-arity t))
                (not (tree-is-buffer? ref)))
	   (tree-remove! ref l (- (- (tm-arity ref) r) l))
	   (if (== (tm-car ref) (tm-car t)) ref
	       (tree-assign-node! ref (tm-car t))))
	  (else
	   (with pos (tree-focus-index ref (tm-cdr t))
	     (if (or (not pos) (tree-is-buffer? ref))
                 (tree-assign! ref t)
		 (let* ((tl (tm->list t))
			(head (list-head tl (+ pos 1)))
			(mid  (list-ref tl (+ pos 1)))
			(tail (list-tail tl (+ pos 2)))
			(merged (append head tail)))
                   (set! ref (tree-set-diff ref mid))
		   (tree-insert-node! ref pos merged))))))))

(tm-define-macro (tree-set-diff! ref t)
  (:synopsis "Assign @ref with @t.")
  `(begin
     (set! ,ref (tree-set-diff ,ref ,t))
     ,ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level tree access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-ref t . l)
  (:synopsis "Access a subtree of @t according to @l.")
  (cond ((not (tree? t)) #f)
	;; NOTE: the following special cases are treated fast,
	((null? l) t)
	((integer? (car l))
	 (with i (car l)
	   (and (tree-compound? t) (>= i 0) (< i (tree-arity t))
		(apply tree-ref (cons (tree-child-ref t i) (cdr l))))))
	((== (car l) :first)
	 (apply tree-ref (cons t (cons 0 (cdr l)))))
	((== (car l) :last)
	 (and (tree-compound? t)
	      (apply tree-ref (cons t (cons (- (tree-arity t) 1) (cdr l))))))
	((symbol? (car l))
	 (and (tree-compound? t)
	      (with i (list-find-index (tree-children t)
				       (cut tree-is? <> (car l)))
		(apply tree-ref (cons t (cons i (cdr l)))))))
	;; but they can all be replaced by the general code below
	(else (with r (select t l)
		(and (nnull? r) (car r))))))

(define (tree-set-sub-error t l)
  (texmacs-error "tree-set-sub" "~S does not admit a subtree along ~S" t l))

(define (tree-set-sub t l u)
  (cond ((not (tree? t)) (texmacs-error "tree-set-sub" "~S is not a tree" t))
	;; NOTE: the following special cases are treated fast and apart
	((null? l)
	 (if (tree-active? t)
	     (tree-set-diff t u)
	     (tree-assign t u)))
	((integer? (car l))
	 (with i (car l)
	   (if (and (tree-compound? t) (>= i 0) (< i (tree-arity t)))
	       (if (or (nnull? (cdr l)) (tree-active? t))
		   (tree-set-sub (tree-child-ref t (car l)) (cdr l) u)
		   (tree-child-set! t (car l) u))
	       (tree-set-sub-error t l))))
	((== (car l) :first)
	 (tree-set-sub t (cons 0 (cdr l)) u))
	((== (car l) :last)
	 (if (tree-compound? t)
	     (tree-set-sub t (cons (- (tree-arity t) 1) (cdr l)) u)
	     (tree-set-sub-error t l)))
	((symbol? (car l))
	 (with i (and (tree-compound? t)
		      (list-find-index (tree-children t)
				       (cut tree-is? <> (car l))))
	   (if i (tree-set-sub t (cons i (cdr l)) u)
	         (tree-set-sub-error t l))))
	;; More cases can be treated for trees in a document
	((tree-active? t)
	 (with r (select t l)
	   (if (nnull? r)
	       (tree-set-diff (car r) u)
	       (tree-set-sub-error t l))))
	(else (tree-set-sub-error t l))))

(tm-define (tree-set t . args)
  (:synopsis "Set a subtree of @t to a new value according to @l.")
  (with r (reverse args)
    (tree-set-sub t (reverse (cdr r)) (car r))))

(tm-define-macro (tree-set! t . l)
  (:synopsis "Set a subtree of @t to a new value according to @l.")
  (if (list-1? l)
      `(if (tree-active? ,t)
	   (tree-set-diff! ,t ,@l)
	   (tree-assign! ,t ,@l))
      `(tree-set ,t ,@l)))

(tm-define (tree-start t . l)
  (path->tree (cDr (apply tree->path (rcons (cons t l) :start)))))

(tm-define (tree-end t . l)
  (path->tree (cDr (apply tree->path (rcons (cons t l) :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upward searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-search-upwards t what)
  (:synopsis "Find ancestor of @t which matches @what")
  (cond ((list? what)
         (tree-search-upwards t (lambda (x) (in? (tree-label x) what))))
        ((symbol? what)
         (tree-search-upwards t (lambda (x) (== (tree-label x) what))))
        ((and (procedure? what) (what t)) t)         
        ((or (tree-is-buffer? t) (not (tree-up t))) #f)
        (else (tree-search-upwards (tree-up t) what))))

(tm-define (tree-innermost x . opt-flag)
  (:type (-> symbol tree)
	 (-> (list symbol) tree)
	 (-> (-> bool) tree))
  (:synopsis "Search upwards from the cursor position.")
  (with p ((if (null? opt-flag) cDDr cDr) (cursor-path))
    (tree-search-upwards (path->tree p) x)))

(tm-define (inside-which l)
  (:type (-> (list symbol) symbol))
  (:synopsis "Get innermost node type among possibilities in @l.")
  (with t (tree-innermost l)
    (and t (tree-label t))))

(tm-define-macro (with-innermost t x . body)
  `(let ((,t (tree-innermost ,x)))
     (if ,t (begin ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-replace t what by)
  (cond ((and (procedure? what) (procedure? by))
	 (if (what t) (by t)
	     (if (tree-compound? t)
		 (for-each (lambda (u) (tree-replace u what by))
			   (tree-children t)))))
	((symbol? what)
	 (tree-replace t (lambda (u) (tree-is? u what)) by))
	((symbol? by)
	 (tree-replace t what
	   (lambda (u) (if (tree-compound? u) (tree-assign-node u by)))))
	(else
	  (let* ((w (tm->tree what))
		 (b (tm->tree by)))
	    (tree-replace t (lambda (u) (== u w))
			    (lambda (u) (tree-assign u (tree-copy b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further routines for trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-is? t . l)
  (let* ((st  (apply tree-ref (cons t (cDr l))))
	 (lab (cAr l)))
    (and st (== (tree-label st) lab))))

(tm-define (tree-in? t . l)
  (let* ((st (apply tree-ref (cons t (cDr l))))
	 (ls (cAr l)))
    (and st (in? (tree-label st) ls))))

(tm-define (tree->path t . l)
  (:synopsis "Get the position of the tree @t.")
  (if (null? l) (tree-get-path t)
      (with i (cAr l)
	(if (or (== i :start) (== i :end) (integer? i))
	    (with u (apply tree-ref (cons t (cDr l)))
	      (cond ((not u) #f)
		    ((== i :start) (path-start (root-tree) (tree->path u)))
		    ((== i :end) (path-end (root-tree) (tree->path u)))
		    ((integer? i) (rcons (tree->path u) i))))
	    (with u (apply tree-ref (cons t l))
	      (and u (tree->path u)))))))

(tm-define (tree-cursor-path t . l)
  (:synopsis "Retrieve the current cursor position relative to the tree @t.")
  (let* ((p (apply tree->path (cons t l)))
	 (c (cursor-path)))
    (and p (list-starts? c p) (list-tail c (length p)))))

(tm-define (tree-go-to t . l)
  (:synopsis "Go to a position determined by @l inside the tree @t.")
  (with p (apply tree->path (cons t l))
    (if p (go-to p))))

(tm-define (tree-cursor-at? t . l)
  (:synopsis "Is the cursor at the position determined by @l inside @t?")
  (with p (apply tree->path (cons t l))
    (== (cursor-path) p)))

(tm-define (tree-select t . l)
  (:synopsis "Select the tree @(tree-ref t . l)")
  (and-with t (apply tree-ref (cons t l))
    (and-with p (tree->path t)
      (selection-set (rcons p 0) (rcons p (tree-right-index t))))))

(tm-define (tree-focus t . l)
  (:synopsis "Focus on the tree @(tree-ref t . l)")
  (and-with t (apply tree-ref (cons t l))
    (and-with p (tree->path t)
      (set-manual-focus-path p))))

(tm-define-macro (with-focus-after t . body)
  `(with tp (tree->tree-pointer ,t)
     ,@body
     (tree-focus (tree-pointer->tree tp))
     (tree-pointer-detach tp)))

(tm-define-macro (conserve-focus . body)
  `(with-focus-after (focus-tree)
     ,@body))

(tm-define (tree-correct-old t . l)
  (:synopsis "Deprecated old tree correction routine")
  (with p (apply tree->path (cons t l))
    (if p (path-correct-old p))))

(tm-define (tree-correct-node t . l)
  (:synopsis "Make the node @(tree-ref t . l) correct")
  (cpp-tree-correct-node (apply tree-ref (cons t l))))

(tm-define (tree-correct-downwards t . l)
  (:synopsis "Correct the tree @(tree-ref t . l) and its descendants")
  (cpp-tree-correct-downwards (apply tree-ref (cons t l))))

(tm-define (tree-correct-upwards t . l)
  (:synopsis "Correct the tree @(tree-ref t . l) and its ancestors")
  (cpp-tree-correct-upwards (apply tree-ref (cons t l))))

(tm-define (update-tree t . l)
  (:synopsis "Re-typeset and render the tree @(tree-ref t . l)")
  (and-let* ((u (apply tree-ref (cons t l)))
             (p (tree->path u)))
    (update-path p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try a modification with possibility for cancelation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (try-modification . body)
  `(with mark-nr (mark-new)
     (mark-start mark-nr)
     (archive-state)
     (with mark-ok (begin ,@body)
       (if mark-ok
	   (mark-end mark-nr)
           (mark-cancel mark-nr))
       mark-ok)))
