
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tree.scm
;; DESCRIPTION : routines for trees and for modifying documents
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental modification routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-assign ref t)
  (:synopsis "Assign @ref with @t.")
  ;;(display* "Assign " ref ", " t "\n")
  (with p (tree->path ref)
    (if p (path-assign p t)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(tm-define-macro (tree-assign! ref t)
  (with var (gensym)
    `(with ,var ,t
       (tree-assign ,ref ,var)
       (set! ,ref ,var))))

(tm-define (tree-insert ref pos t)
  (:synopsis "Insert the children of @t into @ref at position @pos.")
  ;;(display* "Insert " ref ", " pos ", " t "\n")
  (with p (tree->path ref)
    (if p (path-insert (rcons p pos) t)
	(texmacs-error "tree-insert" "~S is not part of a document" ref))))

(tm-define tree-insert! tree-insert)

(tm-define (tree-remove ref pos nr)
  (:synopsis "Remove @nr children from @ref at position @pos.")
  ;;(display* "Remove " ref ", " pos ", " nr "\n")
  (with p (tree->path ref)
    (if p (path-remove (rcons p pos) nr)
	(texmacs-error "tree-remove" "~S is not part of a document" ref))))

(tm-define tree-remove! tree-remove)

(tm-define (tree-split ref pos at)
  (:synopsis "Split the @pos-th child of @ref at position @at.")
  ;;(display* "Split " ref ", " pos ", " at "\n")
  (with p (tree->path ref)
    (if p (path-split (rcons* p pos at))
	(texmacs-error "tree-split" "~S is not part of a document" ref))))

(tm-define tree-split! tree-split)

(tm-define (tree-join ref pos)
  (:synopsis "Split the @pos-th child of @ref with the next child.")
  ;;(display* "Join " ref ", " pos "\n")
  (with p (tree->path ref)
    (if p (path-join (rcons p pos))
	(texmacs-error "tree-join" "~S is not part of a document" ref))))

(tm-define tree-join! tree-join)

(tm-define (tree-insert-node ref pos ins)
  (:synopsis "Transform @ref into @lab with @ref inserted at position @pos.")
  ;;(display* "Insert node " ref ", " pos ", " ins "\n")
  (with p (tree->path ref)
    (if p (path-insert-node (rcons p pos) ins)
	(texmacs-error "tree-insert-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-insert-node! ref pos ins)
  (with var (gensym)
    `(with ,var (tree->path ,ref)
       (tree-insert-node ,ref ,pos ,ins)
       (set! ,ref (path->tree ,var)))))

(tm-define (tree-remove-node ref pos)
  (:synopsis "Replace @ref by its @pos-th child.")
  ;;(display* "Remove node " ref ", " pos "\n")
  (with p (tree->path ref)
    (if p (path-remove-node (rcons p pos))
	(texmacs-error "tree-remove-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-remove-node! ref pos)
  `(begin
     (tree-remove-node ,ref ,pos)
     (set! ,ref (tree-ref ,ref ,pos))))

(tm-define (tree-assign-node ref lab)
  (:synopsis "Replace the label of @ref by @lab.")
  ;;(display* "Assign node " ref ", " lab "\n")
  (with p (tree->path ref)
    (if p (path-assign-node p lab)
	(texmacs-error "tree-assign-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-assign-node! ref lab)
  (with var (gensym)
    `(with ,var (tree->path ,ref)
       (tree-assign-node ,ref ,lab)
       (set! ,ref (path->tree ,var)))))

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

(define (tree-focus ref l)
  (cond ((null? l) #f)
	((tree-inside? (car l) ref) 0)
	((and (list? (car l)) (tree-focus ref (car l))) 0)
	(else
	 (with r (tree-focus ref (cdr l))
	   (if r (+ r 1) #f)))))

(define (tree-get-focus ref t l)
  (if (or (== t ref) (not (tree-inside? t ref)))
      (tree-focus ref l)
      (with r (tree-focus t l)
	(if r r (tree-get-focus ref (tree-up t) l)))))

(tm-define (tree-set-diff ref t)
  (:type (-> tree content void))
  (:synopsis "Assign @ref with @t.")
  (let* ((p (tree->path ref))
	 (l (tree-common-left ref t))
	 (r (tree-common-right (tm-range ref l (tm-length ref))
			       (tm-range t l (tm-length t)))))
    (cond ((not p)
	   (texmacs-error "tree-set-diff" "~S isn't part of a document" ref))
	  ((tm-equal? ref t) (noop))
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
	  ((not (tm-compound? t)) (tree-assign ref t))
	  ((and (tm-compound? ref) (= l (tm-arity ref)) (= l (tm-arity t)))
	   (tree-assign-node ref (tm-car t)))
	  ((and (tm-compound? ref)
		(= (+ l r) (tm-arity ref)) (< (tm-arity ref) (tm-arity t)))
	   (tree-insert! ref l
			 (cons (tm-car ref)
			       (sublist (tm-cdr t) l (- (tm-arity t) r))))
	   (if (!= (tm-car ref) (tm-car t)) (tree-assign-node ref (tm-car t))))
	  ((and (tm-compound? ref)
		(= (+ l r) (tm-arity t)) (> (tm-arity ref) (tm-arity t)))
	   (tree-remove! ref l (- (- (tm-arity ref) r) l))
	   (if (!= (tm-car ref) (tm-car t)) (tree-assign-node ref (tm-car t))))
	  (else
	   (with pos (tree-focus ref (tm-cdr t))
	     (if (not pos) (tree-assign ref t)
		 (let* ((tl (tm->list t))
			(head (list-head tl (+ pos 1)))
			(mid  (list-ref tl (+ pos 1)))
			(tail (list-tail tl (+ pos 2))))
		   (tree-set-diff! ref mid)
		   (tree-insert-node ref pos (append head tail)))))))))

(tm-define-macro (tree-set-diff! ref t)
  (with var (gensym)
    `(with ,var (tree->path ,ref)
       (tree-set-diff ,ref ,t)
       (set! ,ref (path->tree ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level tree access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-ref t . l)
  (:synopsis "Access a subtree of @t according to @l.")
  (cond ((not (tree? t)) #f)
	((and (list-1? l) (integer? (car l)))
	 (tree-child t (car l)))
	(else (with r (select t l)
		(and (nnull? r) (car r))))))

(tm-define (tree-set t . args)
  (:synopsis "Set a subtree of @t to a new value according to @l.")
  (let ((l (cDr args))
	(u (cAr args)))
    (with r (select t l)
      (if (nnull? r)
	  (tree-set-diff (car r) u)))))

(tm-define-macro (tree-set! t . l)
  (if (list-1? l)
      `(tree-set-diff! ,t ,@l)
      `(tree-set ,t ,@l)))

(tm-define (tree-start t . l)
  (path->tree (cDr (apply tree->path (rcons (cons t l) :start)))))

(tm-define (tree-end t . l)
  (path->tree (cDr (apply tree->path (rcons (cons t l) :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upward searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-innermost-sub p pred?)
  (with t (path->tree p)
    (cond ((and (tm-compound? t) (pred? t)) t)
	  ((or (null? p) (== p (buffer-path))) #f)
	  (else (tree-innermost-sub (cDr p) pred?)))))

(tm-define (tree-innermost x)
  (:type (-> symbol tree)
	 (-> (list symbol) tree)
	 (-> (-> bool) tree))
  (:synopsis "Search upwards from the cursor position.")
  (let* ((p (cDDr (cursor-path)))
	 (pred? (cond ((procedure? x) x)
		      ((list? x) (lambda (t) (and (tm-compound? t)
						  (in? (tree-label t) x))))
		      (else (lambda (t) (and (tm-compound? t)
					     (== (tree-label t) x)))))))
    (tree-innermost-sub p pred?)))

(tm-define (inside-which l)
  (:type (-> (list symbol) symbol))
  (:synopsis "Get innermost node type among possibilities in @l.")
  (with t (tree-innermost l)
    (and t (tree-label t))))

(tm-define-macro (with-innermost t x . body)
  `(let ((,t (tree-innermost ,x)))
     (if ,t (begin ,@body))))

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

(tm-define (tree-correct t . l)
  (with p (apply tree->path (cons t l))
    (if p (path-correct p))))
