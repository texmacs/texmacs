
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : list.scm
;; DESCRIPTION : extra routines for lists
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library list)
  (:use (kernel texmacs tm-define))
  (:export
    ;; constructors
    rcons rcons* set-cons! set-rcons! list-concatenate list-intersperse
    ;; selectors
    first second third fourth fifth sixth seventh eighth ninth tenth
    cAr cDr cADr cDDr cDdr cDddr last but-last car+cdr
    ;; extraction of sublists
    list-take list-drop list-take-right list-drop-right sublist
    ;; circulating lists
    list-circulate-right
    ;; fold, unfold & map
    list-fold list-fold-right pair-fold pair-fold-right append-map
    ;; filtering & partitioning
    list-filter list-partition list-break list-span
    list-drop-while list-scatter
    ;; search and replace
    list-find list-any list-every in? list-starts? list-replace
    ;; other operations on lists
    list-and list-or list-length=2?))
(re-export cons*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cons* . l)
  (:type (forall T ((tuple T) (list T) -> (list T))))
  (:synopsis "Construct a list from head of several elements and a tail.")
  ;; This function is in the kernel since GUILE-1.4.0
  (let ((r (reverse l)))
    (append (reverse (cdr r)) (car r))))

(tm-define (rcons l x)
  (:type (forall T ((list T) T -> (list T))))
  (:synopsis "Append @x to @l at the end.")
  (append l (list x)))

(tm-define (rcons* l . xs)
  (:type (forall T ((list T) (tuple T) -> (list T))))
  (:synopsis "Append several elements @xs to @l at the end.")
  (append l xs))

(define-macro (set-cons! sym x)
  `(set! ,sym (cons ,x ,sym)))

(define-macro (set-rcons! sym x)
  `(set! ,sym (rcons ,sym ,x)))

(tm-define (list-concatenate ls)
  (:type (forall T ((tuple (list T)) -> (list T))))
  (:synopsis "Append the elements of @ls toghether.")
  ;; WARNING: not portable for long lists
  (apply append ls))

(tm-define (list-intersperse l x)
  (:type (forall T ((list T) T -> (list T))))
  (:synopsis "Insert @x between each element of @l.")
  (if (null? l) '()
      (cdr (list-fold-right (lambda (kar kdr) (cons* x kar kdr)) '() l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define first
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get first element of @l.")
  car)

(tm-define second
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get second element of @l.")
  cadr)

(tm-define third
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get third element of @l.")
  caddr)

(tm-define fourth
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get first element of @l.")
  cadddr)

(tm-define (fifth l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get fifth element of @l.")
  (car (cddddr l)))

(tm-define (sixth l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get sixth element of @l.")
  (cadr (cddddr l)))

(tm-define (seventh l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get seventh element of @l.")
  (caddr (cddddr l)))

(tm-define (eighth l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get eighht element of @l.")
  (cadddr (cddddr l)))

(tm-define (ninth l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get ninth element of @l.")
  (car (cddddr (cddddr l))))

(tm-define (tenth l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get tenth element of @l.")
  (cadr (cddddr (cddddr l))))

(tm-define (cAr l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get last element of @l.")
  (car (last-pair l)))

(tm-define (cDr l)
  (:type (forall T ((list T) -> (list T))))
  (:synopsis "Remove last element from @l.")
  (reverse (cdr (reverse l))))

(tm-define (cADr l)
  (:type (forall T ((list T) -> T)))
  (:synopsis "Get before last element of @l.")
  (cadr (reverse l)))

(tm-define (cDDr l)
  (:type (forall T ((list T) -> (list T))))
  (:synopsis "Remove two last elements from @l")
  (reverse (cddr (reverse l))))

(tm-define (cDdr l)
  (:type (forall T ((list T) -> (list T))))
  (:synopsis "Remove first and last elements from @l")
  (cDr (cdr l)))

(tm-define (cDddr l)
  (:type (forall T ((list T) -> (list T))))
  (:synopsis "Remove two first and last elements from @l")
  (cDr (cddr l)))

(define last cAr)
(define but-last cDr)

(tm-define (car+cdr p)
  (:type (forall T ((list T) -> T (list T))))
  (:synopsis "Fundamental pair deconstructor.")
  (values (car p) (cdr p)))

;(tm-define (but-last! l)
;  (:type (forall T ((list T) -> (list T))))
;  (:synopsis "Remove last element from @l. Linear update.")
;  (reverse! (cdr (reverse! l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extraction of sublists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-take list-head) ;; SRFI-1
(define list-drop list-tail) ;; SRFI-1

(tm-define (list-take-right l i)
  (:type (forall T ((list T) int -> (list T))))
  (:synopsis "Return the last @i elements of @l.")
  (list-tail l (- (length l) i)))

(tm-define (list-drop-right l i)
  (:type (forall T ((list T) int -> (list T))))
  (:synopsis "Return all but the last @i elements of @l.")
  (list-head l (- (length l) i)))

;(tm-define (list-drop-right! flist i)
;  (:type (forall T ((list T) int -> (list T))))
;  (:synopsis "Remove the last @i elements of @flist. Linear update.")
;  (if (<= i 0) flist
;      (let lp ((n (+ i 1)) (l flist))
;	(if (<= n 0)
;	    (let lp0 ((s flist) (l l))
;	      (if (null? l)
;		  (begin (set-cdr! s '()) flist)
;		  (lp0 (cdr s) (cdr l))))
;	    (if (null? l) '()
;		(lp (- n 1) (cdr l)))))))

(tm-define (sublist l i j)
  (:type (forall T ((list T) int int -> (list T))))
  (:synopsis "Extract sublist from @l beginning at @i and ending at @j.")
  (list-head (list-tail l i) (- j i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circulating lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-circulate-right l n)
  (:type (forall T ((list T) int int -> (list T))))
  (:synopsis "Right circular shift of @n steps on @l.")
  (if (= n 1) (cons (last l) (but-last l))
      (append (list-take-right l n) (list-drop-right l n))))

;(define (list-circulate-right! l n)
;  (:type (forall T ((list T) int int -> (list T))))
;  (:synopsis "Right circular shift of @n steps on @l. Linear update.")
;  (if (= n 1) (cons (last l) (but-last! l))
;      (append (list-take-right l n) (list-drop-right! l n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold, unfold & map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-fold kons knil list1 . rest)
  (:type (forall T U (((tuple T) U -> U) U (tuple (list T)) -> U)))
  (:synopsis "Fundamental list iterator.")
  (if (null? rest)
      (let f ((knil knil) (list1 list1))
	(if (null? list1)
	    knil
	    (f (kons (car list1) knil) (cdr list1))))
      (let f ((knil knil) (lists (cons list1 rest)))
	(if (list-any null? lists)
	    knil
	    (let ((cars (map-in-order car lists))
		  (cdrs (map-in-order cdr lists)))
	      (f (apply kons (append! cars (list knil))) cdrs))))))

(tm-define (list-fold-right kons knil clist1 . rest)
  (:type (forall T U (((tuple T) U -> U) U (tuple (list T)) -> U)))
  (:synopsis "Fundamental list recursion operator.")
  (if (null? rest)
      (let f ((list1 clist1))
	(if (null? list1) knil
	    (kons (car list1) (f (cdr list1)))))
      (let f ((lists (cons clist1 rest)))
	(if (list-any null? lists) knil
	    (apply kons (append! (map-in-order car lists)
				 (list (f (map-in-order cdr lists)))))))))

(tm-define (pair-fold kons knil clist1 . rest)
  (:type (forall T U (((tuple (list T)) U -> U) U (tuple (list T)) -> U)))
  (:synopsis "Analogous to fold but applies @kons to pairs of @clist1...")
  (if (null? rest)
      (let f ((knil knil) (list1 clist1))
	(if (null? list1) knil
	    (let ((tail (cdr list1)))
	      (f (kons list1 knil) tail))))
      (let f ((knil knil) (lists (cons clist1 rest)))
	(if (list-any null? lists) knil
	    (let ((tails (map-in-order cdr lists)))
	      (f (apply kons (append! lists (list knil))) tails))))))

(tm-define (pair-fold-right kons knil clist1 . rest)
  (:type (forall T U (((tuple (list T)) U -> U) U (tuple (list T)) -> U)))
  (:synopsis "Analogous to fold-right but applies kons to pairs of @clist1...")
  (if (null? rest)
    (let f ((list1 clist1))
      (if (null? list1)
	knil
	(kons list1 (f (cdr list1)))))
    (let f ((lists (cons clist1 rest)))
      (if (list-any null? lists)
	knil
	(apply kons (append! lists (list (f (map-in-order cdr lists)))))))))

(tm-define (append-map . params)
  (:type (forall T U (((tuple T) -> (list U)) (tuple (list T)) -> (list U))))
  (:synopsis "Apply @f to all elements of @l and append the resulting lists.")
  ;; WARNING: not portable for long lists
  (apply append (apply map-in-order params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering & partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-filter l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list T))))
  (:synopsis "Return the list of elements from @l which match @pred?.")
  (let rec ((l l))
    (cond ((null? l) l)
	  ((pred? (car l)) (cons (car l) (rec (cdr l))))
	  (else (rec (cdr l))))))

(tm-define (list-partition l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list T) (list T))))
  (:synopsis "Partition a @l into two parts according to @pred?.")
  (:args (l "list to be partitioned")
	 (pred? "predicate"))
  (:returns (1 "list of elements which satisfy @pred?")
	    (2 "list of elements which do not satisfy @pred?"))
  (let rec ((l l))
    (cond ((null? l) (values '() '()))
	  ((pred? (car l)) (receive (in out) (rec (cdr l))
			     (values (cons (car l) in) out)))
	  (else (receive (in out) (rec (cdr l))
		  (values in (cons (car l) out)))))))

(tm-define (list-break l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list T) (list T))))
  (:synopsis "Break @l at the first element satisfying @pred?.")
  (:args (l "list to be broken")
	 (pred? "predicate"))
  (:returns (1 "longest prefix of @t such that no element matches @pred?")
	    (2 "corresponding suffix"))
  (let rec ((l l))
    (cond ((null? l) (values '() '()))
	  ((pred? (car l)) (values '() l))
	  (else (receive (first last) (rec (cdr l))
		  (values (cons (car l) first) last))))))

(tm-define (list-span l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list T) (list T))))
  (:synopsis "Break @l at the first element not satisfying @pred?.")
  (:see-also list-break)
  (list-break l (negate pred?)))

(tm-define (list-drop-while l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list T))))
  (:synopsis "Drop the first elements of @l while @pred? is satisfied.")
  (:returns "Longest tail of @l whose first element satisfies @pred?.")
  (let next ((l l))
    (cond ((null? l) '())
	  ((pred? (car l)) (next (cdr l)))
	  (else l))))

(tm-define (list-scatter l pred?)
  (:type (forall T ((list T) (T -> bool) -> (list (list T)))))
  (:synopsis "Break @l in list of sublists at points satisfying @pred?.")
  (:args (l "list to be broken")
	 (pred? "predicate"))
  (receive (head tail) (list-break l pred?)
    (if (null? tail) (list head)
	(with r (list-scatter (cdr tail) pred?)
	  (cons* head (cons (car tail) (car r)) (cdr r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-find l pred?)
  (:type (forall T ((list T) (T -> bool) -> T)))
  (:synopsis "Applies @pred? on elements of @l until it evaluates to true.")
  (:returns "First element of @l that satisfies @pred? or @#f.")
  (let next ((l l))
    (if (null? l) #f
	(if (pred? (car l))
	    (car l)
	    (next (cdr l))))))

(define (any1 pred? ls)
  ;; Internal helper.
  (let lp ((ls ls))
    (cond ((null? ls) #f)
	  ((null? (cdr ls)) (pred? (car ls)))
	  (else (or (pred? (car ls)) (lp (cdr ls)))))))

(tm-define (list-any pred? ls . lists)
  (:synopsis "Applies @pred? on elements of @ls until it evaluates to true.")
  (:returns "last value returned by pred? or @#f")
  ;; FIXME: pred? and ls should be interchanged
  (if (null? lists) (any1 pred? ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists) #f)
	      ((any1 null? (map-in-order cdr lists))
	       (apply pred? (map-in-order car lists)))
	      (else (or (apply pred? (map-in-order car lists))
			(lp (map-in-order cdr lists))))))))

(define (every1 pred? ls)
  ;; Internal helper.
  (let lp ((ls ls))
    (cond ((null? ls)  #t)
	  ((null? (cdr ls)) (pred? (car ls)))
	  (else (and (pred? (car ls)) (lp (cdr ls)))))))

(tm-define (list-every pred? ls . lists)
  (:synopsis "Applies @pred? on elements of @ls until it evaluates to @#f.")
  (:returns "last value returned by @pred? or @#t")
  ;; FIXME: pred? and ls should be interchanged
  (if (null? lists) (every1 pred? ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists) #t)
	      ((any1 null? (map-in-order cdr lists))
	       (apply pred? (map-in-order car lists)))
	      (else (and (apply pred? (map-in-order car lists))
			 (lp (map-in-order cdr lists))))))))

(tm-define (in? x l)
  (:type (forall T (T (list T) -> bool)))
  (:synopsis "Test whether @x occurs among the elements of @l.")
  (not (not (member x l))))

(tm-define (list-starts? l what)
  (:type (forall T ((list T) (list T) -> bool)))
  (:synopsis "Test whether @what is a prefix of @l.")
  (cond ((null? what) #t)
	((null? l) #f)
	(else (and (== (car l) (car what))
		   (list-starts? (cdr l) (cdr what))))))

(tm-define (list-replace l what by)
  (:type (forall T ((list T) (list T) (list T) -> (list T))))
  (:synopsis "Replace @what by @by in @l.")
  (cond ((null? l) l)
	((list-starts? l what)
	 (let ((tail (list-tail l (length what))))
	   (append by (list-replace tail what by))))
	(else (cons (car l) (list-replace (cdr l) what by)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other operations on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-and l)
  (:type (forall T ((list bool) -> bool)))
  (:synopsis "Compute logical and of list @l of booleans.")
  (or (null? l) (and (car l) (list-and (cdr l)))))

(tm-define (list-or l)
  (:type (forall T ((list bool) -> bool)))
  (:synopsis "Compute logical or of list @l of booleans.")
  (and (not (null? l)) (or (car l) (list-or (cdr l)))))

(tm-define (list-length=2? x)
  (:type ((list T) -> bool))
  (:synopsis "Is @x a proper list of exactly two elements?")
  (and (pair? x)
       (pair? (cdr x))
       (null? (cddr x))))
