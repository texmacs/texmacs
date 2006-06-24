
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

(define-module (tools list))
(use-modules (tools base) (tools abbrevs) (tools ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (cons* . l)
  "Construct a list from head of several elements and a tail."
  ;; This function is in the kernel since GUILE-1.4.0
  (let ((r (reverse l)))
    (append (reverse (cdr r)) (car r))))

(define-public (rcons l x)
  "Append @x to @l at the end."
  (append l (list x)))

(define-public (rcons* l . xs)
  "Append several elements @xs to @l at the end."
  (append l xs))

(define-public-macro (set-cons! sym x)
  `(set! ,sym (cons ,x ,sym)))

(define-public-macro (set-rcons! sym x)
  `(set! ,sym (rcons ,sym ,x)))

(define-public (list-concatenate ls)
  "Append the elements of @ls toghether."
  ;; WARNING: not portable for long lists
  (apply append ls))

(define-public (list-intersperse l x)
  "Insert @x between each element of @l."
  (if (null? l) '()
      (cdr (list-fold-right (lambda (kar kdr) (cons* x kar kdr)) '() l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (first l)
  "Get first element of @l."
  (car l))

(define-public (second l)
  "Get second element of @l."
  (cadr l))

(define-public (third l)
  "Get third element of @l."
  (caddr l))

(define-public (fourth l)
  "Get first element of @l."
  (cadddr l))

(define-public (fifth l)
  "Get fifth element of @l."
  (car (cddddr l)))

(define-public (sixth l)
  "Get sixth element of @l."
  (cadr (cddddr l)))

(define-public (seventh l)
  "Get seventh element of @l."
  (caddr (cddddr l)))

(define-public (eighth l)
  "Get eighht element of @l."
  (cadddr (cddddr l)))

(define-public (ninth l)
  "Get ninth element of @l."
  (car (cddddr (cddddr l))))

(define-public (tenth l)
  "Get tenth element of @l."
  (cadr (cddddr (cddddr l))))

(define-public (cAr l)
  "Get last element of @l."
  (car (last-pair l)))

(define-public (cDr l)
  "Remove last element from @l."
  (reverse (cdr (reverse l))))

(define-public (cADr l)
  "Get before last element of @l."
  (cadr (reverse l)))

(define-public (cDDr l)
  "Remove two last elements from @l"
  (reverse (cddr (reverse l))))

(define-public (cDDDr l)
  "Remove two last elements from @l"
  (reverse (cdddr (reverse l))))

(define-public (cDdr l)
  "Remove first and last elements from @l"
  (cDr (cdr l)))

(define-public (cDddr l)
  "Remove two first and last elements from @l"
  (cDr (cddr l)))

(define-public (cdAr l)
  "All but the first element of the last element of @l"
  (cdr (cAr l)))

(define-public last cAr)
(define-public but-last cDr)

(define-public (car+cdr p)
  "Fundamental pair deconstructor."
  (values (car p) (cdr p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extraction of sublists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public list-take list-head) ;; SRFI-1
(define-public list-drop list-tail) ;; SRFI-1

(define-public (list-take-right l i)
  "Return the last @i elements of @l."
  (list-tail l (- (length l) i)))

(define-public (list-drop-right l i)
  "Return all but the last @i elements of @l."
  (list-head l (- (length l) i)))

(define-public (sublist l i j)
  "Extract sublist from @l beginning at @i and ending at @j."
  (list-head (list-tail l i) (- j i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circulating lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-circulate-right l n)
  "Right circular shift of @n steps on @l."
  (if (= n 1) (cons (last l) (but-last l))
      (append (list-take-right l n) (list-drop-right l n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold, unfold & map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-fold kons knil list1 . rest)
  "Fundamental list iterator."
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

(define-public (list-fold-right kons knil clist1 . rest)
  "Fundamental list recursion operator."
  (if (null? rest)
      (let f ((list1 clist1))
	(if (null? list1) knil
	    (kons (car list1) (f (cdr list1)))))
      (let f ((lists (cons clist1 rest)))
	(if (list-any null? lists) knil
	    (apply kons (append! (map-in-order car lists)
				 (list (f (map-in-order cdr lists)))))))))

(define-public (pair-fold kons knil clist1 . rest)
  "Analogous to @fold but applies @kons to pairs of @clist1..."
  (if (null? rest)
      (let f ((knil knil) (list1 clist1))
	(if (null? list1) knil
	    (let ((tail (cdr list1)))
	      (f (kons list1 knil) tail))))
      (let f ((knil knil) (lists (cons clist1 rest)))
	(if (list-any null? lists) knil
	    (let ((tails (map-in-order cdr lists)))
	      (f (apply kons (append! lists (list knil))) tails))))))

(define-public (pair-fold-right kons knil clist1 . rest)
  "Analogous to @fold-right but applies @kons to pairs of @clist1..."
  (if (null? rest)
    (let f ((list1 clist1))
      (if (null? list1)
	knil
	(kons list1 (f (cdr list1)))))
    (let f ((lists (cons clist1 rest)))
      (if (list-any null? lists)
	knil
	(apply kons (append! lists (list (f (map-in-order cdr lists)))))))))

(define-public (append-map . params)
  "Apply @f to all elements of @l and append the resulting lists."
  ;; WARNING: not portable for long lists
  (apply append (apply map-in-order params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering & partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-filter l pred?)
  "Return the list of elements from @l which match @pred?."
  (let rec ((l l))
    (cond ((null? l) l)
	  ((pred? (car l)) (cons (car l) (rec (cdr l))))
	  (else (rec (cdr l))))))

(define-public (list-partition l pred?)
  "Partition a @l into two parts according to @pred?."
  (let rec ((l l))
    (cond ((null? l) (values '() '()))
	  ((pred? (car l)) (receive (in out) (rec (cdr l))
			     (values (cons (car l) in) out)))
	  (else (receive (in out) (rec (cdr l))
		  (values in (cons (car l) out)))))))

(define-public (list-break l pred?)
  "Break @l at the first element satisfying @pred?."
  ;; Adaptation of "break" (SRFI-26)
  (let rec ((l l))
    (cond ((null? l) (values '() '()))
	  ((pred? (car l)) (values '() l))
	  (else (receive (first last) (rec (cdr l))
		  (values (cons (car l) first) last))))))

(define-public (list-span l pred?)
  "Break @l at the first element not satisfying @pred?."
  (list-break l (non pred?)))

(define-public (list-drop-while l pred?)
  "Drop the first elements of @l while @pred? is satisfied."
  (let next ((l l))
    (cond ((null? l) '())
	  ((pred? (car l)) (next (cdr l)))
	  (else l))))

(define-public (list-scatter l pred? keep?)
  "Break @l in list of sublists at points satisfying @pred?."
  (receive (head tail) (list-break l pred?)
    (if (null? tail) (list head)
	(with r (list-scatter (cdr tail) pred? keep?)
	  (if keep?
	      (cons* head (cons (car tail) (car r)) (cdr r))
	      (cons head r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-find l pred?)
  "Applies @pred? on elements of @l until it evaluates to true."
  (let next ((l l))
    (if (null? l) #f
	(if (pred? (car l))
	    (car l)
	    (next (cdr l))))))

(define-public (list-find-index l pred?)
  "Find first index in @l which matches @pred"
  (let find ((l l) (i 0))
    (cond ((null? l) #f)
	  ((pred? (car l)) i)
	  (else (find (cdr l) (+ i 1))))))

(define (any1 pred? ls)
  ;; Internal helper.
  (let lp ((ls ls))
    (cond ((null? ls) #f)
	  ((null? (cdr ls)) (pred? (car ls)))
	  (else (or (pred? (car ls)) (lp (cdr ls)))))))

(define-public (list-any pred? ls . lists)
  "Applies @pred? on elements of @ls until it evaluates to true."
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

(define-public (list-every pred? ls . lists)
  "Applies @pred? on elements of @ls until it evaluates to @#f."
  ;; FIXME: pred? and ls should be interchanged
  (if (null? lists) (every1 pred? ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists) #t)
	      ((any1 null? (map-in-order cdr lists))
	       (apply pred? (map-in-order car lists)))
	      (else (and (apply pred? (map-in-order car lists))
			 (lp (map-in-order cdr lists))))))))

(define-public (list-starts? l what)
  "Test whether @what is a prefix of @l."
  (cond ((null? what) #t)
	((null? l) #f)
	(else (and (== (car l) (car what))
		   (list-starts? (cdr l) (cdr what))))))

(define-public (list-replace l what by)
  "Replace @what by @by in @l."
  (cond ((null? l) l)
	((list-starts? l what)
	 (let ((tail (list-tail l (length what))))
	   (append by (list-replace tail what by))))
	(else (cons (car l) (list-replace (cdr l) what by)))))

(define-public (list-common-left l1 l2)
  "Length of maximal common list on the left."
  (if (or (npair? l1) (npair? l2) (!= (car l1) (car l2))) 0
      (+ (list-common-left (cdr l1) (cdr l2)) 1)))

(define-public (list-common-right l1 l2)
  "Length of maximal common list on the right."
  (list-common-left (reverse l1) (reverse l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set operations on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-remove-duplicates-sub t l)
  (cond ((null? l) l)
	((ahash-ref t (car l)) (list-remove-duplicates-sub t (cdr l)))
	(else
	 (ahash-set! t (car l) #t)
	 (cons (car l) (list-remove-duplicates-sub t (cdr l))))))

(define-public (list-remove-duplicates l)
  (with t (make-ahash-table)
    (list-remove-duplicates-sub t l)))

(define-public (ahash-set->list s)
  (map car (ahash-table->list s)))

(define-public (list->ahash-set l)
  (list->ahash-table (map (lambda (x) (cons x #t)) l)))

(define-public (list-intersection l1 l2)
  (with s (list->ahash-set l2)
    (list-filter l1 (lambda (x) (ahash-ref s x)))))

(define-public (list-difference l1 l2)
  (with s (list->ahash-set l2)
    (list-filter l1 (lambda (x) (not (ahash-ref s x))))))

(define-public (list-union l1 l2)
  (append l1 (list-difference l2 l1)))

(define-public (list-permutation? l1 l2)
  (and (null? (list-difference l1 l2))
       (null? (list-difference l2 l1))))

(define-public (list-remove l what)
  (cond ((null? l) l)
	((== (car l) what) (list-remove (cdr l) what))
	(else (cons (car l) (list-remove (cdr l) what)))))

(define-public (list-add l what)
  (append (list-remove l what) (list what)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other operations on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-and l)
  "Compute logical and of list @l of booleans."
  (or (null? l) (and (car l) (list-and (cdr l)))))

(define-public (list-or l)
  "Compute logical or of list @l of booleans."
  (and (nnull? l) (or (car l) (list-or (cdr l)))))

(define-public (list-length=2? x)
  "Is @x a proper list of exactly two elements?"
  (and (pair? x)
       (pair? (cdr x))
       (null? (cddr x))))
