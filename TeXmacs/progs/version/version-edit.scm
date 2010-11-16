
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-edit.scm
;; DESCRIPTION : editing routines for versioning
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-edit)
  (:use (version version-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-context? t)
  (version-tag? (tree-label t)))

(tm-define (inside-version?)
  (not (not (tree-innermost version-context?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine for computing all subtrees in a selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (selection-subtrees t p1 p2)
  (cond ((or (null? p1) (null? p2)) '())
	((and (== p1 (list 0)) (== p2 (list (tree-right-index t)))) (list t))
	((tree-atomic? t) '())
	((== p1 (list (tree-right-index t))) '())
	((== p2 (list 0)) '())
	((== p1 (list 0))
	 (selection-subtrees t (list 0 0) p2))
	((== p2 (list (tree-right-index t)))
	 (with n (- (tree-arity t) 1)
	   (with c (tree-ref t n)
	     (selection-subtrees t p1 (list n (tree-right-index c))))))
	((== (car p1) (car p2))
	 (selection-subtrees (tree-ref t (car p1)) (cdr p1) (cdr p2)))
	((< (car p1) (car p2))
	 (let* ((i1 (car p1))
		(i2 (car p2))
		(t1 (tree-ref t i1))
		(t2 (tree-ref t i2))
		(l1 (selection-subtrees t p1 (list i1 (tree-right-index t1))))
		(l2 (selection-subtrees t (list i2 0) p2))
		(ll (sublist (tree-children t) (+ i1 1) i2)))
	   (append l1 ll l2)))
	(else '())))

(tm-define (selection-trees)
  (let* ((p1 (selection-get-start))
	 (p2 (selection-get-end)))
    (selection-subtrees (root-tree) p1 p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving across the differences between both versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-first-difference)
  (go-start)
  (version-next-difference))

(tm-define (version-previous-difference)
  (go-to-previous-tag (group-resolve 'version-tag)))

(tm-define (version-next-difference)
  (go-to-next-tag (group-resolve 'version-tag)))

(tm-define (version-last-difference)
  (go-end)
  (version-previous-difference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch between different visualizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-show tag)
  (cond ((selection-active-any?)
	 (for-each (lambda (u) (tree-replace u version-context? tag))
		   (selection-trees)))
	((inside-version?)
         (with t (tree-innermost version-context?)
           (variant-set t tag)))))

(tm-define (version-show-all tag)
  (tree-replace (buffer-tree) version-context? tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining only one version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-normalize t)
  (when (tree-compound? t)
    (for-each tree-normalize (tree-children t))
    (when (tree-in? t '(concat document))
      (with fun
	  (lambda (u)
	    (cond ((tree-is? u (tree-label t)) (tree-children u))
		  ((and (== u (tree "")) (tree-is? t 'concat)) (list))
		  (else (list u))))
	(with c (apply append (map fun (tree-children t)))
	  (when (!= c (tree-children t))
	    (cond ((null? c)
		   (tree-assign t (if (tree-is? t 'concat) "" '(document ""))))
		  ((and (tree-is? t 'concat) (null? (cdr c)))
		   (tree-assign t (car c)))
		  (else (tree-assign t `(,(tree-label t) ,@c))))))))))

(define (retain-version t new)
  (if (or (== new (tm->tree '(version-suppressed)))
	  (== new (tm->tree '(document (version-suppressed)))))
      (let* ((p (tree-up t))
	     (i (tree-index t)))
	(cond ((and p i) (tree-remove p i 1))
	      ((== new (tm->tree '(version-suppressed))) (tree-set t ""))
	      (else (tree-set t '(document)))))
      (tree-set t new)))

(define (version-retain-version where which)
  (with p (if (version-context? where) (tree-up where) where)
    (tree-replace where version-context?
		  (lambda (t)
		    (cond ((number? which)
			   (retain-version t (tree-ref t which)))
			  ((tree-is? t 'version-old)
			   (retain-version t (tree-ref t 0)))
			  (else
			   (retain-version t (tree-ref t 1))))))
    (tree-normalize p)))

(define (tree-go-to-start t p)
  (if (and (nnull? p) (< (car p) (tree-arity t)))
      (tree-go-to-start (tree-ref t (car p)) (cdr p))
      (tree-go-to t :start)))

(tm-define (version-retain which)
  (cond ((selection-active-any?)
	 (for-each (lambda (u) (version-retain-version u which))
		   (selection-trees)))
	((inside-version?)
	 (with-innermost t version-context?
	   (with p (tree->path t)
	     (tree-go-to t (if (tree-is? t 'version-old) 0 1) :end)
	     (version-next-difference)	   
	     (version-retain-version t which)
	     (when (not (inside-version?))
	       (tree-go-to-start (root-tree) p)
	       (version-next-difference)
	       (when (not (inside-version?))
		 (version-previous-difference))))))))

(tm-define (version-retain-all which)
  (version-retain-version (buffer-tree) which))
