
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

(tm-define (version-context? t)
  (version-tag? (tree-label t)))

(tm-define (inside-version?)
  (not (not (tree-innermost version-context?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving across the differences between both versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-first-difference)
  (go-start)
  (go-to-next-tag (group-resolve 'version-tag)))

(tm-define (version-previous-difference)
  (go-to-previous-tag (group-resolve 'version-tag)))

(tm-define (version-next-difference)
  (go-to-next-tag (group-resolve 'version-tag)))

(tm-define (version-last-difference)
  (go-end)
  (go-to-previous-tag (group-resolve 'version-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify which version to show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-show-both)
  (:context version-context?)
  (variant-replace version-context? 'version-both))

(tm-define (version-show-old)
  (:context version-context?)
  (variant-replace version-context? 'version-old))

(tm-define (version-show-new)
  (:context version-context?)
  (variant-replace version-context? 'version-new))

(tm-define (version-show-all tag)
  (tree-replace (buffer-tree) version-context? tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining only one version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-retain-current)
  (:context version-context?)
  (variant-replace version-context? 'version-both))

(tm-define (version-retain-old)
  (with-innermost t version-context?
    (tree-set t (tree-ref t 0))
    (version-next-difference)))

(tm-define (version-retain-new)
  (with-innermost t version-context?
    (tree-set t (tree-ref t 1))
    (version-next-difference)))

(tm-define (version-retain-current)
  (:inside version-old)
  (version-retain-old))

(tm-define (version-retain-current)
  (:inside version-new version-both)
  (version-retain-new))

(tm-define (version-retain-all which)
  (tree-replace (buffer-tree) version-context?
		(lambda (t)
		  (cond ((number? which)
			 (tree-set t (tree-ref t which)))
			((tree-is? t 'version-old)
			 (tree-set t (tree-ref t 0)))
			(else
			 (tree-set t (tree-ref t 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding a long common subsequence where to break
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (common l1 l2)
  (if (or (null? l1) (null? l2) (!= (car l1) (car l2))) 0
      (+ (common (cdr l1) (cdr l2)) 1)))

(define (longest-common l1 l2)
  ;;(display* "longest-common " l1 ", " l2 "\n")
  (cond ((or (null? l1) (null? l2))
	 (values 0 0 0))
	((in? (car l1) l2)
	 (let* ((i (list-find-index l2 (lambda (x) (== x (car l1)))))
		(r2 (list-tail l2 i))
		(n (common l1 r2)))
	   (if (>= n 25) (values 0 i n) ;; truncate for efficiency reasons
	       (receive (i1 i2 nn) (longest-common l1 (cdr r2))
		 (if (>= n nn)
		     (values 0 i n)
		     (values i1 (+ i2 i 1) nn))))))
	(else
	  (receive (i1 i2 n) (longest-common (cdr l1) l2)
	    (values (+ i1 1) i2 n)))))

(define (skeleton t)
  (if (string? t) 'concat (car t)))

(define (var-longest-common l1 l2)
  (receive (i1 i2 n) (longest-common l1 l2)
    (if (> n 0) (values i1 i2 n)
	(longest-common (map skeleton l1) (map skeleton l2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show old and new versions into a single document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (diff t1 t2)
  ;;(display* "diff " t1 ", " t2 "\n")
  (cond ((and (or (string? t1) (!= (car t1) 'document))
	      (tree-multi-paragraph? (tm->tree t1)))
	 (diff `(document ,t1) t2))
	((and (or (string? t2) (!= (car t2) 'document))
	      (tree-multi-paragraph? (tm->tree t2)))
	 (diff t1 `(document ,t2)))
	(else `(version-both ,t1 ,t2))))

(define (merge-versions-string s1 s2)
  (let* ((l1 (string->list s1))
	 (l2 (string->list s2))
	 (i1 (common l1 l2))
	 (i2 (common (reverse (list-tail l1 i1)) (reverse (list-tail l2 i1))))
	 (n1 (string-length s1))
	 (n2 (string-length s2))
	 (j1 (- n1 i2)) 
	 (j2 (- n2 i2)) 
	 (r1 (substring s1 0 i1))
	 (r2 (diff (substring s1 i1 j1) (substring s2 i1 j2)))
	 (r3 (substring s1 j1 n1)))
    (cond ((and (== i1 0) (== i2 0)) r2)
	  ((== i1 0) `(concat ,r2 ,r3))
	  ((== i2 0) `(concat ,r1 ,r2))
	  (else `(concat ,r1 ,r2 ,r3)))))

(define (merge-versions-list tag l1 l2)
  ;;(display* "merge-versions-list " tag ", " l1 ", " l2 "\n\n")
  (cond ((and (null? l1) (null? l2)) '())
	((null? l1) (list (diff "" `(,tag ,@l2))))
	((null? l2) (list (diff `(,tag ,@l1) "")))
	((== (car l1) (car l2))
	 (cons (car l1) (merge-versions-list tag (cdr l1) (cdr l2))))
	(else
	  (receive (i1 i2 n) (var-longest-common l1 l2)
	    ;;(display* "  break at " i1 ", " i2 ", " n "\n")
	    (cond ((== n 0)
		   (list (diff `(,tag ,@l1) `(,tag ,@l2))))
		  ((and (== n (length l1)) (== n (length l2)))
		   (map merge-versions l1 l2))
		  (else
		    (let* ((ll1 (sublist l1 0 i1))
			   (ll2 (sublist l2 0 i2))
			   (mm1 (sublist l1 i1 (+ i1 n)))
			   (mm2 (sublist l2 i2 (+ i2 n)))
			   (rr1 (sublist l1 (+ i1 n) (length l1)))
			   (rr2 (sublist l2 (+ i2 n) (length l2))))
		      (append (merge-versions-list tag ll1 ll2)
			      (merge-versions-list tag mm1 mm2)
			      (merge-versions-list tag rr1 rr2)))))))))

(tm-define (merge-versions t1 t2)
  ;;(display* "merge-versions " t1 ", " t2 "\n\n")
  (cond ((== t1 t2) t1)
	((and (string? t1) (string? t2))
	 (merge-versions-string t1 t2))
	((and (string? t1) (tm-is? t2 'concat))
	 (merge-versions `(concat ,t1) t2))
	((and (tm-is? t1 'concat) (string? t2))
	 (merge-versions t1 `(concat ,t2)))
	((and (list? t1) (list? t2) (== (car t1) (car t2)))
	 (cond ((in? (car t1) '(document concat))
		(cons (car t1)
		      (merge-versions-list (car t1) (cdr t1) (cdr t2))))
	       ((!= (length t1) (length t2))
		(diff t1 t2))
	       ((in? (car t1) '(raw-data graphics postscript))
		(diff t1 t2))
	       (else
		 (cons (car t1) (map merge-versions (cdr t1) (cdr t2))))))
	(else (diff t1 t2))))

(tm-define (compare-file old)
  (let* ((t1 (tree-load-inclusion old))
	 (t2 (buffer-tree))
	 (u1 (tree->stree t1))
	 (u2 (tree->stree t2))
	 (mv (merge-versions u1 u2))
	 (rt (stree->tree mv)))
    ;;(display* "merged= " (tree-simplify rt) "\n")
    (tree-set (buffer-tree) (tree-simplify rt))
    (version-first-difference)))
