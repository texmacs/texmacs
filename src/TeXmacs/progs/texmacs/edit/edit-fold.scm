
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold.scm
;; DESCRIPTION : routines for switching, folding and layers
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-fold)
  (:export
    make-fold fold unfold mouse-fold mouse-unfold
    make-switch switch-get-position switch-get-last
    switch-insert switch-remove switch-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-fold)
  (insert-tree-go-to
   (object->tree (tmp-compound-object "fold" '(document "") '(document "")))
   (list (tmp-d-hide-exp) 0)))

(define (fold)
  (let ((p (search-upwards "unfold")))
    (if (not (null? p))
	(let ((t (tm-subtree p)))
	  (tm-assign p (tree3 'hide_expand (string->tree "fold")
			      (tree-ref t (tmp-d-exp))
			      (tree-ref t (+ (tmp-d-exp) 1))))
	  (tm-go-to (tm-start (rcons p (tmp-d-hide-exp))))))))

(define (unfold)
  (let ((p (search-upwards "fold")))
    (if (not (null? p))
	(let ((t (tm-subtree p)))
	  (if (= (tmp-d-exp) 1)
	      (tm-assign p (tree3 'expand (string->tree "unfold")
				  (tree-ref t (tmp-d-hide-exp))
				  (tree-ref t (+ (tmp-d-hide-exp) 1))))
	      (tm-assign p (tree2 'unfold
				  (tree-ref t (tmp-d-hide-exp))
				  (tree-ref t (+ (tmp-d-hide-exp) 1)))))
	  (tm-go-to (tm-start (rcons p (+ (tmp-d-exp) 1))))))))

(tm-define (mouse-fold)
  (:type (void -> void))
  (:synopsis "fold using the mouse")
  (:secure #t)
  (if (has-action-path?)
      (begin
	(tm-go-to-start (get-action-path))
	(fold))))

(tm-define (mouse-unfold)
  (:type (void -> void))
  (:synopsis "unfold using the mouse")
  (:secure #t)
  (if (has-action-path?)
      (begin
	(tm-go-to-start (get-action-path))
	(unfold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-switch)
  (insert-tree-go-to
   (object->tree (tmp-compound-object "switch"
				      '(document "") '(tuple (tmarker))))
   (list (tmp-d-hide-exp) 0)))

(define (switch-find-marker t i)
  (cond ((= i (tree-arity t)) -1)
	((== (tree-ref t i) (object->tree '(tmarker))) i)
	(else (switch-find-marker t (+ i 1)))))

(define (switch-get-position)
  (let ((p (search-upwards "switch")))
    (if (null? p) -1
	(switch-find-marker (tm-subtree (rcons p (+ (tmp-d-hide-exp) 1))) 0))))

(define (switch-get-last)
  (let ((p (search-upwards "switch")))
    (if (null? p) -1
	(- (tree-arity (tm-subtree (rcons p (+ (tmp-d-hide-exp) 1)))) 1))))

(define (switch-unselect)
  (let ((p (search-upwards "switch"))
	(i (switch-get-position)))
    (if (>= i 0)
	(let ((t (tm-subtree (rcons p (tmp-d-hide-exp)))))
	  (tm-assign (rcons p (tmp-d-hide-exp)) (string->tree ""))
	  (tm-assign (rcons* p (+ (tmp-d-hide-exp) 1) i) t)))))

(define (switch-select i)
  (let ((p (search-upwards "switch")))
    (if (not (null? p))
	(let ((t (tm-subtree (rcons* p (+ (tmp-d-hide-exp) 1) i))))
	  (tm-assign (rcons* p (+ (tmp-d-hide-exp) 1) i)
		     (object->tree '(tmarker)))
	  (tm-assign (rcons p (tmp-d-hide-exp)) t)))))

(define (switch-pos where pos last)
  (cond ((== where "before") pos)
	((== where "after") (+ pos 1))
	((== where "here") pos)
	((== where "previous") (max 0 (- pos 1)))
	((== where "next") (min last (+ pos 1)))
	((== where "rotate backward") (switch-to "rotated previous"))
	((== where "rotate forward") (switch-to "rotated next"))
	((== where "first") 0)
	((== where "last") last)))

(define (switch-insert where)
  (let ((p (search-upwards "switch"))
	(pos  (switch-get-position))
	(last (switch-get-last)))
    (cond ((= pos -1) (noop))
	  ((string? where) (switch-insert (switch-pos where pos last)))
	  (else (switch-unselect)
		(tm-insert (rcons* p (+ (tmp-d-hide-exp) 1) where)
			   (object->tree '(tuple (document ""))))
		(switch-select where)))))

(define (switch-remove where)
  (let ((p (search-upwards "switch"))
	(pos  (switch-get-position))
	(last (switch-get-last)))
    (cond ((= pos -1) (noop))
	  ((= last 0) (tm-assign p (string->tree "")) (tm-correct (cDr p)))
	  ((string? where) (switch-remove (switch-pos where pos last)))
	  (else (switch-unselect)
		(tm-remove (rcons* p (+ (tmp-d-hide-exp) 1) where) 1)
		(switch-select (min where (- last 1)))))))

(define (switch-to where)
  (let ((pos  (switch-get-position))
	(last (switch-get-last)))
    (cond ((= pos -1) (noop))
	  ((string? where) (switch-to (switch-pos where pos last)))
	  (else (switch-unselect)
		(switch-select where)))))
