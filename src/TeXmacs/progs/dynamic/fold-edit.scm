
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-edit.scm
;; DESCRIPTION : routines for switching, folding and layers
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fold-unfold-types
  '((fold . unfold)
    (fold-text . unfold-text)
    (fold-proof . unfold-proof)
    (fold-algorithm . unfold-algorithm)
    (fold-exercise . unfold-exercise)))

(define unfold-fold-types
  (map (lambda (x) (cons (cdr x) (car x))) fold-unfold-types))

(tm-define (make-fold)
  (:type (-> void))
  (:synopsis "Insert a 'fold' environment")
  (insert-go-to '(fold (document "") (document "")) (list 0 0)))

(define (fold-unfold l to)
  (with-innermost t (map car l)
    (tree-assign-node! t (assoc-ref l (tm-car t)))
    (tree-go-to t to :start)))

(tm-define (fold)
  (:type (-> void))
  (:synopsis "Fold at the current cursor position")
  (fold-unfold unfold-fold-types 0))

(tm-define (unfold)
  (:type (-> void))
  (:synopsis "Unfold at the current cursor position")
  (fold-unfold fold-unfold-types 1))

(tm-define (mouse-fold)
  (:type (-> void))
  (:synopsis "Fold using the mouse")
  (:secure #t)
  (with-action t
    (tree-go-to t :start)
    (fold)))

(tm-define (mouse-unfold)
  (:type (-> void))
  (:synopsis "Unfold using the mouse")
  (:secure #t)
  (with-action t
    (tree-go-to t :start)
    (unfold)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-switch)
  (:type (-> void))
  (:synopsis "Insert a 'switch' environment")
  (insert-go-to '(switch (document "") (tuple (tmarker))) (list 0 0)))

(define (switch-find-marker t i)
  (cond ((= i (tree-arity t)) #f)
	((tm-equal? (tree-ref t i) '(tmarker)) i)
	(else (switch-find-marker t (+ i 1)))))

(tm-define (switch-get-position)
  (:type (-> int))
  (:synopsis "Get current position inside a 'switch' environment")
  (with t (tree-innermost 'switch)
    (and t (switch-find-marker (tree-ref t 1) 0))))

(tm-define (switch-get-last)
  (:type (-> int))
  (:synopsis "Get last node of the current 'switch' environment")
  (with t (tree-innermost 'switch)
    (and t (- (tree-arity (tree-ref t 1)) 1))))

(define (switch-unselect)
  (with-innermost t 'switch
    (let* ((i (switch-get-position))
	   (st (tree-copy (tree-ref t 0))))
      (tree-set t 0 "")
      (tree-set t 1 i st))))

(define (switch-select i)
  (with-innermost t 'switch
    (with st (tree-copy (tree-ref t 1 i))
      (tree-set t 1 i '(tmarker))
      (tree-set t 0 st))))

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

(tm-define (switch-insert where)
  (:type (-> int void))
  (:synopsis "Add a new branch to the current switch environment at @where")
  (with-innermost t 'switch
    (let ((pos  (switch-get-position))
	  (last (switch-get-last)))
      (if (string? where)
	  (switch-insert (switch-pos where pos last))
	  (begin
	    (switch-unselect)
	    (tree-insert (tree-ref t 1) where '(tuple (document "")))
	    (switch-select where))))))

(tm-define (structured-insert forwards?)
  (:inside switch)
  (switch-insert (if forwards? "after" "before")))

(tm-define (switch-remove where)
  (:type (-> int void))
  (:synopsis "Remove a branch from the current switch environment at @where")
  (with-innermost t 'switch
    (let ((pos  (switch-get-position))
	  (last (switch-get-last)))
      (cond ((= last 0) (tree-set! t "") (tree-correct (tree-up t)))
	    ((string? where) (switch-remove (switch-pos where pos last)))
	    (else (switch-unselect)
		  (tree-remove (tree-ref t 1) where 1)
		  (switch-select (min where (- last 1))))))))

(tm-define (structured-remove forwards?)
  (:inside switch)
  (switch-remove "here"))

(tm-define (switch-to where)
  (:type (-> int void))
  (:synopsis "Switch to branch @where of the current switch environment")
  (let ((pos  (switch-get-position))
	(last (switch-get-last)))
    (cond ((not pos) (noop))
	  ((string? where) (switch-to (switch-pos where pos last)))
	  (else (switch-unselect)
		(switch-select where)))))
