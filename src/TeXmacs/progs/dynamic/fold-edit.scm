
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

(texmacs-module (dynamic fold-edit)
  (:use (utils library tree)
	(dynamic dynamic-drd)))

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

(tm-define (hidden-variant)
  (:inside fold)
  (unfold))

(tm-define (hidden-variant)
  (:inside unfold)
  (fold))

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
	((== where "rotate backward") (if (= pos 0) last (- pos 1)))
	((== where "rotate forward") (if (= pos last) 0 (+ pos 1)))
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

(tm-define (hidden-variant)
  (:inside switch)
  (switch-to "rotate forward"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New type switches (common library)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-switch-context? t)
  (tree-in? t (switch-tag-list)))

(tm-define (new-switch-arity)
  (with t (tree-innermost new-switch-context?)
    (and t (tree-arity t))))

(tm-define (new-switch-valid-child? i)
  (with t (tree-innermost new-switch-context?)
    (and t i (>= i 0) (< i (tree-arity t)))))

(tm-define (new-switch-ref i)
  (:context new-switch-context?)
  (with t (tree-innermost new-switch-context?)
    (and t (>= i 0) (< i (tree-arity t)) (not (tree-is? t i 'hidden)))))

(tm-define (new-switch-set i on?)
  (:context new-switch-context?)
  (with-innermost t new-switch-context?
    (when (and (>= i 0) (< i (tree-arity t)))
      (cond ((and on? (tree-is? t i 'hidden))
	     (tree-remove-node (tree-ref t i) 0))
	    ((and (not on?) (not (tree-is? t i 'hidden)))
	     (tree-insert-node (tree-ref t i) 0 '(hidden)))))))

(tm-define (new-switch-set-range first last on?)
  (:context new-switch-context?)
  (if (== last :last) (set! last (new-switch-arity)))
  (for (i first last) (new-switch-set i on?)))

(tm-define (new-switch-index . args)
  (:context new-switch-context?)
  (and-let* ((i (if (null? args) :current (car args)))
	     (t (tree-innermost new-switch-context?))
	     (c (tree-down-index t))
	     (l (- (tree-arity t) 1))
	     (v l))
    (while (and (>= v 0) (not (new-switch-ref v)))
      (set! v (- v 1)))
    (cond ((< v 0) #f)
	  ((== i :visible) v)
	  ((== i :current) c)
	  ((== i :previous) (max 0 (- c 1)))
	  ((== i :next) (min l (+ c 1)))
	  ((== i :var-previous) (- c 1))
	  ((== i :var-next) (+ c 1))
	  ((== i :rotate-backward) (if (= c 0) l (- c 1)))
	  ((== i :rotate-forward) (if (= c l) 0 (+ c 1)))
	  ((== i :first) 0)
	  ((== i :last) l)
	  (else i))))

(tm-define (new-switch-to i . args)
  (set! i (new-switch-index i))
  (if (null? args) (set! args '(:start)))
  (when (new-switch-valid-child? i)
    (new-switch-select i)
    (with-innermost t new-switch-context?
      (apply tree-go-to (cons* t i args)))))

(tm-define (new-switch-insert-at i)
  (set! i (if (== i :end) (new-switch-arity) (new-switch-index i)))
  (with-innermost t new-switch-context?
    (when (and (>= i 0) (<= i (tree-arity t)))
      (let* ((empty (if (tree-in? t (big-switch-tag-list)) '(document "") ""))
	     (v (new-switch-index :visible)))
	(tree-insert! t i `(,(tree-label t) ,empty))
	(if (tree-in? t (alternative-tag-list))
	    (new-switch-select i)
	    (new-switch-select (+ v 1)))
	(tree-go-to t i :start)))))

(tm-define (new-switch-remove-at i)
  (set! i (new-switch-index i))
  (with-innermost t new-switch-context?
    (when (and (>= i 0) (< i (tree-arity t)) (> (tree-arity t) 1))
      (new-switch-set (+ 1 i) #t)
      (tree-remove! t i 1)
      (tree-go-to t i :start)
      (new-switch-to i :start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New type switches (specific types of switches)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alternative-context? t)
  (tree-in? t (alternative-tag-list)))

(tm-define (new-switch-select i)
  (:context alternative-context?)
  (new-switch-set-range 0 :last #f)
  (new-switch-set i #t))

(define (unroll-context? t)
  (tree-in? t (unroll-tag-list)))

(tm-define (new-switch-select i)
  (:context unroll-context?)
  (new-switch-set-range 0 (+ i 1) #t)
  (new-switch-set-range (+ i 1) :last #f))

(define (expanded-context? t)
  (tree-in? t (expanded-tag-list)))

(tm-define (new-switch-select i)
  (:context expanded-context?)
  (new-switch-set-range 0 :last #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface to switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-left)
  (:context new-switch-context?)
  (new-switch-to :previous))

(tm-define (structured-right)
  (:context new-switch-context?)
  (new-switch-to :next))

(tm-define (structured-up)
  (:context new-switch-context?)
  (new-switch-to :previous))

(tm-define (structured-down)
  (:context new-switch-context?)
  (new-switch-to :next))

(tm-define (structured-first)
  (:context new-switch-context?)
  (new-switch-to :first :start))

(tm-define (structured-last)
  (:context new-switch-context?)
  (new-switch-to :last :end))

(tm-define (structured-top)
  (:context new-switch-context?)
  (new-switch-to :first :start))

(tm-define (structured-bottom)
  (:context new-switch-context?)
  (new-switch-to :last :end))

(tm-define (structured-insert forwards?)
  (:context new-switch-context?)
  (new-switch-insert-at (if forwards? :var-next :current)))

(tm-define (structured-insert-up)
  (:context new-switch-context?)
  (new-switch-insert-at :current))

(tm-define (structured-insert-down)
  (:context new-switch-context?)
  (new-switch-insert-at :var-next))

(tm-define (structured-remove forwards?)
  (:context new-switch-context?)
  (with-innermost t new-switch-context?
    (with i (if forwards? :current :var-previous)
      (set! i (new-switch-index i))
      (cond ((< i 0) (tree-go-to t :start))
	    ((and forwards? (= i (- (tree-arity t) 1))) (tree-go-to t :end))
	    (else (new-switch-remove-at i))))))

(tm-define (hidden-variant)
  (:context new-switch-context?)
  (new-switch-to :rotate-forward))

(tm-define (variant-circulate forward?)
  (:context new-switch-context?)
  (with-innermost t new-switch-context?
    (let* ((old (tree-label t))
	   (val (switch-tag-list))
	   (rot (list-search-rotate val old))
	   (new (if (and forward? (nnull? rot)) (cadr rot) (cAr rot)))
	   (i (new-switch-index)))
      (variant-replace old new)
      (new-switch-select i))))
