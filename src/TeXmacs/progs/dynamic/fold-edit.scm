
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

(tm-define (toggle-context? t)
  (tree-in? t (toggle-tag-list)))

(tm-define (toggle-first-context? t)
  (tree-in? t (toggle-first-tag-list)))

(tm-define (toggle-second-context? t)
  (tree-in? t (toggle-second-tag-list)))

(tm-define (make-toggle tag)
  (:type (-> void))
  (:synopsis "Insert a 'fold' environment")
  (insert-go-to `(,tag (document "") (document "")) (list 0 0)))

(tm-define (fold)
  (:type (-> void))
  (:synopsis "Fold at the current cursor position")
  (with-innermost t toggle-second-context?
    (tree-assign-node! t (ahash-ref toggle-table (tree-label t)))
    (tree-go-to t 0 :start)))

(tm-define (unfold)
  (:type (-> void))
  (:synopsis "Unfold at the current cursor position")
  (with-innermost t toggle-first-context?
    (tree-assign-node! t (ahash-ref toggle-table (tree-label t)))
    (tree-go-to t 1 :start)))

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
  (:context toggle-first-context?)
  (unfold))

(tm-define (hidden-variant)
  (:context toggle-second-context?)
  (fold))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on switch trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (switch-ref t i)
  (and t (>= i 0) (< i (tree-arity t)) (not (tree-is? t i 'hidden))))

(define (switch-set t i on?)
  (if (== i :last) (set! i (- (tree-arity t) 1)))
  (when (and (>= i 0) (< i (tree-arity t)))
    (cond ((and on? (tree-is? t i 'hidden))
	   (tree-remove-node (tree-ref t i) 0))
	  ((and (not on?) (not (tree-is? t i 'hidden)))
	   (tree-insert-node (tree-ref t i) 0 '(hidden))))))

(define (switch-set-range t first last on?)
  (if (== last :last) (set! last (tree-arity t)))
  (for (i first last) (switch-set t i on?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines on innermost switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (switch-context? t)
  (tree-in? t (switch-tag-list)))

(tm-define (switch-arity)
  (with t (tree-innermost switch-context?)
    (and t (tree-arity t))))

(tm-define (switch-valid-child? i)
  (with t (tree-innermost switch-context?)
    (and t i (>= i 0) (< i (tree-arity t)))))

(tm-define (switch-index . args)
  (:context switch-context?)
  (and-let* ((i (if (null? args) :current (car args)))
	     (t (tree-innermost switch-context?))
	     (c (tree-down-index t))
	     (l (- (tree-arity t) 1))
	     (v l))
    (while (and (>= v 0) (not (switch-ref t v)))
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

(tm-define (switch-to i . args)
  (set! i (switch-index i))
  (if (null? args) (set! args '(:start)))
  (when (switch-valid-child? i)
    (switch-select i)
    (with-innermost t switch-context?
      (apply tree-go-to (cons* t i args)))))

(tm-define (switch-insert-at i)
  (set! i (if (== i :end) (switch-arity) (switch-index i)))
  (with-innermost t switch-context?
    (when (and (>= i 0) (<= i (tree-arity t)))
      (let* ((empty (if (tree-in? t (big-switch-tag-list)) '(document "") ""))
	     (v (switch-index :visible)))
	(tree-insert! t i `(,(tree-label t) ,empty))
	(if (tree-in? t (alternative-tag-list))
	    (switch-select i)
	    (switch-select (+ v 1)))
	(tree-go-to t i :start)))))

(tm-define (switch-remove-at i)
  (set! i (switch-index i))
  (with-innermost t switch-context?
    (when (and (>= i 0) (< i (tree-arity t)) (> (tree-arity t) 1))
      (let* ((v (switch-index :visible))
	     (l (- (tree-arity t) 2)))
	(switch-set-range t (max 0 (- i 1)) (min l (+ i 1)) #t)
	(tree-remove! t i 1)
	(tree-go-to t (min i l) :start)
	(if (tree-in? t (alternative-tag-list))
	    (switch-select (min i l))
	    (switch-select (max 0 (- v 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific types of switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alternative-context? t)
  (tree-in? t (alternative-tag-list)))

(tm-define (switch-select i)
  (:context alternative-context?)
  (with-innermost t alternative-context?
    (switch-set-range t 0 :last #f)
    (switch-set t i #t)))

(define (unroll-context? t)
  (tree-in? t (unroll-tag-list)))

(tm-define (switch-select i)
  (:context unroll-context?)
  (with-innermost t unroll-context?
    (switch-set-range t 0 (+ i 1) #t)
    (switch-set-range t (+ i 1) :last #f)))

(define (expanded-context? t)
  (tree-in? t (expanded-tag-list)))

(tm-define (switch-select i)
  (:context expanded-context?)
  (with-innermost t expanded-context?
    (switch-set-range t 0 :last #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface to switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-switch tag)
  (if (in? tag (big-switch-tag-list))
      (insert-go-to `(,tag (document "")) '(0 0 0))
      (insert-go-to `(,tag "") '(0 0))))

(tm-define (structured-left)
  (:context switch-context?)
  (switch-to :previous :end))

(tm-define (structured-right)
  (:context switch-context?)
  (switch-to :next :start))

(tm-define (structured-up)
  (:context switch-context?)
  (switch-to :previous :end))

(tm-define (structured-down)
  (:context switch-context?)
  (switch-to :next :start))

(tm-define (structured-first)
  (:context switch-context?)
  (switch-to :first :start))

(tm-define (structured-last)
  (:context switch-context?)
  (switch-to :last :end))

(tm-define (structured-top)
  (:context switch-context?)
  (switch-to :first :start))

(tm-define (structured-bottom)
  (:context switch-context?)
  (switch-to :last :end))

(tm-define (structured-insert forwards?)
  (:context switch-context?)
  (switch-insert-at (if forwards? :var-next :current)))

(tm-define (structured-insert-up)
  (:context switch-context?)
  (switch-insert-at :current))

(tm-define (structured-insert-down)
  (:context switch-context?)
  (switch-insert-at :var-next))

(tm-define (structured-remove forwards?)
  (:context switch-context?)
  (with-innermost t switch-context?
    (with i (if forwards? :current :var-previous)
      (set! i (switch-index i))
      (cond ((< i 0) (tree-go-to t :start))
	    ((and forwards? (= i (- (tree-arity t) 1))) (tree-go-to t :end))
	    (else (switch-remove-at i))))))

(tm-define (hidden-variant)
  (:context switch-context?)
  (switch-to :rotate-forward))

(tm-define (variant-circulate forward?)
  (:context switch-context?)
  (with-innermost t switch-context?
    (let* ((old (tree-label t))
	   (val (big-switch-tag-list))
	   (rot (list-search-rotate val old))
	   (new (if (and forward? (nnull? rot)) (cadr rot) (cAr rot)))
	   (i (switch-index)))
      (variant-replace old new)
      (switch-select i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global routines for folding/unfolding/compressing/expanding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-operate t mode)
  (force-output)
  (when (tree-compound? t)
    (for-each (lambda (x) (dynamic-operate x mode)) (tree-children t))
    (cond ((toggle-first-context? t)
	   (cond ((in? mode '(:unfold :expand :last))
		  (tree-assign-node!
		   t (ahash-ref toggle-table (tree-label t))))))
	  ((toggle-second-context? t)
	   (cond ((in? mode '(:fold :compress :first))
		  (tree-assign-node!
		   t (ahash-ref toggle-table (tree-label t))))))
	  ((alternative-context? t)
	   (cond ((in? mode '(:expand))
		  (tree-assign-node! t 'expanded)
		  (switch-set-range t 0 :last #t))
		 ((in? mode '(:first))
		  (switch-set-range t 1 :last #f)
		  (switch-set t 0 #t))
		 ((in? mode '(:last))
		  (switch-set-range t 0 :last #f)
		  (switch-set t :last #t))))
	  ((unroll-context? t)
	   (cond ((in? mode '(:expand :last))
		  (switch-set-range t 0 :last #t))
		 ((in? mode '(:compress :first))
		  (switch-set-range t 1 :last #f))))
	  ((expanded-context? t)
	   (cond ((in? mode '(:compress))
		  (tree-assign-node! t 'switch)
		  (switch-set t 0 #t)
		  (switch-set-range t 1 :last #f)))))))

(tm-define (dynamic-operate-on-buffer mode)
  (dynamic-operate (buffer-tree) mode))
