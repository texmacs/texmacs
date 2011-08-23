
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-edit.scm
;; DESCRIPTION : routines for switching, folding and layers
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-edit)
  (:use (utils library tree)
	(utils library cursor)
	(dynamic dynamic-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic movements for fold tags and switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (dynamic-context? t)
  (or (toggle-tag? (tree-label t))
      (switch-tag? (tree-label t))))

(tm-define (dynamic-extremal t forwards?)
  (and-with p (tree-outer t)
    (dynamic-extremal p forwards?)))

(tm-define (dynamic-incremental t forwards?)
  (and-with p (tree-outer t)
    (dynamic-incremental p forwards?)))

(tm-define (dynamic-first)
  (dynamic-extremal (focus-tree) #f))
(tm-define (dynamic-last)
  (dynamic-extremal (focus-tree) #t))
(tm-define (dynamic-previous)
  (dynamic-incremental (focus-tree) #f))
(tm-define (dynamic-next)
  (dynamic-incremental (focus-tree) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract stuff for fold tags and switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-horizontal t forwards?)
  (:require (dynamic-context? t))
  (dynamic-incremental t forwards?))

(tm-define (structured-vertical t downwards?)
  (:require (dynamic-context? t))
  (dynamic-incremental t downwards?))

(tm-define (structured-extremal t forwards?)
  (:require (dynamic-context? t))
  (dynamic-extremal t forwards?))

(tm-define (structured-incremental t downwards?)
  (:require (dynamic-context? t))
  (dynamic-extremal t downwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-context? t)
  (toggle-tag? (tree-label t)))

(tm-define (toggle-first-context? t)
  (toggle-first-tag? (tree-label t)))

(tm-define (toggle-second-context? t)
  (toggle-second-tag? (tree-label t)))

(tm-define (fold-context? t)
  (or (folded-tag? (tree-label t)) (unfolded-tag? (tree-label t))))

(tm-define (make-toggle tag)
  (:type (-> void))
  (:synopsis "Insert a 'fold' environment")
  (insert-go-to `(,tag (document "") (document "")) (list 0 0)))

(tm-define (alternate-toggle t)
  (:require (toggle-context? t))
  (with i (if (toggle-first-context? t) 1 0)
    (variant-set t (ahash-ref alternate-table (tree-label t)))
    (tree-go-to t i :start)))

(tm-define (dynamic-extremal t forwards?)
  (:require (toggle-context? t))
  (with action (if forwards? alternate-unfold alternate-fold)
    (action t)))

(tm-define (dynamic-incremental t forwards?)
  (:require (toggle-context? t))
  (with action (if forwards? alternate-unfold alternate-fold)
    (action t)))

(tm-define (tree-show-hidden t)
  (:require (toggle-context? t))
  (alternate-toggle t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on switch trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (switch-ref t i)
  (and t (>= i 0) (< i (tree-arity t)) (not (tree-is? t i 'hidden))))

(define (switch-set t i on?)
  (if (== i :last) (set! i (- (tree-arity t) 1)))
  (when (and (>= i 0) (< i (tree-arity t)))
    (cond ((and on? (tree-is? t i 'hidden))
	   (tree-assign-node (tree-ref t i) 'shown))
	  ((and (not on?) (tree-is? t i 'shown))
	   (tree-assign-node (tree-ref t i) 'hidden))
	  ((and on? (not (tree-is? t i 'shown)))
	   (tree-insert-node (tree-ref t i) 0 '(shown)))
	  ((and (not on?) (not (tree-is? t i 'hidden)))
	   (tree-insert-node (tree-ref t i) 0 '(hidden))))))

(define (switch-set-range t first last on?)
  (if (== last :last) (set! last (tree-arity t)))
  (for (i first last) (switch-set t i on?)))

(define (switch-last-visible t)
  (with v (- (tree-arity t) 1)
    (while (and (>= v 0) (not (switch-ref t v)))
      (set! v (- v 1)))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines on switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (switch-context? t)
  (switch-tag? (tree-label t)))

(tm-define (switch-valid-child? t i)
  (and t i (>= i 0) (< i (tree-arity t))))

(tm-define (switch-index t . args)
  (if (not (switch-context? t)) -1
      (and-let* ((i (if (null? args) :current (car args)))
                 (c (tree-down-index t))
                 (l (- (tree-arity t) 1))
                 (v (switch-last-visible t)))
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
              (else i)))))

(tm-define (switch-to t i . args)
  (set! i (switch-index t i))
  (if (null? args) (set! args '(:start)))
  (when (switch-valid-child? t i)
    (switch-select t i)
    (apply tree-go-to (cons* t i 0 args))))

(tm-define (switch-insert-at t i)
  (set! i (if (== i :end) (tree-arity t) (switch-index t i)))
  (when (and (>= i 0) (<= i (tree-arity t)))
    (let* ((empty (if (tree-in? t (big-switch-tag-list)) '(document "") ""))
           (v (switch-index t :visible)))
      (tree-insert! t i `((shown ,empty)))
      (if (tree-in? t (alternative-tag-list))
          (switch-select t i)
          (switch-select t (+ v 1)))
      (tree-go-to t i :start))))

(tm-define (switch-remove-at t i)
  (set! i (switch-index t i))
  (when (and (>= i 0) (< i (tree-arity t)) (> (tree-arity t) 1))
    (let* ((v (switch-index t :visible))
           (l (- (tree-arity t) 2)))
      (switch-set-range t (max 0 (- i 1)) (min l (+ i 1)) #t)
      (tree-remove! t i 1)
      (tree-go-to t (min i l) :start)
      (if (tree-in? t (alternative-tag-list))
          (switch-select t (min i l))
          (switch-select t (max 0 (- v 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific types of switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alternative-context? t)
  (alternative-tag? (tree-label t)))

(tm-define (switch-select t i)
  (:require (alternative-context? t))
  (switch-set-range t 0 :last #f)
  (switch-set t i #t))

(define (unroll-context? t)
  (unroll-tag? (tree-label t)))

(tm-define (switch-select t i)
  (:require (unroll-context? t))
  (switch-set-range t 0 (+ i 1) #t)
  (switch-set-range t (+ i 1) :last #f))

(define (expanded-context? t)
  (expanded-tag? (tree-label t)))

(tm-define (switch-select t i)
  (:require (expanded-context? t))
  (switch-set-range t 0 :last #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface to switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-switch tag)
  (if (in? tag (big-switch-tag-list))
      (insert-go-to `(,tag (shown (document ""))) '(0 0 0 0))
      (insert-go-to `(,tag (shown "")) '(0 0 0))))

(tm-define (dynamic-extremal t forwards?)
  (:require (switch-context? t))
  (if forwards?
      (switch-to t :last :end)
      (switch-to t :first :start)))

(tm-define (dynamic-incremental t forwards?)
  (:require (switch-context? t))
  (if forwards?
      (switch-to t :next :start)
      (switch-to t :previous :end)))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (switch-context? t))
  (switch-insert-at t (if forwards? :var-next :current)))

(tm-define (structured-insert-vertical t downwards?)
  (:require (switch-context? t))
  (structured-insert-horizontal t downwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (switch-context? t))
  (with i (if forwards? :current :var-previous)
    (set! i (switch-index t i))
    (cond ((< i 0) (tree-go-to t :start))
          ((and forwards? (= i (- (tree-arity t) 1))) (tree-go-to t :end))
          (else (switch-remove-at t i)))))

(tm-define (structured-remove-vertical t downwards?)
  (:require (switch-context? t))
  (structured-remove-horizontal t downwards?))

(tm-define (alternate-toggle t)
  (:require (switch-context? t))
  (switch-to t :rotate-forward))

(tm-define (variant-circulate t forward?)
  (:require (switch-context? t))
  (with i (switch-index t)
    (variant-circulate-in t (big-switch-tag-list) forward?)
    (switch-select t i)))

(tm-define (tree-show-hidden t)
  (:require (switch-context? t))
  (with i (tree-down-index t)
    (if (tree-is? (tree-ref t i) 'hidden)
	(switch-select t i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analyzing the environments occurring in folds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fold-environments (make-ahash-table))
(define fold-environments-first (make-ahash-table))
(define fold-environments-second (make-ahash-table))

(define (fold-add-environment t first?)
  (cond ((and (toggle-first-context? t)
	      (not (tree-is? t 'summarized-algorithm)))
	 (fold-add-environment (tree-ref t 1) #t))
	((and (toggle-second-context? t)
	      (not (tree-is? t 'detailed-algorithm)))
	 (fold-add-environment (tree-ref t 1) #f))
	((tm-func? t 'document 1)
	 (fold-add-environment (tree-ref t 0) first?))
	(else
	 (with tag 'text
	   (if (tree-compound? t) (set! tag (tree-label t)))
	   (if (== tag 'concat) (set! tag 'text))
	   (if (== tag 'document) (set! tag 'text))
	   (if (== tag 'render-proof) (set! tag 'proof))
	   (if (in? tag '(summarized-algorithm detailed-algorithm))
	       (set! tag 'algorithm))
	   (ahash-set! fold-environments tag #t)
	   (ahash-set! (if first?
			   fold-environments-first
			   fold-environments-second)
		       tag #t)))))

(define (fold-get-environments-sub t)
  (if (tree-compound? t)
      (for-each fold-get-environments-sub (tree-children t)))
  (if (toggle-context? t)
      (fold-add-environment t (toggle-first-context? t))))

(tm-define (fold-get-environments-in-buffer)
  (set! fold-environments (make-ahash-table))
  (set! fold-environments-first (make-ahash-table))
  (set! fold-environments-second (make-ahash-table))
  (fold-get-environments-sub (buffer-tree))
  (with envl (map car (ahash-table->list fold-environments))
    (values
      (list-sort (map symbol->string envl) string<=?)
      fold-environments-first
      fold-environments-second)))

(define (fold-matching-env? t tag)
  (cond ((tree-in? t '(summarized-algorithm detailed-algorithm))
	 (== tag 'algorithm))
	((toggle-first-context? t)
	 (fold-matching-env? (tree-ref t 1) tag))
	((toggle-second-context? t)
	 (fold-matching-env? (tree-ref t 1) tag))
	((tm-func? t 'document 1)
	 (fold-matching-env? (tree-ref t 0) tag))
	((or (tree-atomic? t) (tree-in? t '(document concat)))
	 (== tag 'text))
	((== tag 'proof)
	 (tree-in? t '(proof render-proof)))
	(else (tree-is? t tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global routines for folding/unfolding/compressing/expanding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-operate-sub t mode)
  (if (tree-compound? t)
      (for-each (lambda (x) (dynamic-operate x mode)) (tree-children t)))
  (cond ((toggle-first-context? t)
	 (cond ((== mode :var-last)
		(tree-insert-node! t 0 '(traversed)))
	       ((in? mode '(:unfold :expand :var-expand :last))
		(alternate-toggle t))
	       ((and (pair? mode) (== (car mode) :unfold)
		     (fold-matching-env? t (cadr mode)))
		(alternate-toggle t))))
	((toggle-second-context? t)
	 (cond ((== mode :var-last)
		(alternate-toggle t)
		(tree-insert-node! t 0 '(traversed)))
	       ((in? mode '(:fold :compress :var-compress :first))
		(alternate-toggle t))
	       ((and (pair? mode) (== (car mode) :fold)
		     (fold-matching-env? t (cadr mode)))
		(alternate-toggle t))))
	((and (== mode :expand) (switch-context? t))
	 (switch-set-range t 0 :last #t))
	((and (== mode :compress) (switch-context? t))
	 (switch-set t 0 #t)
	 (switch-set-range t 1 :last #f))
	((alternative-context? t)
	 (cond ((== mode :first)
		(switch-set-range t 1 :last #f)
		(switch-set t 0 #t))
	       ((== mode :last)
		(switch-set-range t 0 :last #f)
		(switch-set t :last #t))
	       ((== mode :var-expand)
		(tree-assign-node! t 'expanded)
		(switch-set-range t 0 :last #t))))
	((unroll-context? t)
	 (cond ((== mode :var-last)
		(switch-set-range t 1 :last #f)
		(switch-set t 0 #t)
		(tree-insert-node t 0 '(traversed)))
	       ((== mode :first)
		(switch-set-range t 1 :last #f)
		(switch-set t 0 #t))
	       ((== mode :last)
		(switch-set-range t 0 :last #t))
	       ((== mode :var-expand)
		(switch-set-range t 0 :last #t))
	       ((== mode :var-compress)
		(switch-set t 0 #t)
		(switch-set-range t 1 :last #f))))
	((expanded-context? t)
	 (cond ((== mode :var-compress)
		(tree-assign-node! t 'switch)
		(switch-set t 0 #t)
		(switch-set-range t 1 :last #f))))))

(define (dynamic-operate t mode)
  (when (tree-compound? t)
    (cond ((tree-is? t 'traversed)
	   (when (!= mode :var-last)
	     (dynamic-operate (tree-ref t 0) mode)
	     (if (in? mode '(:unfold :expand :var-expand :first))
		 (tree-remove-node! t 0))))
	  ((tree-is? t 'fold-back)
	   (if (== mode :last) (set! mode :var-last))
	   (dynamic-operate (tree-ref t 0) mode))
	  ((tree-is? t 'keep-folded)
	   (if (== mode :var-last) (set! mode :last))
	   (dynamic-operate (tree-ref t 0) mode))
	  (else (dynamic-operate-sub t mode)))))

(tm-define (dynamic-operate-on-buffer mode)
  (dynamic-operate (buffer-tree) mode)
  (if (in? mode '(:first :var-first)) (tree-go-to (buffer-tree) :start))
  (if (in? mode '(:last :var-last)) (tree-go-to (buffer-tree) :end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform presentation into slides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-make-slide t)
  (when (or (tm-func? t 'shown 1) (tm-func? t 'hidden 1))
    (tree-assign-node! t 'slide))
  (dynamic-operate (tree-ref t 0) :var-expand))

(tm-define (dynamic-make-slides)
  (init-add-package "slides")
  (init-default "page-medium" "page-type" "page-width" "page-height"
		"page-width-margin" "page-height-margin"
		"page-odd" "page-even" "page-right"
		"par-width" "page-odd-shift" "page-even-shift"
		"page-top" "page-bot" "page-height-margin")
  (with t (buffer-tree)
    (when (and (tm-func? t 'document 1) (switch-context? (tree-ref t 0)))
      (tree-assign-node (tree-ref t 0) 'document)
      (tree-set! t (tree-ref t 0))
      (for-each dynamic-make-slide (tree-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global navigation in recursive fold/switch structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-traverse-folded t mode)
  (cond ((in? mode '(:next :var-next))
	 (alternate-toggle t)
	 (dynamic-operate (tree-ref t 1) :first)
	 (tree-go-to t 1 :end)
	 #t)
	(else #f)))

(define (dynamic-traverse-unfolded t mode)
  (cond ((== mode :var-next)
	 (alternate-toggle t)
	 (tree-insert-node! t 0 '(traversed))
	 #t)
	((in? mode '(:previous :var-previous))
	 (with last-mode (if (== mode :previous) :last :var-last)
	   (alternate-toggle t)
	   (dynamic-operate (tree-ref t 0) last-mode)
	   (tree-go-to t 0 :end)
	   #t))
	(else #f)))

(define (dynamic-traverse-switch t i l mode)
  (cond ((and (== mode :var-next) (= i l) (unroll-context? t))
	 (dynamic-operate t :first)
	 (tree-insert-node! t 0 '(traversed))
	 #t)
	((and (in? mode '(:next :var-next)) (< i l))
	 (dynamic-operate (tree-ref t (1+ i)) :first)
	 (tree-go-to t i :end)
	 (switch-to t :next)
	 #t)
	((and (in? mode '(:previous :var-previous)) (> i 0))
	 (with last-mode (if (== mode :previous) :last :var-last)
	   (dynamic-operate (tree-ref t (- i 1)) last-mode)
	   (tree-go-to t i :start)
	   (switch-to t :previous)
	   #t))
	(else #f)))

(define (dynamic-traverse-traversed t mode)
  (and (in? mode '(:previous :var-previous))
       (begin
	 (tree-remove-node! t 0)
	 (dynamic-operate t :last)
	 (if (and (== mode :var-previous) (tree-compound? t))
	     (for-each (lambda (x) (dynamic-operate x :var-last))
		       (tree-accessible-children t)))
	 #t)))

(define (dynamic-traverse-list l mode)
  (and (nnull? l)
       (or (dynamic-traverse (car l) mode)
	   (dynamic-traverse-list (cdr l) mode))))

(define (dynamic-traverse t mode)
  (cond ((tree-atomic? t) #f)
	((tree-is? t 'traversed)
	 (dynamic-traverse-traversed t mode))
	((tree-is? t 'fold-back)
	 (if (== mode :next) (set! mode :var-next))
	 (if (== mode :previous) (set! mode :var-previous))
	 (dynamic-traverse (tree-ref t 0) mode))
	((tree-is? t 'keep-folded)
	 (if (== mode :var-next) (set! mode :next))
	 (if (== mode :var-previous) (set! mode :previous))
	 (dynamic-traverse (tree-ref t 0) mode))
	((toggle-first-context? t)
	 (or (dynamic-traverse (tree-ref t 0) mode)
	     (dynamic-traverse-folded t mode)))
	((toggle-second-context? t)
	 (or (dynamic-traverse (tree-ref t 1) mode)
	     (dynamic-traverse-unfolded t mode)))
	((or (alternative-context? t) (unroll-context? t))
	 (let* ((i (switch-last-visible t))
		(l (- (tree-arity t) 1)))
	   (or (dynamic-traverse (tree-ref t i) mode)
	       (dynamic-traverse-switch t i l mode))))
	(else
	 (let* ((c (tree-accessible-children t))
		(forward? (in? mode '(:next :var-next)))
		(l (if forward? c (reverse c))))
	   (dynamic-traverse-list l mode)))))

(tm-define (dynamic-traverse-buffer mode)
  (dynamic-traverse (buffer-tree) mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific navigation for 'screens' switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (screens-switch-to which)
  (and-with t (tree-innermost 'screens)
    (switch-to t which :start)))
