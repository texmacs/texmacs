
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-group.scm
;; DESCRIPTION : editing routines for graphics group mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004-2007  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-group)
  (:use (graphics graphics-env)
        (graphics graphics-single)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; State
(define group-old-x #f)
(define group-old-y #f)
(define group-first-x #f)
(define group-first-y #f)
(define group-bary-x #f)
(define group-bary-y #f)
(define group-first-go #f)

(define (store-important-points)
  (define so-points '())
  (define (store-points)
    (lambda (o)
       (if (match? o '(point :%2))
	   (set! so-points (cons o so-points))
       )
       o)
  )
  (set! group-bary-x #f)
  (set! group-bary-y #f)
  (if (nnull? (sketch-get))
  (with so (map tree->stree (sketch-get))
     (traverse-transform so (store-points))
     (if (nnull? so-points)
     (with n 0
	(set! group-bary-x 0)
	(set! group-bary-y 0)
	(for (p so-points)
          (set! group-bary-x (+ group-bary-x (s2f (cadr p))))
          (set! group-bary-y (+ group-bary-y (s2f (caddr p))))
          (set! n (+ n 1)))
	(set! group-bary-x (/ group-bary-x n))
	(set! group-bary-y (/ group-bary-y n))))
  ))
  (set! group-first-x (s2f current-x))
  (set! group-first-y (s2f current-y))
  (> (point-norm (sub-point `(,group-first-x ,group-first-y)
			    `(,group-bary-x ,group-bary-y))) 1e-3))

;; Transformations
(define (sub-point p1 p2)
 `(,(- (car p1) (car p2))
   ,(- (cadr p1) (cadr p2))))

(define (point-norm p)
  (sqrt (+ (* (car p) (car p))
	   (* (cadr p) (cadr p)))))

(define (traverse-transform o opn)
  (define (traverse o)
    (opn (if (pair? o) (map traverse o) o))
  )
  (traverse o))

(define (translate-point x y)
  (lambda (o)
     (if (match? o '(point :%2))
	`(point ,(f2s (+ x (s2f (cadr o)))) ,(f2s (+ y (s2f (caddr o)))))
	 o)))

(define (group-translate x y)
  (lambda (o)
     (traverse-transform o (translate-point x y))))

(define (zoom-point x0 y0 h)
  (lambda (o)
     (if (match? o '(point :%2))
	 (let* ((x (s2f (cadr o)))
		(y (s2f (caddr o)))
	    )
	   `(point ,(f2s (+ x0 (* (- x group-bary-x) h)))
		   ,(f2s (+ y0 (* (- y group-bary-y) h))))
	 )
	 o)))

(define (group-zoom x y)
  (with h (/ (point-norm (sub-point `(,x ,y)
				    `(,group-bary-x ,group-bary-y)))
	     (point-norm (sub-point `(,group-first-x ,group-first-y)
				    `(,group-bary-x ,group-bary-y))))
  (lambda (o)
     (let* ((res (traverse-transform o (zoom-point group-bary-x group-bary-y h)))
	    (curmag #f)
	    (gmag (s2f (graphics-eval-magnification)))
	)
	(if (eq? (car res) 'with)
	    (with curmag (s2f (find-prop res "magnification" "1.0"))
	       (list-find&set-prop
		  res "magnification" (f2s (* curmag gmag h))))
	   `(with "magnification" ,(f2s (* gmag h)) ,res))))))

(define (rotate-point x0 y0 alpha)
  (lambda (o)
     (if (match? o '(point :%2))
	 (let* ((x (- (s2f (cadr o)) group-bary-x))
		(y (- (s2f (caddr o)) group-bary-y))
	    )
	   `(point ,(f2s (+ x0 (* x (cos alpha)) (* (- y) (sin alpha))))
		   ,(f2s (+ y0 (* x (sin alpha)) (* y (cos alpha)))))
	 )
	 o)))

(define (group-rotate x y)
  (let* ((b (make-rectangular group-bary-x group-bary-y))
	 (f (make-rectangular group-first-x group-first-y))
	 (p (make-rectangular x y))
	 (alpha (- (angle (- p b)) (angle (- f b))))
     )
  (lambda (o)
     (traverse-transform o (rotate-point group-bary-x group-bary-y alpha)))))

;; Grouping/Ungrouping
(tm-define (group-selected-objects)
  (if (and (not sticky-point) (nnull? (sketch-get)))
  (begin
    (graphics-store-state 'group-selected-objects)
    (sketch-checkout)
    (with o (cons 'gr-group (sketch-get))
       (sketch-reset)
       (sketch-toggle o)
    )
    (sketch-commit)
    (graphics-group-start)
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

(tm-define (ungroup-current-object)
  (if (and (not sticky-point)
	   (== (length (sketch-get)) 1)
	   (== (tree-label (car (sketch-get))) 'gr-group))
	;; TODO: Add support for ungrouping <with|...props...|<gr-group|...>>
  (with obj (car (sketch-get))
    (graphics-store-state 'ungroup-selected-objects)
    (sketch-checkout)
    (sketch-reset)
    (foreach-number (i 0 < (tree-arity obj))
       (sketch-toggle (tree-ref obj i)))
    (sketch-commit)
    (graphics-group-start)
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

;; Removing
(tm-define (remove-selected-objects)
  (sketch-checkout)
  (sketch-reset)
  (sketch-commit)
  (graphics-group-start))

;; State transitions
(tm-define (start-operation opn p obj)
  (:require (in? (car obj) (append '(point text-at) gr-tags-curves)))
  (set! current-path #f)
  (if sticky-point
      ;;Perform operation
      (begin
	 (sketch-commit)
	 (graphics-decorations-update)
	 (if (== (state-ref graphics-first-state 'graphics-action)
		 'start-operation)
	     (remove-undo-mark))
	 (set! graphics-undo-enabled #t)
	 (graphics-forget-states))
      ;;Start operation
      (cond
	 ((and (not multiselecting) (eq? (cadr (graphics-mode)) 'group-ungroup))
	  (if (and p (not sticky-point) (null? (sketch-get))
		   (== (tree-label (path->tree p)) 'gr-group))
	      (sketch-set! `(,(path->tree p))))
	  (if (and (not sticky-point)
		   (== (length (sketch-get)) 1)
		   (== (tree-label (car (sketch-get))) 'gr-group))
	      (ungroup-current-object)
	      (group-selected-objects))
	 )
	 ((and (not multiselecting) (== (cadr (graphics-mode)) 'props))
	  (if (null? (sketch-get))
	      (if p
	      (begin
		 (set! obj (stree-at p))
		 (set! current-path (graphics-assign-props p obj))
		 (set! current-obj obj)
		 (graphics-decorations-update)))
	      (with l '()
		 (for (o (sketch-get))
		    (with p (graphics-assign-props
			       (tree->path o)
			       (tree->stree o))
		       (set! l (cons (path->tree p) l))))
		 (sketch-set! (reverse l))
		 (graphics-decorations-update))
	  )
	  (graphics-group-start)
	 )
	 ((and (not multiselecting) (or p (nnull? (sketch-get))))
	  (if (null? (sketch-get))
	      (any_toggle-select #f #f p obj))
	  (if (store-important-points)
	  (begin
	     (graphics-store-state 'start-operation)
	     (sketch-checkout)
	     (sketch-transform tree->stree)
	     (set! group-first-go (copy-tree (sketch-get)))
	     (set! graphics-undo-enabled #f)
	     (graphics-store-state #f)
	     (set! group-old-x (s2f current-x))
	     (set! group-old-y (s2f current-y))))))))

(define (any_toggle-select x y p obj)
  (if (not sticky-point)
  (if multiselecting
      (let* ((x1 (s2f selecting-x0))
	     (y1 (s2f selecting-y0))
	     (x2 (s2f x))
	     (y2 (s2f y))
	     (tmp 0)
	     (sel #f)
	 )
	 (if (> x1 x2)
	     (begin
		(set! tmp x1)
		(set! x1 x2)
		(set! x2 tmp))
	 )
	 (if (> y1 y2)
	     (begin
		(set! tmp y1)
		(set! y1 y2)
		(set! y2 tmp))
	 )
	 (set! sel (graphics-select-area x1 y1 x2 y2))
	 (sketch-reset)
	 (for (p sel)
            (sketch-toggle (path->tree p)))
	 (graphics-decorations-update)
	 (set! multiselecting #f)
	 (set! selecting-x0 #f)
	 (set! selecting-y0 #f)
      )
      (if p
	  (with t (path->tree p)
	     (sketch-toggle t)
	     (graphics-decorations-update))
	  (begin
	     (set! selecting-x0 x)
	     (set! selecting-y0 y)
	     (set! multiselecting #t))))))

(tm-define (toggle-select x y p obj)
  (:require (in? (car obj) (append '(point text-at) gr-tags-curves)))
  (any_toggle-select x y p obj))

(define (any_unselect-all p obj)
  (if (nnull? (sketch-get))
  (begin
     (sketch-reset)
     (graphics-decorations-update))
  (if (and p (not multiselecting)
	   (== (cadr (graphics-mode)) "props"))
      (graphics-copy-props p))))

(tm-define (unselect-all p obj)
  (:require (in? (car obj) (append '(point text-at) gr-tags-curves)))
  (any_unselect-all p obj))

;; Dispatch
(tm-define (edit_move mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (if sticky-point
      (begin
	 (set! x (s2f x))
	 (set! y (s2f y))
	 (with mode (graphics-mode)
	    (cond ((== (cadr mode) 'move)
		     (sketch-transform
			(group-translate (- x group-old-x)
					 (- y group-old-y))))
		  ((== (cadr mode) 'zoom)
		     (sketch-set! group-first-go)
		     (sketch-transform
			(group-zoom x y))
		  )
		  ((== (cadr mode) 'rotate)
		     (sketch-set! group-first-go)
		     (sketch-transform
			(group-rotate x y))
		  ))
	 )
	 (set! group-old-x x)
	 (set! group-old-y y))
      (if multiselecting
	  (begin
	     (graphical-object!
		(append
		   (create-graphical-props 'default #f)
		  `((with color red
		      (cline (point ,selecting-x0 ,selecting-y0)
		 	     (point ,x ,selecting-y0)
			     (point ,x ,y)
			     (point ,selecting-x0 ,y)))))))
	  (graphics-decorations-update))))

(tm-define (edit_left-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (start-operation 'move current-path current-obj))

(tm-define (edit_right-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (toggle-select x y current-path current-obj))

(tm-define (edit_middle-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (if (!= (logand (get-keyboard-modifiers) ShiftMask) 0)
      (if (null? (sketch-get))
	  (middle-button)
	  (remove-selected-objects))
      (unselect-all current-path current-obj)))

(tm-define (edit_tab-key mode inc)
  (:require (eq? mode 'group-edit))
  ;;(display* "Graphics] Group-edit(Tab)\n")
  (edit_tab-key 'edit inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut & paste actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-selection-active?)
  (:state graphics-state)
  (nnull? (sketch-get)))

(tm-define (graphics-copy)
  (:state graphics-state)
  (if (== (car (graphics-mode)) 'group-edit)
  (with copied-objects (list-copy (sketch-get))
     (any_unselect-all #f #f)
     (update-buffer)
     (if (null? copied-objects)
	 (stree->tree "")
	 (stree->tree (cons 'graphics copied-objects)))
  )
  (stree->tree "")))

(tm-define (graphics-cut)
  (:state graphics-state)
  (if (== (car (graphics-mode)) 'group-edit)
  (let* ((l (list-copy (sketch-get)))
	 (res (graphics-copy))
     )
     (sketch-set! l)
     (sketch-checkout)
     (sketch-reset)
     (sketch-commit)
     res
  )
  (stree->tree "")))

(tm-define (graphics-paste sel)
  (:state graphics-state)
 ;(display* "sel=" sel "\n")
  (if (and (== (car (graphics-mode)) 'group-edit)
	   (tree-compound? sel)
	   (== (tree-label sel) 'graphics)
	   (> (tree-arity sel) 0))
  (begin
     (sketch-reset)
     (sketch-checkout)
     (foreach-number (i 0 < (tree-arity sel))
       (sketch-toggle (tree-ref sel i)))
     (sketch-commit)
     (graphics-group-start))))
