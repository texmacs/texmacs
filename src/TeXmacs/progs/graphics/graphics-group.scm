
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-group.scm
;; DESCRIPTION : editing routines for graphics group mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004-2007  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-group)
  (:use (utils library cursor) (utils library tree)
	(kernel texmacs tm-states)
        (graphics graphics-utils) (graphics graphics-main)
        (graphics graphics-object) (graphics graphics-env)
        (graphics graphics-kbd) (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Util
(define (group-list l tag)
  (if (pair? l)
      (if (and #f ; FIXME: Just added #f. This (if) should not be
		  ;   needed anymore. After some time using the soft,
		  ;   if no strange behaviour is observed, remove it.
	       (!= tag 'point)
	       (== (car l) "point-style"))
	  (group-list (cddr l) tag)
	  (with val (if (== (car l) "magnification")
	                (local-magnification (cadr l))
			(cadr l))
	     (cons `(,(car l) ,val) (group-list (cddr l) tag))))
     '()))

(define (restore-selected-objects so)
  (foreach (t so)
     (sketch-toggle
	(if (eq? (tree-label t) 'with)
		 (tree-ref t (- (tree-arity t) 1))
		 t))))

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
	(foreach (p so-points)
	   (set! group-bary-x (+ group-bary-x (s2f (cadr p))))
	   (set! group-bary-y (+ group-bary-y (s2f (caddr p))))
	   (set! n (+ n 1))
	)
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
	(foreach-cons (c (cdr res))
	   (set-car! c (if (eq? (caar c) 'with)
			   (with curmag
				 (s2f (find-prop
					 (car c) "magnification" "1.0"))
			      (list-find&set-prop
				 (car c) "magnification" (f2s (* curmag gmag h))))
			  `(with "magnification" ,(f2s (* gmag h)) ,(car c)))))
	res))))

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
    (graphics-decorations-update 'object)
    (foreach (o (sketch-get))
       (graphics-remove (tree->path o))
    )
    (sketch-reset)
    (with go (tree->stree (get-graphical-object))
       (if (nnull? (cdr go))
       (with l '()
	  (foreach (o (cdr go))
	  (with t (cadr (cAr o))
	     (set-cdr! (list-tail o (- (length o) 2)) '())
	     (set! l (cons (graphics-enrich-sub
			      t (group-list (cdr o) (car t))) l))
	  ))
	  (with p (graphics-group-insert (cons 'gr-group (reverse l)))
	     (sketch-set! `(,(path->tree p)))
	     (graphics-decorations-update)
	  )
	  (graphics-group-start)))
    )
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

(tm-define (ungroup-current-object)
  (if (and (not sticky-point)
	   (== (length (sketch-get)) 1)
	   (== (tree-label (car (sketch-get))) 'gr-group))
  (let* ((so0 (sketch-get))
	 (obj (car (sketch-get)))
    )
    (graphics-store-state 'ungroup-selected-objects)
    (sketch-reset)
    (foreach-number (i 0 < (tree-arity obj))
       (with o (tree-ref obj i)
	  (if (== (tree-label o) 'with)
	      (set! o (tree-ref o (- (tree-arity o) 1))))
	  (sketch-toggle o))
    )
    (graphics-decorations-update 'object)
    (foreach (o so0)
       (graphics-remove (tree->path o) 'memoize-layer)
    )
    (sketch-reset)
    (with go (tree->stree (get-graphical-object))
       (if (nnull? (cdr go))
       (with l '()
	  (foreach (o (cdr go))
	  (let* ((t (cadr (cAr o)))
                 (layer layer-of-last-removed-object)
	     )
	     (set-cdr! (list-tail o (- (length o) 2)) '())
	     (with p (graphics-group-insert
			(graphics-enrich-sub
			   t (group-list (cdr o) (car t))))
		(set! layer-of-last-removed-object layer)
		(sketch-toggle (path->tree p))
	     )
	  ))
	  (set! layer-of-last-removed-object #f)
	  (graphics-decorations-update)
	  (graphics-group-start)))
    )
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

;; Removing
(tm-define (remove-selected-objects)
  (foreach (o (sketch-get))
     (graphics-remove (tree->path o))
  )
  (sketch-reset)
  (graphics-decorations-update)
  (graphics-group-start))

;; State transitions
(tm-define (start-operation opn p obj)
  (:require (in? (car obj) (append '(point text-at) gr-tags-curves)))
  (set! current-path #f)
  (if sticky-point
      ;;Perform operation
      (begin
	 (let* ((go (tree->stree (get-graphical-object)))
		(so '())
	    )
	    (if (nnull? (cdr go))
	    (begin
	       (foreach (o (cdr go))
	       (with t (cadr (cAr o))
		  (set-cdr! (list-tail o (- (length o) 2)) '())
		  (set! so (cons (path->tree 
		     (graphics-group-insert
			(graphics-enrich-sub
			   t (group-list (cdr o) (car t)))))
		     so))
	       ))
	       (restore-selected-objects (reverse so))
	      ;(sketch-reset)
	       (graphics-decorations-update)
	       (graphics-group-start)))
	 )
	 (if (== (state-ref graphics-first-state 'graphics-action)
		 'start-operation)
	     (remove-undo-mark))
	 (set! sticky-point #f)
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
	; FIXME: when the state is recalculated, if we are in group
	;   mode, obj is := to '(point). Find if it is still so and
	;   why, and thus correct the problem if necessary.
	  (if (null? (sketch-get))
	      (if p
	      (begin
		 (set! obj (stree-at p))
		 (set! current-path
		       (graphics-assign-props p obj))
		     ; FIXME: In order for (graphics-decorations-update)
		     ;   to work appropriately in the current case,
		     ;   we need to manually update the value of
		     ;   current-path. At some point, clean this.
		 (set! current-obj obj)
		 (graphics-decorations-update)))
	      (with l '()
		 (foreach (o (sketch-get))
		    (with p (graphics-assign-props
			       (tree->path o)
			       (tree->stree o))
		       (set! l (cons (path->tree p) l)))
		 )
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
	     (graphics-decorations-update 'object)
	     (set! group-first-go (get-graphical-object))
	     (set! layer-of-last-removed-object '())
	     (foreach (o (sketch-get))
		(graphics-remove (tree->path o) 'memoize-layer)
	     )
	     (sketch-reset)
	     (set! sticky-point #t)
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
	 (foreach (p sel)
            (sketch-toggle (path->tree p))
	 )
	 (graphics-decorations-update)
	 (set! multiselecting #f)
	 (set! selecting-x0 #f)
	 (set! selecting-y0 #f)
      )
      (if p
	; FIXME: when the state is recalculated, if we are in group
	;   mode, obj is := to '(point). Find if it is still so and
	;   why, and thus correct the problem if necessary.
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
		     (transform-graphical-object
			(group-translate (- x group-old-x)
					 (- y group-old-y))))
		  ((== (cadr mode) 'zoom)
		     (set-graphical-object group-first-go)
		     (transform-graphical-object
			(group-zoom x y))
		  )
		  ((== (cadr mode) 'rotate)
		     (set-graphical-object group-first-go)
		     (transform-graphical-object
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
	  (middle-button x y current-path current-obj current-point-no)
	  (remove-selected-objects))
      (unselect-all current-path current-obj)))

(tm-define (edit_tab-key mode next)
  (:require (eq? mode 'group-edit))
 ;(display* "Graphics] Group-edit(Tab)\n")
  (edit_tab-key 'edit next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut & paste actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-selection-active?)
  (:state graphics-state)
  (nnull? (sketch-get)))

(tm-define (graphics-copy)
  (:state graphics-state)
  (if (== (car (graphics-mode)) 'group-edit)
  (with copied-objects '()
     (foreach (o (sketch-get))
     (with p (tm-upwards-path (tree->path o) '(with) '())
	(let* ((t (if p (path->tree p) #f))
	       (n (if t (tree-arity t) #f))
	       (o2 (if (and t n (> n 0)) (tree-ref t (- n 1)) #f))
	   )
	   (if (or (not p) (and o2 (!= o2 o)))
	       (set! p (tree->path o)))
	)
	(set! copied-objects
	      (cons (tree->stree (path->tree p)) copied-objects)))
     )
     (set! copied-objects (reverse copied-objects))
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
  (let* ((l (sketch-get))
	 (res (graphics-copy))
     )
     (foreach (o l)
	(graphics-remove (tree->path o))
     )
     res
  )
  (stree->tree "")))

(tm-define (graphics-paste sel)
  (:state graphics-state)
  (if (and (== (car (graphics-mode)) 'group-edit)
	   (tree-compound? sel)
	   (== (tree-label sel) 'graphics)
	   (> (tree-arity sel) 0))
  (with l '()
     (foreach-number (i 0 < (tree-arity sel))
     (let* ((t (tree-ref sel i))
	    (v (if (== (tree-label t) 'with)
		   (if (> (tree-arity t) 0)
		       (tree-ref t (- (tree-arity t) 1))
		       t)
		   t))
	)
	(if (in? (tree-label v) gr-tags-all)
	    (with p (graphics-group-insert (tree->stree t))
	       (if p (set! l (cons (path->tree p) l))))))
     )
     (sketch-set! (reverse l))
     (graphics-decorations-update)
     (graphics-group-start))))
