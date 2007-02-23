
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
     (set! selected-objects (rcons selected-objects 
	   (if (eq? (tree-label t) 'with)
		    (tree-ref t (- (tree-arity t) 1))
		    t)))))

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
  (if (nnull? selected-objects)
  (with so (map tree->stree selected-objects)
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
  (if (and (not sticky-point) (nnull? selected-objects))
  (begin
    (graphics-store-state 'group-selected-objects)
    (create-graphical-object '(nothing) #f 'object 'group)
    (foreach (o selected-objects)
       (graphics-remove (reverse (tree-ip o)))
    )
    (set! selected-objects '())
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
	     (set! selected-objects `(,(path->tree p)))
	     (create-graphical-object '(nothing) #f 'points 'group)
	  )
	 ;(create-graphical-object #f #f #f #f)
	  (graphics-group-start)))
    )
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

(tm-define (ungroup-current-object)
  (if (and (not sticky-point)
	   (== (length selected-objects) 1)
	   (== (tree-label (car selected-objects)) 'gr-group))
  (let* ((so0 selected-objects)
	 (obj (car selected-objects))
    )
    (graphics-store-state 'ungroup-selected-objects)
    (set! selected-objects '())
    (foreach-number (i 0 < (tree-arity obj))
       (with o (tree-ref obj i)
	  (if (== (tree-label o) 'with)
	      (set! o (tree-ref o (- (tree-arity o) 1))))
	  (set! selected-objects (cons o selected-objects)))
    )
    (set! selected-objects (reverse selected-objects))
    (create-graphical-object '(nothing) #f 'object 'group)
    (foreach (o so0)
       (graphics-remove (reverse (tree-ip o)) 'memoize-layer)
    )
    (set! selected-objects '())
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
		(set! selected-objects
		      (cons (path->tree p) selected-objects))
	     )
	  ))
	  (set! layer-of-last-removed-object #f)
	  (set! selected-objects (reverse selected-objects))
	  (create-graphical-object '(nothing) #f 'points 'group)
	 ;(create-graphical-object #f #f #f #f)
	  (graphics-group-start)))
    )
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

;; Removing
(tm-define (remove-selected-objects)
  (foreach (o selected-objects)
     (graphics-remove (reverse (tree-ip o)))
  )
  (set! selected-objects '())
  (create-graphical-object #f #f #f #f)
  (graphics-group-start))

;; State transitions
(tm-define (point_start-operation opn p obj)
  (set! current-path-under-mouse #f)
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
	      ;(set! selected-objects '())
	       (create-graphical-object '(nothing) #f 'points 'group)
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
	 ((and (not multiselecting) (== (cadr (graphics-mode)) 'group-ungroup))
	  (if (and p (not sticky-point) (null? selected-objects)
		   (== (tree-label (path->tree p)) 'gr-group))
	      (set! selected-objects `(,(path->tree p))))
	  (if (and (not sticky-point)
		   (== (length selected-objects) 1)
		   (== (tree-label (car selected-objects)) 'gr-group))
	      (ungroup-current-object)
	      (group-selected-objects))
	 )
	 ((and (not multiselecting) (== (cadr (graphics-mode)) 'props))
	; FIXME: in (with-graphics-context), if we are in group mode,
	;   obj is := to '(point). Find why it is so, and remove this.
	  (if (null? selected-objects)
	      (if p
	      (begin
		 (set! obj (stree-at p))
		 (set! current-path-under-mouse
		       (graphics-assign-props p obj 'no-group))
		     ; FIXME: In order for (create-graphical-object)
		     ;   to work appropriately in the current case,
		     ;   we need to manually update the value of
		     ;   current-path-under-mouse. At some point,
		     ;   clean this.
		 (create-graphical-object obj '() 'points 'no-group)))
	      (with l '()
		 (foreach (o selected-objects)
		    (with p (graphics-assign-props
		       (reverse (tree-ip o))
		       (tree->stree o) #f)
		       (set! l (cons (path->tree p) l)))
		 )
		 (set! selected-objects (reverse l))
		 (create-graphical-object '(nothing) #f 'points 'group))
	  )
	  (graphics-group-start)
	 )
	 ((and (not multiselecting) (or p (nnull? selected-objects)))
	  (if (null? selected-objects)
	      (point_toggle-select #f #f p obj))
	  (if (store-important-points)
	  (begin
	     (graphics-store-state 'start-operation)
	     (create-graphical-object obj p 'object #f)
	     (set! group-first-go (get-graphical-object))
	     (set! layer-of-last-removed-object '())
	     (foreach (o selected-objects)
		(graphics-remove (reverse (tree-ip o)) 'memoize-layer)
	     )
	     (set! selected-objects '())
	     (set! sticky-point #t)
	     (set! graphics-undo-enabled #f)
	     (graphics-store-state #f)
	     (set! group-old-x (s2f current-x))
	     (set! group-old-y (s2f current-y))))))))

(tm-define (point_toggle-select x y p obj)
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
	 (set! selected-objects '())
	 (foreach (p sel)
	    (set! selected-objects (cons (path->tree p) selected-objects))
	 )
	 (set! selected-objects (reverse selected-objects))
	 (create-graphical-object '(nothing) #f 'points 'group)
	 (set! multiselecting #f)
	 (set! selecting-x0 #f)
	 (set! selecting-y0 #f)
      )
      (if p
	; FIXME: in (with-graphics-context), if we are in group mode,
	;   obj is := to '(point). Find why it is so, and remove this.
	  (with t (path->tree p)
	     (if (seek-eq? t selected-objects)
		 (remove-eq? t selected-objects)
		 (set! selected-objects (rcons selected-objects t))
	     )
	     (create-graphical-object obj p 'points #f))
	  (begin
	     (set! selecting-x0 x)
	     (set! selecting-y0 y)
	     (set! multiselecting #t))))))

(tm-define (point_unselect-all p)
  (if (nnull? selected-objects)
  (begin
     (set! selected-objects '())
     (create-graphical-object '(nothing) #f 'points 'group))
  (if (and p (not multiselecting)
	   (== (cadr (graphics-mode)) 'props))
      (graphics-copy-props p))))

;; Dispatch
(tm-define (group-edit_move x y)
  (with-graphics-context ";move" x y p obj no edge
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
	     (create-graphical-object obj p 'points #f)))))

(tm-define (group-edit_left-button x y)
  (with-graphics-context "start-operation" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       start-operation ('move p obj) do-tick)))

(tm-define (group-edit_right-button x y)
  (with-graphics-context "toggle-select" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       toggle-select (x y p obj) do-tick)))

(tm-define (group-edit_middle-button x y)
  (with-graphics-context "unselect-all" x y p obj no edge
     (if (!= (logand (get-keyboard-modifiers) ShiftMask) 0)
	 (if (null? selected-objects)
	     (point_middle-button x y p obj no)
	     (remove-selected-objects))
	 (dispatch (car obj) ((point line cline spline cspline arc carc
			       text-at))
		   unselect-all (p) do-tick))))

(tm-define (group-edit_tab-key next)
 ;(display* "Graphics] Group-edit(Tab)\n")
  (edit_tab-key next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut & paste actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-selection-active?)
  (nnull? selected-objects))

(tm-define (graphics-copy)
  (if (== (car (graphics-mode)) 'group-edit)
  (with copied-objects '()
     (foreach (o selected-objects)
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
     (point_unselect-all #f)
     (update-buffer)
     (if (null? copied-objects)
	 (stree->tree "")
	 (stree->tree (cons 'graphics copied-objects)))
  )
  (stree->tree "")))

(tm-define (graphics-cut)
  (if (== (car (graphics-mode)) 'group-edit)
  (let* ((l selected-objects)
	 (res (graphics-copy))
     )
     (foreach (o l)
	(graphics-remove (reverse (tree-ip o)))
     )
     res
  )
  (stree->tree "")))

(tm-define (graphics-paste sel)
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
     (set! selected-objects (reverse l))
     (create-graphical-object '(nothing) #f 'points 'group)
     (graphics-group-start))))
