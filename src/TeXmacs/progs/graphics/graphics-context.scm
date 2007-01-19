
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-context.scm
;; DESCRIPTION : context datastructures for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-context)
  (:use (utils library cursor) (utils library tree)
	(graphics graphics-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for calculating with the graphical object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Closest point
;;NOTE: This subsection is OK.
(define (graphics-norm p1 p2)
  (let ((x (- (s2f (cadr p2)) (s2f (cadr p1))))
	(y (- (s2f (caddr p2)) (s2f (caddr p1)))))
	(sqrt (+ (* x x) (* y y)))))

(define infinity (/ 1.0 0.0))
(define (is-point? p)
  (and (pair? p) (in? (car p) '(point tuple))))
  
(define (graphics-closest-point-pos-bis p l)
  (if (null? l)
      '()
       (let ((n1 (if (is-point? (car l))
				(graphics-norm p (car l))
				infinity)))
	    (if (null? (cdr l))
		l
		(let* ((p2 (graphics-closest-point-pos-bis p (cdr l)))
		       (n2 (if (is-point? (car p2))
			       (graphics-norm p (car p2))
			       infinity)))
		      (if (<= n1 n2) l p2))))))

(define (graphics-closest-point-pos p l)
  (- (length l) (length (graphics-closest-point-pos-bis p l))))

(define (object-closest-point-pos obj x y)
 ;(display* "obj(" x ", " y ")=" obj "\n")
  (if (pair? obj)
      (with type (car obj)
	 (if (== type 'point)
	     0
	 (if (not (in? (car obj) gr-tags-noncurves))
	     (graphics-closest-point-pos (list 'point x y) (cdr obj))
	 0)))
      0))

;; Graphical object
(tm-define default-color-go-points "#4040ff")
(tm-define default-color-selected-points "#ff6060")
(tm-define graphical-color "default")
(tm-define graphical-pstyle "default")
(tm-define graphical-lwidth "default")
(tm-define graphical-magnification "default")
(tm-define graphical-lstyle "default")
(tm-define graphical-lstyle-unit "default")
(tm-define graphical-larrows "default")
(tm-define graphical-fcolor "default")
(tm-define graphical-textat-halign "default")
(tm-define graphical-textat-valign "default")

(tm-define (graphical-object fetch)
; FIXME: Remove this (tree->stree) should give more speed, but I'm not sure
;   about what is the best way now. Then, directly plug the tree and test
;   the new version of (find-prop) that works directly on trees.
  (with o (tree->stree (get-graphical-object))
   ;(display* "o=" o "\n")
    (if (and fetch (pair? o))
       (begin
	  (set! graphical-color (find-prop o "color" "default"))
	  (set! graphical-pstyle (find-prop o "point-style" "default"))
	  (set! graphical-lwidth (find-prop o "line-width" "default"))
	  (set! graphical-magnification (find-prop o "magnification" "default"))
	  (set! graphical-lstyle (find-prop o "dash-style" "default"))
	  (set! graphical-lstyle-unit
		(find-prop o "dash-style-unit" "default"))
	  (set! graphical-larrows (find-prop o "line-arrows" "default"))
	  (set! graphical-fcolor (find-prop o "fill-color" "default"))
	  (set! graphical-textat-halign
		(find-prop o "text-at-halign" "default"))
	  (set! graphical-textat-valign
		(find-prop o "text-at-valign" "default"))))
    (if (pair? o)
	(if (== (cdr o) '()) o (cAr o))
	'(concat))))

(tm-define (graphical-object! obj)
 ;(display* "graphical object=" obj "\n")
  (set-graphical-object (stree->tree obj)))

;; Graphical props
;;NOTE: This subsection is OK.

;; Given a variable named "varname" :
;;
;; When in sticky mode, the graphical-[varname] global variables
;; which contain the properties of the graphical object are defined.
;;
;; Meaning of the "mode" parameter :
;;
;; -> 'active        <=> the value is graphical-[varname] ;
;; -> 'new           <=> the value is the value of the "gr-[varname]"
;;                       property of the <graphics> ;
;; -> 'default       <=> the value is the default environment value
;;                       of the "gr-[varname]" variable ;
;; -> mode is a path <=> the value is the value of the "[varname]"
;;                       property of the current graphic object (i.e.,
;;                       the one located at cursor) ;
;; -> 'basic         <=> if sticky-point => 'active
;;                             otherwise => current-path-under-mouse.
(define (dv var val)
  (if (== val "default")
      (get-default-val var)
      val)
)
(tm-define (get-graphical-prop mode prop)
  (with res
     (if (== mode 'basic)
	 (if sticky-point
	     (get-graphical-prop 'active prop)
	     (get-graphical-prop current-path-under-mouse prop))
	 (cond
	    ((== mode 'active)
	     (cond
		((== prop "color")
		 graphical-color)
		((== prop "point-style")
		 graphical-pstyle)
		((== prop "line-width")
		 graphical-lwidth)
		((== prop "magnification")
		 graphical-magnification)
		((== prop "dash-style")
		 graphical-lstyle)
		((== prop "dash-style-unit")
		 graphical-lstyle-unit)
		((== prop "line-arrows")
		 graphical-larrows)
		((== prop "fill-color")
		 graphical-fcolor)
		((== prop "text-at-halign")
		 graphical-textat-halign)
		((== prop "text-at-valign")
		 graphical-textat-valign)))
	    ((list? mode)
	     (if (== prop "magnification")
		 (graphics-eval-magnification-at (rcons mode 0))
		 (graphics-path-property mode prop)))
	    ((== mode 'new)
	     (graphics-get-property (string-append "gr-" prop)))
	    ((== mode 'default)
	     (get-default-val (string-append "gr-" prop))))
     )
     (dv prop res)))

(tm-define (create-graphical-props mode ps0)
  (let ((color #f)
	(ps #f)
	(lw #f)
	(mag #f)
	(st #f)
	(stu #f)
	(lp #f)
	(fc #f)
	(ha #f)
	(va #f)
     )
     (cond
	((== mode 'active)
	 (set! color graphical-color)
	 (set! ps graphical-pstyle)
	 (set! lw graphical-lwidth)
	 (set! mag graphical-magnification)
	 (set! st graphical-lstyle)
	 (set! stu graphical-lstyle-unit)
	 (set! lp graphical-larrows)
	 (set! fc graphical-fcolor)
	 (set! ha graphical-textat-halign)
	 (set! va graphical-textat-valign))
	((list? mode)
	 (set! color (graphics-path-property mode "color"))
	 (set! ps (graphics-path-property mode "point-style"))
	 (set! lw (graphics-path-property mode "line-width"))
	 (set! mag (graphics-eval-magnification-at (rcons mode 0)))
	 (set! st (graphics-path-property mode "dash-style"))
	 (set! stu (graphics-path-property mode "dash-style-unit"))
	 (set! lp (graphics-path-property mode "line-arrows"))
	 (set! fc (graphics-path-property mode "fill-color"))
	 (set! ha (graphics-path-property mode "text-at-halign"))
	 (set! va (graphics-path-property mode "text-at-valign")))
	((== mode 'new)
	 (set! color (graphics-get-property "gr-color"))
	 (set! ps (graphics-get-property "gr-point-style"))
	 (set! lw (graphics-get-property "gr-line-width"))
	 (set! st (graphics-get-property "gr-dash-style"))
	 (set! stu (graphics-get-property "gr-dash-style-unit"))
	 (set! lp (graphics-get-property "gr-line-arrows"))
	 (set! fc (graphics-get-property "gr-fill-color"))
	 (set! ha (graphics-get-property "gr-text-at-halign"))
	 (set! va (graphics-get-property "gr-text-at-valign")))
	((== mode 'default)
	 (set! color (get-default-val "gr-color"))
	 (set! ps (get-default-val "gr-point-style"))
	 (set! lw (get-default-val "gr-line-width"))
	 (set! st (get-default-val "gr-dash-style"))
	 (set! stu (get-default-val "gr-dash-style-unit"))
	 (set! lp (get-default-val "gr-line-arrows"))
	 (set! fc (get-default-val "gr-fill-color"))
	 (set! ha (get-default-val "gr-text-at-halign"))
	 (set! va (get-default-val "gr-text-at-valign")))
     )
     (list 'with "point-style"
		  (if ps0 ps0 (if ps (dv "point-style" ps) "square"))
		 "color" (dv "color" color)
		 "line-width" (dv "line-width" lw)
		 (if mag "magnification" "magnification-none")
		 (dv "magnification" (if mag mag "default"))
		 "dash-style" (dv "dash-style" st)
		 "dash-style-unit" (dv "dash-style-unit" stu)
		 "line-arrows" (dv "line-arrows" lp)
		 "fill-color" (dv "fill-color" fc)
		 "text-at-halign" (dv "text-at-halign" ha)
		 "text-at-valign" (dv "text-at-valign" va))))

;; Graphical contours
;;NOTE: This subsection is OK.
(define (create-graphical-embedding-box o ha0 va0 halign valign mag len)
  (define (create-haligns l b r t)
     (with x (cond ((== halign "left") l)
		   ((== halign "center") (f2s (/ (+ (s2f l) (s2f r)) 2)))
		   ((== halign "right") r)
		   (else l))
       `((line (point ,x ,b) (point ,x ,(f2s (- (s2f b) len))))
	 (line (point ,x ,t) (point ,x ,(f2s (+ (s2f t) len))))))
  )
  (define (create-valigns l b r t)
     (with y (cond ((== valign "bottom") b)
		   ((== valign "center") (f2s (/ (+ (s2f b) (s2f t)) 2)))
		   ((== valign "top") t)
		   (else b))
       `((line (point ,l ,y) (point ,(f2s (- (s2f l) len)) ,y))
	 (line (point ,r ,y) (point ,(f2s (+ (s2f r) len)) ,y))))
  )
  (define (get-textat-vbase b0)
     (if (and (eq? (car o) 'text-at)
	      (equal? va0 "base"))
	 (begin
	    (set! o `(with "text-at-halign" ,ha0
			   "text-at-valign" "bottom" ,o))
	    (let* ((info0 (cdr (box-info o "lbLB")))
		   (b (f2s (min (s2f (cadr info0)) (s2f (cadddr info0)))))
		  )
		  (set! o (list-ref o 5))
		  b)
	 )
	 b0)
  )
  (let* ((o1 (with res (if (in? (car o) '(text-at gr-group))
			  `(with "text-at-halign" ,ha0
				 "text-at-valign" ,va0 ,o)
			   o)
		(if (!= mag "default")
		    (set! res `(with "magnification" ,mag ,res)))
		res))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (f2s (min (s2f (car  info0)) (s2f (caddr  info0)))))
	 (b0 (f2s (min (s2f (cadr info0)) (s2f (cadddr info0)))))
	 (r (f2s (max (s2f (car  info1)) (s2f (caddr  info1)))))
	 (t (f2s (max (s2f (cadr info1)) (s2f (cadddr info1)))))
	 (b (get-textat-vbase b0))
	 (p00 (frame-inverse `(tuple ,l ,b0)))
	 (p10 (frame-inverse `(tuple ,r ,b0)))
	 (p0 (frame-inverse `(tuple ,l ,b)))
	 (p1 (frame-inverse `(tuple ,r ,b)))
	 (p2 (frame-inverse `(tuple ,r ,t)))
	 (p3 (frame-inverse `(tuple ,l ,t)))
	)
	(set-car! p0 'point)
	(set-car! p1 'point)
	(set-car! p2 'point)
	(set-car! p3 'point)
	(with res `((cline ,p00 ,p10 ,p2 ,p3)
		   )
		   (if halign (set! res (append res
				 (create-haligns (cadr p00) (caddr p00)
						 (cadr p10) (caddr p2)))))
		   (if valign (set! res (append res
				 (create-valigns (cadr p0) (caddr p0)
						 (cadr p1) (caddr p2)))))
		   res)))

(define (in-interval? x i1 i2 supop infop)
  (and (supop x i1) (infop x i2)))

(tm-define (on-graphical-embedding-box? x y o eps)
  (set! eps (length-decode eps))
  (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	 (va (get-graphical-prop 'basic "text-at-valign"))
	 (o1 (if (and (pair? o) (eq? (car o) 'text-at))
		`(with "text-at-halign" ,ha
		       "text-at-valign" ,va ,o)
		 o))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (min (s2f (car  info0)) (s2f (caddr  info0))))
	 (b (min (s2f (cadr info0)) (s2f (cadddr info0))))
	 (r (max (s2f (car  info1)) (s2f (caddr  info1))))
	 (t (max (s2f (cadr info1)) (s2f (cadddr info1))))
	 (p (frame-direct `(tuple ,x ,y)))
	)
	(set! x (s2f (cadr p)))
	(set! y (s2f (caddr p)))
	(or (and (in-interval? x (- l eps) l >= <)
		 (in-interval? y (- b eps) (+ t eps) >= <=))
	    (and (in-interval? x r (+ r eps) > <=)
		 (in-interval? y (- b eps) (+ t eps) >= <=))
	    (and (in-interval? x (- l eps) (+ r eps) >= <=)
		 (in-interval? y (- b eps) b >= <))
	    (and (in-interval? x (- l eps) (+ r eps) >= <=)
		 (in-interval? y t (+ t eps) > <=)))))

(define draw-nonsticky-curp #t)
(define (create-graphical-contour o edge no) ;; Point mode
  (define (curp lp)
     (if draw-nonsticky-curp lp '())
  )
  (cond ((== (car o) 'point)
         (cons o '())
        )
        ((== (car o) 'text-at)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic "text-at-valign"))
	        (mag (get-graphical-prop 'basic "magnification"))
	    )
	    (create-graphical-embedding-box o ha va ha va mag 0.1))
        )
        ((== (car o) 'gr-group)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic "text-at-valign"))
	        (mag (get-graphical-prop 'basic "magnification"))
	    )
	    (create-graphical-embedding-box
	       o ha va "center" "center" mag 0.1))
        )
        (else (if (integer? no)
		  (let* ((l (list-tail (cdr o) no))
		         (ll (length l)))
		        (append
		  	  (with h (list-head (cdr o) no)
			    (if (and edge
				  (in? (car o)
				      '(cline cspline carc))
				  (== (+ no 1) (length (cdr o))))
			      (cons `(with "point-style"
					   ,(if sticky-point
					        "square" "disk")
			        ,(car h)) (cdr h))
			      h))
			  (cons
			    (list 'with "point-style" "disk"
			      (cons 'concat
			        (if (< ll 2)
				    (if sticky-point
				       '()
				        (if edge
					    (list-head l 1)
					    (curp (list-head l 1))))
				     (if edge
				        (with l2 (list-head l 2)
				        (if sticky-point
					   `(,(list* 'with
					         "point-style"
					         "square"
					        `((concat .
						    ,(cdr l2)))))
					    l2))
				        (cons
					  `(with "point-style"
					         "square"
					        ,(list-ref l 1))
					   (curp (list-head l 1))
					   ))))
			    ) '())
			  (if (> ll 2) (list-tail l 2) '())))
		  (cdr o)))))

;; Graphical contours (group mode)
;;NOTE: This subsection is OK
(define (add-selections-colors op color fill-color)
  (if (not color) (set! color "none"))
  (if (not fill-color) (set! fill-color "none"))
 `((with "color" ,color
	 "point-style" "square"
	 "fill-color" ,fill-color
	 (concat . ,op))))

(define (create-graphical-contours l ptr pts) ;; Group mode
;; This routine draws the contours of each one
;; of the trees contained in the list l. If the
;; path ptr is the path of one of the trees in
;; the list, the corresponding object is drawn
;; using special points. Finally, the drawing
;; is made according to the mode in pts (i.e.,
;; object, points, etc.).
  (define on-aobj #f)
  (define aobj-selected #f)
  (define (asc col fcol op)
     (if (and on-aobj (not aobj-selected))
	 (set! fcol #f))
     (add-selections-colors op col fcol)
  )
  (define res '())
  (define curscol #f)
  (foreach (o l)
     (if (tree? o)
	 (with path (reverse (tree-ip o))
	       (if (equal? path ptr)
		   (set! aobj-selected #t)))))
  (if (and (== pts 'points) ptr)
  (begin
     (set! l (cons (path->tree ptr) l))))
  (foreach (o l)
     (if (not (and (tree? o) (< (cAr (tree-ip o)) 0)))
     (let* ((props #f)
	    (t #f)
	    (path0 #f)
	)
	(set! curscol #f)
	(set! on-aobj #f)
	(if (tree? o)
	    (with path (reverse (tree-ip o))
	       (set! props (create-graphical-props (if (== pts 'points)
						       'default path)
						   (if (== pts 'object)
						       #f "square")))
	       (if (equal? path ptr)
	       (begin
		  (set! on-aobj #t)
		  (set! curscol default-color-go-points)))
	       (set! path0 path)
	       (set! o (tree->stree o))) ;; FIXME: Remove this (tree->stree)
	)
	(if (and (== (car o) 'gr-group) (!= pts 'object))
	    (set! props (create-graphical-props 'default #f)))
	(cond ((== (car o) 'point)
	       (if (not curscol)
		   (set! curscol default-color-selected-points))
	       (set! t (if (== pts 'object)
			  `(,o)
			   (asc curscol #f `(,o))))
	      )
	      ((== (car o) 'text-at)
	       (if (not curscol)
		   (set! curscol default-color-selected-points))
	       (set! t (let* ((ha (get-graphical-prop path0 "text-at-halign"))
			      (va (get-graphical-prop path0 "text-at-valign"))
			      (mag (get-graphical-prop path0 "magnification"))
			      (gc (asc curscol #f
				    (create-graphical-embedding-box
				      o ha va ha va mag 0.1)))
			  )
			  (if (== pts 'object-and-points)
			      (cons o gc)
			      (if (== pts 'object)
				 `(,o)
				  gc))))
	      )
	      ((== (car o) 'gr-group)
	       (if (not curscol)
		   (set! curscol default-color-selected-points))
	       (set! t (with gc (asc curscol #f
				  (let* ((ha (get-graphical-prop
						path0 "text-at-halign"))
					 (va (get-graphical-prop
						path0 "text-at-valign"))
					 (mag (get-graphical-prop
						path0 "magnification"))
				     )
				     (create-graphical-embedding-box
					o ha va "center" "center" mag 0.1)))
			  (if (== pts 'object-and-points)
			      (cons o gc)
			      (if (== pts 'object)
				 `(,o)
				  gc))))
	      )
	      (else
		 (set! t (if (== pts 'object-and-points)
			     (cons o (asc curscol default-color-selected-points
					  (cdr o)))
			     (if (== pts 'object)
				`(,o)
				 (asc curscol default-color-selected-points
				      (cdr o))))))
	)
	(set! res (append res
			  (if props
			     `(,(append props `(,(list* 'concat t))))
			      t)))))
  )
  res)

;; Create graphical object
;;NOTE: This subsection is OK

(tm-define (create-graphical-object o mode pts no)
;; o    == the objet one wants to draw
;; mode == 'active, 'new, <path>, etc. (cf. "Graphical props" above).
;; pts  == points, object, object-and-points.
;; no   == description of the edge  <=> no | (edge no)
;;      == flag used in group modes <=> 'group | 'no-group
  (define edge #t)
 ;(display* "o[create-graphical-object]=")(write o)(newline)
  (if (pair? no)
      (begin
	 (set! edge (car no))
	 (set! no (cadr no))))
  (if o
      (let* ((op (add-selections-colors
		    (create-graphical-contour o edge no)
		    default-color-go-points #f))
	     (props (if (and pts (!= pts 'points))
			(create-graphical-props mode #f)
			(create-graphical-props 'default #f)))
         )
	 (graphical-object!
	    (if (or (eq? no 'group)
		    (and (not (eq? no 'no-group))
			 (graphics-group-mode? (graphics-mode)))
		)
	       `(concat .
                  ,(create-graphical-contours
		      selected-objects current-path-under-mouse pts)
		)
		(append
		   props
		  `((concat . ,(cond ((== pts 'points) op)
				     ((== pts 'object) `(,o))
				     ((== pts 'object-and-points)
				      (cons o op))))))))
      )
      (graphical-object! '(concat))))

;; Operating on the graphical object
(tm-define (transform-graphical-object opn)
  (with o (tree->stree (get-graphical-object))
     (if (pair? o)
     (begin
	(set! o (opn o))
	(graphical-object! o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State variables & history for the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK till we find a better system for handling the GC

;; Past events
(define past-events #('() '()))
(tm-define (event-exists! o e)
  (vector-set!
     past-events 0
     (cons (list o e) (vector-ref past-events 0))))

(tm-define (event-exists? o e t)
  (define (seek-event l)
     (if (or (null? l) (npair? (car l)))
	 #f
	 (with x (car l)
	    (if (and (== (car x) o) (== (cadr x) e))
		#t
		(seek-event (cdr l)))))
  )
  (if (in? t '(0 1))
      (seek-event (vector-ref past-events t))
      #f))

(tm-define (events-clocktick)
  (vector-set! past-events 1 (vector-ref past-events 0))
  (vector-set! past-events 0 '()))

(define (events-forget)
  (set! past-events (make-vector 2))
  (vector-set! past-events 0 '())
  (vector-set! past-events 1 '()))

;; State variables
(tm-define sticky-point #f)
(tm-define current-point-no #f)
(tm-define current-edge-sel? #f)
(tm-define current-selection #f)
(tm-define previous-selection #f)
(tm-define subsel-no #f)
(tm-define current-x #f)
(tm-define current-y #f)
(tm-define graphics-undo-enabled #t)
(tm-define selected-objects '())
(tm-define selecting-x0 #f)
(tm-define selecting-y0 #f)
(tm-define multiselecting #f)
(tm-define current-path-under-mouse #f) ; volatile
(tm-define layer-of-last-removed-object #f)

(tm-define state-slots
  ''(graphics-action
     graphical-object
     sticky-point
     current-point-no
     current-edge-sel?
     current-selection
     previous-selection
     subsel-no
     current-x
     current-y
     graphics-undo-enabled
     selected-objects
     selecting-x0
     selecting-y0
     multiselecting))

(define-export-macro (state-len)
  `(length ,state-slots))

(tm-define (new-state)
  (make-vector (state-len)))

(define-export-macro (state-slot-ref i)
  `(- (length (memq ,i ,state-slots)) 1))

(define-export-macro (state-ref st var)
  `(vector-ref ,st (state-slot-ref ,var)))

(define-export-macro (state-set! st var val)
  `(vector-set! ,st (state-slot-ref ,var) ,val))

(tm-define (graphics-state-get)
  (with st (new-state)
    (state-set! st 'graphics-action #f)
    (state-set! st 'graphical-object (get-graphical-object))
    (state-set! st 'sticky-point sticky-point)
    (state-set! st 'current-point-no current-point-no)
    (state-set! st 'current-edge-sel? current-edge-sel?)
    (state-set! st 'current-selection current-selection)
    (state-set! st 'previous-selection previous-selection)
    (state-set! st 'subsel-no subsel-no)
    (state-set! st 'current-x current-x)
    (state-set! st 'current-y current-y)
    (state-set! st 'graphics-undo-enabled graphics-undo-enabled)
    (state-set! st 'selected-objects selected-objects)
    (state-set! st 'selecting-x0 selecting-x0)
    (state-set! st 'selecting-y0 selecting-y0)
    (state-set! st 'multiselecting multiselecting)
    st))

(tm-define (graphics-state-set st)
  (with o (state-ref st 'graphical-object)
    (if (pair? (tree->stree o))
	(set-graphical-object o)
	(create-graphical-object #f #f #f #f)))
  (set! sticky-point (state-ref st 'sticky-point))
  (set! current-point-no (state-ref st 'current-point-no))
  (set! current-edge-sel? (state-ref st 'current-edge-sel?))
  (set! current-selection (state-ref st 'current-selection))
  (set! previous-selection (state-ref st 'previous-selection))
  (set! subsel-no (state-ref st 'subsel-no))
  (set! current-x (state-ref st 'current-x))
  (set! current-y (state-ref st 'current-y))
  (set! graphics-undo-enabled (state-ref st 'graphics-undo-enabled))
  (set! selected-objects (state-ref st 'selected-objects))
  (set! selecting-x0 (state-ref st 'selecting-x0))
  (set! selecting-y0 (state-ref st 'selecting-y0))
  (set! multiselecting (state-ref st 'multiselecting)))

;; State stack
(tm-define graphics-states '())

(tm-define (graphics-states-void?)
  (null? graphics-states))

(tm-define (graphics-state-first?)
  (and (not (graphics-states-void?)) (null? (cdr graphics-states))))

(tm-define (graphics-push-state st)
  (set! graphics-states (cons st graphics-states)))

(tm-define (graphics-pop-state)
  (if (graphics-states-void?)
      (display* "(graphics-pop-state)::void(graphics-states)\n")
      (with st (car graphics-states)
	(set! graphics-states (cdr graphics-states))
	st)))

;; User interface
(tm-define graphics-first-state #f)

(tm-define (graphics-store-first action)
  (if graphics-first-state
      (display* "(graphics-store-first)::!void(graphics-store-first)\n"))
  (set! graphics-first-state (graphics-state-get))
  (state-set! graphics-first-state 'graphics-action action))

(tm-define (graphics-back-first)
  (graphics-state-set graphics-first-state)
  (set! graphics-first-state #f))

(tm-define (graphics-store-state first?)
  (if first?
      (graphics-store-first first?)
      (graphics-push-state (graphics-state-get))))

(tm-define (graphics-back-state first?)
  (if first?
      (graphics-back-first)
      (if (graphics-state-first?)
	  (undo)
	  (with st (graphics-pop-state)
	    (graphics-state-set st)
	    (if (graphics-states-void?)
		(graphics-push-state st))))))

(tm-define (graphics-reset-state)
  (create-graphical-object #f #f #f #f)
  (set! sticky-point #f)
  (set! current-point-no #f)
  (set! current-edge-sel? #f)
  (set! current-selection #f)
  (set! previous-selection #f)
  (set! subsel-no #f)
  (set! current-x #f)
  (set! current-y #f)
  (set! graphics-undo-enabled #t)
  (set! selected-objects '())
  (set! selecting-x0 #f)
  (set! selecting-y0 #f)
  (set! multiselecting #f)
  (set! current-path-under-mouse #f)
  (set! layer-of-last-removed-object #f))

(tm-define (graphics-forget-states)
  (set! graphics-first-state #f)
  (set! graphics-states '())
  (events-forget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for using and maintaining the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Graphical select
;;NOTE: This subsection is OK, except the hack for custom objects ;-(...
(define (filter-graphical-select l x y)
  (define (filter-path p)
     (define (cut-after-first-negative p) ;; NOTE: Belongs to the hack
	(with ref #f
	   (foreach-cons (x p)
	      (if (and (not ref) (< (car x) 0))
		  (set! ref x))
	   )
	   (if ref
	       (set-cdr! ref '()))
	)
	p
     )
     (with p2 (map string->number (cdr p))
       (set! p2 (cut-after-first-negative p2)) ;; NOTE: Hack
       (if (graphics-path p2) p2 #f))
  )
  (define (filter-sel e)
    (with e2 (map filter-path (cdr e))
   ;; FIXME: Hack to workaround the bad results of (graphical-select) for
   ;;   custom objects (sends back paths with negative elements).
      (if (and (>= (length e2) 1)
	       (pair? (car e2))
	       (< (cAr (car e2)) 0)
	  )
	  (set-car! e2 (rcons (cDr (car e2)) 0)))
      (if (and (>= (length e2) 2)
	       (pair? (cadr e2))
	       (< (cAr (cadr e2)) 0)
	  )
	  (set-car! (cdr e2) (rcons (cDr (cadr e2)) 0)))
      (if (and (>= (length e2) 2)
	       (== (car e2) (cadr e2))
	  )
	  (set-cdr! e2 '()))
      (if (car e2)
      (let* ((p (graphics-path (car e2)))
	     (o (if p (path->tree p) #f))
	 )
	 (if (and (compound-tree? o)
		  (eq? (tree-label o) 'with)
		  (eq? (tree-label (tree-ref o (- (tree-arity o) 1)))
		       'graphics))
	 (begin
	    (set! o #f)
	    (set-car! e2 #f)))
	 (if (and o (not (in? (tree-label o) gr-tags-all)))
          ;; Custom markup
	     (set-car! e2 (rcons p (if x (object-closest-point-pos
					    (tree->stree o) (f2s x) (f2s y))
					 0))))))
   ;; FIXME: End hack
      (if (== (car e2) #f) #f e2))
  )
  (define (remove-filtered-elts l)
    (if (pair? l)
      (if (pair? (cdr l))
	(if (== (cadr l) #f)
	    (begin
	      (set-cdr! l (cddr l))
	      (remove-filtered-elts l))
	    (remove-filtered-elts (cdr l)))
	l)
      #f)
  )
  (with l2 (cons 'tuple (map filter-sel (cdr l)))
    (remove-filtered-elts l2)
   ;(display* "res2=" (cdr l2) "\n")
    (cdr l2)))

(tm-define (graphics-select x y d)
  (with res (tree->stree (graphical-select x y))
   ;(display* "res=" res "\n")
    (filter-graphical-select res x y)))

(tm-define (graphics-select-area x1 y1 x2 y2)
  (define l '())
  (with res (tree->stree (graphical-select-area x1 y1 x2 y2))
   ;(display* "res=" res "\n")
    (set! res (filter-graphical-select res #f #f))
    (foreach (e res)
       (set! l (cons (graphics-path (car e)) l))
    )
    (reverse (list-remove-duplicates l))))

(tm-define (select-first x y)
  (with sel (graphics-select x y 15)
    (if (pair? sel) (car sel) #f)))

(tm-define (select-choose x y)
  (with sel (graphics-select x y 15)
    (set! previous-selection current-selection)
    (set! current-selection sel)
    (if (or (null? sel) (!= current-selection previous-selection))
	(set! subsel-no 0))
    (if (pair? sel) (car (list-tail sel subsel-no)) #f)))

(tm-define (select-next)
  (if (and current-selection subsel-no)
      (begin
	(set! subsel-no (+ subsel-no 1))
	(if (>= subsel-no (length current-selection))
	    (set! subsel-no 0)))))

;; Graphics X cursor
;;NOTE: This subsection is OK
(tm-define graphics-texmacs-pointer 'none)
(tm-define (set-texmacs-pointer curs)
  (define (set-pointer name)
     (if (symbol? name)
	 (set! name (symbol->string name)))
     (set! graphics-texmacs-pointer name)
  )
  (if (!= graphics-texmacs-pointer curs)
  (cond ((== curs 'none)
	 (set-pointer 'none)
	 (set-mouse-pointer
	    (tm_xpm "tm_cursor_none.xpm")
	    (tm_xpm "tm_mask_none.xpm")))
	((== curs 'text-arrow)
	 (set-pointer 'none)
	 (set-predef-mouse-pointer "XC_top_left_arrow"))
	((== curs 'graphics-cross)
	 (set-texmacs-pointer 'none)
	 (set-pointer 'graphics-cross))
	((== curs 'graphics-cross-arrows)
	 (set-texmacs-pointer 'none)
	 (set-pointer 'graphics-cross-arrows))
	(else
	   #t))))

;; Graphics context
;;NOTE: This subsection is OK till we find a better system for handling the GC
(define-export-macro (with-graphics-context msg x y path obj no edge . body)
  `(with res #t
     (set! current-x x)
     (set! current-y y)
     (set! current-path-under-mouse #f)
     (with gm (graphics-group-mode? (graphics-mode))
	(if sticky-point
	    (with o (graphical-object #t)
	      (if (or gm (and (pair? o) (nnull? (cdr o)))
		  )
		  (let* ((,obj (if gm '(point) (cadr o)))
			 (,no current-point-no)
			 (,edge current-edge-sel?)
			 (,path (cDr (cursor-path))))
		       ,(cons 'begin body))
		  (begin
		     (set! res #f)
		     (if (not (and (string? ,msg)
				   (== (substring ,msg 0 1) ";")))
			 (display* "Uncaptured gc(sticky) "
			    ,msg " " o ", " ,x ", " ,y "\n")))))
	    (let* (( sel (select-choose (s2f ,x) (s2f ,y)))
		   ( pxy (if sel (car sel) '()))
		   (,path (graphics-path pxy))
		   (,obj (if gm '(point) (graphics-object pxy)))
		   (,edge (and sel (== (length sel) 2)))
		   (,no (if sel (cAr (car sel)) #f)))
	      (set! current-path-under-mouse ,path)
	      (if ,obj
		  ,(cons 'begin body)
		  (if (and (string? ,msg)
			   (== (substring ,msg 0 1) ";"))
		      (if (== (substring ,msg 0 2) ";:")
			 ,(cons 'begin body)
			  (set! res #f))
		      (begin
			 (set! res #f)
			 (display* "Uncaptured gc(!sticky) "
				   ,msg " " ,obj ", " ,x ", " ,y "\n"))))))
     )
     res))

(define current-cursor #f)
(define TM_PATH (var-eval-system "echo $TEXMACS_PATH"))
(define (tm_xpm name)
  (string-append TM_PATH "/misc/pixmaps/" name))

(tm-define (graphics-reset-context cmd)
  ;; cmd in { begin, exit, undo }
  ;; (display* "Graphics] Reset-context " cmd "\n")
  (if (in? cmd '(begin exit))
      (set! current-path-under-mouse #f))
  (cond
   ((== cmd 'text-cursor)
    (if (not (== current-cursor 'text-cursor))
    (begin
       (set! current-cursor 'text-cursor)
       (set-texmacs-pointer 'text-arrow))))
   ((== cmd 'graphics-cursor)
    (if (not (== current-cursor 'graphics-cursor))
    (begin
       (set! current-cursor 'graphics-cursor)
       (set-texmacs-pointer 'graphics-cross))))
   ((and (in? cmd '(begin exit)) (or (== cmd 'begin) (not sticky-point)))
   ; FIXME : when we move the cursor from one <graphics> to another,
   ;   we are not called, whereas it should be the case.
    (graphics-reset-state)
    (graphics-forget-states)
    (with p (graphics-active-path)
       (if p (create-graphical-object (graphics-active-object) p 'points #f))))
   ((and (== cmd 'exit) sticky-point)
    (set! graphics-undo-enabled #t)
    (if graphics-first-state
	(begin
	  (if (== (state-ref graphics-first-state 'graphics-action)
		   'start-move)
	      (with p (cursor-path)
		(unredoable-undo)
		(go-to p)))))
    (graphics-reset-state)
    (graphics-forget-states))
   ((== cmd 'undo)
    (if (and sticky-point (not graphics-undo-enabled)
	     (in? (state-ref graphics-first-state 'graphics-action)
		 '(start-move start-operation)))
	(begin
	; FIXME : In this begin, the state variables should be raz-ed as well !
	  (set! graphics-undo-enabled #t)
	  (unredoable-undo))
	(begin
	  (set! selected-objects '())
	  (invalidate-graphical-object)
	  (if (and graphics-undo-enabled (not sticky-point))
	      (with p (graphics-active-path)
		(if p
		    (create-graphical-object
		       (graphics-active-object) p 'points #f)
		    (create-graphical-object #f #f #f #f))))
	  (if (and (not graphics-undo-enabled) sticky-point)
	      (create-graphical-object #f #f #f #f))
	  (set! sticky-point #f)
	  (set! current-point-no #f)
	  (set! graphics-undo-enabled #t)
	  (set! multiselecting #f)
	  (if graphics-first-state
	      (graphics-back-first))
	  (graphics-forget-states)
	  (invalidate-graphical-object))))
   (else (display* "Uncaptured reset-context " cmd "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK till we find a better system for dispatching

(define-export-macro (dispatch obj vals func vars . parms)
  (define (cond-case elt)
    `(,(if (null? (cdr elt))
	   `(== ,obj ',(car elt))
	   `(in? ,obj ',elt))
      (,(string->symbol
	 (string-append
	  (symbol->string (car elt))
	  "_"
	  (symbol->string func)))
       . ,vars)))
  `(begin
    ,(if (and (pair? parms) (in? 'do-tick parms))
	`(begin
	    (events-clocktick)
	    (event-exists! ,obj ',func)))
     (if (and (not sticky-point)
	      (tm-upwards-path (cDr (cursor-path)) '(text-at) '(graphics)))
	 (when-inside-text-at ',func . ,vars)
	,(append (cons
	    'cond
	    (map cond-case vals))
	   `(,(if (and (pair? parms) (in? 'handle-other parms))
		 `(else (,(string->symbol
			   (string-append
			    "other_" (symbol->string func))) . ,vars))
		 `(else (display* "Uncaptured " ',func " " ,obj "\n"))))))))
