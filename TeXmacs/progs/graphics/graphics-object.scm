
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-object.scm
;; DESCRIPTION : routines for managing the graphical object
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004-2007  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-object)
  (:use (graphics graphics-utils)))

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

(tm-define (points-dist< p1 p2 eps)
  (set! p1 (frame-direct `(tuple ,(cadr p1) ,(caddr p1))))
  (set! p2 (frame-direct `(tuple ,(cadr p2) ,(caddr p2))))
  (set! eps (length-decode eps))
  (let* ((x1 (s2f (cadr p1)))
         (y1 (s2f (caddr p1)))
         (x2 (s2f (cadr p2)))
         (y2 (s2f (caddr p2)))
     )
     (< (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))) (* eps eps))))

;; Graphical object
(tm-define default-color-go-points "#4040ff")
(tm-define default-color-selected-points "#ff6060")
(tm-define graphical-id "default")
(tm-define graphical-opacity "default")
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

(tm-define (graphical-fetch-props o)
  (set! graphical-id (find-prop o "gid" "default"))
  (set! graphical-opacity (find-prop o "opacity" "default"))
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
	(find-prop o "text-at-valign" "default")))

(tm-define (graphical-object fetch)
; FIXME: Remove this (tree->stree) should give more speed, but I'm not sure
;   about what is the best way now. Then, directly plug the tree and test
;   the new version of (find-prop) that works directly on trees.
  (with o (tree->stree (get-graphical-object))
   ;(display* "o=" o "\n")
    (if (and fetch (pair? o))
	(graphical-fetch-props o))
    (if (pair? o)
	(if (== (cdr o) '()) o (cAr o))
	'(concat))))

(tm-define (graphical-object! obj)
  ;;(display* "obj= " obj "\n")
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
;;                             otherwise => current-path.
(define (dv var val)
  (if (== val "default")
      (get-default-val var)
      val)
)
(tm-define (get-graphical-prop mode prop)
  (with res
     (cond
	((== mode 'basic)
	 (if sticky-point
	     (get-graphical-prop 'active prop)
	     (get-graphical-prop current-path prop))
         )
	((== mode 'active)
	 (cond
	    ((== prop "opacity")
	     graphical-opacity)
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
	(else ;(== mode 'default)
	 (get-default-val (string-append "gr-" prop)))
     )
     (dv prop res)))

(tm-define (create-graphical-props mode ps0)
  (let ((op #f)
	(color #f)
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
	 (set! op graphical-opacity)
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
	 (set! op (graphics-path-property mode "opacity"))
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
	 (set! op (graphics-get-property "gr-opacity"))
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
	 (set! op (get-default-val "gr-opacity"))
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
		 "opacity" (dv "opacity" op)
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
  (define (get-textat-vbase b0)
     (if (and (== (car o) 'text-at) (== va0 "base"))
	 (begin
           (set! o `(with "text-at-halign" ,ha0
                      "text-at-valign" "bottom" ,o))
           (let* ((info0 (cdr (box-info o "lbLB")))
                  (b (f2s (min (s2f (cadr info0)) (s2f (cadddr info0))))))
             (set! o (list-ref o 5))
             b))
	 b0))
  (define (create-text-at-handle o)
    (cond ((func? o 'with)
           (create-text-at-handle (cAr o)))
          ((func? o 'text-at)
           `((with "point-style" "disk" 
               ,(cAr o))))
          (else '())))
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
	 (p3 (frame-inverse `(tuple ,l ,t))))
    (set-car! p0 'point)
    (set-car! p1 'point)
    (set-car! p2 'point)
    (set-car! p3 'point)
    (with res `((cline ,p00 ,p10 ,p2 ,p3))
      (set! res (append res (create-text-at-handle o)))
      res)))

(define (in-interval? x i1 i2 supop infop)
  (and (supop x i1) (infop x i2)))

(tm-define (on-graphical-embedding-box? x y o eps)
  (set! eps (length-decode eps))
  (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	 (va (get-graphical-prop 'basic "text-at-valign"))
	 (o1 (if (and (pair? o) (== (car o) 'text-at))
		`(with "text-at-halign" ,ha
		       "text-at-valign" ,va ,o)
		 o))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (min (s2f (car  info0)) (s2f (caddr  info0))))
	 (b (min (s2f (cadr info0)) (s2f (cadddr info0))))
	 (r (max (s2f (car  info1)) (s2f (caddr  info1))))
	 (t (max (s2f (cadr info1)) (s2f (cadddr info1))))
	 (p (frame-direct `(tuple ,x ,y))))
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
     (if draw-nonsticky-curp lp '()))
  (cond ((== (car o) 'point)
         (cons o '()))
        ((== (car o) 'text-at)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic "text-at-valign"))
	        (mag (get-graphical-prop 'basic "magnification")))
           (create-graphical-embedding-box o ha va ha va mag 0.1)))
        ((== (car o) 'gr-group)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic "text-at-valign"))
	        (mag (get-graphical-prop 'basic "magnification")))
           (create-graphical-embedding-box
            o ha va "center" "center" mag 0.1)))
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
                                                `(,(cons* 'with
					         "point-style"
					         "square"
					        `((concat .
                                                          ,(cdr l2)))))
                                                l2))
                                          (cons
                                           `(with "point-style"
                                                "square"
                                              ,(list-ref l 1))
					   (curp (list-head l 1))))))) '())
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
    (add-selections-colors op col fcol))
  (define res '())
  (define curscol #f)
  (for (o l)
    (if (tree? o)
        (with path (reverse (tree-ip o))
          (if (== path ptr)
              (set! aobj-selected #t)))))
  (if (and (== pts 'points) ptr)
      (begin
        (set! l (cons (path->tree ptr) l))))
  (for (o l)
    (if (not (and (tree? o) (< (cAr (tree-ip o)) 0)))
        (let* ((props #f)
               (t #f)
               (path0 #f))
          (set! curscol #f)
          (set! on-aobj #f)
          (if (tree? o)
              (with path (reverse (tree-ip o))
                (set! props (create-graphical-props (if (== pts 'points)
                                                        'default path)
                                                    (if (== pts 'object)
                                                        #f "square")))
                (if (== path ptr)
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
                             (asc curscol #f `(,o)))))
                ((== (car o) 'text-at)
                 (if (not curscol)
                     (set! curscol default-color-selected-points))
                 (set! t
                       (let* ((ha (get-graphical-prop path0 "text-at-halign"))
                              (va (get-graphical-prop path0 "text-at-valign"))
                              (mag (get-graphical-prop path0 "magnification"))
			      (gc (asc curscol #f
                                       (create-graphical-embedding-box
                                        o ha va ha va mag 0.1))))
                         (if (== pts 'object-and-points)
                             (cons o gc)
                             (if (== pts 'object)
				 `(,o)
                                 gc)))))
                ((== (car o) 'gr-group)
                 (if (not curscol)
                     (set! curscol default-color-selected-points))
                 (set! t (with gc (asc curscol #f
                                       (let* ((ha (get-graphical-prop
                                                   path0 "text-at-halign"))
                                              (va (get-graphical-prop
                                                   path0 "text-at-valign"))
                                              (mag (get-graphical-prop
                                                    path0 "magnification")))
                                         (create-graphical-embedding-box
                                          o ha va "center" "center" mag 0.1)))
                           (if (== pts 'object-and-points)
                               (cons o gc)
                               (if (== pts 'object)
                                   `(,o)
                                   gc)))))
                (else
                  (set! t (if (== pts 'object-and-points)
                              (cons o
                                    (asc curscol default-color-selected-points
                                         (cdr o)))
                              (if (== pts 'object)
                                  `(,o)
                                  (asc curscol default-color-selected-points
                                       (cdr o)))))))
          (set! res (append res
                            (if props
                                `(,(append props `(,(cons* 'concat t))))
                                t))))))
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
 ;(display* "o[create-graphical-object](" mode "," pts "," no ")=")(write o)(newline)
  (if (pair? no)
      (begin
	 (set! edge (car no))
	 (set! no (cadr no))))
  (if (and o (not (null? o)))
      (let* ((op (add-selections-colors
		    (create-graphical-contour o edge no)
		    default-color-go-points #f))
	     (props (if (and pts (!= pts 'points))
			(create-graphical-props mode #f)
			(create-graphical-props 'default #f)))
             (mag (graphics-get-property "magnification"))
             (mag-o `(with "magnification" ,mag ,o))
         )
	 (graphical-object!
	    (if (or (== no 'group)
		    (and (!= no 'no-group)
			 (graphics-group-mode? (graphics-mode)))
		)
	       `(concat .
                  ,(create-graphical-contours
		      (map (lambda (x)
			      (if (tree? x)
				  (enhanced-tree->radical x)
				  x)
			   )
			   the-sketch)
		      current-path pts)
		)
		(append
		   props
		  `((concat . ,(cond ((== pts 'points) op)
				     ((== pts 'object) `(,mag-o))
				     ((== pts 'object-and-points)
				      (cons mag-o op)))))))))
      (graphical-object! '(concat))))

(tm-define (graphics-decorations-update . parms)
;; Creating the graphical object exclusively from the context
  (invalidate-graphical-object)
  (if (== (length parms) 4)
      (create-graphical-object
	 (car parms) (cadr parms) (caddr parms) (cadddr parms))
      (if (graphics-group-mode? (graphics-mode))
	  (with pts #f
	     (if (null? parms)
		 (set! pts (if sticky-point 'object 'points))
		 (set! pts (car parms)))
	     (if (and (null? (sketch-get))
		      (pair? current-obj)
		      (> (length current-obj) 1))
		 (create-graphical-object current-obj '() 'points 'no-group)
		 (create-graphical-object current-obj current-path pts #f)))
	  (let* ((mode (if (null? parms)
			   (if sticky-point
			      'active
			       current-path)
			   (car parms)))
		 (tag (if current-obj (car current-obj) #f))
	     )
	     (if (and sticky-point
		      (== 1 (length (sketch-get))))
		 (set! current-obj (car (sketch-get))))
	     (if (tree? current-obj)
		 (set! current-obj (tree->stree current-obj)))
	     (if (and (== mode 'active)
		      (pair? current-obj))
		 (begin
		    (graphical-fetch-props 
		       (if (== (car current-obj) 'with)
			   current-obj `(with ,current-obj)))
		    (set! current-obj (stree-radical current-obj))))
	     (create-graphical-object
		current-obj
		mode
		(if sticky-point 'object-and-points 'points)
                (if (== tag 'text-at)
                    1
                    (cond ((or (not tag)
			       (== tag 'gr-group))
                           #f)
                          (else
			     (if current-point-no
				 (if current-edge-sel?
				     current-point-no
				    `(,current-edge-sel? ,current-point-no))
				 #f))))))))
  (invalidate-graphical-object))

(tm-define (graphics-decorations-reset)
  (create-graphical-object #f #f #f #f))

;; Operating on the graphical object
(tm-define (transform-graphical-object opn)
  (with o (tree->stree (get-graphical-object))
     (if (pair? o)
     (begin
	(set! o (opn o))
	(graphical-object! o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing the sketch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-root x)
  (if (and (tree? x) (tree->path x))
      (radical->enhanced-tree x)
      x))

(tm-define (sketch-get)
  the-sketch)

(tm-define (sketch-set! l)
  (set! the-sketch (map get-root l)))

(tm-define (sketch-in? t)
  (set! t (get-root t))
  (seek-eq? t the-sketch))

(tm-define (sketch-reset)
  (set! the-sketch '()))

(tm-define (sketch-toggle t)
  (set! t (get-root t))
  (set! the-sketch
        (if (sketch-in? t)
            (remove-eq? t the-sketch)
            (rcons the-sketch t))))

(tm-define (sketch-transform opn)
  (set! the-sketch (map opn the-sketch))
  (set! current-obj
	(if (graphics-group-mode? (graphics-mode)) '(nothing) #f))
  (set! current-path #f)
  (graphics-decorations-update))

;; TODO: Replace sticky-point by a more appropriate name for
;;   telling whether we are in SELECTING or MODIFYING mode.
(tm-define (sketch-checkout)
  (if sticky-point
      (graphics-error "(sketch-checkout)"))
  (set! layer-of-last-removed-object
	(if (== (length (sketch-get)) 1)
	    #f
	   '()))
  (set! remove-undo-mark? #f)
  (foreach-cons (c (sketch-get))
     (with o (car c)
     (if (tree? o)
     (begin
	(set! remove-undo-mark? #t)
	(graphics-remove (tree->path o) 'memoize-layer)
	(if (or (tree->path o) (tree->path (enhanced-tree->radical o)))
	    (set-car! c (tree-copy o))))))
  )
  (set! sticky-point #t)
  (set! graphics-undo-enabled #f)
  (graphics-decorations-update))

(tm-define (sketch-commit)
  (define group-mode? (graphics-group-mode? (graphics-mode)))
  (if (not sticky-point)
      (graphics-error "(sketch-commit)"))
  (with sketch0 (list-copy (sketch-get))
     (sketch-reset)
     (for (o sketch0)
	(with layer layer-of-last-removed-object
	   (sketch-toggle (path->tree (graphics-group-insert-bis o group-mode?)))
	   (if (not (list? layer))
	       (set! layer-of-last-removed-object layer)))
     )
     (set! layer-of-last-removed-object #f)
     (set! current-obj
	   (if group-mode?
	      '(nothing)
	       (if (== 1 (length (sketch-get)))
		   (stree-radical (tm->stree (car (sketch-get))))
		   #f)))
     (set! current-path
	   (if (and (== 1 (length (sketch-get)))
		    (tree? (car (sketch-get))))
	       (tree->path (car (sketch-get)))
	       #f))
  )
  (set! sticky-point #f)
  (set! graphics-undo-enabled #t)
  (if remove-undo-mark?
      (remove-undo-mark))
  (graphics-decorations-update))

(tm-define (sketch-cancel)
  #t)
