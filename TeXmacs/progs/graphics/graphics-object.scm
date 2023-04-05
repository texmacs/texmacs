
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

(define infinity +inf.0)

(define (is-point? p)
  (and (pair? p)
       (in? (car p) '(point tuple))
       (pair? (cdr p))
       (npair? (cadr p))))

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

(tm-define (object-closest-point-pos obj x y)
  ;;(display* "obj(" x ", " y ")=" obj "\n")
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
         (y2 (s2f (caddr p2))))
    (< (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))) (* eps eps))))

;; Graphical object
(tm-define default-color-go-points "#4040ff")
(tm-define default-color-selected-points "#ff6060")

(tm-define graphical-attrs
  (with t (make-ahash-table)
    (for (var (graphics-all-attributes))
      (ahash-set! t var "default"))
    t))

(tm-define (graphical-fetch-props o)
  (for (var (graphics-all-attributes))
    (ahash-set! graphical-attrs var (find-prop o var "default"))))

(tm-define (graphical-object fetch)
; FIXME: Remove this (tree->stree) should give more speed, but I'm not sure
;   about what is the best way now. Then, directly plug the tree and test
;   the new version of (find-prop) that works directly on trees.
  (with o (tree->stree (get-graphical-object))
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
      val))

(tm-define (get-graphical-prop mode prop)
  (with res
      (cond
	((== mode 'basic)
	 (if sticky-point
	     (get-graphical-prop 'active prop)
	     (get-graphical-prop current-path prop)))
	((== mode 'active)
         (ahash-ref graphical-attrs prop))
	((list? mode)
	 (if (== prop "magnify")
	     (graphics-eval-magnify-at (rcons mode 0))
	     (graphics-path-property mode prop)))
	((== mode 'new)
	 (graphics-get-property (string-append "gr-" prop)))
	(else ;;(== mode 'default)
          (get-default-val (string-append "gr-" prop))))
    (dv prop res)))

(tm-define (create-graphical-props mode ps0)
  ;;(display* "create-graphical-props " mode ", " ps0 "\n")
  (let ((tab (make-ahash-table))
        (l (graphics-all-attributes)))
    (set! l (list-difference l '("gid" "anim-id")))
    (cond
      ((== mode 'active)
       (for (var l)
         (ahash-set! tab var (ahash-ref graphical-attrs var))))
      ((list? mode)
       (for (var l)
         (ahash-set! tab var (graphics-path-property mode var)))
       (ahash-set! tab "magnify"
                   (graphics-eval-magnify-at (rcons mode 0))))
      ((== mode 'new)
       (for (var l)
         (ahash-set! tab var (graphics-get-property (gr-prefix var)))))
      ((== mode 'default)
       (for (var l)
         (ahash-set! tab var (graphics-attribute-default var)))))
    (with ps (ahash-ref tab "point-style")
      (ahash-set! tab "point-style"
                  (if ps0 ps0 (if ps (dv "point-style" ps) "square"))))
    (let* ((l1 (ahash-table->list tab))
           (l2 (map (lambda (x) (list (car x) (dv (car x) (cdr x)))) l1))
           (l3 (apply append l2)))
      (cons 'with l3))))

;; Graphical contours
;;NOTE: This subsection is OK.
(define (create-graphical-embedding-box o ha0 va0 halign valign w hm pp mag)
  (define (create-text-at-handle o)
    (cond ((func? o 'with)
           (create-text-at-handle (cAr o)))
          ((graphical-text-at-context? o)
           `((with "point-style" "disk" 
               ,(cAr o))))
          (else '())))
  (let* ((o1 (with res (if (or (graphical-text-at-context? o)
                               (== (car o) 'gr-group))
                           `(with "text-at-halign" ,ha0
                                  ,(graphics-valign-var o) ,va0
                                  "doc-at-width" ,w
                                  "doc-at-hmode" ,hm
                                  "doc-at-ppsep" ,pp ,o)
			   o)
               `(with "magnify" ,(if (== mag "default") "1" mag) ,res)))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (f2s (min (s2f (car  info0)) (s2f (caddr  info0)))))
	 (b (f2s (min (s2f (cadr info0)) (s2f (cadddr info0)))))
	 (r (f2s (max (s2f (car  info1)) (s2f (caddr  info1)))))
	 (t (f2s (max (s2f (cadr info1)) (s2f (cadddr info1)))))
	 (p0 (frame-inverse `(tuple ,l ,b)))
	 (p1 (frame-inverse `(tuple ,r ,b)))
	 (p2 (frame-inverse `(tuple ,r ,t)))
	 (p3 (frame-inverse `(tuple ,l ,t))))
    ;;(display* "o= " o1 "\n")
    ;;(display* "p= " (cursor-path) ", " (cursor-tree) "\n")
    ;;(display* "b= " l ", " b "; " r ", " t "\n")
    (set-car! p0 'point)
    (set-car! p1 'point)
    (set-car! p2 'point)
    (set-car! p3 'point)
    (with res `((cline ,p0 ,p1 ,p2 ,p3))
      (set! res (append res (create-text-at-handle o)))
      res)))

(define (in-interval? x i1 i2 supop infop)
  (and (supop x i1) (infop x i2)))

(tm-define (on-graphical-embedding-box? x y o eps)
  (set! eps (length-decode eps))
  (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	 (va (get-graphical-prop 'basic (graphics-valign-var o)))
	 (o1 (if (graphical-text-at-context? o)
                 `(with "text-at-halign" ,ha
                        ,(graphics-valign-var o) ,va ,o)
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
(define (create-graphical-contour* o edge no) ;; Point mode
  (define (curp lp)
    (if draw-nonsticky-curp lp '()))
  ;;(display* "Contour for " o "\n")
  (cond ((== (car o) 'point)
         (cons o '()))
        ((graphical-text-at-context? o)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic (graphics-valign-var o)))
                (w  (get-graphical-prop 'basic "doc-at-width"))
                (hm (get-graphical-prop 'basic "doc-at-hmode"))
                (pp (get-graphical-prop 'basic "doc-at-ppsep"))
	        (mag (get-graphical-prop 'basic "magnify")))
           (create-graphical-embedding-box o ha va ha va w hm pp mag)))
        ((== (car o) 'gr-group)
         (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	        (va (get-graphical-prop 'basic "text-at-valign"))
	        (mag (get-graphical-prop 'basic "magnify")))
           (create-graphical-embedding-box
            o ha va "center" "center" "1par" "min" "0fn" mag)))
	((in? (car o) '(anim-edit))
	 (create-graphical-contour* (caddr o) 0 #f))
	((in? (car o) '(anim-static anim-dynamic))
         (let* ((a (or (graphics-anim-frames o) (list (cadr o))))
                (c (map (lambda (x) (create-graphical-contour* x 0 #f)) a)))
           (map (lambda (x) `(concat ,@x)) c)))
	((== (car o) 'with)
	 (create-graphical-contour* (cAr o) 0 #f))
        ((integer? no)
         (let* ((l (list-tail (cdr o) no))
                (ll (length l)))
           (append
            (with h (list-head (cdr o) no)
              (if (and edge
                       (in? (car o) (graphical-closed-curve-tag-list))
                       (== (+ no 1) (length (cdr o))))
                  (cons `(with "point-style"
                             ,(if sticky-point "square" "disk")
                           ,(car h)) (cdr h))
                  h))
            (cons
             (list 'with "point-style" "disk"
                   (cons 'concat
                         (if (< ll 2)
                             (if sticky-point '()
                                 (if edge
                                     (list-head l 1)
                                     (curp (list-head l 1))))
                             (if edge
                                 (with l2 (list-head l 2)
                                   (if sticky-point
                                       `(with "point-style" "square"
                                          (concat ,@(cdr l2)))
                                       l2))
                                 (cons
                                  `(with "point-style" "square"
                                     ,(list-ref l 1))
                                  (curp (list-head l 1))))))) '())
            (if (> ll 2) (list-tail l 2) '()))))
        (else (cdr o))))

(define (compress l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((null? (cddr l)) (cdr l))
        (else (cons (car l) (compress (cddr l))))))

(define (compress* l)
  (if (<= (length l) 50) l
      (compress* (compress l))))

(define (create-graphical-contour o edge no)
  (if (> (length o) 50)
      (let* ((o2 (cons (car o) (compress (cdr o))))
             (no2 (if (integer? no) (quotient no 2) no)))
        (create-graphical-contour o2 edge no2))
      (create-graphical-contour* o edge no)))

;; Graphical contours (group mode)
;;NOTE: This subsection is OK
(define (add-selections-colors op color fill-color)
  (if (not color) (set! color "none"))
  (if (not fill-color) (set! fill-color "none"))
  `((with "color" ,color
      "point-style" "square"
      "fill-color" ,fill-color
      (concat ,@op))))

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
  ;;(display* "create-graphical-contours " l ", " ptr ", " pts "\n")
  (for (o l)
    (if (tree? o)
        (with path (reverse (tree-ip o))
          (if (== path ptr)
              (set! aobj-selected #t)))))
  (if (and (== pts 'points) ptr)
      (begin
        (set! l (cons (path->tree ptr) l))))
  (set! l (append-map (lambda (x)
                        (or (graphics-anim-radicals x) (list x))) l))
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
                (if (or (== path ptr)
                        (and (list? path) (list? ptr)
                             (list-starts? path ptr)))
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
                ((graphical-text-at-context? o)
                 (if (not curscol)
                     (set! curscol default-color-selected-points))
                 (set! t
                       (let* ((valign-var (graphics-valign-var o))
                              (ha (get-graphical-prop path0 "text-at-halign"))
                              (va (get-graphical-prop path0 valign-var))
                              (w  (get-graphical-prop path0 "doc-at-width"))
                              (hm (get-graphical-prop path0 "doc-at-hmode"))
                              (pp (get-graphical-prop path0 "doc-at-ppsep"))
                              (mag (get-graphical-prop path0 "magnify"))
			      (gc (asc curscol #f
                                       (create-graphical-embedding-box
                                        o ha va ha va w hm pp mag))))
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
                                                    path0 "magnify")))
                                         (create-graphical-embedding-box
                                          o ha va "center" "center"
                                          "1par" "min" "0fn" mag)))
                           (if (== pts 'object-and-points)
                               (cons o gc)
                               (if (== pts 'object)
                                   `(,o)
                                   gc)))))
                (else
                  (set! t (if (== pts 'object-and-points)
                              (cons o
                                    (asc curscol default-color-selected-points
                                         (compress* (cdr o))))
                              (if (== pts 'object)
                                  `(,o)
                                  (asc curscol default-color-selected-points
                                       (compress* (cdr o))))))))
          (set! res (append res
                            (if props
                                `(,(append props `(,(cons* 'concat t))))
                                t))))))
  res)

;; Create graphical object
;;NOTE: This subsection is OK

(define (get-local-magnify)
  (let* ((m1 (graphics-get-property "magnify"))
         (m2 (and graphical-attrs (ahash-ref graphical-attrs "magnify"))))
    (number->string (* (magnify->number m1) (magnify->number m2)))))

(tm-define (create-graphical-object o mode pts no)
  ;; o    == the objet one wants to draw
  ;; mode == 'active, 'new, <path>, etc. (cf. "Graphical props" above).
  ;; pts  == points, object, object-and-points.
  ;; no   == description of the edge  <=> no | (edge no)
  ;;      == flag used in group modes <=> 'group | 'no-group
  (define edge #t)
  ;;(display* "o[create-graphical-object](" mode "," pts "," no ")=")(write o)(newline)
  (if (graphical-text-arg-context? o)
      (set! o #f))
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
             (mag-o `(with "magnify" ,(get-local-magnify) ,o)))
        ;;(display* "-------\n")
        ;;(display* "o= " o ", mode= " mode ", pts= " pts ", op= " op "\n")
        ;;(display* "no= " no ", props= " props "\n")
        (when (== (car (graphics-mode)) 'hand-edit)
          (set! op (list `(concat))))
        (graphical-object!
         (if (or (== no 'group)
                 (and (!= no 'no-group)
                      (graphics-group-mode? (graphics-mode))))
             `(with "magnify" ,(number->string
                                (magnify->number
                                 (graphics-get-property "magnify")))
                (concat ,@(create-graphical-contours
                           (map (lambda (x)
                                  (if (tree? x)
                                      (enhanced-tree->radical x)
                                      x))
                                the-sketch)
                           current-path pts)))
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
		 (tag (if current-obj (car current-obj) #f)))
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
                  (set! current-obj (stree-radical* current-obj #f))))
            (create-graphical-object
             current-obj
             mode
             (if sticky-point 'object-and-points 'points)
             (if (graphical-text-tag? tag)
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
                (set-car! c (tree-copy o)))))))
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
            (set! layer-of-last-removed-object layer))))
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
              #f)))
  (set! sticky-point #f)
  (set! graphics-undo-enabled #t)
  (if remove-undo-mark?
      (remove-undo-mark))
  (graphics-decorations-update))

(tm-define (sketch-cancel)
  #t)
