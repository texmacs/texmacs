
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-graphics)
  (:export
    ;; making graphics and setting global graphics properties
    make-graphics
    graphics-set-property graphics-remove-property
    graphics-set-unit graphics-set-unit-ia
    graphics-set-origin graphics-set-origin-ia
    graphics-set-extents-ia
    graphics-set-mode graphics-set-color graphics-set-line-width
    ;; call-backs
    graphics-move-point graphics-insert-point
    graphics-remove-point graphics-last-point
    graphics-reset-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-at p)
  (tree->stree (tm-subtree p)))

(define (graphics-graphics-path)
  ;; path to innermost graphics tag
  (let* ((p (cDr (tm-where)))
	 (t (stree-at p)))
    (if (func? t 'graphics) p
	(with q (search-upwards "graphics")
	  (if (null? q) #f q)))))

(define (graphics-path path)
  (if (or (null? path) (null? (cdr path)))
      #f
      (with p (cDr path)
         (with o (stree-at p)
            (if (and (pair? o)
		     (in? (car o)
			  '(point line cline spline cspline arc text-at)))
                p
                (graphics-path (cDr path)))))))

(define (graphics-active-path)
  ;; path to active tag
  (graphics-path (tm-where)))

(define (graphics-group-path)
  ;; path to innermost group
  (graphics-graphics-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global geometry of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-go-to
   '(with "gr-mode" "point"
          "gr-frame" (tuple "scale" "1cm" (tuple "0.5par" "0cm"))
	  "gr-clip"  (tuple "clip"
			    (tuple "0par" "-0.3par")
			    (tuple "1par" "0.3par"))
      (graphics))
   '(6 1)))

(define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (tm-insert-with p var val))))

(define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (tm-remove-with p var))))

(define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :2))
	frame
	'(tuple "scale" "1cm" (tuple "0.5par" "0cm")))))

(define (graphics-set-unit u)
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-set-unit-ia)
  (interactive '("Graphical unit:") 'graphics-set-unit))

(define (graphics-set-origin x y)
  (with frame (graphics-cartesian-frame)
    (with new-frame (append (cDr frame) `((tuple ,x ,y)))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-set-origin-ia)
  (interactive
    '("Origin's x-coordinate:" "Origin's y-coordinate:")
    'graphics-set-origin))

(define (graphics-set-extents-ia)
  (interactive
    '("Left corner:" "Bottom corner:" "Right corner:" "Top corner:")
    '(lambda (l b r t)
       (with clip `(tuple "clip" (tuple ,l ,b) (tuple ,r ,t))
	 (graphics-set-property "gr-clip" clip)))))

(define (graphics-mode)
  (with m (tree->stree (get-env-tree "gr-mode"))
     (cond ((string? m)
	   `(edit ,(string->symbol m)))
	   ((pair? m)
	    (map string->symbol (cdr m))))))

(define (graphics-set-mode val)
  (graphics-group-start)
  (graphics-set-property "gr-mode"
     (cond ((or (symbol? val) (string? val))
	    (list 'tuple 'edit val))
	   ((pair? val)
	    (cons 'tuple val)))))

(define (graphics-set-color val)
  (graphics-set-property "gr-color" val))

(define (graphics-set-line-width val)
  (graphics-set-property "gr-line-width" val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-enrich-filter l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter (cdr l))))
	(if (== (cadr head) "default") tail
	    (cons* (car head) (cadr head) tail)))))

(define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(define (graphics-enrich-bis t color lw)
  (let* ((mode (car t)))
    (cond ((== mode 'point)
	   (graphics-enrich-sub t `(("color" , color))))
	  ((in? mode '(line cline spline cspline))
	   (graphics-enrich-sub t `(("color" , color) ("line-width" ,lw))))
	  (else
	   (graphics-enrich-sub t '())))))

(define (graphics-enrich t)
  (let* ((color (get-env "gr-color"))
	 (lw (get-env "gr-line-width")))
    (graphics-enrich-bis t color lw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the innermost group of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-group-insert-bis t go-into)
  (with p (graphics-group-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (tm-insert (rcons p n) (list 'tuple t))
	    (if (func? t 'with)
		(tm-go-to (append p (list n (- (length t) 2) 1)))
		(if (and go-into (func? t 'text-at))
		    (tm-go-to (append p (list n 0 0)))
		    (tm-go-to (append p (list n 1)))))))))

(define (graphics-group-insert t)
  (graphics-group-insert-bis t #t))

(define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(define (graphics-group-enrich-insert-bis t color lw go-into)
  (graphics-group-insert-bis (graphics-enrich-bis t color lw) go-into))

(define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (tm-go-to (rcons p 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the active tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-object path)
  (with p (graphics-path path)
    (if p (tree->stree (tm-subtree p)) #f)))

(define (graphics-active-object)
  (with p (graphics-active-path)
    (if p (tree->stree (tm-subtree p)) #f)))

(define (graphics-active-type)
  (with t (graphics-active-object)
    (if t (car t) #f)))

(define (graphics-active-color)
  (get-env "color"))

(define (graphics-active-lwidth)
  (get-env "line-width"))

(define (graphics-active-property-bis var default-val)
  (with c (get-env var)
    (if (== c "") default-val c)))

(define (graphics-active-property var)
  (graphics-active-property-bis var "default"))

(define (search-upwards-from p tag)
  (if (null? p)
     '() 
      (with o (stree-at p)
         (if (func? o tag)
             p
             (search-upwards-from (cDr p) tag)))))

(define nothing (gensym))
(define (find-prop l var)
  (define (find l)
     (if (or (null? l) (null? (cdr l)))
         nothing
         (if (== (car l) var)
             (cadr l)
             (if (null? (cdr l))
                 nothing
                 (find (cddr l)))))
  )
  (if (null? l)
      nothing
      (find (cdr l))))

(define (find-prop-bis l var default)
  (with val (find-prop l var)
     (if (== val nothing)
         default
         val)))

(define (get-upwards-property p var)
  (if (null? p)
      nothing
      (with q (search-upwards-from p 'with)
         (if (null? q)
             nothing
             (with val (find-prop (stree-at q) var)
                (if (== val nothing)
                    (get-upwards-property (cDr q) var)
                    val))))))

(define (graphics-path-property-bis p var default-val)
  (with c (get-upwards-property p var)
    (if (== c nothing) default-val c)))

(define (graphics-path-property p var)
  (graphics-path-property-bis p var "default"))

(define (graphics-active-assign t)
  (with p (graphics-active-path)
    (if p (begin
	    (tm-assign p t)
	    (tm-go-to (rcons p 1))))))

(define (graphics-active-set-tag l)
  (with t (graphics-active-object)
    (if t (graphics-active-assign (cons l (cdr t))))))

(define (graphics-active-insert t)
  (with p (graphics-active-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (tm-insert (rcons p n) (list 'tuple t))
	    (tm-go-to (rcons p 1))))))

(define (graphics-remove p)
  (let* ((q (search-upwards-from p 'with))
         (path (rcons (if (and (not (null? q))
                               (== (+ (length q) 1) (length p)))
                          q p
                      )
                      1
               )))
        (tm-remove (cDr path) (cAr path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for calculating with the graphical object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Closest point
(define (s2i s)
  (exact->inexact (string->number s)))

(define (i2s s)
  (number->string s))

(define (graphics-norm p1 p2)
   (let ((x (- (s2i (cadr p2)) (s2i (cadr p1))))
         (y (- (s2i (caddr p2)) (s2i (caddr p1)))))
        (sqrt (+ (* x x) (* y y)))))

(define (graphics-closest-point-pos-bis p l)
   (if (null? l)
       '()
       (let ((n1 (graphics-norm p (car l))))
            (if (null? (cdr l))
                l
                (let* ((p2 (graphics-closest-point-pos-bis p (cdr l)))
                       (n2 (graphics-norm p (car p2))))
                      (if (<= n1 n2) l p2))))))

(define (graphics-closest-point-pos p l)
   (- (length l) (length (graphics-closest-point-pos-bis p l))))

(define (object-closest-point-pos obj x y)
  (if (pair? obj)
      (with type (car obj)
         (if (== type 'point)
             0
         (if (in? (car obj) '(line cline spline cspline))
             (graphics-closest-point-pos (list 'point x y) (cdr obj))
         0)))
      0))

;; Graphical object
(define graphical-color "default")
(define graphical-lwidth "default")

(define (graphical-object fetch)
  (with o (tree->stree (get-graphical-object))
    (if (and fetch (pair? o))
       (begin
          (set! graphical-color (find-prop-bis o "color" "default"))
          (set! graphical-lwidth (find-prop-bis o "line-width" "default"))))
    (if (pair? o)
        (if (== (cdr o) '()) o (car (list-tail o 7)))
        '(concat))))
            
(define (create-graphical-contour o halign valign len)
  (define (create-haligns l b r t)
     (with x (cond ((== halign "left") l)
		   ((== halign "center") (i2s (/ (+ (s2i l) (s2i r)) 2)))
		   ((== halign "right") r)
		   (else l))
       `((line (point ,x ,b) (point ,x ,(i2s (- (s2i b) len))))
	 (line (point ,x ,t) (point ,x ,(i2s (+ (s2i t) len))))))
  )
  (define (create-valigns l b r t)
     (with y (cond ((== valign "bottom") b)
		   ((== valign "center") (i2s (/ (+ (s2i b) (s2i t)) 2)))
		   ((== valign "top") t)
		   (else b))
       `((line (point ,l ,y) (point ,(i2s (- (s2i l) len)) ,y))
	 (line (point ,r ,y) (point ,(i2s (+ (s2i r) len)) ,y))))
  )
  (let* ((info0 (cdr (tree->stree (box-info (stree->tree o) "lbLB"))))
	 (info1 (cdr (tree->stree (box-info (stree->tree o) "rtRT"))))
	 (l (i2s (min (s2i (car  info0)) (s2i (caddr  info0)))))
	 (b (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
	 (r (i2s (max (s2i (car  info1)) (s2i (caddr  info1)))))
	 (t (i2s (max (s2i (cadr info1)) (s2i (cadddr info1)))))
	 (p0 (tree->stree (frame-inverse (stree->tree `(tuple ,l ,b)))))
	 (p1 (tree->stree (frame-inverse (stree->tree `(tuple ,r ,b)))))
	 (p2 (tree->stree (frame-inverse (stree->tree `(tuple ,r ,t)))))
	 (p3 (tree->stree (frame-inverse (stree->tree `(tuple ,l ,t)))))
        )
	(with res `((line ,p0 ,p1) (line ,p1 ,p2)
		    (line ,p2 ,p3) (line ,p3 ,p0)
		   )
		   (if halign (set! res (append res
				 (create-haligns (cadr p0) (caddr p0)
						 (cadr p1) (caddr p2)))))
		   (if valign (set! res (append res
				 (create-valigns (cadr p0) (caddr p0)
						 (cadr p1) (caddr p2)))))
		   res)))

(define (create-graphical-object o mode pts)
  (if o (with op
	      (cond ((== (car o) 'point) ;; FIXME : doesnt work for arcs
		     (cons o '())
		    )
		    ((== (car o) 'text-at)
		     (with a (cdddr o)
			(create-graphical-contour o (car a) (cadr a) 0.1))
		    )
		    (else (cdr o)))
	  (let ((color #f)
		(lw #f))
                 (if (== mode 'active)
                     (begin
		       (set! color graphical-color)
		       (set! lw graphical-lwidth)))
                 (if (list? mode)
                     (begin
		       (set! color (graphics-path-property mode "color"))
		       (set! lw (graphics-path-property mode "line-width"))))
                 (if (== mode 'new)
                     (begin
		       (set! color (get-env "gr-color"))
		       (set! lw (get-env "gr-line-width"))))
                 (set-graphical-object
		  (stree->tree
		   (list 'with "point-style" "square"
			 "color" color
			 "line-width" lw
			 (cons 'concat
			       (cond ((== pts 'points) op)
				     ((== pts 'object) `(,o))
				     ((== pts 'object-and-points)
				      (cons o op)))))))))
      (set-graphical-object (stree->tree '(concat)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State variables & history for the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Past events
(define past-events #('() '()))
(define (event-exists! o e)
  (vector-set!
     past-events 0
     (cons (list o e) (vector-ref past-events 0))))

(define (event-exists? o e t)
  (define (seek-event l)
     (if (or (null? l) (not (pair? (car l))))
	 #f
	 (with x (car l)
	    (if (and (eq? (car x) o) (eq? (cadr x) e))
		#t
		(seek-event (cdr l)))))
  )
  (if (in? t '(0 1))
      (seek-event (vector-ref past-events t))
      #f))

(define (events-clocktick)
  (vector-set! past-events 1 (vector-ref past-events 0))
  (vector-set! past-events 0 '()))

(define (events-forget)
  (set! past-events (make-vector 2))
  (vector-set! past-events 0 '())
  (vector-set! past-events 1 '()))

;; State variables
(define sticky-point #f)
(define current-point-no #f)

(define state-slots
  ''(graphics-action
     graphical-object
     sticky-point
     current-point-no))

(define-macro (state-len)
  `(length ,state-slots))

(define (new-state)
  (make-vector (state-len)))

(define-macro (state-slot-ref i)
  `(- (length (memq ,i ,state-slots)) 1))

(define-macro (state-ref st var)
  `(vector-ref ,st (state-slot-ref ,var)))

(define-macro (state-set! st var val)
  `(vector-set! ,st (state-slot-ref ,var) ,val))

(define (graphics-state-get)
  (with st (new-state)
    (state-set! st 'graphics-action #f)
    (state-set! st 'graphical-object (get-graphical-object))
    (state-set! st 'sticky-point sticky-point)
    (state-set! st 'current-point-no current-point-no)
    st))

(define (graphics-state-set st)
  (with o (state-ref st 'graphical-object)
    (if (pair? (tree->stree o))
	(set-graphical-object o)
	(create-graphical-object #f #f #f)))
  (set! sticky-point (state-ref st 'sticky-point))
  (set! current-point-no (state-ref st 'current-point-no)))

;; State stack
(define graphics-states '())

(define (graphics-states-void?)
  (null? graphics-states))

(define (graphics-push-state st)
  (set! graphics-states (cons st graphics-states)))

(define (graphics-pop-state)
  (if (graphics-states-void?)
      (display* "(graphics-pop-state)::void(graphics-states)\n")
      (with st (car graphics-states)
	(set! graphics-states (cdr graphics-states))
	st)))

;; User interface
(define graphics-first-state #f)

(define (graphics-store-first action)
  (if graphics-first-state
      (display* "(graphics-store-first)::!void(graphics-store-first)\n"))
  (set! graphics-first-state (graphics-state-get))
  (state-set! graphics-first-state 'graphics-action action))

(define (graphics-back-first)
  (graphics-state-set graphics-first-state)
  (set! graphics-first-state #f))

(define (graphics-store-state first?)
  (if first?
      (graphics-store-first first?)
      (graphics-push-state (graphics-state-get))))

(define (graphics-back-state first?)
  (if first?
      (graphics-back-first)
      (with st (graphics-pop-state)
        (graphics-state-set st)
        (if (graphics-states-void?)
            (graphics-push-state st)))))

(define (graphics-reset-state)
  (create-graphical-object #f #f #f)
  (set! sticky-point #f)
  (set! current-point-no #f))

(define (graphics-forget-states)
  (set! graphics-first-state #f)
  (set! graphics-states '())
  (events-forget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for using and maintaining the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (with-graphics-context msg x y path obj no . body)
  `(if sticky-point
       (with o (graphical-object #t)
	 (if (and (pair? o) (not (null? (cdr o))))
	     (let* ((,obj (cadr o))
		    (,no current-point-no)
		    (,path (cDr (tm-where))))
	       ,(cons 'begin body))
	     (if (not (and (string? ,msg) (== (substring ,msg 0 1) ";")))
		 (display* "Uncaptured " ,msg " " ,x ", " ,y "\n"))))
       (let* ((pxy (path-xy (s2i ,x) (s2i ,y)))
              (,path (graphics-path pxy))
              (,obj (graphics-object pxy)))
	 (if ,obj
	     (let* ((,no (object-closest-point-pos ,obj ,x ,y)))
	       ,(cons 'begin body))
	     (if (not (and (string? ,msg) (== (substring ,msg 0 1) ";")))
		 (display* "Uncaptured " ,msg " " ,x ", " ,y "\n"))))))

(define (graphics-reset-context cmd)
  ;; cmd in { begin, exit, undo }
  ;; (display* "Graphics] Reset-context " cmd "\n")
  (cond
   ((and (in? cmd '(begin exit)) (or (== cmd 'begin) (not sticky-point)))
    (graphics-reset-state)
    (graphics-forget-states)
    (with p (graphics-active-path)
       (if p (create-graphical-object (graphics-active-object) p  'points))))
   ((and (== cmd 'exit) sticky-point)
    (if graphics-first-state
	(begin
	  (if (eq? (state-ref graphics-first-state 'graphics-action)
		   'start-move)
	      (undo))))
    (graphics-reset-state)
    (graphics-forget-states))
   ((== cmd 'undo)
    (if (not sticky-point)
	(with p (graphics-active-path)
	  (if p
	      (create-graphical-object (graphics-active-object) p  'points)
	      (create-graphical-object #f #f #f))))
    (set! sticky-point #f)
    (set! current-point-no #f)
    (if graphics-first-state
	(begin
	  (if (eq? (state-ref graphics-first-state 'graphics-action)
		   'start-create)
	      (redo))
	  (graphics-back-first)))
    (graphics-forget-states))
   (else (display* "Uncaptured reset-context " cmd "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (dispatch obj vals func vars . parms)
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
    ,(append (cons
	     'cond
	     (map cond-case vals))
	    `((else (display* "Uncaptured " ',func " " ,obj "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Left button
(define (point_left-button x y p obj no)
  (if sticky-point
      (with l (list-tail (cdr obj) no)
	(graphics-store-state #f)
	(set-cdr! l (cons `(point ,x ,y) (cdr l)))
	(create-graphical-object obj 'active 'object-and-points)
	(set! current-point-no (+ no 1))
	(set! sticky-point #t))
      (begin
	(graphics-store-state 'start-move)
	(create-graphical-object obj p 'object-and-points)
	(graphics-remove p)
	(set! sticky-point #t)
	(set! current-point-no no)
	(graphics-store-state #f))))

(define (text-at_left-button x y p obj no)
  (if sticky-point
      (display "Text-at left-button not implemented\n")
      (begin
	 (if (event-exists? 'text-at 'left-button 1)
	     (tm-go-to (path-xy (s2i x) (s2i y)))))))

;; Move
(define (point_move x y p obj no)
  (if sticky-point
      (begin
	(if (== (car obj) 'point)
	    (set! obj `(point ,x ,y))
	    (set-car! (list-tail (cdr obj) no) `(point ,x ,y)))
	(create-graphical-object obj 'active 'object-and-points))
      (begin
	(create-graphical-object obj p 'points)
	(tm-go-to (rcons p 1)))))

(define (text-at_move x y p obj no)
  (if (and (not sticky-point) (event-exists? 'text-at 'left-button 1))
      (point_left-button x y p obj 1)
      (point_move x y p obj 1)))

;; Middle button
(define (point_middle-button x y p obj no)
  (if sticky-point
      ;;Back
      (begin
	(graphics-back-state #f)
	(graphics-move-point x y))
      ;;Remove
      (begin
	(if (or (in? (car obj) '(point text-at)) (null? (cddr obj)))
	    (begin
	      (graphics-remove p)
	      (create-graphical-object #f #f #f)
	      (graphics-group-start))
	    (with l (if (<= no 0) obj (list-tail (cdr obj) (- no 1)))
	      (set-cdr! l (cddr l))
	      (create-graphical-object obj p 'points)
	      (graphics-active-assign obj)))
	(set! sticky-point #f))))

;; Right button (last)
(define (point_sticky-right-button x y p obj no)
  (create-graphical-object obj 'active 'points)
  (graphics-group-enrich-insert-bis
   obj graphical-color graphical-lwidth #f)
  (set! sticky-point #f)
  (graphics-forget-states))

;; Right button (create)
(define (point_nonsticky-right-button x y mode)
  (graphics-group-enrich-insert `(point ,x ,y)))

(define (line_nonsticky-right-button x y mode)
  (with o `(,mode (point ,x ,y) (point ,x ,y))
    (graphics-store-state 'start-create)
    (create-graphical-object o 'new 'object-and-points)
    (set! current-point-no 1)
    (set! sticky-point #t)
    (graphics-store-state #f)))

(define (text-at_nonsticky-right-button x y mode)
  (graphics-group-enrich-insert
    `(text-at "" (point ,x ,y)
       ,(graphics-active-property-bis "gr-text-halign" "left")
       ,(graphics-active-property-bis "gr-text-valign" "bottom"))))

;; Dispatch
(define (edit_left-button x y)
  (with-graphics-context
   "insert" x y p obj no
   (dispatch (car obj) ((point line cline spline cspline)
			(text-at))
	     left-button (x y p obj no) do-tick)))

(define (edit_move x y)
  (with-graphics-context
   ";move" x y p obj no
   (dispatch (car obj) ((point line cline spline cspline)
			(text-at))
	     move (x y p obj no) do-tick)))

(define (edit_middle-button x y)
  (with-graphics-context
   "remove" x y p obj no
   (dispatch (car obj) ((point line cline spline cspline text-at))
	     middle-button (x y p obj no) do-tick)))

(define (edit_right-button x y)
  (if sticky-point
      (with-graphics-context
       "last" x y p obj no
       (dispatch (car obj) ((point line cline spline cspline text-at))
		 sticky-right-button (x y p obj no) do-tick))
      (with mode (cadr (graphics-mode))
	(dispatch mode ((point)
			(line cline spline cspline)
			(text-at))
		  nonsticky-right-button (x y mode) do-tick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit properties mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Left button
(define (point_assign-props p obj)
  (graphics-remove p)
  (graphics-group-enrich-insert-bis
     obj (get-env "gr-color") (get-env "gr-line-width") #f)
  (create-graphical-object obj 'new 'points))

(define (text-at_change-halign p obj)
  (graphics-remove p)
  (with halign (cadddr obj)
     (set-car! (cdddr obj) (cond ((== halign "left") "center")
				 ((== halign "center") "right")
				 ((== halign "right") "left")
				 (else "left")))
     (graphics-group-insert-bis obj #f)
     (create-graphical-object obj '() 'points)))

(define (text-at_assign-props p obj)
  (text-at_change-halign p obj))

;; Right button
(define (text-at_change-valign p obj)
  (graphics-remove p)
  (with valign (car (cddddr obj))
     (set-car! (cddddr obj) (cond ((== valign "bottom") "center")
				  ((== valign "center") "top")
				  ((== valign "top") "bottom")
				  (else "bottom")))
     (graphics-group-insert-bis obj #f)
     (create-graphical-object obj '() 'points)))

;; Dispatch
(define (edit-prop_move x y)
  (edit_move x y))

(define (edit-prop_left-button x y)
  (with-graphics-context "assign-props" x y p obj no
     (dispatch (car obj) ((point line cline spline cspline)
			  (text-at))
	       assign-props (p obj) do-tick)))

(define (edit-prop_right-button x y)
  (with-graphics-context "change-valign" x y p obj no
     (dispatch (car obj) ((text-at))
               change-valign (p obj) do-tick)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    left-button (x y)))

(define (graphics-move-point x y)
  ;(display* "Graphics] Move " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    move (x y)))

(define (graphics-remove-point x y)
  ;(display* "Graphics] Remove " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit))
	    middle-button (x y)))

(define (graphics-last-point x y)
  ;(display* "Graphics] Last " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    right-button (x y)))

(define (graphics-finish)
  ;;(display* "Graphics] Finish\n")
  (with mode (graphics-mode)
    (cond ((== (car mode) 'edit)
	  (with submode (cadr mode)
	     (cond ((== submode 'point) (noop))
		   ((in? submode '(line cline spline cspline)) (noop))
		   ((== submode 'text-at) (noop))
		   (else (display* "Uncaptured finish (edit)\n")))))
	 ((== (car mode) 'edit-prop)
	   (noop))
	  (else (display* "Uncaptured finish\n")))))
