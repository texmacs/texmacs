
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-main.scm
;; DESCRIPTION : routines concerning the graphics as a whole (grid,
;;                                           [ change extents, etc.)
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004-2007  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-main)
  (:use (graphics graphics-utils)))

;; TODO: Have a look at if there is still some part of the previous
;;   mess due to unsafe synchro (fetching grids, egrid-as-vgrid?, etc.)

;; REMARK: Otherwise, except for some details described in FIXMEs comments
;;   below, this code is clean.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global properties of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-geometry)
  (with geo (tree->stree (get-env-tree "gr-geometry"))
    (if (match? geo '(tuple "geometry" :%2))
	(append geo '("center"))
	(if (match? geo '(tuple "geometry" :%3))
	    geo
	    '(tuple "geometry" "1par" "0.6par" "center")))))

(tm-define (graphics-set-width w)
  (:argument w "Width of the graphics")
  (let* ((geo (graphics-geometry))
	 (align (if (>= (length geo) 5) (cAr geo) "center"))
	 (new-geo `(tuple "geometry" ,w ,(cadddr geo) ,align))
      )
      (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-set-height h)
  (:argument h "Height of the graphics")
  (let* ((geo (graphics-geometry))
	 (align (if (>= (length geo) 5) (cAr geo) "center"))
	 (new-geo `(tuple "geometry" ,(caddr geo) ,h ,align))
      )
      (graphics-set-property "gr-geometry" new-geo)))

(define (geo-valign-has-value? val)
  (let* ((geo (graphics-geometry))
	 (align (car (cddddr geo)))
      )
      (== val align)))

(tm-define (graphics-set-geo-valign a)
  (:argument a "Alignment of the graphics")
  (:check-mark "*" geo-valign-has-value?)
  (let* ((geo (graphics-geometry))
	 (new-geo `(tuple "geometry" ,(caddr geo) ,(cadddr geo) ,a))
      )
      (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-set-extents w h)
  (:argument w "Width of the graphics")
  (:argument h "Height of the graphics")
  (let* ((geo (graphics-geometry))
	 (align (if (>= (length geo) 5) (cAr geo) "center"))
	 (new-geo `(tuple "geometry" ,w ,h ,align))
      )
      (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :%2))
	frame
	'(tuple "scale" "1cm" (tuple "0.5par" "0cm")))))

(define (graphics-unit-has-value? val)
  (let* ((fr (graphics-cartesian-frame))
	 (unit (caddr fr))
     )
     (== val unit)))

(tm-define (graphics-set-unit u)
  (:argument u "Graphical unit")
  (:check-mark "*" graphics-unit-has-value?)
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-origin-has-value? x y)
  (let* ((fr (graphics-cartesian-frame))
	 (orig (cAr fr))
     )
     (if (pair? x)
	 (set! x (length-add (cadr x) (caddr x))))
     (if (pair? y)
	 (set! y (length-add (cadr y) (caddr y))))
   ; FIXME: The 2 (if)s above lack perfection...
     (== `(tuple ,x ,y) orig)))

(tm-define (graphics-set-origin x y)
  (:argument x "Origin's x-coordinate")
  (:argument y "Origin's y-coordinate")
  (:check-mark "*" graphics-origin-has-value?)
  (with frame (graphics-cartesian-frame)
    (with new-frame (append (cDr frame) `((tuple ,x ,y)))
      (graphics-set-property "gr-frame" new-frame))))

(tm-define (length-extract-unit len)
  (define l (reverse (string->list len)))
  (define (traverse l)
     (if (pair? l)
	 (if (char-alphabetic? (car l))
	     (traverse (cdr l))
	     (set-cdr! l '())))
  )
  (traverse l)
  (set! l (reverse l))
  (if (and (pair? l) (not (char-alphabetic? (car l))))
      (set! l (cdr l))
  )
  (list->string l)) ;; TODO: Move this in the utils (?)

(tm-define (graphics-zoom e)
  (let* ((fr (graphics-cartesian-frame))
         (u (caddr fr))
         (newu (length-mult e u))
         (newud (length-decode newu))

         (x1 (cadr (cadddr fr)))
         (y1 (caddr (cadddr fr)))
         (x2 (length-add x1 "-0.5gw"))
         (y2 (length-add y1 "-0.5gh"))
         (x3 (length-mult e x2))
         (y3 (length-mult e y2))
         (x4 (length-add x3 "0.5gw"))
         (y4 (length-add y3 "0.5gh"))
         (x5 (if (and (string? x4) (string-ends? x4 "gw")) x4 x1))
         (y5 (if (and (string? y4) (string-ends? y4 "gh")) y4 y1))

         (newfr `(tuple "scale" ,newu (tuple ,x4 ,y4))))            
     (if (and (> newud 100) (< newud 10000000))
     (with magn (multiply-magnification
                   (graphics-get-property "magnification") e)
        (graphics-decorations-reset)
        (graphics-set-property "gr-frame" newfr)
        (if magn  
            (graphics-set-property "magnification" magn))))))

(tm-define (graphics-move-origin dx dy)
  (define (add l1 l2)
     (if (pair? l1)
        `(tmlen ,(f2s (+ (s2f (cadr l1)) (length-decode l2))))
         (length-add l1 l2))
  )  
  (let* ((fr (graphics-cartesian-frame))
         (x (cadr (cadddr fr)))
         (y (caddr (cadddr fr)))
         (newfr `(tuple "scale" ,(caddr fr)
                                 (tuple ,(add x dx)
                                        ,(add y dy))))
     )         
     (graphics-decorations-reset)
     (graphics-set-property "gr-frame" newfr)))

(tm-define (graphics-change-extents dw dh)
  (let* ((geo (graphics-geometry))
         (w (caddr geo))
         (h (cadddr geo)) 
         (w2 (length-add w dw))
         (h2 (length-add h dh))
     )
     (if (> (length-decode w2) 0)
         (set! w w2))
     (if (> (length-decode h2) 0)
         (set! h h2))
     (graphics-decorations-reset)
     (graphics-set-extents w h)))

(tm-define (graphics-change-geo-valign dirn)
  (let* ((geo (graphics-geometry))
         (a (car (cddddr geo)))
     )
     (graphics-set-geo-valign
        (if dirn
            (cond ((== a "top") "bottom")
                  ((== a "center") "top")
                  ((== a "bottom") "center")
                  (else "default"))
            (cond ((== a "top") "center")
                  ((== a "center") "bottom")
                  ((== a "bottom") "top")
                  (else "default"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define graphics-current-type #f)
(tm-define graphics-current-center #f)
(tm-define graphics-current-step #f)
(tm-define graphics-current-astep #f)
(tm-define graphics-current-base #f)

(tm-define egrid-as-vgrid? #t)

;; Fetching/Setting a grid
(define default-polar-astep 24)
(define default-polar-nsubd 10)
(define (graphics-fetch-grid-vars type visual?)
  (set! graphics-current-type (if type (symbol->string type) "empty"))
  (set! graphics-current-center '(point "0" "0"))
  (set! graphics-current-step
	(if (or visual?
		(== graphics-current-type "logarithmic")) "1" "0.1"))
  (set! graphics-current-astep
	(f2s (if visual?
		 default-polar-astep
		 (* default-polar-astep default-polar-nsubd))))
  (set! graphics-current-base "10")
  (with grid (tree->stree (get-env-tree (if visual? "gr-grid" "gr-edit-grid")))
    (cond ((match? grid '(tuple "empty"))
	   (set! graphics-current-type "empty")
	  )
	  ((match? grid '(tuple "cartesian"))
	   (set! graphics-current-type "cartesian")
	  )
	  ((match? grid '(tuple "cartesian" :%1))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "cartesian" :%2))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	  )
	  ((match? grid '(tuple "polar"))
	   (set! graphics-current-type "polar")
	  )
	  ((match? grid '(tuple "polar" :%1))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "polar" :%2))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-astep (list-ref grid 3))
	  )
	  ((match? grid '(tuple "polar" :%3))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-astep (list-ref grid 4))
	  )
	  ((match? grid '(tuple "logarithmic"))
	   (set! graphics-current-type "logarithmic")
	  )
	  ((match? grid '(tuple "logarithmic" :%1))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "logarithmic" :%2))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-base (list-ref grid 3))
	  )
	  ((match? grid '(tuple "logarithmic" :%3))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-base (list-ref grid 4))
	  ))))

(tm-define (graphics-get-grid-type visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string->symbol graphics-current-type))

(define (get-actual-grid-type visual?)
  (with grid (tree->stree (get-env-tree (if visual? "gr-grid" "gr-edit-grid")))
     (if (and (pair? grid) (> (length grid) 1))
	 (cadr grid)
	 #f)))

(define (graphics-set-grid visual?)
  (let* ((type     (string->symbol graphics-current-type))
	 (center   graphics-current-center)
	 (step     graphics-current-step)
	 (astep    graphics-current-astep)
	 (base     graphics-current-base)
	 (prop     (if visual? "gr-grid" "gr-edit-grid"))
	 (prop-old (if visual? "gr-grid-old" "gr-edit-grid-old"))
	 (the-grid #f)
    )
    (cond ((== type 'empty)
	   (set! the-grid
		`(tuple "empty"))
	  )
	  ((== type 'cartesian)
	   (set! the-grid
		`(tuple "cartesian" ,center ,step))
	  )
	  ((== type 'polar)
	   (set! the-grid
		`(tuple "polar" ,center ,step ,astep))
	  )
	  ((== type 'logarithmic)
	   (set! the-grid
		`(tuple "logarithmic" ,center ,step ,base)))
    )
    (if the-grid
    (begin
       (with grid-old (tree->stree (get-env-tree prop-old))
	  (if (and (== (get-actual-grid-type visual?) "empty")
		   (> (length grid-old) 1)
		   (== (cadr the-grid) (cadr grid-old)))
	      (graphics-set-property prop grid-old)
	      (begin
		 (graphics-set-property prop the-grid)
		 (if (!= type 'empty)
		     (graphics-set-property prop-old the-grid)))))))
    (if visual? (update-edit-grid 'grid-change))))

(define (grids-defaulted?)
  (with p (cDr (cursor-path))
     (and (== (get-upwards-property p "gr-grid") nothing)
	  (== (get-upwards-property p "gr-grid-old") nothing)
	  (== (get-upwards-property p "gr-edit-grid") nothing)
	  (== (get-upwards-property p "gr-edit-grid-old") nothing)
	  (== (get-upwards-property p "gr-edit-grid-aspect") nothing)
	  (== (get-upwards-property p "gr-grid-aspect") nothing)
	  (== (get-upwards-property p "gr-grid-aspect-props") nothing))))

(tm-define (graphics-reset-grids)
  (:check-mark "*" grids-defaulted?)
  (graphics-remove-property "gr-grid")
  (graphics-remove-property "gr-grid-old")
  (if (grid-as-visual-grid?)
  (begin
     (graphics-remove-property "gr-edit-grid")
     (graphics-remove-property "gr-edit-grid-old")
     (graphics-remove-property "gr-edit-grid-aspect")
     (graphics-remove-property "gr-grid-aspect")
     (graphics-remove-property "gr-grid-aspect-props"))))

(define (visual-type-has-value? type)
  (graphics-fetch-grid-vars #f #t)
  (set! type (cadr type))
  (== type (string->symbol graphics-current-type)))

(tm-define (graphics-set-visual-grid type)
  (:check-mark "*" visual-type-has-value?)
  (graphics-fetch-grid-vars type #t)
  (with new-polar? #f
     (if (and (== type 'polar)
	      (!= type (string->symbol graphics-current-type))
	 )
	 (let* ((aspect (graphics-grid-aspect #t))
		(nsubds (aspect-ref aspect 3))
	    )
	    (if nsubds
		(set! nsubds (cadr nsubds)))
	    (set! new-polar? #t)
	    (graphics-set-grid-aspect 'detailed nsubds #t)
	    (set! graphics-current-astep (f2s default-polar-astep))))
     (set! graphics-current-type (symbol->string type))
     (graphics-set-grid #t)
     (graphics-fetch-grid-vars type #t)
     (if new-polar? (begin
	 (set! graphics-current-type (symbol->string type))
	 (set! graphics-current-astep (f2s default-polar-astep))
	 (update-edit-grid 'grid-change)))))

(define (edit-type-has-value? type)
  (graphics-fetch-grid-vars #f #f)
  (set! type (cadr type))
  (if (== type 'default)
      (set! type 'empty))
  (== type (string->symbol graphics-current-type)))

(tm-define (graphics-set-edit-grid type)
  (:check-mark "*" edit-type-has-value?)
  (cond ((or (== type 'default)
	     (== type 'grid-change)
	 )
	 (let* ((aspect (graphics-grid-aspect-props))
		(nsubds0 (cadr (list-ref aspect (- (length aspect) 1))))
		(nsubds (if (number? nsubds0)
			    nsubds0
			    (if (string? nsubds0)
				(string->number nsubds0)
				#f)))
	    )
	    (if (or (== nsubds #f) (not (grid-aspect-show-subunits?)))
		(set! nsubds 1))
	    (if (== type 'default)
		(graphics-fetch-grid-vars 'cartesian #t))
	    (if (!= graphics-current-type "logarithmic")
	    (begin
	       (graphics-set-grid-aspect 'update nsubds #f)))
	    (graphics-set-grid #f))
	)
	(else
	  (grid-as-visual-grid! #f)
	  (graphics-fetch-grid-vars type #f)
	  (set! graphics-current-type (symbol->string type))
	  (graphics-set-grid #f))))

(define (update-edit-grid cmd)
  (if egrid-as-vgrid?
      (graphics-set-edit-grid cmd)))

;; Setting grid properties
(tm-define (graphics-set-grid-center x y visual?)
  (if (not visual?)
      (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-center `(point ,x ,y))
  (graphics-set-grid visual?))

(define (grid-step-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number=? val graphics-current-step))

(tm-define (graphics-set-grid-step val visual?)
  (:check-mark "*" grid-step-has-value?)
  (if (not visual?)
      (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-step val)
  (graphics-set-grid visual?))

(define (grid-astep-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number=? val graphics-current-astep))

(tm-define (graphics-set-grid-astep val visual?)
  (:check-mark "*" grid-astep-has-value?)
  (if (not visual?)
      (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-astep val)
  (graphics-set-grid visual?))

(define (grid-base-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (== val graphics-current-base))

(tm-define (graphics-set-grid-base val visual?)
  (:check-mark "*" grid-base-has-value?)
  (if (not visual?)
      (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-base val)
  (graphics-set-grid visual?))

(tm-define (graphics-interactive-set-grid-center visual?)
  (:interactive #t)
  (interactive
      (lambda (x y)
	(graphics-set-grid-center x y visual?))
    "Center's x-coordinate" "Center's y-coordinate"))

(tm-define (graphics-interactive-set-grid-step visual?)
  (:interactive #t)
  (interactive (lambda (x) (graphics-set-grid-step x visual?))
    "Unit length"))

(tm-define (graphics-interactive-set-grid-astep visual?)
  (:interactive #t)
  (interactive (lambda (x) (graphics-set-grid-astep x visual?))
    "Number of angular steps"))

(tm-define (graphics-interactive-set-grid-base visual?)
  (:interactive #t)
  (interactive (lambda (x) (graphics-set-grid-base x visual?))
    "Logarithmic base"))

;; Setting visual grid aspect properties
(tm-define (graphics-set-grid-aspect-properties c0 c1 s2 c2)
  (:argument c0 "Color(axes)")
  (:argument c1 "Color(unit)")
  (:argument s2 "Subdivisions per unit")
  (:argument c2 "Color(subds)")
  (with aspect `(tuple (tuple "axes" ,c0) (tuple "1" ,c1) (tuple ,s2 ,c2))
    (graphics-set-property "gr-grid-aspect" aspect)
    (graphics-set-property "gr-grid-aspect-props" aspect))
  (update-edit-grid 'default))

(define (cmp-aspect-items x y)
  (if (== (cadr x) "axes") #t
  (if (== (cadr y) "axes") #f
  (let* ((xval (s2f (cadr x)))
	 (yval (s2f (cadr y))))
    (< xval yval)))))

(define (graphics-grid-aspect-props)
  (define res #f)
  (with aspect
      ;;(tree->stree (get-env-tree "gr-grid-aspect-props"))
	(get-upwards-property
	   (cDr (cursor-path)) "gr-grid-aspect-props")
      ;;FIXME: The synchro still doesn't work with (get-env-tree),
      ;;  so we proceed differently. Take this into account everywhere
      ;;  else (depends on if (get-env-tree) is gonna be fixed or not.
      ;;  If not, then we should avoid using it, and rely on the way
      ;;  above (if it doesn't raises other problems...)).
    (if (match? aspect '(tuple (tuple :%2) (tuple :%2) :*))
	(set! res aspect)
	(begin
	  (set! aspect (graphics-path-property
			  (graphics-graphics-path) "gr-grid-aspect"))
	  (if (match? aspect '(tuple (tuple :%2) (tuple :%2) :*))
	      (set! res aspect)
	      (set! res (get-default-val "gr-grid-aspect")))))
  )
  (cons 'tuple (sort (cdr res) cmp-aspect-items)))

(define (graphics-grid-aspect visual?)
  (with gr (if visual? "gr-grid-aspect" "gr-edit-grid-aspect")
  (with aspect (tree->stree (get-env-tree gr))
     (if (not (match? aspect '(tuple (tuple :%2) (tuple :%2) :*)))
	 (set! res (get-default-val gr)))
     (cons 'tuple (sort (cdr aspect) cmp-aspect-items)))))

(define (aspect-ref a i)
  (if (and (pair? a) (integer? i) (> (length a) i))
      (list-ref a i)
      #f))

(define (nsubd-has-value? type nsubd visual?)
  (with aspect (graphics-grid-aspect visual?)
     (with ref (aspect-ref aspect 3)
	(if ref
	    (if (number? nsubd)
		(== (number->string nsubd) (cadr ref))
		(let* ((aspect (get-default-val
				 (if visual?
				     "gr-grid-aspect"
				     "gr-edit-grid-aspect")))
		       (val    (cadr (list-ref aspect 3)))
		   )
		   (== (cadr ref) val))
	    )
	    #f))))

(tm-define (graphics-set-grid-aspect type nsubd visual?)
  (:check-mark "*" nsubd-has-value?)
  (if visual?
      (with aspect (graphics-grid-aspect-props)
	(cond ((== type 'units-only)
	       (graphics-set-property "gr-grid-aspect-props" aspect)
	       (set-cdr! (cddr aspect) '())
	       (graphics-set-property "gr-grid-aspect" aspect)
	      )
	      ((== type 'detailed)
	       (if nsubd
		   (set-car! (cdr (list-ref aspect 3))
			     (number->string nsubd))
		   (set-car! (cdr (list-ref aspect 3))
			     (cadr (list-ref
				     (get-default-val "gr-grid-aspect")
					 3)))
	       )
	       (graphics-set-property "gr-grid-aspect" aspect)
	       (graphics-set-property "gr-grid-aspect-props" aspect))
	)
	(update-edit-grid 'default)
      )
      (with aspect
	    `(tuple (tuple "axes" "none") (tuple "1" "none")
		    (tuple ,(number->string nsubd) "none"))
	(if (not nsubd)
	    (set-car! (cdr (list-ref aspect 3))
		      (cadr (list-ref
			      (get-default-val "gr-edit-grid-aspect")
			      3))))
	(graphics-set-property "gr-edit-grid-aspect" aspect)
	(if (!= type 'update)
	    (grid-as-visual-grid! #f)))))

(tm-define (graphics-interactive-set-grid-nsubds visual?)
  (:interactive #t)
  (interactive
    (lambda (x) (graphics-set-grid-aspect 'detailed (string->number x) visual?))
    "Number of subunit steps"))

;; Setting visual grid aspect properties (colors)
(define (grid-aspect-ofs where)
  (cond ((== where 'axes) 1)
	 ((== where 'units) 2)
	 ((== where 'subunits) 3)
	 (else #f)))

(define (grid-color-has-value? where color)
  (let* ((i (grid-aspect-ofs (cadr where)))
	 (aspect (graphics-grid-aspect #t))
	 (aspect-props (graphics-grid-aspect-props))
	 (ref (aspect-ref aspect i))
	 (ref-props (aspect-ref aspect-props i))
    )
    (if (== color "default")
	(let* ((aspect (get-default-val "gr-grid-aspect"))
	       (ref2   (aspect-ref aspect i))
	   )
	   (if (and ref2 (or ref ref-props))
	       (== (caddr ref2) (if ref (caddr ref) (caddr ref-props)))
	       #f)
	)
	(if ref
	    (== color (caddr ref))
	    (== color (caddr ref-props))))))

(tm-define (graphics-set-grid-color where color)
  (:check-mark "*" grid-color-has-value?)
  (define i 0)
  (let* ((i (grid-aspect-ofs where))
	 (aspect (graphics-grid-aspect #t))
	 (aspect-props (graphics-grid-aspect-props))
    )
    (if i
    (begin
       (if (== color "default")
	   (let* ((aspect (get-default-val "gr-grid-aspect"))
		  (ref2   (aspect-ref aspect i))
	      )
	      (set! color (caddr ref2)))
       )
       (set-car! (cddr (list-ref aspect-props i)) color)
       (graphics-set-property "gr-grid-aspect-props" aspect-props)
       (if (and (pair? aspect) (> (length aspect) 3))
       (begin
	  (set-car! (cddr (list-ref aspect i)) color)
	  (graphics-set-property "gr-grid-aspect" aspect)))))))

;; Grid interface elements
(tm-define (grid-as-visual-grid?)
  (!= (tree->stree (get-env-tree "gr-as-visual-grid")) "off"))

(define (grid-as-visual-grid! b)
  (set! egrid-as-vgrid? b)
  (graphics-set-property "gr-as-visual-grid" (if b "on" "off")))

(tm-define (grid-toggle-as-visual-grid)
  (:check-mark "v" grid-as-visual-grid?)
  (grid-as-visual-grid! (not (grid-as-visual-grid?)))
  (update-edit-grid 'default))

(define (grid-aspect-show-subunits?)
  (with aspect (tree->stree (get-env-tree "gr-grid-aspect"))
    (and (pair? aspect) (> (length aspect) 3))))

(tm-define (grid-show-subunits?)
  (let* ((grid   (tree->stree (get-env-tree "gr-grid")))
	 (aspect (tree->stree (get-env-tree "gr-grid-aspect")))
    )
    (and (pair? grid) (pair? aspect) (> (length aspect) 3))))

(tm-define (grid-toggle-show-subunits)
  (:check-mark "v" grid-show-subunits?)
  (if (grid-show-subunits?)
      (graphics-set-grid-aspect 'units-only #f #t)
      (graphics-set-grid-aspect 'detailed #f #t)))

;; Toggling grids
(tm-define (graphics-toggle-grid visual?)
  (let* ((prop (if visual? "gr-grid" "gr-edit-grid"))
	 (prop-old (if visual? "gr-grid-old" "gr-edit-grid-old"))
	 (p (cDr (cursor-path)))
	 (gr (get-upwards-property p prop))
	 (gr-old (get-upwards-property p prop-old))
     )
     (if (!= gr-old nothing)
	 (if (or (== gr nothing)
		 (== (cadr gr) "empty"))
	     (graphics-set-property prop gr-old)
	     (graphics-set-property prop '(tuple "empty")))
	 (if (!= gr nothing)
	 (begin
	    (graphics-set-property prop '(tuple "empty"))
	    (graphics-set-property prop-old gr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-mode)
  (with m (tree->stree (get-env-tree "gr-mode"))
     (cond ((string? m)
	   `(edit ,(string->symbol m)))
	   ((pair? m)
	    (map string->symbol (cdr m))))))

(define (graphics-mode-has-value? mode)
  (if (and (pair? mode) (eq? (car mode) 'quote))
      (set! mode (cadr mode)))
  (== mode (graphics-mode)))

(tm-define (graphics-set-mode val)
  (:check-mark "v" graphics-mode-has-value?)
  (graphics-group-start)
  (graphics-enter-mode (graphics-mode) val)
  (graphics-set-property "gr-mode" `(tuple ,@(map symbol->string val))))

(tm-define (graphics-group-mode? mode)
  (func? mode 'group-edit 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes for graphical objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opacity-has-value? opacity)
  (== opacity (graphics-get-property "gr-opacity")))

(tm-define (graphics-set-opacity val)
  (:argument val "Opacity")
  (:check-mark "*" opacity-has-value?)
  (graphics-change-property "gr-opacity" val))

(define (color-has-value? color)
  (== color (graphics-get-property "gr-color")))

(tm-define (graphics-set-color val)
  (:argument val "Color")
  (:check-mark "*" color-has-value?)
  (graphics-change-property "gr-color" val))

(define (point-style-has-value? val)
  (== val (graphics-get-property "gr-point-style")))

(tm-define (graphics-set-point-style val)
  (:argument val "Point style")
  (:check-mark "*" point-style-has-value?)
  (graphics-change-property "gr-point-style" val))

(define (line-width-has-value? val)
  (== val (graphics-get-property "gr-line-width")))

(tm-define (graphics-set-line-width val)
  (:argument val "Line width")
  (:check-mark "*" line-width-has-value?)
  (graphics-change-property "gr-line-width" val))

(define (convert-dash-style val)
  (define (convert-1 ch)
    (if (or (eq? ch #\0) (eq? ch #\space)) "0" "1"))
  (if (and (string? val) (not (equal? val "")))
      (cons 'tuple (map convert-1 (string->list val)))
      'none))

(define (dash-style-has-value? val)
  (with sty (graphics-get-property "gr-dash-style")
    (if (string? sty)
	(== val sty)
	(== (convert-dash-style val) sty))))

(tm-define (graphics-set-dash-style val)
  (:argument val "Dash style")
  (:check-mark "*" dash-style-has-value?)
  (graphics-change-property
   "gr-dash-style"
   (if (== val "default")
       "default"
       (convert-dash-style val))))

(tm-define (decode-dash x)
  (cond ((== x "default") "---")
        ((== x '(tuple "1" "0")) ". . . . .")
        ((== x '(tuple "1" "1" "1" "0" "0")) "- - - - -")
        ((== x '(tuple "1" "1" "1" "1" "0" "1" "0")) "- . - . -")
        (else "other")))

(define (dash-style-unit-has-value? val)
  (== val (graphics-get-property "gr-dash-style-unit")))

(tm-define (graphics-set-dash-style-unit val)
  (:argument val "Dash style unit")
  (:check-mark "*" dash-style-unit-has-value?)
  (graphics-change-property "gr-dash-style-unit" val))

(define (fill-color-has-value? color)
  (== color (graphics-get-property "gr-fill-color")))

(tm-define (graphics-set-fill-color val)
  (:argument val "Fill color")
  (:check-mark "*" fill-color-has-value?)
  (graphics-change-property "gr-fill-color" val))

(define default-line-arrows
  ;; REMARK: the points of the arrow are specified
  ;; in absolute coordinates using tuples. Alternatively,
  ;; one might include the arrows in a (with "gr-frame" ...) tag,
  ;; but this does not yet work due to incorrect frame retrieval
  ;; in edit_graphics.cpp.
  #("none"
    (tuple
     (with "dash-style" "none"
	(line (tuple "-10ln" "6ln") (tuple "0ln" "0ln")
	      (tuple "-10ln" "-6ln"))))
    (tuple
     (with "dash-style" "none"
	(line (tuple "10ln" "6ln") (tuple "0ln" "0ln")
	      (tuple "10ln" "-6ln")))
     (with "dash-style" "none"
	(line (tuple "-10ln" "6ln") (tuple "0ln" "0ln")
	      (tuple "-10ln" "-6ln"))))))

(define (line-arrows-has-value? arrows)
  (with gr-arrows (graphics-get-property "gr-line-arrows")
     (if (pair? gr-arrows)
       ; FIXME: Shitty workaround around the <quote|none> bug...
	 (set-car! (cddadr gr-arrows) "none"))
     (if (number? arrows)
	 (== (vector-ref default-line-arrows arrows) gr-arrows)
	 (== arrows gr-arrows))))

(tm-define (graphics-set-line-arrows arrows)
  (:argument val "Arrows")
  (:check-mark "*" line-arrows-has-value?)
  (cond ((string? arrows)
	 (graphics-change-property "gr-line-arrows" arrows))
        ((integer? arrows)
         (graphics-change-property
          "gr-line-arrows"
          (vector-ref default-line-arrows arrows)))
	((pair? arrows)
	 (graphics-change-property "gr-line-arrows" arrows))))

(tm-define (decode-arrows val)
  (cond ((== val "default") "---")
        ((== val (vector-ref default-line-arrows 0)) "---")
        ((== val (vector-ref default-line-arrows 1)) "--->")
        ((== val (vector-ref default-line-arrows 2)) "<--->")
        (else "other")))

(define (text-at-halign-has-value? val)
  (== val (graphics-get-property "gr-text-at-halign")))

(tm-define (graphics-set-textat-halign val)
  (:argument val "Text-at horizontal alignment")
  (:check-mark "*" text-at-halign-has-value?)
  (graphics-change-property "gr-text-at-halign" val))

(define (text-at-valign-has-value? val)
  (== val (graphics-get-property "gr-text-at-valign")))

(tm-define (graphics-set-textat-valign val)
  (:argument val "Text-at vertical alignment")
  (:check-mark "*" text-at-valign-has-value?)
  (graphics-change-property "gr-text-at-valign" val))
