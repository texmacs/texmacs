
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

(texmacs-module (texmacs edit edit-graphics))

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
			  '(point line cline spline cspline arc carc text-at)))
		p
		(graphics-path (cDr path)))))))

(define (graphics-active-path)
  ;; path to active tag
  (graphics-path (tm-where)))

(define (graphics-group-path)
  ;; path to innermost group
  (graphics-graphics-path))

(define (notify-error msg vals)
  (display "Dont worry ! A bug has just been detected (")
  (display msg)(display "; [")(write vals)(display "]")
  (display "). Please send us a report for this bug at bugs@texmacs.org\n")
  (do ((i 0 (+ i 1))) ((= i 1000) #t)
      (display* "." (integer->char 7)))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global geometry of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-go-to
   '(with "gr-mode" "point"
	  "gr-frame" (tuple "scale" "1cm" (tuple "0.5par" "0cm"))
	  "gr-clip"  (tuple "clip"
			    (tuple "0par" "-0.3par")
			    (tuple "1par" "0.3par"))
      (graphics))
   '(6 1)))

(tm-define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (tm-insert-with p var val))))

(tm-define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (tm-remove-with p var))))

(define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :2))
	frame
	'(tuple "scale" "1cm" (tuple "0.5par" "0cm")))))

(tm-define (graphics-set-unit u)
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(tm-define (graphics-set-unit-ia)
  (interactive '("Graphical unit:") 'graphics-set-unit))

(tm-define (graphics-set-origin x y)
  (with frame (graphics-cartesian-frame)
    (with new-frame (append (cDr frame) `((tuple ,x ,y)))
      (graphics-set-property "gr-frame" new-frame))))

(tm-define (graphics-set-origin-ia)
  (interactive
    '("Origin's x-coordinate:" "Origin's y-coordinate:")
    'graphics-set-origin))

(tm-define (graphics-set-extents-ia)
  (interactive
    '("Left corner:" "Bottom corner:" "Right corner:" "Top corner:")
    '(lambda (l b r t)
       (with clip `(tuple "clip" (tuple ,l ,b) (tuple ,r ,t))
	 (graphics-set-property "gr-clip" clip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define graphics-current-type #f)
(define graphics-current-center #f)
(define graphics-current-step #f)
(define graphics-current-astep #f)
(define graphics-current-base #f)

(define egrid-as-vgrid? #t)
(define graphics-current-aspect #f)

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
	(i2s (if visual?
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
	  ((match? grid '(tuple "cartesian" :1))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "cartesian" :2))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	  )
	  ((match? grid '(tuple "polar"))
	   (set! graphics-current-type "polar")
	  )
	  ((match? grid '(tuple "polar" :1))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "polar" :2))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-astep (list-ref grid 3))
	  )
	  ((match? grid '(tuple "polar" :3))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-astep (list-ref grid 4))
	  )
	  ((match? grid '(tuple "logarithmic"))
	   (set! graphics-current-type "logarithmic")
	  )
	  ((match? grid '(tuple "logarithmic" :1))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2))
	  )
	  ((match? grid '(tuple "logarithmic" :2))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-base (list-ref grid 3))
	  )
	  ((match? grid '(tuple "logarithmic" :3))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-base (list-ref grid 4))
	  ))))

(tm-define (graphics-get-grid-type visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string->symbol graphics-current-type))

(define (graphics-set-grid visual?)
  (let* ((type   (string->symbol graphics-current-type))
	 (center graphics-current-center)
	 (step   graphics-current-step)
	 (astep  graphics-current-astep)
	 (base   graphics-current-base)
	 (prop   (if visual? "gr-grid" "gr-edit-grid"))
    )
    (cond ((== type 'empty)
	   (graphics-set-property prop
	      `(tuple "empty"))
	  )
	  ((== type 'cartesian)
	   (graphics-set-property prop
	      `(tuple "cartesian" ,center ,step))
	  )
	  ((== type 'polar)
	   (graphics-set-property prop
	      `(tuple "polar" ,center ,step ,astep))
	  )
	  ((== type 'logarithmic)
	   (graphics-set-property prop
	      `(tuple "logarithmic" ,center ,step ,base)))
    )
    (if visual? (update-edit-grid 'grid-change))))

(define (visual-type-has-value? type)
  (graphics-fetch-grid-vars #f #t)
  (set! type (cadr type))
  (if (== type 'default)
      (set! type 'empty))
  (== type (string->symbol graphics-current-type)))

(tm-define (graphics-set-visual-grid type)
  (:check-mark "*" visual-type-has-value?)
  (cond ((== type 'default)
	 (graphics-remove-property "gr-grid")
	 (if egrid-as-vgrid?
	 (begin
	     (graphics-remove-property "gr-edit-grid")
	    (graphics-remove-property "gr-grid-aspect")
	    (graphics-remove-property "gr-grid-aspect-props")))
	)
	(else
	  (graphics-fetch-grid-vars type #t)
       ;; FIXME: Remove this bloat with o1, o2, new-polar?, etc. when
       ;;   an appropriate means of synchronization will be available
       ;;   for (tm-assign), etc.
	  (let* ((o1 graphics-current-center)
		 (o2 graphics-current-step)
		 (new-polar? #f)
	     )
	     (if (and (== type 'polar)
		      (!= type (string->symbol graphics-current-type))
		 )
		 (begin
		    (set! new-polar? #t)
		    (graphics-set-grid-aspect
		      'detailed (i2s default-polar-nsubd) #t)
		    (set! graphics-current-center o1)
		    (set! graphics-current-step o2)
		    (set! graphics-current-astep (i2s default-polar-astep))))
	     (set! graphics-current-type (symbol->string type))
	     (graphics-set-grid #t)
	     (if new-polar? (begin
		 (set! graphics-current-type (symbol->string type))
		 (set! graphics-current-center o1)
		 (set! graphics-current-step o2)
		 (set! graphics-current-astep (i2s default-polar-astep))
		 (update-edit-grid 'grid-change-aspect)))))))

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
	     (== type 'grid-aspect-change)
	     (== type 'grid-change-aspect)
	 )
	 (let* ((aspect (if (or (== type 'grid-aspect-change)
				(== type 'grid-change-aspect)
			    )
			    graphics-current-aspect
			    (graphics-grid-aspect-props)))
		(nsubds0 (cadr (list-ref aspect (- (length aspect) 1))))
		(nsubds (if (number? nsubds0)
			    nsubds0
			    (if (string? nsubds0)
				(string->number nsubds0)
				#f)))
	    )
	    (if (or (== nsubds #f) (not (grid-aspect-show-subunits?)))
		(set! nsubds 1))
	 ;; FIXME: The difference between 'default', 'grid-change'
	 ;;   and 'grid-aspect-change' is because currently, the
	 ;;   updates with (tm-assign), etc., are asynchronous.
	 ;;   When the grid has been (tm-assign)-ed in (graphics-set-grid),
	 ;;   the (graphics-fetch-grid-vars) below fetches the not
	 ;;   yet updated version of the grid, and this is wrong.
	 ;;   On the other hand, when (graphics-set-edit-grid) is
	 ;;   called directly from the menu, the instruction below
	 ;;   is mandatory.
	 ;; TODO: As soon as a better control of the update synchro
	 ;;   is available, clean this code.
	    (if (or (== type 'default)
		    (== type 'grid-aspect-change)
		)
		(graphics-fetch-grid-vars 'cartesian #t))
	    (if (!= graphics-current-type "logarithmic")
	    (begin
	       (graphics-set-grid-aspect 'update nsubds #f)))
	    (graphics-set-grid #f))
	)
	(else
	  (graphics-set-property "gr-as-visual-grid" "off")
	  (graphics-fetch-grid-vars type #f)
	  (set! graphics-current-type (symbol->string type))
	  (graphics-set-grid #f))))

(define (update-edit-grid cmd)
  (if egrid-as-vgrid?
      (graphics-set-edit-grid cmd)))

;; Setting grid properties
(define (grid-prop-input prompt func visual?)
  (interactive prompt (lambda (x) (func x visual?))))

(tm-define (graphics-set-grid-center x y visual?)
  (if (not visual?)
      (graphics-set-property "gr-as-visual-grid" "off"))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-center `(point ,x ,y))
  (graphics-set-grid visual?))

(define (grid-step-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number== val graphics-current-step))

(tm-define (graphics-set-grid-step val visual?)
  (:check-mark "*" grid-step-has-value?)
  (if (not visual?)
      (graphics-set-property "gr-as-visual-grid" "off"))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-step val)
  (graphics-set-grid visual?))

(define (grid-astep-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number== val graphics-current-astep))

(tm-define (graphics-set-grid-astep val visual?)
  (:check-mark "*" grid-astep-has-value?)
  (if (not visual?)
      (graphics-set-property "gr-as-visual-grid" "off"))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-astep val)
  (graphics-set-grid visual?))

(define (grid-base-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (== val graphics-current-base))

(tm-define (graphics-set-grid-base val visual?)
  (:check-mark "*" grid-base-has-value?)
  (if (not visual?)
      (graphics-set-property "gr-as-visual-grid" "off"))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-base val)
  (graphics-set-grid visual?))

(tm-define (graphics-set-grid-center-ia visual?)
  (interactive
    '("X:" "Y:")
     (lambda (x y)
       (graphics-set-grid-center x y visual?))))

(tm-define (graphics-set-grid-step-ia visual?)
  (grid-prop-input '("Unit length:") graphics-set-grid-step visual?))

(tm-define (graphics-set-grid-astep-ia visual?)
  (grid-prop-input '("Nb angular steps:") graphics-set-grid-astep visual?))

(tm-define (graphics-set-grid-base-ia visual?)
  (grid-prop-input '("Logarithmic base:") graphics-set-grid-base visual?))

;; Setting visual grid aspect properties
(tm-define (graphics-set-grid-aspect-properties c0 c1 s2 c2)
  (with aspect `(tuple (tuple "axes" ,c0) (tuple "1" ,c1) (tuple ,s2 ,c2))
    (graphics-set-property "gr-grid-aspect" aspect)
    (graphics-set-property "gr-grid-aspect-props" aspect)
    (set! graphics-current-aspect aspect)
  )
  (update-edit-grid 'grid-aspect-change))

(tm-define (graphics-set-grid-aspect-properties-ia)
  (interactive
    '("Color(axes):" "Color(unit):" "Subdivisions per unit:" "Color(subds):")
    '(lambda (c0 c1 s2 c2)
       (graphics-set-grid-aspect-properties c0 c1 s2 c2))))

(define (cmp-aspect-items x y)
  (if (== (cadr x) "axes") #t
  (if (== (cadr y) "axes") #f
  (let* ((xval (s2i (cadr x)))
	 (yval (s2i (cadr y))))
    (< xval yval)))))

(define (graphics-grid-aspect-props)
  (define res #f)
  (with aspect (tree->stree (get-env-tree "gr-grid-aspect-props"))
    (if (match? aspect '(tuple (tuple :2) (tuple :2) :*))
	(set! res aspect)
	(begin
	  (set! aspect (graphics-path-property
			  (graphics-graphics-path) "gr-grid-aspect"))
	  (if (match? aspect '(tuple (tuple :2) (tuple :2) :*))
	      (set! res aspect)
	      (set! res (get-default-val "gr-grid-aspect")))))
  )
  (cons 'tuple (sort (cdr res) cmp-aspect-items)))

(define (graphics-grid-aspect visual?)
  (with gr (if visual? "gr-grid-aspect" "gr-edit-grid-aspect")
  (with aspect (tree->stree (get-env-tree gr))
     (if (not (match? aspect '(tuple (tuple :2) (tuple :2) :*)))
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
		   (set-car! (cdr (list-ref aspect 3)) nsubd)
		   (set-car! (cdr (list-ref aspect 3))
			     (cadr (list-ref
				     (get-default-val "gr-grid-aspect")
			    	     3)))
	       )
	       (graphics-set-property "gr-grid-aspect" aspect)
	       (graphics-set-property "gr-grid-aspect-props" aspect))
	)
	(set! graphics-current-aspect aspect)
	(update-edit-grid 'grid-aspect-change)
      )
      (with aspect
	    `(tuple (tuple "axes" "none") (tuple "1" "none")
		    (tuple ,nsubd "none"))
	(if (not nsubd)
	    (set-car! (cdr (list-ref aspect 3))
		      (cadr (list-ref
			      (get-default-val "gr-edit-grid-aspect")
			      3))))
	(graphics-set-property "gr-edit-grid-aspect" aspect)
	(if (!= type 'update)
	    (graphics-set-property "gr-as-visual-grid" "off")))))

(tm-define (graphics-set-grid-nsubds-ia visual?)
  (interactive '("Number of subunit steps:")
		(lambda (x) (graphics-set-grid-aspect 'detailed x visual?))))

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

(tm-define (grid-toggle-as-visual-grid)
  (:check-mark "v" grid-as-visual-grid?)
  (set! egrid-as-vgrid? (not (grid-as-visual-grid?)))
  (graphics-set-property "gr-as-visual-grid"
			 (if egrid-as-vgrid? "on" "off"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-mode)
  (with m (tree->stree (get-env-tree "gr-mode"))
     (cond ((string? m)
	   `(edit ,(string->symbol m)))
	   ((pair? m)
	    (map string->symbol (cdr m))))))

(tm-define (graphics-set-mode val)
  (graphics-group-start)
  (graphics-set-property "gr-mode"
     (cond ((or (symbol? val) (string? val))
	    (list 'tuple 'edit val))
	   ((pair? val)
	    (cons 'tuple val)))))

(tm-define (graphics-set-color val)
  (graphics-set-property "gr-color" val))

(tm-define (graphics-set-line-width val)
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
	  ((in? mode '(line cline spline cspline arc carc))
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

(define (get-default-val var)
  (tree->stree (get-init-tree var)))

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

(define (graphics-object-root-path p)
  (let* ((q (search-upwards-from p 'with))
	 (path (if (and (nnull? q)
			(== (+ (length q) 1) (length p)))
		   q p
	       )))
	path))
    
(define (graphics-remove p)
  (tm-remove (graphics-object-root-path p) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for calculating with the graphical object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Closest point
(define (s2i s)
  (exact->inexact (string->number s)))

(define (i2s s)
  (number->string s))

(define (string-number== s1 s2)
  (if (and (string? s1) (string? s2))
      (let* ((i1 (s2i s1))
	     (i2 (s2i s2)))
	    (if (and i1 i2) (== i1 i2) #f)
      )
      #f))

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
	 (if (in? (car obj) '(line cline spline cspline arc carc))
	     (graphics-closest-point-pos (list 'point x y) (cdr obj))
	 0)))
      0))

(define (box-info t cmd)
  (tree->stree (texmacs-exec `(box-info ,t ,cmd))))

(define (frame-direct p)
  (tree->stree (texmacs-exec `(frame-direct ,p))))

(define (frame-inverse p)
  (tree->stree (texmacs-exec `(frame-inverse ,p))))

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
  (let* ((info0 (cdr (box-info o "lbLB")))
	 (info1 (cdr (box-info o "rtRT")))
	 (l (i2s (min (s2i (car  info0)) (s2i (caddr  info0)))))
	 (b (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
	 (r (i2s (max (s2i (car  info1)) (s2i (caddr  info1)))))
	 (t (i2s (max (s2i (cadr info1)) (s2i (cadddr info1)))))
	 (p0 (frame-inverse `(tuple ,l ,b)))
	 (p1 (frame-inverse `(tuple ,r ,b)))
	 (p2 (frame-inverse `(tuple ,r ,t)))
	 (p3 (frame-inverse `(tuple ,l ,t)))
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

(define (create-graphical-object o mode pts no)
  (if o (with op
	      (cond ((== (car o) 'point)
		     (cons o '())
		    )
		    ((== (car o) 'text-at)
		     (with a (cdddr o)
			(create-graphical-contour o (car a) (cadr a) 0.1))
		    )
		    (else (if no
			      (let* ((l (list-tail (cdr o) no))
				     (ll (length l)))
				    (append
				      (with h (list-head (cdr o) no)
					(if (and (in? (car o) '(cline cspline))
					      (== (+ no 1) (length (cdr o))))
					  (cons `(with "point-style" "disk"
					    ,(car h)) (cdr h))
					  h))
				      (cons
					(list 'with "point-style" "disk"
					  (cons 'concat
					    (if (< ll 2) (list-head l 1)
							 (list-head l 2)))) '())
				      (if (> ll 2) (list-tail l 2) '())))
			      (cdr o))))
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
(define current-edge-sel? #f)
(define current-selection #f)
(define previous-selection #f)
(define subsel-no #f)
(define current-x #f)
(define current-y #f)
(define-public graphics-undo-enabled #t)

(define state-slots
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
     graphics-undo-enabled))

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
    (state-set! st 'current-edge-sel? current-edge-sel?)
    (state-set! st 'current-selection current-selection)
    (state-set! st 'previous-selection previous-selection)
    (state-set! st 'subsel-no subsel-no)
    (state-set! st 'current-x current-x)
    (state-set! st 'current-y current-y)
    (state-set! st 'graphics-undo-enabled graphics-undo-enabled)
    st))

(define (graphics-state-set st)
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
  (set! graphics-undo-enabled (state-ref st 'graphics-undo-enabled)))

;; State stack
(define graphics-states '())

(define (graphics-states-void?)
  (null? graphics-states))

(define (graphics-state-first?)
  (and (not (graphics-states-void?)) (null? (cdr graphics-states))))

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
      (if (graphics-state-first?)
	  (undo)
	  (with st (graphics-pop-state)
	    (graphics-state-set st)
	    (if (graphics-states-void?)
		(graphics-push-state st))))))

(define (graphics-reset-state)
  (create-graphical-object #f #f #f #f)
  (set! sticky-point #f)
  (set! current-point-no #f)
  (set! current-edge-sel? #f)
  (set! current-selection #f)
  (set! previous-selection #f)
  (set! subsel-no #f)
  (set! current-x #f)
  (set! current-y #f)
  (set! graphics-undo-enabled #t))

(define (graphics-forget-states)
  (set! graphics-first-state #f)
  (set! graphics-states '())
  (events-forget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for using and maintaining the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Graphical select
(tm-define (graphics-select x y d)
  (define (filter-path p)
    (with p2 (map string->number (cdr p))
      (if (graphics-path p2) p2 #f))
  )
  (define (filter-sel e)
    (with e2 (map filter-path (cdr e))
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
  (define (filter l)
    (with l2 (cons 'tuple (map filter-sel (cdr l)))
      (remove-filtered-elts l2)
      (cdr l2))
  )
  (with res (tree->stree (graphical-select x y))
    (filter res)))

(define (select-first x y)
  (with sel (graphics-select x y 15)
    (if (pair? sel) (car sel) #f)))

(define (select-choose x y)
  (with sel (graphics-select x y 15)
    (set! previous-selection current-selection)
    (set! current-selection sel)
    (if (or (null? sel) (!= current-selection previous-selection))
	(set! subsel-no 0))
    (if (pair? sel) (car (list-tail sel subsel-no)) #f)))

(define (select-next)
  (if (and current-selection subsel-no)
      (begin
	(set! subsel-no (+ subsel-no 1))
	(if (>= subsel-no (length current-selection))
	    (set! subsel-no 0)))))

;; Graphics context
(define-macro (with-graphics-context msg x y path obj no edge . body)
  `(begin
     (set! current-x x)
     (set! current-y y)
     (if sticky-point
	 (with o (graphical-object #t)
	   (if (and (pair? o) (nnull? (cdr o)))
	       (let* ((,obj (cadr o))
		      (,no current-point-no)
		      (,edge current-edge-sel?)
		      (,path (cDr (tm-where))))
		    ,(cons 'begin body))
	       (if (not (and (string? ,msg) (== (substring ,msg 0 1) ";")))
		   (display* "Uncaptured " ,msg " " ,x ", " ,y "\n"))))
	 (let* (( sel (select-choose (s2i ,x) (s2i ,y)))
		( pxy (if sel (car sel) '()))
		(,path (graphics-path pxy))
		(,obj (graphics-object pxy))
		(,edge (and sel (== (length sel) 2)))
		(,no (if sel (cAr (car sel)) #f)))
	   (if ,obj
	       ,(cons 'begin body)
	       (if (not (and (string? ,msg) (== (substring ,msg 0 1) ";")))
		   (display* "Uncaptured " ,msg " " ,x ", " ,y "\n")))))))

(tm-define (graphics-reset-context cmd)
  ;; cmd in { begin, exit, undo }
  ;; (display* "Graphics] Reset-context " cmd "\n")
  (cond
   ((and (in? cmd '(begin exit)) (or (== cmd 'begin) (not sticky-point)))
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
	      (with p (tm-where)
		(unredoable-undo)
		(tm-go-to p)))))
    (graphics-reset-state)
    (graphics-forget-states))
   ((== cmd 'undo)
    (if (and sticky-point (not graphics-undo-enabled)
	     (== (state-ref graphics-first-state 'graphics-action)
		  'start-move))
	(begin
	  (set! graphics-undo-enabled #t)
	  (unredoable-undo))
	(begin
	  (invalidate-graphical-object)
	  (if (and graphics-undo-enabled (not sticky-point))
	      (with p (graphics-active-path)
		(if p
		    (create-graphical-object (graphics-active-object) p 'points #f)
		    (create-graphical-object #f #f #f #f))))
	  (if (and (not graphics-undo-enabled) sticky-point)
	      (create-graphical-object #f #f #f #f))
	  (set! sticky-point #f)
	  (set! current-point-no #f)
	  (set! graphics-undo-enabled #t)
	  (if graphics-first-state
	      (graphics-back-first))
	  (graphics-forget-states)
	  (invalidate-graphical-object))))
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
(define (point_left-button x y p obj no edge)
  (if sticky-point
      ;;Last
      (if (not (and (in? (car obj) '(arc carc)) (<= (length obj) 3)))
      (begin
	(create-graphical-object obj 'active 'points #f)
	(graphics-group-enrich-insert-bis
	 obj graphical-color graphical-lwidth #f)
	(if (== (state-ref graphics-first-state 'graphics-action)
		'start-move)
	    (remove-undo-mark))
	(set! sticky-point #f)
	(set! current-edge-sel? #f)
	(set! graphics-undo-enabled #t)
	(graphics-forget-states)))
      ;;Start move
      (begin
	(graphics-store-state 'start-move)
	(create-graphical-object obj p 'object-and-points #f)
	(graphics-remove p)
	(set! sticky-point #t)
	(set! current-point-no no)
	(set! current-edge-sel? #t)
	(set! graphics-undo-enabled #f)
	(if edge
	  (point_sticky-right-button x y p
	    (cadr (graphical-object #t)) no edge)
	  (graphics-store-state #f)))))

(define (text-at_left-button x y p obj no edge)
  (if sticky-point
      (point_left-button x y p obj 1 edge)
      (begin
	 (if (event-exists? 'text-at 'left-button 1)
	     (tm-go-to (car (select-first (s2i x) (s2i y))))))))

;; Move
(define (point_move x y p obj no edge)
  (if sticky-point
      (begin
	(if (== (car obj) 'point)
	    (set! obj `(point ,x ,y))
	    (set-car! (list-tail (cdr obj) no) `(point ,x ,y)))
	(create-graphical-object obj 'active 'object-and-points
	  (if edge no #f)))
      (begin
	(create-graphical-object obj p 'points (if edge no #f))
	(tm-go-to (rcons p 1)))))

(define (text-at_move x y p obj no edge)
  (if (and (not sticky-point) (event-exists? 'text-at 'left-button 1))
      (point_left-button x y p obj 1 edge)
      (point_move x y p obj 1 edge)))

;; Middle button
(define (point_middle-button x y p obj no)
  (if sticky-point
      ;;Back
      (begin
	(graphics-back-state #f)
	(graphics-move-point x y))
      ;;Remove
      (begin
	(if (or (in? (car obj) '(point text-at arc carc)) (null? (cddr obj)))
	    (begin
	      (graphics-remove p)
	      (create-graphical-object #f #f #f #f)
	      (graphics-group-start))
	    (with l (if (<= no 0) obj (list-tail (cdr obj) (- no 1)))
	      (set-cdr! l (cddr l))
	      (create-graphical-object obj p 'points #f)
	      (graphics-active-assign obj)))
	(set! sticky-point #f))))

;; Right button (add point)
(define (point_sticky-right-button x y p obj no edge)
  (if (not (and (in? (car obj) '(arc carc)) (> (length obj) 3)))
      (with l (list-tail (cdr obj) no)
	(graphics-store-state #f)
	(set-cdr! l (cons `(point ,x ,y) (cdr l)))
	(create-graphical-object obj 'active 'object-and-points (+ no 1))
	(set! current-point-no (+ no 1))
	(set! current-edge-sel? #t)
	(set! sticky-point #t))))

;; Right button (create)
(define (point_nonsticky-right-button x y mode)
  (graphics-group-enrich-insert `(point ,x ,y)))

(define (line_nonsticky-right-button x y mode)
  (with o `(,mode (point ,x ,y) (point ,x ,y))
    (graphics-store-state 'start-create)
    (set! graphics-undo-enabled #f)
    (create-graphical-object o 'new 'object-and-points #f)
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
   "insert" x y p obj no edge
   (dispatch (car obj) ((point line cline spline cspline arc carc)
			(text-at))
	     left-button (x y p obj no edge) do-tick)))

(define (edit_move x y)
  (with-graphics-context
   ";move" x y p obj no edge
   (dispatch (car obj) ((point line cline spline cspline arc carc)
			(text-at))
	     move (x y p obj no edge) do-tick)))

(define (edit_middle-button x y)
  (with-graphics-context
   "remove" x y p obj no edge
   (dispatch (car obj) ((point line cline spline cspline arc carc text-at))
	     middle-button (x y p obj no) do-tick)))

(define (edit_right-button x y)
  (if sticky-point
      (with-graphics-context
       "last" x y p obj no edge
       (dispatch (car obj) ((point line cline spline cspline arc carc text-at))
		 sticky-right-button (x y p obj no edge) do-tick))
      (with mode (cadr (graphics-mode))
	(dispatch mode ((point)
			(line cline spline cspline arc carc)
			(text-at))
		  nonsticky-right-button (x y mode) do-tick))))

(define (edit_tab-key)
 ;(display* "Graphics] Edit(Tab)\n")
  (if (and current-x current-y)
      (begin
	(select-next)
	(invalidate-graphical-object)
	(edit_move current-x current-y)
	(invalidate-graphical-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit properties mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Left button
(define (point_assign-props p obj)
  (graphics-remove p)
  (graphics-group-enrich-insert-bis
     obj (get-env "gr-color") (get-env "gr-line-width") #f)
  (create-graphical-object obj 'new 'points #f))

(define (text-at_change-halign p obj)
  (graphics-remove p)
  (with halign (cadddr obj)
     (set-car! (cdddr obj) (cond ((== halign "left") "center")
				 ((== halign "center") "right")
				 ((== halign "right") "left")
				 (else "left")))
     (graphics-group-insert-bis obj #f)
     (create-graphical-object obj '() 'points #f)))

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
     (create-graphical-object obj '() 'points #f)))

;; Dispatch
(define (edit-prop_move x y)
  (edit_move x y))

(define (edit-prop_left-button x y)
  (with-graphics-context "assign-props" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc)
			  (text-at))
	       assign-props (p obj) do-tick)))

(define (edit-prop_right-button x y)
  (with-graphics-context "change-valign" x y p obj no edge
     (dispatch (car obj) ((text-at))
	       change-valign (p obj) do-tick)))

(define (edit-prop_tab-key)
  (display* "Graphics] Edit-prop(Tab)\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    left-button (x y)))

(tm-define (graphics-move-point x y)
  ;(display* "Graphics] Move " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    move (x y)))

(tm-define (graphics-remove-point x y)
  ;(display* "Graphics] Remove " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit))
	    middle-button (x y)))

(tm-define (graphics-last-point x y)
  ;(display* "Graphics] Last " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    right-button (x y)))

(tm-define (graphics-start-drag x y)
  ;(display* "Graphics] Start-drag " x ", " y "\n")
  (graphics-insert-point x y))

(tm-define (graphics-dragging x y)
  ;(display* "Graphics] dragging " x ", " y "\n")
  (graphics-move-point x y))

(tm-define (graphics-end-drag x y)
  ;(display* "Graphics] End-drag " x ", " y "\n")
  (graphics-insert-point x y))

(tm-define (graphics-choose-point)
  ;(display* "Graphics] Choose\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop))
	    tab-key ()))

(define (graphics-finish)
  ;;(display* "Graphics] Finish\n")
  (with mode (graphics-mode)
    (cond ((== (car mode) 'edit)
	  (with submode (cadr mode)
	     (cond ((== submode 'point) (noop))
		   ((in? submode '(line cline spline cspline arc carc)) (noop))
		   ((== submode 'text-at) (noop))
		   (else (display* "Uncaptured finish (edit)\n")))))
	 ((== (car mode) 'edit-prop)
	   (noop))
	  (else (display* "Uncaptured finish\n")))))
