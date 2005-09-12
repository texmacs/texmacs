
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit.scm
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

(texmacs-module (graphics graphics-edit)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloaded keyboard actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-tab)
  (:mode in-graphics?)
  (graphics-choose-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-at p)
  (tree->stree (path->tree p)))

(define (get-env-stree var)
  (tree->stree (get-env-tree var)))

(define (graphics-graphics-path)
  ;; path to innermost graphics tag
  (let* ((p (cDr (cursor-path)))
	 (t (stree-at p)))
    (if (func? t 'graphics) p
	(with u (tree-innermost 'graphics)
	  (and u (tree->path u))))))

(define gr-tags-all '(point
		      line cline spline cspline
		      arc carc
		      text-at
		      gr-group))
(define gr-tags-curves  '(line cline spline cspline arc carc))
(define gr-tags-oneshot '(point arc carc text-at gr-group))
(define (graphics-path path)
  (if (or (null? path) (null? (cdr path)))
      #f
      (with p (cDr path)
	 (with o (stree-at p)
	    (if (and (pair? o) (in? (car o) gr-tags-all))
		p
		(graphics-path (cDr path)))))))

(define (graphics-active-path)
  ;; path to active tag
  (graphics-path (cursor-path)))

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

(define seek-eq?-prec #f)
(define (seek-eq? obj l)
  (define (seek l)
     (if (pair? l)
         (if (or (and (tree? (car l)) (tree obj)
		      (equal? (tree-ip (car l)) (tree-ip obj))
		 )
		 (eq? (car l) obj)
             )
	   ;;FIXME: Should be only (eq? (car l) obj) ; unfortunately,
	   ;;  (eq?) doesn't work properly on TeXmacs trees, so we
	   ;;  use this convoluted test instead.
             #t 
             (begin
                (set! seek-eq?-prec l)
                (seek (cdr l)))
         )
         #f)
  )
  (set! seek-eq?-prec #f)
  (seek l))

(define-macro (seek-eq?-remove obj l)
 `(if (seek-eq? ,obj ,l)
      (if seek-eq?-prec
          (set-cdr! seek-eq?-prec (cddr seek-eq?-prec))
          (set! ,l (cdr ,l)))))

(define-macro (foreach i . b)
  `(for-each (lambda
                (,(car i))
                ,(cons 'begin b))
            ,(cadr i)))

(define-macro (foreach-number i . b)
  `(do ((,(car i) ,(cadr i)
        (,(if (memq (caddr i) '(> >=)) '- '+) ,(car i) 1)))
       ((,(if (eq? (caddr i) '>)
             '<=
              (if (eq? (caddr i) '<)
                 '>=
                  (if (eq? (caddr i) '>=) '< '>)))
         ,(car i) ,(cadddr i))
       ,(car i))
      ,(cons 'begin b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global geometry of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-raw-go-to
   '(with "gr-mode" "point"
	  "gr-frame" (tuple "scale" "1cm" (tuple "0.5par" "0cm"))
	  "gr-clip"  (tuple "clip"
			    (tuple "0par" "-0.3par")
			    (tuple "1par" "0.3par"))
      (graphics))
   '(6 1)))

(tm-define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (path-insert-with p var val))))

(tm-define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (path-remove-with p var))))

(define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :2))
	frame
	'(tuple "scale" "1cm" (tuple "0.5par" "0cm")))))

(tm-define (graphics-set-unit u)
  (:argument u "Graphical unit")
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(tm-define (graphics-set-origin x y)
  (:argument x "Origin's x-coordinate")
  (:argument y "Origin's y-coordinate")
  (with frame (graphics-cartesian-frame)
    (with new-frame (append (cDr frame) `((tuple ,x ,y)))
      (graphics-set-property "gr-frame" new-frame))))

(tm-define (graphics-set-extents l b r t)
  (:argument l "Left corner")
  (:argument b "Bottom corner")
  (:argument r "Right corner")
  (:argument t "Top corner")
  (with clip `(tuple "clip" (tuple ,l ,b) (tuple ,r ,t))
    (graphics-set-property "gr-clip" clip)))

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
  (list->string l))

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
       ;;   for (path-assign), etc.
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
	 ;;   updates with (path-assign), etc., are asynchronous.
	 ;;   When the grid has been (path-assign)-ed in (graphics-set-grid),
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
    (graphics-set-property "gr-grid-aspect-props" aspect)
    (set! graphics-current-aspect aspect))
  (update-edit-grid 'grid-aspect-change))

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

(tm-define (graphics-interactive-set-grid-nsubds visual?)
  (:interactive #t)
  (interactive (lambda (x) (graphics-set-grid-aspect 'detailed x visual?))
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
  (with old-mode (graphics-mode)
     (graphics-set-property "gr-mode"
	(cond ((or (symbol? val) (string? val))
	       (list 'tuple 'edit val))
	      ((pair? val)
	       (cons 'tuple val)))
     )
     (graphics-enter-mode old-mode val)))

(define (graphics-group-mode? mode)
  (and (pair? mode) (eq? (car mode) 'group-edit)))

(tm-define (graphics-set-color val)
  (:argument val "Color")
  (graphics-set-property "gr-color" val))

(tm-define (graphics-set-line-width val)
  (:argument val "Line width")
  (graphics-set-property "gr-line-width" val))

(tm-define (graphics-set-dash-style val)
  (:argument val "Dash style")
  (define (convert)
    (define (convert-1 ch)
      (if (or (eq? ch #\0) (eq? ch #\space)) "0" "1"))
    (if (and (string? val) (not (equal? val "")))
	(cons 'tuple (map convert-1 (string->list val)))
        'none))
  (graphics-set-property
   "gr-dash-style" (if (== val "default") "default" (convert))))

(tm-define (graphics-set-fill-mode val)
  (:argument val "Fill mode")
  (graphics-set-property "gr-fill-mode" val))

(tm-define (graphics-set-fill-color val)
  (:argument val "Fill color")
  (graphics-set-property "gr-fill-color" val))

(define default-line-arrows
  ;; IMPORTANT NOTE: the points of the arrow are specified
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

(tm-define (graphics-set-line-arrows arrows)
  (cond ((integer? arrows)
	 (graphics-set-property
	   "gr-line-arrows"
	   (vector-ref default-line-arrows arrows)))
        ((pair? arrows)
	 (graphics-set-property "gr-line-arrows" arrows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-enrich-filter l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter (cdr l))))
	(if (or (== (cadr head) "default")
		(== (cadr head) (get-default-val (car head))))
	    tail
	    (cons* (car head) (cadr head) tail)))))

(define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(define (graphics-enrich-bis t color lw st lp fm fc)
  (let* ((mode (car t)))
    (cond ((== mode 'point)
	   (graphics-enrich-sub t `(("color" , color))))
	  ((in? mode gr-tags-curves)
	   (graphics-enrich-sub t `(("color" , color)
	      ("line-width" ,lw) ("dash-style" ,st)
	      ("line-arrows" ,lp)
	      ("fill-mode" ,fm) ("fill-color" ,fc))))
	  (else
	   (graphics-enrich-sub t '())))))

(define (graphics-enrich t)
  (let* ((color (get-env "gr-color"))
	 (lw (get-env "gr-line-width"))
	 (st (get-env-stree "gr-dash-style"))
	 (lp (get-env-stree "gr-line-arrows"))
	 (fm (get-env "gr-fill-mode"))
	 (fc (get-env "gr-fill-color")))
    (graphics-enrich-bis t color lw st lp fm fc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the innermost group of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-group-insert-bis t go-into)
  (with p (graphics-group-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (path-insert (rcons p n) (list 'tuple t))
	    (if (func? t 'with)
		(go-to (append p (list n (- (length t) 2) 1)))
		(if (and go-into (func? t 'text-at))
		    (go-to (append p (list n 0 0)))
		    (go-to (append p (list n 1)))))))))

(define (graphics-group-insert t)
  (graphics-group-insert-bis t #t))

(define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(define (graphics-group-enrich-insert-bis t color lw st lp fm fc go-into)
  (graphics-group-insert-bis
    (graphics-enrich-bis t color lw st lp fm fc) go-into))

(define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (go-to (rcons p 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the active tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-object path)
  (with p (graphics-path path)
    (if p (tree->stree (path->tree p)) #f)))

(define (graphics-active-object)
  (with p (graphics-active-path)
    (if p (tree->stree (path->tree p)) #f)))

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

(define (get-default-val var)
  (tree->stree (get-init-tree var)))

(define (graphics-active-assign t)
  (with p (graphics-active-path)
    (if p (begin
	    (path-assign p t)
	    (go-to (rcons p 1))))))

(define (graphics-active-set-tag l)
  (with t (graphics-active-object)
    (if t (graphics-active-assign (cons l (cdr t))))))

(define (graphics-active-insert t)
  (with p (graphics-active-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (path-insert (rcons p n) (list 'tuple t))
	    (go-to (rcons p 1))))))

(define (graphics-object-root-path p)
  (let* ((q (search-upwards-from p 'with))
	 (path (if (and (nnull? q)
			(== (+ (length q) 1) (length p)))
		   q p
	       )))
	path))
    
(define (graphics-remove p)
  (path-remove (graphics-object-root-path p) 1))

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
	 (if (in? (car obj) gr-tags-curves)
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
(define graphical-lstyle "default")
(define graphical-larrows "default")
(define graphical-fmode "default")
(define graphical-fcolor "default")

(define (graphical-object fetch)
  (with o (tree->stree (get-graphical-object))
   ;(display* "o=" o "\n")
    (if (and fetch (pair? o))
       (begin
	  (set! graphical-color (find-prop-bis o "color" "default"))
	  (set! graphical-lwidth (find-prop-bis o "line-width" "default"))
	  (set! graphical-lstyle (find-prop-bis o "dash-style" "default"))
          (set! graphical-larrows (find-prop-bis o "line-arrows" "default"))
	  (set! graphical-fmode (find-prop-bis o "fill-mode" "default"))
	  (set! graphical-fcolor (find-prop-bis o "fill-color" "default"))))
    (if (pair? o)
	(if (== (cdr o) '()) o (cAr o))
	'(concat))))
	    
(define (graphical-object! obj)
 ;(display* "graphical object=" obj "\n")
  (set-graphical-object (stree->tree obj)))

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
  (define (get-textat-vbase b0)
     (if (and (eq? (car o) 'text-at)
	      (equal? (car (cddddr o)) "base"))
	 (begin
	    (set-car! (cddddr o) "bottom")
	    (let* ((info0 (cdr (box-info o "lbLB")))
		   (b (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
		  )
		  (set-car! (cddddr o) "base")
		  b)
	 )
	 b0)
  )
  (let* ((info0 (cdr (box-info o "lbLB")))
	 (info1 (cdr (box-info o "rtRT")))
	 (l (i2s (min (s2i (car  info0)) (s2i (caddr  info0)))))
	 (b0 (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
	 (r (i2s (max (s2i (car  info1)) (s2i (caddr  info1)))))
	 (t (i2s (max (s2i (cadr info1)) (s2i (cadddr info1)))))
         (b (get-textat-vbase b0))
	 (p0 (frame-inverse `(tuple ,l ,b)))
	 (p1 (frame-inverse `(tuple ,r ,b)))
	 (p2 (frame-inverse `(tuple ,r ,t)))
	 (p3 (frame-inverse `(tuple ,l ,t)))
	)
	(set-car! p0 'point)
	(set-car! p1 'point)
	(set-car! p2 'point)
	(set-car! p3 'point)
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

(define (in-interval? x i1 i2)
  (and (>= x i1) (<= x i2)))

(define (on-graphical-contour? x y o eps)
  (set! eps (length-decode eps))
  (let* ((info0 (cdr (box-info o "lbLB")))
	 (info1 (cdr (box-info o "rtRT")))
	 (l (min (s2i (car  info0)) (s2i (caddr  info0))))
	 (b (min (s2i (cadr info0)) (s2i (cadddr info0))))
	 (r (max (s2i (car  info1)) (s2i (caddr  info1))))
	 (t (max (s2i (cadr info1)) (s2i (cadddr info1))))
	 (p (frame-direct `(tuple ,x ,y)))
	)
	(set! x (s2i (cadr p)))
	(set! y (s2i (caddr p)))
        (or (and (in-interval? x (- l eps) (+ l eps))
		 (in-interval? y (- b eps) (+ t eps)))
	    (and (in-interval? x (- r eps) (+ r eps))
		 (in-interval? y (- b eps) (+ t eps)))
	    (and (in-interval? x (- l eps) (+ r eps))
		 (in-interval? y (- b eps) (+ b eps)))
	    (and (in-interval? x (- l eps) (+ r eps))
		 (in-interval? y (- t eps) (+ t eps))))))

(define (create-graphical-props mode ps)
  (let ((color #f)
	(lw #f)
	(st #f)
	(lp #f)
	(fm #f)
	(fc #f)
     )
     (if (== mode 'active)
     (begin
	(set! color graphical-color)
	(set! lw graphical-lwidth)
	(set! st graphical-lstyle)
	(set! lp graphical-larrows)
	(set! fm graphical-fmode)
	(set! fc graphical-fcolor))
     )
     (if (list? mode)
     (begin
	(set! color (graphics-path-property mode "color"))
	(set! lw (graphics-path-property mode "line-width"))
	(set! st (graphics-path-property mode "dash-style"))
	(set! lp (graphics-path-property mode "line-arrows"))
	(set! fm (graphics-path-property mode "fill-mode"))
	(set! fc (graphics-path-property mode "fill-color")))
     )
     (if (== mode 'new)
     (begin
	(set! color (get-env "gr-color"))
	(set! lw (get-env "gr-line-width"))
	(set! st (get-env-stree "gr-dash-style"))
	(set! lp (get-env-stree "gr-line-arrows"))
	(set! fm (get-env "gr-fill-mode"))
	(set! fc (get-env "gr-fill-color")))
     )
     (list 'with "point-style" (if ps ps "square")
		 "color" color
		 "line-width" lw
		 "dash-style" st
		 "line-arrows" lp
		 "fill-mode" fm
		 "fill-color" (if (== fc "default")
				  (get-default-val "fill-color")
				  fc))))

(define (create-graphical-contours l mode)
  (define res '())
  (foreach (o l)
     (if (not (and (tree? o) (< (cAr (tree-ip o)) 0)))
     (let* ((props #f)
	    (t #f)
	)
	(if (tree? o)
	    (begin
	       (set! props (create-graphical-props (reverse (tree-ip o))
						   (if (== mode 'object)
						       "disc" "square")))
	       (set! o (tree->stree o)))
	)
	(cond ((== (car o) 'point)
	       (set! t `(,o))
	      )
	      ((== (car o) 'text-at)
	       (set! t (let* ((a (cdddr o))
			      (gc (create-graphical-contour
				    o (car a) (cadr a) 0.1))
			  )
			  (if (== mode 'object-and-points)
			      (cons o gc)
			      (if (== mode 'object)
				 `(,o)
				  gc))))
	      )
	      ((== (car o) 'gr-group)
	       (set! t (with gc (create-graphical-contour
				   o "center" "center" 0.1)
			  (if (== mode 'object-and-points)
			      (cons o gc)
			      (if (== mode 'object)
				 `(,o)
				  gc))))
	      )
	      (else
		 (set! t (if (== mode 'object-and-points)
			     (cons o (cdr o))
			     (if (== mode 'object)
				`(,o)
				 (cdr o)))))
	)
	(set! res (append res
			  (if props
			     `(,(append props `(,(list* 'concat t))))
			      t)))))
  )
  res)

(define (create-graphical-object o mode pts no)
  (define edge #t)
  (if (pair? no)
      (begin
	 (set! edge (car no))
         (set! no (cadr no))))
  (if o (let* ((op
	        (cond ((== (car o) 'point)
		       (cons o '())
		      )
		      ((== (car o) 'text-at)
		       (with a (cdddr o)
			  (create-graphical-contour o (car a) (cadr a) 0.1))
		      )
		      ((== (car o) 'gr-group)
		       (create-graphical-contour o "center" "center" 0.1)
		      )
		      (else (if (integer? no)
			        (let* ((l (list-tail (cdr o) no))
				       (ll (length l)))
				      (append
				        (with h (list-head (cdr o) no)
					  (if (and edge
						(in? (car o)
						    '(cline cspline))
					        (== (+ no 1) (length (cdr o))))
					    (cons `(with "point-style" "disk"
					      ,(car h)) (cdr h))
					    h))
				        (cons
					  (list 'with "point-style" "disk"
					    (cons 'concat
					      (if (< ll 2)
						  (list-head l 1)
 						  (if edge
						      (list-head l 2)
						      (cons
							`(with "point-style"
							       "square"
							      ,(list-ref l 1))
							 (list-head l 1)))))
					  ) '())
				        (if (> ll 2) (list-tail l 2) '())))
			        (cdr o))))
	       )
	       (props (create-graphical-props mode #f))
	   )
	   (graphical-object!
	      (if (or (eq? no 'group)
		      (graphics-group-mode? (graphics-mode))
		  )
		  (cons 'concat
			(create-graphical-contours selected-objects pts)
		  )
		  (append
		     props
		     (cons (cons 'concat
			    (cond ((== pts 'points) op)
				  ((== pts 'object) `(,o))
				  ((== pts 'object-and-points)
				   (cons o op)))) '())))))
      (graphical-object! '(concat))))

;; Operating on the graphical object
(define (transform-graphical-object opn)
  (with o (tree->stree (get-graphical-object))
     (if (pair? o)
     (begin
	(set! o (opn o))
	(graphical-object! o)))))

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
(tm-define graphics-undo-enabled #t)
(define selected-objects '())

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
     graphics-undo-enabled
     selected-objects))

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
    (state-set! st 'selected-objects selected-objects)
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
  (set! graphics-undo-enabled (state-ref st 'graphics-undo-enabled))
  (set! selected-objects (state-ref st 'selected-objects)))

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
  (set! graphics-undo-enabled #t)
  (set! selected-objects '()))

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
   ;(display* "res=" res "\n")
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
	          (if (not (and (string? ,msg) (== (substring ,msg 0 1) ";")))
		      (display* "Uncaptured gc(sticky) " ,msg " " o ", "
						         ,x ", " ,y "\n"))))
	    (let* (( sel (select-choose (s2i ,x) (s2i ,y)))
		   ( pxy (if sel (car sel) '()))
		   (,path (graphics-path pxy))
		   (,obj (if gm '(point) (graphics-object pxy)))
		   (,edge (and sel (== (length sel) 2)))
		   (,no (if sel (cAr (car sel)) #f)))
	      (if ,obj
	          ,(cons 'begin body)
	          (if (and (string? ,msg) (== (substring ,msg 0 1) ";"))
		      (if (== (substring ,msg 0 2) ";:")
			 ,(cons 'begin body)
			  #t)
		      (display* "Uncaptured gc(!sticky) " ,msg " " ,obj ", "
						          ,x ", " ,y "\n"))))))))

(define current-cursor #f)
(define TM_PATH (var-eval-system "echo $TEXMACS_PATH"))
(define (tm_xpm name)
  (string-append TM_PATH "/misc/pixmaps/" name))

(tm-define (graphics-reset-context cmd)
  ;; cmd in { begin, exit, undo }
  ;; (display* "Graphics] Reset-context " cmd "\n")
  (cond
   ((== cmd 'text-cursor)
    (if (not (== current-cursor 'text-cursor))
    (begin
       (set! current-cursor 'text-cursor)
       (set-predef-mouse-pointer "XC_top_left_arrow"))))
   ((== cmd 'graphics-cursor)
    (if (not (== current-cursor 'graphics-cursor))
    (begin
       (set! current-cursor 'graphics-cursor)
       (set-mouse-pointer
	 (tm_xpm "tm_graphics_cursor.xpm") (tm_xpm "tm_graphics_mask.xpm")))))
      ;; TODO: There is a problem now that we use the X cursor
      ;;   during graphics editing : the cursor doesn't aligns
      ;;   on the grid anymore. A good solution to this problem
      ;;   would be to shut it down as soon as we enter the
      ;;   sticky mode (all the more because that most of the
      ;;   time, the cursor is more a hindrance than something
      ;;   else, e.g., when editing a line).
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
	      (with p (cursor-path)
		(unredoable-undo)
		(go-to p)))))
    (graphics-reset-state)
    (graphics-forget-states))
   ((== cmd 'undo)
    (if (and sticky-point (not graphics-undo-enabled)
	     (in? (state-ref graphics-first-state 'graphics-action)
		 '(start-move start-operation start-redim)))
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
	 obj graphical-color graphical-lwidth
	 graphical-lstyle
	 graphical-larrows
	 graphical-fmode graphical-fcolor #f)
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
      (if (on-graphical-contour? x y obj "1mm")
	  (begin
	     (set-mouse-pointer
		(tm_xpm "tm_graphics_cursor.xpm")
		(tm_xpm "tm_graphics_mask.xpm")
	     )
	     (point_left-button x y p obj 1 edge))
	  (go-to (car (select-first (s2i x) (s2i y)))))))

;; Move
(define (point_move x y p obj no edge)
  (if sticky-point
      (begin
	(if (== (car obj) 'point)
	    (set! obj `(point ,x ,y))
	    (set-car! (list-tail (cdr obj) no) `(point ,x ,y)))
	(create-graphical-object obj 'active 'object-and-points
	  (if edge no `(,edge ,no))))
      (begin
	(create-graphical-object obj p 'points (if edge no `(,edge ,no)))
	(go-to (rcons p 1)))))

(define (text-at_move x y p obj no edge)
  (if (and (not sticky-point)
	   (on-graphical-contour? x y obj "1mm"))
      (set-mouse-pointer
	 (tm_xpm "tm_graphics_cursorM.xpm")
	 (tm_xpm "tm_graphics_maskM.xpm"))
      (set-mouse-pointer
	 (tm_xpm "tm_graphics_cursor.xpm")
	 (tm_xpm "tm_graphics_mask.xpm"))
  )
  (point_move x y p obj 1 edge))

(define (gr-group_move x y p obj no edge)
  (if sticky-point
      (display* "Sticky move(gr-group) !yet implemented\n")
      (begin
	(create-graphical-object obj p 'points #f))))

;; Middle button
(define (point_middle-button x y p obj no)
  (if sticky-point
      ;;Back
      (begin
	(graphics-back-state #f)
	(graphics-move-point x y))
      ;;Remove
      (begin
	(if (or (in? (car obj) gr-tags-oneshot) (null? (cdddr obj)))
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
   ";:move" x y p obj no edge
   (if obj
       (dispatch (car obj) ((point line cline spline cspline arc carc)
			    (text-at)
			    (gr-group))
		 move (x y p obj no edge) do-tick
       )
       (begin
	  (set-mouse-pointer
	     (tm_xpm "tm_graphics_cursor.xpm")
	     (tm_xpm "tm_graphics_mask.xpm")
	  )
	  (create-graphical-object '(nothing) #f 'points #f)))))

(define (edit_middle-button x y)
  (with-graphics-context
   "remove" x y p obj no edge
   (dispatch (car obj) ((point line cline spline cspline arc carc
			 text-at gr-group))
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
     obj (get-env "gr-color") (get-env "gr-line-width")
     (get-env-stree "gr-dash-style")
     (get-env-stree "gr-line-arrows")
     (get-env "gr-fill-mode") (get-env "gr-fill-color") #f)
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
     (set-car! (cddddr obj) (cond ((== valign "bottom") "base")
				  ((== valign "base") "center")
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
 ;(display* "Graphics] Edit-prop(Tab)\n")
  (edit_tab-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Util
(define (group-list l)
  (if (pair? l)
      (if (== (car l) "point-style")
	  (group-list (cddr l))
	  (cons `(,(car l) ,(cadr l)) (group-list (cddr l))))
     '()))

(define (restore-selected-objects n)
  (define gp (graphics-group-path))
  (if gp
  (let* ((gt (path->tree gp))
         (ng (tree-arity gt))
     )
     (foreach-number (i 0 < n)
     (let* ((t (tree-ref gt (+ (- ng n) i)))
	    (t2 (if (eq? (tree-label t) 'with)
		    (tree-ref t (- (tree-arity t) 1))
		    t))
        )
        (set! selected-objects (rcons selected-objects t2)))))))

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
       (if (match? o '(point :2))
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
	   (set! group-bary-x (+ group-bary-x (s2i (cadr p))))
	   (set! group-bary-y (+ group-bary-y (s2i (caddr p))))
	   (set! n (+ n 1))
	)
	(set! group-bary-x (/ group-bary-x n))
	(set! group-bary-y (/ group-bary-y n))))
  ))
  (set! group-first-x (s2i current-x))
  (set! group-first-y (s2i current-y))
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
     (if (match? o '(point :2))
	`(point ,(i2s (+ x (s2i (cadr o)))) ,(i2s (+ y (s2i (caddr o)))))
	 o)))

(define (group-translate x y)
  (lambda (o)
     (traverse-transform o (translate-point x y))))

(define (zoom-point x0 y0 h)
  (lambda (o)
     (if (match? o '(point :2))
	 (let* ((x (s2i (cadr o)))
	        (y (s2i (caddr o)))
	    )
	   `(point ,(i2s (+ x0 (* (- x group-bary-x) h)))
		   ,(i2s (+ y0 (* (- y group-bary-y) h))))
	 )
	 o)))

(define (group-zoom x y)
  (with h (/ (point-norm (sub-point `(,x ,y)
				    `(,group-bary-x ,group-bary-y)))
             (point-norm (sub-point `(,group-first-x ,group-first-y)
				    `(,group-bary-x ,group-bary-y))))
  (lambda (o)
     (traverse-transform o (zoom-point group-bary-x group-bary-y h)))))

(define (rotate-point x0 y0 alpha)
  (lambda (o)
     (if (match? o '(point :2))
	 (let* ((x (- (s2i (cadr o)) group-bary-x))
	        (y (- (s2i (caddr o)) group-bary-y))
	    )
	   `(point ,(i2s (+ x0 (* x (cos alpha)) (* (- y) (sin alpha))))
		   ,(i2s (+ y0 (* x (sin alpha)) (* y (cos alpha)))))
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
	     (set! l (cons (graphics-enrich-sub t (group-list (cdr o))) l))
	  ))
	  (graphics-group-insert
	     (cons 'gr-group (reverse l))
	  )
	  (create-graphical-object #f #f #f #f)
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
       (graphics-remove (reverse (tree-ip o)))
    )
    (set! selected-objects '())
    (with go (tree->stree (get-graphical-object))
       (if (nnull? (cdr go))
       (with l '()
	  (foreach (o (cdr go))
	  (with t (cadr (cAr o))
	     (set-cdr! (list-tail o (- (length o) 2)) '())
	     (graphics-group-insert
		(graphics-enrich-sub t (group-list (cdr o))))
	  ))
	  (create-graphical-object #f #f #f #f)
	  (graphics-group-start)))
    )
    (set! graphics-undo-enabled #t)
    (graphics-forget-states))))

;; State transitions
(define (point_start-operation opn p obj)
  (if sticky-point
      ;;Perform operation
      (begin
	 (let* ((go (tree->stree (get-graphical-object)))
	        (n 0)
	    )
	    (if (nnull? (cdr go))
	    (begin
	       (foreach (o (cdr go))
	       (with t (cadr (cAr o))
		  (set-cdr! (list-tail o (- (length o) 2)) '())
		  (graphics-group-insert
		     (graphics-enrich-sub t (group-list (cdr o)))
		  )
		  (set! n (+ n 1))
	       ))
	       (restore-selected-objects n)
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
      (if (or p (nnull? selected-objects))
      (begin
	 (if (null? selected-objects)
	     (point_toggle-select p obj))
	 (if (store-important-points)
	 (begin
	    (graphics-store-state 'start-operation)
	    (create-graphical-object obj p 'object #f)
	    (set! group-first-go (get-graphical-object))
	    (foreach (o selected-objects)
	       (graphics-remove (reverse (tree-ip o)))
	    )
	    (set! selected-objects '())
	    (set! sticky-point #t)
	    (set! graphics-undo-enabled #f)
	    (graphics-store-state #f)
	    (set! group-old-x (s2i current-x))
	    (set! group-old-y (s2i current-y))))))))

(define (point_toggle-select p obj)
  (if p (with t (path->tree p)
     (if (seek-eq? t selected-objects)
	 (seek-eq?-remove t selected-objects)
	 (set! selected-objects (rcons selected-objects t)))
  ))
  (create-graphical-object obj p 'points #f))

(define (point_unselect-all)
  (set! selected-objects '())
  (create-graphical-object '(nothing) #f 'points 'group))

;; Dispatch
(define (group-edit_move x y)
  (with-graphics-context ";move" x y p obj no edge
     (if sticky-point
	 (begin
	    (set! x (s2i x))
	    (set! y (s2i y))
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
	 (create-graphical-object obj p 'points #f))))

(define (group-edit_left-button x y)
  (with-graphics-context "start-operation" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       start-operation ('move p obj) do-tick)))

(define (group-edit_right-button x y)
  (with-graphics-context "toggle-select" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       toggle-select (p obj) do-tick)))

(define (group-edit_middle-button x y)
  (with-graphics-context "unselect-all" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       unselect-all () do-tick)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redim graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define redim-did1move #f)
(define redim-direction #f)
(define redim-precx #f)
(define redim-precy #f)
(define (redim-graphics_left-button x y)
  (if sticky-point
      (begin
	(set! sticky-point #f)
	(set! graphics-undo-enabled #t)
	(graphics-forget-states))
      (begin
	(set! redim-direction #f)
	(let* ((p (frame-direct `(point ,x ,y)))
	       (xp (cadr p))
	       (yp (caddr p))
	   )
	   (set! redim-precx (s2i xp))
	   (set! redim-precy (s2i yp)))
	(redim-graphics_move x y)
	(graphics-store-state 'start-redim)
	(set! redim-did1move #f)
        (create-graphical-object '(nothing) #f 'points #f)
	(set! sticky-point #t)
	(set! graphics-undo-enabled #f))))

(define (redim-graphics_move x y)
  (let* ((p (frame-direct `(point ,x ,y)))
	 (xp (cadr p))
	 (yp (caddr p))
	 (clip (get-env-stree "gr-clip"))
	 (N (caddr (cadddr clip)))
	 (S (caddr (caddr clip)))
	 (E (cadr (cadddr clip)))
	 (O (cadr (caddr clip)))
	 (UN (length-extract-unit N))
	 (US (length-extract-unit S))
	 (UE (length-extract-unit E))
	 (UND (length-decode (string-append "1" UN)))
	 (USD (length-decode (string-append "1" US)))
	 (UED (length-decode (string-append "1" UE)))
     )
     (if sticky-point
	 (let* ((newN N)
		(newS S)
		(newE E)
	    )
	    (cond ((== redim-direction 'N)
		   (set! newN (+ (length-decode N) (- (s2i xp) redim-precx)))
		   (if (< newN (length-decode "2mm"))
		       (set! newN (length-decode "2mm")))
		   (set! newN (/ newN UND))
		   (set! newN (string-append (i2s newN) UN))
		  )
		  ((== redim-direction 'S)
		   (set! newS (- (s2i yp) (length-decode "5mm")))
		   (if (> newS (length-decode "2mm"))
		       (set! newS (length-decode "2mm")))
		   (set! newS (/ newS USD))
		   (set! newS (string-append (i2s newS) US))
		  )
		  ((== redim-direction 'E)
		   (set! newE (+ (s2i xp) (length-decode "5mm")))
		   (if (> newE (length-decode "1par"))
		       (set! newE (length-decode "1par")))
		   (set! newE (/ newE UED))
		   (set! newE (string-append (i2s newE) UE))
		  )
	    )
	    (with clip `(tuple "clip" (tuple ,O ,newS) (tuple ,newE ,newN))
	       (graphics-set-property "gr-clip" clip)
	    )
	    (set! redim-precx (s2i xp))
	    (set! redim-precy (s2i yp))
	    (if redim-did1move
	        (remove-undo-mark)
	        (set! redim-did1move #t))
         )
         (let* ((OX (length-decode O))
		(OY (length-decode S))
		(wquad (/ (- (length-decode E) OX) 3))
		(hquad (/ (- (length-decode N) OY) 3))
	   )
           (set! xp (s2i xp))
           (set! yp (s2i yp))
           (set! redim-direction (cond
		 ((and (>= (- xp OX) wquad) (<= (- xp OX) (* 2 wquad))
		       (>= (- yp OY) (* 2 hquad)))
		  'N
		 )
		 ((and (>= (- xp OX) wquad) (<= (- xp OX) (* 2 wquad))
		       (<= (- yp OY) hquad))
		  'S
		 )
		 ((and (>= (- xp OX) (* 2 wquad))
		       (>= (- yp OY) hquad) (<= (- yp OY) (* 2 hquad)))
		  'E
		 )
		 (else #f)
	   ))
	   (cond ((== redim-direction 'N)
		  (set-mouse-pointer
		     (tm_xpm "tm_graphics_cursor_redimN.xpm")
		     (tm_xpm "tm_graphics_mask_redimN.xpm"))
		 )
		 ((== redim-direction 'S)
		  (set-mouse-pointer
		     (tm_xpm "tm_graphics_cursor_redimS.xpm")
		     (tm_xpm "tm_graphics_mask_redimS.xpm"))
		 )
		 ((== redim-direction 'E)
		  (set-mouse-pointer
		     (tm_xpm "tm_graphics_cursor_redimE.xpm")
		     (tm_xpm "tm_graphics_mask_redimE.xpm"))
		 )
		 (else
		    (set-mouse-pointer
		       (tm_xpm "tm_graphics_cursor.xpm")
		       (tm_xpm "tm_graphics_mask.xpm"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop)
	     (group-edit)
	     (redim-graphics))
	    left-button (x y)))

(tm-define (graphics-move-point x y)
  ;(display* "Graphics] Move " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop)
	     (group-edit)
	     (redim-graphics))
	    move (x y)))

(tm-define (graphics-remove-point x y)
  ;(display* "Graphics] Remove " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (group-edit))
	    middle-button (x y)))

(tm-define (graphics-last-point x y)
  ;(display* "Graphics] Last " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (edit-prop)
	     (group-edit))
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

(define (graphics-enter-mode old-mode new-mode)
  (if (and (graphics-group-mode? old-mode)
           (not (graphics-group-mode? new-mode))
      )
      (graphics-reset-state)
  )
  (if (and (not (graphics-group-mode? old-mode))
           (graphics-group-mode? new-mode)
      )
      (create-graphical-object '(nothing) #f 'points 'group)
  )
  (if (== old-mode '(redim-graphics))
      (set-mouse-pointer
	(tm_xpm "tm_graphics_cursor.xpm") (tm_xpm "tm_graphics_mask.xpm"))))

(define (graphics-finish)
  ;;(display* "Graphics] Finish\n")
  (with mode (graphics-mode)
    (cond ((== (car mode) 'edit)
	  (with submode (cadr mode)
	     (cond ((== submode 'point) (noop))
		   ((in? submode gr-tags-curves) (noop))
		   ((== submode 'text-at) (noop))
		   (else (display* "Uncaptured finish (edit)\n")))))
	 ((== (car mode) 'edit-prop)
	   (noop))
	 ((== (car mode) 'group-edit)
	   (noop))
	 ((== (car mode) 'redim-graphics)
	   (noop))
	  (else (display* "Uncaptured finish\n")))))
