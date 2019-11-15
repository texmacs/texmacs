
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

(define (inside-draw-over?)
  (inside? 'draw-over))

(tm-define (graphics-toggle-over-under)
  (:check-mark "*" inside-draw-over?)
  (with-innermost t graphical-over-under-context?
    (cond ((tree-is? t 'draw-over)
           (tree-assign-node! t 'draw-under)
           (tree-go-to t 0 :end))
          ((tree-is? t 'draw-under)
           (tree-assign-node! t 'draw-over)
           (if (tree-is? (tree-ref t 1) 'with)
               (tree-go-to t 1 (- (tree-arity (tree-ref t 1)) 1) :end)
               (tree-go-to t 1 :end))))))

(tm-define (graphics-enter-into t)
  (set! t (tree-ref t 1))
  (while (tree-is? t 'with)
    (set! t (tm-ref t :last)))
  (when (tree-is? t 'graphics)
    (tree-go-to t :last :end)))

(tm-define (graphics-enter)
  (with t (cursor-tree)
    (when (tree-is? t 'draw-under)
      (tree-assign-node! t 'draw-over))
    (if (tree-is? t 'draw-over)
        (graphics-enter-into t)
        (with-innermost u 'draw-under
          (tree-assign-node! u 'draw-over)
          (graphics-enter-into u)))))

(tm-define (graphics-exit-right)
  (cond ((inside-graphical-over-under?)
         (with-innermost t graphical-over-under-context?
           (tree-go-to t :end)))
        ((inside? 'graphics)
         (with-innermost t 'graphics
           (while (and (tree-up t) (tree-func? (tree-up t) 'with))
             (set! t (tree-up t)))
           (tree-go-to t :end)))
        ((tree-is? (cursor-tree) 'graphics)
         (with t (cursor-tree)
           (while (and (tree-up t) (tree-func? (tree-up t) 'with))
             (set! t (tree-up t)))
           (tree-go-to t :end)))))

(tm-define (graphics-set-overlap w)
  (:argument w "Width of overlapping border")
  (when (inside-graphical-over-under?)
    (with-innermost t graphical-over-under-context?
      (tree-set t 2 w))))

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
	 (new-geo `(tuple "geometry" ,w ,(cadddr geo) ,align)))
    (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-set-height h)
  (:argument h "Height of the graphics")
  (let* ((geo (graphics-geometry))
	 (align (if (>= (length geo) 5) (cAr geo) "center"))
	 (new-geo `(tuple "geometry" ,(caddr geo) ,h ,align)))
    (graphics-set-property "gr-geometry" new-geo)))

(define (geo-valign-has-value? val)
  (let* ((geo (graphics-geometry))
	 (align (car (cddddr geo))))
    (== val align)))

(tm-define (graphics-set-geo-valign a)
  (:argument a "Alignment of the graphics")
  (:check-mark "*" geo-valign-has-value?)
  (let* ((geo (graphics-geometry))
	 (new-geo `(tuple "geometry" ,(caddr geo) ,(cadddr geo) ,a)))
    (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-set-extents w h)
  (:argument w "Width of the graphics")
  (:argument h "Height of the graphics")
  (let* ((geo (graphics-geometry))
	 (align (if (>= (length geo) 5) (cAr geo) "center"))
	 (new-geo `(tuple "geometry" ,w ,h ,align)))
    (graphics-set-property "gr-geometry" new-geo)))

(tm-define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :%2))
	frame
        `(tuple "scale" ,(graphics-default-unit)
                (tuple "0.5par" "0cm")))))

(define (graphics-unit-has-value? val)
  (let* ((fr (graphics-cartesian-frame))
	 (unit (caddr fr)))
    (== val unit)))

(tm-define (graphics-set-unit u)
  (:argument u "Graphical unit")
  (:check-mark "*" graphics-unit-has-value?)
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-origin-has-value? x y)
  (let* ((fr (graphics-cartesian-frame))
	 (orig (cAr fr)))
    (if (pair? x)
        (set! x (length-add (cadr x) (caddr x))))
    (if (pair? y)
        (set! y (length-add (cadr y) (caddr y))))
    ;; FIXME: The 2 (if)s above lack perfection...
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
            (set-cdr! l '()))))
  (traverse l)
  (set! l (reverse l))
  (if (and (pair? l) (not (char-alphabetic? (car l))))
      (set! l (cdr l)))
  (list->string l)) ;; TODO: Move this in the utils (?)

(tm-define (graphics-auto-crop?)
  (== (graphics-get-property "gr-auto-crop") "true"))

(tm-define (graphics-toggle-auto-crop)
  (:check-mark "v" graphics-auto-crop?)
  (with new (if (graphics-auto-crop?) "false" "true")
    (graphics-set-property "gr-auto-crop" new)))

(define (graphics-has-crop-padding? val)
  (== (graphics-get-property "gr-crop-padding") val))

(tm-define (graphics-set-crop-padding val)
  (:argument val "Padding around cropped graphics")
  (:check-mark "*" graphics-has-crop-padding?)
  (graphics-set-property "gr-crop-padding" val))

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
        (with magn (multiply-magnify (graphics-get-property "magnify") e)
          (graphics-decorations-reset)
          (graphics-set-property "gr-frame" newfr)
          (graphics-set-property "magnify" magn)))))

(tm-define (graphics-set-zoom z)
  (with magn (graphics-get-property "magnify")
    (if (or (not magn) (== magn "default")) (set! magn "1"))
    (graphics-zoom (/ z (string->number magn)))))

(tm-define (graphics-move-origin dx dy)
  (define (add l1 l2)
    (if (pair? l1)
        `(tmlen ,(f2s (+ (s2f (cadr l1)) (length-decode l2))))
        (length-add l1 l2)))  
  (when (not (graphics-auto-crop?))
    (let* ((fr (graphics-cartesian-frame))
           (x (cadr (cadddr fr)))
           (y (caddr (cadddr fr)))
           (newfr `(tuple "scale" ,(caddr fr)
                          (tuple ,(add x dx) ,(add y dy)))))
      (graphics-decorations-reset)
      (graphics-set-property "gr-frame" newfr))))

(tm-define (graphics-change-extents dw dh)
  (when (not (graphics-auto-crop?))
    (let* ((geo (graphics-geometry))
           (w (caddr geo))
           (h (cadddr geo)) 
           (w2 (length-add w dw))
           (h2 (length-add h dh)))
      (if (> (length-decode w2) 0)
          (set! w w2))
      (if (> (length-decode h2) 0)
          (set! h h2))
      (graphics-decorations-reset)
      (graphics-set-extents w h))))

(tm-define (graphics-change-extents dw dh)
  (:require (tree-innermost 'draw-over #t))
  (and-with t (tree-innermost 'draw-over #t)
    (and-with l (tree-ref t 2)
      (let* ((d (if (!= (length-decode dw) 0) dw dh))
             (old-pad (tree->stree l))
             (new-pad (length-add old-pad d)))
        (when (<= (length-decode new-pad) 0)
          (set! new-pad "0cm"))
        (tree-set l new-pad)))))

(tm-define (graphics-change-geo-valign down?)
  (let* ((geo (graphics-geometry))
         (a (car (cddddr geo))))
    (graphics-set-geo-valign
     (if down?
         (cond ((== a "top") "center")
               ((== a "center") "bottom")
               ((== a "bottom") "top")
               (else "default"))
         (cond ((== a "top") "bottom")
               ((== a "center") "top")
               ((== a "bottom") "center")
               (else "default"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commutative diagrams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-set-notebook-grid)
  (graphics-set-visual-grid 'cartesian)
  (graphics-set-unit "1cm")
  (graphics-set-grid-aspect 'detailed 2 #t)
  (graphics-set-grid-color 'subunits "#e0e0ff")
  (delayed
    (:idle 1)
    (graphics-set-grid-color 'units "#e0e0ff")
    (delayed
      (:idle 1)
      (graphics-set-grid-color 'axes "#e0e0ff"))))

(tm-define (make-cd)
  (make-graphics)
  (delayed
    (:idle 1)
    (graphics-set-extents "8.1cm" "3.1cm")
    (graphics-set-text-at-halign "center")
    (graphics-set-arrow-end "<gtr>")
    (graphics-set-mode '(edit math-at))
    (graphics-set-notebook-grid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree->number x)
  (string->number x))

(define (stree->vector v)
  (map stree->number (cdr v)))

(define (stree->matrix m)
  (map stree->vector (cdr m)))

(define (number->stree x)
  (number->string x))

(define (vector->stree v)
  (cons 'tuple (map number->stree v)))

(define (matrix->stree m)
  (cons 'tuple (map vector->stree m)))

(tm-define (graphics-transformation)
  (stree->matrix (tree->stree (get-env-tree "gr-transformation"))))

(tm-define (graphics-set-transformation m)
  (graphics-set-property "gr-transformation" (matrix->stree m)))

(tm-define (xz-rotation a)
  (list (list (cos a) 0.0 (sin a) 0.0)
        (list 0.0 1.0 0.0 0.0)
        (list (- (sin a)) 0.0 (cos a) 0.0)
        (list 0.0 0.0 0.0 1.0)))

(define (yz-rotation a)
  (list (list 1.0 0.0 0.0 0.0)
        (list 0.0 (cos a) (sin a) 0.0)
        (list 0.0 (- (sin a)) (cos a) 0.0)
        (list 0.0 0.0 0.0 1.0)))

(define (matrix-columns m)
  (if (null? m) 0 (length (car m))))

(define (matrix-column m i)
  (map (cut list-ref <> i) m))

(define (matrix-transpose m)
  (map (cut matrix-column m <>) (.. 0 (matrix-columns m))))

(define (vector-vector-inner v w)
  (if (or (null? v) (null? w)) 0.0
      (+ (* (car v) (car w)) (vector-vector-inner (cdr v) (cdr w)))))

(define (vector-matrix-inner v m)
  (map (cut vector-vector-inner v <>) m))

(tm-define (matrix-multiply m1 m2)
  (map (cut vector-matrix-inner <> (matrix-transpose m2)) m1))

(tm-define (graphics-rotate-xz a)
  (with m (graphics-transformation)
    (graphics-set-transformation (matrix-multiply (xz-rotation a) m))))

(tm-define (graphics-rotate-yz a)
  (with m (graphics-transformation)
    (graphics-set-transformation (matrix-multiply (yz-rotation a) m))))

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
	   (set! graphics-current-type "empty"))
	  ((match? grid '(tuple "cartesian"))
	   (set! graphics-current-type "cartesian"))
	  ((match? grid '(tuple "cartesian" :%1))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-step (list-ref grid 2)))
	  ((match? grid '(tuple "cartesian" :%2))
	   (set! graphics-current-type "cartesian")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3)))
	  ((match? grid '(tuple "polar"))
	   (set! graphics-current-type "polar"))
	  ((match? grid '(tuple "polar" :%1))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2)))
	  ((match? grid '(tuple "polar" :%2))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-astep (list-ref grid 3)))
	  ((match? grid '(tuple "polar" :%3))
	   (set! graphics-current-type "polar")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-astep (list-ref grid 4)))
	  ((match? grid '(tuple "logarithmic"))
	   (set! graphics-current-type "logarithmic"))
	  ((match? grid '(tuple "logarithmic" :%1))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2)))
	  ((match? grid '(tuple "logarithmic" :%2))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-step (list-ref grid 2))
	   (set! graphics-current-base (list-ref grid 3)))
	  ((match? grid '(tuple "logarithmic" :%3))
	   (set! graphics-current-type "logarithmic")
	   (set! graphics-current-center (list-ref grid 2))
	   (set! graphics-current-step (list-ref grid 3))
	   (set! graphics-current-base (list-ref grid 4))))))

(tm-define (graphics-get-grid-type visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string->symbol graphics-current-type))

(define (get-actual-grid-type visual?)
  (with grid (tree->stree (get-env-tree (if visual? "gr-grid" "gr-edit-grid")))
    (and (pair? grid) (> (length grid) 1) (cadr grid))))

(define (graphics-set-grid visual?)
  (let* ((type     (string->symbol graphics-current-type))
	 (center   graphics-current-center)
	 (step     graphics-current-step)
	 (astep    graphics-current-astep)
	 (base     graphics-current-base)
	 (prop     (if visual? "gr-grid" "gr-edit-grid"))
	 (prop-old (if visual? "gr-grid-old" "gr-edit-grid-old"))
	 (the-grid #f))
    (cond ((== type 'empty)
	   (set! the-grid `(tuple "empty")))
	  ((== type 'cartesian)
	   (set! the-grid `(tuple "cartesian" ,center ,step)))
	  ((== type 'polar)
	   (set! the-grid `(tuple "polar" ,center ,step ,astep)))
	  ((== type 'logarithmic)
	   (set! the-grid `(tuple "logarithmic" ,center ,step ,base))))
    (if the-grid
        (with grid-old (tree->stree (get-env-tree prop-old))
          (if (and (== (get-actual-grid-type visual?) "empty")
                   (> (length grid-old) 1)
                   (== (cadr the-grid) (cadr grid-old)))
              (graphics-set-property prop grid-old)
              (begin
                (graphics-set-property prop the-grid)
                (if (!= type 'empty)
                    (graphics-set-property prop-old the-grid))))))
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
             (!= type (string->symbol graphics-current-type)))
        (let* ((aspect (graphics-grid-aspect #t))
               (nsubds (aspect-ref aspect 3)))
          (if nsubds (set! nsubds (cadr nsubds)))
          (set! new-polar? #t)
          (graphics-set-grid-aspect 'detailed nsubds #t)
          (set! graphics-current-astep (f2s default-polar-astep))))
    (set! graphics-current-type (symbol->string type))
    (graphics-set-grid #t)
    (graphics-fetch-grid-vars type #t)
    (if new-polar?
        (begin
          (set! graphics-current-type (symbol->string type))
          (set! graphics-current-astep (f2s default-polar-astep))
          (update-edit-grid 'grid-change)))))

(define (edit-type-has-value? type)
  (graphics-fetch-grid-vars #f #f)
  (set! type (cadr type))
  (if (== type 'default) (set! type 'empty))
  (== type (string->symbol graphics-current-type)))

(tm-define (graphics-set-edit-grid type)
  (:check-mark "*" edit-type-has-value?)
  (cond ((or (== type 'default)
	     (== type 'grid-change))
	 (let* ((aspect (graphics-grid-aspect-props))
		(nsubds0 (cadr (list-ref aspect (- (length aspect) 1))))
		(nsubds (if (number? nsubds0)
			    nsubds0
			    (if (string? nsubds0)
				(string->number nsubds0)
				#f))))
           (if (or (== nsubds #f) (not (grid-aspect-show-subunits?)))
               (set! nsubds 1))
           (if (== type 'default)
               (graphics-fetch-grid-vars 'cartesian #t))
           (if (!= graphics-current-type "logarithmic")
               (graphics-set-grid-aspect 'update nsubds #f))
           (graphics-set-grid #f)))
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
  (if (not visual?) (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-center `(point ,x ,y))
  (graphics-set-grid visual?))

(define (grid-step-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number=? val graphics-current-step))

(tm-define (graphics-set-grid-step val visual?)
  (:check-mark "*" grid-step-has-value?)
  (if (not visual?) (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-step val)
  (graphics-set-grid visual?))

(define (grid-astep-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number=? val graphics-current-astep))

(tm-define (graphics-set-grid-astep val visual?)
  (:check-mark "*" grid-astep-has-value?)
  (if (not visual?) (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-astep val)
  (graphics-set-grid visual?))

(define (grid-base-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (== val graphics-current-base))

(tm-define (graphics-set-grid-base val visual?)
  (:check-mark "*" grid-base-has-value?)
  (if (not visual?) (grid-as-visual-grid! #f))
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
  (:argument c0 "Color axes")
  (:argument c1 "Color units")
  (:argument s2 "Subdivisions per unit")
  (:argument c2 "Color subdivisions")
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
	      (set! res (get-default-val "gr-grid-aspect"))))))
  (cons 'tuple (sort (cdr res) cmp-aspect-items)))

(define (graphics-grid-aspect visual?)
  (with gr (if visual? "gr-grid-aspect" "gr-edit-grid-aspect")
    (with aspect (tree->stree (get-env-tree gr))
      (if (not (match? aspect '(tuple (tuple :%2) (tuple :%2) :*)))
          (set! res (get-default-val gr)))
      (cons 'tuple (sort (cdr aspect) cmp-aspect-items)))))

(define (aspect-ref a i)
  (and (pair? a) (integer? i) (> (length a) i) (list-ref a i)))

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
                     (val (cadr (list-ref aspect 3))))
                (== (cadr ref) val)))
          #f))))

(tm-define (graphics-set-grid-aspect type nsubd visual?)
  (:check-mark "*" nsubd-has-value?)
  (if visual?
      (with aspect (graphics-grid-aspect-props)
	(cond ((== type 'units-only)
	       (graphics-set-property "gr-grid-aspect-props" aspect)
	       (set-cdr! (cddr aspect) '())
	       (graphics-set-property "gr-grid-aspect" aspect))
	      ((== type 'detailed)
	       (if nsubd
		   (set-car! (cdr (list-ref aspect 3))
                             (or (and (string? nsubd) nsubd)
                                 (number->string nsubd)))
		   (set-car! (cdr (list-ref aspect 3))
			     (cadr (list-ref
                                    (get-default-val "gr-grid-aspect")
                                    3))))
	       (graphics-set-property "gr-grid-aspect" aspect)
	       (graphics-set-property "gr-grid-aspect-props" aspect)))
	(update-edit-grid 'default))
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
      (lambda (x)
        (graphics-set-grid-aspect 'detailed (string->number x) visual?))
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
	 (ref-props (aspect-ref aspect-props i)))
    (if (== color "default")
	(let* ((aspect (get-default-val "gr-grid-aspect"))
	       (ref2 (aspect-ref aspect i)))
          (and ref2 (or ref ref-props)
               (== (caddr ref2) (if ref (caddr ref) (caddr ref-props)))))
	(if ref
	    (== color (caddr ref))
	    (== color (caddr ref-props))))))

(tm-define (graphics-set-grid-color where color)
  (:check-mark "*" grid-color-has-value?)
  (define i 0)
  (let* ((i (grid-aspect-ofs where))
	 (aspect (graphics-grid-aspect #t))
	 (aspect-props (graphics-grid-aspect-props)))
    (if i
        (begin
          (if (== color "default")
              (let* ((aspect (get-default-val "gr-grid-aspect"))
                     (ref2 (aspect-ref aspect i)))
                (set! color (caddr ref2))))
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
  (let* ((grid (tree->stree (get-env-tree "gr-grid")))
	 (aspect (tree->stree (get-env-tree "gr-grid-aspect"))))
    (and (pair? grid) (pair? aspect) (> (length aspect) 3))))

(tm-define (grid-toggle-show-subunits)
  (:check-mark "v" grid-show-subunits?)
  (if (grid-show-subunits?)
      (graphics-set-grid-aspect 'units-only #f #t)
      (graphics-set-grid-aspect 'detailed #f #t)))

;; Toggling grids

(define (test-grid*? visual?)
  (let* ((prop (if visual? "gr-grid" "gr-edit-grid"))
	 (prop-old (if visual? "gr-grid-old" "gr-edit-grid-old"))
	 (p (cDr (cursor-path)))
	 (gr (get-upwards-property p prop))
	 (gr-old (get-upwards-property p prop-old)))
    (if (!= gr-old nothing)
        (and (!= gr nothing) (!= (cadr gr) "empty"))
        (!= gr nothing))))

(define (graphics-toggle-grid* visual?)
  (let* ((prop (if visual? "gr-grid" "gr-edit-grid"))
	 (prop-old (if visual? "gr-grid-old" "gr-edit-grid-old"))
	 (p (cDr (cursor-path)))
	 (gr (get-upwards-property p prop))
	 (gr-old (get-upwards-property p prop-old)))
    (if (!= gr-old nothing)
        (if (or (== gr nothing)
                (== (cadr gr) "empty"))
            (graphics-set-property prop gr-old)
            (graphics-set-property prop '(tuple "empty")))
        (if (!= gr nothing)
            (begin
              (graphics-set-property prop '(tuple "empty"))
              (graphics-set-property prop-old gr))))))

(define (test-visual-grid?) (test-grid*? #t))
(tm-define (graphics-toggle-visual-grid)
  (:check-mark "v" test-visual-grid?)
  (graphics-toggle-grid* #t))

(define (test-logical-grid?) (test-grid*? #f))
(tm-define (graphics-toggle-logical-grid)
  (:check-mark "v" test-logical-grid?)
  (graphics-toggle-grid* #f))

(define (test-grid?) (and (test-grid*? #f) (test-grid*? #t)))
(tm-define (graphics-toggle-grid)
  (:check-mark "v" test-grid?)
  (if (grids-defaulted?)
      (graphics-set-visual-grid 'cartesian)
      (begin
        (graphics-toggle-grid* #t)
        (graphics-toggle-grid* #f))))

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
;; Provisos for graphical objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-get-proviso)
  (graphics-get-property "gr-proviso"))

(define (graphics-test-proviso? val)
  (with old (graphics-get-property "gr-proviso")
    (or (and (func? val 'quasiquote 1)
             (graphics-test-proviso? (cadr val)))
        (and (pair? val) (pair? old)
             (== (car val) (car old)))
        (== val old))))

(tm-define (graphics-set-proviso val)
  (:argument val "Proviso")
  (:check-mark "*" graphics-test-proviso?)
  (graphics-set-property "gr-proviso" val))

(define (update-proviso-sub l val)
  (when (and (nnull? l) (nnull? (cdr l)))
    (when (and (tm-equal? (car l) "gr-proviso")
               (tree-compound? (cadr l))
               (== (tree-arity (cadr l)) 1)
               (tree-atomic? (tree-ref (cadr l) 0)))
      (tree-set (tree-ref (cadr l) 0) val))
    (update-proviso-sub (cddr l) val)))

(tm-define (graphics-update-proviso t val)
  (when (tree-compound? t)
    (when (tree-is? t 'with)
      (update-proviso-sub (cDr (tree-children t)) val))
    (for-each (cut graphics-update-proviso <> val) (tree-children t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes for graphical objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-test-property? var)
  (lambda (val)
    (or (== val (graphics-get-property var))
        (and (== (graphics-get-property var) "default")
             (== val (graphics-attribute-default var))))))

(tm-define (graphics-set-opacity val)
  (:argument val "Opacity")
  (:check-mark "*" (graphics-test-property? "gr-opacity"))
  (graphics-set-property "gr-opacity" val))

(tm-define (graphics-set-color val)
  (:argument val "Color")
  (:check-mark "*" (graphics-test-property? "gr-color"))
  (graphics-set-property "gr-color" val))

(tm-define (graphics-set-point-style val)
  (:argument val "Point style")
  (:check-mark "*" (graphics-test-property? "gr-point-style"))
  (graphics-set-property "gr-point-style" val))

(tm-define (graphics-set-point-size val)
  (:argument val "Point size")
  (:check-mark "*" (graphics-test-property? "gr-point-size"))
  (graphics-set-property "gr-point-size" val))

(tm-define (graphics-set-point-border val)
  (:argument val "Point border")
  (:check-mark "*" (graphics-test-property? "gr-point-border"))
  (graphics-set-property "gr-point-border" val))

(tm-define (graphics-set-line-width val)
  (:argument val "Line width")
  (:check-mark "*" (graphics-test-property? "gr-line-width"))
  (graphics-set-property "gr-line-width" val))

(tm-define (graphics-set-dash-style val)
  (:argument val "Dash style")
  (:check-mark "*" (graphics-test-property? "gr-dash-style"))
  (graphics-set-property "gr-dash-style" val))

(tm-define (graphics-set-dash-style-unit val)
  (:argument val "Dash style unit")
  (:check-mark "*" (graphics-test-property? "gr-dash-style-unit"))
  (graphics-set-property "gr-dash-style-unit" val))

(tm-define (graphics-set-dash-style-unit* hu vu)
  (:argument hu "Horizontal dash style unit")
  (:argument vu "Vertical dash style unit")
  (graphics-set-property "gr-dash-style-unit" `(tuple ,hu ,vu)))

(tm-define (graphics-set-line-portion val)
  (:argument val "Line portion")
  (:check-mark "*" (graphics-test-property? "gr-line-portion"))
  (graphics-set-property "gr-line-portion" val))

(tm-define (graphics-set-fill-color val)
  (:argument val "Fill color")
  (:check-mark "*" (graphics-test-property? "gr-fill-color"))
  (graphics-set-property "gr-fill-color" val))

(tm-define (graphics-set-arrow-begin val)
  (:argument val "Left arrow")
  (:check-mark "*" (graphics-test-property? "gr-arrow-begin"))
  (graphics-set-property "gr-arrow-begin" val))

(tm-define (graphics-set-arrow-end val)
  (:argument val "Right arrow")
  (:check-mark "*" (graphics-test-property? "gr-arrow-end"))
  (graphics-set-property "gr-arrow-end" val))

(tm-define (graphics-set-text-at-halign val)
  (:argument val "Text-at horizontal alignment")
  (:check-mark "*" (graphics-test-property? "gr-text-at-halign"))
  (graphics-set-property "gr-text-at-halign" val))

(tm-define (graphics-set-text-at-valign val)
  (:argument val "Text-at vertical alignment")
  (:check-mark "*" (graphics-test-property? "gr-text-at-valign"))
  (graphics-set-property "gr-text-at-valign" val))

(tm-define (graphics-set-doc-at-valign val)
  (:argument val "Document-at vertical alignment")
  (:check-mark "*" (graphics-test-property? "gr-doc-at-valign"))
  (graphics-set-property "gr-doc-at-valign" val))

(define (graphics-check-width? val)
  (if (== val "1par") (set! val "default"))
  (and (== (graphics-get-property "gr-doc-at-width") val)
       (== (graphics-get-property "gr-doc-at-hmode") "exact")))
(tm-define (graphics-set-doc-at-width val)
  (:argument val "Document-at width")
  (:check-mark "*" graphics-check-width?)
  (graphics-set-property "gr-doc-at-width" val)
  (graphics-set-property "gr-doc-at-hmode" "exact")
  (graphics-set-property "gr-doc-at-ppsep" ""))

(define (graphics-check-compact?)
  (== (graphics-get-property "gr-doc-at-hmode") "default"))
(tm-define (graphics-set-doc-at-compact)
  (:check-mark "*" graphics-check-compact?)
  (graphics-set-property "gr-doc-at-width" "default")
  (graphics-set-property "gr-doc-at-hmode" "default")
  (graphics-set-property "gr-doc-at-ppsep" "default"))

(define (graphics-toggled-property? var)
  (lambda ()
    (!= (graphics-get-property var) "default")))

(tm-define (graphics-toggle-doc-at-border)
  (:check-mark "*" (graphics-toggled-property? "gr-doc-at-border"))
  (if (== (graphics-get-property "gr-doc-at-border") "default")
      (begin
        (graphics-set-property "gr-doc-at-border" "1ln")
        (graphics-set-property "gr-doc-at-padding" "1spc"))
      (begin
        (graphics-set-property "gr-doc-at-border" "default")
        (graphics-set-property "gr-doc-at-padding" "default"))))

(tm-define (graphics-toggle-doc-at-padded)
  (:check-mark "*" (graphics-toggled-property? "gr-doc-at-padding"))
  (if (== (graphics-get-property "gr-doc-at-padding") "default")
      (graphics-set-property "gr-doc-at-padding" "1spc")
      (graphics-set-property "gr-doc-at-padding" "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-snap)
  (with val (graphics-get-property "gr-snap")
    (if (tm-func? val 'tuple) (tm-children val) (list "all"))))

(define (set-snap l)
  (graphics-set-property "gr-snap" `(tuple ,@l)))

(define graphics-snap-types
  (list "control point"
        "grid point" "grid curve point" "curve-grid intersection"
        "curve point" "curve-curve intersection"
         "text border point" "text border"))

(tm-define (graphics-get-snap-mode)
  (tm->tree (if (== (car (graphics-mode)) 'hand-edit)
		`(tuple)
		`(tuple ,@(get-snap)))))

(tm-define (graphics-get-snap-distance)
  (with val (graphics-get-property "gr-snap-distance")
    (if (string? val) val "10px")))

(tm-define (graphics-get-snap type)
  (or (in? type (get-snap))
      (in? "all" (get-snap))))

(tm-define (graphics-test-snap? type)
  (if (== type "none")
      (null? (get-snap))
      (graphics-get-snap type)))

(tm-define (graphics-set-snap type)
  (:check-mark "*" graphics-test-snap?)
  (cond ((== type "none")
         (set-snap (list)))
        ((== type "all")
         (set-snap (list "all")))
        ((nin? type (get-snap))
         (set-snap (cons type (get-snap))))))

(tm-define (graphics-reset-snap type)
  (when (in? "all" (get-snap))
    (set-snap graphics-snap-types))
  (when (in? type (get-snap))
    (set-snap (list-remove (get-snap) type))))

(tm-define (graphics-toggle-snap type)
  (:check-mark "*" graphics-test-snap?)
  (if (graphics-get-snap type)
      (graphics-reset-snap type)
      (graphics-set-snap type)))

(tm-define (graphics-set-snap-distance val)
  (:argument val "Snap distance")
  (:check-mark "*" (graphics-test-property? "gr-snap-distance"))
  (graphics-set-property "gr-snap-distance" val))

(tm-define (graphics-set-snap-text-padding val)
  (:argument val "Text padding for snapping")
  (:check-mark "*" (graphics-test-property? "gr-text-at-margin"))
  (graphics-set-property "gr-text-at-margin" val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special routines for text-at boxes and its variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (object-get-property-bis t var)
  (tree->stree (get-env-tree var)))

(define (with-set t var val i)
  (cond ((>= i (- (tree-arity t) 1))
         (when (!= val "default")
           (tree-insert! t i (list var val))))
        ((tm-equal? (tree-ref t i) var)
         (if (!= val "default")
             (tree-assign (tree-ref t (+ i 1)) val)
             (tree-remove t i 2)))
        (else (with-set t var val (+ i 2)))))

(define (object-set-property-bis t var val)
  (cond ((tree-is? t :up 'with)
         (with-set (tree-up t) var val 0))
        ((!= val "default")
         (tree-set! t `(with ,var ,val ,t)))))

(define (object-test-property? var)
  (lambda (val)
    (if (== val "default") (set! val (tree->stree (get-init-tree var))))
    (== (object-get-property var) val)))

(tm-define (object-get-property var)
  (tree->stree (get-env-tree var)))

(tm-define (object-set-property var val)
  (and-with t (tree-innermost graphical-context?)
    (object-set-property-bis t var val)))

(tm-define (object-set-fill-color val)
  (:argument val "Fill color")
  (:check-mark "*" (object-test-property? "fill-color"))
  (object-set-property "fill-color" val))

(tm-define (object-set-text-at-halign val)
  (:argument val "Horizontal alignment")
  (:check-mark "*" (object-test-property? "text-at-halign"))
  (object-set-property "text-at-halign" val))

(tm-define (object-set-text-at-valign val)
  (:argument val "Vertical alignment")
  (:check-mark "*" (object-test-property? "text-at-valign"))
  (object-set-property "text-at-valign" val))

(tm-define (object-set-doc-at-valign val)
  (:argument val "Vertical alignment")
  (:check-mark "*" (object-test-property? "doc-at-valign"))
  (object-set-property "doc-at-valign" val))

(define (object-check-width? val)
  (and (== (object-get-property "doc-at-width") val)
       (== (object-get-property "doc-at-hmode") "exact")))
(tm-define (object-set-doc-at-width val)
  (:argument val "Document-at width")
  (:check-mark "*" object-check-width?)
  (if (== val "1par") (set! val "default"))
  (object-set-property "doc-at-width" val)
  (object-set-property "doc-at-hmode" "exact")
  (object-set-property "doc-at-ppsep" ""))

(define (object-check-compact?)
  (== (object-get-property "doc-at-hmode") "min"))
(tm-define (object-set-doc-at-compact)
  (:check-mark "*" object-check-compact?)
  (object-set-property "doc-at-width" "default")
  (object-set-property "doc-at-hmode" "default")
  (object-set-property "doc-at-ppsep" "default"))

(define (object-toggled-property? var)
  (lambda ()
    (!= (object-get-property var) (get-init var))))

(tm-define (object-toggle-doc-at-border)
  (:check-mark "*" (object-toggled-property? "doc-at-border"))
  (if (== (object-get-property "doc-at-border") "0ln")
      (begin
        (object-set-property "doc-at-border" "1ln")
        (object-set-property "doc-at-padding" "1spc"))
      (begin
        (object-set-property "doc-at-border" "default")
        (object-set-property "doc-at-padding" "default"))))

(tm-define (object-toggle-doc-at-padded)
  (:check-mark "*" (object-toggled-property? "doc-at-padding"))
  (if (== (object-get-property "doc-at-padding") "0spc")
      (object-set-property "doc-at-padding" "1spc")
      (object-set-property "doc-at-padding" "default")))
