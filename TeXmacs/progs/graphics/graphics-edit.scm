
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-edit)
  (:use (utils library cursor) (utils library tree)))
;; TODO : Remove all the (stree-at), (tree->stree), etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ShiftMask   1)
(define LockMask    2)
(define ControlMask 4)
(define Mod1Mask    8)
(define Mod2Mask   16)
(define Mod3Mask   32)
(define Mod4Mask   64)
(define Mod5Mask  128)

(tm-define (kbd-tab)
  (:mode in-graphics?)
  (graphics-choose-point))

(tm-define (kbd-left)
  (:inside text-at)
  (go-to-remain-inside go-left 'text-at))

(tm-define (kbd-right)
  (:inside text-at)
  (go-to-remain-inside go-right 'text-at))

(tm-define (kbd-up)
  (:inside text-at)
  (go-to-remain-inside go-up 'text-at))

(tm-define (kbd-down)
  (:inside text-at)
  (go-to-remain-inside go-down 'text-at))

(tm-define (kbd-start-line)
  (:inside text-at)
  (with move (lambda () (go-to-remain-inside go-left 'text-at))
    (go-to-repeat move)))

(tm-define (kbd-end-line)
  (:inside text-at)
  (with move (lambda () (go-to-remain-inside go-right 'text-at))
    (go-to-repeat move)))

(define (in-active-graphics?)
  (and (in-graphics?) (== (get-env "preamble") "false")))

(define (graphics-kbd-remove forward?)
  (if (and (with-active-selection?)
	   (with-cursor (rcons (selection-path) 0)
	     (not (in-graphics?))))
      (begin
	(go-to (rcons (selection-path) 0))
	(clipboard-cut "primary"))))

(kbd-map
  (:mode in-active-graphics?)
  ("+" (graphics-zoom (/ 1.0 0.75)))
  ("-" (graphics-zoom 0.75))
  ("left" (graphics-move-origin "+0.1gw" "0gh"))
  ("right" (graphics-move-origin "-0.1gw" "0gh"))
  ("down" (graphics-move-origin "0gw" "+0.1gh"))
  ("up" (graphics-move-origin "0gw" "-0.1gh"))
  ("home" (graphics-zmove 'foreground))
  ("end" (graphics-zmove 'background))
  ("pageup" (graphics-zmove 'closer))
  ("pagedown" (graphics-zmove 'farther))
  ("C-left" (graphics-change-extents "-0.5cm" "0cm"))
  ("C-right" (graphics-change-extents "+0.5cm" "0cm"))
  ("C-down" (graphics-change-extents "0cm" "+0.5cm"))
  ("C-up" (graphics-change-extents "0cm" "-0.5cm"))
  ("M-left"  (if (current-is-textat?)
		 (text-at-change-halign current-path-under-mouse #f)))
  ("M-right" (if (current-is-textat?)
		 (text-at-change-halign current-path-under-mouse #t)))
  ("M-down"  (if (current-is-textat?)
		 (text-at-change-valign current-path-under-mouse #f)
		 (graphics-change-geo-valign #f)))
  ("M-up"    (if (current-is-textat?)
		 (text-at-change-valign current-path-under-mouse #t)
		 (graphics-change-geo-valign #t)))
  ("backspace" (graphics-kbd-remove #f))
  ("delete" (graphics-kbd-remove #t))
  ("C-g" (graphics-toggle-grid #f))
  ("C-G" (graphics-toggle-grid #t)))

(tm-define (inside-draw-over/under?)
  (or (inside? 'draw-over) (inside? 'draw-under)))

(tm-define (graphics-toggle-draw-over/under)
  (with-innermost t (lambda (x) (tree-in? x '(draw-over draw-under)))
    (if (tree-is? t 'draw-over)
	(begin
	  (tree-assign-node! t 'draw-under)
	  (tree-go-to t 0 :end))
	(begin
	  (tree-assign-node! t 'draw-over)
	  (if (tree-is? (tree-ref t 1) 'with)
	      (tree-go-to t 1 (- (tree-arity (tree-ref t 1)) 1) :end)
	      (tree-go-to t 1 :end))))))

(kbd-map
  (:mode inside-draw-over/under?)
  ("C-*" (graphics-toggle-draw-over/under)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-at p)
  (tree->stree (path->tree p)))

(define (get-env-stree var)
  (tree->stree (get-env-tree var)))
  ; NOTE : (graphics-get-property) should supersede this one

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
	 (with o (path->tree p)
	    (if (and (tree? o) (in? (tree-label o) gr-tags-all))
		(begin
		  ;(display* "gp=" (path->tree (cDr path)) "\n")
		   p)
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

(define (list-filter-multiple-elements l)
  (define already '())
  (foreach (e l)
     (if (not (in? e already))
	 (set! already (cons e already)))
  )
  (reverse already))

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

(define (tm-upper-path p tags nottags)
  (if (in? (tree-label (path->tree p)) tags)
      p
      (if (in? (tree-label (path->tree p)) nottags)
	  #f
	  (if (> (length p) 2)
	      (tm-upper-path (reverse (cdr (reverse p))) tags nottags)
	      #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global geometry of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-raw-go-to
   '(with "gr-mode" "point"
	  "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))
	  "gr-geometry" (tuple "geometry" "1par" "0.6par")
      (graphics))
   '(6 1)))

(tm-define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (path-insert-with p var val))))

(tm-define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (path-remove-with p var))))

(tm-define (graphics-frozen-property? var)
  (with val (get-env-tree var)
     (and (tree? val) (== (tree-label val) 'frozen))))

(tm-define (graphics-frozen-property! var b)
  (if b
      (if (not (graphics-frozen-property? var))
	  (graphics-set-property var
	    `(quote (frozen ,(tree->stree (get-env-tree var))))))
      (if (graphics-frozen-property? var)
	  (graphics-set-property var (tree-ref (get-env-tree var) 0)))))

(tm-define (graphics-get-property var)
  (with val (get-env-tree var)
	    ;; FIXME: (get-env-tree) is synchro-unsafe (see below).
     (tree->stree
	(if (graphics-frozen-property? var)
	    (tree-ref val 0)
	    val))))

(tm-define (graphics-change-property var val)
  (if (tree? val)
      (set! val (tree->stree val)))
  (if (graphics-frozen-property? var)
      (graphics-set-property var `(quote (frozen ,val)))
      (graphics-set-property var val)))

(define (graphics-geometry)
  (with geo (tree->stree (get-env-tree "gr-geometry"))
    (if (match? geo '(tuple "geometry" :2))
	(append geo '("center"))
	(if (match? geo '(tuple "geometry" :3))
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

(define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :2))
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
	    (set! graphics-current-astep (i2s default-polar-astep))))
     (set! graphics-current-type (symbol->string type))
     (graphics-set-grid #t)
     (graphics-fetch-grid-vars type #t)
     (if new-polar? (begin
	 (set! graphics-current-type (symbol->string type))
	 (set! graphics-current-astep (i2s default-polar-astep))
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
  (string-number== val graphics-current-step))

(tm-define (graphics-set-grid-step val visual?)
  (:check-mark "*" grid-step-has-value?)
  (if (not visual?)
      (grid-as-visual-grid! #f))
  (graphics-fetch-grid-vars #f visual?)
  (set! graphics-current-step val)
  (graphics-set-grid visual?))

(define (grid-astep-has-value? val visual?)
  (graphics-fetch-grid-vars #f visual?)
  (string-number== val graphics-current-astep))

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
  (let* ((xval (s2i (cadr x)))
	 (yval (s2i (cadr y))))
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
	(update-edit-grid 'default)
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
	    (grid-as-visual-grid! #f)))))

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

(define (graphics-mode)
  (with m (tree->stree (get-env-tree "gr-mode"))
     (cond ((string? m)
	   `(edit ,(string->symbol m)))
	   ((pair? m)
	    (map string->symbol (cdr m))))))

(define (graphics-mode-has-value? mode)
  (if (string? mode)
      (set! mode `(edit ,(string->symbol mode))))
  (if (and (pair? mode) (eq? (car mode) 'quote))
      (set! mode (cadr mode)))
; FIXME: The parameters of a call inside
;   a menu are non evaluated, thus when
;   we write (foo '(a b)) in a menu, we
;   receive (quote (a b)) as a parameter.
;   This is why we had to add the crap (if)
;   above...
  (== mode (graphics-mode)))

(tm-define (graphics-set-mode val)
  (:check-mark "*" graphics-mode-has-value?)
  (graphics-group-start)
  (with old-mode (graphics-mode)
     (graphics-enter-mode old-mode val)
     (graphics-set-property "gr-mode"
	(cond ((or (symbol? val) (string? val))
	       (list 'tuple 'edit val))
	      ((pair? val)
	       (cons 'tuple val))))))

(define (graphics-group-mode? mode)
  (and (pair? mode) (eq? (car mode) 'group-edit)))

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

(define (line-arrows-has-value? arrows)
  (with gr-arrows (graphics-get-property "gr-line-arrows")
     (if (== gr-arrows "default")
	 (set! gr-arrows "none")) ; FIXME: Not sure it's so nice...
     (if (pair? gr-arrows)
       ; FIXME: Shitty workaround around the <quote|none> bug...
	 (set-car! (cddadr gr-arrows) "none"))
     (if (number? arrows)
	 (== (vector-ref default-line-arrows arrows) gr-arrows)
	 (== arrows gr-arrows))))

(tm-define (graphics-set-line-arrows arrows)
  (:argument val "Arrows")
  (:check-mark "*" line-arrows-has-value?)
  (cond ((integer? arrows)
	 (graphics-change-property
	   "gr-line-arrows"
	   (vector-ref default-line-arrows arrows)))
	((pair? arrows)
	 (graphics-change-property "gr-line-arrows" arrows))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-valid-attribute? attr tag)
  (cond ((== tag 'point)
	 (in? attr '("color" "fill-color" "point-style")))
	((in? tag gr-tags-curves)
	 (in? attr '("color" "fill-color" "line-width"
		     "dash-style" "dash-style-unit"
		     "line-arrows")))
	((== tag 'text-at)
	 (in? attr '("text-at-halign" "text-at-valign")))
	((== tag 'gr-group)
	 (in? attr '("color" "fill-color"
		     "point-style" "line-width"
		     "dash-style" "dash-style-unit"
		     "line-arrows"
		     "text-at-halign" "text-at-valign")))
	(else #f)))

(define (graphics-enrich-filter t l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter t (cdr l))))
	(if (or (== (cadr head) "default")
		(== (cadr head) (get-default-val (car head)))
		(not (graphics-valid-attribute? (car head) t)))
	    tail
	    (cons* (car head) (cadr head) tail)))))

(define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter (car t) l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(define (graphics-enrich-bis t color ps lw st stu lp fc ha va)
  (let* ((mode (car t)))
    (cond ((== mode 'point)
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("fill-color" ,fc)
	      ("point-style" ,ps))))
	  ((in? mode gr-tags-curves)
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("line-width" ,lw)
	      ("dash-style" ,st) ("dash-style-unit" ,stu)
	      ("line-arrows" ,lp)
	      ("fill-color" ,fc))))
	  ((== mode 'text-at)
	   (graphics-enrich-sub t
	    `(("text-at-halign" ,ha)
	      ("text-at-valign" ,va))))
	  ((== mode 'gr-group)
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("point-style" ,ps)
	      ("line-width" ,lw)
	      ("dash-style" ,st) ("dash-style-unit" ,stu)
	      ("line-arrows" ,lp)
	      ("fill-color" ,fc)
	      ("text-at-halign" ,ha)
	      ("text-at-valign" ,va))))
	  (else
	   (graphics-enrich-sub t '())))))

(define (graphics-enrich t)
  (let* ((color (graphics-get-property "gr-color"))
	 (ps (graphics-get-property "gr-point-style"))
	 (lw (graphics-get-property "gr-line-width"))
	 (st (graphics-get-property "gr-dash-style"))
	 (stu (graphics-get-property "gr-dash-style-unit"))
	 (lp (graphics-get-property "gr-line-arrows"))
	 (fc (graphics-get-property "gr-fill-color"))
	 (ha (graphics-get-property "gr-text-at-halign"))
	 (va (graphics-get-property "gr-text-at-valign")))
    (graphics-enrich-bis t color ps lw st stu lp fc ha va)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the innermost group of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-group-insert-bis t go-into)
  (let* ((p (graphics-group-path))
	 (p2 #f)
    )
    (if (null? layer-of-last-removed-object)
	(set! layer-of-last-removed-object #f))
    (if p (with n (if layer-of-last-removed-object
		      (if (pair? layer-of-last-removed-object)
			  (with val (car layer-of-last-removed-object)
			     (set! layer-of-last-removed-object
				   (cdr layer-of-last-removed-object))
                             val)
			  layer-of-last-removed-object)
                      (tree-arity (path->tree p)))
	    (path-insert (rcons p n) (list 'tuple t))
	    (if (func? t 'with)
		(if (and go-into (func? (cAr t) 'text-at))
		    (set! p2 (append p (list n (- (length t) 2) 0 0)))
		    (set! p2 (append p (list n (- (length t) 2) 1))))
		(if (and go-into (func? t 'text-at))
		    (set! p2 (append p (list n 0 0)))
		    (set! p2 (append p (list n 0))))
	    )
	    (go-to p2)
	    (graphics-path p2)
	  )
	  #f)))

(define (graphics-group-insert t)
  (graphics-group-insert-bis t #t))

(define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(define (graphics-group-enrich-insert-bis
	 t color ps lw st stu lp fc ha va go-into)
  (graphics-group-insert-bis
    (graphics-enrich-bis t color ps lw st stu lp fc ha va) go-into))

(define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (go-to (rcons p 0)))))

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

(define (graphics-active-property var default-val)
  (with c (graphics-get-property var)
    (if (== c "default") default-val c)))

(define (search-upwards-from p tag)
  (if (null? p)
     '() 
      (with o (path->tree p)
	 (if (== (tree-label o) tag)
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
  (with val ((if (tree? l) tm-find-prop find-prop) l var)
     (if (== val nothing)
	 default
	 val)))

(define (tm-find-prop p var)
  (if (null? p)
      nothing
      (let* ((t (if (tree? p) p (path->tree p)))
	     (n (tree-arity t))
	 )
	 (if (> n 2)
	     (with res nothing
		(foreach-number (i 0 < (- (/ n 2) 1))
		   (if (== (tree->stree (tree-ref t (* 2 i))) var)
		       (set! res (tree->stree (tree-ref t (+ (* 2 i) 1)))))
		)
		res
	     )
	     nothing))))

(define (get-upwards-property p var)
  (if (null? p)
      nothing
      (with q (search-upwards-from p 'with)
	 (if (null? q)
	     nothing
	     (with val (tm-find-prop q var)
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
    (if p (with n (tree-arity (path->tree p))
	    (path-insert (rcons p n) (list 'tuple t))
	    (go-to (rcons p 1))))))

(define (graphics-object-root-path p)
  (let* ((q (search-upwards-from p 'with))
	 (path (if (and (nnull? q)
			(== (+ (length q) 1) (length p)))
		   q p
	       )))
	path))
    
(define (graphics-remove p . parms)
  (with p0 (graphics-object-root-path p)
     (set! layer-of-last-removed-object
	   (if (and (pair? parms) (eq? (car parms) 'memoize-layer))
	       (if (list? layer-of-last-removed-object)
		   (cons (cAr p0) layer-of-last-removed-object)
		   (cAr p0))
	       #f))
     (path-remove p0 1)))

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
(define default-color-go-points "#4040ff")
(define default-color-selected-points "#ff6060")
(define graphical-color "default")
(define graphical-pstyle "default")
(define graphical-lwidth "default")
(define graphical-lstyle "default")
(define graphical-lstyle-unit "default")
(define graphical-larrows "default")
(define graphical-fcolor "default")
(define graphical-textat-halign "default")
(define graphical-textat-valign "default")

(define (graphical-object fetch)
; FIXME: Remove this (tree->stree) should give more speed, but I'm not sure
;   about what is the best way now. Then, directly plug the tree and test
;   the new version of (find-prop-bis) that works directly on trees.
  (with o (tree->stree (get-graphical-object))
   ;(display* "o=" o "\n")
    (if (and fetch (pair? o))
       (begin
	  (set! graphical-color (find-prop-bis o "color" "default"))
	  (set! graphical-pstyle (find-prop-bis o "point-style" "default"))
	  (set! graphical-lwidth (find-prop-bis o "line-width" "default"))
	  (set! graphical-lstyle (find-prop-bis o "dash-style" "default"))
	  (set! graphical-lstyle-unit
		(find-prop-bis o "dash-style-unit" "default"))
	  (set! graphical-larrows (find-prop-bis o "line-arrows" "default"))
	  (set! graphical-fcolor (find-prop-bis o "fill-color" "default"))
	  (set! graphical-textat-halign
		(find-prop-bis o "text-at-halign" "default"))
	  (set! graphical-textat-valign
		(find-prop-bis o "text-at-valign" "default"))))
    (if (pair? o)
	(if (== (cdr o) '()) o (cAr o))
	'(concat))))

(define (graphical-object! obj)
 ;(display* "graphical object=" obj "\n")
  (set-graphical-object (stree->tree obj)))

(define (create-graphical-contour o ha0 va0 halign valign len)
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
	      (equal? va0 "base"))
	 (begin
	    (set! o `(with "text-at-halign" ,ha0
			   "text-at-valign" "bottom" ,o))
	    (let* ((info0 (cdr (box-info o "lbLB")))
		   (b (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
		  )
		  (set! o (list-ref o 5))
		  b)
	 )
	 b0)
  )
  (let* ((o1 (if (in? (car o) '(text-at gr-group))
		`(with "text-at-halign" ,ha0
		       "text-at-valign" ,va0 ,o)
		 o))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (i2s (min (s2i (car  info0)) (s2i (caddr  info0)))))
	 (b0 (i2s (min (s2i (cadr info0)) (s2i (cadddr info0)))))
	 (r (i2s (max (s2i (car  info1)) (s2i (caddr  info1)))))
	 (t (i2s (max (s2i (cadr info1)) (s2i (cadddr info1)))))
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

(define (on-graphical-contour? x y o eps)
  (set! eps (length-decode eps))
  (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
	 (va (get-graphical-prop 'basic "text-at-valign"))
	 (o1 (if (and (pair? o) (eq? (car o) 'text-at))
		`(with "text-at-halign" ,ha
		       "text-at-valign" ,va ,o)
		 o))
	 (info0 (cdr (box-info o1 "lbLB")))
	 (info1 (cdr (box-info o1 "rtRT")))
	 (l (min (s2i (car  info0)) (s2i (caddr  info0))))
	 (b (min (s2i (cadr info0)) (s2i (cadddr info0))))
	 (r (max (s2i (car  info1)) (s2i (caddr  info1))))
	 (t (max (s2i (cadr info1)) (s2i (cadddr info1))))
	 (p (frame-direct `(tuple ,x ,y)))
	)
	(set! x (s2i (cadr p)))
	(set! y (s2i (caddr p)))
	(or (and (in-interval? x (- l eps) l >= <)
		 (in-interval? y (- b eps) (+ t eps) >= <=))
	    (and (in-interval? x r (+ r eps) > <=)
		 (in-interval? y (- b eps) (+ t eps) >= <=))
	    (and (in-interval? x (- l eps) (+ r eps) >= <=)
		 (in-interval? y (- b eps) b >= <))
	    (and (in-interval? x (- l eps) (+ r eps) >= <=)
		 (in-interval? y t (+ t eps) > <=)))))

(define (dv var val)
  (if (== val "default")
      (get-default-val var)
      val)
)
(define (get-graphical-prop mode prop)
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
	     (graphics-path-property mode prop))
	    ((== mode 'new)
	     (graphics-get-property (string-append "gr-" prop)))
	    ((== mode 'default)
	     (get-default-val (string-append "gr-" prop))))
     )
     (dv prop res)))

(define (create-graphical-props mode ps0)
  (let ((color #f)
	(ps #f)
	(lw #f)
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
		 "dash-style" (dv "dash-style" st)
		 "dash-style-unit" (dv "dash-style-unit" stu)
		 "line-arrows" (dv "line-arrows" lp)
		 "fill-color" (dv "fill-color" fc)
		 "text-at-halign" (dv "text-at-halign" ha)
		 "text-at-valign" (dv "text-at-valign" va))))

(define (add-selections-colors op color fill-color)
  (if (not color) (set! color "none"))
  (if (not fill-color) (set! fill-color "none"))
  (list (list 'with "color" color
		    "point-style" "square"
		    "fill-color" fill-color
		    (cons 'concat op))))

;; FIXME: This routine is hardwired to draw the current
;;   selection & the points of the object under cursor.
;;   Generalize it, and reuse it to clean the code around.
(define (create-graphical-contours l mode0 mode)
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
	       (if (equal? path current-path-under-mouse)
		   (set! aobj-selected #t)))))
  (if (and (== mode 'points) current-path-under-mouse)
  (begin
     (set! l (cons (path->tree current-path-under-mouse) l))))
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
	       (set! props (create-graphical-props (if (== mode 'points)
						       'default path)
						   (if (== mode 'object)
						       #f "square")))
	       (if (equal? path current-path-under-mouse)
	       (begin
		  (set! on-aobj #t)
		  (set! curscol default-color-go-points)))
	       (set! path0 path)
	       (set! o (tree->stree o)))
	)
	(if (and (== (car o) 'gr-group) (!= mode 'object))
	    (set! props (create-graphical-props 'default #f)))
	(cond ((== (car o) 'point)
	       (if (not curscol)
		   (set! curscol default-color-selected-points))
	       (set! t (if (== mode 'object)
			  `(,o)
			   (asc curscol #f `(,o))))
	      )
	      ((== (car o) 'text-at)
	       (if (not curscol)
		   (set! curscol default-color-selected-points))
	       (set! t (let* ((ha (get-graphical-prop path0 "text-at-halign"))
			      (va (get-graphical-prop path0 "text-at-valign"))
			      (gc (asc curscol #f
				    (create-graphical-contour
				      o ha va ha va 0.1)))
			  )
			  (if (== mode 'object-and-points)
			      (cons o gc)
			      (if (== mode 'object)
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
				     )
				     (create-graphical-contour
					o ha va "center" "center" 0.1)))
			  (if (== mode 'object-and-points)
			      (cons o gc)
			      (if (== mode 'object)
				 `(,o)
				  gc))))
	      )
	      (else
		 (set! t (if (== mode 'object-and-points)
			     (cons o (asc curscol default-color-selected-points
					  (cdr o)))
			     (if (== mode 'object)
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

(define draw-nonsticky-curp #t)
(define (create-graphical-object o mode pts no)
  (define edge #t)
  (define (curp lp)
     (if draw-nonsticky-curp
	lp '())
  )
  (if (pair? no)
      (begin
	 (set! edge (car no))
	 (set! no (cadr no))))
  (if o (let* ((op
		(cond ((== (car o) 'point)
		       (cons o '())
		      )
		      ((== (car o) 'text-at)
		       (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
			      (va (get-graphical-prop 'basic "text-at-valign"))
			  )
			  (create-graphical-contour o ha va ha va 0.1))
		      )
		      ((== (car o) 'gr-group)
		       (let* ((ha (get-graphical-prop 'basic "text-at-halign"))
			      (va (get-graphical-prop 'basic "text-at-valign"))
			  )
			  (create-graphical-contour
			     o ha va "center" "center" 0.1))
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
				(cdr o))))
	       )
	       (props (create-graphical-props 'default #f))
	   )
	   (if (and pts (!= pts 'points))
	       (set! props (create-graphical-props mode #f)))
	   (set! op (add-selections-colors op default-color-go-points #f))
	   (graphical-object!
	      (if (or (eq? no 'group)
		      (and (not (eq? no 'no-group))
			   (graphics-group-mode? (graphics-mode)))
		  )
		  (cons 'concat
			(create-graphical-contours selected-objects mode pts)
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
(define selecting-x0 #f)
(define selecting-y0 #f)
(define multiselecting #f)
(define current-path-under-mouse #f) ; volatile
(define layer-of-last-removed-object #f)

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
     selected-objects
     selecting-x0
     selecting-y0
     multiselecting))

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
    (state-set! st 'selecting-x0 selecting-x0)
    (state-set! st 'selecting-y0 selecting-y0)
    (state-set! st 'multiselecting multiselecting)
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
  (set! selected-objects (state-ref st 'selected-objects))
  (set! selecting-x0 (state-ref st 'selecting-x0))
  (set! selecting-y0 (state-ref st 'selecting-y0))
  (set! multiselecting (state-ref st 'multiselecting)))

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
  (set! selected-objects '())
  (set! selecting-x0 #f)
  (set! selecting-y0 #f)
  (set! multiselecting #f)
  (set! current-path-under-mouse #f)
  (set! layer-of-last-removed-object #f))

(define (graphics-forget-states)
  (set! graphics-first-state #f)
  (set! graphics-states '())
  (events-forget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for using and maintaining the current graphics context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Graphical select
(define (filter-graphical-select l)
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
  (with l2 (cons 'tuple (map filter-sel (cdr l)))
    (remove-filtered-elts l2)
    (cdr l2)))

(tm-define (graphics-select x y d)
  (with res (tree->stree (graphical-select x y))
   ;(display* "res=" res "\n")
    (filter-graphical-select res)))

(tm-define (graphics-select-area x1 y1 x2 y2)
  (define l '())
  (with res (tree->stree (graphical-select-area x1 y1 x2 y2))
   ;(display* "res=" res "\n")
    (set! res (filter-graphical-select res))
    (foreach (e res)
       (set! l (cons (graphics-path (car e)) l))
    )
    (reverse (list-filter-multiple-elements l))))

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

;; Graphics X cursor
(tm-define graphics-texmacs-pointer 'none)
(define (set-texmacs-pointer curs)
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
(define-macro (with-graphics-context msg x y path obj no edge . body)
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
	    (let* (( sel (select-choose (s2i ,x) (s2i ,y)))
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
     (if (and (not sticky-point)
	      (tm-upper-path (cDr (cursor-path)) '(text-at) '(graphics)))
	 (when-inside-text-at ',func . ,vars)
	,(append (cons
	    'cond
	    (map cond-case vals))
	   `((else (display* "Uncaptured " ',func " " ,obj "\n")))))))

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
	 obj graphical-color graphical-pstyle
	 graphical-lwidth
	 graphical-lstyle
	 graphical-lstyle-unit
	 graphical-larrows
	 graphical-fcolor
	 graphical-textat-halign
	 graphical-textat-valign #f)
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
	(graphics-remove p 'memoize-layer)
	(graphics-group-start)
	(set! sticky-point #t)
	(set! current-point-no no)
	(set! current-edge-sel? #t)
	(set! graphics-undo-enabled #f)
	(if (and edge (not (and (in? (car obj) '(arc carc))
				(> (length obj) 3))))
	  (point_sticky-right-button x y p
	    (cadr (graphical-object #t)) no edge))
	  (graphics-store-state #f))))

(define (text-at_left-button x y p obj no edge)
  (if sticky-point
      (point_left-button x y p obj 1 edge)
      (if (on-graphical-contour? x y obj "1mm")
	  (begin
	     (set-texmacs-pointer 'graphics-cross)
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
	(if p
	    (with p2 (tm-upper-path p '(text-at) '(graphics))
	       (if (not p2) (go-to (rcons p 0))))))))

(define (text-at_move x y p obj no edge)
  (if (and (not sticky-point)
	   (on-graphical-contour? x y obj "1mm"))
      (set-texmacs-pointer 'graphics-cross-arrows)
      (set-texmacs-pointer 'graphics-cross)
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
	(if (or (in? (car obj) gr-tags-oneshot) (null? (cdddr obj))
		(!= (logand (get-keyboard-modifiers) ShiftMask) 0))
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
	(if (!= (logand (get-keyboard-modifiers) ShiftMask) 0)
	    (begin
	      (set-cdr! l (cons (car l) (cdr l)))
	      (set-car! l `(point ,x ,y))
	      (create-graphical-object obj 'active 'object-and-points no)
	      (set! current-point-no no))
	    (begin
	      (set-cdr! l (cons `(point ,x ,y) (cdr l)))
	      (create-graphical-object obj 'active 'object-and-points (+ no 1))
	      (set! current-point-no (+ no 1))))
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
    `(text-at "" (point ,x ,y))))

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
	  (set-texmacs-pointer 'graphics-cross)
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
;; Edit properties (implemented as a group mode, see below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions with check marks for all the properties ("Enable change" menu)
(define (enabled-var? var)
  (not (graphics-frozen-property? var)))

(tm-define (graphics-color-enabled?)
  (enabled-var? "gr-color"))

(tm-define (graphics-toggle-color-enabled)
  (:check-mark "v" graphics-color-enabled?)
  (graphics-frozen-property! "gr-color"
			     (graphics-color-enabled?)))

(tm-define (graphics-point-style-enabled?)
  (enabled-var? "gr-point-style"))

(tm-define (graphics-toggle-point-style-enabled)
  (:check-mark "v" graphics-point-style-enabled?)
  (graphics-frozen-property! "gr-point-style"
			     (graphics-point-style-enabled?)))

(tm-define (graphics-line-width-enabled?)
  (enabled-var? "gr-line-width"))

(tm-define (graphics-toggle-line-width-enabled)
  (:check-mark "v" graphics-line-width-enabled?)
  (graphics-frozen-property! "gr-line-width"
			     (graphics-line-width-enabled?)))

(tm-define (graphics-dash-style-enabled?)
  (enabled-var? "gr-dash-style"))

(tm-define (graphics-toggle-dash-style-enabled)
  (:check-mark "v" graphics-dash-style-enabled?)
  (graphics-frozen-property! "gr-dash-style"
			     (graphics-dash-style-enabled?)))

(tm-define (graphics-dash-style-unit-enabled?)
  (enabled-var? "gr-dash-style-unit"))

(tm-define (graphics-toggle-dash-style-unit-enabled)
  (:check-mark "v" graphics-dash-style-unit-enabled?)
  (graphics-frozen-property! "gr-dash-style-unit"
			     (graphics-dash-style-unit-enabled?)))

(tm-define (graphics-line-arrows-enabled?)
  (enabled-var? "gr-line-arrows"))

(tm-define (graphics-toggle-line-arrows-enabled)
  (:check-mark "v" graphics-line-arrows-enabled?)
  (graphics-frozen-property! "gr-line-arrows"
			     (graphics-line-arrows-enabled?)))

(tm-define (graphics-fill-color-enabled?)
  (enabled-var? "gr-fill-color"))

(tm-define (graphics-toggle-fill-color-enabled)
  (:check-mark "v" graphics-fill-color-enabled?)
  (graphics-frozen-property! "gr-fill-color"
			     (graphics-fill-color-enabled?)))

(tm-define (graphics-textat-halign-enabled?)
  (enabled-var? "gr-text-at-halign"))

(tm-define (graphics-toggle-textat-halign-enabled)
  (:check-mark "v" graphics-textat-halign-enabled?)
  (graphics-frozen-property! "gr-text-at-halign"
			     (graphics-textat-halign-enabled?)))

(tm-define (graphics-textat-valign-enabled?)
  (enabled-var? "gr-text-at-valign"))

(tm-define (graphics-toggle-textat-valign-enabled)
  (:check-mark "v" graphics-textat-valign-enabled?)
  (graphics-frozen-property! "gr-text-at-valign"
			     (graphics-textat-valign-enabled?)))

;; Functions for managing properties
(define (graphics-assign-props p obj mode)
  (let* ((color (graphics-path-property p "color"))
	 (ps (graphics-path-property p "point-style"))
	 (lw (graphics-path-property p "line-width"))
	 (st (graphics-path-property p "dash-style"))
	 (stu (graphics-path-property p "dash-style-unit"))
	 (lp (graphics-path-property p "line-arrows"))
	 (fc (graphics-path-property p "fill-color"))
	 (ha (graphics-path-property p "text-at-halign"))
	 (va (graphics-path-property p "text-at-valign"))
     )
     (graphics-remove p 'memoize-layer)
     (with res
	   (graphics-group-enrich-insert-bis obj
	      (if (graphics-color-enabled?)
		  (graphics-get-property "gr-color") color)
	      (if (graphics-point-style-enabled?)
		  (graphics-get-property "gr-point-style") ps)
	      (if (graphics-line-width-enabled?)
		  (graphics-get-property "gr-line-width") lw)
	      (if (graphics-dash-style-enabled?)
		  (graphics-get-property "gr-dash-style") st)
	      (if (graphics-dash-style-unit-enabled?)
		  (graphics-get-property "gr-dash-style-unit") stu)
	      (if (graphics-line-arrows-enabled?)
		  (graphics-get-property "gr-line-arrows") lp)
	      (if (graphics-fill-color-enabled?)
		  (graphics-get-property "gr-fill-color") fc)
	      (if (graphics-textat-halign-enabled?)
		  (graphics-get-property "gr-text-at-halign") ha)
	      (if (graphics-textat-valign-enabled?)
		  (graphics-get-property "gr-text-at-valign") va) #f)
	(if mode
	    (create-graphical-object obj 'new 'points mode))
	res)))

(define (graphics-copy-props p)
  (let* ((color (graphics-path-property p "color"))
	 (ps (graphics-path-property p "point-style"))
	 (lw (graphics-path-property p "line-width"))
	 (st (graphics-path-property p "dash-style"))
	 (stu (graphics-path-property p "dash-style-unit"))
	 (lp (graphics-path-property p "line-arrows"))
	 (fc (graphics-path-property p "fill-color"))
	 (ha (graphics-path-property p "text-at-halign"))
	 (va (graphics-path-property p "text-at-valign"))
     )
     (if (!= color "default")
	 (graphics-change-property "gr-color" color)
	 (graphics-remove-property "gr-color"))
     (if (!= ps "default")
	 (graphics-change-property "gr-point-style" ps)
	 (graphics-remove-property "gr-point-style"))
     (if (!= lw "default")
	 (graphics-change-property "gr-line-width" lw)
	 (graphics-remove-property "gr-line-width"))
     (if (!= st "default")
	 (graphics-change-property "gr-dash-style" st)
	 (graphics-remove-property "gr-dash-style"))
     (if (!= stu "default")
	 (graphics-change-property "gr-dash-style-unit" stu)
	 (graphics-remove-property "gr-dash-style-unit"))
     (if (!= lp "default")
	 (graphics-change-property "gr-line-arrows" lp)
	 (graphics-remove-property "gr-line-arrows"))
     (if (!= fc "default")
	 (graphics-change-property "gr-fill-color" fc)
	 (graphics-remove-property "gr-fill-color"))
     (if (!= ha "default")
	 (graphics-change-property "gr-text-at-halign" ha)
	 (graphics-remove-property "gr-text-at-halign"))
     (if (!= va "default")
	 (graphics-change-property "gr-text-at-valign" va)
	 (graphics-remove-property "gr-text-at-valign"))))

(define (current-is-textat?)
  (and current-path-under-mouse
       (== (tree-label (path->tree current-path-under-mouse)) 'text-at)))

(define (text-at-change-halign p dirn)
  (let* ((obj (stree-at p))
	 (halign (get-graphical-prop p "text-at-halign"))
	 (valign (get-graphical-prop p "text-at-valign"))
	 (halign2 (if dirn
		      (cond ((== halign "left") "right")
			    ((== halign "center") "left")
			    ((== halign "right") "center")
			    (else "left"))
		      (cond ((== halign "left") "center")
			    ((== halign "center") "right")
			    ((== halign "right") "left")
			    (else "left"))))
     )
     (graphics-remove p 'memoize-layer)
     (set! current-path-under-mouse
	(graphics-group-enrich-insert-bis
	   obj #f #f #f #f #f #f #f halign2 valign #f))
     (create-graphical-object obj '() 'points 'no-group)
     (graphics-group-start)))

(define (text-at-change-valign p dirn)
  (let* ((obj (stree-at p))
	 (halign (get-graphical-prop p "text-at-halign"))
	 (valign (get-graphical-prop p "text-at-valign"))
	 (valign2 (if dirn
		      (cond ((== valign "bottom") "top")
			    ((== valign "base") "bottom")
			    ((== valign "center") "base")
			    ((== valign "top") "center")
			    (else "base"))
		      (cond ((== valign "bottom") "base")
			    ((== valign "base") "center")
			    ((== valign "center") "top")
			    ((== valign "top") "bottom")
			    (else "base"))))
     )
     (graphics-remove p 'memoize-layer)
     (set! current-path-under-mouse
	(graphics-group-enrich-insert-bis
	   obj #f #f #f #f #f #f #f halign valign2 #f))
     (create-graphical-object obj '() 'points 'no-group)
     (graphics-group-start)))

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
	  (cons `(,(car l) ,(cadr l)) (group-list (cddr l) tag)))
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
(define (point_start-operation opn p obj)
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
	     (set! group-old-x (s2i current-x))
	     (set! group-old-y (s2i current-y))))))))

(define (point_toggle-select x y p obj)
  (if (not sticky-point)
  (if multiselecting
      (let* ((x1 (s2i selecting-x0))
	     (y1 (s2i selecting-y0))
	     (x2 (s2i x))
	     (y2 (s2i y))
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
		 (seek-eq?-remove t selected-objects)
		 (set! selected-objects (rcons selected-objects t))
	     )
	     (create-graphical-object obj p 'points #f))
	  (begin
	     (set! selecting-x0 x)
	     (set! selecting-y0 y)
	     (set! multiselecting #t))))))

(define (point_unselect-all p)
  (if (nnull? selected-objects)
  (begin
     (set! selected-objects '())
     (create-graphical-object '(nothing) #f 'points 'group))
  (if (and p (not multiselecting)
	   (== (cadr (graphics-mode)) 'props))
      (graphics-copy-props p))))

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

(define (group-edit_left-button x y)
  (with-graphics-context "start-operation" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       start-operation ('move p obj) do-tick)))

(define (group-edit_right-button x y)
  (with-graphics-context "toggle-select" x y p obj no edge
     (dispatch (car obj) ((point line cline spline cspline arc carc
			   text-at))
	       toggle-select (x y p obj) do-tick)))

(define (group-edit_middle-button x y)
  (with-graphics-context "unselect-all" x y p obj no edge
     (if (!= (logand (get-keyboard-modifiers) ShiftMask) 0)
	 (if (null? selected-objects)
	     (point_middle-button x y p obj no)
	     (remove-selected-objects))
	 (dispatch (car obj) ((point line cline spline cspline arc carc
			       text-at))
		   unselect-all (p) do-tick))))

(define (group-edit_tab-key)
 ;(display* "Graphics] Group-edit(Tab)\n")
  (edit_tab-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut & paste actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-selection-active?)
  (nnull? selected-objects))

(tm-define (graphics-copy)
  (if (== (car (graphics-mode)) 'group-edit)
  (with copied-objects '()
     (foreach (o selected-objects)
     (with p (tm-upper-path (tree->path o) '(with) '())
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing graphics global properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-zoom e)
  (let* ((fr (graphics-cartesian-frame))
	 (u (caddr fr))
	 (newu (length-mult e u))
	 (newud (length-decode newu))
	 (newfr `(tuple "scale" ,newu ,(cAr fr)))
     )
     (if (and (> newud 100) (< newud 10000000))
     (begin
	(create-graphical-object #f #f #f #f)
	(graphics-set-property "gr-frame" newfr)))))

(define (graphics-move-origin dx dy)
  (define (add l1 l2)
     (if (pair? l1)
	`(tmlen ,(i2s (+ (s2i (cadr l1)) (length-decode l2))))
	 (length-add l1 l2))
  )
  (let* ((fr (graphics-cartesian-frame))
	 (x (cadr (cadddr fr)))
	 (y (caddr (cadddr fr)))
	 (newfr `(tuple "scale" ,(caddr fr)
				 (tuple ,(add x dx)
					,(add y dy))))
     )
     (create-graphical-object #f #f #f #f)
     (graphics-set-property "gr-frame" newfr)))

(define (graphics-change-extents dw dh)
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
     (create-graphical-object #f #f #f #f)
     (graphics-set-extents w h)))

(define (graphics-change-geo-valign dirn)
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
;; Events when the cursor is inside a text-at
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (when-inside-text-at func x y)
  (define (uncaptured)
     (if (!= func 'move)
     (begin
	(graphics-group-start)
	(graphics-move-point x y)))
  )
 ;(display* "Inside text-at=" func "; x=" x "; y=" y "\n")
  (with res (with-graphics-context
	       ";insert" x y p obj no edge
		(if (and (pair? obj) (eq? (car obj) 'text-at))
		    (cond
		      ((== func 'left-button)
		       (text-at_left-button x y p "" no edge)))
		    (uncaptured))
	    )
	    (if (not res) (uncaptured))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with superpositions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-zmove dirn)
 ;(display* "dirn(graphics-zmove)=" dirn "\n")
 ;(display* "current-path-under-mouse=" current-path-under-mouse "\n")
  (if current-path-under-mouse
  (let* ((p (graphics-object-root-path current-path-under-mouse))
	 (t (if p (path->tree p) #f))
	 (t0 (if t (path->tree (cDr p)) #f))
     )
    ;(display* "t0=" (get-tag? t0) "\n")
    ;(display* "t=" t "\n")
     (cond ((eq? dirn 'background)
	    (if (> (cAr p) 0)
	    (let* ((p-1 (rcons (cDr p) 0))
		   (t-1 (path->tree p-1))
	       )
	       (path-remove p 1)
	       (path-insert p-1 `(tuple ,(tree->stree t)))
	       (set! current-path-under-mouse p-1)
	    ))
	   )
	   ((eq? dirn 'foreground)
	    (if (< (+ (cAr p) 1) (tree-arity t0))
	    (let* ((p+1 (rcons (cDr p) (- (tree-arity t0) 1)))
		   (t+1 (path->tree p+1))
	       )
	       (path-remove p 1)
	       (path-insert p+1 `(tuple ,(tree->stree t)))
	       (set! current-path-under-mouse p+1)
	    ))
	   )
	   ((eq? dirn 'farther)
	    (if (> (cAr p) 0)
	    (let* ((p-1 (rcons (cDr p) (- (cAr p) 1)))
		   (t-1 (path->tree p-1))
	       )
	       (path-assign p-1 (tree->stree t))
	       (path-assign p (tree->stree t-1))
	       (set! current-path-under-mouse p-1)
	    ))
	   )
	   ((eq? dirn 'closer)
	    (if (< (+ (cAr p) 1) (tree-arity t0))
	    (let* ((p+1 (rcons (cDr p) (+ (cAr p) 1)))
		   (t+1 (path->tree p+1))
	       )
	       (path-assign p+1 (tree->stree t))
	       (path-assign p (tree->stree t+1))
	       (set! current-path-under-mouse p+1)
	    ))
	   )
	   (else #t)
     )
     (set! selected-objects '())
     (graphics-group-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (group-edit))
	    left-button (x y)))

(tm-define (graphics-move-point x y)
  ;(display* "Graphics] Move " x ", " y "\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (group-edit))
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

(tm-define (graphics-start-right-drag x y)
  ;(display* "Graphics] Start-right-drag " x ", " y "\n")
  (graphics-last-point x y))

(tm-define (graphics-right-dragging x y)
  ;(display* "Graphics] right-dragging " x ", " y "\n")
  (graphics-move-point x y))

(tm-define (graphics-end-right-drag x y)
  ;(display* "Graphics] End-right-drag " x ", " y "\n")
; FIXME : test due to timing problems in detecting the drag
  (if (not sticky-point)
      (graphics-last-point x y)))

(tm-define (graphics-choose-point)
  ;(display* "Graphics] Choose\n")
  (dispatch (car (graphics-mode))
	    ((edit)
	     (group-edit))
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
      (begin
	 (if sticky-point (undo))
	 (create-graphical-object '(nothing) #f 'points 'group))))

(define (graphics-finish)
  ;;(display* "Graphics] Finish\n")
  (with mode (graphics-mode)
    (cond ((== (car mode) 'edit)
	  (with submode (cadr mode)
	     (cond ((== submode 'point) (noop))
		   ((in? submode gr-tags-curves) (noop))
		   ((== submode 'text-at) (noop))
		   (else (display* "Uncaptured finish (edit)\n")))))
	 ((== (car mode) 'group-edit)
	   (noop))
	 (else (display* "Uncaptured finish\n")))))
