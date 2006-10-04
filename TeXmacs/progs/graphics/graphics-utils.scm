
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-utils.scm
;; DESCRIPTION : utilities routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-utils)
  (:use (utils library cursor) (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic scheme processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: Except for the (define-export-macro) problem, this section is OK.
(define-macro (define-export-macro head . body)
 `(begin
     (define-macro ,(car head)
        (lambda ,(cdr head) ,@body)
     )
     (export ,(car head))))
     (export define-export-macro)
  ;; NOTE: It appears that (define-public-macro) is *NOT* equivalent
  ;;   to (define-macro) + (export). The appropriate macro declaration
  ;;   function is the current one, otherwise, strange bugs happen, namely,
  ;;   crashing when accessing unbound variables inside complex macros,
  ;;   and errors during accessing the Scheme namespace, too (i.e. : the
  ;;   same variable gives one value at one point in the code, and a little
  ;;   bit later, something else).
  ;;   It seems that as soon as macros become a little bit complex (c.f.
  ;;   for example (with-graphics-context) or (dispatch)), the Guile
  ;;   macroexpander interacts poorly with the memoizing stuff in
  ;;   (define-public-macro), and then it becomes unstable.
  ;;
  ;; TODO: Solve this problem.

;; Conversions
(tm-define (int->string s)
  (number->string s))

(tm-define (string->int s)
  (exact->inexact (string->number s)))
  
(tm-define (object->string o) ;; 1st defined in Scheme/evaluate.cpp
  (with-output-to-string (lambda () (write o))))

(tm-define (string->object s) ;; defined in library/base.scm
  (with-input-from-string s (lambda () (read))))

(tm-define (tree->object t)
  (if (tree? t)
      (tree->stree t)
      t))

(tm-define (object->tree o)
  (if (tree? o)
      o
      (stree->tree o)))
;;TODO: Put these functions in library/base.scm

;;These abbreviations are very convenient
;;to use. A nice naming scheme is :
;;
;;  -> b=bool ;
;;  -> i=integer ;
;;  -> f=float ;
;;  -> sy=symbol ;
;;  -> s=string ;
;;  -> o=Scheme object ;
;;  -> p=path.
;;  -> t=tree.
;;
;;  One can add the missing ones on demand.
(tm-define i2s int->string)
(tm-define s2i string->int)
(tm-define sy2s symbol->string)
(tm-define s2sy string->symbol)
(tm-define o2s object->string)
(tm-define s2o string->object)
(tm-define t2o tree->object)
(tm-define o2t object->tree)
;;TODO: Put these abbreviations in library/base.scm

(tm-define (string-number== s1 s2)
  (if (and (string? s1) (string? s2))
      (let* ((i1 (s2i s1))
             (i2 (s2i s2)))
            (if (and i1 i2) (== i1 i2) #f)
      )     
      #f))
;;TODO: Put this in library/base.scm

(tm-define (tm-eq? o1 o2)
  (or (and (tree? o1) (tree? o2)
           (equal? (tree-ip o1) (tree-ip o2))
      )
      (eq? o1 o2)))
;;TODO: Put this in library/content.scm

(define (tm-tag tree)
  (if (pair? tree) 
      (car tree)
      (if (compound-tree? tree)
          (tree-label tree)
	  (if (tree? tree)
	      'string
	      #f))))

(define (has-tag? tree tag)
  (eq? (tm-tag tree) tag))
;;TODO: Put these two in library/content.scm
;;FIXME: Improve the inefficient implementation of (tm-car)
      
(tm-define (list-find-cons l pred? . opt)
; Similar to (list-find) ; the parameter opt works like in (seek-eq?)
  (define res #f)
  (define prec #f)
  (for@ (e l)
     (if (not res)
	 (if (pred? (car e))
	     (set! res e)
	     (set! prec e)))
  )
  (if (and (nnull? opt) (pair? (car opt)))
      (set-car! (car opt) (if res prec #f)))
  res)

(define-export-macro (list-find&remove l pred)
 `(with prec '(#f)
     (if (list-find-cons ,l ,pred prec)
	 (if (car prec)
	     (begin
		(set! prec (car prec))
		(set-cdr! prec (cddr prec)))
	     (set! ,l (cdr ,l))))))
;;TODO: Put these two in library/list.scm

(tm-define (seek-eq? obj l . opt)
; (with prec '(#f) (seek-eq? 2 '(1 2 3) prec)) -> (2 3); prec=((1 2 3))"
; (with prec '(#f) (seek-eq? 1 '(1 2 3) prec)) -> (1 2 3); prec=(#f)"
; (with prec '(#f) (seek-eq? "1" '("1" 2 3) prec)) -> #f ; prec=(#f)"
; This routine works if obj is a tree, as well.
  (define prec '(#f))
  (if (and (nnull? opt) (pair? (car opt)))
      (set! prec (car opt)))
  (list-find-cons l (lambda (x) (tm-eq? x obj)) prec))

(define-export-macro (seek-eq?-remove obj l)
 `(list-find&remove ,l (lambda (x) (tm-eq? x ,obj))))
;;TODO: Put these two in library/list.scm

(tm-define (list-filter-multiple-elements l)
  (define already '())
  (foreach (e l)
     (if (not (in? e already))
	 (set! already (cons e already)))
  )
  (reverse already))
;;TODO: Put this in library/list.scm

(define-export-macro (For what . body)
  (cond ((list-2? what)
        ;;(for (elt list) body[elt])
         `(for-each (lambda (,(car what)) ,@body)
                    ,(cadr what)))
        ((list-3? what)
        ;;(for (i i0 iN) body[i])
         `(for-each (lambda (,(car what)) ,@body)
                    (.. ,(cadr what) ,(caddr what))))
        ((list-4? what)
        ;;(for (i i0 [< <= > >=] iN) body[i])
	 `(do ((,(car what) ,(cadr what)
	       (,(if (memq (caddr what) '(> >=)) '- '+) ,(car what) 1)))
	      ((,(if (eq? (caddr what) '>)
		    '<=
		     (if (eq? (caddr what) '<)
			'>=
			 (if (eq? (caddr what) '>=) '< '>)))
		,(car what) ,(cadddr what))
	      ,(car what))
	     ,(cons 'begin body)))
        (else '(noop))))
;;TODO: Put this in boot/abbrevs.scm

(define-public foreach For)
(define-public foreach-number For)
;;TODO: Put these two in boot/abbrevs.scm

(define-export-macro (for@ i . b)
;;(for@ (e l) i   body[elt]) -> for each cons e of the list l
;;(for@ (e l1 l2) body[elt]) -> for each cons e in the cons interval [l1...l2]
  (if (null? (cddr i))
    `(do ((,(car i) ,(cadr i) (cdr ,(car i)))
         )
         ((null? ,(car i)) ,(cadr i))
        ,(cons 'begin b)
     )
    `(do ((,(car i) ,(cadr i) (cdr ,(car i)))
         )
         ((or (null? ,(car i))
              (eq? ,(car i) ,(caddr i))
          )
          ,(cadr i)
         )
        ,(cons 'begin b)
     )))
;;TODO: Extend (for@) for recursively traversing tree nodes.
;;TODO: Put this in boot/abbrevs.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Should use (tm-adjust-path), otherwise, crashes in some cases
(tm-define (tm-upwards-path p tags nottags)
  (if (in? (tree-label (path->tree p)) tags)
      p
      (if (in? (tree-label (path->tree p)) nottags)
	  #f
	  (if (> (length p) 2)
	      (tm-upwards-path (cDr p) tags nottags)
	      #f))))
;; TODO: Put this one in kernel/library/tree.scm

;;NOTE: This section is OK.
(tm-define nothing (gensym))
(tm-define (list-find-prop l var)
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

(tm-define (tm-find-prop p var)
;;(tm-find-prop '<tree <with|a|1|...>> "a")              -> <tree 1>
;;(tm-find-prop (tree->path '<tree <with|a|1|...>>) "a") -> <tree 1>
  (if (null? p)
      nothing
      (let* ((t (if (tree? p) p (path->tree p)))
	     (n (tree-arity t))
	 )
	 (if (> n 2)
	     (with res nothing
		(foreach-number (i 0 < (- (/ n 2) 1))
		   (if (== (tree->stree (tree-ref t (* 2 i))) var)
		       (set! res (tree-ref t (+ (* 2 i) 1))))
		)
		res
	     )
	     nothing))))

(tm-define (find-prop l var . default)
;;(find-prop '(with "a" 1) "a")             -> 1
;;(find-prop '(with "a" 1) "b" "not found") -> "not found"
;;(find-prop '<tree <with|a|1|...>> "a")    -> 1
  (set! default (if (pair? default) (car default) #f))
  (with val ((if (tree? l) tm-find-prop list-find-prop) l var)
     (if (== val nothing)
	 default
	 (if (tree? val) (tree->stree val) val))))
;; TODO : Put this in utils/library/tree.scm

(tm-define (get-upwards-tree-property p var)
  (if (null? p)
      nothing
      (with q (tm-upwards-path p '(with) '())
	 (if (not q)
	     nothing
	     (with val (tm-find-prop q var)
		(if (== val nothing)
		    (get-upwards-property (cDr q) var)
		    val))))))

(tm-define (get-upwards-property p var)
  (t2o (get-upwards-tree-property p var)))
;; TODO : Put these two in utils/library/tree.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing the graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define gr-tags-all '(point
			 line cline spline cspline
			 arc carc
			 text-at
			 gr-group))
(tm-define gr-tags-curves  '(line cline spline cspline arc carc))
(tm-define gr-tags-oneshot '(point arc carc text-at gr-group))

(tm-define (stree-at p)
  (tree->stree (path->tree p)))
;; TODO: Put this in kernel/library/tree.scm

(tm-define (graphics-graphics-path)
  ;; path to innermost graphics tag
  (let* ((p (cDr (cursor-path)))
	 (t (stree-at p)))
    (if (func? t 'graphics) p
	(with u (tree-innermost 'graphics)
	  (and u (tree->path u))))))

(tm-define (graphics-path path)
  (if (or (null? path) (null? (cdr path)))
      #f
      (with p (cDr path)
	 (with o (path->tree p)
	    (if (and (tree? o) (in? (tree-label o) gr-tags-all))
		(begin
		  ;(display* "gp=" (path->tree (cDr path)) "\n")
		   p)
		(graphics-path (cDr path)))))))

(tm-define (graphics-active-path)
  ;; path to active tag
  (graphics-path (cursor-path)))

(tm-define (graphics-group-path)
  ;; path to innermost group
  (graphics-graphics-path))

(tm-define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-raw-go-to 
   '(with "gr-mode" "point" 
          "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))
          "gr-geometry" (tuple "geometry" "1par" "0.6par")
      (graphics))
   '(6 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing the properties of the graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(define (graphics-get-raw-property var)
  (with val (get-upwards-tree-property (graphics-graphics-path) var)
     (if (eq? val nothing)
	 (get-default-tree-val var)
	 (if (has-tag? val 'quote)
	     (tree-ref val 0)
	     val))))
         
(tm-define (graphics-frozen-property? var)
  (with val (graphics-get-raw-property var)
     (and (tree? val) (== (tree-label val) 'frozen))))
         
(tm-define (graphics-frozen-property! var b)
  (if b
      (if (not (graphics-frozen-property? var))
          (graphics-set-property var
            `(quote (frozen ,(tree->stree (get-env-tree var))))))
      (if (graphics-frozen-property? var)
          (graphics-set-property
	     var (tree-ref (graphics-get-raw-property var) 0)))))
         
(tm-define (graphics-get-property var)
  (with val (graphics-get-raw-property var)
     (tree->stree
        (if (graphics-frozen-property? var)
            (tree-ref val 0) 
            val))))
               
(tm-define (graphics-change-property var val)
  (set! val (t2o val))
  (if (graphics-frozen-property? var)
      (graphics-set-property var `(quote (frozen ,val)))
      (graphics-set-property var val)))

(tm-define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (path-insert-with p var val))))

(tm-define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (path-remove-with p var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (graphics-valid-attribute? attr tag)
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

(tm-define (graphics-enrich-filter t l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter t (cdr l))))
	(if (or (== (cadr head) "default")
		(== (cadr head) (get-default-val (car head)))
		(not (graphics-valid-attribute? (car head) t)))
	    tail
	    (cons* (car head) (cadr head) tail)))))

(tm-define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter (car t) l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(tm-define (graphics-enrich-bis t color ps lw st stu lp fc ha va)
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

(tm-define (graphics-enrich t)
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

;;NOTE: layer-of-last-removed-object is part of the environment.
;;TODO: Look at this when the appropriate stuff for environments
;;  will have been implemented.
(tm-define (graphics-group-insert-bis t go-into)
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

(tm-define (graphics-group-insert t)
  (graphics-group-insert-bis t #t))

(tm-define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(tm-define (graphics-group-enrich-insert-bis
	    t color ps lw st stu lp fc ha va go-into)
  (graphics-group-insert-bis
    (graphics-enrich-bis t color ps lw st stu lp fc ha va) go-into))

(tm-define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (go-to (rcons p 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the active tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (graphics-tree path)
  (with p (graphics-path path)
    (if p (path->tree p) #f)))

(tm-define (graphics-object path)
  (with p (graphics-path path)
    (if p (tree->stree (path->tree p)) #f)))

(tm-define (graphics-active-tree)
  (with p (graphics-active-path)
    (if p (path->tree p) #f)))

(tm-define (graphics-active-object)
  (with p (graphics-active-path)
    (if p (tree->stree (path->tree p)) #f)))

(tm-define (graphics-active-type)
  (with t (graphics-active-tree)
    (if t (tm-tag t) #f)))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-active-val var)
  (graphics-active-property var (get-default-val var)))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (get-default-tree-val var)
  (get-init-tree var))

(tm-define (get-default-val var)
  (tree->stree (get-init-tree var)))

(tm-define (graphics-active-color)
  (graphics-active-val "color"))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-active-lwidth)
  (graphics-active-val "line-width"))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-path-property-bis p var default-val)
  (with c (get-upwards-property p var)
    (if (== c nothing) default-val c)))

(tm-define (graphics-path-property p var)
  (graphics-path-property-bis p var "default"))

(tm-define (graphics-active-property var default-val)
  (graphics-path-property-bis (graphics-active-path) var default-val))

(tm-define (graphics-active-assign t)
  (with p (graphics-active-path)
    (if p (begin
	    (path-assign p t)
	    (go-to (rcons p 1))))))

(tm-define (graphics-active-set-tag l)
  (with t (graphics-active-object)
    (if t (graphics-active-assign (cons l (cdr t))))))

(tm-define (graphics-active-insert t)
  (with p (graphics-active-path)
    (if p (with n (tree-arity (path->tree p))
	    (path-insert (rcons p n) (list 'tuple t))
	    (go-to (rcons p 1))))))

(tm-define (graphics-object-root-path p)
  (let* ((q (tm-upwards-path p '(with) '()))
	 (path (if (and q
			(== (+ (length q) 1) (length p)))
		   q p
	       )))
	path))
    
;;NOTE: layer-of-last-removed-object is part of the environment.
(tm-define (graphics-remove p . parms)
  (with p0 (graphics-object-root-path p)
     (set! layer-of-last-removed-object
	   (if (and (pair? parms) (eq? (car parms) 'memoize-layer))
	       (if (list? layer-of-last-removed-object)
		   (cons (cAr p0) layer-of-last-removed-object)
		   (cAr p0))
	       #f))
     (path-remove p0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Box info & frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (box-info t cmd)
  (tree->stree (texmacs-exec `(box-info ,t ,cmd))))

(tm-define (frame-direct p)
  (tree->stree (texmacs-exec `(frame-direct ,p))))

(tm-define (frame-inverse p)
  (tree->stree (texmacs-exec `(frame-inverse ,p))))

(tm-define (interval-intersects i1 i2)
  (let* ((i1a (car i1))
	 (i1b (cadr i1))
	 (i2a (car i2))
	 (i2b (cadr i2))
     )
     (or (and (<= i1a i2a) (>= i1b i2b))
	 (and (<= i2a i1a) (>= i2b i1b))
	 (and (>= i1a i2a) (<= i1a i2b))
	 (and (>= i2a i1a) (<= i2a i1b)))))

(tm-define (box-intersects t1 t2)
  (define (max-box t)
     (let* ((bx1 (box-info t "lbrt"))
	    (bx2 (box-info t "LBRT"))
	)
	(set! bx1 (map s2i (cdr bx1)))
	(set! bx2 (map s2i (cdr bx2)))
       `(,(min (car bx1) (car bx2))
	 ,(min (cadr bx1) (cadr bx2))
	 ,(max (caddr bx1) (caddr bx2))
	 ,(max (cadddr bx1) (cadddr bx2))))
  )
  (let* ((box1 (max-box t1))
	 (box2 (max-box t2))
    )
    (and (interval-intersects `(,(car box1) ,(caddr box1))
			      `(,(car box2) ,(caddr box2)))
	 (interval-intersects `(,(cadr box1) ,(cadddr box1))
			      `(,(cadr box2) ,(cadddr box2))))))
