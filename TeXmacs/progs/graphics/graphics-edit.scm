
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-edit)
  (:use (utils library cursor) (utils library tree)
        (graphics graphics-utils) (graphics graphics-menu-functions)
        (graphics graphics-context)))

;; TODO: Oui, ca serait bien de commencer par une reorganisation grossiere.
;; Tu peux systematiser l'utilisation de l'API des arbres (tree-assign, etc.)
;; et rajouter systematiquement des "synopsis" pour les tm-define.
;; Aussi la separation en plusieurs fichiers graphics-edit, graphics-env,
;; graphics-object, etc. serait bienvenue.
;;
;; Ensuite, il faudrait chercher scrupuleusement a factoriser et simplifier
;; le code. Par exemple, des macros comme foreach et foreach-number, etc.,
;; devraient etre dans kernel/boot/abbrevs ou kernel/library (et chercher
;; des noms plus elegants). De maniere plus generale, je pense que
;; des "grosses routines" sont un mauvais style de programmation
;; dans Scheme.
;; En general, ca indique qu'il existe une solution plus elegante.
;;
;; On en discutera davantage apres un premier passage en revue.

;; TODO : Remove all the (stree-at), (tree->stree), etc.
;; TODO : Replace the remaining (tree->stree) by (tree->object) or (t2o)
;; TODO : Except in simple cases, remove all the (tree->stree) which
;;   slow the code, and operate everywhere and all the time on trees.

;; TODO: Remove the synchro-unsafe (get-env) & (get-env-tree) everywhere.

;; TODO: Use systematically (first), (second), etc., instead
;;   of (car), (cadr), etc.

;; TODO : Dans la doc, preciser **exactement** les conditions d'evaluation
;;   des differentes fonctions (par exemple (stree->tree #f) == #f,
;;   mais (stree->tree 1) == <tree 1>.

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
	 (local-magnification graphical-magnification)
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
      (if (on-graphical-embedding-box? x y obj "1mm")
	  (begin
	     (set-texmacs-pointer 'graphics-cross)
	     (point_left-button x y p obj 1 edge))
	  (go-to (car (select-first (s2i x) (s2i y)))))))

(define (other_left-button x y p obj no edge)
  (point_left-button x y p obj no edge))

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
	    (with p2 (tm-upwards-path p '(text-at) '(graphics))
	       (if (not p2) (go-to (rcons p 0))))))))

(define (text-at_move x y p obj no edge)
  (if (and (not sticky-point)
	   (on-graphical-embedding-box? x y obj "1mm"))
      (set-texmacs-pointer 'graphics-cross-arrows)
      (set-texmacs-pointer 'graphics-cross)
  )
  (point_move x y p obj 1 edge))

(define (gr-group_move x y p obj no edge)
  (if sticky-point
      (display* "Sticky move(gr-group) !yet implemented\n")
      (begin
	(create-graphical-object obj p 'points #f))))

(define (other_move x y p obj no edge)
  (point_move x y p obj no edge))

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
		(not (in? (car obj) gr-tags-all))
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

(define (other_middle-button x y p obj no)
  (point_middle-button x y p obj no))

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
	     left-button (x y p obj no edge) do-tick handle-other)))

(define (edit_move x y)
  (with-graphics-context
   ";:move" x y p obj no edge
   (if obj
       (dispatch (car obj) ((point line cline spline cspline arc carc)
			    (text-at)
			    (gr-group))
		 move (x y p obj no edge) do-tick handle-other
       )
       (begin
	  (set-texmacs-pointer 'graphics-cross)
	  (create-graphical-object '(nothing) #f 'points #f)))))

(define (edit_middle-button x y)
  (with-graphics-context
   "remove" x y p obj no edge
   (dispatch (car obj) ((point line cline spline cspline arc carc
			 text-at gr-group))
	     middle-button (x y p obj no) do-tick handle-other)))

(define (edit_right-button x y)
  (if sticky-point
      (with-graphics-context
       "last" x y p obj no edge
       (dispatch (car obj) ((point line cline spline cspline arc carc text-at))
		 sticky-right-button (x y p obj no edge) do-tick))
      (with mode (cadr (graphics-mode))
	(set! layer-of-last-removed-object #f)
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
	 (mag (graphics-path-property-1 p "magnification"))
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
	      mag
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
	 (mag (get-graphical-prop p "magnification"))
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
	   obj #f #f #f mag #f #f #f #f halign2 valign #f))
     (create-graphical-object obj '() 'points 'no-group)
     (graphics-group-start)))

(define (text-at-change-valign p dirn)
  (let* ((obj (stree-at p))
	 (mag (get-graphical-prop p "magnification"))
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
	   obj #f #f #f mag #f #f #f #f halign valign2 #f))
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
	  (with val (if (== (car l) "magnification")
	                (local-magnification (cadr l))
			(cadr l))
	     (cons `(,(car l) ,val) (group-list (cddr l) tag))))
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
     (let* ((res (traverse-transform o (zoom-point group-bary-x group-bary-y h)))
	    (curmag #f)
	    (gmag (s2i (graphics-eval-magnification)))
	)
	(for@ (c (cdr res))
	   (set-car! c (if (eq? (caar c) 'with)
			   (with curmag
				 (s2i (find-prop
					 (car c) "magnification" "1.0"))
			      (list-find&set-prop
				 (car c) "magnification" (i2s (* curmag gmag h))))
			  `(with "magnification" ,(i2s (* gmag h)) ,(car c)))))
	res))))

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
     (with p (tm-upwards-path (tree->path o) '(with) '())
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
     (with magn (multiply-magnification
		   (graphics-get-property "magnification") e)
	(create-graphical-object #f #f #f #f)
	(graphics-set-property "gr-frame" newfr)
	(if magn
	    (graphics-set-property "magnification" magn))))))

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

(define (graphics-zprevious p)
  (let* ((p0 (cDr p))
	 (ofs (cAr p))
	 (res #f)
     )
     (foreach-number (i (- ofs 1) >= 0)
	(with t (path->tree (rcons p0 i))
	   (if (and (not res) (box-intersects t (path->tree p)))
	       (set! res i)))
     )
     res))

(define (graphics-znext p)
  (let* ((p0 (cDr p))
	 (t0 (path->tree p0))
	 (ofs (cAr p))
	 (res #f)
     )
     (foreach-number (i (+ 1 ofs) < (tree-arity t0))
	(with t (path->tree (rcons p0 i))
	   (if (and (not res) (box-intersects t (path->tree p)))
	       (set! res i)))
     )
     res))

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
	    (let* ((no (graphics-zprevious p))
		   (p-1 (if no (rcons (cDr p) no) #f))
		   (t-1 (if p-1 (path->tree p-1) #f))
	       )
	       (if no
	       (begin
		  (path-remove p 1)
		  (path-insert p-1 `(tuple ,(tree->stree t)))
		  (set! current-path-under-mouse p-1)))
	    ))
	   )
	   ((eq? dirn 'closer)
	    (if (< (+ (cAr p) 1) (tree-arity t0))
	    (let* ((no (graphics-znext p))
		   (p+1 (if no (rcons (cDr p) no) #f))
		   (t+1 (if p+1 (path->tree p+1) #f))
	       )
	       (if no
	       (begin
		  (path-remove p 1)
		  (path-insert p+1 `(tuple ,(tree->stree t)))
		  (set! current-path-under-mouse p+1)))
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

(tm-define (graphics-enter-mode old-mode new-mode)
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

(tm-define (graphics-finish)
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
