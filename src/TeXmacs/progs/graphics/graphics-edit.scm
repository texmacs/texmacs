
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
	(kernel texmacs tm-states)
        (graphics graphics-utils) (graphics graphics-main)
        (graphics graphics-object) (graphics graphics-env)
        (graphics graphics-kbd) (graphics graphics-group)))

;; TODO:
;;   -> systematiser l'utilisation de l'API des arbres (tree-assign, etc.)
;;   -> rajouter systematiquement des "synopsis" pour les tm-define.
;;
;;   -> chercher scrupuleusement a factoriser et simplifier le code.
;;   -> des macros comme foreach et foreach-number, etc., devraient
;;      etre dans kernel/boot/abbrevs ou kernel/library (et chercher
;;      des noms plus elegants).
;;
;;   -> On en discutera davantage apres un premier passage en revue.
;;
;;   -> Remove all the (stree-at), (tree->stree), etc.
;;   -> Replace the remaining (tree->stree) by (tm->stree) or (t2o)
;;   -> Except in simple cases, remove all the (tree->stree) which
;;      slow the code, and operate everywhere and all the time on trees.
;;
;;   -> Remove the synchro-unsafe (get-env) & (get-env-tree) everywhere.
;;
;;   -> Use systematically (first), (second), etc., instead
;;      of (car), (cadr), etc.

;; NOTE: Dans la doc, preciser **exactement** les conditions d'evaluation
;;   des differentes fonctions (par exemple (stree->tree #f) == #f,
;;   mais (stree->tree 1) == <tree 1>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic operations
(define moveclick-tolerance "5px")
(define previous-leftclick #f)
(define (points-dist< p1 p2 eps)
  (set! p1 (frame-direct `(tuple ,(cadr p1) ,(caddr p1))))
  (set! p2 (frame-direct `(tuple ,(cadr p2) ,(caddr p2))))
  (set! eps (length-decode eps))
  (let* ((x1 (s2f (cadr p1)))
	 (y1 (s2f (caddr p1)))
	 (x2 (s2f (cadr p2)))
	 (y2 (s2f (caddr p2)))
     )
     (< (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))) (* eps eps))))

;; Basic operations (create)
(tm-define (create x y mode)
  (:require (eq? mode 'point))
  (graphics-group-enrich-insert `(point ,x ,y)))

(tm-define (create x y mode)
  (:require (in? mode gr-tags-curves))
  (with o `(,mode (point ,x ,y) (point ,x ,y))
    (graphics-store-state 'start-create)
    (set! graphics-undo-enabled #f)
    (create-graphical-object o 'new 'object-and-points #f)
    (set! current-point-no 1)
    (set! sticky-point #t)
    (graphics-store-state #f)))

(tm-define (create x y mode)
  (:require (eq? mode 'text-at))
  (graphics-group-enrich-insert
    `(text-at "" (point ,x ,y))))

;; Basic operations (add point)
(define (object_set-point xcur ycur obj no)
 ;(display* "obj=" obj "\n")
  (if (not (and (in? (car obj) '(arc carc)) (> (length obj) 3)))
      (with l (list-tail (cdr obj) no)
	(set-car! l `(point ,xcur ,ycur)))))

(define (object_add-point xcur ycur x y obj no dirn)
  ;;(display* "obj=" obj "\n")
  (if (not (and (in? (car obj) '(arc carc)) (> (length obj) 3)))
      (with l (list-tail (cdr obj) no)
  	(graphics-store-state #f)
 	(if dirn
 	    (begin
 	      (set-cdr! l (cons `(point ,x ,y) (cdr l)))
 	      (if (and xcur ycur)
 		  (set-car! l `(point ,xcur ,ycur)))
 	      (create-graphical-object obj 'active 'object-and-points (+ no 1))
 	      (set! current-point-no (+ no 1)))
  	    (begin
  	      (set-cdr! l (cons (car l) (cdr l)))
  	      (set-car! l `(point ,x ,y))
 	      (if (and xcur ycur)
 		  (set-car! (cdr l) `(point ,xcur ,ycur)))
  	      (create-graphical-object obj 'active 'object-and-points no)
 	      (set! current-point-no no)))
  	(set! current-edge-sel? #t)
  	(set! sticky-point #t))))

;; Basic operations (commit)
(define (object_commit x y obj)
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
     (set! leftclick-waiting #f)
     (set! current-edge-sel? #f)
     (set! graphics-undo-enabled #t)
     (graphics-forget-states))))

;; Left button
(define (any_left-button x y p obj no edge)
  (if sticky-point
      ;;Last
      (begin
	(if (and leftclick-waiting
		 (points-dist<
		   previous-leftclick `(point ,x ,y) moveclick-tolerance))
	    (begin
	       (object_set-point
		  (cadr previous-leftclick) (caddr previous-leftclick) obj no)
	       (object_commit x y obj))
	    (begin
              (if (and (not leftclick-waiting)
                       previous-leftclick
                       (points-dist<
			previous-leftclick `(point ,x ,y) moveclick-tolerance))
                  (undo)
		  (begin
		    (set-message "Left click: finish" "")
		    (set! leftclick-waiting #t)))))
	;(display* "prev-leftc=" previous-leftclick "\n")
	;(display* "x=" x "\n")
	;(display* "y=" y "\n\n")
        (set! previous-leftclick `(point ,x ,y)))
      ;;Start move
      (begin
	(set-message "Left click: add point; Middle click: undo" "")
	(graphics-store-state 'start-move)
	(create-graphical-object obj p 'object-and-points #f)
	(graphics-remove p 'memoize-layer)
	(graphics-group-start)
	(set! sticky-point #t)
	(set! leftclick-waiting #f)
	(set! current-point-no no)
	(set! current-edge-sel? #t)
	(set! graphics-undo-enabled #f)
        (set! previous-leftclick  `(point ,x ,y))
	(if (and edge (not (and (in? (car obj) '(arc carc))
				(> (length obj) 3))))
	  (object_add-point #f #f x y
	    (cadr (graphical-object #t)) no #t))
	(graphics-store-state #f))))

(tm-define (left-button x y p obj no edge)
  (:require (in? (car obj) gr-tags-point-curves))
  (any_left-button x y p obj no edge))

(tm-define (left-button x y p obj no edge)
  (:require (eq? (car obj) 'text-at))
  (if sticky-point
      (object_commit x y obj)
      (if (on-graphical-embedding-box? x y obj "1mm")
	  (begin
	     (set-texmacs-pointer 'graphics-cross)
	     (any_left-button x y p obj 1 edge))
	  (begin
	     (set-texmacs-pointer 'text-arrow)
	     (go-to (car (select-first (s2f x) (s2f y))))))))

(tm-define (left-button x y p obj no edge)
  (:require (not (in? (car obj) gr-tags-all)))
  (any_left-button x y p obj no edge))

;; Move
(define (any_move x y p obj no edge)
 ;(display* "obj[move<" sticky-point ">]=")(write obj)(display "\n")
  (if sticky-point
      (if (and leftclick-waiting
	       (not (points-dist<
		      previous-leftclick `(point ,x ,y) moveclick-tolerance)))
          (begin
	     ;(display* "prev-leftc(2)=" previous-leftclick "\n")
	     ;(display* "x(2)=" x "\n")
	     ;(display* "y(2)=" y "\n\n")
	     (set! leftclick-waiting #f)
	     (object_add-point
	       (cadr previous-leftclick) (caddr previous-leftclick)
	       x y obj no (== (logand (get-keyboard-modifiers) ShiftMask) 0)))
	  (begin
	     (if leftclick-waiting
		 (set-message "Left click: finish" "")
		 (set-message "Left click: add point; Middle click: undo" ""))
	     (if (== (car obj) 'point)
	         (set! obj `(point ,x ,y))
	         (set-car! (list-tail (cdr obj) no) `(point ,x ,y)))
	     (create-graphical-object obj 'active 'object-and-points
	       (if edge no `(,edge ,no)))))
      (begin
	(set-message "Left click: add or edit object; Middle click: remove object" "")
	(create-graphical-object obj p 'points (if edge no `(,edge ,no)))
	(if p
	    (with p2 (tm-upwards-path p '(text-at) '(graphics))
	       (if (not p2) (go-to (rcons p 0))))))))

(tm-define (move x y p obj no edge)
  (:require (in? (car obj) gr-tags-point-curves))
  (any_move x y p obj no edge))

(tm-define (move x y p obj no edge)
  (:require (eq? (car obj) 'text-at))
  (if (and (not sticky-point)
	   (on-graphical-embedding-box? x y obj "1mm"))
      (set-texmacs-pointer 'graphics-cross-arrows)
      (set-texmacs-pointer 'graphics-cross)
  )
  (any_move x y p obj 1 edge))

(tm-define (move x y p obj no edge)
  (:require (eq? (car obj) 'gr-group))
  (if sticky-point
      (display* "Sticky move(gr-group) !yet implemented\n")
      (begin
	(create-graphical-object obj p 'points #f))))

(tm-define (move x y p obj no edge)
  (:require (not (in? (car obj) gr-tags-all)))
  (any_move x y p obj no edge))

;; Middle button
(tm-define (any_middle-button x y p obj no)
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
	      (graphics-decorations-reset)
	      (graphics-group-start))
	    (with l (if (<= no 0) obj (list-tail (cdr obj) (- no 1)))
	      (set-cdr! l (cddr l))
	      (create-graphical-object obj p 'points #f)
	      (graphics-active-assign obj)))
	(set! sticky-point #f))))

(tm-define (middle-button x y p obj no)
  (any_middle-button x y p obj no))

;; Dispatch
(define (edit-insert x y)
  (with mode (cadr (graphics-mode))
    (if just-started-dragging
	(set! disable-drag #t))
    (set! layer-of-last-removed-object #f)
    (create x y mode)))

(tm-define (edit_left-button mode x y)
  (:require (eq? mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
 ;(display "obj[left-button]=")(write current-obj)(display "\n")
  (if sticky-point
      (begin
	(if just-started-dragging
	    (set! disable-drag #t))
	(left-button
	  x y current-path current-obj current-point-no current-edge-sel?))
      (if current-obj
	  (begin
	    (edit_tab-key 'edit
			  (if (graphics-states-void?)
			      #f
			      (state-ref (graphics-pop-state) 'choosing)))
	    (graphics-store-state #f)
	    (set! choosing #t))
	  (edit-insert x y))))

(tm-define (edit_move mode x y)
  (:require (eq? mode 'edit))
  (:state graphics-state)
 ;(display "obj[move]=")(write current-obj)(display "\n")
  (set-texmacs-pointer 'graphics-cross #t)
  (if choosing
   ;; Start moving point/object or inserting a new point
      (with first #f
	 (graphics-state-set (graphics-pop-state))
      ;; Restores the state when choosing has been set
	 (set! first (not choosing))
	 (set! choosing #f)
	 (graphics-forget-states)
	 (if (and first
		  (not just-started-dragging)
		  (not (eq? (car current-obj) 'text-at)))
	     (edit-insert current-x current-y)
	     (left-button current-x current-y
		current-path current-obj current-point-no current-edge-sel?)))
   ;; Moving
      (begin
       (if current-obj
	   (move
	     x y current-path current-obj current-point-no current-edge-sel?)
	   (create-graphical-object '(nothing) #f 'points #f)))))

(tm-define (edit_middle-button mode x y)
  (:require (eq? mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (if current-obj
      (middle-button x y current-path current-obj current-point-no)))

(tm-define (edit_right-button mode x y)
  (:require (eq? mode 'edit))
  (display* "Right button(edit) currently unused\n"))

(tm-define (edit_tab-key mode next)
  (:require (eq? mode 'edit))
  (:state graphics-state)
 ;(display* "Graphics] Edit(Tab)\n")
  (if (and current-x current-y)
      (begin
	(if next
	    (select-next))
	(invalidate-graphical-object)
	(if current-obj
	    (create-graphical-object
	       current-obj current-path 'points
	       (with tag (car current-obj)
		  (if (== tag 'text-at)
		      (set! current-point-no 1))
		  (cond ((== tag 'gr-group)
			 #f)
			(else
		         (if current-edge-sel?
			     current-point-no
			    `(,current-edge-sel? ,current-point-no))))))))
      (invalidate-graphical-object)))

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
(tm-define (graphics-assign-props p obj mode)
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

(tm-define (graphics-copy-props p)
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

(tm-define (current-is-textat?)
  (and current-path
       (== (tree-label (path->tree current-path)) 'text-at)))

(tm-define (text-at-change-halign p dirn)
  (:state graphics-state)
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
     (set! current-path
	(graphics-group-enrich-insert-bis
	   obj #f #f #f mag #f #f #f #f halign2 valign #f))
     (create-graphical-object obj '() 'points 'no-group)
     (graphics-group-start)))

(tm-define (text-at-change-valign p dirn)
  (:state graphics-state)
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
     (set! current-path
	(graphics-group-enrich-insert-bis
	   obj #f #f #f mag #f #f #f #f halign valign2 #f))
     (create-graphical-object obj '() 'points 'no-group)
     (graphics-group-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events when the cursor is inside a text-at
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (when-inside-text-at func x y)
  (:state graphics-state)
  (let* ((res #t)
	 (uncaptured (lambda ()
		       (if (!= func 'move)
			   (begin
			     (graphics-group-start)
			     (graphics-move-point x y))))))
    ;;(display* "Inside text-at=" func "; x=" x "; y=" y "\n")
    (if (and (not sticky-point)
	     (tm-upwards-path (cDr (cursor-path)) '(text-at) '(graphics)))
	(if (and (pair? current-obj) (eq? (car current-obj) 'text-at))
	    (cond
	      ((== func 'left-button)
	       (left-button current-x current-y
		 current-path current-obj current-point-no current-edge-sel?)))
	    (uncaptured))
	(set! res #f))
    ;;(display* "res=" res "\n")
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with superpositions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-zprevious p)
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

(tm-define (graphics-znext p)
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

(tm-define (graphics-zmove dirn)
 ;(display* "dirn(graphics-zmove)=" dirn "\n")
 ;(display* "current-path" current-path "\n")
  (if current-path
  (let* ((p (graphics-object-root-path current-path))
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
	       (tree-remove t0 (cAr p) 1)
	       (tree-insert (tree-up t-1) (cAr p-1) `(,(tree->stree t)))
	       (set! current-path p-1)
	    ))
	   )
	   ((eq? dirn 'foreground)
	    (if (< (+ (cAr p) 1) (tree-arity t0))
	    (let* ((p+1 (rcons (cDr p) (- (tree-arity t0) 1)))
		   (t+1 (path->tree p+1))
	       )
	       (tree-remove t0 (cAr p) 1)
	       (tree-insert (tree-up t+1) (cAr p+1) `(,(tree->stree t)))
	       (set! current-path p+1)
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
		  (tree-remove t0 (cAr p) 1)
		  (tree-insert (tree-up t-1) (cAr p-1) `(,(tree->stree t)))
		  (set! current-path p-1)))
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
		  (tree-remove t0 (cAr p) 1)
		  (tree-insert (tree-up t+1) (cAr p+1) `(,(tree->stree t)))
		  (set! current-path p+1)))
	    ))
	   )
	   (else #t)
     )
     (sketch-reset)
     (graphics-group-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (if (not (when-inside-text-at 'left-button x y))
      (edit_left-button (car (graphics-mode)) x y)))

(tm-define (graphics-move-point x y)
 ;(display* "Graphics] Move " x ", " y "\n")
  (if (not (when-inside-text-at 'move x y))
      (edit_move (car (graphics-mode)) x y)))

(tm-define (graphics-remove-point x y)
  ;(display* "Graphics] Remove " x ", " y "\n")
  (if (not (when-inside-text-at 'middle-button x y))
      (edit_middle-button (car (graphics-mode)) x y)))

(tm-define (graphics-last-point x y)
  ;(display* "Graphics] Last " x ", " y "\n")
  (if (not (when-inside-text-at 'right-button x y))
      (edit_right-button (car (graphics-mode)) x y)))

(define just-started-dragging #f)
(define disable-drag #f)
;; FIXME : put these 2 variables inside the state.

(tm-define (graphics-start-drag x y)
  ;(display* "Graphics] Start-drag " x ", " y "\n")
  (if (when-inside-text-at 'start-drag x y)
      (set! disable-drag #t)
      (begin
        (set! just-started-dragging #t)
        (graphics-insert-point x y))))

(tm-define (graphics-dragging x y)
  ;(display* "Graphics] dragging " x ", " y "\n")
  (graphics-move-point x y))

(tm-define (graphics-end-drag x y)
  ;(display* "Graphics] End-drag " x ", " y "\n")
  (if (not (when-inside-text-at 'end-drag x y))
  (begin
    (set! just-started-dragging #f)
    (if disable-drag
        (set! disable-drag #f)
        (begin
	  (graphics-insert-point x y)
	  (if (eq? (car (graphics-mode)) 'edit)
	      (graphics-insert-point x y)))))))

(tm-define (graphics-start-right-drag x y)
  ;(display* "Graphics] Start-right-drag " x ", " y "\n")
  (graphics-last-point x y))

(tm-define (graphics-right-dragging x y)
  ;(display* "Graphics] right-dragging " x ", " y "\n")
  (graphics-move-point x y))

(tm-define (graphics-end-right-drag x y)
  (:state graphics-state)
  ;(display* "Graphics] End-right-drag " x ", " y "\n")
  (if (not sticky-point)
; FIXME : test due to timing problems in detecting the drag
      (graphics-last-point x y)))

(tm-define (graphics-choose-point)
  ;(display* "Graphics] Choose\n")
  (edit_tab-key (car (graphics-mode)) #t))

(tm-define (graphics-enter-mode old-mode new-mode)
  (:state graphics-state)
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
	 ((== (car mode) 'group-edit) (noop))
	 (else (display* "Uncaptured finish\n")))))
