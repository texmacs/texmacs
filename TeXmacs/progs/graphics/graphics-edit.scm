
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-edit)
  (:use (utils library cursor) (utils library tree)
	(kernel texmacs tm-states)
        (graphics graphics-utils) (graphics graphics-main)
        (graphics graphics-object) (graphics graphics-env)
        (graphics graphics-kbd) (graphics graphics-group)))

;; TODO:
;;
;;   1. Chercher scrupuleusement a factoriser et simplifier le code.
;;
;;   -> Enlever *tous* les parametres dans les fonctions de l'editeur
;;      graphique, et utiliser uniquement les variables d'etat ;
;;
;;   -> Use systematically (first), (second), etc., instead
;;      of (car), (cadr), etc.
;;
;;   2. travailler exclusivement avec des trees, et sauf dans les
;;      rares cas ou peut-etre c'est necessaire, supprimer toute
;;      utilisation des paths ;
;;
;;   -> Remove all the (stree-at), (tree->stree), etc.
;;
;;   -> Replace the remaining (tree->stree) by (tm->stree) or (t2o)
;;
;;   -> Except in simple cases, remove all the (tree->stree) which
;;      slow the code, and operate everywhere and all the time on trees.
;;
;;   -> Remove the synchro-unsafe (get-env) & (get-env-tree) everywhere.
;;
;;   3. Doc, reorganisation code
;;
;;   ->  rajouter systematiquement des "synopsis" pour les tm-define.
;;
;;   -> des macros comme foreach et foreach-number, etc., devraient
;;      etre dans kernel/boot/abbrevs ou kernel/library (et chercher
;;      des noms plus elegants).
;;
;;   -> dans la doc, preciser **exactement** les conditions d'evaluation
;;      des differentes fonctions (par exemple (stree->tree #f) == #f,
;;      mais (stree->tree 1) == <tree 1>.
;;
;;   -> On en discutera davantage apres un premier passage en revue.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic operations
;;
;; NOTE: Imperative functions, which inconditionnaly perform a given
;;   operation on the sketch.
;;
;;   These functions depend on, and can change the current edit state (i.e.,
;;   being in modifying or in selecting mode), and they maintain the value
;;   of the state variables current-point-no and current-edge-sel?, which
;;   are basic pointers inside the edited object.
;;
;;   In other words, these functions are methods which operate on the sketch,
;;   according to what you need to do when editing in point mode.

;; Basic operations (setting the object)
(define (object-set! o . opt)
 ;(display* "o=" o "\n")
  (set! layer-of-last-removed-object #f)
  (set! current-obj o)    ;; FIXME: Hmmm, I still have a doubt for this one.
                          ;;   Should completely clarify its role & centralize
                          ;;   where it's managed (and for similar gvs as well).
  (if sticky-point
      (sketch-set! `(,o))
      (if (in? 'checkout opt)
	  (begin
	     (sketch-set! `(,o))
	     (sketch-checkout))
	  (if (in? 'new opt)
	      (graphics-group-enrich-insert o)
	      (graphics-assign current-path o)))))

;; Basic operations (create)
(define (sketch-get1)
  (if (not (and (pair? (sketch-get)) (eq? 1 (length (sketch-get)))))
      (graphics-error "(sketch-get1)"))
  (sketch-get))
  
(tm-define (object_create tag x y)
  (:require (eq? tag 'point))
  (object-set! `(point ,x ,y) 'new))

(tm-define (object_create tag x y)
  (:require (in? tag gr-tags-curves))
  (with o (graphics-enrich `(,tag (point ,x ,y) (point ,x ,y)))
    (graphics-store-state 'start-create)
    (set! current-point-no 1)
    (object-set! o 'checkout)
    (graphics-store-state #f)))

(tm-define (object_create tag x y)
  (:require (eq? tag 'text-at))
  (object-set! `(text-at "" (point ,x ,y)) 'new))

(define (set-point-sub obj no x y)
  ;;(display* "set-point-sub " obj ", " no ", " x ", " y "\n")
  (cond ((== (car obj) 'with)
         (set-point-sub (cAr obj) no x y))
        ((== (car obj) 'point)
	 (set-car! (cdr obj) x)
	 (set-car! (cddr obj) y))
        ((and (not (not no)) (list? obj) (> (length obj) (+ no 1)))
         (set-point-sub (list-ref obj (+ no 1)) #f x y))
        (else #f)))

;; Basic operations (set & add point)
(define (object_set-point no xcur ycur)
  (define obj (stree-radical (car (sketch-get1))))
  ;;(display* "obj[" no ";" xcur "," ycur "]=" obj "\n")
  (set-point-sub obj no xcur ycur)
  (object-set! (car (sketch-get))))

(define (object_add-point no xcur ycur x y dirn)
  (define obj (stree-radical (car (sketch-get1))))
 ;(display* "obj=" obj "\n")
  (if (not (and (in? (car obj) '(arc carc)) (> (length obj) 3)))
      (with l (list-tail (cdr obj) no)
  	(graphics-store-state #f)
 	(if dirn
 	    (begin
 	      (set-cdr! l (cons `(point ,x ,y) (cdr l)))
 	      (if (and xcur ycur)
 		  (set-car! l `(point ,xcur ,ycur)))
 	      (set! current-point-no (+ no 1)))
  	    (begin
  	      (set-cdr! l (cons (car l) (cdr l)))
  	      (set-car! l `(point ,x ,y))
 	      (if (and xcur ycur)
 		  (set-car! (cdr l) `(point ,xcur ,ycur)))
 	      (set! current-point-no no))
        )
	(object-set! (car (sketch-get)))
  	(set! current-edge-sel? #t))))

;; Basic operations (remove)
(define (object_remove-point no)
;; FIXME: should read the radical & memoize it
  (with l (if (<= no 0)
	      current-obj
	      (list-tail (cdr current-obj) (- no 1)))
    (set-cdr! l (cddr l))
    (set! current-point-no #f)
    (object-set! current-obj)))
  ;; FIXME: Should assign the memoized radical, here

(define (object_remove)
  (graphics-remove current-path))

;; Basic operations (checkout & commit)
(define (object_checkout)
  (sketch-set! `(,(path->tree current-path)))
  (sketch-checkout)
  ;;(display* "Checked out " (sketch-get) "\n")
  (sketch-set! (map tree->stree (sketch-get))))

(define (object_commit)
  (define obj (stree-radical (car (sketch-get1))))
  (if (not (and (in? (car obj) '(arc carc)) (<= (length obj) 3)))
      (begin
        (graphical-fetch-props (car (sketch-get)))
        (set! obj (graphics-enrich-bis
                   obj
                   graphical-id
                   graphical-opacity
                   graphical-color
                   graphical-pstyle
                   graphical-lwidth
                   (local-magnification graphical-magnification)
                   graphical-lstyle
                   graphical-lstyle-unit
                   graphical-larrows
                   graphical-fcolor
                   graphical-textat-halign
                   graphical-textat-valign))
        (set! current-edge-sel? #f)
        (sketch-set! `(,obj))
        ;;(display* "Commited " (sketch-get) "\n")
        (sketch-commit)
        (set! leftclick-waiting #f)
        (set! current-obj (stree-radical obj))
        (set! current-point-no #f)
        (graphics-forget-states)))
  (delayed
    (graphics-update-constraints)))

(define (current-in? l)
  (and (pair? current-obj) (in? (car current-obj) l)))

;; Edition operations
;;
;; NOTE: Intelligent functions, which take into account the state,
;;   the previous mouse clicks, etc. They also perform printing the
;;   help messages. These functions maintain the value of the state
;;   variables, and they manage state stacking.
;;
;;   In other words, these functions implement the different states
;;   of the editing automaton.

(define moveclick-tolerance "5px")
(define previous-leftclick #f)

(define (move-over)
 ;(display* "obj[move<" sticky-point ">]=")(write current-obj)(display "\n")
  (set-message "Left click: add or edit object; Middle click: remove object" "")
  (graphics-decorations-update)
  (if current-path
      (with p2 (tm-upwards-path current-path '(text-at) '(graphics))
         (if (not p2) (go-to (rcons current-path 0))))))

(define (edit-insert x y)
  (object_create (cadr (graphics-mode)) x y))

(define (start-move)
  (define edge current-edge-sel?)
  (set-message "Left click: add point; Middle click: undo" "")
  (graphics-store-state 'start-move)
  (object_checkout)
  (graphics-group-start)
  (set! current-edge-sel? #t)
  (set! leftclick-waiting #f)
  (if (and edge
	   (not (and (current-in? '(arc carc))
                     (> (length current-obj) 3))))
      (begin
	 (object_add-point current-point-no #f #f current-x current-y #t)
	 (graphics-decorations-update)))
  (graphics-store-state #f))

(define (move-point)
 ;(display* "obj[move<" sticky-point ">]=")(write current-obj)(display "\n")
  (if (and leftclick-waiting
	   (not (points-dist<
		  previous-leftclick
		 `(point ,current-x ,current-y)
		  moveclick-tolerance)))
      (begin
	 ;(display* "prev-leftc(2)=" previous-leftclick "\n")
	 ;(display* "x(2)=" current-x "\n")
	 ;(display* "y(2)=" current-y "\n\n")
	 (set! leftclick-waiting #f)
	 (object_add-point
	   current-point-no
	   (cadr previous-leftclick) (caddr previous-leftclick)
	   current-x current-y
	   (== (logand (get-keyboard-modifiers) ShiftMask) 0)))
      (begin
	 (if leftclick-waiting
	     (set-message "Left click: finish" "")
	     (set-message "Left click: add point; Middle click: undo" ""))
	 (object_set-point current-point-no current-x current-y))
  )
  (graphics-decorations-update))

(define (last-point)
  (if (and leftclick-waiting
	   (points-dist<
	     previous-leftclick
	    `(point ,current-x ,current-y)
	     moveclick-tolerance))
      (begin
        (object_set-point
         current-point-no
         (cadr previous-leftclick)
         (caddr previous-leftclick))
        (object_commit))
      (begin
        (if (and (not leftclick-waiting)
                 previous-leftclick
                 (points-dist<
		  previous-leftclick
                 `(point ,current-x ,current-y)
		  moveclick-tolerance))
	    (begin
              (undo 0)
              (set! choosing #f)
              (set! leftclick-waiting #f)
              (set! just-started-dragging #f))
	    (begin
	      (set-message "Left click: finish" "")
	      (set! leftclick-waiting #t)))))
 ;(display* "prev-leftc=" previous-leftclick "\n")
 ;(display* "x=" current-x "\n")
 ;(display* "y=" current-y "\n\n")
  )

(define (back)
 ;(display* "obj[" p "]=" obj "\n")
  (graphics-back-state #f)
  (graphics-move current-x current-y))

(define (remove-point)
 ;(display* "obj[" p "]=" obj "\n")
  (if (or (current-in? gr-tags-oneshot) (null? (cdddr current-obj))
	  (not (current-in? gr-tags-all))
	  (!= (logand (get-keyboard-modifiers) ShiftMask) 0))
      (begin
        (object_remove)
        (graphics-decorations-reset)
        (graphics-group-start))
      (begin
        (object_remove-point current-point-no)
        (graphics-decorations-update))))

;; Left button
(tm-define (left-button)
  (:require (current-in? gr-tags-point-curves))
  (if sticky-point
      (last-point)
      (start-move)))

(tm-define (left-button)
  (:require (current-in? '(text-at)))
  ;;(display* "Text at left button\n")
  (if sticky-point
      (object_commit)
      (if (== (graphics-mode) '(edit text-at))
	  (begin
            (set-texmacs-pointer 'text-arrow)
            (go-to (car (select-first (s2f current-x) (s2f current-y)))))
	  (begin
	     (set-texmacs-pointer 'graphics-cross)
	     (set! current-point-no 1)
	     (start-move)))))

(tm-define (left-button)
  (:require (not (current-in? gr-tags-all)))
  (if sticky-point
      (last-point)
      (start-move)))

;; Move
(tm-define (move)
  (:require (current-in? gr-tags-point-curves))
  (if sticky-point
      (move-point)
      (move-over)))

(tm-define (move)
  (:require (current-in? '(text-at)))
  (set! current-point-no 1)
  (if sticky-point
      (move-point)
      (move-over)))

(tm-define (move)
  (:require (current-in? '(gr-group)))
  (if sticky-point
      (display* "Sticky move(gr-group) !yet implemented\n")
      (begin
	(set! current-point-no #f)
	(graphics-decorations-update))))

(tm-define (move)
  (:require (not (current-in? gr-tags-all)))
  (if sticky-point
      (move-point)
      (move-over)))

;; Middle button
(tm-define (middle-button)
  (if sticky-point
      (back)
      (remove-point)))

;; Dispatch
(tm-define (edit_left-button mode x y)
  (:require (eq? mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  ;;(display "obj[left-button]=")(write current-obj)(display "\n")
  (if (or sticky-point (current-in? '(text-at)))
      (begin
	(left-button))
      (if current-obj
	  (begin
	    (if (not just-started-dragging)
                (begin
                  (edit_tab-key 'edit
                                (if (graphics-states-void?)
                                    #f
                                    choosing)
                                1)))
	    (if (or (not just-started-dragging) (not choosing))
                (begin
                  (if (not (graphics-states-void?))
                      (graphics-pop-state))
                  (graphics-store-state #f)))
	    (set! choosing #t))
	  (edit-insert x y)))
  (set! previous-leftclick `(point ,current-x ,current-y)))

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
		  (not (current-in? '(xtext-at))))
	     (edit-insert current-x current-y)
	     (left-button)))
   ;; Moving
      (begin
       (if current-obj
	   (move)
	   (graphics-decorations-reset)))))

(tm-define (edit_middle-button mode x y)
  (:require (eq? mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
 ;(display* "Graphics] Edit(Middle-button)[" current-path "]=" current-obj "\n")
  (if current-obj
      (begin
	 (if choosing
	     (graphics-state-set (graphics-pop-state)))
	 (middle-button))))

(tm-define (edit_right-button mode x y)
  (:require (eq? mode 'edit))
  (display* "Right button(edit) currently unused\n"))

(tm-define (edit_tab-key mode next inc)
  (:require (eq? mode 'edit))
  (:state graphics-state)
 ;(display* "\nGraphics] Edit(Tab)[" next "," current-path "]=" current-obj "\n")
  (if (and current-x current-y)
      (begin
	(if current-obj
	    (graphics-decorations-update))
	(if next
	    (select-next inc))
      )
      (invalidate-graphical-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit properties (implemented as a group mode, see below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions with check marks for all the properties ("Enable change" menu)
(define (enabled-var? var)
  (not (graphics-frozen-property? var)))

(tm-define (graphics-opacity-enabled?)
  (enabled-var? "gr-opacity"))

(tm-define (graphics-color-enabled?)
  (enabled-var? "gr-color"))

(tm-define (graphics-toggle-opacity-enabled)
  (:check-mark "v" graphics-opacity-enabled?)
  (graphics-frozen-property! "gr-opacity"
			     (graphics-opacity-enabled?)))

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
(tm-define (graphics-assign-props p obj)
  (let* ((op (graphics-path-property p "opacity"))
	 (color (graphics-path-property p "color"))
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
	   (graphics-group-enrich-insert-bis (stree-radical obj)
	      (if (graphics-opacity-enabled?)
		  (graphics-get-property "gr-opacity") op)
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
	res)))

(tm-define (graphics-copy-props p)
  (let* ((op (graphics-path-property p "opacity"))
	 (color (graphics-path-property p "color"))
	 (ps (graphics-path-property p "point-style"))
	 (lw (graphics-path-property p "line-width"))
	 (st (graphics-path-property p "dash-style"))
	 (stu (graphics-path-property p "dash-style-unit"))
	 (lp (graphics-path-property p "line-arrows"))
	 (fc (graphics-path-property p "fill-color"))
	 (ha (graphics-path-property p "text-at-halign"))
	 (va (graphics-path-property p "text-at-valign"))
     )
     (if (!= op "default")
	 (graphics-change-property "gr-opacity" op)
	 (graphics-remove-property "gr-opacity"))
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
     (sketch-reset)
     (graphics-decorations-update)
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
     (sketch-reset)
     (graphics-decorations-update)
     (graphics-group-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with superpositions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-graphical? t)
  (and (tree? t)
       (or (tree-in? t gr-tags-all)
           (and (tree-is? t 'with)
                (graphics-graphical? (tree-ref t :last))))))

(tm-define (graphics-graphical-index t)
  (or (and (tree? t) (tree-compound? t)
           (list-find-index (tree-children t) graphics-graphical?)) 0))

(tm-define (graphics-zprevious p)
  (let* ((p0 (cDr p))
	 (ofs (cAr p))
         (i0 (graphics-graphical-index (path->tree p0)))
	 (res #f))
     (foreach-number (i (- ofs 1) >= i0)
	(with t (path->tree (rcons p0 i))
	   (if (and (not res) (box-intersects t (path->tree p)))
	       (set! res i))))
     res))

(tm-define (graphics-znext p)
  (let* ((p0 (cDr p))
	 (t0 (path->tree p0))
	 (ofs (cAr p))
	 (res #f))
     (foreach-number (i (+ 1 ofs) < (tree-arity t0))
	(with t (path->tree (rcons p0 i))
	   (if (and (not res) (box-intersects t (path->tree p)))
	       (set! res i))))
     res))

(tm-define (graphics-zmove dirn)
 ;(display* "dirn(graphics-zmove)=" dirn "\n")
 ;(display* "current-path" current-path "\n")
  (if current-path
  (let* ((p (graphics-object-root-path current-path))
	 (t (if p (path->tree p) #f))
	 (t0 (if t (path->tree (cDr p)) #f))
         (i0 (graphics-graphical-index t0)))
    ;(display* "t0=" (get-tag? t0) "\n")
    ;(display* "t=" t "\n")
     (cond ((eq? dirn 'background)
	    (if (> (cAr p) i0)
	    (let* ((p-1 (rcons (cDr p) i0))
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
	    (if (> (cAr p) i0)
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
;; Updating the constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-update-constraints t)
  (cond ((not (tree? t)) (noop))
        ((tree-atomic? t) (noop))
        ((and (match? t '(with "gid" :%1 (point :%2)))
              (graphics-has? (tree-ref t 1)))
         (let* ((old (tree-ref t :last))
                (new (graphics-ref (tree-ref t 1))))
           (when (!= new old) (tree-set t :last new))
           (graphics-notify-update (tree-ref t 1))))         
        (else (for-each tree-update-constraints (tree-children t)))))

(tm-define (graphics-update-constraints)
  (when (graphics-needs-update?)
    (with-innermost t 'graphics
      (remove-undo-mark)
      (tree-update-constraints t)
      (add-undo-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-keyboard-modifiers 0)

(tm-define (set-keyboard-modifiers mods)
  (set! the-keyboard-modifiers mods))

(tm-define (get-keyboard-modifiers)
  the-keyboard-modifiers)

(tm-define (graphics-move x y)
  ;;(display* "Graphics] Move " x ", " y "\n")
  (when (not (inside? 'text-at))
    (edit_move (car (graphics-mode)) x y)))

(tm-define (graphics-release-left x y)
  ;;(display* "Graphics] Insert " x ", " y "\n")
  (if (inside? 'text-at)
      (with-innermost t 'text-at
        (let* ((ps (select-first (s2f x) (s2f y)))
               (p (and ps (car ps))))
          (if (and p (list-starts? p (tree->path t)))
              (go-to p)
              (tree-go-to t :start))))
      (edit_left-button (car (graphics-mode)) x y)))

(tm-define (graphics-release-middle x y)
  ;;(display* "Graphics] Remove " x ", " y "\n")
  (when (not (inside? 'text-at))
    (edit_middle-button (car (graphics-mode)) x y)))

(tm-define (graphics-release-right x y)
  ;;(display* "Graphics] Last " x ", " y "\n")
  (when (not (inside? 'text-at))
    (edit_right-button (car (graphics-mode)) x y)))

(define just-started-dragging #f)
;; FIXME : put these 2 variables inside the state.

(tm-define (graphics-start-drag x y)
  ;;(display* "Graphics] Start-drag " x ", " y "\n")
  ;;(display* "  just-started " just-started-dragging "\n")
  ;;(display* "  sticky-point " sticky-point "\n")
  ;;(display* "  choosing " choosing "\n")
  ;;(display* "  leftclick-waiting " leftclick-waiting "\n")
  ;;(display* "  current-graphical-object " current-graphical-object "\n")
  ;;(display* "  graphics-action " graphics-action "\n")
  ;;(display* "  current-point-no " current-point-no "\n")
  (when (not (inside? 'text-at))
    (set! just-started-dragging #t)
    (graphics-release-left x y)))

(tm-define (graphics-dragging x y)
  ;;(display* "Graphics] dragging " x ", " y "\n")
  (when (not (inside? 'text-at))
    (graphics-move x y)))

(tm-define (graphics-end-drag x y)
  ;;(display* "Graphics] End-drag " x ", " y "\n")
  (when (not (inside? 'text-at))
    (set! just-started-dragging #f)
    (graphics-release-left x y)
    (if (== (car (graphics-mode)) 'edit)
        (if (not (current-in? '(text-at)))
            (graphics-release-left x y)))))

(tm-define (graphics-start-right-drag x y)
  ;(display* "Graphics] Start-right-drag " x ", " y "\n")
  (when (not (inside? 'text-at))
    (graphics-release-right x y)))

(tm-define (graphics-right-dragging x y)
  ;(display* "Graphics] right-dragging " x ", " y "\n")
  (when (not (inside? 'text-at))
    (graphics-move x y)))

(tm-define (graphics-end-right-drag x y)
  (:state graphics-state)
  ;(display* "Graphics] End-right-drag " x ", " y "\n")
  (when (not (inside? 'text-at))
    (if (not sticky-point)
        ;; FIXME : test due to timing problems in detecting the drag
        (graphics-release-right x y))))

(tm-define (graphics-choose-point inc)
  (:state graphics-state)
  ;(display* "Graphics] Choose\n")
  (if (not (graphics-states-void?))
      (graphics-pop-state))
  (set! choosing #t)
  (graphics-store-state #f)
  (edit_tab-key (car (graphics-mode)) #t inc))
  
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
	 (if sticky-point (undo 0))
	 (sketch-reset)
	 (graphics-decorations-reset))))

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
