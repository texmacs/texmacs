
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;               (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-edit)
  (:use (graphics graphics-env)
        (graphics graphics-single)
        (graphics graphics-group)
        (graphics graphics-animate)))

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
;;   -> des macros comme foreach-number, etc., devraient
;;      etre dans kernel/boot/abbrevs ou kernel/library (et chercher
;;      des noms plus elegants).
;;
;;   -> dans la doc, preciser **exactement** les conditions d'evaluation
;;      des differentes fonctions (par exemple (stree->tree #f) == #f,
;;      mais (stree->tree 1) == <tree 1>.
;;
;;   -> On en discutera davantage apres un premier passage en revue.


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
  (if current-path
  (let* ((p (graphics-object-root-path current-path))
	 (t (if p (path->tree p) #f))
	 (t0 (if t (path->tree (cDr p)) #f))
         (i0 (graphics-graphical-index t0)))
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
;; Major extern interface routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-enter-mode old-mode new-mode)
  (:state graphics-state)
  (if (and (graphics-group-mode? old-mode)
	   (not (graphics-group-mode? new-mode)))
      (graphics-reset-state))
  (if (and (not (graphics-group-mode? old-mode))
	   (graphics-group-mode? new-mode))
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
		   ((in? submode gr-tags-user) (noop))
		   ((graphical-text-tag? submode) (noop))
		   (else (display* "Uncaptured finish (edit)\n")))))
	 ((== (car mode) 'group-edit) (noop))
	 ((== (car mode) 'hand-edit) (noop))
	 (else (display* "Uncaptured finish\n")))))

(tm-define (graphics-busy?)
  (:state graphics-state)
  sticky-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-keyboard-modifiers 0)
(tm-define the-graphics-drop-object #f)

(tm-define (set-keyboard-modifiers mods)
  (set! the-keyboard-modifiers mods))

(tm-define (get-keyboard-modifiers)
  the-keyboard-modifiers)

;;(display* "  sticky-point " sticky-point "\n")
;;(display* "  leftclick-waiting " leftclick-waiting "\n")
;;(display* "  current-graphical-object " current-graphical-object "\n")
;;(display* "  graphics-action " graphics-action "\n")
;;(display* "  current-point-no " current-point-no "\n")

(tm-define (graphics-move x y)
  ;;(display* "Graphics] Move " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (edit_move (car (graphics-mode)) x y)))

(tm-define (graphics-release-left x y)
  ;;(display* "Graphics] Release-left " x ", " y "\n")
  (if (inside-graphical-text?)
      (with-innermost t graphical-text-context?
        (let* ((ps (select-first (s2f x) (s2f y)))
               (p (and ps (car ps))))
          (if (and p (list-starts? p (tree->path t)))
              (go-to p)
              (tree-go-to t :start))))
      (edit_left-button (car (graphics-mode)) x y)))

(tm-define (graphics-release-middle x y)
  ;;(display* "Graphics] Release-middle " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (edit_middle-button (car (graphics-mode)) x y)))

(tm-define (graphics-release-right x y)
  ;;(display* "Graphics] Release-right " x ", " y "\n")  
  (when (not (inside-graphical-text?))
    (edit_right-button (car (graphics-mode)) x y)))

(tm-define (graphics-start-drag-left x y)
  ;;(display* "Graphics] Start-drag-left " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (edit_start-drag (car (graphics-mode)) x y)))

(tm-define (graphics-dragging-left x y)
  ;;(display* "Graphics] Dragging-left " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (edit_drag (car (graphics-mode)) x y)))

(tm-define (graphics-end-drag-left x y)
  ;;(display* "Graphics] End-drag-left " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (edit_end-drag (car (graphics-mode)) x y)))

(tm-define (graphics-start-drag-right x y)
  ;;(display* "Graphics] Start-right-drag " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (graphics-release-right x y)))

(tm-define (graphics-dragging-right x y)
  ;;(display* "Graphics] Right-dragging " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (graphics-move x y)))

(tm-define (graphics-end-drag-right x y)
  (:state graphics-state)
  ;;(display* "Graphics] End-right-drag " x ", " y "\n")
  (when (not (inside-graphical-text?))
    (if (not sticky-point)
        ;; FIXME : test due to timing problems in detecting the drag
        (graphics-release-right x y))))

(tm-define (graphics-choose-point inc)
  (:state graphics-state)
  ;(display* "Graphics] Choose\n")
  (edit_tab-key (car (graphics-mode)) inc))

(tm-define (graphics-drop-object x y)
  (:state graphics-state)
  (sketch-reset)
  (and-with gp (graphics-graphics-path)
    (let* ((gt (path->tree gp))
           (n (- (tree-arity gt) 1))
           (ha (graphics-get-property "gr-text-at-halign"))
           (va (graphics-get-property "gr-text-at-valign"))
           (obj `(text-at ,the-graphics-drop-object (point ,x ,y)))
           (rich `(with "text-at-halign" ,ha "text-at-valign" ,va ,obj)))
      (tree-insert gt n (list rich))
      (tree-go-to gt n 0 :end)
      (graphics-set-mode '(edit text-at)))))
