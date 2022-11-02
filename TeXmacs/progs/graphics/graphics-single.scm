
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-single.scm
;; DESCRIPTION : editing routines for single graphical objects
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;               (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-single)
  (:use (graphics graphics-object)
        (graphics graphics-env)
        (graphics graphics-main)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic operations
;;
;; NOTE: Imperative functions, which unconditionally perform a given
;;   operation on the sketch.
;;
;;   These functions depend on, and can change the current edit state (i.e.,
;;   being in modifying or in selecting mode), and they maintain the value
;;   of the state variables current-point-no and current-edge-sel?, which
;;   are basic pointers inside the edited object.
;;
;;   In other words, these functions are methods which operate on the sketch,
;;   according to what you need to do when editing in point mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic operations (setting the object)
(define (object-set! o . opt)
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
  (texmacs-error "object-create" "invalid tag"))

(tm-define (object_create tag x y)
  (:require (== tag 'point))
  (object-set! `(point ,x ,y) 'new))

(tm-define (object_create tag x y)
  (:require (or (in? tag gr-tags-curves) (in? tag gr-tags-user)))
  (with o (graphics-enrich `(,tag (point ,x ,y) (point ,x ,y)))
    (graphics-store-state 'start-create)
    (set! current-point-no 1)
    (object-set! o 'checkout)
    (graphics-store-state #f)))

(tm-define (object_create tag x y)
  (:require (graphical-text-tag? tag))
  (with long? (graphical-long-text-tag? tag)
    (object-set! `(,tag ,(if long? `(document "") "") (point ,x ,y)) 'new)
    (and-with d (path->tree (cDr (cursor-path)))
      (when (tree-func? d 'document)
        (tree-go-to d 0 :start)))))

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
  (set-point-sub obj no xcur ycur)
  (object-set! (car (sketch-get))))

(define (object_add-point no xcur ycur x y dirn)
  (define obj (stree-radical (car (sketch-get1))))
  (if (not (graphics-complete? obj))
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
  (let* ((compl (car (sketch-get1)))
         (obj (stree-radical compl)))
    (if (not (graphics-incomplete? obj))
        (with (xobj xp) (graphics-complete obj)
          (set! obj xobj)
          (with tab (make-ahash-table)
            (for (var (graphics-all-attributes))
              (when (nin? var '("gid"))
                (ahash-set! tab var (ahash-ref graphical-attrs var))))
            (graphical-fetch-props (car (sketch-get)))
            (for (var (list "anim-id"))
              (ahash-set! tab var (ahash-ref graphical-attrs var)))
            (set! obj (graphics-enrich-bis
                       obj (ahash-ref graphical-attrs "gid") tab))
            (set! obj (graphics-re-enhance obj compl #f))
            (set! current-edge-sel? #f)
            (sketch-set! `(,obj))
            ;;(display* "Commited " (sketch-get) "\n")
            (sketch-commit)
            (set! leftclick-waiting #f)
            (set! current-obj (stree-radical obj))
            (set! current-point-no #f)
            (graphics-forget-states))))
    (delayed
      (graphics-update-constraints))))

(tm-define (current-in? l)
  (and (pair? current-obj) (in? (car current-obj) l)))

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
;; Edit operations
;;
;; NOTE: Intelligent functions, which take into account the state,
;;   the previous mouse clicks, etc. They also perform printing the
;;   help messages. These functions maintain the value of the state
;;   variables, and they manage state stacking.
;;
;;   In other words, these functions implement the different states
;;   of the editing automaton.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define moveclick-tolerance "5px")
(define previous-leftclick #f)

(define (hardly-moved?)
  (and previous-leftclick
       (points-dist<
        previous-leftclick
        `(point ,current-x ,current-y)
        moveclick-tolerance)))

(define (move-over)
  (set-message (string-append "Left click: new object; "
                              "Drag: edit object; "
                              "Right click: remove; "
                              "Return: apply properties; "
                              "S-Return: fetch properties")
               "Mouse over object")
  (graphics-decorations-update)
  (if current-path
      (with p2 (tm-upwards-path current-path
                                (graphical-text-tag-list) '(graphics))
         (if (not p2) (go-to (rcons current-path 0))))))

(define (edit-clean-up)
  ;; remove cruft which uncareful editing may create
  (with-innermost t 'graphics
    (for (i (reverse (.. 0 (tree-arity t))))
      (with c (tree-ref t i)
        (if (tm-func? c 'with) (set! c (tree-ref c :last)))
        (when (and (graphical-text-context? c)
                   (== (tree->stree (tree-ref c 0)) ""))
          (tree-remove! t i 1))))))

(define (edit-insert x y)
  (edit-clean-up)
  (object_create (cadr (graphics-mode)) x y))

(define (start-move)
  (define edge current-edge-sel?)
  (graphics-store-state 'start-move)
  (object_checkout)
  (graphics-group-start)
  (set! current-edge-sel? #t)
  (set! leftclick-waiting #f)
  (if (and edge (not (graphics-complete? current-obj)))
      (begin
	 (object_add-point current-point-no #f #f current-x current-y #t)
	 (graphics-decorations-update)))
  (graphics-store-state #f))

(define (move-point)
  (if (and leftclick-waiting (not (hardly-moved?)))
      (begin
        (set! leftclick-waiting #f)
        (object_add-point
         current-point-no
         (cadr previous-leftclick) (caddr previous-leftclick)
         current-x current-y
         (== (logand (get-keyboard-modifiers) ShiftMask) 0)))
      (begin
        (if leftclick-waiting
            (set-message "Left click: finish; Right click: undo"
                         "Inserting control points")
            (set-message "Left click: add point; Right click: undo"
                         "Inserting control points"))
        (object_set-point current-point-no current-x current-y)))
  (graphics-decorations-update))

(define (last-point)
  (object_set-point current-point-no current-x current-y)
  (object_commit))

(define (next-point)
  (cond ((not (hardly-moved?))
         (set-message "Left click: finish; Right click: undo"
                      "Inserting control points")
         (set! leftclick-waiting #t))
        (leftclick-waiting
         (last-point))
        ((== current-point-no 1)
         (undo 0)
         (set! leftclick-waiting #f))
        (else
          (set-message "Left click: finish; Right click: undo"
                       "Inserting control points")
         (graphics-back-state #f)
         (graphics-move current-x current-y)
         (set! leftclick-waiting #t))))

(define (remove-point)
  (if (or (graphics-minimal? current-obj)
	  (not (current-in? gr-tags-all))
	  (!= (logand (get-keyboard-modifiers) ShiftMask) 0))
      (begin
        (object_remove)
        (graphics-decorations-reset)
        (graphics-group-start))
      (begin
        (object_remove-point current-point-no)
        (graphics-decorations-update))))

;; Middle button
(tm-define (graphics-delete)
  (if sticky-point
      (begin
        (graphics-back-state #f)
        (graphics-move current-x current-y))
      (remove-point)))

(tm-define (graphics-update-decorations)
  (:state graphics-state)
  (if current-obj (graphics-decorations-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default global dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit_move mode x y)
  (display* "Uncaptured graphical move " mode ", " x ", " y "\n"))

(tm-define (edit_left-button mode x y)
  (display* "Uncaptured graphical left-button " mode ", " x ", " y "\n"))

(tm-define (edit_middle-button mode x y)
  (display* "Uncaptured graphical middle-button " mode ", " x ", " y "\n"))

(tm-define (edit_right-button mode x y)
  (display* "Uncaptured graphical right-button " mode ", " x ", " y "\n"))

(tm-define (edit_start-drag mode x y t p)
  (edit_left-button mode x y))

(tm-define (edit_drag mode x y t p)
  (edit_move mode x y))

(tm-define (edit_end-drag mode x y t p)
  (edit_left-button mode x y))

(tm-define (edit_tab-key mode inc)
  (display* "Uncaptured graphical tab-key " mode ", " inc "\n"))

(tm-define (edit_delete)
  (:state graphics-state)
  (edit_middle-button 'edit current-x current-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit_move mode x y)
  (:require (== mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross #t)
  (if current-obj
      (begin
        (if (current-in? (graphical-text-tag-list))
            (set! current-point-no 1))
        (if sticky-point
            (move-point)
            (move-over)))
      (begin
        (set-message "Left click: new object" "Graphics")
        (graphics-decorations-reset))))

(define (pointer-inside-graphical-text?)
  (and-with l (select-first (s2f current-x) (s2f current-y))
    (and-with p (and (nnull? l) (car l))
      (and-with t (path->tree (cDr p))
	(not (tree-in? t '(text-at math-at document-at)))))))

(tm-define (edit_left-button mode x y)
  (:require (== mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (cond (sticky-point
         (if (current-in? (graphical-text-tag-list))
             (object_commit)
             (next-point)))
        ((and (current-in? (graphical-text-tag-list))
              (== (car (graphics-mode)) 'edit)
              (graphical-contains-text-tag? (cadr (graphics-mode)))
              (not (graphical-contains-curve-tag? (cadr (graphics-mode))))
	      (pointer-inside-graphical-text?))
         (set-texmacs-pointer 'text-arrow)
         (go-to (car (select-first (s2f current-x) (s2f current-y)))))
        (else
	 (edit-insert x y)))
  (set! previous-leftclick `(point ,current-x ,current-y)))

(tm-define (edit_middle-button mode x y)
  (:require (== mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (when current-obj
    (graphics-delete)))

(tm-define (edit_right-button mode x y)
  (:require (== mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (when current-obj
    (graphics-delete)))

(tm-define (edit_start-drag mode x y t p)
  (:require (== mode 'edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (set! dragging-busy? #t)
  (set! dragging-create? (or sticky-point (not current-obj)))
  (if (or sticky-point current-obj)
      (begin
        (if (current-in? (graphical-text-tag-list))
            (set! current-point-no 1))
        (if sticky-point
            (next-point)
            (start-move)))
      (edit-insert x y))
  (set! previous-leftclick `(point ,current-x ,current-y)))

(tm-define (edit_drag mode x y t p)
  (:require (== mode 'edit))
  (:state graphics-state)
  (edit_move mode x y)
  (set-message "Release left button: finish editing" "Dragging"))

(tm-define (edit_end-drag mode x y t p)
  (:require (== mode 'edit))
  (:state graphics-state)
  (when dragging-busy?
    (set-texmacs-pointer 'graphics-cross)
    (if (or sticky-point current-obj)
        (if dragging-create?
            (edit_move mode x y)
            (last-point)))
    (set! dragging-busy? #f)
    (set! dragging-create? #f)
    (set! previous-leftclick `(point ,current-x ,current-y))))

(tm-define (edit_tab-key mode inc)
  (:require (== mode 'edit))
  (:state graphics-state)
  (if (and current-x current-y)
      (begin
        (select-next inc)
        (graphics-update-decorations))
      (invalidate-graphical-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hand drawn objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit_move mode x y)
  (:require (== mode 'hand-edit))
  (:state graphics-state)
  (noop))

(tm-define (edit_left-button mode x y)
  (:require (== mode 'hand-edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (edit-clean-up)
  (object-set! `(with "point style" "disk"
		      "point-size" ,(graphics-get-property "line-width")
		  (point ,x ,y)) 'new))

(tm-define (edit_start-drag mode x y t* p*)
  (:require (== mode 'hand-edit))
  (:state graphics-state)
  (set-texmacs-pointer 'graphics-cross)
  (edit-clean-up)
  (let* ((t (number->string t*))
         (p (number->string p*))
         (pen (cadr (graphics-mode)))
         (cal `(,pen (point ,x ,y) (point ,x ,y)
                     (ink-meta ,(create-unique-id)
                               ,(number->string (get-graphical-pixel)))
                     (tuple (tuple ,x ,y ,t ,p))))
         (o (graphics-enrich cal)))
    (graphics-store-state 'start-create)
    (object-set! o 'checkout)
    (graphics-store-state #f)))

(tm-define (edit_drag mode x y t* p*)
  (:require (== mode 'hand-edit))
  (:state graphics-state)
  (let* ((t (number->string t*))
         (p (number->string p*))
         (obj (car (sketch-get1)))
         (cal (stree-radical obj))
         (rad (cAr cal)))
    (set-cdr! (cdr cal) (cons `(point ,x ,y) (cdddr cal)))
    (set-cdr! rad (append (cdr rad) (list `(tuple ,x ,y ,t ,p))))
    (object-set! obj))
  (graphics-decorations-update))

(tm-define (edit_end-drag mode x y t p)
  (:require (== mode 'hand-edit))
  (:state graphics-state)
  (object_commit)
  (graphics-decorations-reset))

(tm-define (graphics-complete? obj)
  (:require (tm-func? obj 'calligraphy))
  ;; Temporarily redefine; we should decide on
  ;; the arity of the 'calligraphy' tag
  (>= (tm-arity obj) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't dispatch certain actions on textual arguments of graphical macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-macro-arg? mode)
  (and (== mode 'edit)
       (graphical-text-arg-context? current-obj)))

(tm-define (edit_middle-button mode x y)
  (:require (edit-macro-arg? mode))
  (:state graphics-state)
  ;; FIXME: should destroy graphical macro
  (noop))

(tm-define (edit_start-drag mode x y t p)
  (:require (edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

(tm-define (edit_drag mode x y t p)
  (:require (edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

(tm-define (edit_end-drag mode x y t p)
  (:require (edit-macro-arg? mode))
  (:state graphics-state)
  (noop))
