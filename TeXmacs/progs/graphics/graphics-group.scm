
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-group.scm
;; DESCRIPTION : editing routines for graphics group mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004-2007  Joris van der Hoeven and Henri Lesourd
;;               (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-group)
  (:use (graphics graphics-env)
        (graphics graphics-single)
        (kernel gui kbd-handlers)
        (dynamic animate-edit)))

(cond-expand
 (guile-3
  (use-modules (ice-9 copy-tree))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group edit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (if (match? o '(point :%2))
	   (set! so-points (cons o so-points)))
       o))
  (set! group-bary-x #f)
  (set! group-bary-y #f)
  (if (nnull? (sketch-get))
      (with so (map tree->stree (sketch-get))
        (traverse-transform so (store-points))
        (if (nnull? so-points)
            (with n 0
              (set! group-bary-x 0)
              (set! group-bary-y 0)
              (for (p so-points)
                (set! group-bary-x (+ group-bary-x (s2f (cadr p))))
                (set! group-bary-y (+ group-bary-y (s2f (caddr p))))
                (set! n (+ n 1)))
              (set! group-bary-x (/ group-bary-x n))
              (set! group-bary-y (/ group-bary-y n))))))
  (set! group-first-x (s2f current-x))
  (set! group-first-y (s2f current-y))
  (> (point-norm (sub-point `(,group-first-x ,group-first-y)
			    `(,group-bary-x ,group-bary-y))) 1e-3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sub-point p1 p2)
  `(,(- (car p1) (car p2))
    ,(- (cadr p1) (cadr p2))))

(define (point-norm p)
  (sqrt (+ (* (car p) (car p))
	   (* (cadr p) (cadr p)))))

(define (traverse-transform o opn)
  (define (traverse o)
    (opn (if (pair? o) (map traverse o) o)))
  (traverse o))

(define (translate-point x y)
  (lambda (o)
    (if (match? o '(point :%2))
	`(point ,(f2s (+ x (s2f (cadr o)))) ,(f2s (+ y (s2f (caddr o)))))
        o)))

(define (group-translate x y)
  (lambda (o)
    (traverse-transform o (translate-point x y))))

(define (zoom-point x0 y0 h)
  (lambda (o)
    (if (match? o '(point :%2))
        (let* ((x (s2f (cadr o)))
               (y (s2f (caddr o))))
          `(point ,(f2s (+ x0 (* (- x group-bary-x) h)))
                  ,(f2s (+ y0 (* (- y group-bary-y) h)))))
        o)))

(define (group-zoom x y)
  (with h (/ (point-norm (sub-point `(,x ,y)
				    `(,group-bary-x ,group-bary-y)))
	     (point-norm (sub-point `(,group-first-x ,group-first-y)
				    `(,group-bary-x ,group-bary-y))))
    (lambda (o)
      (let* ((res (traverse-transform
                   o (zoom-point group-bary-x group-bary-y h)))
             (curmag #f))
	(if (eq? (car res) 'with)
	    (with curmag (s2f (find-prop res "magnify" "1.0"))
              (list-find&set-prop
               res "magnify" (f2s (* curmag h))))
            `(with "magnify" ,(f2s h) ,res))))))

(define (rotate-point x0 y0 alpha)
  (lambda (o)
    (if (match? o '(point :%2))
        (let* ((x (- (s2f (cadr o)) group-bary-x))
               (y (- (s2f (caddr o)) group-bary-y)))
          `(point ,(f2s (+ x0 (* x (cos alpha)) (* (- y) (sin alpha))))
                  ,(f2s (+ y0 (* x (sin alpha)) (* y (cos alpha))))))
        o)))

(define (group-rotate x y)
  (let* ((b (make-rectangular group-bary-x group-bary-y))
	 (f (make-rectangular group-first-x group-first-y))
	 (p (make-rectangular x y))
	 (alpha (- (angle (- p b)) (angle (- f b)))))
    (lambda (o)
      (traverse-transform o (rotate-point group-bary-x group-bary-y alpha)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group / ungroup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (group-selected-objects)
  (if (and (not sticky-point) (nnull? (sketch-get)))
      (begin
        (graphics-store-state 'group-selected-objects)
        (sketch-checkout)
        (with o (cons 'gr-group (sketch-get))
          (sketch-reset)
          (sketch-toggle o))
        (sketch-commit)
        (graphics-group-start)
        (set! graphics-undo-enabled #t)
        (graphics-forget-states))))

(tm-define (ungroup-current-object)
  (if (and (not sticky-point)
	   (== (length (sketch-get)) 1)
	   (== (tree-label (car (sketch-get))) 'gr-group))
      ;; TODO: Add support for ungrouping <with|...props...|<gr-group|...>>
      (with obj (car (sketch-get))
        (graphics-store-state 'ungroup-selected-objects)
        (sketch-checkout)
        (sketch-reset)
        (foreach-number (i 0 < (tree-arity obj))
                        (sketch-toggle (tree-ref obj i)))
        (sketch-commit)
        (graphics-group-start)
        (set! graphics-undo-enabled #t)
        (graphics-forget-states))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste attribute style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-assign-props p obj)
  (let* ((l1 (graphics-all-attributes))
         (l2 (map gr-prefix l1))
         (l3 (map (graphics-get-property-at p) l2))
         (l4 (map cons l1 l3))
         (tab (list->ahash-table l4)))
    (graphics-remove p 'memoize-layer)
    (graphics-group-enrich-insert-table (stree-radical obj) tab #f)))

(tm-define (graphics-get-props p)
  (and-with t (path->tree p)
    (let* ((attrs (graphical-relevant-attributes t))
           (vars (list-difference attrs '("gid" "anim-id")))
           (get-prop (lambda (var) (graphics-path-property p var)))
           (gr-vars (map gr-prefix vars))
           (vals (map get-prop vars)))
      (for-each graphics-set-property gr-vars vals))))

(tm-define (graphics-get-props-at-mouse)
  (and-with p current-path
    (graphics-get-props p)))

(define (with-list vars vals)
  (cond ((or (null? vars) (null? vals)) (list))
        ((== (car vals) "default") (with-list (cdr vars) (cdr vals)))
        (else (cons* (car vars) (car vals)
                     (with-list (cdr vars) (cdr vals))))))

(define (graphics-tree-apply-props t vars vals)
  (with l (with-list vars vals)
    (and-with w (tree-up t)
      (if (tree-is? w 'with)
          (if (null? l)
              (tree-set! w (tm-ref w :last))
              (tree-set! w `(with ,@l ,(tm-ref w :last))))
          (if (null? l)
              (noop)
              (tree-set! t `(with ,@l ,t)))))))

(tm-define (graphics-apply-props p)
  (and-with t (path->tree p)
    (let* ((attrs (graphical-relevant-attributes t))
           (vars (list-difference attrs '("gid" "anim-id")))
           (gr-vars (map gr-prefix vars))
           (vals (map graphics-get-property gr-vars)))
      (graphics-tree-apply-props t vars vals))))

(tm-define (graphics-apply-props-at-mouse)
  (and-with p current-path
    (graphics-apply-props p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has-attribute? t var)
  (cond ((not (tree? t)) #f)
        ((tree-is? t 'anim-edit) (has-attribute? (tm-ref t 1) var))
        ((tree-in? t '(anim-static anim-dynamic))
         (with c (or (graphics-anim-frames t) (list))
           (list-or (map (cut has-attribute? <> var) c))))
        ((tree-is? t 'with) (has-attribute? (tm-ref t :last) var))
        ((tree-atomic? t) #f)
        (else (graphics-attribute? (tree-label t) var))))

(tm-define (graphics-mode-attribute? mode var)
  (:require (== (graphics-mode) '(group-edit edit-props)))
  (with v (if (string-starts? var "gr-") (string-drop var 3) var)
    (with l (map (cut has-attribute? <> v) (sketch-get))
      (list-or l))))

(define (property-get t var i)
  (cond ((not (tree? t)) "default")
        ((tree-is? t 'anim-edit) (property-get (tm-ref t 1) var 0))
        ((tree-in? t '(anim-static anim-dynamic))
         (with c (or (graphics-anim-frames t) (list))
           (properties-and (map (cut property-get <> var 0) c))))
        ((not (tree-is? t 'with)) "default")
        ((>= i (- (tree-arity t) 1)) "default")
        ((tm-equal? (tree-ref t i) var) (tree->stree (tree-ref t (+ i 1))))
        (else (property-get t var (+ i 2)))))

(define (property-and p1 p2)
  (if (== p1 p2) p1 "mixed"))

(tm-define (properties-and l)
  (cond ((null? l) "default")
        ((null? (cdr l)) (car l))
        (else (property-and (car l) (properties-and (cdr l))))))

(tm-define (graphics-get-property var)
  (:require (and (== (graphics-mode) '(group-edit edit-props))
                 (graphics-selection-active?)))
  (with v (if (string-starts? var "gr-") (string-drop var 3) var)
    (if (graphics-mode-attribute? (graphics-mode) v)
        (with l (map (cut property-get <> v 0) (sketch-get))
          (properties-and l))
        (former var))))

(define (property-remove t var i)
  (cond ((>= i (- (tree-arity t) 1)) t)
        ((tm-equal? (tree-ref t i) var)
         (if (== (tree-arity t) 3)
             (tree-remove-node! t 2)
             (tree-remove! t i 2))
         t)
        (else (property-remove t var (+ i 2)))))

(define (property-set-sub t var val i)
  (cond ((>= i (- (tree-arity t) 1))
         (tree-insert! t i (list var val))
         t)
        ((tm-equal? (tree-ref t i) var)
         (tree-set (tree-ref t (+ i 1)) val)
         t)
        (else (property-set-sub t var val (+ i 2)))))

(define (property-set t var val)
  (cond ((not (tree? t)) t)
        ((tree-is? t 'anim-edit)
         (property-set (tree-ref t 1) var val))
        ((tree-in? t '(anim-static anim-dynamic))
         (with c (or (graphics-anim-frames t) (list))
           (for-each (cut property-set <> var val) c)
           t))
        ((tree-is? t 'with)
         (if (== val "default")
             (property-remove t var 0)
             (property-set-sub t var val 0)))
        ((== val "default") t)
        (else
          (tree-set! t `(with ,var ,val ,t))
          t)))

(tm-define (graphics-set-property var val)
  (:require (and (== (graphics-mode) '(group-edit edit-props))
                 (graphics-selection-active?)))
  (with v (if (string-starts? var "gr-") (string-drop var 3) var)
    (if (graphics-mode-attribute? (graphics-mode) v)
        (with r (map (cut property-set <> v val) (sketch-get))
          (sketch-set! r))
        (former var val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remove-selected-objects)
  (sketch-checkout)
  (sketch-reset)
  (sketch-commit)
  (graphics-group-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State transitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (start-operation opn p obj)
  (texmacs-error "start-operation" "invalid context"))

(tm-define (start-operation opn p obj)
  (:require (graphical-non-group-tag? (car obj)))
  (set! current-path #f)
  (if (not sticky-point)
      (set! preselected (nnull? (sketch-get))))
  (cond
    ;;Perform operation
    (sticky-point
     (sketch-commit)
     (graphics-decorations-update)
     (if (== (state-ref graphics-first-state 'graphics-action)
             'start-operation)
         (remove-undo-mark))
     (set! graphics-undo-enabled #t)
     (graphics-forget-states)
     (if (not preselected) (unselect-all p obj))
     (set! preselected #f))
    ;;Start operation
    ((and (not multiselecting) (eq? (cadr (graphics-mode)) 'group-ungroup))
     (if (and p (not sticky-point) (null? (sketch-get))
              (== (tree-label (path->tree p)) 'gr-group))
         (sketch-set! `(,(path->tree p))))
     (if (and (not sticky-point)
              (== (length (sketch-get)) 1)
              (== (tree-label (car (sketch-get))) 'gr-group))
         (ungroup-current-object)
         (group-selected-objects)))
    ((and (not multiselecting) (== (cadr (graphics-mode)) 'props))
     (if (null? (sketch-get))
         (if p
             (begin
               (set! obj (stree-at p))
               (set! current-path (graphics-assign-props p obj))
               (set! current-obj obj)
               (graphics-decorations-update)))
         (with l '()
           (for (o (sketch-get))
             (with p (graphics-assign-props
                      (tree->path o)
                      (tree->stree o))
               (set! l (cons (path->tree p) l))))
           (sketch-set! (reverse l))
           (graphics-decorations-update)))
     (graphics-group-start))
    ((and (not multiselecting) (or p (nnull? (sketch-get))))
     (if (null? (sketch-get))
         (any_toggle-select #f #f p obj))
     (store-important-points) ;; ignore return value?
     (graphics-store-state 'start-operation)
     (sketch-checkout)
     (sketch-transform tree->stree)
     (set! group-first-go (copy-tree (sketch-get)))
     (set! graphics-undo-enabled #f)
     (graphics-store-state #f)
     (set! group-old-x (s2f current-x))
     (set! group-old-y (s2f current-y)))))

(define (any_toggle-select x y p obj)
  (if (not sticky-point)
      (if multiselecting
          (let* ((x1 (s2f selecting-x0))
                 (y1 (s2f selecting-y0))
                 (x2 (s2f x))
                 (y2 (s2f y))
                 (tmp 0)
                 (sel #f))
            (if (> x1 x2)
                (begin
                  (set! tmp x1)
                  (set! x1 x2)
                  (set! x2 tmp)))
            (if (> y1 y2)
                (begin
                  (set! tmp y1)
                  (set! y1 y2)
                  (set! y2 tmp)))
            (set! sel (graphics-select-area x1 y1 x2 y2))
            (sketch-reset)
            (for (p sel)
              (sketch-toggle (path->tree p)))
            (graphics-decorations-update)
            (set! multiselecting #f)
            (set! selecting-x0 #f)
            (set! selecting-y0 #f))
          (if p
              (with t (path->tree p)
                (sketch-toggle t)
                (graphics-decorations-update))
              (begin
                (set! selecting-x0 x)
                (set! selecting-y0 y)
                (set! multiselecting #t))))))

(tm-define (toggle-select x y p obj)
  (texmacs-error "toggle-select" "invalid context"))

(tm-define (toggle-select x y p obj)
  (:require (graphical-non-group-tag? (car obj)))
  (when (list? p)
    (and-with t (path->tree p)
      (tree-go-to t :end)))
  (any_toggle-select x y p obj))

(define (any_unselect-all p obj)
  (cond ((nnull? (sketch-get))
         (sketch-reset)
         (graphics-decorations-update))
        ((and p (not multiselecting) (== (cadr (graphics-mode)) 'props))
         (graphics-get-props p))))

(tm-define (unselect-all p obj)
  (texmacs-error "unselect-all" "invalid context"))

(tm-define (unselect-all p obj)
  (:require (graphical-non-group-tag? (car obj)))
  (any_unselect-all p obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit_move mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (cond (sticky-point
         (set! x (s2f x))
         (set! y (s2f y))
         (with mode (graphics-mode)
           (cond ((== (cadr mode) 'move)
                  (sketch-transform
                   (group-translate (- x group-old-x)
                                    (- y group-old-y))))
                 ((== (cadr mode) 'zoom)
                  (sketch-set! group-first-go)
                  (sketch-transform (group-zoom x y)))
                 ((== (cadr mode) 'rotate)
                  (sketch-set! group-first-go)
                  (sketch-transform (group-rotate x y)))))
         (set! group-old-x x)
         (set! group-old-y y))
        (multiselecting
         (graphical-object!
          (append
           (create-graphical-props 'default #f)
           `((with color red
               (cline (point ,selecting-x0 ,selecting-y0)
                      (point ,x ,selecting-y0)
                      (point ,x ,y)
                      (point ,selecting-x0 ,y)))))))
        (else
          (cond (current-path
                 (set-message (string-append "Left click: operate; "
                                             "Right click: select/unselect")
                              "Group of objects"))
                ((nnull? (sketch-get))
                 (set-message "Left click: operate"
                              "Group of objects"))
                (else
                  (set-message "Move over object on which to operate"
                               "Edit groups of objects")))
          (graphics-decorations-update))))

(tm-define (edit_move mode x y)
  (:require (and (== mode 'edit) (current-in? '(gr-group))))
  (:state graphics-state)
  (if sticky-point
      (display* "Uncaptured graphical move " mode ", " x ", " y "\n")
      (begin
        (set! current-point-no #f)
	(graphics-decorations-update))))

(tm-define (edit_left-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (start-operation 'move current-path current-obj))

(tm-define (edit_left-button mode x y)
  (:require (in? (graphics-mode) '((group-edit edit-props)
                                   (group-edit animate))))
  (:state graphics-state)
  (if (and (not current-path) (graphics-selection-active?))
      (unselect-all current-path current-obj)
      (begin
        (unselect-all current-path current-obj)
        (toggle-select x y current-path current-obj))))

(tm-define (edit_right-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (if (and (not current-path) (graphics-selection-active?))
      (unselect-all current-path current-obj)
      (toggle-select x y current-path current-obj)))

(tm-define (edit_middle-button mode x y)
  (:require (eq? mode 'group-edit))
  (:state graphics-state)
  (if (!= (logand (get-keyboard-modifiers) ShiftMask) 0)
      (if (null? (sketch-get))
	  (graphics-delete)
	  (remove-selected-objects))
      (unselect-all current-path current-obj)))

(tm-define (edit_tab-key mode inc)
  (:require (eq? mode 'group-edit))
  ;;(display* "Graphics] Group-edit(Tab)\n")
  (edit_tab-key 'edit inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't act on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (group-edit-macro-arg? mode)
  (and (== mode 'group-edit)
       current-path
       (path->tree current-path)
       (graphical-text-arg-context? (path->tree current-path))))

(tm-define (edit_move mode x y)
  (:require (group-edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

(tm-define (edit_left-button mode x y)
  (:require (group-edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

(tm-define (edit_right-button mode x y)
  (:require (group-edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

(tm-define (edit_middle-button mode x y)
  (:require (group-edit-macro-arg? mode))
  (:state graphics-state)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut & paste actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-selection-active?)
  (:state graphics-state)
  (nnull? (sketch-get)))

(tm-define (graphics-copy)
  (:state graphics-state)
  (if (== (car (graphics-mode)) 'group-edit)
      (with copied-objects (list-copy (sketch-get))
        (any_unselect-all #f #f)
        (update-current-buffer)
        (if (null? copied-objects)
            (stree->tree "")
            (stree->tree (cons 'graphics copied-objects))))
      (stree->tree "")))

(tm-define (graphics-cut)
  (:state graphics-state)
  (if (== (car (graphics-mode)) 'group-edit)
      (let* ((l (list-copy (sketch-get)))
             (res (graphics-copy)))
        (sketch-set! l)
        (sketch-checkout)
        (sketch-reset)
        (sketch-commit)
        res)
      (stree->tree "")))

(tm-define (graphics-paste sel)
  (:state graphics-state)
  ;;(display* "sel=" sel "\n")
  (if (and (== (car (graphics-mode)) 'group-edit)
	   (tree-compound? sel)
	   (== (tree-label sel) 'graphics)
	   (> (tree-arity sel) 0))
      (begin
        (sketch-reset)
        (sketch-checkout)
        (foreach-number (i 0 < (tree-arity sel))
                        (sketch-toggle (tree-ref sel i)))
        (sketch-commit)
        (graphics-group-start))))
