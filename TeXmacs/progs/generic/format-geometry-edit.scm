
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-geometry-edit.scm
;; DESCRIPTION : routines for resizing and repositioning
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-geometry-edit)
  (:use (utils edit selections)
	(generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines for length manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (length-increase t by)
  (cond ((tree-in? t '(plus minimum maximum))
	 (length-increase (tree-ref t :last) by))
	((tree-in? t '(minus))
	 (length-increase (tree-ref t :last) (- by)))
	((tm-length? t)
	 (let* ((l (tree->string t))
		(v (tm-length-value l))
		(u (tm-length-unit l))
		(a (if (== u "spc") 0.2 1))
		(new-v (+ v (* by a)))
		(new-l (tm-make-length new-v u)))
	   (tree-set t new-l)))))

(define (length-rightmost t)
  (cond ((tree-in? t '(plus minus minimum maximum))
	 (length-rightmost (tree-ref t :last)))
	(else t)))

(define (lengths-consistent? len1 len2)
  (let* ((t1 (length-rightmost len1))
	 (t2 (length-rightmost len2)))
    (and (tm-length? t1)
	 (tm-length? t2)
	 (== (tm-length-unit t1) (tm-length-unit t2)))))

(define (replace-empty t i by)
  (when (tree-empty? (tree-ref t i))
    (tree-assign (tree-ref t i) by)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rigid horizontal spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (space-context? t)
  (and-with u (tree-down t)
    (tm-func? u 'space)))

(define (space-make-ternary t)
  (cond ((== (tm-arity t) 1) (tree-insert t 1 '("0ex" "1ex")))
	((== (tm-arity t) 2) (tree-insert t 1 '("1ex")))))

(define (space-consistent? t)
  (and (== (tm-arity t) 3)
       (lengths-consistent? (tree-ref t 1) (tree-ref t 2))))

(tm-define (geometry-left)
  (:context space-context?)
  (with-innermost t space-context?
    (length-increase (tree-ref t :down 0) -1)))

(tm-define (geometry-right)
  (:context space-context?)
  (with-innermost t space-context?
    (length-increase (tree-ref t :down 0) 1)))

(tm-define (geometry-up)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (length-increase (tree-ref t 2) 1))))

(tm-define (geometry-down)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (length-increase (tree-ref t 2) -1))))

(tm-define (geometry-top)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (when (space-consistent? t)
	(length-increase (tree-ref t 1) 1)
	(length-increase (tree-ref t 2) 1)))))

(tm-define (geometry-bottom)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (when (space-consistent? t)
	(length-increase (tree-ref t 1) -1)
	(length-increase (tree-ref t 2) -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rubber horizontal spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (hspace-context? t)
  (and-with u (tree-down t)
    (tm-func? u 'hspace)))

(define (rubber-space-consistent? t)
  (or (== (tm-arity t) 1)
      (and (== (tm-arity t) 3)
	   (lengths-consistent? (tree-ref t 0) (tree-ref t 1))
	   (lengths-consistent? (tree-ref t 1) (tree-ref t 2)))))

(define (rubber-space-increase t by)
  (when (rubber-space-consistent? t)
    (length-increase (tree-ref t 0) by)
    (when (== (tm-arity t) 3)
      (length-increase (tree-ref t 1) by)
      (length-increase (tree-ref t 2) by))))

(tm-define (geometry-left)
  (:context hspace-context?)
  (with-innermost t hspace-context?
    (rubber-space-increase (tree-ref t :down) -1)))

(tm-define (geometry-right)
  (:context hspace-context?)
  (with-innermost t hspace-context?
    (rubber-space-increase (tree-ref t :down) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertical spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (vspace-context? t)
  (and-with u (tree-down t)
    (or (tm-func? u 'vspace) (tm-func? u 'vspace*))))

(tm-define (geometry-up)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (rubber-space-increase (tree-ref t :down) -1)))

(tm-define (geometry-down)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (rubber-space-increase (tree-ref t :down) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move and shift
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (move-context? t)
  (tree-in? t '(move shift)))

(tm-define (make-move hor ver)
  (:argument hor "Horizontal")
  (:argument ver "Vertical")
  (wrap-selection-small
    (insert-go-to `(move "" ,hor ,ver) '(0 0))))

(tm-define (make-shift hor ver)
  (:argument hor "Horizontal")
  (:argument ver "Vertical")
  (wrap-selection-small
    (insert-go-to `(shift "" ,hor ,ver) '(0 0))))

(tm-define (geometry-left)
  (:context move-context?)
  (with-innermost t move-context?
    (replace-empty t 1 "0em")
    (length-increase (tree-ref t 1) -1)))

(tm-define (geometry-right)
  (:context move-context?)
  (with-innermost t move-context?
    (replace-empty t 1 "0em")
    (length-increase (tree-ref t 1) 1)))

(tm-define (geometry-down)
  (:context move-context?)
  (with-innermost t move-context?
    (replace-empty t 2 "0ex")
    (length-increase (tree-ref t 2) -1)))

(tm-define (geometry-up)
  (:context move-context?)
  (with-innermost t move-context?
    (replace-empty t 2 "0ex")
    (length-increase (tree-ref t 2) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resize and clipped
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (resize-context? t)
  (tree-in? t '(resize clipped)))

(tm-define (make-resize l b r t)
  (:argument l "Left")
  (:argument b "Bottom")
  (:argument r "Right")
  (:argument t "Top")
  (wrap-selection-small
    (insert-go-to `(resize "" ,l ,b ,r ,t) '(0 0))))

(tm-define (make-clipped l b r t)
  (:argument l "Left")
  (:argument b "Bottom")
  (:argument r "Right")
  (:argument t "Top")
  (wrap-selection-small
    (insert-go-to `(clipped "" ,l ,b ,r ,t) '(0 0))))

(define (replace-empty-horizontal t)
  (replace-empty t 1 '(plus "1l" "0em"))
  (replace-empty t 3 '(plus "1r" "0em")))

(define (replace-empty-vertical t)
  (replace-empty t 2 '(plus "1b" "0em"))
  (replace-empty t 4 '(plus "1t" "0em")))

(define (resize-consistent-horizontal? t)
  (replace-empty-horizontal t)
  (lengths-consistent? (tree-ref t 1) (tree-ref t 3)))

(define (resize-consistent-vertical? t)
  (replace-empty-vertical t)
  (lengths-consistent? (tree-ref t 2) (tree-ref t 4)))

(tm-define (geometry-left)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty-horizontal t)
    (length-increase (tree-ref t 3) -1)))

(tm-define (geometry-right)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty-horizontal t)
    (length-increase (tree-ref t 3) 1)))

(tm-define (geometry-down)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty-vertical t)
    (length-increase (tree-ref t 4) -1)))

(tm-define (geometry-up)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty-vertical t)
    (length-increase (tree-ref t 4) 1)))

(tm-define (geometry-start)
  (:context resize-context?)
  (with-innermost t resize-context?
    (when (resize-consistent-horizontal? t)
      (length-increase (tree-ref t 1) -1)
      (length-increase (tree-ref t 3) -1))))

(tm-define (geometry-end)
  (:context resize-context?)
  (with-innermost t resize-context?
    (when (resize-consistent-horizontal? t)
      (length-increase (tree-ref t 1) 1)
      (length-increase (tree-ref t 3) 1))))

(tm-define (geometry-bottom)
  (:context resize-context?)
  (with-innermost t resize-context?
    (when (resize-consistent-vertical? t)
      (length-increase (tree-ref t 2) -1)
      (length-increase (tree-ref t 4) -1))))

(tm-define (geometry-top)
  (:context resize-context?)
  (with-innermost t resize-context?
    (when (resize-consistent-vertical? t)
      (length-increase (tree-ref t 2) 1)
      (length-increase (tree-ref t 4) 1))))
