
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
;; Modifying lengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (length-increase t by)
  (cond ((tree-in? t '(plus minus minimum maximum))
	 (length-increase (tree-ref t :last) by))
	((tm-length? t)
	 (let* ((l (tree->string t))
		(v (tm-length-value l))
		(u (tm-length-unit l))
		(a (if (== u "spc") 0.2 1))
		(new-v (+ v (* by a)))
		(new-l (tm-make-length new-v u)))
	   (tree-set t new-l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (space-context? t)
  (and-with u (tree-down t)
    (and (tm-func? u 'space)
	 (tm-length? (tree-ref u 0)))))

(define (space-make-ternary t)
  (cond ((== (tm-arity t) 1) (tree-insert t 1 '("0ex" "1ex")))
	((== (tm-arity t) 2) (tree-insert t 1 '("1ex")))))

(define (space-consistent? t)
  (and (== (tm-arity t) 3)
       (tm-length? (tree-ref t 1))
       (tm-length? (tree-ref t 2))
       (== (tm-length-unit (tree-ref t 1))
	   (tm-length-unit (tree-ref t 2)))))

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
;; Resizing vertical spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (vspace-context? t)
  (and-with u (tree-down t)
    (and (or (tm-func? u 'vspace 1) (tm-func? u 'vspace* 1))
	 (tm-length? (tree-ref u 0)))))

(tm-define (geometry-up)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (length-increase (tree-ref t :down 0) -1)))

(tm-define (geometry-down)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (length-increase (tree-ref t :down 0) 1)))

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

(define (replace-empty t i by)
  (when (tree-empty? (tree-ref t i))
    (tree-assign (tree-ref t i) by)))

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

(tm-define (geometry-left)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty t 1 '(plus "1l" "0em"))
    (replace-empty t 3 '(plus "1r" "0em"))
    (length-increase (tree-ref t 3) -1)))

(tm-define (geometry-right)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty t 1 '(plus "1l" "0em"))
    (replace-empty t 3 '(plus "1r" "0em"))
    (length-increase (tree-ref t 3) 1)))

(tm-define (geometry-down)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty t 2 '(plus "1b" "0ex"))
    (replace-empty t 4 '(plus "1t" "0ex"))
    (length-increase (tree-ref t 4) -1)))

(tm-define (geometry-up)
  (:context resize-context?)
  (with-innermost t resize-context?
    (replace-empty t 2 '(plus "1b" "0ex"))
    (replace-empty t 4 '(plus "1t" "0ex"))
    (length-increase (tree-ref t 4) 1)))
