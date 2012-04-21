
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-markup.scm
;; DESCRIPTION : extra graphical macros
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-markup)
  (:use (graphics graphics-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of graphical macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ca*r x) (if (pair? x) (ca*r (car x)) x))

(tm-define-macro (define-graphics head . l)
  (receive (opts body) (list-break l not-define-option?)
    `(begin
       (set! gr-tags-user (cons ',(ca*r head) gr-tags-user))
       (tm-define ,head ,@opts (:secure #t) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm-point? p) (tm-func? p 'point 2))
(tm-define (tm-x p) (tm-ref p 0))
(tm-define (tm-y p) (tm-ref p 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-graphics (rectangle p1 p2)
  (if (and (tm-point? p1) (tm-point? p2))
      `(cline ,p1 (point ,(tm-x p2) ,(tm-y p1))
              ,p2 (point ,(tm-x p1) ,(tm-y p2)))
      `(line ,p1 ,p2)))

(define-graphics (circle c p)
  (if (and (tm-point? c) (tm-point? p))
      (let* ((cx (tm-x c)) (cy (tm-y c))
             (px (tm-x p)) (py (tm-y p))
             (dx `(minus ,px ,cx)) (dy `(minus ,py ,cy))
             (q1 `(point (minus ,cx ,dx) (minus ,cy ,dy)))
             (q2 `(point (minus ,cx ,dy) (plus ,cy ,dx))))
        `(superpose (with "point-style" "none" ,c) (carc ,p ,q1 ,q2)))
      `(line ,c ,p)))
