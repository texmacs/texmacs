
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtable.scm
;; DESCRIPTION : tools for converting colors from and to other formats
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmcolor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal record utilities

(define tmcolor-type (make-record-type
		      'tmcolor '(name-promise red green blue)))
(define tmcolor-record (record-constructor tmcolor-type))
(define tmcolor-name-promise (record-accessor tmcolor-type 'name-promise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors

(tm-define tmcolor? (record-predicate tmcolor-type))
(define tmcolor-red (record-accessor tmcolor-type 'red))
(define tmcolor-green (record-accessor tmcolor-type 'green))
(define tmcolor-blue (record-accessor tmcolor-type 'blue))
(define (tmcolor-name c) (force (tmcolor-name-promise c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors

(tm-define (tmcolor red green blue)
  (for-each check-tmcolor-range (list red green blue))
  (tmcolor-record (delay (tmcolor-closest-name
			  (tmcolor-record #f red green blue)))
		  red green blue))

(tm-define (rgb255->tmcolor rgb255)
  (apply tmcolor (map (cut / <> 255) rgb255)))

(define (check-tmcolor-range n)
  ;; Helper argument checking procedure
  (check-arg-range (lambda (n) (and (<= 0 n) (<= n 1)))
		   (check-arg-type number? n "tmcolor")
		   "tmcolor"))

(tm-define (stm->tmcolor name)
  (let ((c (list-any (lambda (c) (== name (tmcolor-name c)))
		     texmacs-colors)))
    (if c c (stm->tmcolor "black"))))

(tm-define tmcolor->stm tmcolor-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding name closest to an arbitrary color

(define (named-tmcolor name red green blue)
  (for-each check-tmcolor-range (list red green blue))
  (tmcolor-record (delay name) red green blue))

(define (named-rgb255->tmcolor name rgb255)
  (with (red green blue) (map (cut / <> 255) rgb255)
    (named-tmcolor name red green blue)))

(define pastel 223)
(define dark-pastel (- (* 2 pastel) 255))
(define texmacs-colors
  (map
   (cut apply named-rgb255->tmcolor <>)
   ;; TODO: loading TeXmacs/langs/colors/base.scm
   `(("black" (0 0 0)) ("white" (255 255 255)) ("grey" (184 184 184))
     ("red" (255 0 0)) ("blue" (0 0 255)) ("yellow" (255 255 0))
     ("green" (0 255 0)) ("magenta" (255 0 255)) ("cyan" (0 255 255))
     ("orange" (255 128 0)) ("brown" (128 32 0)) ("pink" (255 128 128))
     ("broken white" (255 255 ,pastel)) ("light grey" (208 208 208))
     ("dark grey" (112 112 112)) ("darker grey" (64 64 64))
     ("dark red" (128 0 0)) ("dark blue" (0 0 128))
     ("dark yellow" (128 128 0)) ("dark green" (0 128 0))
     ("dark magenta" (128 0 128)) ("dark cyan" (0 128 128))
     ("dark orange" (128 64 0)) ("dark brown" (64 16 0))
     ("pastel grey" (,pastel ,pastel ,pastel))
     ("pastel red" (255 ,pastel ,pastel)) ("pastel blue" (,pastel ,pastel 255))
     ("pastel yellow" (255 255 ,pastel)) ("pastel green" (,pastel 255 ,pastel))
     ("pastel magenta" (255 ,pastel 255)) ("pastel cyan" (,pastel 255 255))
     ("pastel orange" (255 ,pastel ,dark-pastel))
     ("pastel brown" (,pastel ,dark-pastel ,dark-pastel)))))

(define (tmcolor-closest-name c)
  ((cut <> #f)
   (list-fold (lambda (kar kdr) (kdr kar))
	      (cut closest-name/step <> c (car texmacs-colors)
		   (tmcolor-distance c (car texmacs-colors)))
	      (cdr texmacs-colors))))

(define (closest-name/step candidate goal best best-distance)
  (if candidate
      (let ((candidate-distance (tmcolor-distance goal candidate)))
	(if (< candidate-distance best-distance)
	    (cut closest-name/step <> goal candidate candidate-distance)
	    (cut closest-name/step <> goal best best-distance)))
      (tmcolor-name best)))

(define (tmcolor-distance c1 c2)
  (cartesian-distance (tmcolor->list c1) (tmcolor->list c2)))

(define (cartesian-distance x1s x2s)
  (sqrt (list-fold + 0 (map (lambda (x1 x2) (let ((Dx (- x1 x2))) (* Dx Dx)))
			    x1s x2s))))
(define (tmcolor->list c)
  (list (tmcolor-red c) (tmcolor-green c) (tmcolor-blue c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color convertions

(define (rgb255->cmy255 col)
  (map (lambda (x) (round (- 255 x))) col))

(define (cmy255->rgb255 col)
  (map (lambda (x) (round (- 255 x))) col))

(define (cmy255->cmyk255 col)
  (let ((c (car col))
        (m (cadr col))
        (y (caddr col))
        (k  255))
    (if (< c k) (set! k c))
    (if (< m k) (set! k m))
    (if (< y k) (set! k y))
    (if (== k 255) '(0 0 0 255)
      (let* ((k*   (- 1 (/ k 255)))
             (col* (map (lambda (x) (round (/ (- x k) k*))) col)))
        `(,@col* ,k)))))

(define (cmyk255->cmy255 col)
  (let ((col* (cDr col))
        (k    (cAr col)))
    (map (lambda (x) (round (+ k (* x (- 1 (/ k 255)))))) col*)))

(define (cmyk255->rgb255 col)
  (cmy255->rgb255 (cmyk255->cmy255 col)))

(define (rgb255->cmyk255 col)
  (cmy255->cmyk255 (rgb255->cmy255 col)))
