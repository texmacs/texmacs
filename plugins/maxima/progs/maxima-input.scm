
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : maxima-input.scm
;; DESCRIPTION : Initialize maxima plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (maxima-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maxima-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(maxima-input-var-row (cdr r)))))

(define (maxima-input-row r)
  (display "[")
  (plugin-input (car r))
  (maxima-input-var-row (cdr r))
  (display "]"))

(define (maxima-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(maxima-input-row (car t))
	(maxima-input-var-rows (cdr t)))))

(define (maxima-input-rows t)
  (display "matrix(")
  (maxima-input-row (car t))
  (maxima-input-var-rows (cdr t))
  (display ")"))

(define (maxima-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (maxima-input-descend-last (cdr args))))

(define (maxima-input-det args)
  (display "determinant(")
  (maxima-input-descend-last args)
  (display ")"))

(define (maxima-input-binom args)
  (display "binomial(")
  (plugin-input (car args))
  (display ",")
  (plugin-input (cadr args))
  (display ")"))

(define (maxima-input-sqrt args)
  (if (= (length args) 1)
      (begin
        (display "sqrt(")
        (plugin-input (car args))
        (display ")"))
      (begin
        (display "(")
        (plugin-input (car args))
        (display ")^(1/(")
        (plugin-input (cadr args))
        (display "))"))))

(define (maxima-input-sum args)
  (if (nnull? args)
      (if (nnull? (cdr args))
          (begin ;; both lower and upper index
            (display "tmsum(")
            (plugin-input (car args))
            (display ",")
            (plugin-input (cadr args))
            (display ","))
          (begin ;; lower index only
            (display "tmlsum(")
            (plugin-input (car args))
            (display ",")))
      (display "tmsum(")))

(define (maxima-input-prod args)
  (if (nnull? args)
      (begin
        (display "tmprod(")
        (plugin-input (car args))
        (if (nnull? (cdr args))
            (begin
              (display ",")
              (plugin-input (cadr args))))
        (display ","))
      (display "tmprod(")))

(define (maxima-input-int args)
  (if (nnull? args)
      (begin
        (display "tmint(")
        (plugin-input (car args))
        (if (nnull? (cdr args))
            (begin
              (display ",")
              (plugin-input (cadr args))))
        (display ","))
      (display "integrate(")))


(define (maxima-input-big-around args)
  (let* ((b `(big-around ,@args))
	 (op (big-name b))
	 (sub (big-subscript b))
	 (sup (big-supscript b))
	 (body (big-body b))
	 (l (cond ((and sub sup) (list sub sup))
		  (sub (list sub))
		  (else (list)))))
    (cond ((== op "sum")  (maxima-input-sum l))
	  ((== op "prod") (maxima-input-prod l))
	  ((== op "int")  (maxima-input-int l))
	  (else (display op) (display "(")))
    (plugin-input body)
    (display ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters maxima
  (rows maxima-input-rows)
  (det maxima-input-det)
  (sqrt maxima-input-sqrt)
  (big-around maxima-input-big-around)
  (binom maxima-input-binom)

  ("<infty>"      "inf")
  ("<emptyset>"   "[]")
  ("<mathd>"      "1,")
  ("<mathi>"      "%i")
  ("<mathe>"      "%e")
  ("<in>"         "=")

  ("<times>"	  "~")
  ("<cdot>"	  ".")

  ("<gamma>"      "%gamma")
  ("<pi>"         "%pi"))
