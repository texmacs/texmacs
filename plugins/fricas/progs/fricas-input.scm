
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fricas-input.scm
;; DESCRIPTION : Fricas input converters
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fricas-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fricas-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(fricas-input-var-row (cdr r)))))

(define (fricas-input-row r)
  (display "[")
  (plugin-input (car r))
  (fricas-input-var-row (cdr r))
  (display "]"))

(define (fricas-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(fricas-input-row (car t))
	(fricas-input-var-rows (cdr t)))))

(define (fricas-input-rows t)
  (display "matrix([")
  (fricas-input-row (car t))
  (fricas-input-var-rows (cdr t))
  (display "])"))

(define (fricas-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (fricas-input-descend-last (cdr args))))

(define (fricas-input-det args)
  (display "determinant(")
  (fricas-input-descend-last args)
  (display ")"))

(define (fricas-input-big-around args)
  (let* ((b `(big-around ,@args))
	 (op (big-name b))
	 (sub (big-subscript b))
	 (sup (big-supscript b))
	 (body (big-body b)))
    (cond
        ((== op "int")
            (begin (display "integrate(") (plugin-input body)
                (if (and sub sup)
                    (begin (display "=") (plugin-input sub)
                        (display "..") (plugin-input sup)))))
        ((== op "sum")
            (begin (display "sum(") (plugin-input body)
                (cond
                    ((and sub sup)
                        (begin (display ",")
                            (plugin-input (texmacs->code (tm->tree sub)))
                            (display "..") (plugin-input sup)))
                    (sub (begin (display ",") (plugin-input sub))))))
        ((== op "prod")
            (begin (display "product(") (plugin-input body)
                (cond
                    ((and sub sup)
                        (begin (display ",")
                            (plugin-input (texmacs->code (tm->tree sub)))
                            (display "..") (plugin-input sup)))
                    (sub (begin (display ",") (plugin-input sub))))))
        (else (display op) (display "(") (plugin-input body)))
    (display ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters fricas
  (rows fricas-input-rows)
  (det fricas-input-det)
  (big-around fricas-input-big-around)
  ("<infty>"      "%infinity")
  ("<emptyset>"   "[]")
  ("<mathd>"      "1,")
  ("<mathi>"      "%i")
  ("<mathe>"      "%e")
  ("<pi>"         "%pi")
  ("<ldots>"      " .. ")
  ("<cdots>"      " .. "))
