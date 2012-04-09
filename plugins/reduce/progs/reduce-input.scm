
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : reduce-input.scm
;; DESCRIPTION : Reduce input converters
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (reduce-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reduce-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(reduce-input-var-row (cdr r)))))

(define (reduce-input-row r)
  (display "(")
  (plugin-input (car r))
  (reduce-input-var-row (cdr r))
  (display ")"))

(define (reduce-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(reduce-input-row (car t))
	(reduce-input-var-rows (cdr t)))))

(define (reduce-input-rows t)
  (display "mat(")
  (reduce-input-row (car t))
  (reduce-input-var-rows (cdr t))
  (display ")"))

(define (reduce-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (reduce-input-descend-last (cdr args))))

(define (reduce-input-det args)
  (display "det(")
  (reduce-input-descend-last args)
  (display ")"))

(define (reduce-input-big-around args)
  (let* ((b `(big-around ,@args))
	 (op (big-name b))
	 (sub (big-subscript b))
	 (sup (big-supscript b))
	 (body (big-body b)))
    (cond
        ((== op "int")
            (begin (display "int(") (plugin-input body)
                (if (and sub sup)
                    (begin (display ",") (plugin-input sub)
                        (display ",") (plugin-input sup)))))
        ((== op "sum")
            (begin (display "sum(") (plugin-input body)
                (cond
                    ((and sub sup)
                        (begin (display ",")
                            (plugin-input (string-replace (texmacs->code (tm->tree sub)) "=" ","))
                            (display ",") (plugin-input sup)))
                    (sub (begin (display ",") (plugin-input sub))))))
        ((== op "prod")
            (begin (display "prod(") (plugin-input body)
                (cond
                    ((and sub sup)
                        (begin (display ",")
                            (plugin-input (string-replace (texmacs->code (tm->tree sub)) "=" ","))
                            (display ",") (plugin-input sup)))
                    (sub (begin (display ",") (plugin-input sub))))))
        (else (display op) (display "(") (plugin-input body)))
    (display ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters reduce
  (rows reduce-input-rows)
  (det reduce-input-det)
  (big-around reduce-input-big-around)
  ("<infty>"      "infinity")
  ("<emptyset>"   "{}")
  ("<mathd>"      ",")
  ("<mathi>"      "i")
  ("<mathe>"      "e"))
