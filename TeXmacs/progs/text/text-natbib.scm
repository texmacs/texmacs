
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-natbib.scm
;; DESCRIPTION : Support for the natbib bibliography styles
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-natbib))

(define (natbib-author s)
  (let* ((i (string-index s #\()))
    (if i (substring s 0 i) s)))

(define (natbib-year s)
  (let* ((i (string-index s #\())
	 (j (string-index s #\))))
    (if (and i j (< i j)) (substring s (+ i 1) j) "?")))

(define (natbib-author* s)
  (let* ((i (string-index s #\())
	 (j (string-index s #\)))
	 (n (string-length s)))
    (cond ((not (and i j (< i j))) s)
	  ((< (+ j 1) n) (substring s (+ j 1) n))
	  (else (substring s 0 i)))))

(tm-define (natbib-get t type-t)
  (:secure #t)
  (if (tm-func? t 'natbib-triple 3)
      (cond ((== type "author") (tm-ref t 1))
            ((== type "year") (tm-ref t 2))
            ((== type "author*") (tm-ref t 0))
            (else "?"))
      (let* ((s (tree-as-string t))
             (type (tree-as-string type-t)))
        (cond ((== type "author") (string-trim-spaces (natbib-author s)))
              ((== type "year") (string-trim-spaces (natbib-year s)))
              ((== type "author*") (string-trim-spaces (natbib-author* s)))
              (else "?")))))
