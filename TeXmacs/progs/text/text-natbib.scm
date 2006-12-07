
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-natbib.scm
;; DESCRIPTION : Support for the natbib bibliography styles
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  (let* ((s (tree-as-string t))
	 (type (tree-as-string type-t)))
    (cond ((== type "author") (natbib-author s))
	  ((== type "year") (natbib-year s))
	  ((== type "author*") (natbib-author* s))
	  (else "?"))))
