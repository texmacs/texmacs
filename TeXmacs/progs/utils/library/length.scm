
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : length.scm
;; DESCRIPTION : subroutines for TeXmacs lengths
;; COPYRIGHT   : (C) 2001  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Length arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (length+ . ops)
  (define (length-add* ops res)
    (if (null? ops)
	res
	(length-add* (cdr ops) (length-add res (car ops)))))
  (if (null? ops)
      "0tmpt"
      (length-add* (cdr ops) (car ops))))

(tm-define (length- . ops)
  (if (null? (cdr ops))
      (string-append "-" (car ops))
      (length+ (car ops) (length- (apply length+ (cdr ops))))))

(tm-define (length-zero? len)
  (= 0 (length-decode len)))
