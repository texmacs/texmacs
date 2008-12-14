
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : htmltest.scm
;; DESCRIPTION : test Html conversion routines
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html htmltest)
  (:use (convert html htmlout) (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Html output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define html-expression
  '(body
    (p (@ ("color" "red")) "Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat.")
    (p (!concat "Hallo allemaal " (b "hopsa") ". Grapje"))
    (p (!concat "Hallo allemaal " (pre "hopsa") ". Grapje"))
    (p "Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat.")))

(define (test-html-document)
  html-expression)

(tm-define (out)
  (let ((s (serialize-html (test-html-document))))
    ;;(display s)
    ;;(display "\n")
    s))
