
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : htmltest.scm
;; DESCRIPTION : test Html conversion routines
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
