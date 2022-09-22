
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : textest.scm
;; DESCRIPTION : test LaTeX conversion routines
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex textest)
  (:use
    (convert tools tmpre) (convert tools output)
    (convert latex tmtex) (convert latex texout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tex-expression
  '(!document
    "Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat."
    (!paragraph
     "Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat."
     ((!begin "equation")
      ((!begin "array" "ccc")
       (!table
        (!row "a" "b" "c")
        (!row "x" "y" "z")
        (!row "a" "b" "c"))))
     "Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat.")
    ((!begin "itemize")
     (!document
      (!concat
       (item)
       " Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat.")
      (!concat
       (item)
       " Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat. Hallo allemaal laten we eens even kijken hoe het hiermee staat.")))
    (!concat
     "The formula "
     (!math
      (!concat
       (frac "1" "2")
       " + "
       (frac "1" "3")
       " + "
       (frac (sqrt "3") "4")
       " + "
       (frac "1" "5")
       " + "
       (frac "1" "6")
       " + "
       (frac "1" "7")))
     " is funny no? "
     "The formula "
     (!math
      (!concat
       (frac "1" (!concat "x" (!sub "2")))
       " + "
       (frac "1" "3")
       " + "
       (frac "1" "4")
       " + "
       (frac "1" "5")
       " + "
       (frac "1" "6")
       " + "
       (frac "1" "7")))
     " is funny no? "
     "The formula "
     (!math
      (!concat
       (frac "1" "2")
       " + "
       (frac "1" "3")
       " + "
       (frac "1" "4")
       " + "
       (frac "1" "5")
       " + "
       (frac "1" "6")
       " + "
       (frac "1" "7")))
     " is funny no? "
     "The formula "
     (!math
      (!concat
       (frac "1" "2")
       " + "
       (frac "1" "3")
       " + "
       (frac "1" "4")
       " + "
       (frac "1" "5")
       " + "
       (frac "1" "6")
       " + "
       (frac "1" "7")))
      " is funny no? ")))

(define (test-tex-document)
  (list '!file
        tex-expression
        (list "article" "vdh")
        "french"
        "~"))

(define (out)
  (let ((s (serialize-latex (test-tex-document))))
    ;;(display s)
    ;;(display "\n")
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define text-document
  '(document
     (make-title
       (concat
         (title "Een testje")
         (author "Joris van der Hoeven")))
     (assign
       "test"
       (macro "x" "y"
         (with "font-size" "1.4" "font-series" "bold"
               (concat (arg "y") " and " (arg "x")))))
     (theorem
       (document
         "Hopsakee"
         (equation*
           (concat "<alpha>" (rsup "2")
                   "+<beta>" (rsup "2")
                   "=<gamma>" (rsup "2")))
         "Bla bla bla"
         (eqnarray*
          (table
           (row "a" "=" "b")
           (row "c" "=" "d")))
         "Bla bla bla"
         (equation
          (concat
           "<Delta>="
           (choice
            (tformat
             (cwith "1" "-1" "1" "2" "cell-halign" "l")
             (cwith "1" "-1" "3" "4" "cell-halign" "r")
             (table
              (row "a" "b" "c" "d")
              (row "e" "f" "g" "h"))))))
         "Bla bla bla"))
     (concat
       "Hopsakee, eens even kijken hoe {#~$dit$~#} er uit ziet. Hopsakee, eens even kijken hoe dit er uit ziet. Hopsakee, eens even kijken hoe dit er uit ziet. "
       (with "font-series" "bold" "Hopsakee")
       " "
       (cite "Joris" "Piet")
       (vspace "1.5fn"))
     (example
       (document
         (label "Hallo")
         (enumerate-numeric
           (document
             (concat (item) "Eerste punt. " (verbatim "Holala") " en hopsa")
               (verbatim
                 (document
                   "Hopsakee"
                   "  hola hop"
                   "  geintje"))
               "En weer verder"
               (concat (item* "hop") "Tweede punt.")
               (concat (item) "Derde punt.")))))
     (with "mode" "math"
       (concat
         (big "sum")
         (with "mode" "text" "Hallo")
         "+"
         (wide "a+b" "^")
         "+"
         (wide "<alpha>" "~")
         "+"
         (wide (wide "x" "^") "^")
         "+12*x+cos x+"
         (left "[")
         (frac "1" "2")
         "+k"
         (rsub (sqrt "3"))
         (right "]")
         "+<alpha>+"
         (sqrt "5" "n")
         (neg "<less>")
         "a+b"
         (rsub
           (tabular
             (table
               (row "a" "b")
               (row "c" "d"))))
         "+c"
         (rprime "<dag><dag>")))
     "Hopsakee"
     (input "1]" "x := a & b & c")
     (surround
       (with "color" "red" "Theorem 1. ")
         (Box)
         "Hopsakee, eens even kijken hoe dit er uit ziet. Hopsakee, eens even kijken hoe dit er uit ziet. Hopsakee, eens even kijken hoe dit er uit ziet.")
     (float "float" "tbh" (document (big-figure "Test" "Een plaatje.")))
     "Voor meer informatie, schrijf aan <less>vdhoeven@texmacs.org<gtr>"))

(define (pre)
  (tmtex-initialize)
  (tmpre-produce text-document))

(define (tex)
  (tmtex-initialize)
  (tmtex (tmpre-produce text-document)))

(tm-define (test)
  (serialize-latex (texmacs->latex text-document '())))
