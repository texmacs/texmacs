
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-drd.scm
;; DESCRIPTION : Formal specification of the part of LaTeX
;;               which is understood by TeXmacs
;; COPYRIGHT   : (C) 1999-2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-drd)
  (:use (convert latex latex-overload)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Order in which packages should be included
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-package-priority%
  ("geometry" 10)
  ("amsmath" 20)
  ("amssymb" 30)
  ("graphicx" 40)
  ("wasysym" 50)
  ("stmaryrd" 60)
  ("textcomp" 60)
  ("enumerate" 70)
  ("epsfig" 80)
  ("mathrsfs" 90)
  ("bbm" 100)
  ("dsfont" 110)
  ("euscript" 120)
  ("multicol" 130)
  ("hyperref" 140)
  ("mathtools" 150)
  ("cleveref" 160))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies between style files and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-depends%
  ("amsart" "amstex")
  ("amstex" "amsmath")
  ("amstex" "amsthm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies of commands on packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-needs%
  (!verbatim "alltt")
  (!verbatim* "alltt")
  (begin-alltt "alltt")

  (begin-tabularx "tabularx")

  (geometry "geometry")
  (epsfig "epsfig")
  (includegraphics "graphicx")
  (rotatebox "graphicx")
  (scalebox "graphicx")
  (reflectbox "graphicx")
  (adjustbox "adjustbox")

  (mathscr "mathrsfs")
  (EuScript "euscript")
  (mathbbm "bbm")
  (mathbbmss "bbm")
  (mathds "dsfont")
  (mathfrak "amssymb")
  (mathbb "amssymb")
  (theorembodyfont "theorem")
  (substack "mathtools")

  (begin-align "amsmath")
  (begin-align* "amsmath")
  (begin-alignat "amsmath")
  (begin-alignat* "amsmath")
  (begin-xalignat "amsmath")
  (begin-xxalignat "amsmath")
  (begin-flalign "amsmath")
  (begin-flalign* "amsmath")
  (begin-gather "amsmath")
  (begin-gather* "amsmath")
  (begin-multline "amsmath")
  (begin-multline* "amsmath")
  (begin-split "amsmath")

  (text "amsmath")
  (binom "amsmath")
  (dbinom "amsmath")
  (tbinom "amsmath")
  (dddot "amsmath")
  (ddddot "amsmath")
  (genfrac "amsmath")
  (mod "amsmath")
  (pod "amsmath")
  (overset "amsmath")
  (underset "amsmath")
  (operatorname "amsmath")
  (boldsymbol "amsmath")
  (overleftrightarrow "amsmath")
  (underleftarrow "amsmath")
  (underrightarrow "amsmath")
  (underleftrightarrow "amsmath")

  (underaccent "accents")
  (ring "accents")
  
  (ifthenelse "ifthen")
  (captionof "capt-of")
  (widthof "calc")
  
  (color     "xcolor")
  (fcolorbox "xcolor")
  (textcolor "xcolor")

  (euro "eurosym")

  (mdfsetup ("tikz" "mdframed"))
  (tikz "tikz")

  (omicron "pslatex")
  (multicols "multicol")
  (bundle "epic")
  (chunk "epic")
  (bundle "ecltree")
  (chunk "ecltree")

  (url "hyperref")
  (href "hyperref")
  (hyperref "hyperref")

  (cref "cleveref")
  (Cref "cleveref")
  
  (citet "natbib")
  (citep "natbib")
  (citet* "natbib")
  (citep* "natbib")
  (citealt "natbib")
  (citealp "natbib")
  (citealt* "natbib")
  (citealp* "natbib")
  (citetext "natbib")
  (citeauthor "natbib")
  (citeauthor* "natbib")
  (citeyear "natbib")

  (index "makeidx")
  (printindex "makeidx")

  (inparaenum "paralist")

  (listpart "expdlist")

  (ifthispageodd "scrextend")

  (begin-linenumbers "lineno")
  (resetlinenumber "lineno"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-paper-opts%
  ("page-top"         "top")
  ("page-bot"         "bottom")
  ("page-odd"         "left")
  ("page-even"        "left")
  ("page-right"       "right")
  ("page-height"      "paperheight")
  ("page-width"       "paperwidth")
  ("page-type"        "page-type")
  ("page-orientation" "page-orientation"))

(logic-table latex-paper-type%
  ("a0" "a0paper")
  ("a1" "a1paper")
  ("a2" "a2paper")
  ("a3" "a3paper")
  ("a4" "a4paper")
  ("a5" "a5paper")
  ("a6" "a6paper")
  ("a7" "papersize={74mm,105mm}")
  ("a8" "papersize={52mm,74mm")
  ("a9" "papersize={37mm,52mm}")
  ("b0" "b0paper")
  ("b1" "b1paper")
  ("b2" "b2paper")
  ("b3" "b3paper")
  ("b4" "b4paper")
  ("b5" "b5paper")
  ("b6" "b6paper")
  ("b7" "papersize={88mm,125mm}")
  ("b8" "papersize={62mm,88mm}")
  ("b9" "papersize={44mm,62mm}")
  ("legal" "legalpaper")
  ("letter" "letterpaper")
  ("executive" "executivepaper")
  ("archA" "papersize={9in,12in}")
  ("archB" "papersize={12in,18in}")
  ("archC" "papersize={18in,24in}")
  ("archD" "papersize={24in,36in}")
  ("archE" "papersize={36in,48in}")
  ("10x14" "papersize={10in,14in}")
  ("11x17" "papersize={11in,17in}")
  ("C5" "papersize={162mm,229mm}")
  ("Comm10" "papersize={297pt,684pt}")
  ("DL" "papersize={110mm,220mm}")
  ("halfletter" "papersize={140mm,216mm}")
  ("halfexecutive" "papersize={133mm,184mm}")
  ("ledger" "papersize={432mm,279mm}")
  ("Monarch" "papersize={98mm,190mm}")
  ("csheet" "papersize={432mm,559mm}")
  ("dsheet" "papersize={559mm,864mm}")
  ("esheet" "papersize={864mm,1118mm}")
  ("flsa" "papersize={216mm,330mm}")
  ("flse" "papersize={216mm,330mm}")
  ("folio" "papersize={216mm,330mm}")
  ("lecture note" "papersize={15.5cm,23.5cm}")
  ("note" "papersize={216mm,279mm}")
  ("quarto" "papersize={215mm,275mm}")
  ("statement" "papersize={140mm,216mm}")
  ("tabloid" "papersize={279mm,432mm}"))

;; cpp interface with reversed access

(tm-define (latex-paper-opts s)
  (with r (query `(latex-paper-opts% 'x ,s))
    (if (nnull? r) (cdaar r) "undefined")))

(tm-define (latex-paper-type s)
  (with r (query `(latex-paper-type% 'x ,s))
    (if (nnull? r) (cdaar r) "undefined")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for consulting the database (might become deprecated)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-resolve s)
  (define (safe-string2symbol s)
    (if (== s "") (string->symbol " ") (string->symbol s)))

  (if (string-starts? s "\\")
      (set! s (substring s 1 (string-length s))))

  (with arity (logic-ref latex-arity% (safe-string2symbol s))
    (if (logic-in? (safe-string2symbol s) latex-optional-arg%)
        (set! arity (- -1 arity)))
    (if (string-starts? s "end-")
        (begin
          (set! s (string-append "begin-" (substring s 4 (string-length s))))
          (set! arity 0)))
    (values (safe-string2symbol s) arity)))

(tm-define (latex-arity tag)
  "Get the arity of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (or arity 0)))

(tm-define (latex-type tag)
  "Get the type of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (cond ((not arity) "undefined")
          ((logic-in? s latex-command%) "command")
          ((logic-in? s latex-length%) "length")
          ((logic-in? s latex-ignore%) "ignore")
          ((logic-in? s latex-as-pic%) "as-picture")
          ((logic-in? s latex-name%) "name")
          ((logic-in? s latex-counter%) "counter")
          ((logic-in? s latex-modifier%) "modifier")
          ((logic-in? s latex-control%) "control")
          ((logic-in? s latex-operator%) "operator")
          ((logic-in? s latex-list%) "list")
          ((logic-in? s latex-math-environment%) "math-environment")
          ((logic-in? s latex-enunciation%) "enunciation")
          ((logic-in? s latex-environment%) "environment")
          ((logic-in? s latex-texmacs%) "texmacs")
          ((logic-in? s latex-symbol%) "symbol")
          ((logic-in? s latex-big-symbol%) "big-symbol")
          (else "undefined"))))
