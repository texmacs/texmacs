
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-drd.scm
;; DESCRIPTION : Formal specification of the part of LaTeX
;;               which is understood by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-drd)
  (:use (convert latex latex-symbol-drd)
	(convert latex latex-texmacs-drd)))

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
  ("enumerate" 70)
  ("epsfig" 80)
  ("mathrsfs" 90)
  ("bbm" 100)
  ("dsfont" 110)
  ("euscript" 120)
  ("multicol" 130)
  ("hyperref" 140))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies between style files and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-depends%
  ("amsart" "amsmath")
  ("amsart" "amsthm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies of commands on packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-needs%
  (!verbatim "alltt")
  (!verbatim* "alltt")
  ("alltt" "alltt")

  (geometry "geometry")
  (epsfig "epsfig")
  (includegraphics "graphicx")

  (mathscr "mathrsfs")
  (EuScript "euscript")
  (mathbbm "bbm")
  (mathbbmss "bbm")
  (mathds "dsfont")
  (mathfrak "amssymb")
  (mathbb "amssymb")
  (theorembodyfont "theorem")

  (leadsto "leadsto")
  (nleadsto "leadsto")
  (Diamond "amssymb")
  (align "amsmath")
  (align* "amsmath")
  (alignat "amsmath")
  (alignat* "amsmath")
  (equation "amsmath")
  (equation* "amsmath")
  (flalign "amsmath")
  (flalign* "amsmath")
  (gather "amsmath")
  (gather* "amsmath")
  (multline "amsmath")
  (multline* "amsmath")
  (split "amsmath")
  (text "amsmath")
  (binom "amsmath")
  (dbinom "amsmath")
  (tbinom "amsmath")
  (dddot "amsmath")
  (ddddot "amsmath")
  (ontop "amsmath")
  (mod "amsmath")
  (pod "amsmath")
  (overset "amsmath")
  (underset "amsmath")
  (operatorname "amsmath")
  (boldsymbol "amsmath")
  (lleq "amsmath")
  (llleq "amsmath")
  (ggeq "amsmath")
  (gggeq "amsmath")
  (qed "amsmath")

  (xminus "amsmath")
  (xleftarrow "amsmath")
  (xrightarrow "amsmath")
  (xleftrightarrow "amsmath")
  (xmapsto "amsmath")
  (xmapsfrom "amsmath")
  (xequal "amsmath")
  (xLeftarrow "amsmath")
  (xRightarrow "amsmath")
  (xLeftrightarrow "amsmath")

  ;;(proof "amsthm")

  (bigbox "stmaryrd")
  (bigcurlyvee "stmaryrd")
  (bigcurlywedge "stmaryrd")
  (biginterleave "stmaryrd")
  (bignplus "stmaryrd")
  (bigparallel "stmaryrd")
  (bigsqcap "stmaryrd")

  (btimes "graphicx")
  (Backepsilon "graphicx")
  (Mho "graphicx")
  (mho "graphicx")
  (upequal "graphicx")

  (ifthenelse "ifthen")
  (captionof "capt-of")
  (widthof "calc")
  
  (color "xcolor")

  (mdfsetup ("tikz" "mdframed"))

  (omicron "pslatex")
  (multicols "multicol")
  (bundle "epic")
  (chunk "epic")
  (bundle "ecltree")
  (chunk "ecltree")

  (url "hyperref")
  (href "hyperref")

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

  (inparaenum "paralist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands which are provided in certain packages or style files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-provides%
  (begin-proof "amsthm")
  (qed "amsthm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for consulting the database (might become deprecated)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-resolve s)
  (if (string-starts? s "\\")
      (set! s (substring s 1 (string-length s))))
  (with arity (logic-ref latex-arity% (string->symbol s))
    (if (logic-in? (string->symbol s) latex-optional-arg%)
	(set! arity (- -1 arity)))
    (if (string-starts? s "end-")
	(begin
	  (set! s (string-append "begin-" (substring s 4 (string-length s))))
	  (set! arity 0)))
    (values (string->symbol s) arity)))

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
