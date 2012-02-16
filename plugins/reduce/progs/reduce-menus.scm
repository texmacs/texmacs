
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : reduce-menus.scm
;; DESCRIPTION : Reduce menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (reduce-menus)
  (:use (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind reduce-help-menu
  ("Abstract" (load-buffer "$reduce/doc/manual/abstract.tex"))
  ("Acknowledgement" (load-buffer "$reduce/doc/manual/acknowl.tex"))
  ("Introduction" (load-buffer "$reduce/doc/manual/intro.tex"))
  ---
  (-> "Programming"
      ("Structure of programs"
       (load-buffer "$reduce/doc/manual/progstr.tex"))
      ("Expressions"
       (load-buffer "$reduce/doc/manual/exprn.tex"))
      ("Lists"
       (load-buffer "$reduce/doc/manual/list.tex"))
      ("Statements"
       (load-buffer "$reduce/doc/manual/statemnt.tex"))
      ("Commands and declarations"
       (load-buffer "$reduce/doc/manual/command.tex"))
      ("Procedures"
       (load-buffer "$reduce/doc/manual/proc.tex"))
      ("The remember statement"
       (load-buffer "$reduce/doc/manual/rememb.tex")))
  (-> "Operators"
      ("Built-in prefix operators"
       (load-buffer "$reduce/doc/manual/oper.tex"))
      ("Operators with special properties"
       (load-buffer "$reduce/doc/manual/oper2.tex"))
      ("Map operator"
       (load-buffer "$reduce/doc/manual/map.tex"))
      ("Substitutions"
       (load-buffer "$reduce/doc/manual/subst.tex"))
      ("Solve operator"
       (load-buffer "$reduce/doc/manual/solve.tex"))
      ("Root val operator"
       (load-buffer "$reduce/doc/manual/rest.tex")))
  (-> "Data types"
      ;;("conversions"
      ;;(load-buffer "$reduce/doc/manual/convert.tex"))
      ("Polynomials and rational functions"
       (load-buffer "$reduce/doc/manual/polyrat.tex"))
      ("Matrix calculations"
       (load-buffer "$reduce/doc/manual/matrix.tex")))
  (-> "Using reduce"
      ("Structure of expressions"
       (load-buffer "$reduce/doc/manual/structr.tex"))
      ("File handling"
       (load-buffer "$reduce/doc/manual/io.tex"))
      ("Interactive use"
       (load-buffer "$reduce/doc/manual/inter.tex"))
      ("Symbolic mode"
       (load-buffer "$reduce/doc/manual/symbolic.tex"))
      ("Rlisp '88"
       (load-buffer "$reduce/doc/manual/rlisp88.tex"))
      ("Reduce and rlisp utilities"
       (load-buffer "$reduce/doc/manual/util.tex")))
  (-> "Miscellaneous"
      ("Algebraic properties"
       (load-buffer "$reduce/doc/manual/aprop.tex"))
      ("Continued fractions"
       (load-buffer "$reduce/doc/manual/cfrac.tex"))
      ("Heuristic g.c.d."
       (load-buffer "$reduce/doc/manual/heugcd.tex"))
      ("High energy physics"
       (load-buffer "$reduce/doc/manual/hephys.tex")))
  ---
  ("Maintaining reduce"
   (load-buffer "$reduce/doc/manual/maintain.tex"))
  ("Reserved identifiers"
   (load-buffer "$reduce/doc/manual/appenda.tex")))

(menu-bind session-help-icons
  (:require (and (in-reduce?)
                 (url-exists? "$reduce/doc/manual/abstract.tex")))
  /
  (=> (balloon (icon "tm_help.xpm") "Reduce documentation")
      (link reduce-help-menu)))
