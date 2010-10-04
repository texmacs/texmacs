
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-markup.scm
;; DESCRIPTION : extra mathematical markup
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-markup))

(tm-define (math-check t)
  (:secure #t)
  (if (packrat-correct? "std-math" "Main" t) t
      `(math-error ,t)))

(tm-define (math-check-table t)
  (:secure #t)
  ;;(display* "t= " t "\n")
  (cond ((tree-is? t 'tformat)
	 (with l (tree-children t)
	   `(tformat ,@(cDr l) ,(math-check-table (cAr l)))))
	((tree-in? t '(document table row cell))
	 `(,(tree-label t) ,@(map math-check-table (tree-children t))))
	((== t (tree "")) t)
	(else (math-check t))))
