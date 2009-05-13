
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texgraph-scripts-edit.scm
;; DESCRIPTION : routines for on-the-fly evaluation of TeXgraph scripts
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : scripts-edit.scm
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texgraph-scripts-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils plugins plugin-cmd)
	(convert tools tmconcat)
	(dynamic scripts-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(tm-define (texgraph-script-plot-command lan t)
  (cond ((== (car t) 'texgraph-plot-curve) 
	 `(concat "Echellex:=" ,(tm-ref t 14) "+0*i, if Echellex=0 then Echellex:=1 fi, ~
		Echelley:=" ,(tm-ref t 15) "+0*i, if Echelley=0 then Echelley:=1 fi, ~
		xmini:=" ,(tm-ref t 16) "+0*i, if xmini=0 then xmini:=-5 fi, ~
		ymaxi:=" ,(tm-ref t 19) "+0*i, if ymaxi=0 then ymaxi:=5 fi, ~
		xmaxi:=" ,(tm-ref t 17) "+0*i, if xmaxi=0 then xmaxi:=5 fi, ~
		ymini:=" ,(tm-ref t 18) "+0*i, if ymini=0 then ymini:=-5 fi, ~
		Fenetre(xmini+ymaxi*i,xmaxi+ymini*i,Echellex+i*Echelley), ~	
		Gradx:=" ,(tm-ref t 7) "+0*i, if Gradx=0 then Gradx:=1 fi, ~
		Grady:=" ,(tm-ref t 8) "+0*i, if Grady=0 then Grady:=1 fi, ~
		Origine:=" ,(tm-ref t 9) "+0*i, ~
		Color:=gray, " ,(tm-ref t 12) " ~
		if StrComp(\"" ,(tm-ref t 11) "\",\"non\")=0 
		then Grille(Origine,Gradx+Grady*i)
		fi, ~
		if StrComp(\"" ,(tm-ref t 6) "\",\"non\")=0 then 
			Color:=black, Width:=4, LineStyle:=solid, Arrows:=1, " ,(tm-ref t 10) " ~
			Axes(Origine,Gradx+Grady*i), Arrows:=0 
		fi, ~
		tMin:=xmini, tMax:=xmaxi,
		Width:=6, Color:=blue, LineStyle:=solid, " ,(tm-ref t 1) " ~
		NewMac(\"f\", \"",(tm-ref t 0)"\",x), Courbe(t+i*f(t)), ~
 		Width:=6, Color:=red, LineStyle:=solid, " ,(tm-ref t 3) " ~
		NewMac(\"g\", \"",(tm-ref t 2)"\",x), Courbe(t+i*g(t)), ~
		Width:=6, Color:=green, LineStyle:=solid, " ,(tm-ref t 5) " ~
		NewMac(\"h\", \"",(tm-ref t 4)"\",x), Courbe(t+i*h(t)), ~
		Color:=black, LineStyle:=solid, Width:=6, ~
		" ,(tm-ref t 20) " ~ 
		Taille:=" ,(tm-ref t 13) "+0*i, size(Taille), ~"))

	((== (car t) 'texgraph-plot-curve*)
	 `(concat "set samples 1000 ~ "
		  "set parametric ~ "
		  "set trange [" ,(tm-ref t 2) ":" ,(tm-ref t 3) "] ~ "
		  "plot " ,(tm-ref t 0) ", " ,(tm-ref t 1)))
	((== (car t) 'plot-surface)
	 `(concat "set samples 50 ~ set isosamples 50 ~ set hidden3d ~"
		  "set pm3d ~ "
		  "set xrange [" ,(tm-ref t 1) ":" ,(tm-ref t 2) "] ~ "
		  "set yrange [" ,(tm-ref t 3) ":" ,(tm-ref t 4) "] ~ "
		  "splot " ,(tm-ref t 0)))
	((== (car t) 'plot-surface*)
	 `(concat "set samples 50 ~ set isosamples 50 ~ set hidden3d ~"
		  "set parametric ~ "
		  "set pm3d ~ "
		  "set urange [" ,(tm-ref t 3) ":" ,(tm-ref t 4) "] ~ "
		  "set vrange [" ,(tm-ref t 5) ":" ,(tm-ref t 6) "] ~ "
		  "splot " ,(tm-ref t 0)
		  ", " ,(tm-ref t 1)
		  ", " ,(tm-ref t 2)))))

(define (texgraph-activate-plot)
  (with-innermost t '(texgraph-plot-curve texgraph-plot-curve* plot-surface plot-surface*)
    (let* ((lan "texgraph")
	   (session "default")
	   (in (texgraph-script-plot-command lan (tree->stree t))))
      (tree-set! t `(plot-output ,t ""))
      (script-eval-at (tree-ref t 1) lan session in :math-correct :math-input)
      (tree-go-to t 1 :end))))

(tm-define (kbd-return)
  (:inside texgraph-plot-curve texgraph-plot-curve* plot-surface plot-surface*)
  (with-innermost t '(texgraph-plot-curve texgraph-plot-curve* plot-surface plot-surface*)
    (if (= (tree-down-index t) (- (tree-arity t) 1))
	(texgraph-activate-plot)
	(tree-go-to t (1+ (tree-down-index t)) :end))))

(tm-define (hidden-variant)
  (:inside texgraph-plot-curve texgraph-plot-curve* plot-surface plot-surface*)
  (texgraph-activate-plot))

(tm-define (hidden-variant)
  (:inside plot-output)
  (with-innermost t 'plot-output
    (tree-remove-node! t 0)
    (tree-go-to t 0 :end)))

