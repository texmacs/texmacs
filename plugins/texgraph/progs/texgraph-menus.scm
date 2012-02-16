
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texgraph-menus.scm
;; DESCRIPTION : TeXgraph menus
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : giac-menus.scm
;; COPYRIGHT   : (C) 1999  Bernard Parisse and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texgraph-menus))

(lazy-menu (texgraph-2d-menu) texgraph-2d-menu)
(lazy-menu (texgraph-3d-menu) texgraph-3d-menu)
(lazy-menu (texgraph-figures-menu) texgraph-figures-menu)
(lazy-menu (texgraph-scripts-menu) texgraph-scripts-plot-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert TeXgraph primitive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texgraph-cursor-pos l)
  (cond ((null? l) 0)
	((null? (cdr l)) 1)
	((and (== (car l) #\() (== (cadr l) #\))) 1)
	((and (== (car l) #\() (== (cadr l) #\,)) 1)
	((and (== (car l) #\,) (== (cadr l) #\))) 1)
	((and (== (car l) #\,) (== (cadr l) #\,)) 1)
	(else (+ (texgraph-cursor-pos (cdr l)) 1))))

(define (texgraph-insert s)
  (insert-go-to s (list (texgraph-cursor-pos (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXgraph menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texgraph-functions-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      (link texgraph-scripts-plot-menu)
      ---)

  (->"Aide"
;	;; FIXME : Problem when TeXgraph isn't installed by user in the default place
;	;; the help files should be looked for in a generic $TEXGRAPH_PATH/doc
	("Aide TeXgraph.pdf" (system "texdoc /usr/local/share/TeXgraph/doc/TeXgraph &"))
	("Aide TeXgraph.html (en ligne)" (system "xdg-open http://texgraph.tuxfamily.org/aide/TeXgraph.html &"))

 )

  (->"Fenetre graphique, marges et export"
	("Marges" 
	  (texgraph-insert "Marges(0.25,0.25,0.25,0.25), "))
	("Fentre graphique" 
	  (texgraph-insert "Fenetre(-5+5*i,5-5*i,1+i), "))
	("Taille de la figure" 
	  (texgraph-insert "size(<{largeur=}>8+i*<{hauteur=}>8,<{ratio x/y=}>1), "))
	---
	("Centrer la vue en un point" 
	  (texgraph-insert "centerView(A), "))
	("Restreindre a une courbe fermee (clipper)" 
	  (texgraph-insert "Clip([A,B,C,..]), "))
	("Bordure" 
	  (texgraph-insert "Border(1<{ou0}>), "))
	---
	(group "Export")
	(group "Seuls les 2 premiers cas donnent un affichage ecran correct")
	("epsc - Export avec compilation des formules LaTeX - Par defaut" 
	  (texgraph-insert "{export=epsc} "))
	("eps - Export sans compilation des formules LaTeX" 
	  (texgraph-insert "{export=eps} "))
	("pdfc puis eps - Export de la transparence avec compilation des formules LaTeX" 
	  (texgraph-insert "{export=pdfc} "))
	("pdf puis eps - Export de la transparence sans compilation des formules LaTeX" 
	  (texgraph-insert "{export=pdf} "))
  )


  (link texgraph-figures-menu)

  ---
  (group "Styles et couleurs")
  (->"Style de point"
	("Point" (texgraph-insert "DotStyle:=dot, "))
	("Gros Point" (texgraph-insert "DotStyle:=bigdot, "))
	("Croix" (texgraph-insert "DotStyle:=cross, "))
  )

  (->"Proprietes des lignes"
	(->"Epaisseur des lignes"
		("Ligne fine (2 pt)" (texgraph-insert "Width:=thinlines, "))
		("Ligne epaisse (8 pt)" (texgraph-insert "Width:=thicklines, "))
		("Ligne tres epaisse (14 pt)" (texgraph-insert "Width:=Thicklines, "))
		---
		("Autre" (texgraph-insert "Width:=, "))
	)
	(->"Style des lignes"
		("Ligne non tracee" (texgraph-insert "LineStyle:=noline, "))
		("------------------" (texgraph-insert "LineStyle:=solid, "))
		("---- ---- ---- --" (texgraph-insert "LineStyle:=dashed, "))
		(". . . . . . . ." (texgraph-insert "LineStyle:=dotted, "))
	)
	(->"Fleches"
		(" ------------------" (texgraph-insert "Arrows:=0, "))
		(" ------------------>" (texgraph-insert "Arrows:=1, "))
		("<---------------->" (texgraph-insert "Arrows:=2, "))
	)
	---
	(->"Color"
		("Black" (texgraph-insert "Color:=black, "))
		("White" (texgraph-insert "Color:=white, "))
		("Grey" (texgraph-insert "Color:=gray, "))
		("Red" (texgraph-insert "Color:=red, "))
		("Blue" (texgraph-insert "Color:=blue, "))
		("Yellow" (texgraph-insert "Color:=yellow, "))
		("Green" (texgraph-insert "Color:=green, "))
		("Orange" (texgraph-insert "Color:=orange, "))
		("Violet" (texgraph-insert "Color:=violet, "))
		("Brun" (texgraph-insert "Color:=brown, "))
		("Pink" (texgraph-insert "Color:=pink, "))
		---
		("Eclaircir une couleur" (texgraph-insert "Color:=Light(couleur,<{facteur=}>0.5), "))
		("Autre" (texgraph-insert "Color:=, "))
	)
  )

  (->"Proprietes du remplissage"
	(->"Style de remplissage"
		("Pas de remplissage" 
		  (texgraph-insert "FillStyle:=none, "))
		("Remplir avec la couleur de FillColor" 
		  (texgraph-insert "FillStyle:=full, "))
		---
		("Hachures orientees SO-NE" 
		  (texgraph-insert "FillStyle:=bdiag, "))
		("Hachures orientees NO-SE" 
		  (texgraph-insert "FillStyle:=fdiag, "))
		("Hachures diagonales doubles" 
		  (texgraph-insert "FillStyle:=diagcross, "))
		---
		("Hachures horizontales" 
		  (texgraph-insert "FillStyle:=horizontal, "))
		("Hachures verticales" 
		  (texgraph-insert "FillStyle:=vertical, "))
		("Hachures horizontales et verticales" 
		  (texgraph-insert "FillStyle:=invcross, "))
	)
	(->"Couleur de remplissage"
		("Black" (texgraph-insert "FillColor:=black, "))
		("White" (texgraph-insert "FillColor:=white, "))
		("Grey" (texgraph-insert "FillColor:=gray, "))
		("Red" (texgraph-insert "FillColor:=red, "))
		("Blue" (texgraph-insert "FillColor:=blue, "))
		("Yellow" (texgraph-insert "FillColor:=yellow, "))
		("Green" (texgraph-insert "FillColor:=green, "))
		("Orange" (texgraph-insert "FillColor:=orange, "))
		("Violet" (texgraph-insert "FillColor:=violet, "))
		("Brun" (texgraph-insert "FillColor:=brown, "))
		("Pink" (texgraph-insert "FillColor:=pink, "))
		---
		("Autre" (texgraph-insert "FillColor:=, "))
	)
	("Opacite" (texgraph-insert "FillOpacity:=0.5, "))
	---
	("Couleur du fond" (texgraph-insert "background(fillstyle,fillcolor), "))
  )

  (->"Proprietes des aretes cachees (3D)"
	(->"Epaisseur des aretes"
		("Ligne fine (2 pt)" (texgraph-insert "HideWidth:=thinlines, "))
		("Ligne epaisse (8 pt)" (texgraph-insert "HideWidth:=thicklines, "))
		("Ligne tres epaisse (14 pt)" (texgraph-insert "HideWidth:=Thicklines, "))
		---
		("Autre" (texgraph-insert "HideWidth:=, "))
	)
	(->"Style des lignes"
		("Ligne non tracee" (texgraph-insert "HideStyle:=noline, "))
		("------------------" (texgraph-insert "HideStyle:=solid, "))
		("---- ---- ---- --" (texgraph-insert "HideStyle:=dashed, "))
		(". . . . . . . ." (texgraph-insert "HideStyle:=dotted, "))
	)
	---
	(->"Color"
		("Black" (texgraph-insert "HideColor:=black, "))
		("White" (texgraph-insert "HideColor:=white, "))
		("Grey" (texgraph-insert "HideColor:=gray, "))
		("Red" (texgraph-insert "HideColor:=red, "))
		("Blue" (texgraph-insert "HideColor:=blue, "))
		("Yellow" (texgraph-insert "HideColor:=yellow, "))
		("Green" (texgraph-insert "HideColor:=green, "))
		("Orange" (texgraph-insert "HideColor:=orange, "))
		("Violet" (texgraph-insert "HideColor:=violet, "))
		("Brun" (texgraph-insert "HideColor:=brown, "))
		("Pink" (texgraph-insert "HideColor:=pink, "))
		---
		("Eclaircir une couleur" (texgraph-insert "HideColor:=Light(couleur,<{facteur=}>0.5), "))
		("Autre" (texgraph-insert "HideColor:=, "))
	)
  )

  (->"Proprietes des etiquettes"
	(->"Taille des etiquettes"
		("Tiny" (texgraph-insert "LabelSize:=tiny, "))
		("Script" (texgraph-insert "LabelSize:=scriptsize, "))
		("Note" (texgraph-insert "LabelSize:=footnotesize, "))
		("Small" (texgraph-insert "LabelSize:=smal, "))
		("Normal" (texgraph-insert "LabelSize:=normalsize, "))
		("large" (texgraph-insert "LabelSize:=large, "))
		("Large" (texgraph-insert "LabelSize:=Large, "))
		("Tres Grand" (texgraph-insert "LabelSize:=LARGE, "))
		("huge" (texgraph-insert "LabelSize:=huge, "))
		("Huge" (texgraph-insert "LabelSize:=Huge, "))
	)
	(->"Style des etiquettes"
		("Texte centre encadre" (texgraph-insert "LabelStyle:=framed, "))
		("Texte centre avec sauts (\\\\)" (texgraph-insert "LabelStyle:=stacked, "))
		---
		("Texte aligne a gauche" (texgraph-insert "LabelStyle:=left, "))
		("Texte aligne a droite" (texgraph-insert "LabelStyle:=right, "))
		("Texte aligne en haut" (texgraph-insert "LabelStyle:=top, "))
		("Texte aligne en bas" (texgraph-insert "LabelStyle:=bottom, "))
	)
	---
	("Angle des etiquettes" (texgraph-insert "LabelAngle:=45<{en_degre}>, "))
  )
  ---
  (group "Geometrie dans le plan")
  (link texgraph-2d-menu)
  
  ---
  (group "Geometrie dans l'espace")
  (link texgraph-3d-menu)
)

(menu-bind plugin-menu
  (:require (or (in-texgraph?) (and (not-in-session?) (texgraph-scripts?))))
  (=> "TeXgraph" (link texgraph-functions-menu)))
