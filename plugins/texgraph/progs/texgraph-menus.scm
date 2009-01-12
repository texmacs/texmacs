
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texgraph-menus.scm
;; DESCRIPTION : TeXgraph menus
;; BY	       : Emmanuel Corcelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : giac-menus.scm
;; COPYRIGHT   : (C) 1999  Bernard Parisse and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texgraph-menus))

(lazy-menu (2d-menu) 2d-menu)
(lazy-menu (3d-menu) 3d-menu)

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
  (->"Aide"
	;; FIXME : Problem when TeXgraph isn't installed by user in the default place
	;; the help files should be looked for in a generic $TEXGRAPH_PATH/doc
	("Aide (TeXgraph.pdf)" (shell "texdoc /usr/local/share/TeXgraph/doc/TeXgraph.pdf"))
	;("Aide (html)" (shell "texmacs /usr/local/share/TeXgraph/doc/html/aide.html &"))
  )

  (->"Fenetre graphique et marges"
	("Marges" 
	  (texgraph-insert "Marges(0.25,0.25,0.25,0.25), "))
	("Fenetre graphique" 
	  (texgraph-insert "Fenetre(-5+5*i,5-5*i,1+i), "))
	("Taille de la figure" 
	  (texgraph-insert "size({largeur=}8+i*{hauteur=}8,{ratio x/y=}1), "))
	---
	("Centrer la vue en un point" 
	  (texgraph-insert "centerView(A), "))
	("Restreindre a une courbe fermee (clipper)" 
	  (texgraph-insert "Clip([A,B,C,..]), "))
	("Bordure" 
	  (texgraph-insert "Border(1{ou0}), "))
  )

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
		("Eclaircir une couleur" (texgraph-insert "Color:=Light(couleur,{facteur=}0.5), "))
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
		("Eclaircir une couleur" (texgraph-insert "HideColor:=Light(couleur,{facteur=}0.5), "))
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
	("Angle des etiquettes" (texgraph-insert "LabelAngle:=45{en_degre}, "))
  )
  ---
  (group "Geometrie dans le plan")
  (link 2d-menu)
  
  ---
  (group "Geometrie dans l'espace")
  (link 3d-menu)
)



