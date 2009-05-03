
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 2d-menu.scm
;; DESCRIPTION : TeXgraph 2D Geometry menus
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (2d-menu))

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
;; 2D menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind 2d-menu
  (->"Axes et grille"
	("Grille" 
	  (texgraph-insert "Grille(0,1+i), "))
	("Axes fleches" 
	  (texgraph-insert "Arrows:=1, Axes(0,1+i), Arrows:=0, "))
	; add more macros for other kind of axes/grids (mm, log...)
	---
	("Droite horizontale graduee" 
	  (texgraph-insert "Arrows:=1, GradDroite([<{origin=}>0,0+i*1],[<{unite=}>2,<{pas=}>1],xyticks,<{subdiv=}>2,bottom,i,1,\"$a$\"), Arrows:=0, "))
	("Droite verticale graduee" 
	  (texgraph-insert "Arrows:=1, GradDroite([<{origin=}>0,0+i*1],[<{unite=}>i*2,<{pas=}>1],xyticks,<{subdiv=}>2,top,right,1,\"$a$\"), Arrows:=0, "))
  )

  (->"Points et lignes"
	("Affectations (A:=...)" 
	  (texgraph-insert "A:=1+2*i, "))
	---
	("Point(s)" 
	  (texgraph-insert "Point(A,B,C), "))
	("Point et etiquette associee" 
	  (texgraph-insert "LabelDot(A,\"$A$\",\"NO\",<{mark=}>1,<{dist=}>0.25), "))
	("Barycentre" 
	  (texgraph-insert "bary([A1,c1,A2,c2,A3,c3]), "))
	---
	("Segment [AB]" 
	  (texgraph-insert "Seg(A,B), "))
	("Droite (AB)" 
	  (texgraph-insert "Droite(A,B), "))
	("Droite a.x+b.y=c" 
	  (texgraph-insert "Droite(a,b,c), "))
	("Demi-droite [AB)" 
	  (texgraph-insert "Ddroite(A,B), "))
	---
	("Cercle (centre-rayon)" 
	  (texgraph-insert "Cercle(A,<{rayon=}>4), "))
	("Cercle (defini par 3 points)" 
	  (texgraph-insert "Cercle(A,B,C), "))
	("Arc de cercle" 
	  (texgraph-insert "Arc(B,A,C,<{rayon=}>3,<{sens=}>+1), "))
	("Ellipse" 
	  (texgraph-insert "ellipse(A,a,b,<{inclin_deg=}>0), "))
	;("Cercle (centre-rayon) avec Bezier" (texgraph-insert "cercle(A,rayon), "))
	("Arc d'ellipse" 
	  (texgraph-insert "ellipseArc(B,A,C,a,b,<{sens=}>+1,<{inclin_deg=}>0), "))
	---
	("Ligne polygonale" 
	  (texgraph-insert "Ligne([A,B,..],<{fermee=}>0,<{rayon=}>0), "))
	("Chemin (ligne, arc, arc d'ellipse, bezier)" 
	  (texgraph-insert "Path([A,B,line,M,N,P,<{sens=}>+1,arc,..],<{fermee=}>1), "))
	("Spline cubique" 
	  (texgraph-insert "Spline(v0,A0,v1,A1,..), "))
	("Courbe de Bezier" 
	  (texgraph-insert "Bezier([A,C1,C2,B]), "))
  )

  (->"Constructions (droites et polygones)"
	(->"Objets non dessines"
		("Parallele" 
		  (texgraph-insert "parallel([A,B],C), "))
		("Perpendiculaire" 
		  (texgraph-insert "perp([A,B],C,<{angle_droit=}>1), "))
		("Mediatrice" 
		  (texgraph-insert "med(A,B,<{angle_droit=}>1), "))
		("Bissectrice" 
		  (texgraph-insert "bissec(B,A,C,<{int=}>1), "))
		---
		("Carre" 
		  (texgraph-insert "carre(A,B,<{sens=}>+1), "))
		("Rectangle" 
		  (texgraph-insert "rectangle(A,B,C), "))
		("Parallelogramme" 
		  (texgraph-insert "parallelo(A,B,C), "))
		("Polygone regulier" 
		  (texgraph-insert "polyreg(A,S1,<{nb_cotes=}>5), "))
		---
		("Intersection de deux droites" 
		  (texgraph-insert "I:=Intersec(Droite(A,B),Droite(C,D)), I1:=I[1], I2:=I[2], Point(I), "))
		("Intersection droite-cercle (ou ligne-ligne)" 
		  (texgraph-insert "I:=Intersec(Droite(A,B),Cercle(C,<{rayon=}>4)),I1:=I[1], I2:=I[2], Point(I), "))
		("Intersection de deux lignes polygonales" 
		  (texgraph-insert "I:=Intersec(Ligne([A,B,C],1), Ligne([D,E,F],1)), Point(I), "))
		---
		("Liste des points d'un element graphique (sans le dessin)" "Get"
		  (texgraph-insert "Get(objet), "))
	)
	---
	(group "Objets dessines")
	("Parallele" 
	  (texgraph-insert "Dparallel([A,B],C), "))
	("Perpendiculaire" 
	  (texgraph-insert "Dperp([A,B],C,<{angle_droit=}>1), "))
	("Mediatrice" 
	  (texgraph-insert "Dmed(A,B,<{angle_droit=}>1), "))
	("Bissectrice" 
	  (texgraph-insert "Dbissec(B,A,C,<{int=}>1), "))
	---
	("Carre" 
	  (texgraph-insert "Dcarre(A,B,<{sens=}>+1), "))
	("Rectangle" 
	  (texgraph-insert "Drectangle(A,B,C), "))
	("Parallelogramme" 
	  (texgraph-insert "Dparallelo(A,B,C), "))
	("Polygone regulier" 
	  (texgraph-insert "Dpolyreg(A,S1,<{nb_cotes=}>5), "))
	---
	("Intersection de deux droites" 
	  (texgraph-insert "I:=Intersec(Droite(A,B),Droite(C,D)), I1:=I[1], I2:=I[2], Point(I), "))
	("Intersection droite-cercle (ou ligne-ligne)" 
	  (texgraph-insert "I:=Intersec(Droite(A,B),Cercle(C,<{rayon=}>4)),I1:=I[1], I2:=I[2], Point(I), "))
	("Intersection de deux lignes polygonales" 
	  (texgraph-insert "I:=Intersec(Ligne([A,B,C],1), Ligne([D,E,F],1)), Point(I), "))
  )

  (->"Courbes"
	("Courbe cartesienne" 
	  (texgraph-insert "Courbe(t+i*f(t)), "))
	("Courbe parametree" 
	  (texgraph-insert "Courbe(x(t)+i*y(t)), "))
	("Courbe polaire" 
	  (texgraph-insert "Courbe(r(t)*exp(i*t)), "))
	("Courbe de fonction periodique" 
	  (texgraph-insert "periodic(f(t),a,b{,div,discont}), "))
	---
	("Domaine entre Cf et Ox sur [a;b]" 
	  (texgraph-insert "FillStyle:=bdiag, FillColor:=blue, domaine1(f(t) {,a,b}), "))
	("Domaine entre Cf et Cg sur [a;b]" 
	  (texgraph-insert "FillStyle:=bdiag, FillColor:=blue, domaine2(f(t),g(t) {,a,b}), "))
	("Domaine entre Cf et Cg entre points d'intersection" 
	  (texgraph-insert "FillStyle:=bdiag, FillColor:=blue, domaine3(f(t),g(t)), "))
	("Tangente a Cf en x0" 
	  (texgraph-insert "tangente(f(x),x0 <{,long=}>), "))
	("Tangente a la courbe parametree par f(t)" 
	  (texgraph-insert "tangenteP(f(t),t0 <{,long=}>), "))
	---
	("Representation de la suite Un+1=f(Un)" 
	  (texgraph-insert "suite(f(x),<{U0=}>1,<{n=}>10), "))
	("Solution de l'equa diff x'(t)+iy'(t)=f(t,x,y)" 
	  (texgraph-insert "EquaDif(f(t,x,y),t0,x0+i*y0,<{mode=}>0), "))
	("Courbe implicite f(x,y)=0" 
	  (texgraph-insert "Implicit(f(x,y),<{n=}>25,<{m=}>25), "))
  )	

  (->"Transformations geometriques"
	(->"Objets non dessines"
		("Symetrie axiale" 
		  (texgraph-insert "sym([liste_pts],[A,B]), "))
		("Rotation" 
		  (texgraph-insert "rot([liste_pts],A,alpha), "))
		("Translation" 
		  (texgraph-insert "ftransform([liste_pts],z+<{vect=}>v), "))
		("Homothetie" 
		  (texgraph-insert "hom([liste_pts],A,k), "))
		("Projection othogonale" 
		  (texgraph-insert "proj([liste_pts],[A,B]), "))
		("Similitude" 
		  (texgraph-insert "simil([liste_pts],A,k,alpha), "))
		("Inversion" 
		  (texgraph-insert "inv([liste_pts],A,rayon), "))
		---
		("Symetrie glissee" 
		  (texgraph-insert "symG([liste_pts],A,vect), "))
		("Symetrie oblique" 
		  (texgraph-insert "symG([liste_pts],[A,B],vect), "))
		("Projection oblique" 
		  (texgraph-insert "projO([liste_pts],[A,B],vect), "))
		("Images par une fonction" 
		  (texgraph-insert "ftransform([liste_pts],f(z)), "))
		("Affinite" 
		  (texgraph-insert "affin([liste_pts],[A,B],vect,k), "))
		---
		("Application affine" 
		  (texgraph-insert "defAff(nom,A,A',partie_lineaire), "))
	)
	---
	(group "Objets dessines")
	("Symetrie axiale" 
	  (texgraph-insert "M':=sym(M,[A,B]), Point(M'), "))
	("Rotation" 
	  (texgraph-insert "M':=rot(M,A,alpha), Point(M'), "))
	("Translation" 
	  (texgraph-insert "M':=ftransform(M,z+<{vect=}>v), Point(M'), "))
	("Homothetie" 
	  (texgraph-insert "M':=hom(M,A,k), Point(M'), "))
	("Projection othogonale" 
	  (texgraph-insert "M':=proj(M,[A,B]), Point(M'), "))
	("Similitude" 
	  (texgraph-insert "M':=simil(M,A,k,alpha), Point(M'), "))
	("Inversion" 
	  (texgraph-insert "M':=inv(M,A,rayon), Point(M'), "))
	---
	("Symetrie glissee" 
	  (texgraph-insert "M':=symG(M,A,vect), Point(M'), "))
	("Symetrie oblique" 
	  (texgraph-insert "M':=symG(M,[A,B],vect), Point(M'), "))
	("Projection oblique" 
	  (texgraph-insert "M':=projO(M,[A,B],vect), Point(M'), "))
	("Images par une fonction" 
	  (texgraph-insert "M':=ftransform(M,f(z)), Point(M'), "))
	("Affinite" 
	  (texgraph-insert "M':=affin(M,[A,B],vect,k), Point(M'), "))
	---
	("Application affine" 
	  (texgraph-insert "defAff(nom,A,A',partie_lineaire), "))
  )

  (->"Codage de la figure"
	("Angle droit" 
	  (texgraph-insert "angleD(B,A,C,<{rayon=}>0.8), "))
	("Angle (arc de cercle)" 
	  (texgraph-insert "Arc(B,A,C,<{rayon=}>0.8,<{sens=}>+1), "))
	("Secteur angulaire" 
	  (texgraph-insert "wedge(B,A,C,<{rayon=}>0.8), "))
	---
	("Marquer un segment" 
	  (texgraph-insert "markseg(A,B,<{nb=}>2,<{esp=}>0.1,<{long=}>0.4), "))
	("Marquer un angle" 
	  (texgraph-insert "markangle(B,A,C,<{rayon=}>0.8,<{nb=}>2,<{esp=}>0.1,<{long=}>0.3), "))
	("Flecher une ligne polygonale" 
	  (texgraph-insert "flecher([A,B,..],[pos1,pos2,..]), "))
  )

  (->"Etiquettes"
	("Label(s)" 
	  (texgraph-insert "Label(A,\"$A$\",B,\"$B$\",..), "))
	("Point et etiquette associee" 
	  (texgraph-insert "LabelDot(A,\"$A$\",\"NO\",<{mark=}>1,<{dist=}>0.25), "))
	---
	("Droite horizontale graduee" 
	  (texgraph-insert "Arrows:=1, GradDroite([<{origin=}>0,0+i*1],[<{unite=}>2,<{pas=}>1],xyticks,<{subdiv=}>2,bottom,i,1,\"$a$\"), Arrows:=0, "))
	("Droite verticale graduee" 
	  (texgraph-insert "Arrows:=1, GradDroite([<{origin=}>0,0+i*1],[<{unite=}>i*2,<{pas=}>1],xyticks,<{subdiv=}>2,top,right,1,\"$a$\"), Arrows:=0, "))
	("Etiquette supplementaire sur Ox" 
	  (texgraph-insert "LabelAxe(x,pi,\"$\\\\pi$\",1,<{mark=}>1), "))
	("Etiquette supplementaire sur Oy" 
	  (texgraph-insert "LabelAxe(y,pi,\"$\\\\pi$\",2,<{mark=}>1), "))
  )

  (->"Macros complementaires"  
	("Nouvelle macro" 
	  (texgraph-insert "NewMac(nom,corps [,param1,param2,..]), "))
	(->"Macros instrumentsDessin"
		("Charger les macros instrumentsDessin.mac" 
		  (texgraph-insert "Load(\"instrumentsDessin.mac\"), "))
		("Aide (instrumentsDessin.pdf)" 
		  (shell "texdoc /usr/local/share/TeXgraph/doc/instrumentsDessin.pdf"))
		---
		("Dessiner un crayon" 
		  (texgraph-insert "crayon(0,[long:=3,angle:=-50]), "))
		("Dessiner un compas" 
		  (texgraph-insert "compas(0,[long:=4,dir:=exp(i*45*deg)]), "))
		("Dessiner une equerre" 
		  (texgraph-insert "equerre(0,[long:=4,dir:=exp(i*45*deg)]), "))
		("Dessiner un rapporteur" 
		  (texgraph-insert "rapporteur(0,[rayon:=4,dir:=exp(i*45*deg)]), "))
		("Dessiner une regle" 
		  (texgraph-insert "regle(0,[long:=4,dir:=1]), "))
	)
	(->"Macros Dessin a main levee"
		("Charger les macros trembling.mac" 
		  (texgraph-insert "Load(\"trembling.mac\"), "))
		---
		("Cercle (centre-rayon)" 
		  (texgraph-insert "tCercle([A,R], <{pour_R=1_ecart_en_cm=}>0.05), "))
		("Cercle (defini par 3 points)" 
		  (texgraph-insert "tCercle([A,B,C]), <{pour_R=1_ecart_en_cm=}>0.05), "))
		("Droite(AB)" 
		  (texgraph-insert "tDroite([A,B], <{ecart_en_cm=}>0.1), "))
		("Droite a.x+b.y=c" 
		  (texgraph-insert "tDroite([a,b,c], <{ecart_en_cm=}>0.1), "))
		("Ligne polygonale" 
		  (texgraph-insert "tLigne(<{commande_ou_liste_de_points}>, <{ferme=0ou}>1, <{ecart_en_cm=}>0.1), "))
		("Courbe" 
		  (texgraph-insert "tCourbe(<{commande_ou_liste_de_points}>, <{ouvert=1ou}>0 + i*<{clip=}>1, <{ecart_en_cm=}>0.075), "))
	)
  )
)



