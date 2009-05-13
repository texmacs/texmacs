
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 3d-menu.scm
;; DESCRIPTION : TeXgraph 3D Geometry menus
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (3d-menu))

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
;; 3D menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind 3d-menu
  (group "Objets dessines                         ")
  (->"Axes3D et orientation" 
	("Axes3D" 
	  (texgraph-insert "Arrows:=1, Axes3D(0,0,0,<{pas_x=}>0,<{pas_y=}>0,<{pas_z=}>0), Arrows:=0, "))
	("Orientation du repere (variables theta et phi)" 
	  (texgraph-insert "theta:=pi/6, phi:=pi/3, "))
	("Fenetre graphique 3D" 
	  (texgraph-insert "view3D(xmin,xmax,ymin,ymax,zmin,zmax), "))
  )

  (->"Points, lignes et surfaces"
	("Point et etiquette associee" 
	  (texgraph-insert "LabelDot(Proj3D(A)),\"$A$\",\"NO\",1,<{dist=}>0.8), "))
	("Droite" 
	  (texgraph-insert "DrawDroite([A,v]), "))
	("Segment" 
	  (texgraph-insert "DrawDroite([A,v],long_gauche,long_droite), "))
	("Ligne polygonale" 
	  (texgraph-insert "Ligne3D([A,B,..],<{fermee=}>1), "))
	("Arc de cercle" 
	  (texgraph-insert "Arc3D(B,A,C,rayon,<{sens=}>+1 <{,vect_normal}>), "))
	("Cercle" 
	  (texgraph-insert "Cercle3D(A,<{rayon=}>5,<{vect_normal=}>vecK), "))
	---
	("Courbe gauche" 
	  (texgraph-insert "Courbe3D(x(t),y(t),z(t)), "))
	("Surface parametree" 
	  (texgraph-insert "Surface(x(u,v),y(u,v),z(u,v) {,uMin+i*uMax,vMin+i*vMax,degrade(1 ou 0)}), "))
	---
	("Plan" 
	  (texgraph-insert "DrawPlan([A,vet_normal],vect,long1,long2 {,type}), "))
  )

  (->"Solides"
	("Parallelepipede" 
	  (texgraph-insert "pa:=Parallelep(A,vecI,vecJ,vecK), DrawPoly(pa,4), "))
	("Prisme" 
	  (texgraph-insert "pr:=Prisme([A,B,C],<{transl=}>v), DrawPoly(pr,4), "))
	("Pyramide" 
	  (texgraph-insert "py:=Pyramide([A,B,C],S), DrawPoly(py,4), "))
	("Tetraedre" 
	  (texgraph-insert "te:=Tetra(S,vect1,vect2,vect3), DrawPoly(te,4), "))
	("Cone" 
	  (texgraph-insert "Dcone(S,vect_axe,<{rayon=}>3,<{mode=0a}>4), "))
	("Cylindre" 
	  (texgraph-insert "Dcylindre(A,vect_axe,<{rayon=}>3,<{mode=0a}>4), "))
	("Sphere" 
	  (texgraph-insert "Dsphere(O,<{rayon=}>3,<{mode=0a}>4), "))
  )

  (group "Objets non dessines                     ")

  (->"Points, lignes et surfaces"
	("Point ou vecteur" 
	  (texgraph-insert "M(x,y,z), "))
	("Projete sur le plan dirige vers l'observateur (revenir en 2D)" 
	  (texgraph-insert "Proj3D([A,B,..]), "))
	("Plan" 
	  (texgraph-insert "p:=[S,vect_normal], "))
	("Angle entre deux vecteurs" 
	  (texgraph-insert "angle(v1,v2), "))
	("Isobarycentre" 
	  (texgraph-insert "isobar3d([A,B,..]), "))
	("Barycentre" 
	  (texgraph-insert "bary3d([A,a,B,b,..]), ")) 
	("Facettes de la surface parametree par f(u,v)" "GetSurface"
	  (texgraph-insert "GetSurface(f(u,v) {,uMin+i*uMax,vMin+i*vMax}), "))
	("Mapper une fonction" 
	  (texgraph-insert "Map3D(f,x,[A,zA,B,zB,..]), "))
	---
	(group "Intersections droite et plan")
	("Intersection droite-droite" 
	  (texgraph-insert "interDD(d1,d2), ")) 
	("Intersection droite-plan" 
	  (texgraph-insert "interDP(d,p), "))
	("Intersection plan-plan" 
	  (texgraph-insert "interPP(p1,p2), "))
	("Restreindre une liste de points a un demi-espace" "Clip3DLine"
	  (texgraph-insert "Clip3DLine([liste-pts],plan,<{fermee=}>0), "))
  )

  (->"Solides"
	("Parallelepipede" 
	  (texgraph-insert "Parallelep(S,vecI,vecJ,vecK), "))
	("Prisme" 
	  (texgraph-insert "Prisme([A,B,C],<{transl=}>v), "))
	("Pyramide" 
	  (texgraph-insert "Pyramide([A,B,C],S), "))
	("Tetraedre" 
	  (texgraph-insert "Tetra(S,vect1,vect2,vect3), "))
	("Cylindre (polyedre)" 
	  (texgraph-insert "Cylindre(A,vect_axe,<{rayon=}>3,<{nb_faces=}>20), "))
	("Cone (polyedre)" 
	  (texgraph-insert "Cone(S,vect_axe,<{rayon=}>3,<{nb_faces=}>20), "))
	("Sphere (polyedre)" 
	  (texgraph-insert "Sphere(Origin,<{rayon=}>3,<{nb_fuseaux=}>20,<{nb_tranches=}>20), "))
	---
	(group "(Inter)sections plan-solide")
	("Section (plan-polyedre)" 
	  (texgraph-insert "s:=Section(plan,polyedre), "))
	("Intersection (plan-polyedre)" 
	  (texgraph-insert "Intersection(plan,polyedre {,sortie_facette}), "))
	("Restreindre une liste de facettes a un demi-espace" "ClipFacet"
	  (texgraph-insert "ClipFacet([liste-facettes],plan), "))
  )

  (->"Transformations geometriques"
	("Symetrie orthogonale"
	  (texgraph-insert "dsym3d([A,B,..],droite ou plan), "))
	("Rotation" 
	  (texgraph-insert "rot3d([A,B,..],droite,alpha), "))
	("Translation" 
	  (texgraph-insert "shift3d([A,B,..],vect), "))
	("Projection orthogonale" 
	  (texgraph-insert "proj3d([A,B,..],droite ou plan), "))
	("Homothetie" 
	  (texgraph-insert "hom3d([A,B,..],S,k), "))
	("Inversion" 
	  (texgraph-insert "inv3d([A,B,..],O,rayon), "))
	("Images par une fonction" 
	  (texgraph-insert "ftransform3d([A,B,..],f(M)), "))
  )

  (->"Commandes de dessin"
	("Dessiner un polyedre convexe" "DrawPoly"
	  (texgraph-insert "DrawPoly(polyedre,<{mode=0a}>4), "))
	("Dessiner un polyedre non convexe" "DrawPolyNC"
	  (texgraph-insert "DrawPolyNC(polyedre,<{mode=0ou}>1), "))
	("Dessiner des aretes" "DrawAretes"
	  (texgraph-insert "DrawAretes([liste_aretes],<{mode=0ou}>1), "))
	;("MakePoly" (texgraph-insert ", "))
	---
	(group "Dessiner une scene 3D")
	("Regrouper plusieurs objets 3D ensemble" "Build3D"
	  (texgraph-insert "Buil3D([1<{ou2}>,white,objet1],[1<{ou2}>,white,objet2],..), "))
	("Dessiner la scene creee avec Buil3D" "Display3D"
	  (texgraph-insert "Display3D(), "))
	("Renvoyer la scene 3D creee avec Build3D" "Get3D" 
	  (texgraph-insert "Get3D(), "))
  )

  (->"Calculs"
	(->"Coordonnees d'un point"
		("Abscisse" 
		  (texgraph-insert "Xde(A), ")) 
		("Ordonnee" 
		  (texgraph-insert "Yde(A), ")) 
		("Cote" 
		  (texgraph-insert "Zde(A), ")) 
	)
	(->"Projections d'un point sur les axes"
		("Projete sur Ox" 
		  (texgraph-insert "px(A), ")) 
		("Projete sur Oy" 
		  (texgraph-insert "py(A), ")) 
		("Projete sur Oz" 
		  (texgraph-insert "pz(A), ")) 
	)
	(->"Projections d'un point sur les plans du repere"
		("Projete sur xOy" 
		  (texgraph-insert "pxy(A), ")) 
		("Projete sur xOz" 
		  (texgraph-insert "pxz(A), ")) 
		("Projete sur yOz" 
		  (texgraph-insert "pyz(A), ")) 
	)
	(->"Calculs vectoriels"
		("Determinant" 
		  (texgraph-insert "det(v1,v2,v3, "))
		("Norme" 
		  (texgraph-insert "Norm(v), ")) 
		("Vecteur normalise" 
		  (texgraph-insert "normalize(v), ")) 
		("Produit vectoriel" 
		  (texgraph-insert "Prodvec(v1,v2), ")) 
		("Produit scalaire" 
		  (texgraph-insert "Prodscal(v1,v2), ")) 
	)
  )
  ---
  (->"Macros Polyedres II"
	("Aide (PolyedresII.pdf)" 
	  (system "texdoc /usr/local/share/TeXgraph/doc/PolyedresII &"))
	("Charger les macros PolyedresII.mac" 
	  (texgraph-insert "Load(\"PolyedresII.mac\"), "))
		
  )
)



