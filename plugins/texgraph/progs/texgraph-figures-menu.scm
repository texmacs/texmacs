
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : figures-menu.scm
;; DESCRIPTION : TeXgraph Figures menus
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texgraph-figures-menu))

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
;; TeXgraph Figures menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texgraph-figures-menu
  (->"Outils pour les professeurs"

	("Etiquetage automatique dun polygone"
	  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
	  (insert-return) 
	  (texgraph-insert "LabelsPolygone(L,[dist:=0.35],\"$A$\",\"$B$\",\"$C$\",\"$D$\",\"$E$\",\"$F$\",\"$G$\"), "))

	(->"Triangles"

		("Triangle equilateral"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "PolygoneRegulier(<{nb_cotes=}>3, <{longueur_cotes=}>3, ")
		  (insert-return) 
		  (texgraph-insert "[rotation:=90*deg, CodCote:=2, CodAngle:=1], ")
		  (insert-return) 
		  (texgraph-insert "<{label_cote=\"\"_ou}>\"3 cm\",  ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$B$\",\"$C$\"), "))

		("Triangle rectangle" 
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "TriangleRectangle( [0], [4,3], ")
		  (insert-return) 
		  (texgraph-insert "[rotation:=0*deg, CodCotes:=[0,0,0], CodAngles:=[1,0,0]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$B$\",\"$C$\",\"4 cm\",\"5 cm\",\"3 cm\",\"\",\"37\",\"53\",), "))

	)

	(->"Quadrilateres"

		("Carre"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "PolygoneRegulier(<{nb_cotes=}>4, <{longueur_cotes=}>3, ")
	  	  (insert-return) 
	  	  (texgraph-insert "[rotation:=135*deg, CodCote:=2, CodAngle:=1], ")
	  	  (insert-return) 
	  	  (texgraph-insert "<{label_cote=\"\"_ou}>\"3 cm\", ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$B$\",\"$C$\",\"$D$\"), "))		

	)

	("Polygones reguliers" 
	  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
	  (insert-return) 
	  (texgraph-insert "PolygoneRegulier(<{nb_cotes=}>5, <{longueur_cotes=}>2, ")
	  (insert-return) 
	  (texgraph-insert "[rotation:=0*deg, CodCote:=2, CodAngle:=1], ")
	  (insert-return) 
	  (texgraph-insert "<{label_cote=\"\"_ou}>\"2 cm\", ")
	  (insert-return) 
	  (texgraph-insert "\"$A$\",\"$B$\",\"$C$\",\"$D$\",\"$E$\",\"$F$\",\"$G$\",\"$H$\",\"$I$\",\"$J$\"), "))

	---
	(->"Image dune figure par une transformation"
		(group "Symetrie centrale")
		("Image dun point, segment ou polygone"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{point_segment_ou_polygone_L}>  L:=[-2-i,-0.5-0.5*i,-1+2*i,-3], ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_symetrie_S}>  S:=i, DotScale:=2, DotStyle:=times, LabelDot(S,\"$S$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImagePolygone(L, ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symC, transfElem:=[S], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$A'$\",\"$B$\",\"$B'$\",\"$C$\",\"$C'$\",\"$D$\",\"$D'$\",\"$E$\",\"$E'$\"), "))

		("Image dune droite"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{droite_(MN)}>  M:=-2, N:=-1+2*i, ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_symetrie_S}>  S:=i, DotScale:=2, DotStyle:=times, LabelDot(S,\"$S$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageDroite([M,N], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symC, transfElem:=[S], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$M$\",\"$M'$\",\"$N$\",\"$N'$\"), "))

		("Image dun cercle"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{centre_du_cercle_O}>  O:=-1, <{rayon_r}>  r:=1, ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_symetrie_S}>  S:=i, DotScale:=2, DotStyle:=times, LabelDot(S,\"$S$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageCercle([O,r], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symC, transfElem:=[S], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$O$\",\"$O'$\"), "))

		---
		(group "Symetrie axiale")
		("Image dun point, segment ou polygone"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{point_segment_ou_polygone_L}>  L:=[-2-i,-0.5-0.5*i,-1+2*i,-3], ")
		  (insert-return) 
		  (texgraph-insert "<{axe_de_symetrie_(AB)}>  A:=0.1, B:=i, Width:=8, LineStyle:=solid, Droite(A,B),")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImagePolygone(L, ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symA, transfElem:=[A,B], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$A'$\",\"$B$\",\"$B'$\",\"$C$\",\"$C'$\",\"$D$\",\"$D'$\",\"$E$\",\"$E'$\"), "))

		("Image dune droite"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{droite_(MN)}>  M:=-2, N:=-1+2*i,   ")
		  (insert-return) 
		  (texgraph-insert "<{axe_de_symetrie_(AB)}>  A:=0, B:=i, Width:=8, LineStyle:=solid, Droite(A,B), DotScale:=2, ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageDroite([M,N], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symA, transfElem:=[A,B], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$M$\",\"$M'$\",\"$N$\",\"$N'$\"), "))

		("Image dun cercle"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{centre_du_cercle_O}>  O:=-1.5,  <{rayon_r}>  r:=1,  ")
		  (insert-return) 
		  (texgraph-insert "<{axe_de_symetrie_(AB)}> A:=0, B:=1+2*i, Width:=8, LineStyle:=solid, Droite(A,B), DotScale:=2, ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageCercle([O,r], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=symA, transfElem:=[A,B], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$O$\",\"$O'$\"), "))

		---
		(group "Rotation")
		("Image dun point, segment ou polygone"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{point_segment_ou_polygone_L}>  L:=[-2-i,-0.5-0.5*i,-1+2*i,-3], ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_rotation_O}>  O:=1+i, <{angle_alpha}>  alpha:=45*deg, ")
		  (insert-return) 
		  (texgraph-insert "DotScale:=2, DotStyle:=times, LabelDot(O,\"$O$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImagePolygone(L, ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=rotat, transfElem:=[O,alpha], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$A'$\",\"$B$\",\"$B'$\",\"$C$\",\"$C'$\",\"$D$\",\"$D'$\",\"$E$\",\"$E'$\"), "))

		("Image dune droite"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{droite_(MN)}>  M:=-2, N:=-1+2*i, ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_rotation_O}>  O:=1+i, <{angle_alpha}>  alpha:=45*deg,  ")
		  (insert-return) 
		  (texgraph-insert "DotScale:=2, DotStyle:=times, LabelDot(O,\"$O$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageDroite([M,N], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=rotat, transfElem:=[O,alpha], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$M$\",\"$M'$\",\"$N$\",\"$N'$\"), "))

		("Image dun cercle"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{centre_du_cercle_A}>  A:=-1,  <{rayon_r}>  r:=1,  ")
		  (insert-return) 
		  (texgraph-insert "<{centre_de_la_rotation_0}>  O:=1+i, <{angle_alpha}>  alpha:=60*deg, ")
		  (insert-return) 
		  (texgraph-insert "DotScale:=2, DotStyle:=times, LabelDot(O,\"$O$\",\"E\",{mark=}1,0.25), ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageCercle([A,r], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=rotat, transfElem:=[O,alpha], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$A'$\"), "))

		---
		(group "Translation")
		("Image dun point, segment ou polygone"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{point_segment_ou_polygone_L}>  L:=[-2-i,-0.5-0.5*i,-1+2*i,-3],  ")
		  (insert-return) 
		  (texgraph-insert "<{vecteur_de_la_translation_v}>  v:=2-i, DotScale:=2, ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImagePolygone(L, ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>Nil], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=transl, transfElem:=[v], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$A$\",\"$A'$\",\"$B$\",\"$B'$\",\"$C$\",\"$C'$\",\"$D$\",\"$D'$\",\"$E$\",\"$E'$\"), "))

		("Image dune droite"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{droite_(MN)}>  M:=-2, N:=-1+2*i, ")
		  (insert-return) 
		  (texgraph-insert "<{vecteur_de_la_translation_v}>  v:=3-i, DotScale:=2, ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageDroite([M,N], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=transl, transfElem:=[v], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$M$\",\"$M'$\",\"$N$\",\"$N'$\"), "))

		("Image dun cercle"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"} ")
		  (insert-return) 
		  (texgraph-insert "<{centre_du_cercle_O}>  O:=-1, <{rayon_r}>  r:=1,  ")
		  (insert-return) 
		  (texgraph-insert "<{vecteur_de_la_translation_v}>  v:=3-i, DotScale:=2, ")
		  (insert-return) 
		  (texgraph-insert "")
		  (insert-return) 
		  (texgraph-insert "ImageCercle([O,r], ")
		  (insert-return) 
		  (texgraph-insert "[style1:=[solid,6,black,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "style2:=[solid,6,blue,<{sommets=NilouDotStyle}>times], ")
		  (insert-return) 
		  (texgraph-insert "transfType:=transl, transfElem:=[v], ")
		  (insert-return) 
		  (texgraph-insert "style3:=[dashed,4,gray]], ")
		  (insert-return) 
		  (texgraph-insert "\"$O$\",\"$O'$\"), "))


	)

	---
	(->"Probabilites"

		("Lancers de des"
		  (texgraph-insert "{preload=\"texgraph_macros_texmacs.mac\"}")
		  (insert-return) 
		  (texgraph-insert "LancersDes({nb_de_des=}2, {nb_de_lancers=}100, [choixrepresent:=pourcent{ou_nombre}, color:=orange, decimales:=1, LabelAngle:=90, LabelStyle:=left]), ")
		  (insert-return) 
		  (texgraph-insert "size(12+7*i,0) "))
	)

  )
)



