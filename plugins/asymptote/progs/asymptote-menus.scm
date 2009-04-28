
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : asymptote-menus.scm
;; DESCRIPTION : asymptote menus
;; BY	       : Emmanuel Corcelle (corcelle at gmail dot com)
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (asymptote-menus))

; (lazy-menu (asymptote-figures-menu) asymptote-figures-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert asymptote primitive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asymptote-cursor-pos l)
  (cond ((null? l) 0)
	((null? (cdr l)) 1)
	((and (== (car l) #\() (== (cadr l) #\))) 1)
	((and (== (car l) #\() (== (cadr l) #\,)) 1)
	((and (== (car l) #\,) (== (cadr l) #\))) 1)
	((and (== (car l) #\,) (== (cadr l) #\,)) 1)
	(else (+ (asymptote-cursor-pos (cdr l)) 1))))

(define (asymptote-insert s)
  (insert-go-to s (list (asymptote-cursor-pos (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asymptote menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind asymptote-functions-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
  ---
  )

  (group "Examples")

  (-> "Points, lines"
	("Points" 
	  (asymptote-insert "size(6cm,0); pair pO=(0,0), pA=(3,0), pB=(0,2); dot(\"O\",pO,S);
dot(\"A\",pA,E,green); dot(\"B\",pB,N,red); dot(\"I\",I,W,blue); pair pM=3+3I, pN=-2+I; dot(\"$M$\",pM,N,orange); dot(\"$N$\",pN,W,gray); pair pV=pA+pB; dot(\"$V$\",pV,W,pink);
 "))
	("Segments" 
	  (asymptote-insert "size(6cm,0); pair pA=(0,0),pB=(2,1),pC=(2,2),pD=(4,0); draw(pA--pB^^pC--pD,red+1bp); dot(pA--pB^^pC--pD,blue+5bp);
 "))
	("Lines" 
	  (asymptote-insert "size(7cm,0); pair pA=(1,1),pB=(4,0); dot(\"$A$\",pA,N); dot(\"$B$\",pB,N); draw(interp(pA,pB,-.5)--interp(pA,pB,1.25)); "))
	("Path" 
	  (asymptote-insert "size(7.5cm,0); path ligne=(-3,1)..{dir(-45)}(-1,2)..{right}(4,0); draw(ligne,blue); dot(ligne,red); shipout(bbox(3mm,white)); "))
	("Circle" 
	  (asymptote-insert "size(6cm,0); draw(unitcircle); dot((0,0)); draw(Label(\"$r$\"),(0,0)--dir(45),Arrow);
 "))
	("Arcs" 
	  (asymptote-insert "size(7cm,0); real h=5; pair O=(0,0),A=(-h,0),B=(h,0); draw(arc(O,B,A),1bp+blue); draw(A--B,dashed+green); dot(O); dot(\"$A$\",A,SW); dot(\"$B$\",B,SE); shipout(bbox(3mm,white));
 "))
	("Filled arcs" 
	  (asymptote-insert "size(7cm,0); filldraw(buildcycle(arc((0,0),4,0,180), arc((-1,0),3,0,180), arc((3,0),1,0,180)), lightgrey,red); "))
  )  

  (-> "Triangles"
	("Triangle" 
	  (asymptote-insert "size(6cm,0); pair pA=(1,1), pB=(5,1), pC=(4,4); draw(pA--pB--pC--cycle); dot(\"$A$\",pA,dir(pC--pA,pB--pA)); dot(\"$B$\",pB,dir(pC--pB,pA--pB)); dot(\"$C$\",pC,dir(pA--pC,pB--pC)); "))
	("Isosceles triangle" 
	  (asymptote-insert "import math; import markers; size(6cm,0); marker croix=marker(scale(3)*cross(4),1bp+gray); add(grid(6,6,.8lightgray)); pair pA=(1,1), pB=(5,5), pC=(2,4); draw(pA--pB--pC--cycle);
draw(\"$A$\",pA,SW,blue,croix); draw(\"$B$\",pB,SE,blue,croix); draw(\"$C$\",pC,NW,croix); draw(pA--pC,StickIntervalMarker(1,2,size=8,angle=-45,red)); draw(pB--pC,StickIntervalMarker(1,2,size=8,angle=-45,red)); "))

  )

  (-> "Polygons"
	("Square" 
	  (asymptote-insert "unitsize(1cm); dot(Label,(0,0),SW); path carre=scale(5)*unitsquare;
draw(carre); shipout(bbox(5mm,white)); "))
	("Polygon" 
	  (asymptote-insert "size(6cm,0); pair A=(0,0), B=(4,1), C=(3.5,2), D=(1,2.5); draw(A--B--C--D--cycle); dot(\"$A$\",A,dir(D--A,B--A)); dot(\"$B$\",B,dir(C--B,A--B)); dot(\"$C$\",C,dir(B--C,D--C)); dot(\"$D$\",D,dir(A--D,C--D)); "))
  )

  (-> "Styles"
	("Line styles" 
	  (asymptote-insert "unitsize(1cm); draw ((0,5)--(7,5),solid+blue+1bp); draw ((0,4)--(7,4),dotted+.5blue+1bp); draw ((0,3)--(7,3),dashed+red+1bp); draw ((0,2)--(7,2),longdashed+.5red+1bp); draw ((0,1)--(7,1),dashdotted+green+1bp); draw ((0,0)--(7,0),longdashdotted+.5green+1bp);
 "))
	("Arrow styles" 
	  (asymptote-insert "unitsize(1cm); draw((0,0)--(2,2),Arrow); draw((1,0)--(3,2),Arrow(SimpleHead)); draw((2,0)--(4,2),Arrow(HookHead)); draw((3,0)--(5,2),Arrow(TeXHead)); shipout(bbox(5mm,white)); "))
	("Other arrows" 
	  (asymptote-insert "unitsize(1cm,0); DefaultHead=HookHead; 
draw((0,1)--(5,0),BeginArrow());
draw((0,.5)--(5,-.5),MidArrow());
draw((0,0)--(5,-1),EndArrow());
draw((0,-.5)--(5,-1.5),Arrow());
draw((0,-1)--(5,-2),Arrows());
draw((0,-1.5)--(5,-2.5),Arrow(Relative(.25))); shipout(bbox(5mm,white)); "))
	("Marks" 
	  (asymptote-insert "size(7.5cm,0); import markers; path p=(0,0)--(5,1); transform T=shift((0,-1)); draw(p,StickIntervalMarker(dotframe));
draw(T*p,StickIntervalMarker(n=3,angle=45,size=5mm,space=1mm,dotframe));
draw(T^2*p,StickIntervalMarker(n=3,angle=45,size=5mm,space=3mm,dotframe));
draw(T^3*p,StickIntervalMarker(n=3,angle=-45,size=10mm,space=3mm,dotframe));
draw(T^4*p,StickIntervalMarker(i=3,n=2,angle=-45,size=4mm,space=2mm,dotframe));
 "))
  )


)



