
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : figures-menu.scm
;; DESCRIPTION : Eukleides Figures menus
;; BY	       : Emmanuel Corcelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (figures-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert TeXgraph primitive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eukleides-cursor-pos l)
  (cond ((null? l) 0)
	((null? (cdr l)) 1)
	((and (== (car l) #\() (== (cadr l) #\))) 1)
	((and (== (car l) #\() (== (cadr l) #\,)) 1)
	((and (== (car l) #\,) (== (cadr l) #\))) 1)
	((and (== (car l) #\,) (== (cadr l) #\,)) 1)
	(else (+ (eukleides-cursor-pos (cdr l)) 1))))

(define (eukleides-insert s)
  (insert-go-to s (list (eukleides-cursor-pos (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind figures-menu

;  (->"Figures"
	  (->"Detailled examples"
		(group "Triangles")

		("Triangle" 
		  (eukleides-insert "frame(-1,-1,6,3) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C triangle(<AB=>5,<BC=>4,<CA=>3,<polar_angle=>0:) ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$C$\",C,90:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"5 cm\",segment(A,B),-90:) ; draw(\"4 cm\",segment(B,C),45:) ; draw(\"3 cm\",segment(C,A),135:) ; "))

		("Right triangle" 
		  (eukleides-insert "frame(-2,-2,6,5) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C right(<AB=>5,<AC=>4,<polar_angle=>0:) ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(B,A,C,right,<size=>1) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$C$\",C,90:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"5 cm\",segment(A,B),-90:) ; draw(\"4 cm\",segment(B,C),45:) ; draw(\"3 cm\",segment(C,A),135:) ; "))

		("Equilateral triangle" 
		  (eukleides-insert "frame(-1,-1,5,4.5) ; ")
		  (insert-return)
	  	  (eukleides-insert "B C A equilateral(4, <pol_angle=>0:) ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "<marks>")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A,B),double) ; mark(segment(A,C),double) ; mark(segment(B,C),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(C,B,A,simple) ; mark(B,A,C,simple) ; mark(A,C,B,simple) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B$\",B,225:) ; draw(\"$C$\",C,-45:) ; draw(\"$A$\",A,90:) "))

		("Isosceles triangle" 
		  (eukleides-insert "frame(-1,-1,4,4.5) ; ")
		  (insert-return)
	  	  (eukleides-insert "B C A isosceles(<BC=>3,<AB=AC=>4, <pol_angle=>0:) ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "<marks>")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A,B),double) ; mark(segment(A,C),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(C,B,A,simple) ; mark(A,C,B,simple) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B$\",B,225:) ; draw(\"$C$\",C,-45:) ; draw(\"$A$\",A,90:) "))

		("Isosceles triangle with altitude" 
		  (eukleides-insert "frame(-1,-1,8,3) ;")
		  (insert-return)
	  	  (eukleides-insert "A B C isosceles ; draw(A, B, C) ; ")
		  (insert-return)
	  	  (eukleides-insert "<marks>")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A, C), cross) ; mark(segment(C, B), cross) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(B, A, C, double) ; mark(C, B, A, double) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$C$\",C,90:) ; ")
		  (insert-return)
	  	  (eukleides-insert "<altitude>")
		  (insert-return)
	  	  (eukleides-insert "H = projection(C, line(A, B)) ; draw(segment(C, H), dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(H) ; mark(B, H, C, right) ; draw(\"$H$\",H,-90:) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A, H)) ; mark(segment(B, H)) ;"))

		(group "Constructions")

		("Circumcircle and perpendicular bisectors of a triangle" 
		  (eukleides-insert "frame(-1,-2.5,7,4.2) ; ")
		  (insert-return)
	  	  (eukleides-insert "B C A triangle ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "c=circle(A,B,C) ; draw(c) ; ")
		  (insert-return)
	  	  (eukleides-insert "a=segment(B,C) ; A1=midpoint(a) ; dA=bisector(a) ; draw(dA,dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "b=segment(A,C) ; B1=midpoint(b) ; dB=bisector(b) ; draw(dB,dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "c=segment(A,B) ; C1=midpoint(c) ; dC=bisector(c) ; draw(dC,dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "O=intersection(dA,dB) ; draw(O,dot) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(O,B1,C,right) ; mark(O,C1,A,right) ; mark(O,A1,B,right) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B$\",B,225:) ; draw(\"$C$\",C,-45:) ; draw(\"$A$\",A,90:) ; draw(\"$O$\",O,0:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B_1$\",B1,-45:) ; draw(\"$C_1$\",C1,180:) ; draw(\"$A_1$\",A1,0:) ; "))

		("Isobarycenter and medians of a triangle" 
		  (eukleides-insert "frame(-1,-1,7,4.5) ; ")
		  (insert-return)
	  	  (eukleides-insert "B C A triangle ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "a=segment(B,C) ; b=segment(A,C) ; c=segment(A,B) ; ")
		  (insert-return)
	  	  (eukleides-insert "A'=midpoint(a) ; B'=midpoint(b) ; C'=midpoint(c) ; ")
		  (insert-return)
	  	  (eukleides-insert "dA=median(A,B,C) ; dB=median(B,A,C) ; dC=median(C,B,A) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(dA,dashed) ; draw(dB,dashed) ; draw(dC,dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "G=intersection(dA,dB) ; draw(G,dot) ; ")
		  (insert-return)
	  	  (eukleides-insert "<labels>")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B$\",B,-180:) ; draw(\"$C$\",C,0:) ; draw(\"$A$\",A,45:) ; draw(\"$G$\",G,0:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B'$\",B',80:) ; draw(\"$C'$\",C',90:) ; draw(\"$A'$\",A',-110:) ;")
		  (insert-return)
	  	  (eukleides-insert "<marks>")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A,B'),simple) ; mark(segment(C,B'),simple) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(C,A'),double) ; mark(segment(B,A'),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(B,C'),cross) ; mark(segment(A,C'),cross) "))

		("Orthocenter and altitudes of a triangle" 
		  (eukleides-insert "frame(-1,-2,7,4.5) ; ")
		  (insert-return)
	  	  (eukleides-insert "B C A triangle ; draw(A,B,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "a=segment(B,C) ; b=segment(A,C) ; c=segment(A,B) ; ")
		  (insert-return)
	  	  (eukleides-insert "hA=perpendicular(a,A) ; hB=perpendicular(b,B) ; hC=perpendicular(c,C) ; ")
		  (insert-return)
	  	  (eukleides-insert "H=intersection(hA,hB) ; draw(H,dot) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(hA,dashed) ; draw(hB,dashed) ; draw(hC,dashed) ; ")
		  (insert-return)
	  	  (eukleides-insert "H_A=projection(A,line(a)) ; H_B=projection(B,line(b)) ; H_C=projection(C,line(c)) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(B,H_C,C,right) ; mark(A,H_B,B,right) ; mark(C,H_A,A,right) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$B$\",B,180:) ; draw(\"$C$\",C,0:) ; draw(\"$A$\",A,135:) ; draw(\"$H$\",H,0:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$H_B$\",H_B,0:) ; draw(\"$H_C$\",H_C,180:) ; draw(\"$H_A$\",H_A,-45:) "))

		("Incircle and bisectors of a triangle" 
		  (eukleides-insert "frame(-1,-1,7,5) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C triangle ; draw(A, B, C) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(incircle(A, B, C)) ; draw(bisector(B, A, C), dotted) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(bisector(A, B, C), dotted) ; draw(bisector(B, C, A), dotted) ;  ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$C$\",C,90:) ; "))

		---
		(group "Quadrilaterals")

		("Square" 
		  (eukleides-insert "frame(-1,-1,4,4) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C D square(3) ; draw(A,B,C,D) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(A,D,C,right) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(A,B),double) ; mark(segment(B,C),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(C,D),double) ; mark(segment(D,A),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$C$\",C,45:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$D$\",D,135:) ; draw(\"$c$\",segment(B,C),0:) "))

		("Rectangle" 
		  (eukleides-insert "frame(-1,-1,5,3) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C D rectangle(4.5,2.5) ; draw(A,B,C,D) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(A,D,C,right) ; mark(segment(A,B),double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(B,C)) ; mark(segment(C,D),double) ; mark(segment(D,A)) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$C$\",C,45:) ; draw(\"$D$\",D,135:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$l$\",segment(A,D),180:) ; draw(\"$L$\",segment(A,B),-90:) "))

		("Parallelogram" 
		  (eukleides-insert "frame(-1,-1,8,5) ; ")
		  (insert-return)
	  	  (eukleides-insert "A B C D parallelogram ; O = barycenter(A, B, C, D) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(A, B, C, D) ; draw(O) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(segment(A, C), dotted) ; draw(segment(B, D), dotted) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(O, A), double) ; mark(segment(O, C), double) ; ")
		  (insert-return)
	  	  (eukleides-insert "mark(segment(O, B), cross) ; mark(segment(O, D), cross) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$C$\",C,45:) ; draw(\"$D$\",D,135:)"))

		---
		(group "Others")

		("Theorem of Thales" 
		  (eukleides-insert "frame(-1,-1,7,4) ; ")
		  (insert-return)
	  	  (eukleides-insert "A = point(0, 0) ; B = point(6, 0) ; ")
		  (insert-return)
	  	  (eukleides-insert "c = circle(A, B) ; M = point(c, 50:) ; ")
		  (insert-return)
	  	  (eukleides-insert "color(lightgray) ; draw(c, 0:, 180:) ; ")
		  (insert-return)
	  	  (eukleides-insert "color(black) ; draw(A, B, M) ; mark(A, M, B, right) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(\"$A$\",A,-135:) ; draw(\"$B$\",B,-45:) ; draw(\"$M$\",M,45:) ;"))

		("Collinear points" 
		  (eukleides-insert "A B C D square ; A B E equilateral(4) ; B F G equilateral(4, 30:) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(A, B, C, D) ; draw(A, B, E) ; draw(B, F, G) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(line(E, F), dotted) "))

		("A parabola with focus and directrix" 
		  (eukleides-insert "F = point(3, 1.5) ; ")
		  (insert-return)
	  	  (eukleides-insert "D = line(point(1, 0.5), -65:) ; C = parabola(F, D) ; ")
		  (insert-return)
	  	  (eukleides-insert "draw(F) ; draw(D) ; draw(C) "))

		("Tangents to a circle" 
		  (eukleides-insert "O = point(2, 2) ; C = circle(O, 2) ; ")
		  (insert-return)
	  	  (eukleides-insert "A = point(6.5, 2) ; c = circle(O, A) ; ")
		  (insert-return)
	  	  (eukleides-insert "I J intersection(C, c) ; ")
		  (insert-return)
	  	  (eukleides-insert "color(lightgray) ; draw(line(A, I)) ; draw(line(A, J)) ; ")
		  (insert-return)
	  	  (eukleides-insert "color(black) ; draw(O, plus) ; draw(A) ; draw(C) ; draw(c, dotted) "))
	)

;	(->"Triangles"
;		("" 
;		  (eukleides-insert ", "))
;		("" 
;		  (eukleides-insert ", "))
;	)

;  )

)



