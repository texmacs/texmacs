
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : eukleides-menus.scm
;; DESCRIPTION : Eukleides menus
;; BY	       : Emmanuel Corcelle (corcelle at gmail dot com)
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

(texmacs-module (eukleides-menus))

(lazy-menu (figures-menu) figures-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert Eukleides primitive
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
;; Eukleides menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind eukleides-functions-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)

  (link figures-menu)

  (->"Numbers"
	(->"Calculations"
		("Abscissa of A (or u)" 
		  (eukleides-insert "abscissa(A); "))
		("Ordinate of A (or u)" 
		  (eukleides-insert "ordinate(A); "))
		("Distance AB" 
		  (eukleides-insert "distance(A,B); "))
		("Distance from A to l" 
		  (eukleides-insert "distance(A,l); "))
		("Length of s (or u)" 
		  (eukleides-insert "length(s); "))
		("Radius of c" 
		  (eukleides-insert "radius(c); "))
		---
		("Distance between point B and line AC" 
		  (eukleides-insert "height(A, B, C); "))
		("Argument of point A in respect of circle c" 
		  (eukleides-insert "arg(c,A); "))
		("Argument of point A in respect of conic curve cc" 
		  (eukleides-insert "arg(cc, A); "))
		("Half of the major axis if cc is an ellipse..." 
		  (eukleides-insert "major(cc); "))
		("Half of the minor axis if cc is an ellipse..." 
		  (eukleides-insert "minor(cc); "))
		("Eccentricity of conic curve cc" 
		  (eukleides-insert "eccentricity(cc); "))
	)

	(->"Angles"
		("Measure of the angle under ABC" 
		  (eukleides-insert "angle(A, B, C); "))
		("Measure of the angle between vector u and vector v" 
		  (eukleides-insert "angle(u, v); "))
		---
		("Polar angle of s (or u or l)" 
		  (eukleides-insert "angle(s); "))
		("Polar angle of the principal axis of conic curve cc" 
		  (eukleides-insert "angle(cc); "))
	)  
  )

  ---
  (group "Settings ")
  
  ("Visible frame" 
    (eukleides-insert "frame(-2,-2,8,6,1); "))

  (->"Line settings"
	(->"Line style"
		("------------------" 
		  (eukleides-insert "style(full); "))
		("---- ---- ---- --" 
		  (eukleides-insert "style(dashed); "))
		(". . . . . . . ." 
		  (eukleides-insert "style(dotted); "))
	)
	("Line width" 
	    (eukleides-insert "thickness(0.5); "))

  )

  (->"Color"
	("Black" 
	  (eukleides-insert "color(black); "))
	("Darkgrey" 
	  (eukleides-insert "color(darkgray); "))
	("Grey" 
	  (eukleides-insert "color(gray); "))
	("Lightgrey" 
	  (eukleides-insert "color(lightgray); "))
	("White" 
	  (eukleides-insert "color(white); "))
	("Red" 
	  (eukleides-insert "color(red); "))
	("Green" 
	  (eukleides-insert "color(green); "))
	("Blue" 
	  (eukleides-insert "color(blue); "))
	("Cyan" 
	  (eukleides-insert "color(cyan); "))
	("Magenta" 
	  (eukleides-insert "color(magenta); "))
	("Yellow" 
	  (eukleides-insert "color(yellow); "))
  )

  ---
  (group "Objects")

  (->"Points"
	(group "Points")
	("Point (x,y)" 
	  (eukleides-insert "A=point(x, y); "))
	("Point of radial coordinate x and polar angle a" 
	  (eukleides-insert "A=point(x, <pol_angle=>a:); "))
	("Point of abscissa x on the graduated line l" 
	  (eukleides-insert "A=point(l, <abs=>x); "))
	("Point of abscissa x on the graduated line containing segment s" 
	  (eukleides-insert "A=point(s, <abs=>x); "))
	("Point of argument a on circle c" 
	  (eukleides-insert "A=point(c, <arg=>a:); "))
	---
	(group "Constructions")
	("Barycenter of A and B with coefficient x and y" 
	  (eukleides-insert "barycenter(A , <coeff=>1, B , <coeff=>1); "))
	("Midpoint of segment s" 
	  (eukleides-insert "M=midpoint(s); "))
	("Center of circle c" 
	  (eukleides-insert "A=center(c); "))
	("Orthocenter of triangle ABC" 
	  (eukleides-insert "O=orthocenter(A, B, C); "))
	---
	(group "Others")
	("Point on conic curve cc with argument x" 
	  (eukleides-insert "A=point(cc, <arg=>x); "))
	("Point of abscissa x on line l" 
	  (eukleides-insert "A=abscissa(l, <abs=>x); "))
	("Point of ordinate y on line l" 
	  (eukleides-insert "A=ordinate(l, <ord=>y); "))
	("Center of conic curve cc (its vertex for a parabola)" 
	  (eukleides-insert "A=center(cc); "))
	("Beginning of segment s" 
	  (eukleides-insert "A=begin(s); "))
	("End of segment s" 
	  (eukleides-insert "A=end(s); "))
  )

  (->"Vectors"
	("Vector (x,y)" 
	  (eukleides-insert "v=vector(x, y); "))
	("Vector AB" 
	  (eukleides-insert "v=vector(A, B); "))
	("Vector of length x and argument a" 
	  (eukleides-insert "v=vector(x, <arg=>a); "))
	---
	("Unit length vector with same direction than line l" 
	  (eukleides-insert "v=vector(l); "))
	("Vector from begin of segment s to end of s" 
	  (eukleides-insert "v=vector(s); "))
  )

  (->"Segments"
	("Segment [AB]" 
	  (eukleides-insert "s=segment(A, B); "))
	("Segment of direction u, beginning with A" 
	  (eukleides-insert "s=segment(A, u); "))
	("Segment of length x and polar angle a, beginning with A" 
	  (eukleides-insert "s=segment(A, x, <pol_angle=>a); "))
	("Segment from center of circle c to point of argument a" 
	  (eukleides-insert "s=segment(c, <arg=>a); "))
  )

  (->"Lines"
	(group "Lines")
	("Line (AB)" 
	  (eukleides-insert "l=line(A, B); "))
	("Line of direction u, containing A" 
	  (eukleides-insert "l=line(A, u); "))
	("Line containing segment s" 
	  (eukleides-insert "l=line(s); "))
	("Line of polar angle a, containing point A" 
	  (eukleides-insert "l=line(A, <pol_angle=>a); "))
	---
	(group "Constructions")
	("Tangent line of circle c (at point of argument a)" 
	  (eukleides-insert "l=line(c, <arg=>a); "))
	("Tangent line of conic cc (at point of argument x)" 
	  (eukleides-insert "l=line(cc, <arg=>x); "))
	---
	("Parallel line to line l, containing point A" 
	  (eukleides-insert "l=parallel(l, A); "))
	("Perpendicular line to line l, containing point A" 
	  (eukleides-insert "l=perpendicular(l, A); "))
	("Bisector of the sharp angle formed by l and l'" 
	  (eukleides-insert "l=bisector(l, l'); "))
	---
	("Perpendicular bisector of segment s" 
	  (eukleides-insert "l=bisector(s); "))
	("Altitude of triangle ABC containing vertex A" 
	  (eukleides-insert "l=altitude(A, B, C); "))
	("Median of triangle ABC containing vertex A" 
	  (eukleides-insert "l=median(A, B, C); "))
	("Bisector of angle under ABC" 
	  (eukleides-insert "l=bisector(A, B, C); "))
  )

  (->"Circles"
	(group "Circles")
	("Circle of center A and radius x" 
	  (eukleides-insert "c=circle(A, <radius=>1); "))
	("Circle of diameter [AB]" 
	  (eukleides-insert "c=circle(A, B); "))
	---
	(group "Constructions")
	("Circumcircle of triangle ABC" 
	  (eukleides-insert "c=circle(A, B, C); "))
	("Incircle of triangle ABC" 
	  (eukleides-insert "c=incircle(A, B, C); "))
  )

  (->"Conics"
	("Conic curve of focus A, directrix l and eccentricity x" 
	  (eukleides-insert "cc=conic(A, <dir=>l, <ecc=>x); "))
	("Conic curve of foci A and B" 
	  (eukleides-insert "cc=conic(A, B, x); "))
	("Parabola of focus A and directrix l" 
	  (eukleides-insert "cc=parabola(A, l); "))
	("Parabola of center A" 
	  (eukleides-insert "cc=parabola(A, x, a); "))
	("Ellipse of center A" 
	  (eukleides-insert "cc=ellipse(A, x, y, a); "))
	("Hyperbola of center A" 
	  (eukleides-insert "cc=hyperbola(A, x, y, a); "))
	---
	("Foci of conic cc" 
	  (eukleides-insert "A B foci(cc); "))
	("Vertices of conic cc" 
	  (eukleides-insert "A B vertices(cc); "))
  )

  ---
  (group "Other constructions")

  (->"Triangles"
	(group "When 1 point is already defined")
	("Scalene triangle ABC knowing AB" 
	  (eukleides-insert "A B C triangle(<AB>, <pol_angle=>0:); "))
	("Scalene triangle ABC knowing AB, BC and CA" 
	  (eukleides-insert "A B C triangle(<AB>, <BC>, <CA>, <pol_angle=>0:); "))
	("Scalene triangle ABC knowing AB, BAC, CBA" 
	  (eukleides-insert "A B C triangle(<AB>, <BAC>, <CBA>, <pol_angle=>0:); "))
	("Right triangle ABC (right angle in A) knowing AB and AC" 
	  (eukleides-insert "A B C right(<AB>, <AC>, <pol_angle=>0:); "))
	("Right triangle ABC (right angle in B) knowing AB, BAC" 
	  (eukleides-insert "A B C right(<AB>, <BAC>, <pol_angle=>0:); "))
	("Isosceles triangle ABC knowing AB, BAC = CBA" 
	  (eukleides-insert "A B C isosceles(<AB>, <BAC=CBA>, <pol_angle=>0:); "))
	("Isosceles triangle ABC knowing AB and AC = BC" 
	  (eukleides-insert "A B C isosceles(<AB>, <AC=BC>, <pol_angle=>0:); "))
	("Equilateral triangle of side length x" 
	  (eukleides-insert "A B C equilateral(x, <pol_angle=>0:); "))
	---
	(group "When 2 points (A and B) are already defined")
	("Scalene triangle ABC knowing BC and AC" 
	  (eukleides-insert "A B C triangle(<BC>, <AC>); "))
	("Scalene triangle ABC knowing BAC, CBA" 
	  (eukleides-insert "A B C triangle(<BAC>, <CBA>); "))
	("Right triangle ABC (right angle in A) knowing AC" 
	  (eukleides-insert "A B C right(<AC>); "))
	("Right triangle ABC (right angle in B) knowing BAC" 
	  (eukleides-insert "A B C right(<BAC>); "))
	("Isosceles triangle ABC with BAC = CBA" 
	  (eukleides-insert "A B C isosceles(<BAC=CBA>); "))
	("Isosceles triangle ABC with CA = CB" 
	  (eukleides-insert "A B C isosceles(<CA=CB>); "))
	("Equilateral triangle" 
	  (eukleides-insert "A B C equilateral; "))
  )

  (->"Quadrilaterals"
	(group "When 1 points is already defined")
	("Parallelogram ABCD knowing AB, AD and BAD" 
	  (eukleides-insert "A B C D parallelogram(<AB>, <AD>, <BAD>, <pol_angle=>0:); "))
	("Parallelogram ABCD with vector AB = u and vector AD = v" 
	  (eukleides-insert "A B C D parallelogram(<AB=u>, <AD=v>, <pol_angle=>0:); "))
	("Rectangle ABCD knowing AB and AD" 
	  (eukleides-insert "A B C D rectangle(<AB>, <AD>, <pol_angle=>0:); "))
	("Square ABCD with AB = x" 
	  (eukleides-insert "A B C D square(x, <pol_angle=>0:); "))
	---
	(group "When 2 points (A and B) are already defined")
	("Parallelogram ABCD knowing AD and BAD" 
	  (eukleides-insert "A B C D parallelogram(<AD>, <BAD>); "))
	("Rectangle ABCD knowing AD" 
	  (eukleides-insert "A B C D rectangle(<AD>); "))
	("Square ABCD" 
	  (eukleides-insert "A B C D square; "))
	---
	(group "When 3 points are already defined")
	("Parallelogram ABCD" 
	  (eukleides-insert "A B C D parallelogram; "))
	("Rectangle ABCD" 
	  (eukleides-insert "A B C D rectangle; "))
  )

  (->"Polygons"
	("Pentagon of center F" 
	  (eukleides-insert "A B C D E pentagon(F, <radius=>1, <angle=>0:); "))
	("Hexagon of center G" 
	  (eukleides-insert "A B C D E F hexagon(G, <side=>1, <angle=>0:); "))
  )

  (->"Intersections"
	("Intersection point of lines l and l'" 
	  (eukleides-insert "I=intersection(l, l'); "))
	---
	("Intersection of line l and circle c" 
	  (eukleides-insert "I J intersection(l, c); "))
	("Intersection of circle c and circle c'" 
	  (eukleides-insert "I J intersection(c, c'); "))
	("Intersection of line l and conic cc" 
	  (eukleides-insert "I J intersection(l, cc); "))
  )

  (->"Transformations"
	(group "for a point, a line, a circle, a conic")
	("Translation" 
	  (eukleides-insert "A'=translation(A, <vect>u); "))
	("Reflection in respect of line l" 
	  (eukleides-insert "A'=reflection(A, <resp_of>l); "))
	("Rotation of center A and angle a" 
	  (eukleides-insert "M'=rotation(M, <center=>A, <angle=>90:); "))
	("Homothecy of center A and ratio x" 
	  (eukleides-insert "M'=homothecy(M, <center=>A, <ratio=>2); "))
	---
	(group "for a point")
	("Projection on line l perpendicularly to line l" 
	  (eukleides-insert "A'=projection(A, <perp_to>l); "))
	("Projection on line l in direction of line l'" 
	  (eukleides-insert "A'=projection(A, <perp_to>l, <dir_of>l'); "))
	---
	(group "for a vector")
	("Rotation of angle a "
	  (eukleides-insert "u'=rotation(u, <angle=>90:); "))
  )

  ---
  (group "Drawings and Labels")

  (->"Drawing commands"
	(->"Draw point A" 
		("Dot"
		  (eukleides-insert "draw(A, dot, <size=>1); "))
		("Cross"
		  (eukleides-insert "draw(A, cross, <size=>1); "))
		("Plus"
		  (eukleides-insert "draw(A, plus, <size=>1); "))
		("Disc"
		  (eukleides-insert "draw(A, disc, <size=>1); "))
		("Box"
		  (eukleides-insert "draw(A, box, <size=>1); "))
	)

	(->"Draw vector u from point A" 
		("------------------"
		  (eukleides-insert "draw(u, <from>A, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(u, <from>A, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(u, <from>A, dotted); "))
	)

	(->"Draw line l" 
		("------------------"
		  (eukleides-insert "draw(l, full, entire); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(l, dashed, entire); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(l, dotted, entire); "))
	)

	(->"Draw halfline l" 
		(group "last argument : halfline or backhalfline")
		("------------------"
		  (eukleides-insert "draw(l, full, halfline); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(l, dashed, halfline); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(l, dotted, halfline); "))
	)

	(->"Draw segment s" 
		("------------------"
		  (eukleides-insert "draw(s, full, noarrow); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(s, dashed, noarrow); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(s, dotted, noarrow); "))
		---
		("<------------------>"
		  (eukleides-insert "draw(s, full, doublearrow); "))
		("<---- ---- ---- -->"
		  (eukleides-insert "draw(s, dashed, doublearrow); "))
		("<. . . . . . . .>"
		  (eukleides-insert "draw(s, dotted, doublearrow); "))
		---
		("------------------>"
		  (eukleides-insert "draw(s, full, arrow); "))
		("---- ---- ---- -->"
		  (eukleides-insert "draw(s, dashed, arrow); "))
		(". . . . . . . .>"
		  (eukleides-insert "draw(s, dotted, arrow); "))
		---
		("<------------------"
		  (eukleides-insert "draw(s, full, backarrow); "))
		("<---- ---- ---- --"
		  (eukleides-insert "draw(s, dashed, backarrow); "))
		("<. . . . . . . ."
		  (eukleides-insert "draw(s, dotted, backarrow); "))

	)
	---
	(->"Draw circle c" 
		("------------------"
		  (eukleides-insert "draw(c, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(c, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(c, full); "))
	)
	(->"Draw an arc of circle c" 
		("------------------"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, noarrow); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, noarrow); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(c, <from>a, <to>a', full, noarrow); "))
		---
		("<------------------>"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, doublearrow); "))
		("<---- ---- ---- -->"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, doublearrow); "))
		("<. . . . . . . .>"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, doublearrow); "))
		---
		("------------------>"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, arrow); "))
		("---- ---- ---- -->"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, arrow); "))
		(". . . . . . . .>"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, arrow); "))
		---
		("<------------------"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, backarrow); "))
		("<---- ---- ---- --"
		  (eukleides-insert "draw(c, <from>a, <to>a', full, backarrow); "))
		("<. . . . . . . ."
		  (eukleides-insert "draw(c, <from>a, <to>a', full, backarrow); "))
	)
	(->"Draw conic curve cc" 
		("------------------"
		  (eukleides-insert "draw(cc, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(cc, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(cc, dotted); "))
	)
	(->"Draw an arc of conic curve cc" 
		("------------------"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, noarrow); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, noarrow); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, noarrow); "))
		---
		("<------------------>"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, doublearrow); "))
		("<---- ---- ---- -->"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, doublearrow); "))
		("<. . . . . . . .>"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, doublearrow); "))
		---
		("------------------>"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, arrow); "))
		("---- ---- ---- -->"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, arrow); "))
		(". . . . . . . .>"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, arrow); "))
		---
		("<------------------"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, backarrow); "))
		("<---- ---- ---- --"
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, backarrow); "))
		("<. . . . . . . ."
		  (eukleides-insert "draw(cc, <from>x, <to>y, full, backarrow); "))
	)
	---
	(->"Draw triangle ABC" 
		("------------------"
		  (eukleides-insert "draw(A, B, C, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(A, B, C, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(A, B, C, dotted); "))
	)
	(->"Draw quadrilateral ABCD" 
		("------------------"
		  (eukleides-insert "draw(A, B, C, D, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(A, B, C, D, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(A, B, C, D, dotted); "))
	)
	(->"Draw pentagon ABCDE" 
		("------------------"
		  (eukleides-insert "draw(A, B, C, D, E, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(A, B, C, D, E, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(A, B, C, D, E, dotted); "))
	)
	(->"Draw hexagon ABCDEF" 
		("------------------"
		  (eukleides-insert "draw(A, B, C, D, E, F, full); "))
		("---- ---- ---- --"
		  (eukleides-insert "draw(A, B, C, D, E, F, dashed); "))
		(". . . . . . . ."
		  (eukleides-insert "draw(A, B, C, D, E, F, dotted); "))
	)
  )
  (->"Marks"
	(->"Mark the segment s with"
		("a simple line" 
		  (eukleides-insert "mark(s,simple,<size=>1); "))
		("a double line" 
		  (eukleides-insert "mark(s,double,<size=>1); "))
		("a triple line" 
		  (eukleides-insert "mark(s,triple,<size=>1); "))
		("a cross" 
		  (eukleides-insert "mark(s,cross,<size=>1); "))
	)

	(->"Mark the angle under ABC with"
		("a square" 
		  (eukleides-insert "mark(A,B,C,right,<size=>1); "))
		("a simple line" 
		  (eukleides-insert "mark(A,B,C,simple,<size=>1); "))
		("a double line" 
		  (eukleides-insert "mark(A,B,C,double,<size=>1); "))
		("a triple line" 
		  (eukleides-insert "mark(A,B,C,triple,<size=>1); "))
		("a cross" 
		  (eukleides-insert "mark(A,B,C,cross,<size=>1); "))
		("a dashed line" 
		  (eukleides-insert "mark(A,B,C,dashed,<size=>1); "))
		("a dot" 
		  (eukleides-insert "mark(A,B,C,dot,<size=>1); "))
		("an arrow" 
		  (eukleides-insert "mark(A,B,C,arrow,<size=>1); "))
		("a backarrow" 
		  (eukleides-insert "mark(A,B,C,backarrow,<size=>1); "))
	)
  )

  (->"Labels"
	("Label of point A" 
	  (eukleides-insert "label(A, <dist=>0.3, <angle=>0:); "))
	("Label of point A and point A" 
	  (eukleides-insert "draw(A, cross, <size=>1); label(A, <dist=>0.3, <angle=>0:); "))
	("Texte at point A" 
	  (eukleides-insert "draw(\"$A$\", A, <dist=>0.3, <angle=>0:); "))
	("Texte at midpoint of segment s" 
	  (eukleides-insert "draw(\"$A$\", s, <dist=>0.3, <angle=>0:); "))
	---
	("Value of a variable x at point A" 
	  (eukleides-insert "draw(x, A, <dist=>0.3, <angle=>0:); "))
	("Value of a variable x at midpoint of segment s" 
	  (eukleides-insert "draw(x, s, <dist=>0.3, <angle=>0:); "))
  )

)
