
/******************************************************************************
* MODULE     : poly_line.hpp
* DESCRIPTION: Poly lines
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "array.hpp"

typedef array<double> point;
typedef array<point> poly_line;
typedef array<poly_line> contours;

point operator -  (point p);
point operator +  (point p1, point p2);
point operator -  (point p1, point p2);
point operator *  (double x, point p );
point operator /  (point  p, double x);
bool  operator == (point p1, point p2);

double min (point p);
double max (point p);
double distance (point p, point q);
point project (point p, point q1, point q2);
double distance (point p, point q1, point q2);
point inf (point p, point q);
point sup (point p, point q);

double distance (point p, poly_line pl);
bool nearby (point p, poly_line pl);
point inf (poly_line pl);
point sup (poly_line pl);
poly_line operator + (poly_line pl, point p);
poly_line operator - (poly_line pl, point p);
poly_line operator * (double x, poly_line pl);
poly_line normalize (poly_line pl);

double distance (point p, contours pl);
bool nearby (point p, contours pl);
point inf (contours pl);
point sup (contours pl);
contours operator + (contours pl, point p);
contours operator - (contours pl, point p);
contours operator * (double x, contours pl);
contours normalize (contours pl);
double similarity (contours g1, contours g2);
