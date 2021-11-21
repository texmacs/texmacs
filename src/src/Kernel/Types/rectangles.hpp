
/******************************************************************************
* MODULE     : rectangles.hpp
* DESCRIPTION: Rectangles and lists of rectangles with reference counting.
*              Used in graphical programs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RECTANGLES_H
#define RECTANGLES_H
#include "list.hpp"

class rectangle_rep: concrete_struct {
public:
  SI x1, y1;
  SI x2, y2;

  rectangle_rep (SI x1b, SI y1b, SI x2b, SI y2b);
  friend class rectangle;
};

class rectangle {
  CONCRETE(rectangle);
  rectangle (SI x1b=0, SI y1b=0, SI x2b=0, SI y2b=0);
  operator tree ();
};
CONCRETE_CODE(rectangle);

tm_ostream& operator << (tm_ostream& out, rectangle r);
rectangle copy (rectangle r);
bool operator == (rectangle r1, rectangle r2);
bool operator != (rectangle r1, rectangle r2);
bool intersect (rectangle r1, rectangle r2);
bool operator <= (rectangle r1, rectangle r2);
rectangle translate (rectangle r, SI x, SI y);
rectangle operator * (rectangle r, int d);
rectangle operator / (rectangle r, int d);
rectangle operator * (rectangle r, double x);
rectangle operator / (rectangle r, double x);
rectangle thicken (rectangle r, SI width, SI height);
rectangle least_upper_bound (rectangle r1, rectangle r2);
double area (rectangle r);

typedef list<rectangle> rectangles;

rectangles operator - (rectangles l1, rectangles l2);
rectangles operator & (rectangles l1, rectangles l2);
rectangles operator | (rectangles l1, rectangles l2);
rectangles operator * (rectangles l, int d);
rectangles operator / (rectangles l, int d);
rectangles translate (rectangles l, SI x, SI y);
rectangles thicken (rectangles l, SI width, SI height);
rectangles outlines (rectangles l, SI pixel);
rectangles correct (rectangles l);
rectangles simplify (rectangles l);
rectangle  least_upper_bound (rectangles l);
double area (rectangles r);

#endif // defined RECTANGLES_H
