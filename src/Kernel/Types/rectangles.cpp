
/******************************************************************************
* MODULE     : rectangles.cpp
* DESCRIPTION: Rectangles and lists of rectangles with reference counting.
*              Used in graphical programs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "rectangles.hpp"

/******************************************************************************
* Routines for rectangles
******************************************************************************/

rectangle_rep::rectangle_rep (SI x1b, SI y1b, SI x2b, SI y2b):
  x1 (x1b), y1 (y1b), x2 (x2b), y2 (y2b) { }

rectangle::rectangle (SI x1b, SI y1b, SI x2b, SI y2b):
  rep (tm_new<rectangle_rep> (x1b, y1b, x2b, y2b)) { }

rectangle::operator tree () {
  return tree (TUPLE,
	       as_string (rep->x1), as_string (rep->y1),
	       as_string (rep->x2), as_string (rep->y2));
}

ostream&
operator << (ostream& out, rectangle r) {
  out << "rectangle ("
      << r->x1 << ", " << r->y1 << ", "
      << r->x2 << ", " << r->y2 << ")";
  return out;
}

rectangle
copy (rectangle r) {
  return rectangle (r->x1, r->y1, r->x2, r->y2);
}

bool
operator == (rectangle r1, rectangle r2) {
  return
    (r1->x1==r2->x1) && (r1->y1==r2->y1) &&
    (r1->x2==r2->x2) && (r1->y2==r2->y2);
}

bool
operator != (rectangle r1, rectangle r2) {
  return
    (r1->x1!=r2->x1) || (r1->y1!=r2->y1) ||
    (r1->x2!=r2->x2) || (r1->y2!=r2->y2);
}

bool
operator <= (rectangle r1, rectangle r2) {
  return
    (r1->x1>=r2->x1) && (r1->x2<=r2->x2) &&
    (r1->y1>=r2->y1) && (r1->y2<=r2->y2);
}

bool
intersect (rectangle r1, rectangle r2) {
  return
    (r1->x1<r2->x2) && (r1->x2>r2->x1) &&
    (r1->y1<r2->y2) && (r1->y2>r2->y1);
}

rectangle
translate (rectangle r, SI x, SI y) {
  return rectangle (r->x1+x, r->y1+y, r->x2+x, r->y2+y);
}

double
area (rectangle r) {
  double w= max (r->x2 - r->x1, 0);
  double h= max (r->y2 - r->y1, 0);
  return w*h;
}

/******************************************************************************
* Miscellaneous subroutines
******************************************************************************/

// FIXME: Why do we need this? Compiler bug?
#define min(x,y) ((x)<=(y)?(x):(y))
#define max(x,y) ((x)<=(y)?(y):(x))

void
complement (rectangle r1, rectangle r2, rectangles& l) {
  if (!intersect (r1, r2)) { r1 >> l; return; }
  if (r1->x1 < r2->x1) rectangle (r1->x1, r1->y1, r2->x1, r1->y2) >> l;
  if (r1->x2 > r2->x2) rectangle (r2->x2, r1->y1, r1->x2, r1->y2) >> l;
  if (r1->y1 < r2->y1) rectangle (max (r1->x1, r2->x1), r1->y1,
				  min (r1->x2, r2->x2), r2->y1) >> l;
  if (r1->y2 > r2->y2) rectangle (max (r1->x1, r2->x1), r2->y2,
				  min (r1->x2, r2->x2), r1->y2) >> l;
}

void
complement (rectangles l1, rectangle r2, rectangles& l) {
  for (; !is_nil (l1); l1= l1->next)
    complement (l1->item, r2, l);
}

void
intersection (rectangle r1, rectangle r2, rectangles& l) {
  if (!intersect (r1, r2)) return;
  rectangle (max (r1->x1, r2->x1), max (r1->y1, r2->y1),
	     min (r1->x2, r2->x2), min (r1->y2, r2->y2)) >> l;
}

rectangle
operator * (rectangle r, int d) {
  return rectangle (r->x1*d, r->y1*d, r->x2*d, r->y2*d);
}

rectangle
operator / (rectangle r, int d) {
  return rectangle (r->x1/d, r->y1/d, r->x2/d, r->y2/d);
}

/******************************************************************************
* Exported routines for rectangles
******************************************************************************/

rectangles
operator - (rectangles l1, rectangles l2) {
  rectangles a=l1;
  for (; !is_nil (l2); l2= l2->next) {
    rectangles b;
    complement (a, l2->item, b);
    a=b;
  }
  return a;
}

rectangles
operator & (rectangles l1, rectangles l2) {
  rectangles l, lc1, lc2;
  for (lc1= l1; !is_nil (lc1); lc1= lc1->next)
    for (lc2= l2; !is_nil (lc2); lc2= lc2->next)
      intersection (lc1->item, lc2->item, l);
  return l;
}

bool
adjacent (rectangle r1, rectangle r2) {
  return
    (((r1->x2==r2->x1) || (r1->x1==r2->x2)) &&
     ((r1->y1==r2->y1) && (r1->y2==r2->y2))) ||
    (((r1->y2==r2->y1) || (r1->y1==r2->y2)) &&
     ((r1->x1==r2->x1) && (r1->x2==r2->x2)));
}

rectangles
disjoint_union (rectangles l, rectangle r) {
  if (is_nil (l)) return r;
  if (adjacent (l->item, r))
    return disjoint_union (l->next,
			   least_upper_bound (rectangles (l->item, r)));
  return rectangles (l->item, disjoint_union (l->next, r));
}

rectangles
operator | (rectangles l1, rectangles l2) {
  rectangles l (l1-l2);
  while (!is_nil (l2)) {
    l = disjoint_union (l, l2->item);
    l2= l2->next;
  }
  return l;
}

rectangles
translate (rectangles l, SI x, SI y) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1+ x, r->y1+ y, r->x2+ x, r->y2+ y),
		     translate (l->next, x, y));
}

rectangles
thicken (rectangles l, SI width, SI height) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1- width, r->y1- height,
				r->x2+ width, r->y2+ height),
		     thicken (l->next, width, height));
}

rectangles
outline (rectangles rs, SI pixel) {
  return simplify (correct (thicken (rs, pixel, 3*pixel) -
			    thicken (rs, 0, 2*pixel)));
}

rectangles
operator * (rectangles l, int d) {
  if (is_nil (l)) return l;
  return rectangles (l->item*d, l->next*d);
}

rectangles
operator / (rectangles l, int d) {
  if (is_nil (l)) return l;
  return rectangles (l->item/d, l->next/d);
}

rectangles
correct (rectangles l) {
  if (is_nil (l)) return l;
  if ((l->item->x1 >= l->item->x2) || (l->item->y1 >= l->item->y2))
    return correct (l->next);
  return rectangles (l->item, correct (l->next));
}

rectangles
simplify (rectangles l) {
  if (is_nil (l) || is_atom (l)) return l;
  return simplify (l->next) | rectangles (l->item);
}

rectangle
least_upper_bound (rectangles l) {
  ASSERT (!is_nil (l), "no rectangles in list");
  rectangle r1= copy (l->item);
  while (!is_nil (l->next)) {
    l= l->next;
    rectangle r2= l->item;
    r1->x1= min (r1->x1, r2->x1);
    r1->y1= min (r1->y1, r2->y1);
    r1->x2= max (r1->x2, r2->x2);
    r1->y2= max (r1->y2, r2->y2);
  }
  return r1;
}

double
area (rectangles r) {
  double sum= 0.0;
  while (!is_nil (r)) {
    sum += area (r->item);
    r= r->next;
  }
  return sum;
}
