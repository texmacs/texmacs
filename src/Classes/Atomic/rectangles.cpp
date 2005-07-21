
/******************************************************************************
* MODULE     : rectangles.cpp
* DESCRIPTION: Rectangles and lists of rectangles with reference counting.
*              Used in graphical programs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "rectangles.hpp"

/******************************************************************************
* Routines for rectangles
******************************************************************************/

rectangle_rep::rectangle_rep (SI x1b, SI y1b, SI x2b, SI y2b):
  x1 (x1b), y1 (y1b), x2 (x2b), y2 (y2b) { }

rectangle::rectangle (SI x1b, SI y1b, SI x2b, SI y2b):
  rep (new rectangle_rep (x1b, y1b, x2b, y2b)) { }

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
  for (; !nil (l1); l1= l1->next)
    complement (l1->item, r2, l);
}

void
intersection (rectangle r1, rectangle r2, rectangles& l) {
  if (!intersect (r1, r2)) return;
  rectangle (max (r1->x1, r2->x1), max (r1->y1, r2->y1),
	     min (r1->x2, r2->x2), min (r1->y2, r2->y2)) >> l;
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
  for (; !nil (l2); l2= l2->next) {
    rectangles b;
    complement (a, l2->item, b);
    a=b;
  }
  return a;
}

rectangles
operator & (rectangles l1, rectangles l2) {
  rectangles l, lc1, lc2;
  for (lc1= l1; !nil (lc1); lc1= lc1->next)
    for (lc2= l2; !nil (lc2); lc2= lc2->next)
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
  if (nil (l)) return r;
  if (adjacent (l->item, r))
    return disjoint_union (l->next,
			   least_upper_bound (rectangles (l->item, r)));
  return rectangles (l->item, disjoint_union (l->next, r));
}

rectangles
operator | (rectangles l1, rectangles l2) {
  rectangles l (l1-l2);
  while (!nil (l2)) {
    l = disjoint_union (l, l2->item);
    l2= l2->next;
  }
  return l;
}

rectangles
translate (rectangles l, SI x, SI y) {
  if (nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1+ x, r->y1+ y, r->x2+ x, r->y2+ y),
		     translate (l->next, x, y));
}

rectangles
thicken (rectangles l, SI width, SI height) {
  if (nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1- width, r->y1- height,
				r->x2+ width, r->y2+ height),
		     thicken (l->next, width, height));
}

rectangles
operator / (rectangles l, int d) {
  if (nil (l)) return l;
  return rectangles (l->item/d, l->next/d);
}

rectangles
correct (rectangles l) {
  if (nil (l)) return l;
  if ((l->item->x1 >= l->item->x2) || (l->item->y1 >= l->item->y2))
    return correct (l->next);
  return rectangles (l->item, correct (l->next));
}

rectangles
simplify (rectangles l) {
  if (nil (l) || atom (l)) return l;
  return simplify (l->next) | rectangles (l->item);
}

rectangle
least_upper_bound (rectangles l) {
  if (nil (l)) fatal_error ("no rectangles in list", "least_upper_bound");
  rectangle r1= copy (l->item);
  while (!nil (l->next)) {
    l= l->next;
    rectangle r2= l->item;
    r1->x1= min (r1->x1, r2->x1);
    r1->y1= min (r1->y1, r2->y1);
    r1->x2= max (r1->x2, r2->x2);
    r1->y2= max (r1->y2, r2->y2);
  }
  return r1;
}
