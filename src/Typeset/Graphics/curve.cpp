
/******************************************************************************
* MODULE     : curve.cpp
* DESCRIPTION: mathematical curves
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "curve.hpp"

/******************************************************************************
* General routines
******************************************************************************/

array<point>
curve_rep::rectify (double err) {
  array<point> a (1);
  a[0]= evaluate (0.0);
  rectify_cumul (a, err);
  return a;
}

/******************************************************************************
* Straight curves
******************************************************************************/

struct straight_curve_rep: public curve_rep {
  point p1, p2;
  straight_curve_rep (point p1b, point p2b): p1 (p1b), p2 (p2b) {}
  point evaluate (double t) { return (1.0-t)*p1 + t*p2; }
  void rectify_cumul (array<point>& a, double err) { a << p2; }
};

curve::curve (point p1, point p2):
  rep (new straight_curve_rep (p1, p2)) {}

/******************************************************************************
* Compound curves
******************************************************************************/

struct compound_curve_rep: public curve_rep {
  curve c1, c2;
  int n1, n2;
  compound_curve_rep (curve c1b, curve c2b):
    c1 (c1b), c2 (c2b), n1 (c1->nr_components()), n2 (c2->nr_components()) {}
  int nr_components () { return n1 + n2; }
  point evaluate (double t) {
    double n= n1+n2;
    if (t <= n1/n) return c1 (t*n/n1);
    else return c2 (t*n/n2 - n1); }
  void rectify_cumul (array<point>& a, double err) {
    c1->rectify_cumul (a, err);
    c2->rectify_cumul (a, err);
  }
};

curve
operator * (curve c1, curve c2) {
  // FIXME: we might want to test whether c1(1.0) is approx equal to c2(0.0)
  return new compound_curve_rep (c1, c2);
}

/******************************************************************************
* Inverted curves
******************************************************************************/

struct inverted_curve_rep: public curve_rep {
  curve c;
  int n;
  inverted_curve_rep (curve c2): c (c2), n (c->nr_components()) {}
  int nr_components () { return n; }
  point evaluate (double t) { return c (1.0 - t); }
  void rectify_cumul (array<point>& a, double err) {
    array<point> b= c->rectify (err);
    int i, k= N(b);
    for (i=k-1; i>=0; i--) a << b[i];
  }
};

curve
invert (curve c) {
  return new inverted_curve_rep (c);
}
