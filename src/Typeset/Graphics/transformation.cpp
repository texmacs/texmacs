
/******************************************************************************
* MODULE     : transformation.cpp
* DESCRIPTION: mathematical transformations
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "transformation.hpp"

/******************************************************************************
* Dilatations
******************************************************************************/

struct dilatation_rep: public transformation_rep {
  array<double> magn;
  dilatation_rep (array<double> magn2): magn (magn2) {}
  point direct_transform (point p) {
    int i, n= min (N(p), N(magn));
    point q (n);
    for (i=0; i<n; i++) q[i]= p[i] * magn[i];
    return q;
  }
  point inverse_transform (point p) {
    int i, n= min (N(p), N(magn));
    point q (n);
    for (i=0; i<n; i++) q[i]= p[i] / magn[i];
    return q;
  }
};

transformation::transformation (array<double> magn):
  rep (new dilatation_rep (magn)) {}

/******************************************************************************
* Compound transformations
******************************************************************************/

struct compound_transformation_rep: public transformation_rep {
  transformation T1, T2;
  compound_transformation_rep (transformation T1b, transformation T2b):
    T1 (T1b), T2 (T2b) {}
  point direct_transform (point p) { return T1 (T2 (p)); }
  point inverse_transform (point p) {
    return T2->inverse_transform (T1->inverse_transform (p)); }
};

transformation
operator * (transformation T1, transformation T2) {
  return new compound_transformation_rep (T1, T2);
}

/******************************************************************************
* Inverted transformations
******************************************************************************/

struct inverted_transformation_rep: public transformation_rep {
  transformation T;
  inverted_transformation_rep (transformation T2): T (T2) {}
  point direct_transform (point p) { return T->inverse_transform (p); }
  point inverse_transform (point p) { return T (p); }
};

transformation
invert (transformation T) {
  return new inverted_transformation_rep (T);
}
