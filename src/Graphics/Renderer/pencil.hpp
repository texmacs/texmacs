
/******************************************************************************
* MODULE     : pencil.hpp
* DESCRIPTION: pencils for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PENCIL_H
#define PENCIL_H
#include "tree.hpp"

extern int std_shrinkf;
#define PIXEL 256

enum pencil_cap { cap_square, cap_flat, cap_round };
enum pencil_join { join_bevel, join_miter, join_round };

class pencil_rep: concrete_struct {
public:
  color c;
  SI w;
  pencil_cap cap;
  pencil_join join;
  double miter_lim;

  inline pencil_rep (color c2, SI w2, pencil_cap cap2,
                     pencil_join join2, double l):
    c (c2), w (w2), cap (cap2), join (join2), miter_lim (l) {}

  friend class pencil;
};

class pencil {
  CONCRETE(pencil);
  inline pencil (color c= 0xff000000, SI w= std_shrinkf * PIXEL,
                 pencil_cap cap= cap_round,
                 pencil_join join= join_round, double l= 2.0):
    rep (tm_new<pencil_rep> (c, w, cap, join, l)) {}
};
CONCRETE_CODE(pencil);

#endif // defined PENCIL_H
