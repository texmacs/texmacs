
/******************************************************************************
* MODULE     : concat_graphics.cpp
* DESCRIPTION: Typeset graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"

/******************************************************************************
* Typesetting graphics
******************************************************************************/

void
concater_rep::typeset_graphics (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_superpose (tree t, path ip) {
  int i, n= N(t);
  array<box> bs (n);
  for (i=0; i<n; i++)
    bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (STD_ITEM, composite_box (ip, bs));
}

void
concater_rep::typeset_text_at (tree t, path ip) {
  box    b     = typeset_as_concat (env, t[0], descend (ip, 0));
  tree   pos   = env->exec (t[1]);
  string halign= as_string (env->exec (t[2]));
  string valign= as_string (env->exec (t[3]));

  bool error;
  SI x, y;
  env->get_point (pos, x, y, error);
  if (halign == "left") x -= b->x1;
  else if (halign == "center") x -= ((b->x1 + b->x2) >> 1);
  else if (halign == "right") x -= b->x2;
  if (valign == "bottom") y -= b->y1;
  else if (valign == "center") y -= ((b->y1 + b->y2) >> 1);
  else if (valign == "top") y -= b->y2;
  print (STD_ITEM, move_box (ip, b, x, y));
}

void
concater_rep::typeset_point (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_line (tree t, path ip, bool close) {
  int i, n= N(t);
  tree u[n];
  array<box> bs;
  for (i=0; i<n; i++)
    u[i]= env->exec (t[i]);
  for (i=0; i<(close?n:n-1); i++) {
    bool error= false;
    SI x1, y1, x2, y2;
    env->get_point (t[i], x1, y1, error);
    env->get_point (t[(i+1)%n], x2, y2, error);
    bs << line_box (decorate_right (ip), x1, y1, x2, y2, env->lw, env->col);
  }
  box b= composite_box (ip, bs);
  print (STD_ITEM, b);
}

void
concater_rep::typeset_spline (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_var_spline (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_cspline (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_fill (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}
