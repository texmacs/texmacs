
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
#include "Boxes/graphics.hpp"

/******************************************************************************
* Typesetting graphics
******************************************************************************/

void
concater_rep::typeset_graphics (tree t, path ip) {
  int i, n= N(t);
  array<box> bs (n);
  for (i=0; i<n; i++)
    bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  if (n == 0) bs << empty_box (decorate_right (ip));
  box b= graphics_box (ip, bs, env->fr, env->clip_lim1, env->clip_lim2);
  print (STD_ITEM, b);
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
  point  p     = env->fr (env->decode_point (env->exec (t[1])));
  string halign= as_string (env->exec (t[2]));
  string valign= as_string (env->exec (t[3]));

  SI x= (SI) p[0], y= (SI) p[1];
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
  int i, n= N(t);
  tree u (TUPLE, N(t));
  for (i=0; i<n; i++)
    u[i]= env->exec (t[i]);
  if (N(u) < 2) typeset_dynamic (tree (ERROR, "bad point", t), ip);
  else {
    point p= env->fr (env->decode_point (u));
    print (STD_ITEM, point_box (ip, p, 20*PIXEL, env->col));
  }
}

void
concater_rep::typeset_line (tree t, path ip, bool close) {
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->decode_point (env->exec (t[i]));
  if (close) a << copy (a[0]);
  if (N(a) == 0 || N(a[0]) == 0)
    typeset_dynamic (tree (ERROR, "bad line"), ip);
  else {
    if (N(a) == 1) a << copy (a[0]);
    curve c= env->fr (poly_segment (a));
    print (STD_ITEM, curve_box (ip, c, env->lw, env->col));
  }
}

void
concater_rep::typeset_spline (tree t,path ip,bool close) {
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->decode_point (env->exec (t[i]));
  if (N(a) == 0 || N(a[0]) == 0)
    typeset_dynamic (tree (ERROR, "bad spline"), ip);
  else {
    if (N(a) == 1) a << copy (a[0]) << copy (a[0]);
    if (N(a) == 2) a << copy (a[0]);
    curve c= env->fr (spline (a,close));
    print (STD_ITEM, curve_box (ip, c, env->lw, env->col));
  }
}

void
concater_rep::typeset_var_spline (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}

void
concater_rep::typeset_cspline (tree t, path ip) {
  typeset_spline(t,ip,true);
}

void
concater_rep::typeset_fill (tree t, path ip) {
  (void) t; (void) ip;
  print (STD_ITEM, test_box (ip));
}
