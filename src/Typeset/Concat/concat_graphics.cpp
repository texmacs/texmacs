
/******************************************************************************
* MODULE     : concat_graphics.cpp
* DESCRIPTION: Typeset graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven and Henri Lesourd
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

/* NOTE: We use the ascending order for drawing, like in SVG. The code
   below conforms to this convention, which says that the first elements
   are painted first, and can be covered by the subsequent ones.
 */
/* TODO: Check that the active elements (f.e. the <action> markup) also
   adhere to the same convention.
 */
void
concater_rep::typeset_graphics (tree t, path ip) {
  int i, n= N(t);
  grid gr= as_grid (env->read (GR_GRID));
  array<box> bs (n+1);
  gr->set_aspect (env->read (GR_GRID_ASPECT));
  bs[0]= grid_box (ip, gr, env->fr, env->as_length ("2ln"),
		   env->clip_lim1, env->clip_lim2);
  for (i=0; i<n; i++)
    bs[i+1]= typeset_as_concat (env, t[i], descend (ip, i));
  // if (n == 0) bs << empty_box (decorate_right (ip));
  gr= as_grid (env->read (GR_EDIT_GRID));
  gr->set_aspect (env->read (GR_EDIT_GRID_ASPECT));
  box b= graphics_box (ip, bs, env->fr, gr, env->clip_lim1, env->clip_lim2);
  print (STD_ITEM, b);
}

void
concater_rep::typeset_superpose (tree t, path ip) {
  int i, n= N(t);
  array<box> bs (n);
  for (i=0; i<n; i++)
    bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (STD_ITEM, superpose_box (ip, bs));
}

void
concater_rep::typeset_text_at (tree t, path ip) {
  box    b     = typeset_as_concat (env, t[0], descend (ip, 0));
  point  p     = env->fr (env->as_point (env->exec (t[1])));
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
    point p= env->fr (env->as_point (u));
    print (STD_ITEM, point_box (ip, p, 20*PIXEL, env->col, env->point_style));
  }
}

void
concater_rep::typeset_line (tree t, path ip, bool close) {
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->as_point (env->exec (t[i]));
  array<path> cip(n);
  for (i=0; i<n; i++)
    cip[i]= descend (ip, i);
  if (close) {
    a << copy (a[0]);
    cip << cip[0];
  }
  if (N(a) == 0 || N(a[0]) == 0)
    typeset_dynamic (tree (ERROR, "bad line"), ip);
  else {
    if (N(a) == 1) {
      a << copy (a[0]);
      cip << cip[0];
    }
    curve c= env->fr (poly_segment (a, cip));
    print (STD_ITEM, curve_box (ip, c, env->lw, env->col,
				env->line_style, env->line_style_unit));
  }
}

void
concater_rep::typeset_arc (tree t, path ip, bool close) {
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->as_point (env->exec (t[i]));
  array<path> cip(n);
  for (i=0; i<n; i++)
    cip[i]= descend (ip, i);
  if (N(a) == 0 || N(a[0]) == 0)
    typeset_dynamic (tree (ERROR, "bad arc"), ip);
  else
  if (n != 3 || linearly_dependent (a[0], a[1], a[2]) ||
     (N (intersect (midperp (a[0], a[1], a[2]),
		    midperp (a[1], a[2], a[0]))) == 0))
    typeset_line (t, ip, close);
  else {
    curve c= env->fr (arc (a, cip, close));
    print (STD_ITEM, curve_box (ip, c, env->lw, env->col,
				env->line_style, env->line_style_unit));
  }
}

void
concater_rep::typeset_spline (tree t, path ip, bool close) {
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->as_point (env->exec (t[i]));
  array<path> cip(n);
  for (i=0; i<n; i++)
    cip[i]= descend (ip, i);
  if (N(a) == 0 || N(a[0]) == 0)
    typeset_dynamic (tree (ERROR, "bad spline"), ip);
  else {
    if (N(a) == 1) {
      a << copy (a[0]);
      cip << cip[0];
    }
    curve c= env->fr (
      N(a)>=3 ? spline (a, cip, close) : poly_segment (a, cip));
    print (STD_ITEM, curve_box (ip, c, env->lw, env->col,
				env->line_style, env->line_style_unit));
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
