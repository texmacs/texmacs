
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

void
concater_rep::typeset_graphics (tree t, path ip) {
  int i, n= N(t);
  array<box> bs (n+1);
  bs[0]= typeset_as_grid (env, env->read (GR_GRID),
			  ip, env->read (GR_GRID_ASPECT));
  for (i=0; i<n; i++)
    bs[i+1]= typeset_as_concat (env, t[i], descend (ip, i));
  // if (n == 0) bs << empty_box (decorate_right (ip));
  box b= graphics_box (ip, bs, env->fr, env->clip_lim1, env->clip_lim2);
  print (STD_ITEM, b);
}

box
typeset_as_grid (edit_env env, tree t, path ip, tree aspect) {
  array<SI> subd (0, 1);
  array<color> col (env->dis->black, env->dis->black);
  if (is_tuple (aspect)) {
    int i;
    bool b= false;
    subd= array<SI> (N(aspect));
    col= array<color> (N(aspect));
    for (i=0; i<N(aspect); i++) {
       if (is_tuple (aspect[i], "axes", 1)) {
         subd[i]= 0;
         b= true;
       }
       else {
         subd[i]= as_int (aspect[i][0]);
       }
       col[i]= env->dis->get_color (as_string (aspect[i][1]));
    }
    if (!b) {
      array<SI> subd0 (1);
      array<color> col0 (1);
      subd0[0]= 0;
      col0[0]= env->dis->black;
      subd= subd0 << subd;
      col= col0 << col;
    }
    do {
      b= true;
      for (i=1; i<N(subd); i++)
         if (subd[i-1]>subd[i]) {
           SI j;
           color c;
           j= subd[i-1];subd[i-1]= subd[i];subd[i]= j;
           c= col[i-1];col[i-1]= col[i];col[i]= c;
           b= false;
         }
    }
    while (!b);
  }
  grid gr= empty_grid ();
  double step= 1;
  point center= point (0.0, 0.0);
  if (is_tuple (t, "cartesian", 1)) {
    step= as_double (t[1]);
    step= env->fr->direct_scalar (step) - env->fr->direct_scalar (0);
    gr= cartesian (subd, col, env->fr (center), (SI)step);
  }    
  else   
  if (is_tuple (t, "polar")) {
    SI astep= 8;
    if (is_tuple (t, "polar", 2)) {
      step= as_double (t[1]);
      astep= as_int (t[2]);
    }
    else
    if (is_tuple (t, "polar", 3)) { 
      center= env->decode_point (t[1]);
      step= as_double (t[2]);
      astep= as_int (t[3]);
    }
    step= env->fr->direct_scalar (step) - env->fr->direct_scalar (0);
    gr=polar (subd, col, env->fr (center), (SI) step, astep);
  }
  else
  if (is_tuple (t, "logarithmic")) {
    SI base= 10;
    if (is_tuple (t, "logarithmic", 1)) {
      base= as_int (t[1]);
    }
    else
    if (is_tuple (t, "logarithmic", 2)) {
      step= as_double (t[1]);
      base= as_int (t[2]);
    }
    step= env->fr->direct_scalar (step) - env->fr->direct_scalar (0);
    gr= logarithmic (subd, col, env->fr (center), (SI)step, base);
  }
  return grid_box (decorate (ip), gr, env->fr, env->clip_lim1, env->clip_lim2);
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
    if (N(a) == 1) a << copy (a[0]);
    curve c= env->fr (N(a)>=3 ? spline (a, close) : poly_segment (a));
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
