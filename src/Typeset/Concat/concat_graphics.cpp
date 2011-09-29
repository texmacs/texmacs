
/******************************************************************************
* MODULE     : concat_graphics.cpp
* DESCRIPTION: Typeset graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven and Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "Boxes/graphics.hpp"
#include "drd_std.hpp"

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
  grid gr= as_grid (env->read (GR_GRID));
  array<box> bs;
  gr->set_aspect (env->read (GR_GRID_ASPECT));
  bs << grid_box (ip, gr, env->fr, env->as_length ("2ln"),
                  env->clip_lim1, env->clip_lim2);
  typeset_graphical (bs, t, ip);
  gr= as_grid (env->read (GR_EDIT_GRID));
  gr->set_aspect (env->read (GR_EDIT_GRID_ASPECT));
  box b= graphics_box (ip, bs, env->fr, gr, env->clip_lim1, env->clip_lim2);
  print (b);
}

void
concater_rep::typeset_gr_group (tree t, path ip) {
  array<box> bs;
  typeset_graphical (bs, t, ip);
  print (graphics_group_box (ip, bs));
}

void
concater_rep::typeset_superpose (tree t, path ip) {
  int i, n= N(t);
  array<box> bs (n);
  for (i=0; i<n; i++) {
    bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
    if (is_func (t[i], FREEZE))
      // FIXME: this dirty hack is necessary, because typeset_as_concat
      // would put an accessible concat_box around bs[i] otherwise
      bs[i]= frozen_box (decorate_middle (descend (ip, i)), bs[i]);
  }
  print (superpose_box (ip, bs));
}

void
concater_rep::typeset_gr_linear_transform (tree t, path ip) {
  if (N(t) != 2 || !is_tuple (t[1])) { typeset_error (t, ip); }
  frame f= affine_2D (as_matrix<double> (t[1]));
  box   b= typeset_as_concat (env, t[0], descend (ip, 0));
        /* The call should be performed with 'typeset_as_atomic()',
	   but we should re-test transform() under these circumstances.
         */
  print (b->transform (env->fr * (f * invert (env->fr))));
}

void
concater_rep::typeset_text_at (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box    b     = typeset_as_concat (env, t[0], descend (ip, 0));
  point  p     = env->fr (env->as_point (env->exec (t[1])));
  string halign= env->textat_halign;
  string valign= env->textat_valign;

  if (N(p) == 0)
    typeset_dynamic (tree (ERROR, "bad text-at"), ip);
  else {
    SI x= (SI) p[0], y= (SI) p[1];
    if (halign == "left") x -= b->x1;
    else if (halign == "center") x -= ((b->x1 + b->x2) >> 1);
    else if (halign == "right") x -= b->x2;
    if (valign == "bottom") y -= b->y1;
    else if (valign == "center") y -= ((b->y1 + b->y2) >> 1);
    else if (valign == "top") y -= b->y2;
    print (textat_box (ip, b, x, y));
  }
}

void
concater_rep::typeset_point (tree t, path ip) {
  if (N(t) < 2) { typeset_error (t, ip); return; }
  int i, n= N(t);
  tree u (TUPLE, N(t));
  for (i=0; i<n; i++)
    u[i]= env->exec (t[i]);
  point p= env->fr (env->as_point (u));
  print (point_box (ip, p, 20*PIXEL, env->col,
                    env->fill_mode, env->fill_color,
                    env->point_style));
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
  if (N(a) == 0 || N(a[0]) == 0) { typeset_error (t, ip); return; }
  if (N(a) == 1) {
    a << copy (a[0]);
    cip << cip[0];
  }
  curve c= env->fr (poly_segment (a, cip));
  print (curve_box (ip, c, env->lw, env->col,
                    env->dash_style, env->dash_style_unit,
                    env->fill_mode, env->fill_color,
                    env->line_arrows));
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
  if (N(a) == 0 || N(a[0]) == 0) { typeset_error (t, ip); return; }
  if (n != 3 || linearly_dependent (a[0], a[1], a[2]) ||
      (N (intersection (midperp (a[0], a[1], a[2]),
                        midperp (a[1], a[2], a[0]))) == 0))
    typeset_line (t, ip, close);
  else {
    curve c= env->fr (arc (a, cip, close));
    print (curve_box (ip, c, env->lw, env->col,
                      env->dash_style, env->dash_style_unit,
                      env->fill_mode, env->fill_color,
                      env->line_arrows));
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
  if (N(a) == 0 || N(a[0]) == 0) { typeset_error (t, ip); return; }
  if (N(a) == 1) {
    a << copy (a[0]);
    cip << cip[0];
  }
  curve c= env->fr (N(a)>=3 ? spline (a, cip, close) : poly_segment (a, cip));
  print (curve_box (ip, c, env->lw, env->col,
                    env->dash_style, env->dash_style_unit,
                    env->fill_mode, env->fill_color,
                    env->line_arrows));
}

void
concater_rep::typeset_var_spline (tree t, path ip) {
  (void) t; (void) ip;
  print (test_box (ip));
}

void
concater_rep::typeset_cspline (tree t, path ip) {
  typeset_spline(t,ip,true);
}

void
concater_rep::typeset_fill (tree t, path ip) {
  (void) t; (void) ip;
  print (test_box (ip));
}

/******************************************************************************
* Constrainted graphics
******************************************************************************/

hashmap<string,tree> graphical_values (UNINIT);

void
set_graphical_value (tree var, tree val) {
  //cout << "Set " << var << " := " << val << "\n";
  if (is_atomic (var))
    graphical_values (var->label)= val;
}

bool
has_graphical_value (tree var) {
  //cout << "Has " << var << "?\n";
  if (is_func (var, QUOTE, 1)) return has_graphical_value (var[0]);
  return is_atomic (var) && graphical_values->contains (var->label);
}

tree
get_graphical_value (tree var) {
  if (is_func (var, QUOTE, 1)) return get_graphical_value (var[0]);
  ASSERT (has_graphical_value (var), "invalid graphical id");
  //cout << "Get " << var << " = " << graphical_values [var->label] << "\n";
  return graphical_values [var->label];
}

static void
set_graphical_values (tree t) {
  if (is_atomic (t));
  else if (is_func (t, WITH)) {
    for (int i=0; i<N(t)-1; i+=2)
      if (t[i] == "gid" && is_atomic (t[i+1]))
        set_graphical_value (t[i+1]->label, t[N(t)-1]);
    set_graphical_values (t[N(t)-1]);
  }
  else {
    for (int i=0; i<N(t); i++)
      set_graphical_values (t[i]);
  }
}

void
concater_rep::typeset_graphical (array<box>& bs, tree t, path ip) {
  int i, n= N(t);
  set_graphical_values (t);

  for (i=0; i<n; i++)
    if (the_drd->get_type (t[i]) == TYPE_CONSTRAINT) {
      tree u= t[i];
      switch (L(u)) {
      case IS_EQUAL:
        if (has_graphical_value (u[1]))
          set_graphical_value (u[0], get_graphical_value (u[1]));
        break;
      case IS_INTERSECTION:
        break;
      case ON_CURVE:
        break;
      case ON_TEXT_BORDER:
        break;
      case ON_GRID:
        break;
      default:
        break;
      }
    }
  
  for (i=0; i<n; i++)
    if (the_drd->get_type (t[i]) == TYPE_CONSTRAINT)
      bs << empty_box (decorate (descend (ip, 1)));
    else
      bs << typeset_as_atomic (env, t[i], descend (ip, i));
}
