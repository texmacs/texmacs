
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
#include "hashset.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

#define BEGIN_MAGNIFY                                           \
  tree new_mag= as_string (env->magn * env->mgfy);              \
  tree old_mfy= env->local_begin (MAGNIFY, "1");                \
  tree old_mag= env->local_begin (MAGNIFICATION, new_mag);

#define END_MAGNIFY                             \
  env->local_end (MAGNIFICATION, old_mag);      \
  env->local_end (MAGNIFY, old_mfy);

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
notify_graphics_extents (tree t, point lbot, point rtop) {
  static hashmap<string,tree> h (UNINIT);
  if (is_atomic (t)) {
    string id= t->label;
    tree val= tuple (as_string (lbot[0]), as_string (lbot[1]),
                     as_string (rtop[0]), as_string (rtop[1]));
    if (h[id] != val) {
      h (id)= val;
      array<object> args;
      args << object (id)
           << object (lbot[0]) << object (lbot[1])
           << object (rtop[0]) << object (rtop[1]);
      call ("graphics-notify-extents", args);
    }
  }
  else if (is_func (t, GRAPHICS) || is_func (t, GR_GROUP)) {
    for (int i=0; i<N(t); i++)
      notify_graphics_extents (t[i], lbot, rtop);
  }
  else if (is_func (t, LOCUS) || is_func (t, WITH)) {
    if (is_func (t, LOCUS, 2) &&
        is_func (t[0], ID, 1) &&
        is_atomic (t[0][0]) &&
        starts (t[0][0]->label, "graph-"))
      notify_graphics_extents (t[0][0], lbot, rtop);
    notify_graphics_extents (t[N(t)-1], lbot, rtop);
  }
}

void
concater_rep::typeset_graphics (tree t, path ip) {
BEGIN_MAGNIFY
  grid gr= as_grid (env->read (GR_GRID));
  array<box> bs;
  gr->set_aspect (env->read (GR_GRID_ASPECT));
  bs << grid_box (ip, gr, env->fr, env->as_length ("2ln"),
                  env->clip_lim1, env->clip_lim2);
  typeset_graphical (bs, t, ip);

  point lim1= env->clip_lim1;
  point lim2= env->clip_lim2;
  if (env->get_bool (GR_AUTO_CROP)) {
    SI x1= MAX_SI, y1= MAX_SI, x2= -MAX_SI, y2= -MAX_SI;
    for (int i=1; i<N(bs); i++) {
      box b= bs[i];
      //cout << i << ", " << b << ", " << b->get_type () << "\n";
      x1= min (x1, b->x1); y1= min (y1, b->y1);
      x2= max (x2, b->x2); y2= max (y2, b->y2);
      x1= min (x1, b->x3); y1= min (y1, b->y3);
      x2= max (x2, b->x4); y2= max (y2, b->y4);
    }
    SI pad= env->get_length (GR_CROP_PADDING);
    lim1= env->fr [point (x1 - pad, y1 - pad)];
    lim2= env->fr [point (x2 + pad, y2 + pad)];
    //cout << lim1 << " -- " << lim2 << "\n";
  }

  gr= as_grid (env->read (GR_EDIT_GRID));
  gr->set_aspect (env->read (GR_EDIT_GRID_ASPECT));
  box b= graphics_box (ip, bs, env->fr, gr, lim1, lim2);
  print (b);

  notify_graphics_extents (t, lim1, lim2);
END_MAGNIFY
}

void
concater_rep::typeset_gr_group (tree t, path ip) {
BEGIN_MAGNIFY
  array<box> bs;
  typeset_graphical (bs, t, ip);
  print (graphics_group_box (ip, bs));
END_MAGNIFY
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
BEGIN_MAGNIFY
  if (N(t) != 2) typeset_error (t, ip);
  else {
    box    b     = typeset_as_concat (env, t[0], descend (ip, 0));
    point  p     = env->fr (env->as_point (env->exec (t[1])));
    string halign= env->text_at_halign;
    string valign= env->text_at_valign;

    if (N(p) == 0)
      typeset_dynamic (tree (ERROR, "bad text-at"), ip);
    else {
      SI x= (SI) p[0], y= (SI) p[1], axis= (b->h() >> 1);
      if (halign == "left") x -= b->x1;
      else if (halign == "center") x -= ((b->x1 + b->x2) >> 1);
      else if (halign == "right") x -= b->x2;
      if (valign == "bottom") y -= b->y1;
      else if (valign == "axis") {
	axis= env->fn->yfrac - b->y1;
	y -= env->fn->yfrac;
      }
      else if (valign == "center") y -= ((b->y1 + b->y2) >> 1);
      else if (valign == "top") y -= b->y2;
      print (text_at_box (ip, b, x, y, axis, env->fn->spc->def));
    }
  }
END_MAGNIFY
}

void
concater_rep::typeset_math_at (tree t, path ip) {
BEGIN_MAGNIFY
  if (N(t) != 2) typeset_error (t, ip);
  else {
    // FIXME: attaching ip to compound ("math", t[0]) is a bit hacky,
    // but it seems to work fine for the time being
    box    b     = typeset_as_concat (env, compound ("math", t[0]), ip);
    point  p     = env->fr (env->as_point (env->exec (t[1])));
    string halign= env->text_at_halign;
    string valign= env->text_at_valign;

    if (N(p) == 0)
      typeset_dynamic (tree (ERROR, "bad text-at"), ip);
    else {
      SI x= (SI) p[0], y= (SI) p[1], axis= (b->h() >> 1);
      if (halign == "left") x -= b->x1;
      else if (halign == "center") x -= ((b->x1 + b->x2) >> 1);
      else if (halign == "right") x -= b->x2;
      if (valign == "bottom") y -= b->y1;
      else if (valign == "axis") {
	axis= (env->fn->yx >> 1) - b->y1;
	y -= (env->fn->yx >> 1);
      }
      else if (valign == "center") y -= ((b->y1 + b->y2) >> 1);
      else if (valign == "top") y -= b->y2;
      print (text_at_box (ip, b, x, y, axis, env->fn->spc->def));
    }
  }
END_MAGNIFY
}

void
concater_rep::typeset_point (tree t, path ip) {
BEGIN_MAGNIFY
  if (N(t) < 2) typeset_error (t, ip);
  else {
    int i, n= N(t);
    tree u (TUPLE, N(t));
    for (i=0; i<n; i++)
      u[i]= env->exec (t[i]);
    point p= env->fr (env->as_point (u));
    print (point_box (ip, p, 20*PIXEL, env->col,
                      env->fill_mode, env->fill_color,
                      env->point_style));
  }
END_MAGNIFY
}

static tree
protect_arrow (edit_env env, tree t) {
  return tree (WITH, "arrow-begin", "none", "arrow-end", "none",
               "dash-style", "none", t);
}

array<box>
concater_rep::typeset_line_arrows (path ip) {
  array<box> bs (2);
  if (env->line_arrows[0] != "") {
    tree a= protect_arrow (env, env->line_arrows[0]);
    bs[0]= typeset_as_concat (env, a, decorate (ip));
  }
  if (env->line_arrows[1] != "") {
    tree a= protect_arrow (env, env->line_arrows[1]);
    bs[1]= typeset_as_concat (env, a, decorate (ip));
  }
  return bs;
}

void
concater_rep::typeset_line (tree t, path ip, bool close) {
BEGIN_MAGNIFY
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
  if (N(a) == 0 || N(a[0]) == 0) typeset_error (t, ip);
  else {
    if (N(a) == 1) {
      a << copy (a[0]);
      cip << cip[0];
    }
    curve c= env->fr (poly_segment (a, cip));
    print (curve_box (ip, c, env->lw, env->col,
                      env->dash_style, env->dash_style_unit,
                      env->fill_mode, env->fill_color,
                      typeset_line_arrows (ip)));
  }
END_MAGNIFY
}

void
concater_rep::typeset_arc (tree t, path ip, bool close) {
BEGIN_MAGNIFY
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->as_point (env->exec (t[i]));
  array<path> cip(n);
  for (i=0; i<n; i++)
    cip[i]= descend (ip, i);
  if (N(a) == 0 || N(a[0]) == 0) typeset_error (t, ip);
  else {
    if (n != 3 || linearly_dependent (a[0], a[1], a[2]) ||
        (N (intersection (midperp (a[0], a[1], a[2]),
                          midperp (a[1], a[2], a[0]))) == 0))
      typeset_line (t, ip, close);
    else {
      curve c= env->fr (arc (a, cip, close));
      print (curve_box (ip, c, env->lw, env->col,
                        env->dash_style, env->dash_style_unit,
                        env->fill_mode, env->fill_color,
                        typeset_line_arrows (ip)));
    }
  }
END_MAGNIFY
}

void
concater_rep::typeset_spline (tree t, path ip, bool close) {
BEGIN_MAGNIFY
  int i, n= N(t);
  array<point> a(n);
  for (i=0; i<n; i++)
    a[i]= env->as_point (env->exec (t[i]));
  array<path> cip(n);
  for (i=0; i<n; i++)
    cip[i]= descend (ip, i);
  if (N(a) == 0 || N(a[0]) == 0) typeset_error (t, ip);
  else {
    if (N(a) == 1) {
      a << copy (a[0]);
      cip << cip[0];
    }
    curve c= env->fr (N(a)>=3 ? spline (a, cip, close): poly_segment (a, cip));
    print (curve_box (ip, c, env->lw, env->col,
                      env->dash_style, env->dash_style_unit,
                      env->fill_mode, env->fill_color,
                      typeset_line_arrows (ip)));
  }
END_MAGNIFY
}

void
concater_rep::typeset_var_spline (tree t, path ip) {
  (void) t; (void) ip;
  print (test_box (ip));
}

void
concater_rep::typeset_cspline (tree t, path ip) {
  typeset_spline (t, ip, true);
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
hashset<string> graphical_modified;

void
set_graphical_value (tree var, tree val) {
  //cout << "Set " << var << " := " << val << "\n";
  if (is_atomic (var))
    graphical_values (var->label)= val;
}

bool
has_graphical_value (tree var) {
  //cout << "Has " << var << "?\n";
  return is_atomic (var) && graphical_values->contains (var->label);
}

tree
get_graphical_value (tree var) {
  ASSERT (has_graphical_value (var), "invalid graphical id");
  //cout << "Get " << var << " = " << graphical_values [var->label] << "\n";
  return graphical_values [var->label];
}

bool
graphics_needs_update () {
  return N(graphical_modified) > 0;
}

void
graphics_require_update (tree var) {
  if (is_atomic (var))
    graphical_modified->insert (var->label);
  //cout << "Set " << var << ", " << N(graphical_modified) << "\n";
}

void
graphics_notify_update (tree var) {
  if (is_atomic (var))
    graphical_modified->remove (var->label);
  //cout << "Reset " << var << ", " << N(graphical_modified) << "\n";
}

static void
set_graphical_values (tree t) {
  if (is_atomic (t));
  else if (is_func (t, WITH)) {
    for (int i=0; i<N(t)-1; i+=2)
      if (t[i] == GID && is_atomic (t[i+1]))
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
	cout << "Not yet implemented 'is-intersection'\n";
        break;
      case ON_CURVE:
	cout << "Not yet implemented 'on-curve'\n";
        break;
      case ON_TEXT_BORDER:
	cout << "Not yet implemented 'on-text-border'\n";
        break;
      case ON_GRID:
	cout << "Not yet implemented 'on-grid'\n";
        break;
      default:
        break;
      }
    }

  for (i=0; i<n; i++)
    if (the_drd->get_type (t[i]) != TYPE_CONSTRAINT && !is_atomic (t[i]))
      bs << typeset_as_atomic (env, t[i], descend (ip, i));
}
