
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
#include "matrix.hpp"

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
  env->update_color ();
  env->update_dash_style_unit ();
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
    bs[i]= typeset_as_atomic (env, t[i], descend (ip, i));
    //bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
    if (is_func (t[i], FREEZE))
      // FIXME: this dirty hack is necessary, because typeset_as_concat
      // would put an accessible concat_box around bs[i] otherwise
      bs[i]= frozen_box (decorate_middle (descend (ip, i)), bs[i]);
  }
  print (superpose_box (ip, bs));
}

bool
is_transformation (tree t) {
  if (is_tuple (t, "rotation", 1) &&
      is_double (t[1]))
    return true;
  if (is_tuple (t, "rotation", 2) &&
      is_func (t[1], _POINT, 2) &&
      is_double (t[1][0]) &&
      is_double (t[1][1]) &&
      is_double (t[2]))
    return true;
  if (is_tuple (t, "scaling", 2) &&
      is_double (t[1]) &&
      is_double (t[2]))
    return true;
  if (is_tuple (t, "slanting", 1) &&
      is_double (t[1]))
    return true;
  if (is_tuple (t, "linear", 4) &&
      is_double (t[1]) &&
      is_double (t[2]) &&
      is_double (t[3]) &&
      is_double (t[4]))
    return true;
  return false;
}

frame
get_transformation (tree t) {
  if (is_tuple (t, "rotation", 1))
    return rotation_2D (point (0.0, 0.0), as_double (t[1]) / 57.2957795131);
  if (is_tuple (t, "rotation", 2))
    return rotation_2D (as_point (t[1]), as_double (t[2]) / 57.2957795131);
  if (is_tuple (t, "scaling", 2))
    return scaling (point (as_double (t[1]), as_double (t[2])),
                    point (0.0, 0.0));
  if (is_tuple (t, "slanting", 1))
    return slanting (point (0.0, 0.0), as_double (t[1]));
  if (is_tuple (t, "linear", 4)) {
    matrix<double> m (0.0, 2, 2);
    m (0, 0) = as_double (t[1]);
    m (0, 1) = as_double (t[2]);
    m (1, 0) = as_double (t[3]);
    m (1, 1) = as_double (t[4]);
    return linear_2D (m);
  }
  failed_error << "t= " << t << LF;
  FAILED ("transformation expected");
  return frame ();
}

void
concater_rep::typeset_gr_transform (tree t, path ip) {
  if (N(t) != 2) typeset_error (t, ip);
  tree tr= env->exec (t[1]);
  if (!is_transformation (tr)) typeset_error (t, ip);
  else {
    frame f= get_transformation (tr);
    box   b= typeset_as_atomic (env, t[0], descend (ip, 0));
    print (transformed_box (ip, b, f));
  }
}

void
concater_rep::typeset_gr_effect (tree t, path ip) {
  if (N(t) < 2) typeset_error (t, ip);
  array<box> bs (N(t)-1);
  for (int i=0; i<N(t)-1; i++)
    bs[i]= typeset_as_atomic (env, t[i], descend (ip, i));
  tree eff= env->exec (t[N(t)-1]);
  print (effect_box (ip, bs, eff));
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
      SI pad= env->get_length (TEXT_AT_MARGIN);
      print (text_at_box (ip, b, x, y, axis, pad));
    }
  }
END_MAGNIFY
}

void
concater_rep::typeset_math_at (tree t, path ip) {
BEGIN_MAGNIFY
  if (N(t) != 2) typeset_error (t, ip);
  else {
    // FIXME: attaching an ip to compound ("math", t[0]) is a bit hacky,
    // but it seems to work fine for the time being
    box    b     = typeset_as_concat (env, compound ("math", t[0]), ip);
    point  p     = env->fr (env->as_point (env->exec (t[1])));
    string halign= env->text_at_halign;
    string valign= env->text_at_valign;

    if (N(p) == 0)
      typeset_dynamic (tree (ERROR, "bad math-at"), ip);
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
concater_rep::typeset_document_at (tree t, path ip) {
BEGIN_MAGNIFY
  if (N(t) != 2) typeset_error (t, ip);
  else {
    // FIXME: attaching ip to compound ("paragraph-box", t[0]) is a bit hacky,
    // but it seems to work fine for the time being
    box    b= typeset_as_concat (env, compound ("paragraph-box", t[0]), ip);
    point  p= env->fr (env->as_point (env->exec (t[1])));
    string halign= env->text_at_halign;
    string valign= env->doc_at_valign;

    if (N(p) == 0)
      typeset_dynamic (tree (ERROR, "bad document-at"), ip);
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
    pencil pen= env->pen->set_width (env->point_border);
    print (point_box (ip, p, env->point_size, pen,
                      env->fill_brush, env->point_style));
  }
END_MAGNIFY
}

static tree
protect_arrow (edit_env env, tree t) {
  (void) env;
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
    print (curve_box (ip, c, env->line_portion, env->pen,
                      env->dash_style, env->dash_motif, env->dash_style_unit,
                      env->fill_brush, typeset_line_arrows (ip)));
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
      print (curve_box (ip, c, env->line_portion, env->pen,
                        env->dash_style, env->dash_motif, env->dash_style_unit,
                        env->fill_brush, typeset_line_arrows (ip)));
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
    print (curve_box (ip, c, env->line_portion, env->pen,
                      env->dash_style, env->dash_motif, env->dash_style_unit,
                      env->fill_brush, typeset_line_arrows (ip)));
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
concater_rep::typeset_bezier (tree t, path ip) {
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
    curve c;
    if (N(a) < 3) c= poly_segment (a, cip);
    else {
      bool simple= is_func (t, SMOOTH) || is_func (t, CSMOOTH);
      bool closed= is_func (t, CBEZIER) || is_func (t, CSMOOTH);
      c= poly_bezier (a, cip, simple, closed);
    }
    c= env->fr (c);
    print (curve_box (ip, c, env->line_portion, env->pen,
                      env->dash_style, env->dash_motif, env->dash_style_unit,
                      env->fill_brush, typeset_line_arrows (ip)));
  }
END_MAGNIFY
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

box
typeset_gr_item (edit_env env, tree t, path ip, bool& ok) {
  ok= false;
  if (is_func (t, WITH)) {
    int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
    if ((n&1) != 1) return empty_box (ip);

    STACK_NEW_ARRAY(vars,string,k);
    STACK_NEW_ARRAY(oldv,tree,k);
    STACK_NEW_ARRAY(newv,tree,k);
    for (i=0; i<k; i++) {
      tree var_t= env->exec (t[i<<1]);
      if (is_atomic (var_t)) {
        string var= var_t->label;
        vars[i]= var;
        oldv[i]= env->read (var);
        newv[i]= env->exec (t[(i<<1)+1]);
        if (var == PROVISO && newv[i] == "false") {
          STACK_DELETE_ARRAY(vars);
          STACK_DELETE_ARRAY(oldv);
          STACK_DELETE_ARRAY(newv);
          return empty_box (ip);
        }
      }
      else {
        STACK_DELETE_ARRAY(vars);
        STACK_DELETE_ARRAY(oldv);
        STACK_DELETE_ARRAY(newv);
        return empty_box (ip);
      }
    }

    // for (i=0; i<k; i++) env->monitored_write_update (vars[i], newv[i]);
    for (i=0; i<k; i++) env->write_update (vars[i], newv[i]);
    box b= typeset_as_atomic (env, t[n-1], descend (ip, n-1));
    for (i=k-1; i>=0; i--) env->write_update (vars[i], oldv[i]);
    STACK_DELETE_ARRAY(vars);
    STACK_DELETE_ARRAY(oldv);
    STACK_DELETE_ARRAY(newv);
    ok= true;
    return b;
  }
  else if ((L(t) == ANIM_STATIC || L(t) == ANIM_DYNAMIC) &&
	   (N(t) >= 1 && is_func (t[0], MORPH))) {
    ok= true;
    for (int i=0; i<N(t[0]); i++)
      if (is_func (t[0][i], TUPLE, 2) && is_func (t[0][i][1], WITH)) {
        tree w= t[0][i][1];
        for (int j=0; j+1<N(w); j++) {
          if (w[j] == PROVISO && env->exec (w[j+1]) == "false") {
            ok= false;
            break;
          }
        }
      }
    if (!ok) return empty_box (ip);
    return typeset_as_atomic (env, t, ip);
  }
  else {
    ok= true;
    return typeset_as_atomic (env, t, ip);
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
    if (the_drd->get_type (t[i]) != TYPE_CONSTRAINT && !is_atomic (t[i])) {
      bool ok;
      box b= typeset_gr_item (env, t[i], descend (ip, i), ok);
      if (ok) bs << b;
    }
}

/******************************************************************************
* 3D graphics
******************************************************************************/

static point
as_point_3d (tree t) {
  if (is_func (t, _POINT, 3) && is_point (t)) return as_point (t);
  else return point ();
}

static triangle
as_triangle_3d (tree t, color& col) {
  if (is_func (t, TRIANGLE_3D, 4)) {
    point p1= as_point_3d (t[0]);
    point p2= as_point_3d (t[1]);
    point p3= as_point_3d (t[2]);
    if (N(p1) == 0 || N(p2) == 0 || N(p3) == 0) return triangle ();
    col= named_color (as_string (t[3]));
    return triangle (p1, p2, p3);
  }
  else return triangle ();
}

static spacial
as_spacial (tree t) {
  if (is_func (t, TRANSFORM_3D, 2)) {
    spacial obj= as_spacial (t[0]);
    matrix<double> m= as_matrix<double> (t[1]);
    return transformed (obj, m);
  }
  else if (is_func (t, LIGHT_3D, 2)) {
    spacial obj= as_spacial (t[0]);
    return enlightened (obj, t[1]);
  }
  else if (is_func (t, OBJECT_3D)) {
    array<triangle> ts;
    array<color> cs;
    for (int i=0; i<N(t); i++) {
      color col;
      triangle tri= as_triangle_3d (t[i], col);
      if (N(tri) == 0) return spacial ();
      ts << tri;
      cs << col;
    }
    return triangulated (ts, cs);
  }
  else if (is_func (t, TRIANGLE_3D, 4)) {
    color col;
    triangle tri= as_triangle_3d (t, col);
    if (N(tri) == 0) return spacial ();
    array<triangle> ts;
    array<color> cs;
    ts << tri;
    cs << col;
    return triangulated (ts, cs);
  }
  else return spacial ();
}

void
concater_rep::typeset_graphics_3d (tree t, path ip) {
BEGIN_MAGNIFY
  point o = env->fr (point (0.0, 0.0));
  point ux= env->fr (point (1.0, 0.0));
  point uy= env->fr (point (0.0, 1.0));
  matrix<double> vt (0.0, 4, 4);
  vt (0, 0)= ux[0] - o[0];
  vt (0, 1)= ux[1] - o[1];
  vt (1, 0)= uy[0] - o[0];
  vt (1, 1)= uy[1] - o[1];
  vt (2, 2)= 1.0;
  vt (3, 3)= 1.0;
  vt (0, 3)= o[0];
  vt (1, 3)= o[1];
  vt= vt * as_matrix<double> (env->read (GR_TRANSFORMATION));
  tree u= env->exec (t);
  spacial obj= as_spacial (u);
  if (is_nil (obj))
    typeset_dynamic (tree (ERROR, "bad spacial object"), ip);
  else
    print (spacial_box (ip, transformed (obj, vt)));
END_MAGNIFY
}
