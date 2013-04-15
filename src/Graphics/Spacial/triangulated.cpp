
/******************************************************************************
* MODULE     : triangulated.cpp
* DESCRIPTION: Triangulated spacial vector graphics
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "spacial.hpp"
#include "renderer.hpp"
#include "merge_sort.hpp"
#include "gui.hpp"
#include "true_color.hpp"

/******************************************************************************
* The abstract spacial class
******************************************************************************/

class triangulated_rep: public spacial_rep {
  bool sorted;
  array<triangle> ts;
  array<color>    cs;

public:
  triangulated_rep (array<triangle> ts2, array<color> cs2):
    sorted (false), ts (ts2), cs (cs2) {}

  spacial_kind get_type () { return spacial_triangulated; }
  void* get_handle () { return (void*) this; }

  void sort ();
  rectangle get_extents ();
  void draw (renderer ren);
  spacial transform (matrix<double> m);
  spacial enlighten (tree light);
};

spacial
triangulated (array<triangle> ts, array<color> cs) {
  ASSERT (N(ts) == N(cs), "array lengths do not match");
  return tm_new<triangulated_rep> (ts, cs);
}

/******************************************************************************
* Sorting the triangles by distance to the eye
******************************************************************************/

double
average_z (triangle t) {
  return (t[0][2] + t[1][2] + t[2][2]) / 3;
}

void
triangulated_rep::sort () {
  if (sorted) return;
  sorted= true;
  array<double> z (N(ts));
  for (int i=0; i<N(ts); i++)
    z[i]= average_z (ts[i]);
  array<int> p= merge_sort_leq_permutation<double> (z);
  ts= permute (ts, p);
  cs= permute (cs, p);
}

/******************************************************************************
* Extents, drawing and transforming the triangles
******************************************************************************/

rectangle
triangulated_rep::get_extents () {
  SI x1= MAX_SI, y1= MAX_SI, x2= -MAX_SI, y2= -MAX_SI;
  for (int i=0; i<N(ts); i++)
    for (int j=0; j<N(ts[i]); j++) {
      SI x= as_int (ts[i][j][0]);
      SI y= as_int (ts[i][j][1]);
      x1= min (x1, x); y1= min (y1, y);
      x2= max (x2, x); y2= max (y2, y);
    }
  return rectangle (x1, y1, x2, y2);
}

void
triangulated_rep::draw (renderer ren) {
  sort ();
  for (int i=0; i<N(ts); i++) {
    ren->set_brush (cs[i]);
    ren->draw_triangle (as_int (ts[i][0][0]), as_int (ts[i][0][1]),
                        as_int (ts[i][1][0]), as_int (ts[i][1][1]),
                        as_int (ts[i][2][0]), as_int (ts[i][2][1]));
  }
}

spacial
triangulated_rep::transform (matrix<double> m) {
  array<triangle> ts2 (N(ts));
  for (int i=0; i<N(ts); i++)
    ts2[i]= projective_apply (m, ts[i]);
  return triangulated (ts2, cs);  
}

/******************************************************************************
* Light
******************************************************************************/

point
cross (point p, point q) {
  point r (3);
  r[0]= p[1] * q[2] - p[2] * q[1];
  r[1]= p[2] * q[0] - p[0] * q[2];
  r[2]= p[0] * q[1] - p[1] * q[0];
  return r;
}

point
normal_vector (triangle t) {
  return cross (t[1] - t[0], t[2] - t[1]);
}

point
barycenter (triangle t) {
  return (t[0] + t[1] + t[2]) / 3.0;
}

array<color>
diffuse_light (array<triangle> ts, array<color> cs,
               point p, color shad, color sun)
{
  true_color col1 (shad);
  true_color col2 (sun);
  array<color> cs2 (N(cs));
  for (int i=0; i<N(cs); i++) {
    point  nv = normal_vector (ts[i]);
    point  bar= barycenter (ts[i]);
    point  lv = (N(p) == 4? p[3] * bar - range (p, 0, 3): bar - p);
    double npr= norm (nv) * norm (lv);
    double val= (npr == 0? 0.0: max (0.0, inner (nv, lv) / npr));
    true_color acol= mix (col1, 1.0 - val, col2, val);
    true_color ocol (cs[i]);
    true_color ncol= source_over (ocol, acol);
    cs2[i]= (color) ncol;
  }
  return cs2;
}

point
reflect (point p, point v) {
  double x= inner (p, v) / inner (v, v);
  return p - (2 * x) * v;
}

array<color>
specular_light (array<triangle> ts, array<color> cs,
                point p1, point p2, color c)
{
  true_color col (c);
  array<color> cs2 (N(cs));
  for (int i=0; i<N(cs); i++) {
    point  nv = normal_vector (ts[i]);
    point  bar= barycenter (ts[i]);
    point  lv1= (N(p1) == 4? p1[3] * bar - range (p1, 0, 3): bar - p1);
    point  lv2= (N(p2) == 4? p2[3] * bar - range (p2, 0, 3): bar - p2);
    if (inner (nv, nv) > 0) lv2= reflect (lv2, nv);
    double npr= norm (lv1) * norm (lv2);
    double a  = (npr == 0? 0.0: max (0.0, inner (lv1, lv2) / npr));
    a= a * a; a= a * a; a= a * a;
    true_color acol (col.r, col.g, col.b, col.a * a);
    true_color ocol (cs[i]);
    true_color ncol= source_over (ocol, acol);
    cs2[i]= (color) ncol;
  }
  return cs2;
}

spacial
triangulated_rep::enlighten (tree light) {
  array<color> cs2;
  (void) light;
  if (is_func (light, LIGHT_DIFFUSE, 3) &&
      is_func (light[0], _POINT) &&
      is_atomic (light[1]) &&
      is_atomic (light[2])) {
    point p = as_point (light[0]);
    color c1= named_color (as_string (light[1]));
    color c2= named_color (as_string (light[2]));
    cs2= diffuse_light (ts, cs, p, c1, c2);
  }
  else if (is_func (light, LIGHT_SPECULAR, 3) &&
           is_func (light[0], _POINT) &&
           is_func (light[1], _POINT) &&
           is_atomic (light[2])) {
    point p1= as_point (light[0]);
    point p2= as_point (light[1]);
    color c = named_color (as_string (light[2]));
    cs2= specular_light (ts, cs, p1, p2, c);
  }
  else cs2= copy (cs);
  return triangulated (ts, cs2);
}
