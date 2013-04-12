
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
