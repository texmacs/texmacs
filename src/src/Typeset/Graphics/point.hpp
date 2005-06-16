
/******************************************************************************
* MODULE     : point.hpp
* DESCRIPTION: points
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef POINT_H
#define POINT_H
#include "tree.hpp"

typedef array<double> point;

point operator - (point p);
point operator + (point p1, point p2);
point operator - (point p1, point p2);
point operator * (double x, point p);
point operator / (point p, double x);

inline point as_point(double x) {
  point p(1); p[0]=x; return p; }
point as_point (tree t);
tree  as_tree (point p);

double operator * (point p1, point p2);
point  rotate_2D (point p, point o, double angle);

double norm (point p);
double arg (point p);
bool   collinear (point p1, point p2);
bool   linearly_dependent (point p1, point p2, point p3);
bool   orthogonalize (point &i, point &j, point p1, point p2, point p3);

typedef struct { point p0, p1; } axis;

point  proj      (axis a, point p);
double dist      (axis a, point p);
double seg_dist  (axis a, point p);
axis   midperp   (point p1, point p2, point p3);
point  intersect (axis A, axis B);

bool inside_rectangle (point p, point p1, point p2);

#endif // defined POINT_H
