
/******************************************************************************
* MODULE     : point.cpp
* DESCRIPTION: points
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "point.hpp"
#include <math.h>
#include "math_util.hpp"

point
operator - (point p) {
  int i, n= N(p);
  point r (n);
  for (i=0; i<n; i++)
    r[i]= - p[i];
  return r;
}

point
operator + (point p1, point p2) {
  int i, n= min (N(p1), N(p2));
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] + p2[i];
  return r;
}

point
operator - (point p1, point p2) {
  int i, n= min (N(p1), N(p2));
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] - p2[i];
  return r;
}

point
operator * (double x, point p) {
  int i, n= N(p);
  point r (n);
  for (i=0; i<n; i++)
    r[i]= x * p[i];
  return r;
}

point
operator / (point p, double x) {
  int i, n= N(p);
  point r (n);
  for (i=0; i<n; i++)
    r[i]= p[i] / x;
  return r;
}

point
as_point (tree t) {
  if (!is_tuple (t)) return point ();
  else {
    int i, n= N(t);
    point p(n);
    for (i=0; i<n; i++)
      p[i]= as_double (t[i]);
    return p;
  }
}

tree
as_tree (point p) {
  int i, n= N(p);
  tree t (TUPLE, n);
  for (i=0; i<n; i++)
    t[i]= as_string (p[i]);
  return t;
}

double
operator * (point p1, point p2) {
  int i, n= min (N(p1), N(p2));
  double r= 0;
  for (i=0; i<n; i++)
    r+= p1[i] * p2[i];
  return r;
}

double
norm (point p) {
  return sqrt (p*p);
}

double
arg (point p) {
  double n= norm(p);
  p=p/n;
  if (p[1]<0) return 2*tm_PI-acos(p[0]);
  else return acos(p[0]);
}

point
proj (axis ax, point p) {
  int i, n= min (N(ax.p0), N(ax.p1));
  point a (n), b (n);
  for (i=0; i<n ; i++) {
    a[i]= ax.p1[i] - ax.p0[i];
    b[i]= ax.p0[i];
  }
  if (norm (a) < 1.0e-6)
    return ax.p0;
  else
    return b + ((a*p - a*b) / (a*a)) * a;
}

double
dist (axis ax, point p) {
  return norm (p - proj (ax, p));
}

double
seg_dist (axis ax, point p) {
  point ab= ax.p1 - ax.p0;
  point ba= ax.p0 - ax.p1;
  point ap= p - ax.p0;
  point bp= p - ax.p1;
  if (ab * ap > 0 && ba * bp > 0)
    return dist (ax, p);
  else
    return min (norm (ap), norm (bp));
}

bool
collinear (point p1, point p2) {
  return fnull (fabs (p1*p2) - norm(p1)*norm(p2), 1.0e-6);
}

bool
linearly_dependent (point p1, point p2, point p3) {
  return fnull (norm (p1-p2), 1e-6) ||
	 fnull (norm (p2-p3), 1e-6) ||
	 fnull (norm (p3-p1), 1e-6) ||
	 collinear (p2-p1, p3-p1);
}

bool orthogonalize (point &i, point &j, point p1, point p2, point p3) {
  if (linearly_dependent (p1, p2, p3)) return false;
  i= (p2-p1) / norm (p2-p1);
  j= (p3-p1) - ((p3-p1) * i) * i;
  j= j / norm (j);
  return true;
}

axis
midperp (point p1, point p2, point p3) {
  axis a;
  if (linearly_dependent (p1, p2, p3))
    a.p0= a.p1= point (0);
  else {
    point i, j;
    orthogonalize (i, j, p1, p2, p3);
    a.p0= (p1+p2) / 2;
    a.p1= a.p0 + j;
  }
  return a;
}

point
intersect (axis A, axis B) {
  point i, j;
  if (!orthogonalize (i, j, A.p0, A.p1, B.p0)) {
    if (orthogonalize (i, j, A.p0, A.p1, B.p1))
      return B.p0;
    else
      return point (0);
  }
  point a(2), b(2), b2 (2), u(2), v(2), p(2);
  a[0]= a[1]= 0;
  u[0]= (A.p1 - A.p0) * i;
  u[1]= (A.p1 - A.p0) * j;
  b[0]= (B.p0 - A.p0) * i;
  b[1]= (B.p0 - A.p0) * j;
  v[0]= (B.p1 - B.p0) * i;
  v[1]= (B.p1 - B.p0) * j;
  b2[0]= (B.p1 - A.p0) * i;
  b2[1]= (B.p1 - A.p0) * j;
  if (fnull (norm (B.p1 - (b2[0]*i + b2[1]*j)), 1e-6))
    return point (0);
  if (fnull (norm (u), 1e-6) ||
      fnull (norm (v), 1e-6) ||
      collinear (u, v))
    return point (0);
  else {
    double t;
    t= (v[0] * (b[1]-a[1]) + v[1] * (a[0]-b[0]))
       /
       (v[0]*u[1] - v[1]*u[0]);
    return A.p0 + t * (u[0]*i + u[1]*j);
  }
}
