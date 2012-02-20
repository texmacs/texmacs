
/******************************************************************************
* MODULE     : poly_line.cpp
* DESCRIPTION: Poly lines
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "poly_line.hpp"

inline double square (double x) { return x*x; }

/******************************************************************************
* Extra routines for points
******************************************************************************/

double
distance (point p, point q) {
  ASSERT (N(p) == N(q), "unequal lengths");
  double s= 0.0;
  for (int i=0; i<N(p); i++)
    s += square (q[i] - p[i]);
  return sqrt (s);
}

point
project (point p, point q1, point q2) {
  ASSERT (N(p) == N(q1) && N(p) == N(q2), "unequal lengths");
  int i, n= N(p);
  double s= 0.0, t= 0.0;
  for (i=0; i<n; i++) {
    s += (q2[i] - q1[i]) * (p[i] - q1[i]);
    t += square (q2[i] - q1[i]);
  }
  double a= s / t;
  if (a < 0.0) return q1;
  if (a > 1.0) return q2;
  return q1 + a * (q2 - q1);
}

double
distance (point p, point q1, point q2) {
  if (q1 == q2) return distance (p, q1);
  else return distance (p, project (p, q1, q2));
}

point
inf (point p, point q) {
  ASSERT (N(p) == N(q), "unequal lengths");
  point r (N(p));
  for (int i=0; i<N(p); i++)
    r[i]= min (p[i], q[i]);
  return r;
}

point
sup (point p, point q) {
  ASSERT (N(p) == N(q), "unequal lengths");
  point r (N(p));
  for (int i=0; i<N(p); i++)
    r[i]= max (p[i], q[i]);
  return r;
}

/******************************************************************************
* Poly lines
******************************************************************************/

typedef array<point> poly_line;

double
distance (point p, poly_line pl) {
  double m= 1.0e10;
  if (N(pl) == 1) return distance (p, pl[0]);
  for (int i=0; i+1<N(pl); i++)
    m= min (m, distance (p, pl[i], pl[i+1]));
  return m;
}

bool
nearby (point p, poly_line pl) {
  return distance (p, pl) <= 5.0;
}

point
inf (array<point> a) {
  ASSERT (N(a) > 0, "non zero length expected");
  point p= a[0];
  for (int i=1; i<N(a); i++)
    p= inf (p, a[i]);
  return p;
}

point
sup (array<point> a) {
  ASSERT (N(a) > 0, "non zero length expected");
  point p= a[0];
  for (int i=1; i<N(a); i++)
    p= sup (p, a[i]);
  return p;
}
