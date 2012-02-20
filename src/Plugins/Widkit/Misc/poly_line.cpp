
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
min (point p) {
  ASSERT (N(p) > 0, "non zero length expected");
  double r= p[0];
  for (int i=1; i<N(p); i++)
    r= min (r, p[i]);
  return r;
}

double
max (point p) {
  ASSERT (N(p) > 0, "non zero length expected");
  double r= p[0];
  for (int i=1; i<N(p); i++)
    r= max (r, p[i]);
  return r;
}

double
l2_norm (point p) {
  double s= 0.0;
  for (int i=0; i<N(p); i++)
    s += square (p[i]);
  return sqrt (s);
}

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

point
abs (point p) {
  point r (N(p));
  for (int i=0; i<N(p); i++)
    r[i]= (p[i] > 0? p[i]: -p[i]);
  return r;
}

/******************************************************************************
* Poly lines
******************************************************************************/

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
inf (poly_line pl) {
  ASSERT (N(pl) > 0, "non zero length expected");
  point p= pl[0];
  for (int i=1; i<N(pl); i++)
    p= inf (p, pl[i]);
  return p;
}

point
sup (poly_line pl) {
  ASSERT (N(pl) > 0, "non zero length expected");
  point p= pl[0];
  for (int i=1; i<N(pl); i++)
    p= sup (p, pl[i]);
  return p;
}

poly_line
operator + (poly_line pl, point p) {
  int i, n= N(pl);
  poly_line r (n);
  for (i=0; i<n; i++)
    r[i]= pl[i] + p;
  return r;
}

poly_line
operator - (poly_line pl, point p) {
  int i, n= N(pl);
  poly_line r (n);
  for (i=0; i<n; i++)
    r[i]= pl[i] - p;
  return r;
}

poly_line
operator * (double x, poly_line pl) {
  int i, n= N(pl);
  poly_line r (n);
  for (i=0; i<n; i++)
    r[i]= x * pl[i];
  return r;
}

poly_line
normalize (poly_line pl) {
  if (N(pl) == 0) return pl;
  pl= pl - inf (pl);
  if (N(pl) == 1) return pl;
  double sc= max (sup (pl));
  if (sc == 0.0) return pl;
  return (1.0 / sc) * pl;
}

double
length (poly_line pl) {
  double len= 0.0;
  for (int i=1; i<N(pl); i++)
    len += distance (pl[i], pl[i-1]);
  return len;
}

point
access (poly_line pl, double t) {
  for (int i=1; i<N(pl); i++) {
    point dp= pl[i] - pl[i-1];
    double len= l2_norm (dp);
    if (t < len) return pl[i-1] + (t / len) * dp;
    t -= len;
  }
  FAILED ("not on poly_line");
}

/******************************************************************************
* Contours
******************************************************************************/

double
distance (point p, contours gl) {
  double m= 1.0e10;
  for (int i=0; i<N(gl); i++)
    m= min (m, distance (p, gl[i]));
  return m;
}

bool
nearby (point p, contours gl) {
  return distance (p, gl) <= 5.0;
}

point
inf (contours gl) {
  ASSERT (N(gl) > 0, "non zero length expected");
  point p= inf (gl[0]);
  for (int i=1; i<N(gl); i++)
    p= inf (p, inf (gl[i]));
  return p;
}

point
sup (contours gl) {
  ASSERT (N(gl) > 0, "non zero length expected");
  point p= sup (gl[0]);
  for (int i=1; i<N(gl); i++)
    p= sup (p, sup (gl[i]));
  return p;
}

contours
operator + (contours gl, point p) {
  int i, n= N(gl);
  contours r (n);
  for (i=0; i<n; i++)
    r[i]= gl[i] + p;
  return r;
}

contours
operator - (contours gl, point p) {
  int i, n= N(gl);
  contours r (n);
  for (i=0; i<n; i++)
    r[i]= gl[i] - p;
  return r;
}

contours
operator * (double x, contours gl) {
  int i, n= N(gl);
  contours r (n);
  for (i=0; i<n; i++)
    r[i]= x * gl[i];
  return r;
}

contours
normalize (contours gl) {
  if (N(gl) == 0) return gl;
  gl= gl - inf (gl);
  double sc= max (sup (gl));
  if (sc == 0.0) return gl;
  gl= (1.0 / sc) * gl;
  return gl;
}

/******************************************************************************
* Associate invariants to glyphs
******************************************************************************/

array<int>
discrete_invariant (contours gl) {
  array<int> r;
  r << N(gl);
  return r;
}

array<double>
continuous_invariant (poly_line pl) {
  array<double> r;
  double l= length (pl);
  int pieces= 20;
  for (int i=0; i<=pieces; i++) {
    double t= (0.999999999 * i) / pieces;
    point  p= access (pl, t * l);
    r << p;
  }
  return r;
}

array<double>
continuous_invariant (contours gl) {
  gl= normalize (gl);
  array<double> r;
  for (int i=0; i<N(gl); i++)
    r << continuous_invariant (gl[i]);
  return r;
}
