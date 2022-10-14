
/******************************************************************************
* MODULE     : bezier.hpp
* DESCRIPTION: least square approximations by poly-Bezier curves
* COPYRIGHT  : (C) 2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "point.hpp"
#include "matrix.hpp"

/******************************************************************************
* For the first fitting method, each inner node comes with potentially
* distinct incoming and outgoing slopes
******************************************************************************/

matrix<double>
bezier_matrix (int segments, array<double> times) {
  int rows= N(times), cols= 3*segments + 1;
  matrix<double> m (0.0, rows, cols);
  for (int i=0; i<rows; i++) {
    double t= times[i];
    int j= max (0, min (segments - 1, (int) floor (segments * t)));
    double u = max (0.0, min (1.0, segments * t - j));
    double v = 1.0 - u;
    double u2= u*u;
    double v2= v*v;
    m (i, 3*j)   =       v  * v2;
    m (i, 3*j+1) = 3.0 * u  * v2;
    m (i, 3*j+2) = 3.0 * u2 * v ;
    m (i, 3*j+3) =       u2 * u ;
  }
  return m;
}

array<point>
bezier_fit (array<point> a, int segments, array<double> times) {
  ASSERT (N(a) == N(times) && N(a) > 0, "invalid number of points");
  int dim= N(a[0]), nr= N(times);
  matrix<double> m  = bezier_matrix (segments, times);
  matrix<double> tm = transpose (m);
  matrix<double> cov= invert (tm * m);
  matrix<double> b (0.0, nr, dim);
  for (int i=0; i<nr; i++ ) {
    ASSERT (N(a[i]) == dim, "dimension mismatch");
    for (int j=0; j<dim; j++) b (i, j)= a[i][j];
  }
  matrix<double> sol= cov * (tm * b);
  array<point> r;
  for (int i=0; i<NR(sol); i++) {
    point x;
    for (int j=0; j<NC(sol); j++) x << sol (i, j);
    r << x;
  }
  return r;
}

array<point>
bezier_fit (array<point> a, int pack_size) {
  ASSERT (N(a) > 0, "invalid number of points");
  ASSERT (pack_size >= 7, "invalid pack size");
  if (N(a) <= 3) {
    array<point> r;
    r << a[0];
    if (N(a) == 1) r << a[0] << a[0] << a[0];
    else if (N(a) == 2) {
      point m= 0.5 * (a[0] + a[1]);
      r << m << m << a[1];
    }
    else r << a[1] << a[1] << a[2];
    return r;
  }

  int segments= (N(a) + pack_size - 1) / pack_size;
  array<double> times;
  for (int i=0; i<N(a); i++)
    times << (1.0 * i) / (N(a) - 1.0);
  return bezier_fit (a, segments, times);
}

/******************************************************************************
* For the second fitting method, the incoming and outgoing slopes coincide
* for each inner node
******************************************************************************/

matrix<double>
alt_bezier_matrix (int segments, array<double> times) {
  int rows= N(times), cols= 2*segments + 2;
  matrix<double> m (0.0, rows, cols);
  for (int i=0; i<rows; i++) {
    double t= times[i];
    int j= max (0, min (segments - 1, (int) floor (segments * t)));
    double u = max (0.0, min (1.0, segments * t - j));
    double v = 1.0 - u;
    double u2= u*u;
    double v2= v*v;
    m (i, 2*j  ) =        v  * v2;
    m (i, 2*j+1) =  3.0 * u  * v2;
    m (i, 2*j+3) = -3.0 * u2 * v ;
    m (i, 2*j+2) =        u2 * u + 6.0 * u2 * v;
  }
  return m;
}

array<point>
alt_bezier_fit (array<point> a, int segments, array<double> times) {
  ASSERT (N(a) == N(times) && N(a) > 0, "invalid number of points");
  int dim= N(a[0]), nr= N(times);
  matrix<double> m  = alt_bezier_matrix (segments, times);
  matrix<double> tm = transpose (m);
  matrix<double> cov= invert (tm * m);
  matrix<double> b (0.0, nr, dim);
  for (int i=0; i<nr; i++ ) {
    ASSERT (N(a[i]) == dim, "dimension mismatch");
    for (int j=0; j<dim; j++) b (i, j)= a[i][j];
  }
  matrix<double> sol= cov * (tm * b);
  array<point> alt_r;
  for (int i=0; i<NR(sol); i++) {
    point x;
    for (int j=0; j<NC(sol); j++) x << sol (i, j);
    alt_r << x;
  }
  array<point> r;
  for (int i=0; i<segments; i++) {
    r << alt_r[2*i];
    r << alt_r[2*i+1];
    r << 2*alt_r[2*i+2] - alt_r[2*i+3];
  }
  r << alt_r[2*segments];
  return r;
}

array<point>
alt_bezier_fit (array<point> a, int pack_size) {
  ASSERT (N(a) > 0, "invalid number of points");
  ASSERT (pack_size >= 5, "invalid pack size");
  if (N(a) <= 3) {
    array<point> r;
    r << a[0];
    if (N(a) == 1) r << a[0] << a[0] << a[0];
    else if (N(a) == 2) {
      point m= 0.5 * (a[0] + a[1]);
      r << m << m << a[1];
    }
    else r << a[1] << a[1] << a[2];
    return r;
  }

  int segments= (N(a) + pack_size - 1) / pack_size;
  array<double> times;
  for (int i=0; i<N(a); i++)
    times << (1.0 * i) / (N(a) - 1.0);
  return alt_bezier_fit (a, segments, times);
}
