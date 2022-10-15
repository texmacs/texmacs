
/******************************************************************************
* MODULE     : curve_extras.hpp
* DESCRIPTION: helper routines for the manipulation of curves
* COPYRIGHT  : (C) 2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "point.hpp"
#include "matrix.hpp"
#include "path.hpp"
#include "curve.hpp"

/******************************************************************************
* Approximations by poly-Bezier curves using least square fitting
* For this first method, each inner node comes with potentially distinct
* incoming and outgoing slopes
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
* Approximations by poly-Bezier curves using least square fitting
* For this second method, the incoming and outgoing slopes coincide
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

/******************************************************************************
* Gaussian smoothing
******************************************************************************/

array<point>
refine (array<point> a, int factor) {
  array<point> r;
  if (N(a) == 0) return r;
  r << a[0];
  for (int i=1; i<N(a); i++)
    for (int j=1; j<=factor; j++)
      r << a[i-1] + (((double) j)/((double) factor)) * (a[i] - a[i-1]);
  return r;
}

array<point>
smoothen (array<point> a, int width) {
  array<point> r;
  int cached_w= -1;
  array<double> coeffs;
  double weight;
  for (int i=0; i<N(a); i++) {
    int w= min (i, min (N(a)-1-i, width));
    if (w != cached_w) {
      double alpha= 3.0 / max (w * w, 1);
      coeffs= array<double> ();
      weight= 0.0;
      for (int j=0; j<=w; j++) {
        double c= exp (-alpha * j * j);
        coeffs << c;
        weight += c;
        if (j!=0) weight += c;
      }
      cached_w= w;
    }
    point cum (0.0, 0.0);
    for (int j=-w; j<=w; j++) {
      double c= coeffs [j>=0? j: -j];
      cum = cum + c * a[i+j];
    }
    r << cum / weight;
  }
  return r;
}

/******************************************************************************
* Calligraphy
******************************************************************************/

static double TWO_PI= 6.283185307179586;

double
get_phi (curve c, double t) {
  bool error= false;
  point g= c->grad (t, error);
  while (error) {
    t += 0.000001;
    if (t >= 1.0) t -= 1.0;
    g= c->grad (t, error);
  }
  if (g == point (0.0, 0.0)) return 0.0;
  double phi= atan2 (g[1], g[0]) / TWO_PI;
  return phi - floor (phi);
}

array<point>
angle_profile (curve c, int nr) {
  array<point> r;
  int steps= 5*nr;
  double t = 0.0;
  double dt= 1.0 / steps;
  double phi0= get_phi (c, t);
  double phi1= get_phi (c, t + dt);
  for (int i=0; i<nr; i++) {
    double psi= ((double) i) / ((double) nr);
    while (true) {
      if ((phi0 <= phi1 && (phi0 <= psi && psi < phi1)) ||
          (phi1 + 1.0e-6 < phi0 && ((phi0 <= psi && psi < phi1 + 1) ||
                                    (phi0 - 1 <= psi && psi < phi1)))) break;
      phi0= phi1;
      t += dt;
      if (t >= 1.0) t -= 1.0;
      phi1= get_phi (c, t + dt);
    }
    r << c->evaluate (t);
  }
  return r;
}

array<point>
oval_profile (double rx, double ry, double a, int nr) {
  array<point> oval;
  int n= 5*nr;
  double cos_a = cos (a);
  double sin_a = sin (a);
  for (int i=0; i<n; i++) {
    double phi= (TWO_PI * i) / n;
    double x  = rx * cos (phi);
    double y  = ry * sin (phi);
    double tx =  cos_a * x + sin_a * y;
    double ty = -sin_a * x + cos_a * y;
    oval << point (tx, ty);
  }
  curve oval_c= poly_segment (oval, array<path> ());
  return angle_profile (oval_c, nr);
}

array<point>
calligraphy (array<point> a, array<point> pen) {
  int p= N(pen);
  array<point> c;
  c << a;
  for (int i=N(a)-2; i>=0; i--) c << a[i];
  c << a[1];
  int prev_k= -1;
  double prev_phi;
  array<point> r;
  for (int i=0; i<N(c)-1; i++) {
    point c0= c[i];
    point c1= c[i+1];
    point dc= c1 - c0;
    if (c1 != c0) {
      double phi= atan2 (dc[1], dc[0]) / TWO_PI;
      phi= phi - floor (phi);
      int k= round (phi * p);
      if (k == p) k= 0;
      if (k != prev_k && prev_k != -1) {
        double dphi= phi - prev_phi;
        dphi= phi - round (phi);
        if (dphi >= 0.0 || dphi < 0.4999999) {
          int nr= k - prev_k;
          if (nr < 0) nr += p;
          for (int j=prev_k; j<(prev_k+nr); j++)
            r << c0 + pen[j<p? j: j-p];
        }
        else {
          int nr= prev_k - k;
          if (nr < 0) nr += p;
          for (int j=prev_k; j>(prev_k-nr); j--)
            r << c0 + pen[j>=0? j: j+p];
        }
      }
      r << c0 + pen[k];
      prev_k  = k;
      prev_phi= phi;
    }
  }
  return r;
}
