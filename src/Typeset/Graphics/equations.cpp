
/******************************************************************************
* MODULE     : equations.cpp
* DESCRIPTION: Some tools for solving systems of equations
* COPYRIGHT  : (C) 2004  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "point.hpp"
#include "math_util.hpp"

// Tridiagonal & quasi tridiagonal systems
void
tridiag_solve (array<double> a, array<double> b, array<double> c,
	       array<point> x, array<point> y, int n)
{
  array<double> u(n);
  int i;
  double li;
  li= b[0];
  ASSERT (b[0] != 0, "failed tridiag_solve (1)");
  x[0]= y[0]/li;
  for (i=0; i<n-1; i++) {
     u[i]= c[i]/li;
     li= b[i+1] - a[i+1]*u[i];
     ASSERT (li != 0, "failed tridiag_solve (2)");
     x[i+1]= (y[i+1] - a[i+1]*x[i]) / li;
  }
  for (i=n-2; i>=0; i--) {
     x[i]= x[i] - u[i]*x[i+1];
  }
}

void
quasitridiag_solve (array<double> a, array<double> b, array<double> c,
		    array<double> u, array<double> v,
		    array<point> x, array<point> y, int n)
{
  int i;
  array<point> z(n), up(n);
  for (i=0; i<n; i++) up[i]= as_point (u[i]);
  tridiag_solve (a, b, c, x, y, n);
  tridiag_solve (a, b, c, z, up, n);
  point vx;
  vx= v[0]*x[0];
  for (i=1; i<n; i++) vx= vx + v[i]*x[i];
  double vz;
  vz= v[0]*z[0][0];
  for (i=1; i<n; i++) vz+= v[i]*z[i][0];
  vx= vx / (1+vz);
  for (i=0; i<n; i++) {
     x[i]= x[i] - z[i][0]*vx;
  }
}

void
xtridiag_solve (array<double> a, array<double> b, array<double> c,
		double a0, double a1,
		array<point> x, array<point> y, int n)
{
  array<double> u(n), v(n);
  int i;
  for (i=0; i<n; i++) u[i]= v[i]= 0;
  u[0]= u[n-1]= 1;
  v[0]= a0;
  v[n-1]= a1;
  b[0]-= a0;
  b[n-1]-= a1;
  quasitridiag_solve (a, b, c, u, v, x, y, n);
  b[0]+= a0;
  b[n-1]+= a1;
}
