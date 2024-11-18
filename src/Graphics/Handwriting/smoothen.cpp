
/******************************************************************************
* MODULE     : smoothen.cpp
* DESCRIPTION: Smoothening handwritten curves
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "point.hpp"

/******************************************************************************
* Remove superfluous points
******************************************************************************/

static void
extract (array<point>& r, array<point> a, array<int> prev, int i) {
  if (prev[i] != -1) extract (r, a, prev, prev[i]);
  r << a[i];
}

array<point>
simplify (array<point> a, double eps, double thr) {
  if (N(a) <= 2) return a;
  array<int>    prev_i;
  array<double> prev_d;
  prev_i << -1;
  prev_d << 0.0;
  int count= 0; (void) count;
  for (int i=1; i<N(a); i++) {
    prev_i << -1;
    prev_d << 1.0e100;
    for (int j=i-1; j>=0; j--) {
      count++;
      if (norm (a[i] - a[j]) > thr) break;
      else if (norm (a[i] - a[j]) < eps) continue;
      else if (prev_d[j] >= prev_d[i]) continue;
      else {
        double s= 0.0;
        for (int k=j+1; k<i; k++)
          s += seg_dist (a[i], a[j], a[k]);
        if (prev_d[j] + s < prev_d[i]) {
          prev_i[i]= j;
          prev_d[i]= prev_d[j] + s;
        }
      }
    }
  }
  if (prev_i[N(a)-1] == -1) return a;
  array<point> r;
  extract (r, a, prev_i, N(a) - 1);
  //cout << count << "\n";
  return r;
}
