
/******************************************************************************
* MODULE     : matrix.cpp
* DESCRIPTION: matrixes
* COPYRIGHT  : (C) 2005  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "matrix.hpp"
#include "math_util.hpp"

matrix::matrix(int nl, int nc) {
  int i, j;
  a=array<array<double> >(nl);
  for (i=0;i<nl;i++) a[i]=array<double>(nc);
  for (i=0;i<nl;i++)
    for (j=0;j<nc;j++) a[i][j]=0;
}

matrix
operator + (matrix m1, matrix m2) {
  int i, j, nl= N(m1), nc= N(N(m1[0]));
  if (N(m2)!=nl || N(N(m2[0]))!=nc)
    fatal_error ("matrix::+");
  matrix r (nl, nc);
  for (i=0;i<nl;i++)
    for (j=0;j<nc;j++) r[i][j]= m1[i][j] + m2[i][j];
  return r;
}

matrix
operator * (matrix m1, matrix m2) {
  int i, j, k, nl= N(m1), nc= N(N(m1[0])), nc2= N(N(m2[0]));
  if (N(m2[0])!=nc || nc2==0)
    fatal_error ("matrix::*");
  matrix r (nl, nc2);
  for (i=0;i<nl;i++)
    for (j=0;j<nc2;j++) {
      double rij= 0;
      for (k=0;k<nc;k++) rij+= m1[i][k] * m2[k][j];
      r[i][j]= rij;
    }
  return r;
}

matrix
operator * (double c, matrix m) {
  int i, j, nl= N(m), nc= N(N(m[0]));
  matrix r (nl, nc);
  for (i=0;i<nl;i++)
    for (j=0;j<nc;j++) r[i][j]= c*m[i][j];
  return r;
}

point
matrix::operator () (point p) {
  int i, j, nl= N(a), nc= N(N(a[0]));
  if (N(p)!=nc)
    fatal_error ("matrix::()");
  point r (nl);
  for (i=0; i<nl; i++) {
    double ri= 0;
    for (j=0;j<nc;j++) {
      ri+= a[i][j]*p[j];
    }
    r[i]= ri;
  }
  return r;
}

matrix
as_matrix (tree t) {
  if (!is_tuple (t))
    fatal_error ("as_matrix");
  else {
    int i, j, nl= N(t), nc= N(t[0]);
    matrix r(nl, nc);
    for (i=0; i<nl; i++) {
      tree t2= t[i];
      for (j=0; j<nc; j++)
        r[i][j]= as_double (t2[j]);
    }
    return r;
  }
}

tree
as_tree (matrix m) {
  int i, j, nl= N(m), nc= N(N(m[0]));
  tree t (TUPLE, nl);
  for (i=0; i<nl; i++) {
    tree t2 (TUPLE, nc);
    for (j=0; j<nc; j++)
      t2[j]= as_string (m[i][j]);
    t[i]= t2;
  }
  return t;
}
