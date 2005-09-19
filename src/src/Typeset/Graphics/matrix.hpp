
/******************************************************************************
* MODULE     : matrix.hpp
* DESCRIPTION: matrixes
* COPYRIGHT  : (C) 2005  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MATRIX_H
#define MATRIX_H
#include "tree.hpp"
#include "point.hpp"

class matrix {
  array<array<double> > a;

public:
  inline matrix(): a(array<array<double> >(0)) {}
         matrix(int nl, int nc);
  inline ~matrix() {}
  inline array<double>& operator [] (int i) { return a[i]; }
  point operator () (point p);

  friend inline int N (matrix m) { return N(m.a); }
};

matrix operator + (matrix m1, matrix m2);
matrix operator * (matrix m1, matrix m2);
matrix operator * (double c, matrix m);

matrix as_matrix (tree t);
tree   as_tree (matrix p);

#endif // defined MATRIX_H
