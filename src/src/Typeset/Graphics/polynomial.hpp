
/******************************************************************************
* MODULE     : polynomial.hpp
* DESCRIPTION: polynomials
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef POLYNOMIAL_H
#define POLYNOMIAL_H
#include "tree.hpp"
#include "point.hpp"

class polynomial {
  array<double> a;

public:
  inline polynomial(): a(array<double>(0)) {}
         polynomial(int n);
  inline ~polynomial() {}
  inline double& operator [] (int i) { return a[i]; }
  double operator () (double x,int o=0);

  friend inline int N (polynomial p) { return N(p.a); }
};

polynomial operator + (polynomial p1, polynomial p2);
polynomial operator * (double c, polynomial p);

class polynomials {
  array<polynomial> a;

public:
  inline polynomials(): a(array<polynomial>(0)) {}
         polynomials(int n);
  inline ~polynomials() {}
  inline polynomial& operator [] (int i) { return a[i]; }
  point operator () (double x,int o=0);

  friend inline int N (polynomials p) { return N(p.a); }
};

polynomials operator + (polynomials p1, polynomials p2);
polynomials operator * (double c, polynomials p);
polynomials operator * (point c, polynomial p);

point coeffs(polynomials p,int i);

#endif // defined POLYNOMIAL_H
