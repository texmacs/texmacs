
/******************************************************************************
* MODULE     : polynomial.cpp
* DESCRIPTION: polynomials
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "polynomial.hpp"
#include "math_util.hpp"

// Util
double
prod (double x, int n) {
  double r;
  if (n<=0) return 1;
  r=x; n--;
  while (n--)
    r*=(x-n);
  return r;
}

// Polynomials
polynomial::polynomial(int n) {
  int i;
  a=array<double>(n+1);
  for (i=0;i<=n;i++) a[i]=0;
}

polynomial
operator + (polynomial p1, polynomial p2) {
  int i, n= min (N(p1), N(p2));
  polynomial r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] + p2[i];
  return r;
}

polynomial
operator * (double c, polynomial p) {
  int i, n= N(p);
  polynomial r (n);
  for (i=0;i<n;i++) {
    r[i]= c*p[i];
  }
  return r;
}

double
polynomial::operator () (double x,int o) {
  int i,n=N(a);
  double r=0;
  if (n==0) return 0;
  for (i=o; i<n; i++)
    r += prod(i,o)*a[i]*pow(x,i-o);
  return r;
}

polynomials::polynomials(int n) {
  int i;
  a=array<polynomial>(n);
  for (i=0;i<n;i++) a[i]=polynomial();
}

point
polynomials::operator () (double x,int o) {
  int i, n= N(a);
  point r (n);
  for (i=0;i<n;i++) {
    r[i]= a[i](x,o);
  }
  return r;
}

polynomials
operator + (polynomials p1, polynomials p2) {
  int i, n= min (N(p1), N(p2));
  polynomials r (n);
  for (i=0; i<n; i++)
    r[i]= p1[i] + p2[i];
  return r;
}

polynomials
operator * (double c, polynomials p) {
  int i, n= N(p);
  polynomials r (n);
  for (i=0;i<n;i++)
    r[i]= c*p[i];
  return r;
}

polynomials
operator * (point c, polynomial p) {
  int i, n= N(c);
  polynomials r (n);
  for (i=0;i<n;i++)
    r[i]= c[i]*p;
  return r;
}

point
coeffs (polynomials p, int i) {
  int j,n;
  point r(n=N(p));
  for (j=0;j<n;j++) {
    r[j]=0;
    if (i<N(p[j])) r[j]=p[j][i];
  }
  return r;
}
