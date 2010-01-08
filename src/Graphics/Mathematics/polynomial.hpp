
/******************************************************************************
* MODULE     : polynomial.hpp
* DESCRIPTION: polynomials with reference counting and pointer copying
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef POLYNOMIAL_H
#define POLYNOMIAL_H
#include "vector.hpp"
#define TMPL template<typename T>
#define BINARY_TMPL template<typename T, typename U>
#define R typename unary_properties<T>::norm_type
#define M typename binary_properties<T,U>::product_type

TMPL class polynomial;
TMPL int N (polynomial<T> a);
TMPL T* A (polynomial<T> a);

/******************************************************************************
* The polynomial class
******************************************************************************/

TMPL
class polynomial_rep: concrete_struct {
  int n;
  T* a;
public:
  inline polynomial_rep (T* a2, int n2): n(n2), a(a2) {
    while (n > 0 && a[n-1] == 0) n--; }
  inline ~polynomial_rep () { if (a != NULL) tm_delete_array (a); }
  friend class polynomial<T>;
  friend int N LESSGTR (polynomial<T> a);
  friend T* A LESSGTR (polynomial<T> a);
};

TMPL
class polynomial {
CONCRETE_TEMPLATE(polynomial,T);
  inline polynomial (T *a, int n):
    rep (tm_new<polynomial_rep<T> > (a, n)) {}
  inline polynomial (T c, int n) {
    T* a= (n == 0? NULL: tm_new_array<T> (n));
    for (int i=0; i<n; i++) a[i]= c;
    rep= tm_new<polynomial_rep<T> > (a, n); }
  inline polynomial () {
    rep= tm_new<polynomial_rep<T> > ((T*) NULL, 0); }
  inline polynomial (T c1) {
    T* a= tm_new_array<T> (1); a[0]= c1;
    rep= tm_new<polynomial_rep<T> > (a, 1); }
  inline polynomial (T c1, T c2) {
    T* a= tm_new_array<T> (2); a[0]= c1; a[1]= c2;
    rep= tm_new<polynomial_rep<T> > (a, 2); }
  inline polynomial (T c1, T c2, T c3) {
    T* a= tm_new_array<T> (3); a[0]= c1; a[1]= c2; a[2]= c3;
    rep= tm_new<polynomial_rep<T> > (a, 3); }
  inline T operator [] (int i) { return i<rep->n? rep->a[i]: T(0); }
  T operator () (T c);
  T operator () (T c, int order);
};
CONCRETE_TEMPLATE_CODE(polynomial,typename,T);

TMPL
class unary_properties<polynomial<T> > {
public:
  typedef T scalar_type;
  typedef typename unary_properties<T>::norm_type norm_type;
  typedef int index_type;
  static inline tree index_name (index_type i) {
    return tree (RSUB, "x", as_string (i+1)); }
  static inline scalar_type access (polynomial<T> p, index_type var) {
    return p[var]; }
};

BINARY_TMPL
class binary_properties<polynomial<T>,polynomial<U> > {
public:
  typedef polynomial<M > product_type;
};

BINARY_TMPL
class binary_properties<T,polynomial<U> > {
public:
  typedef polynomial<M > product_type;
};

BINARY_TMPL
class binary_properties<polynomial<T>,U > {
public:
  typedef polynomial<M > product_type;
};

/******************************************************************************
* Basic functions on polynomials
******************************************************************************/

TMPL inline int N (polynomial<T> p) { return p->n; }
TMPL inline T* A (polynomial<T> p) { return p->a; }
TMPL inline int degree (polynomial<T> p) { return N(p)-1; }

TMPL T
polynomial<T>::operator () (T c) {
  int i, n= rep->n;
  if (n == 0) return 0;
  T* a= rep->a;
  T sum= a[n-1];
  for (i=n-2; i>=0; i--)
    sum = c * sum + a[i];
  return sum;
}

TMPL T
polynomial<T>::operator () (T c, int order) {
  int i, n= rep->n;
  if (n <= order) return 0;
  T* a= rep->a;
  T prod= 1;
  for (i=n-order; i<n; i++) prod *= T(i);
  T sum= prod * a[n-1];
  for (i=n-2; i>=order; i--) {
    prod= (T(i+1-order) * prod) / T(i+1);
    sum = c * sum + prod * a[i];
  }
  return sum;
}

TMPL tree
as_tree (polynomial<T> p) {
  int i, n= N(p);
  if (n == 0) return "0";
  tree sum= as_tree (p[0]);
  for (i=1; i<n; i++)
    sum= add (sum, mul (as_tree (p[i]), pow (tree ("x"), as_tree (i))));
  return sum;
}

TMPL inline tm_ostream&
operator << (tm_ostream& out, polynomial<T> p) {
  return out << as_math_string (as_tree (p));
}

/******************************************************************************
* Basic arithmetic
******************************************************************************/

template<typename T, typename Op> polynomial<T>
unary (polynomial<T> p) {
  int i, n= N(p);
  T* a= A(p);
  T* r= tm_new_array<T> (n);
  for (i=0; i<n; i++)
    r[i]= Op::eval (a[i]);
  return polynomial<T> (r, n);
}

TMPL inline polynomial<T>
copy (polynomial<T> v) {
  return unary<T,copy_op> (v); }

TMPL inline polynomial<T>
operator - (polynomial<T> v) {
  return unary<T,neg_op> (v); }

template<typename T, typename Op> polynomial<T>
binary (polynomial<T> p1, polynomial<T> p2) {
  int i, n1= N(p1), n2=N(p2), m= min (n1, n2), n= max (n1, n2);
  T* a= A(p1);
  T* b= A(p2);
  T* r= tm_new_array<T> (n);
  for (i=0; i<m; i++)
    r[i]= Op::eval (a[i], b[i]);
  if (n1 < n2)
    for (; i<n; i++)
      r[i]= Op::eval (T(0), b[i]);
  else
    for (; i<n; i++)
      r[i]= Op::eval (a[i], T(0));
  return polynomial<T> (r, n);
}

TMPL inline polynomial<T>
operator + (polynomial<T> p1, polynomial<T> p2) {
  return binary<T,add_op> (p1, p2); }

TMPL inline polynomial<T>
operator - (polynomial<T> p1, polynomial<T> p2) {
  return binary<T,sub_op> (p1, p2); }

TMPL inline polynomial<T>
derive (polynomial<T> p) {
  if (N(p) <= 1) return 0;
  int i, n= N(p);
  T* a= A(p);
  T* r= tm_new_array<T> (n-1);
  for (i=1; i<n; i++)
    r[i-1]= T(i) * a[i];
  return polynomial<T> (r, n-1);
}

/******************************************************************************
* Multiplication
******************************************************************************/

TMPL polynomial<T>
operator * (polynomial<T> p1, polynomial<T> p2) {
  int i, j, n1= N(p1), n2=N(p2), n= n1+n2;
  T* a= A(p1);
  T* b= A(p2);
  T* r= tm_new_array<T> (n);
  for (i=0; i<n; i++) r[i]= 0;
  for (i=0; i<n1; i++)
    for (j=0; j<n2; j++)
      r[i+j] += a[i] * b[j];
  return polynomial<T> (r, n);
}

TMPL polynomial<T>
operator * (T c, polynomial<T> p) {
  int i, n= N(p);
  T* a= A(p);
  T* r= tm_new_array<T> (n);
  for (i=0; i<n; i++)
    r[i]= c * a[i];
  return polynomial<T> (r, n);
}

/******************************************************************************
* Polynomials and vectors
******************************************************************************/

TMPL vector<polynomial<T> >
operator * (T c, vector<polynomial<T> > v) {
  // TODO: superfluous after more general asymmetric vector multiplication
  int i, n= N(v);
  polynomial<T>* a= A(v);
  polynomial<T>* r= tm_new_array<polynomial<T> > (n);
  for (i=0; i<n; i++)
    r[i]= c * v[i];
  return vector<polynomial<T> > (r, n);
}

TMPL vector<polynomial<T> >
operator * (array<T> c, polynomial<T> p) {
  // NOTE: replace array by vector as soon as "points" are vectors
  int i, n= N(c);
  T* a= A(c);
  polynomial<T>* r= tm_new_array<polynomial<T> > (n);
  for (i=0; i<n; i++)
    r[i]= a[i] * p;
  return vector<polynomial<T> > (r, n);
}

TMPL array<T>
extract (vector<polynomial<T> > v, int i) {
  // NOTE: replace array by vector as soon as "points" are vectors
  int j, n= N(v);
  array<T> r (n);
  // vector<T> r (T(0), n);
  for (j=0; j<n; j++)
    r[j]= v[j][i];
  return r;
}

#undef TMPL
#undef BINARY_TMPL
#undef R
#undef M
#endif // defined POLYNOMIAL_H
