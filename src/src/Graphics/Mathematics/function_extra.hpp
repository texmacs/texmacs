
/******************************************************************************
* MODULE     : function_extra.hpp
* DESCRIPTION: Extra routines for functions from F into T
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FUNCTION_EXTRA_HPP
#define FUNCTION_EXTRA_HPP
#include "function.hpp"
#include "vector.hpp"
#include "polynomial.hpp"
#define TMPL template<typename F, typename T>
#define V typename properties<F>::index_type
#define C typename properties<F>::scalar_type

#define MATH_INFINITY HUGE_VAL

/******************************************************************************
* Piecewise functions
******************************************************************************/

TMPL function<F,T>
pw_function (vector<function<F,T> > v, bool jump_flag= false);

TMPL
class pw_function_rep: public function_rep<F,T> {
  vector<function<F,T> > v;
  bool jump_flag;
public:
  inline pw_function_rep (vector<function<F,T> > v2, bool jump_flag2):
    v (v2), jump_flag (jump_flag2) {}
  int which (F x) {
    if (x <= 0) return 0;
    if (x >= 1) return N(v)-1;
    return (int) floor (N(v) * x);
  }
  F position (F x) { return N(v) * x - which (x); }
  T apply (F x) { return v[which (x)] (position (x)); }
  ball<T> apply (ball<F> x) {
    int w1= which (lower (x));
    int w2= which (upper (x));
    if (w1 == w2) return v[w1] (ball<F> (N(v)) * x - ball<F> (w1));
    if (jump_flag) return ball<T> (T(0), MATH_INFINITY);
  }
  function<F,T> derive (V var) {
    function<F,T> dummy;
    vector<function<F,T> > w (dummy, N(v));
    for (int i=0; i<N(v); i++)
      w[i]= function<F,T> (N(v)) * v[i]->derive (var);
    bool flag= jump_flag;
    for (int i=0; i<N(v)-1; i++)
      flag= flag || (v[i](F(1)) != v[i+1](F(0)));
    return pw_function (w, flag);
  }
  tree expression () {
    int i, n= N(v);
    array<tree> a (n+1);
    for (i=0; i<n; i++)
      a[i]= as_tree (v[i]);
    a[n]= (jump_flag? tree ("true"): tree ("false"));
    return compound ("piecewise", a);
  }
};

TMPL inline function<F,T>
pw_function (vector<function<F,T> > v, bool jump_flag) {
  return tm_new<pw_function_rep<F,T> > (v, jump_flag);
}

/******************************************************************************
* Vector functions
******************************************************************************/

template<typename T> ball<vector<T> >
as_ball (vector<ball<T> > v) {
  typedef typename properties<T>::norm_type radius_type;
  int i, n= N(v);
  vector<T> c (T(0), n);
  vector<radius_type> r (radius_type(0), n);
  for (i=0; i<n; i++) {
    c[i]= center (v[i]);
    r[i]= radius (v[i]);
  }
  return ball<vector<T> > (c, norm (r));
}

TMPL function<F,vector<T> > vector_function (vector<function<F,T> > v);

TMPL
class vector_function_rep: public function_rep<F,vector<T> > {
  vector<function<F,T> > v;
public:
  inline vector_function_rep (vector<function<F,T> > v2): v (v2) {}
  vector<T> apply (F x) {
    int i, n= N(v);
    T* a= tm_new_array<T> (n);
    for (i=0; i<n; i++)
      a[i]= v[i] (x);
    return vector<T> (a, n);
  }
  ball<vector<T> > apply (ball<F> x) {
    int i, n= N(v);
    ball<T>* a= tm_new_array<ball<T> > (n);
    for (i=0; i<n; i++)
      a[i]= v[i] (x);
    return as_ball (vector<ball<T> > (a, n));
  }
  function<F,vector<T> > derive (V var) {
    int i, n= N(v);
    vector<function<F,T> > w;
    for (i=0; i<n; i++) w[i]= v[i]->derive (var);
    return vector_function (w);
  }
  tree expression () {
    return compound ("as_function", as_tree (v));
  }
};

TMPL inline function<F,vector<T> >
vector_function (vector<function<F,T> > v) {
  return tm_new<vector_function_rep<F,T> > (v);
}

/******************************************************************************
* Polynomial functions
******************************************************************************/

template<typename T> function<T,T> polynomial_function (polynomial<T> p);

template<typename T>
class polynomial_function_rep: public function_rep<T,T> {
  polynomial<T> p;
public:
  inline polynomial_function_rep (polynomial<T> p2): p (p2) {}
  inline T apply (T x) { return p (x); }
  ball<T> apply (ball<T> x) {
    int i, n= N(p);
    if (n == 0) return 0;
    T* a= A(p);
    ball<T> sum= a[n-1];
    for (i=n-2; i>=0; i--)
      sum = x * sum + ball<T> (a[i]);
    return sum; }
  function<T,T> derive (typename properties<T>::index_type var) {
    return polynomial_function<T> (::derive (p)); }
  tree expression () {
    return compound ("as_function", as_tree (p)); }
};

template<typename T> inline function<T,T>
polynomial_function (polynomial<T> p) {
  return tm_new<polynomial_function_rep<T> > (p);
}

#undef TMPL
#undef V
#undef C
#endif // FUNCTION_EXTRA_H
