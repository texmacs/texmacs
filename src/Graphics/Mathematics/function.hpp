
/******************************************************************************
* MODULE     : function.hpp
* DESCRIPTION: Mathematical functions from F into T
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FUNCTION_HPP
#define FUNCTION_HPP
#include "ball.hpp"
#include "vector.hpp"
#include "polynomial.hpp"
#define TMPL template<typename F, typename T>
#define V typename properties<F>::index_type
#define C typename properties<F>::scalar_type

TMPL class function;
TMPL bool is_nil (function<F,T> f);

/******************************************************************************
* Abstract function class
******************************************************************************/

TMPL
class function_rep: public abstract_struct {
public:
  inline function_rep () {}
  inline virtual ~function_rep () {}
  virtual T apply (F x) = 0;
  virtual ball<T> apply (ball<F> x) = 0;
  virtual function<F,T> derive (V var) = 0;
  virtual tree expression () = 0;
};

TMPL
class function {
public:
ABSTRACT_NULL_TEMPLATE_2(function,F,T);
  inline function (T x);
  inline T operator () (F x);
  inline ball<T> operator () (ball<F> x);
};
ABSTRACT_NULL_TEMPLATE_2_CODE(function,typename,F,typename,T);

TMPL inline T function<F,T>::operator () (F x) {
  return rep->apply (x); }
TMPL inline ball<T> function<F,T>::operator () (ball<F> x) {
  return rep->apply (x); }
TMPL inline function<F,T> derive (function<F,T> f, V var) {
  return f->derive (var); }
TMPL inline tree as_tree (function<F,T> f) {
  return f->expression (); }
TMPL inline tm_ostream& operator << (tm_ostream& out, function<F,T> f) {
  return out << as_math_string (as_tree (f)); }

TMPL
class properties<function<F,T> > {
public:
  typedef function<F,typename properties<T>::scalar_type> scalar_type;
  typedef function<F,typename properties<T>::norm_type> norm_type;
  typedef function<F,typename properties<T>::index_type> index_type;
};

/******************************************************************************
* Basic functions
******************************************************************************/

TMPL
class constant_function_rep: public function_rep<F,T> {
  T c;
public:
  inline constant_function_rep (T c2): c (c2) {}
  inline T apply (F x) { return c; }
  inline ball<T> apply (ball<F> x) { return ball<T> (c, 0); }
  inline function<F,T> derive (V var) { return C(0) * c; }
  inline tree expression () { return as_tree (c); }
};

TMPL inline function<F,T>::function (T c):
  rep (tm_new<constant_function_rep<F,T> > (c)) { rep->ref_count++; }

TMPL
class coordinate_function_rep: public function_rep<F,T> {
  V var;
  T c;
public:
  inline coordinate_function_rep (V var2, T c2): var (var2), c (c2) {}
  inline T apply (F x) { return properties<F>::access (x, var) * c; }
  inline ball<T> apply (ball<F> x) {
    return properties<ball<F> >::access (x, var) * ball<T> (c, 0); }
  inline function<F,T> derive (V var2) { return var2==var? c: C(0) * c; }
  inline tree expression () {
    return mul (as_tree (c), properties<F>::index_name (var)); }
};

TMPL inline function<F,T>
coordinate_function (V var, T c=1) {
  return tm_new<coordinate_function_rep<F,T> > (var, c); }

/******************************************************************************
* Operators on functions
******************************************************************************/

template<typename F, typename T, typename Op>
class unary_function_rep: public function_rep<F,T> {
  function<F,T> f;
public:
  inline unary_function_rep (function<F,T> f2): f (f2) {}
  inline T apply (F x) { return Op::eval (f (x)); }
  inline ball<T> apply (ball<F> x) { return Op::eval (f (x)); }
  inline function<F,T> derive (V var) { return Op::diff (f, var); }
  inline tree expression () { return Op::eval (as_tree (f)); }
};

template<typename F, typename T, typename Op> inline function<F,T>
unary_function (function<F,T> f) {
  return tm_new<unary_function_rep<F,T,Op> > (f); }

template<typename F, typename T, typename Op>
class binary_function_rep: public function_rep<F,T> {
  function<F,T> f, g;
public:
  inline binary_function_rep (function<F,T> f2, function<F,T> g2):
    f (f2), g (g2) {}
  inline T apply (F x) { return Op::eval (f (x), g (x)); }
  inline ball<T> apply (ball<F> x) { return Op::eval (f (x), g (x)); }
  inline function<F,T> derive (V var) { return Op::diff (f, g, var); }
  inline tree expression () { return Op::eval (as_tree (f), as_tree (g)); }
};

template<typename F, typename T, typename Op> inline function<F,T>
binary_function (function<F,T> f, function<F,T> g) {
  return tm_new<binary_function_rep<F,T,Op> > (f, g); }

/******************************************************************************
* Standard functions
******************************************************************************/

TMPL inline function<F,T>
operator - (function<F,T> f) {
  return unary_function<F,T,neg_op> (f); }

TMPL inline function<F,T>
operator + (function<F,T> f, function<F,T> g) {
  return binary_function<F,T,add_op> (f, g); }

TMPL inline function<F,T>
operator - (function<F,T> f, function<F,T> g) {
  return binary_function<F,T,sub_op> (f, g); }

TMPL inline function<F,T>
operator * (function<F,T> f, function<F,T> g) {
  return binary_function<F,T,mul_op> (f, g); }

TMPL inline function<F,T>
operator / (function<F,T> f, function<F,T> g) {
  return binary_function<F,T,div_op> (f, g); }

TMPL inline function<F,T>
sqrt (function<F,T> f) {
  return unary_function<F,T,sqrt_op> (f); }

TMPL inline function<F,T>
exp (function<F,T> f) {
  return unary_function<F,T,exp_op> (f); }

TMPL inline function<F,T>
log (function<F,T> f) {
  return unary_function<F,T,log_op> (f); }

TMPL inline function<F,T>
pow (function<F,T> f, function<F,T> g) {
  return binary_function<F,T,pow_op> (f, g); }

TMPL inline function<F,T>
cos (function<F,T> f) {
  return unary_function<F,T,cos_op> (f); }

TMPL inline function<F,T>
sin (function<F,T> f) {
  return unary_function<F,T,sin_op> (f); }

TMPL inline function<F,T>
tan (function<F,T> f) {
  return unary_function<F,T,tan_op> (f); }

#undef TMPL
#undef V
#undef C
#endif // FUNCTION_H
