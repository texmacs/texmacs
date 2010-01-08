
/******************************************************************************
* MODULE     : matrix.hpp
* DESCRIPTION: matrixs with reference counting and pointer copying
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MATRIX_H
#define MATRIX_H
#include "vector.hpp"
#include "ntuple.hpp"
#define TMPL template<typename T>
#define BINARY_TMPL template<typename T, typename U>
#define R typename unary_properties<T>::norm_type
#define M typename binary_properties<T,U>::product_type

TMPL class matrix;
TMPL int NR (matrix<T> m);
TMPL int NC (matrix<T> m);
TMPL T*  A  (matrix<T> m);

/******************************************************************************
* The matrix class
******************************************************************************/

TMPL
class matrix_rep: concrete_struct {
  int rows;
  int cols;
  T* a;
public:
  inline matrix_rep (T* a2, int rows2, int cols2):
    rows (rows2), cols (cols2), a (a2) {}
  inline ~matrix_rep () { if (a != NULL) tm_delete_array (a); }
  friend class matrix<T>;
  friend int NR LESSGTR (matrix<T> m);
  friend int NC LESSGTR (matrix<T> m);
  friend T*  A  LESSGTR (matrix<T> m);
};

TMPL
class matrix {
CONCRETE_TEMPLATE(matrix,T);
  inline matrix (T *a, int rows, int cols):
    rep (tm_new<matrix_rep<T> > (a, rows, cols)) {}
  inline matrix (T c, int rows, int cols) {
    int i, n= rows * cols;
    T* a= (n == 0? (T*) NULL: tm_new_array<T> (n));
    for (i=0; i<n; i++)
      a[i]= ((i%(cols+1)) == 0? c: T(0));
    rep= tm_new<matrix_rep<T> > (a, rows, cols); }
  inline matrix () {
    rep= tm_new<matrix_rep<T> > ((T*) NULL, 0, 0); }
  inline T& operator () (int i, int j) {
    return rep->a[i*rep->cols + j]; }
};
CONCRETE_TEMPLATE_CODE(matrix,typename,T);

TMPL inline int NR (matrix<T> m) { return m->rows; }
TMPL inline int NC (matrix<T> m) { return m->cols; }
TMPL inline T*  A  (matrix<T> m) { return m->a; }

TMPL
class unary_properties<matrix<T> > {
public:
  typedef T scalar_type;
  typedef typename unary_properties<T>::norm_type norm_type;
  typedef pair<int,int> index_type;
  static inline tree index_name (index_type i) {
    return tree (RSUB, tree (RSUB, "x", as_string (i.x1 + 1)),
		 as_string (i.x2 + 1)); }
  static inline scalar_type access (matrix<T> m, index_type var) {
    return m (var.x1, var.x2); }
};

BINARY_TMPL
class binary_properties<matrix<T>,matrix<U> > {
public:
  typedef matrix<M > product_type;
};

BINARY_TMPL
class binary_properties<T,matrix<U> > {
public:
  typedef matrix<M > product_type;
};

BINARY_TMPL
class binary_properties<matrix<T>,U > {
public:
  typedef matrix<M > product_type;
};

/******************************************************************************
* Conversions
******************************************************************************/

TMPL tree
as_tree (matrix<T> m) {
  int i, rows= NR (m);
  int j, cols= NC (m);
  tree t (TUPLE, rows);
  for (i=0; i<rows; i++) {
    t[i]= tree (TUPLE, cols);
    for (j=0; j<cols; j++)
      t[i][j]= as_tree (m(i,j));
  }
  return t;
}

TMPL inline tm_ostream&
operator << (tm_ostream& out, matrix<T> m) {
  return out << as_math_string (as_tree (m));
}

TMPL void
parse (tree t, matrix<T>& m) {
  ASSERT (is_tuple (t) && N(t)>0 && is_tuple (t[0]), "not a matrix");
  int i, j, rows= N(t), cols= N(t[0]);
  m= matrix<T> (T(0), rows, cols);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      m(i,j)= parse_as<T> (t[i][j]);
}

TMPL inline matrix<T>
as_matrix (tree t) {
  matrix<T> result;
  parse (t, result);
  return result;
}

/******************************************************************************
* Vectorial operations on matrices
******************************************************************************/

template<typename T, typename Op> matrix<T>
unary (matrix<T> m) {
  int i, n= NR(m) * NC(m);
  T* a= A(m);
  T* r= tm_new_array<T> (n);
  for (i=0; i<n; i++)
    r[i]= Op::eval (a[i]);
  return matrix<T> (r, NR(m), NC(m));
}

TMPL inline matrix<T>
copy (matrix<T> m) {
  return unary<T,copy_op> (m); }

TMPL inline matrix<T>
operator - (matrix<T> m) {
  return unary<T,neg_op> (m); }

template<typename T, typename Op> matrix<T>
binary (matrix<T> m1, matrix<T> m2) {
  int i, n= NR(m1) * NC(m1);
  ASSERT (NR(m1) == NR(m2) && NC(m1) == NC(m2), "matrix sizes don't match");
  T* a= A(m1);
  T* b= A(m2);
  T* r= tm_new_array<T> (n);
  for (i=0; i<n; i++)
    r[i]= Op::eval (a[i], b[i]);
  return matrix<T> (r, NR(m1), NC(m1));
}

TMPL inline matrix<T>
operator + (matrix<T> m1, matrix<T> m2) {
  return binary<T,add_op> (m1, m2); }

TMPL inline matrix<T>
operator - (matrix<T> m1, matrix<T> m2) {
  return binary<T,sub_op> (m1, m2); }

/******************************************************************************
* Multiplication
******************************************************************************/

TMPL inline matrix<T>
operator * (matrix<T> m1, matrix<T> m2) {
  int i, j, k, rows= NR (m1), aux= NC (m1), cols= NC (m2);
  ASSERT (NR (m2) == aux, "dimensions don't match");
  matrix<T> prod (T(0), rows, cols);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      for (k=0; k<aux; k++)
	prod (i, j) += m1 (i, k) * m2 (k, j);
  return prod;
}

TMPL inline vector<T>
operator * (matrix<T> m, vector<T> v) {
  int i, j, rows= NR (m), cols= NC (m);
  ASSERT (N (v) == cols, "dimensions don't match");
  vector<T> prod (T(0), rows);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      prod [i] += m (i, j) * v[j];
  return prod;
}

TMPL inline array<T>
operator * (matrix<T> m, array<T> v) {
  int i, j, rows= NR (m), cols= NC (m);
  ASSERT (N (v) == cols, "dimensions don't match");
  array<T> prod (T(0), rows);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      prod [i] += m (i, j) * v[j];
  return prod;
}

#undef TMPL
#undef BINARY_TMPL
#undef R
#undef M
#endif // defined MATRIX_H
