
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
    r[i]= Op::op (a[i]);
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
    r[i]= Op::op (a[i], b[i]);
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
  vector<T> prod (rows);
  for (j=0; j<cols; j++)
    prod[j]= T(0);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      prod [i] += m (i, j) * v[j];
  return prod;
}

TMPL inline array<T>
operator * (matrix<T> m, array<T> v) {
  int i, j, rows= NR (m), cols= NC (m);
  ASSERT (N (v) == cols, "dimensions don't match");
  array<T> prod (rows);
  for (j=0; j<cols; j++)
    prod[j]= T(0);
  for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
      prod [i] += m (i, j) * v[j];
  return prod;
}

TMPL matrix<T>
matrix_2D (T a, T b, T c, T d) {
  matrix<T> m (T(0), 2, 2);
  m (0, 0)= a;
  m (0, 1)= b;
  m (1, 0)= c;
  m (1, 1)= d;
  return m;
}

TMPL matrix<T>
transpose (matrix<T> m) {
  int rows= NR (m), cols= NC (m);
  matrix<T> tm (T(0), cols, rows);
  for (int i=0; i<rows; i++)
    for (int j=0; j<cols; j++)
      tm (j, i)= m (i, j);
  return tm;
}

template<typename T> inline void
el_swap (T& a, T& b) { T x= a; a= b; b= x; }

TMPL matrix<T>
invert (matrix<T> m) {
  int rows= NR (m), cols= NC (m);
  ASSERT (rows == cols, "invalid matrix dimensions");
  if (rows == 2) {
    T det= m (0, 0) * m (1, 1) - m (0, 1) * m (1, 0);
    matrix<T> inv (T(0), rows, cols);
    inv (0, 0)=  m (1, 1) / det;
    inv (0, 1)= -m (0, 1) / det;
    inv (1, 0)= -m (1, 0) / det;
    inv (1, 1)=  m (0, 0) / det;
    return inv;
  }
  else {
    matrix<T> mat= copy (m);
    matrix<T> inv (T(1), rows, cols);
    for (int j=0; j<cols; j++) {
      int best_i= j;
      T   best_a= abs (mat (j, j));
      for (int i=j+1; i<rows; i++)
        if (abs (mat (i, j)) > best_a) {
          best_i = i;
          best_a = abs (mat (i, j));
        }
      ASSERT (mat (best_i, j) != T(0), "matrix is not invertible");
      if (best_i != j) {
        for (int k=0; k<cols; k++) {
          el_swap (mat (j, k), mat (best_i, k));
          el_swap (inv (j, k), inv (best_i, k));
        }
      }
      T u= T(1) / mat (j, j);
      for (int k=0; k<cols; k++) {
        mat (j, k) *= u;
        inv (j, k) *= u;
      }
      for (int i=0; i<rows; i++)
        if (i != j) {
          T c= mat (i, j);
          for (int k=0; k<cols; k++) {
            mat (i, k) -= c * mat (j, k);
            inv (i, k) -= c * inv (j, k);
          }
        }
    }
    return inv;
  }
}

TMPL array<T>
projective_apply (matrix<T> m, array<T> v) {
  int n= NR (m);
  ASSERT (NC (m) == n && N(v) + 1 == n, "dimensions don't match");
  array<T> pv= copy (v);
  pv << T (1.0);
  array<T> pr= m * pv;
  array<T> r (n-1);
  for (int i=0; i<n-1; i++)
    r[i]= pr[i] / pr[n-1];
  return r;
}

TMPL array<array<T> >
projective_apply (matrix<T> m, array<array<T> > v) {
  array<array<T> > r (N(v));
  for (int i=0; i<N(v); i++)
    r[i]= projective_apply (m, v[i]);
  return r;
}

#undef TMPL
#undef BINARY_TMPL
#undef R
#undef M
#endif // defined MATRIX_H
