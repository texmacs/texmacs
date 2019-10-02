
/******************************************************************************
* MODULE     : array.cpp
* DESCRIPTION: fixed size arrays with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ARRAY_CC
#define ARRAY_CC
#include "array.hpp"

/******************************************************************************
* Routines intern to the array<T> class
******************************************************************************/

inline int
round_length (int n, size_t s) {
  (void) s;
  if (n<6) return n;
  int i=8;
  while (n>i) i<<=1;
  return i;
}

template<class T>
array_rep<T>::array_rep (int n2):
  n(n2), a((n==0)?((T*) NULL):(tm_new_array<T> (round_length(n, sizeof (T))))) {}

template<class T> void
array_rep<T>::resize (int m) {
  int nn= round_length (n, sizeof (T));
  int mm= round_length (m, sizeof (T));
  if (mm != nn) {
    if (mm != 0) {
      int i, k= (m<n? m: n);
      T* b= tm_new_array<T> (mm);
      for (i=0; i<k; i++) b[i]= a[i];
      if (nn != 0) tm_delete_array (a);
      a= b;
    }
    else {
      if (nn != 0) tm_delete_array (a);
      a= NULL;
    }
  }
  n= m;
}

template<class T>
array<T>::array (T* a, int n) {
  int i;
  rep= tm_new<array_rep<T> > (n);
  for (i=0; i<n; i++)
    rep->a[i]=a[i];
}

template<class T>
array<T>::array (T x1, T x2) {
  rep= tm_new<array_rep<T> > (2);
  rep->a[0]= x1;
  rep->a[1]= x2;
}

template<class T>
array<T>::array (T x1, T x2, T x3) {
  rep= tm_new<array_rep<T> > (3);
  rep->a[0]= x1;
  rep->a[1]= x2;
  rep->a[2]= x3;
}

template<class T>
array<T>::array (T x1, T x2, T x3, T x4) {
  rep= tm_new<array_rep<T> > (4);
  rep->a[0]= x1;
  rep->a[1]= x2;
  rep->a[2]= x3;
  rep->a[3]= x4;
}

template<class T>
array<T>::array (T x1, T x2, T x3, T x4, T x5) {
  rep= tm_new<array_rep<T> > (5);
  rep->a[0]= x1;
  rep->a[1]= x2;
  rep->a[2]= x3;
  rep->a[3]= x4;
  rep->a[4]= x5;
}

/******************************************************************************
* Other routines on arrays
******************************************************************************/

template<class T> bool
operator == (array<T> a, array<T> b) {
  int i;
  if (N(a)!=N(b)) return false;
  for (i=0; i<N(a); i++)
    if (a[i]!=b[i]) return false;
  return true;
}

template<class T> bool
operator != (array<T> a, array<T> b) {
  int i;
  if (N(a)!=N(b)) return true;
  for (i=0; i<N(a); i++)
    if (a[i]!=b[i]) return true;
  return false;
}

template<class T> tm_ostream&
operator << (tm_ostream& out, array<T> a) {
  int i;
  
  if (N(a)==0) return out << "[ ]";
  out << "[ ";
  for (i=0; i<N(a)-1; i++)
    out << a[i] << ", ";
  if (N(a)!=0) out << a[i];
  out << " ]";
  return out;
}

template<class T> array<T>&
operator << (array<T>& a, T x) {
  a->resize (N(a)+ 1);
  a[N(a)-1]=x;
  return a;
}

template<class T> array<T>&
operator << (array<T>& a, array<T> b) {
  int i, k= N(a);
  a->resize (N(a)+ N(b));
  for (i=0; i<N(b); i++) a[i+k]= b[i];
  return a;
}

template<class t> bool
contains (t a, array<t> b) {
  int i, l= N(b);
  for (i=0; i<l; i++)
    if (a == b[i])
      return true;
  return false;
}

template<class t> array<t>
append (t a, array<t> b) {
  int i, l= N(b);
  array<t> c (l+1);
  c[0]= a;
  for (i=0; i<l; i++) c[i+1]= b[i];
  return c;
}

template<class T> array<T>
append (array<T> a, array<T> b) {
  int i, k= N(a), l= N(b);
  array<T> c (k+l);
  for (i=0; i<k; i++) c[i]= a[i];
  for (i=0; i<l; i++) c[i+k]= b[i];
  return c;
}

template<class T> array<T>
range (array<T> a, int i, int j) {
  int k;
  ASSERT (i>=0 && j<=N(a), "out of range");
  array<T> r (j-i);
  for (k=i; k<j; k++) r[k-i]= a[k];
  return r;
}

template<class T> array<T>
reverse (array<T> a) {
  int i, n= N(a);
  array<T> r (n);
  for (i=0; i<n; i++) r[i]= a[n-1-i];
  return r;
}

template<class T> int
hash (array<T> a) {
  int i, n=N(a), h= 0;
  for (i=0; i<n; i++)
    h= hash(a[i]) ^ ((h<<7) + (h>>25));
  return h;
}

template<class T> array<T>
operator * (array<T> a, T c) {
  int i, n= N(a);
  array<T> r (n);
  for (i=0; i<n; i++) r[i]= a[i] * c;
  return r;
}

template<class T> array<T>
operator / (array<T> a, T c) {
  int i, n= N(a);
  array<T> r (n);
  for (i=0; i<n; i++) r[i]= a[i] / c;
  return r;
}

#endif // defined ARRAY_CC
