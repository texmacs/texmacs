
/******************************************************************************
* MODULE     : array.cpp
* DESCRIPTION: fixed size arrays with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ARRAY_CC
#define ARRAY_CC
#include "array.hpp"

/******************************************************************************
* Routines intern to the array<T> class
******************************************************************************/

static inline int
round_length (int n, size_t s) {
  (void) s;
  if (n<6) return n;
  register int i=8;
  while (n>i) i<<=1;
  return i;
}

template<class T>
array_rep<T>::array_rep (int n2):
  n(n2), a((n==0)?((T*) NULL):(new T[round_length(n, sizeof (T))])) {}

template<class T> void
array_rep<T>::resize (register int m) {
  register int nn= round_length (n, sizeof (T));
  register int mm= round_length (m, sizeof (T));
  if (mm != nn) {
    if (mm != 0) {
      register int i, k= (m<n? m: n);
      T* b= new T[mm];
      for (i=0; i<k; i++) b[i]= a[i];
      if (nn != 0) delete[] a;
      a= b;
    }
    else {
      if (nn != 0) delete[] a;
      a= NULL;
    }
  }
  n= m;
}

template<class T>
array<T>::array (T* a, int n) {
  register int i;
  rep= new array_rep<T>(n);
  for (i=0; i<n; i++)
    rep->a[i]=a[i];
}

template<class T>
array<T>::operator tree () {
  int i, n=rep->n;
  tree t (TUPLE, n);
  for (i=0; i<n; i++)
    t[i]= as_tree(rep->a[i]);
  return t;
}

/******************************************************************************
* Other routines on arrays
******************************************************************************/

template<class T> bool
operator == (array<T> a, array<T> b) {
  register int i;
  if (N(a)!=N(b)) return false;
  for (i=0; i<N(a); i++)
    if (a[i]!=b[i]) return false;
  return true;
}

template<class T> bool
operator != (array<T> a, array<T> b) {
  register int i;
  if (N(a)!=N(b)) return true;
  for (i=0; i<N(a); i++)
    if (a[i]!=b[i]) return true;
  return false;
}

template<class T> ostream&
operator << (ostream& out, array<T> a) {
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
  register int i, k= N(a);
  a->resize (N(a)+ N(b));
  for (i=0; i<N(b); i++) a[i+k]= b[i];
  return a;
}

template<class T> int
hash (array<T> a) {
  int i, n=N(a), h= 0;
  for (i=0; i<n; i++)
    h= hash(a[i]) ^ ((h<<7) + (h>>25));
  return h;
}

#endif // defined ARRAY_CC
