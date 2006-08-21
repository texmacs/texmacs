
/******************************************************************************
* MODULE     : array
* DESCRIPTION: fixed size arrays with reference counting and
*              pointer copying
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ARRAY_H
#define ARRAY_H
#include "basic.hpp"

class tree;
template<class T> class array;
template<class T> int N (array<T> a);
template<class T> T*  A (array<T> a);
template<class T> array<T> copy (array<T> x);

template<class T> class array_rep: concrete_struct {
  int n;
  T* a;

public:
  inline array_rep (): n(0), a(NULL) {}
         array_rep (int n);
  inline ~array_rep () { if (n!=0) delete[] a; }
  void resize (int n);
  friend class array<T>;
  friend int N LESSGTR (array<T> a);
  friend T*  A LESSGTR (array<T> a);
  friend array<T> copy LESSGTR (array<T> a);
};

template<class T> class array {
  CONCRETE_TEMPLATE(array,T);
  inline array (int n=0): rep (new array_rep<T>(n)) {}
  array (T *a, int n);
  array (T x1, T x2);
  inline T& operator [] (int i) { return rep->a[i]; }
  operator tree (); // defined in tree.hpp
};
CONCRETE_TEMPLATE_CODE(array,class,T);

#define TMPL template<class T>
TMPL inline int N (array<T> a) { return a->n; }
TMPL inline T*  A (array<T> a) { return a->a; }
TMPL inline array<T> copy (array<T> a) {
  return array<T> (a->a, a->n); }
TMPL ostream& operator << (ostream& out, array<T> a);
TMPL array<T>& operator << (array<T>& a, T x);
TMPL array<T>& operator << (array<T>& a, array<T> b);
TMPL bool operator == (array<T> a, array<T> b);
TMPL bool operator != (array<T> a, array<T> b);

TMPL int hash (array<T> a);
#undef TMPL

#ifdef STRING_H
// Function parameters participating in template signature resolution are never
// cast implicitely. 
inline array<string>& operator << (array<string>& a, char* x) {
  return a << string(x); }
#endif

#include "array.cpp"

#endif // defined ARRAY_H
