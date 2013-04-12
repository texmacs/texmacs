
/******************************************************************************
* MODULE     : merge_sort
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MERGE_SORT_H
#define MERGE_SORT_H
#include "array.hpp"
#include "hashmap.hpp"

/******************************************************************************
* Basic merge sort
******************************************************************************/

template<class T> struct less_eq_operator {
  static inline bool leq(T& a, T& b) { return a<=b; }
};

template<class T, class LEQ> static void
merge_sort_sub (array<T>& a, int start, int end, array<T> &merge_buf) {
  if (end-start<=1) return;
  if (end-start==2) {
    if (!LEQ::leq(a[start], a[start+1])) {
      merge_buf[start]=a[start];
      a[start]=a[start+1];
      a[start+1]=merge_buf[start];
    }
    return;
  }
  int middle=(start+end)>>1; 
  merge_sort_sub<T, LEQ> (a,start,middle,merge_buf);
  merge_sort_sub<T, LEQ> (a,middle,end,merge_buf);
  int i,j,k;
  for (i=start, j=middle, k=start; (i<middle) && (j<end); )
    if (LEQ::leq(a[i], a[j])) merge_buf[k++]=a[i++];
    else                      merge_buf[k++]=a[j++];
  j=k;
  while (i!=middle) a[k++]=a[i++];
  for (i=start; i<j; i++) a[i]=merge_buf[i];
}

template<class T, class LEQ> void
merge_sort_leq (array<T>& a) {
  array<T> merge_buf(N(a)); 
  merge_sort_sub<T, LEQ> (a, 0, N(a), merge_buf); }

template<class T> inline void
merge_sort (array<T>& a) {
  merge_sort_leq <T, less_eq_operator<T> > (a); }

struct less_eq_associate {
  static inline bool leq (tree& a, tree& b) {
    return as_string(a[0]) <= as_string(b[0]); }
};

template <class T, class U> static tree
make_collection (hashmap<T,U> h) {
  tree t(h);
  array<tree> a=A(h);
  merge_sort_leq <tree, less_eq_associate> (a);
  int i, n=N(a);
  for (i=0; i<n; i++) t[i] = a[i];
  return t;
}

/******************************************************************************
* Sort two arrays on the first array
******************************************************************************/

template<class T, class U, class LEQ> static void
merge_sort_sub (array<T>& a, array<U>& b, int start, int end,
                array<T>& ma, array<U>& mb) {
  if (end-start<=1) return;
  if (end-start==2) {
    if (!LEQ::leq(a[start], a[start+1])) {
      ma[start]=a[start];
      a[start]=a[start+1];
      a[start+1]=ma[start];
      mb[start]=b[start];
      b[start]=b[start+1];
      b[start+1]=mb[start];
    }
    return;
  }
  int middle=(start+end)>>1; 
  merge_sort_sub<T,U,LEQ> (a,b,start,middle,ma,mb);
  merge_sort_sub<T,U,LEQ> (a,b,middle,end,ma,mb);
  int i,j,k;
  for (i=start, j=middle, k=start; (i<middle) && (j<end); )
    if (LEQ::leq(a[i], a[j])) { mb[k]= b[i]; ma[k++]= a[i++]; }
    else                      { mb[k]= b[j]; ma[k++]= a[j++]; }
  j=k;
  while (i!=middle) { b[k]= b[i]; a[k++]= a[i++]; }
  for (i=start; i<j; i++) { b[i]= mb[i]; a[i]=ma[i]; }
}

template<class T, class U, class LEQ> void
merge_sort_leq (array<T>& a, array<U>& b) {
  ASSERT (N(a) == N(b), "arrays of the same length expected");
  array<T> ma (N(a));
  array<U> mb (N(b));
  merge_sort_sub<T,U,LEQ> (a, b, 0, N(a), ma, mb);
}

template<class T, class LEQ> array<int>
merge_sort_leq_permutation (array<T>& a) {
  array<int> b (N(a));
  for (int i=0; i<N(a); i++) b[i]= i;
  merge_sort_leq<T,int,LEQ> (a, b);
  return b;
}

template<class T> array<int>
merge_sort_leq_permutation (array<T>& a) {
  return merge_sort_leq_permutation <T, less_eq_operator<T> > (a);
}

template<class T> array<T>
permute (array<T>& a, array<int> b) {
  ASSERT (N(a) == N(b), "arrays of the same length expected");
  array<T> r (N(a));
  for (int i=0; i<N(a); i++) r[i]= a[b[i]];
  return r;
}

#endif // MERGE_SORT_H
