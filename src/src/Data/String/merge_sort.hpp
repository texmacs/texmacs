
/******************************************************************************
* MODULE     : merge_sort
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MERGE_SORT_H
#define MERGE_SORT_H
#include "array.hpp"

template<class T> struct less_eq_operator {
  static inline bool leq(T& a, T& b) { return a<=b; }
};

template<class T, class LEQ> static void
merge_sort (array<T>& a, int start, int end, array<T> &merge_buf) {
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
  merge_sort<T, LEQ> (a,start,middle,merge_buf);
  merge_sort<T, LEQ> (a,middle,end,merge_buf);
  int i,j,k;
  for (i=start, j=middle, k=start; (i<middle) && (j<end); )
    if (LEQ::leq(a[i], a[j])) merge_buf[k++]=a[i++];
    else                      merge_buf[k++]=a[j++];
  j=k;
  while (i!=middle) a[k++]=a[i++];
  for (i=start; i<j; i++) a[i]=merge_buf[i];
}

template<class T, class LEQ> void
merge_sort (array<T>& a) {
  array<T> merge_buf(N(a)); 
  merge_sort<T, LEQ> (a, 0, N(a), merge_buf); }

template<class T> inline void
merge_sort (array<T>& a) {
  merge_sort <T, less_eq_operator<T> > (a); }

#endif // MERGE_SORT_H
