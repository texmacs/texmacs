
/******************************************************************************
* MODULE     : fast_search.cpp
* DESCRIPTION: Fast multiple searches in same string
* COPYRIGHT  : (C) 2014  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "fast_search.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

/******************************************************************************
* Subroutines
******************************************************************************/

static int
hash_combine (int c1, int c2, int l) {
  // FIXME: we should rather compute modulo 2^32 - 1 instead of 2^32,
  // or modulo some prime number < 2^32, and avoid the cyclicity of period 32
  int rot= (9 * l) & 31;
  if (rot == 0) return c1 ^ c2;
  unsigned int u1= c1;
  unsigned int u2= c2;
  unsigned int r2= ((u2 << rot) & 0xffffffff) ^
                   ((u2 >> (32 - rot)) & 0xffffffff);
  return (int) (u1 ^ r2);
}

static int
fast_hash (string s) {
  unsigned int h= 0;
  for (int i=N(s)-1; i>=0; i--) {
    h= ((h << 9) & 0xffffffff) ^ ((h >> 23) & 0xffffffff);
    h= h ^ ((unsigned int) (unsigned char) s[i]);
  }
  return (int) h;
}

/******************************************************************************
* Construct search engine
******************************************************************************/

string_searcher_rep::string_searcher_rep (string s2): s (s2), a () {
  // NOTE: a[i] contains a hashmap with all associations c :-> j,
  // where c is the hash code of the substring s (j, j + 2^i).
  int i, n= N(s);
  array<int> codes (n);
  for (i=0; i<n; i++)
    codes[i]= (int) (unsigned int) (unsigned char) s[i];
  int l=1;
  while (l <= n) {
    hashmap<int,array<int> > h;
    for (i=0; i+l <= n; i++) {
      int c= codes[i];
      if (!h->contains (c))
        h(c)= array<int> ();
      h(c) << i;
    }
    a << h;
    int d= 2*l;
    for (i=0; i+d <= n; i++)
      codes[i]= hash_combine (codes[i], codes[i+l], l);
    l= d;
  }
}

string
string_searcher_rep::get_string () {
  return s;
}

/******************************************************************************
* Search
******************************************************************************/

array<int>
string_searcher_rep::search_sub (string what) {
  if (N(what) == 0) {
    array<int> r;
    for (int i=0; i<=N(s); i++) r << i;
    return r;
  }
  int k=1, l=0;
  while ((k<<1) <= N(what)) { k <<= 1; l++; }
  int code= fast_hash (what (0, k));
  if (!a[l]->contains (code)) return array<int> ();
  else return a[l][code];
}

int
string_searcher_rep::search_next (string what, int pos) {
  array<int> ps= search_sub (what);
  for (int i=0; i<N(ps); i++) {
    int next= ps[i];
    if (next >= pos && test (s, next, what)) return next;
  }
  return -1;
}

array<int>
string_searcher_rep::search_all (string what) {
  // NOTE: There are better algorithms based on generalized suffix trees.
  // However, in practice, our approach based on hashtables might be faster
  array<int> r;
  array<int> ps= search_sub (what);
  for (int i=0; i<N(ps); i++) {
    int pos= ps[i];
    if (test (s, pos, what)) r << pos;
  }
  return r;
}

/******************************************************************************
* Get (leftmost) longest common substring
******************************************************************************/

void
get_longest_common (string s1, string s2, int& b1, int& e1, int& b2, int& e2) {
  // NOTE: There are better algorithms based on generalized suffix trees.
  b1= e1= b2= e2= 0;
  int bestl= 0;
  string_searcher ss1 (s1);
  string_searcher ss2 (s2);
  for (int i= min (N(ss1->a), N(ss2->a)) - 1; i>=0; i--) {
    hashmap<int,array<int> > a1= ss1->a[i];
    hashmap<int,array<int> > a2= ss2->a[i];
    iterator<int> it= iterate (a1);
    while (it->busy ()) {
      int h= it->next ();
      array<int> ps1= a1[h];
      array<int> ps2= a2[h];
      if (N(ps1) == 0 || N(ps2) == 0) continue;
      for (int j1=0; j1<N(ps1); j1++) {
        int pos1= ps1[j1];
        if (b1 <= pos1 && pos1 < e1) continue;
        for (int j2=0; j2<N(ps2); j2++) {
          int pos2= ps2[j2];
          int bb1= pos1, ee1= pos1, bb2= pos2, ee2= pos2;
          while (bb1 > 0 && bb2 > 0 && s1[bb1-1] == s2[bb2-1]) {
            bb1--; bb2--; }
          while (ee1 < N(s1) && ee2 < N(s2) && s1[ee1] == s2[ee2]) {
            ee1++; ee2++; }
          if (ee1 - bb1 > bestl ||
              (ee1 - bb1 == bestl &&
               (bb1 < b1 ||
                (bb1 == b1 && bb2 < b2)))) {
            b1= bb1; e1= ee1;
            b2= bb2; e2= ee2;
            bestl= e1 - b1;
          }
        }
      }
    }
  }
}
