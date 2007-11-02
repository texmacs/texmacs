
/******************************************************************************
* MODULE     : collection.cpp
* DESCRIPTION: collections
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* A collection is a "change set" of strings, represented by a hashmap
* string -> int. Positive elements correspond to strings which have
* to be inserted, and negative values to strings which have to be removed.
* Furthermore, there are the following types of values:
*   1, -1: regular values
*   2, -2: indicates that the "change set" reduces to a single string
*   3, -3: indicates that the "change set" reduces to a single, large string
*          which should be stored in a separate file on disk.
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Routines for collections
******************************************************************************/

collection
singleton (string s, int eps) {
  collection c;
  c (s)= eps;
  return c;
}

string
first (collection c) {
  iterator<string> it= iterate (c);
  return it->next ();
}

collection
filter (collection c, bool file_flag) {
  collection r;
  iterator<string> it= iterate (c);
  while (it->busy ()) {
    string s= it->next ();
    int eps= c[s];
    if ((eps > -3 && eps < 3) ^ file_flag) r(s)= eps;
  }
  return r;
}

collection
simplify (collection c) {
  collection r;
  iterator<string> it= iterate (c);
  while (it->busy ()) {
    string s= it->next ();
    int eps= c[s];
    if (eps > 0) r(s)= eps;
  }
  return r;
}

collection
invert (collection c) {
  collection r;
  iterator<string> it= iterate (c);
  while (it->busy ()) {
    string s= it->next ();
    r(s)= -c[s];
  }
  return r;
}

void
merge (collection& c1, collection c2) {
  //cout << "Merge " << c1 << ", " << c2 << "\n";
  iterator<string> it= iterate (c2);
  while (it->busy ()) {
    string s= it->next ();
    int eps= c2[s];
    if (eps < -1 || eps > 1) c1= collection ();
    c1(s)= c2[s];
  }
}

collection
operator * (collection c1, collection c2) {
  collection r;
  merge (r, c1);
  merge (r, c2);
  return r;
}

int
total_size (collection c) {
  int sz= 1;
  iterator<string> it= iterate (c);
  while (it->busy ()) sz += (N (it->next ()) + 1);
  return sz;
}

/******************************************************************************
* Basic conversion routines
******************************************************************************/

collection
as_collection (strings a) {
  collection s;
  for (int i=0; i<N(a); i++)
    s(a[i])= 1;
  return s;
}

strings
as_strings (collection c) {
  strings a;
  iterator<string> it= iterate (c);
  while (it->busy ()) {
    string s= it->next ();
    if (c[s] > 0) a << s;
  }
  return a;
}
