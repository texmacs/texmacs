
/******************************************************************************
* MODULE     : tmfs_utils.cpp
* DESCRIPTION: convenient subroutines
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Building arrays of strings
******************************************************************************/

strings
seq (string s1) {
  strings a; a << s1; return a;
}

strings
seq (string s1, string s2) {
  strings a; a << s1 << s2; return a;
}

strings
seq (string s1, string s2, string s3) {
  strings a; a << s1 << s2 << s3; return a;
}

strings
seq (string s1, string s2, string s3, string s4) {
  strings a; a << s1 << s2 << s3 << s4; return a;
}

/******************************************************************************
* Routines for collections
*******************************************************************************
* A collection is a "change set" of strings, represented by a hashmap
* string -> int. Positive elements correspond to strings which have
* to be inserted, and negative values to strings which have to be removed.
* Furthermore, there are the following types of values:
*   1, -1: regular values
*   2, -2: indicates that the "change set" reduces to a single string
*   3, -3: indicates that the "change set" reduces to a single, large string
*          which should be stored in a separate file on disk.
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
* Routines for transactions
******************************************************************************/

transaction
atom (string key, collection val) {
  transaction t;
  t(key)= val;
  return t;
}

void
add (transaction& t, string key, string val, int eps) {
  //cout << "Add " << key << ", " << val << ", " << eps << "\n";
  if (!t->contains (key)) t (key)= collection ();
  t (key) (val)= eps;
}

transaction
filter (transaction t, bool file_flag) {
  transaction r;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    collection c= filter (t[s], file_flag);
    if (N(c) != 0) r(s)= c;
  }
  return r;
}

transaction
simplify (transaction t) {
  transaction r;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    collection c= simplify (t[s]);
    if (N(c) != 0) r(s)= c;
  }
  return r;
}

transaction
invert (transaction t) {
  transaction r;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    r(s)= invert (t[s]);
  }
  return r;
}

void
merge (transaction& t1, transaction t2) {
  //cout << "Merge " << t1 << ", " << t2 << "\n";
  iterator<string> it= iterate (t2);
  while (it->busy()) {
    string s= it->next ();
    if (!t1->contains (s)) t1(s)= collection ();
    merge (t1(s), t2[s]);
  }
}

transaction
operator * (transaction t1, transaction t2) {
  transaction r;
  merge (r, t1);
  merge (r, t2);
  return r;
}

int
total_size (transaction t) {
  int sz= 1;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    sz += N(s) + total_size (t[s]);
  }
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

/******************************************************************************
* Conversion routines for trees
******************************************************************************/

collection
as_collection (tree t) {
  collection s;
  if (is_tuple (t))
    for (int i=0; i<N(t); i++)
      if (is_atomic (t[i]))
	s(t[i]->label)= 1;
  return s;
}

tree
as_tree (collection c) {
  tree t (TUPLE);
  iterator<string> it= iterate (c);
  while (it->busy ()) {
    string s= it->next ();
    if (c[s] > 0) t << s;
  }
  return t;
}

strings
as_strings (tree t) {
  strings a;
  if (!is_compound (t)) return a;
  for (int i=0; i<N(t); i++)
    a << as_string (t[i]);
  return a;
}

tree
as_tree (strings a) {
  int i, n= N(a);
  tree t (TUPLE, n);
  for (i=0; i<n; i++)
    t[i]= a[i];
  return t;
}

tree
as_tree (solutions sols) {
  tree t (TUPLE);
  for (int i=0; i<N(sols); i++) {
    tree u (TUPLE);
    iterator<string> it= iterate (sols[i]);
    while (it->busy ()) {
      string s= it->next ();
      u << tuple (s, sols[i][s]);
    }
    t << u;
  }
  return t;
}
