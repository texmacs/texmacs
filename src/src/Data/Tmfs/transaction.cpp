
/******************************************************************************
* MODULE     : transaction.cpp
* DESCRIPTION: transactions or changesets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"

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
