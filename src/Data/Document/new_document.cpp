
/******************************************************************************
* MODULE     : new_document.cpp
* DESCRIPTION: Management of the global TeXmacs tree
* COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_document.hpp"

/******************************************************************************
* Management of all edit trees
******************************************************************************/

tree the_et;

path
new_document () {
  int i, n= N(the_et);
  for (i=0; i<n; i++)
    if (the_et[i] == UNINIT) {
      assign (the_et[i], tree (DOCUMENT, ""));
      return path (i); // obtain_ip (the_et[i]);
    }
  insert (the_et, n, tuple (tree (DOCUMENT, "")));
  return path (n); // obtain_ip (the_et[n]);
}

void
delete_document (path rp) {
  assign (subtree (the_et, rp), UNINIT);
  clean_observers (subtree (the_et, rp));
}

void
set_document (path rp, tree t) {
  //assign (subtree (the_et, rp), t);
  assign (subtree (the_et, rp), copy (t));
}
