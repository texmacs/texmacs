
/******************************************************************************
* MODULE     : tree_modify.cpp
* DESCRIPTION: high level tree modification subroutines
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_modify.hpp"
#include "drd_std.hpp"
#include "path.hpp"

extern tree the_et;

/******************************************************************************
* DRD-based correction of trees
******************************************************************************/

void
correct_concat_node (tree& t, int done) {
  //cout << "Correct " << t << ", " << done << "\n";
  int i, n= N(t);
  if (n == 0) {
    assign (t, "");
    return;
  }
  for (i=done; i<n; i++) {
    if (t[i] == "") {
      remove (t, i, 1);
      correct_concat_node (t, i);
      return;
    }
    if ((i<n-1) && is_atomic (t[i]) && is_atomic (t[i+1])) {
      join (t, i);
      correct_concat_node (t, i);
      return;
    }
    if (is_concat (t[i])) {
      insert_node (t, 0, CONCAT);
      split (t, 0, i);
      split (t, 1, 1);
      remove_node (t[1], 0);
      if (t[0] == tree (CONCAT)) remove (t, 0, 1);
      else join (t, 0);
      if (t[1] == tree (CONCAT)) remove (t, 1, 1);
      else join (t, 0);
      remove_node (t, 0);
      correct_concat_node (t, max (i-1, 0));
      return;
    }    
  }
}

void
correct_node (tree& t) {
  // NOTE: this routine should only modify t and its descendants,
  // but not any ancestors
  if (is_compound (t)) {
    if (the_drd->contains (as_string (L(t))) &&
        !the_drd->correct_arity (L(t), N(t)))
      assign (t, "");
    if (is_concat (t))
      correct_concat_node (t, 0);
  }
}

void
correct_downwards (tree& t) {
  if (is_compound (t))
    for (int i=0; i<N(t); i++)
      correct_downwards (t[i]);
  correct_node (t);
}

void
correct_upwards (tree& t) {
  correct_node (t);
  path ip= obtain_ip (t);
  if (ip_attached (ip) && !is_nil (ip))
    correct_upwards (subtree (the_et, reverse (ip->next)));
}
