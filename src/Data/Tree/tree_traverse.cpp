
/******************************************************************************
* MODULE     : tree_traverse.cpp
* DESCRIPTION: abstract cursor movement and tree traversal
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree_traverse.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"

/******************************************************************************
* Traversal of a tree
******************************************************************************/

static path
traverse_sub (tree t, path p, bool forward) {
  path q = path_up (p);
  int  l = last_item (p);
  tree st= subtree (t, q);
  if (is_atomic (st)) {
    string s= st->label;
    if (forward) {
      if (l<N(s)) {
	tm_char_forwards (s, l);
	return q * l;
      }
    }
    else {
      if (l>0) {
	tm_char_backwards (s, l);
	return q * l;
      }
    }
  }
  else if ((forward && l==0) || (!forward && l==1)) {
    int i, n= N(st);
    if (forward) {
      for (i=0; i<n; i++)
	if (the_drd->is_accessible_child (st, i))
	  return q * path (i, 0);
    }
    else {
      for (i=n-1; i>=0; i--)
	if (the_drd->is_accessible_child (st, i))
	  return q * path (i, right_index (st[i]));
    }
    return q * (1-l);
  }
  else if (nil (q)) return p;

  l = last_item (q);
  q = path_up (q);
  st= subtree (t, q);
  int i, n= N(st);
  if (forward) {
    for (i=l+1; i<n; i++)
      if (the_drd->is_accessible_child (st, i))
	return q * path (i, 0);
  }
  else {
    for (i=l-1; i>=0; i--)
      if (the_drd->is_accessible_child (st, i)) {
	return q * path (i, right_index (st[i]));
    }
  }
  return q * (forward? 1: 0);
}

static path
traverse (tree t, path p, bool forward) {
  path q= p;
  while (true) {
    path r= traverse_sub (t, q, forward);
    if (r == q) return p;
    if (valid_cursor (t, r)) return r;
    q= r;
  }
}

path
next (tree t, path p) {
  return traverse (t, p, true);
}

path
previous (tree t, path p) {
  return traverse (t, p, false);
}

/******************************************************************************
* Word based traversal of a tree
******************************************************************************/

static path
traverse_word (tree t, path p, bool forward) {
  while (true) {
    path q= traverse (t, p, forward);
    if (q == p) return p;
    tree st= subtree (t, path_up (q));
    if (is_atomic (st)) {
      string s= st->label;
      int l= last_item (q), n= N(s);
      if (forward && l>0 && is_iso_alpha (s[l-1]) &&
	  (l==n || !is_iso_alpha (s[l])))
	return q;
      if (!forward && l<n && is_iso_alpha (s[l]) &&
	  (l==0 || !is_iso_alpha (s[l-1])))
	return q;
    }
    else return q;
    p= q;
  }
}

path
next_word (tree t, path p) {
  return traverse_word (t, p, true);
}

path
previous_word (tree t, path p) {
  return traverse_word (t, p, false);
}
