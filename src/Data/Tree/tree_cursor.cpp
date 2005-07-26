
/******************************************************************************
* MODULE     : tree_cursor.cpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree_cursor.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"

/******************************************************************************
* Finding a closest cursor inside a tree
******************************************************************************/

bool
is_inside (tree t, path p) {
  if (nil (p)) return false;
  else if (is_atomic (t)) {
    string s= t->label;
    int i, n= N(s), k= p->item;
    if (!atom (p) || k<0 || k>n) return false;
    for (i=0; i<k; tm_char_forwards (s, i));
    return i == k;
  }
  else if (atom (p)) return p->item == 0 || p->item == 1;
  else return p->item >= 0 && p->item < N(t) &&
	      is_inside (t[p->item], p->next);
}

path
closest_inside (tree t, path p) {
  // Arbitrary paths may be outside the tree.
  // This routine returns a closest path to p inside the tree t
  if (nil (p)) return path (0);
  else if (is_atomic (t)) {
    string s= t->label;
    int i, n= N(s), k= max (0, min (n, p->item));
    for (i=0; i<k; tm_char_forwards (s, i));
    return i;
  }
  else if (atom (p) || p->item < 0 || p->item >= N(t))
    return path (max (0, min (1, p->item)));
  else return path (p->item, closest_inside (t[p->item], p->next));
}

bool
is_accessible_cursor (tree t, path p) {
  if (is_atomic (t)) return true;
  else if (atom (p)) return !the_drd->is_child_enforcing (t);
  else return the_drd->is_accessible_child (t, p->item) &&
	      is_accessible_cursor (t[p->item], p->next);
}

path
closest_accessible (tree t, path p) {
  // Given a path p inside t, the path may be unaccessible
  // This routine returns the closest path to p inside t which is accessible
  // The routine returns nil if there exists no accessible path inside t
  if (is_atomic (t)) return p;
  else if (atom (p) && !the_drd->is_child_enforcing (t)) return p;
  else {
    int i, k= p->item, n= N(t);
    if (p == 1) k= max (0, n-1);
    for (i=0; i<n; i++) {
      int j= (i&1 == 0? (k+(i>>1) % n): (k+n-((i+1)>>1) % n));
      if (the_drd->is_accessible_child (t, j)) {
	// FIXME: certain tags modify source accessability props
	// FIXME: cells with non-trivial span may lead to unaccessability
	// FIXME: very dynamic markup should be treated after typesetting
	if (atom (p) && is_atomic (t[j]))
	  return path (j, p->item * N (t[j]->label));
	path r= closest_accessible (t[j], atom (p)? p: p->next);
	if (!nil (r)) return path (j, r);
      }
      return path ();
    }
  }
}

bool
is_shifted (tree t, path p, int dir= -1, bool flag= false) {
  if (dir == 0) return true;
  else if (atom (p)) {
    if (flag) {
      if (dir < 0) return p->item != 0;
      else return p->item != right_index (t);
    }
    else return true;
  }
  else if (is_concat (t)) {
    int  i    = p->item;
    bool sflag= flag || (dir<0? i>0: i<N(t)-1);
    return is_shifted (t[i], p->next, dir, sflag);
  }
  else return is_shifted (t[p->item], p->next, dir, false);
}

/******************************************************************************
* Subroutines for cursor paths in trees
******************************************************************************/

bool
valid_cursor (tree t, path p, bool start_flag) {
  if ((!nil (p)) && (!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] testing valid cursor " << p << " in " << t << "\n";
    fatal_error ("bad path", "valid_cursor", "tree_cursor.cpp");
  }

  if (nil (p)) return false;
  if (atom (p)) {
    if (the_drd->is_child_enforcing (t)) return false;
    if (start_flag) return (p->item!=0);
    return true;
  }
  if (is_concat (t))
    return valid_cursor (t[p->item], p->next, start_flag || (p->item!=0));
  if (is_mod_active_once (t))
    return is_atomic (t[0]) || (!atom (p->next));
  if (is_prime (t)) return false;
  // FIXME: hack for treating VAR_EXPAND "math"
  if (is_compound (t, "input", 2) && (N(p) == 2) &&
      is_compound (t[1], "math", 1) && (p->item == 1))
    return false;
  return valid_cursor (t[p->item], p->next, false);
}

static path
pre_correct (tree t, path p) {
  // cout << "Precorrect " << p << " in " << t << "\n";
  if ((!nil (p)) && (!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] precorrecting " << p << " in " << t << "\n";
    fatal_error ("bad path", "pre_correct", "tree_cursor.cpp");
  }

  if (nil (p)) return pre_correct (t, path(0));
  if (atom (p)) {
    if (the_drd->is_child_enforcing (t)) {
      if ((p->item == 0) && the_drd->is_accessible_child (t, 0))
	return path (0, pre_correct (t[0], path (0)));
      else {
	int l= N(t)-1;
	return path (l, pre_correct (t[l], path (right_index (t[l]))));
      }
    }
    return p;
  }
  if (is_mod_active_once (t) && is_compound (t[0]) && atom (p->next)) {
    if (N (t[0]) == 0) return path (0);
    t= t[0]; p= p->next;
    if (p->item==0) return path (0, path (0, pre_correct (t[0], path (0))));
    else {
      int l=N(t)-1;
      return path (0, path (l, pre_correct (t[l], path (right_index (t[l])))));
    }
  }
  if (is_prime (t)) {
    if (p->next->item == 0) return path (0);
    else return path (1);
  }
  // FIXME: hack for treating VAR_EXPAND "math"
  if (is_compound (t, "input", 2) && (N(p) == 2) &&
      is_compound (t[1], "math", 1) && (p->item == 1))
    {
      int i= (p->next->item == 0? 0: right_index (t[1][0]));
      return path (1, 0, pre_correct (t[1][0], path (i)));
    }
  return path (p->item, pre_correct (t[p->item], p->next));
}

static bool
left_most (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "left_most", "tree_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] left most " << p << " in " << t << "\n";
    fatal_error ("bad path", "left_most", "tree_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return i==0;
  if (is_concat (p)) return (i==0) && left_most (t[0], p->next);
  return false;
}

static path
left_correct (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "left_correct", "tree_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] left correcting " << p << " in " << t << "\n";
    fatal_error ("bad path", "left_correct", "tree_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return p;
  if (is_concat (t) && (i>0) && left_most (t[i], p->next))
    return path (i-1, pre_correct (t[i-1], path (right_index (t[i-1]))));
  if (is_prime (t)) return path (0);
  return path (i, left_correct (t[i], p->next));
}

static bool
right_most (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "right_most", "tree_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] right most " << p << " in " << t << "\n";
    fatal_error ("bad path", "right_most", "tree_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return i==right_index (t);
  if (is_concat (p)) return (i==1) && right_most (t[N(t)-1], p->next);
  return false;
}

static path
right_correct (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "right_correct", "tree_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] right correcting " << p << " in " << t << "\n";
    fatal_error ("bad path", "right_correct", "tree_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return p;
  if (is_concat (t) && (i<N(t)-1) && right_most (t[i], p->next))
    return path (i+1, pre_correct (t[i-1], path (0)));
  if (is_prime (t)) return path (1);
  return path (i, right_correct (t[i], p->next));
}

/******************************************************************************
* Exported routines for cursor paths in trees
******************************************************************************/

path
correct_cursor (tree t, path p) {
  // cout << "Correct cursor " << p << " in " << t << "\n";
  return left_correct (t, pre_correct (t, p));
}

path
start (tree t, path p) {
  // cout << "Start " << p << " in " << t << "\n";
  if ((!nil (p)) && (arity (parent_subtree (t, p)) == 0)) return p;
  return correct_cursor (t, p * 0);
}

path
end (tree t, path p) {
  // cout << "End " << p << " in " << t << "\n";
  if ((!nil (p)) && (arity (parent_subtree (t, p)) == 0)) return p;
  return correct_cursor (t, p * right_index (subtree (t, p)));
}

path start (tree t) { return start (t, path ()); }
path end (tree t) { return end (t, path ()); }

path
up_correct (tree t, path p, bool active= true) {
  if (nil (p)) return p;
  if ((p->item<0) || (p->item>=N(t))) return path ();
  if (active && (!the_drd->is_accessible_child (t, p->item))) return path ();
  return path (p->item,
	       up_correct (t[p->item], p->next, !is_mod_active_once (t)));
}

path
super_correct (tree t, path p) {
  path q= path_up (p);
  path r= up_correct (t, q);
  if (q != r) {
    if (nil (r)) fatal_error ("Unexpected situation", "super_correct");
    else if (is_atomic (subtree (t, r))) p= path_up (r) * 0;
    else p= r * 0;
  }
  return correct_cursor (t, p);
}
