
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

/******************************************************************************
* Subroutines for cursor paths in trees
******************************************************************************/

static bool
valid_cursor (tree t, path p, bool start_flag=false) {
  if ((!nil (p)) && (!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] testing valid cursor " << p << " in " << t << "\n";
    fatal_error ("bad path", "valid_cursor", "edit_cursor.cpp");
  }

  if (nil (p)) return false;
  if (atom (p)) {
    if (start_flag && (is_concat (t) || is_prime (t))) return (p->item!=0);
    if (std_drd->is_child_enforcing (t)) return false;
    return true;
  }
  if (is_concat (t))
    return valid_cursor (t[p->item], p->next, start_flag || (p->item!=0));
  if (is_inactive (t))
    return is_atomic (t[0]) || (!atom (p->next));
  if (is_prime (t)) return false;
  return valid_cursor (t[p->item], p->next, false);
}

static path
pre_correct (tree t, path p) {
  if ((!nil (p)) && (!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] precorrecting " << p << " in " << t << "\n";
    fatal_error ("bad path", "pre_correct", "edit_cursor.cpp");
  }

  if (nil (p)) return pre_correct (t, path(0));
  if (atom (p)) {
    if (std_drd->is_child_enforcing (t)) {
      if ((p->item==0) && std_drd->is_accessible_child (t, 0))
	return path (0, pre_correct (t[0], path (0)));
      else {
	int l=N(t)-1;
	return path (l, pre_correct (t[l], path (right_index (t[l]))));
      }
    }
    return p;
  }
  if (is_inactive (t) && is_compound (t[0]) && atom (p->next)) {
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
  return path (p->item, pre_correct (t[p->item], p->next));
}

static bool
left_most (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "left_most", "edit_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] left most " << p << " in " << t << "\n";
    fatal_error ("bad path", "left_most", "edit_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return i==0;
  if (is_concat (p)) return (i==0) && left_most (t[0], p->next);
  return false;
}

static path
left_correct (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "left_correct", "edit_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] left correcting " << p << " in " << t << "\n";
    fatal_error ("bad path", "left_correct", "edit_cursor.cpp");
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
    fatal_error ("invalid nil path", "right_most", "edit_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] right most " << p << " in " << t << "\n";
    fatal_error ("bad path", "right_most", "edit_cursor.cpp");
  }

  int i=p->item;
  if (atom (p)) return i==right_index (t);
  if (is_concat (p)) return (i==1) && right_most (t[N(t)-1], p->next);
  return false;
}

static path
right_correct (tree t, path p) {
  if (nil (p))
    fatal_error ("invalid nil path", "right_correct", "edit_cursor.cpp");
  if ((!atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    cerr << "TeXmacs] right correcting " << p << " in " << t << "\n";
    fatal_error ("bad path", "right_correct", "edit_cursor.cpp");
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
  if (active && (!std_drd->is_accessible_child (t, p->item))) return path ();
  return path (p->item,
	       up_correct (t[p->item], p->next, !is_inactive (t)));
}

path
super_correct (tree t, path p) {
  path q= path_up (p);
  path r= up_correct (t, q);
  if (q != r) {
    if ((!nil (r)) && is_atomic (subtree (t, r))) p= path_up (r) * 0;
    else p= r * 0;
  }
  return correct_cursor (t, p);
}
