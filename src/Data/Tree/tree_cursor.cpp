
/******************************************************************************
* MODULE     : tree_cursor.cpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_cursor.hpp"
#include "drd_std.hpp"
#include "drd_mode.hpp"
#include "analyze.hpp"
#include "vars.hpp"

/******************************************************************************
* Finding a closest cursor inside a tree
******************************************************************************/

bool
is_inside (tree t, path p) {
  if (is_nil (p)) return false;
  else if (is_atomic (t)) {
    string s= t->label;
    int i, n= N(s), k= p->item;
    if (!is_atom (p) || k<0 || k>n) return false;
    for (i=0; i<k; tm_char_forwards (s, i)) {}
    return i == k;
  }
  else if (is_atom (p))
    return p->item == 0 || p->item == 1;
  else if (is_func (t, RAW_DATA, 1))
    return p == path (0, 0);
  else return p->item >= 0 && p->item < N(t) &&
	      is_inside (t[p->item], p->next);
}

path
closest_inside (tree t, path p) {
  // Arbitrary paths may be outside the tree.
  // This routine returns a closest path to p inside the tree t
  if (is_nil (p)) return path (0);
  else if (is_atomic (t)) {
    string s= t->label;
    int i, n= N(s), k= max (0, min (n, p->item));
    for (i=0; i<k; tm_char_forwards (s, i)) {}
    return i;
  }
  else if (is_atom (p) || p->item < 0 || p->item >= N(t))
    return path (max (0, min (1, p->item)));
  else if (is_func (t, RAW_DATA, 1))
    return path (0, 0);
  else return path (p->item, closest_inside (t[p->item], p->next));
}

static bool
is_modified_accessible (tree t, path p, bool activate, bool persistent) {
  if (p->item != 0) return false;
  t= t[0]; p= p->next;
  if (is_atomic (t)) return is_accessible_cursor (t, p);
  else if (is_atom (p) && !persistent) return false;
  else {
    bool r;
    int old= set_access_mode (activate? DRD_ACCESS_NORMAL: DRD_ACCESS_SOURCE);
    if (persistent) r= is_accessible_cursor (t, p);
    else r= the_drd->is_accessible_child (t, p->item);
    set_access_mode (old);
    if (!persistent) r= r && is_accessible_cursor (t[p->item], p->next);
    return r;
  }
}

static bool
next_without_border (tree t, path p) {
  return false;
  // Assuming that t is a concat, check whether p does not correspond
  // to an inaccessible border position
  int i= p->item + 1;
  if (i >= N(t)) return false;
  if (is_compound (t[i]) && the_drd->is_child_enforcing (t[i]))
    return p->next == end (t[p->item]);
  return false;
}

static int
lowest_accessible_child (tree t) {
  for (int i=0; i<N(t); i++)
    if (the_drd->is_accessible_child (t, i))
      return i;
  return 0;
}

static int
highest_accessible_child (tree t) {
  for (int i=N(t)-1; i>=0; i--)
    if (the_drd->is_accessible_child (t, i))
      return i;
  return N(t) - 1;
}

static bool
graphics_in_path (tree t, path p) {
  // FIXME: when in the cursor is inside graphics,
  // it cannot be at the start/end.  There should be
  // a more robust way to ensure this.
  if (is_nil (p) || is_atom (p)) return false;
  if (is_atomic (t) || p->item < 0 || p->item >= N(t)) return false;
  if (is_func (t, GRAPHICS)) return true;
  return graphics_in_path (t[p->item], p->next);
}

bool
is_accessible_cursor (tree t, path p) {
  if (is_atomic (t) || is_atom (p)) {
    if (get_writable_mode () == DRD_WRITABLE_INPUT &&
	get_access_mode () != DRD_ACCESS_SOURCE)
      return false;
    else if (is_atomic (t))
      return is_atom (p) && p->item >= 0 && p->item <= N(t->label);
    else return !the_drd->is_child_enforcing (t);
  }
  else if (0 > p->item || p->item >= N(t)) return false;
  else if (the_drd->is_parent_enforcing (t) &&
           !graphics_in_path (t, p) &&
	   ((p->item == lowest_accessible_child (t) &&
             p->next == start (t[p->item])) ||
	    (p->item == highest_accessible_child (t) &&
             p->next == end (t[p->item]))))
    return false;
  else switch (L(t)) {
    case CONCAT:
      if (!is_accessible_cursor (t[p->item], p->next)) return false;
      else return !next_without_border (t, p);
    case ACTIVE:
      return is_modified_accessible (t, p, true, false);
    case VAR_ACTIVE:
      return is_modified_accessible (t, p, true, true);
    case INACTIVE:
      return is_modified_accessible (t, p, false, false);
    case VAR_INACTIVE:
      return is_modified_accessible (t, p, false, true);
    default:
      if (!the_drd->is_accessible_child (t, p->item)) return false;
      else if (the_drd->get_env_child (t, p->item, MODE, "") == "src") {
	int old_mode= set_access_mode (DRD_ACCESS_SOURCE);
	bool r= is_accessible_cursor (t[p->item], p->next);
	set_access_mode (old_mode);
	return r;
      }
      else {
	int old_mode= get_writable_mode ();
	if (old_mode != DRD_WRITABLE_ANY) {
	  int w  = the_drd->get_writability_child (t, p->item);
	  if (w == WRITABILITY_DISABLE)
	    set_writable_mode (DRD_WRITABLE_INPUT);
	  else if (w == WRITABILITY_ENABLE)
	    set_writable_mode (DRD_WRITABLE_NORMAL);
	}
	bool r= is_accessible_cursor (t[p->item], p->next);
	set_writable_mode (old_mode);
	return r;
      }
    }
}

path
closest_accessible (tree t, path p, int dir) {
  // Given a path p inside t, the path may be unaccessible
  // This routine returns the closest path to p inside t which is accessible
  // dir in {-1, 0, 1} specifies the search direction
  // The routine returns nil if there exists no accessible path inside t
  if (is_atomic (t)) return p;
  else if (is_nil (p)) return closest_accessible (t, path (0), dir);
  else if (is_atom (p) && !the_drd->is_child_enforcing (t)) return p;
  else {
    int i, k= p->item, n= N(t);
    if (p == path (1)) k= max (0, n-1);
    for (i=0; i<n; i++) {
      int j;
      if (dir == 0)
        j= ((i&1) == 0? (k+(i>>1) % n): (k+n-((i+1)>>1) % n));
      else if (dir == 1) {
        j= k+i;
        if (j >= n) {
          if (the_drd->is_child_enforcing (t)) break;
          return path (1);
        }
      }
      else {
        j= k-i;
        if (j < 0) {
          if (the_drd->is_child_enforcing (t)) break;
          return path (0);
        }
      }
      if (the_drd->is_accessible_child (t, j)) {
	// FIXME: certain tags modify source accessability props
	// FIXME: cells with non-trivial span may lead to unaccessability
	// FIXME: very dynamic markup should be treated after typesetting
	if (is_atom (p) && is_atomic (t[j]))
	  return path (j, p->item * (j < k? N (t[j]->label): 0));
	path sp  = (j == k? p->next: (j < k? path (1): path (0)));
        int  sdir= (j == k? dir: (j < k? -1: 1));
        path prop= (p == path (0)? p: path (right_index (t[j])));
        path sp2 = (is_nil (sp)? prop: sp);
	path r   = closest_accessible (t[j], sp2, sdir);
	if (!is_nil (r)) {
	  r= path (j, r);
	  if (!is_concat (t) || !next_without_border (t, r)) {
	    if (the_drd->is_parent_enforcing (t) &&
                !graphics_in_path (t, p) &&
                !is_accessible_cursor (t, p)) {
	      if (r->item == lowest_accessible_child (t)) return path (0);
	      if (r->item == highest_accessible_child (t)) return path (1);
	    }
	    return r;
	  }
	}
      }
    }
    return path ();
  }
}

path
closest_accessible_inside (tree t, path p, int dir) {
  path q= closest_accessible (t, p, dir);
  if (!is_nil (q)) return q;
  if (dir <= 0) return path (0);
  else return path (right_index (t));
}

/******************************************************************************
* Shifting the cursor
******************************************************************************/

bool
is_shifted (tree t, path p, int dir= -1, bool flag= false) {
  if (dir == 0) return true;
  else if (is_atom (p)) {
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

static path
extreme (tree t, int dir) {
  if (dir < 0) return start (t);
  else return end (t);
}

path
shift (tree t, path p, int dir) {
  if (dir == 0 || is_nil (p) || is_atom (p)) return p;
  else if (is_concat (t) && p->next == extreme (t[p->item], dir)) {
    for (int i= p->item + dir; i >= 0 && i < N(t); i += dir)
      if (the_drd -> is_accessible_child (t, i))
	return path (i, extreme (t[i], -dir));
    return p;
  }
  else return path (p->item, shift (t[p->item], p->next, dir));
}

/******************************************************************************
* Subroutines for cursor paths in trees
******************************************************************************/

bool
valid_cursor (tree t, path p, bool start_flag) {
  if ((!is_nil (p)) && (!is_atom (p)) &&
      ((p->item < 0) || (p->item >= arity (t)))) {
    failed_error << "Testing valid cursor " << p << " in " << t << "\n";
    FAILED ("bad path");
  }

  if (is_nil (p)) return false;
  if (is_atom (p)) {
    if (the_drd->is_child_enforcing (t)) return false;
    if (start_flag) return (p->item!=0);
    return true;
  }
  if (the_drd->is_parent_enforcing (t) &&
      !graphics_in_path (t, p) &&
      ((p->item == lowest_accessible_child (t) &&
        p->next == start (t[p->item])) ||
       (p->item == highest_accessible_child (t) &&
        p->next == end (t[p->item]))))
    return false;
  if (is_concat (t)) {
    if (next_without_border (t, p)) return false;
    return valid_cursor (t[p->item], p->next, start_flag || (p->item!=0));
  }
  if (is_mod_active_once (t))
    return is_atomic (t[0]) || (!is_atom (p->next));
  if (is_prime (t)) return false;
  // FIXME: hack for treating VAR_EXPAND "math"
  if (is_compound (t, "input", 2) && (N(p) == 2) &&
      is_compound (t[1], "math", 1) && (p->item == 1))
    return false;
  if (is_func (t, BIG_AROUND) && p->item == 1) {
    if (p == path (1, 0) && is_right_script_prime (t[1])) return false;
    if (p == path (1, 0, 0) && is_concat (t[1]) &&
        N(t[1]) > 0 && is_right_script_prime (t[1][0])) return false;
  }
  return valid_cursor (t[p->item], p->next, false);
}

static path
pre_correct (tree t, path p) {
  //cout << "Precorrect " << p << " in " << t << "\n";
  if ((!is_nil (p)) && (!is_atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    if (is_func (t, GRAPHICS)) {
      std_warning << "Precorrecting " << p << " in " << t << "\n";
      p= path (1);
    }
    else {
      failed_error << "Precorrecting " << p << " in " << t << "\n";
      FAILED ("bad path");
    }
  }

  if (is_nil (p)) return pre_correct (t, path(0));
  if (is_atom (p)) {
    if (!is_atomic (t) && the_drd->is_child_enforcing (t)) {
      if (p->item == 0) {
        for (int i=0; i<N(t); i++)
          if (i == N(t)-1 || the_drd->is_accessible_child (t, i))
            return path (i, pre_correct (t[i], path (0)));
      }
      else {
        for (int i=N(t)-1; i>=0; i--)
          if (i == 0 || the_drd->is_accessible_child (t, i))
            return path (i, pre_correct (t[i], path (right_index (t[i]))));
      }
      FAILED ("nullary tree with no border");
    }
    return p;
  }
  if (is_mod_active_once (t) && is_compound (t[0]) && is_atom (p->next)) {
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
  path r (p->item, pre_correct (t[p->item], p->next));
  if (the_drd->is_parent_enforcing (t) &&
      !graphics_in_path (t, p)) {
    if (r->item == lowest_accessible_child (t) &&
        !valid_cursor (t, p, false))
      return path (0);
    if (r->item == highest_accessible_child (t) &&
        !valid_cursor (t, p, false))
      return path (1);	    
  }
  return r;
}

static bool
left_most (tree t, path p) {
  if (is_nil (p)) FAILED ("invalid nil path");
  if ((!is_atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    failed_error << "Left most " << p << " in " << t << "\n";
    FAILED ("bad path");
  }

  int i=p->item;
  if (is_atom (p)) return i==0;
  if (is_concat (p)) return (i==0) && left_most (t[0], p->next);
  return false;
}

static path
left_correct (tree t, path p) {
  //cout << "Left correct " << p << " in " << t << "\n";
  if (is_nil (p)) FAILED ("invalid nil path");
  if ((!is_atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    failed_error << "Left correcting " << p << " in " << t << "\n";
    FAILED ("bad path");
  }

  int i=p->item;
  if (is_atom (p)) return p;
  if (is_concat (t) && (i>0) && left_most (t[i], p->next))
    return path (i-1, pre_correct (t[i-1], path (right_index (t[i-1]))));
  if (is_prime (t)) return path (0);
  return path (i, left_correct (t[i], p->next));
}

static bool
right_most (tree t, path p) {
  if (is_nil (p)) FAILED ("invalid nil path");
  if ((!is_atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    failed_error << "Right most " << p << " in " << t << "\n";
    FAILED ("bad path");
  }

  int i=p->item;
  if (is_atom (p)) return i==right_index (t);
  if (is_concat (p)) return (i==1) && right_most (t[N(t)-1], p->next);
  return false;
}

static path
right_correct (tree t, path p) {
  if (is_nil (p)) FAILED ("invalid nil path");
  if ((!is_atom (p)) && ((p->item < 0) || (p->item >= arity (t)))) {
    failed_error << "Right correcting " << p << " in " << t << "\n";
    FAILED ("bad path");
  }

  int i=p->item;
  if (is_atom (p)) return p;
  if (is_concat (t) && (i<N(t)-1) && right_most (t[i], p->next))
    return path (i+1, pre_correct (t[i+1], path (0)));
  if (is_prime (t)) return path (1);
  return path (i, right_correct (t[i], p->next));
}

static path
keep_positive (path p) {
  if (is_nil (p)) return p;
  if (p->item < 0) return path ();
  return path (p->item, keep_positive (p->next));
}

/******************************************************************************
* Exported routines for cursor paths in trees
******************************************************************************/

path
correct_cursor (tree t, path p, bool forwards) {
  //cout << "Correct cursor " << p << " in " << t << "\n";
  p= keep_positive (p);
  path pp= pre_correct (t, p);
  if (forwards) return right_correct (t, pp);
  else return left_correct (t, pp);
}

path
start (tree t, path p) {
  //cout << "Start " << p << " in " << t << "\n";
  if ((!is_nil (p)) && (arity (parent_subtree (t, p)) == 0)) return p;
  return correct_cursor (t, p * 0);
}

path
end (tree t, path p) {
  //cout << "End " << p << " in " << t << "\n";
  if ((!is_nil (p)) && (arity (parent_subtree (t, p)) == 0)) return p;
  return correct_cursor (t, p * right_index (subtree (t, p)));
}

path start (tree t) { return start (t, path ()); }
path end (tree t) { return end (t, path ()); }

path
up_correct (tree t, path p, bool active= true) {
  //cout << "Up correct " << p << " in " << t << "\n";
  if (is_nil (p)) return p;
  if ((p->item<0) || (p->item>=N(t))) return path ();
  if (active && (!the_drd->is_accessible_child (t, p->item))) return path ();
  return path (p->item,
	       up_correct (t[p->item], p->next, !is_mod_active_once (t)));
}

path
super_correct (tree t, path p, bool forwards) {
  path q= path_up (p);
  path r= up_correct (t, q);
  if (q != r) {
    ASSERT (!is_nil (r), "unexpected situation");
    int last= (forwards? 1: 0);
    if (is_atomic (subtree (t, r))) p= path_up (r) * last;
    else p= r * last;
  }
  return correct_cursor (t, p, forwards);
}
