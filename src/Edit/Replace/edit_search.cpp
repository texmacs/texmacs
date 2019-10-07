
/******************************************************************************
* MODULE     : edit_search.cpp
* DESCRIPTION: search and query replace
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Replace/edit_replace.hpp"
#include "Interface/edit_interface.hpp"
#include "drd_std.hpp"
#include "drd_mode.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

edit_replace_rep::edit_replace_rep () {}
edit_replace_rep::~edit_replace_rep () {}

/******************************************************************************
* Structural search routines
******************************************************************************/

bool
edit_replace_rep::inside (string what) {
  return inside (make_tree_label (what));
}

path
edit_replace_rep::search_upwards (string what) {
  return search_upwards (make_tree_label (what));
}

bool
edit_replace_rep::inside (tree_label l) {
  return !is_nil (search_upwards (l));
}

path
edit_replace_rep::search_upwards (tree_label l) {
  path p= path_up (tp, 2);
  while (!is_func (subtree (et, p), l)) {
    if (p == rp) return path ();
    p= path_up (p);
  }
  return p;
}

path
edit_replace_rep::search_parent_upwards (tree_label l, int& last) {
  path p= path_up (tp);
  while (!is_func (subtree (et, path_up (p)), l)) {
    p= path_up (p);
    if (p == rp) {
      last= -1;
      return path ();
    }
  }
  last= last_item (p);
  return path_up (p);
}

path
edit_replace_rep::search_parent_upwards (tree_label l) {
  int last;
  path p= search_parent_upwards (l, last);
  return p * last;
}

bool
edit_replace_rep::inside_with (string var, string val) {
  return !is_nil (search_upwards_with (var, val));
}

path
edit_replace_rep::search_upwards_with (string var, string val) {
  path p= path_up (tp);
  while (true) {
    p= path_up (p);
    if (p == rp) return path ();
    tree st= subtree (et, p);
    if (is_func (st, WITH) && (st[0] == var) && (st[1] == val)) return p;
  }
}

string
edit_replace_rep::inside_which (tree t) {
  path p= search_upwards_in_set (t);
  if ((p == rp) || is_nil (p)) return "";
  tree st= subtree (et, p);
  if (is_func (st, COMPOUND)) return as_string (st[0]);
  else return as_string (L(st));
}

path
edit_replace_rep::search_upwards_in_set (tree t) {
  if (!is_tuple (t)) return path ();
  int i, n=N(t);
  path p= path_up (tp);
  while (true) {
    p= path_up (p);
    if (p == rp) return path ();
    tree st= subtree (et, p);
    for (i=0; i<n; i++) {
      if (is_atomic (t[i])) {
        string s= t[i]->label;
        if (is_quoted (s)) s= raw_unquote (s);
        if (std_contains (s)) {
          tree_label l= as_tree_label (s);
          if (is_func (st, l)) return p;
        }
        else if (is_compound (st, s)) return p;
      }
      else if (is_func (st, L(t))) return p;
    }
  }
}

path
edit_replace_rep::search_previous_compound (path init, string which) {
  path p= init;
  while (true) {
    if (p == rp) return init;
    if (last_item (p) == 0) p= path_up (p);
    else {
      p= path_dec (p);
      while (true) {
        tree st= subtree (et, p);
        if (arity (st) == 0) break;
        p= p * (N(st)-1);
      }
    }
    if (drd->is_accessible_path (et, p) &&
        is_compound (subtree (et, p), which))
      return p;
  }
}

path
edit_replace_rep::search_next_compound (path init, string which) {
  path p= init;
  while (true) {
    if (p == rp) return init;
    if (last_item (p) == (N (subtree (et, path_up (p))) - 1)) p= path_up (p);
    else {
      p= path_inc (p);
      while (true) {
        tree st= subtree (et, p);
        if (arity (st) == 0) break;
        p= p * 0;
      }
    }
    if (drd->is_accessible_path (et, p) &&
        is_compound (subtree (et, p), which))
      return p;
  }
}

/******************************************************************************
* Test whether we found a match
******************************************************************************/

static bool
test_match (tree t, tree pat) {
  // FIXME: we use empty strings for wildcards now
  // we should support real wildcards and regular expressions later
  if (pat == "") return true;
  else if (is_atomic (t) || is_atomic (pat)) return t == pat;
  else if ((L(t) != L(pat)) || (N(t) != N(pat))) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (!test_match (t[i], pat[i]))
        return false;
    return true;
  }
}

path
edit_replace_rep::test_sub (path p, tree t) {
  // cout << "Test " << subtree (et, path_up (p))
  //      << " :: " << t << " at " << p << "\n";
  if (is_concat (t) && (N(t) > 1)) {
    if (N(p) <= 1) return p;
    tree st= subtree (et, path_up (p, 2));
    if (!is_concat (st)) return p;
    int i, l= last_item (path_up (p)), n= N(t);
    if (N(st) < (n + l)) return p;
    if ((t[0] != "") && (p == end (et, path_up (p)))) return p;
    if (test_sub (p, t[0]) != end (et, path_up (p))) return p;
    for (i=1; i<n-1; i++)
      if (!test_match (st[l+i], t[i]))
        return p;
    path r= path_up (p, 2) * path (l+n-1, start (st[l+n-1], path ()));
    path q= test_sub (r, t[n-1]);
    if (q == r) return p;
    return q;
  }
  else {
    tree st= subtree (et, path_up (p));
    if (is_compound (t)) {
      if (!is_compound (st)) return p;
      // cout << "Test with " << st << "\n";
      if (last_item (p) != 0) return p;
      if (!test_match (st, t)) return p;
      return path_inc (p);
    }
    else {
      if (is_compound (st)) return p;
      int l= last_item (p);
      // cout << "Test with " << st->label << " at " << l << "\n";
      if (N(st->label) < (N(t->label) + l)) return p;
      if (st->label (l, l + N(t->label)) != t->label) return p;
      return path_add (p, N (t->label));
    }
  }
}

path
edit_replace_rep::test (path p, tree t) {
  path q= test_sub (p, t);
  if (q == p) return p;
  string mode= as_string (get_env_value (MODE, p));
  string lan = as_string (get_env_value (MODE_LANGUAGE (mode), p));
  if (search_mode != mode) return p;
  if (search_lan != lan) return p;
  return q;
}

/******************************************************************************
* Traversal of the edit tree
******************************************************************************/

void
edit_replace_rep::step_ascend (bool forward) {
  // cout << "Step ascend at " << search_at << "\n";
  search_at= path_add (path_up (search_at), forward? 1: -1);
  tree st;
  int  l;
  while (true) {
    st= subtree (et, path_up (search_at));
    l = last_item (search_at);
    // cout << "  st= " << st << "\n";
    // cout << "  l = " << l << "\n";
    if ((l<0) || (l>=N(st)) || drd->is_accessible_child (st, l)) break;
    search_at= path_add (search_at, forward? 1: -1);
  }

  if (forward) {
    if (l == N(st)) {
      if (is_atom (search_at / rp)) search_at= rp;
      else step_ascend (forward);
    }
    else step_descend (forward);
  }
  else {
    if (l == -1) {
      if (is_atom (search_at / rp)) search_at= rp;
      else step_ascend (forward);
    }
    else step_descend (forward);
  }
}

void
edit_replace_rep::step_descend (bool forward) {
  // cout << "Step descend at " << search_at << "\n";
  tree st= subtree (et, search_at);
  // cout << "  st= " << st << "\n";
  int last= (is_atomic (st)? N(st->label): N(st)-1);
  search_at= search_at * (forward? 0: last);
  if (is_format (st))
    step_descend (forward);
}

void
edit_replace_rep::step_horizontal (bool forward) {
  // cout << "Step horizontal at " << search_at << "\n";
  if (search_at == rp) step_descend (forward);
  else {
    tree st  = subtree (et, path_up (search_at));
    int  l   = last_item (search_at);

    // cout << "  st= " << st << "\n";
    if (forward) {
      if (l == right_index (st)) step_ascend (forward);
      else {
        if (is_atomic (st)) {
          if (st->label[l]=='<') {
            string s= st->label;
            while ((l<N(s)) && (s[l]!='>')) l++;
            if (l<N(s)) l++;
            search_at= path_up (search_at) * l;
          }
          else search_at= path_inc (search_at);
        }
        else {
          int i;
          for (i=l; i<N(st); i++)
            if (drd->is_accessible_child (st, i)) {
              search_at= path_up (search_at) * i;
              step_descend (forward);
              return;
            }
          step_ascend (forward);
        }
      }
    }
    else {
      if (l == 0) step_ascend (forward);
      else {
        if (is_atomic (st)) {
          if (st->label[l-1]=='>') {
            string s= st->label;
            l--;
            while ((l>0) && (s[l]!='<')) l--;
            search_at= path_up (search_at) * l;
          }
          else search_at= path_dec (search_at);
        }
        else {
          int i;
          for (i=l; i>=0; i--)
            if (drd->is_accessible_child (st, i)) {
              search_at= path_up (search_at) * i;
              step_descend (forward);
              return;
            }
          step_ascend (forward);
        }
      }
    }
  }
}

void
edit_replace_rep::next_match (bool forward) {
  // cout << "Next match at " << search_at << "\n";
  while (true) {
    if (search_at == rp) {
      set_selection (tp, tp);
      notify_change (THE_SELECTION);
      return;
    }
    search_end= test (search_at, search_what);
    if (search_end != search_at) {
      go_to (copy (search_end));
      show_cursor_if_hidden ();
      set_selection (search_at, search_end);
      notify_change (THE_SELECTION);
      return;
    }
    int new_mode= DRD_ACCESS_HIDDEN;
    if (get_init_string (MODE) == "src" || inside ("show-preamble"))
      new_mode= DRD_ACCESS_SOURCE;
    int old_mode= set_access_mode (new_mode);
    step_horizontal (forward);
    set_access_mode (old_mode);
  }
}

/******************************************************************************
* Searching
******************************************************************************/

void
edit_replace_rep::search_start (bool flag) {
  string r ("forward search");
  if (!flag) r= "backward search";

  forward     = flag;
  search_mode = copy (get_env_string (MODE));
  search_lan  = copy (get_env_string (MODE_LANGUAGE (search_mode)));
  search_at   = tp;
  search_what = tree ("");
  where_stack = list<path> ();
  what_stack  = tree ("");
  set_input_mode (INPUT_SEARCH);
  set_message ("Searching", r);
}

void
edit_replace_rep::search_next (bool forward) {
  string r ("forward search");
  if (!forward) r= "backward search";
  string w= as_string (search_what);
  if (is_compound (search_what)) w= "compound expression";

  next_match (forward);
  if (search_at == rp) {
    set_message (concat ("No more matches for ", verbatim (w)), r);
    beep ();
  }
  else set_message (concat ("Searching ", verbatim (w)), r);
}

void
edit_replace_rep::search_next (tree what, bool forward, bool step) {
  where_stack= list<path> (copy (search_at), where_stack);
  what_stack = tuple (copy (search_what), what_stack);
  search_what= copy (what);
  if (step) step_horizontal (forward);
  search_next (forward);
}

void
edit_replace_rep::search_stop () {
  tree t= tuple ("texmacs", search_what, search_mode, search_lan);
  set_input_normal ();
  if (search_what != "")
    selection_raw_set ("search", t);
}

void
edit_replace_rep::search_button_next () {
  set_input_mode (INPUT_SEARCH);
  search_next (search_what, forward, true);
}

bool
edit_replace_rep::search_keypress (string s) {
  set_message ("", "");
  if (s == "space") s= " ";
  if (N(s) == 1) {
    if (is_atomic (search_what))
      search_next (as_string (search_what) * s, forward, false);
  }
  else {
    if ((s == "left") || (s == "right") ||
        (s == "up") || (s == "down") ||
        (s == "pageup") || (s == "pagedown") ||
        (s == "begin") || (s == "end")) {
      search_stop ();
      return false;      
    }
    else if ((s == "C-c") || (s == "C-g"))
      search_stop ();
    else if ((s == "next") || (s == "previous")) {
      if (search_what == "") {
        tree t= selection_raw_get ("search");
        if (is_tuple (t, "texmacs", 3) &&
            (t[1] != "") &&
            (t[2] == search_mode) &&
            (t[3] == search_lan))
          search_next (t[1], s != "previous", true);
      }
      else search_next (search_what, s != "previous", true);
    }
    else if ((s == "delete") || (s == "backspace")) {
      if (is_nil (where_stack))
        search_stop ();
      else if (is_atom (where_stack)) {
        go_to (where_stack->item);
        search_stop ();
      }
      else {
        search_at  = where_stack->item;
        where_stack= where_stack->next;
        search_what= what_stack[0];
        what_stack = what_stack[1];
        search_next (forward);
      }
    }
    else if ((s == "C-left") || (s == "C-right")) {
      // FIXME: integrate better with general searching mechanism
      path p= path_up (search_at);
      while ((p != rp) && (!is_extension (subtree (et, path_up (p)))))
        p= path_up (p);
      if (p == rp) return true;
      path r= path_up (p);
      string w= as_string (L (subtree (et, r)));
      path q= (s == "C-right") ?
        search_next_compound (r, w) :
        search_previous_compound (r, w);
      if (q == r) {
        set_message ("No more matches", "search similar structure");
        beep ();
      }
      else {
        q= q * min (N (subtree (et, q)) - 1, last_item (p));
        search_at= end (et, q);
        go_to (copy (search_at));
      }
    }
  }
  return true;
}

/******************************************************************************
* Replacing
******************************************************************************/

void
edit_replace_rep::replace_start (tree what, tree by, bool flag) {
  forward     = flag;
  search_mode = copy (get_env_string (MODE));
  search_lan  = copy (get_env_string (MODE_LANGUAGE (search_mode)));
  search_at   = tp;
  search_what = copy (what);
  replace_by  = copy (by);
  nr_replaced = 0;
  set_input_mode (INPUT_REPLACE);
  if (search_what == "") {
    tree t= selection_raw_get ("search");
    if (is_tuple (t, "texmacs", 3) &&
        (t[1] != "") &&
        (t[2] == search_mode) &&
        (t[3] == search_lan))
      search_what= t[1];
    t= selection_raw_get ("replace");
    if (is_tuple (t, "texmacs", 3) &&
        (t[1] != "") &&
        (t[2] == search_mode) &&
        (t[3] == search_lan))
      replace_by= t[1];
  }
  replace_next ();
}

void
edit_replace_rep::replace_next () {
  string r ("forward replace");
  if (!forward) r= "backward replace";

  next_match (forward);
  if (search_at == rp) {
    tree l= concat ("Replaced ", as_string (nr_replaced), " occurrences");
    if (nr_replaced == 0) l= "No matches found";
    if (nr_replaced == 1) l= "Replaced one occurrence";
    set_message (l, r);
    beep ();
    set_input_normal ();
  }
  else set_message ("Replace (y,n,a)?", r);
}

bool
edit_replace_rep::replace_keypress (string s) {
  set_message ("", "");
  if (s == "space") s= " ";
  if ((s == "C-c") || (s == "C-g") || (s == "escape"))
    set_input_normal ();
  else if (s == "y") {
    nr_replaced++;
    go_to (copy (search_end));
    cut (search_at, search_end);
    insert_tree (copy (replace_by));
    search_at= copy (tp);
    replace_next ();
  }
  else if (s == "n") {
    step_horizontal (forward);
    replace_next ();
  }
  else if (s == "a" || s == "!") {
    while (search_at != rp) {
      nr_replaced++;
      go_to (copy (search_end));
      cut (search_at, search_end);
      insert_tree (copy (replace_by));
      search_at= copy (tp);
      replace_next ();
    }
  }
  return true;
}
