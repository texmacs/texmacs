
/******************************************************************************
* MODULE     : evaluate_textual.cpp
* DESCRIPTION: operations on text (strings, tuples and trees)
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "analyze.hpp"
#include "vars.hpp"
#include "language.hpp"
#include "gui.hpp"
#include "file.hpp"
#include "dictionary.hpp"

/******************************************************************************
* Array-like operations on strings and compound structures
******************************************************************************/

tree
evaluate_merge (tree t) {
  int i, n= N(t);
  if (n == 0) return "";
  tree acc= evaluate (t[0]);
  if (is_concat (acc)) acc= tree_as_string (acc);
  for (i=1; i<n; i++) {
    tree add= evaluate (t[i]);
    if (is_atomic (acc) && (is_atomic (add) || is_concat (add)))
      acc= acc->label * tree_as_string (add);
    else if (is_tuple (acc) && is_tuple (add))
      acc= acc * add;
    else if (is_func (acc, MACRO) && is_func (add, MACRO) &&
	     (N(acc) == N(add)) &&
	     (acc (0, N(acc)-1) == add (0, N(add)-1)))
      {
	tree r = copy (acc);
	tree u1= copy (acc[N(acc)-1]);
	tree u2= copy (add[N(add)-1]);
	tree u (CONCAT, u1, u2);
	if (u1 == "") u= u2;
	else if (u2 == "") u= u1;
	else if (is_atomic (u1) && is_atomic (u2))
	  u= u1->label * u2->label;
	r[N(r)-1]= u;
	acc= r;
      }
    else return evaluate_error ("bad merge");
  }
  return acc;
}

tree
evaluate_length (tree t) {
  if (N(t)!=1) return evaluate_error ("bad length");
  tree t1= evaluate (t[0]);
  if (is_compound (t1)) {
    if (is_tuple (t1)) return as_string (N (t1));
    return evaluate_error ("bad length");
  }
  return as_string (N (t1->label));
}

tree
evaluate_range (tree t) {
  if (N(t)!=3) return evaluate_error ("bad range");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  tree t3= evaluate (t[2]);
  if (!(is_int (t2) && is_int (t3))) return evaluate_error ("bad range");
  if (is_compound (t1)) {
    if (is_tuple (t1)) {
      int i1= max (0, as_int (t2));
      int i2= min (N (t1), as_int (t3));
      i2 = max (i1, i2);
      return t1 (i1, i2);
    }
    return evaluate_error ("bad range");
  }
  int i1= max (0, as_int (t2));
  int i2= min (N(t1->label), as_int (t3));
  i2 = max (i1, i2);
  return t1->label (i1, i2);
}

/******************************************************************************
* Routines on strings
******************************************************************************/

tree
evaluate_number (tree t) {
  if (N(t)!=2) return evaluate_error ("bad number");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad number");
  string s1= t1->label;
  string s2= t2->label;
  int nr= as_int (s1);
  if (s2 == "arabic") return as_string (nr);
  if (s2 == "roman")  return roman_nr  (nr);
  if (s2 == "Roman")  return Roman_nr  (nr);
  if (s2 == "alpha")  return alpha_nr  (nr);
  if (s2 == "Alpha")  return Alpha_nr  (nr);
  if (s2 == "fnsymbol")
    return tree (WITH, MODE, "math", tree (RIGID, fnsymbol_nr (nr)));
  return evaluate_error ("bad number");
}

tree
evaluate_date (tree t) {
  if (N(t)>2) return evaluate_error ("bad date");
  string lan= as_string (std_env [LANGUAGE]);
  if (N(t) == 2) {
    tree u= evaluate (t[1]);
    if (is_compound (u)) return evaluate_error ("bad date");
    lan= u->label;
  }
  string fm= "";
  if (N(t) != 0) {
    tree u= evaluate (t[0]);
    if (is_compound (u)) return evaluate_error ("bad date");
    fm= u->label;
  }
  return get_date (lan, fm);
}

tree
evaluate_translate (tree t) {
  if (N(t)!=3) return evaluate_error ("bad translate");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  tree t3= evaluate (t[2]);
  if (is_compound (t1) || is_compound (t2) || is_compound (t3))
    return evaluate_error ("bad translate");
  return translate (t1->label, t2->label, t3->label);
}

tree
evaluate_change_case (tree t, tree nc, bool evaluate_flag, bool first) {
  if (is_atomic (t)) {
    string s= t->label;
    tree   r= copy (s);
    int i, n= N(s);

    bool all= true;
    bool up = false;
    bool lo = false;
    if (nc == "Upcase") { all= false; up= true; }
    else if (nc == "UPCASE") { up= true; }
    else if (nc == "locase") { lo= true; }

    for (i=0; i<n; tm_char_forwards (s, i))
      if (is_iso_alpha (s[i]) && (all || (first && (i==0)))) {
	if (up && is_iso_locase (s[i])) r->label[i]= upcase (s[i]);
	if (lo && is_iso_upcase (s[i])) r->label[i]= locase (s[i]);
      }
    r->obs= list_observer (ip_observer (obtain_ip (t)), r->obs);
    return r;
  }
  else if (is_concat (t)) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= evaluate_change_case (t[i], nc, evaluate_flag, first && (i==0));
    r->obs= list_observer (ip_observer (obtain_ip (t)), r->obs);
    return r;
  }
  else {
    if (evaluate_flag) return t;
    else return evaluate_change_case (evaluate (t), nc, true, first);
  }
}

tree
evaluate_change_case (tree t) {
  if (N(t) < 2) return evaluate_error ("bad change case");
  return evaluate_change_case (t[0], evaluate (t[1]), false, true);
}

tree
evaluate_find_file (tree t) {
  int i, n=N(t);
  array<tree> r (n);
  for (i=0; i<n; i++) {
    r[i]= evaluate (t[i]);
    if (is_compound (r[i]))
      return evaluate_error ("bad find file");
  }
  for (i=0; i<(n-1); i++) {
    url u= resolve (url (r[i]->label, r[n-1]->label));
    if (!is_none (u)) {
      if (is_rooted (u, "default")) u= reroot (u, "file");
      return as_string (u);
    }
  }
  url base_file_name (as_string (std_env["base-file-name"]));
  url u= resolve (base_file_name * url_parent () * r[n-1]->label);
  if (!is_none (u)) {
    if (is_rooted (u, "default")) u= reroot (u, "file");
    return as_string (u);
  }
  return "false";
}

/******************************************************************************
* Routines on tuples
******************************************************************************/

tree
evaluate_is_tuple (tree t) {
  if (N(t)!=1) return evaluate_error ("bad tuple query");
  return as_string_bool(is_tuple (evaluate (t[0])));
}

tree
evaluate_lookup (tree t) {
  if (N(t)!=2) return evaluate_error ("bad look up");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (!(is_compound (t1) && is_int (t2)))
    return evaluate_error ("bad look up");
  int i= as_int (t2);
  if (i < 0 || i >= N(t1))
    return evaluate_error ("index out of range in look up");
  return t1[i];
}
