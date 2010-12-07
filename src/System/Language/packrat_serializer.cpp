
/******************************************************************************
* MODULE     : packrat_serializer.cpp
* DESCRIPTION: serializing trees as strings
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "packrat_parser.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Useful subroutines
******************************************************************************/

static tree
replace (tree t, hashmap<tree,tree> h) {
  if (h->contains (t)) return h[t];
  else if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= replace (t[i], h);
    return r;
  }
}

static path
as_path (tree t) {
  ASSERT (is_tuple (t), "invalid path");
  path p;
  int i, n= N(t);
  for (i=n-1; i>=0; i--)
    p= path (as_int (t[i]), p);
  return p;
}

/******************************************************************************
* Setting up the input
******************************************************************************/

void
packrat_parser_rep::serialize (tree t, path p) {
  if (is_nil (p) || p->item != -1)
    current_start (p)= N(current_string);
  if (is_atomic (t)) {
    int pos=0;
    string s= t->label;
    while (pos<N(s)) {
      int start= pos;
      tm_char_forwards (s, pos);
      if (pos == start+1)
	current_string << s[start];
      else {
	string ss= s (start, pos);
	string cl= the_drd->get_class (ss);
	//if (cl != "") cout << "Class " << ss << " -> " << cl << "\n";
	if (cl == "") current_string << ss;
	else current_string << "<\\" << cl << ">" << ss << "</>";
      }
    }
  }
  else if (is_func (t, RAW_DATA, 1))
    current_string << "<\\rawdata></>";
  else if (is_func (t, CONCAT) || is_func (t, DOCUMENT)) {
    for (int i=0; i<N(t); i++) {
      serialize (t[i], p * i);
      if (is_func (t, DOCUMENT)) current_string << "\n";
    }
  }
  else if (is_func (t, WITH) || is_func (t, TWITH) || is_func (t, CWITH) ||
           is_func (t, TFORMAT) || is_func (t, CELL, 1)) {
    if (is_func (t, WITH) && t[0] == "mode" && t[1] != "math");
    else serialize (t[N(t)-1], p * (N(t)-1));
  }
  else if (is_func (t, CELL) ||
           is_func (t, MOVE) || is_func (t, SHIFT) ||
           is_func (t, RESIZE) || is_func (t, CLIPPED))
    serialize (t[0], p * 0);
  else if (is_func (the_drd->get_meaning (L(t)), MACRO) &&
           L(t) >= START_EXTENSIONS) {
    tree fun= the_drd->get_meaning (L(t));
    int i, n= N(fun)-1;
    hashmap<tree,tree> tab (UNINIT);
    for (i=0; i<n; i++) {
      tree var= tree (ARG, fun[i]);
      tree val= "";
      if (i<N(t)) val= tree (QUASI, t[i], (tree) (p * i));
      tab (var)= val;
    }
    tree body= replace (fun[n], tab);
    serialize (body, path (-1));
  }
  else if (is_func (t, QUASI, 2)) {
    tree tt= t[0];
    path pp= as_path (t[1]);
    serialize (tt, pp);
  }
  else {
    string cl= the_drd->get_class (t);
    //if (cl != "") cout << "Class " << t << " -> " << cl << "\n";
    if (cl != "") current_string << "<\\" << cl << ">";
    current_string << "<\\" << as_string (L(t)) << ">";
    for (int i=0; i<N(t); i++) {
      if (i != 0) current_string << "<|>";
      serialize (t[i], p * i);
    }
    current_string << "</>";
    if (cl != "") current_string << "</>";
  }
  if (is_nil (p) || p->item != -1)
    current_end (p)= N(current_string);
}
