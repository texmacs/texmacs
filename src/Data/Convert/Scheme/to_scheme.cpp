
/******************************************************************************
* MODULE     : to_scheme.cpp
* DESCRIPTION: conversion of TeXmacs trees to scheme expressions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "convert.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Converting scheme trees to strings
******************************************************************************/

static string
slash (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '(':
    case ')':
    case ' ':
    case '\'':
      if ((n<2) || (s[0]!='\042') || (s[n-1]!='\042')) r << "\\";
      r << s[i];
      break;
    case '\\':
      r << "\\" << s[i];
      break;
    case '\042':
      if (((i==0) && (s[n-1]=='\042')) ||
	  ((i==(n-1)) && (s[0]=='\042')))
	r << s[i];
      else r << "\\" << s[i];
      break;
    case ((char) 0):
      r << "\\0";
      break;
    case '\t':
      r << "\\t";
      break;
    case '\n':
      r << "\\n";
      break;
    default:
      r << s[i];
    }
  return r;
}

static void
scheme_tree_to_string (string& out, scheme_tree p) {
  if (!is_tuple (p)) out << slash (p->label);
  else {
    if (is_tuple (p, "\'", 1)) {
      out << "\'";
      scheme_tree_to_string (out, p[1]);
    }
    else {
      int i, n= N(p);
      out << "(";
      for (i=0; i<n; i++) {
	if (i>0) out << " ";
	scheme_tree_to_string (out, p[i]);
      }
      out << ")";
    }
  }
}

string
scheme_tree_to_string (scheme_tree p) {
  string out;
  scheme_tree_to_string (out, p);
  return out;
}

string
scheme_tree_to_block (scheme_tree p) {
  string out;
  int i, n= N(p);
  for (i=0; i<n; i++)
    out << scheme_tree_to_string (p[i]) << "\n";
  return out;
}

/******************************************************************************
* Conversion from trees to scheme trees
******************************************************************************/

scheme_tree
tree_to_scheme_tree (tree t) {
  if (is_atomic (t)) return "\"" * t->label * "\"";
  else if (is_func (t, EXPAND) && is_atomic (t[0])
#ifndef WITH_EXTENSIONS
	   && (!std_contains (t[0]->label))
#endif
	   ) {
    int i, n= N(t);
    tree u (TUPLE, n);
    u[0]= copy (t[0]);
    for (i=1; i<n; i++)
      u[i]= tree_to_scheme_tree (t[i]);
    return u;    
  }
  else {
    int i, n= N(t);
    tree u (TUPLE, n+1);
    u[0]= copy (as_string (L(t)));
    for (i=0; i<n; i++)
      u[i+1]= tree_to_scheme_tree (t[i]);
    return u;
  }
}

/******************************************************************************
* Conversion of trees to scheme strings
******************************************************************************/

string
tree_to_scheme (tree t) {
  return scheme_tree_to_string (tree_to_scheme_tree (t));
}
