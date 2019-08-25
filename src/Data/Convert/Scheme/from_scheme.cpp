
/******************************************************************************
* MODULE     : to_scheme.cpp
* DESCRIPTION: conversion of scheme expressions to TeXmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"
#include "path.hpp"

/******************************************************************************
* Handling escape characters
******************************************************************************/

string
unslash (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if ((s[i]=='\\') && ((i+1)<n))
      switch (s[++i]) {
      case '0': r << ((char) 0); break;
      case 'n': r << '\n'; break;
      case 't': r << '\t'; break;
      default: r << s[i];
      }
    else r << s[i];
  return r;
}

/******************************************************************************
* Converting strings to scheme trees
******************************************************************************/

static bool
is_spc (char c) {
  return (c==' ') || (c=='\t') || (c=='\n');
}

static scheme_tree
string_to_scheme_tree (string s, int& i) {
  for (; i<N(s); i++)
    switch (s[i]) {

    case ' ':
    case '\t':
    case '\n':
      break;
      case '(':
      {
        scheme_tree p (TUPLE);
        i++;
        while (true) {
          while ((i<N(s)) && is_spc(s[i])) i++;
          if ((i==N(s)) || (s[i]==')')) break;
          p << string_to_scheme_tree (s, i);
        }
        if (i<N(s)) i++;
        return p;
      }
        
      case '\'':
        i++;
        return scheme_tree (TUPLE, "\'", string_to_scheme_tree (s, i));
        
      case '\"':
      { // "
        int start= i++;
        while ((i<N(s)) && (s[i]!='\"')) { // "
          if ((i<N(s)-1) && (s[i]=='\\')) i++;
          i++;
        }
        if (i<N(s)) i++;
        return scheme_tree (unslash (s (start, i)));
      }
        
      case ';':
        while ((i<N(s)) && (s[i]!='\n')) i++;
        break;
        
      default:
      {
        int start= i;
        while ((i<N(s)) && (!is_spc(s[i])) && (s[i]!='(') && (s[i]!=')')) {
          if ((i<N(s)-1) && (s[i]=='\\')) i++;
          i++;
        }
        return scheme_tree (unslash (s (start, i)));
      }
    }
  
  return "";
}

scheme_tree
string_to_scheme_tree (string s) {
  s= replace (s, "\015", "");
  int i=0;
  return string_to_scheme_tree (s, i);
}

scheme_tree
block_to_scheme_tree (string s) {
  scheme_tree p (TUPLE);
  int i=0;
  while ((i<N(s)) && (is_spc (s[i]) || s[i]==')')) i++;
  while (i<N(s)) {
    p << string_to_scheme_tree (s, i);
    while ((i<N(s)) && (is_spc (s[i]) || s[i]==')')) i++;
  }
  return p;
}

/******************************************************************************
* Converting scheme trees to trees
******************************************************************************/

tree
scheme_tree_to_tree (scheme_tree t, hashmap<string,int> codes, bool flag) {
  if (is_atomic (t)) return scm_unquote (t->label);
  else if ((N(t) == 0) || is_compound (t[0])) {
    convert_error << "Invalid scheme tree " << t << "\n";
    return
      compound ("errput", 
                concat ("The tree was ", as_string (L(t)), ": ", tree (t)));
  }
  else {
    int i, n= N(t);
    tree_label code= (tree_label) codes [t[0]->label];
    if (flag) code= make_tree_label (t[0]->label);
    if (code == UNKNOWN) {
      tree u (EXPAND, n);
      u[0]= copy (t[0]);
      for (i=1; i<n; i++)
        u[i]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
    else {
      tree u (code, n-1);
      for (i=1; i<n; i++)
        u[i-1]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
  }
}

tree
scheme_tree_to_tree (scheme_tree t, string version) {
  version= scm_unquote (version);
  tree doc, error (ERROR, "bad format or data");
  if (version_inf (version, "1.0.2.4"))
    doc= scheme_tree_to_tree (t, get_codes (version), false);
  else doc= scheme_tree_to_tree (t);
  if (!is_document (doc)) return error;
  return upgrade (doc, version);
}

tree
scheme_tree_to_tree (scheme_tree t) {
  return scheme_tree_to_tree (t, STD_CODE, true);
}

/******************************************************************************
* Converting scheme strings to trees
******************************************************************************/

tree
scheme_to_tree (string s) {
  return scheme_tree_to_tree (string_to_scheme_tree (s));
}

tree
scheme_document_to_tree (string s) {
  tree error (ERROR, "bad format or data");
  if (starts (s, "(document (apply \"TeXmacs\" ") ||
      starts (s, "(document (expand \"TeXmacs\" ") ||
      starts (s, "(document (TeXmacs "))
  {
    int i, begin=27;
    if (starts (s, "(document (expand \"TeXmacs\" ")) begin= 28;
    if (starts (s, "(document (TeXmacs ")) begin= 19;
    for (i=begin; i<N(s); i++)
      if (s[i] == ')') break;
    string version= s (begin, i);
    tree t  = string_to_scheme_tree (s);
    return scheme_tree_to_tree (t, version);
  }
  return error;
}
