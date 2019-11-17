
/******************************************************************************
* MODULE     : parsehtml.cpp
* DESCRIPTION: wrapper for HTML parsing to handle extensions such as MathJax
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "hashset.hpp"
#include "converter.hpp"
#include "parse_string.hpp"

static int mathjax_serial= 1;
static hashmap<int,string> mathjax_strings;
static hashmap<int,tree> mathjax_trees;

/******************************************************************************
* MathJax extension
******************************************************************************/

bool
contains_mathjax (string s) {
  int pos= search_forwards ("<head>", 0, s);
  if (pos < 0) return false;
  pos= search_forwards ("<script", pos, s);
  if (pos < 0) return false;
  pos= search_forwards ("MathJax.js", pos, s);
  if (pos < 0) return false;
  pos= search_forwards ("</head>", pos, s);
  return pos >= 0;
}

bool
get_mathjax (string s, int& i, string close) {
  while (i < N(s)) {
    string expect= "";
    if (read (s, i, "$$")) {
      if (close == "$$") return true;
      else expect= "$$";
    }
    else if (read (s, i, "\\(")) expect= "\\)";
    else if (read (s, i, "\\[")) expect= "\\]";
    else if (read (s, i, "\\begin{equation}")) expect= "\\end{equation}";
    else if (read (s, i, "\\begin{equation*}")) expect= "\\end{equation*}";
    else if (read (s, i, "\\begin{eqnarray}")) expect= "\\end{eqnarray}";
    else if (read (s, i, "\\begin{eqnarray*}")) expect= "\\end{eqnarray*}";
    else if (test (s, i, "\\)") ||
             test (s, i, "\\]") ||
             test (s, i, "\\end{equation}") ||
             test (s, i, "\\end{equation*}") ||
             test (s, i, "\\end{eqnarray}") ||
             test (s, i, "\\end{eqnarray*}")) {
      if (!test (s, i, close)) return false;
      i += N(close);
      return true;
    }
    else if (close == "") return false;
    else i++;
    if (N(expect) != 0) {
      if (!get_mathjax (s, i, expect)) return false;
      if (close == "") return true;
    }
  }
  return false;
}

string
process_mathjax (string s) {
  int i=0;
  string r;
  while (i<N(s)) {
    int pos= i;
    if (s[i] == '\\' || s[i] == '$') {
      if (get_mathjax (s, i, "")) {
        mathjax_strings (mathjax_serial)= s (pos, i);
        r << "<mathjax>" << as_string (mathjax_serial) << "</mathjax>";
        mathjax_serial++;
      }
      else {
        i= pos;
        r << s[i++];
      }
    }
    else r << s[i++];
  }
  return r;
}

tree
retrieve_mathjax (int id) {
  if (!mathjax_strings->contains (id)) return "";
  tree r= mathjax_strings[id];
  mathjax_strings->reset (id);
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
parse_html (string s) {
  if (contains_mathjax (s))
    s= process_mathjax (s);
  return parse_plain_html (s);
}
