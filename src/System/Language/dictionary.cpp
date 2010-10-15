
/******************************************************************************
* MODULE     : dictionary.cpp
* DESCRIPTION: used for translations and analyzing text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "dictionary.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "drd_std.hpp"

RESOURCE_CODE(dictionary);

/******************************************************************************
* Dictionary initialization
******************************************************************************/

dictionary_rep::dictionary_rep (string from2, string to2):
  rep<dictionary> (from2 * "-" * to2), table ("?"), from (from2), to (to2) {}

void
dictionary_rep::load (url u) {
  if (is_none (u)) return;
  if (is_or (u)) {
    load (u[1]);
    load (u[2]);
    return;
  }

  string s;
  if (load_string (u, s, false)) return;
  tree t= block_to_scheme_tree (s);
  if (!is_tuple (t)) return;

  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2) &&
	is_atomic (t[i][0]) && is_atomic (t[i][1]))
      {
	string l= t[i][0]->label; if (is_quoted (l)) l= scm_unquote (l);
	string r= t[i][1]->label; if (is_quoted (r)) r= scm_unquote (r);
	if (to == "chinese" || to == "japanese" ||
	    to == "korean" || to == "taiwanese")
	  r= utf8_to_cork (r);
	table (l)= r;
      }
}

void
dictionary_rep::load (string fname) {
  fname= fname * ".scm";
  if (DEBUG_VERBOSE) cout << "TeXmacs] Loading " << fname << "\n";
  url u= url ("$TEXMACS_DIC_PATH") * url_wildcard ("*" * fname);
  load (expand (complete (u)));
}

dictionary
load_dictionary (string from, string to) {
  string name= from * "-" * to;
  if (dictionary::instances -> contains (name))
    return dictionary (name);
  dictionary dict= tm_new<dictionary_rep> (from, to);
  if (from != to) dict->load (name);
  return dict;
}

/******************************************************************************
* Translation routines
******************************************************************************/

string
dictionary_rep::translate (string s) {
  int i, n=N(s);
  if (n==0) return s;
  for (i=0; i<n; tm_char_forwards (s, i))
    if (is_iso_alpha (s[i]) || (s[i]==' ') || ((i>0) && (s[i]=='#'))) break;
  if (i==n) {
    if (s[0]=='#') return " " * translate (s (1,n));
    if (s[n-1]=='#') return translate (s (0,n-1)) * " ";
    if ((to == "french") &&
	((s[n-1] == ':') || (s[n-1] == '!') || (s[n-1] == '?')) &&
	((n==1) || (s[n-2] != ' '))) return s (0, n-1) * " " * s (n-1, n);
    return s;
  }
  if (i>0) return translate (s (0, i)) * translate (s (i, n));
  for (i=0; i<n; tm_char_forwards (s, i))
    if ((!is_iso_alpha (s[i])) && (s[i]!=' ')) break;
  if (i<n) return translate (s (0, i)) * translate (s (i, n));
  for (i=0; i<n && s[i]==' '; i++) {}
  if (i==n) return s;
  if (i>0) return s (0, i) * translate (s (i, n));
  for (i=n; i>0 && s[i-1]==' '; i--) {}
  if (i<n) return translate (s (0, i)) * s (i, n);

  bool flag= is_upcase (s[0]);
  string source= locase_first (s);
  if (!table->contains (source)) return s;
  string dest= table [source];
  if (flag) dest= upcase_first (dest);
  if (N(dest)==0) return s;
  return dest;
}

/******************************************************************************
* Interface
******************************************************************************/

static string in_lan ("english");
static string out_lan ("english");

void set_input_language (string s) { in_lan= s; }
string get_input_language () { return in_lan; }
void set_output_language (string s) { out_lan= s; }
string get_output_language () { return out_lan; }

string
translate (string s, string from, string to) {
  if (N(from)==0) return s;
  dictionary dict= load_dictionary (from, to);
  return dict->translate (s);
}

string
translate (string s) {
  return translate (s, "english", out_lan);
}

string
translate (const char* s) {
  return translate (string (s), "english", out_lan);
}

/******************************************************************************
* Translation of trees
******************************************************************************/

tree
tree_translate (tree t, string from, string to) {
  if (is_atomic (t))
    return translate (t->label, from, to);
  else if (is_compound (t, "verbatim", 1))
    return t[0];
  else {
    tree r (t, N(t));
    for (int i=0; i<N(t); i++)
      if (!the_drd->is_accessible_child (t, i)) r[i]= t[i];
      else r[i]= tree_translate (t[i], from, to);
    return r;
  }
}

tree
tree_translate (tree t) {
  return tree_translate (t, "english", out_lan);
}

/******************************************************************************
* Translate and serialize
******************************************************************************/

static string
serialize (tree t) {
  if (is_atomic (t))
    return t->label;
  else if (is_concat (t)) {
    string s;
    for (int i=0; i<N(t); i++) {
      tree u= t[i];
      while (is_concat (u) && N(u) > 0) u= u[0];
      if (i > 0 && is_compound (u, "render-key"))
	if (!is_atomic (t[i-1]) || !ends (t[i-1]->label, " ")) {
	  if (use_macos_fonts () || gui_is_qt ()) s << "  ";
	  else s << " ";
	}
      s << serialize (t[i]);
    }
    return s;
  }
  else if (is_compound (t, "render-key", 1))
    return serialize (t[0]);
  else if (is_func (t, WITH))
    return serialize (t[N(t)-1]);
  else if (is_compound (t, "math", 1))
    return serialize (t[0]);
  else if (is_compound (t, "op", 1)) {
    t= t[0];
    if (gui_is_qt ()) {
      if (t == "<leftarrow>") return "Left";
      if (t == "<rightarrow>") return "Right";
      if (t == "<uparrow>") return "Up";
      if (t == "<downarrow>") return "Down";
    }
    else {
      if (t == "<leftarrow>") return "left";
      if (t == "<rightarrow>") return "right";
      if (t == "<uparrow>") return "up";
      if (t == "<downarrow>") return "down";
    }
    return serialize (t);
  }
  else return "";
}

string
translate (tree t, string from, string to) {
  return serialize (tree_translate (t, from, to));
}

string
translate (tree t) {
  return serialize (tree_translate (t));
}
