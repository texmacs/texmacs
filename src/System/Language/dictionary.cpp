
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
#include "universal.hpp"
#include "drd_std.hpp"
#include "scheme.hpp"

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
      if (to == "chinese" ||  to == "japanese"  ||
          to == "korean"  ||  to == "taiwanese" ||
          to == "russian" ||  to == "ukrainian" || to == "bulgarian" ||
          to == "german" || to == "greek" || to == "slovak")
        r= utf8_to_cork (r);
      table (l)= r;
    }
}

void
dictionary_rep::load (string fname) {
  fname= fname * ".scm";
  if (DEBUG_CONVERT) debug_convert << "Loading " << fname << "\n";
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
dictionary_rep::translate (string s, bool guess) {
  if (s == "") return s;
  if (from == to) {
    int pos= search_forwards ("::", s);
    if (pos >= 0) return s (0, pos);
    return s;
  }
  
  //cout << "Translate <" << s << ">\n";
  // Is s in dictionary?
  if (table->contains (s) && table[s] != "")
    return table[s];
  
  // Is lowercase version of s in dictionary?
  string ls= locase_first (s);
  if (table->contains (ls) && table[ls] != "")
    return uni_upcase_first (table[ls]);

  // Search for :: disambiguation patterns
  int pos= search_forwards ("::", s);
  if (pos >= 0) return translate (s (0, pos), guess);
  
  // Attempt to split the string and translate its parts?
  if (!guess) return s;
  
  // Remove trailing non iso_alpha characters
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (is_iso_alpha (s[i]))
      break;
  int start= i;
  for (i=n; i>0; i--)
    if (is_iso_alpha (s[i-1]))
      break;
  int end= i;
  if (start >= n || end <= 0) return s;
  if (start != 0 || end != n) {
    ASSERT (start < end, "invalid situation");
    string s1= translate (s (0, start));
    string s2= translate (s (start, end));
    string s3= translate (s (end, n));
    if (to == "french") {
      if (s3 == ":") s3= " :";
      if (s3 == "!") s3= " !";
      if (s3 == "?") s3= " ?";
    }
    return s1 * s2 * s3;
  }

  // Break at last non iso_alpha character which is not a space
  for (i=n; i>0; i--)
    if (!is_iso_alpha (s[i-1]) && s[i-1] != ' ')
      break;
  if (i > 0) {
    string s1= translate (s (0, i));
    string s2= translate (s (i, n));
    return s1 * s2;
  }

  // No translation available
  return s;
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

void
force_load_dictionary (string from, string to) {
  string name= from * "-" * to;
  if (dictionary::instances -> contains (name))
    dictionary::instances -> reset (name);
  load_dictionary (from, to);
  notify_preference ("language");
}

string
translate_as_is (string s, string from, string to) {
  dictionary dict= load_dictionary (from, to);
  return dict->translate (s, false);
}

string
translate_as_is (string s) {
  return translate_as_is (s, "english", out_lan);
}

/******************************************************************************
* Translation of trees
******************************************************************************/

tree
translate_replace (tree t, string from, string to, int n=1)
{
  if (N(t) < 2) return t[0];
  
  string s= t[0]->label;
  string arg= "%" * as_string (n);
  
  if (is_atomic (t[1])) {
    s= replace (s, arg, translate (t[1]->label, from, to));
    return translate_replace (concat (s) * t(2, N(t)), from, to, n+1);
  }
  else {
    int l= search_forwards (arg, s);
    if (l < 0) return t;
    int r= l + N(arg);
    tree r1= tree_translate (t[1], from, to);
    tree r2= translate_replace (tuple (s (r, N(s))) * t(2, N(t)), from, to, n+1);
    s= s(0, l);
    if (is_atomic (r1)) {
      if (is_atomic (r2)) return s * r1->label * r2->label;
      else                return concat (s * r1->label, r2);
    }
    return concat (s, r1, r2);
  }
}

tree
tree_translate (tree t, string from, string to) {
  //cout << "Translating " << t << " from " << from << " into " << to << "\n";
  if (is_atomic (t))
    return translate (t->label, from, to);
  else if (is_compound (t, "replace")) {
    if (!is_atomic (t[0])) {
        //cout << "tree_translate() ERROR: first child should be a string\n";
      return t;
    }
    t[0]->label= translate_as_is (t[0]->label, from, to);
    return translate_replace (t, from, to);
  }
  else if (is_compound (t, "verbatim", 1))
    return t[0];
  else if (is_compound (t, "localize", 1))
    return tree_translate (t[0], "english", out_lan);
  else if (is_compound (t, "render-key", 1))
    return compound ("render-key", tree_translate (t[0], from, to));
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
