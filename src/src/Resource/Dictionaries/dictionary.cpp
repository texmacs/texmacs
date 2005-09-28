
/******************************************************************************
* MODULE     : dictionary.cpp
* DESCRIPTION: used for translations and analyzing text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "dictionary.hpp"
#include "file.hpp"
#include "convert.hpp"

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
  if (load_string (u, s)) return;
  tree t= block_to_scheme_tree (s);
  if (!is_tuple (t)) return;

  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2) &&
	is_atomic (t[i][0]) && is_atomic (t[i][1]))
      {
	string l= t[i][0]->label; if (is_quoted (l)) l= unquote (l);
	string r= t[i][1]->label; if (is_quoted (r)) r= unquote (r);
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
  dictionary dict= new dictionary_rep (from, to);
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
  for (i=0; i<n; i++)
    if (is_iso_alpha (s[i]) || (s[i]=='|') || (s[i]==' ') ||
	((i>0) && (s[i]=='#'))) break;
  if (i==n) {
    if (s[0]=='#') return " " * translate (s (1,n));
    if (s[n-1]=='#') return translate (s (0,n-1)) * " ";
    if ((to == "french") &&
	((s[n-1] == ':') || (s[n-1] == '!') || (s[n-1] == '?')) &&
	((n==1) || (s[n-2] != ' '))) return s (0, n-1) * " " * s (n-1, n);
    return s;
  }
  if (i>0) return translate (s (0, i)) * translate (s (i, n));
  for (i=0; i<n; i++)
    if ((!is_iso_alpha (s[i])) && (s[i]!='|') && (s[i]!=' ')) break;
  if (i<n) return translate (s (0, i)) * translate (s (i, n));
  for (i=0; i<n; i++) if (s[i]!=' ') break;
  if (i==n) return s;
  if (i>0) return s (0, i) * translate (s (i, n));
  for (i=n-1; i>=0; i--) if (s[i]!=' ') break;
  if (i<n-1) return translate (s (0, i+1)) * s (i+1, n);

  for (i=n-1; i>=0; i--)
    if (s[i]=='|') break;
  string radical= s (0, i<0? i+1: i);
  string word   = s (i+1, n);

  if (N(word)==0) return "";
  bool flag= is_upcase (word[0]);
  string source= locase_first (word);
  if (N(radical)>0) source= locase_all (radical) * "|" * source;

  if (!table->contains (source)) {
    if (N(radical)>0) {
      for (i=0; i<n; i++)
	if (s[i]=='|') break;
      return translate (s (i+1, n));
    }
    return word;
  }

  string dest= table [source];
  if (flag) dest= upcase_first (dest);
  return dest;
}
