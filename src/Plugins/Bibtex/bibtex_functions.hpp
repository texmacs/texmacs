
/******************************************************************************
* MODULE     : bibtex_functions.hpp
* DESCRIPTION: BiBTeX internal functions
* COPYRIGHT  : (C) 2010 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "convert.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

//string bib_change_case (string s, string op);
//string bib_field_raw (scheme_tree t, string field);
string bib_preamble (tree t);

string bib_purify (scheme_tree st);
string bib_prefix (scheme_tree st, int i);
scheme_tree bib_locase (scheme_tree st);
scheme_tree bib_upcase (scheme_tree st);
scheme_tree bib_default (scheme_tree st);
int    bib_text_length (scheme_tree st);
bool   bib_empty (scheme_tree st, string f);
scheme_tree bib_field (scheme_tree st, string field);
list<string> bib_field_pages (scheme_tree st);
void   bib_parse_fields (tree& t);
scheme_tree bib_add_period (scheme_tree st);
scheme_tree bib_upcase_first (scheme_tree st);

hashmap<string,string> bib_strings_dict (tree t);
tree   bib_subst_vars (tree t, hashmap<string,string> dict);
tree   bib_entries (tree t, tree bib_t);
scheme_tree bib_abbreviate (scheme_tree st, scheme_tree s1, scheme_tree s2);

