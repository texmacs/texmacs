
/******************************************************************************
* MODULE     : convert.hpp
* DESCRIPTION: various conversion routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONVERT_H
#define CONVERT_H
#include "analyze.hpp"
#include "hashmap.hpp"
typedef tree scheme_tree;
class url;
class object;

/*** Miscellaneous ***/
bool   is_snippet (tree doc);
void   set_file_focus (url u);
url    get_file_focus ();

/*** Generic ***/
string suffix_to_format (string suffix);
string format_to_suffix (string format);
bool   format_exists (string format);
string get_format (string s, string suffix);
tree   generic_to_tree (string s, string format);
string tree_to_generic (tree doc, string format);
array<string> compute_keys (string s, string fm);
array<string> compute_keys (tree t, string fm);
array<string> compute_keys (url u);
scheme_tree compute_index (string s, string fm);
scheme_tree compute_index (tree t, string fm);
scheme_tree compute_index (url u);

/*** Texmacs ***/
tree   texmacs_to_tree (string s);
tree   texmacs_document_to_tree (string s);
string tree_to_texmacs (tree t);
tree   extract (tree doc, string attr);
tree   extract_document (tree doc);
tree   change_doc_attr (tree doc, string attr, tree val);
tree   remove_doc_attr (tree doc, string attr);
hashmap<string,int> get_codes (string version);
tree   string_to_tree (string s, string version);
tree   upgrade (tree t, string version);
tree   substitute (tree t, tree which, tree by);
tree   nonumber_to_eqnumber (tree t);
tree   eqnumber_to_nonumber (tree t);
string search_metadata (tree doc, string kind);

/*** Scheme ***/
string scheme_tree_to_string (scheme_tree t);
string scheme_tree_to_block (scheme_tree t);
scheme_tree tree_to_scheme_tree (tree t);
string tree_to_scheme (tree t);
scheme_tree string_to_scheme_tree (string s);
scheme_tree block_to_scheme_tree  (string s);
tree   scheme_tree_to_tree (scheme_tree t);
tree   scheme_tree_to_tree (scheme_tree t, string version);
tree   scheme_to_tree (string s);
tree   scheme_document_to_tree (string s);

/*** Verbatim ***/
string tree_to_verbatim (tree t, bool wrap= false, string enc= "default");
tree   verbatim_to_tree (string s, bool wrap= false, string enc= "default");
tree   verbatim_document_to_tree (string s, bool w= false, string e= "default");

/*** Latex ***/
tree   parse_latex (string s, bool change= false, bool as_pic= false);
tree   parse_latex_document (string s, bool change= false, bool as_pic= false);
tree   latex_to_tree (tree t);
tree   latex_document_to_tree (string s, bool as_pic= false);
tree   latex_class_document_to_tree (string s);
string latex_verbarg_to_string (tree t);
string get_latex_style (tree t);
string string_arg (tree t, bool u= false);
array<tree> tokenize_concat (tree t, array<tree> a, bool keep= false);
bool   is_verbatim (tree t);
int    latex_search_forwards (string s, int pos, string in);
int    latex_search_forwards (string s, string in);
tree   tracked_latex_to_texmacs (string s, bool as_pic);
string conservative_texmacs_to_latex (tree doc, object opts);
string tracked_texmacs_to_latex (tree doc, object opts);
tree   conservative_latex_to_texmacs (string s, bool as_pic);
int    get_line_number (string s, int pos);
int    get_column_number (string s, int pos);
tree   try_latex_export (tree doc, object opts, url src, url dest);
int    number_latex_errors (url log);
tree   get_latex_errors (url log);
int    number_latex_pages (url log);
tree   postprocess_metadata (tree t);

/*** Xml / Html / Mathml ***/
string old_tm_to_xml_cdata (string s);
object tm_to_xml_cdata (string s);
string old_xml_cdata_to_tm (string s);
string tm_to_xml_name (string s);
string xml_name_to_tm (string s);
string xml_unspace (string s, bool first, bool last);

tree   parse_xml (string s);
tree   parse_plain_html (string s);
tree   parse_html (string s);
tree   clean_html (tree t);
tree   tmml_upgrade (scheme_tree t);
tree   upgrade_mathml (tree t);
tree   retrieve_mathjax (int id);

tree   find_first_element_by_name (tree t, string name);
string get_attr_from_element (tree t, string name, string default_value);
int    parse_xml_length (string length);

/*** BibTeX ***/
tree   parse_bib (string s);
tree   conservative_bib_import (string olds, tree oldt, string news);
string conservative_bib_export (tree oldt, string olds, tree newt);

/*** Post corrections ***/
bool   seems_buggy_html_paste (string s);
string correct_buggy_html_paste (string s);
bool   seems_buggy_paste (string s);
string correct_buggy_paste (string s);
tree   default_with_simplify (tree t);

/*** Coq ***/
tree vernac_to_tree (string s);
tree vernac_document_to_tree (string s);

#endif // defined CONVERT_H
