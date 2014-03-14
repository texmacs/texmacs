
/******************************************************************************
* MODULE     : convert_tex.h
* DESCRIPTION: declarations shared among the tex conversion units
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONVERT_TEX_H
#define CONVERT_TEX_H
#include "convert.hpp"
#include "rel_hashmap.hpp"
#include "hashfunc.hpp"

extern rel_hashmap<string,string> command_type;
extern rel_hashmap<string,int>    command_arity;
extern rel_hashmap<string,array<string> > command_def;

string paper_opts  (string cmd);
string paper_type  (string cmd);
string latex_type  (string cmd);
int    latex_arity (string cmd);

string latex_get_texmacs_preamble (string s);
string latex_remove_texmacs_preamble (string s);
string latex_set_texmacs_preamble (string s, string p);
string latex_get_style (string s, int& b, int& e);
hashmap<string,path> latex_get_packages (string s);
hashmap<string,path> latex_get_declarations (string s);
hashmap<int,int> latex_declaration_positions (string s);
hashmap<string,path> latex_get_metadata (string s, bool abs_flag);
array<path> latex_get_metadata_snippets (string s, bool abs_flag);
bool latex_unchanged_metadata (string olds, string news, bool abs_flag);

bool skip_curly (string s, int& i);
bool skip_square (string s, int& i);
bool skip_latex_spaces (string s, int& i);

string encode_as_string (path p);
path decode_as_path (string s);
string latex_unmark (string, hashset<path>, hashmap<int,array<path> >&);
tree texmacs_unmark (tree t);
string latex_correspondence (string, hashset<path>, hashmap<path,path>&);
int search_doc_data (tree doc);
int search_abstract_data (tree doc);

#endif // defined CONVERT_TEX_H
