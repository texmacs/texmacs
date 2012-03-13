
/******************************************************************************
* MODULE     : new_style.hpp
* DESCRIPTION: Style and DRD computation and caching
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_STYLE_H
#define NEW_STYLE_H
#include "drd_std.hpp"

void new_style_clear_cache ();
void new_style_set_cache (tree style, hashmap<string,tree> H, tree t);
void new_style_get_cache (tree style, hashmap<string,tree>& H, tree& t, bool& f);
bool compute_env_and_drd (tree style);
hashmap<string,tree> get_style_env (tree style);
drd_info get_style_drd (tree style);
tree get_document_preamble (tree t);
drd_info get_document_drd (tree doc);

#endif // NEW_STYLE_H
