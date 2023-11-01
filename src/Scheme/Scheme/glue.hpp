
/******************************************************************************
* MODULE     : glue.hpp
* DESCRIPTION: Glue for linking TeXmacs commands to scheme
* COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GLUE_H
#define GLUE_H

#include "tree.hpp"
#include "path.hpp"
#include "url.hpp"
#include "widget.hpp"
#include "patch.hpp"
//#include "promise.hpp"

#include "object.hpp"

void initialize_glue ();

bool tmscm_is_tree (tmscm obj);
bool tmscm_is_list_string (tmscm obj);
bool tmscm_is_list_tree (tmscm obj);
bool tmscm_is_path (tmscm obj);
bool tmscm_is_url (tmscm obj);
bool tmscm_is_array_double (tmscm obj);
bool tmscm_is_modification (tmscm obj);
bool tmscm_is_patch (tmscm obj);
bool tmscm_is_widget (tmscm obj);

tmscm  bool_to_tmscm (bool b);
tmscm  int_to_tmscm (int i);
tmscm  double_to_tmscm (double i);
tmscm  string_to_tmscm (string s);
tmscm  symbol_to_tmscm (string s);
tmscm  tree_to_tmscm (tree t);
tmscm  list_string_to_tmscm (list<string> l);
tmscm  list_tree_to_tmscm (list<tree> l);
tmscm  path_to_tmscm (path p);
tmscm  url_to_tmscm (url u);
tmscm  array_double_to_tmscm (array<double> a);
tmscm  modification_to_tmscm (modification m);
tmscm  patch_to_tmscm (patch p);
tmscm  scheme_tree_to_tmscm (scheme_tree t);

//int tmscm_to_bool (tmscm obj);
int tmscm_to_int (tmscm obj);
double tmscm_to_double (tmscm  i);
string tmscm_to_string (tmscm obj);
string tmscm_to_symbol (tmscm obj);
tree tmscm_to_tree (tmscm obj);
tree tmscm_to_content (tmscm obj);
list<string> tmscm_to_list_string (tmscm obj);
list<tree> tmscm_to_list_tree (tmscm obj);
path tmscm_to_path (tmscm obj);
url tmscm_to_url (tmscm obj);
array<double> tmscm_to_array_double (tmscm obj);
modification tmscm_to_modification (tmscm obj);
patch tmscm_to_patch (tmscm obj);
scheme_tree tmscm_to_scheme_tree (tmscm obj);
widget tmscm_to_widget (tmscm widget_smob);
command tmscm_to_command (tmscm obj);

#endif // defined GLUE_H
