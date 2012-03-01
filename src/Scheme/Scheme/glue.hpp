
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
//#include "promise.hpp"

#include "object.hpp"



void initialize_glue ();

bool scm_is_tree (scm obj);
bool scm_is_list_string (scm obj);
bool scm_is_list_tree (scm obj);
bool scm_is_path (scm obj);
bool scm_is_url (scm obj);
bool scm_is_widget (scm obj);

scm bool_to_scm (bool b);
scm int_to_scm (int i);
scm double_to_scm (double i);
scm string_to_scm (string s);
scm symbol_to_scm (string s);
scm tree_to_scm (tree t);
scm list_string_to_scm (list<string> l);
scm list_tree_to_scm (list<tree> l);
scm path_to_scm (path p);
scm url_to_scm (url u);
scm scheme_tree_to_scm (scheme_tree t);

//int scm_to_bool (scm obj);
int scm_to_int (scm obj);
double scm_to_double (scm i);
string scm_to_string (scm obj);
string scm_to_symbol (scm obj);
tree scm_to_tree (scm obj);
tree scm_to_content (scm obj);
list<string> scm_to_list_string (scm obj);
list<tree> scm_to_list_tree (scm obj);
path scm_to_path (scm obj);
url scm_to_url (scm obj);
scheme_tree scm_to_scheme_tree (scm obj);
widget scm_to_widget (scm widget_smob);

#endif // defined GLUE_H

