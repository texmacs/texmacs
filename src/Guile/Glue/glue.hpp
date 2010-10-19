
/******************************************************************************
* MODULE     : glue.hpp
* DESCRIPTION: Glue for linking TeXmacs commands to guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GLUE_HH
#define GLUE_HH

//FIXME: if tree.hpp and guile.hpp includes are not in this order
//       we have compilation problems on mingw32 
//       (probably name clashes with Windows headers)
#include "tree.hpp"
#include "guile.hpp"
#include "path.hpp"
#include "url.hpp"
#include "widget.hpp"
#include "promise.hpp"

void initialize_glue ();

bool scm_is_tree (SCM obj);
bool scm_is_list_string (SCM obj);
bool scm_is_list_tree (SCM obj);
bool scm_is_path (SCM obj);
bool scm_is_url (SCM obj);
bool scm_is_widget (SCM obj);

SCM bool_to_scm (bool b);
SCM int_to_scm (int i);
SCM double_to_scm (double i);
SCM string_to_scm (string s);
SCM symbol_to_scm (string s);
SCM tree_to_scm (tree t);
SCM list_string_to_scm (list<string> l);
SCM list_tree_to_scm (list<tree> l);
SCM path_to_scm (path p);
SCM url_to_scm (url u);
SCM scheme_tree_to_scm (scheme_tree t);

#ifndef GUILE_C
int scm_to_bool (SCM obj);
int scm_to_int (SCM obj);
double scm_to_double (SCM i);
#endif
string scm_to_string (SCM obj);
string scm_to_symbol (SCM obj);
tree scm_to_tree (SCM obj);
tree scm_to_content (SCM obj);
list<string> scm_to_list_string (SCM obj);
list<tree> scm_to_list_tree (SCM obj);
path scm_to_path (SCM obj);
url scm_to_url (SCM obj);
scheme_tree scm_to_scheme_tree (SCM obj);
widget scm_to_widget (SCM widget_smob);

#endif // defined GLUE_HH
