
/******************************************************************************
* MODULE     : glue.hpp
* DESCRIPTION: Glue for linking TeXmacs commands to guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GLUE_HH
#define GLUE_HH
#include "guile.hpp"
#include "tree.hpp"
#include "path.hpp"
#include "url.hpp"
#include "Widget/make_widget.hpp"

void initialize_glue ();

bool scm_is_tree (SCM obj);
bool scm_is_list_string (SCM obj);
bool scm_is_list_tree (SCM obj);
bool scm_is_path (SCM obj);
bool scm_is_url (SCM obj);
bool scm_is_widget (SCM obj);
SCM bool_to_scm (bool b);
SCM int_to_scm (int i);
SCM string_to_scm (string s);
SCM symbol_to_scm (string s);
SCM tree_to_scm (tree t);
SCM list_string_to_scm (list<string> l);
SCM list_tree_to_scm (list<tree> l);
SCM path_to_scm (path p);
SCM url_to_scm (url u);
SCM scheme_tree_to_scm (scheme_tree t);
bool scm_to_bool (SCM obj);
int scm_to_int (SCM obj);
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
