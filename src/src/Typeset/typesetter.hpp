
/******************************************************************************
* MODULE     : typesetter.hpp
* DESCRIPTION: The result of typesetting a paragraph is
*              an instance of the paragraph class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TYPESETTER_H
#define TYPESETTER_H
#include "boxes.hpp"
#include "env.hpp"
#include "array.hpp"

class typesetter_rep;
typedef typesetter_rep* typesetter;

typesetter new_typesetter (edit_env& env, tree& et, path ip);
void       delete_typesetter (typesetter ttt);

void notify_assign      (typesetter ttt, path p, tree u);
void notify_insert      (typesetter ttt, path p, tree u);
void notify_remove      (typesetter ttt, path p, int nr);
void notify_split       (typesetter ttt, path p);
void notify_join        (typesetter ttt, path p);
void notify_insert_node (typesetter ttt, path p, tree t);
void notify_remove_node (typesetter ttt, path p);
void notify_assign_node (typesetter ttt, path p, tree_label op);
void exec_until         (typesetter ttt, path p);
box  typeset            (typesetter ttt, SI& x1, SI& y1, SI& x2, SI& y2);

box        typeset_as_concat (edit_env env, tree t, path ip);
box        typeset_as_box (edit_env env, tree t, path ip);
box        typeset_as_stack (edit_env env, tree t, path ip);
box        typeset_as_table (edit_env env, tree t, path ip);
array<box> typeset_as_var_table (edit_env env, tree t, path ip);
box        typeset_as_paragraph (edit_env e, tree t, path ip);
box        typeset_as_document (edit_env e, tree t, path ip);
tree       box_info (edit_env env, tree t, string what);

#endif // defined TYPESETTER_H
