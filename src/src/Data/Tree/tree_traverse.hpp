
/******************************************************************************
* MODULE     : tree_traverse.hpp
* DESCRIPTION: abstract cursor movement and tree traversal
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TREE_TRAVERSE_H
#define TREE_TRAVERSE_H
#include "tree_cursor.hpp"

path next_valid (tree t, path p);
path previous_valid (tree t, path p);
path next_word (tree t, path p);
path previous_word (tree t, path p);
path next_node (tree t, path p);
path previous_node (tree t, path p);
path next_tag (tree t, path p, scheme_tree labs);
path previous_tag (tree t, path p, scheme_tree labs);
path next_argument (tree t, path p);
path previous_argument (tree t, path p);

bool inside_same (tree t, path p, path q, tree_label which);
bool more_inside (tree t, path p, path q, tree_label which);

#endif // defined TREE_TRAVERSE_H
