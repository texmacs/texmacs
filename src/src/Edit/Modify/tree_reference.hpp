
/******************************************************************************
* MODULE     : tree_reference.hpp
* DESCRIPTION: References to trees
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree.hpp"

typedef tree& tree_reference;

void tree_assign (tree r, tree t);
void tree_insert_node (tree r, int pos, tree t);
void tree_remove_node (tree r, int pos);

void tree_reference_assign (tree_reference r, tree t);
void tree_reference_insert (tree_reference r, int pos, tree t);
void tree_reference_remove (tree_reference r, int pos, int nr);
void tree_reference_split (tree_reference r, int pos, int at);
void tree_reference_join (tree_reference r, int pos);
void tree_reference_assign_node (tree_reference r, tree_label op);
void tree_reference_insert_node (tree_reference r, int pos, tree t);
void tree_reference_remove_node (tree_reference r, int pos);
