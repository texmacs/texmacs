
/******************************************************************************
* MODULE     : tree_traverse.hpp
* DESCRIPTION: abstract cursor movement and tree traversal
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_TRAVERSE_H
#define TREE_TRAVERSE_H
#include "tree_cursor.hpp"

int minimal_arity (tree t);
int maximal_arity (tree t);
bool correct_arity (tree t, int n);
int insert_point (tree t, int i);
bool is_dynamic (tree t);
bool is_accessible_child (tree t, int i);
array<tree> accessible_children (tree t);
bool all_accessible (tree t);
bool none_accessible (tree t);

string get_name (tree t);
string get_long_name (tree t);
string get_child_name (tree t, int i);
string get_child_long_name (tree t, int i);
string get_child_type (tree t, int i);

path next_valid (tree t, path p);
path previous_valid (tree t, path p);
path next_word (tree t, path p);
path previous_word (tree t, path p);
path next_node (tree t, path p);
path previous_node (tree t, path p);
path next_tag (tree t, path p, scheme_tree labs);
path previous_tag (tree t, path p, scheme_tree labs);
path next_tag_same_argument (tree t, path p, scheme_tree labs);
path previous_tag_same_argument (tree t, path p, scheme_tree labs);
path next_argument (tree t, path p);
path previous_argument (tree t, path p);

bool inside_same (tree t, path p, path q, tree_label which);
bool more_inside (tree t, path p, path q, tree_label which);

array<tree> search_sections (tree t);
path previous_section (tree t, path p);

#endif // defined TREE_TRAVERSE_H
