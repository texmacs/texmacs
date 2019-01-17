
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

bool is_macro (tree_label l);
bool is_parameter (tree_label l);
string get_tag_type (tree_label l);
int minimal_arity (tree_label l);
int maximal_arity (tree_label l);
bool correct_arity (tree_label l, int n);
int minimal_arity (tree t);
int maximal_arity (tree t);
bool correct_arity (tree t, int n);
int insert_point (tree t, int i);
bool is_dynamic (tree t);
bool is_accessible_child (tree t, int i);
array<tree> accessible_children (tree t);
bool all_accessible (tree t);
bool none_accessible (tree t);
bool exists_accessible_inside (tree t);
tree get_env_child (tree t, int i, tree env);
tree get_env_descendant (tree t, path p, tree env);
tree get_env_descendant (tree t, path p, string var, tree val);

string get_name (tree t);
string get_long_name (tree t);
string get_child_name (tree t, int i);
string get_child_long_name (tree t, int i);
string get_child_type (tree t, int i);
tree   get_env_child (tree t, int i, string var, tree val);

path next_valid (tree t, path p);
path previous_valid (tree t, path p);
path next_accessible (tree t, path p);
path previous_accessible (tree t, path p);
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
bool inside_same_or_more (tree t, path p, path q, tree_label which);

array<tree> search_sections (tree t);
path previous_section (tree t, path p);

#endif // defined TREE_TRAVERSE_H
