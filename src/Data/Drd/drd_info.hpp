
/******************************************************************************
* MODULE     : drd_info.hpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DRD_INFO_H
#define DRD_INFO_H
#include "tree.hpp"
#include "rel_hashmap.hpp"
#include "tag_info.hpp"

class drd_info;
class drd_info_rep: concrete_struct {
public:
  string name;
  rel_hashmap<tree_label,tag_info> info;
  hashmap<string,tree> env;

public:
  drd_info_rep (string name);
  drd_info_rep (string name, drd_info base);
  tree get_locals ();
  bool set_locals (tree t);
  bool contains (string l);

  /* Properties of the tag itself */
  void set_type (tree_label tag, int tp);
  int  get_type (tree_label tag);
  void freeze_type (tree_label tag);
  int  get_type (tree t);

  void set_arity (tree_label tag, int arity, int extra, int am, int cm);
  int  get_arity_mode (tree_label tag);
  int  get_arity_base (tree_label tag);
  int  get_arity_extra (tree_label tag);
  int  get_child_mode (tree_label tag);
  int  get_nr_indices (tree_label tag);
  void freeze_arity (tree_label tag);
  int  get_old_arity (tree_label l);
  int  get_minimal_arity (tree_label l);
  int  get_maximal_arity (tree_label l);
  bool correct_arity (tree_label l, int i);
  bool insert_point (tree_label l, int i, int n);
  bool is_dynamic (tree t, bool hack= true);

  void set_border (tree_label tag, int mode);
  int  get_border (tree_label tag);
  void freeze_border (tree_label tag);
  bool is_child_enforcing (tree t);
  bool is_parent_enforcing (tree t);
  bool var_without_border (tree_label tag);

  void set_block (tree_label tag, int is_block);
  int  get_block (tree_label tag);
  void freeze_block (tree_label tag);

  void set_with_like (tree_label tag, bool is_with_like);
  bool get_with_like (tree_label tag);
  void freeze_with_like (tree_label tag);
  bool is_with_like (tree t);

  void set_var_type (tree_label tag, int vt);
  int  get_var_type (tree_label tag);
  void freeze_var_type (tree_label tag);

  void set_attribute (tree_label tag, string which, tree val);
  tree get_attribute (tree_label tag, string which);
  void set_name (tree_label tag, string val);
  void set_long_name (tree_label tag, string val);
  void set_syntax (tree_label tag, tree val);
  string get_name (tree_label tag);
  string get_long_name (tree_label tag);
  tree   get_syntax (tree_label tag);
  tree   get_syntax (tree t, path p= path (-1));

  /* Properties of the children of the tag */
  void set_type (tree_label tag, int nr, int tp);
  int  get_type (tree_label tag, int nr);
  void freeze_type (tree_label tag, int nr);
  int  get_type_child (tree t, int child);

  void set_accessible (tree_label tag, int nr, int access_mode);
  int  get_accessible (tree_label tag, int nr);
  void freeze_accessible (tree_label tag, int nr);
  bool all_accessible (tree_label tag);
  bool none_accessible (tree_label tag);
  bool is_accessible_child (tree t, int child);
  bool is_accessible_path (tree t, path p);

  void set_writability (tree_label tag, int nr, int writability);
  int  get_writability (tree_label tag, int nr);
  void freeze_writability (tree_label tag, int nr);
  int  get_writability_child (tree t, int child);

  void set_block (tree_label tag, int nr, int require_block);
  int  get_block (tree_label tag, int nr);
  void freeze_block (tree_label tag, int nr);

  void set_env (tree_label tag, int nr, tree env);
  tree get_env (tree_label tag, int nr);
  void freeze_env (tree_label tag, int nr);
  tree get_env_child (tree t, int child, tree env);
  tree get_env_child (tree t, int child, string var, tree val);
  tree get_env_descendant (tree t, path p, tree env);
  tree get_env_descendant (tree t, path p, string var, tree val);

  void set_child_name (tree_label tag, int nr, string val);
  void set_child_long_name (tree_label tag, int nr, string val);
  string get_child_name (tree_label tag, int nr);
  string get_child_long_name (tree_label tag, int nr);
  string get_child_name (tree t, int child);
  string get_child_long_name (tree t, int child);

  /* Heuristic initialization */
  void set_environment (hashmap<string,tree> env);
  tree arg_access (tree t, tree arg, tree env, int& type, bool& found);
  bool heuristic_with_like (tree t, tree arg);
  bool heuristic_init_macro (string var, tree macro);
  bool heuristic_init_xmacro (string var, tree xmacro);
  bool heuristic_init_parameter (string var, string val);
  bool heuristic_init_parameter (string var, tree val);
  void heuristic_init (hashmap<string,tree> env);

  friend class drd_info;
  friend tm_ostream& operator << (tm_ostream& out, drd_info drd);
};

class drd_info {
  CONCRETE(drd_info);
  drd_info (string name);
  drd_info (string name, drd_info base);
  operator tree ();
};
CONCRETE_CODE(drd_info);

tree drd_env_write (tree env, string var, tree val);
tree drd_env_merge (tree env, tree t);
tree drd_env_read (tree env, string var, tree val= tree (UNINIT));

#endif // defined DRD_INFO_H
