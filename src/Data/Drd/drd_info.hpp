
/******************************************************************************
* MODULE     : drd_info.hpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

public:
  drd_info_rep (string name);
  drd_info_rep (string name, drd_info base);
  tree get_locals ();
  void set_locals (tree t);
  bool contains (string l);

  /* Properties of the tag itself */
  void set_arity (tree_label tag, int arity, int extra, int am, int cm);
  int  get_arity_mode (tree_label tag);
  int  get_arity_base (tree_label tag);
  int  get_arity_extra (tree_label tag);
  int  get_child_mode (tree_label tag);
  int  get_nr_indices (tree_label tag);
  void freeze_arity (tree_label tag);
  int  get_old_arity (tree_label l);
  bool is_dynamic (tree t);

  void set_no_border (tree_label tag, bool has_no_border);
  bool get_no_border (tree_label tag);
  void freeze_no_border (tree_label tag);
  bool is_child_enforcing (tree t);

  void set_block (tree_label tag, int is_block);
  int  get_block (tree_label tag);
  void freeze_block (tree_label tag);

  /* Properties of the children of the tag */
  void set_accessible (tree_label tag, int nr, bool is_accessible);
  bool get_accessible (tree_label tag, int nr);
  bool all_accessible (tree_label tag);
  void freeze_accessible (tree_label tag, int nr);
  bool is_accessible_child (tree t, int child);
  
  void set_block (tree_label tag, int nr, int require_block);
  int  get_block (tree_label tag, int nr);
  void freeze_block (tree_label tag, int nr);

  /* Heuristic initialization */
  bool heuristic_init (string var, tree macro);
  void heuristic_init (hashmap<string,tree> env);

  friend class drd_info;
  friend ostream& operator << (ostream& out, drd_info drd);
};

class drd_info {
  CONCRETE(drd_info);
  drd_info (string name);
  drd_info (string name, drd_info base);
  operator tree ();
};
CONCRETE_CODE(drd_info);

#endif // defined DRD_INFO_H
