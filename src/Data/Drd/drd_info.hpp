
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
  string name;
  rel_hashmap<tree_label,tag_info> ti;

public:
  drd_info_rep (string name);
  drd_info_rep (string name, drd_info base);

  void set_arity (tree_label l, int arity);
  void set_props (tree_label l, int props);
  int  get_arity (tree_label l);
  int  get_props (tree_label l);

  bool is_dynamic (tree t);
  bool is_accessible_child (tree t, int child);
  bool is_child_enforcing (tree t);

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
