
/******************************************************************************
* MODULE     : tree_reference.cpp
* DESCRIPTION: References to trees
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "server.hpp"
#include "tree_reference.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Document modification routines
******************************************************************************/

static inline bool
ip_attached (path ip) {
  return nil (ip) || last_item (ip) != DETACHED;
}

void
tree_assign (tree r, tree t) {
  path ip= obtain_ip (r);
  //cout << "Assign " << r << ", " << t << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->assign (reverse (ip), t);
  else assign (r, t);
}

void
tree_insert_node (tree r, int pos, tree t) {
  path ip= obtain_ip (r);
  //cout << "Insert node " << r << ", " << pos << ", " << t
  //     << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->insert_node (reverse (ip) * pos, t);
  else insert_node (r, pos, t);
}

void
tree_remove_node (tree r, int pos) {
  path ip= obtain_ip (r);
  //cout << "Remove node " << r << ", " << pos << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->remove_node (reverse (ip) * pos);
  else remove_node (r, pos);
}

void
tree_reference_assign (tree_reference r, tree t) {
  path ip= obtain_ip (r);
  //cout << "Assign " << r << ", " << t << " at " << ip << "\n";
  if (ip_attached (ip)) {
    path p= reverse (ip);
    get_server()->get_editor()->assign (p, t);
    r= get_server()->get_editor()->the_subtree (p);
  }
  else assign (r, t);
}

void
tree_reference_insert (tree_reference r, int pos, tree t) {
  path ip= obtain_ip (r);
  //cout << "Insert " << r << ", " << pos << ", " << t
  //     << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->insert (reverse (ip) * pos, t);
  else insert (r, pos, t);
}

void
tree_reference_remove (tree_reference r, int pos, int nr) {
  path ip= obtain_ip (r);
  //cout << "Remove " << r << ", " << pos << ", " << nr
  //     << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->remove (reverse (ip) * pos, nr);
  else remove (r, pos, nr);
}

void
tree_reference_split (tree_reference r, int pos, int at) {
  path ip= obtain_ip (r);
  //cout << "Split " << r << ", " << pos << ", " << at
  //     << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->split (reverse (ip) * path (pos, at));
  else split (r, pos, at);
}

void
tree_reference_join (tree_reference r, int pos) {
  path ip= obtain_ip (r);
  //cout << "Join " << r << ", " << pos << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->join (reverse (ip) * pos);
  else join (r, pos);
}

void
tree_reference_assign_node (tree_reference r, tree_label op) {
  path ip= obtain_ip (r);
  //cout << "Assign node " << r << ", " << tree (op) << " at " << ip << "\n";
  if (ip_attached (ip))
    get_server()->get_editor()->assign_node (reverse (ip), op);
  else assign_node (r, op);
}

void
tree_reference_insert_node (tree_reference r, int pos, tree t) {
  path ip= obtain_ip (r);
  //cout << "Insert node " << r << ", " << pos << ", " << t
  //     << " at " << ip << "\n";
  if (ip_attached (ip)) {
    path p= reverse (ip);
    get_server()->get_editor()->insert_node (p * pos, t);
    r= get_server()->get_editor()->the_subtree (p);
  }
  else insert_node (r, pos, t);
}

void
tree_reference_remove_node (tree_reference r, int pos) {
  path ip= obtain_ip (r);
  //cout << "Remove node " << r << ", " << pos << " at " << ip << "\n";
  if (ip_attached (ip)) {
    path p= reverse (ip);
    get_server()->get_editor()->remove_node (p * pos);
    r= get_server()->get_editor()->the_subtree (p);
  }
  else remove_node (r, pos);
}
