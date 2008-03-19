
/******************************************************************************
* MODULE     : tree_pointer.cpp
* DESCRIPTION: Persistently links between trees
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "link.hpp"
#include "list.hpp"

/******************************************************************************
* Definition of the tree_pointer_rep class
******************************************************************************/

class tree_pointer_rep: public observer_rep {
private:
  tree_rep* ptr;

public:
  tree_pointer_rep (tree ref): ptr (ref.rep) {}
  ostream& print (ostream& out) { return out << " pointer"; }

  void notify_assign      (tree& ref, tree t);
  void notify_insert      (tree& ref, int pos, int nr);
  void notify_remove      (tree& ref, int pos, int nr);
  void notify_split       (tree& ref, int pos, tree prev);
  void notify_var_split   (tree& ref, tree t1, tree t2);
  void notify_join        (tree& ref, int pos, tree next);
  void notify_var_join    (tree& ref, tree t, int offset);
  void notify_assign_node (tree& ref, tree_label op);
  void notify_insert_node (tree& ref, int pos);
  void notify_remove_node (tree& ref, int pos);
  void notify_detach      (tree& ref, tree closest, bool right);

  list<observer> get_tree_pointers ();
  bool set_tree (tree t);
  bool get_tree (tree& t);
};

/******************************************************************************
* Specific routines for tree link observers
******************************************************************************/

list<observer>
tree_pointer_rep::get_tree_pointers () {
  return list<observer> (observer (this));
}

bool
tree_pointer_rep::get_tree (tree& t) {
  t= tree (ptr);
  return true;
}

bool
tree_pointer_rep::set_tree (tree t) {
  if (ptr != t.rep) {
    tree ref (ptr);
    remove_observer (ref->obs, observer (this));
    ptr= t.rep;
    insert_observer (t->obs, observer (this));
  }
  return true;
}

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
tree_pointer_rep::notify_assign (tree& ref, tree t) {
  // cout << "Notify assign " << ref << ", " << t << "\n";
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_insert (tree& ref, int pos, int nr) {
  // cout << "Notify insert " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_remove (tree& ref, int pos, int nr) {
  // cout << "Notify remove " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref; (void) pos; (void) prev;
}

void
tree_pointer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  // cout << "Notify var split " << ref << ", " << t1 << ", " << t2 << "\n";
  (void) t2;
  (void) set_tree (t1); // always at the left
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref; (void) pos; (void) next;
}

void
tree_pointer_rep::notify_var_join (tree& ref, tree t, int offset) {
  // cout << "Notify var join " << ref << ", " << t << ", " << offset << "\n";
  (void) ref; (void) offset;
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_assign_node (tree& ref, tree_label op) {
  // cout << "Notify assign node " << ref << ", " << as_string (op) << "\n";
  (void) ref; (void) op;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_insert_node (tree& ref, int pos) {
  // cout << "Notify insert node " << ref << ", " << pos << "\n";
  (void) ref; (void) pos;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_remove_node (tree& ref, int pos) {
  // cout << "Notify remove node " << ref << ", " << pos << "\n";
  (void) set_tree (ref[pos]);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_detach (tree& ref, tree closest, bool right) {
  // cout << "Notify detach " << ref << ", " << closest <<", "<< right << "\n";
  (void) right;
  (void) set_tree (closest);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

/******************************************************************************
* Public interface
******************************************************************************/

observer
tree_pointer (tree ref) {
  return new tree_pointer_rep (ref);
}

tree
obtain_tree (observer obs) {
  tree t;
  (void) obs->get_tree (t);
  return t;
}

void
attach_pointer (tree& ref, observer o) {
  insert_observer (ref->obs, o);
}

void
detach_pointer (tree& ref, observer o) {
  remove_observer (ref->obs, o);
}

observer
tree_pointer_new (tree ref) {
  observer obs= tree_pointer (ref);
  attach_pointer (ref, obs);
  return obs;
}

void
tree_pointer_delete (observer obs) {
  tree ref= obtain_tree (obs);
  detach_pointer (ref, obs);
}
