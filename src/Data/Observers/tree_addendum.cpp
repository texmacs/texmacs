
/******************************************************************************
* MODULE     : tree_addendum.cpp
* DESCRIPTION: Persistently attach data to trees
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree.hpp"
#include "blackbox.hpp"

/******************************************************************************
* Definition of the tree_pointer_rep class
******************************************************************************/

class tree_addendum_rep: public observer_rep {
private:
  tree_rep* ptr;
  int kind;
  blackbox contents;

public:
  tree_addendum_rep (tree ref, int kind2, blackbox contents2):
    ptr (ref.rep), kind (kind2), contents (contents2) {}
  int get_type () { return OBSERVER_ADDENDUM; }
  tm_ostream& print (tm_ostream& out) {
    return out << " addendum (" << kind << ", " << contents << ")"; }

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

  bool get_contents (int kind, blackbox& bb);
  bool set_tree (tree t);
  bool get_tree (tree& t);
};

/******************************************************************************
* Specific routines for tree_addendum observers
******************************************************************************/

bool
tree_addendum_rep::get_contents (int which, blackbox& bb) {
  if (which != kind) return false;
  bb= contents;
  return true;
}

bool
tree_addendum_rep::get_tree (tree& t) {
  t= tree (ptr);
  return true;
}

bool
tree_addendum_rep::set_tree (tree t) {
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
tree_addendum_rep::notify_assign (tree& ref, tree t) {
  // cout << "Notify assign " << ref << ", " << t << "\n";
  (void) ref; (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_insert (tree& ref, int pos, int nr) {
  // cout << "Notify insert " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_remove (tree& ref, int pos, int nr) {
  // cout << "Notify remove " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref; (void) pos; (void) prev;
}

void
tree_addendum_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  // cout << "Notify var split " << ref << ", " << t1 << ", " << t2 << "\n";
  (void) t2; (void) ref;
  (void) set_tree (t1); // always at the left
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref; (void) pos; (void) next;
}

void
tree_addendum_rep::notify_var_join (tree& ref, tree t, int offset) {
  // cout << "Notify var join " << ref << ", " << t << ", " << offset << "\n";
  (void) ref; (void) offset;
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_assign_node (tree& ref, tree_label op) {
  // cout << "Notify assign node " << ref << ", " << as_string (op) << "\n";
  (void) ref; (void) op;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_insert_node (tree& ref, int pos) {
  //cout << "Notify insert node " << ref << ", " << pos << "\n";
  // NOTE: we might want to remove these lines; see tree_pointer.cpp
  remove_observer (ref[pos]->obs, observer (this));
  ptr= ref.rep;
  insert_observer (ref->obs, observer (this));
  //cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_remove_node (tree& ref, int pos) {
  // cout << "Notify remove node " << ref << ", " << pos << "\n";
  (void) set_tree (ref[pos]);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_addendum_rep::notify_detach (tree& ref, tree closest, bool right) {
  // cout << "Notify detach " << ref << ", " << closest <<", "<< right << "\n";
  (void) right; (void) ref;
  (void) set_tree (closest);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

/******************************************************************************
* Public interface
******************************************************************************/

observer
tree_addendum (tree ref, int kind, blackbox contents) {
  return tm_new<tree_addendum_rep> (ref, kind, contents);
}

observer
tree_addendum_new (tree ref, int kind, blackbox contents) {
  observer obs= tree_addendum (ref, kind, contents);
  attach_observer (ref, obs);
  return obs;
}

void
tree_addendum_delete (observer obs) {
  tree ref= obtain_tree (obs);
  detach_observer (ref, obs);
}
