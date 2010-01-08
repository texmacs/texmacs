
/******************************************************************************
* MODULE     : tree_position.cpp
* DESCRIPTION: Persistently attach cursor positions to trees
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree.hpp"
#include "path.hpp"

#define DETACHED (-5)

/******************************************************************************
* Definition of the tree_position_rep class
******************************************************************************/

class tree_position_rep: public observer_rep {
  tree_rep* ptr;
  int index;

public:
  tree_position_rep (tree ref, int index2): ptr (ref.rep), index (index2) {}
  int get_type () { return OBSERVER_POSITION; }
  tm_ostream& print (tm_ostream& out) { return out << " " << index; }

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

  bool get_position (tree& t, int& index);
  bool set_position (tree t, int index);
};

/******************************************************************************
* Re-attaching the position to another tree
******************************************************************************/

bool
tree_position_rep::get_position (tree& t, int& index2) {
  t     = tree (ptr);
  index2= index;
  return true;
}

bool
tree_position_rep::set_position (tree t, int index2) {
  tree ref (ptr);
  detach_observer (ref, observer (this));
  ptr  = t.rep;
  index= index2;
  attach_observer (t, observer (this));
  return true;
}

void
reattach_at (tree_position_rep* rep, tree t, int index) {
  rep->set_position (t, index);
}

void reattach_left (tree_position_rep* rep, tree t) {
  reattach_at (rep, t, 0); }
void reattach_right (tree_position_rep* rep, tree t) {
  reattach_at (rep, t, right_index (t)); }

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
tree_position_rep::notify_assign (tree& ref, tree t) {
  // cout << "Notify assign " << ref << ", " << t << "\n";
  bool left=
    (is_atomic (ref) && (index <= (N(ref->label) >> 1))) || (index == 0);
  if (left) reattach_left (this, t);
  else reattach_right (this, t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_insert (tree& ref, int pos, int nr) {
  // cout << "Notify insert " << ref << ", " << pos << ", " << nr << "\n";
  if (is_atomic (ref) && index >= pos)
    index += nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_remove (tree& ref, int pos, int nr) {
  // cout << "Notify remove " << ref << ", " << pos << ", " << nr << "\n";
  if (is_atomic (ref)) {
    if (index >= pos)
      index= max (pos, index - nr);
  }
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref; (void) pos; (void) prev;
}

void
tree_position_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  // cout << "Notify var split " << ref << ", " << t1 << ", " << t2 << "\n";
  if (is_atomic (ref)) {
    if (index <= N(t1->label)) reattach_at (this, t1, index);
    else reattach_at (this, t2, index - N(t1->label));
  }
  else {
    if (index == 0) reattach_left (this, t1);
    else reattach_right (this, t2);
  }
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref; (void) pos; (void) next;
}

void
tree_position_rep::notify_var_join (tree& ref, tree t, int offset) {
  // cout << "Notify var join " << ref << ", " << t << ", " << offset << "\n";
  if (is_atomic (ref))
    reattach_at (this, t, index + offset);
  else {
    if (index == 0) reattach_right (this, t[offset-1]);
    else reattach_right (this, t);
  }
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_assign_node (tree& ref, tree_label op) {
  // cout << "Notify assign node " << ref << ", " << as_string (op) << "\n";
  (void) ref; (void) op;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_insert_node (tree& ref, int pos) {
  // cout << "Notify insert node " << ref << ", " << pos << "\n";
  (void) ref; (void) pos;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_remove_node (tree& ref, int pos) {
  // cout << "Notify remove node " << ref << ", " << pos << "\n";
  if (index == 0) reattach_left (this, ref[pos]);
  else reattach_right (this, ref[pos]);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_position_rep::notify_detach (tree& ref, tree closest, bool right) {
  // cout << "Notify detach " << ref << ", " << closest <<", "<< right << "\n";
  if (right) reattach_right (this, closest);
  else reattach_left (this, closest);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

/******************************************************************************
* Public interface
******************************************************************************/

observer
tree_position (tree ref, int index) {
  return tm_new<tree_position_rep> (ref, index);
}

path
obtain_position (observer o) {
  tree t;
  int  index;
  if (!o->get_position (t, index)) return path ();
  path ip= obtain_ip (t);
  if (ip == path (DETACHED)) return path ();
  return reverse (ip) * index;
}
