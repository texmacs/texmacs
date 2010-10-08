
/******************************************************************************
* MODULE     : edit_observer.cpp
* DESCRIPTION: Persistently attach editors to trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modification.hpp"

/******************************************************************************
* Definition of the edit_observer_rep class
******************************************************************************/

class edit_observer_rep: public observer_rep {
  editor_rep* ed;
public:
  edit_observer_rep (editor_rep* ed2): ed (ed2) {}
  int get_type () { return OBSERVER_EDIT; }
  tm_ostream& print (tm_ostream& out) { return out << " editor<" << ed << ">"; }

  void announce (tree& ref, modification mod);
  void done     (tree& ref, modification mod);
  void touched  (tree& ref, path p);

  void reattach           (tree& ref, tree t);
  void notify_assign      (tree& ref, tree t);
  void notify_var_split   (tree& ref, tree t1, tree t2);
  void notify_var_join    (tree& ref, tree t, int offset);
  void notify_remove_node (tree& ref, int pos);
  void notify_detach      (tree& ref, tree closest, bool right);
};

/******************************************************************************
* Call back routines for announcements
******************************************************************************/

void
edit_observer_rep::announce (tree& ref, modification mod) {
  //cout << "Editor " << mod << "\n";
  if (ip_attached (obtain_ip (ref)))
    edit_announce (ed, reverse (obtain_ip (ref)) * mod);
}

void
edit_observer_rep::done (tree& ref, modification mod) {
  //cout << "Done " << mod->p << "\n";
  if (ip_attached (obtain_ip (ref)))
    edit_done (ed, reverse (obtain_ip (ref)) * mod);
}

void
edit_observer_rep::touched (tree& ref, path p) {
  //cout << "Touched " << p << "\n";
  if (ip_attached (obtain_ip (ref)))
    edit_touch (ed, reverse (obtain_ip (ref)) * p);
}

/******************************************************************************
* Reattach when necessary
******************************************************************************/

void
edit_observer_rep::reattach (tree& ref, tree t) {
  if (ref.rep != t.rep) {
    remove_observer (ref->obs, observer (this));
    insert_observer (t->obs, observer (this));
  }
}

void
edit_observer_rep::notify_assign (tree& ref, tree t) {
  reattach (ref, t);
}

void
edit_observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  (void) t2;
  reattach (ref, t1); // always at the left
}

void
edit_observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  (void) ref; (void) offset;
  reattach (ref, t);
}

void
edit_observer_rep::notify_remove_node (tree& ref, int pos) {
  reattach (ref, ref[pos]);
}

void
edit_observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  (void) right;
  reattach (ref, closest);
}

/******************************************************************************
* Creation of edit_observers
******************************************************************************/

observer
edit_observer (editor_rep* ed) {
  return tm_new<edit_observer_rep> (ed);
}
