
/******************************************************************************
* MODULE     : undo_observer.cpp
* DESCRIPTION: Persistently attach inverse paths to trees
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
* Definition of the undo_observer_rep class
******************************************************************************/

class undo_observer_rep: public observer_rep {
  archiver_rep* arch;
public:
  undo_observer_rep (archiver_rep* arch2): arch (arch2) {}
  int get_type () { return OBSERVER_UNDO; }
  tm_ostream& print (tm_ostream& out) { return out << " undoer<" << arch << ">"; }
  void announce (tree& ref, modification mod);

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
undo_observer_rep::announce (tree& ref, modification mod) {
  if (mod->k == MOD_ASSIGN && mod->p == path () && mod->t == ref) return;
  if (!ip_attached (obtain_ip (ref))) return;
  //cout << "Archive " << mod << "\n";
  archive_announce (arch, reverse (obtain_ip (ref)) * mod);
}

/******************************************************************************
* Reattach when necessary
******************************************************************************/

void
undo_observer_rep::reattach (tree& ref, tree t) {
  if (ref.rep != t.rep) {
    remove_observer (ref->obs, observer (this));
    insert_observer (t->obs, observer (this));
  }
}

void
undo_observer_rep::notify_assign (tree& ref, tree t) {
  reattach (ref, t);
}

void
undo_observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  (void) t2;
  reattach (ref, t1); // always at the left
}

void
undo_observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  (void) ref; (void) offset;
  reattach (ref, t);
}

void
undo_observer_rep::notify_remove_node (tree& ref, int pos) {
  reattach (ref, ref[pos]);
}

void
undo_observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  (void) right;
  reattach (ref, closest);
}

/******************************************************************************
* Creation of undo_observers
******************************************************************************/

observer
undo_observer (archiver_rep* arch) {
  return tm_new<undo_observer_rep> (arch);
}
