
/******************************************************************************
* MODULE     : edit_observer.cpp
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

#include "tree.hpp"
#include "path.hpp"

/******************************************************************************
* Hooks
******************************************************************************/

void edit_assign      (editor_rep* ed, path p, tree t);
void edit_insert      (editor_rep* ed, path p, tree ins);
void edit_remove      (editor_rep* ed, path p, int nr);
void edit_split       (editor_rep* ed, path p);
void edit_join        (editor_rep* ed, path p);
void edit_assign_node (editor_rep* ed, path p, tree_label op);
void edit_insert_node (editor_rep* ed, path p, tree ins);
void edit_remove_node (editor_rep* ed, path p);
void edit_done        (editor_rep* ed, path p);

/******************************************************************************
* Definition of the edit_observer_rep class
******************************************************************************/

class edit_observer_rep: public observer_rep {
  editor_rep* ed;
public:
  edit_observer_rep (editor_rep* ed2): ed (ed2) {}
  ostream& print (ostream& out) { return out << " editor<" << ed << ">"; }

  void announce_assign      (tree& ref, path p, tree t);
  void announce_insert      (tree& ref, path p, tree ins);
  void announce_remove      (tree& ref, path p, int nr);
  void announce_split       (tree& ref, path p);
  void announce_join        (tree& ref, path p);
  void announce_assign_node (tree& ref, path p, tree_label op);
  void announce_insert_node (tree& ref, path p, tree ins);
  void announce_remove_node (tree& ref, path p);
  void announce_done        (tree& ref, path p);
};

/******************************************************************************
* Call back routines for announcements
******************************************************************************/

void
edit_observer_rep::announce_assign (tree& ref, path p, tree t) {
  //cout << "Assign " << p << ", " << t << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_assign (ed, reverse (obtain_ip (ref)) * p, t);
}

void
edit_observer_rep::announce_insert (tree& ref, path p, tree ins) {
  //cout << "Insert " << p << ", " << ins << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_insert (ed, reverse (obtain_ip (ref)) * p, ins);
}

void
edit_observer_rep::announce_remove (tree& ref, path p, int nr) {
  //cout << "Remove " << p << ", " << nr << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_remove (ed, reverse (obtain_ip (ref)) * p, nr);
}

void
edit_observer_rep::announce_split (tree& ref, path p) {
  //cout << "Split " << p << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_split (ed, reverse (obtain_ip (ref)) * p);
}

void
edit_observer_rep::announce_join (tree& ref, path p) {
  //cout << "Join " << p << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_join (ed, reverse (obtain_ip (ref)) * p);
}

void
edit_observer_rep::announce_assign_node (tree& ref, path p, tree_label op) {
  //cout << "Assign node " << p << ", " << op << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_assign_node (ed, reverse (obtain_ip (ref)) * p, op);
}

void
edit_observer_rep::announce_insert_node (tree& ref, path p, tree ins) {
  //cout << "Insert node " << p << ", " << ins << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_insert_node (ed, reverse (obtain_ip (ref)) * p, ins);
}

void
edit_observer_rep::announce_remove_node (tree& ref, path p) {
  //cout << "Remove node " << p << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_remove_node (ed, reverse (obtain_ip (ref)) * p);
}

void
edit_observer_rep::announce_done (tree& ref, path p) {
  //cout << "Done " << p << "\n";
  //if (ip_attached (obtain_ip (ref)))
  //edit_done (ed, reverse (obtain_ip (ref)) * p);
}

/******************************************************************************
* Creation of edit_observers
******************************************************************************/

observer
edit_observer (editor_rep* ed) {
  return tm_new<edit_observer_rep> (ed);
}

void
attach_editor (tree& ref, observer o) {
  insert_observer (ref->obs, o);
}

void
detach_editor (tree& ref, observer o) {
  remove_observer (ref->obs, o);
}
