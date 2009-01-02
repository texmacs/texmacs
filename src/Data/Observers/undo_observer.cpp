
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

#include "tree.hpp"
#include "path.hpp"

/******************************************************************************
* Hooks
******************************************************************************/

void archive_assign      (tm_buffer buf, path p, tree t);
void archive_insert      (tm_buffer buf, path p, tree ins);
void archive_remove      (tm_buffer buf, path p, int nr);
void archive_split       (tm_buffer buf, path p);
void archive_join        (tm_buffer buf, path p);
void archive_assign_node (tm_buffer buf, path p, tree_label op);
void archive_insert_node (tm_buffer buf, path p, tree ins);
void archive_remove_node (tm_buffer buf, path p);

/******************************************************************************
* Definition of the undo_observer_rep class
******************************************************************************/

class undo_observer_rep: public observer_rep {
  tm_buffer buf;
public:
  undo_observer_rep (tm_buffer buf2): buf (buf2) {}
  int get_type () { return OBSERVER_UNDO; }
  ostream& print (ostream& out) { return out << " undoer<" << buf << ">"; }

  void announce_assign      (tree& ref, path p, tree t);
  void announce_insert      (tree& ref, path p, tree ins);
  void announce_remove      (tree& ref, path p, int nr);
  void announce_split       (tree& ref, path p);
  void announce_join        (tree& ref, path p);
  void announce_assign_node (tree& ref, path p, tree_label op);
  void announce_insert_node (tree& ref, path p, tree ins);
  void announce_remove_node (tree& ref, path p);

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
undo_observer_rep::announce_assign (tree& ref, path p, tree t) {
  if (p == path () && t == ref) return;
  //cout << "Archive assign " << p << ", " << t << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_assign (buf, reverse (obtain_ip (ref)) * p, t);
}

void
undo_observer_rep::announce_insert (tree& ref, path p, tree ins) {
  //cout << "Archive insert " << p << ", " << ins << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_insert (buf, reverse (obtain_ip (ref)) * p, ins);
}

void
undo_observer_rep::announce_remove (tree& ref, path p, int nr) {
  //cout << "Archive remove " << p << ", " << nr << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_remove (buf, reverse (obtain_ip (ref)) * p, nr);
}

void
undo_observer_rep::announce_split (tree& ref, path p) {
  //cout << "Archive split " << p << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_split (buf, reverse (obtain_ip (ref)) * p);
}

void
undo_observer_rep::announce_join (tree& ref, path p) {
  //cout << "Archive join " << p << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_join (buf, reverse (obtain_ip (ref)) * p);
}

void
undo_observer_rep::announce_assign_node (tree& ref, path p, tree_label op) {
  //cout << "Archive assign node " << p << ", " << op << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_assign_node (buf, reverse (obtain_ip (ref)) * p, op);
}

void
undo_observer_rep::announce_insert_node (tree& ref, path p, tree ins) {
  //cout << "Archive insert node " << p << ", " << ins << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_insert_node (buf, reverse (obtain_ip (ref)) * p, ins);
}

void
undo_observer_rep::announce_remove_node (tree& ref, path p) {
  //cout << "Archive remove node " << p << "\n";
  if (ip_attached (obtain_ip (ref)))
    archive_remove_node (buf, reverse (obtain_ip (ref)) * p);
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
undo_observer (tm_buffer buf) {
  return tm_new<undo_observer_rep> (buf);
}
