
/******************************************************************************
* MODULE     : list_observer.cpp
* DESCRIPTION: Attach several observers to trees
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
* Definition of the list_observer_rep class
******************************************************************************/

class list_observer_rep: public observer_rep {
  observer o1;
  observer o2;

public:
  list_observer_rep (observer o1b, observer o2b): o1 (o1b), o2 (o2b) {}
  int get_type () { return OBSERVER_LIST; }
  ostream& print (ostream& out) {
    if (!is_nil (o1)) o1->print (out);
    if (!is_nil (o2)) o2->print (out);
    return out; }

  void announce_assign      (tree& ref, path p, tree t);
  void announce_insert      (tree& ref, path p, tree ins);
  void announce_remove      (tree& ref, path p, int nr);
  void announce_split       (tree& ref, path p);
  void announce_join        (tree& ref, path p);
  void announce_assign_node (tree& ref, path p, tree_label op);
  void announce_insert_node (tree& ref, path p, tree ins);
  void announce_remove_node (tree& ref, path p);
  void announce_done        (tree& ref, path p);

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

  bool get_ip (path& ip);
  bool set_ip (path ip);
  bool get_position (tree& t, int& index);
  bool set_position (tree t, int index);
  observer& get_child (int which);
  list<observer> get_tree_pointers ();
  bool get_tree (tree& t);
};

/******************************************************************************
* Call back routines for announcements
******************************************************************************/

void
list_observer_rep::announce_assign (tree& ref, path p, tree t) {
  if (!is_nil (o1)) o1->announce_assign (ref, p, t);
  if (!is_nil (o2)) o2->announce_assign (ref, p, t);
}

void
list_observer_rep::announce_insert (tree& ref, path p, tree t) {
  if (!is_nil (o1)) o1->announce_insert (ref, p, t);
  if (!is_nil (o2)) o2->announce_insert (ref, p, t);
}

void
list_observer_rep::announce_remove (tree& ref, path p, int nr) {
  if (!is_nil (o1)) o1->announce_remove (ref, p, nr);
  if (!is_nil (o2)) o2->announce_remove (ref, p, nr);
}

void
list_observer_rep::announce_split (tree& ref, path p) {
  if (!is_nil (o1)) o1->announce_split (ref, p);
  if (!is_nil (o2)) o2->announce_split (ref, p);
}

void
list_observer_rep::announce_join (tree& ref, path p) {
  if (!is_nil (o1)) o1->announce_join (ref, p);
  if (!is_nil (o2)) o2->announce_join (ref, p);
}

void
list_observer_rep::announce_assign_node (tree& ref, path p, tree_label op) {
  if (!is_nil (o1)) o1->announce_assign_node (ref, p, op);
  if (!is_nil (o2)) o2->announce_assign_node (ref, p, op);
}

void
list_observer_rep::announce_insert_node (tree& ref, path p, tree t) {
  if (!is_nil (o1)) o1->announce_insert_node (ref, p, t);
  if (!is_nil (o2)) o2->announce_insert_node (ref, p, t);
}

void
list_observer_rep::announce_remove_node (tree& ref, path p) {
  if (!is_nil (o1)) o1->announce_remove_node (ref, p);
  if (!is_nil (o2)) o2->announce_remove_node (ref, p);
}

void
list_observer_rep::announce_done (tree& ref, path p) {
  if (!is_nil (o1)) o1->announce_done (ref, p);
  if (!is_nil (o2)) o2->announce_done (ref, p);
}

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
list_observer_rep::notify_assign (tree& ref, tree t) {
  if (!is_nil (o1)) o1->notify_assign (ref, t);
  if (!is_nil (o2)) o2->notify_assign (ref, t);
}

void
list_observer_rep::notify_insert (tree& ref, int pos, int nr) {
  if (!is_nil (o1)) o1->notify_insert (ref, pos, nr);
  if (!is_nil (o2)) o2->notify_insert (ref, pos, nr);
}

void
list_observer_rep::notify_remove (tree& ref, int pos, int nr) {
  if (!is_nil (o1)) o1->notify_remove (ref, pos, nr);
  if (!is_nil (o2)) o2->notify_remove (ref, pos, nr);
}

void
list_observer_rep::notify_split (tree& ref, int pos, tree prev) {
  if (!is_nil (o1)) o1->notify_split (ref, pos, prev);
  if (!is_nil (o2)) o2->notify_split (ref, pos, prev);
}

void
list_observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  if (!is_nil (o1)) o1->notify_var_split (ref, t1, t2);
  if (!is_nil (o2)) o2->notify_var_split (ref, t1, t2);
}

void
list_observer_rep::notify_join (tree& ref, int pos, tree next) {
  if (!is_nil (o1)) o1->notify_join (ref, pos, next);
  if (!is_nil (o2)) o2->notify_join (ref, pos, next);
}

void
list_observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  if (!is_nil (o1)) o1->notify_var_join (ref, t, offset);
  if (!is_nil (o2)) o2->notify_var_join (ref, t, offset);
}

void
list_observer_rep::notify_assign_node (tree& ref, tree_label op) {
  if (!is_nil (o1)) o1->notify_assign_node (ref, op);
  if (!is_nil (o2)) o2->notify_assign_node (ref, op);
}

void
list_observer_rep::notify_insert_node (tree& ref, int pos) {
  if (!is_nil (o1)) o1->notify_insert_node (ref, pos);
  if (!is_nil (o2)) o2->notify_insert_node (ref, pos);
}

void
list_observer_rep::notify_remove_node (tree& ref, int pos) {
  if (!is_nil (o1)) o1->notify_remove_node (ref, pos);
  if (!is_nil (o2)) o2->notify_remove_node (ref, pos);
}

void
list_observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  if (!is_nil (o1)) o1->notify_detach (ref, closest, right);
  if (!is_nil (o2)) o2->notify_detach (ref, closest, right);
}

/******************************************************************************
* Further methods for special types of observers
******************************************************************************/

bool
list_observer_rep::get_ip (path& ip) {
  return (!is_nil (o1) && o1->get_ip (ip)) |
         (!is_nil (o2) && o2->get_ip (ip));
}

bool
list_observer_rep::set_ip (path ip) {
  return (!is_nil (o1) && o1->set_ip (ip)) |
         (!is_nil (o2) && o2->set_ip (ip));
}

bool
list_observer_rep::get_position (tree& t, int& index) {
  return (!is_nil (o1) && o1->get_position (t, index)) |
         (!is_nil (o2) && o2->get_position (t, index));
}

bool
list_observer_rep::set_position (tree t, int index) {
  return (!is_nil (o1) && o1->set_position (t, index)) |
         (!is_nil (o2) && o2->set_position (t, index));
}

observer&
list_observer_rep::get_child (int which) {
  if (which == 0) return o1;
  else return o2;
}

list<observer>
list_observer_rep::get_tree_pointers () {
  list<observer> l;
  if (!is_nil (o1)) l= l * o1->get_tree_pointers ();
  if (!is_nil (o2)) l= l * o2->get_tree_pointers ();
  return l;
}

bool
list_observer_rep::get_tree (tree& t) {
  return (!is_nil (o1) && o1->get_tree (t)) |
         (!is_nil (o2) && o2->get_tree (t));
}

/******************************************************************************
* Creation of list observers
******************************************************************************/

observer
list_observer (observer o1, observer o2) {
  if (is_nil (o1)) return o2;
  if (is_nil (o2)) return o1;
  return tm_new<list_observer_rep> (o1, o2);
}

void
insert_observer (observer& o, observer what) {
  o= list_observer (o, what);
}

void
remove_observer (observer& o, observer what) {
  if (is_nil (o)) return;
  else if (o == what) o= nil_observer;
  else {
    remove_observer (o->get_child (0), what);
    remove_observer (o->get_child (1), what);
  }
}

void
clean_observers (observer& o) {
  if (is_nil (o)) return;
  if (o->get_type () == OBSERVER_IP) return;
  if (o->get_type () == OBSERVER_LIST) {
    clean_observers (o->get_child (0));
    clean_observers (o->get_child (1));
    if (is_nil (o->get_child (0))) o= o->get_child (1);
    else if (is_nil (o->get_child (1))) o= o->get_child (0);
  }
  else o= nil_observer;
}

void
attach_observer (tree& ref, observer o) {
  insert_observer (ref->obs, o);
}

void
detach_observer (tree& ref, observer o) {
  remove_observer (ref->obs, o);
}

void
clean_observers (tree& ref) {
  clean_observers (ref->obs);
}
