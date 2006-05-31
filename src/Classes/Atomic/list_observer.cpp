
/******************************************************************************
* MODULE     : list_observer.cpp
* DESCRIPTION: Attach several observers to trees
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
  ostream& print (ostream& out) {
    if (!nil (o1)) o1->print (out);
    if (!nil (o2)) o2->print (out);
    return out; }

  void notify_assign      (tree& ref, tree t);
  void notify_insert      (tree& ref, int pos, int nr);
  void notify_remove      (tree& ref, int pos, int nr);
  void notify_split       (tree& ref, int pos, tree prev);
  void notify_var_split   (tree& ref, tree t1, tree t2);
  void notify_join        (tree& ref, int pos, tree next);
  void notify_var_join    (tree& ref, tree t, int offset);
  void notify_insert_node (tree& ref, int pos);
  void notify_remove_node (tree& ref, int pos);
  void notify_assign_node (tree& ref, tree_label op);
  void notify_detach      (tree& ref, tree closest, bool right);

  bool get_ip (path& ip);
  bool set_ip (path ip);
  bool get_position (tree& t, int& index);
  bool set_position (tree t, int index);
  observer& get_child (int which);
  observer get_link_observer ();
  bool get_tree (tree& t);
  bool get_links (list<weak_link>& lns);
  bool insert_link (weak_link ln);
  bool remove_link (weak_link ln);
  bool get_unique_id (string& s);
  bool set_unique_id (string s);
};

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
list_observer_rep::notify_assign (tree& ref, tree t) {
  if (!nil (o1)) o1->notify_assign (ref, t);
  if (!nil (o2)) o2->notify_assign (ref, t);
}

void
list_observer_rep::notify_insert (tree& ref, int pos, int nr) {
  if (!nil (o1)) o1->notify_insert (ref, pos, nr);
  if (!nil (o2)) o2->notify_insert (ref, pos, nr);
}

void
list_observer_rep::notify_remove (tree& ref, int pos, int nr) {
  if (!nil (o1)) o1->notify_remove (ref, pos, nr);
  if (!nil (o2)) o2->notify_remove (ref, pos, nr);
}

void
list_observer_rep::notify_split (tree& ref, int pos, tree prev) {
  if (!nil (o1)) o1->notify_split (ref, pos, prev);
  if (!nil (o2)) o2->notify_split (ref, pos, prev);
}

void
list_observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  if (!nil (o1)) o1->notify_var_split (ref, t1, t2);
  if (!nil (o2)) o2->notify_var_split (ref, t1, t2);
}

void
list_observer_rep::notify_join (tree& ref, int pos, tree next) {
  if (!nil (o1)) o1->notify_join (ref, pos, next);
  if (!nil (o2)) o2->notify_join (ref, pos, next);
}

void
list_observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  if (!nil (o1)) o1->notify_var_join (ref, t, offset);
  if (!nil (o2)) o2->notify_var_join (ref, t, offset);
}

void
list_observer_rep::notify_insert_node (tree& ref, int pos) {
  if (!nil (o1)) o1->notify_insert_node (ref, pos);
  if (!nil (o2)) o2->notify_insert_node (ref, pos);
}

void
list_observer_rep::notify_remove_node (tree& ref, int pos) {
  if (!nil (o1)) o1->notify_remove_node (ref, pos);
  if (!nil (o2)) o2->notify_remove_node (ref, pos);
}

void
list_observer_rep::notify_assign_node (tree& ref, tree_label op) {
  if (!nil (o1)) o1->notify_assign_node (ref, op);
  if (!nil (o2)) o2->notify_assign_node (ref, op);
}

void
list_observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  if (!nil (o1)) o1->notify_detach (ref, closest, right);
  if (!nil (o2)) o2->notify_detach (ref, closest, right);
}

/******************************************************************************
* Further methods for special types of observers
******************************************************************************/

bool
list_observer_rep::get_ip (path& ip) {
  return (!nil (o1) && o1->get_ip (ip)) |
         (!nil (o2) && o2->get_ip (ip));
}

bool
list_observer_rep::set_ip (path ip) {
  return (!nil (o1) && o1->set_ip (ip)) |
         (!nil (o2) && o2->set_ip (ip));
}

bool
list_observer_rep::get_position (tree& t, int& index) {
  return (!nil (o1) && o1->get_position (t, index)) |
         (!nil (o2) && o2->get_position (t, index));
}

bool
list_observer_rep::set_position (tree t, int index) {
  return (!nil (o1) && o1->set_position (t, index)) |
         (!nil (o2) && o2->set_position (t, index));
}

observer&
list_observer_rep::get_child (int which) {
  if (which == 0) return o1;
  else return o2;
}

observer
list_observer_rep::get_link_observer () {
  if (!nil (o1)) {
    observer r1= o1->get_link_observer ();
    if (!nil (r1)) return r1;
  }
  if (!nil (o2)) {
    observer r2= o2->get_link_observer ();
    if (!nil (r2)) return r2;
  }
  return nil_observer;
}

bool
list_observer_rep::get_tree (tree& t) {
  // NOTE: we might also merge the lists
  return (!nil (o1) && o1->get_tree (t)) |
         (!nil (o2) && o2->get_tree (t));
}

bool
list_observer_rep::get_links (list<weak_link>& lns) {
  // NOTE: we might also merge the lists
  return (!nil (o1) && o1->get_links (lns)) |
         (!nil (o2) && o2->get_links (lns));
}

bool
list_observer_rep::insert_link (weak_link ln) {
  return (!nil (o1) && o1->insert_link (ln)) |
         (!nil (o2) && o2->insert_link (ln));
}

bool
list_observer_rep::remove_link (weak_link ln) {
  return (!nil (o1) && o1->remove_link (ln)) |
         (!nil (o2) && o2->remove_link (ln));
}

bool
list_observer_rep::get_unique_id (string& s) {
  return (!nil (o1) && o1->get_unique_id (s)) |
         (!nil (o2) && o2->get_unique_id (s));
}

bool
list_observer_rep::set_unique_id (string s) {
  return (!nil (o1) && o1->set_unique_id (s)) |
         (!nil (o2) && o2->set_unique_id (s));
}

/******************************************************************************
* Creation of list observers
******************************************************************************/

observer
list_observer (observer o1, observer o2) {
  if (nil (o1)) return o2;
  if (nil (o2)) return o1;
  return new list_observer_rep (o1, o2);
}

void
insert_observer (observer& o, observer what) {
  o= list_observer (o, what);
}

void
remove_observer (observer& o, observer what) {
  if (nil (o)) return;
  else if (o == what) o= nil_observer;
  else {
    remove_observer (o->get_child (0), what);
    remove_observer (o->get_child (1), what);
  }
}
