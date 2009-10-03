
/******************************************************************************
* MODULE     : observer.hpp
* DESCRIPTION: Observers of trees
* COPYRIGHT  : (C) 2004  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OBSERVER_H
#define OBSERVER_H
#include "string.hpp"
enum  tree_label;
class tree;
class hard_link_rep;
class observer;
class modification;
template<class T> class list;
template<class T> class array;
typedef hard_link_rep* weak_link;
typedef list<int> path;

#define OBSERVER_UNKNOWN    0
#define OBSERVER_LIST       1
#define OBSERVER_IP         2
#define OBSERVER_POINTER    3
#define OBSERVER_POSITION   4
#define OBSERVER_EDIT       5
#define OBSERVER_UNDO       6

/******************************************************************************
* The observer class
******************************************************************************/

extern int observer_count;
class observer_rep: public abstract_struct {
public:
  inline observer_rep () { TM_DEBUG(observer_count++); }
  inline virtual ~observer_rep () { TM_DEBUG(observer_count--); }
  inline virtual int get_type () { return OBSERVER_UNKNOWN; }
  inline virtual ostream& print (ostream& out) { return out; }

  // Announcing modifications in subtrees
  virtual void announce             (tree& ref, modification mod);
  virtual void announce_assign      (tree& ref, path p, tree t);
  virtual void announce_insert      (tree& ref, path p, tree ins);
  virtual void announce_remove      (tree& ref, path p, int nr);
  virtual void announce_split       (tree& ref, path p);
  virtual void announce_join        (tree& ref, path p);
  virtual void announce_assign_node (tree& ref, path p, tree_label op);
  virtual void announce_insert_node (tree& ref, path p, tree ins);
  virtual void announce_remove_node (tree& ref, path p);
  virtual void done                 (tree& ref, modification mod);

  // Call back routines for tree modifications
  virtual void notify_assign        (tree& ref, tree t);
  virtual void notify_insert        (tree& ref, int pos, int nr);
  virtual void notify_remove        (tree& ref, int pos, int nr);
  virtual void notify_split         (tree& ref, int pos, tree prev);
  virtual void notify_var_split     (tree& ref, tree t1, tree t2);
  virtual void notify_join          (tree& ref, int pos, tree next);
  virtual void notify_var_join      (tree& ref, tree t, int offset);
  virtual void notify_assign_node   (tree& ref, tree_label op);
  virtual void notify_insert_node   (tree& ref, int pos);
  virtual void notify_remove_node   (tree& ref, int pos);
  virtual void notify_detach        (tree& ref, tree closest, bool right);

  // Extra routines for particular types of observers
  virtual bool get_ip (path& ip);
  virtual bool set_ip (path ip);
  virtual bool get_position (tree& t, int& index);
  virtual bool set_position (tree t, int index);
  virtual observer& get_child (int which);
  virtual list<observer> get_tree_pointers ();
  virtual bool get_tree (tree& t);
};

class observer {
public:
  ABSTRACT_NULL(observer);
  inline friend bool operator == (observer o1, observer o2) {
    return o1.rep == o2.rep; }
  inline friend bool operator != (observer o1, observer o2) {
    return o1.rep != o2.rep; }
  inline friend int hash (observer o1) {
    return hash ((pointer) o1.rep); }
};
ABSTRACT_NULL_CODE(observer);

ostream& operator << (ostream& out, observer o);

class editor_rep;
class archiver_rep;

extern observer nil_observer;
observer ip_observer (path ip);
observer list_observer (observer o1, observer o2);
observer tree_pointer (tree t, bool flag= false);
observer tree_position (tree t, int index);
observer edit_observer (editor_rep* ed);
observer undo_observer (archiver_rep* arch);

/******************************************************************************
* Modification routines for trees and other observer-related facilities
******************************************************************************/

extern bool versioning_busy;
bool busy_tree (tree& ref);

void assign      (tree& ref, tree t);
void insert      (tree& ref, int pos, tree t);
void remove      (tree& ref, int pos, int nr);
void split       (tree& ref, int pos, int at);
void join        (tree& ref, int pos);
void assign_node (tree& ref, tree_label op);
void insert_node (tree& ref, int pos, tree t);
void remove_node (tree& ref, int pos);

void assign      (path p, tree t);
void insert      (path p, tree ins);
void remove      (path p, int nr);
void split       (path p);
void join        (path p);
void assign_node (path p, tree_label op);
void insert_node (path p, tree ins);
void remove_node (path p);

void insert_observer (observer& o, observer what);
void remove_observer (observer& o, observer what);
void attach_observer (tree& ref, observer o);
void detach_observer (tree& ref, observer o);
void clean_observers (tree& ref);

path obtain_ip (tree& ref);
void attach_ip (tree& ref, path ip);
void detach_ip (tree& ref);
bool ip_attached (path ip);

tree obtain_tree (observer o);
observer tree_pointer_new (tree t);
void tree_pointer_delete (observer o);

path obtain_position (observer o);

void stretched_print (tree t, bool ips= false, int indent= 0);

#endif // defined OBSERVER_H
