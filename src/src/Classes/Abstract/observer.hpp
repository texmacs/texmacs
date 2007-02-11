
/******************************************************************************
* MODULE     : observer.hpp
* DESCRIPTION: Observers of trees
* COPYRIGHT  : (C) 2004  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef OBSERVER_H
#define OBSERVER_H
#include "string.hpp"
enum  tree_label;
class tree;
class hard_link_rep;
class observer;
template<class T> class list;
template<class T> class array;
typedef hard_link_rep* weak_link;
typedef list<int> path;

/******************************************************************************
* The observer class
******************************************************************************/

extern int observer_count;
class observer_rep: public abstract_struct {
public:
  inline observer_rep () { DEBUG(observer_count++); }
  inline virtual ~observer_rep () { DEBUG(observer_count--); }
  inline virtual ostream& print (ostream& out) { return out; }

  // Call back routines for tree modifications
  virtual void notify_assign      (tree& ref, tree t) = 0;
  virtual void notify_insert      (tree& ref, int pos, int nr) = 0;
  virtual void notify_remove      (tree& ref, int pos, int nr) = 0;
  virtual void notify_split       (tree& ref, int pos, tree prev) = 0;
  virtual void notify_var_split   (tree& ref, tree t1, tree t2) = 0;
  virtual void notify_join        (tree& ref, int pos, tree next) = 0;
  virtual void notify_var_join    (tree& ref, tree t, int offset) = 0;
  virtual void notify_insert_node (tree& ref, int pos) = 0;
  virtual void notify_remove_node (tree& ref, int pos) = 0;
  virtual void notify_assign_node (tree& ref, tree_label op) = 0;
  virtual void notify_detach      (tree& ref, tree closest, bool right) = 0;

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

extern observer nil_observer;
observer ip_observer (path ip);
observer list_observer (observer o1, observer o2);
observer tree_pointer (tree t);
observer tree_position (tree t, int index);

/******************************************************************************
* Modification routines for trees and other observer-related facilities
******************************************************************************/

void assign      (tree& ref, tree t);
void insert      (tree& ref, int pos, tree t);
void remove      (tree& ref, int pos, int nr);
void split       (tree& ref, int pos, int at);
void join        (tree& ref, int pos);
void insert_node (tree& ref, int pos, tree t);
void remove_node (tree& ref, int pos);
void assign_node (tree& ref, tree_label op);

void insert_observer (observer& o, observer what);
void remove_observer (observer& o, observer what);

path obtain_ip (tree& ref);
void attach_ip (tree& ref, path ip);
void detach_ip (tree& ref);

tree obtain_tree (observer o);
void attach_pointer (tree& ref, observer o);
void detach_pointer (tree& ref, observer o);
observer tree_pointer_new (tree t);
void tree_pointer_delete (observer o);

path obtain_position (observer o);
void attach_position (tree& ref, observer o);
void detach_position (tree& ref, observer o);

void stretched_print (tree t, bool ips= false, int indent= 0);

#endif // defined OBSERVER_H
