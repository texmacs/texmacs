
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
template<class T> class list;
typedef list<int> path;

/******************************************************************************
* The observer class
******************************************************************************/

extern int observer_count;
class observer_rep: public abstract_struct {
public:
  inline observer_rep () { DEBUG(observer_count++); }
  inline virtual ~observer_rep () { DEBUG(observer_count--); }

  // Call back routines for tree modifications
  virtual void notify_assign      (tree& ref, tree t) = 0;
  virtual void notify_insert      (tree& ref, int pos, int nr) = 0;
  virtual void notify_remove      (tree& ref, int pos, int nr) = 0;
  virtual void notify_split       (tree& ref, int pos) = 0;
  virtual void notify_join        (tree& ref, int pos) = 0;
  virtual void notify_insert_node (tree& ref, int pos) = 0;
  virtual void notify_remove_node (tree& ref, int pos) = 0;
  virtual void notify_assign_node (tree& ref, tree_label op) = 0;
  //virtual void notify_ins_unary (tree& ref) = 0;
  //virtual void notify_rem_unary (tree& ref) = 0;

  // Extra routines for particular types of observers
  virtual path get_ip (tree& ref);
  virtual bool set_ip (tree& ref, path ip);
};

class observer {
public:
  ABSTRACT_NULL(observer);
};
ABSTRACT_NULL_CODE(observer);

observer ip_observer (path ip);
observer list_observer (observer o1, observer o2);

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
//void ins_unary (tree& ref, tree_label lab);
//void rem_unary (tree& ref);

path obtain_ip (tree& ref);
void attach_ip (tree& ref, path ip);
void detach_ip (tree& ref);

void stretched_print (tree t, bool ips= false, int indent= 0);

#endif // defined OBSERVER_H
