
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
  virtual void assign    (tree& ot, tree t) = 0;
  virtual void insert    (tree& ot, int pos, int nr) = 0;
  virtual void remove    (tree& ot, int pos, int nr) = 0;
  virtual void split     (tree& ot, int pos) = 0;
  virtual void join      (tree& ot, int pos) = 0;
  virtual void ins_unary (tree& ot) = 0;
  virtual void rem_unary (tree& ot) = 0;

  // Extra routines for particular types of observers
  virtual path get_ip    (tree& ot);
  virtual bool set_ip    (tree& ot, path ip);
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

void _assign    (tree& ot, tree t);
void _insert    (tree& ot, int pos, tree t);
void _remove    (tree& ot, int pos, int nr);
void _split     (tree& ot, int pos, int at);
void _join      (tree& ot, int pos);
void _ins_unary (tree& ot, tree_label lab);
void _rem_unary (tree& ot);

path _get_ip    (tree& ot);
void _set_ip    (tree& ot, path ip);
void _detach_ip (tree& ot);

void stretched_print (tree t, bool ips= false, int indent= 0);

#endif // defined OBSERVER_H
