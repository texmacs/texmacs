
/******************************************************************************
* MODULE     : ip_observer.cpp
* DESCRIPTION: Persistently attach inverse paths to trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
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
* Definition of the ip_observer_rep class
******************************************************************************/

class ip_observer_rep: public observer_rep {
  path ip;
public:
  ip_observer_rep (path ip2): ip (ip2) {}

  virtual void notify_assign      (tree& ref, tree t);
  virtual void notify_insert      (tree& ref, int pos, int nr);
  virtual void notify_remove      (tree& ref, int pos, int nr);
  virtual void notify_split       (tree& ref, int pos);
  virtual void notify_join        (tree& ref, int pos);
  virtual void notify_insert_node (tree& ref, int pos);
  virtual void notify_remove_node (tree& ref, int pos);
  virtual void notify_assign_node (tree& ref, tree_label op);

  virtual path get_ip (tree& ref);
  virtual bool set_ip (tree& ref, path ip);
};

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
ip_observer_rep::notify_assign (tree& ref, tree t) {
  path temp_ip= obtain_ip (ref);
  temp_ip= path (temp_ip->item, temp_ip->next); // prevents overriding temp_ip
  detach_ip (ref);
  attach_ip (t, temp_ip);
}

void
ip_observer_rep::notify_insert (tree& ref, int pos, int nr) {
  (void) nr;
  if (is_compound (ref)) {
    int i, n= N(ref);
    for (i=pos; i<n; i++)
      attach_ip (ref[i], path (i, ip));
  }
}

void
ip_observer_rep::notify_remove (tree& ref, int pos, int nr) {
  (void) nr;
  if (is_compound (ref)) {
    int i, n= N(ref);
    for (i=pos; i<(pos+nr); i++)
      detach_ip (ref[i]);
    for (; i<n; i++)
      attach_ip (ref[i], path (i-nr, ip));
  }
}

void
ip_observer_rep::notify_split (tree& ref, int pos) {
  int i, n= N(ref);
  for (i=pos; i<n; i++)
    attach_ip (ref[i], path (i, ip));
}

void
ip_observer_rep::notify_join (tree& ref, int pos) {
  int i, n= N(ref);
  for (i=pos+2; i<n; i++)
    attach_ip (ref[i], path (i-1, ip));
  if (is_compound (ref[pos]) && is_compound (ref[pos+1])) {
    int n1= N(ref[pos]), n2= N(ref[pos+1]);
    for (i=0; i<n2; i++)
      attach_ip (ref[pos+1][i], path (n1+i, obtain_ip (ref[pos])));
  }
  detach_ip (ref[pos+1]);
}

void
ip_observer_rep::notify_insert_node (tree& ref, int pos) {
  ip= path (pos, ip);
  attach_ip (ref[pos], ip); // updates children's ips
  attach_ip (ref, ip->next);
}

void
ip_observer_rep::notify_remove_node (tree& ref, int pos) {
  if ((!nil (ip)) && (ip->item>=0)) attach_ip (ref[pos], ip);
  else detach_ip (ref[pos]);
  ip= DETACHED; // detach_ip (ref);
}

void
ip_observer_rep::notify_assign_node (tree& ref, tree_label op) {
  (void) ref; (void) op;
}

/******************************************************************************
* Setting and getting inverse paths
******************************************************************************/

path
ip_observer_rep::get_ip (tree& ref) {
  (void) ref;
  return ip;
}

bool
ip_observer_rep::set_ip (tree& ref, path ip2) {
  (void) ref;
  if (nil (ip) || nil (ip2))
    fatal_error ("cannot alter global root", "ip_observer_rep::set_ip");
  ip->item= ip2->item;
  ip->next= ip2->next;
  return false;
}

void
attach_ip (tree& ref, path ip) {
  // cout << "Set ip of " << ref << " to " << ip << "\n";
  if (nil (ref->obs) || ref->obs->set_ip (ref, ip)) {
    // cout << "Create ip observer " << ip << " for " << ref << "\n";
    ref->obs= list_observer (ip_observer (ip), ref->obs);
  }
  if (is_compound (ref)) {
    int i, n= N(ref);
    for (i=0; i<n; i++) {
      path old_ip= obtain_ip (ref[i]);
      if ((old_ip->item != i) || (!strong_equal (old_ip->next, ip))) {
	attach_ip (ref[i], path (i, ip));
      }
    }
  }
}

void
detach_ip (tree& ref) {
  // cout << "Detach ip of " << ref << "\n";
  if (!nil (ref->obs))
    (void) ref->obs->set_ip (ref, DETACHED);
}

path
obtain_ip (tree& ref) {
  if (nil (ref->obs)) return DETACHED;
  return ref->obs->get_ip (ref);
}

/******************************************************************************
* Setting and getting inverse paths
******************************************************************************/

observer
ip_observer (path ip) {
  return new ip_observer_rep (ip);
}
