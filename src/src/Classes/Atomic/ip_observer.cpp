
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

  virtual void assign    (tree& ot, tree t);
  virtual void insert    (tree& ot, int pos, int nr);
  virtual void remove    (tree& ot, int pos, int nr);
  virtual void split     (tree& ot, int pos);
  virtual void join      (tree& ot, int pos);
  virtual void ins_unary (tree& ot);
  virtual void rem_unary (tree& ot);

  virtual path get_ip    (tree& ot);
  virtual bool set_ip    (tree& ot, path ip);
};

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
ip_observer_rep::assign (tree& ot, tree t) {
  path temp_ip= _get_ip (ot);
  temp_ip= path (temp_ip->item, temp_ip->next); // prevents overriding temp_ip
  _detach_ip (ot);
  _set_ip (t, temp_ip);
}

void
ip_observer_rep::insert (tree& ot, int pos, int nr) {
  (void) nr;
  if (is_compound (ot)) {
    int i, n= N(ot);
    for (i=pos; i<n; i++)
      _set_ip (ot[i], path (i, ip));
  }
}

void
ip_observer_rep::remove (tree& ot, int pos, int nr) {
  (void) nr;
  if (is_compound (ot)) {
    int i, n= N(ot);
    for (i=pos; i<(pos+nr); i++)
      _detach_ip (ot[i]);
    for (; i<n; i++)
      _set_ip (ot[i], path (i-nr, ip));
  }
}

void
ip_observer_rep::split (tree& ot, int pos) {
  int i, n= N(ot);
  for (i=pos; i<n; i++)
    _set_ip (ot[i], path (i, ip));
}

void
ip_observer_rep::join (tree& ot, int pos) {
  int i, n= N(ot);
  for (i=pos+2; i<n; i++)
    _set_ip (ot[i], path (i-1, ip));
  if (is_compound (ot[pos]) && is_compound (ot[pos+1])) {
    int n1= N(ot[pos]), n2= N(ot[pos+1]);
    for (i=0; i<n2; i++)
      _set_ip (ot[pos+1][i], path (n1+i, _get_ip (ot[pos])));
  }
  _detach_ip (ot[pos+1]);
}

void
ip_observer_rep::ins_unary (tree& ot) {
  ip= path (0, ip);
  _set_ip (ot, ip->next);
}

void
ip_observer_rep::rem_unary (tree& ot) {
  if ((!nil (ip)) && (ip->item>=0)) _set_ip (ot[0], ip);
  else _detach_ip (ot[0]);
  _detach_ip (ot);
}

/******************************************************************************
* Setting and getting inverse paths
******************************************************************************/

path
ip_observer_rep::get_ip (tree& ot) {
  return ip;
}

bool
ip_observer_rep::set_ip (tree& ot, path ip2) {
  if (nil (ip2))
    fatal_error ("cannot set ip to null", "ip_observer_rep::set_ip");
  ip->item= ip2->item;
  ip->next= ip2->next;
  return false;
}

void
_set_ip (tree& ot, path ip) {
  // cout << "Set ip of " << ot << " to " << ip << "\n";
  if (nil (ot->obs) || ot->obs->set_ip (ot, ip))
    ot->obs= list_observer (ip_observer (ip), ot->obs);
  if (is_compound (ot)) {
    int i, n= N(ot);
    for (i=0; i<n; i++)
      if (_get_ip (ot[i]) != path (i, ip))
	_set_ip (ot[i], path (i, ip));
  }
}

void
_detach_ip (tree& ot) {
  // cout << "Detach ip of " << ot << "\n";
  if (!nil (ot->obs))
    (void) ot->obs->set_ip (ot, DETACHED);
}

path
_get_ip (tree& ot) {
  if (nil (ot->obs)) return DETACHED;
  return ot->obs->get_ip (ot);
}

/******************************************************************************
* Setting and getting inverse paths
******************************************************************************/

observer
ip_observer (path ip) {
  return new ip_observer_rep (ip);
}
