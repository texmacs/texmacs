
/******************************************************************************
* MODULE     : observer.cpp
* DESCRIPTION: Observers of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree.hpp"
#include "path.hpp"
#include "analyze.hpp"

#define DETACHED (-5)

/******************************************************************************
* Debugging facilities
******************************************************************************/

extern tree the_et;

static void
consistency_check (tree t, path ip) {
  if (_get_ip (t) != ip)
    cout << "Wrong ip] " << t << " " << _get_ip (t)
	 << " instead of " << ip << "\n";
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      consistency_check (t[i], path (i, ip));
  }
}

void
consistency_check () {
  consistency_check (the_et, path ());
  cout << HRULE;
}

void
stretched_print (tree t, bool ips, int indent) {
  int i;
  for (i=0; i<indent; i++) cout << "  ";
  if (is_atomic (t)) {
    cout << quote (t->label);
    if (ips) cout << " -- " << _get_ip (t);
    cout << "\n";
  }
  else {
    cout << as_string (L(t));
    if (ips) cout << " -- " << _get_ip (t);
    cout << "\n";    
    for (i=0; i<N(t); i++)
      stretched_print (t[i], ips, indent+1);
  }
}

/******************************************************************************
* Routines for modifying trees
*******************************************************************************
* Notice that "inserting modifications" (insert, split and ins_unary)
* invoke the observers call-back routines after the actual modification and
* "assigning and deleting modifications" (assign, remove, join and rem_unary)
* before the actual modification.
******************************************************************************/

void
_assign (tree& ot, tree t) {
  // cout << "Assign " << ot << " := " << t << "\n";
  if (!nil (ot->obs)) ot->obs->assign (ot, t);
  ot= t;
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_insert (tree& ot, int pos, tree t) {
  // cout << "Insert " << ot << " += " << t << " at " << pos << "\n";
  if (is_atomic (ot) && is_atomic (t))
    ot->label= insert (ot->label, pos, t->label);
  else {
    int i, n= N(ot), nr= N(t);
    A(ot)->resize (n+nr);
    for (i=n-1; i>=pos; i--)
      ot[i+nr]= ot[i];
    for (i=0; i<nr; i++)
      ot[pos+i]= t[i];
  }
  if (!nil (ot->obs))
    ot->obs->insert (ot, pos, is_atomic (t)? N(t->label): N(t));
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_remove (tree& ot, int pos, int nr) {
  // cout << "Remove " << ot << " -= " << nr << " at " << pos << "\n";
  if (!nil (ot->obs)) ot->obs->remove (ot, pos, nr);
  if (is_atomic (ot)) ot->label= remove (ot->label, pos, nr);
  else {
    int i, n= N(ot)-nr;
    for (i=pos; i<n; i++)
      ot[i]= ot[i+nr];
    A(ot)->resize (n);
  }
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_split (tree& ot, int pos, int at) {
  // cout << "Split " << ot << " at " << pos << ", " << at << "\n";
  tree t;
  if (is_atomic (ot[pos])) {    
    t= ot[pos]->label (at, N(ot[pos]->label));
    ot[pos]->label->resize (at);
  }
  else {
    t= ot[pos] (at, N(ot[pos]));
    A(ot[pos])->resize (at);
  }
  int i, n= N(ot);
  A(ot)->resize (n+1);
  for (i=n; i>(pos+1); i--)
    ot[i]= ot[i-1];
  ot[pos+1]= t;
  if (!nil (ot->obs)) ot->obs->split (ot, pos);
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_join (tree& ot, int pos) {
  // cout << "Join " << ot << " at " << pos << "\n";
  /* the following code is added for security */
  if (is_atomic (ot[pos]) && (!is_atomic (ot[pos+1])))
    _ins_unary (ot[pos], L(ot[pos+1]));
  if (is_atomic (ot[pos+1]) && (!is_atomic (ot[pos])))
    _ins_unary (ot[pos+1], L(ot[pos]));
  /* end security code */

  if (!nil (ot->obs)) ot->obs->join (ot, pos);
  if (is_atomic (ot[pos]) && is_atomic (ot[pos+1]))
    ot[pos]->label << ot[pos+1]->label;
  else ot[pos] << A (ot[pos+1]);

  int i, n= N(ot)-1;
  for (i=pos+1; i<n; i++)
    ot[i]= ot[i+1];
  A(ot)->resize (n);
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_ins_unary (tree& ot, tree_label lab) {
  // cout << "Insert unary " << ot << " : " << as_string (lab) << "\n";
  ot= tree (lab, ot);
  if (!nil (ot[0]->obs)) ot[0]->obs->ins_unary (ot);
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

void
_rem_unary (tree& ot) {
  // cout << "Remove unary " << ot << "\n";
  if (!nil (ot->obs)) ot->obs->rem_unary (ot);
  ot= ot[0];
  // stretched_print (ot, true, 1);
  // consistency_check ();
}

/******************************************************************************
* Default virtual routines
******************************************************************************/

path
observer_rep::get_ip (tree& ot) {
  (void) ot;
  return DETACHED;
}

bool
observer_rep::set_ip (tree& ot, path ip) {
  (void) ot; (void) ip;
  return true;
}

/******************************************************************************
* This routine should go to list_observer.cpp later on
******************************************************************************/

observer
list_observer (observer o1, observer o2) {
  if (!nil (o2))
    fatal_error ("not yet implemented", "list_observer");
  return o1;
}
