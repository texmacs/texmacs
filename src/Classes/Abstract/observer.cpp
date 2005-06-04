
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
  if (obtain_ip (t) != ip)
    cout << "Wrong ip] " << t << " " << obtain_ip (t)
	 << " instead of " << ip << "\n";
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      //if (!strong_equal (ip, obtain_ip (t[i])->next))
      if (obtain_ip (t) != obtain_ip (t[i])->next)
	cout << "Bad node] " << t << " " << obtain_ip (t) << " #" << i << "\n";
      consistency_check (t[i], path (i, ip));
    }
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
    if (ips) cout << " -- " << obtain_ip (t);
    cout << "\n";
  }
  else {
    cout << as_string (L(t));
    if (ips) cout << " -- " << obtain_ip (t);
    cout << "\n";    
    for (i=0; i<N(t); i++)
      stretched_print (t[i], ips, indent+1);
  }
}

/******************************************************************************
* Routines for modifying trees
*******************************************************************************
* Notice that "inserting modifications" (insert, split and insert_node)
* invoke the observers call-back routines after the actual modification and
* "assigning and deleting modifications" (assign, remove, join,
* assign_node and remove_node)  before the actual modification.
******************************************************************************/

void
assign (tree& ref, tree t) {
  // cout << "Assign " << ref << " := " << t << "\n";
  if (!nil (ref->obs)) ref->obs->notify_assign (ref, t);
  ref= t;
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
insert (tree& ref, int pos, tree t) {
  // cout << "Insert " << ref << " += " << t << " at " << pos << "\n";
  if (is_atomic (ref) && is_atomic (t))
    ref->label= ref->label (0, pos) *t->label* ref->label (pos, N(ref->label));
  else {
    int i, n= N(ref), nr= N(t);
    AR(ref)->resize (n+nr);
    for (i=n-1; i>=pos; i--)
      ref[i+nr]= ref[i];
    for (i=0; i<nr; i++)
      ref[pos+i]= t[i];
  }
  if (!nil (ref->obs))
    ref->obs->notify_insert (ref, pos, is_atomic (t)? N(t->label): N(t));
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
remove (tree& ref, int pos, int nr) {
  // cout << "Remove " << ref << " -= " << nr << " at " << pos << "\n";
  if (!nil (ref->obs)) ref->obs->notify_remove (ref, pos, nr);
  if (is_atomic (ref))
    ref->label= ref->label (0, pos) * ref->label (pos+nr, N(ref->label));
  else {
    int i, n= N(ref)-nr;
    for (i=pos; i<n; i++)
      ref[i]= ref[i+nr];
    AR(ref)->resize (n);
  }
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
split (tree& ref, int pos, int at) {
  // cout << "Split " << ref << " at " << pos << ", " << at << "\n";
  tree t;
  if (is_atomic (ref[pos])) {    
    t= ref[pos]->label (at, N(ref[pos]->label));
    ref[pos]->label->resize (at);
  }
  else {
    t= ref[pos] (at, N(ref[pos]));
    AR(ref[pos])->resize (at);
  }
  int i, n= N(ref);
  AR(ref)->resize (n+1);
  for (i=n; i>(pos+1); i--)
    ref[i]= ref[i-1];
  ref[pos+1]= t;
  if (!nil (ref->obs)) ref->obs->notify_split (ref, pos);
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
join (tree& ref, int pos) {
  // cout << "Join " << ref << " at " << pos << "\n";
  /* the following code is added for security */
  if (is_atomic (ref[pos]) && (!is_atomic (ref[pos+1])))
    insert_node (ref[pos], 0, tree (L(ref[pos+1])));
  if (is_atomic (ref[pos+1]) && (!is_atomic (ref[pos])))
    insert_node (ref[pos+1], 0, tree (L(ref[pos])));
  /* end security code */

  if (!nil (ref->obs)) ref->obs->notify_join (ref, pos);
  if (is_atomic (ref[pos]) && is_atomic (ref[pos+1]))
    ref[pos]->label << ref[pos+1]->label;
  else ref[pos] << A(ref[pos+1]);

  int i, n= N(ref)-1;
  for (i=pos+1; i<n; i++)
    ref[i]= ref[i+1];
  AR(ref)->resize (n);
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
insert_node (tree& ref, int pos, tree t) {
  // cout << "Insert node " << ref << " : " << t << " at " << pos << "\n";
  int i, n= N(t);
  tree r (t, n+1);
  for (i=0; i<pos; i++) r[i]= t[i];
  r[pos]= ref;
  for (i=pos; i<n; i++) r[i+1]= t[i];
  ref= r;
  if (!nil (ref[pos]->obs)) ref[pos]->obs->notify_insert_node (ref, pos);
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
remove_node (tree& ref, int pos) {
  // cout << "Remove node " << ref << " : " << pos << "\n";
  if (!nil (ref->obs)) ref->obs->notify_remove_node (ref, pos);
  ref= ref[pos];
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
assign_node (tree& ref, tree_label op) {
  // cout << "Assign node " << ref << " : " << tree (op) << "\n";
  if (!nil (ref->obs)) ref->obs->notify_assign_node (ref, op);
  LR (ref)= op;
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

/******************************************************************************
* Default virtual routines
******************************************************************************/

path
observer_rep::get_ip (tree& ref) {
  (void) ref;
  return DETACHED;
}

bool
observer_rep::set_ip (tree& ref, path ip) {
  (void) ref; (void) ip;
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
