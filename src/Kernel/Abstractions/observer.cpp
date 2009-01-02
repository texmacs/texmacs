
/******************************************************************************
* MODULE     : observer.cpp
* DESCRIPTION: Observers of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree.hpp"
#include "path.hpp"
#include "analyze.hpp"

#define DETACHED (-5)

observer nil_observer;

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
    cout << raw_quote (t->label);
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

ostream&
operator << (ostream& out, observer o) {
  out << "<observer";
  if (is_nil (o)) out << " null";
  else o->print (out);
  out << ">";
  return out;
}

/******************************************************************************
* Routines for modifying trees
*******************************************************************************
* 1) The "inserting modifications" (insert, split and insert_node) invoke
*    the observers call-back routines after the actual modification and
*    "assigning and deleting modifications" (assign, remove, join,
*    assign_node and remove_node)  before the actual modification.
* 2) The split and join modifications pass the joined tree
*    at position pos as an additional argument to the call-back routines.
* 3) They also admit variant call back routines for the split/join nodes.
******************************************************************************/

static void
simplify (observer& obs) {
  if (is_nil (obs)) return;
  observer& o1= obs->get_child (0);
  observer& o2= obs->get_child (1);  
  if (!is_nil (o1) || !is_nil (o2)) {
    simplify (o1);
    simplify (o2);
    obs= list_observer (o1, o2);
  }
}

static void
detach (tree& ref, tree closest, bool right) {
  if (!is_nil (ref->obs)) {
    ref->obs->notify_detach (ref, closest, right);
    simplify (ref->obs);
  }
  if (is_compound (ref)) {
    int i, n= N(ref);
    for (i=0; i<n; i++)
      detach (ref[i], closest, right);
  }
}

void
assign (tree& ref, tree t) {
  // cout << "Assign " << ref << " := " << t << "\n";
  if (!is_nil (ref->obs)) {
    ref->obs->announce_assign (ref, path (), t);
    ref->obs->notify_assign (ref, t);
    simplify (ref->obs);
  }
  if (is_compound (ref)) {
    int i, n= N(ref), mid= (n+1)>>1;
    for (i=0; i<n; i++)
      detach (ref[i], t, i >= mid);
  }
  ref= t;
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
insert (tree& ref, int pos, tree t) {
  // cout << "Insert " << ref << " += " << t << " at " << pos << "\n";
  if (!is_nil (ref->obs))
    ref->obs->announce_insert (ref, path (pos), t);
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
  if (!is_nil (ref->obs)) {
    ref->obs->notify_insert (ref, pos, is_atomic (t)? N(t->label): N(t));
    simplify (ref->obs);
  }
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
remove (tree& ref, int pos, int nr) {
  // cout << "Remove " << ref << " -= " << nr << " at " << pos << "\n";
  if (!is_nil (ref->obs)) {
    ref->obs->announce_remove (ref, path (pos), nr);
    ref->obs->notify_remove (ref, pos, nr);
    simplify (ref->obs);
  }
  if (is_compound (ref)) {
    int i, n= N(ref), end= pos+nr, mid= (pos+end+1) >> 1;
    for (i=pos; i<mid; i++)
      if (pos == 0) detach (ref[i], ref, false);
      else detach (ref[i], ref[pos-1], true);
    for (; i<end; i++)
      if (end == n) detach (ref[i], ref, true);
      else detach (ref[i], ref[pos+nr], false);
  }

  if (is_atomic (ref))
    ref->label= ref->label (0, pos) * ref->label (pos+nr, N(ref->label));
  else {
    int i, n= N(ref)-nr;
    for (i=pos; i<n; i++)
      ref[i]= ref[i+nr];
    AR(ref)->resize (n);
  }
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
split (tree& ref, int pos, int at) {
  // cout << "Split " << ref << " at " << pos << ", " << at << "\n";
  if (!is_nil (ref->obs))
    ref->obs->announce_split (ref, path (pos, at));
  tree t= ref[pos], t1, t2;
  if (is_atomic (ref[pos])) {    
    t1= ref[pos]->label (0, at);
    t2= ref[pos]->label (at, N(ref[pos]->label));
  }
  else {
    t1= ref[pos] (0, at);
    t2= ref[pos] (at, N(ref[pos]));
  }
  int i, n= N(ref);
  AR(ref)->resize (n+1);
  for (i=n; i>(pos+1); i--)
    ref[i]= ref[i-1];
  ref[pos  ]= t1;
  ref[pos+1]= t2;

  if (!is_nil (ref->obs)) {
    ref->obs->notify_split (ref, pos, t);
    simplify (ref->obs);
  }
  if (!is_nil (t->obs)) {
    t->obs->notify_var_split (t, t1, t2);
    simplify (t->obs);
  }
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
join (tree& ref, int pos) {
  // cout << "Join " << ref << " at " << pos << "\n";
  // the following code is added for security
  if (is_atomic (ref[pos]) && (!is_atomic (ref[pos+1])))
    insert_node (ref[pos], 0, tree (L(ref[pos+1])));
  if (is_atomic (ref[pos+1]) && (!is_atomic (ref[pos])))
    insert_node (ref[pos+1], 0, tree (L(ref[pos])));
  // end security code

  if (!is_nil (ref->obs))
    ref->obs->announce_join (ref, path (pos));
  tree t1= ref[pos], t2= ref[pos+1], t;
  int offset= is_atomic (ref)? N(t1->label): N(t1);
  if (is_atomic (t1) && is_atomic (t2)) t= t1->label * t2->label;
  else t= t1 * t2;
  if (!is_nil (ref->obs)) ref->obs->notify_join (ref, pos, t);
  if (!is_nil (t1->obs)) {
    t1->obs->notify_var_join (t1, t, 0);
    simplify (t1->obs);
  }
  if (!is_nil (t2->obs)) {
    t2->obs->notify_var_join (t2, t, offset);
    simplify (t2->obs);
  }
  ref[pos]= t;

  int i, n= N(ref)-1;
  for (i=pos+1; i<n; i++)
    ref[i]= ref[i+1];
  AR(ref)->resize (n);
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
assign_node (tree& ref, tree_label op) {
  // cout << "Assign node " << ref << " : " << tree (op) << "\n";
  if (!is_nil (ref->obs)) {
    ref->obs->announce_assign_node (ref, path (), op);
    ref->obs->notify_assign_node (ref, op);
    simplify (ref->obs);
  }
  LR (ref)= op;
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
insert_node (tree& ref, int pos, tree t) {
  // cout << "Insert node " << ref << " : " << t << " at " << pos << "\n";
  if (!is_nil (ref->obs))
    ref->obs->announce_insert_node (ref, path (pos), t);
  int i, n= N(t);
  tree r (t, n+1);
  for (i=0; i<pos; i++) r[i]= t[i];
  r[pos]= ref;
  for (i=pos; i<n; i++) r[i+1]= t[i];
  ref= r;
  if (!is_nil (ref[pos]->obs)) {
    ref[pos]->obs->notify_insert_node (ref, pos);
    simplify (ref[pos]->obs);
  }
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

void
remove_node (tree& ref, int pos) {
  // cout << "Remove node " << ref << " : " << pos << "\n";
  if (!is_nil (ref->obs)) {
    ref->obs->announce_remove_node (ref, path (pos));
    ref->obs->notify_remove_node (ref, pos);
    simplify (ref->obs);
  }
  for (int i=0; i<N(ref); i++)
    if (i < pos) detach (ref[i], ref[pos], false);
    else if (i > pos) detach (ref[i], ref[pos], true);
  ref= ref[pos];
  if (!is_nil (ref->obs))
    ref->obs->announce_done (ref, path ());
  // stretched_print (ref, true, 1);
  // consistency_check ();
}

/******************************************************************************
* Default virtual routines
******************************************************************************/

void
observer_rep::announce_assign (tree& ref, path p, tree t) {
  (void) ref; (void) p; (void) t;
}

void
observer_rep::announce_insert (tree& ref, path p, tree ins) {
  (void) ref; (void) p; (void) ins;
}

void
observer_rep::announce_remove (tree& ref, path p, int nr) {
  (void) ref; (void) p; (void) nr;
}

void
observer_rep::announce_split (tree& ref, path p) {
  (void) ref; (void) p;
}

void
observer_rep::announce_join (tree& ref, path p) {
  (void) ref; (void) p;
}

void
observer_rep::announce_assign_node (tree& ref, path p, tree_label op) {
  (void) ref; (void) p; (void) op;
}

void
observer_rep::announce_insert_node (tree& ref, path p, tree ins) {
  (void) ref; (void) p; (void) ins;
}

void
observer_rep::announce_remove_node (tree& ref, path p) {
  (void) ref; (void) p;
}

void
observer_rep::announce_done (tree& ref, path p) {
  (void) ref; (void) p;
}

void
observer_rep::notify_assign (tree& ref, tree t) {
  (void) ref; (void) t;
}

void
observer_rep::notify_insert (tree& ref, int pos, int nr) {
  (void) ref; (void) pos; (void) nr;
}

void
observer_rep::notify_remove (tree& ref, int pos, int nr) {
  (void) ref; (void) pos; (void) nr;
}

void
observer_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref; (void) pos; (void) prev;
}

void
observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  (void) ref; (void) t1; (void) t2;
}

void
observer_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref; (void) pos; (void) next;
}

void
observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  (void) ref; (void) t; (void) offset;
}

void
observer_rep::notify_assign_node (tree& ref, tree_label op) {
  (void) ref; (void) op;
}

void
observer_rep::notify_insert_node (tree& ref, int pos) {
  (void) ref; (void) pos;
}

void
observer_rep::notify_remove_node (tree& ref, int pos) {
  (void) ref; (void) pos;
}

void
observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  (void) ref; (void) closest; (void) right;
}

bool
observer_rep::get_ip (path& ip) {
  (void) ip;
  return false;
}

bool
observer_rep::set_ip (path ip) {
  (void) ip;
  return false;
}

bool
observer_rep::get_position (tree& t, int& index) {
  (void) t; (void) index;
  return false;
}

bool
observer_rep::set_position (tree t, int index) {
  (void) t; (void) index;
  return false;
}

observer&
observer_rep::get_child (int which) {
  (void) which;
  return nil_observer;
}

list<observer>
observer_rep::get_tree_pointers () {
  return list<observer> ();
}

bool
observer_rep::get_tree (tree& t) {
  (void) t;
  return false;
}
