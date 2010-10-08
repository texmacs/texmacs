
/******************************************************************************
* MODULE     : highlight_observer.cpp
* DESCRIPTION: Attach highlighting information to trees
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modification.hpp"

/******************************************************************************
* Definition of the highlight_observer_rep class
******************************************************************************/

class highlight_observer_rep: public observer_rep {
  int lan;
  array<int> cols;
public:
  highlight_observer_rep (int lan2, array<int> cols2):
    lan (lan2), cols (cols2) {}
  int get_type () { return OBSERVER_HIGHLIGHT; }
  tm_ostream& print (tm_ostream& out) {
    return out << " highlight<" << lan << ">"; }

  void announce (tree& ref, modification mod);
  bool set_highlight (int lan, int col, int start, int end);
  bool get_highlight (int lan, array<int>& cols);
};

/******************************************************************************
* Call back routines and highlighting methods
******************************************************************************/

void
highlight_observer_rep::announce (tree& ref, modification mod) {
  (void) mod;
  remove_observer (ref->obs, observer (this));
}

bool
highlight_observer_rep::set_highlight (int l, int col, int start, int end) {
  if (l != lan) return false;
  ASSERT (0 <= start && start <= end && end <= N(cols), "out of range");
  for (int i=start; i<end; i++) cols[i]= col;
  return true;
}


bool
highlight_observer_rep::get_highlight (int l, array<int>& c) {
  if (l != lan) return false;
  c= cols;
  return true;
}

/******************************************************************************
* Attaching and detaching highlighters
******************************************************************************/

observer
highlight_observer (int lan, array<int> cols) {
  return tm_new<highlight_observer_rep> (lan, cols);
}

void
remove_highlight (observer& obs, int lan) {
  if (is_nil (obs)) return;
  else if (obs->get_type () == OBSERVER_HIGHLIGHT) {
    array<int> r;
    if (obs->get_highlight (lan, r))
      obs= nil_observer;
  }
  else {
    remove_highlight (obs->get_child (0), lan);
    remove_highlight (obs->get_child (1), lan);
  }
}

void
attach_highlight (tree& ref, int lan) {
  //cout << "Attach highlight " << ref << "\n";
  if (!has_highlight (ref, lan))
    attach_highlight (ref, lan, 0, 0, 0);
  if (is_compound (ref))
    for (int i=0; i<N(ref); i++)
      attach_highlight (ref[i], lan);
}

void
attach_highlight (tree& ref, int lan, int col, int start, int end) {
  int n= (is_atomic (ref)? N(ref->label): 1);
  ASSERT (0 <= start && start <= end && end <= n, "out of range");
  if (is_nil (ref->obs) || !ref->obs->set_highlight (lan, col, start, end)) {
    array<int> cols (n);
    for (int i=0; n>i; i++) cols[i]= 0;
    for (int i=start; i<end; i++) cols[i]= col;
    attach_observer (ref, highlight_observer (lan, cols));
  }
}

bool
has_highlight (tree& ref, int lan) {
  if (lan == 0 || is_nil (ref->obs)) return false;
  array<int> cols;
  return ref->obs->get_highlight (lan, cols);
}

array<int>
obtain_highlight (tree& ref, int lan) {
  if (lan == 0 || is_nil (ref->obs)) return array<int> ();
  array<int> cols;
  if (ref->obs->get_highlight (lan, cols)) return cols;
  return array<int> ();
}

bool
detach_highlight_sub (tree& ref, int lan) {
  //cout << "Detach highlight " << ref << "\n";
  array<int> cols;
  bool r= !is_nil (ref->obs) && ref->obs->get_highlight (lan, cols);
  remove_highlight (ref->obs, lan);
  if (is_compound (ref))
    for (int i=0; i<N(ref); i++)
      r= detach_highlight_sub (ref[i], lan) | r;
  return r;
}

void
detach_highlight (tree& ref, int lan) {
  if (detach_highlight_sub (ref, lan))
    touch (ref);
}
