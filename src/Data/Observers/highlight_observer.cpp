
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
  array<int> cols;
public:
  highlight_observer_rep (array<int> cols2): cols (cols2) {}
  int get_type () { return OBSERVER_HIGHLIGHT; }
  tm_ostream& print (tm_ostream& out) { return out << " highlight"; }

  void announce (tree& ref, modification mod);
  bool set_highlight (int col, int start, int end);
  array<int> get_highlight ();
  void reset_highlight (tree& ref);
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
highlight_observer_rep::set_highlight (int col, int start, int end) {
  ASSERT (0 <= start && start <= end && end <= N(cols), "out of range");
  for (int i=start; i<end; i++) cols[i]= col;
  return true;
}

array<int>
highlight_observer_rep::get_highlight () {
  return cols;
}

void
highlight_observer_rep::reset_highlight (tree& ref) {
  remove_observer (ref->obs, observer (this));
}

/******************************************************************************
* Attaching and detaching highlighters
******************************************************************************/

observer
highlight_observer (array<int> cols) {
  return tm_new<highlight_observer_rep> (cols);
}

void
attach_highlight (tree& ref, int col, int start, int end) {
  ASSERT (is_atomic (ref), "compound trees cannot be highlighted");
  ASSERT (0 <= start && start <= end && end <= N(ref->label), "out of range");
  if (is_nil (ref->obs) || !ref->obs->set_highlight (col, start, end)) {
    array<int> cols (N(ref->label));
    for (int i=0; i<N(ref->label); i++) cols[i]= 0;
    for (int i=start; i<end; i++) cols[i]= col;
    attach_observer (ref, highlight_observer (cols));
  }
}

array<int>
obtain_highlight (tree& ref) {
  if (is_nil (ref->obs)) return array<int> ();
  return ref->obs->get_highlight ();
}

void
detach_highlight (tree& ref) {
  if (is_compound (ref))
    for (int i=0; i<N(ref); i++)
      detach_highlight (ref[i]);
  else if (!is_nil (ref->obs))
    ref->obs->reset_highlight (ref);
}
