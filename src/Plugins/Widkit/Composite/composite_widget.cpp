
/******************************************************************************
* MODULE     : composite_widget.cpp
* DESCRIPTION: abstract composite widgets accept events for
*              modifying the composite structure of a widget.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/layout.hpp"
#include "Widkit/composite_widget.hpp"

/******************************************************************************
* Routines for abstract composite widgets
******************************************************************************/

composite_widget_rep::composite_widget_rep (gravity grav):
  basic_widget_rep (grav) {}
composite_widget_rep::composite_widget_rep (
  array<wk_widget> a, gravity grav):
    basic_widget_rep (a, grav) {}
composite_widget_rep::composite_widget_rep (
  array<wk_widget> a, array<string> name, gravity grav):
    basic_widget_rep (a, name, grav) {}

void
composite_widget_rep::handle_clean (clean_event ev) { (void) ev;
  a->resize (0);
  name->resize (0);
}

void
composite_widget_rep::handle_insert (insert_event ev) {
  a << ev->w;
  name << ev->s;
}

void
composite_widget_rep::handle_remove (remove_event ev) {
  int i, j, n= N(a);
  for (i=0; i<n; i++)
    if (name[i] == ev->s) {
      for (j=i; j<n-1; j++) {
	a[j]= a[j+1];
	name[j]= name[j+1];
      }
      a->resize (n-1);
      name->resize (n-1);
      return;
    }
}

bool
composite_widget_rep::handle (event ev) {
  switch (ev->type) {
  case CLEAN_EVENT:
    handle_clean (ev);
    return true;
  case INSERT_EVENT:
    handle_insert (ev);
    return true;
  case REMOVE_EVENT:
    handle_remove (ev);
    return true;
  }
  return basic_widget_rep::handle (ev);
}
