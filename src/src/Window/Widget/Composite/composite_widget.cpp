
/******************************************************************************
* MODULE     : composite_widget.cpp
* DESCRIPTION: abstract composite widgets accept events for
*              modifying the composite structure of a widget.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/layout.hpp"
#include "Widget/composite_widget.hpp"

/******************************************************************************
* Routines for abstract composite widgets
******************************************************************************/

composite_widget_rep::composite_widget_rep (display dis, gravity grav):
  basic_widget_rep (dis, grav) {}
composite_widget_rep::composite_widget_rep (display dis, array<widget> a,
  gravity grav):
  basic_widget_rep (dis, a, grav) {}
composite_widget_rep::composite_widget_rep (display dis, array<widget> a,
  array<string> name, gravity grav):
  basic_widget_rep (dis, a, name, grav) {}

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
