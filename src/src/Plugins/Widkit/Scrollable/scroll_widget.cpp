
/******************************************************************************
* MODULE     : scroll_widget.cpp
* DESCRIPTION: Common base for scrollbars and scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/scroll_widget.hpp"
#include "Widkit/Event/scroll_event.hpp"

/******************************************************************************
* Scroll widgets
******************************************************************************/

scroll_widget_rep::scroll_widget_rep (array<wk_widget> a2,
  gravity grav2):
  attribute_widget_rep (a2, grav2) {}

bool
scroll_widget_rep::handle (event ev) {
  if (ev->type != SCROLL_EVENT) return attribute_widget_rep::handle (ev);
  scroll_event e (ev);
  handle_scroll (e);
  return true;
}
