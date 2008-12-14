
/******************************************************************************
* MODULE     : scroll_event.hpp
* DESCRIPTION: Events for scrollbars and scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SCROLL_EVENT_H
#define SCROLL_EVENT_H
#include "Widkit/event.hpp"
#include "Widkit/Event/event_codes.hpp"

/******************************************************************************
* The scroll event
******************************************************************************/

struct scroll_event_rep: public event_rep {
  string which; SI& c1; SI& c2; SI& c3;
  scroll_event_rep (string which, SI& c1, SI& c2, SI& c3);
  operator tree ();
};
EVENT(scroll_event);

/******************************************************************************
* Other events for internal use
******************************************************************************/

event get_width (SI& w);
event get_height (SI& h);
event set_hor_bar (wk_widget bar);
event set_ver_bar (wk_widget bar);
event emit_bar_set_extents (SI min, SI max);
event emit_bar_scroll_to (SI pos);
event emit_scroll (SI& c1, SI& c2, SI& c3);
event emit_hor_scroll (SI& c1, SI& c2, SI& c3);
event emit_ver_scroll (SI& c1, SI& c2, SI& c3);

#endif // defined SCROLL_EVENT_H
