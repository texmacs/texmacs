
/******************************************************************************
* MODULE     : scroll_event.hpp
* DESCRIPTION: Events for scrollbars and scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SCROLL_EVENT_H
#define SCROLL_EVENT_H
#include "event.hpp"
#include "Event/event_codes.hpp"

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
event set_hor_bar (widget bar);
event set_ver_bar (widget bar);
event emit_bar_set_extents (SI min, SI max);
event emit_bar_scroll_to (SI pos);
event emit_scroll (SI& c1, SI& c2, SI& c3);
event emit_hor_scroll (SI& c1, SI& c2, SI& c3);
event emit_ver_scroll (SI& c1, SI& c2, SI& c3);

#endif // defined SCROLL_EVENT_H
