
/******************************************************************************
* MODULE     : composite_event.hpp
* DESCRIPTION: Events for modification of composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef COMPOSITE_EVENT_H
#define COMPOSITE_EVENT_H
#include "event.hpp"
#include "Event/event_codes.hpp"
#include "widget.hpp"

/******************************************************************************
* Events for composite widgets
******************************************************************************/

struct clean_event_rep: public event_rep {
  clean_event_rep ();
  operator tree ();
};
EVENT(clean_event);

struct insert_event_rep: public event_rep {
  string s; widget w;
  insert_event_rep (string s, widget w);
  operator tree ();
};
EVENT(insert_event);

struct remove_event_rep: public event_rep {
  string s;
  remove_event_rep (string s);
  operator tree ();
};
EVENT(remove_event);

#endif // defined COMPOSITE_EVENT_H
