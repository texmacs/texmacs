
/******************************************************************************
* MODULE     : composite_event.hpp
* DESCRIPTION: Events for modification of composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef COMPOSITE_EVENT_H
#define COMPOSITE_EVENT_H
#include "Widkit/event.hpp"
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/event_codes.hpp"

/******************************************************************************
* Events for composite widgets
******************************************************************************/

struct clean_event_rep: public event_rep {
  clean_event_rep ();
  operator tree ();
};
EVENT(clean_event);

struct insert_event_rep: public event_rep {
  string s; wk_widget w;
  insert_event_rep (string s, wk_widget w);
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
