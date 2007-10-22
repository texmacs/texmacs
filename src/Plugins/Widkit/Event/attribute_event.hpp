
/******************************************************************************
* MODULE     : attribute_event.hpp
* DESCRIPTION: Events for setting and retrieving attributes of a widget.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ATTRIBUTE_EVENT_H
#define ATTRIBUTE_EVENT_H
#include "Widkit/event.hpp"
#include "Widkit/Event/event_codes.hpp"

/******************************************************************************
* Events for retrieving attributes
******************************************************************************/

struct get_integer_event_rep: public event_rep {
  string which; int& i;
  get_integer_event_rep (string which, int& i);
  operator tree ();
};
EVENT(get_integer_event);

struct get_double_event_rep: public event_rep {
  string which; double& x;
  get_double_event_rep (string which, double& x);
  operator tree ();
};
EVENT(get_double_event);

struct get_string_event_rep: public event_rep {
  string which; string& s;
  get_string_event_rep (string which, string& s);
  operator tree ();
};
EVENT(get_string_event);

struct get_command_event_rep: public event_rep {
  string which; command& cmd;
  get_command_event_rep (string which, command& cmd);
  operator tree ();
};
EVENT(get_command_event);

struct get_coord1_event_rep: public event_rep {
  string which; SI& c1;
  get_coord1_event_rep (string which, SI& c1);
  operator tree ();
};
EVENT(get_coord1_event);

struct get_coord2_event_rep: public event_rep {
  string which; SI& c1; SI& c2;
  get_coord2_event_rep (string which, SI& c1, SI& c2);
  operator tree ();
};
EVENT(get_coord2_event);

struct get_coord3_event_rep: public event_rep {
  string which; SI& c1; SI& c2; SI& c3;
  get_coord3_event_rep (string which, SI& c1, SI& c2, SI& c3);
  operator tree ();
};
EVENT(get_coord3_event);

struct get_coord4_event_rep: public event_rep {
  string which; SI& c1; SI& c2; SI& c3; SI& c4;
  get_coord4_event_rep (string which, SI& c1, SI& c2, SI& c3, SI& c4);
  operator tree ();
};
EVENT(get_coord4_event);

/******************************************************************************
* Events for setting attributes
******************************************************************************/

struct set_integer_event_rep: public event_rep {
  string which; int i;
  set_integer_event_rep (string which, int i);
  operator tree ();
};
EVENT(set_integer_event);

struct set_double_event_rep: public event_rep {
  string which; double x;
  set_double_event_rep (string which, double x);
  operator tree ();
};
EVENT(set_double_event);

struct set_string_event_rep: public event_rep {
  string which; string s;
  set_string_event_rep (string which, string s);
  operator tree ();
};
EVENT(set_string_event);

struct set_command_event_rep: public event_rep {
  string which; command cmd;
  set_command_event_rep (string which, command cmd);
  operator tree ();
};
EVENT(set_command_event);

struct set_coord1_event_rep: public event_rep {
  string which; SI c1;
  set_coord1_event_rep (string which, SI c1);
  operator tree ();
};
EVENT(set_coord1_event);

struct set_coord2_event_rep: public event_rep {
  string which; SI c1; SI c2;
  set_coord2_event_rep (string which, SI c1, SI c2);
  operator tree ();
};
EVENT(set_coord2_event);

struct set_coord3_event_rep: public event_rep {
  string which; SI c1; SI c2; SI c3;
  set_coord3_event_rep (string which, SI c1, SI c2, SI c3);
  operator tree ();
};
EVENT(set_coord3_event);

struct set_coord4_event_rep: public event_rep {
  string which; SI c1; SI c2; SI c3; SI c4;
  set_coord4_event_rep (string which, SI c1, SI c2, SI c3, SI c4);
  operator tree ();
};
EVENT(set_coord4_event);

#endif // defined ATTRIBUTE_EVENT_H
