
/******************************************************************************
* MODULE     : event.hpp
* DESCRIPTION: Abstract events
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EVENT_H
#define EVENT_H
#include "tree.hpp"
#include "command.hpp"
#include "timer.hpp"

class window_rep;
typedef window_rep* window;
class display_rep;
typedef display_rep* display;
display current_display ();
typedef int color;

class widget;
enum gravity { north_west, north,  north_east,
	       west,       center, east,
	       south_west, south,  south_east };

/******************************************************************************
* The event class
******************************************************************************/

#define __EVENT_CLASS_MASK 0xffff0000

extern int event_count;
struct event_rep: public abstract_struct {
  int     type;  // the event type
  inline  event_rep (int type2): type (type2) { DEBUG(event_count++); }
  inline  virtual ~event_rep () { DEBUG(event_count--); }
  virtual operator tree () = 0;   // for displaying events (debugging)
};

struct event {
  ABSTRACT(event);            
  inline operator tree () { return (tree) (*rep); }
  friend class event_ptr_base;
};
ABSTRACT_CODE(event);

inline ostream& operator << (ostream& out, event ev) {
  return out << ((tree) ev); }

/******************************************************************************
* User interface
******************************************************************************/

/*** basic events ***/
event get_size (SI& w, SI& h, int mode=0);
event get_widget (string which, widget& w);
event set_widget (string which, widget w);
event set_language (string which, bool& b);
event emit_attach_window (window win);
event emit_reposition ();
event emit_position (SI ox, SI oy, SI w, SI h, gravity grav=north_west);
event emit_move ();
event emit_resize ();
event emit_destroy ();
event emit_keypress (string key, time_t t);
event emit_keyboard_focus (bool in_out_flag, time_t t=0);
event emit_mouse (string type, SI x, SI y, time_t t, int status);
event emit_alarm (string message, time_t t);
event emit_clear (SI x1, SI y1, SI x2, SI y2);
event emit_repaint (SI x1, SI y1, SI x2, SI y2, bool& stop);
event emit_update ();
event emit_invalidate_all ();
event emit_invalidate (SI x1, SI y1, SI x2, SI y2);
event emit_keyboard_grab (bool in_out_flag, time_t t=0);
event emit_mouse_grab (bool in_out_flag, time_t t=0);
event emit_request_alarm (event ev, time_t delay);
event emit_find_child (SI x, SI y, int& which);

/*** composite events ***/
event emit_clean ();
event emit_insert (string where, widget wid);
event emit_remove (string where);

/*** attribute events ***/
event get_integer (string which, int& i);
event get_double  (string which, double& d);
event get_string  (string which, string& s);
event get_coord1  (string which, SI& c1);
event get_coord2  (string which, SI& c1, SI& c2);
event get_coord3  (string which, SI& c1, SI& c2, SI& c3);
event get_coord4  (string which, SI& c1, SI& c2, SI& c3, SI& c4);
event set_integer (string which, int i);
event set_double  (string which, double d);
event set_string  (string which, string s);
event set_coord1  (string which, SI c1);
event set_coord2  (string which, SI c1, SI c2);
event set_coord3  (string which, SI c1, SI c2, SI c3);
event set_coord4  (string which, SI c1, SI c2, SI c3, SI c4);

/*** scroll events ***/
event set_scrollable (widget w);
event set_extents    (SI x1, SI y1, SI x2, SI y2);
event set_scroll_pos (SI x, SI y);
event get_extents    (SI& x1, SI& y1, SI& x2, SI& y2);
event get_visible    (SI& x1, SI& y1, SI& x2, SI& y2);

/*** user input events ***/
event set_input_string (string s);
event get_input_string (string& s);

#endif // defined EVENT_H
