
/******************************************************************************
* MODULE     : popup_widget.cpp
* DESCRIPTION: These widgets are decorated as popup windows.
*              A pointer grab is assumed to take place when they popup.
*              They disappear (=>ungrab) when the mouse leaves the window
*              at the 'quit' direction.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/basic_widget.hpp"
#include "Event/attribute_event.hpp"
#include "ps_device.hpp"
#include "Widget/layout.hpp"

/******************************************************************************
* Popup widgets
******************************************************************************/

class popup_widget_rep: public basic_widget_rep {
  gravity quit;
  bool    grabbed;
  bool    stick;
  bool    freeze;

public:
  popup_widget_rep (widget w, gravity quit);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
  void handle_set_integer (set_integer_event ev);
  bool handle (event ev);
};

/******************************************************************************
* Routines for popup widgets
******************************************************************************/

popup_widget_rep::popup_widget_rep (widget w, gravity quit2):
  basic_widget_rep (w->dis, 1), quit (quit2),
  grabbed (false), stick (false), freeze (false)
{
  a[0]= w;
}

popup_widget_rep::operator tree () {
  return tree (TUPLE, "popup", (tree) a[0]);
}

void
popup_widget_rep::handle_get_size (get_size_event ev) {
  a[0] << ev;
  ev->w += 6*PIXEL;
  ev->h += 6*PIXEL;
}

void
popup_widget_rep::handle_position (position_event ev) {
  (void) ev;
  a[0] << emit_position (3*PIXEL, -3*PIXEL, w- 6*PIXEL, h- 6*PIXEL);
}

void
popup_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  layout_higher (win, 0, -h, w, 0);
  layout_default (win, PIXEL, PIXEL-h,
		  w-PIXEL, -PIXEL);
  layout_default (win, 2*PIXEL, 2*PIXEL-h,
		  w-2*PIXEL, -2*PIXEL);
}

/*
extern string pritty (tree t); // from x_display.cpp
*/

void
popup_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;

  bool leaving=
    ((quit == north_west) && (y>0)) ||
    ((quit == north) && (y>0)) ||
    ((quit == west) && (x<0));
  bool pressed=
    ev->pressed ("left") || ev->pressed ("right");

  /*
  if (type != "move") {
    cout << "-> " << (grabbed? "grabbed": "non grabbed")
	 << ", "  << (stick? "sticked": "not sticked")
	 << ", "  << (freeze? "frozen": "not frozen")
	 << ", "  << (pressed? "pressed": "non pressed") << "\n";
    cout << "   " << ((event) ev) << "\n";
    cout << "   " << pritty ((widget) this) << "\n";
    cout << "   " << ((int) ((void*) this)) << "\n";
  }
  */

  if (freeze) { freeze= pressed; stick= !pressed; }
  stick = stick && (!pressed);
  if (grabbed) a[0] << emit_mouse (ev);
  if ((type != "leave") && (!stick) && (!freeze)) {
    if (dis->has_grab_pointer (this)) {
      grabbed= pressed && (!leaving);
      if (!grabbed) this << emit_mouse_grab (false);
    }
  }

  /*
  if (type != "move") {
    cout << "=> " << (grabbed? "grabbed": "non grabbed")
	 << ", "  << (stick? "sticked": "not sticked")
	 << ", "  << (freeze? "frozen": "not frozen")
	 << ", "  << (pressed? "pressed": "non pressed") << "\n";
    cout << "   " << ((event) ev) << "\n";
    cout << "   " << pritty ((widget) this) << "\n";
    cout << "   " << ((int) ((void*) this)) << "\n";
  }
  */
}

void
popup_widget_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "grabbed") { grabbed= (ev->i != 0); stick= freeze= false; }
  else if (ev->which == "stick") { stick= (ev->i != 0); }
  else if (ev->which == "freeze") { freeze= (ev->i != 0); }
  else fatal_error ("Could not set integer attribute " * ev->which);
}

bool
popup_widget_rep::handle (event ev) {
  switch (ev->type) {
  case GET_SIZE_EVENT:
  case ATTACH_WINDOW_EVENT:
  case POSITION_EVENT:
  case UPDATE_EVENT:
  case INVALIDATE_EVENT:
  case MOUSE_EVENT:
  case MOUSE_GRAB_EVENT:
  case REPAINT_EVENT:
    return basic_widget_rep::handle (ev);
  case SET_INTEGER_EVENT:
    handle_set_integer (ev);
    return true;
  default:
    a[0] << ev;
    return true;
  }
}

/******************************************************************************
* Interface
******************************************************************************/

widget
popup_widget (widget w, gravity quit) {
  return new popup_widget_rep (w, quit);
}
