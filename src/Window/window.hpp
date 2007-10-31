
/******************************************************************************
* MODULE     : window.hpp
* DESCRIPTION: Abstract window class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef WINDOW_H
#define WINDOW_H
#include "gui.hpp"
#include "renderer.hpp"

class widget;
class window_rep;
typedef window_rep* window;

class window_rep: virtual public renderer_rep {
public:
  inline window_rep () {}
  inline virtual ~window_rep () {}

  virtual widget get_widget () = 0;
    // get the top widget associated to the window
  virtual void set_name (string name) = 0;
    // set the window title
  virtual void set_visibility (bool flag) = 0;
    // map or unmap the window
  virtual void set_full_screen (bool flag) = 0;
    // set or reset full screen mode
  virtual void set_size (SI w, SI h) = 0;
    // resize the window
  virtual void get_size (SI& w, SI& h) = 0;
    // get the current size of the window
  virtual void set_position (SI x, SI y) = 0;
    // move the window
  virtual void get_position (SI& x, SI& y) = 0;
    // get the current position of the window on the screen
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2) = 0;
    // explicit request for redrawing a region in the window
  virtual void translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) = 0;
    // fast translation of a region in the window (used for scrolling)
  virtual void set_keyboard_focus (widget wid, bool get_focus= true) = 0;
    // obtain or release the keyboard focus
  virtual void set_mouse_grab (widget wid, bool get_grab) = 0;
    // obtain or release the mouse grab
  virtual bool get_mouse_grab (widget w) = 0;
    // does this widget have the mouse grab?
  virtual void set_mouse_pointer (widget wid, string name, string mask) = 0;
    // set the shape of the mouse pointer with a mask ("" means no mask)
  virtual void delayed_message (widget wid, string message, time_t delay) = 0;
    // send message to wid for reception after delay (used for scrolling)
};

window plain_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);
  // construct a plain window with a given name and size hints
window popup_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);
  // construct a popup window with a given name and size hints
int get_identifier (window w);
  // get low-level handle for the window, as used by the operating system
window get_window (int id);
  // determine the window as a function of its identifier

#endif // defined WINDOW_H
