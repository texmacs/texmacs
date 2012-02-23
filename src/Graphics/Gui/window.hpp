
/******************************************************************************
* MODULE     : window.hpp
* DESCRIPTION: Abstract window class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WINDOW_H
#define WINDOW_H
#include "gui.hpp"
#include "renderer.hpp"

class widget;
class window_rep;
typedef window_rep* window;

class window_rep {
public:
  inline window_rep () {}
  inline virtual ~window_rep () {}

  virtual widget get_widget () = 0;
    // Get the top widget associated to the window
  virtual renderer get_renderer () = 0;
    // Get the renderer associated to the window
  virtual void set_name (string name) = 0;
    // Set the window title
  virtual void set_visibility (bool flag) = 0;
    // Map or unmap the window
  virtual void set_full_screen (bool flag) = 0;
    // Set or reset full screen mode
  virtual void set_size (SI w, SI h) = 0;
    // Resize the window
  virtual void set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) = 0;
    // Specify how far the window can be resized
  virtual void get_size (SI& w, SI& h) = 0;
    // Get the current size of the window
  virtual void get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h)=0;
    // Get information on how far the window can be resized
  virtual void set_position (SI x, SI y) = 0;
    // Move the window
  virtual void get_position (SI& x, SI& y) = 0;
    // Get the current position of the window on the screen
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2) = 0;
    // Explicit request for redrawing a region in the window
  virtual void translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) = 0;
    // Fast translation of a region in the window (used for scrolling)
  virtual void set_keyboard_focus (widget wid, bool get_focus= true) = 0;
    // Obtain or release the keyboard focus
  virtual bool get_keyboard_focus (widget wid) = 0;
    // Does this widget have the keyboard focus
  virtual void set_mouse_grab (widget wid, bool get_grab) = 0;
    // Obtain or release the mouse grab. Recursive grabs are stored on
    // a stack: if w1 and w2 successively grab the mouse and w2 releases
    // the mouse grab, then w1 reobtains the mouse grab. Enter and leave
    // mouse events are also issued when obtaining or loosing the grab.
  virtual bool get_mouse_grab (widget wid) = 0;
    // Does this widget have the mouse grab?
  virtual void set_mouse_pointer (widget wid, string name, string mask) = 0;
    // Set the shape of the mouse pointer with a mask ("" means no mask)
  virtual void delayed_message (widget wid, string message, time_t delay) = 0;
    // Send message to wid for reception after delay (used for scrolling)
  inline virtual void begin_draw () {};
    // Use window as renderer outside dedicated repaint method
  inline virtual void end_draw () {};
    // Stop using window as renderer outside dedicated repaint method
};

window plain_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);
  // Construct a plain window with a given name and size hints
window popup_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);
  // Construct a popup window with a given name and size hints
int get_identifier (window w);
  // Get low-level handle for the window, as used by the operating system
window get_window (int id);
  // Determine the window as a function of its identifier
void windows_delayed_refresh (int ms);
  // Refresh all windows after ms milliseconds

#endif // defined WINDOW_H
