
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
#include "ps_device.hpp"
#include "display.hpp"

class widget;

class window_rep: virtual public ps_device_rep {
public:
  inline window_rep () {}
  inline virtual ~window_rep () {}
  virtual widget get_widget () = 0;

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
  virtual void set_keyboard_focus (widget wid) = 0;
    // forward keyboard events to the subwidget wid of the window

  virtual void clip (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void unclip () = 0;
  virtual void translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) = 0;
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2) = 0;
};

window plain_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);
window popup_window (widget w, string name, SI min_w, SI min_h,
		     SI def_w, SI def_h, SI max_w, SI max_h);

#endif // defined WINDOW_H
