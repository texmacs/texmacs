
/******************************************************************************
* MODULE     : aqua_window.h
* DESCRIPTION: Aqua window class
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef AQUA_WINDOW_H
#define AQUA_WINDOW_H
#include "mac_cocoa.h"
#include "window.hpp"
#include "aqua_widget.h"
#include "aqua_renderer.h"

//class widget;
class aqua_window_rep;
typedef aqua_window_rep* aqua_window;

class aqua_window_rep: public aqua_renderer_rep, public window_rep {

  NSWindowController *wc;
  widget wid;	

public:
  aqua_window_rep (widget wid2);
  virtual ~aqua_window_rep ();

  virtual widget get_widget ();
  virtual void set_name (string name);
  virtual void set_visibility (bool flag);
  virtual void set_full_screen (bool flag);
  virtual void set_size (SI w, SI h);
  virtual void get_size (SI& w, SI& h);
  virtual void set_position (SI x, SI y);
  virtual void get_position (SI& x, SI& y);
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2);
  virtual void translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);
  virtual void set_keyboard_focus (widget wid, bool get_focus);
  virtual bool get_keyboard_focus (widget wid);
  virtual void set_mouse_grab (widget wid, bool get_grab);
  virtual bool get_mouse_grab (widget wid);
  virtual void set_mouse_pointer (widget wid, string name, string mask);
  virtual void delayed_message (widget wid, string message, time_t delay);	
  virtual void begin_draw();
  virtual void end_draw();

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

#endif // defined AQUA_WINDOW_H
