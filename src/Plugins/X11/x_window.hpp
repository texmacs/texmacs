
/******************************************************************************
* MODULE     : x_window.hpp
* DESCRIPTION: Windows under X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef X_WINDOW_H
#define X_WINDOW_H
#include "window.hpp"
#include "X11/x_drawable.hpp"
#include "rectangles.hpp"
#include "array.hpp"

/******************************************************************************
* The x_window class
******************************************************************************/

class x_window_rep: public window_rep {
public:
  widget           w;
  x_gui            gui;
  char*            name;
  string           the_name;
  x_drawable_rep*  ren;

  Display*      dpy;
  Window        win;
  GC            gc;
  rectangles    invalid_regions;
  SI            Min_w, Min_h;
  SI            Def_w, Def_h;
  SI            Max_w, Max_h;
  int           win_x, win_y;
  int           win_w, win_h;

  bool          ic_ok;
  XIC           ic;
  widget_rep*   kbd_focus;
  bool          has_focus;

  bool          full_screen_flag;
  Window        save_win;
  int           save_x, save_y;
  int           save_w, save_h;

public:

  /********************* specific routines for x_window **********************/


  x_window_rep (widget w, x_gui gui, char* name,
		SI min_w, SI min_h, SI def_w, SI def_h, SI max_w, SI max_h);
  ~x_window_rep ();
  widget get_widget ();
  renderer get_renderer ();
  void get_extents (int& w, int& h);

  void set_hints (SI min_w, SI min_h, SI max_w, SI max_h);
  void initialize ();

  void move_event (int x, int y);
  void resize_event (int w, int h);
  void destroy_event ();
  void invalidate_event (int x1, int y1, int x2, int y2);
  void key_event (string key);
  void mouse_event (string ev, int x, int y, time_t t);
  void focus_in_event ();
  void focus_out_event ();
  void repaint_invalid_regions ();

  /********************* routines from window.hpp **************************/

  void   set_name (string name);
  string get_name ();
  void   set_visibility (bool flag);
  void   set_full_screen (bool flag);
  void   set_size (SI w, SI h);
  void   get_size (SI& w, SI& h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  void   set_keyboard_focus (widget wid, bool get_focus);
  bool   get_keyboard_focus (widget wid);
  void   set_mouse_grab (widget wid, bool get_grab);
  bool   get_mouse_grab (widget w);
  void   set_mouse_pointer (widget wid, string name, string mask);
  void   delayed_message (widget wid, string s, time_t delay);
  void   invalidate (SI x1, SI y1, SI x2, SI y2);
  bool   repainted ();
  void   translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);

  /****************************** friends ************************************/

  friend class x_gui_rep;
  friend class x_drawable_rep;
  friend Bool my_selnotify_predicate (Display* dpy, XEvent* ev, XPointer arg);
  friend int get_identifier (window w);
};

typedef x_window_rep* x_window;
Window get_Window (widget w);
x_window get_x_window (widget w);

#endif // defined X_WINDOW_H
