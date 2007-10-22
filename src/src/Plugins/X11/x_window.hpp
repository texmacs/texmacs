
/******************************************************************************
* MODULE     : x_window.hpp
* DESCRIPTION: Windows under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

class x_window_rep: public x_drawable_rep, public window_rep {
  widget        w;
  x_display     dis;
  char*         name;
  string        the_name;

  Display*      dpy;
  Window        win;
  GC            gc;
  rectangles    invalid_regions;
  rectangles    clipping;
  int           win_x, win_y;
  int           win_w, win_h;
  bool          win_flag;

  bool          ic_ok;
  XIC           ic;
  widkit_widget_rep*   kbd_focus;
  bool          has_focus;

  bool          full_screen_flag;
  Window        save_win;
  int           save_x, save_y;
  int           save_w, save_h;

public:

  /********************* specific routines for x_window **********************/

  x_window_rep (widget w, x_display dis, char* name);
  x_window_rep (widget w, x_display dis, char* name, SI x, SI y);
  x_window_rep (widget wid, x_display dis, char* name, SI w, SI h, SI x, SI y);
  ~x_window_rep ();
  void get_extents (int& w, int& h);

  void compute_size (SI& w1, SI& h1, SI& w2, SI& h2, SI& w3, SI& h3);
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

  string get_name ();
  void set_name (string s);
  void map ();
  void unmap ();
  void full_screen (bool flag);
  void get_position (SI& x, SI& y);
  void get_size (SI& w, SI& h);
  void move (SI x, SI y);
  void resize (SI w, SI h);
  void set_keyboard_focus (widget wid);

  void invalidate (SI x1, SI y1, SI x2, SI y2);
  bool repainted ();
  void clip (SI x1, SI y1, SI x2, SI y2);
  void unclip ();
  void translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);

  /****************************** friends ************************************/

  friend class x_display_rep;
  friend class x_drawable_rep;
  friend Bool my_predicate (Display* dpy, XEvent* ev, XPointer arg);
};

#endif // defined X_WINDOW_H
