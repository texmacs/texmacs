
/******************************************************************************
* MODULE     : qtwk_window.hpp
* DESCRIPTION: QT/Widkit window class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTWK_WINDOW_HPP
#define QTWK_WINDOW_HPP
#include "widget.hpp"
#include "window.hpp"
#include "Qt/qt_renderer.hpp"

#include "rectangles.hpp"
#include "array.hpp"

#include <QtGui>

/******************************************************************************
* Delayed messages
******************************************************************************/

struct message_rep: concrete_struct {
  widget wid;
  string s;
  time_t t;
  message_rep (widget wid, string s, time_t t);
  friend class message;
};

class message {
  CONCRETE(message);
  message (widget wid, string s, time_t t);
};
CONCRETE_CODE(message);

tm_ostream& operator << (tm_ostream& out, message m);

/******************************************************************************
* The qtwk_window class
******************************************************************************/

class qtwk_window_rep;
typedef qtwk_window_rep* qtwk_window;
class qtwk_gui_rep;
typedef qtwk_gui_rep* qtwk_gui;

class QTWKWindow;

class qtwk_window_rep: public window_rep {
public:
  widget           w;
  qtwk_gui         gui;
  string           orig_name;
  char*            name;
  string           the_name;
  string           mod_name;
  QTWKWindow*      win;
  int              win_id;

  rectangles    invalid_regions;
  SI            Min_w, Min_h;
  SI            Def_w, Def_h;
  SI            Max_w, Max_h;
  SI            win_w, win_h;
  SI            win_x, win_y;

  widget_rep*   kbd_focus;
  bool          has_focus;

  bool          full_screen_flag;
  QWidget*      save_win;
  int           save_x, save_y;
  int           save_w, save_h;

public:

  /********************* specific routines for x_window **********************/


  qtwk_window_rep (widget w, qtwk_gui gui, char* name,
		SI min_w, SI min_h, SI def_w, SI def_h, SI max_w, SI max_h);
  ~qtwk_window_rep ();
  widget get_widget ();

  void initialize ();

  void move_event (int x, int y);
  void resize_event (int w, int h);
  void destroy_event ();
  void invalidate_event (int x1, int y1, int x2, int y2);
  void key_event (string key);
  void mouse_event (string ev, int x, int y, int flags, time_t t);
  void focus_in_event ();
  void focus_out_event ();
  void repaint_invalid_regions ();

  /********************* routines from window.hpp **************************/

  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_full_screen (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  void   set_keyboard_focus (widget wid, bool get_focus);
  bool   get_keyboard_focus (widget wid);
  void   set_mouse_grab (widget wid, bool get_grab);
  bool   get_mouse_grab (widget w);
  void   set_mouse_pointer (widget wid, string name, string mask);
  void   delayed_message (widget wid, string s, time_t delay);
  void   invalidate (SI x1, SI y1, SI x2, SI y2);
  bool   is_invalid ();
  void   translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);


  /****************************** backing store *******************************/

  basic_renderer get_renderer();

protected:
  
//  rectangles   invalid_regions;
  QPixmap      backingPixmap;
//  QPoint       backing_pos;

  void invalidate_rect (int x1, int y1, int x2, int y2);
 // void invalidate_all ();
  
  /****************************** friends ************************************/

  
  friend class qtwk_gui_rep;
  friend int get_identifier (window w);
  friend class QTWKWindow;
};

//Window get_Window (widget w);
qtwk_window get_qtwk_window (widget w);
//extern int alt_mask;
//extern int meta_mask;

#endif // defined QTWK_WINDOW_HPP
