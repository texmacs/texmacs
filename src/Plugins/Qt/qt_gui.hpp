
/******************************************************************************
* MODULE     : qt_gui.hpp
* DESCRIPTION: QT GUI class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_GUI_HPP
#define QT_GUI_HPP

#include <QPixmap>
#include <QApplication>
#include <QTimer>
#include <QLabel>
#include <QList>

#include "timer.hpp"
#include "gui.hpp"
#include "font.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"
#include "socket_notifier.hpp"

/******************************************************************************
* The qt_gui class
******************************************************************************/

typedef class qt_gui_rep* qt_gui;
extern qt_gui the_gui;
class QTMGuiHelper;
class simple_widget_rep;

class qt_gui_rep {
public:
  bool interrupted;
  time_t interrupt_time;
  QTMGuiHelper *gui_helper;
  QTimer *updatetimer;
  QList<QLabel*> waitDialogs;
  QWidget *waitWindow;

  hashmap<string,tree>   selection_t;
  hashmap<string,string> selection_s;

public:
  qt_gui_rep (int &argc, char **argv);
  virtual ~qt_gui_rep ();

  /********************* extents, grabbing, selections ***********************/
  void get_extents (SI& width, SI& height);
  void get_max_size (SI& width, SI& height);
  // void set_button_state (unsigned int state);

  /* important routines */
  void event_loop ();

  /* interclient communication */
  virtual bool get_selection (string key, tree& t, string& s, string format);
  virtual bool set_selection (string key, tree t, string s, string format);
  virtual void clear_selection (string key);

  /* miscellaneous */
  void image_gc (string name= "*");
  void set_mouse_pointer (string name);
  void set_mouse_pointer (string curs_name, string mask_name);
  void show_wait_indicator (widget w, string message, string arg);
  bool check_event (int type);

  void update();
  
  void add_notifier (socket_notifier);
  void remove_notifier (socket_notifier);
  void enable_notifier (socket_notifier, bool);
  
  /* queued processing */
  void process_keypress (simple_widget_rep *wid, string key, time_t t);
  void process_keyboard_focus (simple_widget_rep *wid, bool has_focus, time_t t);
  void process_mouse (simple_widget_rep *wid, string kind, SI x, SI y, int mods, time_t t);
  void process_resize (simple_widget_rep *wid, SI x, SI y);
  void process_command (command _cmd);
  void process_command (command _cmd, object _args);
  void process_socket_notification (socket_notifier sn);
  void process_delayed_commands (); 

  void process_queued_events (int max = -1);
  //void process_get_size_hint (SI& w, SI& h);
  //void process_notify_resize (SI w, SI h);
  //void process_set_shrinking_factor (int sf);
  //void process_clear (SI x1, SI y1, SI x2, SI y2);
  //void process_repaint (SI x1, SI y1, SI x2, SI y2);
  
  
};

void force_update(); 
// force an immediate update of the internal texmacs state

#endif // defined QT_GUI_HPP
