
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

#include "qt_simple_widget.hpp"
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

class qt_gui_rep {

  friend class   QTMGuiHelper;
  friend void needs_update ();

  bool           interrupted;
  time_t      interrupt_time;
  QTimer*        updatetimer;
  QList<QLabel*> waitDialogs;
  QWidget*        waitWindow;
  widget          _popup_wid;
  time_t      popup_wid_time; //!< 0 means not to show _popup_wid
  
  hashmap<string,tree>   selection_t;
  hashmap<string,string> selection_s;
  
  hashmap<socket_notifier,pointer>  read_notifiers;
  hashmap<socket_notifier,pointer> write_notifiers;

public:
  QTMGuiHelper*   gui_helper;

public:
  qt_gui_rep (int &argc, char **argv);
  virtual ~qt_gui_rep ();

  /* extents, grabbing, selections */
  void get_extents (SI& width, SI& height);
  void get_max_size (SI& width, SI& height);
  // void set_button_state (unsigned int state);

  /* important routines */
  void event_loop ();

  /* interclient communication */
  virtual bool get_selection (string key, tree& t, string& s, string format);
  virtual bool set_selection (string key, tree t, string s, string format);
  virtual void clear_selection (string key);
  bool put_graphics_on_clipboard (url file);

  /* miscellaneous */
  void image_gc (string name= "*");
  void set_mouse_pointer (string name);
  void set_mouse_pointer (string curs_name, string mask_name);
  void show_wait_indicator (widget w, string message, string arg);
  void show_help_balloon (widget wid, SI x, SI y);  
  bool check_event (int type);

  void update();
  
  /* socket notifications */
  void add_notifier (socket_notifier);
  void remove_notifier (socket_notifier);
  void enable_notifier (socket_notifier, bool);
  
  /* queued processing */
  void process_keypress (qt_simple_widget_rep *wid, string key, time_t t);
  void process_keyboard_focus (qt_simple_widget_rep *wid, bool has_focus, time_t t);
  void process_mouse (qt_simple_widget_rep *wid, string kind, SI x, SI y, int mods, time_t t);
  void process_resize (qt_simple_widget_rep *wid, SI x, SI y);
  void process_command (command _cmd);
  void process_command (command _cmd, object _args);
  void process_socket_notification (socket_notifier sn);
  void process_delayed_commands (); 
  void process_queued_events (int max = -1);
};

void force_update(); //!< Force an immediate update of the internal texmacs state

#endif // defined QT_GUI_HPP
