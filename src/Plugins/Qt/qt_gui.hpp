
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
public:
  bool interrupted;
  time_t interrupt_time;
  QTMGuiHelper *gui_helper;
  QTimer *updatetimer;

  char*                  selection;
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
  virtual bool get_selection (string key, tree& t, string& s);
  virtual bool set_selection (string key, tree t, string s);
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
};

#endif // defined QT_GUI_HPP
