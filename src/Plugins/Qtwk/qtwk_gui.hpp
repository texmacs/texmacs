
/******************************************************************************
* MODULE     : qtwk_gui.hpp
* DESCRIPTION: QT/Widkit  GUI class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTWK_GUI_HPP
#define QTWK_GUI_HPP

#include <QTranslator>
#include <QTimer>
#include <QLabel>
#include <QList>

#include "array.hpp"
#include "list.hpp"
#include "hashmap.hpp"

#include "gui.hpp"
#include "font.hpp"
#include "widget.hpp"

#include "tm_timer.hpp"
#include "socket_notifier.hpp"

#include "simple_wk_widget.hpp"
#include "qtwk_window.hpp"


typedef class qtwk_gui_rep* qtwk_gui;
extern qtwk_gui the_gui;

/******************************************************************************
 * Event queue
 ******************************************************************************/

class qp_type {
public:
  enum id_t {
    QP_NULL,    QP_KEYPRESS,     QP_KEYBOARD_FOCUS,
    QP_MOUSE,   QP_RESIZE,       QP_SOCKET_NOTIFICATION,
    QP_COMMAND, QP_COMMAND_ARGS, QP_DELAYED_COMMANDS };
  id_t sid;
  inline qp_type (id_t sid2 = QP_NULL): sid (sid2) {}
  inline qp_type (const qp_type& s): sid (s.sid) {}
  inline qp_type& operator = (qp_type s) { sid = s.sid; return *this; }
  inline operator id_t () const { return sid; }
  inline bool operator == (id_t sid2) { return sid == sid2; }
  inline bool operator != (id_t sid2) { return sid != sid2; }
  inline bool operator == (qp_type s) { return sid == s.sid; }
  inline bool operator != (qp_type s) { return sid != s.sid; }
  inline friend tm_ostream& operator << (tm_ostream& out, qp_type s)
  { return out << s.sid; }
};

class queued_event : public pair<qp_type, blackbox>
{
public:
  queued_event (qp_type _type = qp_type(), blackbox _bb = blackbox())
  : pair<qp_type, blackbox>(_type, _bb) { }
};

/*!
 */
class event_queue {
  list<queued_event> q;
  event_queue (const event_queue& q2);  // = delete;
  event_queue& operator= (const event_queue& q2); // = delete;
  
  unsigned int n;  // ugly internal counter to avoid traversal of list in N(q)
public:
  event_queue();
  
  void append (const queued_event& ev);
  queued_event next ();
  bool is_empty() const;
  int size() const;
};


/******************************************************************************
 * Delayed commands
 ******************************************************************************/

/*! The queue of delayed commands.
 */
class command_queue {
  array<object> q;
  array<time_t> start_times;
  time_t lapse;
  
  bool wait;
    // this flag is used in update() to insert QP_DELAYED_COMMANDS events in
    // the TeXmacs event queue to have delayed command handling properly
    // interspersed with the other events

public:
  command_queue();
  ~command_queue();

  void exec (object cmd);
  void exec_pause (object cmd);
  void exec_pending ();
  void clear_pending ();
  bool must_wait (time_t now) const;
  
  friend class qtwk_gui_rep;
};


/******************************************************************************
* The qtwk_gui class
******************************************************************************/

class QTWKGuiHelper;

class qtwk_gui_rep {
  bool           interrupted;
  time_t      interrupt_time;
  QTimer*        updatetimer;
  QList<QLabel*> waitDialogs;
  QWidget*        waitWindow;
  widget          _popup_wid;
  time_t      popup_wid_time; //!< 0 means not to show _popup_wid
  
  
  widget          balloon_wid;
  window          balloon_win;
  SI              balloon_x;
  SI              balloon_y;
  time_t          balloon_time;

  
  
  
  list<qtwk_window>        windows_l;

  list<widget>    grab_ptr;
  list<widget>    grab_kbd;

  hashmap<string,tree>   selection_t;
  hashmap<string,string> selection_s;
  
  list<message>   messages;

  QTranslator* q_translator;
  
  time_t time_credit;        // interval to interrupt long redrawings
  time_t timeout_time;       // new redraw interruption
  
    // marshalling flags between update, needs_update and check_event.
  bool do_check_events;
  bool        updating;
  bool  needing_update;

  event_queue     waiting_events;
  command_queue delayed_commands;

public:
  QTWKGuiHelper*  gui_helper;

public:
  qtwk_gui_rep (int &argc, char **argv);
  virtual ~qtwk_gui_rep ();

  /* extents, grabbing, selections */
  void get_extents (SI& width, SI& height);
  void get_max_size (SI& width, SI& height);
  // void set_button_state (unsigned int state);

  /* important routines */
  void event_loop ();

  /* interclient communication */
  void   created_window (qtwk_window win);
  void   deleted_window (qtwk_window win);
  void   focussed_window (qtwk_window win);

  virtual bool get_selection (string key, tree& t, string& s, string format);
  virtual bool set_selection (string key, tree t, string s, string sv,
                              string sh, string format);
  virtual void clear_selection (string key);
  bool put_graphics_on_clipboard (url file);

  
  void   emulate_leave_enter (widget old_widget, widget new_widget);
  void   obtain_mouse_grab (widget wid);
  void   release_mouse_grab ();
  bool   has_mouse_grab (widget w);

  
  /* miscellaneous */
  void set_mouse_pointer (string name);
  void set_mouse_pointer (string curs_name, string mask_name);
  void show_wait_indicator (widget w, string message, string arg);

  void show_help_balloon (widget wid, SI x, SI y);
  void map_balloon ();
  void unmap_balloon ();

  void add_event (const queued_event& ev);
  bool check_event (int type);
  void set_check_events (bool enable_check);

  void update();
  void force_update();
  void need_update();
  void refresh_language();
  
  /******************************** Fonts ************************************/
  void set_default_font (string name);
  font default_font_sub (bool tt, bool mini, bool bold);
  font default_font (bool tt= false, bool mini= false, bool bold= false);

  /* queued processing */
  void process_keypress (widget wid, string key, time_t t);
  void process_keyboard_focus (widget wid, bool has_focus, time_t t);
  void process_mouse (widget wid, string kind, SI x, SI y, int mods, time_t t);
  void process_resize (widget wid, SI x, SI y);
  void process_command (command _cmd);
  void process_command (command _cmd, object _args);
  void process_delayed_commands (); 
  void process_queued_events (int max = -1);
  
  /* befriended interface functions */
  friend class QTMGuiHelper;
  friend class qtwk_window_rep;
  friend void exec_delayed (object cmd);
  friend void exec_delayed_pause (object cmd);
  friend void clear_pending_commands ();
  friend void needs_update ();
};

/*! Force an immediate update of the internal texmacs state. */
void force_update();

#define BEGIN_SLOT                              \
  try {
#define END_SLOT                                \
  }                                             \
  catch (string s) {                            \
    the_exception= s;                           \
  }

#endif // defined QTWK_GUI_HPP
