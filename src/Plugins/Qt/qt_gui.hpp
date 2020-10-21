
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

#include <QTranslator>
#include <QTimer>
#include <QLabel>
#include <QList>

#include "qt_simple_widget.hpp"
#include "tm_timer.hpp"
#include "gui.hpp"
#include "font.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"
#include "socket_notifier.hpp"

#if (QT_VERSION >= 0x050000) && defined(OS_MACOS) && defined(CocoaPlugin)
#ifndef QT_MAC_USE_COCOA
#define QT_MAC_USE_COCOA 1
#endif
#endif

typedef class qt_gui_rep* qt_gui;
extern qt_gui the_gui;

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
  
  friend class qt_gui_rep;
};


/******************************************************************************
* The qt_gui class
******************************************************************************/

class QTMGuiHelper;

class qt_gui_rep {
  bool           interrupted;
  time_t      interrupt_time;
  QTimer*        updatetimer;
  QList<QLabel*> waitDialogs;
  QWidget*        waitWindow;
  widget          _popup_wid;
  time_t      popup_wid_time; //!< 0 means not to show _popup_wid
  
  hashmap<string,tree>   selection_t;
  hashmap<string,string> selection_s;
  
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
  QTMGuiHelper*  gui_helper;

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
  virtual bool set_selection (string key, tree t, string s, string sv,
                              string sh, string format);
  virtual void clear_selection (string key);
  bool put_graphics_on_clipboard (url file);

  /* miscellaneous */
  void set_mouse_pointer (string name);
  void set_mouse_pointer (string curs_name, string mask_name);
  void show_wait_indicator (widget w, string message, string arg);
  void show_help_balloon (widget wid, SI x, SI y);
  void add_event (const queued_event& ev);
  bool check_event (int type);
  void set_check_events (bool enable_check);

  void update();
  void force_update();
  void need_update();
  void refresh_language();
  
  /* queued processing */
  void process_keypress (qt_simple_widget_rep *wid, string key, time_t t);
  void process_keyboard_focus (qt_simple_widget_rep *wid, bool has_focus,
                               time_t t);
  void process_mouse (qt_simple_widget_rep *wid, string kind, SI x, SI y,
                      int mods, time_t t);
  void process_resize (qt_simple_widget_rep *wid, SI x, SI y);
  void process_command (command _cmd);
  void process_command (command _cmd, object _args);
  void process_delayed_commands (); 
  void process_queued_events (int max = -1);
  
  /* befriended interface functions */
  friend class QTMGuiHelper;
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

#endif // defined QT_GUI_HPP
