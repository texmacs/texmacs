
/******************************************************************************
* MODULE     : qt_gui.cpp
* DESCRIPTION: QT display class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "iterator.hpp"
#include "dictionary.hpp"
#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "analyze.hpp"
#include <locale.h>
#include "language.hpp"
#include "message.hpp"
#include <QDesktopWidget>
#include <QClipboard>
#include <QFileOpenEvent>
#include <QLabel>
#include <QSocketNotifier>
#include <QSetIterator>
#include "QTMGuiHelper.hpp"
#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
#include "qt_renderer.hpp" // for the_qt_renderer

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif


#include "tm_link.hpp" // for number_of_servers

#include "Scheme/object.hpp"
//#include "TeXmacs/server.hpp" // for get_server

#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"

extern window (*get_current_window) (void);

qt_gui_rep* the_gui= NULL;
int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

time_t time_credit = 100;  // interval to interrupt long redrawings
time_t timeout_time; // new redraw interruption
time_t lapse = 0; // optimization for delayed commands

// marshalling flags between update, needs_update and check_event.
bool updating = false;
bool needing_update = false;
bool wait_for_delayed_commands = true;
// this flag is used in update() to insert QP_DELAYED_COMMANDS events in TeXmacs 
// event queue so to have delayed command handling properly interspersed with 
// the other events

/******************************************************************************
* Constructor and geometry
******************************************************************************/

qt_gui_rep::qt_gui_rep(int &argc, char **argv):
  interrupted (false)
{
  (void) argc; (void) argv;
  // argc= argc2;
  // argv= argv2;

  interrupted   = false;
  time_credit = 100;
  timeout_time= texmacs_time () + time_credit;

  //waitDialog = NULL;
  
  set_output_language (get_locale_language ());
  gui_helper = new QTMGuiHelper (this);
  qApp -> installEventFilter (gui_helper);
  
#ifdef QT_MAC_USE_COCOA
  //HACK: this filter is needed to overcome a bug in Qt/Cocoa
  extern void mac_install_filter(); // defined in src/Plugins/MacOS/mac_app.mm
  mac_install_filter();
#endif
  
  
  
  qApp-> installTranslator(new QTMTranslator(qApp));
  
  updatetimer = new QTimer (gui_helper);
  updatetimer->setSingleShot (true);
  QObject::connect ( updatetimer, SIGNAL(timeout()), 
                     gui_helper, SLOT(doUpdate()));
//  (void) default_font ();
}

/* important routines */
void
qt_gui_rep::get_extents (SI& width, SI& height) {
  QDesktopWidget* d= QApplication::desktop();
  int w = d->width();  // returns desktop width
  int h = d->height(); // returns desktop height        
  width = ((SI) w) * PIXEL;
  height= ((SI) h) * PIXEL;
}

void
qt_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}


qt_gui_rep::~qt_gui_rep()  {
  delete gui_helper;
  
  while (waitDialogs.count()) {
    delete waitDialogs.last();
    waitDialogs.removeLast();
  }
  
 // delete updatetimer; we do not need this given that gui_helper is the 
 // parent of updatetimer
}

/******************************************************************************
* interclient communication
******************************************************************************/

bool
qt_gui_rep::get_selection (string key, tree& t, string& s) {
  QClipboard *cb= QApplication::clipboard ();
  QClipboard::Mode mode= QClipboard::Clipboard;
  bool owns= false;
  if (key == "primary" || (key == "mouse" && cb->supportsSelection ())) {
    if (key == "mouse") mode= QClipboard::Selection;
    const QMimeData *md= cb->mimeData (mode);
    if (md) owns= md->hasFormat ("application/x-texmacs-clipboard");  
  }
  else owns= true;
  
  s= "";
  t= "none";
  if (owns) {
    if (!selection_t->contains (key)) return false;
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
  else {
    QString originalText= cb->text (mode);
    QByteArray buf= originalText.toAscii ();
    if (!(buf.isEmpty())) s << string (buf.constData(), buf.size());
    t= tuple ("extern", s);
    return true;
  }
}

bool
qt_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
        
  QClipboard *cb= QApplication::clipboard ();
  QClipboard::Mode mode= QClipboard::Clipboard;
  if (key == "primary");
  else if (key == "mouse" && cb->supportsSelection())
    mode= QClipboard::Selection;
  else return true;
  cb->clear (mode);

  char *selection = as_charp (s);
  cb->setText (selection, mode);
  QByteArray ba (selection);
  QMimeData *md= new QMimeData;
  md->setData ("application/x-texmacs-clipboard", ba);
  md->setText (selection);
  cb->setMimeData (md, mode); 
  // according to the docs, ownership of mimedata is transferred to clipboard
  // so no memory leak here
  tm_delete_array (selection);
  return true;
}

void
qt_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  
  QClipboard *cb= QApplication::clipboard();
  QClipboard::Mode mode= QClipboard::Clipboard;
  bool owns= false;
  if (key == "primary");
  else if (key == "mouse" && cb->supportsSelection())
    mode= QClipboard::Selection;
  else return;

  const QMimeData *md= cb->mimeData (mode);
  if (md) owns= md->hasFormat ("application/x-texmacs");
  if (owns) cb->clear (mode);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void qt_gui_rep::image_gc (string name) { (void) name; }
// FIXME: remove this unused function
void qt_gui_rep::set_mouse_pointer (string name) { (void) name; }
// FIXME: implement this function
void qt_gui_rep::set_mouse_pointer (string curs_name, string mask_name) 
{ (void) curs_name; (void) mask_name; } ;

/******************************************************************************
* Main loop
******************************************************************************/

void
qt_gui_rep::show_wait_indicator (widget w, string message, string arg)  {
  (void) w; (void) message; (void) arg;
  
  if (DEBUG_QT)  cout << "show_wait_indicator \"" << message << "\"\"" << arg << "\"" << LF;

  qt_window_widget_rep *wid = static_cast<qt_window_widget_rep*> (w.rep);

  // we move the texmacs window during an operation. 
  // We need to disable updates of the window to avoid erasure of the canvas
  // area
  //  wid->wid->setUpdatesEnabled(false);
  
  //FIXME: we must center the wait widget wrt the current active window
  
  if (waitDialogs.count()) {
    waitDialogs.last()->close();
  }
  
  if (N(message)) {
    
    // push a new wait message in the list
    
    if (arg != "") message= message * " " * arg * "...";
    
    QLabel* lab = new  QLabel(); //(wid->wid->window());
    lab->setWindowFlags(Qt::Window | Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);
    //lab->setWindowModality(Qt::ApplicationModal);
    lab->setFocusPolicy(Qt::NoFocus);
    lab->setMargin(15);
    lab->setText (to_qstring (tm_var_encode(message)));
    QSize sz = lab->sizeHint();
    QRect rect = QRect(QPoint(0,0),sz);
    //HACK: 
    // processEvents is needed to let Qt update windows coordinates in the case
    qApp->processEvents();
    //ENDHACK
    QPoint pt = wid->wid->window()->geometry().center();
    rect.moveCenter(pt);
    lab->move(rect.topLeft());
    waitDialogs << lab;
    
  } else {
    // pop the next wait message from the list
    if (waitDialogs.count()) {
      waitDialogs.last()->deleteLater();
      waitDialogs.removeLast();
    }    
  }

  if (waitDialogs.count()) {
    waitDialogs.last()->show();
    //waitDialogs.last()->raise();
    qApp->processEvents();
    waitDialogs.last()->repaint();
    qApp->processEvents();
    QApplication::flush();

  }

  //    wid->wid->setUpdatesEnabled(true);

  // next time we do update the dialog will disappear
  needs_update();  
}

void (*the_interpose_handler) (void) = NULL;

void gui_interpose (void (*r) (void)) { the_interpose_handler= r; }

void
qt_gui_rep::event_loop () {
  QApplication *app = (QApplication*) QApplication::instance();

  update();
  //needs_update();
  app->exec();
}

/******************************************************************************
 * Sockets notifications
 ******************************************************************************/

static hashmap<socket_notifier,pointer> read_notifiers;
static hashmap<socket_notifier,pointer> write_notifiers;

void 
qt_gui_rep::add_notifier (socket_notifier sn)
{
  QSocketNotifier *qsn;
  
  // replace any already present notifier

  remove_notifier (sn);

  if (DEBUG_QT) cout << "ADD NOTIFIER " << sn->fd << LF;

  
  // installs both a read and a write notifier 
  // (the texmacs interface does not specify enough its needs)
  
  qsn = new QSocketNotifier(sn->fd, QSocketNotifier::Read, gui_helper);
  read_notifiers (sn) = (pointer) (qsn); 
  QObject::connect( qsn, SIGNAL(activated(int)), 
                    gui_helper, SLOT(doReadSocketNotification(int)) );

  qsn = new QSocketNotifier(sn->fd, QSocketNotifier::Write, gui_helper);
  write_notifiers (sn) = (pointer) (qsn);
  QObject::connect( qsn, SIGNAL(activated(int)), 
                    gui_helper, SLOT(doWriteSocketNotification(int)) );  
}

void 
qt_gui_rep::remove_notifier (socket_notifier sn)
{  
  QSocketNotifier *qsn;

  if (DEBUG_QT) cout << "REMOVE NOTIFIER" << LF;

  // disable the (r/w) notifiers to prevent them to fire past this point
  // and schedule them for deletion at the end of the current runloop

  qsn = (QSocketNotifier *)read_notifiers [sn];
  if (qsn) {
    qsn->setEnabled (false);
    qsn->deleteLater ();
  }
  read_notifiers->reset (sn);

  qsn = (QSocketNotifier *)write_notifiers (sn);
  if (qsn) {
    qsn->setEnabled (false);
    qsn->deleteLater ();
  }
  write_notifiers->reset (sn);
}

void 
qt_gui_rep::enable_notifier (socket_notifier sn, bool flag)
{
  QSocketNotifier *qsn;
  qsn = (QSocketNotifier *)read_notifiers (sn);
  if (qsn) qsn->setEnabled (flag);
  qsn = (QSocketNotifier *)write_notifiers (sn);
  if (qsn) qsn->setEnabled (flag);
}



/******************************************************************************
 * Delayed commands
 ******************************************************************************/

//FIXME: the code below is almost a copy of the code in Guile/Scheme/object.cpp
//       maybe some refactoring would be necessary

static array<object> delayed_queue;
static array<time_t> start_queue;

void
exec_delayed (object cmd) {
  delayed_queue << cmd;
  start_queue << (((time_t) texmacs_time ()) - 1000000000);
  lapse = texmacs_time();
  needs_update();
}

void
exec_delayed_pause (object cmd) {
  delayed_queue << cmd;
  start_queue << ((time_t) texmacs_time ());
  lapse = texmacs_time();
  needs_update();
}

void
exec_pending_commands () {
  array<object> a= delayed_queue;
  array<time_t> b= start_queue;
  delayed_queue= array<object> (0);
  start_queue= array<time_t> (0);
  int i, n= N(a);
  for (i=0; i<n; i++) {
    time_t now=  texmacs_time ();
    if ((now - b[i]) >= 0) {
      object obj= call (a[i]);
      if (is_int (obj) && (now - b[i] < 1000000000)) {
        time_t pause = as_int (obj);
        //cout << "pause= " << obj << "\n";
        delayed_queue << a[i];
        start_queue << (now + pause);
      }
    }
    else {
      delayed_queue << a[i];
      start_queue << b[i];
    }
  }
  if (N(delayed_queue)>0) {
    lapse = start_queue[0];
    int n = N(start_queue);
    for (i=1; i<n; i++) {
      if (lapse > start_queue[i]) lapse = start_queue[i];
    }
  }
}

void
clear_pending_commands () {
  delayed_queue= array<object> (0);
  start_queue= array<time_t> (0);
}

/******************************************************************************
* Main routines
******************************************************************************/


void
gui_open (int& argc, char** argv) {
  // start the gui
 // new QApplication (argc,argv); now in texmacs.cpp
  the_gui = tm_new<qt_gui_rep> (argc, argv);
  
#ifdef MACOSX_EXTENSIONS
  mac_begin_remote();
#endif
}

void
gui_start_loop () {
  // start the main loop
  the_gui->event_loop ();
}

void
gui_close () {
  // cleanly close the gui
  ASSERT (the_gui != NULL, "gui not yet open");
  tm_delete (the_gui);
  the_gui=NULL;

#ifdef MACOSX_EXTENSIONS
  mac_end_remote();
#endif
}

void
gui_root_extents (SI& width, SI& height) {
  // get the screen size
  the_gui->get_extents (width, height);
  if (DEBUG_QT) 
    cout << "gui_root_extents (" << width << "," << height << ")" << LF;
}

void
gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  the_gui->get_max_size (width, height);
  if (DEBUG_QT) 
    cout << "gui_maximal_extents (" << width << "," << height << ")" << LF;
}

void
gui_refresh () {
  // called upon change of output language
  // emit a signal which force every QTMAction to change his text
  // according to the new language

  the_gui->gui_helper->doRefresh();
}


/******************************************************************************
 * QTMGuiHelper methods
 ******************************************************************************/

void
QTMGuiHelper::doUpdate () {
//  cout << "UPDATE " << texmacs_time () << LF;
  gui->update();
}

void
QTMGuiHelper::doRefresh () {
  emit refresh();
}

bool
QTMGuiHelper::eventFilter (QObject *obj, QEvent *event) {
   if (event->type() == QEvent::FileOpen) {
     QFileOpenEvent *openEvent = static_cast<QFileOpenEvent *>(event);
     const char *s = openEvent->file().toAscii().constData();
     //qDebug ("File Open Event %s", s);
     call ( "texmacs-load-buffer", object(url_system (s)), 
            object("generic"), object(1), object(false));
     return true;
   } else {
     // standard event processing
     return QObject::eventFilter(obj, event);
   }
}

void
QTMGuiHelper::doWriteSocketNotification (int socket) {
  if (DEBUG_QT) 
    cout << "WRITE SOCKET NOTIFICATION " << socket << " "
         << texmacs_time () << LF;
  iterator<socket_notifier> it = iterate (write_notifiers);
  while (it->busy ()) {
    socket_notifier sn= it->next ();
    if (sn->fd == socket) {
      //sn->notify();
      the_gui->process_socket_notification (sn);
      the_gui->enable_notifier (sn, false);
    }
  }
}

void
QTMGuiHelper::doReadSocketNotification (int socket) {
  if (DEBUG_QT) 
    cout << "READ SOCKET NOTIFICATION " << socket << " "
         << texmacs_time () << LF;
  iterator<socket_notifier> it = iterate (read_notifiers);
  while (it->busy ()) {
    socket_notifier sn= it->next ();
    if (sn->fd == socket) {
      //sn->notify();
      the_gui->process_socket_notification (sn);
      the_gui->enable_notifier (sn, false);
    }
  }
}


/******************************************************************************
 * Queued processing
 ******************************************************************************/

enum qp_type_id {
  QP_NULL,
  QP_KEYPRESS,
  QP_KEYBOARD_FOCUS,
  QP_MOUSE,
  QP_RESIZE,
  QP_SOCKET_NOTIFICATION,
  QP_COMMAND,
  QP_DELAYED_COMMANDS
};

class qp_type {
public:
  qp_type_id sid;
  inline qp_type (qp_type_id sid2 = QP_NULL): sid (sid2) {}
  inline qp_type (const qp_type& s): sid (s.sid) {}
  inline qp_type& operator = (qp_type s) { sid= s.sid; return *this; }
  inline operator qp_type_id () { return sid; }
  inline bool operator == (qp_type_id sid2) { return sid == sid2; }
  inline bool operator != (qp_type_id sid2) { return sid != sid2; }
  inline bool operator == (qp_type s) { return sid == s.sid; }
  inline bool operator != (qp_type s) { return sid != s.sid; }
  inline friend tm_ostream& operator << (tm_ostream& out, qp_type s) 
    { return out << s.sid; }
};

class queued_event : public pair<qp_type, blackbox> 
{
public:
  queued_event(qp_type _type = qp_type(), blackbox _bb = blackbox()) 
    : pair<qp_type, blackbox>(_type, _bb) {};
};


static array<queued_event> waiting_events;


void
process_event (queued_event ev) {
  switch ((qp_type_id) ev.x1) {
    case QP_NULL :
      break;
    case QP_KEYPRESS :
    {
      typedef triple<widget, string, time_t > T;
      T x = open_box <T> (ev.x2) ;
      concrete_simple_widget(x.x1) -> handle_keypress (x.x2, x.x3) ;
    }
      break;
    case QP_KEYBOARD_FOCUS :
    {
      typedef triple<widget, bool, time_t > T;
      T x = open_box <T> (ev.x2) ;
      concrete_simple_widget(x.x1) -> handle_keyboard_focus (x.x2, x.x3) ;
      send_invalidate_all(x.x1);
      //FIXME: the above invalidate is due to incorrect repainting when
      //       the widget loose the focus. I should investigate better
      //       the problem.
    }
      break;
    case QP_MOUSE :
    {
      typedef quintuple<string, SI, SI, int, time_t > T1;
      typedef pair<widget, T1> T;
      T x = open_box <T> (ev.x2) ;
      concrete_simple_widget(x.x1) -> handle_mouse (x.x2.x1, x.x2.x2, x.x2.x3, x.x2.x4, x.x2.x5) ;
    }
      break;
    case QP_RESIZE :
    {
      typedef triple<widget, SI, SI > T;
      T x = open_box <T> (ev.x2) ;
      concrete_simple_widget(x.x1) -> handle_notify_resize (x.x2, x.x3) ;
    }
      break;
    case QP_SOCKET_NOTIFICATION :
    {
      socket_notifier sn = open_box <socket_notifier> (ev.x2) ;
      // cout << "QP_SOCKET_NOTIFICATION " << sn->fd << LF;
      sn -> notify ();
      the_gui->enable_notifier (sn, true);
    }
      break;
    case QP_COMMAND :
    {
      command cmd = open_box <command> (ev.x2) ;
      // cout << "QP_COMMAND" << LF;
      cmd->apply();
    }
      break;
    case QP_DELAYED_COMMANDS :
    {
      // cout << "QP_DELAYED_COMMANDS" << LF;
      exec_pending_commands();
      wait_for_delayed_commands = true;
    }
      break;
    default:   
      FAILED("Unexpected queued event");
  }
}


void 
qt_gui_rep::process_queued_events (int max) {
  // we process a maximum of max events. There are two kind of events: those
  // which need a pass on interpose_handler just after and the others. We count
  // only the first kind of events. In update() we call this function with
  // max=1 so that only one of these "sensible" events is handled. Otherwise
  // updating of internal TeXmacs structure become very slow. This can be 
  // considerer a limitation of the current implementation of interpose_handler
  // Likewise this function is just an hack to get things working properly.
  
  array<queued_event> a = waiting_events, b;
  waiting_events = array<queued_event> ();
  
  int i, n = N(a);
  int count = 0;
  //cout << "(" << n << " events)";
  for (i=0; i<n; i++) {
    //cout << (int)a[i].x1 << ",";
    if ((max < 0) || (count<max))  {
      process_event(a[i]);
      switch (a[i].x1) {
        case QP_COMMAND:
        case QP_SOCKET_NOTIFICATION:
        case QP_RESIZE:
        case QP_DELAYED_COMMANDS:
          break;
        default:
          count++;
          break;
      }
    } else 
      b << a[i];
  }
  b << waiting_events;
  waiting_events = b;
}


void
add_event(const queued_event &ev)
{
  if (updating) {
    waiting_events << ev;
    needing_update = true;
    //needs_update();
  } else {
    waiting_events << ev;
//    process_event(ev);
//    the_gui->update();
    needs_update();
    // NOTE: we cannot update now since sometimes this seems to give problems
    // to the update of the window size after a resize. In that situation
    // sometimes when the window take again focus, update will be called for the
    // focus_in event and interpose_handler is run which send a slot_extent
    // message to the widget causing a wrong resize of the window.
    // this seems to cure the problem.
  }
}

void 
qt_gui_rep::process_keypress (simple_widget_rep *wid, string key, time_t t) {
  typedef triple<widget, string, time_t > T;
  add_event( queued_event ( QP_KEYPRESS, close_box<T> (T(wid, key, t)))); 
 // wid -> handle_keypress (key, t);
 // needs_update ();
}

void 
qt_gui_rep::process_keyboard_focus ( simple_widget_rep *wid, bool has_focus, 
                                     time_t t ) {
  typedef triple<widget, bool, time_t > T;
  add_event( 
    queued_event ( QP_KEYBOARD_FOCUS, close_box<T> (T(wid, has_focus, t)))); 
  //wid -> handle_keyboard_focus (has_focus, t);
  //needs_update ();
}

void 
qt_gui_rep::process_mouse ( simple_widget_rep *wid, string kind, SI x, SI y, 
                            int mods, time_t t ) {
  typedef quintuple<string, SI, SI, int, time_t > T1;
  typedef pair<widget, T1> T;
  add_event ( 
    queued_event ( QP_MOUSE, close_box<T> ( T (wid, T1 (kind, x, y, mods, t))))); 
//  wid -> handle_mouse (kind, x, y, mods, t);
//  needs_update ();
}

void 
qt_gui_rep::process_resize ( simple_widget_rep *wid, SI x, SI y ) {
  typedef triple<widget, SI, SI > T;
  add_event(  queued_event ( QP_RESIZE, close_box<T> (T(wid, x, y)))); 
//  wid -> handle_notify_resize (x, y);
//  needs_update ();
}

void 
qt_gui_rep::process_socket_notification ( socket_notifier sn ) {
  add_event ( 
    queued_event (QP_SOCKET_NOTIFICATION, close_box< socket_notifier > (sn)));
//  sn -> notify ();
//  enable_notifier (sn, true);
// needs_update ();
}

void 
qt_gui_rep::process_command (command _cmd) {
  add_event ( 
             queued_event (QP_COMMAND, close_box< command > (_cmd)));
}

void 
qt_gui_rep::process_delayed_commands () {
  add_event ( 
             queued_event (QP_DELAYED_COMMANDS, blackbox()));
}


void
fill_event_queue() {
  if (N(waiting_events) == 0) {
    qApp->processEvents(QEventLoop::ExcludeSocketNotifiers, 0); 
    // if (N(waiting_events) > 0) cout << "addins some events" << LF;
  }  
}

bool
qt_gui_rep::check_event (int type) {
//FIXME: add more types and refine, compare with X11 version.
  
// cout << "."; cout.flush();
//  return false;
  
  // do not interrupt while not in update
  // (for example while painting the icons in the menus)
  if (!updating) return false;
  
  switch (type) {
    case INTERRUPT_EVENT:
      if (interrupted) return true;
      else {
        time_t now= texmacs_time ();
        if (now - timeout_time < 0) return false;
        timeout_time= now + time_credit;
        interrupted= (N(waiting_events) > 0);
        //if (interrupted) cout << "INTERRUPT " 
        //  << now << "------------------" << LF;
        return interrupted;
      }
    case INTERRUPTED_EVENT:
      return interrupted;
    default:
      return false;
  }
}


void
qt_gui_rep::update () {
  // this is called by doUpdate, which in turns is fired by a timer activated in 
  // needs_update, and ensuring that interpose_handler is run during a pass in 
  // the eventloop afterwards we reactivate the timer with a pause 
  // (see FIXME below) 

  if (updating) {
    cout << "NESTED UPDATING: This should not happen" << LF;
    needs_update();
    return;
  }

  // cout << "<" << texmacs_time() << " " << N(delayed_queue) << " ";
  
  updatetimer->stop();
  updating = true;
  
  static int count_events = 0;
  static int max_proc_events = 10;

  time_t now = texmacs_time();
  needing_update = false;

  time_credit = 100 / (N(waiting_events)+1);

  
  // 1.
  // check if a wait dialog is active and in the case remove it.
  // if we are here then the long operation is finished.
  
  while (waitDialogs.count()) {
    waitDialogs.last()->close();
    waitDialogs.last()->deleteLater();
    waitDialogs.removeLast();
  }
  
  
  // 2.
  // manage delayed commands
  
  if (wait_for_delayed_commands && (lapse <= now)) process_delayed_commands();
  
  // 3.
  // if there are pending events in the private queue process them up to some
  // limit in processed events is reached. 
  // if there are no events or the limit is reached then proceed to a redraw.

  if (N(waiting_events) == 0) {
    // if there are not waiting events call at least once
    // the interpose handler
    if (the_interpose_handler) the_interpose_handler();
  }
  do {
    if ((N(waiting_events) > 0) && (count_events < max_proc_events)) {
        // cout << "e";
        //cout << "PROCESS QUEUED EVENTS START..."; cout.flush();
        process_queued_events (1);
        //cout << "AND END"  << LF;
        count_events++;
        
        //cout << "TYPESET START..."; cout.flush();
        if (the_interpose_handler) the_interpose_handler();
        //cout << "AND END" << LF;
      } else {
      // redrawing
      // cout << "r";
      count_events = 0;
      
      // repaint invalid regions  
      interrupted = false;
      timeout_time = texmacs_time() + time_credit;
      
      //cout << "REPAINT START..."; cout.flush();
      
      QSetIterator<QTMWidget*> i(QTMWidget::all_widgets);
      while (i.hasNext()) {
        QTMWidget *w = i.next();
        w->repaint_invalid_regions();
      }
      //cout << "AND END" << LF;
    }
  } while (N(waiting_events)>0);
  
  
  if (N(waiting_events) > 0) needing_update = true;
  if (interrupted) needing_update = true;
 // if (interrupted)     cout << "*";

  if (nr_windows == 0) {
    qApp->quit();
  }
  
  time_t delay = lapse - texmacs_time();
  if (delay > 1000/6) delay = 1000/6;
  if (delay < 0) delay = 0;
  if (needing_update) delay = 0;
  //cout << delay << ">" <<  LF;
  updatetimer->start (delay);
  
  updating = false;
  

  // FIXME: we need to ensure that the interpose_handler is run at regular 
  //        intervals (1/6th of sec) so that informations on the footbar are 
  //        updated. (this should be better handled by promoting code in 
  //        tm_editor::apply_changes (which is activated only after idle 
  //        periods) at the level of delayed commands in the gui.
  //        The interval cannot be too small to keep CPU usage low in idle state
} 



/******************************************************************************
* Font support
******************************************************************************/

void
set_default_font (string name) {
        (void) name;
  // set the name of the default font
  // this is ignored since Qt handles fonts for the widgets
}

font
get_default_font (bool tt, bool mini) {
  (void) tt; (void) mini;
  // get the default font or monospaced font (if tt is true)
        
  // return a null font since this function is not called in the Qt port.
  if (DEBUG_QT) cout << "get_default_font(): SHOULD NOT BE CALLED\n";
  return NULL;
  //return tex_font (this, "ecrm", 10, 300, 0);
}

// load the metric and glyphs of a system font
// you are not obliged to provide any system fonts

void
load_system_font (string family, int size, int dpi,
                  font_metric& fnm, font_glyphs& fng)
{
        (void) family; (void) size; (void) dpi; (void) fnm; (void) fng;
        if (DEBUG_QT) cout << "load_system_font(): SHOULD NOT BE CALLED\n";
}

/******************************************************************************
* Clipboard support
******************************************************************************/

bool
set_selection (string key, tree t, string s) {
  // Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
  // Returns true on success
  return the_gui->set_selection (key, t, s);
}

bool
get_selection (string key, tree& t, string& s) {
  // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
  // Returns true on success; sets t to (extern s) for external selections
  return the_gui->get_selection (key, t, s);
}

void
clear_selection (string key) {
  // Clear the selection on clipboard 'cb'
  the_gui->clear_selection (key);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/
int char_clip=0;

void
beep () {
  // Issue a beep
  QApplication::beep();
}

void
needs_update () {
  // 0 ms - call immediately when all other events have 
  // been processed
  //cout << "needs_update" << LF;
  if (updating) {
    needing_update = true;
  }
  else {
    the_gui->updatetimer->start (0);
  }
}

bool
check_event (int type) {
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
  return the_gui->check_event(type);
}

void image_gc (string name) {
// Garbage collect images of a given name (may use wildcards)
// This routine only needs to be implemented if you use your own image cache
 the_qt_renderer()->image_gc(name);
}

void
show_help_balloon (widget balloon, SI x, SI y) {
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
  (void) balloon; (void) x; (void) y;
}

void
show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
  the_gui->show_wait_indicator(base,message,argument);
}

font x_font (string family, int size, int dpi)
{
  (void) family; (void) size; (void) dpi;
  if (DEBUG_QT) cout << "x_font(): SHOULD NOT BE CALLED\n";
  return NULL;
}


QString 
QTMTranslator::translate ( const char * context, const char * sourceText, 
                           const char * disambiguation ) const 
{
  (void) disambiguation;  (void) context;
  if (DEBUG_QT) {
    cout << "Translating: " << sourceText << LF;
    cout << "Translation: " << qt_translate (sourceText) << LF;
  }
  return QString (to_qstring (qt_translate (sourceText)));
}
