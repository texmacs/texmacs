
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
#include "analyze.hpp"
#include <locale.h>
#include "language.hpp"
#include "message.hpp"
#include <QDesktopWidget>
#include <QClipboard>
#include <QFileOpenEvent>
#include <QSocketNotifier>
#include "QTMGuiHelper.hpp"
#include "qt_renderer.hpp" // for the_qt_renderer

#include "socket_server.hpp" // for number_of_servers

#include "Scheme/object.hpp"
//#include "TeXmacs/server.hpp" // for get_server

extern window (*get_current_window) (void);

qt_gui_rep* the_gui= NULL;
int nr_windows = 0; // FIXME: fake variable, referenced in tm_server
bool qt_update_flag = false;

time_t time_credit;
time_t timeout_time;

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
  interrupt_time= texmacs_time ();
  set_output_language (get_locale_language ());
  gui_helper = new QTMGuiHelper (this);
  qApp -> installEventFilter (gui_helper);
  updatetimer = new QTimer (gui_helper);
  updatetimer->setSingleShot (true);
  QObject::connect (updatetimer, SIGNAL(timeout()), gui_helper, SLOT(doUpdate()));
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
 // delete updatetimer; we do not need this given that gui_helper is the parent of updatetimer
}

/******************************************************************************
* interclient communication
******************************************************************************/

#if 0 // old code vs. Nemec patch for correct clipboard handling under X11
bool
qt_gui_rep::get_selection (string key, tree& t, string& s) {
  t= "none";
  s= "";
  if (selection_t->contains (key)) {
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
  if (key != "primary") return false;

  QClipboard *clipboard = QApplication::clipboard();
  QString originalText = clipboard->text();
  QByteArray buf = originalText.toAscii();
  if (!(buf.isEmpty())) {
    s << string(buf.constData(), buf.size());
  }

  t= tuple ("extern", s);
  return true;
}
#else
bool
qt_gui_rep::get_selection (string key, tree& t, string& s) {
  QClipboard *cb= QApplication::clipboard();
  bool owns= true;
  QClipboard::Mode mode;
  if (key == "primary") {
    owns= cb->ownsClipboard();
    mode= QClipboard::Clipboard;
  } else if (key == "mouse" && cb->supportsSelection()) {
    owns= cb->ownsSelection();
    mode= QClipboard::Selection;
  }
  s= "";
  t= "none";
  
  if (owns) {
    if (selection_t->contains (key)) {
      t= copy (selection_t [key]);
      s= copy (selection_s [key]);
      return true;
    }
    return false;
  }
  
  QString originalText = cb->text(mode);
  QByteArray buf = originalText.toAscii();
  if (!(buf.isEmpty())) {
    s << string(buf.constData(), buf.size());
  }
  
  t= tuple ("extern", s);
  return true;
}
#endif

bool
qt_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
        
                
  QClipboard *cb = QApplication::clipboard();
  QClipboard::Mode mode;
  if (key == "primary") {
    mode= QClipboard::Clipboard;
  } else if (key == "mouse" && cb->supportsSelection()) {
    mode= QClipboard::Selection;
  } else {
    return true;
  }

  char *selection = as_charp (s);
  cb->setText(selection,mode);
  tm_delete_array (selection);
  return true;
}

void
qt_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void qt_gui_rep::image_gc (string name) { (void) name; }
// FIXME: remove this unused function
void qt_gui_rep::set_mouse_pointer (string name) { (void) name; }
// FIXME: implement this function
void qt_gui_rep::set_mouse_pointer (string curs_name, string mask_name) { (void) curs_name; (void) mask_name; } ;

/******************************************************************************
* Main loop
******************************************************************************/

bool
qt_gui_rep::check_event (int type) {
  switch (type) {
  /*
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      time_t now= texmacs_time ();
      if (now - timeout_time < 0) return false;
      interrupted= true;
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  */
  default:
    return false;
  }
}

void
qt_gui_rep::show_wait_indicator (widget w, string message, string arg)  {
  (void) w; (void) message; (void) arg;
}

void (*the_interpose_handler) (void) = NULL;

void gui_interpose (void (*r) (void)) { the_interpose_handler= r; }

void
qt_gui_rep::update () {
  // this is called by doUpdate, which in turns is fired by a timer activated in 
  // needs_update, and ensuring that interpose_handler is run during a pass in the eventloop
  // afterwards we reactivate the timer with a pause (see FIXME below) 
  
  if (the_interpose_handler) the_interpose_handler();
  
  qt_update_flag = false;
  interrupted = false;  
  
  updatetimer->start (1000/6);
  
  // FIXME: we need to ensure that the interpose_handler is run at regular intervals (1/6th of sec)
  //        so that informations on the footbar are updated. (this should be better handled by 
  //        promoting code in tm_editor::apply_changes (which is activated only after idle periods)
  //        at the level of delayed commands in the gui.
  //        The interval cannot be too small to keep CPU usage low in idle state
} 



void
qt_gui_rep::event_loop () {
  QApplication *app = (QApplication*) QApplication::instance();

  updatetimer->start (0); // 0 ms - call immediately when all other events have been processed
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

  //  cout << "ADD NOTIFIER" << LF;
  
  // replace any already present notifier

  remove_notifier (sn);

  // installs both a read and a write notifier (the texmacs interface does not specify enough its needs)
  
  read_notifiers (sn) = (pointer) (qsn = new QSocketNotifier(sn->fd, QSocketNotifier::Read, gui_helper)); 
  QObject::connect(qsn, SIGNAL(activated(int)), gui_helper, SLOT(doSocketNotification(int)) );

  write_notifiers (sn) = (pointer) (qsn = new QSocketNotifier(sn->fd, QSocketNotifier::Write, gui_helper));
  QObject::connect(qsn, SIGNAL(activated(int)), gui_helper, SLOT(doSocketNotification(int)) );  
}

void 
qt_gui_rep::remove_notifier (socket_notifier sn)
{  
  QSocketNotifier *qsn;

  //  cout << "REMOVE NOTIFIER" << LF;

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

/******************************************************************************
 * Delayed commands
 ******************************************************************************/

QTMCommandHelper::QTMCommandHelper (object _cmd, int delay = 0) 
  : QObject (), cmd (_cmd),  timer () 
{
  QObject::connect (&timer, SIGNAL (timeout()), this, SLOT (doCommand()));
  timer.setSingleShot (true);
  timer.start (delay);
}

void
QTMCommandHelper::doCommand()
{
  object obj= call (cmd);
  if (is_int (obj)) {
    timer.start (as_int (obj));
  } 
  if (!(timer.isActive ())) deleteLater();
}

static QTimer *global_timer = NULL;


void
QTMGuiHelper::doCommands()
{
  exec_pending_commands ();
  the_gui->update ();
}

void restart_global_timer (int pause = 0) {
  if (!global_timer) {
    global_timer = new QTimer();
    QObject::connect (global_timer, SIGNAL (timeout()), the_gui->gui_helper, SLOT (doCommands()));
  }
  global_timer->start(pause);
}

#if 0
// the following code causes problems in an unpredicable way
// especially to keybindings and dead-key hanlding
// for the moment has been replaced by a unique timer
// and a queue of delayed commands much in the spirit of the
// X11 version.

static array <object> delayed_commands;

void   
exec_delayed (object cmd)
{ 
 delayed_commands << cmd;
  restart_global_timer ();
}

void   
exec_delayed_pause (object cmd)
{
  delayed_commands << cmd;
  restart_global_timer ();
}

void   exec_pending_commands ()
{
  // guarantee sequential execution of delayed commands 
  // otherwise some bugs appear in keyboard handling
  
  int i, n= N(delayed_commands);
  for (i=0; i<n; i++) {
    object obj= call (delayed_commands[i]);
    if (is_int (obj)) {
      new QTMCommandHelper(delayed_commands[i], as_int (obj));
    }
  }
  delayed_commands = array<object>(0);
}

#else
static array<object> delayed_queue;
static array<time_t>    start_queue;

void
exec_delayed (object cmd) {
  delayed_queue << cmd;
  start_queue << (((time_t) texmacs_time ()) - 1000000000);
  restart_global_timer ();
}

void
exec_delayed_pause (object cmd) {
  delayed_queue << cmd;
  start_queue << ((time_t) texmacs_time ());
  restart_global_timer ();
}

void
exec_pending_commands () {
  array<object> a= delayed_queue;
  array<time_t> b= start_queue;
  delayed_queue= array<object> (0);
  start_queue= array<time_t> (0);
  int i, n= N(a);
  for (i=0; i<n; i++) {
    time_t now= (time_t) texmacs_time ();
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
    time_t lapse = start_queue[0];
    int n = N(start_queue);
    for (i=1; i<n; i++) {
      if (lapse > start_queue[i]) lapse = start_queue[i];
    }
    lapse = lapse - (time_t) texmacs_time ();
    if (lapse < 0) lapse = 0;
    // cout << "restarting :" << lapse << LF;
    restart_global_timer (lapse);
  }
}

void
clear_pending_commands () {
  delayed_queue= array<object> (0);
  start_queue= array<time_t> (0);
}
#endif

/******************************************************************************
* Main routines
******************************************************************************/


void
gui_open (int& argc, char** argv) {
  // start the gui
 // new QApplication (argc,argv); now in texmacs.cpp
  the_gui = tm_new<qt_gui_rep> (argc, argv);
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
}

void
gui_root_extents (SI& width, SI& height) {
  // get the screen size
  the_gui->get_extents (width, height);
  if (DEBUG_EVENTS) cout << "gui_root_extents (" << width << "," << height << ")" << LF;
}

void
gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  the_gui->get_max_size (width, height);
  if (DEBUG_EVENTS) cout << "gui_maximal_extents (" << width << "," << height << ")" << LF;
}

void
gui_refresh () {
  // update and redraw all windows (e.g. on change of output language)
  // FIXME: add suitable code
}




/******************************************************************************
 * QTMGuiHelper methods
 ******************************************************************************/

void
QTMGuiHelper::doUpdate () {
//  cout << "UPDATE " << texmacs_time () << LF;
  gui->update();
}

bool
QTMGuiHelper::eventFilter (QObject *obj, QEvent *event) {
   if (event->type() == QEvent::FileOpen) {
     QFileOpenEvent *openEvent = static_cast<QFileOpenEvent *>(event);
     const char *s = openEvent->file().toAscii().constData();
     //qDebug ("File Open Event %s", s);
     call ("texmacs-load-buffer", object(url_system (s)), object("generic"), object(1), object(false));
     return true;
   } else {
     // standard event processing
     return QObject::eventFilter(obj, event);
   }
}


void
QTMGuiHelper::doSocketNotification (int socket) {
//  cout << "SOCKET NOTIFICATION " << socket << " "<< texmacs_time () << LF;
  iterator<socket_notifier> it = iterate (read_notifiers);
  while (it->busy ()) {
    socket_notifier sn= it->next ();
    if (sn->fd == socket) sn->notify();
  }
  it = iterate (write_notifiers);
  while (it->busy ()) {
    socket_notifier sn= it->next ();
    if (sn->fd == socket) sn->notify();
  }
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
get_default_font (bool tt) {
        (void) tt;      
  // get the default font or monospaced font (if tt is true)
        
  // return a null font since this function is not called in the Qt port.
  if (DEBUG_EVENTS) cout << "get_default_font(): SHOULD NOT BE CALLED\n";
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
        if (DEBUG_EVENTS) cout << "load_system_font(): SHOULD NOT BE CALLED\n";
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
  //cout << "needs_update" << LF;
  qt_update_flag = true;
  the_gui->updatetimer->start (0);
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
  if (DEBUG_EVENTS) cout << "x_font(): SHOULD NOT BE CALLED\n";
  return NULL;
}

