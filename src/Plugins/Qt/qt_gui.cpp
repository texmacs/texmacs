
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
#include "QTMGuiHelper.hpp"
#include "qt_renderer.hpp" // for the_qt_renderer

#include "socket_server.hpp" // for number_of_servers

#include "Guile/Scheme/object.hpp"
//#include "TeXmacs/server.hpp" // for get_server

extern window (*get_current_window) (void);

qt_gui_rep* the_gui= NULL;
int nr_windows = 0; // FIXME: fake variable, referenced in tm_server
bool qt_update_flag= false;

int time_credit;
int timeout_time;

/******************************************************************************
* Constructor and geometry
******************************************************************************/

qt_gui_rep::qt_gui_rep(int &argc, char **argv):
  interrupted (false), selection (NULL)
{
  (void) argc; (void) argv;
  // argc= argc2;
  // argv= argv2;
  interrupted   = false;
  interrupt_time= texmacs_time ();
  set_output_language (get_locale_language ());
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
}

/******************************************************************************
* interclient communication
******************************************************************************/

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

bool
qt_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  if (key == "primary") {
    //if (is_nil (windows_l)) return false;
    //Window win= windows_l->item;
    if (selection != NULL) tm_delete_array (selection);
    //XSetSelectionOwner (dpy, XA_PRIMARY, win, CurrentTime);
    //if (XGetSelectionOwner(dpy, XA_PRIMARY)==None) return false;
    selection= as_charp (s);
        
    QClipboard *clipboard = QApplication::clipboard();
    QString originalText = clipboard->text();
                
    clipboard->setText(selection);      
  }
  return true;
}

void
qt_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  if ((key == "primary") && (selection != NULL)) {
    tm_delete_array (selection);
    // FIXME: should we do something with the pasteboard?
    selection= NULL;
  }
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
//void set_interpose_handler (void (*r) (void)) { the_interpose_handler= r; }
void gui_interpose (void (*r) (void)) { the_interpose_handler= r; }

void
update () {
  if (the_interpose_handler) the_interpose_handler();
}

void
qt_gui_rep::update () {
  ::update();
  interrupted = false;
}

void
qt_gui_rep::event_loop () {
  QApplication *app = (QApplication*) QApplication::instance();
  QTimer t (NULL);
  t.start (25);

  time_credit= 1000000;
  while (nr_windows > 0 || number_of_servers () != 0) {
    timeout_time= texmacs_time () + time_credit;
//    app->processEvents (QEventLoop::WaitForMoreEvents | QEventLoop::DeferredDeletion);
    app->processEvents (QEventLoop::WaitForMoreEvents);
    //int start= texmacs_time ();
    update ();
    //int end= texmacs_time ();
    //if (end > start) cout << "Update " << end - start << "\n";
    time_credit= min (1000000, 2 * time_credit);
    qt_update_flag= false;
  }
  //FIXME: QCoreApplication sends aboutToQuit signal before exiting...
  app->sendPostedEvents (0, QEvent::DeferredDelete);
}

/******************************************************************************
* Main routines
******************************************************************************/

QTMGuiHelper *gui_helper;


void
gui_open (int& argc, char** argv) {
  // start the gui
 // new QApplication (argc,argv); now in texmacs.cpp
  the_gui = tm_new<qt_gui_rep> (argc, argv);
  
  gui_helper = new QTMGuiHelper (the_gui);
  qApp -> installEventFilter (gui_helper);
  
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
}

void
gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  the_gui->get_max_size (width, height);
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
QTMGuiHelper::doUpdate() {
  gui->update();
}

bool
QTMGuiHelper::eventFilter (QObject *obj, QEvent *event) {
   if (event->type() == QEvent::FileOpen) {
     QFileOpenEvent *openEvent = static_cast<QFileOpenEvent *>(event);
     const char *s = openEvent->file().toAscii().constData();
     qDebug ("File Open Event %s", s);
     call ("texmacs-load-buffer", object(url_system (s)), object("generic"), object(1), object(false));
     
     return true;
   } else {
     // standard event processing
     return QObject::eventFilter(obj, event);
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
  qt_update_flag= true;
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

