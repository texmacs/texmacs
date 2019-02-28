
/******************************************************************************
 * MODULE     : qt_gui.cpp
 * DESCRIPTION: QT display class
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <locale.h>

#include "convert.hpp"
#include "iterator.hpp"
#include "dictionary.hpp"
#include "file.hpp" // added for copy_as_graphics
#include "analyze.hpp"
#include "language.hpp"
#include "message.hpp"
#include "scheme.hpp"
#include "tm_window.hpp"
#include "new_window.hpp"
#include "boot.hpp"

#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp" // for the_qt_renderer
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"

#include <QDesktopWidget>
#include <QClipboard>
#include <QBuffer>
#include <QFileOpenEvent>
#include <QStackedLayout>
#include <QLabel>
#include <QSocketNotifier>
#include <QSetIterator>
#include <QTranslator>
#include <QLocale>
#include <QMimeData>
#include <QByteArray>
#include <QCoreApplication>
#include <QLibraryInfo>
#include <QImage>
#include <QUrl>
#include <QDesktopWidget>

#include "QTMGuiHelper.hpp"
#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
#include "QTMApplication.hpp"

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif

qt_gui_rep* the_gui = NULL;
int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

/******************************************************************************
* FIXME: temporary hack by Joris
* Additional wait mechanism to keep CPU usage down
******************************************************************************/

#ifdef QT_CPU_FIX
#include <unistd.h>

static double tm_count= 0.0;
static double tm_delay= 1.0;

void
tm_wake_up () {
  tm_delay= 1.0;
}

void
tm_sleep () {
  //tm_count += 1.0;
  tm_delay= 1.0001 * tm_delay;
  if (tm_delay > 250000.0) tm_delay= 250000;
  //cout << tm_count << ", " << tm_delay << "\r";
  //cout.flush ();
  usleep ((int) floor (tm_delay));
}
#endif

/******************************************************************************
* Constructor and geometry
******************************************************************************/

qt_gui_rep::qt_gui_rep (int &argc, char **argv):
interrupted (false), waitWindow (NULL), popup_wid_time (0), q_translator (0),
time_credit (100), do_check_events (false), updating (false), 
needing_update (false)
{
  (void) argc; (void) argv;
    // argc = argc2;
    // argv = argv2;
  
  interrupted  = false;
  time_credit  = 100;
  timeout_time = texmacs_time () + time_credit;
  
    //waitDialog = NULL;
  
  gui_helper = new QTMGuiHelper (this);
  qApp->installEventFilter (gui_helper);
  
#ifdef QT_MAC_USE_COCOA
    //HACK: this filter is needed to overcome a bug in Qt/Cocoa
  extern void mac_install_filter(); // defined in src/Plugins/MacOS/mac_app.mm
  mac_install_filter();
#endif
  
  set_output_language (get_locale_language ());
  refresh_language();
  
  updatetimer = new QTimer (gui_helper);
  updatetimer->setSingleShot (true);
  QObject::connect (updatetimer, SIGNAL (timeout()),
                    gui_helper, SLOT (doUpdate()));
  // (void) default_font ();

  if (!retina_manual) {
    retina_manual= true;
#ifdef MACOSX_EXTENSIONS
    double mac_hidpi = mac_screen_scale_factor();
    if (DEBUG_STD)
      debug_boot << "Mac Screen scaleFfactor: " << mac_hidpi <<  "\n";
          
    if (mac_hidpi == 2) {
      if (DEBUG_STD) debug_boot << "Setting up HiDPI mode\n";
      retina_factor= 2;
      retina_scale = 1.4;
      if (!retina_iman) {
        retina_iman  = true;
        retina_icons = 2;
        // retina_icons = 1;
        // retina_icons = 2;  // FIXME: why is this not better?
      }
    }
#else
    SI w, h;
    get_extents (w, h);
    if (DEBUG_STD)
      debug_boot << "Screen extents: " << w/PIXEL << " x " << h/PIXEL << "\n";
    if (min (w, h) >= 1440 * PIXEL) {
      retina_factor= 2;
      retina_scale = 1.4;
      if (!retina_iman) {
        retina_iman  = true;
        retina_icons = 2;
      }
    }
#endif
  }
  if (has_user_preference ("retina-factor"))
    retina_factor= get_user_preference ("retina-factor") == "on"? 2: 1;
  if (has_user_preference ("retina-icons"))
    retina_icons= get_user_preference ("retina-icons") == "on"? 2: 1;
  if (has_user_preference ("retina-scale"))
    retina_scale= as_double (get_user_preference ("retina-scale"));
}

/* important routines */
void
qt_gui_rep::get_extents (SI& width, SI& height) {
  coord2 size = from_qsize (QApplication::desktop()->size());
  width  = size.x1;
  height = size.x2;
}

void
qt_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height = 6000 * PIXEL;
}

qt_gui_rep::~qt_gui_rep()  {
  delete gui_helper;
  
  while (waitDialogs.count()) {
    waitDialogs.last()->deleteLater();
    waitDialogs.removeLast();
  }
  if (waitWindow) delete waitWindow;
  
    // delete updatetimer; we do not need this given that gui_helper is the
    // parent of updatetimer
}


/******************************************************************************
 * interclient communication
 ******************************************************************************/

bool
qt_gui_rep::get_selection (string key, tree& t, string& s, string format) {
  QClipboard *cb = QApplication::clipboard ();
  QClipboard::Mode mode = QClipboard::Clipboard;
  if (key == "primary" || (key == "mouse" && cb->supportsSelection ()))
    if (key == "mouse") mode = QClipboard::Selection;
  
  QString originalText = cb->text (mode);
  const QMimeData *md = cb->mimeData (mode);
  QByteArray buf;
  string input_format;
  
  s = "";
  t = "none";
    // Knowing when we owns (or not) the content is not clear
  bool owns = (format != "temp" && format != "wrapbuf" && key != "primary") &&
  !(key == "mouse" && cb->supportsSelection ());
  
  if (!owns && md->hasFormat ("application/x-texmacs-pid")) {
    buf = md->data ("application/x-texmacs-pid");
    if (!(buf.isEmpty())) {
      owns = string (buf.constData(), buf.size())
      == as_string (QCoreApplication::applicationPid ());
    }
  }
  
  if (owns) {
    if (!selection_t->contains (key)) return false;
    t = copy (selection_t [key]);
    s = copy (selection_s [key]);
    return true;
  }
  
  if (format == "default") {
    if (md->hasFormat ("application/x-texmacs-clipboard")) {
      buf = md->data ("application/x-texmacs-clipboard");
      input_format = "texmacs-snippet";
    }
    else if (md->hasImage ()) {
      if (md->hasUrls ()) {
        QList<QUrl> l= md->urls ();
        if (l.size () == 1) {
          s= from_qstring (l[0].toString ());
          input_format = "linked-picture";
        }
      }
      else {
        QBuffer qbuf(&buf);
        QImage image= qvariant_cast<QImage> (md->imageData());
        qbuf.open (QIODevice::WriteOnly);
        image.save (&qbuf, "PNG");
        input_format = "picture";
      }
    }
    else if (md->hasHtml ()) {
      buf = md->html().toUtf8 ();
      input_format = "html-snippet";
    }
    else if (md->hasFormat ("text/plain;charset=utf8")) {
      buf = md->data ("text/plain;charset=utf8");
      input_format = "verbatim-snippet";
    }
    else {
      buf = md->text().toUtf8 ();
      input_format = "verbatim-snippet";
    }
  }
  else if (format == "verbatim"
           && (get_preference ("verbatim->texmacs:encoding") == "utf-8" ||
               get_preference ("verbatim->texmacs:encoding") == "auto"  ))
    buf = md->text().toUtf8 ();
  else {
    if (md->hasFormat ("plain/text")) buf = md->data ("plain/text").data();
    else buf = md->text().toUtf8 ();
  }
  if (!(buf.isEmpty())) s << string (buf.constData(), buf.size());
  if (input_format == "html-snippet" && seems_buggy_html_paste (s))
    s = correct_buggy_html_paste (s);
  if (input_format != "picture" && seems_buggy_paste (s))
    s = correct_buggy_paste (s);
  if (input_format != "" &&
      input_format != "picture" &&
      input_format != "linked-picture")
    s = as_string (call ("convert", s, input_format, "texmacs-snippet"));
  if (input_format == "html-snippet") {
    tree t = as_tree (call ("convert", s, "texmacs-snippet", "texmacs-tree"));
    t = default_with_simplify (t);
    s = as_string (call ("convert", t, "texmacs-tree", "texmacs-snippet"));
  }
  if (input_format == "picture") {
    tree t (IMAGE);
    QSize size= qvariant_cast<QImage>(md->imageData()).size ();
    string w= as_string (size.width ()) * "px";
    string h= as_string (size.height ()) * "px";
    t << tuple (tree (RAW_DATA, s), "png") << w << h << "" << "";
    s= as_string (call ("convert", t, "texmacs-tree", "texmacs-snippet"));
  }
  if (input_format == "linked-picture") {
    tree im (IMAGE, s, "", "", "", "");
    s= as_string (call ("convert", im, "texmacs-tree", "texmacs-snippet"));
  }
  t = tuple ("extern", s);
  return true;
}

bool
qt_gui_rep::set_selection (string key, tree t,
                           string s, string sv, string sh, string format) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  
  QClipboard *cb = QApplication::clipboard ();
  QClipboard::Mode mode = QClipboard::Clipboard;
  if (key == "primary");
  else if (key == "mouse" && cb->supportsSelection())
    mode = QClipboard::Selection;
  else return true;
  cb->clear (mode);
  
  c_string selection (s);
  cb->setText (QString::fromLatin1 (selection), mode);
  QMimeData *md = new QMimeData;
  
  if (format == "verbatim" || format == "default") {
    if (format == "default") {
      md->setData ("application/x-texmacs-clipboard", (char*)selection);
      
      QString pid_str;
      pid_str.setNum (QCoreApplication::applicationPid ());
      md->setData ("application/x-texmacs-pid", pid_str.toLatin1());
      
      (void) sh;
        //selection = c_string (sh);
        //md->setHtml (selection);
        //tm_delete_array (selection);
      
      selection = c_string (sv);
    }
    
    string enc = get_preference ("texmacs->verbatim:encoding");
    if (enc == "auto")
      enc = get_locale_charset ();
    
    if (enc == "utf-8" || enc == "UTF-8")
      md->setText (QString::fromUtf8 (selection));
    else if (enc == "iso-8859-1" || enc == "ISO-8859-1")
      md->setText (QString::fromLatin1 (selection));
    else
      md->setText (QString::fromLatin1 (selection));
  }
  else
    md->setText (QString::fromLatin1 (selection));
  cb->setMimeData (md, mode);
    // according to the docs, ownership of mimedata is transferred to clipboard
    // so no memory leak here
  return true;
}

void
qt_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  
  QClipboard *cb = QApplication::clipboard();
  QClipboard::Mode mode = QClipboard::Clipboard;
  if (key == "primary");
  else if (key == "mouse" && cb->supportsSelection())
    mode = QClipboard::Selection;
  else return;
  
  bool owns = false;
  const QMimeData *md = cb->mimeData (mode);
  if (md) owns = md->hasFormat ("application/x-texmacs-clipboard");
  if (owns) cb->clear (mode);
}

/******************************************************************************
 * Miscellaneous
 ******************************************************************************/

void qt_gui_rep::set_mouse_pointer (string name) { (void) name; }
  // FIXME: implement this function
void qt_gui_rep::set_mouse_pointer (string curs_name, string mask_name)
{ (void) curs_name; (void) mask_name; } ;

/******************************************************************************
 * Main loop
 ******************************************************************************/

void
qt_gui_rep::show_wait_indicator (widget w, string message, string arg)  {
  if (DEBUG_QT)
    debug_qt << "show_wait_indicator \"" << message << "\"\"" << arg << "\"\n";
  
  qt_window_widget_rep* wid = static_cast<qt_window_widget_rep*> (w.rep);
  
    // we move the texmacs window during an operation.
    // We need to disable updates of the window to avoid erasure of the canvas
    // area
    //  wid->wid->setUpdatesEnabled (false);
  
    //FIXME: we must center the wait widget wrt the current active window
  
  if (!waitWindow) {
    waitWindow = new QWidget (wid->qwid->window());
    waitWindow->setWindowFlags (Qt::Window | Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);
    QStackedLayout *layout = new QStackedLayout();
    layout->setSizeConstraint (QLayout::SetFixedSize);
    waitWindow->setLayout (layout);
  }
  
  if (waitDialogs.count()) {
    waitWindow->layout()->removeWidget (waitDialogs.last());
  }
  
  if (N(message)) {
      // push a new wait message in the list
    
    if (arg != "") message = message * " " * arg * "...";
    
    QLabel* lab = new  QLabel();
    lab->setFocusPolicy (Qt::NoFocus);
    lab->setMargin (15);
    lab->setText (to_qstring (message));
    waitDialogs << lab;
  } else {
      // pop the next wait message from the list
    if (waitDialogs.count()) {
      waitDialogs.last()->deleteLater();
      waitDialogs.removeLast();
    }
  }
  
  if (waitDialogs.count()) {
    waitWindow->layout()->addWidget (waitDialogs.last());
    waitWindow->updateGeometry();
    {
      QSize sz = waitWindow->geometry().size();
      QRect rect = QRect (QPoint (0,0),sz);
        //HACK:
        // processEvents is needed to let Qt update windows coordinates in the case
      qApp->processEvents (QEventLoop::ExcludeUserInputEvents);
        //ENDHACK
      QPoint pt = wid->qwid->window()->geometry().center();
      rect.moveCenter (pt);
      waitWindow->move (rect.topLeft());
      
    }
    waitWindow->show();
    qApp->processEvents (QEventLoop::ExcludeUserInputEvents);
    waitWindow->repaint();
  } else {
    waitWindow->close();
  }
  qApp->processEvents();
  QApplication::flush();
  
  wid->qwid->activateWindow ();
  send_keyboard_focus (wid);
    // next time we do update the dialog will disappear
  need_update();
}

void (*the_interpose_handler) (void) = NULL;

void gui_interpose (void (*r) (void)) { the_interpose_handler = r; }

void
qt_gui_rep::event_loop () {
  QTMApplication* app = static_cast<QTMApplication*>(QApplication::instance());
  update();
    //need_update();
  app->exec();
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
  the_gui = NULL;
  
#ifdef MACOSX_EXTENSIONS
  mac_end_remote();
#endif
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
  the_gui->refresh_language();
}


/******************************************************************************
 * Queued processing
 ******************************************************************************/

/*!
 We process a maximum of max events. There are two kind of events: those
 which need a pass on interpose_handler just after and the others. We count
 only the first kind of events. In update() we call this function with
 max = 1 so that only one of these "sensible" events is handled. Otherwise
 updating the internal TeXmacs structure becomes very slow. This can be
 considered a limitation of the current implementation of interpose_handler
 Likewise this function is just a hack to get things working properly.
 */

static int keyboard_events = 0;
static int keyboard_special= 0;

void
qt_gui_rep::process_queued_events (int max) {
  int count = 0;
  while (max < 0 || count < max)  {
    const queued_event& ev = waiting_events.next();
    if (ev.x1 == qp_type::QP_NULL) break;
#ifdef QT_CPU_FIX
    if (ev.x1 != qp_type::QP_NULL &&
	ev.x1 != qp_type::QP_SOCKET_NOTIFICATION &&
	ev.x1 != qp_type::QP_DELAYED_COMMANDS)
      tm_wake_up ();
#endif
    switch (ev.x1) {
      case qp_type::QP_NULL :
        break;
      case qp_type::QP_KEYPRESS :
      {
        typedef triple<widget, string, time_t > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1)) {
          concrete_simple_widget (x.x1)->handle_keypress (x.x2, x.x3);
          keyboard_events++;
          if (N(x.x2) > 1) keyboard_special++;
        }
      }
        break;
      case qp_type::QP_KEYBOARD_FOCUS :
      {
        typedef triple<widget, bool, time_t > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          concrete_simple_widget (x.x1)->handle_keyboard_focus (x.x2, x.x3);
      }
        break;
      case qp_type::QP_MOUSE :
      {
        typedef quintuple<string, SI, SI, int, time_t > T1;
        typedef pair<widget, T1> T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          concrete_simple_widget (x.x1)->handle_mouse (x.x2.x1, x.x2.x2,
                                                       x.x2.x3, x.x2.x4, x.x2.x5);
      }
        break;
      case qp_type::QP_RESIZE :
      {
        typedef triple<widget, SI, SI > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          concrete_simple_widget (x.x1)->handle_notify_resize (x.x2, x.x3) ;
      }
        break;
      case qp_type::QP_COMMAND :
      {
        command cmd = open_box <command> (ev.x2) ;
        cmd->apply();
      }
        break;
      case qp_type::QP_COMMAND_ARGS :
      {
        typedef pair<command, object> T;
        T x = open_box <T> (ev.x2);
        x.x1->apply (x.x2);
      }
        break;
      case qp_type::QP_DELAYED_COMMANDS :
      {
        delayed_commands.exec_pending();
      }
        break;
        
      default:
        FAILED ("Unexpected queued event");
    }
    switch (ev.x1) {
      case qp_type::QP_COMMAND:
      case qp_type::QP_COMMAND_ARGS:
      case qp_type::QP_RESIZE:
      case qp_type::QP_DELAYED_COMMANDS:
        break;
      default:
        count++;
        break;
    }
  }
}

void
qt_gui_rep::process_keypress (qt_simple_widget_rep *wid, string key, time_t t) {
  typedef triple<widget, string, time_t > T;
  add_event (queued_event (qp_type::QP_KEYPRESS,
                           close_box<T> (T (wid, key, t))));
}

void
qt_gui_rep::process_keyboard_focus (qt_simple_widget_rep *wid, bool has_focus,
                                    time_t t ) {
  typedef triple<widget, bool, time_t > T;
  add_event (queued_event (qp_type::QP_KEYBOARD_FOCUS,
                           close_box<T> (T (wid, has_focus, t))));
}

void
qt_gui_rep::process_mouse (qt_simple_widget_rep *wid, string kind, SI x, SI y,
                           int mods, time_t t ) {
  typedef quintuple<string, SI, SI, int, time_t > T1;
  typedef pair<widget, T1> T;
  add_event (queued_event (qp_type::QP_MOUSE,
                           close_box<T> ( T (wid, T1 (kind, x, y, mods, t)))));
}

void
qt_gui_rep::process_resize (qt_simple_widget_rep *wid, SI x, SI y ) {
  typedef triple<widget, SI, SI > T;
  add_event (queued_event (qp_type::QP_RESIZE, close_box<T> (T (wid, x, y))));
}

void
qt_gui_rep::process_command (command _cmd) {
  add_event (queued_event (qp_type::QP_COMMAND, close_box<command> (_cmd)));
}

void
qt_gui_rep::process_command (command _cmd, object _args) {
  typedef pair<command, object > T;
  add_event (queued_event (qp_type::QP_COMMAND_ARGS,
                           close_box<T> (T (_cmd,_args))));
}

void
qt_gui_rep::process_delayed_commands () {
  add_event (queued_event (qp_type::QP_DELAYED_COMMANDS, blackbox()));
}

/*!
  FIXME: add more types and refine, compare with X11 version.
 */
bool
qt_gui_rep::check_event (int type) {
    // do not interrupt if not updating (e.g. while painting the icons in menus)
  if (!updating || !do_check_events) return false;
  
  switch (type) {
    case INTERRUPT_EVENT:
      if (interrupted) return true;
      else {
        time_t now = texmacs_time ();
        if (now - timeout_time < 0) return false;
        timeout_time = now + time_credit;
        interrupted  = !waiting_events.is_empty();
        return interrupted;
      }
    case INTERRUPTED_EVENT:
      return interrupted;
    default:
      return false;
  }
}

void
qt_gui_rep::set_check_events (bool enable_check) {
  do_check_events = enable_check;
}

void
qt_gui_rep::add_event (const queued_event& ev) {
  waiting_events.append (ev);
  if (updating) {
    needing_update = true;
  } else {
    need_update();
      // NOTE: we cannot update now since sometimes this seems to give problems
      // to the update of the window size after a resize. In that situation
      // sometimes when the window receives focus again, update will be called
      // for the focus_in event and interpose_handler is run which sends a
      // slot_extent message to the widget causing a wrong resize of the window.
      // This seems to cure the problem.
  }
}


/*!
 This is called by doUpdate(), which in turn is fired by a timer activated in
 needs_update(), and ensuring that interpose_handler() is run during a pass in
 the event loop after we reactivate the timer with a pause (see FIXME below).
 */

void
qt_gui_rep::update () {
#ifdef QT_CPU_FIX
  int std_delay= 1;
  tm_sleep ();
#else
  int std_delay= 1000 / 6;
#endif

  if (updating) {
    cout << "NESTED UPDATING: This should not happen" << LF;
    need_update();
    return;
  }
  
    // cout << "<" << texmacs_time() << " " << N(delayed_queue) << " ";
  
  updatetimer->stop();
  updating = true;
  
  static int count_events    = 0;
  static int max_proc_events = 100;
  
  time_t     now = texmacs_time();
  needing_update = false;
  time_credit    = 100 / (waiting_events.size() + 1);
  
    // 1.
    // Check if a wait dialog is active and in that case remove it.
    // If we are here then the long operation has finished.
  
  if (waitDialogs.count()) {
    waitWindow->layout()->removeWidget (waitDialogs.last());
    waitWindow->close();
    while (waitDialogs.count()) {
      waitDialogs.last()->deleteLater();
      waitDialogs.removeLast();
    }
  }
  
  if (popup_wid_time > 0 && now > popup_wid_time) {
    popup_wid_time = 0;
    _popup_wid->send (SLOT_VISIBILITY, close_box<bool> (true));
  }
  
    // 2.
    // Manage delayed commands
  
  if (delayed_commands.must_wait (now))
    process_delayed_commands();
  
    // 3.
    // If there are pending events in the private queue process them until the
    // limit in processed events is reached.
    // If there are no events or the limit is reached then proceed to a redraw.
  
  if (waiting_events.size() == 0) {
      // If there are no waiting events call the interpose handler at least once
    //if (the_interpose_handler) the_interpose_handler();
  }
  else while (waiting_events.size() > 0 && count_events < max_proc_events) {
    process_queued_events (1);
    count_events++;
    //if (the_interpose_handler) the_interpose_handler();
  }
  // Repaint invalid regions and redraw
  bool postpone_treatment= (keyboard_events > 0 && keyboard_special == 0);
  keyboard_events = 0;
  keyboard_special= 0;
  count_events    = 0;
  
  interrupted  = false;
  timeout_time = texmacs_time() + time_credit;

  if (!postpone_treatment) {
    if (the_interpose_handler) the_interpose_handler();
    qt_simple_widget_rep::repaint_all ();
  }
  
  if (waiting_events.size() > 0) needing_update = true;
  if (interrupted)               needing_update = true;
  if (nr_windows == 0)           qApp->quit();
  
  time_t delay = delayed_commands.lapse - texmacs_time();
  if (needing_update) delay = 0;
  else                delay = max (0, min (std_delay, delay));
  if (postpone_treatment) delay= 100; // NOTE: force occasional display
 
  updatetimer->start (delay);
  updating = false;
  
    // FIXME: we need to ensure that the interpose_handler is run at regular
    //        intervals (1/6th of sec) so that informations on the footbar are
    //        updated. (this should be better handled by promoting code in
    //        tm_editor::apply_changes (which is activated only after idle
    //        periods) at the level of delayed commands in the gui.
    //        The interval cannot be too small to keep CPU usage low in idle state
}

void
qt_gui_rep::force_update() {
  if (updating) needing_update = true;
  else          update();
}

void
qt_gui_rep::need_update () {
  if (updating) needing_update = true;
  else          updatetimer->start (0);
    // 0 ms - call immediately when all other events have been processed
}

void needs_update () {
  the_gui->need_update();
}

/*! Called upon change of output language.
 
 We currently emit a signal which forces every QTMAction to change his text
 according to the new language, but the preferred Qt way seems to use
 LanguageChange events (these are triggered upon installation of QTranslators)
 */
void
qt_gui_rep::refresh_language() {
  /* FIXME: why is this here? We don't use QTranslators...
  QTranslator* qtr = new QTranslator();
  if (qtr->load ("qt_" +
                 QLocale (to_qstring 
                           (language_to_locale (get_output_language()))).name(),
                 QLibraryInfo::location (QLibraryInfo::TranslationsPath))) {
    if (q_translator)
      qApp->removeTranslator (q_translator);
    qApp->installTranslator (qtr);
    q_translator = qtr;
  } else {
    delete qtr;
  }
   */
  gui_helper->doRefresh();
}

/*! Display a popup help balloon (i.e. a tooltip) at window coordinates x, y
 
 We use a dedicated wrapper QWidget which handles mouse events: as soon as the
 mouse is moved out of we hide it.
 Problem: the widget need not appear below the mouse pointer, thus making it
 impossible to access links or widgets inside it.
 Solution?? as soon as the mouse moves (out of the widget), start a timer,
 giving enough time to the user to move (back) into the widget, then abort the
 close operation if he gets there.
 */
void
qt_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  if (popup_wid_time > 0) return;
  
  _popup_wid = popup_window_widget (wid, "Balloon");
  SI winx, winy;
  // HACK around wrong? reporting of window widget for embedded texmacs-inputs:
  // call get_window on the current window (concrete_window()->win is set to
  // the texmacs-input widget whenever there is one)
  get_position (get_window (concrete_window()->win), winx, winy);
  set_position (_popup_wid, x+winx, y+winy);
  popup_wid_time = texmacs_time() + 666;
    // update() will eventually show the widget
}


/******************************************************************************
 * Font support
 ******************************************************************************/

/*! Sets the name of the default font.
 @note This is ignored since Qt handles fonts for the widgets.
 */
void
set_default_font (string name) {
  (void) name;
}

/*! Gets the default font or monospaced font (if tt is true).
 @return A null font since this function is not called in the Qt port.
 */
font
get_default_font (bool tt, bool mini, bool bold) {
  (void) tt; (void) mini; (void) bold;
  if (DEBUG_QT) debug_qt << "get_default_font(): SHOULD NOT BE CALLED\n";
  return NULL;  //return tex_font (this, "ecrm", 10, 300, 0);
}

/*! Loads the metric and glyphs of a system font.
 You are not forced to provide any system fonts.
 */
void
load_system_font (string fam, int sz, int dpi, font_metric& fnm, font_glyphs& fng)
{
  (void) fam; (void) sz; (void) dpi; (void) fnm; (void) fng;
  if (DEBUG_QT) debug_qt << "load_system_font(): SHOULD NOT BE CALLED\n";
}


/******************************************************************************
 * Clipboard support
 ******************************************************************************/

bool
set_selection (string key, tree t,
               string s, string sv, string sh, string format) {
    // Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
    // and possibly the variants 'sv' and 'sh' for verbatim and html
    // Returns true on success
  return the_gui->set_selection (key, t, s, sv, sh, format);
}

bool
get_selection (string key, tree& t, string& s, string format) {
    // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
    // Returns true on success; sets t to (extern s) for external selections
  return the_gui->get_selection (key, t, s, format);
}

void
clear_selection (string key) {
    // Clear the selection on clipboard 'cb'
  the_gui->clear_selection (key);
}

bool
qt_gui_rep::put_graphics_on_clipboard (url file) {
  string extension = suffix (file) ;
  
    // for bitmaps this works :
  if ((extension == "bmp") || (extension == "png") ||
      (extension == "jpg") || (extension == "jpeg")) {
    QClipboard *clipboard = QApplication::clipboard();
    c_string tmp (concretize (file));
    clipboard->setImage (QImage (QString (tmp)));
  }
  else {
      // vector formats
      // Are there applications receiving eps, pdf,... through the clipboard?
      // I have not experimented with EMF/WMF (windows) or SVM (Ooo)
    QString mime ="image/*"; // generic image format;
    if (extension == "eps") mime = "application/postscript";
    if (extension == "pdf") mime = "application/pdf";
    if (extension == "svg") mime = "image/svg+xml"; //this works with Inskcape version >= 0.47
    
    string filecontent;
    load_string (file, filecontent, true);
    
    c_string tmp (filecontent);
    QByteArray rawdata (tmp);
    
    QMimeData *mymimeData = new QMimeData;
    mymimeData->setData (mime, rawdata);
    
    QClipboard *clipboard = QApplication::clipboard();
    clipboard->setMimeData (mymimeData);// default mode = QClipboard::Clipboard
  }
  return true;
}

/******************************************************************************
 * Miscellaneous
 ******************************************************************************/
int char_clip = 0;

void
beep () {
    // Issue a beep
  QApplication::beep();
}

bool
check_event (int type) {
    // Check whether an event of one of the above types has occurred;
    // we check for keyboard events while repainting windows
  return the_gui->check_event (type);
}

void
show_help_balloon (widget balloon, SI x, SI y) {
    // Display a help balloon at position (x, y); the help balloon should
    // disappear as soon as the user presses a key or moves the mouse
  the_gui->show_help_balloon (balloon, x, y);
}

void
show_wait_indicator (widget base, string message, string argument) {
    // Display a wait indicator with a message and an optional argument
    // The indicator might for instance be displayed at the center of
    // the base widget which triggered the lengthy operation;
    // the indicator should be removed if the message is empty
  the_gui->show_wait_indicator (base, message, argument);
}

void
external_event (string type, time_t t) {
    // External events, such as pushing a button of a remote infrared commander
  QTMWidget *tm_focus = qobject_cast<QTMWidget*>(qApp->focusWidget());
  if (tm_focus) {
    simple_widget_rep* wid = tm_focus->tm_widget();
    if (wid) the_gui -> process_keypress (wid, type, t);
  }
}


/******************************************************************************
 * Delayed commands
 ******************************************************************************/

command_queue::command_queue() : lapse (0), wait (true) { }
command_queue::~command_queue() { clear_pending(); /* implicit */ }

void
command_queue::exec (object cmd) {
  q << cmd;
  start_times << (((time_t) texmacs_time ()) - 1000000000);
  lapse = texmacs_time();
  the_gui->need_update();
  wait= true;
}

void
command_queue::exec_pause (object cmd) {
  q << cmd;
  start_times << ((time_t) texmacs_time ());
  lapse = texmacs_time();
  the_gui->need_update();
  wait= true;
}

void
command_queue::exec_pending () {
  array<object> a = q;
  array<time_t> b = start_times;
  q = array<object> (0);
  start_times = array<time_t> (0);
  int i, n = N(a);
  for (i = 0; i<n; i++) {
    time_t now =  texmacs_time ();
    if ((now - b[i]) >= 0) {
      object obj = call (a[i]);
      if (is_int (obj) && (now - b[i] < 1000000000)) {
        time_t pause = as_int (obj);
          //cout << "pause = " << obj << "\n";
        q << a[i];
        start_times << (now + pause);
      }
    }
    else {
      q << a[i];
      start_times << b[i];
    }
  }
  if (N(q) > 0) {
    wait = true;  // wait_for_delayed_commands
    lapse = start_times[0];
    int n = N(start_times);
    for (i = 1; i<n; i++) {
      if (lapse > start_times[i]) lapse = start_times[i];
    }
  } else
    wait = false;
}

void
command_queue::clear_pending () {
  q = array<object> (0);
  start_times = array<time_t> (0);
  wait = false;
}

bool
command_queue::must_wait (time_t now) const {
  return wait && (lapse <= now);
}


/******************************************************************************
 * Delayed commands interface
 ******************************************************************************/

void exec_delayed (object cmd) {
  the_gui->delayed_commands.exec(cmd);
}
void exec_delayed_pause (object cmd) {
  the_gui->delayed_commands.exec_pause(cmd);
}
void clear_pending_commands () {
  the_gui->delayed_commands.clear_pending();
}


/******************************************************************************
 * Queued events
 ******************************************************************************/

event_queue::event_queue() : n(0) { }

void
event_queue::append (const queued_event& ev) {
  q << ev;
  ++n;
}

queued_event
event_queue::next () {
  if (is_nil(q))
    return queued_event();
  queued_event ev = q->item;
  q = q->next;
  --n;
  return ev;
}

bool
event_queue::is_empty() const {
  ASSERT (!(n!=0 && is_nil(q)), "WTF?");
  return n == 0;
}

int
event_queue::size() const {
  return n;
}


