
/******************************************************************************
 * MODULE     : qtwk_gui.cpp
 * DESCRIPTION: QT/Widkit display class
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
#include "locale.hpp"
#include "message.hpp"
#include "blackbox.hpp"
#include "ntuple.hpp"
#include "scheme.hpp"
#include "tm_window.hpp"
#include "new_window.hpp"
#include "boot.hpp"

#include "qtwk_gui.hpp"
#include "Qt/qt_utilities.hpp"
#include "Qt/qt_renderer.hpp" // for the_qt_renderer
//#include "qtwk_simple_widget.hpp"
#include "qtwk_window.hpp"
#include "widget.hpp"

#include <QtGui>

#include "QTWKGuiHelper.hpp"
#include "QTWKWindow.hpp"

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif

qtwk_gui_rep* the_gui = NULL;
int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

inline simple_widget_rep* concrete_simple_widget (widget w) {
  return static_cast<simple_widget_rep*>(w.rep);
}


#if 0
int
qt_zoom (int sz) {
  return (int) (retina_scale * ((double) sz));
}
#endif

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

qtwk_gui_rep::qtwk_gui_rep (int &argc, char **argv):
interrupted (false), popup_wid_time (0), q_translator (0),
time_credit (100), do_check_events (false), updating (false), 
needing_update (false) //waitWindow (NULL), 
{
  (void) argc; (void) argv;
    // argc = argc2;
    // argv = argv2;
  
  interrupted  = false;
  time_credit  = 100;
  timeout_time = texmacs_time () + time_credit;
  
    //waitDialog = NULL;
  
  gui_helper = new QTWKGuiHelper (this);
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
    double ratio= QGuiApplication::primaryScreen()->devicePixelRatio();
    if (DEBUG_STD)
      debug_boot << "Screen pixel ratio: " << ratio << "\n";
    if (ratio > 1) {
#ifdef __EMSCRIPTEN__
      retina_factor= 2;
#endif
      retina_zoom = 2;
      retina_scale= 1.0;
      if (!retina_iman) {
        retina_iman  = true;
        retina_icons = 2;
      }
    }
#endif
  }
  if (has_user_preference ("retina-factor"))
    retina_factor= get_user_preference ("retina-factor") == "on"? 2: 1;
  if (has_user_preference ("retina-zoom"))
    retina_zoom= get_user_preference ("retina-zoom") == "on"? 2: 1;
  if (has_user_preference ("retina-icons"))
    retina_icons= get_user_preference ("retina-icons") == "on"? 2: 1;
  if (has_user_preference ("retina-scale"))
    retina_scale= as_double (get_user_preference ("retina-scale"));
}

/* important routines */
void
qtwk_gui_rep::get_extents (SI& width, SI& height) {
  coord2 size = from_qsize (QGuiApplication::primaryScreen()->availableSize());
  width  = size.x1;
  height = size.x2;
}

void
qtwk_gui_rep::get_max_size (SI& width, SI& height) {
  coord2 size = from_qsize (QSize (2000,1000));
  width  = size.x1;
  height = size.x2;
}

qtwk_gui_rep::~qtwk_gui_rep()  {
  delete gui_helper;

#if 0  
  while (waitDialogs.count()) {
    waitDialogs.last()->deleteLater();
    waitDialogs.removeLast();
  }
  if (waitWindow) delete waitWindow;
#endif
    // delete updatetimer; we do not need this given that gui_helper is the
    // parent of updatetimer
}


/******************************************************************************
* Grabbing
******************************************************************************/

static unsigned int
get_button_state () {
  unsigned int i= 0;
  Qt::MouseButtons bstate= QGuiApplication::mouseButtons ();
  Qt::KeyboardModifiers kstate= QGuiApplication::keyboardModifiers ();
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
  if ((bstate & Qt::MiddleButton   ) != 0) i += 2;
  if ((bstate & Qt::RightButton    ) != 0) i += 4;
  if ((bstate & Qt::XButton1       ) != 0) i += 8;
  if ((bstate & Qt::XButton2       ) != 0) i += 16;
#ifdef Q_OS_MAC
    // We emulate right and middle clicks with ctrl and option, but we pass the
    // modifiers anyway: old code continues to work and new one can use them.
  if ((kstate & Qt::MetaModifier   ) != 0) i = 1024+4; // control key
  if ((kstate & Qt::AltModifier    ) != 0) i = 2048+2; // option key
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 4096;   // cmd key
#else
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 1024;
  if ((kstate & Qt::AltModifier    ) != 0) i += 2048;
  if ((kstate & Qt::MetaModifier   ) != 0) i += 4096;
#endif
  return i;
}




void
qtwk_gui_rep::emulate_leave_enter (widget old_widget, widget new_widget) {
  int state = get_button_state ();
  SI x,y;
  QPoint pt= get_qtwk_window (old_widget)->win->mapFromGlobal (QCursor::pos());
  x= (pt.x() * PIXEL);
  y= ((-pt.y()) * PIXEL);
  // cout << "\nLeave " << old_widget << "\n";
  send_mouse (old_widget, "leave", x, y, state, 0);
  // cout << "Leave OK\n";

  pt= get_qtwk_window (new_widget)->win->mapFromGlobal (QCursor::pos());
  x= (pt.x() * PIXEL);
  y= ((-pt.y()) * PIXEL);
  // cout << "Enter " << new_widget << "\n";
  send_mouse (new_widget, "enter", x, y, state, 0);
  // cout << "Enter OK\n\n";
}



string
pritty (tree t) {
  if (is_atomic (t)) return copy (as_string (t));
  else if (N(t) == 2) return pritty (t[1]);
  else {
    int i;
    string s ("(");
    for (i=1; i<N(t); i++) {
      if (i>1) s << " ";
      s << pritty (t[i]);
    }
    s << ")";
    return s;
  }
}


void
qtwk_gui_rep::obtain_mouse_grab (widget wid) {
  QWindow* win= get_qtwk_window (wid)->win;
  if ((!is_nil (grab_ptr)) && (wid==grab_ptr->item)) return;
  widget old_widget; if (!is_nil (grab_ptr)) old_widget= grab_ptr->item;
  grab_ptr= list<widget> (wid, grab_ptr);
  widget new_widget= grab_ptr->item;
  notify_mouse_grab (new_widget, true);
  win->setMouseGrabEnabled (true);
//   cout << "\n---> In grab " << pritty ((tree) wid) << "\n\n";
//  cout << "\n---> In grab " << wid << "\n\n";
  if (!is_nil (old_widget)) {
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

void
qtwk_gui_rep::release_mouse_grab () {
  if (is_nil (grab_ptr)) return;
  widget old_widget= grab_ptr->item;
  grab_ptr= grab_ptr->next;
  widget new_widget; if (!is_nil (grab_ptr)) new_widget= grab_ptr->item;
  if (is_nil (grab_ptr)) {
    get_qtwk_window (old_widget)->win->setMouseGrabEnabled (false);
  //  cout << "\n---> No grab\n\n";
  }
  else {
    qtwk_window grab_win= get_qtwk_window (new_widget);
    notify_mouse_grab (new_widget, true);
    grab_win->win->setMouseGrabEnabled (true);
  //   cout << "\n---> In grab " << new_widget << "\n";
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

bool
qtwk_gui_rep::has_mouse_grab (widget w) {
  return (!is_nil (grab_ptr)) && (grab_ptr->item == w);
}


/******************************************************************************
 * interclient communication
 ******************************************************************************/

bool
qtwk_gui_rep::get_selection (string key, tree& t, string& s, string format) {
  QClipboard *cb = QGuiApplication::clipboard ();
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
    int ww= size.width (), hh= size.height ();
    string w, h;
    qt_pretty_image_size (ww, hh, w, h);
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
qtwk_gui_rep::set_selection (string key, tree t,
                           string s, string sv, string sh, string format) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  
  QClipboard *cb = QGuiApplication::clipboard ();
  QClipboard::Mode mode = QClipboard::Clipboard;
  if (key == "primary");
  else if (key == "mouse" && cb->supportsSelection())
    mode = QClipboard::Selection;
  else return true;
  cb->clear (mode);
  
  c_string selection (s);
  int ns = N(s);
  cb->setText (QString::fromLatin1 (selection, ns), mode);
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
      ns = N(sv);
    }
    
    string enc = get_preference ("texmacs->verbatim:encoding");
    if (enc == "auto")
      enc = get_locale_charset ();
    
    if (enc == "utf-8" || enc == "UTF-8")
      md->setText (QString::fromUtf8 (selection, ns));
    else if (enc == "iso-8859-1" || enc == "ISO-8859-1")
      md->setText (QString::fromLatin1 (selection, ns));
    else
      md->setText (QString::fromLatin1 (selection, ns));
  }
  else
    md->setText (QString::fromLatin1 (selection, ns));
  cb->setMimeData (md, mode);
    // according to the docs, ownership of mimedata is transferred to clipboard
    // so no memory leak here
  return true;
}

void
qtwk_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  
  QClipboard *cb = QGuiApplication::clipboard();
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

void qtwk_gui_rep::set_mouse_pointer (string name) { (void) name; }
  // FIXME: implement this function
void qtwk_gui_rep::set_mouse_pointer (string curs_name, string mask_name)
{ (void) curs_name; (void) mask_name; } ;


void
qtwk_gui_rep::created_window (qtwk_window win) {
  windows_l << win;
}

void
qtwk_gui_rep::deleted_window (qtwk_window win) {
  windows_l= remove (windows_l, win);
}

void
qtwk_gui_rep::focussed_window (qtwk_window win) {
  windows_l= list<qtwk_window> (win, remove (windows_l, win));
}


/******************************************************************************
 * Main loop
 ******************************************************************************/


void
qtwk_gui_rep::show_wait_indicator (widget w, string message, string arg) {
  if (DEBUG_QT)
    debug_qt << "show_wait_indicator \"" << message << "\"\"" << arg << "\"\n";

  // NOTE: the wait indicator is directly displayed inside the window
  // corresponding to w. We explicitly shortcut the main event loop
  // by invalidating the wait widget and requesting a redraw.
  // Using a popup window does not work, because it would be necessary
  // to return to the main loop to map and redraw it.
  qtwk_window ww= get_qtwk_window (w);
  if (ww == NULL || message == "") return;
  if (arg != "") message= message * " " * arg * "...";
  SI width= 400*PIXEL, height= 160*PIXEL;
  widget wait_wid= wait_widget (width, height, message);
  SI win_w, win_h;
  ww->get_size(win_w, win_h);
  SI mid_x= (win_w>>1)*PIXEL,
  mid_y= -(win_h>>1)*PIXEL + height;
  SI x= mid_x- width/2, y= mid_y- height/2;
  widget old_wid= ww->w;
  ww->w= wait_wid;
  set_position (wait_wid, x, y);
  set_identifier (wait_wid, ww->win_id);
  send_invalidate_all (wait_wid);
  ww->repaint_invalid_regions ();
  ww->w= old_wid;
  //XFlush (dpy);
  send_invalidate_all (old_wid);
}

void (*the_interpose_handler) (void) = NULL;

void gui_interpose (void (*r) (void)) { the_interpose_handler = r; }

void
qtwk_gui_rep::event_loop () {
//  QTMApplication* app = static_cast<QTMApplication*>(QGuiApplication::instance());
 // QCoreApplication* app = QGuiApplication::instance();
  update ();
  //need_update ();
  qApp->exec();
}


/******************************************************************************
 * Main routines
 ******************************************************************************/

void
gui_open (int& argc, char** argv) {
    // start the gui
    // new QGuiApplication (argc,argv); now in texmacs.cpp
  the_gui = tm_new<qtwk_gui_rep> (argc, argv);
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

string
gui_version () {
#if (QT_VERSION >= 0x060000)
  return "qt6";
#else
#if (QT_VERSION >= 0x050000)
  return "qt5";
#else
  return "qt4";
#endif
#endif
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
qtwk_gui_rep::process_queued_events (int max) {
  int count = 0;
  while (max < 0 || count < max)  {
    const queued_event& ev = waiting_events.next ();
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
        if (get_qtwk_window (x.x1)) {
          get_qtwk_window (x.x1)->key_event (x.x2);
          // send_keyboard (x.x1, x.x2);
//          concrete_simple_widget (x.x1)->handle_keypress (x.x2, x.x3);
          keyboard_events++;
          if (N(x.x2) > 1) keyboard_special++;
        }
      }
        break;
      case qp_type::QP_KEYBOARD_FOCUS :
      {
        typedef triple<widget, bool, time_t > T;
        T x = open_box <T> (ev.x2);
        if (get_qtwk_window (x.x1)) {
           if (x.x2)
            get_qtwk_window (x.x1)->focus_in_event ();
          else
            get_qtwk_window (x.x1)->focus_out_event ();
        }
//          send_keyboard_focus (x.x1, x.x2);
//          concrete_simple_widget (x.x1)->handle_keyboard_focus (x.x2, x.x3);
      }
        break;
      case qp_type::QP_MOUSE :
      {
        typedef quintuple<string, SI, SI, int, time_t > T1;
        typedef pair<widget, T1> T;
        T x = open_box <T> (ev.x2);
        if (get_qtwk_window (x.x1))
          get_qtwk_window (x.x1) ->
            mouse_event (x.x2.x1, x.x2.x2, x.x2.x3, x.x2.x4, x.x2.x5);
//          send_mouse (x.x1, x.x2.x1, x.x2.x2, x.x2.x3, x.x2.x4, x.x2.x5);
//          concrete_simple_widget (x.x1)->handle_mouse (x.x2.x1, x.x2.x2,
//                                                       x.x2.x3, x.x2.x4, x.x2.x5);
      }
        break;
      case qp_type::QP_RESIZE :
      {
        typedef triple<widget, SI, SI > T;
        T x = open_box <T> (ev.x2);
        if (get_qtwk_window (x.x1))
          get_qtwk_window (x.x1)->resize_event (x.x2, x.x3);
          //FIXME:
          ; // concrete_simple_widget (x.x1)->handle_notify_resize (x.x2, x.x3) ;
      }
        break;
      case qp_type::QP_COMMAND :
      {
        command cmd = open_box <command> (ev.x2) ;
        cmd->apply ();
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
        delayed_commands.exec_pending ();
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
qtwk_gui_rep::process_keypress (widget wid, string key, time_t t) {
  typedef triple<widget, string, time_t > T;
  add_event (queued_event (qp_type::QP_KEYPRESS,
                           close_box<T> (T (wid, key, t))));
}

void
qtwk_gui_rep::process_keyboard_focus (widget wid, bool has_focus,
                                    time_t t ) {
  typedef triple<widget, bool, time_t > T;
  add_event (queued_event (qp_type::QP_KEYBOARD_FOCUS,
                           close_box<T> (T (wid, has_focus, t))));
}

void
qtwk_gui_rep::process_mouse (widget wid, string kind, SI x, SI y,
                           int mods, time_t t ) {
  typedef quintuple<string, SI, SI, int, time_t > T1;
  typedef pair<widget, T1> T;
  add_event (queued_event (qp_type::QP_MOUSE,
                           close_box<T> ( T (wid, T1 (kind, x, y, mods, t)))));
}

void
qtwk_gui_rep::process_resize (widget wid, SI x, SI y ) {
  typedef triple<widget, SI, SI > T;
  add_event (queued_event (qp_type::QP_RESIZE, close_box<T> (T (wid, x, y))));
}

void
qtwk_gui_rep::process_command (command _cmd) {
  add_event (queued_event (qp_type::QP_COMMAND, close_box<command> (_cmd)));
}

void
qtwk_gui_rep::process_command (command _cmd, object _args) {
  typedef pair<command, object > T;
  add_event (queued_event (qp_type::QP_COMMAND_ARGS,
                           close_box<T> (T (_cmd,_args))));
}

void
qtwk_gui_rep::process_delayed_commands () {
  add_event (queued_event (qp_type::QP_DELAYED_COMMANDS, blackbox()));
}

/*!
  FIXME: add more types and refine, compare with X11 version.
 */
bool
qtwk_gui_rep::check_event (int type) {
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
qtwk_gui_rep::set_check_events (bool enable_check) {
  do_check_events = enable_check;
}

void
qtwk_gui_rep::add_event (const queued_event& ev) {
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
qtwk_gui_rep::update () {
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

#if 0  
  if (waitDialogs.count()) {
    waitWindow->layout()->removeWidget (waitDialogs.last());
    waitWindow->close();
    while (waitDialogs.count()) {
      waitDialogs.last()->deleteLater();
      waitDialogs.removeLast();
    }
  }
#endif

#if 0
  if (popup_wid_time > 0) {
    if (now > popup_wid_time + 2000) {
      _popup_wid->send (SLOT_VISIBILITY, close_box<bool> (false));
      _popup_wid= NULL;
      popup_wid_time = 0;
    } else if (now > popup_wid_time) {
      _popup_wid->send (SLOT_VISIBILITY, close_box<bool> (true));
    }
  }
#else
  if ((now > balloon_time + 2666) && (balloon_win)) unmap_balloon ();
  else if ((now > balloon_time + 666) && (balloon_win)
           ) map_balloon ();
#endif
  
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
    list<qtwk_window> x = windows_l;
    while (!is_nil(x)) {
      x->item->repaint_invalid_regions();
      x = x->next;
    }
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
qtwk_gui_rep::force_update() {
  if (updating) needing_update = true;
  else          update();
}

void
qtwk_gui_rep::need_update () {
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
qtwk_gui_rep::refresh_language() {
  /* FIXME: why is this here? We don't use QTranslators...
  QTranslator* qtr = new QTranslator();
  if (qtr->load ("qtwk_" +
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
#if 0
void
qtwk_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  //if (popup_wid_time > 0) return;
  if (!is_nil(_popup_wid)) {
    tm_delete (get_qtwk_window (_popup_wid));
    _popup_wid= NULL;
  }
  _popup_wid= popup_window_widget (wid, "Balloon");
  get_qtwk_window (_popup_wid)->win->setWindowFlags (Qt::ToolTip);
  get_qtwk_window (_popup_wid)->win->setFocusPolicy (Qt::NoFocus);
  set_position (_popup_wid, x, y);
  popup_wid_time = texmacs_time() + 666;
    // update() will eventually show the widget
}

void
qtwk_gui_rep::map_balloon () {}

void
qtwk_gui_rep::unmap_balloon () {}


#else

void
qtwk_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  unmap_balloon ();
  balloon_wid = wid;
  balloon_win = NULL;
  balloon_x   = x;
  balloon_y   = y;
  balloon_time= texmacs_time ();

  widget win_wid= popup_window_widget (balloon_wid, "Balloon");
  get_qtwk_window (win_wid)->win->setFlags (Qt::ToolTip);
//  get_qtwk_window (win_wid)->win->setFocusPolicy (Qt::NoFocus);
  set_position (win_wid, balloon_x, balloon_y);
  balloon_win= (window) get_qtwk_window (win_wid);

}

void
qtwk_gui_rep::map_balloon () {
  balloon_win->set_visibility (true);
}

void
qtwk_gui_rep::unmap_balloon () {
  if (!is_nil (balloon_wid)) {
    if (balloon_win != NULL) {
      balloon_win->set_visibility (false);
      tm_delete (balloon_win);
      balloon_win= NULL;
    }
    balloon_wid= widget ();
  }
}
#endif

/******************************************************************************
 * Font support
 ******************************************************************************/

/*! Loads the metric and glyphs of a system font.
 You are not forced to provide any system fonts.
 */
void
load_system_font (string fam, int sz, int dpi, font_metric& fnm, font_glyphs& fng)
{
  (void) fam; (void) sz; (void) dpi; (void) fnm; (void) fng;
  if (DEBUG_QT) debug_qt << "load_system_font(): SHOULD NOT BE CALLED\n";
}

static string the_default_font ("");
font the_default_wait_font;

void
qtwk_gui_rep::set_default_font (string name) {
  the_default_font= name;
}

font
qtwk_gui_rep::default_font_sub (bool tt, bool mini, bool bold) {
  string s= the_default_font;
  string series= (bold? string ("bold"): string ("medium"));
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if (is_digit (s[j])) break;
  string fam= s (0, j);
  if (mini && fam == "ecrm") fam= "ecss";
  if (bold && fam == "ecrm") fam= "ecbx";
  if (bold && fam == "ecss") fam= "ecsx";
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (mini) { sz= (int) (0.6 * sz); dpi= (int) (1.3333333 * dpi); }
  {
    tree lucida_fn= tuple ("Fira", "ss", series, "right");
    lucida_fn << as_string (sz) << as_string ((int) (0.95 * dpi));
    return find_font (lucida_fn);
  }
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();
    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
   (out_lan == "ukrainian")) &&
  ((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", series, "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ("fireflysung", sz, dpi);
    if (out_lan == "greek")
      return unicode_font ("Stix", sz, dpi);
    //if (out_lan == "japanese")
    //return unicode_font ("ipagui", sz, dpi);
    //if (out_lan == "korean")
    //return unicode_font ("UnDotum", sz, dpi);
    if (ff == "ec")
      return tex_ec_font (tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (fam, sz, dpi);
  // if (out_lan == "german") return tex_font ("ygoth", 14, 300, 0);
  // return tex_font ("rpagk", 10, 300, 0);
  // return tex_font ("rphvr", 10, 300, 0);
  // return ps_font ("b&h-lucidabright-medium-r-normal", 11, 300);
}

font
qtwk_gui_rep::default_font (bool tt, bool mini, bool bold) {
  font fn= default_font_sub (tt, mini, bold);
  if (!tt && !mini) the_default_wait_font= fn;
  return fn;
}

void
set_default_font (string name) {
  the_gui->set_default_font (name);
}

font
get_default_font (bool tt, bool mini, bool bold) {
  return the_gui->default_font (tt, mini, bold);
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
qtwk_gui_rep::put_graphics_on_clipboard (url file) {
  string extension = suffix (file) ;
  
    // for bitmaps this works :
  if ((extension == "bmp") || (extension == "png") ||
      (extension == "jpg") || (extension == "jpeg")) {
    QClipboard *clipboard = QGuiApplication::clipboard();
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
    
    QClipboard *clipboard = QGuiApplication::clipboard();
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
//  QGuiApplication::beep();
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
#if 0
    // External events, such as pushing a button of a remote infrared commander
  QWidget *tm_focus = qobject_cast<QWidget*>(qApp->focusWidget());
  if (tm_focus) {
    simple_widget_rep* wid = tm_focus->tm_widget();
    if (wid) the_gui -> process_keypress (wid, type, t);
  }
#endif
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


#ifdef __EMSCRIPTEN__
#include "tm_link.hpp"
tm_link make_pipe_link (string cmd) { return tm_link(); }

void close_all_pipes () {}
void process_all_pipes () {}

#endif