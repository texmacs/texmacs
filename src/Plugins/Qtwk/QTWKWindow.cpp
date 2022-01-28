
/******************************************************************************
* MODULE     : QTWKWindow.cpp
* DESCRIPTION: QT/Widkit Texmacs window class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli and Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"

#include "QTWKWindow.hpp"
#include "qtwk_gui.hpp"
#include "Qt/qt_utilities.hpp"
#include "converter.hpp"
#include "boot.hpp"
#include "scheme.hpp"


hashmap<int,string> qtkeymap (0);
hashmap<int,string> qtdeadmap (0);

inline void
map (int code, string name) {
  qtkeymap (code) = name;
}

inline void
deadmap (int code, string name) {
  qtdeadmap (code) = name;
}

void
initkeymap () {
  static bool fInit= false;
  if (fInit) return;
  fInit= true;
  if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "Initializing keymap\n";
  map (Qt::Key_Space     , "space");
  map (Qt::Key_Return    , "return");
  map (Qt::Key_Tab       , "tab");
  map (Qt::Key_Backspace , "backspace");
  map (Qt::Key_Enter     , "enter");
  map (Qt::Key_Escape    , "escape");
  map (Qt::Key_Backspace , "backspace");
  map (Qt::Key_Up        , "up" );
  map (Qt::Key_Down      , "down" );
  map (Qt::Key_Left      , "left" );
  map (Qt::Key_Right     , "right" );
  map (Qt::Key_F1        , "F1" );
  map (Qt::Key_F2        , "F2" );
  map (Qt::Key_F3        , "F3" );
  map (Qt::Key_F4        , "F4" );
  map (Qt::Key_F5        , "F5" );
  map (Qt::Key_F6        , "F6" );
  map (Qt::Key_F7        , "F7" );
  map (Qt::Key_F8        , "F8" );
  map (Qt::Key_F9        , "F9" );
  map (Qt::Key_F10       , "F10" );
  map (Qt::Key_F11       , "F11" );
  map (Qt::Key_F12       , "F12" );
  map (Qt::Key_F13       , "F13" );
  map (Qt::Key_F14       , "F14" );
  map (Qt::Key_F15       , "F15" );
  map (Qt::Key_F16       , "F16" );
  map (Qt::Key_F17       , "F17" );
  map (Qt::Key_F18       , "F18" );
  map (Qt::Key_F19       , "F19" );
  map (Qt::Key_F20       , "F20" );
  map (Qt::Key_F21       , "F21" );
  map (Qt::Key_F22       , "F22" );
  map (Qt::Key_F23       , "F23" );
  map (Qt::Key_F24       , "F24" );
  map (Qt::Key_F25       , "F25" );
  map (Qt::Key_F26       , "F26" );
  map (Qt::Key_F27       , "F27" );
  map (Qt::Key_F28       , "F28" );
  map (Qt::Key_F29       , "F29" );
  map (Qt::Key_F30       , "F30" );
  map (Qt::Key_F31       , "F31" );
  map (Qt::Key_F32       , "F32" );
  map (Qt::Key_F33       , "F33" );
  map (Qt::Key_F34       , "F34" );
  map (Qt::Key_F35       , "F35" );
  map (Qt::Key_Insert    , "insert" );
  map (Qt::Key_Delete    , "delete" );
  map (Qt::Key_Home      , "home" );
  map (Qt::Key_End       , "end" );
  map (Qt::Key_PageUp    , "pageup" );
  map (Qt::Key_PageDown  , "pagedown" );
  map (Qt::Key_ScrollLock, "scrolllock" );
  map (Qt::Key_Pause     , "pause" );
  map (Qt::Key_SysReq    , "sysreq" );
  map (Qt::Key_Stop      , "stop" );
  map (Qt::Key_Menu      , "menu" );
  map (Qt::Key_Print     , "print" );
  map (Qt::Key_Select    , "select" );
  map (Qt::Key_Execute   , "execute" );
  map (Qt::Key_Help      , "help" );
  map (Qt::Key_section   , "section" );

  deadmap (Qt::Key_Dead_Acute     , "acute");
  deadmap (Qt::Key_Dead_Grave     , "grave");
  deadmap (Qt::Key_Dead_Diaeresis , "umlaut");
  deadmap (Qt::Key_Dead_Circumflex, "hat");
  deadmap (Qt::Key_Dead_Tilde     , "tilde");

  // map (0x0003              , "K-enter");
  // map (Qt::Key_Begin       , "begin" );
  // map (Qt::Key_PrintScreen , "printscreen" );
  // map (Qt::Key_Break       , "break" );
  // map (Qt::Key_User        , "user" );
  // map (Qt::Key_System      , "system" );
  // map (Qt::Key_Reset       , "reset" );
  // map (Qt::Key_ClearLine   , "clear" );
  // map (Qt::Key_ClearDisplay, "cleardisplay" );
  // map (Qt::Key_InsertLine  , "insertline" );
  // map (Qt::Key_DeleteLine  , "deleteline" );
  // map (Qt::Key_InsertChar  , "insert" );
  // map (Qt::Key_DeleteChar  , "delete" );
  // map (Qt::Key_Prev        , "prev" );
  // map (Qt::Key_Next        , "next" );
  // map (Qt::Key_Undo        , "undo" );
  // map (Qt::Key_Redo        , "redo" );
  // map (Qt::Key_Find        , "find" );
  // map (Qt::Key_ModeSwitchFunctionKey, "modeswitch" );
}
#ifdef OS_MINGW
enum WindowsNativeModifiers {
    ShiftLeft            = 0x00000001,
    ControlLeft          = 0x00000002,
    AltLeft              = 0x00000004,
    MetaLeft             = 0x00000008,
    ShiftRight           = 0x00000010,
    ControlRight         = 0x00000020,
    AltRight             = 0x00000040,
    MetaRight            = 0x00000080,
    CapsLock             = 0x00000100,
    NumLock              = 0x00000200,
    ScrollLock           = 0x00000400,
    ExtendedKey          = 0x01000000,
};
#endif

/*! Constructor.
 
  \param _tmwid the TeXmacs window who owns this object.
 */
QTWKWindow::QTWKWindow (qtwk_window _tmwid)
  : QWindow (), tmwid (_tmwid) {
  // disable alpha; saves filling the window with transparent pixels
  QSurfaceFormat format;
  format.setAlphaBufferSize(0);
  setFormat(format);
  // ensure a raster surface
  setSurfaceType(QSurface::RasterSurface);
  // force creation of the pixels
  create();

  backing_store= new QBackingStore(this);
  repaintTimer= 0;
}

QTWKWindow::~QTWKWindow () {
  if (backing_store) delete backing_store;
}

widget
QTWKWindow::tm_widget () const {
  return tmwid->w;
}

void 
QTWKWindow::scheduleRepaint() {
  if (!repaintTimer) repaintTimer= startTimer(1);
}

void 
QTWKWindow::timerEvent(QTimerEvent *) {
  if (isExposed()) {
    repaint();
  }
  killTimer(repaintTimer);
  repaintTimer= 0;
}

void 
QTWKWindow::exposeEvent(QExposeEvent *) {
    if (isExposed()) repaint();
}

void
QTWKWindow::resizeEvent (QResizeEvent *event) {
  backing_store->resize(size()); 
  QRect r= QRect(QPoint(), size());
  backing_store->beginPaint(r);
  QPaintDevice *device = backing_store->paintDevice();
  QPainter painter(device);
  painter.fillRect(r, Qt::gray);
  painter.drawPixmap(QPoint(), tmwid->backingPixmap);
  painter.end();
  backing_store->endPaint();
  backing_store->flush(r);

  coord2 s = from_qsize (event->size());
  cout << from_qstring (objectName()) << " resize event " << s << "\n";
 // tmwid->resize_event (s.x1,s.x2);
  the_gui -> process_resize (tm_widget (), s.x1, s.x2);
  scheduleRepaint();
}

/*!
 In the current implementation repainting takes place during the call to
 the window's repaint_invalid_regions() method in qtwk_gui::update. All
 we have to do is to take the backing store and put it on screen according
 to the QRegion marked invalid. 
 CHECK: Maybe just putting onscreen all the region bounding rectangles might 
 be less expensive.
*/
void 
QTWKWindow::repaint (const QRegion &rgn) {
  if (!isExposed()) return;
  backing_store->beginPaint(rgn);
  QPaintDevice *device = backing_store->paintDevice();
  QPainter painter(device);
  for (const QRect &qr : rgn) {
    painter.drawPixmap (QRect (qr.x(), qr.y(), qr.width(), qr.height()),
                  tmwid->backingPixmap,
                  QRect (retina_factor * qr.x(),
                         retina_factor * qr.y(),
                         retina_factor * qr.width(),
                         retina_factor * qr.height()));
  }
  painter.end();
  backing_store->endPaint();
  backing_store->flush(rgn);
}

void 
QTWKWindow::repaint () {
  repaint (QRect(QPoint(), geometry().size()));
}

void
set_shift_preference (int key_code, char shifted) {
  set_user_preference ("shift-" * as_string (key_code), string (shifted));
}

bool
has_shift_preference (int key_code) {
  return has_user_preference ("shift-" * as_string (key_code));
}

string
get_shift_preference (char key_code) {
  return get_user_preference ("shift-" * as_string (key_code));
}

void
QTWKWindow::keyPressEvent (QKeyEvent* event) {
  if (is_nil (tm_widget ())) return;
  initkeymap();

  if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "keypressed\n";
  {
    int key = event->key();
    Qt::KeyboardModifiers mods = event->modifiers();

    if (DEBUG_QT && DEBUG_KEYBOARD) {
      debug_qt << "key  : " << key << LF;
      debug_qt << "text : " << event->text().toLatin1().data() << LF;
      debug_qt << "count: " << event->text().count() << LF;
#ifdef OS_MINGW
      debug_qt << "nativeScanCode: " << event->nativeScanCode() << LF; 
      debug_qt << "nativeVirtualKey: " << event->nativeVirtualKey() << LF;
      debug_qt << "nativeModifiers: " << event->nativeModifiers() << LF;
#endif
      if (mods & Qt::ShiftModifier) debug_qt << "shift\n";
      if (mods & Qt::MetaModifier) debug_qt << "meta\n";
      if (mods & Qt::ControlModifier) debug_qt << "control\n";
      if (mods & Qt::KeypadModifier) debug_qt << "keypad\n";
      if (mods & Qt::AltModifier) debug_qt << "alt\n";
    }

    string r;
#ifdef OS_MINGW 
/* "Qt::Key_AltGr On Windows, when the KeyDown event for this key is sent,
* the Ctrl+Alt modifiers are also set." (excerpt from Qt doc)
* However the AltGr key is used to obtain many symbols 
* which should not be regarded as C-A- shortcuts.
* (e.g. \ or @ on a French keyboard) 
* 
* Hence, when "native modifiers" are (ControlLeft | AltRight) 
* we clear Qt's Ctrl+Alt modifiers
*/
    if ((event->nativeModifiers() & (ControlLeft | AltRight)) == (ControlLeft | AltRight)) {
      if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "assuming it's an AltGr key code"<<LF;
      mods &= ~Qt::AltModifier;
      mods &= ~Qt::ControlModifier;
    }
#endif
    if (qtkeymap->contains (key)) {
      r = qtkeymap[key];
    }
    else if (qtdeadmap->contains (key)) {
      mods &=~ Qt::ShiftModifier;
      r = qtdeadmap[key];
    }
    else {
        // We need to use text(): Alt-{5,6,7,8,9} are []|{} under MacOS, etc.
      QString nss = event->text();
      unsigned int   kc  = event->nativeVirtualKey();
      unsigned short unic= nss.data()[0].unicode();
      /*
      debug_qt << "key  : " << key << LF;
      debug_qt << "text : " << event->text().toLatin1().data() << LF;
      debug_qt << "count: " << event->text().count() << LF;
      if (mods & Qt::ShiftModifier) debug_qt << "shift\n";
      if (mods & Qt::MetaModifier) debug_qt << "meta\n";
      if (mods & Qt::ControlModifier) debug_qt << "control\n";
      if (mods & Qt::KeypadModifier) debug_qt << "keypad\n";
      if (mods & Qt::AltModifier) debug_qt << "alt\n";
      cout << kc << ", " << ((mods & Qt::ShiftModifier) != 0)
           << " -> " << unic << LF;
      */
      if (unic > 32 && unic < 255 &&
          (mods & Qt::ShiftModifier) != 0 &&
          (mods & Qt::ControlModifier) == 0 &&
          (mods & Qt::AltModifier) == 0 &&
          (mods & Qt::MetaModifier) == 0)
        set_shift_preference (kc, (char) unic);
      if (unic < 32 && key < 128 && key > 0) {
        // NOTE: For some reason, the 'shift' modifier key is not applied
        // to 'key' when 'control' is pressed as well.  We perform some
        // dirty hacking to figure out the right shifted variant of a key
        // by ourselves...
        if (is_upcase ((char) key)) {
          if ((mods & Qt::ShiftModifier) == 0)
            key= (int) locase ((char) key);
        }
        else if (has_shift_preference (kc) &&
                 (mods & Qt::ShiftModifier) != 0 &&
                 (mods & Qt::ControlModifier) != 0)
          key= (int) (unsigned char) get_shift_preference (kc) [0];
        mods &=~ Qt::ShiftModifier;
        r= string ((char) key);
      }
      else {
        switch (unic) {
          case 96:   r= "`"; 
            // unicode to cork conversion not appropriate for this case...
#ifdef Q_OS_MAC
            // CHECKME: are these two MAC exceptions really needed?
            if (mods & Qt::AltModifier) r= "grave";
#endif
            break;
          case 168:  r= "umlaut"; break;
          case 180:  r= "acute"; break;
            // the following combining characters should be caught by qtdeadmap
          case 0x300: r= "grave"; break;
          case 0x301: r= "acute"; break;
          case 0x302: r= "hat"; break;
          case 0x308: r= "umlaut"; break;
          case 0x33e: r= "tilde"; break;
          default:
            QByteArray buf= nss.toUtf8();
            string rr (buf.constData(), buf.size());
            string tstr= utf8_to_cork (rr);
            // HACK! The encodings defined in langs/encoding and which
            // utf8_to_cork uses (via the converters loaded in
            // converter_rep::load()), enclose the texmacs symbols in "< >", 
            // but this format is not used for keypresses, so we must remove
            // them.
            int len= N (tstr);
            if (len >= 1 && tstr[0] == '<' && tstr[1] != '#' && tstr[len-1] == '>')
              r= tstr (1, len-1);
            else
              r= tstr;
            if (r == "less") r= "<";
            else if (r == "gtr") r= ">";
        }
#ifdef Q_OS_MAC
          // Alt produces many symbols in Mac keyboards: []|{} etc.
        mods &= ~Qt::AltModifier; //unset Alt
#endif
        mods &= ~Qt::ShiftModifier;
      }
    }
    if (r == "") return;
    if (mods & Qt::ShiftModifier) r= "S-" * r;
    if (mods & Qt::AltModifier) r= "A-" * r;
    //if (mods & Qt::KeypadModifier) r= "K-" * r;
#ifdef Q_OS_MAC
    if (mods & Qt::MetaModifier) r= "C-" * r;        // The "Control" key
    if (mods & Qt::ControlModifier) r= "M-" * r;  // The "Command" key
#else
    if (mods & Qt::ControlModifier) r= "C-" * r;
    if (mods & Qt::MetaModifier) r= "M-" * r;     // The "Windows" key
#endif

    if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << r << LF;
    the_gui->process_keypress (tm_widget(), r, texmacs_time());
  }
}

static unsigned int
mouse_state (Qt::MouseButtons bstate, Qt::KeyboardModifiers kstate) {
  unsigned int i= 0;
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

static string
mouse_decode (unsigned int mstate) {
  if (mstate & 2) return "middle";
  else if (mstate & 4) return "right";
    // we check for left clicks after the others for macos (see ifdef in mouse_state)
  else if (mstate & 1) return "left";
  else if (mstate & 8) return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}

void
QTWKWindow::mousePressEvent (QMouseEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint point = event->position().toPoint();// + origin();
  coord2 pt = from_qpoint (point);
  unsigned int mstate= mouse_state (event->buttons(), event->modifiers());
  string s= "press-" * mouse_decode (event->button());
  the_gui -> process_mouse (tm_widget (), s, pt.x1, pt.x2,
                            mstate, texmacs_time ());
  event->accept();
}

void
QTWKWindow::mouseReleaseEvent (QMouseEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint point = event->position().toPoint();// + origin();
  coord2 pt = from_qpoint (point);
  unsigned int mstate= mouse_state (event->buttons(), event->modifiers());
  string s = "release-" * mouse_decode (event->button());
  the_gui->process_mouse (tm_widget (), s, pt.x1, pt.x2,
                            mstate, texmacs_time());
  event->accept();
}

void
QTWKWindow::mouseMoveEvent (QMouseEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint point = event->position().toPoint();// + origin();
  coord2 pt = from_qpoint (point);
  unsigned int mstate= mouse_state (event->buttons(), event->modifiers());
  string s = "move";
  the_gui->process_mouse (tm_widget (), s, pt.x1, pt.x2,
                          mstate, texmacs_time ());
  event->accept();
}

void
QTWKWindow::wheelEvent (QWheelEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint delta= event->angleDelta ();
  QPoint pos= QCursor::pos ();
  coord2 p= from_qpoint (pos);
  unsigned int mstate= mouse_state (event->buttons(), event->modifiers());
  if (delta.y () >= 1.0)
    the_gui->process_mouse (tm_widget (), "press-up", p.x1, p.x2,
                            mstate, texmacs_time ());
  else if (delta.y () <= -1.0)
    the_gui->process_mouse (tm_widget (), "press-down", p.x1, p.x2,
                            mstate, texmacs_time ());
  event->accept();
}

bool
QTWKWindow::event (QEvent* event) {
  if (event->type() == QEvent::UpdateRequest) {
    repaint();
    return true;
  }
    // Catch Keypresses to avoid default handling of (Shift+)Tab keys
  if (event->type() == QEvent::KeyPress) {
    QKeyEvent *ke = static_cast<QKeyEvent*> (event);
    keyPressEvent (ke);
    return true;
  } 
  /* NOTE: we catch ShortcutOverride in order to disable the QKeySequences we
   assigned to QActions while building menus, etc. In doing this, we keep the
   shortcut text in the menus while relaying all keypresses through the editor*/
  if (event->type() == QEvent::ShortcutOverride) {
    event->accept();
    return true;
  }
  return QWindow::event (event);
}

void
QTWKWindow::focusInEvent (QFocusEvent * event) {
  if (!is_nil (tm_widget ())) {
  //  if (DEBUG_QT) debug_qt << "FOCUSIN: " << tm_widget()->type_as_string() << LF;
    the_gui->process_keyboard_focus (tm_widget (), true, texmacs_time ());
  }
  QWindow::focusInEvent (event);
}

void
QTWKWindow::focusOutEvent (QFocusEvent * event) {
  if (!is_nil (tm_widget ())) {
  //  if (DEBUG_QT) debug_qt << "FOCUSOUT: " << tm_widget()->type_as_string() << LF;
    the_gui -> process_keyboard_focus (tm_widget (), false, texmacs_time ());
  }
  QWindow::focusOutEvent (event);
}

void
QTWKWindow::enterEvent (QEnterEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint point = mapFromGlobal(QCursor::pos());
  coord2 pt = from_qpoint (point);
  unsigned int mstate = mouse_state (QGuiApplication::mouseButtons (),
                                     QGuiApplication::keyboardModifiers ());
  string s = "enter";
  the_gui->process_mouse (tm_widget (), s, pt.x1, pt.x2,
                          mstate, texmacs_time ());
  //QWindow::enterEvent (event);
}

void
QTWKWindow::leaveEvent (QMoveEvent* event) {
  if (is_nil (tm_widget ())) return;
  QPoint point = mapFromGlobal(QCursor::pos());
  coord2 pt = from_qpoint (point);
  unsigned int mstate = mouse_state (QGuiApplication::mouseButtons (),
                                     QGuiApplication::keyboardModifiers ());
  string s = "leave";
  the_gui->process_mouse (tm_widget (), s, pt.x1, pt.x2,
                          mstate, texmacs_time ());
  //QWindow::leaveEvent (event);
}

#if 0
QSize
QTWKWindow::sizeHint () const {
  SI w = 0, h = 0;
//  if (!is_nil (tm_widget ())) tm_widget()->handle_get_size_hint (w, h);
  return to_qsize (w, h);
}
#endif

void 
QTWKWindow::dragEnterEvent (QDragEnterEvent *event)
{
  if (is_nil (tm_widget ())) return;
  const QMimeData *md = event->mimeData();

  if (md->hasText() ||
      md->hasUrls() ||
      md->hasImage() ||
      md->hasFormat("application/pdf") ||
      md->hasFormat("application/postscript"))
      event->acceptProposedAction();
}


// cache to transfer drop data to the editor
// via standard mouse events, see dropEvent below

int drop_payload_serial  =0;
hashmap<int, tree> payloads;

void
QTWKWindow::dropEvent (QDropEvent *event)
{
  if (is_nil (tm_widget ())) return;
  
  QPoint point = event->position().toPoint();// + origin ();
  coord2 pt= from_qpoint (point);

  //qDebug() << event;
  tree doc (CONCAT);
  const QMimeData *md= event->mimeData ();
  QByteArray buf;

  if (md->hasUrls ()) {
    QList<QUrl> l= md->urls ();
//    qDebug() << l;
    for (int i=0; i<l.size (); i++) {
      string name;
#ifdef OS_MACOS
      name= from_qstring (fromNSUrl (l[i]));
#else
      name= from_qstring (l[i].toLocalFile ());
#endif
      string extension = suffix (name);
      if ((extension == "eps") || (extension == "ps")   ||
          (extension == "pdf") || (extension == "png")  ||
          (extension == "jpg") || (extension == "jpeg")) {
        string w, h;
        qt_pretty_image_size (url_system (name), w, h);
        tree im (IMAGE, name, w, h, "", "");
        doc << im;
      } else {
        doc << name;
      }
    }
  } else if (md->hasImage ()) {
    QBuffer qbuf (&buf);
    QImage image= qvariant_cast<QImage> (md->imageData());
    QSize size= image.size ();
    qbuf.open (QIODevice::WriteOnly);
    image.save (&qbuf, "PNG");
    int ww= size.width (), hh= size.height ();
    string w, h;
    qt_pretty_image_size (ww, hh, w, h);
    tree t (IMAGE, tree (RAW_DATA, string (buf.constData (), buf.size()), "png"),
            w, h, "", "");
    doc << t;
  } else if (md->hasFormat("application/postscript")) {
    buf= md->data("application/postscript");
    tree t (IMAGE, tree (RAW_DATA, string (buf.constData (), buf.size ()), "ps"),
                   "", "", "", "");
    doc << t;
  } else if (md->hasFormat("application/pdf")) {
    buf= md->data("application/pdf");
    tree t (IMAGE, tree (RAW_DATA, string (buf.constData (), buf.size ()), "pdf"),
                   "", "", "", "");
    doc << t;
  }  else if (md->hasText ()) {
    buf= md->text ().toUtf8 ();
    doc << string (buf.constData (), buf.size ());
  }

  if (N(doc)>0) {
    if (N(doc) == 1)
      doc= doc[0];
    else {
      tree sec (CONCAT, doc[0]);
      for (int i=1; i<N(doc); i++)
        sec << " " << doc[i];
      doc= sec;
    }
    int ticket= drop_payload_serial++;
    payloads (ticket)= doc;
    the_gui->process_mouse (tm_widget(), "drop", pt.x1, pt.x2,
                            ticket, texmacs_time ());
    event->acceptProposedAction();
  }
}
