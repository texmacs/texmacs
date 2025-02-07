
/******************************************************************************
* MODULE     : QTMWidget.cpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli and Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWidget.hpp"
#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_simple_widget.hpp"
#include "converter.hpp"
#include "boot.hpp"
#include "scheme.hpp"
#include "new_view.hpp"
#include "editor.hpp"
#include "qt_renderer.hpp"
#include "QTMApplication.hpp"
#include "QTMKeyboardEvent.hpp"

#include "config.h"

#include <QDebug>
#include <QEvent>
#include <QResizeEvent>
#include <QKeyEvent>
#include <QPaintEvent>
#include <QMouseEvent>
#include <QFocusEvent>
#include <QPainter>
#include <QApplication>

#ifdef OS_ANDROID
#include <QScrollBar>
#include <QScroller>
#endif

#include <QBuffer>
#include <QMimeData>
#include <QByteArray>
#include <QImage>
#include <QUrl>
#include <QFileInfo>


static long int QTMWcounter = 0; // debugging hack

/*! Constructor.
 
  \param _parent The parent QWidget.
  \param _tmwid the TeXmacs widget who owns this object.
 */
QTMWidget::QTMWidget (QWidget* _parent, qt_widget _tmwid)
: QTMScrollView (_parent), tmwid (_tmwid),  imwidget (NULL),
  preediting (false)
{
  setObjectName (to_qstring ("QTMWidget" * as_string (QTMWcounter++)));// What is this for? (maybe only debugging?)
  setFocusPolicy (Qt::StrongFocus);
  setAttribute (Qt::WA_InputMethodEnabled);
  surface ()->setMouseTracking (true);
  surface ()->setAcceptDrops (true);
#ifdef OS_ANDROID
  // enable scroll with finger on android
  QScroller::grabGesture(this, QScroller::LeftMouseButtonGesture);
#endif
  grabGesture (Qt::PanGesture);
  grabGesture (Qt::PinchGesture);
  grabGesture (Qt::SwipeGesture);

#if (QT_VERSION >= QT_VERSION_CHECK(5,9,0))
  surface ()->setTabletTracking (true);
  for (QWidget *parent = surface()->parentWidget();
       parent != nullptr; parent = parent->parentWidget())
    parent->setTabletTracking(true);
#endif

  if (DEBUG_QT)
    debug_qt << "Creating " << from_qstring(objectName()) << " of widget "
             << (tm_widget() ? tm_widget()->type_as_string() : "NULL") << LF;
  //part 1/2 of the fix for 43373
  if (!isEmbedded ())
    QApplication::postEvent(this, new QFocusEvent(QEvent::FocusIn, Qt::OtherFocusReason));
}

QTMWidget::~QTMWidget () {
  if (DEBUG_QT)
    debug_qt << "Destroying " << from_qstring(objectName()) << " of widget "
             << (tm_widget() ? tm_widget()->type_as_string() : "NULL") << LF;
}

bool
QTMWidget::isEmbedded () const {
  return tm_widget() -> is_embedded_widget ();
}

qt_simple_widget_rep*
QTMWidget::tm_widget () const { 
  return concrete_simple_widget (tmwid); 
}

void 
QTMWidget::scrollContentsBy (int dx, int dy) {
  QTMScrollView::scrollContentsBy (dx,dy);
  the_gui->force_update();
  // we force an update of the internal state to be in sync with the moving
  // scrollbars
}

void 
QTMWidget::resizeEvent (QResizeEvent* event) {
  (void) event;
  // Is this ok?
  //coord2 s = from_qsize (event->size());
  //the_gui -> process_resize (tm_widget(), s.x1, s.x2);

  // the_gui->force_update();

  //FIXME: I would like to have a force_update here but this causes a failed
  //assertion in TeXmacs since the at the boot not every internal structure is
  //initialized at this point. It seems not too difficult to fix but I
  //postpone this to discuss with Joris. 
  //
  //Not having a force_update results in some lack of sync of the surface
  //while the user is actively resizing with the mouse.
}

void
QTMWidget::resizeEventBis (QResizeEvent *event) {
  coord2 s = from_qsize (event->size());
  the_gui -> process_resize (tm_widget(), s.x1, s.x2);
}

/*!
 In the current implementation repainting takes place during the call to
 the widget's repaint_invalid_regions() method in the_gui::update. All
 we have to do is to take the backing store and put it on screen according
 to the QRegion marked invalid. 
 CHECK: Maybe just putting onscreen all the region bounding rectangles might 
 be less expensive.
*/
#if QT_VERSION >= 0x060000
void
QTMWidget::surfacePaintEvent (QPaintEvent *event, QWidget *surfaceWidget) {
  (void) surfaceWidget;
  QPainter p (surface());

  qreal dpr = surface()->devicePixelRatio();
  if (dpr != tm_widget()->backingPixmap->devicePixelRatio()) {
    QMetaObject::invokeMethod (this, "surfaceDprChanged", Qt::QueuedConnection);
    return;
  }

  // We copy the backing buffer on the widget
  QRect qr = event->region().boundingRect();
  p.drawPixmap (QRect (qr.x(), qr.y(), qr.width(), qr.height()),
                *(tm_widget()->backingPixmap),
                QRect (dpr * qr.x(),
                       dpr * qr.y(),
                       dpr * qr.width(),
                       dpr * qr.height()));
  
}

void
QTMWidget::surfaceDprChanged () {
  tm_widget()->reset_all();
  the_gui->force_update();
}
#else
void
QTMWidget::surfacePaintEvent (QPaintEvent *event, QWidget *surfaceWidget) {
  QPainter p (surface());
  QVector<QRect> rects = event->region().rects();
  for (int i = 0; i < rects.count(); ++i) {
    QRect qr = rects.at (i);
    p.drawPixmap (QRect (qr.x(), qr.y(), qr.width(), qr.height()),
                  *(tm_widget()->backingPixmap),
                  QRect (retina_factor * qr.x(),
                         retina_factor * qr.y(),
                         retina_factor * qr.width(),
                         retina_factor * qr.height()));
  }
}
#endif

void
setShiftPreference (int key_code, char shifted) {
  set_user_preference ("shift-" * as_string (key_code), string (shifted));
}

bool
hasShiftPreference (int key_code) {
  return has_user_preference ("shift-" * as_string (key_code));
}

string
getShiftPreference (char key_code) {
  return get_user_preference ("shift-" * as_string (key_code));
}

void
QTMWidget::keyPressEvent (QKeyEvent* event) {
  QTMKeyboardEvent ke (tmapp()->keyboard(), *event);
  string r = ke.texmacsKeyCombination();
  if (r == "") {
    if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: unhandled key" << LF;
    return;
  }
  if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << r << LF;
  the_gui->process_keypress (tm_widget(), r, texmacs_time());

}

static unsigned int
mouse_state (QMouseEvent* event, bool flag) {
  unsigned int i= 0;
  Qt::MouseButtons bstate= event->buttons ();
  Qt::MouseButton  tstate= event->button ();
  Qt::KeyboardModifiers kstate= event->modifiers ();
  if (flag) bstate= bstate | tstate;
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
#if QT_VERSION < 0x060000
    if ((bstate & Qt::MidButton      ) != 0) i += 2;
#else
    if ((bstate & Qt::MiddleButton   ) != 0) i += 2;
#endif
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
QTMWidget::kbdEvent (int key, Qt::KeyboardModifiers mods, const QString& s) {
  QKeyEvent ev (QEvent::KeyPress, key, mods, s);
  keyPressEvent (&ev);
}

void
QTMWidget::inputMethodEvent (QInputMethodEvent* event) {
  QString const & preedit_string = event->preeditString();
  QString const & commit_string = event->commitString();
  
  if (!commit_string.isEmpty()) {
    bool done= false;
#ifdef OS_MACOS
#if (QT_VERSION < 0x050000)
    // NOTE: this hack is only needed for Qt4 under MacOS,
    // but it only works for standard US keyboards
    done= true;
    string s= from_qstring (commit_string);
    Qt::KeyboardModifiers SA= Qt::ShiftModifier | Qt::AltModifier;
    if (s == "\17") kbdEvent (36, Qt::AltModifier, commit_string);
    else if (s == "<ddagger>") kbdEvent (38, Qt::AltModifier, commit_string);
    else if (s == "<leq>") kbdEvent (44, Qt::AltModifier, commit_string);
    else if (s == "<geq>") kbdEvent (46, Qt::AltModifier, commit_string);
    else if (s == "<trademark>") kbdEvent (50, Qt::AltModifier, commit_string);
    else if (s == "<infty>") kbdEvent (53, Qt::AltModifier, commit_string);
    else if (s == "<ldots>") kbdEvent (59, Qt::AltModifier, commit_string);
    else if (s == "<#20AC>") kbdEvent (64, Qt::AltModifier, commit_string);
    else if (s == "<partial>") kbdEvent (68, Qt::AltModifier, commit_string);
    else if (s == "<#192>") kbdEvent (70, Qt::AltModifier, commit_string);
    else if (s == "<dagger>") kbdEvent (84, Qt::AltModifier, commit_string);
    else if (s == "<sqrt>") kbdEvent (86, Qt::AltModifier, commit_string);
    else if (s == "\35") kbdEvent (94, Qt::AltModifier, commit_string);
    else if (s == "\31") kbdEvent (66, SA, commit_string);
    else if (s == "<lozenge>") kbdEvent (89, SA, commit_string);
    else done= false;
#endif
#endif
    
    if (!done) {
      if (DEBUG_QT)
        debug_qt << "IM committing: " << commit_string.toUtf8().data() << LF;
      if (preediting || get_preference ("speech", "off") == "off")
        for (int i = 0; i < commit_string.size(); ++i)
          kbdEvent (0, Qt::NoModifier, commit_string[i]);
      else {
        string s= "speech:" * from_qstring (commit_string);
        kbdEvent (0, Qt::NoModifier, to_qstring (s));
      }
    }
  }
  
  if (DEBUG_QT)
    debug_qt << "IM preediting :" << preedit_string.toUtf8().data() << LF;
  
  string r = "pre-edit:";
  if (!preedit_string.isEmpty())
  {
    
    // find cursor position in the preedit string
    QList<QInputMethodEvent::Attribute>  const & attrs = event->attributes();
    //    int pos = preedit_string.count();
    int pos = 0;
    bool visible_cur = false;
    for (int i=0; i< attrs.count(); i++) 
      if (attrs[i].type == QInputMethodEvent::Cursor) {
        pos = attrs[i].start;
        visible_cur = (attrs[i].length != 0);
      }
    
    // find selection in the preedit string
    int sel_start = 0;
    int sel_length = 0;
#if QT_VERSION >= 0x060000
    if (pos <  preedit_string.size()) {
#else	
    if (pos <  preedit_string.count()) {
#endif
      for (int i=0; i< attrs.count(); i++)
        if ((attrs[i].type == QInputMethodEvent::TextFormat) &&
            (attrs[i].start <= pos) &&
            (pos < attrs[i].start + attrs[i].length)) {
          sel_start = attrs[i].start;
          sel_length =  attrs[i].length;
          if (!visible_cur) pos += attrs[i].length;
        }
    } else {
      sel_start = pos;
      sel_length = 0;
    }
    (void) sel_start; (void) sel_length;
    
    r = r * as_string (pos) * ":" * from_qstring (preedit_string);
  }

  if (!is_nil (tmwid)) {
    preediting = !preedit_string.isEmpty();
    the_gui->process_keypress (tm_widget(), r, texmacs_time());
  }

  event->accept();
}  

QVariant 
QTMWidget::inputMethodQuery (Qt::InputMethodQuery query) const {
  switch (query) {
#if QT_VERSION < 0x060000
    case Qt::ImMicroFocus : {
      const QPoint &topleft= cursor_pos - tm_widget()->backing_pos + surface()->geometry().topLeft();
      return QVariant (QRect (topleft, QSize (5, 5)));
    }
#else
    case Qt::ImEnabled : {
      return QVariant (true);
    }
    case Qt::ImCursorRectangle : {
      const QPoint &topleft= cursor_pos - tm_widget()->backing_pos + surface()->geometry().topLeft();
      return QVariant (QRect (topleft, QSize (5, 5)));
    }
#endif // TODO : Correctly implement input methods
    default:
      return QWidget::inputMethodQuery (query);
  }
}

void
QTMWidget::mousePressEvent (QMouseEvent* event) {
#if defined(OS_ANDROID) && QT_VERSION >= 0x060000
  showKeyboard();
#endif
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  coord2 pt = from_qpoint(point);
  unsigned int mstate= mouse_state (event, false);
  string s= "press-" * mouse_decode (mstate);
  the_gui -> process_mouse (tm_widget(), s, pt.x1, pt.x2,  
                            mstate, texmacs_time ());
  event->accept();
}

void
QTMWidget::mouseReleaseEvent (QMouseEvent* event) {
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  coord2 pt = from_qpoint(point);
  unsigned int mstate = mouse_state (event, true);
  string s = "release-" * mouse_decode (mstate);
  the_gui->process_mouse (tm_widget(), s, pt.x1, pt.x2,
                            mstate, texmacs_time());
  event->accept();
}

void
QTMWidget::mouseMoveEvent (QMouseEvent* event) {
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  coord2 pt = from_qpoint(point);
  unsigned int mstate = mouse_state (event, false);
  string s = "move";
  the_gui->process_mouse (tm_widget(), s, pt.x1, pt.x2, 
                          mstate, texmacs_time ());
  event->accept();
}

#if (QT_VERSION >= 0x050000)
static unsigned int
tablet_state (QTabletEvent* event, bool flag) {
  unsigned int i= 0;
  Qt::MouseButtons bstate= event->buttons ();
  Qt::MouseButton  tstate= event->button ();
  if (flag) bstate= bstate | tstate;
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
#if QT_VERSION < 0x060000
  if ((bstate & Qt::MidButton      ) != 0) i += 2;
#else
  if ((bstate & Qt::MiddleButton   ) != 0) i += 2;
#endif
  if ((bstate & Qt::RightButton    ) != 0) i += 4;
  if ((bstate & Qt::XButton1       ) != 0) i += 8;
  if ((bstate & Qt::XButton2       ) != 0) i += 16;
  return i;
}

void
QTMWidget::tabletEvent (QTabletEvent* event) {
#if QT_VERSION >= 0x060000
  // for testing purposes
  // cout << "tablet name= " << from_qstring(event->pointingDevice ()->name ()) << "\n";
#endif
  if (is_nil (tmwid)) return;
  unsigned int mstate = tablet_state (event, true);
  string s= "move";
  if (event->button() != 0) {
    if (event->pressure () == 0) s= "release-" * mouse_decode (mstate);
    else s= "press-" * mouse_decode (mstate);
  }
  if ((mstate & 4) == 0 || s == "press-right") {
#if QT_VERSION >= 0x060000
    QPoint point = event->position().toPoint() + origin() - surface()->pos();
    double x= point.x();
    double y= point.y();
#else
    QPoint point = event->pos() + origin() - surface()->pos();
    double x= point.x() + event->hiResGlobalX() - event->globalX();
    double y= point.y() + event->hiResGlobalY() - event->globalY();
#endif
    coord2 pt= coord2 ((SI) (x * PIXEL), (SI) (-y * PIXEL));
    array<double> data;
    data << ((double) event->pressure())
         << ((double) event->rotation())
         << ((double) event->xTilt())
         << ((double) event->yTilt())
         << ((double) event->z())
         << ((double) event->tangentialPressure());
    the_gui->process_mouse (tm_widget(), s, pt.x1, pt.x2, 
                            mstate, texmacs_time (), data);
  }
  /*
  cout << HRULE << LF;
  cout << "button= " << event->button() << LF;
  cout << "globalX= " << event->globalX() << LF;
  cout << "globalY= " << event->globalY() << LF;
  cout << "hiResGlobalX= " << event->hiResGlobalX() << LF;
  cout << "hiResGlobalY= " << event->hiResGlobalY() << LF;
  cout << "globalX= " << event->globalX() << LF;
  cout << "globalY= " << event->globalY() << LF;
  cout << "x= " << event->x() << LF;
  cout << "y= " << event->y() << LF;
  cout << "z= " << event->z() << LF;
  cout << "xTilt= " << event->xTilt() << LF;
  cout << "yTilt= " << event->yTilt() << LF;
  cout << "pressure= " << event->pressure() << LF;
  cout << "rotation= " << event->rotation() << LF;
  cout << "tangentialPressure= " << event->tangentialPressure() << LF;
  cout << "pointerType= " << event->pointerType() << LF;
  cout << "uniqueId= " << event->uniqueId() << LF;
  */
  event->accept();
}
#endif

void
QTMWidget::gestureEvent (QGestureEvent* event) {
  if (is_nil (tmwid)) return;
  string s= "gesture";
  array<double> data;
  QPointF hotspot;
  if (QGesture *swipe_gesture = event->gesture(Qt::SwipeGesture)) {
    QSwipeGesture *swipe= static_cast<QSwipeGesture *> (swipe_gesture);
    s= "swipe";
    hotspot = swipe->hotSpot ();
    if (swipe->state() == Qt::GestureFinished) {
      if (swipe->horizontalDirection() == QSwipeGesture::Left)
        s= "swipe-left";
      else if (swipe->horizontalDirection() == QSwipeGesture::Right)
        s= "swipe-right";
      else if (swipe->verticalDirection() == QSwipeGesture::Up)
        s= "swipe-up";
      else if (swipe->verticalDirection() == QSwipeGesture::Down)
        s= "swipe-down";
    }
    else {
      event->accept ();
      return;
    }
  }
  else if (QGesture *pan_gesture = event->gesture(Qt::PanGesture)) {
    QPanGesture *pan= static_cast<QPanGesture *> (pan_gesture);
    string s= "pan";
    hotspot = pan->hotSpot ();
    //QPointF delta = pan->delta();
    //cout << "Pan " << delta.x() << ", " << delta.y() << LF;
  }
  else if (QGesture *pinch_gesture = event->gesture(Qt::PinchGesture)) {
    QPinchGesture *pinch= static_cast<QPinchGesture *> (pinch_gesture);
    s= "pinch";
    hotspot = pinch->hotSpot ();
    QPinchGesture::ChangeFlags changeFlags = pinch->changeFlags();
#if (QT_VERSION >= 0x050000)
    if (pinch->state() == Qt::GestureStarted) {
      pinch->setRotationAngle (0.0);
      pinch->setScaleFactor (1.0);
      s= "pinch-start";
    }
    else if (pinch->state() == Qt::GestureFinished) {
      pinch->setRotationAngle (0.0);
      pinch->setScaleFactor (1.0);
      s= "pinch-end";
    }
    else if (changeFlags & QPinchGesture::RotationAngleChanged) {
      qreal angle = pinch->rotationAngle();
      s= "rotate";
      data << ((double) angle);
    }
    else if (changeFlags & QPinchGesture::ScaleFactorChanged) {
      qreal scale = pinch->totalScaleFactor();
      s= "scale";
      data << ((double) scale);
    }
#else
    if (pinch->state() == Qt::GestureStarted) {
      QPoint point (hotspot.x(), hotspot.y());
      coord2 pt = from_qpoint (point);
      the_gui->process_mouse (tm_widget(), "pinch-start", pt.x1, pt.x2, 
                              0, texmacs_time ());
    }
    if (changeFlags & QPinchGesture::RotationAngleChanged) {
      qreal a1 = pinch->lastRotationAngle();
      qreal a2 = pinch->rotationAngle();
      if (a2 != a1) {
        s= "rotate";
        data << ((double) a2);
      }
    }
    else if (changeFlags & QPinchGesture::ScaleFactorChanged) {
      qreal s1 = pinch->lastScaleFactor();
      qreal s2 = pinch->scaleFactor();
      if (s1 != s2) {
        s= "scale";
        data << ((double) s2);
      }
      else {
        pinch->setScaleFactor (1.0);
        s= "pinch-end";
      }
    }
    else if (pinch->state() == Qt::GestureFinished) {
      pinch->setRotationAngle (0.0);
      pinch->setScaleFactor (1.0);
      s= "pinch-end";
    }
#endif
  }
  else return;
  QPoint point (hotspot.x(), hotspot.y());
  coord2 pt = from_qpoint (point);
  //cout << s << ", " << pt.x1 << ", " << pt.x2 << LF;
  the_gui->process_mouse (tm_widget(), s, pt.x1, pt.x2, 
                          0, texmacs_time (), data);
  event->accept();
}

 
bool
QTMWidget::event (QEvent* event) {
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
  if (event->type() == QEvent::Gesture) {
    gestureEvent(static_cast<QGestureEvent*>(event));
    return true;
  }
  return QTMScrollView::event (event);
}

void
QTMWidget::focusInEvent (QFocusEvent * event) {
  if (!is_nil (tmwid)) {
    if (DEBUG_QT) debug_qt << "FOCUSIN: " << tm_widget()->type_as_string() << LF;
    the_gui->process_keyboard_focus (tm_widget(), true, texmacs_time());
  }
  QTMScrollView::focusInEvent (event);
  // part 2/2 of the fix for bug 43373.
  if (!isEmbedded ()) {
    if (!isActiveWindow()) activateWindow();
    if (isActiveWindow() && !hasFocus()) setFocus (Qt::OtherFocusReason);
    //=> this will send us back here...
    //This redundancy is weird but definitely needed to properly get focus with Qt >= 5.15. Qt bug?
  }
}

void
QTMWidget::focusOutEvent (QFocusEvent * event) {
  if (!is_nil (tmwid)) {
    if (DEBUG_QT) debug_qt << "FOCUSOUT: " << tm_widget()->type_as_string() << LF;
    the_gui -> process_keyboard_focus (tm_widget(), false, texmacs_time());
  }
  QTMScrollView::focusOutEvent (event);
}

QSize
QTMWidget::sizeHint () const {
  SI w = 0, h = 0;
  if (!is_nil (tmwid)) tm_widget()->handle_get_size_hint (w, h);
  return to_qsize (w, h);
}

void 
QTMWidget::dragEnterEvent (QDragEnterEvent *event)
{
  if (is_nil (tmwid)) return;
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
QTMWidget::dropEvent (QDropEvent *event) {
  if (is_nil (tmwid)) return;

#if QT_VERSION >= 0x060000
  QPoint point = event->position ().toPoint () + origin ();
#else
  QPoint point = event->pos () + origin ();
#endif
  coord2 pt= from_qpoint (point);

  tree doc (CONCAT);
  const QMimeData *md= event->mimeData ();
  QByteArray buf;

  if (DEBUG_QT) debug_qt << "DropEvent formats :" << from_qstring(md->formats().join(",")) << LF;

  if (md->hasUrls ()) {
    QList<QUrl> l= md->urls ();
    for (int i=0; i<l.size (); i++) {

      string url = from_qstring(l[i].toString ());
      if (DEBUG_QT) debug_qt << "DropEvent URL [" << i << "] : " << url << LF;

      if (l[i].isLocalFile()) {
        string name;
#ifdef OS_MACOS
        name= from_qstring (fromNSUrl (l[i]));
#else
        name= from_qstring (l[i].toLocalFile ());
#endif
        string orig_name= name;
#ifdef OS_MINGW
        if (N(name) >=2 && is_alpha (name[0]) && name[1] == ':')
          name= "/" * locase_all (name (0, 1)) * name (2, N(name));
#endif
        string extension = suffix (name);
        if ((extension == "eps") || (extension == "ps")   ||
#if (QT_VERSION >= 0x050000)
            (extension == "svg") ||
#endif
            (extension == "pdf") || (extension == "png")  ||
            (extension == "jpg") || (extension == "jpeg")) {
          string w, h;
          qt_pretty_image_size (url_system (orig_name), w, h);
          tree im (IMAGE, name, w, h, "", "");
          doc << im;
        } else {
          doc << name;
        }
      } else {
        // not a local file, drop an slink to the document
        tree ln (HLINK, url, url);
        doc << ln;
        //FIXME: this is still not very nice as clicking on HLINK only works for a limited group
        //of schemas (http, https, ftp). For unrecognized schemas one would like to use the default OS behaviour
        //(e.g. the "message:" schema identify local email messages)
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
  } else if (md->hasText ()) {
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

static unsigned int
wheel_state (QWheelEvent* event) {
  // TODO: factor mouse_state, tablet_state, wheel_state
  // This should be easier on modern versions of Qt
  unsigned int i= 0;
  Qt::MouseButtons bstate= event->buttons ();
  Qt::KeyboardModifiers kstate= event->modifiers ();
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
#if QT_VERSION < 0x060000
  if ((bstate & Qt::MidButton      ) != 0) i += 2;
#else
  if ((bstate & Qt::MiddleButton   ) != 0) i += 2;
#endif
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
QTMWidget::wheelEvent(QWheelEvent *event) {
  if (is_nil (tmwid)) return; 
  if (as_bool (call ("wheel-capture?"))) {
#if (QT_VERSION >= 0x060000)
    QPointF pos  = event->position();
    QPoint  point= QPointF (pos.x(), pos.y()).toPoint () + origin();
#else
    QPoint  point= event->pos() + origin();
#endif
#if (QT_VERSION >= 0x050000)
    QPoint  wheel= event->pixelDelta();
#else
    double delta= event->delta();
    bool   hor  = event->orientation() == Qt::Horizontal;
    QPoint wheel (hor? delta: 0.0, hor? 0.0: delta);
#endif
    coord2 pt = from_qpoint (point);
    coord2 wh = from_qpoint (wheel);
    unsigned int mstate= wheel_state (event);
    array<double> data; data << ((double) wh.x1) << ((double) wh.x2);
    the_gui -> process_mouse (tm_widget(), "wheel", pt.x1, pt.x2,
                              mstate, texmacs_time (), data);
  }
  else if (QApplication::keyboardModifiers() == Qt::ControlModifier) {
#if QT_VERSION >= 0x060000
    QPoint numPixels = event->pixelDelta();
    QPoint numDegrees = event->angleDelta() / 8;
    
    // compute the zoom factor from numPixels or numDegrees
    double zoomFactor = 0.0; (void) zoomFactor;
    if (!numPixels.isNull()) {
      if (numPixels.y() > 0) {
        call ("zoom-in", object (sqrt (sqrt (sqrt (sqrt (numPixels.y()))))));
      } else {
        call ("zoom-out", object (sqrt (sqrt (sqrt (sqrt (-numPixels.y()))))));
      }
    } else if (!numDegrees.isNull()) {
      if (numDegrees.y() > 0) {
        call ("zoom-in", object (sqrt (sqrt (sqrt (sqrt (numDegrees.y()))))));
      } else {
        call ("zoom-out", object (sqrt (sqrt (sqrt (sqrt (-numDegrees.y()))))));
      }
    }
#else
    if (event->delta() > 0) {
      //double x= exp (((double) event->delta ()) / 500.0);
      //call ("zoom-in", object (x));
      call ("zoom-in", object (sqrt (sqrt (sqrt (sqrt (2.0))))));
    }
    else {
      //double x= exp (-((double) event->delta ()) / 500.0);
      //call ("zoom-out", object (x));
      call ("zoom-out", object (sqrt (sqrt (sqrt (sqrt (2.0))))));
    }
#endif
  }
  else QAbstractScrollArea::wheelEvent (event);
}

void QTMWidget::showEvent (QShowEvent *event) {
  (void) event;
  the_gui->force_update();
}

#if defined(OS_ANDROID) && QT_VERSION >= 0x060000
void QTMWidget::showKeyboard() {
  qApp->inputMethod()->show();
}
#endif
