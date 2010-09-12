
/******************************************************************************
* MODULE     : QTMWidget.cpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli and Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtGui>

#include "QTMWidget.hpp"
#include "qt_renderer.hpp"
#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_simple_widget.hpp"
#include "converter.hpp"

#include "config.h"
#include "message.hpp" 

#ifdef USE_CAIRO
#include "Cairo/cairo_renderer.hpp"
#include "Cairo/tm_cairo.hpp"

#if defined(Q_WS_X11)
#include <QX11Info>
extern Drawable qt_x11Handle(const QPaintDevice *pd);
extern const QX11Info *qt_x11Info(const QPaintDevice *pd);
#undef KeyPress  // conflict between QEvent::KeyPrees and X11 defintion
#endif // Q_WS_X11

#endif // USE_CAIRO

#include <QEvent>

#define PIXEL 256

QSet<QTMWidget*> QTMWidget::all_widgets;


hashmap<int,string> qtkeymap (0);
hashmap<int,string> qtdeadmap (0);

inline void
scale (QPoint& point) {
  point.rx() *= PIXEL; point.ry() *= -PIXEL;
}

inline void
map (int code, string name) {
  qtkeymap(code) = name;
}
inline void
deadmap (int code, string name) {
  qtdeadmap(code) = name;
}

void
initkeymap () {
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



QTMWidget::QTMWidget (simple_widget_rep *_wid) 
  : QTMScrollView (), backingPixmap() {
  setObjectName("A QTMWidget");
  setProperty ("texmacs_widget", QVariant::fromValue ((void*) _wid));
  QAbstractScrollArea::viewport()->setMouseTracking (true);
  setFocusPolicy (Qt::StrongFocus);
  backing_pos = origin;
  setAttribute(Qt::WA_InputMethodEnabled);
}


QTMWidget::~QTMWidget () {
  if (DEBUG_QT) cout << "destroying " << this << LF;
}

void 
QTMWidget::invalidate_rect (int x1, int y1, int x2, int y2) {
  // larger rectangle is needed to avoid artifacts during cursor movements
  // see bug #28664
  rectangle r = rectangle (x1-3, y1-1, x2+4, y2+1);
  // cout << "invalidating " << r << LF;
  invalid_regions = invalid_regions | rectangles (r);
}

void 
QTMWidget::invalidate_all () {
  QSize sz = QAbstractScrollArea::viewport()->size();
   //cout << "invalidate all " << LF;
  invalid_regions = rectangles();
  invalidate_rect (0, 0, sz.width(), sz.height());
}


basic_renderer_rep* 
QTMWidget::getRenderer() {
#ifdef USE_CAIRO
  cairo_renderer_rep *ren = the_cairo_renderer ();
  cairo_surface_t *surf;
#ifdef Q_WS_X11
  //const QX11Info & info = x11Info();//qt_x11Info(this);
  //    Display *dpy = x11Info().display();
  //backingPixmap = QPixmap(width(),height());
  //cout << backingPixmap.width() << LF;
  Display *dpy = QX11Info::display();
  Drawable drawable = backingPixmap.handle();
  Visual *visual = (Visual*)(backingPixmap.x11Info().visual());
  surf = tm_cairo_xlib_surface_create (dpy, drawable, visual, 
                            backingPixmap.width (), backingPixmap.height ());
#elif defined(Q_WS_MAC)
  surf = tm_cairo_quartz_surface_create_for_cg_context (
                    (CGContextRef)(this->macCGHandle()), width(), height());
#endif
  cairo_t *ct = tm_cairo_create (surf);
  ren->begin (ct);
  tm_cairo_surface_destroy (surf);
  tm_cairo_destroy (ct);
#else
  qt_renderer_rep * ren = the_qt_renderer();
  ren->begin(&backingPixmap);
#endif
  return ren;
}

void
QTMWidget::repaint_invalid_regions () {

  // this function is called by the qt_gui::update method to keep the backing
  // store in sync and propagate the changes to the surface on screen.
  // first we check that the backing store geometry is right and then we
  // request to the texmacs canvas widget to repaint the regions which were
  // marked invalid. Subsequently, for each succesfully repainted region, we
  // propagate its contents from the backing store to the onscreen surface.
  // If repaint has been interrupted we do not propagate the changes and proceed
  // to mark the region invalid again.

  QRegion qrgn; 
  // qrgn is to keep track of the area on the sceen which needs to be updated 

  // update backing store origin wrt. TeXmacs document
  if ( backing_pos != origin ) {

    int dx =  origin.x() - backing_pos.x();
    int dy =  origin.y() - backing_pos.y();
    backing_pos = origin;
    
    QPixmap newBackingPixmap (backingPixmap.size());
    QPainter p (&newBackingPixmap);
    //newBackingPixmap.fill(Qt::black);
    p.drawPixmap(-dx,-dy,backingPixmap);
    p.end();
    backingPixmap = newBackingPixmap;
    //cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
    
    QSize sz = backingPixmap.size();
    
    rectangles invalid;
    while (!is_nil(invalid_regions)) {
      rectangle r = invalid_regions->item ;
      //      rectangle q = rectangle(r->x1+dx,r->y1-dy,r->x2+dx,r->y2-dy);
      rectangle q = rectangle(r->x1-dx,r->y1-dy,r->x2-dx,r->y2-dy);
      invalid = rectangles (q, invalid);
      //cout << r << " ---> " << q << LF;
      invalid_regions = invalid_regions->next;
    }
    invalid_regions= invalid & 
    rectangles(rectangle(0,0,
                         sz.width(),sz.height())) ;
    
    if (dy<0) 
      invalidate_rect(0,0,sz.width(),min(sz.height(),-dy));
    else if (dy>0)
      invalidate_rect(0,max(0,sz.height()-dy),sz.width(),sz.height());
    
    if (dx<0) 
      invalidate_rect(0,0,min(-dx,sz.width()),sz.height());
    else if (dx>0)
      invalidate_rect(max(0,sz.width()-dx),0,sz.width(),sz.height());
    
    // we cal update now to allow repainint of invalid regions
    // this cannot be done directly since interpose handler needs
    // to be run at least once in some situations
    // (for example when scrolling is initiated by TeXmacs itself)
    //the_gui->update();
    //  QAbstractScrollArea::viewport()->scroll(-dx,-dy);
   // QAbstractScrollArea::viewport()->update();
    qrgn += QRect(QPoint(0,0),sz);
  }
  
  // update backing store size
  {
    QSize _oldSize = backingPixmap.size();
    QSize _newSize = QAbstractScrollArea::viewport()->size();
    if (_newSize != _oldSize) {
      // cout << "RESIZING BITMAP"<< LF;
      QPixmap newBackingPixmap (_newSize);
      //QPainter p (&newBackingPixmap);
      //p.drawPixmap(0,0,backingPixmap);
      //p.end();
      backingPixmap = newBackingPixmap;
      invalidate_all();
      the_gui -> process_resize(tm_widget(), 0, 0); // FIXME
    }
  }
  
  // repaint invalid rectangles
  {
    rectangles new_regions;
    if (!is_nil (invalid_regions)) {
      rectangle lub= least_upper_bound (invalid_regions);
      if (area (lub) < 1.2 * area (invalid_regions))
        invalid_regions= rectangles (lub);
      
      basic_renderer_rep* ren = getRenderer();
      tm_widget()->set_current_renderer(ren);
      
      SI ox = -backing_pos.x()*PIXEL;
      SI oy = backing_pos.y()*PIXEL;
      
      rectangles rects = invalid_regions;
      invalid_regions = rectangles();
      
      while (!is_nil (rects)) {
        rectangle r = copy (rects->item);
        rectangle r0 = rects->item;
        QRect qr = QRect(r0->x1, r0->y1, r0->x2 - r0->x1, r0->y2 - r0->y1);
        //cout << "repainting " << r0 << "\n";
        ren->set_origin(ox,oy); 
        ren->encode (r->x1, r->y1);
        ren->encode (r->x2, r->y2);
        ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
        tm_widget()->handle_repaint (r->x1, r->y2, r->x2, r->y1);
        if (ren->interrupted ()) {
          //cout << "interrupted repainting of  " << r0 << "\n";
          //ren->set_color(green);
          //ren->line(r->x1, r->y1, r->x2, r->y2);
          //ren->line(r->x1, r->y2, r->x2, r->y1);
          invalidate_rect (r0->x1, r0->y1, r0->x2, r0->y2);
        } 
        qrgn += qr;
        rects = rects->next;
      }
      
      tm_widget()->set_current_renderer(NULL);
      ren->end();
    } // !is_nil(invalid_regions)
    
  }

  // propagate immediatly the changes to the screen  
  QAbstractScrollArea::viewport()->repaint(qrgn);
  
}

void 
QTMWidget::scrollContentsBy ( int dx, int dy ) {
  QTMScrollView::scrollContentsBy (dx,dy);
  // the_gui::update needs to be run as soon as possible to refresh the status
  // of the widget.
  needs_update(); 
}

void 
QTMWidget::resizeEvent( QResizeEvent* event ) {
  // cout << "QTMWidget::resizeEvent (" << event->size().width()
  //      << "," << event->size().height() << ")" << LF;
  QTMScrollView::resizeEvent (event);
  // the_gui::update needs to be run as soon as possible to refresh the status
  // of the widget.
  needs_update(); 
}

void
QTMWidget::paintEvent (QPaintEvent* event) {
  // In the current implementation repainting take place during the call to
  // the widget's repaint_invalid_regions method in the_gui::update. All
  // we have to do is to take the backing store and put it on screen according
  // to the QRegion marked invalid. 
  // CHECK: Maybe just put onscreen all the region bounding rectangle could not 
  // be so expensive.
  
  
  if (DEBUG_QT) 
  {
    QRect rect = event->rect ();
    cout << "paintEvent ("<< rect.x() << "," <<  rect.y()
    << "," <<  rect.width() << "," <<  rect.height() << ")" << LF ;
  }
    
  {    
    QPainter p (QAbstractScrollArea::viewport());
    QVector<QRect> rects = event->region().rects();
    for (int i=0; i< rects.count(); i++) {
      QRect qr = rects.at(i);
      p.drawPixmap(qr,backingPixmap,qr);
    }
  }
  
}


void
QTMWidget::keyPressEvent (QKeyEvent* event) {
  static bool fInit = false;
  if (!fInit) {
    if (DEBUG_QT)
      cout << "Initializing keymap\n";
    initkeymap();
    fInit= true;
  }

  if (DEBUG_QT)
    cout << "keypressed\n";
  simple_widget_rep *wid =  tm_widget();
  if (!wid) return;

  {
    int key = event->key();
    Qt::KeyboardModifiers mods = event->modifiers();

    if (DEBUG_QT) {
      cout << "key  : " << key << LF;
      cout << "text : " << event->text().toAscii().data() << LF;
      cout << "count: " << event->text().count() << LF;
      if (mods & Qt::ShiftModifier) cout << "shift\n";
      if (mods & Qt::MetaModifier) cout << "meta\n";
      if (mods & Qt::ControlModifier) cout << "control\n";
      if (mods & Qt::KeypadModifier) cout << "keypad\n";
      if (mods & Qt::AltModifier) cout << "alt\n";
    }

    string r;
    if (qtkeymap->contains (key)) {
      r = qtkeymap[key];
    } else if (qtdeadmap->contains (key)) {
      mods &=~ Qt::ShiftModifier;
      r = qtdeadmap[key];
    } else {
      QString nss = event->text();
      unsigned short unic= nss.data()[0].unicode();
      if (unic < 32 && key < 128) {
        if (((char) key) >= 'A' && ((char) key) <= 'Z') {
          if ((mods & Qt::ShiftModifier) == 0)
            key= (int) (key + ((int) 'a') - ((int) 'A'));
        }
        mods &=~ Qt::ShiftModifier;
        r= string ((char) key);
      } else {
        switch(unic) {
        case 96:   r= "`"; 
            // unicode to cork conversion not appropriate for this case...
#ifdef Q_WS_MAC
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
          string rr (buf.constData(), buf.count());
          r= utf8_to_cork (rr);
          if (r == "<less>") r= "<";
          if (r == "<gtr>") r= ">";
        }
#ifdef Q_WS_MAC
        // CHECKME: are these two MAC exceptions really needed?
        mods &=~ Qt::AltModifier;
#endif
        mods &=~ Qt::ShiftModifier;
      }
    }

    if (r == "") return;

#ifdef Q_WS_MAC
    if (mods & Qt::ShiftModifier) r= "S-" * r;
    if (mods & Qt::MetaModifier) r= "C-" * r;        // The "Control" key
    if (mods & Qt::ControlModifier) r= "Mod1-" * r;  // The "Command" key
    //if (mods & Qt::KeypadModifier) r= "Mod3-" * r;
    if (mods & Qt::AltModifier) r= "Mod4-" * r;
#else
    if (mods & Qt::ShiftModifier) r= "S-" * r;
    if (mods & Qt::ControlModifier) r= "C-" * r;
    if (mods & Qt::AltModifier) r= "Mod1-" * r;
    //if (mods & Qt::KeypadModifier) r= "Mod3-" * r;
    if (mods & Qt::MetaModifier) r= "Mod4-" * r;     // The "Windows" key
#endif

    if (DEBUG_QT)
      cout << "key press: " << r << LF;
    //int start= texmacs_time ();
    //wid -> handle_keypress (r, texmacs_time());
    the_gui -> process_keypress (wid, r, texmacs_time());
    //int end= texmacs_time ();
    //if (end > start) cout << "Keypress " << end - start << "\n";
  //  the_gui->update (); // FIXME: remove this line when
                        // edit_typeset_rep::get_env_value will be faster
    
//    needs_update();
  }
}

static unsigned int
mouse_state (QMouseEvent* event, bool flag) {
  unsigned int i= 0;
  Qt::MouseButtons bstate= event->buttons ();
  Qt::MouseButton  tstate= event->button ();
  Qt::KeyboardModifiers kstate= event->modifiers ();
  if (flag) bstate= bstate | tstate;
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
  if ((bstate & Qt::MidButton      ) != 0) i += 2;
  if ((bstate & Qt::RightButton    ) != 0) i += 4;
  if ((bstate & Qt::XButton1       ) != 0) i += 8;
  if ((bstate & Qt::XButton2       ) != 0) i += 16;
#ifdef Q_WS_MAC
  if ((kstate & Qt::AltModifier    ) != 0) i = 2;
  if ((kstate & Qt::MetaModifier   ) != 0) i = 4;
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 2048;
#else
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 512;
  if ((kstate & Qt::AltModifier    ) != 0) i += 2048;
  if ((kstate & Qt::MetaModifier   ) != 0) i += 16384;
#endif
  return i;
}

static string
mouse_decode (unsigned int mstate) {
  if      (mstate & 1 ) return "left";
  else if (mstate & 2 ) return "middle";
  else if (mstate & 4 ) return "right";
  else if (mstate & 8 ) return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}


void
QTMWidget::inputMethodEvent (QInputMethodEvent* event) {
  QString const & commit_string = event->commitString();
  if (!commit_string.isEmpty()) {
    int key = 0;
    for (int i = 0; i < commit_string.size(); ++i) {
      QKeyEvent ev(QEvent::KeyPress, key, Qt::NoModifier, commit_string[i]);
      keyPressEvent(&ev);
    }
  }
  event->accept();
}  

void
QTMWidget::mousePressEvent (QMouseEvent* event) {
  simple_widget_rep *wid= tm_widget ();
  if (!wid) return;
  QPoint point = event->pos() + origin;
  scale (point);
  unsigned int mstate= mouse_state (event, false);
  string s= "press-" * mouse_decode (mstate);
  the_gui -> process_mouse (wid, s, point.x (), point.y (), 
                            mstate, texmacs_time ());
 // wid -> handle_mouse (s, point.x (), point.y (), mstate, texmacs_time ());
  if (DEBUG_QT)
    cout << "mouse event: " << s << " at "
         << point.x () << ", " << point.y () << LF;
}

void
QTMWidget::mouseReleaseEvent (QMouseEvent* event) {
  simple_widget_rep *wid = tm_widget();
  if (!wid) return;
  QPoint point = event->pos() + origin;;
  scale (point);
  unsigned int mstate= mouse_state (event, true);
  string s= "release-" * mouse_decode (mstate);
  the_gui -> process_mouse (wid, s, point.x (), point.y (), 
                            mstate, texmacs_time ());
//  wid -> handle_mouse (s, point.x (), point.y (), mstate, texmacs_time ());
  if (DEBUG_QT)
    cout << "mouse event: " << s << " at "
         << point.x () << ", " << point.y () << LF;
}

void
QTMWidget::mouseMoveEvent (QMouseEvent* event) {
  simple_widget_rep *wid = tm_widget();
  if (!wid) return;
  QPoint point = event->pos() + origin;
  scale (point);
  unsigned int mstate= mouse_state (event, false);
  string s= "move";
  the_gui -> process_mouse (wid, s, point.x (), point.y (), 
                            mstate, texmacs_time ());
//  wid -> handle_mouse (s, point.x (), point.y (), mstate, texmacs_time ());
  if (DEBUG_QT)
    cout << "mouse event: " << s << " at "
         << point.x () << ", " << point.y () << LF;
}


bool
QTMWidget::event (QEvent* event) {
  if (event->type() == QEvent::KeyPress) {
    QKeyEvent *ke = static_cast<QKeyEvent*> (event);
    keyPressEvent (ke);
    return true;
  }
  return QTMScrollView::event (event);
}


void
QTMWidget::focusInEvent ( QFocusEvent * event ) {
  if (DEBUG_QT) cout << "FOCUSIN" << LF;
  simple_widget_rep *wid = tm_widget ();
  if (wid) {
    the_gui -> process_keyboard_focus (wid, true, texmacs_time());
    //wid -> handle_keyboard_focus (true, texmacs_time ());
  }
  QTMScrollView::focusInEvent (event);
}

void
QTMWidget::focusOutEvent ( QFocusEvent * event ) {
  if (DEBUG_QT)   cout << "FOCUSOUT" << LF;
  simple_widget_rep *wid = tm_widget ();
  if (wid) {
    the_gui -> process_keyboard_focus (wid, false, texmacs_time());
//    wid -> handle_keyboard_focus (false, texmacs_time ());
  }
  QTMScrollView::focusOutEvent (event);
}
