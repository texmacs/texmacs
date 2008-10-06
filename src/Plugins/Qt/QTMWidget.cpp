
/******************************************************************************
* MODULE     : QTMWidget.cpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <QtGui>
#include "QTMWidget.hpp"

#include "QTMWidget.moc"

#include "qt_renderer.hpp"


#include "converter.hpp"


#define PIXEL 256

inline void scale (QPoint &point)
{	
  point.rx() *= PIXEL; point.ry() *= -PIXEL;
}



hashmap<int,string> qtkeymap (0);

inline void map(int code, string name)
{
  qtkeymap(code) = name;
}

void initkeymap()
{
  map(Qt::Key_Space,"space");
  map(Qt::Key_Return,"return");
  map(Qt::Key_Tab,"tab");
  map(Qt::Key_Backspace,"backspace");
  map(Qt::Key_Enter,"enter");
  map(Qt::Key_Escape,"escape");
  //map(0x0003,"K-enter");
  map(Qt::Key_Backspace,"backspace");
  
  map( Qt::Key_Up       ,"up" );
  map( Qt::Key_Down     ,"down" );
  map( Qt::Key_Left     ,"left" );
  map( Qt::Key_Right    ,"right" );
  map( Qt::Key_F1    ,"F1" );
  map( Qt::Key_F2    ,"F2" );
  map( Qt::Key_F3    ,"F3" );
  map( Qt::Key_F4    ,"F4" );
  map( Qt::Key_F5    ,"F5" );
  map( Qt::Key_F6    ,"F6" );
  map( Qt::Key_F7    ,"F7" );
  map( Qt::Key_F8    ,"F8" );
  map( Qt::Key_F9    ,"F9" );
  map( Qt::Key_F10   ,"F10" );
  map( Qt::Key_F11   ,"F11" );
  map( Qt::Key_F12   ,"F12" );
  map( Qt::Key_F13   ,"F13" );
  map( Qt::Key_F14   ,"F14" );
  map( Qt::Key_F15   ,"F15" );
  map( Qt::Key_F16   ,"F16" );
  map( Qt::Key_F17   ,"F17" );
  map( Qt::Key_F18   ,"F18" );
  map( Qt::Key_F19   ,"F19" );
  map( Qt::Key_F20   ,"F20" );
  map( Qt::Key_F21   ,"F21" );
  map( Qt::Key_F22   ,"F22" );
  map( Qt::Key_F23   ,"F23" );
  map( Qt::Key_F24   ,"F24" );
  map( Qt::Key_F25   ,"F25" );
  map( Qt::Key_F26   ,"F26" );
  map( Qt::Key_F27   ,"F27" );
  map( Qt::Key_F28   ,"F28" );
  map( Qt::Key_F29   ,"F29" );
  map( Qt::Key_F30   ,"F30" );
  map( Qt::Key_F31   ,"F31" );
  map( Qt::Key_F32   ,"F32" );
  map( Qt::Key_F33   ,"F33" );
  map( Qt::Key_F34   ,"F34" );
  map( Qt::Key_F35   ,"F35" );
  map( Qt::Key_Insert        ,"insert" );
  map( Qt::Key_Delete        ,"delete" );
  map( Qt::Key_Home  ,"home" );
  // map( Qt::Key_Begin         ,"begin" );
  map( Qt::Key_End   ,"end" );
  map( Qt::Key_PageUp        ,"pageup" );
  map( Qt::Key_PageDown      ,"pagedown" );
  // map( Qt::Key_PrintScreen   ,"printscreen" );
  map( Qt::Key_ScrollLock    ,"scrolllock" );
  map( Qt::Key_Pause         ,"pause" );
  map( Qt::Key_SysReq        ,"sysreq" );
  // map( Qt::Key_Break         ,"break" );
  //  map( Qt::Key_Reset         ,"reset" );
  map( Qt::Key_Stop  ,"stop" );
  map( Qt::Key_Menu  ,"menu" );
  //  map( Qt::Key_User  ,"user" );
  //  map( Qt::Key_System        ,"system" );
  map( Qt::Key_Print         ,"print" );
  //  map( Qt::Key_ClearLine     ,"clear" );
  // map( Qt::Key_ClearDisplay  ,"cleardisplay" );
  // map( Qt::Key_InsertLine    ,"insertline" );
  // map( Qt::Key_DeleteLine    ,"deleteline" );
  // map( Qt::Key_InsertChar    ,"insert" );
  // map( Qt::Key_DeleteChar    ,"delete" );
  // map( Qt::Key_Prev  ,"prev" );
  // map( Qt::Key_Next  ,"next" );
  map( Qt::Key_Select        ,"select" );
  map( Qt::Key_Execute       ,"execute" );
  // map( Qt::Key_Undo  ,"undo" );
  // map( Qt::Key_Redo  ,"redo" );
  // map( Qt::Key_Find  ,"find" );
  map( Qt::Key_Help  ,"help" );
  // map( Qt::Key_ModeSwitchFunctionKey    ,"modeswitch" );  
  map( Qt::Key_Dead_Acute, "acute");
}


void QTMWidget::postponedUpdate(QRect r) {
  update(r);
}

void QTMWidget::paintEvent ( QPaintEvent * event ) {
  
  QRect rect = event->rect();

  if (DEBUG_EVENTS) {
    QPainter p(this);
    QBrush brush(QColor("red"));
    p.fillRect(rect,brush);
  }
  
  
  the_qt_renderer()->begin (this);  

  the_qt_renderer() -> set_clipping
    (rect.x()*PIXEL, -(rect.y()+rect.height())*PIXEL, 
     (rect.x()+rect.width())*PIXEL, -rect.y()*PIXEL);
  tm_widget()->handle_repaint
    (rect.x()*PIXEL, -(rect.y()+rect.height())*PIXEL, 
     (rect.x()+rect.width())*PIXEL, -rect.y()*PIXEL);

  if (the_qt_renderer()->interrupted()) {
    if (DEBUG_EVENTS)
      cout << "POSTPONE\n"; 
    QTimer::singleShot(0, this, SLOT(postponedUpdate(rect)));
  }
	
  the_qt_renderer()->end();
}


void QTMWidget::keyPressEvent ( QKeyEvent * event )  
{
  static bool fInit = false;
  if (!fInit) {
    if (DEBUG_EVENTS)
      cout << "Initializing keymap\n";
    initkeymap();
    fInit= true;
  }
	
  if (DEBUG_EVENTS)
    cout << "keypressed\n";
  simple_widget_rep *wid =  tm_widget();
  if (!wid) return;
  
  {
    // char str[256];
    int key = event->key();
    QString nss = event->text();
    Qt::KeyboardModifiers mods = event->modifiers();
    if (DEBUG_EVENTS) {
      cout << "key  : " << key << LF;
      cout << "text : " << nss.toAscii().data() << LF;
      //cout << "count: " << nss.count() << LF;
      if (mods & Qt::ShiftModifier) cout << "shift\n";
      if (mods & Qt::MetaModifier) cout << "meta\n";
      if (mods & Qt::ControlModifier) cout << "control\n";
      if (mods & Qt::KeypadModifier) cout << "keypad\n";
      if (mods & Qt::AltModifier) cout << "alt\n";
    }

    bool flag= true;
    string r;
    if (qtkeymap->contains (key)) {
      r = qtkeymap[key];
      if (mods & Qt::ShiftModifier) r= "S-" * r;
    }
    else {
      QByteArray buf= nss.toUtf8();
      string rr (buf.constData(), buf.count());
      r= utf8_to_cork (rr);
      unsigned short unic= nss.data()[0].unicode();
      if (unic > 0 && unic < 32) {
	if ((mods & Qt::ShiftModifier) == 0)
	  if (((char) key) >= 'A' && ((char) key) <= 'Z')
	    key= (int) (key + ((int) 'a') - ((int) 'A'));
	r= string ((char) key);
      }
      else {
	if (unic == 168) r= "umlaut";
	if (unic == 96) {
	  if ((mods & Qt::AltModifier) != 0) r= "grave";
	  else r= "`";
	}
	if (unic == 180) r= "acute";
	if (unic == 710) r= "hat";
	if (unic == 732) r= "tilde";
	flag= false;
      }
    }

    if (mods & Qt::MetaModifier) r= "C-" * r;
    if (mods & Qt::ControlModifier) r= "Mod1-" * r;
    //if (mods & Qt::KeypadModifier) r= "Mod3-" * r;
    if (flag && ((mods & Qt::AltModifier) != 0)) r= "Mod4-" * r;

    if (r == "") return;
    if (DEBUG_EVENTS)
      cout << "key press: " << r << LF;
    wid -> handle_keypress (r, texmacs_time());        
  }
}


void QTMWidget::mousePressEvent ( QMouseEvent * event ) {
  simple_widget_rep *wid =  tm_widget();
  if (!wid) return;
  QPoint point = event->pos();
  scale(point);
  Qt::KeyboardModifiers flags = event->modifiers();
  if (flags & Qt::MetaModifier)
    wid -> handle_mouse ("press-right", point.x(), point.y(),
			 3, texmacs_time());
  else if (flags & Qt::AltModifier)
    wid -> handle_mouse ("press-middle", point.x(), point.y(),
			 2, texmacs_time());
  else
    wid -> handle_mouse ("press-left", point.x(), point.y(),
			 1, texmacs_time());
}

void QTMWidget::mouseReleaseEvent ( QMouseEvent * event ) {
  simple_widget_rep *wid =  tm_widget();
  if (!wid) return;
  QPoint point = event->pos();
  scale(point);
  wid -> handle_mouse ("release-left", point.x(), point.y(),  1, texmacs_time()); // FIXME: rough implementation
  
}

void QTMWidget::mouseMoveEvent ( QMouseEvent * event ) {
  simple_widget_rep *wid =  tm_widget();
  if (!wid) return;
  QPoint point = event->pos();
  scale(point);
  wid -> handle_mouse ("move", point.x(), point.y(),  1, texmacs_time()); // FIXME: rough implementation
  
}

bool QTMWidget::event(QEvent *event)
{
	if (event->type() == QEvent::KeyPress) {
		QKeyEvent *ke = static_cast<QKeyEvent *>(event);
		keyPressEvent(ke);
		return true;
	}
	
	return QWidget::event(event);
}


