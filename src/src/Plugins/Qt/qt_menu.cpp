
/******************************************************************************
* MODULE     : qt_menu.h
* DESCRIPTION: QT menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/


#include "qt_menu.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_simple_widget.hpp"
#include "qt_basic_widgets.hpp"

#include "QTMMenuHelper.hpp"
#include "widget.hpp" 
#include "message.hpp"

#include "promise.hpp"
//#import "TMView.h"
#include <QPointer>

extern char  *slot_name(slot s); // from qt_widget.cpp

class qt_menu_rep : public qt_widget_rep  {
public:
  QPointer<QAction> item;
  qt_menu_rep(QAction * _item) : item(_item) {  }
  ~qt_menu_rep()  {  }

  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget (); 
  virtual widget popup_window_widget (string s); 

  virtual QAction *as_qaction() { return item; }

};

widget qt_menu_rep::make_popup_widget ()
{
  return this;
}

widget qt_menu_rep::popup_window_widget (string s)
{
  item->menu()->setWindowTitle(to_qstring(s));
  return this;
}


void qt_menu_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_menu_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      if (type_box (val) != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_POSITION");
    }
    break;
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      (void) flag;
    }	
    break;
  case SLOT_MOUSE_GRAB:
    {	
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);
      (void) flag;
      //	[NSMenu popUpContextMenu:[item submenu] withEvent:[NSApp currentEvent] forView:( (qt_view_widget_rep*)(the_keyboard_focus.rep))->view ];
      if (item->menu()) {
	item->menu()->exec(QCursor::pos());
      }
    }	
    //			send_mouse_grab (THIS, val);
    break;

  default:
    fatal_error ("cannot handle slot type", "qt_menu_rep::send");
  }
}


/******************************************************************************
* Widgets for the construction of menus
******************************************************************************/

widget horizontal_menu (array<widget> a) 
// a horizontal menu made up of the widgets in a
{
  QAction *act = new QAction("Menu",NULL);
  QMenu *m = new QMenu();
  for(int i = 0; i < N(a); i++) {
    if (is_nil(a[i])) break;
    m->addAction(concrete(a[i])->as_qaction());
  };
  act->setMenu(m);
  return new qt_menu_rep(act);	
}

widget vertical_menu (array<widget> a) { return horizontal_menu(a); }
// a vertical menu made up of the widgets in a



widget tile_menu (array<widget> a, int cols) { (void) cols; return horizontal_menu(a); }
// a menu rendered as a table of cols columns wide & made up of widgets in a



widget menu_separator (bool vertical) 
// a horizontal or vertical menu separator
{
  (void) vertical;
  QAction *a = new QAction(NULL);
  a->setSeparator(true);
  return new qt_menu_rep(a); 
}

widget menu_group (string name, string lan) 
// a menu group; the name should be greyed and centered
{
  (void) lan;
  QAction *a = new QAction(to_qstring(name),NULL);
  a->setEnabled(false);
  return new qt_menu_rep(a);
}

widget pulldown_button (widget w, promise<widget> pw) 
// a button w with a lazy pulldown menu pw
{
  //	NSString *title = (is_nil(w)? @"":((qt_menu_text_rep*)w.rep)->text);
  //	NSMenuItem *mi = [[alloc_menuitem() initWithTitle:title action:NULL keyEquivalent:@""] autorelease];
  //	TMMenuItem* mi =  (TMMenuItem*)((qt_menu_rep*)w.rep) -> item;
  QAction* a = concrete(w) -> as_qaction();
   
  QTMLazyMenu *lm = new QTMLazyMenu(pw.rep);
  a->setMenu(lm);
  return new qt_menu_rep(a);
}

widget pullright_button (widget w, promise<widget> pw)
// a button w with a lazy pullright menu pw
{
  return pulldown_button(w, pw);
}


QAction* qt_text_widget_rep::as_qaction()
{
  return new QAction(to_qstring(str),NULL);
}

QAction* qt_image_widget_rep::as_qaction()
{
  QAction *a = new QAction(NULL);
  QPixmap *img = the_qt_renderer()->xpm_image(image);
  QIcon icon(*img);
  //cout << pxm.size().width() << " " <<  pxm.size().height() << "\n";
  a->setIcon(icon);  
  return  a;
}

QAction * qt_balloon_widget_rep::as_qaction()
{
  QAction *a = concrete(text)->as_qaction();
  a->setToolTip(to_qstring(((qt_text_widget_rep*)hint.rep)->str));
  return a;
}


widget menu_button (widget w, command cmd, string pre, string ks, bool ok) 
// a command button with an optional prefix (o, * or v) and
// keyboard shortcut; if ok does not hold, then the button is greyed
{
  (void) ks;
  QAction *a = NULL;
  
  a = concrete(w)->as_qaction();
  QTMCommand *c = new QTMCommand(cmd.rep);
  QObject::connect(a,SIGNAL(triggered()),c,SLOT(apply()));
  a->setEnabled((ok ? true : false));
  // FIXME: implement complete prefix handling and keyboard shortcuts
  // cout << "ks: "<< ks << "\n";
  a->setCheckable(pre!="" ? true : false);
  a->setChecked(pre!="" ? true : false);
  if (pre == "v") {
  } else if (pre == "*") {
    //		[mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
  } else if (pre == "o") {
  }
  return new qt_menu_rep(a);
}

widget balloon_widget (widget w, widget help) 
// given a button widget w, specify a help balloon which should be displayed
// when the user leaves the mouse pointer on the button for a small while
{ 
  return new qt_balloon_widget_rep(w,help);
}

extern string max_translate (string);

widget text_widget (string s, color col, bool tsp, string lan) 
// a text widget with a given color, transparency and language
{
  return new qt_text_widget_rep(max_translate (s),col,tsp,lan);
}

widget xpm_widget (url file_name)// { return widget(); }
// a widget with an X pixmap icon
{
  return new qt_image_widget_rep(file_name);
#if 0  
  NSImage *image = the_qt_renderer()->xpm_image(file_name);
  //	TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:to_nsstring(as_string(file_name)) action:NULL keyEquivalent:@""] autorelease];
  TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:@"" action:NULL keyEquivalent:@""] autorelease];
  [mi setRepresentedObject:image];
  [mi setImage:image];
  return new qt_menu_rep(mi);
  //	return new qt_menu_text_rep(to_nsstring(as_string(file_name)));
#endif
}

QMenu* to_qmenu(widget w)
{
  QAction *a = concrete(w)->as_qaction();
  QMenu *m = a->menu();
  return m;
}

QAction* to_qaction(widget w)
{
  return concrete(w)->as_qaction();
}


QPixmap impress(simple_widget_rep *wid)
{
  if (wid)
    {
      int width, height;
      wid->handle_get_size_hint (width,height);
      QSize s = QSize(width/PIXEL,height/PIXEL);
		
      QPixmap pxm(s);
      the_qt_renderer()->begin(&pxm);
		
      QRect rect = QRect(0,0,s.width(),s.height());
      the_qt_renderer()->clear(rect.x()*PIXEL,  -(rect.y()+rect.height())*PIXEL, 
			       (rect.x()+rect.width())*PIXEL, -rect.y()*PIXEL);
		
      the_qt_renderer()->set_clipping (rect.x()*PIXEL,  -(rect.y()+rect.height())*PIXEL, 
				       (rect.x()+rect.width())*PIXEL, -rect.y()*PIXEL);
      wid->handle_repaint (rect.x()*PIXEL,  -(rect.y()+rect.height())*PIXEL, 
			   (rect.x()+rect.width())*PIXEL, -rect.y()*PIXEL);
		
		
      the_qt_renderer()->end();
      return pxm;
    }
  else {
    // return arbitrary image...
    QPixmap pxm(10,10);
    return pxm;
  }
}

QAction *simple_widget_rep::as_qaction()
{
  QAction *a = new QAction(NULL);
  QPixmap pxm(impress(this));
  QIcon icon(pxm);
  a->setIcon(icon);
  return a;
}


#include "QTMMenuHelper.moc"




void QTMLazyMenu::force()
{
  if (!forced) {
    widget w = pm->eval();
    qt_menu_rep *wid = (qt_menu_rep*)(w.rep); 
    QMenu *menu2 = wid->item->menu();
    replaceActions(this,menu2);
    delete (wid->item); 
    wid->item = NULL;
    DEC_COUNT_NULL(pm); pm = NULL;
    forced = true;
  }
	
	
}
