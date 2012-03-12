
/******************************************************************************
* MODULE     : qt_menu.cpp
* DESCRIPTION: QT menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_menu.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_simple_widget.hpp"
#include "qt_basic_widgets.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMStyle.hpp"
#include "analyze.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
//#import "TMView.h"
#include <QtGui>


#include "QTMGuiHelper.hpp"
#include "qt_gui.hpp"

// REMARK on memory management.
// the hierarchy of a QMenu has parents correctly set to the proper supermenu
// this guarantees that deletion of the root menu correclty deletes all the tree below it.
// the root menu itself (without parent QObject) is "owned" by the associate qt_menu_rep instance
// and it is deallocated by it. This ensures correct memory management between TeXmacs and Qt
// since qt_menu_rep is sometimes cached at TeXmacs level.
// this also means that when we install some menu in the GUI (in the main menu or in the toolbar)
// we should just add actions and not reroot the qt parent hierarchy since even if the menu will
// be eventually removed from the GUI it has some chance to still be cached in the TeXmacs side
// conventions are as follows:
// - the method as_qaction passes the ownership of the action and the eventual submenu to the caller 
//   responsibility. When creating menu hierachies (eg. via the scheme interface) you should use this method
//   to retrieve the relevant Qt objects.
// - the method qt_menu_rep::get_menu preserves the onwership of the menu to the called qt_menu_rep
//   (to guarantee correct caching). when installing menus in the gui you should use this method.
// Submenus belongs implicitly to the parent QTMAction since it controls if the menu has an explicit parent
// and in negative case delete the submenu. All submenus in the menu hierarchy should have an empty parent widget
// and be attached to some QTMAction. This guarantees correct memory management.



QTMAction::QTMAction(QObject *parent) : QAction(parent) { 
  QObject::connect(the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
  _timer = new QTimer(this);
  QObject::connect(_timer, SIGNAL(timeout()), this, SLOT(doShowToolTip()));
}

QTMAction::~QTMAction() { 
  if (menu() && !(menu()->parent())) delete menu(); 
}


void 
QTMAction::doRefresh() {
  if (N(str)) {
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText(to_qstring (t));
  }
}

void
QTMAction::showToolTip()
{
  _timer->start(500);   // Restarts the timer if already running
  _pos = QCursor::pos();
}

/*
 * This is the best I could come up with: under MacOSX menu items receive no
 * mouse events, nor are they QWidgets whose geometry we can query. As far as I
 * know, it is not possible to know whether the menu item currently under the
 * cursor is this particular one, so in order to avoid displaying outdated
 * toolTips (because the user moved fast over items) we compute distances.
 * This is obviously wrong, and will behave weirdly under certain resolutions,
 * for given menu item sizes, etc. Also, one typically moves for a while 
 * horizontally over the first item in an extensible menu, so once the user
 * stops, the distance is bigger than the given constant and no tooltip is
 * displayed.
 */
void
QTMAction::doShowToolTip() {
  _timer->stop();
  if((QCursor::pos() - _pos).manhattanLength() < 10)  // Hideous HACK
    QToolTip::showText(QCursor::pos(), toolTip());
  else
    QToolTip::hideText();
}


/*******************************************************************************
* Default action is empty.
*******************************************************************************/

QAction *qt_widget_rep::as_qaction() {
  QAction *a = new QTMAction (NULL); 
  //  a->setSeparator(true);
  a->setEnabled(false);
  return a;
};


/******************************************************************************/


qt_menu_rep::qt_menu_rep (QAction* _item) 
 : item (_item ? _item : new QTMAction (NULL)) {  }


QAction*
qt_menu_rep::as_qaction() {
  // FIXME: the convention is that as_qaction give ownership of
  // the action to the caller. However in this case we do not want
  // to replicate the action so we must be sure to be called only once.
  if (!item) cout << "THIS MUST NOT HAPPEN TWICE" << LF;
  QAction *ret = item;
  item = NULL;
  return ret;
}

widget
qt_menu_rep::make_popup_widget () {
  return this;
}

widget
qt_menu_rep::popup_window_widget (string s) {
  item->menu()->setWindowTitle (to_qstring (s));
  return this;
}

widget
qt_menu_rep::plain_window_widget (string s, command q) {
  item->menu()->setWindowTitle (to_qstring (s));
  (void) q; // FIXME: to be ignored?
  return this;
}

void
qt_menu_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_menu_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    ASSERT (type_box (val) == type_helper<coord2>::id, "type mismatch");
    break;
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (flag)
        item->menu()->show();
      (void) flag;
    }   
    break;
  case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);
      (void) flag;
      // [NSMenu popUpContextMenu:[item submenu] withEvent:[NSApp currentEvent] forView:( (qt_view_widget_rep*)(the_keyboard_focus.rep))->view ];
      if (item->menu ())
        item->menu()->exec (QCursor::pos ());
    }   
    break;
  default:
    FAILED ("cannot handle slot type");
  }
}



QPixmap
impress (simple_widget_rep* wid) {
  if (wid) {
    int width, height;
    wid->handle_get_size_hint (width, height);
    QSize s = QSize (width/PIXEL, height/PIXEL);
    QPixmap pxm(s);
    //cout << "impress (" << s.width() << "," << s.height() << ")\n";
    pxm.fill (Qt::transparent);
    {
      qt_renderer_rep *ren = the_qt_renderer();
      ren->begin (static_cast<QPaintDevice*>(&pxm));
      wid->set_current_renderer(the_qt_renderer());
      rectangle r = rectangle (0, 0, s.width(), s.height());
      ren->set_origin(0,0);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      {
        // we do not want to be interrupted here...
        extern bool disable_check_event;
        bool cache = disable_check_event;
        disable_check_event= true;
        wid->handle_repaint (r->x1, r->y2, r->x2, r->y1);
        disable_check_event= cache;
      }
      ren->end();
      wid->set_current_renderer(NULL);
    }
    return pxm;
  }
  else {
    // return arbitrary image...
    QPixmap pxm (10, 10);
    return pxm;
  }
}

QAction*
simple_widget_rep::as_qaction () {
  QAction* a= new QTMAction (NULL);
  QPixmap pxm (impress (this));
  QIcon icon (pxm);
  a->setIcon (icon);
  return a;
}

