
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
#include "qt_window_widget.hpp"
#include "QTMMenuHelper.hpp"


qt_menu_rep::qt_menu_rep (QAction* _item) 
 : qt_widget_rep(vertical_menu), item (_item ? _item : new QTMAction (NULL)) { }

/*
 The convention is that any implementation of as_qaction() gives ownership of
 the action to the caller. However in this case, because we are the root menu,
 we do not want to replicate the action so we must be sure to be called only once.
 
 FIXME: I (mbd) don't see how exactly the code below relates to this assertion.
 */
QAction*
qt_menu_rep::as_qaction() {
  if (!item) cout << "THIS MUST NOT HAPPEN TWICE" << LF;
  QAction *ret = item;
  item = NULL;
  return ret;
}

QWidget*
qt_menu_rep::as_qwidget() {
  qwid= item->menu();
  item= NULL;
  return qwid;
}

/*!
 This method is actually never called, but we reimplement it just in case: the
 default implementation in qt_widget_rep creates an additional widget and is
 not suitable for system default popups.
 */
widget
qt_menu_rep::make_popup_widget () {
  return this;
}

widget
qt_menu_rep::popup_window_widget (string s) {
  item->menu()->setWindowTitle (to_qstring (s));  // totally useless
  return this;
}


// FIXME: does this make sense?
widget
qt_menu_rep::plain_window_widget (string s, command q) {
  qwid= as_qwidget();
  qwid->setWindowTitle (to_qstring (s));
  return tm_new<qt_window_widget_rep>(qwid, q);
}

void
qt_menu_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_menu_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      QPoint pt = to_qpoint (open_box<coord2> (val));
      if (item)
        item->menu()->move (pt);
      else if (DEBUG_QT)
        cout << "   Attempt to set position on empty menu." << LF;
    }
    break;
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (flag)
        if (item)
          item->menu()->show();
        else if (DEBUG_QT)
          cout << "   Attempt to set visibility on empty menu." << LF;
    }   
    break;
  case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab
      if (flag)
        if (item)
          item->menu()->exec();
        else if (DEBUG_QT)
          cout << "   Attempt to set mouse grab on empty menu." << LF;
    }   
    break;
  default:
      qt_widget_rep::send(s, val);
  }
}
