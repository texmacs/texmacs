
/******************************************************************************
* MODULE     : qt_menu.h
* DESCRIPTION: QT menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_MENU_HPP
#define QT_MENU_HPP

#include "qt_widget.hpp"
#include <QMenu>
#include <QAction>


/*! Implements a system standard popup menu or a collection of QActions for
    insertion in a QToolBar.
 
 This widget owns no underlying QWidget but a QAction. When SLOT_VISIBILITY or
 SLOT_MOUSE_GRAB are sent, it show()s or exec()s the QMenu associated to the
 action.
 
 REMARK on memory management:
 
 The hierarchy of a QMenu has parents correctly set to the proper supermenu, 
 this guarantees that deletion of the root menu correclty deletes all of the
 tree below it. The root menu itself (without parent QObject) is "owned" by the
 associated qt_menu_rep instance and it is deallocated by it. This ensures
 correct memory management between TeXmacs and Qt since qt_menu_rep is sometimes
 cached at TeXmacs level.
 This also means that whenever we install some menu in the GUI (in the main menu
 or in the toolbar) we should just add actions and not reroot the Qt parent 
 hierarchy, because even if the menu will be eventually removed from the GUI it
 has some chance to still be cached in the TeXmacs side.
 
 Conventions are as follows:
 
 - qt_widget_rep::as_qaction() gives ownership of the action and the eventual
   submenu to the caller responsibility. When creating menu hierachies (eg. via 
   the scheme interface) you should use this method to retrieve the relevant Qt
   objects. The exception to this policy is qt_menu_rep::as_qaction(), because
   as said above, qt_menu_rep is responsible for the deletion of the QObjects.
 
 - qt_menu_rep::get_qmenu() does not take ownership of the menu from the
   qt_menu_rep instance (to guarantee correct caching). When installing menus in
   the gui you should use this method. Submenus belong implicitly to the parent
   QTMAction because it controls whether the menu has an explicit parent and in
   case it hasn't, deletes the submenu. All submenus in the menu hierarchy
   should have an empty parent widget and be attached to some QTMAction. This
   guarantees correct memory management.
 */
class qt_menu_rep: public qt_widget_rep {
public:
  QAction  *item;
  qt_menu_rep (QAction* _item);
  ~qt_menu_rep () { 
    delete item; // the submenu is usually also deleted since item is a QTMAction
  }

  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
  virtual widget plain_window_widget (string s, command q);
  
  virtual QAction* as_qaction ();
  virtual QWidget* as_qwidget ();

  virtual QMenu* get_qmenu() { return (item ? item->menu() : NULL); }
};


#endif // defined QT_MENU_HPP
