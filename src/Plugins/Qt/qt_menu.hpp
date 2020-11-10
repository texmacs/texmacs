
/******************************************************************************
* MODULE     : qt_menu.hpp
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


/*! Implements a system standard contextual popup menu.

 @note This object is *not intended* to be used in toolbars, menu bars or
 windows.
 
 This widget owns no underlying QWidget but a QAction. When SLOT_VISIBILITY or
 SLOT_MOUSE_GRAB are sent, it show()s or exec()s the QMenu associated to the
 action.
 
 REMARK on memory management:
 
 Once a qt_menu_rep is created with some qt_widget as content, it owns all the
 QActions in the latter and is responsible for their deletion.
 Access to the QObject generating virtual functions is set to private to
 enforce this rule (notice that the overriden methods in the base class return
 empty values).
 The hierarchy of a QMenu must have parents correctly set to the proper 
 supermenu, in order to guarantee that deletion of the root menu correctly
 deletes all of the tree below it. The root menu itself (without parent QObject)
 is owned by us as explained. This ensures correct memory management between
 TeXmacs and Qt since qt_menu_rep is sometimes cached at TeXmacs level.
 */
class qt_menu_rep: public qt_widget_rep {
  QAction*     qact;
  qt_widget content;

public:
  qt_menu_rep (qt_widget content);
  virtual ~qt_menu_rep();

  virtual void send (slot s, blackbox val);
  virtual widget popup_window_widget (string s);

private:
  QMenu* get_qmenu();
};

#endif // defined QT_MENU_HPP
