
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


qt_menu_rep::qt_menu_rep (qt_widget _content)
 : qt_widget_rep (vertical_menu), qact (NULL), content (_content) { }

/*! Destructor. Remember that qt_menu is the only parsed widget which by
 default owns its actions.
 */
qt_menu_rep::~qt_menu_rep () {
  delete qact;
}

QMenu*
qt_menu_rep::get_qmenu() {
  if (!qact) qact = content->as_qaction();
  return qact->menu();
}

widget
qt_menu_rep::popup_window_widget (string s) {
  (void) s;
//  as_qaction()->menu()->setWindowTitle (to_qstring (s));  // totally useless
  return this;
}

void
qt_menu_rep::send (slot s, blackbox val) {
  switch (s) {
  case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      get_qmenu()->move (to_qpoint (open_box<coord2> (val)));
    }
    break;
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, s);
      get_qmenu()->setVisible (open_box<bool> (val));
    }   
    break;
  case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab
      if (flag) get_qmenu()->exec();
    }   
    break;
  default:
      qt_widget_rep::send(s, val);
      return;
  }
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_menu_rep: sent " << slot_name (s) 
                  << "\t\tto widget\t" << type_as_string() << LF;
}
