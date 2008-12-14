
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

QMenu* to_qmenu (widget w);
QAction* to_qaction (widget w);

#endif // defined QT_MENU_HPP
