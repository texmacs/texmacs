
/******************************************************************************
* MODULE     : aqua_menu.h
* DESCRIPTION: Aqua menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef AQUA_MENU_H
#define AQUA_MENU_H

#include "aqua_widget.h"

NSMenu* to_nsmenu(widget w);
NSMenuItem* to_nsmenuitem(widget w);


#endif // defined AQUA_MENU_H
