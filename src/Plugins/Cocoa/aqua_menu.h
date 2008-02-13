
/******************************************************************************
* MODULE     : aqua_menu.h
* DESCRIPTION: Aqua menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef AQUA_MENU_H
#define AQUA_MENU_H

#include "aqua_widget.h"

NSMenu* to_nsmenu(widget w);
NSMenuItem* to_nsmenuitem(widget w);


#endif // defined AQUA_MENU_H
