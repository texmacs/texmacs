
/******************************************************************************
* MODULE     : layout.hpp
* DESCRIPTION: some general routines for the layout of widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LAYOUT_H
#define LAYOUT_H
#include "window.hpp"

color layout_light (window win);
color layout_normal (window win);
color layout_dark (window win);
void  layout_default (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_dark (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_outline (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_dark_outline (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_lower (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_higher (window win, SI x1, SI y1, SI x2, SI y2);
void  layout_submenu_triangle (window win, SI x, SI y);
void  layout_up_arrow (window win, SI x, SI y, SI w, SI h);
void  layout_down_arrow (window win, SI x, SI y, SI w, SI h);
void  layout_left_arrow (window win, SI x, SI y, SI w, SI h);
void  layout_right_arrow (window win, SI x, SI y, SI w, SI h);

#endif // defined LAYOUT_H
