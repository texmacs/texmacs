
/******************************************************************************
* MODULE     : layout.hpp
* DESCRIPTION: some general routines for the layout of widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
