
/******************************************************************************
* MODULE     : layout.cpp
* DESCRIPTION: some general routines for the layout of widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"

color layout_light (window win) { return light_grey; }
color layout_normal (window win) { return grey; }
color layout_dark (window win) { return dark_grey; }

void
layout_default (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_background (layout_light (win));
  win->set_color (layout_light (win));
  win->fill (x1, y1, x2, y2);
}

void
layout_dark (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_background (layout_normal (win));
  win->set_color (layout_normal (win));
  win->fill (x1, y1, x2, y2);
}

void
layout_outline (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_line_style (PIXEL);
  win->set_color (layout_light (win));
  win->line (x1, y1, x2, y1);
  win->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  win->line (x1, y2- PIXEL, x2, y2- PIXEL);
  win->line (x1, y1, x1, y2- PIXEL);
}

void
layout_dark_outline (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_line_style (PIXEL);
  win->set_color (layout_dark (win));
  win->line (x1, y1, x2, y1);
  win->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  win->line (x1, y2- PIXEL, x2, y2- PIXEL);
  win->line (x1, y1, x1, y2- PIXEL);
}

void
layout_lower (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_line_style (PIXEL);
  win->set_color (white);
  win->line (x1, y1, x2-PIXEL, y1);
  win->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  win->set_color (layout_dark (win));
  win->line (x1, y2- PIXEL, x2, y2- PIXEL);
  win->line (x1, y1, x1, y2- PIXEL);
}

void
layout_higher (window win, SI x1, SI y1, SI x2, SI y2) {
  win->set_line_style (PIXEL);
  win->set_color (layout_dark (win));
  win->line (x1, y1, x2- PIXEL, y1);
  win->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  win->set_color (white);
  win->line (x1, y2- PIXEL, x2, y2- PIXEL);
  win->line (x1, y1, x1, y2- PIXEL);
}

void
layout_submenu_triangle (window win, SI x, SI y) {
  win->set_line_style (PIXEL);
  win->set_color (layout_normal (win));
  win->triangle (x, y-3*PIXEL, x, y+3*PIXEL, x+7*PIXEL, y);
  win->set_color (layout_dark (win));
  win->line (x, y-3*PIXEL, x, y+3*PIXEL);
  win->line (x, y+3*PIXEL, x+7*PIXEL, y);
  win->set_color (white);
  win->line (x, y-3*PIXEL, x+7*PIXEL, y);
}

void
layout_up_arrow (window win, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hw= ((w/PIXEL)>>1)*PIXEL;
  win->set_line_style (PIXEL);
  win->set_color (layout_normal (win));
  win->triangle (x, y, x+w, y, x+hw, y+h);
  win->set_color (layout_dark (win));
  win->line (x, y, x+w, y);
  win->line (x+w, y, x+hw, y+h);
  win->set_color (white);
  win->line (x, y, x+hw, y+h);
}

void
layout_down_arrow (window win, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hw= ((w/PIXEL)>>1)*PIXEL;
  win->set_line_style (PIXEL);
  win->set_color (layout_normal (win));
  win->triangle (x, y+h, x+w, y+h, x+hw, y);
  win->set_color (layout_dark (win));
  win->line (x+w, y+h, x+hw, y);
  win->set_color (white);
  win->line (x, y+h, x+w, y+h);
  win->line (x, y+h, x+hw, y);
}

void
layout_left_arrow (window win, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hh= ((h/PIXEL)>>1)*PIXEL;
  win->set_line_style (PIXEL);
  win->set_color (layout_normal (win));
  win->triangle (x+w, y+w, x+w, y+h, x, y+hh);
  win->set_color (layout_dark (win));
  win->line (x+w, y, x+w, y+h);
  win->line (x+w, y, x, y+hh);
  win->set_color (white);
  win->line (x+w, y+h, x, y+hh);
}

void
layout_right_arrow (window win, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hh= ((h/PIXEL)>>1)*PIXEL;
  win->set_line_style (PIXEL);
  win->set_color (layout_normal (win));
  win->triangle (x, y, x, y+h, x+w, y+hh);
  win->set_color (layout_dark (win));
  win->line (x, y, x+w, y+hh);
  win->set_color (white);
  win->line (x, y+h, x+w, y+hh);
  win->line (x, y, x, y+h);
}
