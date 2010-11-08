
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

color layout_pastel (renderer ren) { return rgb_color (228, 228, 220); }
color layout_light (renderer ren) { return light_grey; }
color layout_normal (renderer ren) { return grey; }
color layout_dark (renderer ren) { return dark_grey; }

void
layout_pastel (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_background (layout_pastel (ren));
  ren->set_color (layout_pastel (ren));
  ren->fill (x1, y1, x2, y2);
}

void
layout_default (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_background (layout_light (ren));
  ren->set_color (layout_light (ren));
  ren->fill (x1, y1, x2, y2);
}

void
layout_dark (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_background (layout_normal (ren));
  ren->set_color (layout_normal (ren));
  ren->fill (x1, y1, x2, y2);
}

void
layout_outline (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_line_style (PIXEL);
  ren->set_color (layout_light (ren));
  ren->line (x1, y1, x2, y1);
  ren->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  ren->line (x1, y2- PIXEL, x2, y2- PIXEL);
  ren->line (x1, y1, x1, y2- PIXEL);
}

void
layout_dark_outline (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_line_style (PIXEL);
  ren->set_color (layout_dark (ren));
  ren->line (x1, y1, x2, y1);
  ren->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  ren->line (x1, y2- PIXEL, x2, y2- PIXEL);
  ren->line (x1, y1, x1, y2- PIXEL);
}

void
layout_lower (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_line_style (PIXEL);
  ren->set_color (white);
  ren->line (x1, y1, x2-PIXEL, y1);
  ren->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  ren->set_color (layout_dark (ren));
  ren->line (x1, y2- PIXEL, x2, y2- PIXEL);
  ren->line (x1, y1, x1, y2- PIXEL);
}

void
layout_higher (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ren->set_line_style (PIXEL);
  ren->set_color (layout_dark (ren));
  ren->line (x1, y1, x2- PIXEL, y1);
  ren->line (x2- PIXEL, y1, x2- PIXEL, y2- PIXEL);
  ren->set_color (white);
  ren->line (x1, y2- PIXEL, x2, y2- PIXEL);
  ren->line (x1, y1, x1, y2- PIXEL);
}

void
layout_submenu_triangle (renderer ren, SI x, SI y) {
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x, y-3*PIXEL, x, y+3*PIXEL, x+7*PIXEL, y);
  ren->set_color (layout_dark (ren));
  ren->line (x, y-3*PIXEL, x, y+3*PIXEL);
  ren->line (x, y+3*PIXEL, x+7*PIXEL, y);
  ren->set_color (white);
  ren->line (x, y-3*PIXEL, x+7*PIXEL, y);
}

void
layout_pulldown_triangle (renderer ren, SI x, SI y) {
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x-4*PIXEL, y, x+4*PIXEL, y, x, y-4*PIXEL);
  ren->set_color (layout_dark (ren));
  ren->line (x+4*PIXEL, y, x, y-4*PIXEL);
  ren->set_color (white);
  ren->line (x-4*PIXEL, y, x+4*PIXEL, y);
  ren->line (x-4*PIXEL, y, x, y-4*PIXEL);
}

void
layout_pulldown_dash (renderer ren, SI x, SI y, SI w) {
  /*
  ren->set_line_style (PIXEL);
  ren->set_color (layout_dark (ren));
  ren->line (x+PIXEL, y, x+w-PIXEL, y);
  ren->set_color (layout_normal (ren));
  ren->line (x+PIXEL, y+PIXEL, x+w, y+PIXEL);
  ren->set_color (white);
  ren->line (x+PIXEL, y, x+PIXEL, y);
  ren->line (x, y+PIXEL, x+2*PIXEL, y+PIXEL);
  ren->line (x+PIXEL, y+2*PIXEL, x+w-PIXEL, y+2*PIXEL);
  */

  SI offset, inc= 3*PIXEL;
  bool parity= false;
  ren->set_line_style (PIXEL);

  for (offset= 0; offset<w; offset += inc, parity= !parity) {
    SI pos= x + offset;
    ren->set_color (parity? layout_normal (ren): layout_dark (ren));
    ren->line (pos, y, min (pos+inc, x+w), y);
    ren->set_color (parity? layout_normal (ren): white);
    ren->line (pos, y + PIXEL, min (pos+inc, x+w), y + PIXEL);
  }
}

void
layout_up_arrow (renderer ren, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hw= ((w/PIXEL)>>1)*PIXEL;
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x, y, x+w, y, x+hw, y+h);
  ren->set_color (layout_dark (ren));
  ren->line (x, y, x+w, y);
  ren->line (x+w, y, x+hw, y+h);
  ren->set_color (white);
  ren->line (x, y, x+hw, y+h);
}

void
layout_down_arrow (renderer ren, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hw= ((w/PIXEL)>>1)*PIXEL;
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x, y+h, x+w, y+h, x+hw, y);
  ren->set_color (layout_dark (ren));
  ren->line (x+w, y+h, x+hw, y);
  ren->set_color (white);
  ren->line (x, y+h, x+w, y+h);
  ren->line (x, y+h, x+hw, y);
}

void
layout_left_arrow (renderer ren, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hh= ((h/PIXEL)>>1)*PIXEL;
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x+w, y+w, x+w, y+h, x, y+hh);
  ren->set_color (layout_dark (ren));
  ren->line (x+w, y, x+w, y+h);
  ren->line (x+w, y, x, y+hh);
  ren->set_color (white);
  ren->line (x+w, y+h, x, y+hh);
}

void
layout_right_arrow (renderer ren, SI x, SI y, SI w, SI h) {
  w -= PIXEL; h -= PIXEL; SI hh= ((h/PIXEL)>>1)*PIXEL;
  ren->set_line_style (PIXEL);
  ren->set_color (layout_normal (ren));
  ren->triangle (x, y, x, y+h, x+w, y+hh);
  ren->set_color (layout_dark (ren));
  ren->line (x, y, x+w, y+hh);
  ren->set_color (white);
  ren->line (x, y+h, x+w, y+hh);
  ren->line (x, y, x, y+h);
}
