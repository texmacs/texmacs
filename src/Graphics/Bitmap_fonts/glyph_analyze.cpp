
/******************************************************************************
* MODULE     : glyph_analyze.cpp
* DESCRIPTION: analyze glyph properties
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"

double
fill_rate (glyph g) {
  int r= 0;
  for (int y=0; y<g->height; y++)
    for (int x=0; x<g->width; x++)
      r += g->get_1 (x, y);
  return ((double) r) / ((double) (g->width * g->height));
}
