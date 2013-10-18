
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

/******************************************************************************
* Useful subroutines on rows and columns
******************************************************************************/

int
next_column (glyph g, int x, int dx) {
  while (x >= 0 && x < g->width) {
    for (int y=0; y<g->height; y++)
      if (g->get_1 (x, y) != 0) return x;
    x += dx;
  }
  return x;
}

int
next_row (glyph g, int y, int dy) {
  while (y >= 0 && y < g->height) {
    for (int x=0; x<g->width; x++)
      if (g->get_1 (x, y) != 0) return y;
    y += dy;
  }
  return y;
}

int
search_in_row (glyph g, int y, int what, int x, int dx) {
  while (x >= 0 && x < g->width) {
    if (g->get_1 (x, y) == what) return x;
    x += dx;
  }
  return x;
}

int
search_in_column (glyph g, int x, int what, int y, int dy) {
  while (y >= 0 && y < g->height) {
    if (g->get_1 (x, y) == what) return y;
    y += dy;
  }
  return y;
}

int
count_row_changes (glyph g, int y) {
  int cur= 0, count= 0;
  for (int x=0; x<g->width; x++)
    if (g->get_1 (x, y) != cur) {
      cur= g->get_1 (x, y);
      if (cur == 1) count++;
    }
  return count;
}

int
count_column_changes (glyph g, int x) {
  int cur= 0, count= 0;
  for (int y=0; y<g->height; y++)
    if (g->get_1 (x, y) != cur) {
      cur= g->get_1 (x, y);
      if (cur == 1) count++;
    }
  return count;
}

int
count_row_pixels (glyph g, int y) {
  int count= 0;
  for (int x=0; x<g->width; x++)
    count += g->get_1 (x, y);
  return count;
}

int
count_column_pixels (glyph g, int x) {
  int count= 0;
  for (int y=0; y<g->height; y++)
    count += g->get_1 (x, y);
  return count;
}

int
search_protrusion (glyph g, int x, int dx, int threshold) {
  int s= 0;
  while (x >= 0 && x<g->width && s < threshold) {
    s += count_column_pixels (g, x);
    x += dx;
  }
  return x;
}

/******************************************************************************
* Public routines
******************************************************************************/

int
pixel_count (glyph g) {
  int r= 0;
  for (int y=0; y<g->height; y++)
    for (int x=0; x<g->width; x++)
      r += g->get_1 (x, y);
  return r;
}

double
left_protrusion (glyph g, glyph o) {
  // propose a protrusion for the glyph g,
  // while providing the glyph of the character 'o' as a 'reference'
  if (g->lwidth <= 0 || o->lwidth <= 0) return 0.0;
  if (g->width <= 0 || o->width <= 0) return 0.0;
  int threshold= max (pixel_count (o) / 8, 1);
  int xg= search_protrusion (g, 0, 1, threshold);
  if (xg >= g->width) cout << "** too small\n";
  int xo= search_protrusion (o, 0, 1, threshold);
  //int dx= (xg + g->xoff) - (xo + o->xoff);
  //int dx= xg - xo;
  //return ((double) dx) / ((double) g->lwidth);
  return (((double) xg) / ((double) g->width)) -
         (((double) xo) / ((double) o->width));
}

double
fill_rate (glyph g) {
  return ((double) pixel_count (g)) / ((double) (g->width * g->height));
}

int
vertical_stroke_width (glyph g) {
  // Input : glyph of 'o' or 'O' character
  // Output: vertical stroke width
  int x= next_column (g, 0, 1);
  if (x >= g->width) return 0;
  int y1= search_in_column (g, x, 1, 0, 1);
  int y2= search_in_column (g, x, 1, g->height-1, -1);
  if (y1 >= g->height || y1 < 0) return 0;
  int y= (y1 + y2) >> 1;
  if (g->get_1 (x, y) != 1) return 0;
  int nx= search_in_row (g, y, 0, x, 1);
  return nx - x;
}

int
horizontal_stroke_width (glyph g) {
  // Input : glyph of 'o' or 'O' character
  // Output: horizontal stroke width
  int y= next_row (g, g->height-1, -1);
  if (y < 0) return 0;
  int x1= search_in_row (g, y, 1, 0, 1);
  int x2= search_in_row (g, y, 1, g->width-1, -1);
  if (x1 >= g->width || x1 < 0) return 0;
  int x= (x1 + x2) >> 1;
  if (g->get_1 (x, y) != 1) return 0;
  int ny= search_in_column (g, x, 0, y, -1);
  return y - ny;
}

bool
is_sans_serif (glyph g) {
  // Input : glyph of 'L' character
  // Output: determine whether we are using a sans serif font
  int y= next_row (g, 0, 1);
  if (count_row_changes (g, y) != 1) return false;
  int cur= count_row_pixels (g, y);
  while (y < (g->height >> 1)) {
    if (count_row_changes (g, y) != 1) return false;
    int next= count_row_pixels (g, y);
    if ((next + 1) < cur) return false;
    y++;
  }
  return g->height > 4;
}

double
get_slant (glyph g) {
  // Input : glyph of '[' character
  // Output: determine the slant being used
  int y1= next_row (g, 0, 1);
  int y2= next_row (g, g->height-1, -1);
  int dy= y2 - y1;
  if (dy <= 1) return 0.0;
  y1 += (dy / 10);
  y2 -= (dy / 10);
  int x1= search_in_row (g, y1, 1, 0, 1);
  int x2= search_in_row (g, y2, 1, 0, 1);
  double sl= ((double) (x1 - x2)) / ((double) (y2 - y1));
  if (sl > -0.05 && sl < 0.05) sl= 0.0;
  if (sl > 0.19 && sl < 0.21) sl= 0.2;
  if (sl > 0.24 && sl < 0.26) sl= 0.25;
  if (sl > 0.323 && sl < 0.343) sl= 0.33333;
  return sl;
}

int
italic_a_status (glyph g) {
  int x1= next_column (g, 0, 1);
  int x2= next_column (g, g->width-1, -1);
  int y1= next_row (g, 0, 1);
  int y2= next_row (g, g->height-1, -1);
  int c1= count_row_changes (g, (5*y1 + 3*y2) / 8);
  int c2= count_column_changes (g, (2*x1 + x2) / 3);
  if (c1 < 2) return 1;
  if (c1 > 2) return 2;
  if (c2 > 2) return 3;
  int y= (9*y1 + y2) / 10;
  for (; y<g->height; y++)
    if (count_row_changes (g, y) >= 2) break;
  for (; y<g->height; y++)
    if (count_row_changes (g, y) == 1) {
      int X1= search_in_row (g, y, 1, 0, 1);
      if (X1 > ((x1 + 2*x2) / 3)) break;
    }
  if (y < ((y1+y2+1) >> 1)) return 4;
  return 0;
}
