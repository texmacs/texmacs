
/******************************************************************************
* MODULE     : glyph_unserif.cpp
* DESCRIPTION: removing serifs from characters
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "renderer.hpp"
#include "hashset.hpp"
#include "frame.hpp"

/******************************************************************************
* Graphical routines for removing serifs
******************************************************************************/

bool
is_empty (glyph gl, int i1, int i2, int j) {
  i1= max (i1, 0);
  i2= min (i2, gl->width - 1);
  for (int i= i1; i <= i2; i++)
    if (gl->get_x (i, j) != 0) return false;
  return true;
}

void
search (glyph gl, int j, int pw, int serial, int& i, int& rj) {
  bool start= true;
  int ww= gl->width;
  i= 0; rj= j;
  while (serial > 0 && i < ww) {
    if (start)
      while (i < ww && gl->get_x (i, j) == 0) i++;
    else
      while (i < ww && !is_empty (gl, i, i+pw-1, j)) i++;
    serial--;
    start= !start;
  }
  if (start && ww > i && i > 0) i--;
}

glyph
unserif (glyph gl, SI ypos, SI yrad, SI yext, SI penw, int serial) {
  if (serial < 0) {
    glyph fgl = hor_flip (gl);
    glyph ufgl= unserif (fgl, ypos, yrad, yext, penw, -serial);
    return hor_flip (ufgl);
  }
  int ww= gl->width, hh= gl->height;
  int pj= gl->yoff - (ypos + (PIXEL >> 1)) / PIXEL;
  int xj= gl->yoff - (yext + (PIXEL >> 1)) / PIXEL;
  int dj= (yrad + PIXEL - 1) / PIXEL;
  int pw= (penw + (PIXEL >> 1)) / PIXEL;
  if (xj >= ((19*hh)/20)) xj= hh-1;
  else if (hh/20 >= xj) xj= 0;
  if (ww == 0 || 0 > pj - dj || pj + dj >= hh || 0 >= dj) return gl;
  int i1, j1, i2, j2, i3, j3;
  search (gl, pj - dj, pw, serial, i1, j1);
  search (gl, pj, pw, serial, i2, j2);
  search (gl, pj + dj, pw, serial, i3, j3);
  if (i1 >= ww || i2 >= ww || i3 >= ww) return gl;
  bool clear_right= ((serial & 1) == 0);
  int di= ((i1 + i3) >> 1) - i2;
  di= clear_right? max (di, 0): min (di, 0);
  i1 += di; i2 += di; i3 += di;
  int J1= max (min (j1, xj), 0);
  int J2= min (max (j3, xj), hh-1);
  bool pos= (xj > j2);
  glyph bmr= copy (gl);
  int ii, must= 0;
  for (int j= (pos? J1: J2); (pos? j <= J2: j >= J1); j += (pos? 1: -1)) {
    // FIXME: a better way to remove serifs would be to first draw
    // a tiny blank line to separate the character from the serif and
    // then flood fill the separated serif to the background
    double slope= ((double) (i3 - i1)) / ((double) (j3 - j1));
    int i= i1 + floor (slope * ((double) (j - j1)) + 0.5);
    i += clear_right? 1: -1;
    i= max (0, min (i, ww-1));
    if (serial == 1)
      for (ii=0; ii<=i; ii++)
        bmr->set_x (ii, j, 0);
    else if (clear_right)
      for (ii=i; ii<ww; ii++) {
        if (is_empty (gl, ii, ii+pw-1, j) && ii-i >= must) break;
        if (gl->get_x (ii, j) != 0) must= max (ii-i, must);
        bmr->set_x (ii, j, 0);
      }
    else
      for (ii=i; ii>=0; ii--) {
        if (is_empty (gl, ii-pw+1, ii, j) && i-ii >= must) break;
        if (gl->get_x (ii, j) != 0) must= max (i-ii, must);
        bmr->set_x (ii, j, 0);
      }
  }
  return bmr;
}

/******************************************************************************
* First more user friendly backend
******************************************************************************/

#define UNSERIF_TOP      0
#define UNSERIF_BOTTOM   1

glyph
unserif (glyph gl, int kind, SI penw, int serial) {
  int hh= gl->height;
  SI yext= gl->yoff * PIXEL;
  if (kind == UNSERIF_BOTTOM)
    yext= (gl->yoff - hh + 1) * PIXEL;
  SI mid = (gl->yoff - (hh >> 1)) * PIXEL;
  SI yrad= max (hh/20, 1) * PIXEL;
  SI ypos= yext;
  if (ypos < mid) ypos += (hh/5) * PIXEL;
  else ypos -= (hh/5) * PIXEL;
  return unserif (gl, ypos, yrad, yext, penw, serial);
}

#define UNSERIF_HALF_TOP_LEFT       2
#define UNSERIF_HALF_TOP_RIGHT      3
#define UNSERIF_HALF_BOTTOM_LEFT    4
#define UNSERIF_HALF_BOTTOM_RIGHT   5
#define UNSERIF_TOP_LEFT            6
#define UNSERIF_TOP_RIGHT           7
#define UNSERIF_TOP_CENTER          8
#define UNSERIF_TOP_SINGLE          9
#define UNSERIF_BOTTOM_LEFT        10
#define UNSERIF_BOTTOM_RIGHT       11
#define UNSERIF_BOTTOM_CENTER      12
#define UNSERIF_BOTTOM_SINGLE      13

glyph
unserif_one (glyph gl, int kind, SI penw) {
  switch (kind) {
  case UNSERIF_TOP:
    return unserif (gl, UNSERIF_TOP, penw, 1);
  case UNSERIF_BOTTOM:
    return unserif (gl, UNSERIF_BOTTOM, penw, 1);
  case UNSERIF_HALF_TOP_LEFT:
    return unserif (gl, UNSERIF_TOP, penw, 1);
  case UNSERIF_HALF_TOP_RIGHT:
    return unserif (gl, UNSERIF_TOP, penw, -1);
  case UNSERIF_HALF_BOTTOM_LEFT:
    return unserif (gl, UNSERIF_BOTTOM, penw, 1);
  case UNSERIF_HALF_BOTTOM_RIGHT:
    return unserif (gl, UNSERIF_BOTTOM, penw, -1);
  case UNSERIF_TOP_LEFT:
    return unserif (unserif (gl, UNSERIF_TOP, penw, 1),
                    UNSERIF_TOP, penw, 2);
  case UNSERIF_TOP_RIGHT:
    return unserif (unserif (gl, UNSERIF_TOP, penw, -1),
                    UNSERIF_TOP, penw, -2);
  case UNSERIF_TOP_CENTER:
    return unserif (unserif (gl, UNSERIF_TOP, penw, 3),
                    UNSERIF_TOP, penw, -3);
  case UNSERIF_TOP_SINGLE:
    return unserif (unserif (gl, UNSERIF_TOP, penw, 1),
                    UNSERIF_TOP, penw, -1);
  case UNSERIF_BOTTOM_LEFT:
    return unserif (unserif (gl, UNSERIF_BOTTOM, penw, 1),
                    UNSERIF_BOTTOM, penw, 2);
  case UNSERIF_BOTTOM_RIGHT:
    return unserif (unserif (gl, UNSERIF_BOTTOM, penw, -1),
                    UNSERIF_BOTTOM, penw, -2);
  case UNSERIF_BOTTOM_CENTER:
    return unserif (unserif (gl, UNSERIF_BOTTOM, penw, 3),
                    UNSERIF_BOTTOM, penw, -3);
  case UNSERIF_BOTTOM_SINGLE:
    return unserif (unserif (gl, UNSERIF_BOTTOM, penw, 1),
                    UNSERIF_BOTTOM, penw, -1);
  }
  return gl;
}

/******************************************************************************
* High level interface
******************************************************************************/

void
normalize_borders (glyph& gl, metric& ex) {
  int ww= gl->width, hh= gl->height;
  int i1= first_in_rows (gl, 0, hh - 1);
  int i2= last_in_rows  (gl, 0, hh - 1);
  int dw= i1 + ww - 1 - i2;
  gl->xoff += i1;
  gl->lwidth -= dw;
  gl= simplify (gl);
  ex->x2 -= dw * PIXEL;
  ex->x4 -= dw * PIXEL;
}

glyph
unserif (glyph gl, int code, SI penw) {
  //cout << "Unserif " << code << "\n";
  if (code < 32) return unserif_one (gl, code, penw);
  switch (code) {
  case 0x41:
    return unserif (unserif (gl, UNSERIF_BOTTOM_LEFT, penw),
                    UNSERIF_BOTTOM_RIGHT, penw);
  case 0x42:
  case 0x44:
    return unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_HALF_BOTTOM_LEFT, penw);
  case 0x45:
    return unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_HALF_BOTTOM_LEFT, penw);
  case 0x46:
    return unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_BOTTOM_LEFT, penw);
  case 0x48:
    return unserif (unserif (gl, 0x41, penw), 0x55, penw);
  case 0x49:
    return unserif (unserif (gl, UNSERIF_TOP_SINGLE, penw),
                    UNSERIF_BOTTOM_SINGLE, penw);
  case 0x4a:
    return unserif (gl, UNSERIF_TOP_SINGLE, penw);
  case 0x4b:
    return unserif (gl, 0x48, penw);
  case 0x4c:
    return unserif (unserif (gl, UNSERIF_TOP_SINGLE, penw),
                    UNSERIF_HALF_BOTTOM_LEFT, penw);
  case 0x4d:
    return unserif (unserif (unserif (gl, 0x41, penw),
                             UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_HALF_TOP_RIGHT, penw);
  case 0x4e:
    return unserif (unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                             UNSERIF_TOP_RIGHT, penw),
                    UNSERIF_BOTTOM_LEFT, penw);
  case 0x50:
    return unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_BOTTOM_LEFT, penw);
  case 0x52:
    return unserif (unserif (gl, UNSERIF_HALF_TOP_LEFT, penw),
                    UNSERIF_BOTTOM_LEFT, penw);
  case 0x54:
    return unserif (gl, UNSERIF_BOTTOM_LEFT, penw);
  case 0x55:
  case 0x56:
    return unserif (unserif (gl, UNSERIF_TOP_LEFT, penw),
                    UNSERIF_TOP_RIGHT, penw);
  case 0x57:
    return unserif (unserif (gl, 0x56, penw), UNSERIF_TOP_CENTER, penw);
  case 0x58:
    return unserif (gl, 0x48, penw);
  case 0x59:
    return unserif (unserif (gl, 0x56, penw), UNSERIF_BOTTOM_LEFT, penw);
  case 0x39b:
    return unserif (gl, 0x41, penw);
  }
  return gl;
}
