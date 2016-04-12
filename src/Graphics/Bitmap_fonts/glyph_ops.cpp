
/******************************************************************************
* MODULE     : glyph_ops.cpp
* DESCRIPTION: operation on glyphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "renderer.hpp"

void
get_bounding_box (glyph gl, SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= (-gl->xoff) * PIXEL;
  x2= (gl->width-1 - gl->xoff) * PIXEL;
  y1= (gl->yoff - (gl->height-1)) * PIXEL;
  y2= (gl->yoff) * PIXEL;
}

glyph
join (glyph gl1, glyph gl2) {
  int x1= min (-gl1->xoff, -gl2->xoff);
  int y1= min (gl1->yoff- gl1->height, gl2->yoff- gl2->height);
  int x2= max (gl1->width- gl1->xoff, gl2->width- gl2->xoff);
  int y2= max (gl1->yoff, gl2->yoff);
  glyph bmr (x2-x1, y2-y1, -x1, y2, max (gl1->depth, gl2->depth));

  int i, j, dx, dy;
  dx= -gl1->xoff- x1, dy= y2- gl1->yoff;
  for (j=0; j<gl1->height; j++)
    for (i=0; i<gl1->width; i++)
      bmr->set_x (i+dx, j+dy, gl1->get_x (i, j));

  dx= -gl2->xoff- x1; dy= y2- gl2->yoff;
  for (j=0; j<gl2->height; j++)
    for (i=0; i<gl2->width; i++)
      bmr->set_x (i+dx, j+dy,
		  max (bmr->get_x (i+dx, j+dy), gl2->get_x (i, j)));

  return bmr;
}

glyph
move (glyph gl, SI x, SI y) {
  x += PIXEL/2; y += PIXEL/2; abs_round (x, y);
  int xx= x/PIXEL, yy= y/PIXEL;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff- xx, gl->yoff+ yy, gl->depth);

  int i, j;
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j));
  bmr->lwidth= gl->lwidth;
  return bmr;
}

glyph
clip (glyph gl, SI x1, SI y1, SI x2, SI y2) {
  abs_round (x1, y1);
  abs_round (x2, y2);
  x1= x1/PIXEL; y1= y1/PIXEL;
  x2= x2/PIXEL; y2= y2/PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      bool x_ok= (i-gl->xoff >= x1) && (i-gl->xoff < x2);
      bool y_ok= (gl->yoff-j >= y1) && (gl->yoff-j < y2);
      bmr->set_x (i, j, x_ok && y_ok? gl->get_x (i, j): 0);
    }
  return bmr;
}

bool
empty_column (glyph gl, int i) {
  int hh= gl->height;
  for (int j=0; j<hh; j++)
    if (gl->get_x (i, j) != 0)
      return false;
  return true;
}

bool
empty_row (glyph gl, int j) {
  int ww= gl->width;
  for (int i=0; i<ww; i++)
    if (gl->get_x (i, j) != 0)
      return false;
  return true;
}

glyph
simplify (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  int i1= 0, i2= ww-1, j1= 0, j2= hh-1;
  while (i1 < ww && empty_column (gl, i1)) i1++;
  while (i2 >= 0 && empty_column (gl, i2)) i2--;
  while (j1 < hh && empty_row (gl, j1)) j1++;
  while (j2 >= 0 && empty_row (gl, j2)) j2--;
  if (i1 == ww) { i1= j1= 0; i2= j2= -1; }
  glyph bmr (i2-i1+1, j2-j1+1, gl->xoff-i1, gl->yoff-j1, gl->depth);
  for (j=j1; j<=j2; j++)
    for (i=i1; i<=i2; i++)
      bmr->set_x (i-i1, j-j1, gl->get_x (i, j));
  return bmr;
}

glyph
hor_flip (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (ww-1-i, j, gl->get_x (i, j));
  bmr->lwidth= gl->lwidth;
  return bmr;
}

glyph
ver_flip (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, hh-1-j, gl->get_x (i, j));
  bmr->lwidth= gl->lwidth;
  return bmr;
}

glyph
pos_rotate (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (hh, ww, gl->yoff, gl->width- gl->xoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (j, ww-1-i, gl->get_x (i, j));
  return bmr;
}

glyph
hor_extend (glyph gl, int pos, int by) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  bool ok= false;
  while (!ok) { // hack for corrupted pfb translations
    for (j=0; j<hh; j++)
      ok= ok || (gl->get_x (pos, j) != 0);
    if (!ok) pos += (pos < (ww>>1)? 1: -1);
    if (pos >= (ww>>2) && pos < (3*ww>>2)) break;
  }
  glyph bmr (ww+ by, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<(ww+by); i++)
      bmr->set_x (i, j, gl->get_x (i<pos? i: (i<pos+by? pos: i-by), j));
  return bmr;
}

glyph
hor_take (glyph gl, int pos, int nr) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  bool ok= false;
  while (!ok) { // hack for corrupted pfb translations
    for (j=0; j<hh; j++)
      ok= ok || (gl->get_x (pos, j) != 0);
    if (!ok) pos += (pos < (ww>>1)? 1: -1);
    if (pos >= (ww>>2) && pos < (3*ww>>2)) break;
  }
  glyph bmr (nr, hh, 0, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<nr; i++)
      bmr->set_x (i, j, gl->get_x (pos, j));
  return bmr;
}

glyph
ver_extend (glyph gl, int pos, int by) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh+by, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<(hh+by); j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j<pos? j: (j<pos+by? pos: j-by)));
  return bmr;
}

glyph
ver_take (glyph gl, int pos, int nr) {
  int i, j;
  int ww= gl->width;
  glyph bmr (ww, nr, gl->xoff, 0, gl->depth);
  for (j=0; j<nr; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, pos));
  return bmr;
}
