
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

/******************************************************************************
* Information about glyphs
******************************************************************************/

void
get_bounding_box (glyph gl, SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= (-gl->xoff) * PIXEL;
  x2= (gl->width - gl->xoff) * PIXEL;
  y1= (gl->yoff - gl->height) * PIXEL;
  y2= (gl->yoff) * PIXEL;
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

int
first_in_row (glyph gl, int j) {
  int ww= gl->width;
  for (int i=0; i<ww; i++)
    if (gl->get_x (i, j) != 0)
      return i;
  return ww;
}

int
first_in_rows (glyph gl, int j1, int j2) {
  int i= gl->width;
  for (int j=j1; j<=j2; j++)
    i= min (i, first_in_row (gl, j));
  return i;
}

int
last_in_row (glyph gl, int j) {
  int ww= gl->width;
  for (int i=ww-1; i>=0; i--)
    if (gl->get_x (i, j) != 0)
      return i;
  return -1;
}

int
last_in_rows (glyph gl, int j1, int j2) {
  int i= -1;
  for (int j=j1; j<=j2; j++)
    i= max (i, last_in_row (gl, j));
  return i;
}

int
first_in_column (glyph gl, int i) {
  int hh= gl->height;
  for (int j=0; j<hh; j++)
    if (gl->get_x (i, j) != 0)
      return j;
  return hh;
}

int
last_in_column (glyph gl, int i) {
  int hh= gl->height;
  for (int j=hh-1; j>=0; j--)
    if (gl->get_x (i, j) != 0)
      return j;
  return -1;
}

SI
collision_offset (glyph gl1, glyph gl2, bool overlap) {
  int w1= gl1->width, h1= gl1->height, h2= gl2->height;
  int best= -1000000;
  for (int j2=0; j2<h2; j2++) {
    int y = gl2->yoff - j2;
    int j1= gl1->yoff - y;
    if (0 <= j1 && j1 < h1 &&
        !empty_row (gl1, j1) &&
        !empty_row (gl2, j2)) {
      int end1  = last_in_row (gl1, j1);
      int start2= first_in_row (gl2, j2);
      int end2  = last_in_row (gl2, j2);
      int di    = end1 - (overlap? end2: start2);
      int dx    = di - gl1->xoff + gl2->xoff;
      best= max (best, dx);
    }
  }
  if (best > -1000000) return best * PIXEL;
  return (gl1->xoff + w1 - gl2->xoff) * PIXEL;
}

int
probe (glyph gl, int x, int y, int dx, int dy) {
  // Find distance to first pixel from a given point into a given direction
  int w= gl->width, h= gl->height;
  int i0= gl->xoff + x;
  int j0= gl->yoff - y;
  int i= i0, j= j0, di= dx, dj= -dy;
  if (i < 0 && di > 0) i= 0;
  if (i >= w && di < 0) i= w-1;
  if (j < 0 && dj > 0) j= 0;
  if (j >= h && dj < 0) j= h-1;
  while (true) {
    if (0 > i || i >= w || 0 > j || j >= h) return 0;
    if (gl->get_x (i, j) != 0) break;
    i += di; j += dj;
  }
  if (di != 0) return i - i0;
  if (dj != 0) return j0 - j;
  return 0;
}

/******************************************************************************
* Combining glyphs
******************************************************************************/

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

  int lo= min (-gl1->xoff, -gl2->xoff);
  int hi= max (gl1->lwidth - gl1->xoff, gl2->lwidth - gl2->xoff);
  bmr->lwidth= hi - lo;
  return bmr;
}

glyph
intersect (glyph gl1, glyph gl2) {
  int i, j;
  int ww= gl1->width, hh= gl1->height, ww2= gl2->width, hh2= gl2->height;
  glyph bmr (ww, hh, gl1->xoff, gl1->yoff, gl1->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      int c = gl1->get_x (i, j);
      int i2= i - gl1->xoff + gl2->xoff;
      int j2= j - gl1->yoff + gl2->yoff;
      if (i2 >= 0 && ww2 > i2 && j2 >= 0 && hh2 > j2)
        c= min (c, gl2->get_x (i2, j2));
      else c= 0;
      bmr->set_x (i, j, c);
    }
  bmr->lwidth= gl1->lwidth;
  return simplify (bmr);
}

glyph
exclude (glyph gl1, glyph gl2) {
  int i, j;
  int ww= gl1->width, hh= gl1->height, ww2= gl2->width, hh2= gl2->height;
  glyph bmr (ww, hh, gl1->xoff, gl1->yoff, gl1->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      int c = gl1->get_x (i, j);
      int i2= i - gl1->xoff + gl2->xoff;
      int j2= j - gl1->yoff + gl2->yoff;
      if (i2 >= 0 && ww2 > i2 && j2 >= 0 && hh2 > j2)
        if (gl2->get_x (i2, j2) != 0) c= 0;
      bmr->set_x (i, j, c);
    }
  bmr->lwidth= gl1->lwidth;
  return simplify (bmr);
}

glyph
bar_right (glyph gl1, glyph gl2) {
  int ww1= gl1->width, hh1= gl1->height;
  if (ww1 == 0 || hh1 == 0) return gl1;
  int i, j;
  int r1= 0, rj1= 0, r2= 0, rj2= hh1-1;
  for (j=0; j<(hh1>>1); j++)
    if (last_in_row (gl1, j) > r1) {
      r1= last_in_row (gl1, j); rj1= j; }
  for (j=hh1-1; j>=(hh1>>1); j--)
    if (last_in_row (gl1, j) > r2) {
      r2= last_in_row (gl1, j); rj2= j; }
  glyph filled= copy (gl1);
  for (j= rj1; j <= rj2; j++)
    for (i= max (last_in_row (gl1, j), 0); i<ww1; i++)
      filled->set_x (i, j, 1);
  return join (gl1, intersect (gl2, filled));
}

glyph
bar_bottom (glyph gl1, glyph gl2) {
  int ww1= gl1->width, hh1= gl1->height;
  if (ww1 == 0 || hh1 == 0) return gl1;
  int i, j;
  int b1= 0, bi1= 0, b2= 0, bi2= ww1-1;
  for (i=0; i<(ww1>>1); i++)
    if (last_in_column (gl1, i) > b1) {
      b1= last_in_column (gl1, i); bi1= i; }
  for (i=ww1-1; i>=(ww1>>1); i--)
    if (last_in_column (gl1, i) > b2) {
      b2= last_in_column (gl1, i); bi2= i; }
  glyph filled= copy (gl1);
  for (i= bi1; i <= bi2; i++)
    for (j= max (last_in_column (gl1, i), 0); j<hh1; j++)
      filled->set_x (i, j, 1);
  return join (gl1, intersect (gl2, filled));
}

/******************************************************************************
* Operating on glyphs
******************************************************************************/

glyph
copy (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j));
  return bmr;
}

glyph
junc_left (glyph gl, int jw) {
  int i, j, k;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  jw= min (jw, ww);
  for (j=0; j<hh; j++) {
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j));
    for (i=0; i<jw; i++)
      if (gl->get_x (i, j) != 0) {
        for (k=0; k<i; k++)
          bmr->set_x (k, j, gl->get_x (i, j));
        break;
      }
  }
  return bmr;
}

glyph
junc_right (glyph gl, int jw) {
  int i, j, k;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  jw= min (jw, ww);
  for (j=0; j<hh; j++) {
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j));
    for (i=ww-1; i>=ww-jw; i--)
      if (gl->get_x (i, j) != 0) {
        for (k=i+1; k<ww; k++)
          bmr->set_x (k, j, gl->get_x (i, j));
        break;
      }
  }
  return bmr;
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
  bmr->lwidth= gl->lwidth;
  return bmr;
}

glyph
padded (glyph gl, int l, int t, int r, int b) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww+l+r, hh+t+b, gl->xoff+l, gl->yoff+t, gl->depth);
  for (j=0; j<hh+t+b; j++)
    for (i=0; i<ww+l+r; i++)
      bmr->set_x (i, j, 0);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i + l, j + t, gl->get_x (i, j));
  bmr->lwidth= gl->lwidth;
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
  bmr->lwidth= gl->lwidth;
  return simplify (bmr);
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
transpose (glyph gl) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (hh, ww, gl->yoff, gl->xoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (j, i, gl->get_x (i, j));
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
  return simplify (bmr);
}

glyph
ver_extend (glyph gl, int pos, int by) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh+by, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<(hh+by); j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, gl->get_x (i, j<pos? j: (j<pos+by? pos: j-by)));
  bmr->lwidth= gl->lwidth;
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
  bmr->lwidth= gl->lwidth;
  return simplify (bmr);
}

glyph
bottom_edge (glyph gl, SI penh, SI keepy) {
  int ph= (penh + PIXEL - 1) / PIXEL;
  int kj= gl->yoff - (keepy + (PIXEL >> 1)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (i=0; i<ww; i++) {
    int j2= last_in_column (gl, i);
    int j1= min (j2 - ph, kj);
    bool ok= j2 > 0 && j2 > (kj - (ph >> 1));
    for (j=0; j<hh; j++)
      if (ok && j > j1 && j2 >= j)
        bmr->set_x (i, j, gl->get_x (i, j));
      else bmr->set_x (i, j, 0);
  }
  bmr->lwidth= gl->lwidth;
  return simplify (bmr);
}

glyph
flood_fill (glyph gl, SI px, SI py) {
  gl= simplify (gl);
  glyph bmr= copy (gl);
  int ww= gl->width, hh= gl->height;
  int pi= gl->xoff + (px + (PIXEL >> 1)) / PIXEL;
  int pj= gl->yoff - (py + (PIXEL >> 1)) / PIXEL;
  if (pi < 0) pi= 0;
  if (pi >= ww) pi= ww-1;
  if (pj < 0) pi= 0;
  if (pj >= hh) pj= hh-1;
  
  array<int> todo;
  todo << pi << pj;
  array<int> next;
  while (N(todo) != 0) {
    for (int k=0; k+1<N(todo); k+=2) {
      int i= todo[k], j= todo[k+1];
      if ((i < ww) && (i >= 0) && (j < hh) && (j >= 0))
        if (bmr->get_x (i, j) == 0) {
          bmr->set_x (i, j, 1);
          next << i+1 << j << i-1 << j << i << j+1 << i << j-1;
        }
    }
    todo= next;
    next= array<int> ();
  }
  bmr->lwidth= gl->lwidth;
  return bmr;
}

/******************************************************************************
* Miscellaneous glyphs
******************************************************************************/

glyph
circle_glyph (SI rad, SI penw) {
  int rr= (rad + (penw >> 1)) / PIXEL + 1;
  int ww= 2*rr-1, hh= 2*rr-1;
  glyph bmr (ww, hh, rr, rr, 1);
  for (int i=0; i<ww; i++)
    for (int j=0; j<hh; j++) {
      double x= (double) ((i - rr) * PIXEL);
      double y= (double) ((j - rr) * PIXEL);
      SI d= (SI) ceil (sqrt (x*x + y*y));
      if ((rad + (penw >> 1)) > d && (d >= rad - (penw >> 1)))
        bmr->set_x (i, j, 1);
      else bmr->set_x (i, j, 0);
    }
  return simplify (bmr);
}
