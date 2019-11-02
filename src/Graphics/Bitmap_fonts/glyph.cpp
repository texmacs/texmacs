
/******************************************************************************
* MODULE     : glyph.cpp
* DESCRIPTION: glyphs of characters
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"

/******************************************************************************
* Constructors and destructors for glyph
******************************************************************************/

glyph_rep::glyph_rep (int w2, int h2, int xoff2, int yoff2,
		      int depth2, int status2)
{
  index    = 0;
  depth    = depth2;
  width    = w2;
  height   = h2;
  xoff     = xoff2;
  yoff     = yoff2;
  lwidth   = w2;
  status   = status2;
  artistic = 0;

  int i, n= (depth==1? (width*height+7)/8: width*height);
  raster= tm_new_array<QN> (n);
  for (i=0; i<n; i++) raster[i]=0;
}

glyph_rep::~glyph_rep () {
  tm_delete_array (raster);
}

glyph::glyph (int w2, int h2, int xoff2, int yoff2, int depth2, int status2) {
  rep= tm_new<glyph_rep> (w2, h2, xoff2, yoff2, depth2, status2);
}

/******************************************************************************
* Getting and setting pixels
******************************************************************************/

int
glyph_rep::get_x (int i, int j) {
  if (i<0 ||  (i-width)>=0) return 0;
  if (j<0 || (j-height)>=0) return 0;
  if (depth==1) {
    int bit= j*width+i;
    return (raster[bit>>3] >> (bit&7)) & 1;
  }
  else return raster[j*width+i];
}

void
glyph_rep::set_x (int i, int j, int with) {
  if ((i<0) || (i>=width)) FAILED ("bad x-index");
  if ((j<0) || (j>=height)) FAILED ("bad y-index");
  if (depth==1) {
    int bit= j*width+i;
    if (with==0) raster[bit>>3] &= ~(1 << (bit&7));
    else raster[bit>>3] |= (1 << (bit&7));
  }
  else raster [j*width+ i]= with;
}

int
glyph_rep::get (int i, int j) {
  return get_x (i+xoff, yoff-j);
}

void
glyph_rep::set (int i, int j, int with) {
  set_x (i+xoff, yoff-j, with);
}

/******************************************************************************
* Adjusting top and bottom lines for extensible characters
******************************************************************************/

void
glyph_rep::adjust_bot () {
  int i;
  if (height<=2) return;
  for (i=0; i<width; i++) set_x (i, height-1, get_x (i, height-2));
}

void
glyph_rep::adjust_top () {
  int i;
  if (height<=2) return;
  for (i=0; i<width; i++) set_x (i, 0, get_x (i, 1));
}

/******************************************************************************
* Text output
******************************************************************************/

tm_ostream&
operator << (tm_ostream& out, glyph gl) {
  int i, j;
  out << "Size   : (" << gl->width << ", " << gl->height << ")\n";
  out << "Offset : (" << gl->xoff << ", " << gl->yoff << ")\n";
  out << "Advance: " << gl->lwidth << "\n";
  for (i=0; i<gl->width+2; i++) out << "-";
  out << "\n";
  for (j=0; j<gl->height; j++) {
    out << "|";
    for (i=0; i<gl->width; i++) {
      int k= gl->get_x (i, j);
      if (k==0) out << " ";
      else if (gl->depth==1) out << "*";
      else if (k <= 9) out << k;
      else out << ((char) (55+k));
    }
    out << "|\n";
  }
  for (i=0; i<gl->width+2; i++) out << "-";
  out << "\n";
  return out;
}
