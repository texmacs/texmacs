
/******************************************************************************
* MODULE     : bitmap_char.cpp
* DESCRIPTION: bitmaps of characters
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bitmap_font.hpp"

#define PIXEL 256

/******************************************************************************
* Constructors and destructors for glief
******************************************************************************/

glief_rep::glief_rep (int w2, int h2, int xoff2, int yoff2,
				  int depth2, int status2)
{
  depth   = depth2;
  width   = w2;
  height  = h2;
  xoff    = xoff2;
  yoff    = yoff2;
  lwidth  = w2;
  status  = status2;

  int i, n= (depth==1? (width*height+7)/8: width*height);
  raster= new QN [n];
  for (i=0; i<n; i++) raster[i]=0;
}

glief_rep::~glief_rep () {
  delete[] raster;
}

glief::glief (int w2, int h2, int xoff2, int yoff2, int depth2, int status2) {
  rep= new glief_rep (w2, h2, xoff2, yoff2, depth2, status2);
}

/******************************************************************************
* Getting and setting pixels
******************************************************************************/

int
glief_rep::get_x (int i, int j) {
  if ((i<0) || (i>=width))  return 0;
  if ((j<0) || (j>=height)) return 0;
  if (depth==1) {
    int bit= j*width+i;
    return (raster[bit>>3] >> (bit&7)) & 1;
  }
  else return raster[j*width+i];
}

void
glief_rep::set_x (int i, int j, int with) {
  if ((i<0) || (i>=width))
    fatal_error ("bad x-index", "glief_rep::set_x");
  if ((j<0) || (j>=height))
    fatal_error ("bad y-index", "glief_rep::set_x");
  if (depth==1) {
    int bit= j*width+i;
    if (with==0) raster[bit>>3] &= ~(1 << (bit&7));
    else raster[bit>>3] |= (1 << (bit&7));
  }
  else raster [j*width+ i]= with;
}

int
glief_rep::get (int i, int j) {
  return get_x (i+xoff, yoff-j);
}

void
glief_rep::set (int i, int j, int with) {
  set_x (i+xoff, yoff-j, with);
}

/******************************************************************************
* Adjusting top and bottom lines for extensible characters
******************************************************************************/

void
glief_rep::adjust_bot () {
  int i;
  if (height<=2) return;
  for (i=0; i<width; i++) set_x (i, height-1, get_x (i, height-2));
}

void
glief_rep::adjust_top () {
  int i;
  if (height<=2) return;
  for (i=0; i<width; i++) set_x (i, 0, get_x (i, 1));
}

/******************************************************************************
* Text output
******************************************************************************/

ostream&
operator << (ostream& out, glief bmc) {
  int i, j;
  out << "Size  : (" << bmc->width << ", " << bmc->height << ")\n";
  out << "Offset: (" << bmc->xoff << ", " << bmc->yoff << ")\n";
  for (i=0; i<bmc->width+2; i++) out << "-";
  out << "\n";
  for (j=0; j<bmc->height; j++) {
    out << "|";
    for (i=0; i<bmc->width; i++) {
      int k= bmc->get_x (i, j);
      if (k==0) out << " ";
      else if (bmc->depth==1) out << "*";
      else if (k <= 9) out << k;
      else out << ((char) (55+k));
    }
    out << "|\n";
  }
  for (i=0; i<bmc->width+2; i++) out << "-";
  out << "\n";
  return out;
}
