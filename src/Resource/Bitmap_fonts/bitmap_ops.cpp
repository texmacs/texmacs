
/******************************************************************************
* MODULE     : bitmap_ops.cpp
* DESCRIPTION: operation on character bitmaps
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bitmap_font.hpp"
#include "ps_device.hpp"

glief
join (glief bmc1, glief bmc2) {
  int x1= min (-bmc1->xoff, -bmc2->xoff);
  int y1= min (bmc1->yoff- bmc1->height, bmc2->yoff- bmc2->height);
  int x2= max (bmc1->width- bmc1->xoff, bmc2->width- bmc2->xoff);
  int y2= max (bmc1->yoff, bmc2->yoff);
  glief bmr (x2-x1, y2-y1, -x1, y2, max (bmc1->depth, bmc2->depth));

  int i, j, dx, dy;
  dx= -bmc1->xoff- x1, dy= y2- bmc1->yoff;
  for (j=0; j<bmc1->height; j++)
    for (i=0; i<bmc1->width; i++)
      bmr->set_x (i+dx, j+dy, bmc1->get_x (i, j));

  dx= -bmc2->xoff- x1; dy= y2- bmc2->yoff;
  for (j=0; j<bmc2->height; j++)
    for (i=0; i<bmc2->width; i++)
      bmr->set_x (i+dx, j+dy,
		  max (bmr->get_x (i+dx, j+dy), bmc2->get_x (i, j)));

  return bmr;
}

glief
move (glief bmc, SI x, SI y) {
  x += PIXEL/2; y += PIXEL/2; abs_round (x, y);
  int xx= x/PIXEL, yy= y/PIXEL;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (ww, hh, bmc->xoff- xx, bmc->yoff+ yy, bmc->depth);

  int i, j;
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, bmc->get_x (i, j));
  bmr->lwidth= bmc->lwidth;
  return bmr;
}

glief
hor_flip (glief bmc) {
  int i, j;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (ww, hh, bmc->xoff, bmc->yoff, bmc->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (ww-1-i, j, bmc->get_x (i, j));
  bmr->lwidth= bmc->lwidth;
  return bmr;
}

glief
ver_flip (glief bmc) {
  int i, j;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (ww, hh, bmc->xoff, bmc->yoff, bmc->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, hh-1-j, bmc->get_x (i, j));
  bmr->lwidth= bmc->lwidth;
  return bmr;
}

glief
pos_rotate (glief bmc) {
  int i, j;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (hh, ww, bmc->yoff, bmc->width- bmc->xoff, bmc->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      bmr->set_x (j, ww-1-i, bmc->get_x (i, j));
  return bmr;
}

glief
hor_extend (glief bmc, int pos, int by) {
  int i, j;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (ww+ by, hh, bmc->xoff, bmc->yoff, bmc->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<(ww+by); i++)
      bmr->set_x (i, j, bmc->get_x (i<pos? i: (i<pos+by? pos: i-by), j));
  return bmr;
}

glief
ver_extend (glief bmc, int pos, int by) {
  int i, j;
  int ww= bmc->width, hh= bmc->height;
  glief bmr (ww, hh+by, bmc->xoff, bmc->yoff, bmc->depth);
  for (j=0; j<(hh+by); j++)
    for (i=0; i<ww; i++)
      bmr->set_x (i, j, bmc->get_x (i, j<pos? j: (j<pos+by? pos: j-by)));
  return bmr;
}
