
/******************************************************************************
* MODULE     : glyph_shrink.cpp
* DESCRIPTION: shrinking glyphs for anti-aliasing
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "renderer.hpp"

static int
log2i (int i) {
  int l;
  if (i==1) return 0;
  for (l=1; l<30; l++)
    if (i <= (1 << l)) return l;
  FAILED ("too large shrinking factor");
  return 0;
}

static int
my_div (int a, int b) {
  if (a>=0) return a/b;
  else return -((b-1-a)/b);
}

static int
my_mod (int a, int b) {
  if (a>=0) return a%b;
  else return b-1-((-1-a)%b);
}

static int
my_norm (int a, int m) {
  a= my_mod (a, m);
  if (a<=(m>>1)) return a;
  else return m-a;
}

int
get_hor_shift (glyph gl, int xfactor, int tx) {
  STACK_NEW_ARRAY (flag, bool, gl->width);

  // cout << "[";
  int x;
  for (x=0; x<gl->width; x++) {
    int max_count= 0, count=0, y;
    for (y=0; y<gl->height; y++)
      if (gl->get_1 (x,y)) count++;
      else {
	max_count = max (max_count, count);
	count     = 0;
      }
    max_count= max (max_count, count);
    flag[x]= (max_count>(gl->height>>1));
    // if (flag[x]) cout << "*"; else cout << " ";
  }
  // cout << "]   ";

  int first0=-1, first1=-1, last0=-1, last1=-1;
  for (x=0; x<gl->width; x++)
    if (flag[x]) {
      if (first0<0) first0= x;
      last0= x;
      while ((x<gl->width) && flag[x]) x++;
      if (first1<0) first1= x+ tx;
      last1= x+ tx;
      x--;
    }
  /* cout << first0 << ", " << first1 << ", "
     << last0  << ", " << last1  << "\n"; */

  STACK_DELETE_ARRAY (flag);

  if (first0==-1) return 0;
  if (first0==last0) return my_mod (gl->xoff- first0, xfactor);

  int first, last;
  int d00= my_norm (last0- first0, xfactor);
  int d01= my_norm (last1- first0, xfactor);
  int d10= my_norm (last0- first1, xfactor);
  int d11= my_norm (last1- first1, xfactor);
  if ((d00<=d01) && (d00<=d10) && (d00<=d11)) { first= first0; last= last0; }
  else if ((d01<=d10) && (d01<=d11)) { first= first0; last= last1; }
  else if (d10<=d11) { first= first1; last= last0; }
  else { first= first1; last= last1; }

  int middle, rest= my_mod (last- first, xfactor);
  if (rest <= (xfactor>>1)) middle= first+ (rest>>1);
  else middle= first- ((xfactor-rest)>>1);
  return my_mod (gl->xoff- middle, xfactor);
}

int
get_ver_shift (glyph gl, int yfactor, int ty) {
  STACK_NEW_ARRAY (flag, bool, gl->height);

  // cout << "[";
  int y;
  for (y=0; y<gl->height; y++) {
    int max_count= 0, count=0, x;
    for (x=0; x<gl->width; x++)
      if (gl->get_1 (x,y)) count++;
      else {
	max_count = max (max_count, count);
	count     = 0;
      }
    max_count= max (max_count, count);
    flag[y]= (max_count>(gl->width>>1));
    // if (flag[y]) cout << "*"; else cout << " ";
  }
  // cout << "]   ";

  int first0=-1, first1=-1, last0=-1, last1=-1;
  for (y=0; y<gl->height; y++)
    if (flag[y]) {
      if (first0<0) first0= y;
      last0= y;
      while ((y<gl->height) && flag[y]) y++;
      if (first1<0) first1= y+ ty;
      last1= y+ ty;
      y--;
    }
  /* cout << first0 << ", " << first1 << ", "
     << last0  << ", " << last1  << "\n"; */

  STACK_DELETE_ARRAY (flag);

  if (first0==-1) return 0;
  if (first0==last0)
    return my_mod (gl->height- gl->yoff- 1- first0, yfactor);

  int first, last;
  int d00= my_norm (last0- first0, yfactor);
  int d01= my_norm (last1- first0, yfactor);
  int d10= my_norm (last0- first1, yfactor);
  int d11= my_norm (last1- first1, yfactor);
  if ((d00<=d01) && (d00<=d10) && (d00<=d11)) { first= first0; last= last0; }
  else if ((d01<=d10) && (d01<=d11)) { first= first0; last= last1; }
  else if (d10<=d11) { first= first1; last= last0; }
  else { first= first1; last= last1; }

  int middle, rest= my_mod (last- first, yfactor);
  if (rest <= (yfactor>>1)) middle= first+ (rest>>1);
  else middle= first- ((yfactor-rest)>>1);
  return my_mod (gl->height- gl->yoff- 1- middle, yfactor);
}

glyph
shrink (glyph gl, int xfactor, int yfactor,
	int dx, int dy, int tx, int ty, SI& xo, SI& yo)
{
  /*
  cout << "------------------------------------------------------------------------------\n";
  cout << "Shift by " << dx << ", " << dy << "\n";
  cout << "------------------------------------------------------------------------------\n\n";
  cout << gl << "\n";
  */

  int x1= dx- gl->xoff;
  int x2= dx- gl->xoff+ gl->width+ tx;
  int X1= my_div (x1, xfactor);
  int X2= my_div (x2+xfactor-1, xfactor);

  int y1= dy+ gl->yoff+ 1- gl->height;
  int y2= dy+ gl->yoff+ 1+ ty;
  int Y1= my_div (y1, yfactor);
  int Y2= my_div (y2+yfactor-1, yfactor);

  int frac_x= (dx- gl->xoff- X1*xfactor);
  int frac_y= (dy+ gl->yoff- Y1*yfactor);
  SI  off_x = (((-X1) *xfactor+ dx)*PIXEL + ((tx*PIXEL)>>1))/xfactor;
  SI  off_y = (((Y2-1)*yfactor- dy)*PIXEL - ((ty*PIXEL)>>1))/yfactor;

  int i, j, x, y;
  int index, indey, entry;
  int ww=(X2-X1)*xfactor, hh=(Y2-Y1)*yfactor;
  int* bitmap= tm_new_array<int> (ww*hh);
  //STACK_NEW_ARRAY (bitmap, int, ww*hh);
  for (i=0; i<ww*hh; i++) bitmap[i]=0;
  for (y=0, index= ww*frac_y+ frac_x; y<gl->height; y++, index-=ww)
    for (x=0; x<gl->width; x++)
      if (gl->get_1(x,y))
        for (j=0, indey=ww*ty; j<=ty; j++, indey-=ww) {
	  entry = index+indey+x;
	  for (i=0; i<=tx; i++, entry++)
	    bitmap[entry]= 1;
	}

  int X, Y, sum, nr= xfactor*yfactor;
  int new_depth= gl->depth+ log2i (nr);
  if (new_depth > 8) new_depth= 8;
  glyph CB (X2-X1, Y2-Y1, -X1, Y2-1, new_depth, gl->status);
  CB->index = gl->index;
  for (Y=Y1; Y<Y2; Y++)
    for (X=X1; X<X2; X++) {
      sum=0;
      indey= ((Y-Y1)*ww+ (X-X1))*xfactor;
      for (j=0, index= indey; j<yfactor; j++, index+=ww)
	for (i=0; i<xfactor; i++)
	  sum += bitmap[index+ i];
      if (nr >= 64) sum= (64 * sum) / nr;
      CB->set (X, Y, sum);
    }
  xo= off_x;
  yo= off_y;
  tm_delete_array (bitmap);
  //STACK_DELETE_ARRAY (bitmap);

  // cout << CB << "\n";
  return CB;
}

glyph
shrink (glyph gl, int xfactor, int yfactor, SI& xo, SI& yo) {
  if ((gl->width==0) || (gl->height==0)) {
    int nr= xfactor*yfactor;
    int new_depth= gl->depth+ log2i (nr);
    return glyph (0, 0, 0, 0, new_depth);
  }

  int tx= ((xfactor/3) * (retina_factor+1)) / 2;
  int ty= ((yfactor/3) * (retina_factor+1)) / 2;
  int dx=0, dy=0;
  if ((gl->status==0) && (xfactor>1)) dx= get_hor_shift (gl, xfactor, tx);
  // if ((gl->status==0) && (yfactor>1)) dy= get_ver_shift (gl, yfactor, ty);
  if (gl->artistic != 0) tx= ty= 0;

  glyph ret= shrink (gl, xfactor, yfactor, dx, dy, tx, ty, xo, yo);
#ifndef QTTEXMACS
  if (ret->status != 0) {
    if (ret->status&1) ret->adjust_top ();
    if (ret->status&2) ret->adjust_bot ();
    ret->yoff= yo= 0;
  }
#endif
  return ret;
}
