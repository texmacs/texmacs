
/******************************************************************************
* MODULE     : shrink.cpp
* DESCRIPTION: shrinking bitmaps for anti-aliasing
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bitmap_font.hpp"
#include "ps_device.hpp"

static int
log2i (int i) {
  if (i==1) return 0;
  if (i<=2) return 1;
  if (i<=4) return 2;
  if (i<=8) return 3;
  if (i<=16) return 4;
  if (i<=32) return 5;
  if (i<=64) return 6;
  if (i<=128) return 7;
  fatal_error ("too large shrinking factor", "shrink", "glief.cpp");
  return 0; // Because of bug in certain versions of g++
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
get_hor_shift (glief bmc, int xfactor, int tx) {
  STACK_NEW_ARRAY (flag, bool, bmc->width);

  // cout << "[";
  int x;
  for (x=0; x<bmc->width; x++) {
    int max_count= 0, count=0, y;
    for (y=0; y<bmc->height; y++)
      if (bmc->get_1 (x,y)) count++;
      else {
	max_count = max (max_count, count);
	count     = 0;
      }
    max_count= max (max_count, count);
    flag[x]= (max_count>(bmc->height>>1));
    // if (flag[x]) cout << "*"; else cout << " ";
  }
  // cout << "]   ";

  int first0=-1, first1=-1, last0=-1, last1=-1;
  for (x=0; x<bmc->width; x++)
    if (flag[x]) {
      if (first0<0) first0= x;
      last0= x;
      while ((x<bmc->width) && flag[x]) x++;
      if (first1<0) first1= x+ tx;
      last1= x+ tx;
      x--;
    }
  /* cout << first0 << ", " << first1 << ", "
     << last0  << ", " << last1  << "\n"; */

  STACK_DELETE_ARRAY (flag);

  if (first0==-1) return 0;
  if (first0==last0) return my_mod (bmc->xoff- first0, xfactor);

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
  return my_mod (bmc->xoff- middle, xfactor);
}

int
get_ver_shift (glief bmc, int yfactor, int ty) {
  STACK_NEW_ARRAY (flag, bool, bmc->height);

  // cout << "[";
  int y;
  for (y=0; y<bmc->height; y++) {
    int max_count= 0, count=0, x;
    for (x=0; x<bmc->width; x++)
      if (bmc->get_1 (x,y)) count++;
      else {
	max_count = max (max_count, count);
	count     = 0;
      }
    max_count= max (max_count, count);
    flag[y]= (max_count>(bmc->width>>1));
    // if (flag[y]) cout << "*"; else cout << " ";
  }
  // cout << "]   ";

  int first0=-1, first1=-1, last0=-1, last1=-1;
  for (y=0; y<bmc->height; y++)
    if (flag[y]) {
      if (first0<0) first0= y;
      last0= y;
      while ((y<bmc->height) && flag[y]) y++;
      if (first1<0) first1= y+ ty;
      last1= y+ ty;
      y--;
    }
  /* cout << first0 << ", " << first1 << ", "
     << last0  << ", " << last1  << "\n"; */

  STACK_DELETE_ARRAY (flag);

  if (first0==-1) return 0;
  if (first0==last0)
    return my_mod (bmc->height- bmc->yoff- 1- first0, yfactor);

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
  return my_mod (bmc->height- bmc->yoff- 1- middle, yfactor);
}

glief
shrink (glief bmc, int xfactor, int yfactor,
	int dx, int dy, int tx, int ty, SI& xo, SI& yo)
{
  /*
  cout << "------------------------------------------------------------------------------\n";
  cout << "Shift by " << dx << ", " << dy << "\n";
  cout << "------------------------------------------------------------------------------\n\n";
  cout << bmc << "\n";
  */

  int x1= dx- bmc->xoff;
  int x2= dx- bmc->xoff+ bmc->width+ tx;
  int X1= my_div (x1, xfactor);
  int X2= my_div (x2+xfactor-1, xfactor);

  int y1= dy+ bmc->yoff+ 1- bmc->height;
  int y2= dy+ bmc->yoff+ 1+ ty;
  int Y1= my_div (y1, yfactor);
  int Y2= my_div (y2+yfactor-1, yfactor);

  int frac_x= (dx- bmc->xoff- X1*xfactor);
  int frac_y= (dy+ bmc->yoff- Y1*yfactor);
  SI  off_x = (((-X1) *xfactor+ dx)*PIXEL + ((tx*PIXEL)>>1))/xfactor;
  SI  off_y = (((Y2-1)*yfactor- dy)*PIXEL - ((ty*PIXEL)>>1))/yfactor;

  int i, j, x, y;
  int index, indey;
  int ww=(X2-X1)*xfactor, hh=(Y2-Y1)*yfactor;
  STACK_NEW_ARRAY (bitmap, int, ww*hh);
  for (i=0; i<ww*hh; i++) bitmap[i]=0;
  for (y=0, index= ww*frac_y+ frac_x; y<bmc->height; y++, index-=ww)
    for (x=0; x<bmc->width; x++)
      for (j=0, indey=ww*ty; j<=ty; j++, indey-=ww)
	for (i=0; i<=tx; i++) {
	  int entry= index+indey+x+i;
	  int value= bmc->get_1(x,y);
	  if (value>bitmap[entry]) bitmap[entry]= value;
	}

  int X, Y, sum;
  glief CB (X2-X1, Y2-Y1, -X1, Y2-1,
	      bmc->depth+ log2i (xfactor*yfactor), bmc->status);
  for (Y=Y1; Y<Y2; Y++)
    for (X=X1; X<X2; X++) {
      sum=0;
      indey= ((Y-Y1)*ww+ (X-X1))*xfactor;
      for (j=0, index= indey; j<yfactor; j++, index+=ww)
	for (i=0; i<xfactor; i++)
	  sum += bitmap[index+ i];
      CB->set (X, Y, sum);
    }
  xo= off_x;
  yo= off_y;
  STACK_DELETE_ARRAY (bitmap);

  // cout << CB << "\n";
  return CB;
}

glief
shrink (glief bmc, int xfactor, int yfactor, SI& xo, SI& yo) {
  if ((bmc->width==0) || (bmc->height==0))
    fatal_error ("zero size character", "shrink", "glief.cpp");

  int tx= xfactor/3;
  int ty= yfactor/3;
  int dx=0, dy=0;
  if ((bmc->status==0) && (xfactor>1)) dx= get_hor_shift (bmc, xfactor, tx);
  // if ((bmc->status==0) && (yfactor>1)) dy= get_ver_shift (bmc, yfactor, ty);

  glief ret= shrink (bmc, xfactor, yfactor, dx, dy, tx, ty, xo, yo);
  if (ret->status != 0) {
    if (ret->status&1) ret->adjust_top ();
    if (ret->status&2) ret->adjust_bot ();
    ret->yoff= yo= 0;
  }
  return ret;
}
