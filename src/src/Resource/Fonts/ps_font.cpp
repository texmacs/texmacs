
/******************************************************************************
* MODULE     : ps_font.cpp
* DESCRIPTION: post-script fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"

/******************************************************************************
* The ps_font representation
******************************************************************************/

struct ps_font_rep: font_rep {
  string        family;
  int           dpi;
  bitmap_metric bmm;
  bitmap_font   bmf;

  ps_font_rep (display dis, string name, string family, int size, int dpi);
  void get_extents (string s, text_extents& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (ps_device dev, string s, SI x, SI y);
  bitmap_char get_bitmap (string s);
};

/******************************************************************************
* The implementation
******************************************************************************/

ps_font_rep::ps_font_rep (
  display dis, string name, string family2, int size2, int dpi2):
    font_rep (dis, name)
{
  text_extents ex;
  dis->load_ps_font (family2, size2, dpi2, bmm, bmf);

  family       = family2;
  size         = size2;
  dpi          = dpi2;

  // get main font parameters
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  display_size = y2-y1;
  design_size  = size << 8;

  // get character heights
  get_extents ("x", ex);
  yx           = ex->y4;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute widths
  wpt          = (dpi*PIXEL)/72;
  wquad        = (wpt*design_size) >> 8;
  wline        = wquad/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (ex->x2-ex->x1)<<1);
  extra= spc;
  sep  = wquad/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size);
  if (slope<0.15) slope= 0.0;
}

void
ps_font_rep::get_extents (string s, text_extents& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    QN c= s[0];
    text_extents_struct* first= bmm->get (c);
    ex->x1= first->x1; ex->y1= first->y1;
    ex->x2= first->x2; ex->y2= first->y2;
    ex->x3= first->x3; ex->y3= first->y3;
    ex->x4= first->x4; ex->y4= first->y4;
    SI x= first->x2;

    int i;
    for (i=1; i<N(s); i++) {
      QN c= s[i];
      text_extents_struct* next= bmm->get (c);
      ex->x1= min (ex->x1, x+ next->x1); ex->y1= min (ex->y1, next->y1);
      ex->x2= max (ex->x2, x+ next->x2); ex->y2= max (ex->y2, next->y2);
      ex->x3= min (ex->x3, x+ next->x3); ex->y3= min (ex->y3, next->y3);
      ex->x4= max (ex->x4, x+ next->x4); ex->y4= max (ex->y4, next->y4);
      x += next->x2;
    }
  }
}

void
ps_font_rep::get_xpositions (string s, SI* xpos) {
  register int i, n= N(s);
  if (n == 0) return;
  
  register SI x= 0;
  for (i=0; i<N(s); i++) {
    text_extents_struct* next= bmm->get ((QN) s[i]);
    x += next->x2;
    xpos[i+1]= x;
  }
}

void
ps_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  if (N(s)!=0) {
    int i;
    for (i=0; i<N(s); i++) {
      QN c= s[i];
      dev->draw (c, bmf, x, y);
      text_extents_struct* ex (bmm->get (c));
      x += ex->x2;
    }
  }
}

bitmap_char
ps_font_rep::get_bitmap (string s) {
  if (N(s)!=1) return font_rep::get_bitmap (s);
  int c= ((QN) s[0]);
  bitmap_char bmc= bmf->get (c);
  if (nil (bmc)) return font_rep::get_bitmap (s);
  return bmc;
}

/******************************************************************************
* Interface
******************************************************************************/

font
ps_font (display dis, string family, int size, int dpi) {
  string name= "ps:" * family * as_string (size) * "@" * as_string (dpi);
  if (font::instances -> contains (name)) return font (name);
  else return new ps_font_rep (dis, name, family, size, dpi);
}
