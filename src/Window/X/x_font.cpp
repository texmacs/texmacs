
/******************************************************************************
* MODULE     : x_font.cpp
* DESCRIPTION: server_ps_fonts and server_tex_fonts under X windows
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "X/x_window.hpp"
#include "X/x_font.hpp"
#include "font.hpp"

/******************************************************************************
* Displaying characters
******************************************************************************/

bool char_clip= true;

#define conv(x) ((SI) (((double) (x))*(fn->unit)))

void
x_drawable_rep::draw_clipped (Pixmap pm, Pixmap bm, int w, int h, SI x, SI y) {
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
  int X1= max (x1- x, 0); if (X1>=w) return;
  int Y1= max (y1- y, 0); if (Y1>=h) return;
  int X2= min (x2- x, w); if (X2<0) return;
  int Y2= min (y2- y, h); if (Y2<0) return;

  if (char_clip) {
#ifdef OS_WIN32_LATER
    /*
    int X, Y, N;
    for (Y=Y1; Y<Y2; Y++) {
      for (X=X1, N=0; X<X2; X++) {
	if (XGetBitmapPixel (bm, X, Y)) N++;
	else {
	  if (N > 0)
	    XCopyArea (dpy, (Drawable)pm, win, gc, X-N, Y, N, 1, x+X1-N, y+Y1);
	  N= 0;
	}
      }
      if (N > 0)
	XCopyArea (dpy, (Drawable)pm, win, gc, X, Y-N, N, 1, x+X1-N, y+Y1);
    }
    */
    XCopyClipped (dpy, (Drawable) pm, (Drawable) bm, win, gc,
		  X, Y-N, N, 1, x+X1-N, y+Y1);
    // FIXME: Dan should write a routine for this.
#else
    XSetClipMask (dpy, gc, bm);
    XSetClipOrigin (dpy, gc, x, y);
    XCopyArea (dpy, (Drawable) pm, win, gc, X1, Y1, X2-X1, Y2-Y1, x+X1, y+Y1);
    set_clipping (cx1- ox, cy1- oy, cx2- ox, cy2- oy);
#endif
  }
  else
    XCopyArea (dpy, (Drawable) pm, win, gc, X1, Y1, X2-X1, Y2-Y1, x+X1, y+Y1);
}

void
x_drawable_rep::draw (int c, bitmap_font bmf, SI x, SI y) {
  // get the pixmap
  x_character xc (c, bmf, sfactor, cur_fg, cur_bg);
  Pixmap pm= (Pixmap) dis->character_pixmap [xc];
  if (pm == 0) {
    dis->prepare_color (sfactor, cur_fg, cur_bg);
    x_character col_entry (0, bitmap_font (), sfactor, cur_fg, cur_bg);
    color* cols= (color*) dis->color_scale [col_entry];
    SI xo, yo;
    bitmap_char pre_bmc= bmf->get (c); if (nil (pre_bmc)) return;
    bitmap_char bmc= shrink (pre_bmc, sfactor, sfactor, xo, yo);
    int i, j, w= bmc->width, h= bmc->height;
    pm= XCreatePixmap (dis->dpy, dis->root, w, h, dis->depth);
    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
	color col= cols [bmc->get_x(i,j)];
	XSetForeground (dis->dpy, dis->pixmap_gc, dis->cmap[col]);
	XDrawPoint (dis->dpy, (Drawable) pm, dis->pixmap_gc, i, j);
      }
    dis->character_pixmap (xc)= (pointer) pm;
  }

  // get the bitmap
  xc= x_character (c, bmf, sfactor, 0, 0);
  Bitmap bm= (Bitmap) dis->character_bitmap [xc];
  if (bm == NULL) {
    SI xo, yo;
    bitmap_char pre_bmc= bmf->get (c); if (nil (pre_bmc)) return;
    bitmap_char bmc= shrink (pre_bmc, sfactor, sfactor, xo, yo);
    int i, j, b, on, w= bmc->width, h= bmc->height;
    int byte_width= ((w-1)>>3)+1;
    char* data= new char [byte_width * h];
    for (i=0; i<(byte_width * h); i++) data[i]=0;

    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
	b = j*byte_width + (i>>3);
	on= (bmc->get_x(i,j)!=0 ? 1:0);
	if (on) data[b]= data[b] | (1<<(i&7));
      }
    bm= new Bitmap_rep;
    bm->bm    = XCreateBitmapFromData (dis->dpy, dis->root, data, w, h);
    bm->width = bmc->width;
    bm->height= bmc->height;
    bm->xoff  = xo;
    bm->yoff  = yo;
    dis->character_bitmap (xc)= (pointer) bm;
    delete[] data;
  }

  // draw the character
  draw_clipped (pm, bm->bm, bm->width, bm->height,
		x- bm->xoff*sfactor, y+ bm->yoff*sfactor);
}

#undef conv

/******************************************************************************
* Loading postscript fonts
******************************************************************************/

void
x_display_rep::get_ps_char (Font fn, int c,
			    text_extents& ex, bitmap_char& bmc)
{
  XCharStruct xcs;
  int i1, i2, i3;
  char temp[1]; temp[0]= (char) c;

  XQueryTextExtents (dpy, fn, temp, 1, &i1, &i2, &i3, &xcs);
  ex->x1= 0;
  ex->y1= (-1-xcs.descent) * PIXEL;
  ex->x2= xcs.width * PIXEL;
  ex->y2= (-1+xcs.ascent) * PIXEL;
  ex->x3= xcs.lbearing * PIXEL;
  ex->y3= (-1-xcs.descent) * PIXEL;
  ex->x4= xcs.rbearing * PIXEL;
  ex->y4= (-1+xcs.ascent) * PIXEL;

  int w   = xcs.rbearing- xcs.lbearing;
  int h   = xcs.ascent+ xcs.descent;
  int xoff=  -xcs.lbearing;
  int yoff= h-xcs.descent ;
  if ((w == 0) || (h == 0)) return;

  Pixmap pm= XCreatePixmap (dpy, root, w, h, depth);
  XSetForeground (dpy, pixmap_gc, white);
  XFillRectangle (dpy, pm, pixmap_gc, 0, 0, w, h);
  XSetForeground (dpy, pixmap_gc, black);
  XSetFont (dpy, pixmap_gc, fn);
  XDrawString (dpy, pm, pixmap_gc, xoff, yoff, temp, 1);
  XImage* im= XGetImage (dpy, pm, 0, 0, w, h, 0xffffffff, ZPixmap);

  int i, j;
  bmc= bitmap_char (w, h, xoff, yoff);
  for (j=0; j<h; j++)
    for (i=0; i<w; i++) {
      int c = im->f.get_pixel (im, i, j);
      int on= (c == black? 1: 0);
      bmc->set_x (i, j, on);
    }
  bmc->lwidth= xcs.width;

  im->f.destroy_image (im);
  XFreePixmap (dpy, pm);
}

void
x_display_rep::load_ps_font (string family, int size, int dpi,
			     bitmap_metric& bmm, bitmap_font& bmf)
{
  string fn_name= "ps:" * family * as_string (size) * "@" * as_string (dpi);
  if (bitmap_metric::instances -> contains (fn_name)) {
    bmm= bitmap_metric (fn_name);
    bmf= bitmap_font (fn_name);
  }

  string name= "-" * family;
  string sz1= as_string ((size*dpi)/72);
  string sz2= as_string (10*((size*dpi)/72));
  name << "-*-" * sz1 * "-" * sz2 * "-*-*-*-*-*-*";
  if (size == 0) name= family;

  if (DEBUG_AUTO) cout << "TeXmacs] Loading ps font " << name << "\n";
  char* temp= as_charp (name);
  Font fn = XLoadFont (dpy, temp);
  delete[] temp;
  if (XQueryFont (dpy, fn) == NULL) {
    if (DEBUG_AUTO) cout << "TeXmacs] Font " << name << " not found\n";
    if (DEBUG_AUTO) cout << "TeXmacs] Using default font instead\n";
    fn = XLoadFont (dpy, "*");
    if (XQueryFont (dpy, fn) == NULL)
      fatal_error ("Could not load default X font", "x_ps_font_rep::prepare");
  }

  int i;
  text_extents* texs= new text_extents[256];
  bitmap_char * bmcs= new bitmap_char [256];
  for (i=0; i<=255; i++)
    get_ps_char (fn, i, texs[i], bmcs[i]);
  bmm= std_bitmap_metric (fn_name, texs, 0, 255);
  bmf= std_bitmap_font (fn_name, bmcs, 0, 255);
}

/******************************************************************************
* Server fonts
******************************************************************************/

static string the_default_display_font ("");
font the_default_wait_font;

void
x_display_rep::set_default_font (string name) {
  the_default_display_font= name;
}

font
x_display_rep::default_font_sub (bool tt) {
  string s= the_default_display_font;
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if ((s[j] >= '0') && (s[j] <= '9')) break;
  string fam= s (0, j);
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    if (((out_lan == "russian") || (out_lan == "ukrainian")) &&
	((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (ff == "ec")
      return tex_ec_font (this, tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (this, tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (this, tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (this, fam, sz, dpi);
  // if (out_lan == "german") return tex_font (this, "ygoth", 14, 300, 0);
  // return tex_font (this, "rpagk", 10, 300, 0);
  // return tex_font (this, "rphvr", 10, 300, 0);
  // return ps_font (this, "b&h-lucidabright-medium-r-normal", 11, 300);
}

font
x_display_rep::default_font (bool tt) {
  font fn= default_font_sub (tt);
  the_default_wait_font= fn;
  return fn;
}
