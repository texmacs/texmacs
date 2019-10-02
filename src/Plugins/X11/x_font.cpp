
/******************************************************************************
* MODULE     : x_font.cpp
* DESCRIPTION: server_ps_fonts and server_tex_fonts under X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "X11/x_window.hpp"
#include "X11/x_font.hpp"
#include "analyze.hpp"
#include "dictionary.hpp"

/******************************************************************************
* Displaying characters
******************************************************************************/

bool char_clip= true;

#define conv(x) ((SI) (((double) (x))*(fn->unit)))

void
x_drawable_rep::draw_clipped (Pixmap pm, Pixmap bm, int w, int h, SI x, SI y) {
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  // outer_round (x1, y1, x2, y2); // might be needed somewhere
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
x_drawable_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  color fgc= pen->get_color ();
  color bgc= bg_brush->get_color ();
  x_character xc (c, fng, std_shrinkf, fgc, bgc);
  Pixmap pm= (Pixmap) gui->character_pixmap [xc];
  if (pm == 0) {
    gui->prepare_color (std_shrinkf, fgc, bgc);
    x_character col_entry (0, font_glyphs (), std_shrinkf, fgc, bgc);
    color* cols= (color*) gui->color_scale [col_entry];
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    pm= XCreatePixmap (gui->dpy, gui->root, w, h, gui->depth);
    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
	color col= cols [gl->get_x(i,j)];
	XSetForeground (gui->dpy, gui->pixmap_gc, CONVERT (col));
	XDrawPoint (gui->dpy, (Drawable) pm, gui->pixmap_gc, i, j);
      }
    gui->character_pixmap (xc)= (pointer) pm;
  }

  // get the bitmap
  xc= x_character (c, fng, std_shrinkf, 0, 0);
  Bitmap bm= (Bitmap) gui->character_bitmap [xc];
  if (bm == NULL) {
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int i, j, b, on, w= gl->width, h= gl->height;
    int byte_width= ((w-1)>>3)+1;
    char* data= tm_new_array<char> (byte_width * h);
    for (i=0; i<(byte_width * h); i++) data[i]=0;

    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
	b = j*byte_width + (i>>3);
	on= (gl->get_x(i,j)!=0 ? 1:0);
	if (on) data[b]= data[b] | (1<<(i&7));
      }
    bm= tm_new<Bitmap_rep> ();
    bm->bm    = XCreateBitmapFromData (gui->dpy, gui->root, data, w, h);
    bm->width = gl->width;
    bm->height= gl->height;
    bm->xoff  = xo;
    bm->yoff  = yo;
    gui->character_bitmap (xc)= (pointer) bm;
    tm_delete_array (data);
  }

  // draw the character
  draw_clipped (pm, bm->bm, bm->width, bm->height,
		x- bm->xoff*std_shrinkf, y+ bm->yoff*std_shrinkf);
}

#undef conv

/******************************************************************************
* Server fonts
******************************************************************************/

static string the_default_font ("");
font the_default_wait_font;

void
x_gui_rep::set_default_font (string name) {
  the_default_font= name;
}

font
x_gui_rep::default_font_sub (bool tt, bool mini, bool bold) {
  string s= the_default_font;
  string series= (bold? string ("bold"): string ("medium"));
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if ((s[j] >= '0') && (s[j] <= '9')) break;
  string fam= s (0, j);
  if (mini && fam == "ecrm") fam= "ecss";
  if (bold && fam == "ecrm") fam= "ecbx";
  if (bold && fam == "ecss") fam= "ecsx";
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (mini) { sz= (int) (0.6 * sz); dpi= (int) (1.3333333 * dpi); }
  if (use_macos_fonts ()) {
    tree lucida_fn= tuple ("apple-lucida", "ss", series, "right");
    lucida_fn << as_string (sz) << as_string ((int) (0.95 * dpi));
    return find_font (lucida_fn);
  }
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();
    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
	 (out_lan == "ukrainian")) &&
	((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", series, "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ("fireflysung", sz, dpi);
    if (out_lan == "greek")
      return unicode_font ("Stix", sz, dpi);
    //if (out_lan == "japanese")
    //return unicode_font ("ipagui", sz, dpi);
    //if (out_lan == "korean")
    //return unicode_font ("UnDotum", sz, dpi);
    if (ff == "ec")
      return tex_ec_font (tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (fam, sz, dpi);
  // if (out_lan == "german") return tex_font ("ygoth", 14, 300, 0);
  // return tex_font ("rpagk", 10, 300, 0);
  // return tex_font ("rphvr", 10, 300, 0);
  // return ps_font ("b&h-lucidabright-medium-r-normal", 11, 300);
}

font
x_gui_rep::default_font (bool tt, bool mini, bool bold) {
  font fn= default_font_sub (tt, mini, bold);
  if (!tt && !mini) the_default_wait_font= fn;
  return fn;
}

void
set_default_font (string name) {
  the_gui->set_default_font (name);
}

font
get_default_font (bool tt, bool mini, bool bold) {
  return the_gui->default_font (tt, mini, bold);
}

/******************************************************************************
* Loading postscript fonts
******************************************************************************/

void
x_gui_rep::get_ps_char (Font fn, int c, metric& ex, glyph& gl) {
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
  gl= glyph (w, h, xoff, yoff);
  for (j=0; j<h; j++)
    for (i=0; i<w; i++) {
      int c = im->f.get_pixel (im, i, j);
      int on= (((color) c) == black? 1: 0);
      gl->set_x (i, j, on);
    }
  gl->lwidth= xcs.width;

  im->f.destroy_image (im);
  XFreePixmap (dpy, pm);
}

void
x_gui_rep::load_system_font (string family, int size, int dpi,
			     font_metric& fnm, font_glyphs& fng)
{
  string fn_name= "ps:" * family * as_string (size) * "@" * as_string (dpi);
  if (font_metric::instances -> contains (fn_name)) {
    fnm= font_metric (fn_name);
    fng= font_glyphs (fn_name);
  }

  string name= "-" * family;
  string sz1= as_string ((size*dpi)/72);
  string sz2= as_string (10*((size*dpi)/72));
  name << "-*-" * sz1 * "-" * sz2 * "-*-*-*-*-*-*";
  if (size == 0) name= family;

  if (DEBUG_VERBOSE) debug_fonts << "Loading X font " << name << "\n";
  XFontStruct *xfs = NULL;
  c_string temp (name);
  xfs = XLoadQueryFont (dpy, temp);
  if (xfs == NULL) {
    if (DEBUG_VERBOSE) debug_fonts << "Font " << name << " not found\n";
    if (DEBUG_VERBOSE) debug_fonts << "Using default font instead\n";
    xfs = XLoadQueryFont (dpy, "*");
    ASSERT (xfs != NULL, "could not load default X font");
  }
  Font fn = xfs->fid;
  int i;
  metric* texs= tm_new_array<metric> (256);
  glyph * gls = tm_new_array<glyph> (256);
  for (i=0; i<=255; i++)
    get_ps_char (fn, i, texs[i], gls[i]);
  fnm= std_font_metric (fn_name, texs, 0, 255);
  fng= std_font_glyphs (fn_name, gls , 0, 255);

  XFreeFont(dpy, xfs);
}

void
load_system_font (string family, int size, int dpi,
		  font_metric& fnm, font_glyphs& fng)
{
  the_gui->load_system_font (family, size, dpi, fnm, fng);
}

/******************************************************************************
* The implementation
******************************************************************************/

x_font_rep::x_font_rep (string name, string family2, int size2, int dpi2):
  font_rep (name)
{
  metric ex;
  load_system_font (family2, size2, dpi2, fnm, fng);

  family       = family2;
  size         = size2;
  dpi          = dpi2;

  // get main font parameters
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  display_size = y2-y1;
  design_size  = size << 8;

  // get character dimensions
  get_extents ("x", ex);
  yx           = ex->y2;
  get_extents ("M", ex);
  wquad        = ex->x2;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute other widths
  wpt          = (dpi*PIXEL)/72;
  hpt          = (dpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (ex->x2-ex->x1)<<1);
  extra= spc;
  mspc = spc;
  sep  = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size);
  if (slope<0.15) slope= 0.0;
}


bool
x_font_rep::supports (string c) {
  glyph gl;
  if (c == "<less>") gl= fng->get ('<');
  else if (c == "<gtr>") gl= fng->get ('>');
  else if (N(c) == 1) gl= fng->get (c[0]);
  return !is_nil (gl);
}

void
x_font_rep::get_extents (string s, metric& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    QN c= s[0];
    metric_struct* first= fnm->get (c);
    ex->x1= first->x1; ex->y1= first->y1;
    ex->x2= first->x2; ex->y2= first->y2;
    ex->x3= first->x3; ex->y3= first->y3;
    ex->x4= first->x4; ex->y4= first->y4;
    SI x= first->x2;

    int i;
    for (i=1; i<N(s); i++) {
      QN c= s[i];
      metric_struct* next= fnm->get (c);
      ex->x1= min (ex->x1, x+ next->x1); ex->y1= min (ex->y1, next->y1);
      ex->x2= max (ex->x2, x+ next->x2); ex->y2= max (ex->y2, next->y2);
      ex->x3= min (ex->x3, x+ next->x3); ex->y3= min (ex->y3, next->y3);
      ex->x4= max (ex->x4, x+ next->x4); ex->y4= max (ex->y4, next->y4);
      x += next->x2;
    }
  }
}

void
x_font_rep::get_xpositions (string s, SI* xpos) {
  int i, n= N(s);
  if (n == 0) return;
  
  SI x= 0;
  for (i=0; i<N(s); i++) {
    metric_struct* next= fnm->get ((QN) s[i]);
    x += next->x2;
    xpos[i+1]= x;
  }
}

void
x_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  if (N(s)!=0) {
    int i;
    for (i=0; i<N(s); i++) {
      QN c= s[i];
      ren->draw (c, fng, x, y);
      metric_struct* ex= fnm->get (c);
      x += ex->x2;
    }
  }
}

font
x_font_rep::magnify (double zoomx, double zoomy) {
  if (zoomx != zoomy) return poor_magnify (zoomx, zoomy);
  return x_font (family, size, (int) tm_round (dpi * zoomx));
}

glyph
x_font_rep::get_glyph (string s) {
  if (N(s)!=1) return font_rep::get_glyph (s);
  int c= ((QN) s[0]);
  glyph gl= fng->get (c);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

int
x_font_rep::index_glyph (string s, font_metric& rm, font_glyphs& rg) {
  if (N(s)!=1) return font_rep::index_glyph (s, rm, rg);
  int c= ((QN) s[0]);
  glyph gl= fng->get (c);
  if (is_nil (gl)) return font_rep::index_glyph (s, rm, rg);
  rm= fnm;
  rg= fng;
  return c;
}

/******************************************************************************
* Interface
******************************************************************************/

font
x_font (string family, int size, int dpi) {
  string name= "ps:" * family * as_string (size) * "@" * as_string (dpi);
  if (font::instances -> contains (name)) return font (name);
  else return tm_new<x_font_rep> (name, family, size, dpi);
}
