
/******************************************************************************
* MODULE     : x_picture.cpp
* DESCRIPTION: X pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "X11/x_window.hpp"
#include "x_picture.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "iterator.hpp"
#include "Ghostscript/ghostscript.hpp"
#include "effect.hpp"

#define IMLIB2_X11TEXMACS // for imlib2_display
#include "Imlib2/imlib2.hpp"

//extern hashmap<tree,string> ps_bbox; //not used
extern int nr_windows;

#define VCONVERT(c) (true_colors? (c & 0xffffff): the_gui->cmap [c & 0xffffff])

/******************************************************************************
* Abstract X pictures
******************************************************************************/

x_picture_rep::x_picture_rep (Pixmap pm2, int w2, int h2, int ox2, int oy2):
  pm (pm2), im (NULL), bm (0), data (NULL),
  w (w2), h (h2), ox (ox2), oy (oy2), ok (true) {}
x_picture_rep::~x_picture_rep () {
  XFreePixmap (the_gui->dpy, pm);
  if (im != NULL) XDestroyImage (im);
  if (bm != 0) XFreePixmap (the_gui->dpy, bm);
  if (data != NULL) tm_delete_array (data); }

void
x_picture_rep::force_mask () {
  int byte_width= ((w-1)>>3)+1;
  if (data == NULL) {
    data= tm_new_array<char> (byte_width * h);
    for (int i=0; i<byte_width * h; i++) data[i]= 0x00;
  }
  if (bm == 0)
    bm= XCreateBitmapFromData (the_gui->dpy, the_gui->root, data, w, h);
  ok= true;
}

picture_kind x_picture_rep::get_type () { return picture_native; }
void* x_picture_rep::get_handle () { return (void*) this; }

int x_picture_rep::get_width () { return w; }
int x_picture_rep::get_height () { return h; }
int x_picture_rep::get_origin_x () { return ox; }
int x_picture_rep::get_origin_y () { return oy; }
void x_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
x_picture_rep::internal_get_pixel (int x, int y) {
  if (im == NULL)
    im= XGetImage (the_gui->dpy, pm, 0, 0, w, h, AllPlanes, XYPixmap);
  if (im != NULL) {
    unsigned long c= XGetPixel (im, x, h-1-y);
    int r= (c >> 16) & 0xff;
    int g= (c >> 8 ) & 0xff;
    int b= (c      ) & 0xff;
    if (r == 0x64 && g == 0x65 && b == 0x66) return 0x00ffffff;
    return rgb_color (r, g, b, 255);
  }
  return 0;
}

void
x_picture_rep::internal_set_pixel (int x, int y, color col) {
  if (0 > x || 0 > y || x >= w || y >= h) return;
  ok= false;
  int r, g, b, a;
  get_rgb_color (col, r, g, b, a);
  r= (r * a + 255 * (255 - a)) / 255;
  g= (g * a + 255 * (255 - a)) / 255;
  b= (b * a + 255 * (255 - a)) / 255;
  col= rgb_color (r, g, b, 255);
  XSetForeground (the_gui->dpy, the_gui->pixmap_gc, VCONVERT (col));
  XDrawPoint (the_gui->dpy, (Drawable) pm, the_gui->pixmap_gc, x, h - 1 - y);
  if (im != NULL)
    XPutPixel (im, x, h-1-y, (r << 16) + (g << 8) + b);
  if (data == NULL && a < 64) {
    int byte_width= ((w-1)>>3)+1;
    data= tm_new_array<char> (byte_width * h);
    for (int i=0; i<byte_width * h; i++) data[i]= 0xff;
  }
  if (data != NULL) {
    int byte_width= ((w-1)>>3)+1;
    int idx= (h-1-y) * byte_width + (x>>3);
    if (a < 64)
      data[idx]= data[idx] & (~(1<<(x&7)));
    else
      data[idx]= data[idx] | (1<<(x&7));
  }
}

picture
x_picture (Pixmap pm, int w, int h, int ox, int oy) {
  return (picture) tm_new<x_picture_rep> (pm, w, h, ox, oy);
}

picture
as_x_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  int w= pic->get_width (), h= pic->get_height ();
  int ox= pic->get_origin_x (), oy= pic->get_origin_y ();
  Pixmap pm= XCreatePixmap (the_gui->dpy, the_gui->root, w, h, the_gui->depth);
  picture ret= x_picture (pm, w, h, ox, oy);
  ret->copy_from (pic);
  return ret;
}

picture
as_native_picture (picture pict) {
  return as_x_picture (pict);
}

Pixmap
retrieve_bitmap (picture pic) {
  x_picture_rep* rep= (x_picture_rep*) pic->get_handle ();
  rep->force_mask ();
  return rep->bm;
}

Pixmap
retrieve_pixmap (picture pic) {
  x_picture_rep* rep= (x_picture_rep*) pic->get_handle ();
  return rep->pm;
}

picture
native_picture (int w, int h, int ox, int oy) {
  Pixmap pm= XCreatePixmap (the_gui->dpy, the_gui->root, w, h, the_gui->depth);
  return x_picture (pm, w, h, ox, oy);
}

void
x_drawable_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  (void) alpha; // FIMXE: might be improved
  p= as_x_picture (p);
  x_picture_rep* pict= (x_picture_rep*) p->get_handle ();
  int w= pict->w, h= pict->h;
  int x0= pict->ox, y0= pict->h - 1 - pict->oy;
  decode (x, y);
  x -= x0; y -= y0;
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  int X1= max (x1- x, 0); if (X1>=w) return;
  int Y1= max (y1- y, 0); if (Y1>=h) return;
  int X2= min (x2- x, w); if (X2<0) return;
  int Y2= min (y2- y, h); if (Y2<0) return;
  if (pict->data != NULL) {
    if (!pict->ok) {
      if (pict->bm != 0) XFreePixmap (gui->dpy, pict->bm);
      pict->bm= XCreateBitmapFromData (gui->dpy, gui->root, pict->data, w, h);
      pict->ok= true;
    }
    XSetClipMask (dpy, gc, pict->bm);
    XSetClipOrigin (dpy, gc, x, y);
    XCopyArea (dpy, pict->pm, win, gc, X1, Y1, X2-X1, Y2-Y1, x+X1, y+Y1);
    set_clipping (cx1- ox, cy1- oy, cx2- ox, cy2- oy);
  }
  else
    XCopyArea (dpy, pict->pm, win, gc, X1, Y1, X2-X1, Y2-Y1, x+X1, y+Y1);
}

/******************************************************************************
* Rendering on images
******************************************************************************/

Pixmap
get_Pixmap (picture p) {
  x_picture_rep* rep= (x_picture_rep*) p->get_handle ();
  return rep->pm;
}

x_image_renderer_rep::x_image_renderer_rep (picture p, double zoom):
  x_drawable_rep (the_gui, get_Pixmap (p), p->get_width (), p->get_height ()),
  pict (p)
{
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox = pox * pixel;
  oy = poy * pixel;
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;

  Region region= XCreateRegion ();
  XRectangle r;
  r.x     = 0;
  r.y     = 0;
  r.width = w;
  r.height= h;
  XUnionRectWithRegion (&r, region, region);
  XSetRegion (dpy, gc, region);
  XDestroyRegion (region);
  XSetForeground (dpy, gc, VCONVERT (white));
  XFillRectangle (dpy, win, gc, 0, 0, w, h);
}

x_image_renderer_rep::~x_image_renderer_rep () {}

void*
x_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<x_image_renderer_rep> (p, zoomf);
}

/******************************************************************************
* Loading pictures
******************************************************************************/

Pixmap
load_Pixmap (url u, int w, int h) {
  if (the_gui->gswindow == NULL) {
    SI max_w= the_gui->screen_width  * PIXEL;
    SI max_h= the_gui->screen_height * PIXEL;
    //widget dummy= text_widget (0, "ghostscript window");
    if (ghostscript_bugged ()) {
      max_w *= 2;
      max_h *= 2;
      //dummy= glue_widget (false, false, max_w, max_h);
    }
    widget dummy = glue_widget (false, false, max_w, max_h);
    widget win   = plain_window_widget (dummy, "Ghostscript");
    the_gui->gswindow= get_x_window (win);
    //the_gui->gswindow= tm_new<x_window_rep> (dummy, the_gui, "ghostscript",
    //0, 0);
    //the_gui->gswindow= tm_new<x_window_rep> (dummy, the_gui, "ghostscript",
    //max_w, max_h, max_w, max_h, max_w, max_h);
    nr_windows--; // the dummy window should not be counted
  }

  Pixmap pm;
  Window gs_win= the_gui->gswindow->win;

  // XCreatePixmap does not allow for zero sized images.
  // This fixes bug #10425.
  w = (w==0 ? 1 : w);
  h = (h==0 ? 1 : h);

  pm= XCreatePixmap (the_gui->dpy, gs_win, w, h, the_gui->depth);
  if (imlib2_supports (u))
    imlib2_display (the_gui->dpy, pm, u, w, h);
  else {
    //XSetForeground (the_gui->dpy, gc, white);
    //XFillRectangle (the_gui->dpy, pm, gc, 0, 0, w, h);
    ghostscript_run (the_gui->dpy, gs_win, pm, u, w, h);
  }

  return pm;
}

picture
load_picture (url u, int w, int h) {
  Pixmap pm= load_Pixmap (u, w, h);
  if (pm == 0) return error_picture (w, h);
  return x_picture (pm, w, h, 0, 0);
}

picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  picture pic= load_picture (u, w, h);
  if (eff != "") {
    effect e= build_effect (eff);
    array<picture> a; a << pic;
    pic= e->apply (a, pixel);
  }
  return pic;
}

void
save_picture (url dest, picture p) {
  (void) dest; (void) p;
  FAILED ("saving bitmap pictures has not been implemented under X11");
}
