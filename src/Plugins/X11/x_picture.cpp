
/******************************************************************************
* MODULE     : x_picture.cpp
* DESCRIPTION: X pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "x_picture.hpp"

#define VCONVERT(c) (true_color? (c & 0xffffff): the_gui->cmap [c & 0xffffff])

/******************************************************************************
* Abstract X pictures
******************************************************************************/

x_picture_rep::x_picture_rep (Pixmap pm2, int w2, int h2, int ox2, int oy2):
  pm (pm2), w (w2), h (h2), ox (ox2), oy (oy2) {}
x_picture_rep::~x_picture_rep () {
  XFreePixmap (the_gui->dpy, pm); }

picture_kind x_picture_rep::get_type () { return picture_native; }
void* x_picture_rep::get_handle () { return (void*) this; }

int x_picture_rep::get_width () { return w; }
int x_picture_rep::get_height () { return h; }
int x_picture_rep::get_origin_x () { return ox; }
int x_picture_rep::get_origin_y () { return oy; }
void x_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
x_picture_rep::internal_get_pixel (int x, int y) {
  (void) x; (void) y;
  // NOTE: structure is write only
}

void
x_picture_rep::internal_set_pixel (int x, int y, color col) {
  if (0 > x || 0 > y || x >= w || y >= h) return;
  XSetForeground (the_gui->dpy, the_gui->pixmap_gc, VCONVERT (col));
  XDrawPoint (the_gui->dpy, (Drawable) pm, the_gui->pixmap_gc, x, h - 1 - y);
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
pixmap_picture (int w, int h, int ox, int oy) {
  Pixmap pm= XCreatePixmap (the_gui->dpy, the_gui->root, w, h, the_gui->depth);
  return x_picture (pm, w, h, ox, oy);
}

picture
scalable_picture (int w, int h, int ox, int oy) {
  (void) w; (void) h; (void) ox; (void) oy;
  FAILED ("not yet implemented");
}

picture
x_drawable_rep::create_picture (SI x1, SI y1, SI x2, SI y2) {
  SI x0= 0, y0= 0;
  decode (x0, y0);
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  x2= max (x1, x2);
  y2= min (y1, y2);
  return pixmap_picture (x2-x1, y1-y2, x0 - x1, (y1 - y2 - 1) - (y0 - y2));
}

void
x_drawable_rep::draw_picture (picture p, SI x, SI y) {
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
  //y--; // top-left origin to bottom-left origin conversion
  int X1= max (x1- x, 0); if (X1>=w) return;
  int Y1= max (y1- y, 0); if (Y1>=h) return;
  int X2= min (x2- x, w); if (X2<0) return;
  int Y2= min (y2- y, h); if (Y2<0) return;
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

x_image_renderer_rep::x_image_renderer_rep (picture p, renderer m):
  x_drawable_rep (the_gui, get_Pixmap (p),
                  p->get_width (), p->get_height ()),
  pict (p)
{
  ox = m->ox;
  oy = m->oy;
  cx1= m->cx1;
  cy1= m->cy1;
  cx2= m->cx2;
  cy2= m->cy2;
  is_screen= m->is_screen;
  zoomf= m->zoomf;
  shrinkf= m->shrinkf;
  pixel= m->pixel;
  thicken= m->thicken;

  x_drawable_rep* mren= (x_drawable_rep*) m->get_handle ();
  SI x0= 0, y0= 0;
  mren->decode (x0, y0);
  int x1b= x0 - p->get_origin_x ();
  int y2b= y0 + p->get_origin_y () - (p->get_height () - 1);
  ox  -= x1b * pixel;
  oy  += y2b * pixel;
  cx1 -= x1b * pixel;
  cy1 += y2b * pixel;
  cx2 -= x1b * pixel;
  cy2 += y2b * pixel;

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

renderer
picture_renderer (picture p, renderer m) {
  return (renderer) tm_new<x_image_renderer_rep> (p, m);
}

void
delete_renderer (renderer ren) {
  tm_delete (ren);
}
