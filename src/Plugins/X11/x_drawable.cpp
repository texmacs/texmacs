
/******************************************************************************
* MODULE     : x_drawables.cpp
* DESCRIPTION: Drawables under X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "X11/x_window.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "x_picture.hpp"

extern bool reverse_colors;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

x_drawable_rep::x_drawable_rep (x_gui gui2, x_window_rep* x_win2):
  renderer_rep (true), gui (gui2), x_win (x_win2),
  drawable_type (0), w (0), h (0)
{
  dpy     = gui->dpy;
  gc      = gui->gc;
  pen     = pencil (black);
  bg_brush= brush (white);
}

x_drawable_rep::x_drawable_rep (x_gui gui2, int w2, int h2):
  renderer_rep (true), gui (gui2), x_win (NULL),
  drawable_type (1), w (w2), h (h2)
{
  dpy     = gui->dpy;
  gc      = gui->gc;
  pen     = pencil (black);
  bg_brush= brush (white);
  win= (Drawable) XCreatePixmap (gui->dpy, gui->root, w, h, gui->depth);
}

x_drawable_rep::x_drawable_rep (x_gui gui2, Pixmap pm, int w2, int h2):
  renderer_rep (true), gui (gui2), win ((Drawable) pm), x_win (NULL),
  drawable_type (2), w (w2), h (h2)
{
  XGCValues values;
  int    scr  = DefaultScreen (gui->dpy);
  Window root = RootWindow (gui->dpy, scr);
  dpy     = gui->dpy;
  gc      = XCreateGC (dpy, root, 0, &values);
  pen     = pencil (black);
  bg_brush= brush (white);
}

x_drawable_rep::~x_drawable_rep () {
  if (drawable_type == 1)
    XFreePixmap (dpy, (Pixmap) win);
  if (drawable_type == 2)
    XFreeGC (dpy, gc);
}

void*
x_drawable_rep::get_handle () {
  return (void*) this;
}

void
x_drawable_rep::get_extents (int& w2, int& h2) {
  if (x_win != NULL) return x_win->get_extents (w2, h2);
  else {
    w2= w;
    h2= h;
  }
}

/******************************************************************************
* Clipping
******************************************************************************/

void
x_drawable_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
  Region region= XCreateRegion ();
  decode (x1, y1);
  decode (x2, y2);
  XRectangle r;
  r.x     = x1;
  r.y     = y2;
  r.width = x2-x1;
  r.height= y1-y2;
  XUnionRectWithRegion (&r, region, region);
  XSetRegion (dpy, gc, region);
  XDestroyRegion (region);
}

/******************************************************************************
* Drawing into drawables
******************************************************************************/

pencil
x_drawable_rep::get_pencil () {
  return pen;
}

brush
x_drawable_rep::get_background () {
  return bg_brush;
}

void
x_drawable_rep::set_pencil (pencil p) {
  pen= p;
  color col= blend_colors (pen->get_color (), bg_brush->get_color ());
  XSetForeground (dpy, gc, CONVERT (col));
  if (pen->get_width () <= pixel)
    XSetLineAttributes (dpy, (GC) gc,
                        1, LineSolid,
                        pen->get_cap () == cap_round? CapRound: CapButt,
                        JoinRound);
  else
    XSetLineAttributes (dpy, (GC) gc,
                        (pen->get_width () + thicken) / pixel, LineSolid,
                        pen->get_cap () == cap_round? CapRound: CapButt,
                        JoinRound);
}

void
x_drawable_rep::set_background (brush b) {
  bg_brush= b;
  XSetBackground (dpy, gc, CONVERT (b->get_color ()));
}

void
x_drawable_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  y1--; y2--; // top-left origin to bottom-left origin conversion
  XDrawLine (dpy, win, gc, x1, y1, x2, y2);
}

void
x_drawable_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, XPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i].x= xx;
    pnt[i].y= yy;
  }
  XDrawLines (dpy, win, gc, pnt, n, CoordModeOrigin);
  STACK_DELETE_ARRAY (pnt);
}

void
x_drawable_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  outer_round (x1, y1, x2, y2); // might be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
  XSetForeground (dpy, gc, CONVERT (bg_brush->get_color ()));
  XFillRectangle (dpy, win, gc, x1, y2, x2-x1, y1-y2);
  color col= blend_colors (pen->get_color (), bg_brush->get_color ());
  XSetForeground (dpy, gc, CONVERT (col));
}

void
x_drawable_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x2>x1) && ((x2-x1)<pixel)) {
    SI d= pixel-(x2-x1);
    x1 -= (d>>1);
    x2 += ((d+1)>>1);
  }
  if ((y2>y1) && ((y2-y1)<pixel)) {
    SI d= pixel-(y2-y1);
    y1 -= (d>>1);
    y2 += ((d+1)>>1);
  }

  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); // might be needed somewhere
  if ((x1>=x2) || (y1>=y2)) return;

  decode (x1, y1);
  decode (x2, y2);
  XFillRectangle (dpy, win, gc, x1, y2, x2-x1, y1-y2);
}

void
x_drawable_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
x_drawable_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
x_drawable_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, XPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i].x= xx;
    pnt[i].y= yy;
  }
  XFillPolygon (dpy, win, gc, pnt, n, convex?Convex:Complex, CoordModeOrigin);
  STACK_DELETE_ARRAY (pnt);
}

/*
 * TODO: is this needed?
 *
 ******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************

color
xpm_color (string s) {
  if (s == "none") return rgb_color (100, 100, 100, 0);
  c_string _def (s);
  XColor exact, closest;
  XLookupColor (the_gui->dpy, the_gui->cols, _def, &exact, &closest);
  if (!reverse_colors && XAllocColor (the_gui->dpy, the_gui->cols, &exact))
    return rgb_color (exact.red/256, exact.green/256, exact.blue/256);
  if (!reverse_colors && XAllocColor (the_gui->dpy, the_gui->cols, &closest))
    return rgb_color (closest.red/256, closest.green/256, closest.blue/256);
  return rgb_color (exact.red/256, exact.green/256, exact.blue/256);
}
*/
