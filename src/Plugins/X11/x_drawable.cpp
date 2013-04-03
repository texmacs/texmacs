
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

extern bool reverse_colors;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

x_drawable_rep::x_drawable_rep (x_gui gui2, x_window_rep* x_win2):
  renderer_rep (true), gui (gui2), x_win (x_win2),
  drawable_type (0), w (0), h (0)
{
  dpy    = gui->dpy;
  gc     = gui->gc;
  cur_fg = black;
  cur_bg = white;
}

x_drawable_rep::x_drawable_rep (x_gui gui2, int w2, int h2):
  renderer_rep (true), gui (gui2), x_win (NULL),
  drawable_type (1), w (w2), h (h2)
{
  dpy    = gui->dpy;
  gc     = gui->gc;
  cur_fg = black;
  cur_bg = white;
  win= (Drawable) XCreatePixmap (gui->dpy, gui->root, w, h, gui->depth);
}

x_drawable_rep::x_drawable_rep (x_gui gui2, Pixmap pm, int w2, int h2):
  renderer_rep (true), gui (gui2), win ((Drawable) pm), x_win (NULL),
  drawable_type (2), w (w2), h (h2)
{
  XGCValues values;
  int    scr  = DefaultScreen (gui->dpy);
  Window root = RootWindow (gui->dpy, scr);
  dpy    = gui->dpy;
  gc     = XCreateGC (dpy, root, 0, &values);
  cur_fg = black;
  cur_bg = white;
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

color
x_drawable_rep::get_color () {
  return cur_fg;
}

brush
x_drawable_rep::get_background () {
  return bg_brush;
}

void
x_drawable_rep::set_color (color c) {
  cur_fg= c;
  XSetForeground (dpy, gc, CONVERT (blend (cur_fg, cur_bg)));
}

void
x_drawable_rep::set_background (brush b) {
  bg_brush= b;
  cur_bg= b->c;
  XSetBackground (dpy, gc, CONVERT (cur_bg));
}

void
x_drawable_rep::set_line_style (SI lw, int type, bool round) { (void) type;
  if (lw <= pixel)
    XSetLineAttributes (dpy, (GC) gc, 1,
			LineSolid, round?CapRound:CapButt, JoinRound);
  else
    XSetLineAttributes (dpy, (GC) gc, (lw+thicken) / pixel,
			LineSolid, round?CapRound:CapButt, JoinRound);
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
  XSetForeground (dpy, gc, CONVERT (cur_bg));
  XFillRectangle (dpy, win, gc, x1, y2, x2-x1, y1-y2);
  XSetForeground (dpy, gc, CONVERT (blend (cur_fg, cur_bg)));
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

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

void
x_drawable_rep::xpm_initialize (url file_name) {
  tree t= xpm_load (file_name);

  // get main info
  int ok, i=0, j, k, w, h, c, b, x, y;
  string s= as_string (t[0]);
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0)) {
    cerr << "file_name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }

  // setup colors
  string first_name;
  hashmap<string,int> pmcs(0);
  hashmap<string,int> bmcs(1);
  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string name= "";
    string def = "none";
    if (N(s)<b) i=N(s);
    else { name= s(0,b); i=b; }
    if (k==0) first_name= name;

    skip_spaces (s, i);
    if ((i<N(s)) && (s[i]=='s')) {
      i++;
      skip_spaces (s, i);
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      skip_spaces (s, i);
    }
    if ((i<N(s)) && (s[i]=='c')) {
      i++;
      skip_spaces (s, i);
      j=i;
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      def= locase_all (s (j, i));
    }
    if (def == "none") {
      bmcs(name)= 0;
      def= "lightgrey";
    }
    else bmcs(name)= 1;
 /* FIXME: to avoid code duplication, replace this code by
      a call to xpm_colors(), plus the appropriate code to
      fill bmcs() & set first_name. */

    c_string _def (def);
    XColor exact, closest;
    XLookupColor (gui->dpy, gui->cols, _def, &exact, &closest);
    if (!reverse_colors && XAllocColor (gui->dpy, gui->cols, &exact))
      pmcs(name)= exact.pixel;
    else if (!reverse_colors && XAllocColor (gui->dpy, gui->cols, &closest))
      pmcs(name)= closest.pixel;
    else {
      color myc= rgb_color (exact.red/256, exact.green/256, exact.blue/256);
      pmcs(name)= CONVERT (myc);
    }
  }

  // setup bitmap and pixmap
  Pixmap pm= XCreatePixmap (gui->dpy, gui->root, w, h, gui->depth);
  int byte_width= ((w-1)>>3)+1;
  char* data= tm_new_array<char> (byte_width * h);
  for (i=0; i<(byte_width * h); i++) data[i]=0;
  for (y=0; y<h; y++) {
    if (N(t)< (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      int bit;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      int bmc= bmcs[name];
      int pmc= pmcs[name];
      if (!bmcs->contains (name)) bmc= bmcs[first_name];
      if (!pmcs->contains (name)) pmc= pmcs[first_name];
      XSetForeground (gui->dpy, gui->pixmap_gc, pmc);
      XDrawPoint (gui->dpy, (Drawable) pm, gui->pixmap_gc, x, y);
      bit= y*byte_width + (x>>3);
      if (bmc!=0) data[bit]= data[bit] | (1<<(x&7));      
    }
  }
  Pixmap bm= XCreateBitmapFromData (gui->dpy, gui->root, data, w, h);
  gui->xpm_pixmap (as_string (file_name))= (int) pm;
  gui->xpm_bitmap (as_string (file_name))= (int) bm;
  tm_delete_array (data);
}

extern bool char_clip;

void
x_drawable_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  if (!gui->xpm_pixmap->contains (as_string (file_name)))
    xpm_initialize (file_name);
  ASSERT (pixel == PIXEL, "pixel and PIXEL should coincide");
  int w, h;
  xpm_size (file_name, w, h);
  Pixmap bm= (Pixmap) gui->xpm_bitmap [as_string (file_name)];
  Pixmap pm= (Pixmap) gui->xpm_pixmap [as_string (file_name)];
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (pm, bm, w, h, x, y);
  char_clip=old_clip;
}

/******************************************************************************
* General pictures
******************************************************************************/

Pixmap load_Pixmap (url u, int w, int h);

void
x_drawable_rep::image (url u, SI w, SI h, SI x, SI y, int alpha) {
  // Given an image of original size (W, H),
  // we display it at position (x, y) in a rectangle of size (w, h)
  (void) alpha; // FIXME

  w= w/pixel; h= h/pixel;
  decode (x, y);

  Pixmap pm= load_Pixmap (u, w, h);

  XCopyArea (dpy, (Drawable) pm, win, gc, 0, 0, w, h, x, y-h);
}
