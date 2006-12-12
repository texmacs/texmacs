
/******************************************************************************
* MODULE     : x_drawables.cpp
* DESCRIPTION: Drawables under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "X/x_window.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include "Ghostscript/ghostscript.hpp"
#include "Imlib2/imlib2.hpp"

extern bool reverse_colors;
extern hashmap<tree,string> ps_bbox;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

x_drawable_rep::x_drawable_rep (x_display dis2, int w2, int h2):
  dis (dis2), w (w2), h (h2)
{
  dpy         = dis->dpy;
  gc          = dis->gc;
  cur_fg      = dis->black;
  cur_bg      = dis->white;

  black       = dis->black;
  white       = dis->white;
  red         = dis->red;
  green       = dis->green;
  blue        = dis->blue;
  yellow      = dis->yellow;
  magenta     = dis->magenta;
  orange      = dis->orange;
  brown       = dis->brown;
  pink        = dis->pink;
  light_grey  = dis->light_grey;
  grey        = dis->grey;
  dark_grey   = dis->dark_grey;

  if ((w>0) && (h>0))
    win= (Drawable) XCreatePixmap (dis->dpy, dis->root, w, h, dis->depth);
}

x_drawable_rep::~x_drawable_rep () {
  if ((w>0) && (h>0)) XFreePixmap (dis->dpy, (Pixmap) win);
}

bool
x_drawable_rep::is_x_drawable () {
  return true;
}

x_drawable_rep*
x_drawable_rep::as_x_drawable () {
  return this;
}

void
x_drawable_rep::get_extents (int& w2, int& h2) {
  w2= w;
  h2= h;
}

bool
x_drawable_rep::interrupted (bool check) {
  return dis->check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
}

/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
x_drawable_rep::encode (SI& x, SI& y) {
  x= (x*pixel) - ox;
  y= ((-y)*pixel) - oy;
}

void
x_drawable_rep::decode (SI& x, SI& y) {
  x += ox; y += oy;
  if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
  if (y>=0) y= -(y/pixel); else y= -((y-pixel+1)/pixel);
}

/******************************************************************************
* Clipping
******************************************************************************/

void
x_drawable_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  ps_device_rep::set_clipping (x1, y1, x2, y2);
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
x_drawable_rep::rgb (int r, int g, int b) {
  return dis->rgb (r, g, b);
}

void
x_drawable_rep::get_rgb (color col, int& r, int& g, int& b) {
  dis->get_rgb (col, r, g, b);
}

color
x_drawable_rep::get_color () {
  return cur_fg;
}

color
x_drawable_rep::get_color (string s) {
  return dis->get_color (s);
}

color
x_drawable_rep::get_background () {
  return cur_bg;
}

void
x_drawable_rep::set_color (color c) {
  XSetForeground (dpy, gc, dis->cmap[c]);
  cur_fg= c;
}

void
x_drawable_rep::set_background (color c) {
  XSetBackground (dpy, gc, dis->cmap[c]);
  cur_bg= c;
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
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
  XSetForeground (dpy, gc, dis->cmap[cur_bg]);
  XFillRectangle (dpy, win, gc, x1, y2, x2-x1, y1-y2);
  XSetForeground (dpy, gc, dis->cmap[cur_fg]);
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
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
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
  if ((!ok) || (N(t)<(c+1)) || (c<=0))
    fatal_error ("Invalid xpm (" * as_string (file_name) * ")",
		 "x_drawable_rep::xpm_initialize");

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

    char* _def= as_charp (def);
    XColor exact, closest;
    XLookupColor (dis->dpy, dis->cols, _def, &exact, &closest);
    if (!reverse_colors && XAllocColor (dis->dpy, dis->cols, &exact))
      pmcs(name)= exact.pixel;
    else if (!reverse_colors && XAllocColor (dis->dpy, dis->cols, &closest))
      pmcs(name)= closest.pixel;
    else {
      int myc= dis->rgb (exact.red/256, exact.green/256, exact.blue/256);
      pmcs(name)= dis->cmap[myc];
    }
    delete[] _def;
  }

  // setup bitmap and pixmap
  Pixmap pm= XCreatePixmap (dis->dpy, dis->root, w, h, dis->depth);
  int byte_width= ((w-1)>>3)+1;
  char* data= new char [byte_width * h];
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
      XSetForeground (dis->dpy, dis->pixmap_gc, pmc);
      XDrawPoint (dis->dpy, (Drawable) pm, dis->pixmap_gc, x, y);
      bit= y*byte_width + (x>>3);
      if (bmc!=0) data[bit]= data[bit] | (1<<(x&7));      
    }
  }
  Pixmap bm= XCreateBitmapFromData (dis->dpy, dis->root, data, w, h);
  dis->xpm_pixmap (as_string (file_name))= (int) pm;
  dis->xpm_bitmap (as_string (file_name))= (int) bm;
  delete[] data;
}

extern bool char_clip;

void
x_drawable_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  if (!dis->xpm_pixmap->contains (as_string (file_name)))
    xpm_initialize (file_name);
  if (sfactor != 1)
    fatal_error ("Shrinking factor should be 1", "x_drawable_rep::xpm");
  int w, h;
  xpm_size (file_name, w, h);
  Pixmap bm= (Pixmap) dis->xpm_bitmap [as_string (file_name)];
  Pixmap pm= (Pixmap) dis->xpm_pixmap [as_string (file_name)];
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (pm, bm, w, h, x, y);
  char_clip=old_clip;
}

/******************************************************************************
* Invocation of ghostscript
******************************************************************************/

static int cache_image_last_gc = 0;
static int cache_image_tot_size= 0;
static int cache_image_max_size= 10000;
static hashmap<tree,Pixmap> cache_image (0);
static hashmap<tree,int> cache_image_w (0);
static hashmap<tree,int> cache_image_h (0);
static hashmap<tree,int> cache_image_time (0);
static hashmap<tree,int> cache_image_nr (0);

void
x_drawable_rep::image (
  url u, SI w, SI h, SI x, SI y,
  double cx1, double cy1, double cx2, double cy2)
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)

  w= w/pixel; h= h/pixel;
  decode (x, y);

  if (dis->gswindow == NULL) {
    widget dummy= text_widget ("ghostscript window");
    if (ghostscript_bugged ()) {
      SI max_w= 2 * dis->display_width  * PIXEL;
      SI max_h= 2 * dis->display_height * PIXEL;
      dummy= glue_widget (false, false, max_w, max_h);
    }
    dis->gswindow= new x_window_rep (dummy, dis, "ghostscript", 0, 0);
  }

  Pixmap pm;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
	 << as_string (cx1) << as_string (cy1)
	 << as_string (cx2) << as_string (cy2);
  if (cache_image->contains (lookup)) pm= (Pixmap) cache_image [lookup];
  else {
    // rendering
    Window gs_win= dis->gswindow->win;
    pm= XCreatePixmap (dis->dpy, gs_win, w, h, dis->depth);
    if (imlib2_supports (u))
      imlib2_display (dpy, pm, u, w, h, cx1, cy1, cx2, cy2);
    else
      ghostscript_run (dpy, gs_win, pm, u, w, h, cx1, cy1, cx2, cy2);

    // caching
    if (N(cache_image_nr) == 0) cache_image_last_gc= texmacs_time ();
    cache_image      (lookup)= (int) pm;
    cache_image_w    (lookup)= w;
    cache_image_h    (lookup)= h;
    cache_image_time (lookup)= texmacs_time ();
    cache_image_nr   (lookup)= cache_image_nr [lookup] + 1;
    cache_image_tot_size += w*h;
    if (cache_image_tot_size > cache_image_max_size) {
      dis->image_auto_gc ();
      if (cache_image_tot_size > cache_image_max_size)
	cache_image_max_size= cache_image_tot_size << 1;
    }
  }

  XCopyArea (dpy, (Drawable) pm, win, gc, 0, 0, w, h, x, y-h);
}

void
x_display_rep::image_auto_gc () {
  int time= texmacs_time ();
  if (time-cache_image_last_gc <= 300000) return;
  cache_image_last_gc= time;
  if (DEBUG_AUTO)
    cout << "TeXmacs] Launching garbage collection for unused pictures\n";

  iterator<tree> it= iterate (cache_image);
  while (it->busy()) {
    tree lookup= it->next();
    int diff= time- cache_image_time [lookup];
    int fact= cache_image_nr [lookup];
    fact= fact * fact * fact;
    if (cache_image_w [lookup] * cache_image_h [lookup] < 400) fact= fact * 5;
    if (cache_image_w [lookup] * cache_image_h [lookup] < 6400) fact= fact * 5;
    if (diff/fact > 60000) {
      Pixmap pm= (Pixmap) cache_image [lookup];
      XFreePixmap (dpy, pm);
      cache_image->reset (lookup);
      cache_image_w->reset (lookup);
      cache_image_h->reset (lookup);
      cache_image_time->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}

void
x_display_rep::image_gc (string name) {
  (void) name;
  cache_image_last_gc= texmacs_time ();
  iterator<tree> it= iterate (cache_image);
  while (it->busy()) {
    tree lookup= it->next();
    if (!is_ramdisc (as_url (lookup[0]))) {
      Pixmap pm= (Pixmap) cache_image [lookup];
      XFreePixmap (dpy, pm);
      cache_image->reset (lookup);
      cache_image_w->reset (lookup);
      cache_image_h->reset (lookup);
      cache_image_time->reset (lookup);
      cache_image_nr->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}
