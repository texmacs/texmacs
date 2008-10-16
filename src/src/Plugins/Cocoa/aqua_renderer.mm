
/******************************************************************************
* MODULE     : aqua_renderer.mm
* DESCRIPTION: Cocoa drawing interface class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "mac_cocoa.h"
#include "aqua_renderer.h"
#include "analyze.hpp"
#include "image_files.hpp"
#include "aqua_rgb.h"

aqua_renderer_rep::aqua_renderer_rep (aqua_gui dis2, int w2, int h2)
  : dis (dis2), w (w2), h (h2)
{
  cur_fg      = black;
  cur_bg      = white;
 
  #if 0 
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
  #endif
}

aqua_renderer_rep::~aqua_renderer_rep () {} ;


/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
aqua_renderer_rep::encode (SI& x, SI& y) {
  x= (x*pixel) - ox;
  y= ((-y)*pixel) - oy;
}

void
aqua_renderer_rep::decode (SI& x, SI& y) {
  x += ox; y += oy;
  if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
  if (y>=0) y= -(y/pixel); else y= -((y-pixel+1)/pixel);
}

/******************************************************************************/

void aqua_renderer_rep::get_extents (int& w2, int& h2) { w2 = w; h2 = h; } 
//bool aqua_renderer_rep::interrupted (bool check) { return false; } 
bool
aqua_renderer_rep::interrupted (bool check) {
	return false;
  bool ret = dis->check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
  if (ret) cout << "INTERRUPTED:" << ret << "\n";
  return ret;
}


/* routines from renderer.hpp **********************************************/

/******************************************************************************
* Drawing into drawables
******************************************************************************/

color
aqua_renderer_rep::rgb (int r, int g, int b) {
  return rgb_color (r, g, b);
}

void
aqua_renderer_rep::get_rgb (color col, int& r, int& g, int& b) {
  get_rgb_color (col, r, g, b);
}

color
aqua_renderer_rep::get_color () {
  return cur_fg;
}

#if 0
color
aqua_renderer_rep::get_color (string s) {
  return named_color (s);
}
#endif

color
aqua_renderer_rep::get_background () {
  return cur_bg;
}

void
aqua_renderer_rep::set_color (color c) {
  [dis->cmap[c] set];
  cur_fg= c;
}

void
aqua_renderer_rep::set_background (color c) {
//  XSetBackground (dpy, gc, dis->cmap[c]);
  cur_bg= c;
}

void
aqua_renderer_rep::set_line_style (SI lw, int type, bool round) { (void) type;
  if (lw <= pixel)
  {
    [NSBezierPath setDefaultLineWidth:1.0];
  }
  else
  {
    [NSBezierPath setDefaultLineWidth:(lw+thicken)/(1.0*pixel)];
  }
  [NSBezierPath setDefaultLineCapStyle:(round ? NSRoundLineCapStyle : NSButtLineCapStyle)];
  [NSBezierPath setDefaultLineJoinStyle: NSRoundLineJoinStyle];
}

void
aqua_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  y1--; y2--; // top-left origin to bottom-left origin conversion
  [NSBezierPath strokeLineFromPoint:NSMakePoint(x1,y1) toPoint:NSMakePoint(x2,y2)];
}


void
aqua_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i].x= xx;
    pnt[i].y= yy;
    if (i>0) [NSBezierPath strokeLineFromPoint:pnt[i-1] toPoint:pnt[i]]; // FIX: hack
  }
 // XDrawLines (dpy, win, gc, pnt, n, CoordModeOrigin);
  STACK_DELETE_ARRAY (pnt);
}


void
aqua_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
	  if ((x1>=x2) || (y1<=y2)) return;
	NSRect rect = NSMakeRect(x1,y2,x2-x1,y1-y2);
  [dis->cmap[cur_bg] set];
  [NSBezierPath fillRect:rect];
  [dis->cmap[cur_fg] set];
}

void
aqua_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
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
  [NSBezierPath fillRect:NSMakeRect(x1,y2,x2-x1,y1-y2)];
}

void
aqua_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
aqua_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
aqua_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i] = NSMakePoint(xx,yy);
  }
  
  NSBezierPath *path = [NSBezierPath bezierPath];
  [path  appendBezierPathWithPoints:pnt count:n];
  [path setWindingRule:(convex? NSEvenOddWindingRule : NSNonZeroWindingRule)];
  [path fill];
  STACK_DELETE_ARRAY (pnt);
}


//void aqua_renderer_rep::draw (int char_code, font_glyphs fn, SI x, SI y) {} ;
//void aqua_renderer_rep::xpm (url file_name, SI x, SI y) {} ;
void aqua_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
                              double cx1, double cy1, double cx2, double cy2) {} ;


int char_clip=0;

#define conv(x) ((SI) (((double) (x))*(fn->unit)))


void
aqua_renderer_rep::draw_clipped (NSImage *im, int w, int h, SI x, SI y) {
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
  [im drawAtPoint:NSMakePoint(x,y) fromRect:NSMakeRect(0,0,w,h) operation:NSCompositeSourceAtop fraction:1.0];
}  



void aqua_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  x_character xc (c, fng, sfactor, cur_fg, cur_bg);
  aqua_image mi = dis->character_image [xc];
  if (is_nil(mi)) {
    // cout << "CACHING:" << c << "\n" ;
    dis->prepare_color (sfactor, cur_fg, cur_bg);
    x_character col_entry (0, font_glyphs (), sfactor, cur_fg, cur_bg);
    color* cols= (color*) dis->color_scale [col_entry];
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
    [im lockFocus];
    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
        if (gl->get_x(i,j)!=0) {
        color col= cols [gl->get_x(i,j)];
        [(NSColor*)dis->cmap[col] set];
        [NSBezierPath fillRect:NSMakeRect(i,j,1,1)];
        }
      }
    [im unlockFocus];
    
    aqua_image mi2(im, xo, yo, w, h );
	mi = mi2;
    [im release]; // aqua_image retains im
    dis->character_image (xc)= mi;
    // FIXME: we must release the image at some point (this should be ok now, see aqua_image)
  }
  
  // draw the character
  draw_clipped (mi->img, mi->w, mi->h,
                x- mi->xo*sfactor, y+ mi->yo*sfactor);
}

#undef conv

#undef conv

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

NSColor *xpm_to_ns_color(string s)
{
  if (s == "none") return [NSColor colorWithDeviceWhite:0.5 alpha:0.0];
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return [NSColor colorWithDeviceRed:r/256.0 green:g/256.0 blue:b/256.0 alpha:1.0];
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return [NSColor colorWithDeviceRed:r/256.0 green:g/256.0 blue:b/256.0 alpha:1.0];
  }
  if ((N(s) == 13) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 5));
    int g= from_hexadecimal (s (5, 9));
    int b= from_hexadecimal (s (9, 13));
    return [NSColor colorWithDeviceRed:r/256.0 green:g/256.0 blue:b/256.0 alpha:1.0];
  }
  char *name = as_charp(s);
  for(int i = 0; i<RGBColorsSize; i++) {
   if (strcmp(name,RGBColors[i].name)==0) {
	 delete [] name;

     return [NSColor colorWithDeviceRed:RGBColors[i].r/256.0 green:RGBColors[i].g/256.0 blue:RGBColors[i].b/256.0 alpha:1.0];
   }
  }
  delete  [] name;
  return [NSColor blackColor];
}



//void aqua_renderer_rep::xpm_initialize (url file_name) 
NSImage* xpm_init(url file_name)
{
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
                 "aqua_renderer_rep::xpm_initialize");
  
  // setup colors
  string first_name;
  hashmap<string,NSColor *> pmcs(0);
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

	pmcs(name)= xpm_to_ns_color(def);
	#if 0
	int r=0,g=0,b=0;
		pmcs(name)= (def == "none" ?
	    [NSColor colorWithDeviceWhite:1.0 alpha:0.0] :
		(get_rgb_color(named_color(def),r,g,b),
		[NSColor colorWithDeviceRed:r/256.0 green:g/256.0 blue:b/256.0 alpha:1.0]) );
	#endif	
  }
  
  NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
	[im setFlipped:YES];
  [im lockFocus];

  // setup pixmap
  for (y=0; y<h; y++) {
    if (N(t)< (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      NSColor* pmc = pmcs[(pmcs->contains (name) ? name : first_name)];
      [pmc set];
	  [NSBezierPath fillRect:NSMakeRect(x,y,1,1)];
    }
  }
  [im unlockFocus];
  return im;
//  dis->xpm_pixmap (as_string (file_name))= (int) pm;
//  dis->xpm_bitmap (as_string (file_name))= (int) bm;
}


extern int char_clip;

NSImage *aqua_renderer_rep::xpm_image(url file_name)
{ 
	NSImage *image = nil;
  aqua_image mi = dis->images [as_string(file_name)];
  if (is_nil(mi)) {    
		image = xpm_init(file_name);
		aqua_image mi2(image,0,0,w,h);
		mi = mi2;
		dis->images(as_string(file_name)) = mi2; 	
  }  
  else image = mi->img;
	return image;
}

void aqua_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  
 // char *chstr = as_charp(as_string(file_name));
//  NSString *name = [NSString stringWithCString:chstr];
 // delete [] chstr;
//  name = [[name stringByDeletingPathExtension] stringByAppendingPathExtension:@"png"];
  ///name = [name stringByDeletingPathExtension];
  NSImage *image = xpm_image(file_name);
  
  if (sfactor != 1)
    fatal_error ("Shrinking factor should be 1", "aqua_renderer_rep::xpm");
  int w, h;
  NSSize imgSize = [image size];
  w = imgSize.width; h = imgSize.height;

//  [(NSImageRep*)[[image representations] objectAtIndex:0]  drawAtPoint:NSMakePoint(x,y)];
  
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}



/* clipping */
//void aqua_renderer_rep::get_clipping (SI &x1, SI &y1, SI &x2, SI &y2) {} ;
//void aqua_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {} ;

void
aqua_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
//	[NSBezierPath clipRect:NSMakeRect(x1,y2,x2-x1,y1-y2)];
	//	[NSBezierPath clipRect:NSMakeRect(x1,y2,x2-x1,y1-y2)];
}



/* shadowing and copying rectangular regions across devices */


void aqua_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer dev, SI x, SI y) {} ;
void aqua_renderer_rep::new_shadow (renderer& dev) { dev =  this; } ;
void aqua_renderer_rep::delete_shadow (renderer& dev) {     dev= NULL; } ;
void aqua_renderer_rep::get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {} ;
void aqua_renderer_rep::put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {} ;
void aqua_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {} ;

#if 1
font x_font (string family, int size, int dpi)
{
  return NULL;
}
#endif


