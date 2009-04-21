
/******************************************************************************
* MODULE     : aqua_renderer.mm
* DESCRIPTION: Cocoa drawing interface class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "aqua_renderer.h"
#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"

#include "aqua_utilities.h"
#include "MacOS/mac_images.h"



/******************************************************************************
 * Cocoa images
 ******************************************************************************/

struct aqua_image_rep: concrete_struct {
	NSImage* img;
	SI xo,yo;
	int w,h;
	aqua_image_rep (NSImage* img2, SI xo2, SI yo2, int w2, int h2) :
  img (img2), xo (xo2), yo (yo2), w (w2), h (h2) { [img retain]; };
	~aqua_image_rep()  {  [img release]; };
	friend class aqua_image;
};

class aqua_image {
	CONCRETE_NULL(aqua_image);
  aqua_image (NSImage* img2, SI xo2, SI yo2, int w2, int h2):
  rep (tm_new <aqua_image_rep> (img2, xo2, yo2, w2, h2)) {}	
};

CONCRETE_NULL_CODE(aqua_image);

/******************************************************************************
 * CG images
 ******************************************************************************/

struct cg_image_rep: concrete_struct {
	CGImageRef img;
	SI xo,yo;
	int w,h;
	cg_image_rep (CGImageRef img2, SI xo2, SI yo2, int w2, int h2) :
    img (img2), xo (xo2), yo (yo2), w (w2), h (h2) { CGImageRetain(img); };
	~cg_image_rep()  {  CGImageRelease(img); };
	friend class cg_image;
};

class cg_image {
	CONCRETE_NULL(cg_image);
	cg_image (CGImageRef img2, SI xo2, SI yo2, int w2, int h2):
    rep (tm_new <cg_image_rep> (img2, xo2, yo2, w2, h2)) {}	
};

CONCRETE_NULL_CODE(cg_image);


/******************************************************************************
 * Global support variables for all aqua_renderers
 ******************************************************************************/


static hashmap<basic_character,cg_image> character_image;  // bitmaps of all characters
static hashmap<string,aqua_image> images; 



/******************************************************************************
 * aqua_renderer
 ******************************************************************************/

void 
aqua_set_color (color col) {
  int r, g, b;
  get_rgb_color(col,r,g,b);
  [[NSColor colorWithDeviceRed: r/255.0 green:g/255.0 blue:b/255.0 alpha:1.0] set];
}

aqua_renderer_rep::aqua_renderer_rep (int w2, int h2) :
  basic_renderer_rep(w2,h2), context(NULL)
{
}

aqua_renderer_rep::~aqua_renderer_rep () {
  if (context) end();
} ;

void 
aqua_renderer_rep::begin (void * c) { 
  context = (NSGraphicsContext*)c; 
  [context retain];
//  CGContextBeginPage(context, NULL);
}

void 
aqua_renderer_rep::end () { 
//  CGContextEndPage(context);
  [context release];
//  CGContextRelease(context); 
  context = NULL;  
}

void
aqua_renderer_rep::set_color (color c) {
  basic_renderer_rep::set_color(c);
  aqua_set_color(cur_fg);
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
 // y1--; y2--; // top-left origin to bottom-left origin conversion
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
  aqua_set_color (cur_bg);
  [NSBezierPath fillRect:rect];
  aqua_set_color (cur_fg);
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

/******************************************************************************
 * Image rendering
 ******************************************************************************/
struct aqua_cache_image_rep: cache_image_element_rep {
	aqua_cache_image_rep (int w2, int h2, int time2, NSImage *ptr2) :
  cache_image_element_rep(w2,h2,time2,ptr2) {  [(NSImage*)ptr retain]; };
	virtual ~aqua_cache_image_rep() {   [(NSImage*)ptr release]; };
};


void
aqua_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
                        double cx1, double cy1, double cx2, double cy2) 
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)
  
  // if (DEBUG_EVENTS) cout << "cg_renderer_rep::image " << as_string(u) << LF;
  
  w= w/pixel; h= h/pixel;
  decode (x, y);
  
  //painter.setRenderHints (0);
  //painter.drawRect (QRect (x, y-h, w, h));
  
  NSImage *pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
  << as_string (cx1) << as_string (cy1)
  << as_string (cx2) << as_string (cy2) << "cg-image" ;
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci)) {
    pm = static_cast<NSImage*> (ci->ptr);
  } else {
	  if (suffix (u) == "png") {
      // rendering
      string suu = as_string (u);
      // cout << suu << LF;
      pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
	  } else if (suffix (u) == "ps" ||
               suffix (u) == "eps" ||
               suffix (u) == "pdf") {
      url temp= url_temp (".png");
      mac_image_to_png (u, temp); 
//      system ("convert", u, temp);
      string suu = as_string (temp);
      pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
      remove (temp);
    }
    
    if (pm == NULL ) {
      cout << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
      return;
    }
    // caching
    ci = tm_new <aqua_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }
  
  NSSize isz = [pm size];
  [pm setFlipped:YES];
//  [pm drawAtPoint:NSMakePoint(x,y) fromRect:NSMakeRect(0,0,w,h) operation:NSCompositeSourceAtop fraction:1.0];
  [pm drawInRect:NSMakeRect(x,y-h,w,h) fromRect:NSMakeRect(0,0,isz.width,isz.height) operation:NSCompositeSourceAtop fraction:1.0];
}


void
aqua_renderer_rep::draw_clipped (NSImage *im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
  [im drawAtPoint:NSMakePoint(x,y) fromRect:NSMakeRect(0,0,w,h) operation:NSCompositeSourceAtop fraction:1.0];
}  



static CGContextRef 
MyCreateBitmapContext (int pixelsWide, int pixelsHigh) {
    int bitmapBytesPerRow   = (pixelsWide * 4);
    int bitmapByteCount     = (bitmapBytesPerRow * pixelsHigh);	
    CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    void *bitmapData = malloc( bitmapByteCount );
    if (bitmapData == NULL) {
        //fprintf (stderr, "Memory not allocated!");
        return NULL;
    }
    CGContextRef context = CGBitmapContextCreate (bitmapData, pixelsWide,	pixelsHigh,	8,
                                                  bitmapBytesPerRow, colorSpace, kCGImageAlphaPremultipliedLast);
    if (context == NULL) {
        free (bitmapData);
		// fprintf (stderr, "Context not created!");
        return NULL;
    }
    CGColorSpaceRelease (colorSpace);
    return context;
}


void
aqua_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
	// get the pixmap
	basic_character xc (c, fng, sfactor, 0, 0);
	cg_image mi = character_image [xc];
	if (is_nil(mi)) {
		SI xo, yo;
		glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
		glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
		int i, j, w= gl->width, h= gl->height;
		CGImageRef im = NULL;
		{
			CGContextRef ic = MyCreateBitmapContext(w,h);
			int nr_cols= sfactor*sfactor;
			if (nr_cols >= 64) nr_cols= 64;
			//CGContextSetShouldAntialias(ic,true);
			CGContextSetBlendMode(ic,kCGBlendModeCopy);
			//CGContextSetRGBFillColor(ic,1.0,1.0,1.0,0.0);
			//CGContextFillRect(ic,CGRectMake(0,0,w,h));
			
			for (j=0; j<h; j++)
				for (i=0; i<w; i++) {
					int col = gl->get_x (i, j);
					CGContextSetRGBFillColor(ic, 0.0,0.0,0.0,  ((255*col)/(nr_cols+1))/255.0);
					CGContextFillRect(ic,CGRectMake(i,j,1,1));
				}
			im = CGBitmapContextCreateImage (ic);
			CGContextRelease (ic);
		}
		cg_image mi2 (im, xo, yo, w, h);
		mi = mi2;
		CGImageRelease(im); // cg_image retains im
		character_image (xc)= mi;
	}
	
	// draw the character
	{
    CGContextRef cgc = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];

		(void) w; (void) h;
		int x1= x- mi->xo*sfactor;
		int y1=  y+ mi->yo*sfactor;
		decode (x1, y1);
		y1--; // top-left origin to bottom-left origin conversion
		CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
		CGContextSetShouldAntialias (cgc, true);
		CGContextSaveGState (cgc);
		//  cg_set_color (context, cur_fg);
		CGContextClipToMask (cgc, r, mi->img); 
		CGContextFillRect (cgc, r);
		CGContextRestoreGState (cgc);
	}  
}
#if 0
void aqua_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  basic_character xc (c, fng, sfactor, 0, 0);
  cg_image mi = character_image [xc];
  if (is_nil(mi)) {
    // cout << "CACHING:" << c << "\n" ;
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
    int nr_cols= sfactor*sfactor;
    if (nr_cols >= 64) nr_cols= 64;

    [im lockFocus];
    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
        int col = gl->get_x (i, j);
        [[NSColor colorWithDeviceRed:0.0 green:0.0 blue:0.0 alpha: ((255*col)/(nr_cols+1))/255.0] set]; 
        [NSBezierPath fillRect:NSMakeRect(i,j,1,1)];
      }
    [im unlockFocus];
    
    aqua_image mi2(im, xo, yo, w, h );
	mi = mi2;
    [im release]; // aqua_image retains im
    character_image (xc)= mi;
    // FIXME: we must release the image at some point (this should be ok now, see aqua_image)
  }
  
  // draw the character
  {
    CGContextRef cgc = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
    (void) w; (void) h;
    int x1= x- mi->xo*sfactor;
    int y1=  y+ mi->yo*sfactor;
    decode (x1, y1);
    y1--; // top-left origin to bottom-left origin conversion
    CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
    CGContextSetShouldAntialias (cgc, true);
    CGContextSaveGState (cgc);
    //  aqua_set_color (context, cur_fg);
    CGContextClipToMask (cgc, r, (CGImage*)(mi->img)); 
    CGContextFillRect (cgc, r);
    CGContextRestoreGState (cgc);
  }  

  
  // draw the character
//  draw_clipped (mi->img, mi->w, mi->h,
 //               x- mi->xo*sfactor, y+ mi->yo*sfactor);
}
#endif

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

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
  if ((!ok) || (N(t)<(c+1)) || (c<=0)) {
    cerr << "File name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }
  
  // setup colors
  string first_name;
  hashmap<string,color> pmcs;
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

		pmcs(name)= xpm_to_color(def);
  }
  NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
	[im setFlipped:YES];
  [im lockFocus];
  [[NSGraphicsContext currentContext] setCompositingOperation: NSCompositeCopy];

  // setup pixmap
  for (y=0; y<h; y++) {
    if (N(t)< (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      if ((name == first_name) || !(pmcs->contains (name)))
        [[NSColor colorWithDeviceWhite:1.0 alpha:0.0] set] ;      
      else {
      color col = pmcs[name];
      aqua_set_color (col);
        }
	  [NSBezierPath fillRect:NSMakeRect(x,y,1,1)];
    }
  }
  [im unlockFocus];
  return im;
}


extern int char_clip;

NSImage *
aqua_renderer_rep::xpm_image(url file_name)
{ 
	NSImage *image = nil;
  aqua_image mi = images [as_string(file_name)];
  if (is_nil(mi)) {    
		image = xpm_init(file_name);
    int w, h;
    NSSize imgSize = [image size];
    w = imgSize.width; h = imgSize.height;
		aqua_image mi2(image,0,0,w,h);
		mi = mi2;
		images(as_string(file_name)) = mi2; 	
    [image release];
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
  
  ASSERT (sfactor == 1, "shrinking factor should be 1");
  int w, h;
  NSSize imgSize = [image size];
  w = imgSize.width; h = imgSize.height;

//  [(NSImageRep*)[[image representations] objectAtIndex:0]  drawAtPoint:NSMakePoint(x,y)];
  
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}


/******************************************************************************
 * main cocoa renderer
 ******************************************************************************/


aqua_renderer_rep*
the_aqua_renderer () {
  static aqua_renderer_rep* the_renderer= NULL;
	if (!the_renderer) the_renderer= tm_new <aqua_renderer_rep> ();
	return the_renderer;
}

