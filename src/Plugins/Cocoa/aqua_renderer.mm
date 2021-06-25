
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

// bitmaps of all characters
static hashmap<basic_character,cg_image> character_image;
// image cache
static hashmap<string,aqua_image> images; 

/******************************************************************************
 * aqua_renderer
 ******************************************************************************/

aqua_renderer_rep::aqua_renderer_rep (int w2, int h2) :
  basic_renderer_rep (true, w2, h2), context(nil), view(nil)
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
  view = nil;
}

void
aqua_set_color (color col) {
    int r, g, b, a;
    get_rgb_color (col, r, g, b, a);
    [[NSColor colorWithDeviceRed: r/255.0 green:g/255.0 blue:b/255.0 alpha:a/255.0] set];
}


void
aqua_renderer_rep::set_pencil (pencil p) {
  basic_renderer_rep::set_pencil (p);
  aqua_set_color (pen->get_color ());
  double pw= (((double) pen->get_width ()) / ((double) pixel));
#if 0
  if (pw <= pixel) {
    [NSBezierPath setDefaultLineWidth:1.0];
  }
  else {
    [NSBezierPath setDefaultLineWidth: (pen->w+thicken)/(1.0*pixel)];
  }
#else
  [NSBezierPath setDefaultLineWidth: pw];
#endif
    if (pen->get_type () == pencil_brush) {
        brush br= pen->get_brush ();
#if 0
        //FIXME: brushes
        QImage* pm= get_pattern_image (br, pixel);
        int pattern_alpha= br->get_alpha ();
        painter->setOpacity (qreal (pattern_alpha) / qreal (255));
        if (pm != NULL) {
            b= QBrush (*pm);
            double pox, poy;
            decode (0, 0, pox, poy);
            QTransform tr;
            tr.translate (pox, poy);
            b.setTransform (tr);
            p= QPen (b, pw);
        }
#endif
    }
  [NSBezierPath setDefaultLineCapStyle: ((pen->get_cap () == cap_round) ? NSRoundLineCapStyle : NSButtLineCapStyle)];
  [NSBezierPath setDefaultLineJoinStyle: NSRoundLineJoinStyle];
}

void
aqua_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
    double rx1, ry1, rx2, ry2;
    decode (x1, y1, rx1, ry1);
    decode (x2, y2, rx2, ry2);
 // y1--; y2--; // top-left origin to bottom-left origin conversion
  [NSBezierPath strokeLineFromPoint:NSMakePoint(rx1,ry1) toPoint:NSMakePoint(rx2,ry2)];
}

void
aqua_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    double xx, yy;
    decode (x[i], y[i], xx, yy);
    pnt[i] = NSMakePoint (xx,yy);
    if (i>0) [NSBezierPath strokeLineFromPoint:pnt[i-1] toPoint:pnt[i]]; // FIX: hack
  }
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
	NSRect rect = NSMakeRect (x1, y2, x2-x1, y1-y2);
  aqua_set_color (bg_brush->get_color ());
  [NSBezierPath fillRect:rect];
  aqua_set_color (pen->get_color ());
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
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  //FIXME: XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
aqua_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  //FIXME: XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
aqua_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    double xx,yy;
    decode (x[i], y[i], xx, yy);
    pnt[i] = NSMakePoint(xx,yy);
  }
  
  NSBezierPath *path = [NSBezierPath bezierPath];
  [path  appendBezierPathWithPoints:pnt count:n];
  [path setWindingRule:(convex? NSEvenOddWindingRule : NSNonZeroWindingRule)];
  [path fill];
  STACK_DELETE_ARRAY (pnt);
}

void
aqua_renderer_rep::draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
    array<SI> x (3), y (3);
    x[0]= x1; y[0]= y1;
    x[1]= x2; y[1]= y2;
    x[2]= x3; y[2]= y3;
    NSPoint pnt[3];
    int i, n= N(x);
    if ((N(y) != n) || (n<1)) return;
    for (i=0; i<n; i++) {
        double xx,yy;
        decode (x[i], y[i], xx, yy);
        pnt[i] = NSMakePoint(xx,yy);
    }
    NSBezierPath *path = [NSBezierPath bezierPath];
    [path  appendBezierPathWithPoints:pnt count:n];
    [path setWindingRule: NSEvenOddWindingRule];
    [path fill];
}


/******************************************************************************
 * Image rendering
 ******************************************************************************/

void
aqua_renderer_rep::image (url u, SI w, SI h, SI x, SI y, int alpha) {
  // Given an image of original size (W, H),
  // we display it at position (x, y) in a rectangle of size (w, h)
  
  // if (DEBUG_EVENTS) debug_events << "cg_renderer_rep::image " << as_string(u) << LF;
  (void) alpha; // FIXME
  
  w= w/pixel; h= h/pixel;
  decode (x, y);
  
  //painter.setRenderHints (QFlags<QPainter::RenderHint> ());
  //painter.drawRect (QRect (x, y-h, w, h));
  
  NSImage *pm = NULL;
  if (suffix (u) == "png") {
    // rendering
    string suu = as_string (u);
    // debug_events << suu << LF;
    pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
  }
  else if (suffix (u) == "ps" ||
           suffix (u) == "eps" ||
           suffix (u) == "pdf") {
    url temp= url_temp (".png");
    mac_image_to_png (u, temp, w, h);
//  system ("convert", u, temp);
    string suu = as_string (temp);
    pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
    remove (temp);
  }
    
  if (pm == NULL ) {
    debug_events << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
    return;
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
	basic_character xc (c, fng, std_shrinkf, 0, 0);
	cg_image mi = character_image [xc];
	if (is_nil(mi)) {
		SI xo, yo;
		glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
		glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
		int i, j, w= gl->width, h= gl->height;
		CGImageRef im = NULL;
		{
			CGContextRef ic = MyCreateBitmapContext(w,h);
			int nr_cols= std_shrinkf*std_shrinkf;
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
		int x1= x- mi->xo*std_shrinkf;
		int y1=  y+ mi->yo*std_shrinkf;
		decode (x1, y1);
		y1--; // top-left origin to bottom-left origin conversion
		CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
		CGContextSetShouldAntialias (cgc, true);
		CGContextSaveGState (cgc);
		//  cg_set_color (context, pen->get_color ());
		CGContextClipToMask (cgc, r, mi->img); 
		CGContextFillRect (cgc, r);
		CGContextRestoreGState (cgc);
	}  
}
#if 0
void aqua_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  basic_character xc (c, fng, std_shrinkf, 0, 0);
  cg_image mi = character_image [xc];
  if (is_nil(mi)) {
    // debug_events << "CACHING:" << c << "\n" ;
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
    int nr_cols= std_shrinkf*std_shrinkf;
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
    int x1= x- mi->xo*std_shrinkf;
    int y1=  y+ mi->yo*std_shrinkf;
    decode (x1, y1);
    y1--; // top-left origin to bottom-left origin conversion
    CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
    CGContextSetShouldAntialias (cgc, true);
    CGContextSaveGState (cgc);
    //  aqua_set_color (context, pen->get_color ());
    CGContextClipToMask (cgc, r, (CGImage*)(mi->img)); 
    CGContextFillRect (cgc, r);
    CGContextRestoreGState (cgc);
  }  

  
  // draw the character
//  draw_clipped (mi->img, mi->w, mi->h,
 //               x- mi->xo*std_shrinkf, y+ mi->yo*std_shrinkf);
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
    failed_error << "File name= " << file_name << "\n";
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

		pmcs(name)= xpm_color(def);
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

/******************************************************************************
 * main cocoa renderer
 ******************************************************************************/


aqua_renderer_rep*
the_aqua_renderer () {
  static aqua_renderer_rep* the_renderer= NULL;
	if (!the_renderer) the_renderer= tm_new <aqua_renderer_rep> ();
	return the_renderer;
}



/******************************************************************************
 * Shadow management methods
 ******************************************************************************/

/* Shadows are auxiliary renderers which allow double buffering and caching of
 * graphics. TeXmacs has explicit double buffering from the X11 port. Maybe
 * it would be better to design a better API abstracting from the low level
 * details but for the moment the following code and the aqua_proxy_renderer_rep
 * is designed to avoid double buffering : when texmacs asks
 * a aqua_renderer_rep for a shadow it is given a proxy of the original renderer
 * texmacs uses this shadow for double buffering and the proxy will simply
 * forward the drawing operations to the original surface and neglect all the
 * syncronization operations
 *
 * to solve the second problem we do not draw directly on screen in QTMWidget.
 * Instead we maintain an internal pixmap which represents the state of the pixels
 * according to texmacs. When we are asked to initialize a aqua_shadow_renderer_rep
 * we simply read the pixels form this backing store. At the Qt level then
 * (in QTMWidget) we make sure that the state of the backing store is in sync
 * with the screen via paintEvent/repaint mechanism.
 *
 */

class aqua_shadow_renderer_rep: public aqua_renderer_rep {
public:
    NSBitmapImageRep *px;
    aqua_renderer_rep *master;
    
public:
    aqua_shadow_renderer_rep (int _w, int _h);
    ~aqua_shadow_renderer_rep ();
    
    void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

class aqua_proxy_renderer_rep: public aqua_renderer_rep {
public:
    aqua_renderer_rep *base;
    
public:
    aqua_proxy_renderer_rep (aqua_renderer_rep *_base)
    : aqua_renderer_rep(w, h), base(_base) { }
    ~aqua_proxy_renderer_rep () { };
    
    void new_shadow (renderer& ren);
    void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};


void
aqua_renderer_rep::new_shadow (renderer& ren) {
    SI mw, mh, sw, sh;
    get_extents (mw, mh);
    if (ren != NULL) {
        ren->get_extents (sw, sh);
        if (sw != mw || sh != mh) {
            delete_shadow (ren);
            ren= NULL;
        } else
            static_cast<aqua_shadow_renderer_rep*>(ren)->end();
        // cout << "Old: " << sw << ", " << sh << "\n";
    }

    if (ren == NULL)  ren= (renderer) tm_new<aqua_proxy_renderer_rep> (this);
  
    if (ren) static_cast<aqua_renderer_rep*>(ren)->begin(context);

    // cout << "Create " << mw << ", " << mh << "\n";
}

void
aqua_renderer_rep::delete_shadow (renderer& ren)  {
    if (ren != NULL) {
        tm_delete (ren);
        ren= NULL;
    }
}

void
aqua_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
    // FIXME: we should use the routine fetch later
    ASSERT (ren != NULL, "invalid renderer");
    if (ren->is_printer ()) return;
    aqua_renderer_rep* shadow= static_cast<aqua_renderer_rep*>(ren);
    outer_round (x1, y1, x2, y2);
    x1= max (x1, cx1- ox);
    y1= max (y1, cy1- oy);
    x2= min (x2, cx2- ox);
    y2= min (y2, cy2- oy);
    shadow->ox= ox;
    shadow->oy= oy;
    shadow->master= this;
    shadow->cx1= x1+ ox;
    shadow->cy1= y1+ oy;
    shadow->cx2= x2+ ox;
    shadow->cy2= y2+ oy;
    
    decode (x1, y1);
    decode (x2, y2);
    if (x1<x2 && y2<y1) {
        NSRect rect = NSMakeRect(x1, y2, x2-x1, y1-y2);
        //    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);
        NSGraphicsContext *old_context = [[NSGraphicsContext currentContext] retain];
        [NSGraphicsContext setCurrentContext:context];
        NSBezierPath* clipPath = [NSBezierPath bezierPath];
        [clipPath appendBezierPathWithRect: rect];
        [clipPath setClip];
        [NSGraphicsContext setCurrentContext:old_context]; [old_context release];
        //    shadow->painter->drawPixmap (rect, px, rect);
        //    cout << "aqua_shadow_renderer_rep::get_shadow "
        //         << rectangle(x1,y2,x2,y1) << LF;
        //  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
    } else {
//        shadow->painter->setClipRect(QRect());
    }
}

void
aqua_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
    // FIXME: we should use the routine fetch later
    ASSERT (ren != NULL, "invalid renderer");
    if (ren->is_printer ()) return;
    if (context == static_cast<aqua_renderer_rep*>(ren)->context) return;
    aqua_shadow_renderer_rep* shadow= static_cast<aqua_shadow_renderer_rep*>(ren);
    outer_round (x1, y1, x2, y2);
    x1= max (x1, cx1- ox);
    y1= max (y1, cy1- oy);
    x2= min (x2, cx2- ox);
    y2= min (y2, cy2- oy);
    decode (x1, y1);
    decode (x2, y2);
    if (x1<x2 && y2<y1) {
        NSRect rect = NSMakeRect(x1, y2, x2-x1, y1-y2);
        //    cout << "aqua_shadow_renderer_rep::put_shadow "
        //         << rectangle(x1,y2,x2,y1) << LF;
        //    painter->setCompositionMode(QPainter::CompositionMode_Source);
        NSGraphicsContext *old_context = [[NSGraphicsContext currentContext] retain];
        [NSGraphicsContext setCurrentContext:context];
        [shadow->px drawInRect: rect];
        [NSGraphicsContext setCurrentContext:old_context]; [old_context release];
//        painter->drawPixmap (rect, shadow->px, rect);
        //  XCopyArea (dpy, shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
    }
}


void
aqua_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2)  {
    if (master == NULL) return;
    if (context == static_cast<aqua_renderer_rep*>(master)->context) return;
    outer_round (x1, y1, x2, y2);
    decode (x1, y1);
    decode (x2, y2);
    static_cast<aqua_renderer_rep*>(master)->encode (x1, y1);
    static_cast<aqua_renderer_rep*>(master)->encode (x2, y2);
    master->put_shadow (this, x1, y1, x2, y2);
}


/******************************************************************************
 * proxy qt renderer
 ******************************************************************************/

void
aqua_proxy_renderer_rep::new_shadow (renderer& ren) {
    SI mw, mh, sw, sh;
    get_extents (mw, mh);
    if (ren != NULL) {
        ren->get_extents (sw, sh);
        if (sw != mw || sh != mh) {
            delete_shadow (ren);
            ren= NULL;
        }
        else
            static_cast<aqua_renderer_rep*>(ren)->end();
        // cout << "Old: " << sw << ", " << sh << "\n";
    }
    if (ren == NULL) {
//        NSBitmapImageRep *img = [[NSBitmapImageRep alloc] init];
//        ren= (renderer) tm_new<aqua_shadow_renderer_rep> (QPixmap (mw, mh));
        ren= (renderer) tm_new<aqua_shadow_renderer_rep> (mw, mh);
    }
    
    // cout << "Create " << mw << ", " << mh << "\n";
    static_cast<aqua_renderer_rep*>(ren)->begin(context);
}

void
aqua_proxy_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
    // FIXME: we should use the routine fetch later
    ASSERT (ren != NULL, "invalid renderer");
    if (ren->is_printer ()) return;
    aqua_renderer_rep* shadow= static_cast<aqua_renderer_rep*>(ren);
    outer_round (x1, y1, x2, y2);
    x1= max (x1, cx1- ox);
    y1= max (y1, cy1- oy);
    x2= min (x2, cx2- ox);
    y2= min (y2, cy2- oy);
    shadow->ox= ox;
    shadow->oy= oy;
    shadow->cx1= x1+ ox;
    shadow->cy1= y1+ oy;
    shadow->cx2= x2+ ox;
    shadow->cy2= y2+ oy;
    shadow->master= this;
    decode (x1, y1);
    decode (x2, y2);
    
    NSGraphicsContext *old_context = [[NSGraphicsContext currentContext] retain];
    [NSGraphicsContext setCurrentContext:shadow->context];
    if (x1<x2 && y2<y1) {
        NSRect rect = NSMakeRect(x1, y2, x2-x1, y1-y2);

        
        NSBezierPath* clipPath = [NSBezierPath bezierPath];
        [clipPath appendBezierPathWithRect: rect];
        [clipPath setClip];

        
//        shadow->painter->setClipRect(rect);
        
        //    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);
        NSBitmapImageRep *img = [view bitmapImageRepForCachingDisplayInRect:rect];
//        QPixmap *_pixmap = static_cast<QPixmap*>(painter->device());
        if (img) {
            [img drawInRect:rect];
//            shadow->painter->drawPixmap (rect, *_pixmap, rect);
        }
        //    cout << "aqua_shadow_renderer_rep::get_shadow "
        //         << rectangle(x1,y2,x2,y1) << LF;
        //  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);

    } else {
        //shadow->painter->setClipRect(QRect());
    }
    [NSGraphicsContext setCurrentContext:old_context]; [old_context release];
    
}


/******************************************************************************
 * shadow qt renderer
 ******************************************************************************/

aqua_shadow_renderer_rep::aqua_shadow_renderer_rep (int _w, int _h)
// : aqua_renderer_rep (_px.width(),_px.height()), px(_px)
: aqua_renderer_rep (_w, _h), px(nil)
{
    px = [[NSBitmapImageRep alloc] init];
    [px setSize: NSMakeSize(w,h)];
    context = [[NSGraphicsContext graphicsContextWithBitmapImageRep: px] retain];
    //cout << px.width() << "," << px.height() << " " << LF;
    // painter->begin(&px);
}

aqua_shadow_renderer_rep::~aqua_shadow_renderer_rep ()
{
  [px release];
  [context release];
  context  = nil;
}

void
aqua_shadow_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
    // FIXME: we should use the routine fetch later
    ASSERT (ren != NULL, "invalid renderer");
    if (ren->is_printer ()) return;
    aqua_shadow_renderer_rep* shadow= static_cast<aqua_shadow_renderer_rep*>(ren);
    outer_round (x1, y1, x2, y2);
    x1= max (x1, cx1- ox);
    y1= max (y1, cy1- oy);
    x2= min (x2, cx2- ox);
    y2= min (y2, cy2- oy);
    shadow->ox= ox;
    shadow->oy= oy;
    shadow->cx1= x1+ ox;
    shadow->cy1= y1+ oy;
    shadow->cx2= x2+ ox;
    shadow->cy2= y2+ oy;
    shadow->master= this;
    decode (x1, y1);
    decode (x2, y2);
    NSGraphicsContext *old_context = [[NSGraphicsContext currentContext] retain];
    [NSGraphicsContext setCurrentContext:shadow->context];
    if (x1<x2 && y2<y1) {
        NSRect rect = NSMakeRect(x1, y2, x2-x1, y1-y2);
        
        NSBezierPath* clipPath = [NSBezierPath bezierPath];
        [clipPath appendBezierPathWithRect: rect];
        [clipPath setClip];

//        shadow->painter->setClipRect(rect);
        [px drawInRect:rect];
        
        //    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);   
//        shadow->painter->drawPixmap (rect, px, rect);
        //    cout << "aqua_shadow_renderer_rep::get_shadow " 
        //         << rectangle(x1,y2,x2,y1) << LF;
        //  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
    } else {
  //      shadow->painter->setClipRect(QRect());
    }
    [NSGraphicsContext setCurrentContext:old_context]; [old_context release];

}
