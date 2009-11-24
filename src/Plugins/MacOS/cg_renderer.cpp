
/******************************************************************************
* MODULE     : cg_renderer.cpp
* DESCRIPTION: CoreGraphics drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "cg_renderer.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "gui.hpp" // for INTERRUPT_EVENT, INTERRUPTED_EVENT
#include "font.hpp" // for the definition of font

#include "Freetype/tt_file.hpp" // tt_font_find

#include "mac_images.h"

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
 * Global support variables for all cg_renderers
 ******************************************************************************/


static hashmap<basic_character,cg_image> character_image;  // bitmaps of all characters
static hashmap<string,cg_image> images; 



/******************************************************************************
 * cg_renderer
 ******************************************************************************/

void 
cg_set_color (CGContextRef cxt, color col) {
  int r, g, b;
  get_rgb_color(col,r,g,b);
  CGContextSetRGBFillColor(cxt, r/255.0, g/255.0, b/255.0, 1.0);
  CGContextSetRGBStrokeColor(cxt, r/255.0, g/255.0, b/255.0, 1.0);
}

cg_renderer_rep::cg_renderer_rep (int w2, int h2):
 basic_renderer_rep(w2,h2), context(NULL)
{
}

cg_renderer_rep::~cg_renderer_rep () {
  if (context) end();
}


void 
cg_renderer_rep::begin (void * c) { 
  context = (CGContextRef)c; 
  CGContextRetain(context);
  CGContextBeginPage(context, NULL);
}

void 
cg_renderer_rep::end () { 
  CGContextEndPage(context);
  CGContextRelease(context); 
  context = NULL;  
}

void 
cg_renderer_rep::next_page () { 
  CGContextEndPage(context);
  CGContextBeginPage(context, NULL);
}

void
cg_renderer_rep::set_color (color c) {
  basic_renderer_rep::set_color(c);
  cg_set_color(context,cur_fg);
}

void
cg_renderer_rep::set_line_style (SI lw, int type, bool round) {
  (void) type;
	
  CGContextSetLineCap(context, round? kCGLineCapRound : kCGLineCapSquare);
  CGContextSetLineJoin(context, kCGLineJoinRound);
  CGContextSetLineWidth(context, lw <= pixel ? 1 : ((lw+thicken) / (1.0*pixel)));
}

void
cg_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  // y1--; y2--; // top-left origin to bottom-left origin conversion
  CGContextSetShouldAntialias(context, true);
  CGPoint points[2]= { CGPointMake(x1,y1), CGPointMake(x2,y2) };
  CGContextStrokeLineSegments(context, points, 2);
}

void
cg_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, CGPoint, n);
  CGContextSetShouldAntialias(context, true);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i] = CGPointMake(xx,yy);
    if (i>0) {
		CGContextStrokeLineSegments(context, pnt + (i - 1), 2); // FIX: hack
    }
  }
  STACK_DELETE_ARRAY (pnt);
}

void
cg_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
  cg_set_color (context, cur_bg);
  CGContextSetShouldAntialias(context, false);
  CGContextFillRect(context, CGRectMake(x1, y2, x2-x1, y1-y2) );
  cg_set_color (context, cur_fg);
}

void
cg_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
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

 // cg_set_color (context, cur_fg);
  CGContextSetShouldAntialias (context, false);
  CGContextFillRect (context, CGRectMake(x1, y2, x2-x1, y1-y2) );
}

void
cg_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
cg_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
cg_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
	
  CGContextBeginPath(context);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
	if (i==0) CGContextMoveToPoint (context, xx, yy);
	else  CGContextAddLineToPoint(context, xx ,yy);
  }
  CGContextClosePath (context);
//  cg_set_color (context, cur_fg);
  CGContextSetShouldAntialias (context, true);
  if (convex)    CGContextEOFillPath (context);	
  else CGContextFillPath (context);	
}


/******************************************************************************
* Image rendering
******************************************************************************/
struct cg_cache_image_rep: cache_image_element_rep {
	cg_cache_image_rep (int w2, int h2, time_t time2, CGImageRef ptr2) :
    cache_image_element_rep(w2,h2,time2,ptr2) {  CGImageRetain((CGImageRef)ptr); };
	virtual ~cg_cache_image_rep() { CGImageRelease((CGImageRef)ptr); };
};

void
cg_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
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
  
  CGImageRef pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
  << as_string (cx1) << as_string (cy1)
  << as_string (cx2) << as_string (cy2) << "cg-image" ;
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci)) {
    pm = static_cast<CGImageRef> (ci->ptr);
  } else {
	  if (suffix (u) == "png") {
      // rendering
      string suu = as_string (u);
      char * buf = as_charp(suu); 
      // cout << suu << LF;
      CFURLRef uu =  CFURLCreateFromFileSystemRepresentation(NULL, (UInt8*)buf, N(suu),  false);
      tm_delete (buf);
      CGImageSourceRef source =  CGImageSourceCreateWithURL ( uu, NULL );
      pm =  CGImageSourceCreateImageAtIndex(source, 0, NULL);
      CFRelease(source);
      CFRelease(uu);
	  } else if (suffix (u) == "ps" ||
               suffix (u) == "eps" ||
               suffix (u) == "pdf") {
      url temp= url_temp (".png");
//      system ("convert", u, temp);
      mac_image_to_png (u, temp); 
      string suu = as_string (temp);
      char * buf = as_charp(suu); 
      //cout << suu << LF;
      CFURLRef uu =  CFURLCreateFromFileSystemRepresentation(NULL, (UInt8*)buf, N(suu),  false);
      tm_delete (buf);
      CGImageSourceRef source =  CGImageSourceCreateWithURL ( uu, NULL );
      pm =  CGImageSourceCreateImageAtIndex(source, 0, NULL);
      CFRelease(source);
      CFRelease(uu);
      remove (temp);
    }

    if (pm == NULL ) {
      cout << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
      return;
    }
    // caching
    ci = tm_new <cg_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }
  
	CGContextSetShouldAntialias(context, false);
	CGContextSaveGState(context);
	CGContextTranslateCTM(context, x,y);
	CGContextScaleCTM(context,1.0,-1.0);
	CGContextDrawImage(context, CGRectMake(0, 0, w, h), pm); 
	CGContextRestoreGState(context);
}



void
cg_renderer_rep::draw_clipped (CGImageRef im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
       // clear(x1,y1,x2,y2);
  CGContextSetShouldAntialias(context, true);
//  CGContextSetBlendMode(context,kCGBlendModeSourceAtop);
  CGContextDrawImage(context, CGRectMake(x,y,w,h), im); 
}  




static hashmap<string,pointer> native_fonts;
static hashset<string> native_loaded;

int 
posixStringToFSSpec(FSSpec *fss, CFStringRef posixPath, bool isDirectory)  {
	FSRef fsRef;
	FSSpec fileSpec;
	// create a URL from the posix path:
	CFURLRef url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault,posixPath,kCFURLPOSIXPathStyle,isDirectory);
	// check to be sure the URL was created properly:
	if (url == 0) {
		//fprintf(stderr,"Can't get URL");
		return(1);
	}
	// use the CF function to extract an FSRef from the URL:
	if (CFURLGetFSRef(url, &fsRef) == 0){
		//fprintf(stderr,"Can't get FSRef.\n");
		CFRelease(url);
		return(1);
	}
	// use Carbon call to get the FSSpec from the FSRef
	if (FSGetCatalogInfo (&fsRef, kFSCatInfoNone, 0, 0, &fileSpec, 0) != noErr) {		
		//fprintf(stderr,"Can't get FSSpec.\n");
		CFRelease(url);
		return(1);
	}
	// We have a valid FSSpec! Clean up and return it:
	CFRelease(url);
	*fss = fileSpec;
	return 0;
}



bool 
cg_renderer_rep::native_draw (int ch, font_glyphs fn, SI x, SI y) {
	string name= fn->res_name;
	unsigned char c= ch;
	if (ch >= 256) {
		name= name * "-" * as_string (ch / 256);
		c= (unsigned char) (ch & 255);
	}
	
	//	cout << name << LF;
	int size;
	{
		// find size (weird)
		int    pos1  = search_forwards (".", name);
		int pos2= search_backwards (":", name);
		string sz = name(pos2+1,pos1);
		size = as_int(sz);
	}
	CGFontRef f = (CGFontRef)native_fonts(name);
	
	if ((f == NULL)&&(! native_loaded->contains(name))) {
		native_loaded->insert(name);
		string ttf;
		int    pos  = search_forwards (".", name);
		string root = (pos==-1? name: name (0, pos));
		if ((pos!=-1) && ends (name, "tt")) {
			int pos2= search_backwards (":", name);
			root= name (0, pos2);
			url u= tt_font_find (root);
			if (suffix (u) == "pfb") {
//		  cout << u << LF;
        url v= url_temp (".otf");
				string vs = concretize(v);
				system ("/Users/mgubi/t/t1wrap/T1Wrap " * concretize(u) * " > " * vs);
				FSSpec fss;
				ATSFontRef atsFont;
				ATSFontContainerRef container;
				char *p = as_charp(vs);
				CFStringRef font_filename = CFStringCreateWithCString(NULL,p,kCFStringEncodingASCII);
					
				if (posixStringToFSSpec(&fss,font_filename,false)) {
					cout << "Cannot load font" << vs << LF;
				} else {
					int status =  ATSFontActivateFromFileSpecification(&fss,kATSFontContextLocal,kATSFontFormatUnspecified,NULL,NULL,&container);
					cout << "Font " << vs << " loaded" << LF;
					ItemCount count;
					status = ATSFontFindFromContainer(container, 0, 1, &atsFont, &count);
						
					f = CGFontCreateWithPlatformFont((void*)&atsFont);
					native_fonts(name) = f;
				}
				tm_delete (p);
				CFRelease(font_filename);
				remove (v);
      }
		}
	} // end caching
	
	if (f) {
		decode (x , y );
		y--; // top-left origin to bottom-left origin conversion
    CGContextRef cgc = context;
		CGContextSetFont(cgc,f);
		CGContextSetFontSize(cgc,size);
		CGAffineTransform	kHorizontalMatrix = { PIXEL*600.0/(pixel*72.0),  0.0,  0.0,  -PIXEL*600.0/(pixel*72.0),  0.0,  0.0 };
		CGContextSetTextMatrix(cgc, kHorizontalMatrix);
		CGContextSetTextDrawingMode(cgc,  kCGTextFill);
		CGContextSetShouldAntialias(cgc,true);
		CGContextSetShouldSmoothFonts(cgc,true);
		//	 CGContextSetBlendMode(context,kCGBlendModeSourceAtop);
  //  cg_set_color (context, cur_fg);
		CGGlyph buf[1] = {c};
		CGContextShowGlyphsAtPoint(cgc,x,y,(CGGlyph*)buf,1);
	} 
	return true;
}


CGContextRef 
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
cg_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
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
    (void) w; (void) h;
    int x1= x- mi->xo*sfactor;
    int y1=  y+ mi->yo*sfactor;
    decode (x1, y1);
    y1--; // top-left origin to bottom-left origin conversion
    CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
    CGContextSetShouldAntialias (context, true);
    CGContextSaveGState (context);
  //  cg_set_color (context, cur_fg);
    CGContextClipToMask (context, r, mi->img); 
    CGContextFillRect (context, r);
    CGContextRestoreGState (context);
  }  
}

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/



static CGImageRef xpm_init(url file_name)
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
	CGContextRef ic = MyCreateBitmapContext(w,h);
	CGContextSetBlendMode(ic,kCGBlendModeCopy);
	// setup pixmap
	for (y=0; y<h; y++) {
		if (N(t)< (y+c+1)) s= "";
		else s= as_string (t[y+c+1]);
		for (x=0; x<w; x++) {
			string name;
			if (N(s)<(b*(x+1))) name= first_name;
			else name= s (b*x, b*(x+1));
			color col = pmcs[(pmcs->contains (name) ? name : first_name)];
      cg_set_color (ic, col);
			CGContextFillRect (ic,CGRectMake(x,y,1,1));
		}
	}
	CGImageRef im = CGBitmapContextCreateImage (ic);
	CGContextRelease (ic);
	return im;
}



extern int char_clip;

CGImageRef 
cg_renderer_rep::xpm_image (url file_name) { 
  CGImageRef pxm= NULL;
  cg_image mi= images [as_string (file_name)];
  if (is_nil (mi)) {    
	  pxm = xpm_init(file_name);
    cg_image mi2 (pxm, 0, 0, CGImageGetWidth (pxm), CGImageGetHeight (pxm));
    mi= mi2;
    images (as_string (file_name))= mi2;
    CGImageRelease(pxm);
  } else pxm= mi->img;
  return pxm;
}

void
cg_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  CGImageRef image = xpm_image (file_name);
  ASSERT (sfactor == 1, "shrinking factor should be 1");
  int w = CGImageGetWidth(image);
  int h = CGImageGetHeight(image);
  int old_clip= char_clip;
  char_clip = true;
  draw_clipped (image, w, h, x, y);
  char_clip = old_clip;
}

/******************************************************************************
 * main coregraphics renderer
 ******************************************************************************/

static cg_renderer_rep* the_renderer= NULL;

cg_renderer_rep*
the_cg_renderer () {
	if (!the_renderer) the_renderer= tm_new <cg_renderer_rep> ();
	return the_renderer;
}
