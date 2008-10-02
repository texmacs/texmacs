
/******************************************************************************
* MODULE     : qt_renderer.cpp
* DESCRIPTION: QT drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "qt_renderer.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "qt_rgb.hpp"
#include "qt_utilities.hpp"
#include "file.hpp"
#include "iterator.hpp"




/******************************************************************************
* Aqua images
******************************************************************************/


qt_image::qt_image (QTMImage * img2, SI xo2, SI yo2, int w2, int h2) :
  rep (new qt_image_rep(img2,xo2,yo2,w2,h2)) {}
//qt_image::qt_image () : rep(NULL) {}

qt_image_rep::qt_image_rep (QTMImage * img2, SI xo2, SI yo2, int w2, int h2) :
  img(img2), xo(xo2), yo(yo2), w(w2), h(h2) {}

qt_image_rep::~qt_image_rep() { delete img; }

/******************************************************************************/

qt_renderer_rep::qt_renderer_rep (qt_gui dis2, int w2, int h2)
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

qt_renderer_rep::~qt_renderer_rep () {} ;


/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
qt_renderer_rep::encode (SI& x, SI& y) {
  x= (x*pixel) - ox;
  y= ((-y)*pixel) - oy;
}

void
qt_renderer_rep::decode (SI& x, SI& y) {
  x += ox; y += oy;
  if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
  if (y>=0) y= -(y/pixel); else y= -((y-pixel+1)/pixel);
}

/******************************************************************************/

void qt_renderer_rep::get_extents (int& w2, int& h2) { w2 = w; h2 = h; } 
//bool qt_renderer_rep::interrupted (bool check) { return false; } 
bool
qt_renderer_rep::interrupted (bool check) {
	return false;
  bool ret = dis->check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
  if (ret)
    if (DEBUG_EVENTS)
      cout << "INTERRUPTED:" << ret << "\n";
  return ret;
}


/* routines from renderer.hpp **********************************************/

/******************************************************************************
* Drawing into drawables
******************************************************************************/

color
qt_renderer_rep::rgb (int r, int g, int b) {
  return rgb_color (r, g, b);
}

void
qt_renderer_rep::get_rgb (color col, int& r, int& g, int& b) {
  get_rgb_color (col, r, g, b);
}

color
qt_renderer_rep::get_color () {
  return cur_fg;
}

#if 0
color
qt_renderer_rep::get_color (string s) {
  return named_color (s);
}
#endif

color
qt_renderer_rep::get_background () {
  return cur_bg;
}

void
qt_renderer_rep::set_color (color c) {
	QPen p(painter.pen());
	p.setColor(dis->cmap[c]);
	painter.setPen(p);
  cur_fg= c;
}

void
qt_renderer_rep::set_background (color c) {
//  XSetBackground (dpy, gc, dis->cmap[c]);
  cur_bg= c;
}

void
qt_renderer_rep::set_line_style (SI lw, int type, bool round) { (void) type;
	QPen p(painter.pen());
  if (lw <= pixel)
  {
		p.setWidth(0);
  }
  else
  {
		p.setWidth((lw+thicken)/(1.0*pixel));
  }
	p.setCapStyle(round ? Qt::RoundCap : Qt::SquareCap);
	p.setJoinStyle(Qt::RoundJoin);
	painter.setPen(p);
}

void
qt_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  y1--; y2--; // top-left origin to bottom-left origin conversion
	painter.drawLine(x1,y1,x2,y2);
}


void
qt_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, QPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i].rx()= xx;
    pnt[i].ry()= yy;
    if (i>0) painter.drawLine(pnt[i-1],pnt[i]); // FIX: hack
  }
 // XDrawLines (dpy, win, gc, pnt, n, CoordModeOrigin);
  STACK_DELETE_ARRAY (pnt);
}


void
qt_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
	  if ((x1>=x2) || (y1<=y2)) return;

	
	QBrush brush(dis->cmap[cur_bg]);
//	QPen p(painter.pen());
//	p.setColor(dis->cmap[cur_bg]);
//	painter.setPen(p);
	painter.fillRect(x1,y2,x2-x1,y1-y2,brush);	
//	p.setColor(dis->cmap[cur_fg]);
//	painter.setPen(p);
	
}

void
qt_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
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
	
	QBrush brush(dis->cmap[cur_fg]);

	painter.fillRect(x1,y2,x2-x1,y1-y2,brush);	
}

void
qt_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XDrawArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
qt_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  //FIXME: XFillArc (dpy, win, gc, x1, y2, x2-x1, y1-y2, alpha, delta);
}

void
qt_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, QPoint, n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    pnt[i].rx() = xx;
    pnt[i].ry() = yy;
  }
  painter.drawPolygon(pnt,n,convex? Qt::OddEvenFill : Qt::WindingFill);
  STACK_DELETE_ARRAY (pnt);
}


/******************************************************************************
 * Image rendering
 ******************************************************************************/


static int cache_image_last_gc = 0;
static int cache_image_tot_size= 0;
static int cache_image_max_size= 10000;
static hashmap<tree,QImage*> cache_image (0);
static hashmap<tree,int> cache_image_w (0);
static hashmap<tree,int> cache_image_h (0);
static hashmap<tree,int> cache_image_time (0);
static hashmap<tree,int> cache_image_nr (0);

// to inform texmacs about image sizes we need to fill this structure
// see System/Files/image_files.cpp

extern hashmap<tree,string> ps_bbox; 



void image_auto_gc () {
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
      QImage  *pm= (QImage*) cache_image [lookup];
      delete pm;
      cache_image->reset (lookup);
      cache_image_w->reset (lookup);
      cache_image_h->reset (lookup);
      cache_image_time->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}

void image_gc (string name) {
  (void) name;
  cache_image_last_gc= texmacs_time ();
  iterator<tree> it= iterate (cache_image);
  while (it->busy()) {
    tree lookup= it->next();
    if (!is_ramdisc (as_url (lookup[0]))) {
      QImage *pm= (QImage*) cache_image [lookup];
      delete pm;
      cache_image->reset (lookup);
      cache_image_w->reset (lookup);
      cache_image_h->reset (lookup);
      cache_image_time->reset (lookup);
      cache_image_nr->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}






void qt_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
                              double cx1, double cy1, double cx2, double cy2) 
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)

 // if (DEBUG_EVENTS) cout << "qt_renderer_rep::image " << as_string(u) << LF;

  w= w/pixel; h= h/pixel;
  decode (x, y);
  
  
  //painter.drawRect(QRect(x,y-h,w,h));
  
  QImage *pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
  << as_string (cx1) << as_string (cy1)
  << as_string (cx2) << as_string (cy2);
  if (cache_image->contains (lookup)) pm= (QImage*) cache_image [lookup];
  else {
    // rendering
    if (qt_supports_image (u)) {
      pm = new QImage(to_qstring(as_string(u)));
      if (pm->isNull()) {
        cout << "Error reading image file " << as_string(u) << LF;
        delete pm;
        return;
      }
    }
    else {
      //XSetForeground (dpy, gc, white);
      //XFillRectangle (dpy, pm, gc, 0, 0, w, h);
//      ghostscript_run (dpy, gs_win, pm, u, w, h, cx1, cy1, cx2, cy2);
      if (DEBUG_EVENTS)  painter.drawRect(QRect(x,y-h,w,h));
      if (DEBUG_EVENTS)  cout << "HERE WE MUST SOMEHOW RUN GHOSTSCRIPT \n";
      return;
    }
    
    // caching
    if (N(cache_image_nr) == 0) cache_image_last_gc= texmacs_time ();
    cache_image      (lookup)=  pm;
    cache_image_w    (lookup)= w;
    cache_image_h    (lookup)= h;
    cache_image_time (lookup)= texmacs_time ();
    cache_image_nr   (lookup)= cache_image_nr [lookup] + 1;
    cache_image_tot_size += w*h;
    if (cache_image_tot_size > cache_image_max_size) {
      image_auto_gc ();
      if (cache_image_tot_size > cache_image_max_size)
        cache_image_max_size= cache_image_tot_size << 1;
    }
  }
  
  int iw= pm->width ();
  int ih= pm->height ();
  int x1= as_int (cx1 * iw);
  int y1= as_int (cy1 * ih);
  int x2= as_int (cx2 * iw);
  int y2= as_int (cy2 * ih);
  int ww= x2 - x1;
  int hh= y2 - y1;

	painter.drawImage(QRect(x,y-h,w,h),*pm, QRect(x1,hh-y2,ww,hh));
  

};



int char_clip=0;

#define conv(x) ((SI) (((double) (x))*(fn->unit)))

void
qt_renderer_rep::draw_clipped (QTMImage *im, int w, int h, SI x, SI y) {
	(void) w; (void) h;
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
	//clear(x1,y1,x2,y2);
#ifdef Q_WS_MAC
  painter.drawPixmap(x,y,w,h,*im);
#else
  painter.drawImage(x,y,*im);
#endif
//  [im drawAtPoint:NSMakePoint(x,y) fromRect:NSMakeRect(0,0,w,h) operation:NSCompositeSourceAtop fraction:1.0];
}  

void qt_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  x_character xc (c, fng, sfactor, cur_fg, 0);
  qt_image mi = dis->character_image [xc];
  if (is_nil(mi)) {
    int r, g, b;
    get_rgb (cur_fg, r, g, b);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
    int i, j, w= gl->width, h= gl->height;
#ifdef Q_WS_MAC
	  QTMImage *im = new QPixmap(w,h);
	  {
		  int nr_cols= sfactor*sfactor;
		  if (nr_cols >= 64) nr_cols= 64;
		  
		  QPainter pp(im);
		  QPen pen(painter.pen());
		  QBrush brush(pen.color());	
		  pp.setPen(Qt::NoPen);
		  im->fill (QColor (0,0,0,0));
		  for (j=0; j<h; j++)
			  for (i=0; i<w; i++) {
				  int col = gl->get_x(i,j);
				  brush.setColor(QColor(r,g,b,(255*col)/(nr_cols+1)));		
				  pp.fillRect(i,j,1,1,brush);
			  }
	  }
#else
    QTMImage *im = new QImage(w,h,QImage::Format_ARGB32_Premultiplied);
    //if (! (im->hasAlphaChannel())) cout << "WARNING NO ALPHA CHANNEL\n"; 
    {
      int nr_cols= sfactor*sfactor;
      if (nr_cols >= 64) nr_cols= 64;

      QPainter pp(im);
      QPen pen(painter.pen());
      QBrush brush(pen.color());	
      pp.setPen(Qt::NoPen);
      im->fill (qRgba (0,0,0,0));
      for (j=0; j<h; j++)
	for (i=0; i<w; i++) {
	  int col = gl->get_x(i,j);
	  //brush.setColor(QColor(r,g,b,(255*col)/(nr_cols+1)));		
	  //pp.fillRect(i,j,1,1,brush);
	  im->setPixel(i,j,qRgba(r,g,b,(255*col)/(nr_cols+1)));
	}
    }
#endif
    qt_image mi2(im, xo, yo, w, h );
    mi = mi2;
    //[im release]; // qt_image retains im
    dis->character_image (xc)= mi;
    // FIXME: we must release the image at some point (this should be ok now, see qt_image)
  }
  
  // draw the character
  draw_clipped (mi->img, mi->w, mi->h, x- mi->xo*sfactor, y+ mi->yo*sfactor);
}

#undef conv

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

QColor xpm_to_ns_color(string s)
{
  if (s == "none") return QColor(100,100,100);
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return QColor(r,g,b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return QColor(r,g,b);
  }
  if ((N(s) == 13) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 5));
    int g= from_hexadecimal (s (5, 9));
    int b= from_hexadecimal (s (9, 13));
    return QColor(r,g,b);
  }
  char *name = as_charp(s);
  for(int i = 0; i<RGBColorsSize; i++) {
   if (strcmp(name,RGBColors[i].name)==0) {
	 delete [] name;
		 return QColor(RGBColors[i].r,RGBColors[i].g,RGBColors[i].b);
   }
  }
  delete  [] name;
  return QColor(0,0,0);
}



extern int char_clip;

QTMImage *qt_renderer_rep::xpm_image(url file_name)
{ 
	QTMImage *pxm = NULL;
  qt_image mi = dis->images [as_string(file_name)];
  if (is_nil(mi)) {    
    string sss;
  load_string ("$TEXMACS_PIXMAP_PATH" * file_name, sss, false);
  if (sss == "") load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", sss, true);
        uchar *buf = (uchar*)as_charp(sss);
	  pxm = new QTMImage();
	  pxm->loadFromData(buf, N(sss));
		delete buf;
	//	cout << sss;
	//cout << "pxm: " << file_name << "(" << pxm->size().width() << "," <<  pxm->size().height() << ")\n";
	qt_image mi2(pxm,0,0,pxm->width(),pxm->height());
		mi = mi2;
		dis->images(as_string(file_name)) = mi2; 	
  }  
  else pxm = mi->img;
	return pxm;
}

void qt_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  
 // char *chstr = as_charp(as_string(file_name));
//  NSString *name = [NSString stringWithCString:chstr];
 // delete [] chstr;
//  name = [[name stringByDeletingPathExtension] stringByAppendingPathExtension:@"png"];
  ///name = [name stringByDeletingPathExtension];
  QTMImage *image = xpm_image(file_name);
  
  if (sfactor != 1)
    fatal_error ("Shrinking factor should be 1", "qt_renderer_rep::xpm");
  int w, h;
  w = image->width();
	h = image->height();

//  [(NSImageRep*)[[image representations] objectAtIndex:0]  drawAtPoint:NSMakePoint(x,y)];
  
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}



/* clipping */
//void qt_renderer_rep::get_clipping (SI &x1, SI &y1, SI &x2, SI &y2) {} ;
//void qt_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {} ;

void
qt_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
//	[NSBezierPath clipRect:NSMakeRect(x1,y2,x2-x1,y1-y2)];
	//	[NSBezierPath clipRect:NSMakeRect(x1,y2,x2-x1,y1-y2)];
}



/* shadowing and copying rectangular regions across devices */


void qt_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer dev, SI x, SI y) 
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; (void) x; (void) y;
} ;
void qt_renderer_rep::new_shadow (renderer& dev) { dev =  this; } ;
void qt_renderer_rep::delete_shadow (renderer& dev) {     dev= NULL; } ;
void qt_renderer_rep::get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) 
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev;
} ;
void qt_renderer_rep::put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) 
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev;
} ;
void qt_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) 
{
  (void) x1; (void) y1; (void) x2; (void) y2;
} ;

#if 1
font x_font (string family, int size, int dpi)
{
  (void) family; (void) size; (void) dpi;
  if (DEBUG_EVENTS) cout << "x_font(): SHOULD NOT BE CALLED\n";
  return NULL;
}
#endif


