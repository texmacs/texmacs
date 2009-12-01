
/******************************************************************************
* MODULE     : qt_renderer.cpp
* DESCRIPTION: QT drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_renderer.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "qt_utilities.hpp"
#include "file.hpp"

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_images.h"
#endif

#include <QObject>
#include <QWidget>
#include <QPaintDevice>

/******************************************************************************
* Qt images
******************************************************************************/

struct qt_image_rep: concrete_struct {
  QTMImage *img;
  SI xo,yo;
  int w,h;
  qt_image_rep (QTMImage* img2, SI xo2, SI yo2, int w2, int h2):
    img (img2), xo (xo2), yo (yo2), w (w2), h (h2) {};
  ~qt_image_rep() { delete img; };
  friend class qt_image;
};

class qt_image {
CONCRETE_NULL(qt_image);
  qt_image (QTMImage* img2, SI xo2, SI yo2, int w2, int h2):
    rep (tm_new<qt_image_rep> (img2, xo2, yo2, w2, h2)) {};
  // qt_image ();
};

CONCRETE_NULL_CODE(qt_image);

/******************************************************************************
 * Qt pixmaps
 ******************************************************************************/

struct qt_pixmap_rep: concrete_struct {
  QPixmap *img;
  SI xo,yo;
  int w,h;
  qt_pixmap_rep (QPixmap* img2, SI xo2, SI yo2, int w2, int h2):
    img (img2), xo (xo2), yo (yo2), w (w2), h (h2) {};
  ~qt_pixmap_rep()  { delete img; };
  friend class qt_pixmap;
};

class qt_pixmap {
CONCRETE_NULL(qt_pixmap);
  qt_pixmap (QPixmap* img2, SI xo2, SI yo2, int w2, int h2):
    rep (tm_new<qt_pixmap_rep> (img2, xo2, yo2, w2, h2)) {};
  // qt_pixmap ();
};

CONCRETE_NULL_CODE(qt_pixmap);

/******************************************************************************
* Global support variables for all qt_renderers
******************************************************************************/

// bitmaps of all characters
static hashmap<basic_character,qt_image> character_image;  
// image cache
static hashmap<string,qt_pixmap> images;

/******************************************************************************
* qt_renderer
******************************************************************************/

qt_renderer_rep::qt_renderer_rep (int w2, int h2):
  basic_renderer_rep(w2, h2) {}

qt_renderer_rep::~qt_renderer_rep () {}

void
qt_renderer_rep::begin (void* handle) {
  QPaintDevice *device = (QPaintDevice*)handle;
  painter.begin (device);
  w = painter.device()->width();
  h = painter.device()->height();
}

void qt_renderer_rep::end () { painter.end (); }

QColor
qt_color(color c)
{
  int r, g, b;
  get_rgb_color (c,r,g,b);
  return QColor(r, g, b);
}

void 
qt_renderer_rep::get_extents (int& w2, int& h2) {  
  if (painter.device()) {
    w2 = painter.device()->width(); h2 = painter.device()->height();
  } else {
    w2 = w; h2 = h;
  }
}

/******************************************************************************
 * Clipping
 ******************************************************************************/

void
qt_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore)
{
  (void) restore;
  basic_renderer_rep::set_clipping (x1, y1, x2, y2);
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  if ((x1<x2) && (y2<y1)) {
    QRect r(x1,y2,x2-x1,y1-y2);
    painter.setClipRect(r);
  }
}



/******************************************************************************
* Drawing 
******************************************************************************/

void
qt_renderer_rep::set_color (color c) {
  basic_renderer_rep::set_color(c);
  QPen p (painter.pen ());
  QBrush b (painter.brush ());
  p.setColor (qt_color(cur_fg));
  b.setColor (qt_color(cur_fg));
  painter.setPen (p);
  painter.setBrush (b);
}

void
qt_renderer_rep::set_line_style (SI lw, int type, bool round) {
  (void) type;
  QPen p (painter.pen ());
  if (lw <= pixel) p.setWidth (0);
  else p.setWidth ((lw+thicken) / (1.0*pixel));
  p.setCapStyle (round? Qt::RoundCap: Qt::SquareCap);
  p.setJoinStyle (Qt::RoundJoin);
  painter.setPen (p);
}

void
qt_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  decode (x1, y1);
  decode (x2, y2);
  // y1--; y2--; // top-left origin to bottom-left origin conversion
  painter.setRenderHints (QPainter::Antialiasing);
  painter.drawLine (x1, y1, x2, y2);
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
    if (i>0) {
      painter.setRenderHints (QPainter::Antialiasing);
      painter.drawLine (pnt[i-1], pnt[i]); // FIX: hack
    }
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
  QBrush brush (qt_color(cur_bg));
  painter.setRenderHints (0);
  painter.fillRect (x1, y2, x2-x1, y1-y2, brush);       
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

  QBrush brush (qt_color(cur_fg));
  painter.setRenderHints (0);
  painter.fillRect (x1, y2, x2-x1, y1-y2, brush);       
}

void
qt_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  painter.setRenderHints (QPainter::Antialiasing);
  painter.drawArc (x1, y2, x2-x1, y1-y2, alpha/4, (delta-alpha)/4);
}

void
qt_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  (void) alpha; (void) delta;
  if ((x1>=x2) || (y1>=y2)) return;
  decode (x1, y1);
  decode (x2, y2);
  painter.setRenderHints (QPainter::Antialiasing);
  painter.drawArc (x1, y2, x2-x1, y1-y2, alpha/4, (delta-alpha)/4);
}

void
qt_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  QPolygonF poly(n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    poly[i] = QPointF (xx, yy);
  }
  QBrush brush(qt_color(cur_fg));
  QPainterPath pp;
  pp.addPolygon (poly);
  pp.closeSubpath ();
  pp.setFillRule (convex? Qt::OddEvenFill: Qt::WindingFill);
  painter.setRenderHints (QPainter::Antialiasing);
  painter.fillPath (pp, brush);
}


/******************************************************************************
* Image rendering
******************************************************************************/

struct qt_cache_image_rep: cache_image_element_rep {
  qt_cache_image_rep (int w2, int h2, time_t time2, QImage *ptr2):
    cache_image_element_rep (w2, h2, time2, ptr2) {}
  virtual ~qt_cache_image_rep () {
    delete static_cast<QImage*> (ptr); }
};

void
qt_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
                        double cx1, double cy1, double cx2, double cy2)
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)

  if(cx2<=cx1 || cy2<=cy1) return;

  w= w/pixel; h= h/pixel;
  decode (x, y);

  QImage *pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
         << as_string (cx1) << as_string (cy1)
         << as_string (cx2) << as_string (cy2) << "qt-image" ;
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci)) {
    pm= static_cast<QImage*> (ci->ptr);
  } else {
    // rendering
    bool needs_crop= false;
    if (qt_supports_image (u)) {
      pm= new QImage (to_qstring (as_string (u)));
      needs_crop= true;
    } else if (suffix (u) == "ps" ||
             suffix (u) == "eps" ||
             suffix (u) == "pdf") {

#ifdef MACOSX_EXTENSIONS
      url temp= url_temp (".png");
      mac_image_to_png (u, temp);
      needs_crop= true;
#else
      string idstr= eval_system ("identify",u);
      int i=0;
      int a=0,b=0;
      while(i<N(idstr)) {
        b=0;
        if(idstr[i]==' ') {
          i++;
          a=i;
          while(i<N(idstr) && idstr[i]>'0' && idstr[i]<'9')
            i++;
          if(i>=N(idstr))
            break;
          if(idstr[i] != 'x')
            continue;
          i++;
          b=i;
          while(i<N(idstr) && idstr[i]>'0' && idstr[i]<'9')
            i++;
          if(i<N(idstr) && idstr[i]==' ')
            break;
        }
        i++;
      }
      int iw,ih;
      if(b>0) {
        iw=as_int(idstr(a,b-1));
        ih=as_int(idstr(b,i));
      } else {
        int bbx1,bby1,bbx2,bby2;
        ps_bounding_box(u,bbx1,bby1,bbx2,bby2);
        iw=bbx2-bbx1;
        ih=bby2-bby1;
      }

//      float resx = 72*w/((bbx2-bbx1)*(cx2-cx1));
//      float resy = 72*h/((bby2-bby1)*(cy2-cy1));
      float resx = 144*w/(iw*(cx2-cx1));
      float resy = 144*h/(ih*(cy2-cy1));

      url temp= url_temp (".png");
      system ("convert -density " * as_string(resx) * "x" * as_string(resy)
             * " -scale 50% -crop " * as_string(w) * "x" * as_string(h)
             * "+" * as_string (cx1*w/(cx2-cx1))
             * "+" * as_string ((1-cy2)*h/(cy2-cy1))
             * "! -background white -flatten", u, temp);
#endif
      pm= new QImage (to_qstring (as_string (temp)));
      remove (temp);
    }
    if (pm == NULL || pm->isNull ()) {
      cout << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
      if (pm != NULL) delete pm;
      return;
    }

    if(needs_crop) {
      int iw= pm->width ();
      int ih= pm->height ();
      int x1= as_int (cx1 * iw);
      int y1= as_int (cy1 * ih);
      int x2= as_int (cx2 * iw);
      int y2= as_int (cy2 * ih);
      int ww= x2 - x1;
      int hh= y2 - y1;

      (*pm)= pm->copy(QRect (x1, hh-y2, ww, hh));
      (*pm)= pm->scaled(w,h);
    }

    ci = tm_new<qt_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }

  painter.drawImage (x, y-h, *pm);
};


void
qt_renderer_rep::draw_clipped (QImage *im, int w, int h, SI x, SI y) {
  (void) w; (void) h;
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
       // clear(x1,y1,x2,y2);
  painter.setRenderHints (0);
  painter.drawImage (x, y, *im);
}

void
qt_renderer_rep::draw_clipped (QPixmap *im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
  // clear(x1,y1,x2,y2);
  painter.setRenderHints (0);
  painter.drawPixmap (x, y, w, h, *im);
}



void
qt_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  basic_character xc (c, fng, sfactor, cur_fg, 0);
  qt_image mi = character_image [xc];
  if (is_nil(mi)) {
    int r, g, b;
    get_rgb (cur_fg, r, g, b);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, sfactor, sfactor, xo, yo);
    int i, j, w= gl->width, h= gl->height;
#ifdef QTMPIXMAPS
    QTMImage *im = new QPixmap(w,h);
    {
      int nr_cols= sfactor*sfactor;
      if (nr_cols >= 64) nr_cols= 64;

      im->fill (Qt::transparent);
      QPainter pp(im);
      QPen pen(painter.pen());
      QBrush brush(pen.color());
      pp.setPen(Qt::NoPen);
      for (j=0; j<h; j++)
        for (i=0; i<w; i++) {
          int col = gl->get_x (i, j);
          brush.setColor (QColor (r, g, b, (255*col)/nr_cols));
          pp.fillRect (i, j, 1, 1, brush);
        }
      pp.end();
    }
#else
    QTMImage *im= new QImage (w, h, QImage::Format_ARGB32);
    //QTMImage *im= new QImage (w, h, QImage::Format_ARGB32_Premultiplied);
    {
      int nr_cols= sfactor*sfactor;
      if (nr_cols >= 64) nr_cols= 64;

      // the following line is disabled because
      // it causes a crash on Qt/X11 4.4.3
      //im->fill (Qt::transparent);

      for (j=0; j<h; j++)
        for (i=0; i<w; i++) {
          int col = gl->get_x (i, j);
          im->setPixel (i, j, qRgba (r, g, b, (255*col)/nr_cols));
        }
    }
#endif
    qt_image mi2 (im, xo, yo, w, h);
    mi = mi2;
    //[im release]; // qt_image retains im
    character_image (xc)= mi;
    // FIXME: we must release the image at some point 
    //        (this should be ok now, see qt_image)
  }

  // draw the character
  draw_clipped (mi->img, mi->w, mi->h, x- mi->xo*sfactor, y+ mi->yo*sfactor);
}

/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

extern int char_clip;

QPixmap*
qt_renderer_rep::xpm_image (url file_name) {
  QPixmap *pxm= NULL;
  qt_pixmap mi= images [as_string (file_name)];
  if (is_nil (mi)) {
    string sss;
    load_string ("$TEXMACS_PIXMAP_PATH" * file_name, sss, false);
    if (sss == "")
      load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", sss, true);
    uchar *buf= (uchar*) as_charp (sss);
    pxm= new QPixmap();
    pxm->loadFromData (buf, N(sss));
    tm_delete_array ((char*) buf);
    //out << sss;
    //cout << "pxm: " << file_name << "(" << pxm->size().width()
    //     << "," <<  pxm->size().height() << ")\n";
    qt_pixmap mi2 (pxm, 0, 0, pxm->width(), pxm->height());
    mi= mi2;
    images (as_string (file_name))= mi2;
  }
  else pxm=  mi->img ;
  return pxm;
}

void
qt_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  QPixmap* image = xpm_image (file_name);
  ASSERT (sfactor == 1, "shrinking factor should be 1");
  int w, h;
  w = image->width ();
  h = image->height ();
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}

/******************************************************************************
 * main qt renderer
 ******************************************************************************/


qt_renderer_rep*
the_qt_renderer () {
  static qt_renderer_rep* the_renderer= NULL;
//  if (!the_renderer) the_renderer= tm_new<qt_renderer_rep> ();
  if (!the_renderer) the_renderer= 
    tm_new<qt_shadow_renderer_rep> (QPixmap(1,1));
  return the_renderer;
}


/******************************************************************************
 * shadow qt renderer
 ******************************************************************************/




qt_shadow_renderer_rep::qt_shadow_renderer_rep (QPixmap _px) 
// : qt_renderer_rep (_px.width(),_px.height()), px(_px) 
: qt_renderer_rep (), px(_px) 
{ 
  //cout << px.width() << "," << px.height() << " " << LF;
 // painter.begin(&px);
}

qt_shadow_renderer_rep::~qt_shadow_renderer_rep () 
{ 
  painter.end(); 
}

void 
qt_shadow_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
    else 
      ((qt_shadow_renderer_rep*)ren)->end();
    // cout << "Old: " << sw << ", " << sh << "\n";
  }
  if (ren == NULL)  ren= (renderer) tm_new<qt_shadow_renderer_rep> (QPixmap (mw, mh));

  // cout << "Create " << mw << ", " << mh << "\n";
  ((qt_shadow_renderer_rep*)ren)->begin(&(((qt_shadow_renderer_rep*)ren)->px));
}

void 
qt_shadow_renderer_rep::delete_shadow (renderer& ren)  {
  if (ren != NULL) {
    tm_delete (ren);
    ren= NULL;
  }
}

void 
qt_shadow_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  qt_shadow_renderer_rep* shadow= static_cast<qt_shadow_renderer_rep*>(ren);
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
  if (x1<x2 && y2<y1) {
    QRect rect = QRect(x1, y2, x2-x1, y1-y2);
//    shadow->painter.setCompositionMode(QPainter::CompositionMode_Source);   
    shadow->painter.drawPixmap (rect, px, rect);
//    cout << "qt_shadow_renderer_rep::get_shadow " 
//         << rectangle(x1,y2,x2,y1) << LF;
//  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  }
}

void 
qt_shadow_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  qt_shadow_renderer_rep* shadow= static_cast<qt_shadow_renderer_rep*>(ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1) {
    QRect rect = QRect(x1, y2, x2-x1, y1-y2);
//    cout << "qt_shadow_renderer_rep::put_shadow " 
//         << rectangle(x1,y2,x2,y1) << LF;
//    painter.setCompositionMode(QPainter::CompositionMode_Source);
    painter.drawPixmap (rect, shadow->px, rect);
//  XCopyArea (dpy, shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  }
}


void 
qt_shadow_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2)  {
  if (master == NULL) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  master->encode (x1, y1);
  master->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}

