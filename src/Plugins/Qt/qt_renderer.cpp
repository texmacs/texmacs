
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
#include "image_files.hpp"
#include "scheme.hpp"
#include "frame.hpp"

#include <QObject>
#include <QWidget>
#include <QPaintDevice>
#include <QPixmap>

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

/*
** hash contents must be removed because 
** the underlying objects are destroyed during 
** Qt exit function
*/
void del_obj_qt_renderer(void)  {
  character_image= hashmap<basic_character,qt_image> ();  
  images= hashmap<string,qt_pixmap>() ;
}

/******************************************************************************
* qt_renderer
******************************************************************************/

qt_renderer_rep::qt_renderer_rep (QPainter *_painter, int w2, int h2):
  basic_renderer_rep (true, w2, h2), painter(_painter) {
    reset_zoom_factor(); }

qt_renderer_rep::~qt_renderer_rep () {}

void*
qt_renderer_rep::get_handle () {
  return (void*) this;
}

void
qt_renderer_rep::begin (void* handle) {
  QPaintDevice *device = static_cast<QPaintDevice*>(handle);
  if (!painter->begin (device) && DEBUG_QT)
    debug_qt << "qt_renderer_rep::begin(): uninitialized QPixmap of size "
             << ((QPixmap*)handle)->width() << " x "
             << ((QPixmap*)handle)->height() << LF;
    
  w = painter->device()->width();
  h = painter->device()->height();
}

void qt_renderer_rep::end () { painter->end (); }

void 
qt_renderer_rep::get_extents (int& w2, int& h2) {  
  if (painter->device()) {
    w2 = painter->device()->width(); h2 = painter->device()->height();
  } else {
    w2 = w; h2 = h;
  }
}

void
qt_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (retina_factor * zoom);
  retina_pixel= pixel * retina_factor;
}

/******************************************************************************
* Transformations
******************************************************************************/

void
qt_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);

  frame cv= scaling (point (pixel, -pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  //cout << "Set transformation " << o << ", " << ux << ", " << uy << "\n";
  QTransform qtr (ux[0], ux[1], uy[0], uy[1], o[0], o[1]);
  painter->save ();
  painter->setTransform (qtr, true);

  rectangle nclip= fr [oclip];
  clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
qt_renderer_rep::reset_transformation () {
  unclip ();
  painter->restore ();
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
    painter->setClipRect(r);
  } else {
    painter->setClipRect(QRect());
  }
}

/******************************************************************************
* Drawing 
******************************************************************************/
bool is_percentage (tree t, string s= "%");
double as_percentage (tree t);

static QImage*
get_pattern_image (brush br, SI pixel) {
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  QImage* pm= get_image (u, w, h, eff, pixel);
  return pm;
}

void
qt_renderer_rep::set_pencil (pencil np) {
  painter->setOpacity (qreal (1.0));
  basic_renderer_rep::set_pencil (np);
  QPen p (painter->pen ());
  QBrush b (painter->brush ());
  QColor qc= to_qcolor (pen->get_color ());
  p.setColor (qc);
  b.setColor (qc);
  //SI pw= 0;
  //if (pen->get_width () > pixel)
  //pw= (pen->get_width () + thicken) / (1.0*pixel);
  //p.setWidth (pw);
  qreal pw= (qreal) (((double) pen->get_width ()) / ((double) pixel));
  p.setWidthF (pw);
  if (np->get_type () == pencil_brush) {
    brush br= np->get_brush ();
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
  }
  p.setStyle (Qt::SolidLine);
  p.setCapStyle (pen->get_cap () == cap_round? Qt::RoundCap: Qt::SquareCap);
  p.setJoinStyle (Qt::RoundJoin);
  painter->setPen (p);
  painter->setBrush (b);
}

void
qt_renderer_rep::set_brush (brush br) {
  basic_renderer_rep::set_brush (br);
  if (br->get_type () == brush_none) {
    painter->setPen (QPen (Qt::NoPen));
    painter->setBrush (QBrush (Qt::NoBrush));
  }
  else {
    QPen p (painter->pen ());
    QBrush b (painter->brush ());
    QColor col= to_qcolor (pen->get_color ());
    p.setColor (col);
    b.setColor (col);
    painter->setPen (p);
    painter->setBrush (b);
  }
  if (br->get_type () == brush_pattern) {
    QImage* pm= get_pattern_image (br, pixel);
    int pattern_alpha= br->get_alpha ();
    painter->setOpacity (qreal (pattern_alpha) / qreal (255));
    if (pm != NULL) {
      QBrush b (*pm);
      double pox, poy;
      decode (0, 0, pox, poy);
      QTransform tr;
      tr.translate (pox, poy);
      //tr.rotate (45.0);
      b.setTransform (tr);
      painter->setBrush (b);
    }
  }
}

void
qt_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  // y1--; y2--; // top-left origin to bottom-left origin conversion
  painter->setRenderHints (QPainter::Antialiasing);
  painter->drawLine (QPointF (rx1, ry1), QPointF (rx2, ry2));
}

void
qt_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<2)) return;
  STACK_NEW_ARRAY (pnt, QPointF, n);
  for (i=0; i<n; i++)
#ifdef __arm__
// for compiling in Raspbian
  {
    double px = double (pnt[i].rx());//explicit conversion needed for ARM where qreal==float!=double
    double py = double (pnt[i].ry());    
    decode (x[i], y[i], px, py);
    pnt[i].rx()=qreal(px); pnt[i].ry()=qreal(py);
  }
#else
    decode (x[i], y[i], pnt[i].rx(), pnt[i].ry());
#endif

  QPen p= painter->pen();
  p.setCapStyle (pen->get_cap () == cap_round? Qt::RoundCap: Qt::SquareCap);
  if (x[N(x)-1] == x[0] && y[N(y)-1] == y[0]) p.setCapStyle (Qt::RoundCap);
  p.setJoinStyle (Qt::RoundJoin);
  painter->setPen (p);

  painter->setRenderHints (QPainter::Antialiasing);
  painter->drawPolyline (pnt, n);
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
  QBrush br (to_qcolor (bg_brush->get_color ()));
  painter->setRenderHints (0);
  painter->fillRect (x1, y2, x2-x1, y1-y2, br);       
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

  QBrush br (to_qcolor (pen->get_color ()));
  painter->setRenderHints (0);
  painter->fillRect (x1, y2, x2-x1, y1-y2, br);       
}

void
qt_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  painter->setRenderHints (QPainter::Antialiasing);
  painter->drawArc (QRectF (rx1, ry2, rx2-rx1, ry1-ry2), alpha / 4, delta / 4);
}

void
qt_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  QBrush br= painter->brush ();
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern)
    br= QBrush (to_qcolor (pen->get_color ()));
  QPainterPath pp;
  pp.arcMoveTo (QRectF (rx1, ry2, rx2-rx1, ry1-ry2), alpha / 64);
  pp.arcTo (QRectF (rx1, ry2, rx2-rx1, ry1-ry2), alpha / 64, delta / 64);
  pp.closeSubpath ();
  pp.setFillRule (Qt::WindingFill);
  painter->setRenderHints (QPainter::Antialiasing);
  painter->fillPath (pp, br);
}

void
qt_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  QPolygonF poly (n);
  for (i=0; i<n; i++) {
    double qx, qy;
    decode (x[i], y[i], qx, qy);
    poly[i] = QPointF (qx, qy);
  }
  QBrush br= painter->brush ();
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern)
    // FIXME: is this really necessary?
    // The brush should have been set at the moment of set_pencil or set_brush
    br= QBrush (to_qcolor (pen->get_color ()));
  QPainterPath pp;
  pp.addPolygon (poly);
  pp.closeSubpath ();
  pp.setFillRule (convex? Qt::OddEvenFill: Qt::WindingFill);
  painter->setRenderHints (QPainter::Antialiasing);
  painter->fillPath (pp, br);
}

void
qt_renderer_rep::draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;

  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  QPolygonF poly(n);
  for (i=0; i<n; i++) {
    SI xx= x[i], yy= y[i];
    decode (xx, yy);
    poly[i] = QPointF (xx, yy);
    //double qx, qy;
    //decode (x[i], y[i], qx, qy);
    //poly[i] = QPointF (qx, qy);
  }
  QBrush br= painter->brush ();
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern)
    // FIXME: is this really necessary?
    // The brush should have been set at the moment of set_pencil or set_brush
    br= QBrush (to_qcolor (pen->get_color ()));
  QPainterPath pp;
  pp.addPolygon (poly);
  pp.closeSubpath ();
  pp.setFillRule (Qt::OddEvenFill);
  painter->setRenderHints (QPainter::Antialiasing, false);
  painter->fillPath (pp, br);
}

/******************************************************************************
* Image rendering
******************************************************************************/

void
qt_renderer_rep::draw_clipped (QImage *im, int w, int h, SI x, SI y) {
  (void) w; (void) h;
  int x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
       // clear(x1,y1,x2,y2);
  painter->setRenderHints (0);
  painter->drawImage (x, y, *im);
}

void
qt_renderer_rep::draw_clipped (QPixmap *im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
  // clear(x1,y1,x2,y2);
  painter->setRenderHints (0);
  painter->drawPixmap (x, y, w, h, *im);
}

void
qt_renderer_rep::draw_bis (int c, font_glyphs fng, SI x, SI y) {
  // draw with background pattern
  SI xo, yo;
  glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
  glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
  int w= gl->width, h= gl->height;
  QImage *im= new QImage (w, h, QImage::Format_ARGB32);
  im->fill (Qt::transparent);

  {
    brush br= pen->get_brush ();
    QImage* pm= get_pattern_image (br, brushpx==-1? pixel: brushpx);
    int pattern_alpha= br->get_alpha ();
    QPainter glim (im);
    glim.setOpacity (qreal (pattern_alpha) / qreal (255));
    if (pm != NULL) {
      SI tx= x- xo*std_shrinkf, ty= y+ yo*std_shrinkf;
      decode (tx, ty); ty--;
      QBrush qbr (*pm);
      QTransform qtf= painter->transform ();
      qbr.setTransform (qtf.translate (-tx, -ty));
      glim.setBrush (qbr);
    }
    glim.setPen (Qt::NoPen);
    glim.drawRect (0, 0, w, h);

    int nr_cols= std_shrinkf*std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;
    for (int j=0; j<h; j++)
      for (int i=0; i<w; i++) {
        color patcol= im->pixel (i, j);
        int r, g, b, a;
        get_rgb (patcol, r, g, b, a);
        if (get_reverse_colors ()) reverse (r, g, b);
        int col = gl->get_x (i, j);
        im->setPixel (i, j, qRgba (r, g, b, (a*col)/nr_cols));
      }
  }

  draw_clipped (im, w, h, x- xo*std_shrinkf, y+ yo*std_shrinkf);
  delete im;
}

void
qt_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  if (pen->get_type () == pencil_brush) {
    draw_bis (c, fng, x, y);
    return;
  }

  // get the pixmap
  color fgc= pen->get_color ();
  basic_character xc (c, fng, std_shrinkf, fgc, 0);
  qt_image mi = character_image [xc];
  if (is_nil(mi)) {
    int r, g, b, a;
    get_rgb (fgc, r, g, b, a);
    if (get_reverse_colors ()) reverse (r, g, b);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int i, j, w= gl->width, h= gl->height;
#ifdef QTMPIXMAPS
    QTMImage *im = new QPixmap(w,h);
    {
      int nr_cols= std_shrinkf*std_shrinkf;
      if (nr_cols >= 64) nr_cols= 64;

      im->fill (Qt::transparent);
      QPainter pp(im);
      QPen pen(painter->pen());
      QBrush br(pen.color());
      pp.setPen(Qt::NoPen);
      for (j=0; j<h; j++)
        for (i=0; i<w; i++) {
          int col = gl->get_x (i, j);
          br.setColor (QColor (r, g, b, (a*col)/nr_cols));
          pp.fillRect (i, j, 1, 1, br);
        }
      pp.end();
    }
#else
    QTMImage *im= new QImage (w, h, QImage::Format_ARGB32);
    //QTMImage *im= new QImage (w, h, QImage::Format_ARGB32_Premultiplied);
    {
      int nr_cols= std_shrinkf*std_shrinkf;
      if (nr_cols >= 64) nr_cols= 64;

      // the following line is disabled because
      // it causes a crash on Qt/X11 4.4.3
      //im->fill (Qt::transparent);

      for (j=0; j<h; j++)
        for (i=0; i<w; i++) {
          int col = gl->get_x (i, j);
          im->setPixel (i, j, qRgba (r, g, b, (a*col)/nr_cols));
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
  //cout << (char)c << ": " << cx1/256 << ","  << cy1/256 << ","  
  //<< cx2/256 << ","  << cy2/256 << LF; 
  draw_clipped (mi->img, mi->w, mi->h, x- mi->xo*std_shrinkf, y+ mi->yo*std_shrinkf);
}

void
qt_renderer_rep::draw (const QFont& qfn, const QString& qs,
                       SI x, SI y, double zoom) {
  decode (x, y);
  painter->setFont (qfn);
  painter->translate (x, y);
  painter->scale (zoom, zoom);
  painter->drawText (0, 0, qs);
  painter->resetTransform ();
}

/******************************************************************************
 * main qt renderer
 ******************************************************************************/


qt_renderer_rep*
the_qt_renderer () {
  static QPainter *the_painter = NULL;
  static qt_renderer_rep* the_renderer= NULL;
  if (!the_renderer) {
    the_painter = new QPainter();
    the_renderer= tm_new<qt_renderer_rep> (the_painter);
  }
  return the_renderer;
}


/******************************************************************************
 * Shadow management methods 
 ******************************************************************************/

/* Shadows are auxiliary renderers which allow double buffering and caching of
 * graphics. TeXmacs has explicit double buffering from the X11 port. Maybe
 * it would be better to design a better API abstracting from the low level 
 * details but for the moment the following code and the qt_proxy_renderer_rep
 * and qt_shadow_renderer_rep classes are designed to solve two problems:
 * 
 * 1) Qt has already double buffering.
 * 2) in Qt we are not easily allowed to read onscreen pixels (we can only ask a
 *    widget to redraw himself on a pixmap or read the screen pixels -- this has
 *    the drawback that if our widget is under another one we won't read the 
 *    right pixels)
 * 
 * qt_proxy_renderer_rep solves the double buffering problem: when texmacs asks
 * a qt_renderer_rep for a shadow it is given a proxy of the original renderer
 * texmacs uses this shadow for double buffering and the proxy will simply
 * forward the drawing operations to the original surface and neglect all the
 * syncronization operations
 *
 * to solve the second problem we do not draw directly on screen in QTMWidget.
 * Instead we maintain an internal pixmap which represents the state of the pixels
 * according to texmacs. When we are asked to initialize a qt_shadow_renderer_rep
 * we simply read the pixels form this backing store. At the Qt level then
 * (in QTMWidget) we make sure that the state of the backing store is in sync
 * with the screen via paintEvent/repaint mechanism.
 *
 */


void
qt_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
    // cout << "Old: " << sw << ", " << sh << "\n";
  }
  if (ren == NULL)  ren= (renderer) tm_new<qt_proxy_renderer_rep> (this);
  
  // cout << "Create " << mw << ", " << mh << "\n";
}

void 
qt_renderer_rep::delete_shadow (renderer& ren)  {
  if (ren != NULL) {
    tm_delete (ren);
    ren= NULL;
  }
}

void 
qt_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  qt_renderer_rep* shadow= static_cast<qt_renderer_rep*>(ren);
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
    QRect rect = QRect(x1, y2, x2-x1, y1-y2);
    //    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);  
    shadow->painter->setClipRect(rect);
//    shadow->painter->drawPixmap (rect, px, rect);
    //    cout << "qt_shadow_renderer_rep::get_shadow " 
    //         << rectangle(x1,y2,x2,y1) << LF;
    //  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  } else {
    shadow->painter->setClipRect(QRect());
  }
}

void 
qt_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  if (painter == static_cast<qt_renderer_rep*>(ren)->painter) return;
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
    //    painter->setCompositionMode(QPainter::CompositionMode_Source);
    painter->drawPixmap (rect, shadow->px, rect);
    //  XCopyArea (dpy, shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  }
}


void 
qt_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2)  {
  if (master == NULL) return;
  if (painter == static_cast<qt_renderer_rep*>(master)->painter) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  static_cast<qt_renderer_rep*>(master)->encode (x1, y1);
  static_cast<qt_renderer_rep*>(master)->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}


/******************************************************************************
* proxy qt renderer
******************************************************************************/

void 
qt_proxy_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
    else 
      static_cast<qt_shadow_renderer_rep*>(ren)->end();
    // cout << "Old: " << sw << ", " << sh << "\n";
  }
  if (ren == NULL)  
    ren= (renderer) tm_new<qt_shadow_renderer_rep> (QPixmap (mw, mh));
  
  // cout << "Create " << mw << ", " << mh << "\n";
  static_cast<qt_shadow_renderer_rep*>(ren)->begin(
          &(static_cast<qt_shadow_renderer_rep*>(ren)->px));
}

void 
qt_proxy_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  qt_renderer_rep* shadow= static_cast<qt_renderer_rep*>(ren);
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

    shadow->painter->setClipRect(rect);

    //    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);
    QPixmap *_pixmap = static_cast<QPixmap*>(painter->device()); 
    if (_pixmap) {
      shadow->painter->drawPixmap (rect, *_pixmap, rect);
    }
    //    cout << "qt_shadow_renderer_rep::get_shadow " 
    //         << rectangle(x1,y2,x2,y1) << LF;
    //  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  } else {
    shadow->painter->setClipRect(QRect());
  }

}


/******************************************************************************
 * shadow qt renderer
 ******************************************************************************/

qt_shadow_renderer_rep::qt_shadow_renderer_rep (QPixmap _px) 
// : qt_renderer_rep (_px.width(),_px.height()), px(_px) 
: qt_renderer_rep (new QPainter()), px(_px) 
{ 
  //cout << px.width() << "," << px.height() << " " << LF;
 // painter->begin(&px);
}

qt_shadow_renderer_rep::~qt_shadow_renderer_rep () 
{ 
  painter->end(); 
  delete painter;
  painter = NULL;
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
    shadow->painter->setClipRect(rect);

//    shadow->painter->setCompositionMode(QPainter::CompositionMode_Source);   
    shadow->painter->drawPixmap (rect, px, rect);
//    cout << "qt_shadow_renderer_rep::get_shadow " 
//         << rectangle(x1,y2,x2,y1) << LF;
//  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  } else {
    shadow->painter->setClipRect(QRect());
  }
}
