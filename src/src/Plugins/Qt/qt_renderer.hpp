
/******************************************************************************
* MODULE     : qt_renderer.hpp
* DESCRIPTION: QT drawing interface class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef QT_RENDERER_HPP
#define QT_RENDERER_HPP

#include "renderer.hpp"
#include "qt_gui.hpp"
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QtGlobal>

/******************************************************************************
* QT images
******************************************************************************/

// if QTMPIXMAPS is defined we use QPixmap for characters and button icons
// otherwise we use QImage (which support alpha also under X11)

#ifdef Q_WS_MAC
#define QTMPIXMAPS
#else
#undef QTMPIXMAPS
#endif

#ifdef QTMPIXMAPS
typedef QPixmap QTMImage;
#else
typedef QImage QTMImage;	
#endif

struct qt_image_rep: concrete_struct {
  QTMImage *img;
  SI xo,yo;
  int w,h;
  qt_image_rep (QTMImage *img2, SI xo2, SI yo2, int w2, int h2);
  ~qt_image_rep();
  friend class qt_image;
  friend class qt_drawable;
};

class qt_image {
  CONCRETE_NULL(qt_image);
  qt_image (QTMImage *img2, SI xo2, SI yo2, int w2, int h2);
 // qt_image ();
};

CONCRETE_NULL_CODE(qt_image);

class qt_renderer_rep: virtual public renderer_rep {
public:
  qt_gui dis;
  QPainter painter; // FIXME: painter needs begin/end
  int   w, h;
  color cur_fg, cur_bg;

public:
  qt_renderer_rep (qt_gui dis, int w = 0, int h = 0);
  virtual ~qt_renderer_rep ();
  
  virtual void get_extents (int& w, int& h);
  virtual bool interrupted (bool check= false);
  
  /* routines from renderer.hpp **********************************************/

   color rgb (int r, int g, int b);
   void  get_rgb (color col, int& r, int& g, int& b);
   color get_color ();
 //  color get_color (string s);
   color get_background ();
  
  void draw (int char_code, font_glyphs fn, SI x, SI y);

#if 0
  /* main graphical routines */
  virtual void set_color (color c);
  virtual void set_background (color c);
  virtual void draw (int char_code, font_glyphs fn, SI x, SI y);
  virtual void set_line_style (SI w, int type=0, bool round=true);
  virtual void line (SI x1, SI y1, SI x2, SI y2);
  virtual void clear (SI x1, SI y1, SI x2, SI y2);
  virtual void fill (SI x1, SI y1, SI x2, SI y2);
  virtual void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  virtual void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  virtual void polygon (array<SI> x, array<SI> y, bool convex=true);
  virtual void xpm (url file_name, SI x, SI y);
  virtual void image (url u, SI w, SI h, SI x, SI y,
                      double cx1, double cy1, double cx2, double cy2);
 // virtual void get_clipping (SI &x1, SI &y1, SI &x2, SI &y2);
  virtual void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  
  /* shadowing and copying rectangular regions across devices */
  virtual void fetch (SI x1, SI y1, SI x2, SI y2, renderer dev, SI x, SI y);
  virtual void new_shadow (renderer& dev);
  virtual void delete_shadow (renderer& dev);
  virtual void get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2);
  virtual void put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2);
  virtual void apply_shadow (SI x1, SI y1, SI x2, SI y2);
#endif


  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  //color get_color ();
  //color get_background ();
  void  set_color (color c);
  void  set_background (color c);
  void  set_line_style (SI w, int type=0, bool round=true);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  xpm (url file_name, SI x, SI y);
  void  image (url u, SI w, SI h, SI x, SI y,
	       double cx1, double cy1, double cx2, double cy2);

  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  
  void draw_clipped (QTMImage * im, int w, int h, SI x, SI y);

	QTMImage *xpm_image(url file_name);

  void begin(QPaintDevice * device) { painter.begin(device); } ;
  void end() { painter.end(); };
  
 void encode (SI& x, SI& y);
  void decode (SI& x, SI& y);
  
  friend class qt_window_rep;
      
};

qt_renderer_rep *the_qt_renderer();

#endif // defined QT_RENDERER_HPP
