
/******************************************************************************
* MODULE     : qt_renderer.hpp
* DESCRIPTION: QT drawing interface class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_RENDERER_HPP
#define QT_RENDERER_HPP

#include "basic_renderer.hpp"
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QtGlobal>
#include <QWidget>

// if QTMPIXMAPS is defined we use QPixmap for characters
// otherwise we use QImage (which support alpha also under X11)

#ifdef Q_WS_MAC
#define QTMPIXMAPS
#else
#undef QTMPIXMAPS
#endif

#ifdef QTMPIXMAPS
#define QTMImage QPixmap
#else
#define QTMImage QImage
#endif


class qt_renderer_rep:  public basic_renderer_rep {
public:
  QPainter *painter; // FIXME: painter needs begin/end

public:
  qt_renderer_rep (QPainter *_painter, int w = 0, int h = 0);
  ~qt_renderer_rep ();

  void begin (void* handle);
  void end ();

  //void set_extent (int _w, int _h) { w = _w; h = _h; }
  void get_extents (int& w, int& h);

  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore = false);

  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  set_color (color c);
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

  void draw_clipped (QImage * im, int w, int h, SI x, SI y);
  void draw_clipped (QPixmap * im, int w, int h, SI x, SI y);
  
  /***** private section *****************************************************/

  QPixmap *xpm_image(url file_name);

};


class qt_shadow_renderer_rep:  public qt_renderer_rep {
public:
  QPixmap px;   
  qt_renderer_rep *master;
  
public:
  qt_shadow_renderer_rep (QPixmap _px = QPixmap());
  ~qt_shadow_renderer_rep ();
  
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
};


qt_renderer_rep* the_qt_renderer();

#endif // defined QT_RENDERER_HPP
