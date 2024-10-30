
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
#include "QTMPixmapOrImage.hpp"
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QtGlobal>
#include <QWidget>

class qt_renderer_rep:  public basic_renderer_rep {
public:
  QPainter *painter; // FIXME: painter needs begin/end

public:
#if QT_VERSION >= 0x060000
  qt_renderer_rep (QPainter *_painter, qreal dpr, int w, int h);
  qt_renderer_rep (QPainter *_painter, qt_renderer_rep *parent);
#else
  qt_renderer_rep (QPainter *_painter, int w = 0, int h = 0);
#endif
  ~qt_renderer_rep ();
  void* get_handle ();

  void set_zoom_factor (double zoom);

#if QT_VERSION >= 0x060000
  inline qreal get_dpr () { if (parent) return parent->get_dpr(); else return dpr; }
  inline void set_dpr (qreal _dpr) { dpr = _dpr; }
  inline void set_zoom_multiplier (qreal _zoom_multiplier) { zoom_multiplier = _zoom_multiplier; }
#endif

  void begin (void* handle);
#if QT_VERSION >= 0x060000
  void begin ();
#endif
  void end ();

  //void set_extent (int _w, int _h) { w = _w; h = _h; }
  void get_extents (int& w, int& h);

  void set_transformation (frame fr);
  void reset_transformation ();

  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore = false);

  void  draw_bis (int char_code, font_glyphs fn, SI x, SI y);
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  draw (const QFont& qfn, const QString& s, SI x, SI y, double zoom);
  void  set_pencil (pencil p);
  void  set_brush (brush b);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3);

  void draw_clipped (QImage * im, int w, int h, SI x, SI y);
  void draw_clipped (QTMPixmapOrImage * im, int w, int h, SI x, SI y);
  
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);

  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  void draw_picture (picture pict, SI x, SI y, int alpha);

private:
#if QT_VERSION >= 0x060000
  qreal dpr;
  qreal zoom_multiplier = 1.0;
  qt_renderer_rep *parent;
#endif

};

qt_renderer_rep* the_qt_renderer();
QImage* get_image (url u, int w, int h, tree eff, SI pixel);

class qt_shadow_renderer_rep: public qt_renderer_rep {
public:
  QTMPixmapOrImage px;   
  qt_renderer_rep *master;
  
public:
#if QT_VERSION >= 0x060000
  qt_shadow_renderer_rep (QTMPixmapOrImage _px, qt_renderer_rep *parent);
#else
  qt_shadow_renderer_rep (QTMPixmapOrImage _px= QTMPixmapOrImage ());
#endif
  ~qt_shadow_renderer_rep ();
  
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

class qt_proxy_renderer_rep: public qt_renderer_rep {
public:
  qt_renderer_rep *base;
  
public:
#if QT_VERSION >= 0x060000
  qt_proxy_renderer_rep (qt_renderer_rep *_base) 
  : qt_renderer_rep(_base->painter, _base) {};
#else
  qt_proxy_renderer_rep (qt_renderer_rep *_base)
  : qt_renderer_rep(_base->painter), base(_base) {};
#endif
  ~qt_proxy_renderer_rep () {};
  
  void new_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

#endif // defined QT_RENDERER_HPP
