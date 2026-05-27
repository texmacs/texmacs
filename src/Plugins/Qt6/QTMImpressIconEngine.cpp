
/******************************************************************************
* MODULE   : QTMImpressIconEngine.cpp
* DESCRIPTION: A Qt6 QIconEngine for the TeXmacs Document Icons
* COPYRIGHT  : (C) 2024 Liza Belos, 2025 Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMImpressIconEngine.hpp"

#if QT_VERSION >= 0x060000
#include "qt_simple_widget.hpp"
#include "qt_renderer.hpp"
#include "qt_gui.hpp"

#include <QPainter>
#include <QIcon>
#include <QPixmap>

QTMImpressIconEngine::QTMImpressIconEngine (qt_simple_widget_rep* w)
  : wid (w) {
  // get a size hint from the widget that will be used to center the icon
  SI sizeHintW, sizeHintH;
  wid->handle_get_size_hint (sizeHintW, sizeHintH);
  iconSize = to_qsize (sizeHintW, sizeHintH);
}

void
QTMImpressIconEngine::paint (QPainter *painter, const QRect &rect,
			     QIcon::Mode mode, QIcon::State state) {
  (void) mode; (void) state;
  double pixel_ratio= painter->device ()->devicePixelRatio ();
  painter->scale (1/pixel_ratio, 1/pixel_ratio);
  SI pw= painter->device ()->width ();
  SI ph= painter->device ()->height ();
  SI iw= iconSize.width ();
  SI ih= iconSize.height ();
  SI w= rect.width ();
  SI h= rect.height ();
  double z = iw * ih == 0 ? 1
    : std::min (((double) w) / iw, ((double) h) / ih);
  qt_renderer_rep ren (painter, pixel_ratio, pixel_ratio*pw, pixel_ratio*ph);
  ren.zoom_multiplier= z;
  iw *= z; ih *= z;
  ren.basic_renderer_rep::begin (painter);
  SI sx= ::round ((w - iw) / 2);
  SI sy= ::round ((h - ih) / 2);
  ren.encode (sx, sy);
  ren.set_origin (sx, sy);
  rectangle r = rectangle (pixel_ratio * rect.x(), pixel_ratio * rect.y(),
			   ::floor (pixel_ratio*pw), ::ceil (pixel_ratio*ph));
  ren.encode (r->x1, r->y1);
  ren.encode (r->x2, r->y2);
  ren.set_clipping (r->x1, r->y2, r->x2, r->y1); 
  the_gui->set_check_events (false);
  wid->handle_repaint (&ren, r->x1, r->y2, r->x2, r->y1);
  the_gui->set_check_events (true);
  ren.end ();
}

QPixmap
QTMImpressIconEngine::pixmap (const QSize &size,
			      QIcon::Mode mode, QIcon::State state) {
  // to create the pixmap, we use a QPainter that we pass to the paint method
  (void) mode; (void) state;
  QPixmap pxm (size);
  pxm.fill (Qt::transparent);
  QPainter painter (&pxm);
  paint (&painter, QRect (QPoint (0, 0), size), mode, state);
  return pxm;
}

QSize
QTMImpressIconEngine::actualSize (const QSize &size,
				  QIcon::Mode mode, QIcon::State state) {
  (void) mode; (void) state;
  return size;
}

QIconEngine*
QTMImpressIconEngine::clone () const {
  return new QTMImpressIconEngine (wid);
}

#endif
