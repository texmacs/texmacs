/******************************************************************************
* MODULE   : QTMImpressIconEngine.cpp
* DESCRIPTION: A Qt6 QIconEngine for the TeXmacs Document Icons
* COPYRIGHT  : (C) 2024 Liza Belos
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

QTMImpressIconEngine::QTMImpressIconEngine(qt_simple_widget_rep* w) : wid (w) {
  // get a size hint from the widget that will be used to center the icon
  int sizeHintW, sizeHintH;
  w->handle_get_size_hint (sizeHintW, sizeHintH);
  iconSize = to_qsize (sizeHintW, sizeHintH);
}

void QTMImpressIconEngine::paint(QPainter *painter, const QRect &rect, QIcon::Mode mode, QIcon::State state) {
  qt_renderer_rep ren (painter, 1, rect.size().width(), rect.size().height());

  //painter->scale(0.5, 0.5);
  // compute a scale factor to fit the icon in the rect
  double scale = std::min((double)rect.width() / (iconSize.width() + 2), (double)rect.height() / (iconSize.height() + 2));
  //scale = std::min(scale, 1.0);

  // multiply the rect with the dpr since begin() sets the dpr to 1
  QRect rectDpr(rect.x() * painter->device()->devicePixelRatio(),
                rect.y() * painter->device()->devicePixelRatio(),
                rect.width() * painter->device()->devicePixelRatio(),
                rect.height() * painter->device()->devicePixelRatio());
  
  ren.begin ();
  ren.set_zoom_multiplier (scale);

  // center the icon inside the rectDpr
  QSize iconSizeScaled = QSize(iconSize.width() * scale, iconSize.height() * scale);
  int shiftX = (rectDpr.width() - iconSizeScaled.width()) / (2 * painter->device()->devicePixelRatio());
  int shiftY = (rectDpr.height() - iconSizeScaled.height()) / (2 * painter->device()->devicePixelRatio());

  ren.encode (shiftX, shiftY);  
  ren.set_origin (shiftX, shiftY);

  // disable any clipping by TeXmacs
  rectangle r = rectangle (-9999, -9999, 9999, 9999);
  ren.encode (r->x1, r->y1);
  ren.encode (r->x2, r->y2);
  ren.set_clipping (r->x1, r->y2, r->x2, r->y1);
  
  the_gui->set_check_events (false);
  wid->handle_repaint (&ren, 0, 0, painter->device()->width(), painter->device()->height());
  the_gui->set_check_events (true);
  
  ren.end();
}

QPixmap QTMImpressIconEngine::pixmap(const QSize &size, QIcon::Mode mode, QIcon::State state) {
  // to create the pixmap, we use a QPainter that we pass to the paint method
  QPixmap pxm(size);
  pxm.fill(Qt::transparent);
  QPainter painter(&pxm);
  paint(&painter, QRect(QPoint(0, 0), size), mode, state);
  return pxm;
}

QSize QTMImpressIconEngine::actualSize(const QSize &size, QIcon::Mode mode, QIcon::State state) {
  return size;
}

QIconEngine *QTMImpressIconEngine::clone() const {
  return new QTMImpressIconEngine (wid);
}

#endif