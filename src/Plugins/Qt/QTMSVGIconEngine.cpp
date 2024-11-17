/******************************************************************************
* MODULE   : QTMSVGIconEngine.cpp
* DESCRIPTION: A Qt6 QIconEngine for the TeXmacs SVG icons
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMSVGIconEngine.hpp"

#if QT_VERSION >= 0x060000

QTMSVGIconEngine::QTMSVGIconEngine(QString path) : mPath(path) {
  mSvgRenderer.load(path);
  if (mSvgRenderer.viewBox().isEmpty()) {
    // The viewbox is not set in some TeXmacs SVGs. We set it manually, 
    // adding a small margin to avoid clipping
    mSvgRenderer.setViewBox(QRect(-2, 
                                  -2, 
                                  mSvgRenderer.defaultSize().width() + 4,
                                  mSvgRenderer.defaultSize().height() + 4
                            ));
  }
}

void QTMSVGIconEngine::paint(QPainter *painter, 
                             const QRect &rect,
                            QIcon::Mode mode,
                            QIcon::State state) {
  // scale the svg to fit the pixmap, but keep the aspect ratio
  (void) mode; (void) state;
  qreal scale = qMin(rect.width() / qreal(mSvgRenderer.defaultSize().width()), 
                     rect.height() / qreal(mSvgRenderer.defaultSize().height())
                    );
  QRect scaledRect = rect;
  scaledRect.setSize(mSvgRenderer.defaultSize() * scale);
  scaledRect.moveCenter(rect.center());

  // render the svg
  mSvgRenderer.render(painter, scaledRect);
}

QPixmap QTMSVGIconEngine::pixmap(const QSize &size,
                                 QIcon::Mode mode, 
                                QIcon::State state) {
  (void) mode; (void) state;
  QPixmap pixmap(size);
  pixmap.fill(Qt::transparent);
  QPainter painter(&pixmap);
  QRect rect(QPoint(0, 0), mSvgRenderer.defaultSize());

  // scale the svg to fit the pixmap, but keep the aspect ratio
  qreal scale = qMin(size.width() / qreal(rect.width()), 
                     size.height() / qreal(rect.height())
                    );
  rect.setSize(rect.size() * scale);
  
  // center the svg in the pixmap
  rect.moveCenter(pixmap.rect().center());
  
  // render the svg
  mSvgRenderer.render(&painter, rect);
  return pixmap;
}

QSize QTMSVGIconEngine::actualSize(const QSize &size, 
                                   QIcon::Mode mode,
                                   QIcon::State state) {
  (void) mode; (void) state;
  QSize scaled = size;
  //Return a bigger size to avoid scale up later
  QSize defaultSize = mSvgRenderer.defaultSize();
  qreal scale = qMax(size.width() / qreal(defaultSize.width()),
                     size.height() / qreal(defaultSize.height()));
  scaled = defaultSize * scale;
  return scaled;
}

QIconEngine *QTMSVGIconEngine::clone() const {
  return new QTMSVGIconEngine(mPath);
}

#endif // QT_VERSION >= 0x060000
