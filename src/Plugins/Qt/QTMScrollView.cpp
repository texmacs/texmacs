
/******************************************************************************
 * MODULE     : QTMScrollView.cpp
 * DESCRIPTION: QT Texmacs abstract scroll view widget
 * COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMScrollView.hpp"

#include <QScrollBar>
#include <QApplication>
#include <QResizeEvent>
#include <QStyle>


/*! Constructor.
 
 NOTE:
 Don't try to disable double buffering even if we do our own: the flag 
 Qt::WA_PaintOnScreen is only supported on X11 and anyway makes things slower
 */

QTMScrollView::QTMScrollView (QWidget *_parent):
  QAbstractScrollArea (_parent),
  editor_flag (false),
  p_extents (QRect(0,0,0,0))
{
  QWidget *_viewport = QAbstractScrollArea::viewport();
  _viewport->setBackgroundRole (QPalette::Mid);
  _viewport->setAutoFillBackground (true);
  // alternative settings
  //_viewport->setAttribute (Qt::WA_OpaquePaintEvent);
  //_viewport->setAttribute (Qt::WA_StaticContents);
  //_viewport->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  //_viewport->setAttribute(Qt::WA_MacNoClickThrough);
  //_viewport->setAttribute(Qt::WA_NoSystemBackground);
  setFrameShape (QFrame::NoFrame);
}

void 
QTMScrollView::setOrigin ( QPoint newOrigin ) {
  if (newOrigin.x() != p_origin.x())
    QAbstractScrollArea::horizontalScrollBar()->setSliderPosition(newOrigin.x());
  if (newOrigin.y() != p_origin.y())
    QAbstractScrollArea::verticalScrollBar()->setSliderPosition(newOrigin.y());
}

void 
QTMScrollView::setExtents ( QRect newExtents ) {
  //QWidget *_viewport = QAbstractScrollArea::viewport();
  //cout << "Inside  " << _viewport->width() << ", " << _viewport->height() << "\n";
  //cout << "Extents " << newExtents.width() << ", " << newExtents.height() << "\n";
  if (newExtents.width()  < 0) newExtents.setWidth (0);
  if (newExtents.height() < 0) newExtents.setHeight(0);
  if (p_extents != newExtents) {
    p_extents = newExtents;
    updateScrollBars();
  }
}

/*! Scrolls contents so that the given point is visible. */
void 
QTMScrollView::ensureVisible ( int cx, int cy, int mx, int my ) {
  QWidget *_viewport = QAbstractScrollArea::viewport();
  int w = _viewport->width();
  int h = _viewport->height();
  
  int dx = - p_origin.x();
  int dy = - p_origin.y();
  int cw = p_extents.width();
  int ch = p_extents.height();
  
  if (w < mx * 2) mx = w / 2;
  if (h < my * 2) my = h / 2;
  
  if (cw <= w) { mx = 0; dx = 0; }
  
  if (ch <= h) { my = 0; dy = 0; }
  
  if (cx < mx - dx) dx = mx - cx;
  else if (cx >= w - mx - dx) dx  = w - mx - cx;
  
  if (cy < my - dy) dy = my - cy;
  else if (cy >= h - my - dy) dy  = h - my - cy;
  
  if (dx > 0) dx = 0;
  else if (dx < w - cw && cw > w) dx = w - cw;
  
  if (dy > 0) dy = 0;
  else if (dy < h - ch && ch > h) dy = h - ch;
  
  setOrigin (QPoint(-dx, -dy));
}

/*! Scrollbar stabilization */
void 
QTMScrollView::updateScrollBars (void) {
  QWidget *_viewport = QAbstractScrollArea::viewport();
  QScrollBar *_hScrollBar = QAbstractScrollArea::horizontalScrollBar();
  QScrollBar *_vScrollBar = QAbstractScrollArea::verticalScrollBar();

  // (mg) see edit_interface_rep::apply_changes ()  (set_extent)
  // for the 2 pixel mismatch. I'm not really sure why is needed.
  int xw = p_extents.width() - 2;
  int xh = p_extents.height()- 2;
  int w  = _viewport->width();
  int h  = _viewport->height();
  int sbw= qApp->style()->pixelMetric (QStyle::PM_ScrollBarExtent);
  if (_hScrollBar->maximum() > _hScrollBar->minimum()) h += sbw;
  if (_vScrollBar->maximum() > _vScrollBar->minimum()) w += sbw;
  if (!editor_flag) {
    if (xw < w) xw= w;
    if (xh < h) xh= h;
  }
  int cw = (xw > w ? xw - w + sbw : 0);
  if (_hScrollBar->sliderPosition() > cw)
    _hScrollBar->setSliderPosition(cw);
  _hScrollBar->setRange(0, cw);
  _hScrollBar->setSingleStep((w >> 4) + 1);
  _hScrollBar->setPageStep(w);
  
  int ch = (xh > h ? xh - h + sbw : 0);
  if (_vScrollBar->sliderPosition() > ch)
    _vScrollBar->setSliderPosition(ch);
  _vScrollBar->setRange(0, ch);
  _vScrollBar->setSingleStep((h >> 4) + 1);
  _vScrollBar->setPageStep(h);
}

/*! Scroll area updater */
void
QTMScrollView::scrollContentsBy ( int dx, int dy ) {
  if (dx) p_origin.setX(p_origin.x() - dx);
  if (dy) p_origin.setY(p_origin.y() - dy);
}

bool
QTMScrollView::event (QEvent *event) {
  switch (event->type()) {
    case QEvent::Resize:
    {
      bool res = QAbstractScrollArea::event(event);
      QResizeEvent *re = static_cast<QResizeEvent*> (event);
      resizeEventBis (re);
      return res;
    }
    default:
      break;
  }
  return QAbstractScrollArea::event(event);
}


void
QTMScrollView::resizeEventBis (QResizeEvent *event) {
  (void) event;
}
 
