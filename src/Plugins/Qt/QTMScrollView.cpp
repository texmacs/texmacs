
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
#include <QPainter>

#include <QPaintEvent>


QTMScrollView::QTMScrollView ( QWidget *_parent )
  : QAbstractScrollArea (_parent) {
	QWidget *_viewport = QAbstractScrollArea::viewport();
	_viewport->setAttribute(Qt::WA_OpaquePaintEvent);
    _viewport->setAttribute(Qt::WA_MacNoClickThrough);
	//_viewport->setAttribute(Qt::WA_StaticContents);
  //  _viewport->setAttribute(Qt::WA_PaintOnScreen); 
    // the above option disable double-buffering on X11
	_viewport->setAttribute(Qt::WA_NoSystemBackground);
	_viewport->setBackgroundRole(QPalette::NoRole);
	_viewport->setAutoFillBackground(false);
}

QTMScrollView::~QTMScrollView (void) { }

void 
QTMScrollView::setOrigin ( QPoint newOrigin ) {
  if (newOrigin.x() != origin.x()) {
    QAbstractScrollArea::horizontalScrollBar()->setSliderPosition(newOrigin.x());
  }
  if (newOrigin.y() != origin.y()) {
	QAbstractScrollArea::verticalScrollBar()->setSliderPosition(newOrigin.y());
  } 
}

void 
QTMScrollView::setExtents ( QRect newExtents ) {
  if (extents != newExtents) {
    extents = newExtents;
    if (extents.width() < 0) extents.setWidth(0);
    if (extents.height() < 0) extents.setHeight(0);
    updateScrollBars();
  }
}

void 
QTMScrollView::ensureVisible ( int cx, int cy, int mx, int my ) {
	QWidget *_viewport = QAbstractScrollArea::viewport();
	int w = _viewport->width();
	int h = _viewport->height();
  
	int dx = - origin.x();
	int dy = - origin.y();
	int cw = extents.width();
	int ch = extents.height();
  
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

void 
QTMScrollView::updateScrollBars (void) {
	QWidget *_viewport = QAbstractScrollArea::viewport();
	int w = _viewport->width()  ; // -2
	int h = _viewport->height() ; // -2
  
	QScrollBar *_hScrollBar = QAbstractScrollArea::horizontalScrollBar();
	int cw = (extents.width() > w ? extents.width() - w : 0);
	if (_hScrollBar->sliderPosition() > cw)
		_hScrollBar->setSliderPosition(cw);
	_hScrollBar->setRange(0, cw);
	_hScrollBar->setSingleStep((w >> 4) + 1);
	_hScrollBar->setPageStep(w);
  
	QScrollBar *_vScrollBar = QAbstractScrollArea::verticalScrollBar();
	int ch = (extents.height() > h ? extents.height() - h : 0);
	if (_vScrollBar->sliderPosition() > ch)
		_vScrollBar->setSliderPosition(ch);
	_vScrollBar->setRange(0, ch);
	_vScrollBar->setSingleStep((h >> 4) + 1);
	_vScrollBar->setPageStep(h);
}

void
QTMScrollView::resizeEvent ( QResizeEvent *_resizeEvent ) {
	QAbstractScrollArea::resizeEvent(_resizeEvent);
	updateScrollBars();
}

void
QTMScrollView::wheelEvent ( QWheelEvent *wheelEvent ) {
	if (wheelEvent->modifiers()
      & (Qt::ShiftModifier | Qt::ControlModifier)) {
		setOrigin(QPoint(origin.x() + wheelEvent->delta(), origin.y()));
	}
	else QAbstractScrollArea::wheelEvent(wheelEvent);
}

void
QTMScrollView::scrollContentsBy ( int dx, int dy ) {
	if (dx) origin.setX(origin.x() - dx);
	if (dy) origin.setY(origin.y() - dy);
}

void 
QTMScrollView::updateContents ( const QRect& rect ) {
  QAbstractScrollArea::viewport()->update(QRect(rect.topLeft() - origin, rect.size()));
}
