
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
#include <QBoxLayout>

#include <QPaintEvent>


/* Remarks on QTMScrollView
 *
 * The current structure of the central texmacs widget (the canvas) is the 
 * following: the canvas is a derived class of QTMScrollView which beign
 * a QAbstractScrollArea owns a central widget called the "viewport".
 * QAbstractScrollArea coordinate the viewport with the scrollbars and maintain
 * informations like the real extent of the working surface and the current 
 * origin which can be acted upon via the scrollbars. This setup has been 
 * augmented via another widget child of the viewport which we call the 
 * "surface". The only purpose of this widget is to provide automatic centering
 * of the working area inside the viewport. To support this we "un-wired" the
 * event redirection build-in in QAbstractScrollArea (from the viewport widget 
 * to the QAbstractScrollArea) and re-wired event redirection from the surface
 * to the QTMScrollView. All relevants events like resize, I/O events and the 
 * like which are sent to the surface are resent to the QTMScrollView for 
 * handling. This allow to concentrate all the logic in only one object.
 * See QTMSurface::event for info about the redirected events
 *
 */



class QTMSurface : public QWidget {
  QTMScrollView *sv;
public:
  QTMSurface(QWidget *parent) 
    : QWidget (parent), 
      sv (qobject_cast<QTMScrollView*>(parentWidget()->parentWidget())) 
  {}
protected:
  virtual bool event(QEvent *event) {
    return (sv && sv->surfaceEvent(event) ? true : QWidget::event(event));
  }  
};


QTMScrollView::QTMScrollView ( QWidget *_parent )
: QAbstractScrollArea (_parent), p_extents(QRect(0,0,0,0))  {

  QWidget *_viewport = QAbstractScrollArea::viewport();
  _viewport->setBackgroundRole(QPalette::Mid);
  _viewport->setAutoFillBackground(true);

  p_surface = new QTMSurface (_viewport);
  p_surface->setAttribute(Qt::WA_NoSystemBackground);
  p_surface->setAttribute(Qt::WA_StaticContents); 
  p_surface->setAttribute(Qt::WA_MacNoClickThrough);
  p_surface->setAutoFillBackground(false);
  p_surface->setBackgroundRole(QPalette::NoRole);
  p_surface->setAttribute(Qt::WA_OpaquePaintEvent);
  p_surface->setGeometry(_viewport->geometry());
  p_surface->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);

  QHBoxLayout *layout = new QHBoxLayout();
  layout->addWidget(p_surface, 0, Qt::AlignHCenter);
  layout->setContentsMargins(0,0,0,0);
  _viewport->setLayout(layout);
  
}


QWidget* QTMScrollView::surface() {
  return p_surface;
}

QTMScrollView::~QTMScrollView (void) { }

void 
QTMScrollView::setOrigin ( QPoint newOrigin ) {
  if (newOrigin.x() != p_origin.x()) {
    QAbstractScrollArea::horizontalScrollBar()->setSliderPosition(newOrigin.x());
  }
  if (newOrigin.y() != p_origin.y()) {
	QAbstractScrollArea::verticalScrollBar()->setSliderPosition(newOrigin.y());
  } 
}

void 
QTMScrollView::setExtents ( QRect newExtents ) {
  if (p_extents != newExtents) {
    p_extents = newExtents;
    if (p_extents.width() < 0) p_extents.setWidth(0);
    if (p_extents.height() < 0) p_extents.setHeight(0);
    updateScrollBars();
  }
}

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

void 
QTMScrollView::updateScrollBars (void) {
	QWidget *_viewport = QAbstractScrollArea::viewport();
	int w = _viewport->width()  ; // -2
	int h = _viewport->height() ; // -2
  
	QScrollBar *_hScrollBar = QAbstractScrollArea::horizontalScrollBar();
	int cw = (p_extents.width() > w ? p_extents.width() - w : 0);
	if (_hScrollBar->sliderPosition() > cw)
		_hScrollBar->setSliderPosition(cw);
	_hScrollBar->setRange(0, cw);
	_hScrollBar->setSingleStep((w >> 4) + 1);
	_hScrollBar->setPageStep(w);
  
  QRect r = (p_extents.width() > w) ? QRect(0,0,w,h) 
    : QRect ((w-p_extents.width())/2,0,p_extents.width(),h);
//  surface()->setGeometry(r);
  surface()->setMinimumWidth(r.width());
  
	QScrollBar *_vScrollBar = QAbstractScrollArea::verticalScrollBar();
	int ch = (p_extents.height() > h ? p_extents.height() - h : 0);
	if (_vScrollBar->sliderPosition() > ch)
		_vScrollBar->setSliderPosition(ch);
	_vScrollBar->setRange(0, ch);
	_vScrollBar->setSingleStep((h >> 4) + 1);
	_vScrollBar->setPageStep(h);
  
  
  // we may need a relayout if the surface width is changed
  updateGeometry();
}

#if 0
// this code is wrong
void
QTMScrollView::wheelEvent ( QWheelEvent *wheelEvent ) {
	if (wheelEvent->modifiers()
      & (Qt::ShiftModifier | Qt::ControlModifier)) {
		setOrigin(QPoint(p_origin.x() + wheelEvent->delta(), p_origin.y()));
	}
	else QAbstractScrollArea::wheelEvent(wheelEvent);
}
#endif

void
QTMScrollView::scrollContentsBy ( int dx, int dy ) {
	if (dx) p_origin.setX(p_origin.x() - dx);
	if (dy) p_origin.setY(p_origin.y() - dy);
}

bool 
QTMScrollView::viewportEvent(QEvent *e)
{
  switch (e->type()) {
    case QEvent::Resize:
    case QEvent::Paint:
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseButtonDblClick:
#if QT_VERSION >= 0x040600
    case QEvent::TouchBegin:
    case QEvent::TouchUpdate:
    case QEvent::TouchEnd:
#endif
    case QEvent::MouseMove:
    case QEvent::ContextMenu:
    case QEvent::Wheel:
    case QEvent::Drop:
    case QEvent::DragEnter:
    case QEvent::DragMove:
    case QEvent::DragLeave:
//      return QFrame::event(e);
      return false; // let the viewport widget handle the event
    case QEvent::LayoutRequest:
#if QT_VERSION >= 0x040600
#ifndef QT_NO_GESTURES
    case QEvent::Gesture:
    case QEvent::GestureOverride:
      return event(e);
#endif
#endif
    default:
      break;
  }
  return false; // let the viewport widget handle the event
}

bool 
QTMScrollView::surfaceEvent(QEvent *e)
{
  switch (e->type()) {
    case QEvent::Resize:
    case QEvent::Paint:
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseButtonDblClick:
#if QT_VERSION >= 0x040600
    case QEvent::TouchBegin:
    case QEvent::TouchUpdate:
    case QEvent::TouchEnd:
#endif
    case QEvent::MouseMove:
    case QEvent::ContextMenu:
    case QEvent::Wheel:
    case QEvent::Drop:
    case QEvent::DragEnter:
    case QEvent::DragMove:
    case QEvent::DragLeave:
      return QFrame::event(e);
    case QEvent::LayoutRequest:
#if QT_VERSION >= 0x040600
#ifndef QT_NO_GESTURES
    case QEvent::Gesture:
    case QEvent::GestureOverride:
      return event(e);
#endif
#endif
    default:
      break;
  }
  return false; // let the surface widget handle the event
}

bool QTMScrollView::event (QEvent *event) {
  switch (event->type()) {
    case QEvent::Resize:
    {
      bool res = QAbstractScrollArea::event(event);
      updateScrollBars();
      return res;
    }
    default:
      break;
  }
  return QAbstractScrollArea::event(event);
}

