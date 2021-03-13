
/******************************************************************************
 * MODULE     : QTMScrollView.cpp
 * DESCRIPTION: QT Texmacs abstract scroll view widget
 * COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "message.hpp"

#include "QTMScrollView.hpp"

#include <QScrollBar>
#include <QPainter>
#include <QBoxLayout>
#include <QPaintEvent>
#include <QStyle>
#include <QApplication>


/*! Provide automatic centering of the working area inside the viewport.
 
 The only purpose of this widget is to provide this centering. To support this
 we "un-wired" the event redirection built-in in QAbstractScrollArea (from the
 viewport widget to the QAbstractScrollArea) and re-wired event redirection
 from the surface to the QTMScrollView (see event())
 
 All relevant events like resize, I/O events and the like which are sent to the
 surface are resent QTMScrollView::surfaceEvent() for handling. This allows to
 concentrate all the logic in only one object.
 */
class QTMSurface : public QWidget {
    
    QTMScrollView* sv;
public:
    QTMSurface(QWidget* p, QTMScrollView* _sv) : QWidget (p), sv (_sv) { }
    
protected:
    virtual bool event(QEvent *event) {
        return sv->surfaceEvent(event) ? true : QWidget::event(event);
    }  
};

/*! Constructor.
 
 NOTE:
 We tell the p_surface to use all available space by default (this is needed by 
 embedded widgets) by setting the SizePolicy to (Expanding, Expanding).
 In order to draw the vertical margins around the working area use a horizontal
 policy of Fixedl, as in qt_tm_widget_rep (see SLOT_SCROLLABLE there)
 
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
  _viewport->setBackgroundRole(QPalette::Mid);
  _viewport->setAutoFillBackground(true);
  setFrameShape(QFrame::NoFrame);

  p_surface = new QTMSurface (_viewport, this);
  p_surface->setAttribute(Qt::WA_NoSystemBackground);
  p_surface->setAttribute(Qt::WA_StaticContents); 
  p_surface->setAttribute(Qt::WA_MacNoClickThrough);
  p_surface->setAutoFillBackground(false);
  p_surface->setBackgroundRole(QPalette::NoRole);
  p_surface->setAttribute(Qt::WA_OpaquePaintEvent);
  p_surface->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  
  QHBoxLayout *layout = new QHBoxLayout();
  layout->addWidget(p_surface, 0, Qt::AlignHCenter | Qt::AlignVCenter);
  layout->setContentsMargins(0,0,0,0);
  _viewport->setLayout(layout);
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

  int xw = p_extents.width();
  int xh = p_extents.height();
  int w  = _viewport->width() ; // -2
  int h  = _viewport->height(); // -2
  int sbw= qApp->style()->pixelMetric (QStyle::PM_ScrollBarExtent);
  if (_hScrollBar->maximum() > _hScrollBar->minimum()) h += sbw;
  if (_vScrollBar->maximum() > _vScrollBar->minimum()) w += sbw;
  if (xw > w) h -= sbw;
  if (xh > h) w -= sbw;
  if (!editor_flag) {
    if (xw < w) xw= w;
    if (xh < h) xh= h;
  }

  int cw = (xw > w ? xw - w : 0);
  if (_hScrollBar->sliderPosition() > cw)
    _hScrollBar->setSliderPosition(cw);
  _hScrollBar->setRange(0, cw);
  _hScrollBar->setSingleStep((w >> 4) + 1);
  _hScrollBar->setPageStep(w);
  
  int ch = (xh > h ? xh - h : 0);
  if (_vScrollBar->sliderPosition() > ch)
    _vScrollBar->setSliderPosition(ch);
  _vScrollBar->setRange(0, ch);
  _vScrollBar->setSingleStep((h >> 4) + 1);
  _vScrollBar->setPageStep(h);
  
  surface()->setMinimumWidth (w < xw? w: xw);
  surface()->setMinimumHeight(h < xh? h: xh);
  
  // we may need a relayout if the surface width is changed
  updateGeometry();
}

/*! Scroll area updater */
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

bool
QTMScrollView::event (QEvent *event) {
  switch (event->type()) {
    case QEvent::Resize:
    {
      bool res = QAbstractScrollArea::event(event);
      QResizeEvent *re = static_cast<QResizeEvent*> (event);
      updateScrollBars();
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

/*
 // this code is wrong
 void
 QTMScrollView::wheelEvent ( QWheelEvent *wheelEvent ) {
 if (wheelEvent->modifiers()
 & (Qt::ShiftModifier | Qt::ControlModifier)) {
 setOrigin(QPoint(p_origin.x() + wheelEvent->delta(), p_origin.y()));
 }
 else QAbstractScrollArea::wheelEvent(wheelEvent);
 }
 */
