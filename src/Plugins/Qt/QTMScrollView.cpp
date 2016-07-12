
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

#include <QBoxLayout>
#include <QEvent>

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
  QAbstractScrollArea (_parent)

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

