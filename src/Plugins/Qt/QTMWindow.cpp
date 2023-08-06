
/******************************************************************************
* MODULE     : QTMWindow.cpp
* DESCRIPTION: QT Texmacs window class
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWindow.hpp"
#include "window.hpp"
#include "qt_utilities.hpp"

#include <QCloseEvent>

void QTMPlainWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT_WIDGETS) debug_widgets << "Close QTMPlainWindow" << LF;
  // Tell QT not to close the window, qt_window_widget_rep will if need be.
  event->ignore ();
  emit closed();
}

void QTMPlainWindow::moveEvent (QMoveEvent* event)
{
  string name= from_qstring (windowTitle ());
    // FIXME: rather use a slot for this
  coord2 pos= from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pos.x1, pos.x2);
  QWidget::moveEvent (event);
}

void QTMPlainWindow::resizeEvent (QResizeEvent* event)
{
  string name= from_qstring (windowTitle ());
    // FIXME: rather use a slot for this
  coord2 sz= from_qsize (frameSize());
  notify_window_resize (name, sz.x1, sz.x2);
  QWidget::resizeEvent (event);
}

void QTMWindow::closeEvent (QCloseEvent* event)
{
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  if (DEBUG_QT_WIDGETS) debug_widgets << "Close QTMWindow " << name << LF;
  event->ignore ();
#if defined(OS_MACOS) //&& (QT_VERSION < 0x050000)
  notify_window_destroy (name);
  // this caused bug 61884, closing can still be cancelled
#endif
  emit closed();
}

void QTMWindow::moveEvent (QMoveEvent * event)
{
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  // FIXME: rather use a slot for this
  coord2 pt = from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pt.x1, pt.x2);
  QMainWindow::moveEvent (event);
}

void QTMWindow::resizeEvent (QResizeEvent * event)
{
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  // FIXME: rather use a slot for this
  coord2 sz = from_qsize (frameSize());
  notify_window_resize (name, sz.x1, sz.x2);
  QMainWindow::resizeEvent (event);
}

  ////////////////////


QTMPopupWidget::QTMPopupWidget(QWidget* contents) {
  
  QHBoxLayout* l = new QHBoxLayout();
  l->addWidget (contents);
  l->setContentsMargins (0,0,0,0);
  l->setEnabled (false);   // Tell the layout not to adjust itself (!)
  setLayout (l);

  resize (contents->size());
  setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
  setWindowFlags (Qt::Popup);
  setAttribute (Qt::WA_NoSystemBackground);
  setMouseTracking (true);            // Receive mouse events
//  setFocusPolicy(Qt::StrongFocus);   // Don't! Receive key events
//  setWindowOpacity(0.9);

    //cout << "QTMPopupWidget created with size: " << size().width()
    // << " x " << size().height() << LF;
}


/*
 If our contents QWidget is of type QTMWidget it will capture mouse events
 and we won't get called until the pointer exits the contents, so the check 
 inside is unnecessary unless the contents are of another kind.
 
 NOTE that this is intended for popups which appear under the cursor!
 */
void
QTMPopupWidget::mouseMoveEvent(QMouseEvent* event) {
  
  /* It'd be nice to have something like this...
  if (! drawArea().contains(event->globalPos())) {
    hide();
    emit closed();
  } else {
    move(event->globalPos());
  }
   */

  if (! this->rect().contains(QCursor::pos())) {
    hide();
    emit closed();
  }

  event->ignore();
}

void
QTMPopupWidget::keyPressEvent(QKeyEvent* event) {
  (void) event;
  hide();
  emit closed();
}

void
QTMPopupWidget::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT_WIDGETS) debug_widgets << "Close QTMPopupWidget" << LF;
  // Tell QT not to close the window, qt_window_widget_rep will if need be.
  event->ignore ();
  emit closed();
}

