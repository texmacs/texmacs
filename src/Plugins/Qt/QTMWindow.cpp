
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
  if (DEBUG_QT) cout << "Close QTMPlainWindow" << LF;
  // Tell QT not to close the window, qt_window_widget_rep will if need be.
  event->ignore ();
  emit closed();
}

void QTMPlainWindow::moveEvent (QMoveEvent* event)
{
  string name= from_qstring (windowTitle ());
    // FIXME: rather use a slot for this
  int x= event->pos().x();
  int y= event->pos().y();
  if (DEBUG_QT)
    cout << "Move QTMPlainWindow " << name << ": " << x << ", " << y << LF;
  notify_window_move (name, x*PIXEL, -y*PIXEL);
  QWidget::moveEvent (event);
}

void QTMPlainWindow::resizeEvent (QResizeEvent* event)
{
  string name= from_qstring (windowTitle ());
    // FIXME: rather use a slot for this
  int w= event->size().width();
  int h= event->size().height();
  if (DEBUG_QT)
    cout << "Resize QTMPlainWindow " << name << ": " << w << ", " << h << LF;
  notify_window_resize (name, w*PIXEL, h*PIXEL);
  QWidget::resizeEvent (event);
}

/* We must basically do the same as QTMPlainWindow::closeEvent, but we
 choose the other way: send a SLOT_DESTROY, which for qt_tm_widgets just
 calls the command. This is not personal whim but a problem with the destruction
 which has to be investigated further.
 
 Also:
 FIXME! For some reason, not emitting closed(), which actually is supposed to
 execute the very same command, crashes TeXmacs upon exit.
 */
void QTMWindow::closeEvent (QCloseEvent* event)
{
  string name= concrete(tmwid)->get_nickname ();
  if (DEBUG_QT) cout << "Close QTMWindow " << name << LF;
  if (tmwid->ref_count != 0) {
    concrete(tmwid)->send (SLOT_DESTROY, NULL);
    event->ignore ();
    notify_window_destroy (name);
  }
  emit closed();
}

void QTMWindow::moveEvent (QMoveEvent * event)
{
  string name= concrete(tmwid)->get_nickname ();
  QPoint p = event->pos();
  // FIXME: rather use a slot for this
  if (DEBUG_QT)
    cout << "Move QTMWindow " << name << ": " << p.x() << ", " << p.y() << LF;
  coord2 pt = from_qpoint(p);
  notify_window_move (name, pt.x1, pt.x2);
  QMainWindow::moveEvent (event);
}

void QTMWindow::resizeEvent (QResizeEvent * event)
{
  string name= concrete(tmwid)->get_nickname ();
  // FIXME: rather use a slot for this
  QSize s = event->size();
  if (DEBUG_QT)
      cout << "Resize QTMWindow " << name << ": " << s.width() << ", " << s.height() << LF;
  coord2 sz = from_qsize(s);
  notify_window_resize (name, sz.x1, sz.x2);
  QMainWindow::resizeEvent (event);
}

  ////////////////////


QTMPopupWidget::QTMPopupWidget(QWidget* contents) {
  
  QHBoxLayout* l = new QHBoxLayout();
  l->addWidget(contents);
  l->setContentsMargins(0,0,0,0);
  l->setEnabled(false);   // Tell the layout not to adjust itself (!)
  setLayout(l);

  resize(contents->size());
  setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  setWindowFlags(Qt::Popup);
  setAttribute(Qt::WA_NoSystemBackground);
  setMouseTracking(true);            // Receive mouse events
  setFocusPolicy(Qt::StrongFocus);   // Receive key events
    //setWindowOpacity(0.9);

    //cout << "QTMPopupWidget created with size: " << size().width() 
    // << " x " << size().height() << LF;
}


/*
 If our contents QWidget is of type QTMWidget it will capture mouse events
 and we won't get called until the cursor exits the contents, so the check 
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
