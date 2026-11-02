/******************************************************************************
* MODULE     : QTMWindow.cpp
* DESCRIPTION: classes for top level windows
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWindow.hpp"
#include "QTMMainTabWindow.hpp"
#include "window.hpp"
#include "qt_utilities.hpp"
#include "qt_window_widget.hpp"
#include <QCloseEvent>
#include <QHBoxLayout>
#include <QTimer>
#include <QDebug>

#if QT_VERSION < 0x060000

void QTMPlainWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT_WIDGETS) debug_widgets << "Close QTMPlainWindow" << LF;
  event->ignore ();
  emit closed();
}

void QTMPlainWindow::moveEvent (QMoveEvent* event)
{
  string name= from_qstring (windowTitle ());
  coord2 pos= from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pos.x1, pos.x2);
  QWidget::moveEvent (event);
}

void QTMPlainWindow::resizeEvent (QResizeEvent* event)
{
  string name= from_qstring (windowTitle ());
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
#if defined(OS_MACOS)
  notify_window_destroy (name);
#endif
  emit closed();
}

void QTMWindow::moveEvent (QMoveEvent * event)
{
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  coord2 pt = from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pt.x1, pt.x2);
  QMainWindow::moveEvent (event);
}

void QTMWindow::resizeEvent (QResizeEvent * event)
{
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  coord2 sz = from_qsize (frameSize());
  notify_window_resize (name, sz.x1, sz.x2);
  QMainWindow::resizeEvent (event);
}

QTMPopupWidget::QTMPopupWidget(QWidget* contents) {
  QHBoxLayout* l = new QHBoxLayout();
  l->addWidget (contents);
  l->setContentsMargins (0,0,0,0);
  l->setEnabled (false);
  setLayout (l);

  resize (contents->size());
  setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
  setWindowFlags (Qt::Popup);
  setAttribute (Qt::WA_NoSystemBackground);
  setMouseTracking (true);
}

void
QTMPopupWidget::mouseMoveEvent(QMouseEvent* event) {
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
  event->ignore ();
  emit closed();
}

#else // QT_VERSION >= 0x060000

void 
QTMPlainWindow::closeEvent (QCloseEvent* event) {
  event->ignore ();
  emit requestClose();
}

void 
QTMPlainWindow::moveEvent (QMoveEvent* event) {
  string name= from_qstring (windowTitle());
  coord2 pos= from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pos.x1, pos.x2);
  QMainWindow::moveEvent (event);
}

void 
QTMPlainWindow::resizeEvent (QResizeEvent* event) {
  string name= from_qstring (windowTitle());
  coord2 sz= from_qsize (frameSize());
  notify_window_resize (name, sz.x1, sz.x2);
  QMainWindow::resizeEvent (event);
}

void 
QTMPlainWindow::showEvent (QShowEvent* event) {
  QMainWindow::showEvent (event);
  if (parentTabWindow () == nullptr) {
    if (centralWidget ()) {
      if (centralWidget ()->layout ()) {
        centralWidget ()->layout ()->activate ();
      }
    }
  }
}

void 
QTMWindow::closeEvent (QCloseEvent* event) {
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  if (DEBUG_QT_WIDGETS) debug_widgets << "Close QTMWindow " << name << LF;
  event->ignore ();
#if defined(OS_MACOS)
  notify_window_destroy (name);
#endif
  emit requestClose();
}

void 
QTMWindow::moveEvent (QMoveEvent* event) {
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  coord2 pos= from_qpoint (frameGeometry().topLeft());
  notify_window_move (name, pos.x1, pos.x2);
  QMainWindow::moveEvent (event);
}

void 
QTMWindow::resizeEvent (QResizeEvent* event) {
  widget tmwid = qt_window_widget_rep::widget_from_qwidget(this);
  string name= ( !is_nil(tmwid) ? concrete(tmwid)->get_nickname () : "QTMWindow");
  coord2 sz= from_qsize (frameSize());
  notify_window_resize (name, sz.x1, sz.x2);
  QMainWindow::resizeEvent (event);
}

void 
QTMWindow::showEvent (QShowEvent* event) {
  QMainWindow::showEvent (event);
}

QTMPopupWidget::QTMPopupWidget(QWidget* contents) {
  QWidget* central = new QWidget(this);
  QHBoxLayout* l = new QHBoxLayout(central);
  l->addWidget(contents);
  l->setContentsMargins(0, 0, 0, 0);
  l->setEnabled(false);
  setCentralWidget(central);

  resize (contents->size());
  setWindowFlags (Qt::Popup);
  setMouseTracking (true);
  installEventFilter (this);
}

void 
QTMPopupWidget::mouseMoveEvent (QMouseEvent* event) {
  (void) event;
  if (! this->rect().contains(QCursor::pos())) {
    hide();
    emit requestClose();
  }
}

void 
QTMPopupWidget::keyPressEvent (QKeyEvent* event) {
  (void) event;
  hide();
  emit requestClose();
}

void 
QTMPopupWidget::closeEvent (QCloseEvent* event) {
  event->ignore ();
  emit requestClose();
}

#endif // QT_VERSION >= 0x060000

