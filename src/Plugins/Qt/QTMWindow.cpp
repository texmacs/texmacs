
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

#include <QCloseEvent>

void QTMPlainWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT) cout << "Close QTMPlainWindow" << LF;

  if (tmwid->ref_count != 0) {
    concrete(tmwid)->send (SLOT_DESTROY, NULL);
      // Tell QT not to close the window, qt_window_widget_rep will.
    event->ignore ();
  }
  emit closed();
}

void QTMWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT) cout << "Close QTMWindow" << LF;
	
  if (tmwid->ref_count != 0) {
    concrete(tmwid)->send (SLOT_DESTROY, NULL);
    event->ignore ();
  }
  emit closed();
}



  ////////////////////


QTMPopupWidget::QTMPopupWidget(QWidget* contents) {
  
  QHBoxLayout* l = new QHBoxLayout();
  l->addWidget(contents);
  l->setContentsMargins(0,0,0,0);
  setLayout(l);

  resize(contents->size());
  setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed); 
  
  setWindowFlags(Qt::Popup);
  setMouseTracking(true);
  
    //cout << "QTMPopupWidget created with size: " << size().width() 
    // << " x " << size().height() << LF;

}


  /// FIXME: this is intended for popups which appear under the cursor!
void
QTMPopupWidget::mouseMoveEvent(QMouseEvent* event) {
  
  if (! this->rect().contains(QCursor::pos()))
    this->hide();

  event->ignore();
  emit closed();
}


void
QTMPopupWidget::resizeEvent (QResizeEvent* event) {

    //cout << "QTMPopupWidget" << (event->spontaneous() ? " (spontaneous) " : " ") 
    // << "resizeEvent: " << event->size().width() 
    // << " x " << event->size().height() << LF;
  
  event->accept();
}
