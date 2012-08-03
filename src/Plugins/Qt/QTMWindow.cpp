
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
  l->setEnabled(false); // Tell the layout not to adjust itself !!
  setLayout(l);

  resize(contents->size());
  setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  setWindowFlags(Qt::Popup);
  setAttribute(Qt::WA_NoSystemBackground);
    //setWindowOpacity(0.9);
  setMouseTracking(true);
  
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
