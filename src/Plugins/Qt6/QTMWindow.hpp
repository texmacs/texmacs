
/******************************************************************************
 * MODULE     : QTMWindow.hpp
 * DESCRIPTION: QT Texmacs window class
 * COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMWINDOW_HPP
#define QTMWINDOW_HPP

#include <QMainWindow>

#include "qt_tm_widget.hpp"


/*! The interface that any QWidget must implement to become a window in TeXmacs.
 
 The underlying QWidget for a qt_window_widget_rep is of this type. We use it
 to inform the texmacs widget that the window has been closed. Because windows
 can be closed externally to TeXmacs, i.e. with the close button that the
 window manager provides, we must handle QCloseEvent and send the appropriate
 texmacs message to the owning texmacs widget (which is of type
 qt_window_widget_rep). This will allow it to call the associated texmacs
 quit command (and do anything else it requires).
 
 FIXME: Currently, we are bypassing this mechanism using QObject::connect()
 inside qt_window_widget's constructor. However, for QTMWindows, we do use it.
 
 NOTE: I would like to have a base class common to QTMPlainWindow and QTMWindow
 to enforce the data type for a qt_window_widget. But we cannot "virtual
 inherit" QWidget to do this in a nicer way because QMainWindow does not
 inherit virtually from QWidget.
 */
class QTMPlainWindow : public QWidget {
  Q_OBJECT
  
public:

  QTMPlainWindow (QWidget* parent) 
  : QWidget (parent) { 
    if (DEBUG_QT) debug_qt << "Creating QTMPlainWindow" << LF;
  }
  virtual ~QTMPlainWindow () {
    if (DEBUG_QT) debug_qt << "Deleting QTMPlainWindow" << LF;
  }
  
signals:
  void closed ();
  
protected:
  virtual void  closeEvent (QCloseEvent* event);
  virtual void   moveEvent (QMoveEvent* event);
  virtual void resizeEvent (QResizeEvent* event);

};

/*! The underlying QWidget for a qt_tm_widget_rep.
 
 \sa QTMPlainWindow
 
 */
class QTMWindow: public QMainWindow {
  Q_OBJECT
  
public:
  
  QTMWindow (QWidget* parent)
  : QMainWindow (parent) { 
    if (DEBUG_QT) debug_qt << "Creating QTMWindow" << LF;
  }
  virtual ~QTMWindow () {
    if (DEBUG_QT) debug_qt << "Deleting QTMWindow" << LF;
  }
  
signals:
  void closed ();
  
protected:
  virtual void  closeEvent (QCloseEvent* event);
  virtual void   moveEvent (QMoveEvent* event);
  virtual void resizeEvent (QResizeEvent* event);
};


/*! The underlying QWidget for a qt_popup_widget.
 
 This is just a container QWidget that disappears after the mouse leaves it.
 As usual, it takes ownership of its contents.
 */
class QTMPopupWidget : public QWidget {
  Q_OBJECT
  
public:
  QTMPopupWidget (QWidget* contents);

signals:
  void closed();
  
protected:
  virtual void closeEvent (QCloseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  virtual void keyPressEvent  (QKeyEvent* event);
};


#endif // QTMWINDOW_HPP
