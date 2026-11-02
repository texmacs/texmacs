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

#include <QtGlobal>

#if QT_VERSION >= 0x060000
#include "QTMMainTab.hpp"
#endif

#include <QWidget>
#include <QMainWindow>
#include "basic.hpp"

#if QT_VERSION < 0x060000

/*! The underlying QWidget for a qt_simple_widget_rep. */
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

/*! The underlying QWidget for a qt_tm_widget_rep. */
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

/*! The underlying QWidget for a qt_popup_widget. */
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

#else // QT_VERSION >= 0x060000

/*! The underlying QWidget for a qt_simple_widget_rep. */
class QTMPlainWindow : public QTMMainTab {
  Q_OBJECT
  
public:
  QTMPlainWindow (QWidget* parent) 
  : QTMMainTab (parent) { 
    if (DEBUG_QT) debug_qt << "Creating QTMPlainWindow" << LF;
  }
  virtual ~QTMPlainWindow () {
    if (DEBUG_QT) debug_qt << "Deleting QTMPlainWindow" << LF;
  }
  
protected:
  virtual void  closeEvent (QCloseEvent* event) override;
  virtual void   moveEvent (QMoveEvent* event) override;
  virtual void resizeEvent (QResizeEvent* event) override;
  virtual void   showEvent (QShowEvent* event) override;
};

/*! The underlying QWidget for a qt_tm_widget_rep. */
class QTMWindow: public QTMMainTab {
  Q_OBJECT
  
public:
  QTMWindow (QWidget* parent)
  : QTMMainTab (parent) { 
    if (DEBUG_QT) debug_qt << "Creating QTMWindow" << LF;
  }
  virtual ~QTMWindow () {
    if (DEBUG_QT) debug_qt << "Deleting QTMWindow" << LF;
  }
  
protected:
  virtual void  closeEvent (QCloseEvent* event) override;
  virtual void   moveEvent (QMoveEvent* event) override;
  virtual void resizeEvent (QResizeEvent* event) override;
  virtual void   showEvent (QShowEvent* event) override;
};

/*! The underlying QWidget for a qt_popup_widget. */
class QTMPopupWidget : public QTMMainTab {
  Q_OBJECT
  
public:
  QTMPopupWidget (QWidget* contents);
  
protected:
  virtual void closeEvent (QCloseEvent* event) override;
  virtual void mouseMoveEvent (QMouseEvent* event) override;
  virtual void keyPressEvent  (QKeyEvent* event) override;
};

#endif // QT_VERSION >= 0x060000

#endif // QTMWINDOW_HPP

