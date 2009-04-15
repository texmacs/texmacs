
/******************************************************************************
* MODULE     : QTMWidget.hpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMWIDGET_HPP
#define QTMWIDGET_HPP

#include "qt_simple_widget.hpp" 
#include <QVariant>
#include <QWidget>

class QTMWidget: public QWidget {
  Q_OBJECT
  list<QRect> delayed_rects;

public:
  inline QTMWidget(simple_widget_rep *_wid): QWidget () { 
    setObjectName("A QTMWidget");
    setFocusPolicy (Qt::StrongFocus);
    // setBackgroundRole(QPalette::Window);
    // setAutoFillBackground(true);
    setAutoFillBackground(false);
    // setAttribute (Qt::WA_OpaquePaintEvent);
    setAttribute (Qt::WA_NoSystemBackground);
    setProperty ("texmacs_widget", QVariant::fromValue ((void*) _wid));
    setMouseTracking (true);
  }

  inline simple_widget_rep*
  tm_widget() { 
    QVariant v= property("texmacs_widget");
    return (simple_widget_rep *)
      (v.canConvert<void*> ()? v.value<void*> (): NULL);
  }

public slots:
  void postponedUpdate ();

protected:	
  virtual void paintEvent (QPaintEvent* event);
  virtual void focusInEvent (QFocusEvent* event);
  //virtual void focusOutEvent (QFocusEvent* event);
  virtual void keyPressEvent (QKeyEvent* event);
  virtual void mousePressEvent (QMouseEvent* event);
  virtual void mouseReleaseEvent (QMouseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  virtual bool event (QEvent *event);
};

#endif // QTMWIDGET_HPP
