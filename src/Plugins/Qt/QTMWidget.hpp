
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

#include "qt_widget.hpp"
#include "QTMScrollView.hpp"
#include <QLabel>
#include <QGesture>
#include <QGestureEvent>
#include <QScreen>

class qt_simple_widget_rep;

/*! The underlying QWidget for a qt_simple_widget_rep handles drawing for a 
    texmacs canvas, as well as keypresses, international input methods, etc.
 
 QTMWidget needs a valid qt_simple_widget_rep object to function properly, see
 set_tm_widget() for more on this.
 
 */
class QTMWidget: public QTMScrollView {
  Q_OBJECT

  qt_widget    tmwid;
  QLabel*      imwidget;
  QPoint       cursor_pos;
  bool         preediting;

public:

  
  QTMWidget (QWidget* _parent=0, qt_widget _tmwid=0);
  virtual ~QTMWidget ();
  virtual bool isEmbedded () const;
  
  virtual QSize	sizeHint () const override;
  virtual void scrollContentsBy (int dx, int dy) override;

  void setCursorPos (QPoint pos) { cursor_pos = pos; }
  qt_simple_widget_rep* tm_widget () const;
  
  bool isPreediting () { return preediting; }

#if QT_VERSION >= 0x060000
protected slots:
  void surfaceDprChanged ();
#endif

protected:

  virtual bool event (QEvent *event) override;

  void surfacePaintEvent (QPaintEvent *e, QWidget *surface) override;
  virtual void focusInEvent (QFocusEvent* event) override;
  virtual void focusOutEvent (QFocusEvent* event) override;
  virtual void keyPressEvent (QKeyEvent* event) override;
  virtual void kbdEvent (int key, Qt::KeyboardModifiers mods, const QString& s);
  virtual void inputMethodEvent (QInputMethodEvent* event) override;
  virtual void mousePressEvent (QMouseEvent* event) override;
  virtual void mouseReleaseEvent (QMouseEvent* event) override;
  virtual void mouseMoveEvent (QMouseEvent* event) override;
#if (QT_VERSION >= 0x050000)
  virtual void tabletEvent (QTabletEvent* event) override;
#endif
  virtual void gestureEvent (QGestureEvent* event);
  virtual void resizeEvent (QResizeEvent *event) override;
  virtual void resizeEventBis (QResizeEvent *e) override;
  virtual void dragEnterEvent(QDragEnterEvent *event) override;
  //virtual void dragMoveEvent (QDragMoveEvent *event) override;
  virtual void dropEvent(QDropEvent *event) override;

  virtual void wheelEvent(QWheelEvent *event) override;
  virtual QVariant inputMethodQuery (Qt::InputMethodQuery query) const override;

  void showEvent (QShowEvent *event) override;

};

#endif // QTMWIDGET_HPP
