
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
  
public:

  QTMWidget (QWidget* _parent=0, qt_widget _tmwid=0);
  virtual ~QTMWidget ();
  
  virtual QSize	sizeHint () const;
  virtual void scrollContentsBy (int dx, int dy);

  void setCursorPos (QPoint pos) { cursor_pos = pos; }
  qt_simple_widget_rep* tm_widget () const;
  
protected:

  virtual bool event (QEvent *event);

  virtual void paintEvent (QPaintEvent* event);
  virtual void focusInEvent (QFocusEvent* event);
  virtual void focusOutEvent (QFocusEvent* event);
  virtual void keyPressEvent (QKeyEvent* event);
  virtual void kbdEvent (int key, Qt::KeyboardModifiers mods, const QString& s);
  virtual void inputMethodEvent (QInputMethodEvent* event);
  virtual void mousePressEvent (QMouseEvent* event);
  virtual void mouseReleaseEvent (QMouseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  //virtual void tabletEvent (QTabletEvent* event);
  virtual void resizeEvent (QResizeEvent *event);
  virtual void resizeEventBis (QResizeEvent *e);
  virtual void dragEnterEvent(QDragEnterEvent *event);
  //virtual void dragMoveEvent (QDragMoveEvent *event);
  virtual void dropEvent(QDropEvent *event);

  // void wheelEvent(QWheelEvent *event) override;
  // FIXME: Plugins/Qt/QTMWidget.hpp:61: error: expected ';' before 'override'
  virtual QVariant inputMethodQuery (Qt::InputMethodQuery query) const;

};

#endif // QTMWIDGET_HPP
