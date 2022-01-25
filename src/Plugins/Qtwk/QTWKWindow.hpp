
/******************************************************************************
* MODULE     : QTWKWindow.hpp
* DESCRIPTION: QT/Widkit Texmacs window class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTWKWINDOW_HPP
#define QTWKWINDOW_HPP

#include "qtwk_window.hpp"
#include <QLabel>

class qtwk_window_rep;

/*! The underlying QWidget for a qt_simple_widget_rep handles drawing for a 
    texmacs canvas, as well as keypresses, international input methods, etc.
 
 QTWKWindow needs a valid qt_simple_widget_rep object to function properly, see
 set_tm_widget() for more on this.
 
 */
class QTWKWindow: public QWidget {
  Q_OBJECT

  qtwk_window  tmwid;
  QLabel*      imwidget;
  QPoint       cursor_pos;
  
public:

  QTWKWindow (QWidget* _parent=0, qtwk_window _tmwid=0);
  virtual ~QTWKWindow ();
  
//  virtual QSize	sizeHint () const;
//  virtual void scrollContentsBy (int dx, int dy);

  void setCursorPos (QPoint pos) { cursor_pos = pos; }
  widget tm_widget () const;
  
protected:

  virtual bool event (QEvent *event);

  virtual void paintEvent (QPaintEvent* event);
  virtual void focusInEvent (QFocusEvent* event);
  virtual void focusOutEvent (QFocusEvent* event);
  virtual void keyPressEvent (QKeyEvent* event);
  virtual void inputMethodEvent (QInputMethodEvent* event);
  virtual void mousePressEvent (QMouseEvent* event);
  virtual void mouseReleaseEvent (QMouseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  virtual void resizeEvent (QResizeEvent *event);
//  virtual void resizeEventBis (QResizeEvent *e);
  virtual void dragEnterEvent (QDragEnterEvent *event);
  //virtual void dragMoveEvent (QDragMoveEvent *event);
  virtual void dropEvent (QDropEvent *event);
  virtual void wheelEvent (QWheelEvent* event);
  virtual void enterEvent (QEvent* event);
  virtual void leaveEvent (QEvent* event);

  // void wheelEvent(QWheelEvent *event) override;
  // FIXME: Plugins/Qt/QTWKWindow.hpp:61: error: expected ';' before 'override'
  virtual QVariant inputMethodQuery (Qt::InputMethodQuery query) const;

};


/*! The underlying QWidget for a qt_popup_widget.
 
 This is just a container QWidget that disappears after the mouse leaves it.
 As usual, it takes ownership of its contents.
 */
class QTMPopupWindow : public QTWKWindow {
  Q_OBJECT
  
public:
  QTMPopupWindow (QWidget* _parent=0, qtwk_window _tmwid=0);

signals:
  void closed();
  
protected:
  virtual void closeEvent (QCloseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  virtual void keyPressEvent  (QKeyEvent* event);
};

#endif // QTWKWINDOW_HPP
