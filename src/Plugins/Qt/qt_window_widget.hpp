
/******************************************************************************
* MODULE     : qt_window_widget.hpp
* DESCRIPTION: QT window widget.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_WINDOW_WIDGET_HPP
#define QT_WINDOW_WIDGET_HPP

#include "qt_widget.hpp"

class QWidget;


/*! Wrapper for QTMWindow, TeXmacs' QMainWindow.
 *
 */
class qt_window_widget_rep: public widget_rep {
public:
  QWidget *wid;
  command quit;    // Ignored
  
  qt_window_widget_rep (QWidget* _wid, command q);
  ~qt_window_widget_rep ();

  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
};

#endif // defined QT_WINDOW_WIDGET_HPP
