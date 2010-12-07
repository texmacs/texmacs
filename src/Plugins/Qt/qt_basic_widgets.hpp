
/******************************************************************************
* MODULE     : qt_basic_widgets.h
* DESCRIPTION: Basic widgets
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_BASIC_WIDGETS_HPP
#define QT_BASIC_WIDGETS_HPP
#include "qt_widget.hpp"

class QTMInputTextWidgetHelper;

class qt_input_text_widget_rep: public qt_widget_rep {
public:
  command cmd;
  string type;
  array<string> def;
  string text;
  string width;
  
  QTMInputTextWidgetHelper *helper;
  
  qt_input_text_widget_rep (command _cmd, string _type, array<string> _def, string _width);
  ~qt_input_text_widget_rep();

  virtual QAction* as_qaction ();
};

#endif // QT_BASIC_WIDGETS_HPP
