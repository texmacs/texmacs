
/******************************************************************************
* MODULE     : qt_dialogues.hpp
* DESCRIPTION: Widgets for automatically created dialogues (questions in popups)
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_DIALOGUES_HPP
#define QT_DIALOGUES_HPP

#include "qt_widget.hpp"
#include "qt_utilities.hpp"

class QTMInputTextWidgetHelper;

class qt_input_text_widget_rep: public qt_widget_rep {
public:
  command cmd;
  string type;
  array<string> def;
  string text;
  int style;
  string width;
  
  QTMInputTextWidgetHelper *helper;
  bool ok;
  
  qt_input_text_widget_rep (command _cmd, string _type, array<string> _def, 
                            int _style, string _width);
  ~qt_input_text_widget_rep();
  
  QAction* as_qaction ();
  QLayoutItem *as_qlayoutitem ();
  QWidget *as_qwidget ();
};

class qt_field_widget;

class qt_input_widget_rep: public qt_widget_rep {
protected:
  command cmd;
  array<qt_field_widget> fields;
  coord2 size, position;
  string win_title;
  int style;
public:
  qt_input_widget_rep (command, array<string>);
  ~qt_input_widget_rep ();
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);

  virtual widget plain_window_widget (string s, command q);

  void perform_dialog();
};

class qt_field_widget_rep: public qt_widget_rep {
  string prompt;
  string input;
  string type;
  array<string> proposals;
  qt_input_widget_rep *parent;
public:
  qt_field_widget_rep(qt_input_widget_rep *_parent) :
    qt_widget_rep(), prompt(""), input(""),  proposals(), parent(_parent) { }
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);

  friend class qt_input_widget_rep;
};

class qt_field_widget {
public:
  ABSTRACT_NULL(qt_field_widget);
};

ABSTRACT_NULL_CODE(qt_field_widget);

#endif // defined QT_DIALOGUES_HPP
