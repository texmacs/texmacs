
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
class QTMFieldWidgetHelper;
class qt_tm_widget_rep;

/*! A text input field with autocompletion.
 */
class qt_input_text_widget_rep: public qt_widget_rep {
protected:
  command             cmd;
  string             type;
  array<string> proposals;
  string            input;
  int               style;
  string            width;
  bool                 ok;

  bool         done;  //!< Has the command been executed after a modification?

public:
  qt_input_text_widget_rep (command _cmd, string _type, array<string> _proposals,
                            int _style, string _width);

  virtual QAction*  as_qaction ();
  virtual QWidget*  as_qwidget ();
  
  void commit(bool ok);
  
  friend class QTMInputTextWidgetHelper;
  friend class QTMInteractiveInputHelper;
  friend class qt_tm_widget_rep;
};

class qt_field_widget_rep;

/*! A dialog with a list of inputs and ok and cancel buttons.
 
 In the general case each input is a qt_field_widget_rep which we lay out in a
 vertical table. However, for simple yes/no/cancel questions we try to use a
 system default dialog 
 
 TODO?
 We try to use OS dialogs whenever possible, but this still needs improvement.
 We should also use a custom Qt widget and then bundle it in a modal window if
 required, so as to eventually be able to return something embeddable in 
 as_qwidget(), in case we want to reuse this.
 */
class qt_inputs_list_widget_rep: public qt_widget_rep {
protected:
  command cmd;
  coord2 size, position;
  string win_title;
  int style;

public:
  qt_inputs_list_widget_rep (command, array<string>);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  virtual widget plain_window_widget (string s, command q);
  
protected:
  void perform_dialog();
  qt_field_widget_rep* field (int i);
};

/*! Each of the fields in a qt_inputs_list_widget_rep.
 
 Each field is composed of a prompt (a label) and an input (a QTMComboBox).
 */
class qt_field_widget_rep: public qt_widget_rep {
  string           prompt;
  string            input;
  string             type;
  array<string> proposals;
  qt_inputs_list_widget_rep* parent;

public:
  qt_field_widget_rep (qt_inputs_list_widget_rep* _parent, string _prompt);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);

  virtual QWidget* as_qwidget ();

  friend class qt_inputs_list_widget_rep;
  friend class QTMFieldWidgetHelper;
};

#endif // defined QT_DIALOGUES_HPP
