
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
  
  void commit (bool ok);
  
  friend class QTMInputTextWidgetHelper;
  friend class QTMInteractiveInputHelper;
  friend class qt_tm_widget_rep;
};

#endif // defined QT_DIALOGUES_HPP
