
/******************************************************************************
 * MODULE     : qt_color_picker_widget.hpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_COLOR_PICKER_WIDGET_HPP
#define QT_COLOR_PICKER_WIDGET_HPP

#include "qt_widget.hpp"

#include <QColor>

/**
 * This implements a color picker widget, using the native dialogs where
 * available.
 *
 * The "factory" function for this widget is called color_picker_widget(), 
 * in qt_dialogues.cpp
 *
 * Please @see qt_widget_rep for some important info.
 */ 
class qt_color_picker_widget_rep: public qt_widget_rep { 
public:
  qt_color_picker_widget_rep (command, bool, array<tree>);
  ~qt_color_picker_widget_rep ();
  
  virtual void            send (slot s, blackbox val);
  widget   plain_window_widget (string s, command q);

  void showDialog();
  
protected:
  string            _windowTitle;
  command _commandAfterExecution;
  bool              _pickPattern;
};


#endif    // QT_COLOR_PICKER_WIDGET_HPP
