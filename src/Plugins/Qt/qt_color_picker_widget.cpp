
/******************************************************************************
 * MODULE     : qt_color_picker_widget.cpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_color_picker_widget.hpp"
#include "qt_utilities.hpp"

#include "message.hpp"
#include "scheme.hpp"

#include <QColorDialog>


/**
 * Needed for whitebox_rep::display
 */
inline tm_ostream& 
operator << (tm_ostream& out, const QColor& col) {
  return out << "Color: " << from_qcolor (col) << "\n";
}

qt_color_picker_widget_rep::qt_color_picker_widget_rep 
  (command call_back, bool pickPattern, array<tree> proposals)
: _commandAfterExecution(call_back), _pickPattern(pickPattern)
{
  (void) proposals;
}

qt_color_picker_widget_rep::~qt_color_picker_widget_rep() { }

void
qt_color_picker_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_color_picker_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_VISIBILITY:   // Activates the widget
      check_type<bool>(val, s);
      if (open_box<bool>(val) == true)
        showDialog();
      break;
    default:
      qt_widget_rep::send (s, val);
  }
}

/*!
 window_create() expects this method in widgets which implement windows
 @note: name is a unique identifier for the window, but for this widget we
 identify it with the window title. This is not always the case.
 */
widget
qt_color_picker_widget_rep::plain_window_widget (string name, command q, int b) {
  (void) b; (void) q;
  _windowTitle = name;
  return this;
}

void
qt_color_picker_widget_rep::showDialog() {
  if (_pickPattern) {
    // do stuff
  } else {
#if 0 //(QT_VERSION >= 0x040500)
    QColor _sel = QColorDialog::getColor(Qt::white, 0, to_qstring(_windowTitle));
#else
    QColor _sel = QColorDialog::getColor(Qt::white);
#endif
    if(_sel.isValid()) {
      _commandAfterExecution (list_object (object (tree (from_qcolor (_sel)))));
    }
  }
}

