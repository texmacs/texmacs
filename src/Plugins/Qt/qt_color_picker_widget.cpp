
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
#include "Scheme/object.hpp"

#include <QColorDialog>


/**
 * Needed for whitebox_rep::display
 */
inline tm_ostream& 
operator<< (tm_ostream& out, const QColor& col) {
  return out << "Color: " << col << "\n";
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
  if (DEBUG_QT)
    cout << "qt_color_picker_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_VISIBILITY:   // Activates the widget
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool>(val) == true)
        showDialog();
      break;
    default:
      qt_widget_rep::send (s, val);
  }
}

/**
 * window_create() expects this method in widgets which implement windows
 */
widget
qt_color_picker_widget_rep::plain_window_widget (string title)
{
  _windowTitle = title;
  return this;
}

// TEMP HACK
#define SLOT_CURRENT_COLOR -42

blackbox
qt_color_picker_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_color_picker_widget_rep::query " << slot_name(s) << LF;
  if (s != SLOT_CURRENT_COLOR)
    return qt_widget_rep::query (s, type_id);
  else {
    // TODO: return currently selected color in a live dialog.
    TYPE_CHECK (type_id == type_helper<QColor>::id);
    return close_box<QColor>(Qt::white);
  }
}

void
qt_color_picker_widget_rep::showDialog() {
  if (_pickPattern) {
    // do stuff
  } else {
    QColor _sel = QColorDialog::getColor(Qt::white, 0, to_qstring(_windowTitle));
    if(_sel.isValid()) {
      _commandAfterExecution (list_object (object (tree (from_qcolor (_sel)))));
    }
  }
}

