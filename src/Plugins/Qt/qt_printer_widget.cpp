
/******************************************************************************
 * MODULE     : qt_printer_widget.cpp
 * DESCRIPTION: A dialog to manage printing of the document. All printing
 *              options set by the user at this stage are applied as a
 *              postprocessing of an already typeset postscript document.
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_printer_widget.hpp"
#include "qt_utilities.hpp"      // TYPE_CHECK and NOT_IMPLEMENTED
#include "message.hpp"           // slot definitions
#include "qt_sys_utils.hpp"      // qt_system(string)
#include "QTMPrintDialog.hpp"
#include "QTMPrinterSettings.hpp"

QTMPrinterSettings* qt_printer_widget_rep::_settings = NULL;

/*!
 * @todo Load the default printer settings from somewhere.
 */
qt_printer_widget_rep::qt_printer_widget_rep (command _cmd, url _file)
: commandAfterExecution(_cmd) {
#if defined(Q_WS_MAC) || defined(Q_WS_X11)
  if (!_settings)
    _settings = new CupsQTMPrinterSettings();
#endif
#ifdef Q_WS_WIN
  if (!_settings)
    _settings = new WinQTMPrinterSettings();
#endif
  _settings->fileName = to_qstring(as_string(_file));
}



/*!
 * 
 */ 
void
qt_printer_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_printer_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_VISIBILITY:   // Activates the widget
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool>(val) == true)
        showDialog();
      break;
    case SLOT_REFRESH:   // ignore: this widget doesn't need refreshing.
      break;
    default:  // unsupported slots
      qt_widget_rep::send (s, val);  // throws FAILED()
      break;
  }
}



/*! Return the widget as a top-level window to the eyes of TeXmacs. */
widget
qt_printer_widget_rep::plain_window_widget (string s, command q)
{
  // Identify ourselves as already being a top-level window, even though
  // the dialog won't be displayed until later.
  //view->setProperty ("texmacs_window_widget",
  //                 QVariant::fromValue ((void*) this));
  (void) s;
  commandAfterExecution = q;

  return this;
}



/*!
 * Shows the printer dialog. The native ones have more options than we can 
 * handle so we don't use them.
 * @fixme Executing the scheme closure at the end crashes TeXmacs upon exit!
 *        Looks like someone is trying to delete the command twice?
 */
void
qt_printer_widget_rep::showDialog () {
  
  QString _cmd;
  QTMPrintDialog pDialog(_settings);
  
  if (pDialog.exec() != QDialog::Accepted)
    return; // TODO: display message in the status bar.
  
  _cmd = _settings->toSystemCommand();
  
  // Send the document to the printer
  cout << "qt_printer_dialog_rep] Running command: " << from_qstring(_cmd) 
       << "\n";
  qt_system(from_qstring(_cmd));  // FIXME? qt_system is synchronous (blocking!)
  
  // execute the scheme closure 
  commandAfterExecution ();
}


