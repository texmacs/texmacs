
/******************************************************************************
 * MODULE     : qt_printer_widget.cpp
 * DESCRIPTION: A dialog to manage printing of the document
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifdef _MBD_EXPERIMENTAL_PRINTER_WIDGET

#include "qt_printer_widget.hpp"
#include "qt_utilities.hpp"   // macros TYPE_CHECK, NOT_IMPLEMENTED
#include "message.hpp"        // slots stuff

#include <QPrintDialog>
#include <QPrinter>


QPrinter* qt_printer_widget_rep::qtPrinter = NULL;

/**
 * Needed for whitebox_rep::equal, this is just a dummy. Should it not be?
 */
inline bool 
operator== (const PrinterSettings& _l, const PrinterSettings& _r) {
  return _l.accepted == _r.accepted;
}

/**
 * Needed for whitebox_rep::display
 * TODO: should return the strings of commands for lpr/cups if possible...
 */
inline tm_ostream& 
operator<< (tm_ostream& out, const PrinterSettings& sett) {
  return out << "PrinterSettings: " << sett.printerName << " - " 
  << sett.fileName << " - " << sett.paperSize << "\n";
}

/**
 * Reads the configuration parameters from a QPrinter object.
 */
void
PrinterSettings::getFromQPrinter(const QPrinter& from) {
  printToFile   = !(from.outputFileName().isNull ());
  printerName   = from_qstring( from.printerName () );
  fileName      = from_qstring( from.outputFileName () );
  landscape     = (from.orientation() == QPrinter::Landscape);
  paperSize     = qt_printer_widget_rep::qtPaperSizeToString(from.paperSize());
  dpi           = from.resolution ();
  firstPage     = from.fromPage ();
  lastPage      = from.toPage ();
  copyCount     = from.copyCount ();
  collateCopies = from.collateCopies();
  useColorIf    = (from.colorMode () == QPrinter::Color);
  // Yes?
  //printCommand  = from_qstring(qtPrinter->printerSelectionOption());
}

/**
 * Sets the configuration parameters to a QPrinter object.
 */
void
PrinterSettings::setToQPrinter(QPrinter& to) const {
  to.setResolution(dpi);
  to.setFromTo(firstPage, lastPage);
  to.setOrientation(landscape ? QPrinter::Landscape : QPrinter::Portrait);
  to.setOutputFileName(to_qstring(fileName));
  to.setPaperSize(qt_printer_widget_rep::stringToQtPaperSize(paperSize));
  to.setCopyCount(copyCount);
  to.setCollateCopies(collateCopies);
  to.setColorMode(useColorIf ? QPrinter::Color : QPrinter::GrayScale);
}


/**
 * 
 */
qt_printer_widget_rep::qt_printer_widget_rep (command _cmd, url _file)
: internalFileToPrint(_file), commandAfterExecution(_cmd) {
  if (!qtPrinter)
    qtPrinter = new QPrinter();
}


/**
 * UPDATE: this is no longer needed, since we showDialog() right in the
 * constructor and communication with the rest of TeXmacs is not possible.
 * This method is called by the helper template send<T,...> and is a way for
 * TeXmacs to tell our widget to perform one of the actions it can.
 */ 
void
qt_printer_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_printer_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_VISIBILITY:   // Activates the widget
    case SLOT_KEYBOARD_FOCUS:
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool>(val) == true)
        showDialog();
      break;
    case SLOT_PRINTER_SETTINGS:  // Sets values for the widget, before it's displayed
      TYPE_CHECK (type_box (val) == type_helper<PrinterSettings>::id);
      settings = open_box<PrinterSettings>(val);
      break;
    default:
      qt_widget_rep::send (s, val);
  }
}


/**
 * UPDATE: this is no longer needed, since we showDialog() right in the
 * constructor and communication with the rest of TeXmacs is not possible.
 * Used by TeXmacs to obtain information about this widget. You can use the
 * helper templated function query<T>(wid,slot,const T&) or directly
 * call this method using open_box<T> on the output. (T=PrinterSettings)
 */
blackbox
qt_printer_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_printer_widget_rep::query " << slot_name(s) << LF;
  if (s != SLOT_PRINTER_SETTINGS)
    return qt_widget_rep::query (s, type_id);
  else {
    TYPE_CHECK (type_id == type_helper<PrinterSettings>::id);
    return close_box<PrinterSettings>(settings);
  }
}


/**
 * Shows a printer dialog and parses the configuration done by the user. 
 * You'll want to query() this widget to get them.
 */
void qt_printer_widget_rep::showDialog () {
  settings.setToQPrinter(*qtPrinter);   // Try to set our preferences
  QPrintDialog pDialog (qtPrinter);   // ONLY THEN (or crash) create the dialog.
  settings.accepted  = (pDialog.exec() == QDialog::Accepted);
  settings.getFromQPrinter(*qtPrinter);
  
  if (settings.accepted)
    commandAfterExecution ();
}

/**
 * Just for internal use, converts QPrinter::PaperSize to a string representation.
 * Massimiliano's code.
 */
string
qt_printer_widget_rep::qtPaperSizeToString(const QPrinter::PaperSize _size) {
  
#define PAPER(fmt)  case QPrinter::fmt : return "fmt"
  switch (_size) {
      PAPER (A0) ; PAPER (A1) ; PAPER (A2) ; PAPER (A3) ; PAPER (A4) ;
      PAPER (A5) ; PAPER (A6) ; PAPER (A7) ; PAPER (A8) ; PAPER (A9) ;
      PAPER (B0) ; PAPER (B1) ; PAPER (B2) ; PAPER (B3) ; PAPER (B4) ;
      PAPER (B5) ; PAPER (B6) ; PAPER (B7) ; PAPER (B8) ; PAPER (B9) ;
      PAPER (B10) ;  PAPER (Letter) ;
    default: return "A4";
  }
#undef PAPER
}

/**
 * Just for internal use, converts a string to QPrinter::PaperSize.
 */
QPrinter::PaperSize
qt_printer_widget_rep::stringToQtPaperSize(const string _size) {
  
#define PAPER(fmt)  if(_size == "fmt") return QPrinter::fmt
  PAPER (A0) ; PAPER (A1) ; PAPER (A2) ; PAPER (A3) ; PAPER (A4) ;
  PAPER (A5) ; PAPER (A6) ; PAPER (A7) ; PAPER (A8) ; PAPER (A9) ;
  PAPER (B0) ; PAPER (B1) ; PAPER (B2) ; PAPER (B3) ; PAPER (B4) ;
  PAPER (B5) ; PAPER (B6) ; PAPER (B7) ; PAPER (B8) ; PAPER (B9) ;
  PAPER (B10) ;  PAPER (Letter) ;
  return QPrinter::A4;  // Default
#undef PAPER
}

#endif //_MBD_EXPERIMENTAL_PRINTER_WIDGET
