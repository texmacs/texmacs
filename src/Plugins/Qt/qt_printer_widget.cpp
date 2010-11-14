
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
inline PrinterSettings& 
operator<< (PrinterSettings& to, const QPrinter& from) {
  to.printToFile   = !(from.outputFileName().isNull ());
  to.printerName   = from_qstring( from.printerName () );
  to.fileName      = from_qstring( from.outputFileName () );
  to.landscape     = (from.orientation() == QPrinter::Landscape);
  to.paperSize     = qt_printer_widget_rep::qtPaperSizeToString(from.paperSize());
  to.dpi           = from.resolution ();
  to.firstPage     = from.fromPage ();
  to.lastPage      = from.toPage ();
  to.copyCount     = from.copyCount ();
  to.collateCopies = from.collateCopies();
  to.useColorIf    = (from.colorMode () == QPrinter::Color);
  // Yes?
  //settings.printCommand  = from_qstring(qtPrinter->printerSelectionOption());
  return to;
}

/**
 * Sets the configuration parameters to a QPrinter object.
 */
inline QPrinter& 
operator>>(const PrinterSettings& from, QPrinter& to) {
  to.setResolution(from.dpi);
  to.setFromTo(from.firstPage, from.lastPage);
  to.setOrientation(from.landscape ? QPrinter::Landscape : QPrinter::Portrait);
  to.setOutputFileName(to_qstring(from.fileName));
  to.setPaperSize(qt_printer_widget_rep::stringToQtPaperSize(from.paperSize));
  to.setCopyCount(from.copyCount);
  to.setCollateCopies(from.collateCopies);
  to.setColorMode(from.useColorIf ? QPrinter::Color : QPrinter::GrayScale);
  
  return to;
}


/**
 * _targetFile is a proposed file name if we are not really printing. This
 * doesn't fit very well into the scheme of things (export to PS and PDF is
 * for the moment done elsewhere) so it's not used.
 */
qt_printer_widget_rep::qt_printer_widget_rep () {
  if (!qtPrinter)
    qtPrinter = new QPrinter();
}


/**
 * This method is called by the helper template send<T,...> and is a way for
 * TeXmacs to tell our widget to perform one of the actions it can.
 */ 
void
qt_printer_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_printer_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_KEYBOARD_FOCUS:   // Activates the widget
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool>(val) == true)
        showDialog();
      break;
    case SLOT_PRINTER_SETTINGS:  // Sets values for the widget, before it's displayed
      TYPE_CHECK (type_box (val) == type_helper<PrinterSettings>::id);
      settings = open_box<PrinterSettings>(val);
      break;   // If this is missing we crash... Why? [MBD 14.Nov]
    default:
      qt_widget_rep::send (s, val);
  }
}


/**
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
  settings >> *qtPrinter;          // Try to set our preferences to the printer.
  QPrintDialog pDialog (qtPrinter);   // ONLY THEN (or crash) create the dialog.
  settings.accepted  = (pDialog.exec() == QDialog::Accepted);   // Ask the user.
  settings << *qtPrinter;          // Read the user preferences.
  
  return;
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
