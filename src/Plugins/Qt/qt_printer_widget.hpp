
/******************************************************************************
 * MODULE     : qt_printer_widget.hpp
 * DESCRIPTION: A dialog to manage printing of the document
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifdef _MBD_EXPERIMENTAL_PRINTER_WIDGET
#ifndef QT_PRINTER_WIDGET_HPP
#define QT_PRINTER_WIDGET_HPP

#include "qt_widget.hpp"
#include "qt_renderer.hpp"

#include <QPrinter>  // QPrinter::PaperSize

/**
 * This structure holds the printing options that we are able to manage or
 * heed. This should be something GUI independent.
 */
typedef struct PrinterSettings {
  bool        accepted;  /*! whether the dialog was accepted */
  bool     printToFile;
  bool       landscape;
  bool   collateCopies;
  bool      useColorIf;
  string   printerName;
  string      fileName;
  string     paperSize;
  int              dpi;  /*! Mostly useless with the native dialogs */
  int        firstPage;  /*! If firstPage == lastPage == 0, print everything */
  int         lastPage;
  int        copyCount;
public:
  PrinterSettings() 
  : accepted(false), printToFile(true), landscape(false), collateCopies(true),
  useColorIf(true), printerName(""), fileName(""), paperSize("A4"), dpi(600),
  firstPage(0), lastPage(0), copyCount(1) {}
  friend bool operator== (const PrinterSettings&, const PrinterSettings&);
  friend tm_ostream& operator<< (tm_ostream&, const PrinterSettings&);
  void getFromQPrinter(const QPrinter&);
  void setToQPrinter(QPrinter&) const;
  
} PrinterSettings;


/**
 * This implements a printer widget, using the native printer dialogs where
 * available.
 * The printing settings selected by the user can be query()'ed via the slot
 * SLOT_PRINTER_SETTINGS. The query returns a PrinterSettings struct.
 *
 * The "factory" function for this widget is called printer_widget(), 
 * in qt_dialogues.cpp
 */ 
class qt_printer_widget_rep: public qt_widget_rep { 
public:
  qt_printer_widget_rep (command, url);
  ~qt_printer_widget_rep () { };
  
  virtual void          send (slot s, blackbox val);
  virtual blackbox     query (slot s, int type_id);  // shouldn't this be const?
  widget plain_window_widget (string s) { return this; }

  void showDialog ();
  static string qtPaperSizeToString(const QPrinter::PaperSize);
  static QPrinter::PaperSize stringToQtPaperSize(const string);
private:      
  // Making qtPrinter static should preserve settings between calls.
  static QPrinter*    qtPrinter;
  PrinterSettings      settings;
  url       internalFileToPrint;
  command commandAfterExecution;
};

#endif  // QT_PRINTER_WIDGET_HPP
#endif  // _MBD_EXPERIMENTAL_PRINTER_WIDGET
  